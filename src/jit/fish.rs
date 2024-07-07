use std::io::Write;

use hashbrown::HashMap;
use slice_group_by::GroupBy;
use ustr::{UstrMap, Ustr, ustr};

use crate::{aot::fish::{make_string_constant, write_subword_fn}, dfa::DFA, grammar::{DFARef, Specialization}, regex::Input, StateId};

use super::{get_transitions, get_command_transitions, make_external_command_function_name, get_subword_transitions, get_subword_commands, make_subword_external_command_fn_name, make_subword_specialized_external_command_fn_name, make_specialized_external_command_fn_name, make_subword_function_name};


// Returns a map: command -> spec command id
fn get_fish_specialized_command_transitions(transitions: &[Input]) -> UstrMap<usize> {
    let mut id = 0;
    let mut top_level: UstrMap<usize> = Default::default();
    for input in transitions {
        let cmd = match input {
            Input::Nonterminal(_, Some(Specialization { fish: Some(cmd), .. }), ..) => *cmd,
            Input::Subword(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Command(..) => continue,
            Input::Literal(..) => continue,
        };
        top_level.entry(cmd).or_insert_with(|| {
            let sav = id;
            id += 1;
            sav
        });
    }
    top_level
}


fn get_fish_subword_specialized_commands(transitions: &[Input]) -> HashMap<DFARef, Vec<(StateId, Ustr)>> {
    let mut subdfas: HashMap<DFARef, Vec<(StateId, Ustr)>> = Default::default();
    for input in transitions {
        match input {
            Input::Subword(subdfa, ..) => {
                if subdfas.contains_key(subdfa) {
                    continue;
                }
                let transitions = subdfas.entry(subdfa.clone()).or_default();
                for (from, tos) in &subdfa.as_ref().transitions {
                    for (input, _) in tos {
                        match input {
                            Input::Nonterminal(_, Some(Specialization { fish: Some(cmd), .. }), ..) => transitions.push((*from, *cmd)),
                            Input::Nonterminal(..) => continue,
                            Input::Subword(..) => unreachable!(),
                            Input::Command(..) => continue,
                            Input::Literal(..) => continue,
                        }
                    }
                }
            },
            Input::Command(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Literal(..) => continue,
        }
    }
    subdfas
}


pub fn write_fish_completion_shell_code<W: Write>(
    completed_command: &str,
    dfa: &DFA,
    words_before_cursor: &[&str],
    entered_prefix: &str,
    output: &mut W,
    test_mode: bool,
) -> anyhow::Result<()> {
    let mut transitions = get_transitions(&dfa, &words_before_cursor);
    transitions.sort_unstable_by_key(|input| input.get_fallback_level());

    let id_from_top_level_command = get_command_transitions(&transitions);
    for (cmd, id) in &id_from_top_level_command {
        write!(output, r#"function {}
    set 1 $argv[1]
    {cmd}
end
"#, make_external_command_function_name(completed_command, *id))?;
    }

    let id_from_dfa = get_subword_transitions(&transitions);
    let subword_command_transitions = get_subword_commands(&transitions);
    let id_from_subword_command: UstrMap<usize> = subword_command_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (cmd, id) in &id_from_subword_command {
        write!(output, r#"function {}
    set 1 $argv[1]
    {cmd}
end
"#, make_subword_external_command_fn_name(completed_command, *id))?;
    }

    let subword_spec_transitions = get_fish_subword_specialized_commands(&transitions);
    let id_from_subword_spec: UstrMap<usize> = subword_spec_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (cmd, id) in &id_from_subword_spec {
        write!(output, r#"function {}
    set 1 $argv[1]
    {cmd}
end
"#, make_subword_specialized_external_command_fn_name(completed_command, *id))?;
    }

    for (dfa, id) in &id_from_dfa {
        let subword_command_id_from_state: HashMap<StateId, usize> = subword_command_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_command.get(cmd).unwrap())).collect();
        let subword_spec_id_from_state: HashMap<StateId, usize> = subword_spec_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_spec.get(cmd).unwrap())).collect();
        write_subword_fn(output, completed_command, *id, dfa.as_ref(), &subword_command_id_from_state, &subword_spec_id_from_state)?;
        writeln!(output)?;
    }

    let id_from_specialized_command: UstrMap<usize> = get_fish_specialized_command_transitions(&transitions);
    for (cmd, id) in &id_from_specialized_command {
        write!(output, r#"function {}
    set 1 $argv[1]
    {cmd}
end
"#, make_specialized_external_command_fn_name(completed_command, *id))?;
    }

    // Generate shell code for fallback levels in order, optionally calling shell functions defined
    // above.
    writeln!(output, r#"function __complgen_jit
    set --local completions
"#)?;

    let groups: Vec<&[Input]> = transitions.linear_group_by_key(|t| t.get_fallback_level()).collect();
    for group in groups {
        for (literal, description) in group.iter().filter_map(|t| match t {
            Input::Literal(literal, description, ..) => Some((literal, description.unwrap_or(ustr("")).as_str().to_string())),
            _ => None,
        }) {
            if !description.is_empty() {
                writeln!(output, r#"    set -a completions "{literal}	{description}""#)?;
            }
            else {
                writeln!(output, r#"    set -a completions {literal}"#)?;
            }
        }


        // An external command -- execute it and collect stdout lines as completions
        for cmd in group.iter().filter_map(|t| match t {
            Input::Command(cmd, ..) => Some(*cmd),
            Input::Nonterminal(_, Some(Specialization { fish: None, generic: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_top_level_command.get(&cmd).unwrap();
            let fn_name = make_external_command_function_name(completed_command, *command_id);
            writeln!(output, r#"    set -a completions ({fn_name} {})"#, make_string_constant(entered_prefix))?;
        }

        for subdfa in group.iter().filter_map(|t| match t {
            Input::Subword(subdfa, ..) => Some(subdfa),
            _ => None,
        }) {
            let subdfa_id = id_from_dfa.get(subdfa).unwrap();
            let prefix = make_string_constant(entered_prefix);
            writeln!(output, r#"    set -a completions ({} complete {prefix})"#, make_subword_function_name(completed_command, *subdfa_id))?;
        }
        for cmd in group.iter().filter_map(|t| match t {
            Input::Nonterminal(_, Some(Specialization { fish: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_specialized_command.get(&cmd).unwrap();
            let fn_name = make_specialized_external_command_fn_name(completed_command, *command_id);
            writeln!(output, r#"    set -a completions ({fn_name} {})"#, make_string_constant(entered_prefix))?;
        }

        writeln!(output, r#"    if set -q completions[1]; printf %s\n $completions; return; end"#)?;
    }
    writeln!(output, r#"end"#)?;

    if test_mode {
        return Ok(());
    }

    // Call the generated shell function.
    writeln!(output, r#"__complgen_jit"#)?;

    Ok(())
}
