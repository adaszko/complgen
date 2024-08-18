use std::io::Write;

use hashbrown::HashMap;
use slice_group_by::GroupBy;
use ustr::{Ustr, UstrMap};

use crate::{aot::bash::{make_string_constant, write_generic_subword_fn, write_subword_fn}, dfa::DFA, grammar::{DFARef, Specialization}, regex::Input, StateId};

use super::{get_command_transitions, get_subword_commands, get_subword_transitions, get_transitions, make_external_command_fn_name, make_specialized_external_command_fn_name, make_subword_external_command_fn_name, make_subword_fn_name, make_subword_specialized_external_command_fn_name};


// Returns a map: command -> spec command id
fn get_bash_specialized_command_transitions(transitions: &[Input]) -> UstrMap<usize> {
    let mut id = 0;
    let mut top_level: UstrMap<usize> = Default::default();
    for input in transitions {
        let cmd = match input {
            Input::Nonterminal(_, Some(Specialization { bash: Some(cmd), .. }), ..) => *cmd,
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


fn get_bash_subword_specialized_commands(transitions: &[Input]) -> HashMap<DFARef, Vec<(StateId, Ustr)>> {
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
                            Input::Nonterminal(_, Some(Specialization { bash: Some(cmd), .. }), ..) => transitions.push((*from, *cmd)),
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


pub fn write_bash_completion_shell_code<W: Write>(
    completed_command: &str,
    dfa: &DFA,
    ignore_case: bool,
    words_before_cursor: &[&str],
    entered_prefix: &str,
    superfluous_prefix: &str,
    output: &mut W,
    test_mode: bool,
) -> anyhow::Result<()> {
    let prefix_constant = make_string_constant(entered_prefix);

    let mut transitions = get_transitions(&dfa, &words_before_cursor);
    transitions.sort_unstable_by_key(|input| input.get_fallback_level());

    let id_from_top_level_command = get_command_transitions(&transitions);
    for (cmd, id) in &id_from_top_level_command {
        write!(output, r#"{} () {{
    {cmd}
}}
"#, make_external_command_fn_name(completed_command, *id))?;
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
        write!(output, r#"{} () {{
    {cmd}
}}
"#, make_subword_external_command_fn_name(completed_command, *id))?;
    }

    let subword_spec_transitions = get_bash_subword_specialized_commands(&transitions);
    let id_from_subword_spec: UstrMap<usize> = subword_spec_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (cmd, id) in &id_from_subword_spec {
        write!(output, r#"{} () {{
    {cmd}
}}
"#, make_subword_specialized_external_command_fn_name(completed_command, *id))?;
    }

    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(output, completed_command)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(output, completed_command, *id, dfa.as_ref(), &id_from_subword_command, &id_from_subword_spec)?;
        writeln!(output)?;
    }

    let id_from_specialized_command: UstrMap<usize> = get_bash_specialized_command_transitions(&transitions);
    for (cmd, id) in &id_from_specialized_command {
        write!(output, r#"{} () {{
    {cmd}
}}
"#, make_specialized_external_command_fn_name(completed_command, *id))?;
    }

    // Generate shell code for fallback levels in order, optionally calling shell functions defined
    // above.
    writeln!(output, r#"__complgen_jit () {{
    local -a completions=()
    local -a matches=()
"#)?;

    let groups: Vec<&[Input]> = transitions.linear_group_by_key(|t| t.get_fallback_level()).collect();
    for group in groups {
        let literals: Vec<&Ustr> = group.iter().filter_map(|t| match t {
            Input::Literal(literal, ..) => Some(literal),
            _ => None,
        }).collect();
        if !literals.is_empty() {
            writeln!(output, r#"    completions+=({})"#, itertools::join(literals.iter().map(|s| make_string_constant(s)), " "))?;
        }

        // An external command -- execute it and collect stdout lines as completions
        for cmd in group.iter().filter_map(|t| match t {
            Input::Command(cmd, ..) => Some(*cmd),
            Input::Nonterminal(_, Some(Specialization { bash: None, generic: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_top_level_command.get(&cmd).unwrap();
            let fn_name = make_external_command_fn_name(completed_command, *command_id);
            writeln!(output, r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} {prefix_constant})"#)?
        }

        for subdfa in group.iter().filter_map(|t| match t {
            Input::Subword(subdfa, ..) => Some(subdfa),
            _ => None,
        }) {
            let subdfa_id = id_from_dfa.get(subdfa).unwrap();
            let fn_name = make_subword_fn_name(completed_command, *subdfa_id);
            writeln!(output, r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} complete {prefix_constant})"#)?
        }
        for cmd in group.iter().filter_map(|t| match t {
            Input::Nonterminal(_, Some(Specialization { bash: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_specialized_command.get(&cmd).unwrap();
            let fn_name = make_specialized_external_command_fn_name(completed_command, *command_id);
            writeln!(output, r#"    readarray -t -O ${{#completions[@]}} completions < <({fn_name} {prefix_constant})"#)?
        }

        if entered_prefix.is_empty() {
            writeln!(output, r#"    matches=("${{completions[@]}}")"#)?;
        }
        else {
            // Filter `completions` by `entered_prefix`, respecting `ignore_case`
            if ignore_case {
                writeln!(output, r#"    local prefix={prefix_constant}
        lowercase_prefix=${{prefix,,}}
        for item in "${{completions[@]}}"; do
            [[ "${{item,,}}" = ${{lowercase_prefix}}* ]] && matches+=("$item")
        done
"#)?;
            } else {
                writeln!(output, r#"    for item in "${{completions[@]}}"; do
        [[ $item = {prefix_constant}* ]] && matches+=("$item")
    done"#)?;
            }
        }

        // Strip `superfluous_prefix` bashism
        if superfluous_prefix.is_empty() {
            writeln!(output, r#"    if [[ ${{#matches[@]}} -gt 0 ]]; then
        COMPREPLY=("${{matches[@]}}")
        return
    fi"#)?;
        } else {
            writeln!(output, r#"    if [[ ${{#matches[@]}} -gt 0 ]]; then
        matches=("${{matches[@]#{superfluous_prefix}}}")
        COMPREPLY=("${{matches[@]}}")
        return
    fi"#)?;
        }
    }
    writeln!(output, r#"}}"#)?;

    if test_mode {
        return Ok(());
    }

    // Call the generated shell function.
    writeln!(output, r#"__complgen_jit"#)?;
    Ok(())
}
