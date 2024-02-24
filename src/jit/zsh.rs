use std::io::Write;

use hashbrown::HashMap;
use slice_group_by::GroupBy;
use ustr::{ustr, UstrMap, Ustr};

use crate::{grammar::{Specialization, DFARef}, regex::Input, dfa::DFA, StateId, aot::{zsh::write_subword_fn, zsh::make_string_constant}};

use super::get_transitions;


// ASSUMPTION: compadd -d ... -a ... is the only allowed form to pass completion descriptions to
// compadd.  This requirement dictates the to use shell arrays, not invidual calls to compadd.


// Returns a map: command -> command id
pub fn get_command_transitions(transitions: &[Input]) -> UstrMap<usize> {
    let mut id = 0;
    let mut top_level: UstrMap<usize> = Default::default();
    for input in transitions {
        let cmd = match input {
            Input::Command(cmd, ..) => *cmd,
            Input::Subword(..) => continue,
            Input::Nonterminal(..) => continue,
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


// Returns a map: command -> spec command id
fn get_zsh_specialized_command_transitions(transitions: &[Input]) -> UstrMap<usize> {
    let mut id = 0;
    let mut top_level: UstrMap<usize> = Default::default();
    for input in transitions {
        let cmd = match input {
            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), ..) => *cmd,
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


fn get_subword_transitions(transitions: &[Input]) -> HashMap<DFARef, usize> {
    let mut id = 1;
    let mut id_from_dfa: HashMap<DFARef, usize> = Default::default();
    for input in transitions {
        let subdfa = match input {
            Input::Subword(subdfa, ..) => subdfa,
            Input::Command(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Literal(..) => continue,
        };
        id_from_dfa.entry(subdfa.clone()).or_insert_with(|| {
            let save = id;
            id += 1;
            save
        });
    }
    id_from_dfa
}


fn get_subword_commands(transitions: &[Input]) -> HashMap<DFARef, Vec<(StateId, Ustr)>> {
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
                        let cmd = match input {
                            Input::Command(cmd, ..) => *cmd,
                            Input::Subword(..) => unreachable!(),
                            Input::Nonterminal(..) => continue,
                            Input::Literal(..) => continue,
                        };
                        transitions.push((*from, cmd));
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


fn get_subword_specialized_commands(transitions: &[Input]) -> HashMap<DFARef, Vec<(StateId, Ustr)>> {
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
                            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), ..) => transitions.push((*from, *cmd)),
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


fn make_external_command_function_name(command: &str, id: usize) -> String {
    format!("_{command}_cmd_{id}")
}


fn make_specialized_external_command_function_name(command: &str, id: usize) -> String {
    format!("_{command}_spec_{id}")
}


pub fn write_zsh_completion_shell_code<W: Write>(
    completed_command: &str,
    dfa: &DFA,
    words_before_cursor: &[&str],
    entered_prefix: &str,
    output: &mut W,
) -> anyhow::Result<()> {
    let mut transitions = get_transitions(&dfa, &words_before_cursor);
    transitions.sort_unstable_by_key(|input| input.get_fallback_level());

    let id_from_top_level_command = get_command_transitions(&transitions);
    for (cmd, id) in &id_from_top_level_command {
        write!(output, r#"{} () {{
    {cmd}
}}

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
    let subword_spec_transitions = get_subword_specialized_commands(&transitions);
    let id_from_subword_spec: UstrMap<usize> = subword_spec_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (dfa, id) in &id_from_dfa {
        let subword_command_id_from_state: HashMap<StateId, usize> = subword_command_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_command.get(cmd).unwrap())).collect();
        let subword_spec_id_from_state: HashMap<StateId, usize> = subword_spec_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_spec.get(cmd).unwrap())).collect();
        write_subword_fn(output, completed_command, *id, dfa.as_ref(), &subword_command_id_from_state, &subword_spec_id_from_state)?;
        writeln!(output)?;
    }

    let id_from_specialized_command: UstrMap<usize> = get_zsh_specialized_command_transitions(&transitions);
    for (cmd, id) in &id_from_specialized_command {
        write!(output, r#"{} () {{
    {cmd}
}}

"#, make_specialized_external_command_function_name(completed_command, *id))?;
    }

    // Generate shell code for fallback levels in order, optionally calling shell functions defined
    // above.
    writeln!(output, r#"__complgen_jit () {{"#)?;
    writeln!(output, r#"    declare -a completions_no_description_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a completions_no_description_no_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a completions_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a suffixes_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a descriptions_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a completions_no_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a suffixes_no_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a descriptions_no_trailing_space=()"#)?;
    writeln!(output, r#"    declare -a matches=()"#)?;
    writeln!(output)?;

    let groups: Vec<&[Input]> = transitions.linear_group_by_key(|t| t.get_fallback_level()).collect();
    for (index, group) in groups.iter().enumerate() {
        let mut completions_trailing_space: Vec<Ustr> = Default::default();
        let mut suffixes_trailing_space: Vec<Ustr> = Default::default();
        let mut descriptions_trailing_space: Vec<String> = Default::default();
        let mut completions_no_description_trailing_space: Vec<Ustr> = Default::default();

        if index != 0 {
            writeln!(output, r#"    completions_no_description_trailing_space=()"#)?;
            writeln!(output, r#"    completions_no_description_no_trailing_space=()"#)?;
            writeln!(output, r#"    completions_trailing_space=()"#)?;
            writeln!(output, r#"    suffixes_trailing_space=()"#)?;
            writeln!(output, r#"    descriptions_trailing_space=()"#)?;
            writeln!(output, r#"    completions_no_trailing_space=()"#)?;
            writeln!(output, r#"    suffixes_no_trailing_space=()"#)?;
            writeln!(output, r#"    descriptions_no_trailing_space=()"#)?;
            writeln!(output, r#"    matches=()"#)?;
            writeln!(output)?;
        }

        for (literal, description) in group.iter().filter_map(|t| match t {
            Input::Literal(literal, description, ..) => Some((literal, description.unwrap_or(ustr("")).as_str().to_string())),
            _ => None,
        }) {
            if !description.is_empty() {
                completions_trailing_space.push(*literal);
                suffixes_trailing_space.push(*literal);
                descriptions_trailing_space.push(description);
            } else {
                completions_no_description_trailing_space.push(*literal);
            }
        }

        // An external command -- execute it and collect stdout lines as completions
        for cmd in group.iter().filter_map(|t| match t {
            Input::Command(cmd, ..) => Some(*cmd),
            Input::Nonterminal(_, Some(Specialization { zsh: None, generic: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_top_level_command.get(&cmd).unwrap();
            let fn_name = make_external_command_function_name(completed_command, *command_id);
            // TODO entered_prefix == $WORDS[$CURRENT].  Zdecydowac się na jedno z dwoch wszedzie
            if entered_prefix.is_empty() {
                writeln!(output, r#"    local lines=("${{(@f)$({fn_name})}}")"#)?;
            }
            else {
                writeln!(output, r#"    local lines=("${{(@f)$({fn_name} {entered_prefix})}}")"#)?;
            }
            writeln!(output, r#"    for line in ${{lines[@]}}; do
        local parts=(${{(@s:	:)line}})
        if [[ -v "parts[2]" ]]; then
            completions_trailing_space+=("${{parts[1]}}")
            suffixes_trailing_space+=("${{parts[1]}}")
            descriptions_trailing_space+=("${{parts[2]}}")
        else
            completions_no_description_trailing_space+=("${{parts[1]}}")
        fi
    done
"#)?;
                }

        for subdfa in group.iter().filter_map(|t| match t {
            Input::Subword(subdfa, ..) => Some(subdfa),
            _ => None,
        }) {
            let subdfa_id = id_from_dfa.get(subdfa).unwrap();
            // TODO Extract the name generation into a function
            writeln!(output, r#"    _{completed_command}_subword_{subdfa_id} complete "${{words[$CURRENT]}}" completions_no_description_trailing_space completions_trailing_space suffixes_trailing_space descriptions_trailing_space completions_no_description_no_trailing_space completions_no_trailing_space suffixes_no_trailing_space descriptions_no_trailing_space"#)?;
        }
                // level complete, so it's important.
        // An external command that calls compadd itself and return 0 exit code if there
        // were some completions produced.  The exit code is used to declare a fallback
        // level complete, so it's important.
        for cmd in group.iter().filter_map(|t| match t {
            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), ..) => Some(*cmd),
            _ => None,
        }) {
            let command_id = id_from_specialized_command.get(&cmd).unwrap();
            let fn_name = make_specialized_external_command_function_name(completed_command, *command_id);
            writeln!(output, r#"    {}"#, fn_name)?;
            writeln!(output, r#"    [[ $? -eq 0 ]] && matches+=(fallback_level_matched)"#)?;
        }

        if !completions_trailing_space.is_empty() {
            writeln!(output, r#"    completions_trailing_space+=({})"#, itertools::join(completions_trailing_space.iter().map(|s| make_string_constant(s)), " "))?;
        }
        if !suffixes_trailing_space.is_empty() {
            writeln!(output, r#"    suffixes_trailing_space+=({})"#, itertools::join(suffixes_trailing_space.iter().map(|s| make_string_constant(s)), " "))?;
        }
        if !descriptions_trailing_space.is_empty() {
            writeln!(output, r#"    descriptions_trailing_space+=({})"#, itertools::join(descriptions_trailing_space.iter().map(|s| make_string_constant(s)), " "))?;
        }
        if !completions_no_description_trailing_space.is_empty() {
            writeln!(output, r#"    completions_no_description_trailing_space+=({})"#, itertools::join(completions_no_description_trailing_space.iter().map(|s| make_string_constant(s)), " "))?;
        }

        write!(output, r#"    local maxlen=0
    for suffix in ${{suffixes_trailing_space[@]}}; do
        if [[ ${{#suffix}} -gt $maxlen ]]; then
            maxlen=${{#suffix}}
        fi
    done
    for suffix in ${{suffixes_no_trailing_space[@]}}; do
        if [[ ${{#suffix}} -gt $maxlen ]]; then
            maxlen=${{#suffix}}
        fi
    done

    for ((i = 1; i <= $#suffixes_trailing_space; i++)); do
        if [[ -z ${{descriptions_trailing_space[$i]}} ]]; then
            descriptions_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_trailing_space[$i]}}}}"
        else
            descriptions_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_trailing_space[$i]}}}} -- ${{descriptions_trailing_space[$i]}}"
        fi
    done

    for ((i = 1; i <= $#suffixes_no_trailing_space; i++)); do
        if [[ -z ${{descriptions_no_trailing_space[$i]}} ]]; then
            descriptions_no_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_no_trailing_space[$i]}}}}"
        else
            descriptions_no_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_no_trailing_space[$i]}}}} -- ${{descriptions_no_trailing_space[$i]}}"
        fi
    done

    compadd -Q -a completions_no_description_trailing_space
    compadd -Q -S ' ' -a completions_no_description_no_trailing_space
    compadd -l -Q -a -d descriptions_trailing_space completions_trailing_space
    compadd -l -Q -S '' -a -d descriptions_no_trailing_space completions_no_trailing_space
"#)?;

        if index < groups.len()-1 {
            writeln!(output, r#"    compadd -O matches -a completions_no_description_trailing_space"#)?;
            writeln!(output, r#"    compadd -O matches -a completions_no_description_no_trailing_space"#)?;
            writeln!(output, r#"    compadd -O matches -a completions_trailing_space"#)?;
            writeln!(output, r#"    compadd -O matches -a completions_no_trailing_space"#)?;
            writeln!(output, r#"    [[ ${{#matches}} -gt 0 ]] && return"#)?;
            writeln!(output)?;
        }
    }
    writeln!(output, r#"}}"#)?;

    // Call the generated shell function.
    writeln!(output, r#"__complgen_jit"#)?;
    Ok(())
}
