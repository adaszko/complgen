use std::io::Write;

use complgen::{StateId, Result};
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;


/// `literals`: an associative array used for literals deduplication (interning) where:
///   * key: the literal
///   * value: literal's id
///
/// `transitions`: an associative array where:
///   * key: source state number
///   * value: a string that is an initializer of an associative array, e.g. "( [add]=23 [obliterate]=1080 [repair]=1543 )"
///
/// The initializer value above indicates there are 3 transitions from the state of id `key`.
/// First of which is a transition on word `add`, to the state 23.
///
/// `match_anything_transitions`: an associative array where:
///  * key: state number
///  * value: state number
/// An entry in the `match_anything_transitions` array indicates that there's a fallback transition that
/// accepts any word
///
fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, input, description)| ((*input, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| literal), " ");
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A transitions"#)?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to)), " ");
        writeln!(buffer, r#"    transitions[{state}]="({state_transitions})""#)?;
    }

    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{from}]={to}")), " ");
    writeln!(buffer, r#"    match_anything_transitions=({match_anything_transitions})"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let id_from_command: UstrMap<usize> = dfa.get_command_transitions().into_iter().enumerate().map(|(id, (_, cmd))| (cmd, id)).collect();
    for (cmd, id) in &id_from_command {
        write!(buffer, r#"_{command}_{id} () {{
    {cmd}
}}

"#)?;
    }

    let id_from_specialized_command: UstrMap<usize> = dfa.get_bash_command_transitions().into_iter().enumerate().map(|(id, (_, cmd))| (cmd, id)).collect();
    for (cmd, id) in &id_from_specialized_command {
        write!(buffer, r#"_{command}_spec_{id} () {{
    {cmd}
}}

"#)?;
    }

    write!(buffer, r#"_{command} () {{
"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
    local word_index=1
    while [[ $word_index -lt $COMP_CWORD ]]; do
        if [[ -v "transitions[$state]" ]]; then
            local state_transitions_initializer=${{transitions[$state]}}
            declare -A state_transitions
            eval "state_transitions=$state_transitions_initializer"

            local word=${{COMP_WORDS[$word_index]}}
            local word_matched=0
            for literal_id in $(seq 1 ${{#literals[@]}}); do
                if [[ ${{literals[$literal_id]}} = "$word" ]]; then
                    if [[ -v "state_transitions[$literal_id]" ]]; then
                        state=${{state_transitions[$literal_id]}}
                        word_index=$((word_index + 1))
                        word_matched=1
                        break
                    fi
                fi
            done
            if [[ $word_matched -ne 0 ]]; then
                continue
            fi
        fi

        if [[ -v "match_anything_transitions[$state]" ]]; then
            state=${{match_anything_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi

        return 1
    done

"#, starting_state = dfa.starting_state)?;

    write!(buffer, r#"
    local completions=()

    if [[ -v "transitions[$state]" ]]; then
        local state_transitions_initializer=${{transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        for literal_id in "${{!state_transitions[@]}}"; do
            completions+=("${{literals[$literal_id]}}")
        done
    fi
"#)?;

    let command_id_from_state: HashMap<StateId, usize> = dfa.get_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_command.get(&cmd).unwrap())).collect();
    if !command_id_from_state.is_empty() {
        writeln!(buffer, r#"    declare -A commands"#)?;
        let array_initializer = itertools::join(command_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        mapfile -t completions -O "${{#completions[@]}}" < <(_{command}_${{command_id}} "${{COMP_WORDS[$COMP_CWORD]}}" | cut -f1)
    fi

"#)?;
    }

    let specialized_command_id_from_state: HashMap<StateId, usize> = dfa.get_bash_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_specialized_command.get(&cmd).unwrap())).collect();
    if !specialized_command_id_from_state.is_empty() {
        writeln!(buffer, "")?;
        writeln!(buffer, r#"    declare -A specialized_commands"#)?;
        let array_initializer = itertools::join(specialized_command_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        mapfile -t completions -O "${{#completions[@]}}" < <(_{command}_spec_"${{command_id}}" "${{COMP_WORDS[$COMP_CWORD]}}" | cut -f1)
    fi

"#)?;
    }

    write!(buffer, r#"
    local prefix="${{COMP_WORDS[$COMP_CWORD]}}"
    for item in "${{completions[@]}}"; do
        if [[ $item = "${{prefix}}"* ]]; then
            COMPREPLY+=("$item")
        fi
    done
    return 0
}}

complete -F _{command} {command}
"#)?;
    Ok(())
}
