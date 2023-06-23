use std::fmt::Write;

use complgen::{StateId, Result};
use hashbrown::HashMap;
use ustr::UstrMap;
use crate::dfa::DFA;


// Array indexes in ZSH start from 1 (!)


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
    let id_from_input: UstrMap<usize> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (ustr, _))| (ustr, id + 1)).collect();

    writeln!(buffer, r#"    declare -A literals"#)?;
    let literals: String = itertools::join(id_from_input.iter().map(|(literal, id)| format!("[{literal}]={id}")), " ");
    writeln!(buffer, r#"    literals=({literals})"#)?;
    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A transitions"#)?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, to)| (*id_from_input.get(&input).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to + 1)), " ");
        writeln!(buffer, r#"    transitions[{}]="({state_transitions})""#, state + 1)?;
    }

    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{}]={}", from+1, to+1)), " ");
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

    write!(buffer, r#"_{command} () {{
"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
    local word_index=2
    while [[ $word_index -lt $CURRENT ]]; do
        [[ -v "transitions[$state]" ]] || return 1

        local state_transitions_initializer=${{transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        local word=${{words[$word_index]}}

        if [[ -v "literals[$word]" ]]; then
            local literal_id=${{literals[$word]}}
            if [[ -v "state_transitions[$literal_id]" ]]; then
                state=${{state_transitions[$literal_id]}}
                word_index=$((word_index + 1))
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
    [[ -v "transitions[$state]" ]] || return 1

"#, starting_state = dfa.starting_state + 1)?;

    writeln!(buffer, r#"    declare -A commands"#)?;
    let command_id_from_state: HashMap<StateId, usize> = dfa.get_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_command.get(&cmd).unwrap())).collect();
    let commands_array_initializer = itertools::join(command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
    writeln!(buffer, r#"    commands=({commands_array_initializer})"#)?;

    write!(buffer, r#"
    local state_transitions_initializer=${{transitions[$state]}}
    declare -A state_transitions
    eval "state_transitions=$state_transitions_initializer"

    declare -A inverted_literals
    for key in ${{(k)literals}}; do
        local value=${{literals[$key]}}
        inverted_literals[$value]=$key
    done

    local completions=()
    for literal_id in ${{(k)state_transitions}}; do
        completions+=(${{inverted_literals[$literal_id]}})
    done
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        command_completions=("${{(@f)$(_{command}_${{command_id}})}}")
        for line in ${{command_completions[@]}}; do
            completions+=($line)
        done
    fi

    compadd -- ${{completions[@]}}
    return 0
}}

compdef _{command} {command}
"#)?;
    Ok(())
}