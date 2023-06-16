use std::fmt::Write;

use complgen::{StateId, Result};
use ustr::UstrMap;
use crate::dfa::DFA;


// `literals`: an associative array used for literals deduplication (interning) where:
//      key: the literal
//      value: literal's id
// `transitions`: an associative array where:
//      key: state number
//      value: a string that is an initializer of an associative array, e.g. "( [add]=23 [obliterate]=1080 [repair]=1543 )"
//  `asterisk_transitions`: an associative array where:
//      key: state number
//      value: state number
// An entry in the `asterisk_transitions` array indicates that there's a fallback transition that
// accepts any word
fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let id_from_input: UstrMap<usize> = dfa.get_literal_inputs().into_iter().enumerate().map(|(id, ustr)| (ustr, id)).collect();

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
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to)), " ");
        writeln!(buffer, r#"    transitions[{state}]="({state_transitions})""#)?;
    }

    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A asterisk_transitions"#)?;
    let asterisk_transitions = itertools::join(dfa.get_asterisk_transitions().into_iter().map(|(from, to)| format!("[{from}]={to}")), " ");
    writeln!(buffer, r#"    asterisk_transitions=({asterisk_transitions})"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    write!(buffer, r#"_{command} () {{
"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
    local word_index=1
    while [[ $word_index -lt $COMP_CWORD ]]; do
        [[ -v "transitions[$state]" ]] || return 1
        local state_transitions_initializer=${{transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"
        local word=${{COMP_WORDS[$word_index]}}
        if [[ ! -v "literals[$word]" ]]; then
            return 1
        fi
        local literal_id=${{literals[$word]}}
        if [[ -v "state_transitions[$literal_id]" ]]; then
            state=${{state_transitions[$literal_id]}}
            word_index=$((word_index + 1))
            continue
        fi
        if [[ -v "asterisk_transitions[$state]" ]]; then
            state=${{asterisk_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi
        return 1
    done

    [[ -v "transitions[$state]" ]] || return 1
    local state_transitions_initializer=${{transitions[$state]}}
    declare -A state_transitions
    eval "state_transitions=$state_transitions_initializer"

    local -A inverted_literals=()
    for key in "${{!literals[@]}}"; do
        local value=${{literals[$key]}}
        inverted_literals+=([$value]=$key)
    done

    local completions=()
    for literal_id in ${{!state_transitions[@]}}; do
        completions+=(${{inverted_literals[$literal_id]}})
    done
    completions=${{completions[@]}}

    COMPREPLY=($(compgen -W "$completions" -- "${{COMP_WORDS[$COMP_CWORD]}}"))
    return 0
}}

complete -F _{command} {command}
"#, starting_state = dfa.starting_state)?;
    Ok(())
}
