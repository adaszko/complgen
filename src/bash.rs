use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;
use crate::nfa::Input;


// `transitions`: an associative array where:
//      key: state number
//      value: a string that is an initializer of an associative array, e.g. "( [add]=23 [obliterate]=1080 [repair]=1543 )"
//  `asterisk_transitions`: an associative array where:
//      key: state number
//      value: state number
// An entry in the `asterisk_transitions` array indicates that there's a fallback transition that
// accepts any word
fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    writeln!(buffer, r#"    declare -A transitions"#)?;
    for state in dfa.get_all_states() {
        let mut transitions: Vec<(Input, StateId)> = dfa.get_transitions_from(state).into_iter().filter(|(input, _)| !input.is_any()).collect();
        if transitions.is_empty() {
            continue;
        }
        transitions.sort_by_key(|(input, _)| input.clone());
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
        if [[ -v "state_transitions[$word]" ]]; then
            state=${{state_transitions[$word]}}
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
    local completions=${{!state_transitions[@]}}
    COMPREPLY=($(compgen -W "$completions" -- "${{COMP_WORDS[$COMP_CWORD]}}"))
    return 0
}}

complete -F _{command} {command}
"#, starting_state = dfa.starting_state)?;
    Ok(())
}
