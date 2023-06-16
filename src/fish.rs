use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;


fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    for state in dfa.get_all_states() {
        let map = match dfa.transitions.get(&StateId::try_from(state).unwrap()) {
            Some(map) => map,
            None => continue,
        };
        let transitions: Vec<(crate::regex::Input, StateId)> = map.iter().filter(|(input, _)| !input.matches_anything()).map(|(input, state)| (*input, *state)).collect();
        if transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("set -a inputs {}; set -a tos {};", input, to+1)), " ");
        writeln!(buffer, r#"    set transitions[{}] "{state_transitions}""#, state+1)?;
    }

    writeln!(buffer, "")?;

    let match_anything_transitions = dfa.get_match_anything_transitions();
    let match_anything_transitions_from = itertools::join(match_anything_transitions.iter().map(|(from, _)| format!("{}", from+1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_from {match_anything_transitions_from}"#)?;
    let match_anything_transitions_to = itertools::join(match_anything_transitions.iter().map(|(_, to)| format!("{}", to+1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_to {match_anything_transitions_to}"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    write!(buffer, r#"
function _{command}
    set COMP_LINE (commandline --cut-at-cursor)
    set COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end
"#, command = command)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    set --local state {starting_state}
    set --local word_index 2
    while test $word_index -lt $COMP_CWORD
        set --query transitions[$state] || return 1
        set --local --erase inputs
        set --local --erase tos
        eval $transitions[$state]
        set --local -- word $COMP_WORDS[$word_index]
        if contains -- $word $inputs
            set --local index (contains --index -- $word $inputs)
            set state $tos[$index]
            set word_index (math $word_index + 1)
            continue
        end
        if set --query match_anything_transitions_from[$state]
            set --local index (contains --index -- $state $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set word_index (math $word_index + 1)
            continue
        end
        return 1
    end

    set --query transitions[$state] || return 1
    set --local --erase inputs
    set --local --erase tos
    eval $transitions[$state]
    printf '%s\n' $inputs
    return 0
"#, starting_state = dfa.starting_state + 1)?;

    write!(buffer, r#"
end

complete --command {command} --no-files --arguments "(_{command})"
"#, command = command)?;

    Ok(())
}
