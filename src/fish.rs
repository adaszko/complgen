use std::fmt::Write;

use complgen::{StateId, Result};
use hashbrown::HashMap;
use ustr::{Ustr, UstrMap};
use crate::dfa::DFA;


// array indices start at 1 in fish , not 0 (!)


fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let id_from_input: UstrMap<usize> = dfa.get_all_literals().into_iter().enumerate().map(|(id, ustr)| (ustr, id + 1)).collect();
    let literals: String = {
        let mut literals: Vec<(Ustr, usize)> = id_from_input.iter().map(|(s, id)| (*s, *id)).collect();
        literals.sort_unstable_by_key(|(_, id)| *id);
        itertools::join(literals.into_iter().map(|(s, _)| s), " ")
    };
    writeln!(buffer, r#"    set --local literals {literals}"#)?;
    writeln!(buffer, "")?;

    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, to)| (*id_from_input.get(&input).unwrap(), to)).collect();
        if transitions.is_empty() {
            continue;
        }
        let state_inputs: String = itertools::join(transitions.iter().map(|(literal_id, _)| format!("{}", literal_id)), " ");
        let state_tos: String = itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        // TODO Make two arrays out of transitions: transition_inputs and transition_tos to reduce output size
        writeln!(buffer, r#"    set transitions[{}] "set inputs {state_inputs}; set tos {state_tos}""#, state+1)?;
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
    let id_from_command: UstrMap<usize> = dfa.get_command_transitions().into_iter().enumerate().map(|(id, (_, cmd))| (cmd, id)).collect();
    for (cmd, id) in &id_from_command {
        write!(buffer, r#"function _{command}_{id}
    {cmd}
end

"#)?;
    }

    write!(buffer, r#"function _{command}
    set COMP_LINE (commandline --cut-at-cursor)
    set COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end

"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    set --local state {starting_state}
    set --local word_index 2
    while test $word_index -lt $COMP_CWORD
        set --query transitions[$state] || return 1

        set --local -- word $COMP_WORDS[$word_index]
        if not contains -- $word $literals
            return 1
        end
        set --local literal_id (contains --index -- $word $literals)

        set --local --erase inputs
        set --local --erase tos
        eval $transitions[$state]

        if contains -- $literal_id $inputs
            set --local index (contains --index -- $literal_id $inputs)
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

"#, starting_state = dfa.starting_state + 1)?;

    let command_id_from_state: HashMap<StateId, usize> = dfa.get_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_command.get(&cmd).unwrap())).collect();
    let command_states = itertools::join(command_id_from_state.iter().map(|(state, _)| state + 1), " ");
    writeln!(buffer, r#"    set command_states {command_states}"#)?;
    let command_ids = itertools::join(command_id_from_state.into_iter().map(|(_, id)| id), " ");
    writeln!(buffer, r#"    set command_ids {command_ids}"#)?;

    write!(buffer, r#"
    set --local --erase inputs
    set --local --erase tos
    eval $transitions[$state]
    for literal_id in $inputs
        printf '%s\n' $literals[$literal_id]
    end
    if contains $state $command_states
        set --local command_index (contains --index $state $command_states)
        set --local function_id $command_ids[$command_index]
        set --local function_name _{command}_$function_id
        set --local lines (eval $function_name)
        for line in $lines
            printf '%s\n' $line
        end
    end
    return 0
"#)?;

    write!(buffer, r#"
end

complete --command {command} --no-files --arguments "(_{command})"
"#)?;

    Ok(())
}
