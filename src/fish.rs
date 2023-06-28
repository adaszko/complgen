use std::io::Write;

use complgen::{StateId, Result};
use hashbrown::HashMap;
use ustr::{Ustr, UstrMap};
use crate::dfa::DFA;


// array indices start at 1 in fish , not 0 (!)


fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Option<Ustr>)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id + 1, literal, description)).collect();

    // Beware of duplicated literals: e.g. -q and -q
    let literal_id_from_input: UstrMap<usize> = all_literals.iter().map(|(id, literal, _)| (*literal, *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| literal), " ");
    writeln!(buffer, r#"    set --local literals {literals}"#)?;
    writeln!(buffer, "")?;

    let literal_id_from_description: UstrMap<usize> = all_literals.iter().filter_map(|(id, _, description)| description.map(|description| (description, *id))).collect();
    for (description, id) in literal_id_from_description {
        let description = description.replace("\"", "\\\"");
        writeln!(buffer, r#"    set descriptions[{id}] "{description}""#)?;
    }
    writeln!(buffer, "")?;

    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, to)| (*literal_id_from_input.get(&input).unwrap(), to)).collect();
        if transitions.is_empty() {
            continue;
        }
        let state_inputs: String = itertools::join(transitions.iter().map(|(literal_id, _)| format!("{}", literal_id)), " ");
        let state_tos: String = itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        // TODO Make two arrays out of transitions: transition_inputs and transition_tos to reduce output size
        writeln!(buffer, r#"    set transitions[{}] "set inputs {state_inputs}; set tos {state_tos}""#, state + 1)?;
    }

    writeln!(buffer, "")?;

    let match_anything_transitions = dfa.get_match_anything_transitions();
    let match_anything_transitions_from = itertools::join(match_anything_transitions.iter().map(|(from, _)| format!("{}", from + 1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_from {match_anything_transitions_from}"#)?;
    let match_anything_transitions_to = itertools::join(match_anything_transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_to {match_anything_transitions_to}"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA, test_mode: bool) -> Result<()> {
    let command_transitions: Vec<(usize, StateId, Ustr)> = dfa.get_command_transitions().into_iter().enumerate().map(|(id, (from, input))| (id + 1, from, input)).collect();

    // We can't identify commands by state ids because we're deduplicating them
    let mut id_from_command: UstrMap<usize> = Default::default();
    let mut id_from_state: HashMap<StateId, usize> = Default::default();
    for (id, state, command) in &command_transitions {
        if let Some(canonical_id) = id_from_command.get(command) {
            id_from_state.insert(*state, *canonical_id);
        }
        else {
            id_from_command.insert(*command, *id);
            id_from_state.insert(*state, *id);
        }
    }

    for (cmd, id) in &id_from_command {
        write!(buffer, r#"function _{command}_{id}
    set 1 $argv[1]
    {cmd}
end

"#)?;
    }

    write!(buffer, r#"function _{command}"#)?;

    if test_mode {
        write!(buffer, r#"
    set COMP_LINE $argv[1]
"#)?;
    } else {
        write!(buffer, r#"
    set COMP_LINE (commandline --cut-at-cursor)
"#)?;
    }

    write!(buffer, r#"
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
        if set --query transitions[$state] && test -n $transitions[$state]
            set --local --erase inputs
            set --local --erase tos
            eval $transitions[$state]

            set --local -- word $COMP_WORDS[$word_index]
            if contains -- $word $literals
                set --local literal_id (contains --index -- $word $literals)
                if contains -- $literal_id $inputs
                    set --local index (contains --index -- $literal_id $inputs)
                    set state $tos[$index]
                    set word_index (math $word_index + 1)
                    continue
                end
            end
        end

        if test -n $match_anything_transitions_from[$state]
            set --local index (contains --index -- $state $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set word_index (math $word_index + 1)
            continue
        end

        return 1
    end
"#, starting_state = dfa.starting_state + 1)?;

    write!(buffer, r#"
    if set --query transitions[$state] && test -n $transitions[$state]
        set --local --erase inputs
        set --local --erase tos
        eval $transitions[$state]
        for literal_id in $inputs
            if test -n $descriptions[$literal_id]
                printf '%s\t%s\n' $literals[$literal_id] $descriptions[$literal_id]
            else
                printf '%s\n' $literals[$literal_id]
            end
        end
    end

"#)?;

    if !command_transitions.is_empty() {
        writeln!(buffer, r#"    set command_states {}"#, itertools::join(id_from_state.iter().map(|(state, _)| state + 1), " "))?;
        write!(buffer, r#"    set command_ids {}"#, itertools::join(id_from_state.iter().map(|(_, id)| id), " "))?;
        write!(buffer, r#"
    if contains $state $command_states
        set --local index (contains --index $state $command_states)
        set --local function_id $command_ids[$index]
        set --local function_name _{command}_$function_id
        set --local --erase inputs
        set --local --erase tos
        set --local lines (eval $function_name $COMP_WORDS[$COMP_CWORD])
        for line in $lines
            printf '%s\n' $line
        end
    end
"#)?;
    }

    let file_states = dfa.get_file_states();
    if !file_states.is_empty() {
        let file_states_array_initializer: String = itertools::join(file_states.into_iter().map(|state| format!("{}", state + 1)), " ");
        write!(buffer, r#"
    set files {file_states_array_initializer}
    if contains $state $files
        __fish_complete_path $COMP_WORDS[$COMP_CWORD]
    end
"#)?;
    }

    write!(buffer, r#"
    return 0
end

complete --command {command} --no-files --arguments "(_{command})"
"#)?;

    Ok(())
}
