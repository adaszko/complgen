use std::io::Write;

use crate::{StateId, Result};
use hashbrown::HashMap;
use ustr::{Ustr, UstrMap, ustr};
use crate::dfa::DFA;


// array indices start at 1 in fish , not 0 (!)
// variables are block-scoped, so set --local sets a variable local to even a single if statement (!)

pub fn make_string_constant(s: &str) -> String {
    format!(r#""{}""#, s.replace('\\', "\\\\").replace('\"', "\\\"").replace('$', "\\$"))
}


fn write_lookup_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, literal, description)| ((*literal, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| make_string_constant(literal)), " ");
    writeln!(buffer, r#"    set --local literals {literals}"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    set --local descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        if description.is_empty() {
            continue;
        }
        let quoted = make_string_constant(description);
        writeln!(buffer, r#"    set descriptions[{id}] {}"#, quoted)?;
    }
    writeln!(buffer)?;

    writeln!(buffer, r#"    set --local literal_transitions"#)?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        if transitions.is_empty() {
            continue;
        }
        let state_inputs: String = itertools::join(transitions.iter().map(|(literal_id, _)| format!("{}", literal_id)), " ");
        let state_tos: String = itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        // TODO Make two arrays out of transitions: transition_inputs and transition_tos to reduce output size
        writeln!(buffer, r#"    set literal_transitions[{}] "set inputs {state_inputs}; set tos {state_tos}""#, state + 1)?;
    }

    writeln!(buffer)?;

    let match_anything_transitions = dfa.get_match_anything_transitions();
    let match_anything_transitions_from = itertools::join(match_anything_transitions.iter().map(|(from, _)| format!("{}", from + 1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_from {match_anything_transitions_from}"#)?;
    let match_anything_transitions_to = itertools::join(match_anything_transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
    writeln!(buffer, r#"    set --local match_anything_transitions_to {match_anything_transitions_to}"#)?;

    Ok(())
}


pub fn write_subword_fn<W: Write>(buffer: &mut W, command: &str, id: usize, dfa: &DFA, command_id_from_state: &HashMap<StateId, usize>, subword_spec_id_from_state: &HashMap<StateId, usize>) -> Result<()> {
    writeln!(buffer, r#"function _{command}_subword_{id}
    set mode $argv[1]
    set word $argv[2]
"#)?;

    write_lookup_tables(buffer, dfa)?;

    writeln!(buffer, r#"
    set --local state {starting_state}
    set --local char_index 1
    set --local matched 0
    while true
        if test $char_index -gt (string length -- "$word")
            set matched 1
            break
        end

        set --local subword (string sub --start=$char_index -- "$word")

        if set --query literal_transitions[$state] && test -n $literal_transitions[$state]
            set --local --erase inputs
            set --local --erase tos
            eval $literal_transitions[$state]

            set --local literal_matched 0
            for literal_id in (seq 1 (count $literals))
                set --local literal $literals[$literal_id]
                set --local literal_len (string length -- "$literal")
                set --local subword_slice (string sub --end=$literal_len -- "$subword")
                if test $subword_slice = $literal
                    set --local index (contains --index -- "$literal_id" "$inputs")
                    set state $tos[$index]
                    set char_index (math $char_index + $literal_len)
                    set literal_matched 1
                    break
                end
            end
            if test $literal_matched -ne 0
                continue
            end
        end

        if set --query match_anything_transitions_from[$state] && test -n $match_anything_transitions_from[$state]
            set --local index (contains --index -- $state $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set --local matched 1
            break
        end

        break
    end

    if test $mode = matches
        return (math 1 - $matched)
    end
"#, starting_state=dfa.starting_state + 1)?;


    write!(buffer, r#"
    set --local matched_prefix
    if test $char_index -eq 1
        set matched_prefix ""
    else
        set matched_prefix (string sub --end=(math $char_index - 1) -- "$word")
    end
    if set --query literal_transitions[$state] && test -n $literal_transitions[$state]
        set --local --erase inputs
        set --local --erase tos
        eval $literal_transitions[$state]
        for literal_id in $inputs
            if test -n $descriptions[$literal_id]
                printf '%s%s\t%s\n' $matched_prefix $literals[$literal_id] $descriptions[$literal_id]
            else
                printf '%s%s\n' $matched_prefix $literals[$literal_id]
            end
        end
    end

"#)?;


    if !command_id_from_state.is_empty() {
        writeln!(buffer, r#"    set command_states {}"#, itertools::join(command_id_from_state.iter().map(|(state, _)| state + 1), " "))?;
        write!(buffer, r#"    set command_ids {}"#, itertools::join(command_id_from_state.iter().map(|(_, id)| id), " "))?;
        write!(buffer, r#"
    if contains $state $command_states
        set --local index (contains --index $state $command_states)
        set --local function_id $command_ids[$index]
        set --local function_name _{command}_subword_cmd_$function_id
        set --local --erase inputs
        set --local --erase tos
        $function_name "$matched_prefix" | while read --local line
            printf '%s%s' $matched_prefix $line
        end
    end
"#)?;
    }

    if !subword_spec_id_from_state.is_empty() {
        writeln!(buffer, r#"    set specialized_command_states {}"#, itertools::join(subword_spec_id_from_state.iter().map(|(state, _)| state + 1), " "))?;
        write!(buffer, r#"    set specialized_command_ids {}"#, itertools::join(subword_spec_id_from_state.iter().map(|(_, id)| id), " "))?;
        write!(buffer, r#"
    if contains $state $specialized_command_states
        set --local index (contains --index $state $specialized_command_states)
        set --local function_id $specialized_command_ids[$index]
        set --local function_name _{command}_subword_spec_$function_id
        set --local --erase inputs
        set --local --erase tos
        $function_name "$matched_prefix" | while read --local line
            printf '%s%s' $matched_prefix $line
        end
    end
"#)?;
    }

    writeln!(buffer, r#"
    return 0
end
"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let top_level_command_transitions = dfa.get_command_transitions();
    let subword_command_transitions = dfa.get_subword_command_transitions();

    let id_from_top_level_command: UstrMap<usize> = top_level_command_transitions.iter().enumerate().map(|(id, (_, cmd))| (*cmd, id + 1)).collect();
    for (cmd, id) in &id_from_top_level_command {
        write!(buffer, r#"function _{command}_{id}
    set 1 $argv[1]
    {cmd}
end

"#)?;
    }

    let id_from_subword_command: UstrMap<usize> = subword_command_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (cmd, id) in &id_from_subword_command {
        write!(buffer, r#"function _{command}_subword_cmd_{id}
    {cmd}
end

"#)?;
    }

    let top_level_spec_transitions = dfa.get_fish_command_transitions();
    let subword_spec_transitions = dfa.get_fish_subword_command_transitions();

    let id_from_specialized_command: UstrMap<usize> = top_level_spec_transitions.iter().enumerate().map(|(id, (_, cmd))| (*cmd, id + 1)).collect();
    for (cmd, id) in &id_from_specialized_command {
        write!(buffer, r#"function _{command}_spec_{id}
set 1 $argv[1]
{cmd}
end

"#)?;
    }

    let id_from_subword_spec: UstrMap<usize> = subword_spec_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| {
            transitions.iter().map(move |(_, cmd)| (*cmd, id))
        })
        .collect();
    for (cmd, id) in &id_from_subword_spec {
        write!(buffer, r#"function _{command}_subword_spec_{id}
    {cmd}
end

"#)?;
    }

    let id_from_dfa = dfa.get_subwords(1);
    for (dfa, id) in &id_from_dfa {
        let transitions = subword_command_transitions.get(dfa).unwrap();
        let subword_command_id_from_state: HashMap<StateId, usize> = transitions.iter().map(|(state, cmd)| (*state, *id_from_subword_command.get(cmd).unwrap())).collect();
        let subword_spec_id_from_state: HashMap<StateId, usize> = subword_spec_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_spec.get(cmd).unwrap())).collect();
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &subword_command_id_from_state, &subword_spec_id_from_state)?;
        writeln!(buffer)?;
    }

    write!(buffer, r#"function _{command}"#)?;

    write!(buffer, r#"
    set COMP_LINE (commandline --cut-at-cursor)
"#)?;

    write!(buffer, r#"
    set COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end

"#)?;

    write_lookup_tables(buffer, dfa)?;

    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }

        let subword_ids: String = itertools::join(subword_transitions.iter().map(|(dfa, _)| format!("{}", id_from_dfa.get(dfa).unwrap())), " ");
        let tos: String = itertools::join(subword_transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        writeln!(buffer, r#"    set subword_transitions[{}] "set subword_ids {subword_ids}; set tos {tos}""#, state + 1)?;
    }

    write!(buffer, r#"
    set --local state {starting_state}
    set --local word_index 2
    while test $word_index -lt $COMP_CWORD
        set --local -- word $COMP_WORDS[$word_index]

        if set --query literal_transitions[$state] && test -n $literal_transitions[$state]
            set --local --erase inputs
            set --local --erase tos
            eval $literal_transitions[$state]

            if contains -- $word $literals
                set --local literal_matched 0
                for literal_id in (seq 1 (count $literals))
                    if test $literals[$literal_id] = $word
                        set --local index (contains --index -- $literal_id $inputs)
                        set state $tos[$index]
                        set word_index (math $word_index + 1)
                        set literal_matched 1
                        break
                    end
                end
                if test $literal_matched -ne 0
                    continue
                end
            end
        end
"#, starting_state = dfa.starting_state + 1)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
        if set --query subword_transitions[$state] && test -n $subword_transitions[$state]
            set --local --erase subword_ids
            set --local --erase tos
            eval $subword_transitions[$state]

            set --local subword_matched 0
            for subword_id in $subword_ids
                if _{command}_subword_$subword_id matches "$word"
                    set subword_matched 1
                    set state $tos[$subword_id]
                    set word_index (math $word_index + 1)
                    break
                end
            end
            if test $subword_matched -ne 0
                continue
            end
        end
"#)?;
    }

    write!(buffer, r#"
        if set --query match_anything_transitions_from[$state] && test -n $match_anything_transitions_from[$state]
            set --local index (contains --index -- $state $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set word_index (math $word_index + 1)
            continue
        end

        return 1
    end
"#)?;

    write!(buffer, r#"
    if set --query literal_transitions[$state] && test -n $literal_transitions[$state]
        set --local --erase inputs
        set --local --erase tos
        eval $literal_transitions[$state]
        for literal_id in $inputs
            if test -n $descriptions[$literal_id]
                printf '%s\t%s\n' $literals[$literal_id] $descriptions[$literal_id]
            else
                printf '%s\n' $literals[$literal_id]
            end
        end
    end

"#)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
    if set --query subword_transitions[$state] && test -n $subword_transitions[$state]
        set --local --erase subword_ids
        set --local --erase tos
        eval $subword_transitions[$state]

        for subword_id in $subword_ids
            set --local function_name _{command}_subword_$subword_id
            $function_name complete "$COMP_WORDS[$COMP_CWORD]"
        end
    end

"#)?;
    }

    let top_level_command_id_from_state: HashMap<StateId, usize> = top_level_command_transitions.into_iter().map(|(state, cmd)| (state, *id_from_top_level_command.get(&cmd).unwrap())).collect();
    if !top_level_command_id_from_state.is_empty() {
        writeln!(buffer, r#"    set command_states {}"#, itertools::join(top_level_command_id_from_state.iter().map(|(state, _)| state + 1), " "))?;
        write!(buffer, r#"    set command_ids {}"#, itertools::join(top_level_command_id_from_state.iter().map(|(_, id)| id), " "))?;
        write!(buffer, r#"
    if contains $state $command_states
        set --local index (contains --index $state $command_states)
        set --local function_id $command_ids[$index]
        set --local function_name _{command}_$function_id
        set --local --erase inputs
        set --local --erase tos
        $function_name "$COMP_WORDS[$COMP_CWORD]"
    end
"#)?;
    }

    let specialized_command_id_from_state: HashMap<StateId, usize> = top_level_spec_transitions.into_iter().map(|(state, cmd)| (state, *id_from_specialized_command.get(&cmd).unwrap())).collect();
    if !specialized_command_id_from_state.is_empty() {
        writeln!(buffer, r#"    set specialized_command_states {}"#, itertools::join(specialized_command_id_from_state.iter().map(|(state, _)| state + 1), " "))?;
        write!(buffer, r#"    set specialized_command_ids {}"#, itertools::join(specialized_command_id_from_state.iter().map(|(_, id)| id), " "))?;
        write!(buffer, r#"
    if contains $state $specialized_command_states
        set --local index (contains --index $state $specialized_command_states)
        set --local function_id $specialized_command_ids[$index]
        set --local function_name _{command}_spec_$function_id
        set --local --erase inputs
        set --local --erase tos
        set --local lines (eval $function_name $COMP_WORDS[$COMP_CWORD])
        for line in $lines
            printf '%s\n' $line
        end
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
