use std::io::Write;

use crate::dfa::DFA;
use crate::grammar::Specialization;
use crate::jit::fish::MATCH_FN_NAME;
use crate::regex::Input;
use crate::{Result, StateId};
use hashbrown::HashMap;
use indexmap::IndexMap;
use itertools::Itertools;
use ustr::{ustr, Ustr, UstrMap};

use super::get_max_fallback_level;

// * array indices start at 1 in fish, not 0 (!)
// * --local limits the scope of a varible to the innermost block (!), not the function (!).  That
//   includes things like an if block.  Therefore the prevalent use of --local in this module is
//   not warranted and can be changed.
// * Unlike in other shells, scoping *is not* dynamic in fish!  It's lexical-ish!
// * There's
//   [Dereferencing](https://fishshell.com/docs/current/language.html#dereferencing-variables) but
//   eval is still more useful due to weird binding order in expressions like $$array_name[$index]

pub fn make_string_constant(s: &str) -> String {
    format!(
        r#""{}""#,
        s.replace('\\', "\\\\")
            .replace('\"', "\\\"")
            .replace('$', "\\$")
    )
}

fn write_subword_lookup_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
) -> Result<HashMap<(Ustr, Ustr), usize>> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa
        .get_all_literals()
        .into_iter()
        .enumerate()
        .map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr(""))))
        .collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals
        .iter()
        .map(|(id, literal, description)| ((*literal, *description), *id))
        .collect();
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        " ",
    );
    writeln!(buffer, r#"    set --global subword_literals {literals}"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    set --global subword_descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        if description.is_empty() {
            continue;
        }
        let quoted = make_string_constant(description);
        writeln!(
            buffer,
            r#"    set --global subword_descriptions[{id}] {}"#,
            quoted
        )?;
    }
    writeln!(buffer)?;

    writeln!(
        buffer,
        r#"    set --global subword_literal_transitions_inputs"#
    )?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions
            .into_iter()
            .map(|(input, description, to)| {
                (
                    *literal_id_from_input_description
                        .get(&(input, description))
                        .unwrap(),
                    to,
                )
            })
            .collect();
        if transitions.is_empty() {
            continue;
        }
        let state_inputs: String = itertools::join(
            transitions
                .iter()
                .map(|(literal_id, _)| format!("{}", literal_id)),
            " ",
        );
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_inputs[{}] "{state_inputs}""#,
            state + 1
        )?;
        let state_tos: String =
            itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_tos[{}] "{state_tos}""#,
            state + 1
        )?;
    }

    writeln!(buffer)?;

    let match_anything_transitions: Vec<(StateId, StateId)> =
        dfa.iter_match_anything_transitions().collect();
    let match_anything_transitions_from = itertools::join(
        match_anything_transitions
            .iter()
            .map(|(from, _)| format!("{}", from + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    set --global match_anything_transitions_from {match_anything_transitions_from}"#
    )?;
    let match_anything_transitions_to = itertools::join(
        match_anything_transitions
            .iter()
            .map(|(_, to)| format!("{}", to + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    set --global match_anything_transitions_to {match_anything_transitions_to}"#
    )?;

    Ok(literal_id_from_input_description)
}

pub fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
    write!(
        buffer,
        r#"function _{command}_subword
    set mode $argv[1]
    set word $argv[2]
"#
    )?;

    write!(
        buffer,
        r#"
    set char_index 1
    set matched 0
    while true
        if test $char_index -gt (string length -- "$word")
            set matched 1
            break
        end

        set subword (string sub --start=$char_index -- "$word")

        if set --query subword_literal_transitions_inputs[$subword_state] && test -n $subword_literal_transitions_inputs[$subword_state]
            set inputs (string split ' ' $subword_literal_transitions_inputs[$subword_state])
            set tos (string split ' ' $subword_literal_transitions_tos[$subword_state])

            set literal_matched 0
            for literal_id in (seq 1 (count $subword_literals))
                set literal $subword_literals[$literal_id]
                set literal_len (string length -- "$literal")
                set subword_slice (string sub --end=$literal_len -- "$subword")
                if test $subword_slice = $literal
                    set index (contains --index -- $literal_id $inputs)
                    set subword_state $tos[$index]
                    set char_index (math $char_index + $literal_len)
                    set literal_matched 1
                    break
                end
            end
            if test $literal_matched -ne 0
                continue
            end
        end

        if set --query subword_match_anything_transitions_from[$subword_state] && test -n $subword_match_anything_transitions_from[$subword_state]
            set index (contains --index -- $subword_state $subword_match_anything_transitions_from)
            set subword_state $subword_match_anything_transitions_to[$index]
            set matched 1
            break
        end

        break
    end

    if test $mode = matches
        return (math 1 - $matched)
    end
"#
    )?;

    ////////////////// Completion /////////////////////////

    write!(
        buffer,
        r#"
    set unmatched_suffix (string sub --start=$char_index -- $word)

    set matched_prefix
    if test $char_index -eq 1
        set matched_prefix ""
    else
        set matched_prefix (string sub --end=(math $char_index - 1) -- "$word")
    end

    set --local candidates
    for fallback_level in (seq 0 $subword_max_fallback_level)
        set froms_name subword_literal_transitions_from_level_$fallback_level
        set froms (string split ' ' $$froms_name)
        if contains $subword_state $froms
            set index (contains --index $subword_state $froms)
            set transitions_name subword_literal_transitions_level_$fallback_level
            printf 'set transitions (string split \' \' $%s[%d])' $transitions_name $index | source
            for literal_id in $transitions
                set unmatched_suffix_len (string length -- $unmatched_suffix)
                if test $unmatched_suffix_len -gt 0
                    set literal $subword_literals[$literal_id]
                    set slice (string sub --end=$unmatched_suffix_len -- $literal)
                    if test "$slice" != "$unmatched_suffix"
                        continue
                    end
                end
                if test -n "$subword_descriptions[$literal_id]"
                    set --append candidates (printf '%s%s\t%s\n' $matched_prefix $subword_literals[$literal_id] $subword_descriptions[$literal_id])
                else
                    set --append candidates (printf '%s%s\n' $matched_prefix $subword_literals[$literal_id])
                end
            end
        end

        set froms_name subword_commands_from_level_$fallback_level
        set froms (string split ' ' $$froms_name)
        if contains $subword_state $froms
            set index (contains --index $subword_state $froms)
            printf 'set function_id $subword_commands_level_%s[%d]' $fallback_level $index | source
            set function_name _{command}_cmd_$function_id
            $function_name "$matched_prefix" | while read line
                set --append candidates (printf '%s%s\n' $matched_prefix $line)
            end
        end

        printf '%s\n' $candidates | {MATCH_FN_NAME} && break
    end
end
"#
    )?;

    Ok(())
}

pub fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexMap<Ustr, usize>,
) -> Result<()> {
    writeln!(
        buffer,
        r#"function _{command}_subword_{id}
    set mode $argv[1]
    set word $argv[2]
"#
    )?;

    let literal_id_from_input_description = write_subword_lookup_tables(buffer, dfa)?;
    writeln!(buffer)?;

    let max_fallback_level = get_max_fallback_level(dfa).unwrap();

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    for (from, input, _) in dfa.iter_transitions() {
        match input {
            Input::Literal(lit, descr, fallback_level) => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*lit, (*descr).unwrap_or("".into())))
                    .unwrap();
                fallback_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Input::Command(cmd, fallback_level) => {
                let command_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Nonterminal(
                _,
                Some(Specialization {
                    fish: Some(cmd), ..
                }),
                fallback_level,
            ) => {
                let specialized_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(specialized_id);
            }
            _ => (),
        }
    }

    for (level, transitions) in fallback_literals.iter().enumerate() {
        let from_initializer = transitions
            .iter()
            .map(|(from_state, _)| from_state + 1)
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_from_level_{level} {from_initializer}"#
        )?;

        let literals_initializer = transitions
            .iter()
            .map(|(_, literal_ids)| {
                format!(
                    r#""{}""#,
                    literal_ids.iter().map(|id| format!("{}", id)).join(" ")
                )
            })
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_level_{level} {literals_initializer}"#
        )?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let from_initializer = transitions
            .iter()
            .map(|(from_state, _)| from_state + 1)
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_commands_from_level_{level} {from_initializer}"#
        )?;

        let commands_initializer = transitions
            .iter()
            .map(|(_, command_ids)| {
                format!(
                    r#""{}""#,
                    command_ids.iter().map(|id| format!("{}", id)).join(" ")
                )
            })
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_commands_level_{level} {commands_initializer}"#
        )?;
    }

    writeln!(
        buffer,
        r#"    set --global subword_max_fallback_level {max_fallback_level}"#
    )?;

    writeln!(
        buffer,
        r#"    set --global subword_state {}"#,
        dfa.starting_state + 1
    )?;

    writeln!(buffer, r#"    _{command}_subword "$mode" "$word""#)?;

    writeln!(buffer, r#"end"#)?;
    Ok(())
}

fn write_lookup_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
) -> Result<HashMap<(Ustr, Ustr), usize>> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa
        .get_all_literals()
        .into_iter()
        .enumerate()
        .map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr(""))))
        .collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals
        .iter()
        .map(|(id, literal, description)| ((*literal, *description), *id))
        .collect();
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        " ",
    );
    writeln!(buffer, r#"    set literals {literals}"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    set descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        if description.is_empty() {
            continue;
        }
        let quoted = make_string_constant(description);
        writeln!(buffer, r#"    set descriptions[{id}] {}"#, quoted)?;
    }
    writeln!(buffer)?;

    writeln!(buffer, r#"    set literal_transitions_inputs"#)?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions
            .into_iter()
            .map(|(input, description, to)| {
                (
                    *literal_id_from_input_description
                        .get(&(input, description))
                        .unwrap(),
                    to,
                )
            })
            .collect();
        if transitions.is_empty() {
            continue;
        }
        let state_inputs: String = itertools::join(
            transitions
                .iter()
                .map(|(literal_id, _)| format!("{}", literal_id)),
            " ",
        );
        let state_tos: String =
            itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        writeln!(
            buffer,
            r#"    set literal_transitions_inputs[{}] "{state_inputs}""#,
            state + 1
        )?;
        writeln!(
            buffer,
            r#"    set literal_transitions_tos[{}] "{state_tos}""#,
            state + 1
        )?;
    }

    writeln!(buffer)?;

    let match_anything_transitions: Vec<(StateId, StateId)> =
        dfa.iter_match_anything_transitions().collect();
    let match_anything_transitions_from = itertools::join(
        match_anything_transitions
            .iter()
            .map(|(from, _)| format!("{}", from + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    set match_anything_transitions_from {match_anything_transitions_from}"#
    )?;
    let match_anything_transitions_to = itertools::join(
        match_anything_transitions
            .iter()
            .map(|(_, to)| format!("{}", to + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    set match_anything_transitions_to {match_anything_transitions_to}"#
    )?;

    Ok(literal_id_from_input_description)
}

pub fn make_id_from_command_map(dfa: &DFA) -> IndexMap<Ustr, usize> {
    let mut result: IndexMap<Ustr, usize> = Default::default();

    let mut unallocated_id = 0;
    for cmd in dfa.iter_command_transitions().map(|(_, cmd)| cmd) {
        result.entry(cmd).or_insert_with(|| {
            let id = unallocated_id;
            unallocated_id += 1;
            id
        });
    }

    for cmd in dfa.iter_subword_command_transitions() {
        result.entry(cmd).or_insert_with(|| {
            let id = unallocated_id;
            unallocated_id += 1;
            id
        });
    }

    for cmd in dfa.iter_fish_command_transitions() {
        result.entry(cmd).or_insert_with(|| {
            let id = unallocated_id;
            unallocated_id += 1;
            id
        });
    }

    for cmd in dfa.get_fish_subword_command_transitions() {
        result.entry(cmd).or_insert_with(|| {
            let id = unallocated_id;
            unallocated_id += 1;
            id
        });
    }

    result
}

pub fn validate_command_name(command: &str) -> crate::Result<()> {
    if command.contains('/') {
        return Err(crate::Error::InvalidCommandName(command.to_owned()));
    }
    Ok(())
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    validate_command_name(command)?;

    let id_from_cmd = make_id_from_command_map(dfa);
    for (cmd, id) in &id_from_cmd {
        write!(
            buffer,
            r#"function _{command}_{id}
    set 1 $argv[1]
    {cmd}
end

"#
        )?;
    }

    let top_level_command_transitions: Vec<(StateId, Ustr)> =
        dfa.iter_command_transitions().collect();
    let subword_command_transitions = dfa.get_subword_command_transitions();

    let id_from_top_level_command: UstrMap<usize> = top_level_command_transitions
        .iter()
        .enumerate()
        .map(|(id, (_, cmd))| (*cmd, id + 1))
        .collect();
    for (cmd, id) in &id_from_top_level_command {
        write!(
            buffer,
            r#"function _{command}_{id}
    set 1 $argv[1]
    {cmd}
end

"#
        )?;
    }

    let id_from_subword_command: UstrMap<usize> = subword_command_transitions
        .iter()
        .enumerate()
        .flat_map(|(id, (_, transitions))| transitions.iter().map(move |(_, cmd)| (*cmd, id)))
        .collect();
    for (cmd, id) in &id_from_subword_command {
        write!(
            buffer,
            r#"function _{command}_subword_cmd_{id}
    {cmd}
end

"#
        )?;
    }

    let top_level_spec_transitions = dfa.get_fish_command_transitions();

    let id_from_specialized_command: UstrMap<usize> = top_level_spec_transitions
        .iter()
        .enumerate()
        .map(|(id, (_, cmd))| (*cmd, id + 1))
        .collect();
    for (cmd, id) in &id_from_specialized_command {
        write!(
            buffer,
            r#"function _{command}_spec_{id}
set 1 $argv[1]
{cmd}
end

"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(1);
    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(buffer, command)?;
        writeln!(buffer)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &id_from_cmd)?;
        writeln!(buffer)?;
        writeln!(buffer)?;
    }

    write!(buffer, r#"function _{command}"#)?;

    write!(
        buffer,
        r#"
    set COMP_LINE (commandline --cut-at-cursor)
"#
    )?;

    write!(
        buffer,
        r#"
    set COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set COMP_CWORD (count $COMP_WORDS)
    end

"#
    )?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa)?;

    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }

        let subword_ids: String = itertools::join(
            subword_transitions
                .iter()
                .map(|(dfa, _)| format!("{}", id_from_dfa.get(dfa).unwrap())),
            " ",
        );
        let tos: String = itertools::join(
            subword_transitions
                .iter()
                .map(|(_, to)| format!("{}", to + 1)),
            " ",
        );
        writeln!(
            buffer,
            r#"    set subword_transitions_ids[{}] "{subword_ids}""#,
            state + 1
        )?;
        writeln!(
            buffer,
            r#"    set subword_transitions_tos[{}] "{tos}""#,
            state + 1
        )?;
    }

    write!(
        buffer,
        r#"
    set state {starting_state}
    set word_index 2
    while test $word_index -lt $COMP_CWORD
        set -- word $COMP_WORDS[$word_index]

        if set --query literal_transitions_inputs[$state] && test -n $literal_transitions_inputs[$state]
            set inputs $literal_transitions_inputs[$state]
            set tos $literal_transitions_tos[$state]

            if contains -- $word $literals
                set literal_matched 0
                for literal_id in (seq 1 (count $literals))
                    if test $literals[$literal_id] = $word
                        set index (contains --index -- $literal_id $inputs)
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
"#,
        starting_state = dfa.starting_state + 1
    )?;

    if dfa.has_subword_transitions() {
        write!(
            buffer,
            r#"
        if set --query subword_transitions_ids[$state] && test -n $subword_transitions_ids[$state]
            set subword_ids $subword_transitions_ids[$state]
            set tos $subword_transitions_tos[$state]

            set subword_matched 0
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
"#
        )?;
    }

    write!(
        buffer,
        r#"
        if set --query match_anything_transitions_from[$state] && test -n $match_anything_transitions_from[$state]
            set index (contains --index -- $state $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set word_index (math $word_index + 1)
            continue
        end

        return 1
    end
"#
    )?;

    //////////////////////////////// Completion ///////////////////////////////////

    let max_fallback_level = get_max_fallback_level(dfa).unwrap();

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    for (from, input, _) in dfa.iter_transitions() {
        match input {
            Input::Literal(lit, descr, fallback_level) => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*lit, (*descr).unwrap_or("".into())))
                    .unwrap();
                fallback_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Input::Subword(dfa, fallback_level) => {
                let subword_id = *id_from_dfa.get(dfa).unwrap();
                fallback_subwords[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(subword_id);
            }
            Input::Command(cmd, fallback_level) => {
                let command_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Nonterminal(
                _,
                Some(Specialization {
                    fish: Some(cmd), ..
                }),
                fallback_level,
            ) => {
                let specialized_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(specialized_id);
            }
            _ => (),
        }
    }

    for (level, transitions) in fallback_literals.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, literal_ids)| {
                let joined_literal_ids = itertools::join(literal_ids, " ");
                format!(
                    r#"[{from_state_fish}]="{joined_literal_ids}""#,
                    from_state_fish = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    set literal_transitions_level_{level} {initializer}"#
        )?;
    }

    for (level, transitions) in fallback_subwords.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, subword_ids)| {
                let joined_subword_ids = itertools::join(subword_ids, " ");
                format!(
                    r#"[{from_state_fish}]="{joined_subword_ids}""#,
                    from_state_fish = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    set subword_transitions_level_{level} {initializer}"#
        )?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_command_ids = itertools::join(command_ids, " ");
                format!(
                    r#"[{from_state_fish}]="{joined_command_ids}""#,
                    from_state_fish = from_state + 1
                )
            }),
            " ",
        );
        writeln!(buffer, r#"    set commands_level_{level} {initializer}"#)?;
    }

    write!(
        buffer,
        r#"
    set max_fallback_level {max_fallback_level}
    for fallback_level in (seq 0 {max_fallback_level})
        set name literal_transitions_level_$fallback_level
        set transitions (string split ' ' $$name[$state])
        if set --query literal_transitions_inputs[$state] && test -n $literal_transitions_inputs[$state]
            set inputs $literal_transitions_inputs[$state]
            set tos $literal_transitions_tos[$state]
            for literal_id in $inputs
                if test -n $descriptions[$literal_id]
                    printf '%s\t%s\n' $literals[$literal_id] $descriptions[$literal_id]
                else
                    printf '%s\n' $literals[$literal_id]
                end
            end
        end

        if set --query subword_transitions_ids[$state] && test -n $subword_transitions_ids[$state]
            set subword_ids $subword_transitions_ids[$state]

            for subword_id in $subword_ids
                set function_name _{command}_subword_$subword_id
                $function_name complete "$COMP_WORDS[$COMP_CWORD]"
            end
        end

        if contains $state $command_states
            set index (contains --index $state $command_states)
            set function_id $command_ids[$index]
            set function_name _{command}_$function_id
            $function_name "$COMP_WORDS[$COMP_CWORD]"
        end
    end
"#
    )?;

    writeln!(
        buffer,
        "    printf '%s\n' $candidates | {MATCH_FN_NAME} && return 0"
    )?;

    write!(
        buffer,
        r#"
end
"#
    )?;

    write!(
        buffer,
        r#"complete --command {command} --no-files --arguments "(_{command})""#
    )?;
    Ok(())
}
