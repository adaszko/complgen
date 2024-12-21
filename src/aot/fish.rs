use std::io::Write;

use crate::dfa::DFA;
use crate::grammar::Specialization;
use crate::regex::Input;
use crate::{Result, StateId};
use hashbrown::HashMap;
use indexmap::IndexMap;
use itertools::Itertools;
use ustr::{ustr, Ustr};

use super::get_max_fallback_level;

// * array indices start at 1 in fish, not 0 (!)
// * --local limits the scope of a varible to the innermost block (!), not the function (!).  That
//   includes things like an if block.  Therefore the prevalent use of --local in this module is
//   not warranted and can be changed.
// * Unlike in other shells, scoping *is not* dynamic in fish!  It's lexical-ish!
// * Metaprogramming:
//   1) $$var_name (https://fishshell.com/docs/current/language.html#dereferencing-variables)
//   2) printf [...] | source

// TODO Optimization: Do not emit __complgen_match if there's just one fallback level as it's
// unnecessary in that case.

pub fn make_string_constant(s: &str) -> String {
    if s.is_empty() {
        return r#""""#.to_string();
    }
    if s.contains(&[' ', '\t', '\n', '|']) {
        format!(
            r#""{}""#,
            s.replace('\\', "\\\\")
                .replace('\"', "\\\"")
                .replace('$', "\\$")
        )
    } else {
        format!(
            r#"{}"#,
            s.replace('\\', "\\\\")
                .replace('\"', "\\\"")
                .replace('$', "\\$")
        )
    }
}

pub const MATCH_FN_NAME: &str = "__complgen_match";
pub fn write_match_fn<W: Write>(output: &mut W) -> anyhow::Result<()> {
    // Unzip candidates from stdin into two arrays -- candidates and descriptions
    writeln!(
        output,
        r#"function {MATCH_FN_NAME}
    set prefix $argv[1]

    set candidates
    set descriptions
    while read c
        set a (string split --max 1 -- "	" $c)
        set --append candidates $a[1]
        if set --query a[2]
            set --append descriptions $a[2]
        else
            set --append descriptions ""
        end
    end

    if test -z "$candidates"
        return 1
    end

    set escaped_prefix (string escape --style=regex -- $prefix)
    set regex "^$escaped_prefix.*"

    set matches_case_sensitive
    set descriptions_case_sensitive
    for i in (seq 1 (count $candidates))
        if string match --regex --quiet --entire -- $regex $candidates[$i]
            set --append matches_case_sensitive $candidates[$i]
            set --append descriptions_case_sensitive $descriptions[$i]
        end
    end

    if set --query matches_case_sensitive[1]
        for i in (seq 1 (count $matches_case_sensitive))
            printf '%s	%s\n' $matches_case_sensitive[$i] $descriptions_case_sensitive[$i]
        end
        return 0
    end

    set matches_case_insensitive
    set descriptions_case_insensitive
    for i in (seq 1 (count $candidates))
        if string match --regex --quiet --ignore-case --entire -- $regex $candidates[$i]
            set --append matches_case_insensitive $candidates[$i]
            set --append descriptions_case_insensitive $descriptions[$i]
        end
    end

    if set --query matches_case_insensitive[1]
        for i in (seq 1 (count $matches_case_insensitive))
            printf '%s	%s\n' $matches_case_insensitive[$i] $descriptions_case_insensitive[$i]
        end
        return 0
    end

    return 1
end
"#
    )?;

    Ok(())
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
        // TODO Initialize subword_literal_transitions_inputs as a single assigment
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_inputs[{}] {}"#,
            state + 1,
            make_string_constant(&state_inputs),
        )?;
        let state_tos: String =
            itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
        writeln!(
            buffer,
            r#"    set --global subword_literal_transitions_tos[{}] {}"#,
            state + 1,
            make_string_constant(&state_tos),
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
        r#"    set --global subword_match_anything_transitions_from {match_anything_transitions_from}"#
    )?;
    let match_anything_transitions_to = itertools::join(
        match_anything_transitions
            .iter()
            .map(|(_, to)| format!("{}", to + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    set --global subword_match_anything_transitions_to {match_anything_transitions_to}"#
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
                    set index (contains --index -- "$literal_id" $inputs)
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

        set index (contains --index -- "$subword_state" $subword_match_anything_transitions_from)
        if test -n "$index"
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

    for fallback_level in (seq 0 $subword_max_fallback_level)
        set candidates
        set froms_name subword_literal_transitions_from_level_$fallback_level
        set froms (string split ' ' $$froms_name)
        if contains $subword_state $froms
            set index (contains --index -- "$subword_state" $froms)
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
        set index (contains --index -- "$subword_state" $froms)
        if test -n "$index"
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
                make_string_constant(&literal_ids.iter().map(|id| format!("{}", id)).join(" "))
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
            .map(|(_, command_ids)| command_ids.iter().map(|id| format!("{}", id)).join(" "))
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

    writeln!(buffer)?;

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
        // TODO Optimize for output size: Emit a single assigment to literal_transitions_inputs
        // instead of many
        writeln!(
            buffer,
            r#"    set literal_transitions_inputs[{}] {}"#,
            state + 1,
            make_string_constant(&state_inputs),
        )?;
        // TODO Optimize for output size: Emit a single assigment to literal_transitions_tos
        // instead of many
        writeln!(
            buffer,
            r#"    set literal_transitions_tos[{}] {}"#,
            state + 1,
            make_string_constant(&state_tos),
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

pub fn write_completion_script<W: Write>(
    buffer: &mut W,
    command: &str,
    dfa: &DFA,
) -> anyhow::Result<()> {
    validate_command_name(command)?;

    let id_from_cmd = make_id_from_command_map(dfa);
    for (cmd, id) in &id_from_cmd {
        write!(
            buffer,
            r#"function _{command}_cmd_{id}
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

    write_match_fn(buffer)?;
    writeln!(buffer)?;

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
            r#"    set subword_transitions_ids[{}] {}"#,
            state + 1,
            make_string_constant(&subword_ids),
        )?;
        writeln!(
            buffer,
            r#"    set subword_transitions_tos[{}] {}"#,
            state + 1,
            make_string_constant(&tos),
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
            set inputs (string split ' ' $literal_transitions_inputs[$state])
            set tos (string split ' ' $literal_transitions_tos[$state])

            set literal_id (contains --index -- "$word" $literals)
            if test -n "$literal_id"
                set index (contains --index -- "$literal_id" $inputs)
                set state $tos[$index]
                set word_index (math $word_index + 1)
                continue
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
            set subword_ids (string split ' ' $subword_transitions_ids[$state])
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
            set index (contains --index -- "$state" $match_anything_transitions_from)
            set state $match_anything_transitions_to[$index]
            set word_index (math $word_index + 1)
            continue
        end

        return 1
    end
"#
    )?;

    writeln!(buffer)?;

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
        let froms_initializer = itertools::join(
            transitions
                .iter()
                .map(|(from_state, _)| format!("{}", from_state + 1)),
            " ",
        );
        writeln!(
            buffer,
            r#"    set literal_froms_level_{level} {froms_initializer}"#
        )?;

        let inputs_initializer = itertools::join(
            transitions.iter().map(|(_, state_inputs)| {
                let cell = itertools::join(
                    state_inputs
                        .iter()
                        .map(|literal_id| format!("{}", literal_id)),
                    " ",
                );
                format!(r#"{}"#, cell)
            }),
            "|",
        );
        writeln!(
            buffer,
            r#"    set literal_inputs_level_{level} {}"#,
            make_string_constant(&inputs_initializer),
        )?;
    }

    if !fallback_subwords.first().unwrap().is_empty() {
        for (level, transitions) in fallback_subwords.iter().enumerate() {
            let froms_initializer = itertools::join(
                transitions
                    .iter()
                    .map(|(from_state, _)| format!("{}", from_state + 1)),
                " ",
            );
            writeln!(
                buffer,
                r#"    set subword_froms_level_{level} {froms_initializer}"#
            )?;

            let subwords_initializer = itertools::join(
                transitions.iter().map(|(_, state_subwords)| {
                    let cell = itertools::join(
                        state_subwords
                            .iter()
                            .map(|literal_id| format!("{}", literal_id)),
                        " ",
                    );
                    format!(r#""{}""#, cell)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    set subwords_level_{level} {subwords_initializer}"#
            )?;
        }
    }

    if !fallback_commands.first().unwrap().is_empty() {
        for (level, transitions) in fallback_commands.iter().enumerate() {
            let from_initializer = transitions
                .iter()
                .map(|(from_state, _)| from_state + 1)
                .join(" ");
            writeln!(
                buffer,
                r#"    set command_froms_level_{level} {from_initializer}"#
            )?;

            let commands_initializer = itertools::join(
                transitions.iter().map(|(_, state_commands)| {
                    let cell = itertools::join(
                        state_commands
                            .iter()
                            .map(|literal_id| format!("{}", literal_id)),
                        " ",
                    );
                    format!(r#""{}""#, cell)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    set commands_level_{level} {commands_initializer}"#
            )?;
        }
    }

    write!(
        buffer,
        r#"
    set max_fallback_level {max_fallback_level}
    for fallback_level in (seq 0 {max_fallback_level})
        set candidates
        set froms_name literal_froms_level_$fallback_level
        set froms $$froms_name
        set index (contains --index -- "$state" $froms)
        if test -n "$index"
            set level_inputs_name literal_inputs_level_$fallback_level
            set input_assoc_values (string split '|' $$level_inputs_name)
            set state_inputs (string split ' ' $input_assoc_values[$index])
            for literal_id in $state_inputs
                if test -n "$descriptions[$literal_id]"
                    set --append candidates (printf '%s\t%s\n' $literals[$literal_id] $descriptions[$literal_id])
                else
                    set --append candidates (printf '%s\n' $literals[$literal_id])
                end
            end
        end
"#
    )?;

    if !fallback_subwords.first().unwrap().is_empty() {
        write!(
            buffer,
            r#"
        set subwords_name subword_froms_level_$fallback_level
        set subwords $$subwords_name
        set index (contains --index -- "$state" $subwords)
        if test -n "$index"
            set subwords_name subwords_level_$fallback_level
            set subwords (string split ' ' $$subwords_name)
            for id in $subwords
                set function_name _{command}_subword_$id
                set --append candidates ($function_name complete "$COMP_WORDS[$COMP_CWORD]")
            end
        end
"#
        )?;
    }

    if !fallback_commands.first().unwrap().is_empty() {
        write!(
            buffer,
            r#"
        set commands_name command_froms_level_$fallback_level
        set commands $$commands_name
        set index (contains --index -- "$state" $commands)
        if test -n "$index"
            set commands_name commands_level_$fallback_level
            set commands (string split ' ' $$commands_name)
            set function_id $commands[$index]
            set function_name _{command}_cmd_$function_id
            set --append candidates ($function_name "$COMP_WORDS[$COMP_CWORD]")
        end
"#
        )?;
    }

    writeln!(
        buffer,
        r#"        printf '%s\n' $candidates | {MATCH_FN_NAME} $COMP_WORDS[$word_index] && return 0
    end
end
"#
    )?;

    writeln!(buffer, r#"complete --erase {command}"#)?;
    writeln!(
        buffer,
        r#"complete --command {command} --no-files --arguments "(_{command})""#
    )?;
    Ok(())
}
