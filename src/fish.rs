use std::io::Write;

use crate::dfa::DFA;
use crate::regex::Inp;
use crate::{Result, StateId};
use hashbrown::HashMap;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use ustr::{Ustr, ustr};

// * array indices start at 1 in fish, not 0 (!)
// * --local limits the scope of a varible to the innermost block (!), not the function (!).  That
//   includes things like an if block.  Therefore the prevalent use of --local in this module is
//   not warranted and can be changed.
// * Unlike in other shells, scoping *is not* dynamic in fish!  It's lexical-ish!
// * Metaprogramming:
//   1) $$var_name (https://fishshell.com/docs/current/language.html#dereferencing-variables)
//   2) printf [...] | source
// * echo foo:$bar prints nothing if $bar expands to an empty string (!)

// TODO Optimization: Do not emit __complgen_match if there's just one fallback level as it's
// unnecessary in that case.

fn make_string_constant(s: &str) -> String {
    if s.is_empty() {
        return r#""""#.to_string();
    }
    if s.contains([' ', '\t', '\n', '|']) {
        format!(
            r#""{}""#,
            s.replace('\\', "\\\\")
                .replace('\"', "\\\"")
                .replace('$', "\\$")
        )
    } else {
        s.replace('\\', "\\\\")
            .replace('\"', "\\\"")
            .replace('$', "\\$")
    }
}

pub const MATCH_FN_NAME: &str = "__complgen_match";
fn write_match_fn<W: Write>(output: &mut W) -> anyhow::Result<()> {
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
    id_from_regex: &IndexSet<Ustr>,
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

    // Use dummy value as 0th element due to fish arrays starting at 1
    let descrs: IndexSet<Ustr> = std::iter::once(ustr(""))
        .chain(
            all_literals
                .iter()
                .map(|(_, _, descr)| *descr)
                .filter(|d| !d.is_empty()),
        )
        .collect();
    writeln!(buffer, r#"    set --global subword_descrs"#)?;
    for descr in &descrs {
        if descr.is_empty() {
            continue;
        }
        let id = descrs.get_index_of(descr).unwrap();
        let quoted = make_string_constant(descr);
        writeln!(buffer, r#"    set subword_descrs[{id}] {quoted}"#)?;
    }

    let descr_id_from_literal_id: IndexMap<usize, usize> = all_literals
        .iter()
        .filter_map(|(id, _, description)| descrs.get_index_of(description).map(|d| (*id, d)))
        .filter(|(_, d)| *d > 0)
        .collect();
    let descr_literal_ids = itertools::join(
        descr_id_from_literal_id
            .iter()
            .map(|(literal_id, _)| format!("{literal_id}")),
        " ",
    );
    writeln!(
        buffer,
        r#"    set --global subword_descr_literal_ids {descr_literal_ids}"#
    )?;
    let descr_ids = itertools::join(
        descr_id_from_literal_id
            .iter()
            .map(|(_, descr_id)| format!("{descr_id}")),
        " ",
    );
    writeln!(buffer, r#"    set --global subword_descr_ids {descr_ids}"#)?;

    let regexes: String = itertools::join(
        id_from_regex
            .iter()
            .map(|regex| make_string_constant(regex)),
        " ",
    );
    writeln!(buffer, r#"    set --global subword_regexes {regexes}"#)?;

    writeln!(
        buffer,
        r#"    set --global subword_literal_transitions_inputs"#
    )?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !transitions.is_empty() {
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

        let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
        if !nontail_transitions.is_empty() {
            let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                .iter()
                .map(|(regex, to)| (id_from_regex.get_index_of(regex).unwrap(), *to))
                .collect();
            let nontail_regexes: String = itertools::join(
                nontail_command_transitions
                    .iter()
                    .map(|(regex_id, _)| format!("{}", regex_id + 1)),
                " ",
            );
            writeln!(
                buffer,
                r#"    set --global subword_nontail_regexes[{}] {nontail_regexes}"#,
                state + 1,
            )?;
            let nontail_tos: String = itertools::join(
                nontail_command_transitions
                    .into_iter()
                    .map(|(_, to)| format!("{}", to + 1)),
                " ",
            );
            writeln!(
                buffer,
                r#"    set --global subword_nontail_tos[{}] {nontail_tos}"#,
                state + 1,
            )?;
        }
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

fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
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

        if set --query subword_nontail_regexes[$subword_state] && test -n $subword_nontail_regexes[$subword_state]
            set regex_ids (string split ' ' $subword_nontail_regexes[$subword_state])
            set tos (string split ' ' $subword_nontail_tos[$subword_state])

            set nontail_matched 0
            for regex_id in $regex_ids
                set regex $subword_regexes[$regex_id]
                string match --regex --quiet "^(?<match>$regex).*" -- $subword
                if test -n "$match"
                    set subword_state $tos[$regex_id]
                    set match_len (string length -- $match)
                    set char_index (math $char_index + $match_len)
                    set nontail_matched 1
                    break
                end
            end
            if test $nontail_matched -ne 0
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

    // /////////////// Completion /////////////////////////

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
                set subword_descr_index (contains --index -- "$literal_id" $subword_descr_literal_ids)
                if test -n "$subword_descr_index"
                    set --append candidates (printf '%s%s\t%s\n' $matched_prefix $subword_literals[$literal_id] $subword_descrs[$subword_descr_ids[$subword_descr_index]])
                else
                    set --append candidates (printf '%s%s\n' $matched_prefix $subword_literals[$literal_id])
                end
            end
        end

        set name subword_nontail_command_froms_level_$fallback_level
        set commands $$name
        set index (contains --index -- "$subword_state" $commands)
        if test -n "$index"
            set name subword_nontail_commands_level_$fallback_level
            set commands (string split ' ' $$name)
            set function_id $commands[$index]
            set function_name _{command}_cmd_$function_id
            set name subword_nontail_regexes_level_$fallback_level
            set rxs (string split ' ' $$name)
            set rx $subword_regexes[$rxs[$index]]
            for line in ($function_name "$COMP_WORDS[$COMP_CWORD]")
                string match --regex --quiet "^(?<match>$rx).*" -- $line
                if test -n "$match"
                    set --append candidates (printf "%s%s\n" $matched_prefix $match)
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

fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    id_from_regex: &IndexSet<Ustr>,
) -> Result<()> {
    writeln!(
        buffer,
        r#"function _{command}_subword_{id}
    set mode $argv[1]
    set word $argv[2]
"#
    )?;

    let literal_id_from_input_description =
        write_subword_lookup_tables(buffer, dfa, id_from_regex)?;
    writeln!(buffer)?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(1);

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    fallback_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input_id, _) in dfa.iter_transitions() {
        match dfa.get_input(input_id).clone() {
            Inp::Literal {
                literal: lit,
                description,
                fallback_level,
                ..
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(lit, description.unwrap_or("".into())))
                    .unwrap();
                fallback_literals[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Inp::Command {
                zsh_compadd: true, ..
            } => unreachable!(),
            Inp::Command {
                cmd,
                regex: None,
                fallback_level,
                zsh_compadd: false,
            } => {
                let command_id = id_from_cmd.get_index_of(&cmd).unwrap();
                fallback_commands[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Inp::Command {
                cmd,
                regex: Some(rx),
                fallback_level,
                zsh_compadd: false,
            } => {
                let cmd_id = id_from_cmd.get_index_of(&cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(&rx).unwrap();
                fallback_nontails[fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Inp::Star => {}
            Inp::Subword { .. } => unreachable!(),
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

    for (level, transitions) in fallback_nontails.iter().enumerate() {
        let from_initializer = transitions
            .iter()
            .map(|(from_state, _)| from_state + 1)
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_nontail_command_froms_level_{level} {from_initializer}"#
        )?;

        let commands_initializer = transitions
            .iter()
            .map(|(_, command_ids)| {
                command_ids
                    .iter()
                    .map(|(cmd_id, _)| format!("{}", cmd_id))
                    .join(" ")
            })
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_nontail_commands_level_{level} {commands_initializer}"#
        )?;

        let regexes_initializer = transitions
            .iter()
            .map(|(_, command_ids)| {
                command_ids
                    .iter()
                    .map(|(_, regex_id)| format!("{}", regex_id + 1))
                    .join(" ")
            })
            .join(" ");
        writeln!(
            buffer,
            r#"    set --global subword_nontail_regexes_level_{level} {regexes_initializer}"#
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
    id_from_regex: &IndexSet<Ustr>,
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

    // Use dummy value as 0th element due to fish arrays starting at 1
    let descrs: IndexSet<Ustr> = std::iter::once(ustr(""))
        .chain(
            all_literals
                .iter()
                .map(|(_, _, descr)| *descr)
                .filter(|d| !d.is_empty()),
        )
        .collect();
    writeln!(buffer, r#"    set descrs"#)?;
    for descr in &descrs {
        if descr.is_empty() {
            continue;
        }
        let id = descrs.get_index_of(descr).unwrap();
        let quoted = make_string_constant(descr);
        writeln!(buffer, r#"    set descrs[{id}] {quoted}"#)?;
    }

    let descr_id_from_literal_id: IndexMap<usize, usize> = all_literals
        .iter()
        .filter_map(|(id, _, description)| descrs.get_index_of(description).map(|d| (*id, d)))
        .filter(|(_, d)| *d > 0)
        .collect();
    let descr_literal_ids = itertools::join(
        descr_id_from_literal_id
            .iter()
            .map(|(literal_id, _)| format!("{literal_id}")),
        " ",
    );
    writeln!(buffer, r#"    set descr_literal_ids {descr_literal_ids}"#)?;
    let descr_ids = itertools::join(
        descr_id_from_literal_id
            .iter()
            .map(|(_, descr_id)| format!("{descr_id}")),
        " ",
    );
    writeln!(buffer, r#"    set descr_ids {descr_ids}"#)?;

    let regexes: String = itertools::join(
        id_from_regex
            .iter()
            .map(|regex| make_string_constant(regex)),
        " ",
    );
    writeln!(buffer, r#"    set regexes {regexes}"#)?;

    writeln!(buffer, r#"    set literal_transitions_inputs"#)?;
    writeln!(buffer, r#"    set nontail_transitions"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !literal_transitions.is_empty() {
            let transitions: Vec<(usize, StateId)> = literal_transitions
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
            let state_inputs: String = itertools::join(
                transitions
                    .iter()
                    .map(|(literal_id, _)| format!("{}", literal_id)),
                " ",
            );
            // TODO Optimize for output size: Emit a single assigment to literal_transitions_inputs
            // instead of many
            writeln!(
                buffer,
                r#"    set literal_transitions_inputs[{}] {}"#,
                state + 1,
                make_string_constant(&state_inputs),
            )?;

            let state_tos: String =
                itertools::join(transitions.iter().map(|(_, to)| format!("{}", to + 1)), " ");
            // TODO Optimize for output size: Emit a single assigment to literal_transitions_tos
            // instead of many
            writeln!(
                buffer,
                r#"    set literal_transitions_tos[{}] {}"#,
                state + 1,
                make_string_constant(&state_tos),
            )?;
        }

        let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
        if !nontail_transitions.is_empty() {
            let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                .into_iter()
                .map(|(regex, to)| (id_from_regex.get_index_of(&regex).unwrap(), to))
                .collect();
            let state_nontail_transitions: String = itertools::join(
                nontail_command_transitions
                    .into_iter()
                    .map(|(regex_id, to)| format!("[{regex_id}]={}", to + 1)),
                " ",
            );
            writeln!(
                buffer,
                r#"    set nontail_transitions[{}] {state_nontail_transitions}"#,
                state + 1,
            )?;
        }
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

fn make_id_from_command_map(dfa: &DFA) -> (IndexSet<Ustr>, IndexSet<Ustr>) {
    let mut id_from_cmd: IndexSet<Ustr> = Default::default();

    let mut id_from_regex: IndexSet<Ustr> = Default::default();

    for input in dfa.iter_inputs() {
        match input {
            Inp::Literal { .. } | Inp::Star => {}
            Inp::Command {
                zsh_compadd: true, ..
            } => unreachable!(),
            Inp::Command {
                cmd,
                regex,
                zsh_compadd: false,
                fallback_level: _,
            } => {
                id_from_cmd.insert(*cmd);
                if let Some(rx) = regex {
                    id_from_regex.insert(*rx);
                }
            }
            Inp::Subword {
                subdfa: subdfaid, ..
            } => {
                let subdfa = dfa.subdfas.lookup(*subdfaid);
                for input in subdfa.iter_inputs() {
                    match input {
                        Inp::Literal { .. } | Inp::Subword { .. } | Inp::Star => {}
                        Inp::Command {
                            zsh_compadd: true, ..
                        } => unreachable!(),
                        Inp::Command {
                            cmd,
                            regex,
                            zsh_compadd: false,
                            fallback_level: _,
                        } => {
                            id_from_cmd.insert(*cmd);
                            if let Some(rx) = regex {
                                id_from_regex.insert(*rx);
                            }
                        }
                    }
                }
            }
        }
    }

    (id_from_cmd, id_from_regex)
}

pub fn write_completion_script<W: Write>(
    buffer: &mut W,
    command: &str,
    dfa: &DFA,
) -> anyhow::Result<()> {
    let (id_from_cmd, id_from_regex) = make_id_from_command_map(dfa);
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
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
    for (dfaid, id) in &id_from_dfa {
        let dfa = dfa.subdfas.lookup(*dfaid);
        write_subword_fn(buffer, command, *id, dfa, &id_from_cmd, &id_from_regex)?;
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

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, &id_from_regex)?;

    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state);
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

    // ///////////////////////////// Completion ///////////////////////////////////

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(1);

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    fallback_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input_id, _) in dfa.iter_transitions() {
        match dfa.get_input(input_id).clone() {
            Inp::Literal {
                literal: lit,
                description,
                fallback_level,
                ..
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(lit, description.unwrap_or("".into())))
                    .unwrap();
                fallback_literals[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Inp::Subword {
                subdfa: dfa,
                fallback_level,
                ..
            } => {
                let subword_id = *id_from_dfa.get(&dfa).unwrap();
                fallback_subwords[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(subword_id);
            }
            Inp::Command {
                zsh_compadd: true, ..
            } => unreachable!(),
            Inp::Command {
                cmd,
                regex: None,
                fallback_level,
                zsh_compadd: false,
            } => {
                let command_id = id_from_cmd.get_index_of(&cmd).unwrap();
                fallback_commands[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Inp::Command {
                cmd,
                regex: Some(rx),
                fallback_level,
                zsh_compadd: false,
            } => {
                let cmd_id = id_from_cmd.get_index_of(&cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(&rx).unwrap();
                fallback_nontails[fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Inp::Star => {}
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
                itertools::join(
                    state_inputs
                        .iter()
                        .map(|literal_id| format!("{}", literal_id)),
                    " ",
                )
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

    for (level, transitions) in fallback_nontails.iter().enumerate() {
        let from_initializer = transitions
            .iter()
            .map(|(from_state, _)| from_state + 1)
            .join(" ");
        writeln!(
            buffer,
            r#"    set nontail_command_froms_level_{level} {from_initializer}"#
        )?;

        let commands_initializer = itertools::join(
            transitions.iter().map(|(_, state_commands)| {
                let joined_ids = itertools::join(
                    state_commands
                        .iter()
                        .map(|(cmd_id, _)| format!("{}", cmd_id)),
                    " ",
                );
                format!(r#""{}""#, joined_ids)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    set nontail_commands_level_{level} {commands_initializer}"#
        )?;

        let regexes_initializer = itertools::join(
            transitions.iter().map(|(_, state_commands)| {
                let joined_ids = itertools::join(
                    state_commands
                        .iter()
                        .map(|(_, regex_id)| format!("{}", regex_id + 1)),
                    " ",
                );
                format!(r#""{}""#, joined_ids)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    set nontail_regexes_level_{level} {regexes_initializer}"#
        )?;
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
                set descr_index (contains --index -- "$literal_id" $descr_literal_ids)
                if test -n "$descr_index"
                    set --append candidates (printf '%s\t%s\n' $literals[$literal_id] $descrs[$descr_ids[$descr_index]])
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

    if !fallback_nontails.first().unwrap().is_empty() {
        write!(
            buffer,
            r#"
        set name nontail_command_froms_level_$fallback_level
        set commands $$name
        set index (contains --index -- "$state" $commands)
        if test -n "$index"
            set name nontail_commands_level_$fallback_level
            set commands (string split ' ' $$name)
            set function_id $commands[$index]
            set function_name _{command}_cmd_$function_id
            set name nontail_regexes_level_$fallback_level
            set rxs (string split ' ' $$name)
            set rx $regexes[$rxs[$index]]
            for line in ($function_name "$COMP_WORDS[$COMP_CWORD]")
                string match --regex --quiet "^(?<match>$rx).*" -- $line
                if test -n "$match"
                    set --append candidates "$match"
                end
            end
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
