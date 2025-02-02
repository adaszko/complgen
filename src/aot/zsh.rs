use std::io::Write;

use crate::dfa::DFA;
use crate::grammar::{Shell, Specialization};
use crate::regex::Input;
use crate::{Result, StateId};
use hashbrown::HashMap;
use indexmap::IndexMap;
use ustr::{ustr, Ustr};

use super::get_max_fallback_level;

// Array indexes in ZSH start from 1 (!)
// `for i in {{1..$#array}}; do ...; done` loops do not behave well if array is empty!  Prefer i++ loops instead.

// TODO DO not produce quotes if not necessary to save space
pub fn make_string_constant(s: &str) -> String {
    format!(
        r#""{}""#,
        s.replace('\\', "\\\\")
            .replace('\"', "\\\"")
            .replace('`', "\\`")
            .replace('$', "\\$")
    )
}

fn write_lookup_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
    prefix: &str,
) -> Result<HashMap<(Ustr, Ustr), usize>> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa
        .get_all_literals()
        .into_iter()
        .enumerate()
        .map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr(""))))
        .collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals
        .iter()
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        " ",
    );
    writeln!(buffer, r#"    local -a {prefix}literals=({literals})"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A {prefix}descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        if description.is_empty() {
            continue;
        }
        let quoted = make_string_constant(description);
        writeln!(buffer, r#"    {prefix}descriptions[{id}]={quoted}"#)?;
    }
    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A {prefix}literal_transitions"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if literal_transitions.is_empty() {
            continue;
        }
        let literal_transitions: Vec<(usize, StateId)> = literal_transitions
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
        let state_transitions: String = itertools::join(
            literal_transitions
                .into_iter()
                .map(|(input, to)| format!("[{}]={}", input, to + 1)),
            " ",
        );
        writeln!(
            buffer,
            r#"    {prefix}literal_transitions[{}]="({state_transitions})""#,
            state + 1
        )?;
    }

    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A {prefix}match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(
        dfa.iter_match_anything_transitions(Shell::Zsh)
            .into_iter()
            .map(|(from, to)| format!("[{}]={}", from + 1, to + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    {prefix}match_anything_transitions=({match_anything_transitions})"#
    )?;

    Ok(literal_id_from_input_description)
}

pub fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
    writeln!(
        buffer,
        r#"_{command}_subword () {{
    local mode=$1
    local word=$2
"#
    )?;

    write!(
        buffer,
        r#"
    local char_index=0
    local matched=0
    while true; do
        if [[ $char_index -ge ${{#word}} ]]; then
            matched=1
            break
        fi

        local subword=${{word:$char_index}}

        if [[ -v "subword_literal_transitions[$subword_state]" ]]; then
            declare -A state_transitions
            eval "state_transitions=${{subword_literal_transitions[$subword_state]}}"

            local literal_matched=0
            for ((literal_id = 1; literal_id <= $#subword_literals; literal_id++)); do
                local literal=${{subword_literals[$literal_id]}}
                local literal_len=${{#literal}}
                if [[ ${{subword:0:$literal_len}} = "$literal" ]]; then
                    if [[ -v "state_transitions[$literal_id]" ]]; then
                        subword_state=${{state_transitions[$literal_id]}}
                        char_index=$((char_index + literal_len))
                        literal_matched=1
                    fi
                fi
            done
            if [[ $literal_matched -ne 0 ]]; then
                continue
            fi
        fi

        if [[ -v "subword_match_anything_transitions[$subword_state]" ]]; then
            subword_state=${{subword_match_anything_transitions[$subword_state]}}
            matched=1
            break
        fi

        break
    done

    if [[ $mode = matches ]]; then
        return $((1 - matched))
    fi
"#
    )?;

    ////////////////// Completion /////////////////////////

    write!(
        buffer,
        r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    subword_completions_no_description_trailing_space=()
    subword_completions_trailing_space=()
    subword_completions_no_trailing_space=()
    subword_suffixes_trailing_space=()
    subword_suffixes_no_trailing_space=()
    subword_descriptions_trailing_space=()
    subword_descriptions_no_trailing_space=()

    for (( subword_fallback_level=0; subword_fallback_level <= subword_max_fallback_level; subword_fallback_level++ )) {{
         declare literal_transitions_name=subword_literal_transitions_level_${{subword_fallback_level}}
         eval "declare initializer=\${{${{literal_transitions_name}}[$subword_state]}}"
         eval "declare -a transitions=($initializer)"
         for literal_id in "${{transitions[@]}}"; do
             local literal=${{subword_literals[$literal_id]}}
             if [[ $literal = "${{completed_prefix}}"* ]]; then
                 local completion="$matched_prefix$literal"
                 if [[ -v "subword_descriptions[$literal_id]" ]]; then
                     subword_completions_no_trailing_space+=("${{completion}}")
                     subword_suffixes_no_trailing_space+=("${{completion}}")
                     subword_descriptions_no_trailing_space+=("${{subword_descriptions[$literal_id]}}")
                 else
                     subword_completions_no_trailing_space+=("${{completion}}")
                     subword_suffixes_no_trailing_space+=("${{literal}}")
                     subword_descriptions_no_trailing_space+=('')
                 fi
             fi
         done

         declare commands_name=subword_commands_level_${{subword_fallback_level}}
         eval "declare initializer=\${{${{commands_name}}[$subword_state]}}"
         eval "declare -a transitions=($initializer)"
         for command_id in "${{transitions[@]}}"; do
             local candidates=()
             local output=$(_{command}_cmd_${{command_id}} "$matched_prefix")
             local -a command_completions=("${{(@f)output}}")
             for line in ${{command_completions[@]}}; do
                 if [[ $line = "${{completed_prefix}}"* ]]; then
                     local parts=(${{(@s:	:)line}})
                     if [[ -v "parts[2]" ]]; then
                         local completion=$matched_prefix${{parts[1]}}
                         subword_completions_trailing_space+=("${{completion}}")
                         subword_suffixes_trailing_space+=("${{parts[1]}}")
                         subword_descriptions_trailing_space+=("${{parts[2]}}")
                     else
                         line="$matched_prefix$line"
                         subword_completions_no_description_trailing_space+=("$line")
                     fi
                 fi
             done
         done

         declare specialized_commands_name=subword_specialized_commands_level_${{subword_fallback_level}}
         eval "declare initializer=\${{${{specialized_commands_name}}[$subword_state]}}"
         eval "declare -a transitions=($initializer)"
         for command_id in "${{transitions[@]}}"; do
             local output=$(_{command}_cmd_${{command_id}} "$matched_prefix")
             local -a candidates=("${{(@f)output}}")
             for line in ${{candidates[@]}}; do
                 if [[ $line = "${{completed_prefix}}"* ]]; then
                     line="$matched_prefix$line"
                     local parts=(${{(@s:	:)line}})
                     if [[ -v "parts[2]" ]]; then
                         subword_completions_trailing_space+=("${{parts[1]}}")
                         subword_suffixes_trailing_space+=("${{parts[1]}}")
                         subword_descriptions_trailing_space+=("${{parts[2]}}")
                     else
                         subword_completions_no_description_trailing_space+=("$line")
                     fi
                 fi
             done
         done

         if [[ ${{#subword_completions_no_description_trailing_space}} -gt 0 || ${{#subword_completions_trailing_space}} -gt 0 || ${{#subword_completions_no_trailing_space}} -gt 0 ]]; then
             break
         fi
    }}
    return 0
}}
"#
    )?;

    writeln!(buffer)?;

    Ok(())
}

pub fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexMap<Ustr, usize>,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, "subword_")?;

    let max_fallback_level = get_max_fallback_level(dfa).unwrap();

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

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
            Input::Command(cmd, _, fallback_level) => {
                let command_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), fallback_level) => {
                let specialized_id = *id_from_cmd.get(cmd).unwrap();
                fallback_specialized[*fallback_level]
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
                    r#"[{from_state_zsh}]="{joined_literal_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_command_ids = itertools::join(command_ids, " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_command_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_commands_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_specialized.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, specialized_ids)| {
                let joined_specialized_ids = itertools::join(specialized_ids, " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_specialized_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_specialized_commands_level_{level}=({initializer})"#
        )?;
    }

    writeln!(
        buffer,
        r#"    declare subword_max_fallback_level={max_fallback_level}"#
    )?;
    writeln!(
        buffer,
        r#"    declare subword_state={starting_state}"#,
        starting_state = dfa.starting_state + 1
    )?;

    writeln!(buffer, r#"    _{command}_subword "$@""#)?;
    writeln!(buffer, r#"}}"#)?;

    Ok(())
}

pub fn make_id_from_command_map(dfa: &DFA) -> IndexMap<Ustr, usize> {
    let mut result: IndexMap<Ustr, usize> = Default::default();

    let mut unallocated_id = 0;

    for input in dfa.iter_inputs() {
        let mut cmds: Vec<Ustr> = Default::default();

        match input {
            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), ..) => {
                cmds.push(*cmd)
            }
            Input::Command(cmd, ..) => cmds.push(*cmd),
            Input::Subword(subdfa, ..) => {
                for input in subdfa.as_ref().iter_inputs() {
                    match input {
                        Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), _) => {
                            cmds.push(*cmd)
                        }
                        Input::Command(cmd, ..) => cmds.push(*cmd),
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        for cmd in cmds {
            result.entry(cmd).or_insert_with(|| {
                let id = unallocated_id;
                unallocated_id += 1;
                id
            });
        }
    }

    result
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    writeln!(
        buffer,
        r#"#compdef {command}
"#
    )?;

    let id_from_cmd = make_id_from_command_map(dfa);
    for (cmd, id) in &id_from_cmd {
        write!(
            buffer,
            r#"_{command}_cmd_{id} () {{
    {cmd}
}}

"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(1);
    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(buffer, command)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &id_from_cmd)?;
        writeln!(buffer)?;
    }

    writeln!(buffer, r#"_{command} () {{"#)?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, "")?;

    writeln!(buffer)?;
    writeln!(buffer, r#"    declare -A subword_transitions"#)?;
    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(
            subword_transitions
                .into_iter()
                .map(|(dfa, to)| format!("[{}]={}", id_from_dfa.get(&dfa).unwrap(), to + 1)),
            " ",
        );
        writeln!(
            buffer,
            r#"    subword_transitions[{}]="({state_transitions})""#,
            state + 1
        )?;
    }

    write!(
        buffer,
        r#"
    local state={starting_state}
    local word_index=2
    while [[ $word_index -lt $CURRENT ]]; do
        if [[ -v "literal_transitions[$state]" ]]; then
            local -A state_transitions
            eval "state_transitions=${{literal_transitions[$state]}}"

            local word=${{words[$word_index]}}
            local word_matched=0
            for ((literal_id = 1; literal_id <= $#literals; literal_id++)); do
                if [[ ${{literals[$literal_id]}} = "$word" ]]; then
                    if [[ -v "state_transitions[$literal_id]" ]]; then
                        state=${{state_transitions[$literal_id]}}
                        word_index=$((word_index + 1))
                        word_matched=1
                        break
                    fi
                fi
            done
            if [[ $word_matched -ne 0 ]]; then
                continue
            fi
        fi
"#,
        starting_state = dfa.starting_state + 1
    )?;

    if dfa.has_subword_transitions() {
        write!(
            buffer,
            r#"
        if [[ -v "subword_transitions[$state]" ]]; then
            declare -A state_transitions
            eval "state_transitions=${{subword_transitions[$state]}}"

            local subword_matched=0
            for subword_id in ${{(k)state_transitions}}; do
                if _{command}_subword_"${{subword_id}}" matches "$word"; then
                    subword_matched=1
                    state=${{state_transitions[$subword_id]}}
                    word_index=$((word_index + 1))
                    break
                fi
            done
            if [[ $subword_matched -ne 0 ]]; then
                continue
            fi
        fi
"#
        )?;
    }

    write!(
        buffer,
        r#"
        if [[ -v "match_anything_transitions[$state]" ]]; then
            state=${{match_anything_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi

        return 1
    done
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

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

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
            Input::Command(cmd, None, fallback_level) => {
                let command_id = *id_from_cmd.get(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Command(_, Some(..), _) => todo!(),
            Input::Nonterminal(_, Some(Specialization { zsh: Some(cmd), .. }), fallback_level) => {
                let specialized_id = *id_from_cmd.get(cmd).unwrap();
                fallback_specialized[*fallback_level]
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
                    r#"[{from_state_zsh}]="{joined_literal_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_subwords.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, subword_ids)| {
                let joined_subword_ids = itertools::join(subword_ids, " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_subword_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_transitions_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_command_ids = itertools::join(command_ids, " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_command_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A commands_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_specialized.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, specialized_ids)| {
                let joined_specialized_ids = itertools::join(specialized_ids, " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_specialized_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A specialized_commands_level_{level}=({initializer})"#
        )?;
    }

    write!(
        buffer,
        r#"
    local max_fallback_level={max_fallback_level}
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )) {{
        completions_no_description_trailing_space=()
        completions_no_description_no_trailing_space=()
        completions_trailing_space=()
        suffixes_trailing_space=()
        descriptions_trailing_space=()
        completions_no_trailing_space=()
        suffixes_no_trailing_space=()
        descriptions_no_trailing_space=()
        matches=()

        declare literal_transitions_name=literal_transitions_level_${{fallback_level}}
        eval "declare initializer=\${{${{literal_transitions_name}}[$state]}}"
        eval "declare -a transitions=($initializer)"
        for literal_id in "${{transitions[@]}}"; do
            if [[ -v "descriptions[$literal_id]" ]]; then
                completions_trailing_space+=("${{literals[$literal_id]}}")
                suffixes_trailing_space+=("${{literals[$literal_id]}}")
                descriptions_trailing_space+=("${{descriptions[$literal_id]}}")
            else
                completions_no_description_trailing_space+=("${{literals[$literal_id]}}")
            fi
        done

        declare subword_transitions_name=subword_transitions_level_${{fallback_level}}
        eval "declare initializer=\${{${{subword_transitions_name}}[$state]}}"
        eval "declare -a transitions=($initializer)"
        for subword_id in "${{transitions[@]}}"; do
            _{command}_subword_${{subword_id}} complete "${{words[$CURRENT]}}"
            completions_no_description_trailing_space+=("${{subword_completions_no_description_trailing_space[@]}}")
            completions_trailing_space+=("${{subword_completions_trailing_space[@]}}")
            completions_no_trailing_space+=("${{subword_completions_no_trailing_space[@]}}")
            suffixes_no_trailing_space+=("${{subword_suffixes_no_trailing_space[@]}}")
            suffixes_trailing_space+=("${{subword_suffixes_trailing_space[@]}}")
            descriptions_trailing_space+=("${{subword_descriptions_trailing_space[@]}}")
            descriptions_no_trailing_space+=("${{subword_descriptions_no_trailing_space[@]}}")
        done

        declare commands_name=commands_level_${{fallback_level}}
        eval "declare initializer=\${{${{commands_name}}[$state]}}"
        eval "declare -a transitions=($initializer)"
        for command_id in "${{transitions[@]}}"; do
            local output=$(_{command}_cmd_${{command_id}} "${{words[$CURRENT]}}")
            local -a command_completions=("${{(@f)output}}")
            for line in ${{command_completions[@]}}; do
                local parts=(${{(@s:	:)line}})
                if [[ -v "parts[2]" ]]; then
                    completions_trailing_space+=("${{parts[1]}}")
                    suffixes_trailing_space+=("${{parts[1]}}")
                    descriptions_trailing_space+=("${{parts[2]}}")
                else
                    completions_no_description_trailing_space+=("${{parts[1]}}")
                fi
            done
        done

        declare specialized_commands_name=specialized_commands_level_${{fallback_level}}
        eval "declare initializer=\${{${{specialized_commands_name}}[$state]}}"
        eval "declare -a transitions=($initializer)"
        for command_id in "${{transitions[@]}}"; do
            _{command}_cmd_${{command_id}} ${{words[$CURRENT]}}
        done

        local maxlen=0
        for suffix in ${{suffixes_trailing_space[@]}}; do
            if [[ ${{#suffix}} -gt $maxlen ]]; then
                maxlen=${{#suffix}}
            fi
        done
        for suffix in ${{suffixes_no_trailing_space[@]}}; do
            if [[ ${{#suffix}} -gt $maxlen ]]; then
                maxlen=${{#suffix}}
            fi
        done

        for ((i = 1; i <= $#suffixes_trailing_space; i++)); do
            if [[ -z ${{descriptions_trailing_space[$i]}} ]]; then
                descriptions_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_trailing_space[$i]}}}}"
            else
                descriptions_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_trailing_space[$i]}}}} -- ${{descriptions_trailing_space[$i]}}"
            fi
        done

        for ((i = 1; i <= $#suffixes_no_trailing_space; i++)); do
            if [[ -z ${{descriptions_no_trailing_space[$i]}} ]]; then
                descriptions_no_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_no_trailing_space[$i]}}}}"
            else
                descriptions_no_trailing_space[$i]="${{(r($maxlen)( ))${{suffixes_no_trailing_space[$i]}}}} -- ${{descriptions_no_trailing_space[$i]}}"
            fi
        done

        compadd -O m -a completions_no_description_trailing_space; matches+=("${{m[@]}}")
        compadd -O m -a completions_no_description_no_trailing_space; matches+=("${{m[@]}}")
        compadd -O m -a completions_trailing_space; matches+=("${{m[@]}}")
        compadd -O m -a completions_no_trailing_space; matches+=("${{m[@]}}")

        if [[ ${{#matches}} -gt 0 ]]; then
            compadd -Q -a completions_no_description_trailing_space
            compadd -Q -S ' ' -a completions_no_description_no_trailing_space
            compadd -l -Q -a -d descriptions_trailing_space completions_trailing_space
            compadd -l -Q -S '' -a -d descriptions_no_trailing_space completions_no_trailing_space
            return 0
        fi
    }}
}}
"#
    )?;

    write!(
        buffer,
        r#"
if [[ $ZSH_EVAL_CONTEXT =~ :file$ ]]; then
    compdef _{command} {command}
else
    _{command}
fi
"#
    )?;

    Ok(())
}
