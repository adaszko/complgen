use std::io::Write;

use crate::dfa::DFA;
use crate::regex::Inp;
use crate::{Result, StateId};
use hashbrown::HashMap;
use indexmap::{IndexMap, IndexSet};
use ustr::{Ustr, ustr};

// Array indexes in ZSH start from 1 (!)
// `for i in {{1..$#array}}; do ...; done` loops do not behave well if array is empty!  Prefer i++ loops instead.
// Zsh uses *dynamic* scoping for local variables, even if declared with 'local', hence 'declare'
// is used as slightly less misleading (!)

// TODO DO not produce quotes if not necessary to save space
fn make_string_constant(s: &str) -> String {
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
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();
    let literals: String = itertools::join(
        all_literals
            .iter()
            .map(|(_, literal, _)| make_string_constant(literal)),
        " ",
    );
    writeln!(buffer, r#"    declare -a {prefix}literals=({literals})"#)?;

    let descrs: IndexSet<Ustr> = all_literals
        .iter()
        .map(|(_, _, descr)| *descr)
        .filter(|d| !d.is_empty())
        .collect();
    writeln!(buffer, r#"    declare -A {prefix}descrs=()"#)?;
    for descr in &descrs {
        if descr.is_empty() {
            continue;
        }
        let id = descrs.get_index_of(descr).unwrap();
        let quoted = make_string_constant(descr);
        writeln!(buffer, r#"    {prefix}descrs[{id}]={quoted}"#)?;
    }

    let descr_id_from_literal_id: IndexMap<usize, usize> = all_literals
        .iter()
        .filter_map(|(id, _, description)| descrs.get_index_of(description).map(|d| (*id, d)))
        .collect();
    let initializer = itertools::join(
        descr_id_from_literal_id
            .iter()
            .map(|(literal_id, descr_id)| format!("[{literal_id}]={descr_id}")),
        " ",
    );
    writeln!(
        buffer,
        r#"    declare -A {prefix}descr_id_from_literal_id=({initializer})"#
    )?;

    let regexes: String = itertools::join(
        id_from_regex
            .iter()
            .map(|regex| make_string_constant(regex)),
        " ",
    );
    writeln!(buffer, r#"    declare -a {prefix}regexes=({regexes})"#)?;

    writeln!(buffer, r#"    declare -A {prefix}literal_transitions=()"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !literal_transitions.is_empty() {
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
    }

    writeln!(buffer, r#"    declare -A {prefix}nontail_transitions=()"#)?;
    for state in dfa.get_all_states() {
        let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
        if !nontail_transitions.is_empty() {
            let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                .into_iter()
                .map(|(regex, to)| (id_from_regex.get_index_of(&regex).unwrap(), to))
                .collect();
            let state_nontail_transitions: String = itertools::join(
                nontail_command_transitions
                    .into_iter()
                    .map(|(regex_id, to)| format!("[{}]={}", regex_id + 1, to + 1)),
                " ",
            );
            writeln!(
                buffer,
                r#"    {prefix}nontail_transitions[{}]="({state_nontail_transitions})""#,
                state + 1,
            )?;
        }
    }

    let match_anything_transitions = itertools::join(
        dfa.iter_match_anything_transitions()
            .map(|(from, to)| format!("[{}]={}", from + 1, to + 1)),
        " ",
    );
    writeln!(
        buffer,
        r#"    declare -A {prefix}match_anything_transitions=({match_anything_transitions})"#
    )?;

    Ok(literal_id_from_input_description)
}

fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
    write!(
        buffer,
        r#"_{command}_subword () {{
    declare mode=$1
    declare word=$2
"#
    )?;

    write!(
        buffer,
        r#"
    declare char_index=0
    declare matched=0
    while true; do
        if [[ $char_index -ge ${{#word}} ]]; then
            matched=1
            break
        fi

        declare subword=${{word:$char_index}}

        if [[ -v "subword_literal_transitions[$subword_state]" ]]; then
            eval "declare -A state_transitions=${{subword_literal_transitions[$subword_state]}}"

            declare literal_matched=0
            for ((literal_id = 1; literal_id <= $#subword_literals; literal_id++)); do
                declare literal=${{subword_literals[$literal_id]}}
                declare literal_len=${{#literal}}
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

        if [[ -v "subword_nontail_transitions[$subword_state]" ]]; then
            eval "declare -A state_nontails=${{subword_nontail_transitions[$subword_state]}}"

            declare nontail_matched=0
            for regex_id in "${{(k)state_nontails}}"; do
                declare regex="^(${{subword_regexes[$regex_id]}}).*"
                if [[ ${{subword}} =~ $regex && -n ${{match[1]}} ]]; then
                    match="${{match[1]}}"
                    match_len=${{#match}}
                    char_index=$((char_index + match_len))
                    subword_state=${{state_nontails[$regex_id]}}
                    nontail_matched=1
                    break
                fi
            done
            if [[ $nontail_matched -ne 0 ]]; then
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

    // /////////////// Completion /////////////////////////

    write!(
        buffer,
        r#"
    declare matched_prefix="${{word:0:$char_index}}"
    declare completed_prefix="${{word:$char_index}}"

    subword_completions_no_description_trailing_space=()
    subword_completions_trailing_space=()
    subword_completions_no_trailing_space=()
    subword_suffixes_trailing_space=()
    subword_suffixes_no_trailing_space=()
    subword_descriptions_trailing_space=()
    subword_descriptions_no_trailing_space=()

    for (( subword_fallback_level=0; subword_fallback_level <= subword_max_fallback_level; subword_fallback_level++ )); do
        declare literal_transitions_name=subword_literal_transitions_level_${{subword_fallback_level}}
        eval "declare initializer=\${{${{literal_transitions_name}}[$subword_state]}}"
        eval "declare -a transitions=($initializer)"
        for literal_id in "${{transitions[@]}}"; do
            declare literal=${{subword_literals[$literal_id]}}
            if [[ $literal = "${{completed_prefix}}"* ]]; then
                declare completion="$matched_prefix$literal"
                if [[ -v "subword_descr_id_from_literal_id[$literal_id]" ]]; then
                    declare descr_id=$subword_descr_id_from_literal_id[$literal_id]
                    subword_completions_no_trailing_space+=("${{completion}}")
                    subword_suffixes_no_trailing_space+=("${{completion}}")
                    subword_descriptions_no_trailing_space+=("${{subword_descrs[$descr_id]}}")
                else
                    subword_completions_no_trailing_space+=("${{completion}}")
                    subword_suffixes_no_trailing_space+=("${{literal}}")
                    subword_descriptions_no_trailing_space+=('')
                fi
            fi
        done

        declare commands_name=subword_nontail_commands_level_${{subword_fallback_level}}
        eval "declare commands_initializer=\${{${{commands_name}}[$subword_state]}}"
        eval "declare -a command_transitions=($commands_initializer)"
        declare regexes_name=subword_nontail_regexes_level_${{subword_fallback_level}}
        eval "declare regexes_initializer=\${{${{regexes_name}}[$subword_state]}}"
        eval "declare -a regexes_transitions=($regexes_initializer)"
        for (( i=1; i <= ${{#command_transitions[@]}}; i++ )); do
            declare command_id=${{command_transitions[$i]}}
            declare regex_id=${{regexes_transitions[$i]}}
            declare regex="^(${{subword_regexes[$regex_id]}}).*"
            declare candidates=()
            declare output=$(_{command}_cmd_${{command_id}} "$matched_prefix")
            declare -a command_completions=("${{(@f)output}}")
            for line in ${{command_completions[@]}}; do
                if [[ $line = "${{completed_prefix}}"* ]]; then
                    declare parts=(${{(@s:	:)line}})
                    if [[ ${{parts[1]}} =~ $regex && -n ${{match[1]}} ]]; then
                        parts[1]=${{match[1]}}
                        if [[ -v "parts[2]" ]]; then
                            declare completion=$matched_prefix${{parts[1]}}
                            subword_completions_trailing_space+=("${{completion}}")
                            subword_suffixes_trailing_space+=("${{parts[1]}}")
                            subword_descriptions_trailing_space+=("${{parts[2]}}")
                        else
                            subword_completions_no_description_trailing_space+=("$matched_prefix${{parts[1]}}")
                        fi
                    fi
                fi
            done
        done

        declare commands_name=subword_commands_level_${{subword_fallback_level}}
        eval "declare initializer=\${{${{commands_name}}[$subword_state]}}"
        eval "declare -a transitions=($initializer)"
        for command_id in "${{transitions[@]}}"; do
            declare candidates=()
            declare output=$(_{command}_cmd_${{command_id}} "$matched_prefix")
            declare -a command_completions=("${{(@f)output}}")
            for line in ${{command_completions[@]}}; do
                if [[ $line = "${{completed_prefix}}"* ]]; then
                    declare parts=(${{(@s:	:)line}})
                    if [[ -v "parts[2]" ]]; then
                        declare completion=$matched_prefix${{parts[1]}}
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
            declare output=$(_{command}_cmd_${{command_id}} "$matched_prefix")
            declare -a candidates=("${{(@f)output}}")
            for line in ${{candidates[@]}}; do
                if [[ $line = "${{completed_prefix}}"* ]]; then
                    line="$matched_prefix$line"
                    declare parts=(${{(@s:	:)line}})
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
    done
    return 0
}}
"#
    )?;

    writeln!(buffer)?;

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
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    let literal_id_from_input_description =
        write_lookup_tables(buffer, dfa, "subword_", id_from_regex)?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(1);

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    fallback_nontails.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

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
            Inp::Command {
                cmd,
                regex: _,
                fallback_level,
                zsh_compadd: true,
            } => {
                let specialized_id = id_from_cmd.get_index_of(&cmd).unwrap();
                fallback_specialized[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(specialized_id);
            }
            Inp::Star => {}
            Inp::Subword { .. } => unreachable!(),
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

    for (level, transitions) in fallback_nontails.iter().enumerate() {
        let commands_initializer = itertools::join(
            transitions.iter().map(|(from_state, nontails)| {
                let joined_ids = itertools::join(nontails.iter().map(|(cmd_id, _)| cmd_id), " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_nontail_commands_level_{level}=({commands_initializer})"#
        )?;

        let regexes_initializer = itertools::join(
            transitions.iter().map(|(from_state, nontails)| {
                let joined_ids =
                    itertools::join(nontails.iter().map(|(_, regex_id)| regex_id + 1), " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A subword_nontail_regexes_level_{level}=({regexes_initializer})"#
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

fn make_id_from_command_map(dfa: &DFA) -> (IndexSet<Ustr>, IndexSet<Ustr>) {
    let mut id_from_cmd: IndexSet<Ustr> = Default::default();

    let mut id_from_regex: IndexSet<Ustr> = Default::default();

    for input in dfa.iter_inputs() {
        match input {
            Inp::Literal { .. } | Inp::Star => {}
            Inp::Command {
                cmd,
                regex,
                zsh_compadd: _,
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
                            cmd,
                            regex,
                            zsh_compadd: _,
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

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    writeln!(
        buffer,
        r#"#compdef {command}
"#
    )?;

    let (id_from_cmd, id_from_regex) = make_id_from_command_map(dfa);
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
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
        for (dfaid, id) in &id_from_dfa {
            let dfa = dfa.subdfas.lookup(*dfaid);
            write_subword_fn(buffer, command, *id, dfa, &id_from_cmd, &id_from_regex)?;
            writeln!(buffer)?;
        }
    }

    writeln!(buffer, r#"_{command} () {{"#)?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, "", &id_from_regex)?;

    writeln!(buffer, r#"    declare -A subword_transitions=()"#)?;
    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state);
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
    declare state={starting_state}
    declare word_index=2
    while [[ $word_index -lt $CURRENT ]]; do
        if [[ -v "literal_transitions[$state]" ]]; then
            eval "declare -A state_transitions=${{literal_transitions[$state]}}"

            declare word=${{words[$word_index]}}
            declare word_matched=0
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

    writeln!(
        buffer,
        r#"
        if [[ -v "nontail_transitions[$state]" ]]; then
            eval "declare -A state_nontails=${{nontail_transitions[$state]}}"
            declare nontail_matched=0
            for regex_id in "${{(k)state_nontails}}"; do
                declare regex="^(${{regexes[$regex_id]}}).*"
                if [[ ${{subword}} =~ $regex && -n ${{match[1]}} ]]; then
                    match="${{match[1]}}"
                    match_len=${{#match}}
                    char_index=$((char_index + match_len))
                    state=${{state_nontails[$regex_id]}}
                    nontail_matched=1
                    break
                fi
            done
            if [[ $nontail_matched -ne 0 ]]; then
                continue
            fi
        fi
"#
    )?;

    if dfa.has_subword_transitions() {
        write!(
            buffer,
            r#"
        if [[ -v "subword_transitions[$state]" ]]; then
            eval "declare -A state_transitions=${{subword_transitions[$state]}}"

            declare subword_matched=0
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

    writeln!(
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

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

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
            Inp::Command {
                cmd,
                regex: _,
                fallback_level,
                zsh_compadd: true,
            } => {
                let specialized_id = id_from_cmd.get_index_of(&cmd).unwrap();
                fallback_specialized[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(specialized_id);
            }
            Inp::Star => {}
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

    for (level, transitions) in fallback_nontails.iter().enumerate() {
        let commands_initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_ids = itertools::join(command_ids.iter().map(|(cmd_id, _)| cmd_id), " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_commands_level_{level}=({commands_initializer})"#
        )?;

        let regexes_initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_ids =
                    itertools::join(command_ids.iter().map(|(_, regex_id)| regex_id + 1), " ");
                format!(
                    r#"[{from_state_zsh}]="{joined_ids}""#,
                    from_state_zsh = from_state + 1
                )
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_regexes_level_{level}=({regexes_initializer})"#
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
    declare max_fallback_level={max_fallback_level}
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )); do
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
            if [[ -v "descr_id_from_literal_id[$literal_id]" ]]; then
                declare descr_id=$descr_id_from_literal_id[$literal_id]
                completions_trailing_space+=("${{literals[$literal_id]}}")
                suffixes_trailing_space+=("${{literals[$literal_id]}}")
                descriptions_trailing_space+=("${{descrs[$descr_id]}}")
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
            declare output=$(_{command}_cmd_${{command_id}} "${{words[$CURRENT]}}")
            declare -a command_completions=("${{(@f)output}}")
            for line in ${{command_completions[@]}}; do
                declare parts=(${{(@s:	:)line}})
                if [[ -v "parts[2]" ]]; then
                    completions_trailing_space+=("${{parts[1]}}")
                    suffixes_trailing_space+=("${{parts[1]}}")
                    descriptions_trailing_space+=("${{parts[2]}}")
                else
                    completions_no_description_trailing_space+=("${{parts[1]}}")
                fi
            done
        done

        declare commands_name=nontail_commands_level_${{fallback_level}}
        eval "declare command_initializer=\${{${{commands_name}}[$state]}}"
        eval "declare -a command_transitions=($command_initializer)"
        declare regexes_name=nontail_regexes_level_${{fallback_level}}
        eval "declare regexes_initializer=\${{${{regexes_name}}[$state]}}"
        eval "declare -a regexes_transitions=($regexes_initializer)"
        for (( i=1; i <= ${{#command_transitions[@]}}; i++ )); do
            declare command_id=${{command_transitions[$i]}}
            declare regex_id=${{regexes_transitions[$i]}}
            declare regex="^(${{regexes[$regex_id]}}).*"
            declare output=$(_{command}_cmd_${{command_id}} "${{words[$CURRENT]}}")
            declare -a command_completions=("${{(@f)output}}")
            for line in ${{command_completions[@]}}; do
                declare parts=(${{(@s:	:)line}})
                if [[ ${{parts[1]}} =~ $regex && -n ${{match[1]}} ]]; then
                    parts[1]=${{match[1]}}
                    if [[ -v "parts[2]" ]]; then
                        completions_trailing_space+=("${{parts[1]}}")
                        suffixes_trailing_space+=("${{parts[1]}}")
                        descriptions_trailing_space+=("${{parts[2]}}")
                    else
                        completions_no_description_trailing_space+=("${{parts[1]}}")
                    fi
                fi
            done
        done

        declare specialized_commands_name=specialized_commands_level_${{fallback_level}}
        eval "declare initializer=\${{${{specialized_commands_name}}[$state]}}"
        eval "declare -a transitions=($initializer)"
        for command_id in "${{transitions[@]}}"; do
            _{command}_cmd_${{command_id}} ${{words[$CURRENT]}}
        done

        declare maxlen=0
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
    done
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
