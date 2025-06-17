use std::io::Write;

use crate::Result;
use crate::StateId;
use crate::dfa::DFA;
use crate::grammar::CmdRegexDecl;
use crate::grammar::Shell;
use crate::grammar::Specialization;
use crate::regex::Input;
use hashbrown::HashMap;
use indexmap::IndexSet;
use ustr::{Ustr, ustr};

// Bash array indexes start at 0.
// Associative arrays are local by default.
// Bash uses *dynamic* scoping for local variables (!)

pub fn make_string_constant(s: &str) -> String {
    if s.is_empty() {
        return r#""""#.to_string();
    }
    if s.contains(&[' ', '\t', '\n']) {
        format!(
            r#""{}""#,
            s.replace('\"', "\\\"")
                .replace('`', "\\`")
                .replace('$', "\\$")
        )
    } else {
        format!(
            "{}",
            s.replace('\"', "\\\"")
                .replace('`', "\\`")
                .replace('$', "\\$")
        )
    }
}

pub const MATCH_FN_NAME: &str = "__complgen_match";
pub fn write_match_fn<W: Write>(buffer: &mut W) -> Result<()> {
    writeln!(
        buffer,
        r#"{MATCH_FN_NAME} () {{
    [[ $# -lt 2 ]] && return 1
    local ignore_case=$1
    local prefix=$2
    [[ -z $prefix ]] && cat
    if [[ $ignore_case = on ]]; then
        prefix=${{prefix,,}}
        prefix=$(printf '%q' "$prefix")
        while read line; do
            [[ ${{line,,}} = ${{prefix}}* ]] && echo $line
        done
    else
        prefix=$(printf '%q' "$prefix")
        while read line; do
            [[ $line = ${{prefix}}* ]] && echo $line
        done
    fi
}}
"#
    )?;
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
        .map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr(""))))
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
    writeln!(buffer, r#"    declare -a literals=({literals})"#)?;

    let regexes: String = itertools::join(
        id_from_regex
            .iter()
            .map(|regex| make_string_constant(regex)),
        " ",
    );
    writeln!(buffer, r#"    declare -a regexes=({regexes})"#)?;

    writeln!(buffer, r#"    declare -A literal_transitions=()"#)?;
    writeln!(buffer, r#"    declare -A nontail_transitions=()"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions =
            dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if !literal_transitions.is_empty() {
            let literal_transitions: Vec<(usize, StateId)> = literal_transitions
                .into_iter()
                .map(|(literal, description, to)| {
                    (
                        *literal_id_from_input_description
                            .get(&(literal, description))
                            .unwrap(),
                        to,
                    )
                })
                .collect();
            let state_literal_transitions: String = itertools::join(
                literal_transitions
                    .into_iter()
                    .map(|(literal_id, to)| format!("[{}]={}", literal_id, to)),
                " ",
            );
            writeln!(
                buffer,
                r#"    literal_transitions[{state}]="({state_literal_transitions})""#
            )?;
        }

        let nontail_transitions = dfa.get_bash_nontail_transitions_from(state as StateId);
        if !nontail_transitions.is_empty() {
            let nontail_command_transitions: Vec<(usize, StateId)> = nontail_transitions
                .into_iter()
                .map(|(regex, to)| (id_from_regex.get_index_of(&regex).unwrap(), to))
                .collect();
            let state_nontail_transitions: String = itertools::join(
                nontail_command_transitions
                    .into_iter()
                    .map(|(regex_id, to)| format!("[{regex_id}]={to}")),
                " ",
            );
            writeln!(
                buffer,
                r#"    nontail_transitions[{state}]="({state_nontail_transitions})""#
            )?;
        }
    }

    let match_anything_transitions = itertools::join(
        dfa.iter_match_anything_transitions(Shell::Bash)
            .into_iter()
            .map(|(from, to)| format!("[{from}]={to}")),
        " ",
    );
    writeln!(
        buffer,
        r#"    declare -A match_anything_transitions=({match_anything_transitions})"#
    )?;

    Ok(literal_id_from_input_description)
}

pub fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
    writeln!(
        buffer,
        r#"_{command}_subword () {{
    [[ $# -ne 2 ]] && return 1
    local mode=$1
    local word=$2

    local char_index=0
    local matched=0
    while true; do
        if [[ $char_index -ge ${{#word}} ]]; then
            matched=1
            break
        fi

        local subword=${{word:$char_index}}

        if [[ -v "literal_transitions[$state]" ]]; then
            declare -A state_transitions
            eval "state_transitions=${{literal_transitions[$state]}}"

            local literal_matched=0
            for literal_id in $(seq 0 $((${{#literals[@]}} - 1))); do
                local literal=${{literals[$literal_id]}}
                local literal_len=${{#literal}}
                if [[ ${{subword:0:$literal_len}} = "$literal" ]]; then
                    if [[ -v "state_transitions[$literal_id]" ]]; then
                        state=${{state_transitions[$literal_id]}}
                        char_index=$((char_index + literal_len))
                        literal_matched=1
                        break
                    fi
                fi
            done
            if [[ $literal_matched -ne 0 ]]; then
                continue
            fi
        fi

        if [[ -v "nontail_transitions[$state]" ]]; then
            declare -A state_nontails
            eval "state_nontails=${{nontail_transitions[$state]}}"
            local nontail_matched=0
            for regex_id in "${{!state_nontails[@]}}"; do
                local regex="^(${{regexes[$regex_id]}}).*"
                if [[ ${{subword}} =~ $regex && -n ${{BASH_REMATCH[1]}} ]]; then
                    match="${{BASH_REMATCH[1]}}"
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

        if [[ -v "match_anything_transitions[$state]" ]]; then
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

    // TODO Implement a way to determine if a transition within a subword is a final one so that a
    // trailing space can be added

    write!(
        buffer,
        r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    local -a candidates=()
    local -a matches=()
    local ignore_case=$(bind -v | grep completion-ignore-case | cut -d' ' -f3)
    for (( subword_fallback_level=0; subword_fallback_level <= max_fallback_level; subword_fallback_level++ )) {{
        eval "declare literal_transitions_name=literal_transitions_level_${{subword_fallback_level}}"
        eval "declare -a transitions=(\${{$literal_transitions_name[$state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal=${{literals[$literal_id]}}
            candidates+=("$matched_prefix$literal")
        done
        if [[ ${{#candidates[@]}} -gt 0 ]]; then
            readarray -t matches < <(printf "%s\n" "${{candidates[@]}}" | {MATCH_FN_NAME} "$ignore_case" "$matched_prefix$completed_prefix")
        fi

        eval "declare commands_name=commands_level_${{subword_fallback_level}}"
        eval "declare -a transitions=(\${{$commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            readarray -t candidates < <(_{command}_cmd_$command_id "$matched_prefix" | cut -f1)
            for item in "${{candidates[@]}}"; do
                matches+=("$matched_prefix$item")
            done
        done

        eval "declare commands_name=nontail_commands_level_${{subword_fallback_level}}"
        eval "declare -a command_transitions=(\${{$commands_name[$state]}})"
        eval "declare regexes_name=nontail_regexes_level_${{fallback_level}}"
        eval "declare -a regexes_transitions=(\${{$regexes_name[$state]}})"
        for (( i = 0; i < ${{#command_transitions[@]}}; i++ )); do
            local command_id=${{command_transitions[$i]}}
            readarray -t output < <(_{command}_cmd_$command_id "$matched_prefix" | cut -f1)
            local regex_id=${{regexes_transitions[$i]}}
            local regex="^(${{regexes[$regex_id]}}).*"
            declare -a candidates=()
            for line in ${{output[@]}}; do
                if [[ ${{line}} =~ $regex && -n ${{BASH_REMATCH[1]}} ]]; then
                    match="${{BASH_REMATCH[1]}}"
                    candidates+=("$match")
                fi
            done
            for item in "${{candidates[@]}}"; do
                matches+=("$matched_prefix$item")
            done
        done

        if [[ ${{#matches[@]}} -gt 0 ]]; then
            printf '%s\n' "${{matches[@]}}"
            break
        fi
    }}
"#
    )?;

    writeln!(
        buffer,
        r#"    return 0
}}"#
    )?;
    writeln!(buffer)?;

    Ok(())
}

pub fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    id_from_regex: &IndexSet<Ustr>,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, id_from_regex)?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap();

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    fallback_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input, _) in dfa.iter_transitions() {
        match input {
            Input::Literal {
                literal: lit,
                description: descr,
                fallback_level,
                ..
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*lit, (*descr).unwrap_or("".into())))
                    .unwrap();
                fallback_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Input::Command {
                cmd,
                regex: None,
                fallback_level,
                ..
            } => {
                let command_id = id_from_cmd.get_index_of(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Command {
                cmd,
                regex:
                    Some(CmdRegexDecl {
                        bash: Some(bash_regex),
                        ..
                    }),
                fallback_level,
                ..
            } => {
                let cmd_id = id_from_cmd.get_index_of(cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(bash_regex).unwrap();
                fallback_nontails[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Input::Nonterminal {
                nonterm: _,
                spec:
                    Some(Specialization {
                        bash: Some(cmd), ..
                    }),
                fallback_level,
                ..
            } => {
                let specialized_id = id_from_cmd.get_index_of(cmd).unwrap();
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
                format!(r#"[{from_state}]="{joined_literal_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    for (level, transitions) in fallback_nontails.iter().enumerate() {
        let commands_initializer = itertools::join(
            transitions.iter().map(|(from_state, nontail_transitions)| {
                let joined =
                    itertools::join(nontail_transitions.iter().map(|(cmd_id, _)| cmd_id), " ");
                format!(r#"[{from_state}]="{joined}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_commands_level_{level}=({commands_initializer})"#
        )?;
        let regexes_initializer = itertools::join(
            transitions.iter().map(|(from_state, nontail_transitions)| {
                let joined = itertools::join(
                    nontail_transitions.iter().map(|(_, regex_id)| regex_id),
                    " ",
                );
                format!(r#"[{from_state}]="{joined}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_regexes_level_{level}=({regexes_initializer})"#
        )?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, command_ids)| {
                let joined_command_ids = itertools::join(command_ids, " ");
                format!(r#"[{from_state}]="{joined_command_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A commands_level_{level}=({initializer})"#
        )?;
    }

    writeln!(
        buffer,
        r#"    declare max_fallback_level={max_fallback_level}"#
    )?;
    writeln!(
        buffer,
        r#"    declare state={starting_state}"#,
        starting_state = dfa.starting_state
    )?;

    writeln!(buffer, r#"    _{command}_subword "$1" "$2""#)?;

    writeln!(buffer, r#"}}"#)?;

    Ok(())
}

pub fn make_id_from_command_map(dfa: &DFA) -> (IndexSet<Ustr>, IndexSet<Ustr>) {
    // IndexSet's internal index is used for storing command id
    let mut id_from_cmd: IndexSet<Ustr> = Default::default();

    let mut id_from_regex: IndexSet<Ustr> = Default::default();

    for input in dfa.iter_inputs() {
        match input {
            Input::Nonterminal {
                nonterm: _,
                spec:
                    Some(Specialization {
                        bash: Some(cmd), ..
                    }),
                ..
            } => {
                id_from_cmd.insert(*cmd);
            }
            Input::Command { cmd, regex, .. } => {
                id_from_cmd.insert(*cmd);
                if let Some(CmdRegexDecl {
                    bash: Some(bash_regex),
                    ..
                }) = regex
                {
                    id_from_regex.insert(*bash_regex);
                }
            }
            Input::Subword {
                subdfa: subdfaid, ..
            } => {
                let subdfa = dfa.subdfa_interner.lookup(*subdfaid);
                for input in subdfa.iter_inputs() {
                    match input {
                        Input::Nonterminal {
                            spec:
                                Some(Specialization {
                                    bash: Some(cmd), ..
                                }),
                            ..
                        } => {
                            id_from_cmd.insert(*cmd);
                        }
                        Input::Command { cmd, regex, .. } => {
                            id_from_cmd.insert(*cmd);
                            if let Some(CmdRegexDecl {
                                bash: Some(bash_regex),
                                ..
                            }) = regex
                            {
                                id_from_regex.insert(*bash_regex);
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    (id_from_cmd, id_from_regex)
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    write!(
        buffer,
        r#"if [[ $BASH_VERSINFO -lt 4 ]]; then
    echo "This completion script requires bash 4.0 or newer (current is $BASH_VERSION)"
    exit 1
fi

"#
    )?;

    write_match_fn(buffer)?;

    let (id_from_cmd, id_from_regex) = make_id_from_command_map(dfa);
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
        writeln!(
            buffer,
            r#"_{command}_cmd_{id} () {{
    {cmd}
}}
"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(0);
    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(buffer, command)?;
    }
    for (dfaid, id) in &id_from_dfa {
        let dfa = dfa.subdfa_interner.lookup(*dfaid);
        write_subword_fn(buffer, command, *id, dfa, &id_from_cmd, &id_from_regex)?;
        writeln!(buffer)?;
    }

    write!(buffer, r#"_{command} () {{"#)?;

    writeln!(
        buffer,
        r#"
    if [[ $(type -t _get_comp_words_by_ref) != function ]]; then
        echo _get_comp_words_by_ref: function not defined.  Make sure the bash-completion system package is installed
        return 1
    fi

    local words cword
    _get_comp_words_by_ref -n "$COMP_WORDBREAKS" words cword
"#
    )?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa, &id_from_regex)?;

    writeln!(buffer, r#"    declare -A subword_transitions"#)?;
    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(
            subword_transitions
                .into_iter()
                .map(|(dfa, to)| format!("[{}]={}", id_from_dfa.get(&dfa).unwrap(), to)),
            " ",
        );
        writeln!(
            buffer,
            r#"    subword_transitions[{state}]="({state_transitions})""#
        )?;
    }

    write!(
        buffer,
        r#"
    local state={starting_state}
    local word_index=1
    while [[ $word_index -lt $cword ]]; do
        local word=${{words[$word_index]}}

        if [[ -v "literal_transitions[$state]" ]]; then
            declare -A state_transitions
            eval "state_transitions=${{literal_transitions[$state]}}"

            local word_matched=0
            for literal_id in $(seq 0 $((${{#literals[@]}} - 1))); do
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
        starting_state = dfa.starting_state
    )?;

    if dfa.has_subword_transitions() {
        write!(
            buffer,
            r#"
        if [[ -v "subword_transitions[$state]" ]]; then
            declare -A state_transitions
            eval "state_transitions=${{subword_transitions[$state]}}"

            local subword_matched=0
            for subword_id in "${{!state_transitions[@]}}"; do
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

    // ///////////////////////////// Completion ///////////////////////////////////

    let max_fallback_level = dfa.get_max_fallback_level().unwrap();

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    fallback_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input, _) in dfa.iter_transitions() {
        match input {
            Input::Literal {
                literal: lit,
                description: descr,
                fallback_level,
                ..
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*lit, (*descr).unwrap_or("".into())))
                    .unwrap();
                fallback_literals[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Input::Subword {
                subdfa: dfa,
                fallback_level,
                ..
            } => {
                let subword_id = *id_from_dfa.get(dfa).unwrap();
                fallback_subwords[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(subword_id);
            }
            Input::Command {
                cmd,
                regex: None,
                fallback_level,
                ..
            } => {
                let command_id = id_from_cmd.get_index_of(cmd).unwrap();
                fallback_commands[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push(command_id);
            }
            Input::Command {
                cmd,
                regex:
                    Some(CmdRegexDecl {
                        bash: Some(bash_regex),
                        ..
                    }),
                fallback_level,
                ..
            } => {
                let cmd_id = id_from_cmd.get_index_of(cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(bash_regex).unwrap();
                fallback_nontails[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Input::Nonterminal {
                spec:
                    Some(Specialization {
                        bash: Some(cmd), ..
                    }),
                fallback_level,
                ..
            } => {
                let specialized_id = id_from_cmd.get_index_of(cmd).unwrap();
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
                format!(r#"[{from_state}]="{joined_literal_ids}""#)
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
                format!(r#"[{from_state}]="{joined_subword_ids}""#)
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
                format!(r#"[{from_state}]="{joined_command_ids}""#)
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
            transitions.iter().map(|(from_state, ids)| {
                let joined_ids = itertools::join(ids.iter().map(|(cmd_id, _)| cmd_id), " ");
                format!(r#"[{from_state}]="{joined_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_commands_level_{level}=({commands_initializer})"#
        )?;

        let regexes_initializer = itertools::join(
            transitions.iter().map(|(from_state, ids)| {
                let joined_ids = itertools::join(ids.iter().map(|(_, regex_id)| regex_id), " ");
                format!(r#"[{from_state}]="{joined_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    declare -A nontail_regexes_level_{level}=({regexes_initializer})"#
        )?;
    }

    write!(
        buffer,
        r#"
    local -a candidates=()
    local -a matches=()
    local ignore_case=$(bind -v | grep completion-ignore-case | cut -d' ' -f3)
    local max_fallback_level={max_fallback_level}
    local prefix="${{words[$cword]}}"
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )) {{
        eval "declare literal_transitions_name=literal_transitions_level_${{fallback_level}}"
        eval "declare -a transitions=(\${{$literal_transitions_name[$state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal="${{literals[$literal_id]}}"
            candidates+=("$literal ")
        done
        if [[ ${{#candidates[@]}} -gt 0 ]]; then
            readarray -t matches < <(printf "%s\n" "${{candidates[@]}}" | {MATCH_FN_NAME} "$ignore_case" "$prefix")
        fi

        eval "declare subword_transitions_name=subword_transitions_level_${{fallback_level}}"
        eval "declare -a transitions=(\${{$subword_transitions_name[$state]}})"
        for subword_id in "${{transitions[@]}}"; do
            readarray -t -O "${{#matches[@]}}" matches < <(_{command}_subword_$subword_id complete "$prefix")
        done

        eval "declare commands_name=commands_level_${{fallback_level}}"
        eval "declare -a transitions=(\${{$commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            readarray -t candidates < <(_{command}_cmd_$command_id "$prefix" | cut -f1)
            if [[ ${{#candidates[@]}} -gt 0 ]]; then
                readarray -t -O "${{#matches[@]}}" matches < <(printf "%s\n" "${{candidates[@]}}" | {MATCH_FN_NAME} "$ignore_case" "$prefix")
            fi
        done

        eval "declare commands_name=nontail_commands_level_${{fallback_level}}"
        eval "declare -a command_transitions=(\${{$commands_name[$state]}})"
        eval "declare regexes_name=nontail_regexes_level_${{fallback_level}}"
        eval "declare -a regexes_transitions=(\${{$regexes_name[$state]}})"
        for (( i = 0; i < ${{#command_transitions[@]}}; i++ )); do
            local command_id=${{command_transitions[$i]}}
            local regex_id=${{regexes_transitions[$i]}}
            local regex="^(${{regexes[$regex_id]}}).*"
            readarray -t output < <(_{command}_cmd_$command_id "$prefix" | cut -f1)
            declare -a candidates=()
            for line in ${{output[@]}}; do
                if [[ ${{line}} =~ $regex && -n ${{BASH_REMATCH[1]}} ]]; then
                    match="${{BASH_REMATCH[1]}}"
                    candidates+=("$match")
                fi
            done
            if [[ ${{#candidates[@]}} -gt 0 ]]; then
                readarray -t -O "${{#matches[@]}}" matches < <(printf "%s\n" "${{candidates[@]}}" | {MATCH_FN_NAME} "$ignore_case" "$prefix")
            fi
        done

        if [[ ${{#matches[@]}} -gt 0 ]]; then
            local shortest_suffix="$prefix"
            for ((i=0; i < ${{#COMP_WORDBREAKS}}; i++)); do
                local char="${{COMP_WORDBREAKS:$i:1}}"
                local candidate=${{prefix##*$char}}
                if [[ ${{#candidate}} -lt ${{#shortest_suffix}} ]]; then
                    shortest_suffix=$candidate
                fi
            done
            local superfluous_prefix=""
            if [[ "$shortest_suffix" != "$prefix" ]]; then
                local superfluous_prefix=${{prefix%$shortest_suffix}}
            fi
            COMPREPLY=("${{matches[@]#$superfluous_prefix}}")
            break
        fi
    }}
"#
    )?;

    write!(
        buffer,
        r#"
    return 0
}}

complete -o nospace -F _{command} {command}
"#
    )?;
    Ok(())
}
