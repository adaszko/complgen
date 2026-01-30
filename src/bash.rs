use std::io::Write;

use crate::Result;
use crate::StateId;
use crate::dfa::DFA;
use crate::dfa::Inp;
use hashbrown::HashMap;
use indexmap::IndexSet;
use ustr::{Ustr, ustr};

// Bash array indexes start at 0.
// Associative arrays are local by default.
// Bash uses *dynamic* scoping for local variables (!)

pub const ARRAY_START: u32 = 0;
pub const MATCH_FN_NAME: &str = "__complgen_match";

fn make_string_constant(s: &str) -> String {
    format!(
        r#""{}""#,
        s.replace('\"', "\\\"")
            .replace('`', "\\`")
            .replace('$', "\\$")
    )
}

fn write_lookup_tables<W: Write>(
    buffer: &mut W,
    dfa: &DFA,
    id_from_regex: &IndexSet<Ustr>,
    needs_nontails_code: bool,
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
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    writeln!(buffer, r#"    local -A literal_transitions=()"#)?;

    if needs_nontails_code {
        let regexes: String = itertools::join(
            id_from_regex
                .iter()
                .map(|regex| make_string_constant(regex)),
            " ",
        );
        writeln!(buffer, r#"    local -a regexes=({regexes})"#)?;
        writeln!(buffer, r#"    local -A nontail_transitions=()"#)?;
    }
    for state in dfa.get_all_states() {
        let literal_transitions = dfa.get_literal_transitions_from(state);
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

        if needs_nontails_code {
            let nontail_transitions = dfa.get_nontail_transitions_from(state as StateId);
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
    }

    let star_transitions = itertools::join(
        dfa.iter_top_level_star_transitions()
            .map(|(from, to)| format!("[{from}]={to}")),
        " ",
    );
    writeln!(
        buffer,
        r#"    local -A star_transitions=({star_transitions})"#
    )?;

    Ok(literal_id_from_input_description)
}

fn write_generic_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    needs_nontails_code: bool,
    needs_commands_code: bool,
) -> Result<()> {
    writeln!(
        buffer,
        r#"_{command}_subword () {{
    [[ $# -ne 2 ]] && return 1
    local mode=$1
    local word=$2

    local char_index=0
    local matched=0
    local nliterals=${{#literals[@]}}
    while true; do
        if [[ $char_index -ge ${{#word}} ]]; then
            matched=1
            break
        fi

        local subword=${{word:$char_index}}

        if [[ -v "literal_transitions[$state]" ]]; then
            local -A state_transitions=${{literal_transitions[$state]}}

            local literal_matched=0
            for ((literal_id = 0; literal_id < nliterals; literal_id++)); do
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
        fi"#
    )?;

    if needs_nontails_code {
        write!(
            buffer,
            r#"
        if [[ -v "nontail_transitions[$state]" ]]; then
            local -A state_nontails=${{nontail_transitions[$state]}}
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
        fi"#
        )?;
    }

    write!(
        buffer,
        r#"
        if [[ -v "star_transitions[$state]" ]]; then
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

    local -a subword_candidates=()
    local -a subword_matches=()
    for (( subword_fallback_level=0; subword_fallback_level <= max_fallback_level; subword_fallback_level++ )) {{
        eval "local literal_transitions_name=literal_transitions_level_${{subword_fallback_level}}"
        eval "local -a transitions=(\${{$literal_transitions_name[$state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal=${{literals[$literal_id]}}
            subword_candidates+=("$matched_prefix$literal")
        done
        if [[ ${{#subword_candidates[@]}} -gt 0 ]]; then
            {MATCH_FN_NAME} "$matched_prefix$completed_prefix" subword_candidates subword_matches
        fi"#
    )?;

    if needs_nontails_code {
        writeln!(
            buffer,
            r#"
        eval "local commands_name=nontail_commands_level_${{subword_fallback_level}}"
        eval "local -a command_transitions=(\${{$commands_name[$state]}})"
        eval "local regexes_name=nontail_regexes_level_${{fallback_level}}"
        eval "local -a regexes_transitions=(\${{$regexes_name[$state]}})"
        for (( i = 0; i < ${{#command_transitions[@]}}; i++ )); do
            local command_id=${{command_transitions[$i]}}
            readarray -t output < <(_{command}_cmd_$command_id "$completed_prefix" "$matched_prefix" | while read -r f1 _; do echo "$f1"; done)
            local regex_id=${{regexes_transitions[$i]}}
            local regex="^(${{regexes[$regex_id]}}).*"
            local -a subword_candidates=()
            for line in "${{output[@]}}"; do
                if [[ ${{line}} =~ $regex && -n ${{BASH_REMATCH[1]}} ]]; then
                    match="${{BASH_REMATCH[1]}}"
                    subword_candidates+=("$match")
                fi
            done
            for item in "${{subword_candidates[@]}}"; do
                subword_matches+=("$matched_prefix$item")
            done
        done"#
        )?;
    }

    if needs_commands_code {
        writeln!(
            buffer,
            r#"
        eval "local commands_name=commands_level_${{subword_fallback_level}}"
        eval "local -a transitions=(\${{$commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            readarray -t subword_candidates < <(_{command}_cmd_$command_id "$completed_prefix" "$matched_prefix" | while read -r f1 _; do echo "$f1"; done)
            for item in "${{subword_candidates[@]}}"; do
                subword_matches+=("$matched_prefix$item")
            done
        done"#
        )?;
    }

    write!(
        buffer,
        r#"
        if [[ ${{#subword_matches[@]}} -gt 0 ]]; then
            matches+=("${{subword_matches[@]}}")
            break
        fi
    }}"#
    )?;

    writeln!(
        buffer,
        r#"
    return 0
}}"#
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
    needs_nontails_code: bool,
    needs_commands_code: bool,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    let id_from_regex = dfa.get_regexes();

    let literal_id_from_input_description =
        write_lookup_tables(buffer, dfa, &id_from_regex, needs_nontails_code)?;

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    let mut completion_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut completion_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut completion_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    completion_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input_id, _) in dfa.iter_transitions() {
        match dfa.get_input(input_id) {
            Inp::Literal {
                literal,
                description,
                fallback_level,
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(*literal, description.unwrap_or("".into())))
                    .unwrap();
                completion_literals[*fallback_level]
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
                let command_id = id_from_cmd.get_index_of(cmd).unwrap();
                completion_commands[*fallback_level]
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
                let cmd_id = id_from_cmd.get_index_of(cmd).unwrap();
                let regex_id = id_from_regex.get_index_of(rx).unwrap();
                completion_nontails[*fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Inp::Star => {}
            Inp::Subword { .. } => unreachable!(),
        }
    }

    for (level, transitions) in completion_literals.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, literal_ids)| {
                let joined_literal_ids = itertools::join(literal_ids, " ");
                format!(r#"[{from_state}]="{joined_literal_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    local -A literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    if needs_nontails_code {
        for (level, transitions) in completion_nontails.iter().enumerate() {
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
                r#"    local -A nontail_commands_level_{level}=({commands_initializer})"#
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
                r#"    local -A nontail_regexes_level_{level}=({regexes_initializer})"#
            )?;
        }
    }

    if needs_commands_code {
        for (level, transitions) in completion_commands.iter().enumerate() {
            let initializer = itertools::join(
                transitions.iter().map(|(from_state, command_ids)| {
                    let joined_command_ids = itertools::join(command_ids, " ");
                    format!(r#"[{from_state}]="{joined_command_ids}""#)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    local -A commands_level_{level}=({initializer})"#
            )?;
        }
    }

    writeln!(
        buffer,
        r#"    local max_fallback_level={max_fallback_level}"#
    )?;
    writeln!(
        buffer,
        r#"    local state={starting_state}"#,
        starting_state = dfa.starting_state
    )?;

    writeln!(buffer, r#"    _{command}_subword "$1" "$2""#)?;

    writeln!(buffer, r#"}}"#)?;

    Ok(())
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let needs_subwords_code = dfa.needs_subwords_code();
    let needs_nontails_code = dfa.needs_nontails_code();
    let needs_subword_nontails_code = dfa.needs_subword_nontails_code();
    let needs_commands_code = dfa.needs_commands_code();
    let needs_subword_commands_code = dfa.needs_subword_commands_code();

    write!(
        buffer,
        r#"if [[ $BASH_VERSINFO -lt 4 ]]; then
    echo "This completion script requires bash 4.0 or newer (current is $BASH_VERSION)"
    exit 1
fi

"#
    )?;

    let id_from_cmd = dfa.get_commands();
    for cmd in &id_from_cmd {
        let id = id_from_cmd.get_index_of(cmd).unwrap();
        let mut cmd = cmd.trim();
        if cmd.is_empty() {
            // Edge case: bash syntax errors on empty function bodies
            cmd = ":"
        }
        writeln!(
            buffer,
            r#"_{command}_cmd_{id} () {{
    {cmd}
}}
"#
        )?;
    }

    let id_from_dfa = dfa.get_subwords(ARRAY_START as usize);
    let id_from_regex = dfa.get_regexes();
    if needs_subwords_code {
        write_generic_subword_fn(
            buffer,
            command,
            needs_nontails_code,
            needs_subword_commands_code,
        )?;
        for (dfaid, id) in &id_from_dfa {
            let dfa = dfa.subdfas.lookup(*dfaid);
            write_subword_fn(
                buffer,
                command,
                *id,
                dfa,
                &id_from_cmd,
                needs_subword_nontails_code,
                needs_subword_commands_code,
            )?;
            writeln!(buffer)?;
        }
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

    let literal_id_from_input_description =
        write_lookup_tables(buffer, dfa, &id_from_regex, needs_nontails_code)?;

    if needs_subwords_code {
        writeln!(buffer, r#"    local -A subword_transitions"#)?;
        for state in dfa.get_all_states() {
            let subword_transitions = dfa.get_subword_transitions_from(state);
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
    }

    write!(
        buffer,
        r#"
    local state={starting_state}
    local word_index=1
    local nliterals=${{#literals[@]}}
    while [[ $word_index -lt $cword ]]; do
        local word=${{words[$word_index]}}

        if [[ -v "literal_transitions[$state]" ]]; then
            local -A state_transitions=${{literal_transitions[$state]}}

            local word_matched=0
            for ((literal_id = 0; literal_id < nliterals; literal_id++)); do
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

    if needs_subwords_code {
        write!(
            buffer,
            r#"
        if [[ -v "subword_transitions[$state]" ]]; then
            local -A state_transitions=${{subword_transitions[$state]}}

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

    if needs_nontails_code {
        writeln!(
            buffer,
            r#"
        if [[ -v "nontail_transitions[$state]" ]]; then
            local -A state_nontails=${{nontail_transitions[$state]}}

            local nontail_matched=0
            for regex_id in "${{!state_nontails[@]}}"; do
                local regex="^(${{regexes[$regex_id]}}).*"
                if [[ $word =~ $regex ]]; then
                    word_index=$((word_index + 1))
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
    }

    write!(
        buffer,
        r#"
        if [[ -v "star_transitions[$state]" ]]; then
            state=${{star_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi

        return 1
    done

"#
    )?;

    // ///////////////////////////// Completion ///////////////////////////////////

    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(ARRAY_START as usize);

    let mut completion_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut completion_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut completion_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    completion_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut completion_nontails: Vec<HashMap<StateId, Vec<(usize, usize)>>> = Default::default();
    completion_nontails.resize_with(max_fallback_level + 1, Default::default);

    for (from, input_id, _) in dfa.iter_transitions() {
        match dfa.get_input(input_id).clone() {
            Inp::Literal {
                literal,
                description,
                fallback_level,
            } => {
                let literal_id = *literal_id_from_input_description
                    .get(&(literal, description.unwrap_or("".into())))
                    .unwrap();
                completion_literals[fallback_level]
                    .entry(from)
                    .or_default()
                    .push(literal_id);
            }
            Inp::Subword {
                subdfa,
                fallback_level,
            } => {
                let subword_id = *id_from_dfa.get(&subdfa).unwrap();
                completion_subwords[fallback_level]
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
                completion_commands[fallback_level]
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
                completion_nontails[fallback_level]
                    .entry(from)
                    .or_default()
                    .push((cmd_id, regex_id));
            }
            Inp::Star => {}
        }
    }

    for (level, transitions) in completion_literals.iter().enumerate() {
        let initializer = itertools::join(
            transitions.iter().map(|(from_state, literal_ids)| {
                let joined_literal_ids = itertools::join(literal_ids, " ");
                format!(r#"[{from_state}]="{joined_literal_ids}""#)
            }),
            " ",
        );
        writeln!(
            buffer,
            r#"    local -A literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    if needs_subwords_code {
        for (level, transitions) in completion_subwords.iter().enumerate() {
            let initializer = itertools::join(
                transitions.iter().map(|(from_state, subword_ids)| {
                    let joined_subword_ids = itertools::join(subword_ids, " ");
                    format!(r#"[{from_state}]="{joined_subword_ids}""#)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    local -A subword_transitions_level_{level}=({initializer})"#
            )?;
        }
    }

    if needs_commands_code {
        for (level, transitions) in completion_commands.iter().enumerate() {
            let initializer = itertools::join(
                transitions.iter().map(|(from_state, command_ids)| {
                    let joined_command_ids = itertools::join(command_ids, " ");
                    format!(r#"[{from_state}]="{joined_command_ids}""#)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    local -A commands_level_{level}=({initializer})"#
            )?;
        }
    }

    if needs_nontails_code {
        for (level, transitions) in completion_nontails.iter().enumerate() {
            let commands_initializer = itertools::join(
                transitions.iter().map(|(from_state, ids)| {
                    let joined_ids = itertools::join(ids.iter().map(|(cmd_id, _)| cmd_id), " ");
                    format!(r#"[{from_state}]="{joined_ids}""#)
                }),
                " ",
            );
            writeln!(
                buffer,
                r#"    local -A nontail_commands_level_{level}=({commands_initializer})"#
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
                r#"    local -A nontail_regexes_level_{level}=({regexes_initializer})"#
            )?;
        }
    }

    write!(
        buffer,
        r#"
    local ignore_case=$(bind -v | while read -r _ var value; do [[ $var = completion-ignore-case ]] && echo $value; done)
    if [[ $ignore_case = on ]]; then
        {MATCH_FN_NAME} () {{
            [[ $# -lt 3 ]] && return 1
            local prefix=$1
            declare -n candidates_=$2
            declare -n matches_=$3
            if [[ -z $prefix ]]; then
                matches_+=("${{candidates_[@]}}")
            else
                prefix=${{prefix,,}}
                prefix=$(printf '%q' "$prefix")
                for line in "${{candidates_[@]}}"; do
                    [[ ${{line,,}} = ${{prefix}}* ]] && matches_+=("$line")
                done
            fi
        }}
    else
        {MATCH_FN_NAME} () {{
            [[ $# -lt 3 ]] && return 1
            local prefix=$1
            declare -n candidates_=$2
            declare -n matches_=$3
            if [[ -z $prefix ]]; then
                matches_+=("${{candidates_[@]}}")
            else
                prefix=$(printf '%q' "$prefix")
                for line in "${{candidates_[@]}}"; do
                    [[ $line = ${{prefix}}* ]] && matches_+=("$line")
                done
            fi
        }}
    fi


    local -a candidates=()
    local -a matches=()
    local max_fallback_level={max_fallback_level}
    local prefix="${{words[$cword]}}"
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )) {{
        eval "local literal_transitions_name=literal_transitions_level_${{fallback_level}}"
        eval "local -a transitions=(\${{$literal_transitions_name[$state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal="${{literals[$literal_id]}}"
            candidates+=("$literal ")
        done
        if [[ ${{#candidates[@]}} -gt 0 ]]; then
            {MATCH_FN_NAME} "$prefix" candidates matches
        fi"#
    )?;

    if needs_subwords_code {
        write!(
            buffer,
            r#"
        eval "local subword_transitions_name=subword_transitions_level_${{fallback_level}}"
        eval "local -a transitions=(\${{$subword_transitions_name[$state]}})"
        for subword_id in "${{transitions[@]}}"; do
            _{command}_subword_$subword_id complete "$prefix"
        done"#
        )?;
    }

    if needs_commands_code {
        write!(
            buffer,
            r#"
        eval "local commands_name=commands_level_${{fallback_level}}"
        eval "local -a transitions=(\${{$commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            readarray -t candidates < <(_{command}_cmd_$command_id "$prefix" "" | while read -r f1 _; do echo "$f1"; done)
            if [[ ${{#candidates[@]}} -gt 0 ]]; then
                {MATCH_FN_NAME} "$prefix" candidates matches
            fi
        done"#
        )?;
    }

    if needs_nontails_code {
        write!(
            buffer,
            r#"
        eval "local commands_name=nontail_commands_level_${{fallback_level}}"
        eval "local -a command_transitions=(\${{$commands_name[$state]}})"
        eval "local regexes_name=nontail_regexes_level_${{fallback_level}}"
        eval "local -a regexes_transitions=(\${{$regexes_name[$state]}})"
        for (( i = 0; i < ${{#command_transitions[@]}}; i++ )); do
            local command_id=${{command_transitions[$i]}}
            local regex_id=${{regexes_transitions[$i]}}
            local regex="^(${{regexes[$regex_id]}}).*"
            readarray -t output < <(_{command}_cmd_$command_id "$prefix" "" | while read -r f1 _; do echo "$f1"; done)
            local -a candidates=()
            for line in "${{output[@]}}"; do
                if [[ ${{line}} =~ $regex && -n ${{BASH_REMATCH[1]}} ]]; then
                    match="${{BASH_REMATCH[1]}}"
                    candidates+=("$match")
                fi
            done
            if [[ ${{#candidates[@]}} -gt 0 ]]; then
                {MATCH_FN_NAME} "$prefix" candidates matches
            fi
        done"#
        )?;
    }

    write!(
        buffer,
        r#"
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
