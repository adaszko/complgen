use std::io::Write;

use crate::Result;
use crate::dfa::DFA;
use crate::signature;
use crate::tables::CompletionTransitions;
use crate::tables::LookupTables;
use crate::tables::MatchTransitions;
use crate::tables::get_lookup_tables;
use hashbrown::HashMap;
use itertools::Itertools;
use ustr::Ustr;

// Bash array indexes start at 0.
// Associative arrays are local by default.
// Bash uses *dynamic* scoping for local variables (!)
// `declare -n` can be a more readable eval substitute
// Under Bash, the completion script is responsible for filtering candidates (!)

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

fn write_subword_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    needs_commands_code: bool,
    needs_star_code: bool,
) -> Result<()> {
    writeln!(
        buffer,
        r#"_{command}_subword () {{
    [[ $# -ne 2 ]] && return 1
    local mode=$1
    local word=$2

    local subword_state=0
    local char_index=0
    local matched=0
    local nliterals=${{#literals[@]}}
    while true; do
        if [[ $char_index -ge ${{#word}} ]]; then
            matched=1
            break
        fi

        local subword=${{word:$char_index}}

        if [[ -v "literal_transitions[$subword_state]" ]]; then
            local -A state_transitions=${{literal_transitions[$subword_state]}}

            for ((literal_id = 0; literal_id < nliterals; literal_id++)); do
                local literal=${{literals[$literal_id]}}
                if [[ $subword == $literal && -v "state_transitions[$literal_id]" ]]; then
                    subword_state=${{state_transitions[$literal_id]}}
                    char_index=$((char_index + ${{#literal}}))
                    continue 2
                fi
                if [[ $literal == $subword* ]]; then
                    break 2
                fi
                if [[ $subword == $literal* && -v "state_transitions[$literal_id]" ]]; then
                    subword_state=${{state_transitions[$literal_id]}}
                    char_index=$((char_index + ${{#literal}}))
                    continue 2
                fi
            done
        fi"#
    )?;

    if needs_commands_code {
        write!(
            buffer,
            r#"
        if [[ -v "command_transitions[$subword_state]" ]]; then
            local matched_prefix="${{word:0:$char_index}}"
            local -A state_commands=${{command_transitions[$subword_state]}}
            for cmd_id in "${{!state_commands[@]}}"; do
                readarray -t subword_candidates < <(_{command}_cmd_$cmd_id "$subword" "$matched_prefix" | while read -r f1 _; do echo "$f1"; done)
                if [[ ${{#subword_candidates[@]}} -gt 0 ]]; then
                    indexes=($(
                        for i in "${{!subword_candidates[@]}}"; do
                            printf '%s %s %s\n' $i "${{#subword_candidates[i]}}" "${{subword_candidates[i]}}"
                        done | sort -nrk2,2 -rk3 | cut -f1 -d' '
                    ))
                    decreasing_length=()
                    for i in "${{indexes[@]}}" ; do
                        decreasing_length+=("${{subword_candidates[i]}}")
                    done

                    for candidate in "${{decreasing_length[@]}}"; do
                        if [[ $candidate == $subword ]]; then
                            match_len=${{#candidate}}
                            char_index=$((char_index + match_len))
                            subword_state=${{state_commands[$cmd_id]}}
                            continue 3
                        fi

                        if [[ $candidate == $subword* ]]; then
                            break 3
                        fi

                        if [[ $subword == $candidate* ]]; then
                            match_len=${{#candidate}}
                            char_index=$((char_index + match_len))
                            subword_state=${{state_commands[$cmd_id]}}
                            continue 3
                        fi
                    done
                fi
            done
        fi
"#
        )?;
    }

    if needs_star_code {
        write!(
            buffer,
            r#"
        if [[ -v "star_transitions[$subword_state]" ]]; then
            matched=1
            break
        fi
"#
        )?;
    }

    write!(
        buffer,
        r#"
        break
    done
"#
    )?;

    write!(
        buffer,
        r#"
    if [[ $mode = matches ]]; then
        return $((1 - matched))
    fi
"#
    )?;

    // /////////////// Completion /////////////////////////

    write!(
        buffer,
        r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    local -a subword_candidates=()
    local -a subword_matches=()
    for (( subword_fallback_level=0; subword_fallback_level <= max_fallback_level; subword_fallback_level++ )) {{
        eval "local literal_transitions_name=literal_transitions_level_${{subword_fallback_level}}"
        eval "local -a transitions=(\${{$literal_transitions_name[$subword_state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal=${{literals[$literal_id]}}
            subword_candidates+=("$matched_prefix$literal")
        done
        {MATCH_FN_NAME} "$matched_prefix$completed_prefix" subword_candidates subword_matches
"#
    )?;

    if needs_commands_code {
        writeln!(
            buffer,
            r#"
        eval "local commands_name=commands_level_${{subword_fallback_level}}"
        eval "local -a transitions=(\${{$commands_name[$subword_state]}})"
        for command_id in "${{transitions[@]}}"; do
            readarray -t subword_candidates < <(_{command}_cmd_$command_id "$completed_prefix" "$matched_prefix" | while read -r f1 _; do echo "$f1"; done)
            local -a filtered_candidates=()
            {MATCH_FN_NAME} "$completed_prefix" subword_candidates filtered_candidates
            for item in "${{filtered_candidates[@]}}"; do
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

fn write_match_transitions<W: Write>(buffer: &mut W, tables: &MatchTransitions) -> Result<()> {
    writeln!(buffer, r#"    local -A literal_transitions=()"#)?;
    for (state, state_transitions) in &tables.literal {
        let transitions = state_transitions
            .iter()
            .map(|(literal_id, to)| format!("[{literal_id}]={to}"))
            .join(" ");
        writeln!(
            buffer,
            r#"    literal_transitions[{state}]="({transitions})""#
        )?;
    }

    if let Some(command_transitions) = &tables.command {
        writeln!(buffer, r#"    local -A command_transitions=()"#)?;
        for (state, state_transitions) in command_transitions {
            let transitions = state_transitions
                .iter()
                .map(|(cmd_id, to)| format!("[{cmd_id}]={to}"))
                .join(" ");
            writeln!(
                buffer,
                r#"    command_transitions[{state}]="({transitions})""#
            )?;
        }
    }

    if let Some(star_transitions) = &tables.star {
        let transitions = star_transitions
            .iter()
            .map(|(from, to)| format!("[{from}]={to}"))
            .join(" ");
        writeln!(buffer, r#"    local -A star_transitions=({transitions})"#)?;
    }

    Ok(())
}

fn write_literals<W: Write>(buffer: &mut W, literals: &[Ustr]) -> Result<()> {
    let literals = literals
        .iter()
        .map(|lit| make_string_constant(lit))
        .join(" ");
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    Ok(())
}

fn write_completion_tables<W: Write>(
    buffer: &mut W,
    max_fallback_level: usize,
    completions: &CompletionTransitions,
) -> Result<()> {
    for (level, transitions) in completions.literal.iter().enumerate() {
        let initializer = transitions
            .iter()
            .map(|(from_state, literal_ids)| {
                format!(r#"[{from_state}]="{}""#, literal_ids.iter().join(" "))
            })
            .join(" ");
        writeln!(
            buffer,
            r#"    local -A literal_transitions_level_{level}=({initializer})"#
        )?;
    }

    if let Some(command_transitions) = &completions.command {
        for (level, transitions) in command_transitions.iter().enumerate() {
            let initializer = transitions
                .iter()
                .map(|(from_state, command_ids)| {
                    format!(r#"[{from_state}]="{}""#, command_ids.iter().join(" "))
                })
                .join(" ");
            writeln!(
                buffer,
                r#"    local -A commands_level_{level}=({initializer})"#
            )?;
        }
    }

    writeln!(
        buffer,
        r#"    local max_fallback_level={max_fallback_level}"#,
    )?;

    Ok(())
}

fn write_subword_wrapper_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    lookups: &LookupTables,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    write_literals(
        buffer,
        &lookups
            .all_literals
            .iter()
            .map(|(_, l, _)| *l)
            .collect::<Vec<_>>(),
    )?;
    write_match_transitions(buffer, &lookups.match_transitions)?;
    write_completion_tables(
        buffer,
        lookups.completion_transitions.max_fallback_level,
        &lookups.completion_transitions,
    )?;

    writeln!(buffer, r#"    _{command}_subword "$1" "$2""#)?;

    writeln!(buffer, r#"}}"#)?;

    Ok(())
}

fn write_subword_shape_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    shape_id: usize,
    lookups: &LookupTables,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_shape_{shape_id} () {{"#)?;

    write_match_transitions(buffer, &lookups.match_transitions)?;
    write_completion_tables(
        buffer,
        lookups.completion_transitions.max_fallback_level,
        &lookups.completion_transitions,
    )?;

    writeln!(buffer, r#"    _{command}_subword "$1" "$2""#)?;

    writeln!(buffer, r#"}}"#)?;

    Ok(())
}

fn write_subword_shape_wrapper_fn<W: Write>(
    buffer: &mut W,
    command: &str,
    id: usize,
    shape_id: usize,
    lookups: &LookupTables,
) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;
    write_literals(
        buffer,
        &lookups
            .all_literals
            .iter()
            .map(|(_, l, _)| *l)
            .collect::<Vec<_>>(),
    )?;
    writeln!(
        buffer,
        r#"    _{command}_subword_shape_{shape_id} "$1" "$2""#
    )?;
    writeln!(buffer, r#"}}"#)?;
    Ok(())
}

pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let needs_subwords_code = dfa.needs_subwords_code();
    let needs_top_level_commands_code = dfa.needs_top_level_commands_code();
    let needs_subword_commands_code = dfa.needs_subword_commands_code();
    let needs_top_level_star_code = dfa.needs_top_level_star_code();
    let needs_subword_star_code = dfa.needs_subword_star_code();

    writeln!(buffer, "# {}", signature(command))?;

    write!(
        buffer,
        r#"
if [[ ${{BASH_VERSINFO[0]}} -lt 4 ]]; then
    echo "This completion script requires bash 4.0 or newer (current is $BASH_VERSION)"
    exit 1
fi

"#,
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
    if needs_subwords_code {
        let tables_from_id = {
            let mut lookup_tables: HashMap<usize, LookupTables> = Default::default();
            for (dfaid, id) in &id_from_dfa {
                let subdfa = dfa.subdfas.lookup(*dfaid);
                let tables = get_lookup_tables(
                    subdfa,
                    &id_from_cmd,
                    ARRAY_START as usize,
                    needs_subword_commands_code,
                    false,
                    needs_subword_star_code,
                );
                lookup_tables.insert(*id, tables);
            }
            lookup_tables
        };

        let mut hashes: Vec<(usize, u64)> = Default::default();
        for (id, table) in &tables_from_id {
            hashes.push((*id, table.shape_hash()));
        }
        hashes.sort_by_key(|(_, hash)| *hash);

        let isomorphic_subwords =
            hashes.chunk_by(|(left_id, left_hash), (right_id, right_hash)| {
                if left_hash != right_hash {
                    return false;
                }

                let left = tables_from_id.get(left_id).unwrap();
                let right = tables_from_id.get(right_id).unwrap();

                left.isomorphic_to(&right)
            });

        for (shape_id, chunk) in isomorphic_subwords.enumerate() {
            if chunk.len() > 1 {
                let (chunk_leader_id, _) = chunk[0];
                let chunk_leader_tables = tables_from_id.get(&chunk_leader_id).unwrap();
                write_subword_shape_fn(buffer, command, shape_id, chunk_leader_tables)?;
                writeln!(buffer)?;
                for (id, _) in chunk {
                    let tables = tables_from_id.get(id).unwrap();
                    write_subword_shape_wrapper_fn(buffer, command, *id, shape_id, tables)?;
                    writeln!(buffer)?;
                }
            } else {
                let [(id, _)] = chunk else { unreachable!() };
                let tables = tables_from_id.get(id).unwrap();
                write_subword_wrapper_fn(buffer, command, *id, &tables)?;
                writeln!(buffer)?;
            }
        }

        write_subword_fn(
            buffer,
            command,
            needs_subword_commands_code,
            needs_subword_star_code,
        )?;
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

    let lookups = get_lookup_tables(
        dfa,
        &id_from_cmd,
        ARRAY_START as usize,
        needs_top_level_commands_code,
        false,
        needs_top_level_star_code,
    );
    let literals: Vec<Ustr> = lookups.all_literals.iter().map(|(_, l, _)| *l).collect();
    write_literals(buffer, &literals)?;
    write_match_transitions(buffer, &lookups.match_transitions)?;

    if needs_subwords_code {
        writeln!(buffer, r#"    local -A subword_transitions"#)?;
        for state in dfa.get_all_states() {
            let subword_transitions = dfa.get_subword_transitions_from(state);
            if subword_transitions.is_empty() {
                continue;
            }
            let state_transitions: String = subword_transitions
                .into_iter()
                .map(|(dfa, to)| format!("[{}]={}", id_from_dfa.get(&dfa).unwrap(), to))
                .join(" ");
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

            for ((literal_id = 0; literal_id < nliterals; literal_id++)); do
                if [[ ${{literals[$literal_id]}} = "$word" ]]; then
                    if [[ -v "state_transitions[$literal_id]" ]]; then
                        state=${{state_transitions[$literal_id]}}
                        word_index=$((word_index + 1))
                        continue 2
                    fi
                fi
            done
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

            for subword_id in "${{!state_transitions[@]}}"; do
                if _{command}_subword_"${{subword_id}}" matches "$word"; then
                    state=${{state_transitions[$subword_id]}}
                    word_index=$((word_index + 1))
                    continue 2
                fi
            done
        fi
"#
        )?;
    }

    if needs_top_level_commands_code {
        write!(
            buffer,
            r#"
        if [[ -v "command_transitions[$state]" ]]; then
            local -A state_commands=${{command_transitions[$state]}}
            for cmd_id in "${{!state_commands[@]}}"; do
                readarray -t candidates < <(_{command}_cmd_$cmd_id "" "" | while read -r f1 _; do echo "$f1"; done)
                if [[ ${{#candidates[@]}} -gt 0 ]]; then
                    indexes=($(
                        for i in "${{!candidates[@]}}" ; do
                            printf '%s %s %s\n' $i "${{#candidates[i]}}" "${{candidates[i]}}"
                        done | sort -nrk2,2 -rk3 | cut -f1 -d' '
                    ))
                    decreasing_length=()
                    for i in "${{indexes[@]}}"; do
                        decreasing_length+=("${{candidates[i]}}")
                    done

                    for candidate in "${{decreasing_length[@]}}"; do
                        if [[ $candidate == $word ]]; then
                            state=${{state_commands[$cmd_id]}}
                            word_index=$((word_index + 1))
                            continue 3
                        fi
                    done

                    if [[ $(($word_index + 1)) == $cword ]]; then
                        break 3
                    fi
                fi
            done
        fi
"#
        )?;
    }

    if needs_top_level_star_code {
        write!(
            buffer,
            r#"
        if [[ -v "star_transitions[$state]" ]]; then
            state=${{star_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi
"#
        )?;
    }

    write!(
        buffer,
        r#"
        return 1
    done

"#
    )?;

    // ///////////////////////////// Completion ///////////////////////////////////

    write_completion_tables(
        buffer,
        lookups.completion_transitions.max_fallback_level,
        &lookups.completion_transitions,
    )?;

    if needs_subwords_code {
        for (level, transitions) in dfa
            .get_completion_subwords(
                id_from_dfa,
                lookups.completion_transitions.max_fallback_level,
            )
            .iter()
            .enumerate()
        {
            let initializer = transitions
                .iter()
                .map(|(from_state, subword_ids)| {
                    format!(r#"[{from_state}]="{}""#, subword_ids.iter().join(" "))
                })
                .join(" ");
            writeln!(
                buffer,
                r#"    local -A subword_transitions_level_{level}=({initializer})"#
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
        fi"#,
        max_fallback_level = lookups.completion_transitions.max_fallback_level
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

    if needs_top_level_commands_code {
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
