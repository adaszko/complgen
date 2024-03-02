use std::io::Write;

use crate::StateId;
use crate::Result;
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;


// Bash array indexes start at 0.
// Associative arrays are local by default.


pub fn make_string_constant(s: &str) -> String {
    format!(r#""{}""#, s.replace('\"', "\\\"").replace('`', "\\`").replace('$', "\\$"))
}


fn write_lookup_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, input, description)| ((*input, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| make_string_constant(literal)), " ");
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    declare -A literal_transitions"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if literal_transitions.is_empty() {
            continue;
        }
        let literal_transitions: Vec<(usize, StateId)> = literal_transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(literal_transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to)), " ");
        writeln!(buffer, r#"    literal_transitions[{state}]="({state_transitions})""#)?;
    }

    writeln!(buffer)?;

    writeln!(buffer, r#"    declare -A match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{from}]={to}")), " ");
    writeln!(buffer, r#"    match_anything_transitions=({match_anything_transitions})"#)?;

    Ok(())
}


fn write_subword_fn<W: Write>(buffer: &mut W, command: &str, id: usize, dfa: &DFA, command_id_from_state: &HashMap<StateId, usize>, subword_spec_id_from_state: &HashMap<StateId, usize>) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{
    [[ $# -ne 2 ]] && return 1
    local mode=$1
    local word=$2
"#)?;

    write_lookup_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
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
                    fi
                fi
            done
            if [[ $literal_matched -ne 0 ]]; then
                continue
            fi
        fi

        if [[ -v "match_anything_transitions[$state]" ]]; then
            state=${{match_anything_transitions[$state]}}
            matched=1
            break
        fi

        break
    done

    if [[ $mode = matches ]]; then
        return $((1 - matched))
    fi
"#, starting_state = dfa.starting_state)?;

    write!(buffer, r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    local shortest_suffix="$word"
    for ((i=0; i < ${{#COMP_WORDBREAKS}}; i++)); do
        local char="${{COMP_WORDBREAKS:$i:1}}"
        local candidate=${{word##*$char}}
        if [[ ${{#candidate}} -lt ${{#shortest_suffix}} ]]; then
            shortest_suffix=$candidate
        fi
    done
    local superfluous_prefix=""
    if [[ "$shortest_suffix" != "$word" ]]; then
        local superfluous_prefix=${{word%$shortest_suffix}}
    fi

    if [[ -v "literal_transitions[$state]" ]]; then
        declare -A state_transitions
        eval "state_transitions=${{literal_transitions[$state]}}"

        for literal_id in "${{!state_transitions[@]}}"; do
            local literal=${{literals[$literal_id]}}
            if [[ $literal = "${{completed_prefix}}"* ]]; then
                local completion="$matched_prefix$literal"
                completion=${{completion#"$superfluous_prefix"}}
                local to_state=${{state_transitions[$literal_id]}}
                if [[ -v "literal_transitions[$to_state]" || -v "match_anything_transitions[$to_state]" ]]; then
                    echo $completion
                else
                    echo "$completion "
                fi
            fi
        done
    fi
"#)?;

    if !command_id_from_state.is_empty() {
        writeln!(buffer)?;
        writeln!(buffer, r#"    declare -A commands"#)?;
        let array_initializer = itertools::join(command_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        local completions=()
        mapfile -t completions < <(_{command}_subword_cmd_${{command_id}} "$matched_prefix" | cut -f1)
        for item in "${{completions[@]}}"; do
            echo "$item"
        done
    fi

"#)?;
    }

    if !subword_spec_id_from_state.is_empty() {
        writeln!(buffer)?;
        writeln!(buffer, r#"    declare -A specialized_commands"#)?;
        let array_initializer = itertools::join(subword_spec_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        local completions=()
        mapfile -t completions < <(_{command}_subword_spec_"${{command_id}}" "$prefix" | cut -f1)
        for item in "${{completions[@]}}"; do
            echo "$item"
        done
    fi

"#)?;
    }

    writeln!(buffer, r#"    return 0
}}"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let top_level_command_transitions = dfa.get_command_transitions();
    let subword_command_transitions = dfa.get_subword_command_transitions();

    let id_from_top_level_command: UstrMap<usize> = top_level_command_transitions.iter().enumerate().map(|(id, (_, cmd))| (*cmd, id)).collect();
    for (cmd, id) in &id_from_top_level_command {
        write!(buffer, r#"_{command}_cmd_{id} () {{
    {cmd}
}}

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
        write!(buffer, r#"_{command}_subword_cmd_{id} () {{
    {cmd}
}}

"#)?;
    }

    let top_level_spec_transitions = dfa.get_bash_command_transitions();
    let subword_spec_transitions = dfa.get_bash_subword_command_transitions();
    let id_from_specialized_command: UstrMap<usize> = top_level_spec_transitions.iter().enumerate().map(|(id, (_, cmd))| (*cmd, id)).collect();
    for (cmd, id) in &id_from_specialized_command {
        write!(buffer, r#"_{command}_spec_{id} () {{
    {cmd}
}}

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
        write!(buffer, r#"_{command}_subword_spec_{id} () {{
    {cmd}
}}

"#)?;
    }


    let id_from_dfa = dfa.get_subwords(0);
    for (dfa, id) in &id_from_dfa {
        let subword_command_id_from_state: HashMap<StateId, usize> = subword_command_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_command.get(cmd).unwrap())).collect();
        let subword_spec_id_from_state: HashMap<StateId, usize> = subword_spec_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_spec.get(cmd).unwrap())).collect();
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &subword_command_id_from_state, &subword_spec_id_from_state)?;
        writeln!(buffer)?;
    }


    write!(buffer, r#"_{command} () {{"#)?;

    writeln!(buffer, r#"
    if [[ $(type -t _get_comp_words_by_ref) != function ]]; then
        echo _get_comp_words_by_ref: function not defined.  Make sure the bash-completions system package is installed
        return 1
    fi

    local words cword
    _get_comp_words_by_ref -n "$COMP_WORDBREAKS" words cword
"#)?;

    write_lookup_tables(buffer, dfa)?;

    writeln!(buffer, r#"    declare -A subword_transitions"#)?;
    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(subword_transitions.into_iter().map(|(dfa, to)| format!("[{}]={}", id_from_dfa.get(&dfa).unwrap(), to)), " ");
        writeln!(buffer, r#"    subword_transitions[{state}]="({state_transitions})""#)?;
    }

    write!(buffer, r#"
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
"#, starting_state = dfa.starting_state)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
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
"#)?;
    }

        write!(buffer, r#"
        if [[ -v "match_anything_transitions[$state]" ]]; then
            state=${{match_anything_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi

        return 1
    done

"#)?;

    write!(buffer, r#"
    local prefix="${{words[$cword]}}"

    local shortest_suffix="$word"
    for ((i=0; i < ${{#COMP_WORDBREAKS}}; i++)); do
        local char="${{COMP_WORDBREAKS:$i:1}}"
        local candidate="${{word##*$char}}"
        if [[ ${{#candidate}} -lt ${{#shortest_suffix}} ]]; then
            shortest_suffix=$candidate
        fi
    done
    local superfluous_prefix=""
    if [[ "$shortest_suffix" != "$word" ]]; then
        local superfluous_prefix=${{word%$shortest_suffix}}
    fi

    if [[ -v "literal_transitions[$state]" ]]; then
        local state_transitions_initializer=${{literal_transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        for literal_id in "${{!state_transitions[@]}}"; do
            local literal="${{literals[$literal_id]}}"
            if [[ $literal = "${{prefix}}"* ]]; then
                local completion=${{literal#"$superfluous_prefix"}}
                COMPREPLY+=("$completion ")
            fi
        done
    fi
"#)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
    if [[ -v "subword_transitions[$state]" ]]; then
        local state_transitions_initializer=${{subword_transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        for subword_id in "${{!state_transitions[@]}}"; do
            mapfile -t -O "${{#COMPREPLY[@]}}" COMPREPLY < <(_{command}_subword_"${{subword_id}}" complete "${{words[$cword]}}")
        done
    fi
"#)?;
    }

    let top_level_command_id_from_state: HashMap<StateId, usize> = top_level_command_transitions.into_iter().map(|(state, cmd)| (state, *id_from_top_level_command.get(&cmd).unwrap())).collect();
    if !top_level_command_id_from_state.is_empty() {
        writeln!(buffer, r#"    declare -A commands"#)?;
        let array_initializer = itertools::join(top_level_command_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        local completions=()
        mapfile -t completions < <(_{command}_cmd_${{command_id}} "$prefix" | cut -f1)
        for item in "${{completions[@]}}"; do
            if [[ $item = "${{prefix}}"* ]]; then
                COMPREPLY+=("$item")
            fi
        done
    fi

"#)?;
    }

    let specialized_command_id_from_state: HashMap<StateId, usize> = top_level_spec_transitions.into_iter().map(|(state, cmd)| (state, *id_from_specialized_command.get(&cmd).unwrap())).collect();
    if !specialized_command_id_from_state.is_empty() {
        writeln!(buffer)?;
        writeln!(buffer, r#"    declare -A specialized_commands"#)?;
        let array_initializer = itertools::join(specialized_command_id_from_state.into_iter().map(|(state, id)| format!("[{state}]={id}")), " ");
        write!(buffer, r#"    specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        local completions=()
        mapfile -t completions < <(_{command}_spec_"${{command_id}}" "$prefix" | cut -f1)
        for item in "${{completions[@]}}"; do
            if [[ $item = "${{prefix}}"* ]]; then
                COMPREPLY+=("$item")
            fi
        done
    fi

"#)?;
    }

    write!(buffer, r#"
    return 0
}}

complete -o nospace -F _{command} {command}
"#)?;
    Ok(())
}
