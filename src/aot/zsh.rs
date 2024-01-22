use std::io::Write;

use crate::{StateId, Result};
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;


// Array indexes in ZSH start from 1 (!)
// `for i in {{1..$#array}}; do ...; done` loops do not behave well if array is empty!  Prefer i++ loops instead.


pub fn make_string_constant(s: &str) -> String {
    format!(r#""{}""#, s.replace('\\', "\\\\").replace('\"', "\\\"").replace('`', "\\`").replace('$', "\\$"))
}


fn write_lookup_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, input, description)| ((*input, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| make_string_constant(literal)), " ");
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        if description.is_empty() {
            continue;
        }
        let quoted = make_string_constant(description);
        writeln!(buffer, r#"    descriptions[{id}]={quoted}"#)?;
    }
    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A literal_transitions"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if literal_transitions.is_empty() {
            continue;
        }
        let literal_transitions: Vec<(usize, StateId)> = literal_transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(literal_transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to + 1)), " ");
        writeln!(buffer, r#"    literal_transitions[{}]="({state_transitions})""#, state + 1)?;
    }

    writeln!(buffer)?;

    writeln!(buffer, r#"    local -A match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{}]={}", from + 1, to + 1)), " ");
    writeln!(buffer, r#"    match_anything_transitions=({match_anything_transitions})"#)?;

    Ok(())
}


fn write_subword_fn<W: Write>(buffer: &mut W, command: &str, id: usize, dfa: &DFA, command_id_from_state: &HashMap<StateId, usize>, spec_id_from_state: &HashMap<StateId, usize>) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{
    local mode=$1
    local word=$2
    local completions_no_description_trailing_space_array_name=$3
    local completions_trailing_space_array_name=$4
    local suffixes_trailing_space_array_name=$5
    local descriptions_trailing_space_array_name=$6
    local completions_no_description_no_trailing_space_array_name=$7
    local completions_no_trailing_space_array_name=$8
    local suffixes_no_trailing_space_array_name=$9
    local descriptions_no_trailing_space_array_name=${{10}}
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
            for ((literal_id = 1; literal_id <= $#literals; literal_id++)); do
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
"#, starting_state = dfa.starting_state + 1)?;

    write!(buffer, r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    if [[ -v "literal_transitions[$state]" ]]; then
        declare -A state_transitions
        eval "state_transitions=${{literal_transitions[$state]}}"

        for literal_id in ${{(k)state_transitions}}; do
            local literal=${{literals[$literal_id]}}
            if [[ $literal = "${{completed_prefix}}"* ]]; then
                local completion="$matched_prefix$literal"
                local to_state=${{state_transitions[$literal_id]}}
                if [[ -v "literal_transitions[$to_state]" || -v "match_anything_transitions[$to_state]" ]]; then
                    if [[ -v "descriptions[$literal_id]" ]]; then
                        eval "$completions_no_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$suffixes_no_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$descriptions_no_trailing_space_array_name+=(${{(qq)descriptions[$literal_id]}})"
                    else
                        eval "$completions_no_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$suffixes_no_trailing_space_array_name+=(${{(qq)literal}})"
                        eval "$descriptions_no_trailing_space_array_name+=('')"
                    fi
                else
                    if [[ -v "descriptions[$literal_id]" ]]; then
                        eval "$completions_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$suffixes_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$descriptions_trailing_space_array_name+=(${{(qq)descriptions[$literal_id]}})"
                    else
                        eval "$completions_trailing_space_array_name+=(${{(qq)completion}})"
                        eval "$suffixes_trailing_space_array_name+=(${{(qq)literal}})"
                        eval "$descriptions_trailing_space_array_name+=('')"
                    fi
                fi
            fi
        done
    fi
"#)?;

    if !command_id_from_state.is_empty() {
        let commands_array_initializer = itertools::join(command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        writeln!(buffer, r#"    local -A commands=({commands_array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        local output=$(_{command}_subword_cmd_${{command_id}} "$matched_prefix")
        local -a command_completions=("${{(@f)output}}")
        for line in ${{command_completions[@]}}; do
            if [[ $line = "${{completed_prefix}}"* ]]; then
                local parts=(${{(@s:	:)line}})
                if [[ -v "parts[2]" ]]; then
                    local completion=$matched_prefix${{parts[1]}}
                    eval "$completions_trailing_space_array_name+=(${{(qq)completion}})"
                    eval "$suffixes_trailing_space_array_name+=(${{(qq)parts[1]}})"
                    eval "$descriptions_trailing_space_array_name+=(${{(qq)parts[2]}})"
                else
                    eval "$completions_no_description_trailing_space_array_name+=(${{(qq)line}})"
                fi
            fi
        done
    fi
"#)?;
    }

    if !spec_id_from_state.is_empty() {
        writeln!(buffer)?;
        let array_initializer = itertools::join(spec_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        write!(buffer, r#"    local -A specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        local output=$(_{command}_subword_spec_${{command_id}} "$matched_prefix")
        local -a completions=("${{(@f)output}}")
        for line in ${{completions[@]}}; do
            if [[ $line = "${{completed_prefix}}"* ]]; then
                line="$matched_prefix$line"
                local parts=(${{(@s:	:)line}})
                if [[ -v "parts[2]" ]]; then
                    eval "$completions_trailing_space_array_name+=(${{(qq)parts[1]}})"
                    eval "$suffixes_trailing_space_array_name+=(${{(qq)parts[1]}})"
                    eval "$descriptions_trailing_space_array_name+=(${{(qq)parts[2]}})"
                else
                    eval "$completions_no_description_trailing_space_array_name+=(${{(qq)line}})"
                fi
            fi
        done
    fi
"#)?;
    }

    writeln!(buffer, r#"    return 0
}}"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    writeln!(buffer, r#"#compdef {command}
"#)?;

    let (top_level_command_transitions, subword_command_transitions) = dfa.get_command_transitions();

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

    let (top_level_spec_transitions, subword_spec_transitions) = dfa.get_zsh_command_transitions();
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

    let id_from_dfa = dfa.get_subwords(1);
    for (dfa, id) in &id_from_dfa {
        let subword_command_id_from_state: HashMap<StateId, usize> = subword_command_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_command.get(cmd).unwrap())).collect();
        let subword_spec_id_from_state: HashMap<StateId, usize> = subword_spec_transitions.get(dfa).unwrap().iter().map(|(state, cmd)| (*state, *id_from_subword_spec.get(cmd).unwrap())).collect();
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &subword_command_id_from_state, &subword_spec_id_from_state)?;
        writeln!(buffer)?;
    }

    writeln!(buffer, r#"_{command} () {{"#)?;

    write_lookup_tables(buffer, dfa)?;

    writeln!(buffer)?;
    writeln!(buffer, r#"    declare -A subword_transitions"#)?;
    for state in dfa.get_all_states() {
        let subword_transitions = dfa.get_subword_transitions_from(state.try_into().unwrap());
        if subword_transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(subword_transitions.into_iter().map(|(dfa, to)| format!("[{}]={}", id_from_dfa.get(&dfa).unwrap(), to + 1)), " ");
        writeln!(buffer, r#"    subword_transitions[{}]="({state_transitions})""#, state + 1)?;
    }

    write!(buffer, r#"
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
"#, starting_state = dfa.starting_state + 1)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
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
    completions_no_description_trailing_space=()
    completions_no_description_no_trailing_space=()
    completions_trailing_space=()
    suffixes_trailing_space=()
    descriptions_trailing_space=()
    completions_no_trailing_space=()
    suffixes_no_trailing_space=()
    descriptions_no_trailing_space=()

    if [[ -v "literal_transitions[$state]" ]]; then
        local -A state_transitions
        eval "state_transitions=${{literal_transitions[$state]}}"

        for literal_id in ${{(k)state_transitions}}; do
            if [[ -v "descriptions[$literal_id]" ]]; then
                completions_trailing_space+=("${{literals[$literal_id]}}")
                suffixes_trailing_space+=("${{literals[$literal_id]}}")
                descriptions_trailing_space+=("${{descriptions[$literal_id]}}")
            else
                completions_no_description_trailing_space+=("${{literals[$literal_id]}}")
            fi
        done
    fi
"#)?;

    if dfa.has_subword_transitions() {
        write!(buffer, r#"
    if [[ -v "subword_transitions[$state]" ]]; then
        declare -A state_transitions
        eval "state_transitions=${{subword_transitions[$state]}}"

        for subword_id in ${{(k)state_transitions}}; do
            _{command}_subword_${{subword_id}} complete "${{words[$CURRENT]}}" completions_no_description_trailing_space completions_trailing_space suffixes_trailing_space descriptions_trailing_space completions_no_description_no_trailing_space completions_no_trailing_space suffixes_no_trailing_space descriptions_no_trailing_space
        done
    fi
"#)?;
    }

    let top_level_command_id_from_state: HashMap<StateId, usize> = top_level_command_transitions.into_iter().map(|(state, cmd)| (state, *id_from_top_level_command.get(&cmd).unwrap())).collect();
    if !top_level_command_id_from_state.is_empty() {
        let commands_array_initializer = itertools::join(top_level_command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        writeln!(buffer, r#"    local -A commands=({commands_array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
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
    fi
"#)?;
    }

    let specialized_command_id_from_state: HashMap<StateId, usize> = top_level_spec_transitions.into_iter().map(|(state, cmd)| (state, *id_from_specialized_command.get(&cmd).unwrap())).collect();
    if !specialized_command_id_from_state.is_empty() {
        writeln!(buffer)?;
        let array_initializer = itertools::join(specialized_command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        write!(buffer, r#"    local -A specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        _{command}_spec_${{command_id}} ${{words[$CURRENT]}}
    fi
"#)?;
    }


    write!(buffer, r#"
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

    compadd -Q -a completions_no_description_trailing_space
    compadd -Q -S ' ' -a completions_no_description_no_trailing_space
    compadd -l -Q -a -d descriptions_trailing_space completions_trailing_space
    compadd -l -Q -S '' -a -d descriptions_no_trailing_space completions_no_trailing_space
    return 0
}}
"#)?;

    write!(buffer, r#"
if [[ $ZSH_EVAL_CONTEXT =~ :file$ ]]; then
    compdef _{command} {command}
else
    _{command}
fi
"#)?;

    Ok(())
}
