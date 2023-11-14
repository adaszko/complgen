use std::io::Write;

use crate::{StateId, Result};
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;


// Array indexes in ZSH start from 1 (!)


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
    local compadd_array_name=$3
    local describe_completions_array_name=$4
    local describe_descriptions_array_name=$5
    local compadd_completions_array_name=$6
    local compadd_descriptions_array_name=$7
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
            local state_transitions_initializer=${{literal_transitions[$state]}}
            declare -A state_transitions
            eval "state_transitions=$state_transitions_initializer"

            local literal_matched=0
            for literal_id in {{1..$#literals}}; do
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
        local state_transitions_initializer=${{literal_transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        for literal_id in ${{(k)state_transitions}}; do
            local literal=${{literals[$literal_id]}}
            if [[ $literal = "${{completed_prefix}}"* ]]; then
                local completion="$matched_prefix$literal"
                if [[ -v "descriptions[$literal_id]" ]]; then
                    local description="${{completion//:/\\:}}:${{descriptions[$literal_id]}}"
                    eval "$describe_completions_array_name+=('${{completion}}')"
                    eval "$describe_descriptions_array_name+=('${{description}}')"
                else
                    eval "$compadd_completions_array_name+=('${{completion}}')"
                    eval "$compadd_descriptions_array_name+=('${{literal}}')"
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
                    local description="${{parts[1]//:/\\:}}:${{parts[2]}}"
                    eval "$describe_completions_array_name+=('${{completion}}')"
                    eval "$describe_descriptions_array_name+=('${{description}}')"
                else
                    eval "$compadd_array_name+=('${{line}}')"
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
                    local completion=${{parts[1]}}
                    local description="${{parts[1]//:/\\:}}:${{parts[2]}}"
                    eval "$describe_completions_array_name+=('${{completion}}')"
                    eval "$describe_descriptions_array_name+=('${{description}}')"
                else
                    eval "$compadd_array_name+=('${{line}}')"
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
            local state_transitions_initializer=${{literal_transitions[$state]}}
            local -A state_transitions
            eval "state_transitions=$state_transitions_initializer"

            local word=${{words[$word_index]}}
            local word_matched=0
            for literal_id in {{1..$#literals}}; do
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
            local state_transitions_initializer=${{subword_transitions[$state]}}
            declare -A state_transitions
            eval "state_transitions=$state_transitions_initializer"

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
    args=()
    no_descr_args=()
    descrs=()
    compadd_completions=()
    compadd_descriptions=()

    if [[ -v "literal_transitions[$state]" ]]; then
        local state_transitions_initializer=${{literal_transitions[$state]}}
        local -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        for literal_id in ${{(k)state_transitions}}; do
            if [[ -v "descriptions[$literal_id]" ]]; then
                args+=("${{literals[$literal_id]}}")
                descrs+=("${{literals[$literal_id]//:/\\:}}:${{descriptions[$literal_id]}}")
            else
                no_descr_args+=("${{literals[$literal_id]}}")
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

        for subword_id in ${{(k)state_transitions}}; do
            _{command}_subword_${{subword_id}} complete "${{words[$CURRENT]}}" no_descr_args args descrs compadd_completions compadd_descriptions
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
                args+=(${{parts[1]}})
                descrs+=("${{parts[1]//:/\\:}}:${{parts[2]}}")
            else
                no_descr_args+=(${{parts[1]}})
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
    compadd -a -Q -S '' no_descr_args
    compadd -a -Q -S '' -d compadd_descriptions compadd_completions
    _describe '' descrs args -Q -S ''
    return 0
}}
"#)?;


    write!(buffer, r#"
compdef _{command} {command}
"#)?;

    Ok(())
}
