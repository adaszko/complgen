use std::io::Write;

use crate::grammar::Specialization;
use crate::regex::Input;
use crate::StateId;
use crate::Result;
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;

use super::get_max_fallback_level;


// Bash array indexes start at 0.
// Associative arrays are local by default.
// Bash uses *dynamic* scoping for local variables (!)


pub fn make_string_constant(s: &str) -> String {
    if s.is_empty() {
        return r#""""#.to_string();
    }
    if s.contains(&[' ', '\t', '\n']) {
        format!(r#""{}""#, s.replace('\"', "\\\"").replace('`', "\\`").replace('$', "\\$"))
    } else {
        format!("{}", s.replace('\"', "\\\"").replace('`', "\\`").replace('$', "\\$"))
    }
}


fn write_lookup_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<HashMap<(Ustr, Ustr), usize>> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, input, description)| ((*input, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| make_string_constant(literal)), " ");
    writeln!(buffer, r#"    declare -a literals=({literals})"#)?;

    writeln!(buffer, r#"    declare -A literal_transitions=()"#)?;
    for state in dfa.get_all_states() {
        let literal_transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if literal_transitions.is_empty() {
            continue;
        }
        let literal_transitions: Vec<(usize, StateId)> = literal_transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(literal_transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to)), " ");
        writeln!(buffer, r#"    literal_transitions[{state}]="({state_transitions})""#)?;
    }

    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{from}]={to}")), " ");
    writeln!(buffer, r#"    declare -A match_anything_transitions=({match_anything_transitions})"#)?;

    Ok(literal_id_from_input_description)
}


pub fn write_generic_subword_fn<W: Write>(buffer: &mut W, command: &str) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword () {{
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
"#)?;

    ////////////////// Completion /////////////////////////

    // TODO Implement a way to determine if a transition within a subword is a final one so that a
    // trailing space can be added

    write!(buffer, r#"
    local matched_prefix="${{word:0:$char_index}}"
    local completed_prefix="${{word:$char_index}}"

    local -a matches=()
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )) {{
        eval "declare -A literal_transitions_name=literal_transitions_level_${{fallback_level}}"
        eval "declare -a transitions=(\${{$literal_transitions_name[$state]}})"
        for literal_id in "${{transitions[@]}}"; do
            local literal=${{literals[$literal_id]}}
            if [[ $literal = "${{completed_prefix}}"* ]]; then
                matches+=("$matched_prefix$literal")
            fi
        done

        eval "declare -A commands_name=commands_level_${{fallback_level}}"
        eval "declare -a transitions=(\${{$commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            local completions=()
            readarray -t completions < <(_{command}_subword_cmd_$command_id "$matched_prefix" | cut -f1)
            for item in "${{completions[@]}}"; do
                matches+=("$matched_prefix$item")
            done
        done

        eval "declare -A specialized_commands_name=specialized_commands_level_$fallback_level"
        eval "declare -a transitions=(\${{$specialized_commands_name[$state]}})"
        for command_id in "${{transitions[@]}}"; do
            local completions=()
            readarray -t completions < <(_{command}_subword_spec_$command_id "$prefix" | cut -f1)
            for item in "${{completions[@]}}"; do
                matches+=("$matched_prefix$item")
            done
        done

        if [[ ${{#matches[@]}} -gt 0 ]]; then
            printf '%s\n' "${{matches[@]}}"
            break
        fi
    }}
"#)?;

    writeln!(buffer, r#"    return 0
}}"#)?;
    writeln!(buffer)?;

    Ok(())
}


pub fn write_subword_fn<W: Write>(buffer: &mut W, command: &str, id: usize, dfa: &DFA, id_from_subword_command: &UstrMap<usize>, id_from_subword_spec: &UstrMap<usize>) -> Result<()> {
    writeln!(buffer, r#"_{command}_subword_{id} () {{"#)?;

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa)?;

    let Some(max_fallback_level) = get_max_fallback_level(dfa) else {
        todo!("empty DFA, what to emit here?");
    };

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

    for (from, tos) in &dfa.transitions {
        for input in tos.keys() {
            match input {
                Input::Literal(lit, descr, fallback_level ) => {
                    let literal_id = *literal_id_from_input_description.get(&(*lit, (*descr).unwrap_or("".into()))).unwrap();
                    fallback_literals[*fallback_level].entry(*from).or_default().push(literal_id);
                },
                Input::Command(cmd, fallback_level) => {
                    let command_id = *id_from_subword_command.get(cmd).unwrap();
                    fallback_commands[*fallback_level].entry(*from).or_default().push(command_id);
                },
                Input::Nonterminal(_, Some(Specialization { bash: Some(cmd), .. }), fallback_level) => {
                    let specialized_id = *id_from_subword_spec.get(cmd).unwrap();
                    fallback_specialized[*fallback_level].entry(*from).or_default().push(specialized_id);
                },
                _ => (),
            }
        }
    }

    for (level, transitions) in fallback_literals.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, literal_ids)| {
            let joined_literal_ids = itertools::join(literal_ids, " ");
            format!(r#"[{from_state}]="{joined_literal_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A literal_transitions_level_{level}=({initializer})"#)?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, command_ids)| {
            let joined_command_ids = itertools::join(command_ids, " ");
            format!(r#"[{from_state}]="{joined_command_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A commands_level_{level}=({initializer})"#)?;
    }

    for (level, transitions) in fallback_specialized.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, specialized_ids)| {
            let joined_specialized_ids = itertools::join(specialized_ids, " ");
            format!(r#"[{from_state}]="{joined_specialized_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A specialized_commands_level_{level}=({initializer})"#)?;
    }

    writeln!(buffer, r#"    declare max_fallback_level={max_fallback_level}"#)?;
    writeln!(buffer, r#"    declare state={starting_state}"#, starting_state = dfa.starting_state)?;

    writeln!(buffer, r#"    _{command}_subword "$1" "$2""#)?;

    writeln!(buffer, r#"}}"#)?;

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
    if !id_from_dfa.is_empty() {
        write_generic_subword_fn(buffer, command)?;
    }
    for (dfa, id) in &id_from_dfa {
        write_subword_fn(buffer, command, *id, dfa.as_ref(), &id_from_subword_command, &id_from_subword_spec)?;
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

    let literal_id_from_input_description = write_lookup_tables(buffer, dfa)?;

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

    //////////////////////////////// Completion ///////////////////////////////////

    let Some(max_fallback_level) = get_max_fallback_level(dfa) else {
        todo!("empty DFA, what to emit here?");
    };

    let mut fallback_literals: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_literals.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_subwords: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_subwords.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_commands: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_commands.resize_with(max_fallback_level + 1, Default::default);

    let mut fallback_specialized: Vec<HashMap<StateId, Vec<usize>>> = Default::default();
    fallback_specialized.resize_with(max_fallback_level + 1, Default::default);

    for (from, tos) in &dfa.transitions {
        for input in tos.keys() {
            match input {
                Input::Literal(lit, descr, fallback_level ) => {
                    let literal_id = *literal_id_from_input_description.get(&(*lit, (*descr).unwrap_or("".into()))).unwrap();
                    fallback_literals[*fallback_level].entry(*from).or_default().push(literal_id);
                },
                Input::Subword(dfa, fallback_level) => {
                    let subword_id = *id_from_dfa.get(dfa).unwrap();
                    fallback_subwords[*fallback_level].entry(*from).or_default().push(subword_id);
                },
                Input::Command(cmd, fallback_level) => {
                    let command_id = *id_from_top_level_command.get(cmd).unwrap();
                    fallback_commands[*fallback_level].entry(*from).or_default().push(command_id);
                },
                Input::Nonterminal(_, Some(Specialization { bash: Some(cmd), .. }), fallback_level) => {
                    let specialized_id = *id_from_specialized_command.get(cmd).unwrap();
                    fallback_specialized[*fallback_level].entry(*from).or_default().push(specialized_id);
                },
                _ => (),
            }
        }
    }
    for (level, transitions) in fallback_literals.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, literal_ids)| {
            let joined_literal_ids = itertools::join(literal_ids, " ");
            format!(r#"[{from_state}]="{joined_literal_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A literal_transitions_level_{level}=({initializer})"#)?;
    }

    for (level, transitions) in fallback_subwords.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, subword_ids)| {
            let joined_subword_ids = itertools::join(subword_ids, " ");
            format!(r#"[{from_state}]="{joined_subword_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A subword_transitions_level_{level}=({initializer})"#)?;
    }

    for (level, transitions) in fallback_commands.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, command_ids)| {
            let joined_command_ids = itertools::join(command_ids, " ");
            format!(r#"[{from_state}]="{joined_command_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A commands_level_{level}=({initializer})"#)?;
    }

    for (level, transitions) in fallback_specialized.iter().enumerate() {
        let initializer = itertools::join(transitions.iter().map(|(from_state, specialized_ids)| {
            let joined_specialized_ids = itertools::join(specialized_ids, " ");
            format!(r#"[{from_state}]="{joined_specialized_ids}""#)
        }), " ");
        writeln!(buffer, r#"    declare -A specialized_commands_level_{level}=({initializer})"#)?;
    }

    write!(buffer, r#"
    local -a matches=()
    local max_fallback_level={max_fallback_level}
    local prefix="${{words[$cword]}}"
    for (( fallback_level=0; fallback_level <= max_fallback_level; fallback_level++ )) {{
       eval "declare -A literal_transitions_name=literal_transitions_level_${{fallback_level}}"
       eval "declare -a transitions=(\${{$literal_transitions_name[$state]}})"
       for literal_id in "${{transitions[@]}}"; do
           local literal="${{literals[$literal_id]}}"
           if [[ $literal = "${{prefix}}"* ]]; then
               matches+=("$literal ")
           fi
       done

       eval "declare -A subword_transitions_name=subword_transitions_level_${{fallback_level}}"
       eval "declare -a transitions=(\${{$subword_transitions_name[$state]}})"
       for subword_id in "${{transitions[@]}}"; do
           readarray -t -O "${{#matches[@]}}" matches < <(_{command}_subword_$subword_id complete "$prefix")
       done

       eval "declare -A commands_name=commands_level_${{fallback_level}}"
       eval "declare -a transitions=(\${{$commands_name[$state]}})"
       for command_id in "${{transitions[@]}}"; do
           local completions=()
           readarray -t completions < <(_{command}_cmd_$command_id "$prefix" | cut -f1)
           for item in "${{completions[@]}}"; do
               if [[ $item = "${{prefix}}"* ]]; then
                   matches+=("$item")
               fi
           done
       done

       eval "declare -A specialized_commands_name=specialized_commands_level_${{fallback_level}}"
       eval "declare -a transitions=(\${{$specialized_commands_name[$state]}})"
       for command_id in "${{transitions[@]}}"; do
           local completions=()
           readarray -t completions < <(_{command}_spec_"${{command_id}}" "$prefix" | cut -f1)
           for item in "${{completions[@]}}"; do
               if [[ $item = "${{prefix}}"* ]]; then
                   matches+=("$item")
               fi
           done
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
"#)?;

    write!(buffer, r#"
    return 0
}}

complete -o nospace -F _{command} {command}
"#)?;
    Ok(())
}
