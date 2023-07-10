use std::io::Write;

use complgen::{StateId, Result};
use hashbrown::HashMap;
use ustr::{UstrMap, Ustr, ustr};
use crate::dfa::DFA;


// Array indexes in ZSH start from 1 (!)


pub fn escape_zsh_string(s: &str) -> String {
    s.replace("\"", "\\\"").replace("`", "\\`").replace("$", "\\$")
}


/// `literals`: an associative array used for literals deduplication (interning) where:
///   * key: the literal
///   * value: literal's id
///
/// `transitions`: an associative array where:
///   * key: source state number
///   * value: a string that is an initializer of an associative array, e.g. "( [add]=23 [obliterate]=1080 [repair]=1543 )"
///
/// The initializer value above indicates there are 3 transitions from the state of id `key`.
/// First of which is a transition on word `add`, to the state 23.
///
/// `match_anything_transitions`: an associative array where:
///  * key: state number
///  * value: state number
/// An entry in the `match_anything_transitions` array indicates that there's a fallback transition that
/// accepts any word
///
fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let all_literals: Vec<(usize, Ustr, Ustr)> = dfa.get_all_literals().into_iter().enumerate().map(|(id, (literal, description))| (id + 1, literal, description.unwrap_or(ustr("")))).collect();

    let literal_id_from_input_description: HashMap<(Ustr, Ustr), usize> = all_literals.iter().map(|(id, input, description)| ((*input, *description), *id)).collect();
    let literals: String = itertools::join(all_literals.iter().map(|(_, literal, _)| literal), " ");
    writeln!(buffer, r#"    local -a literals=({literals})"#)?;
    writeln!(buffer, "")?;

    writeln!(buffer, r#"    local -A descriptions"#)?;
    for (id, _, description) in all_literals.iter() {
        let description = escape_zsh_string(&description);
        if !description.is_empty() {
            writeln!(buffer, r#"    descriptions[{id}]="{description}""#)?;
        }
    }
    writeln!(buffer, "")?;

    writeln!(buffer, r#"    local -A transitions"#)?;
    for state in dfa.get_all_states() {
        let transitions = dfa.get_literal_transitions_from(StateId::try_from(state).unwrap());
        if transitions.is_empty() {
            continue;
        }
        let transitions: Vec<(usize, StateId)> = transitions.into_iter().map(|(input, description, to)| (*literal_id_from_input_description.get(&(input, description)).unwrap(), to)).collect();
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to + 1)), " ");
        writeln!(buffer, r#"    transitions[{}]="({state_transitions})""#, state + 1)?;
    }

    writeln!(buffer, "")?;

    writeln!(buffer, r#"    local -A match_anything_transitions"#)?;
    let match_anything_transitions = itertools::join(dfa.get_match_anything_transitions().into_iter().map(|(from, to)| format!("[{}]={}", from + 1, to + 1)), " ");
    writeln!(buffer, r#"    match_anything_transitions=({match_anything_transitions})"#)?;

    Ok(())
}


fn write_specialized_commands<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<UstrMap<usize>> {
    let id_from_specialized_command: UstrMap<usize> = dfa.get_zsh_command_transitions().into_iter().enumerate().map(|(id, (_, cmd))| (cmd, id)).collect();
    for (cmd, id) in &id_from_specialized_command {
        write!(buffer, r#"_{command}_{id} () {{
    {cmd}
}}

"#)?;
    }

    Ok(id_from_specialized_command)
}


fn write_specialized_commands_completion_code<W: Write>(buffer: &mut W, command: &str, dfa: &DFA, id_from_specialized_command: &UstrMap<usize>) -> Result<()> {
    let specialized_command_id_from_state: HashMap<StateId, usize> = dfa.get_zsh_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_specialized_command.get(&cmd).unwrap())).collect();
    if !specialized_command_id_from_state.is_empty() {
        writeln!(buffer, "")?;
        let array_initializer = itertools::join(specialized_command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        write!(buffer, r#"    local -A specialized_commands=({array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "specialized_commands[$state]" ]]; then
        local command_id=${{specialized_commands[$state]}}
        _{command}_${{command_id}} ${{words[$CURRENT]}}
    fi
"#)?;
    }

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    let id_from_command: UstrMap<usize> = dfa.get_command_transitions().into_iter().enumerate().map(|(id, (_, cmd))| (cmd, id)).collect();
    for (cmd, id) in &id_from_command {
        write!(buffer, r#"_{command}_{id} () {{
    {cmd}
}}

"#)?;
    }

    let id_from_specialized_command = write_specialized_commands(buffer, command, dfa)?;

    write!(buffer, r#"_{command} () {{
"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
    local word_index=2
    while [[ $word_index -lt $CURRENT ]]; do
        if [[ -v "transitions[$state]" ]]; then
            local state_transitions_initializer=${{transitions[$state]}}
            local -A state_transitions
            eval "state_transitions=$state_transitions_initializer"

            local word=${{words[$word_index]}}
            local word_matched=0
            for literal_id in {{1..$#literals}}; do
                if [[ ${{literals[$literal_id]}} = $word ]]; then
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

        if [[ -v "match_anything_transitions[$state]" ]]; then
            state=${{match_anything_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi

        return 1
    done
"#, starting_state = dfa.starting_state + 1)?;

    write!(buffer, r#"
    if [[ -v "transitions[$state]" ]]; then
        local state_transitions_initializer=${{transitions[$state]}}
        local -A state_transitions
        eval "state_transitions=$state_transitions_initializer"

        local -a args
        local -a descrs
        for literal_id in ${{(k)state_transitions}}; do
            if [[ -v "descriptions[$literal_id]" ]]; then
                args+=(${{literals[$literal_id]}})
                descrs+=("${{literals[$literal_id]}} (${{descriptions[$literal_id]}})")
            else
                args+=(${{literals[$literal_id]}})
                descrs+=(${{literals[$literal_id]}})
            fi
        done
        local joined=${{(j::)descrs}}
        if [[ -z $joined ]]; then
            compadd -a args
        else
            compadd -d descrs -a args
        fi
    fi
"#)?;

    let command_id_from_state: HashMap<StateId, usize> = dfa.get_command_transitions().into_iter().map(|(state, cmd)| (state, *id_from_command.get(&cmd).unwrap())).collect();
    if !command_id_from_state.is_empty() {
        let commands_array_initializer = itertools::join(command_id_from_state.into_iter().map(|(state, id)| format!("[{}]={id}", state + 1)), " ");
        writeln!(buffer, r#"    local -A commands=({commands_array_initializer})"#)?;
        write!(buffer, r#"
    if [[ -v "commands[$state]" ]]; then
        local command_id=${{commands[$state]}}
        local -a args
        local -a descrs
        local -a command_completions=("${{(@f)$(_{command}_${{command_id}} ${{words[$CURRENT]}})}}")
        for line in ${{command_completions[@]}}; do
            local a=$(echo "$line" | cut -f1)
            args+=($a)
            local d=$(echo "$line" | cut -f2-)
            descrs+=($d)
        done
        local joined=${{(j::)descrs}}
        if [[ -z $joined ]]; then
            compadd -a args
        else
            compadd -d descrs -a args
        fi
    fi
"#)?;
    }

    write_specialized_commands_completion_code(buffer, command, dfa, &id_from_specialized_command)?;

    write!(buffer, r#"
    return 0
}}
"#)?;


    write!(buffer, r#"
compdef _{command} {command}
"#)?;

    Ok(())
}
