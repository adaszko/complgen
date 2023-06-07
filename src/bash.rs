use std::fmt::Write;

use complgen::{StateId, Result};
use crate::{dfa::DFA, regex::Input};



// `transitions`: an associative array where:
//      key: state number
//      value: a string that is an initializer of an associative array, e.g. "( [add]=23 [obliterate]=1080 [repair]=1543 )"
//  `asterisk_transitions`: an associative array where:
//      key: state number
//      value: state number
// An entry in the `asterisk_transitions` array indicates that there's a fallback transition that
// accepts any word
fn write_tables<W: Write>(buffer: &mut W, dfa: &DFA) -> Result<()> {
    let id_from_input = {
        let mut id_from_input: ustr::UstrMap<usize> = Default::default();
        let mut unallocated_input_id = 0;
        for ustr in dfa.input_symbols.iter().filter_map(|input| match input {
            Input::Literal(ustr) => Some(ustr),
            Input::Any => None,
        }) {
            id_from_input.insert(*ustr, unallocated_input_id);
            unallocated_input_id += 1;
        }
        id_from_input
    };

    writeln!(buffer, r#"    declare -A symbols"#)?;
    let symbols: String = itertools::join(id_from_input.iter().map(|(symbol, id)| format!("[{symbol}]={id}")), " ");
    writeln!(buffer, r#"    symbols=({symbols})"#)?;
    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A transitions"#)?;
    for state in dfa.get_all_states() {
        let map = match dfa.transitions.get(&StateId::try_from(state).unwrap()) {
            Some(map) => map,
            None => continue,
        };
        let transitions: Vec<(usize, StateId)> = map.iter().filter_map(|(input, to)| match input {
            Input::Literal(ustr) => Some((*id_from_input.get(ustr).unwrap(), *to)),
            Input::Any => None,
        }).collect();
        if transitions.is_empty() {
            continue;
        }
        let state_transitions: String = itertools::join(transitions.into_iter().map(|(input, to)| format!("[{}]={}", input, to)), " ");
        writeln!(buffer, r#"    transitions[{state}]="({state_transitions})""#)?;
    }

    writeln!(buffer, "")?;

    writeln!(buffer, r#"    declare -A asterisk_transitions"#)?;
    let asterisk_transitions = itertools::join(dfa.get_asterisk_transitions().into_iter().map(|(from, to)| format!("[{from}]={to}")), " ");
    writeln!(buffer, r#"    asterisk_transitions=({asterisk_transitions})"#)?;

    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    write!(buffer, r#"_{command} () {{
"#)?;

    write_tables(buffer, dfa)?;

    write!(buffer, r#"
    local state={starting_state}
    local word_index=1
    while [[ $word_index -lt $COMP_CWORD ]]; do
        [[ -v "transitions[$state]" ]] || return 1
        local state_transitions_initializer=${{transitions[$state]}}
        declare -A state_transitions
        eval "state_transitions=$state_transitions_initializer"
        local word=${{COMP_WORDS[$word_index]}}
        if [[ ! -v "symbols[$word]" ]]; then
            return 1
        fi
        local symbol_id=${{symbols[$word]}}
        if [[ -v "state_transitions[$symbol_id]" ]]; then
            state=${{state_transitions[$symbol_id]}}
            word_index=$((word_index + 1))
            continue
        fi
        if [[ -v "asterisk_transitions[$state]" ]]; then
            state=${{asterisk_transitions[$state]}}
            word_index=$((word_index + 1))
            continue
        fi
        return 1
    done

    [[ -v "transitions[$state]" ]] || return 1
    local state_transitions_initializer=${{transitions[$state]}}
    declare -A state_transitions
    eval "state_transitions=$state_transitions_initializer"

    local -A inverted_symbols=()
    for key in "${{!symbols[@]}}"; do
        local value=${{symbols[$key]}}
        inverted_symbols+=([$value]=$key)
    done

    local completions=()
    for symbol_id in ${{!state_transitions[@]}}; do
        completions+=(${{inverted_symbols[$symbol_id]}})
    done
    completions=${{completions[@]}}

    COMPREPLY=($(compgen -W "$completions" -- "${{COMP_WORDS[$COMP_CWORD]}}"))
    return 0
}}

complete -F _{command} {command}
"#, starting_state = dfa.starting_state)?;
    Ok(())
}
