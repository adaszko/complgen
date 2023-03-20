use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;
use crate::nfa::Input;


fn write_dfa_state_function<W: Write>(buffer: &mut W, dfa: &DFA, state: StateId) -> Result<()> {
    let discriminating_inputs: Vec<String> = dfa.get_transitions_from(state).into_iter().filter_map(|(input, _)| match input {
        Input::Literal(s) => Some(s.to_string()),
        Input::Any => None,
    }).collect();

    write!(buffer, r#"
_state_{state} () {{
    local index=$1
    if [[ $index == $COMP_CWORD ]]; then
"#, state = state)?;

    // We're generating completions for the current state
    if !discriminating_inputs.is_empty() {
        write!(buffer, r#"
        COMPREPLY=($(compgen -W '{inputs}' -- "${{COMP_WORDS[$COMP_CWORD]}}"))
"#, inputs = discriminating_inputs.join(" "))?;
    }

    write!(buffer, r#"
        return 0
    fi
"#)?;

    // We're matching $COMP_WORDS[$index] against the DFA
    write!(buffer, r#"
    case ${{COMP_WORDS[$index]}} in
"#)?;

    // XXX Input::Any should be the last case branch to use it as a fallback when everything more
    // specific failed
    for (input, to) in dfa.get_transitions_from(state) {
        write!(buffer, r#"
        {input})
            _state_{to} $((index + 1))
            return
            ;;
"#, input = input)?;
    }

    write!(buffer, r#"
    esac

    return 1
}}

"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    for state in dfa.get_all_states() {
        write_dfa_state_function(buffer, dfa, state)?;
    }

    // XXX Should be exactly one, not many
    let start_state = dfa.start_states.iter().next().unwrap();

    write!(buffer, r#"
_{command}_completions () {{
  _state_{start_state} 1
}}

complete -F _{command}_completions {command}
"#, command = command)?;

    Ok(())
}
