use std::fmt::Write;

use complgen::StateId;
use crate::error::Result;
use crate::dfa::DFA;


fn write_dfa_state_function<W: Write>(buffer: &mut W, dfa: &DFA, state: StateId) -> Result<()> {
    write!(buffer, r#"
_state_{state} () {{
    case ${{COMP_WORDS[$current_dfa_word]}}
"#, state = state)?;

    for (input, to) in dfa.get_transitions_from(state) {
        write!(buffer, r#"
        {input})
            current_dfa_word=$((current_dfa_word+1))
            _state_{to};;
"#, input = input)?;
    }

    write!(buffer, r#"
    esac
}}

"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    for state in dfa.get_states() {
        write_dfa_state_function(buffer, dfa, state)?;
    }

    write!(buffer, r#"
_{command}_completions () {{
  COMPREPLY+=("now")
  COMPREPLY+=("tomorrow")
  COMPREPLY+=("never")
}}

complete -F _{command}_completions {command}
"#, command = command)?;

    Ok(())
}
