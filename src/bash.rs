use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;


fn write_dfa_state_function<W: Write>(buffer: &mut W, dfa: &DFA, state: StateId) -> Result<()> {
    let inputs: Vec<String> = dfa.get_transitions_from(state).into_iter().map(|(input, _)| input.to_string()).collect();
    let inputs = inputs.join(" ");

    // We're generating completions for the current state
    write!(buffer, r#"
_state_{state} () {{
    local index=$1
    if [[ $index == $COMP_CWORD ]]; then
        COMPREPLY=($(compgen -W '{inputs}' -- "${{COMP_WORDS[$COMP_CWORD]}}"))
        return 0
    fi
"#, state = state, inputs = inputs)?;

    // We're matching $COMP_WORDS[$index] against the DFA
    write!(buffer, r#"
    case ${{COMP_WORDS[$index]}} in
"#)?;

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
