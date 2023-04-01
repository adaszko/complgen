use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;
use crate::nfa::Input;


fn make_state_name(command: &str, state: StateId) -> String {
    format!("_{command}_{state}", command = command, state = state)
}


fn write_dfa_state_function<W: Write>(buffer: &mut W, command: &str, dfa: &DFA, state: StateId) -> Result<()> {
    let discriminating_inputs: Vec<String> = dfa.get_transitions_from(state).into_iter().filter_map(|(input, _)| match input {
        Input::Literal(s) => Some(s.to_string()),
        Input::Any => None,
    }).collect();

    write!(buffer, r#"{state_name} () {{
    local i=$1
    if [[ $i == $COMP_CWORD ]]; then
"#, state_name = make_state_name(command, state))?;

    // We're generating completions for the current state
    if !discriminating_inputs.is_empty() {
        writeln!(buffer, r#"        COMPREPLY=($(compgen -W '{inputs}' -- "${{COMP_WORDS[$COMP_CWORD]}}"))"#, inputs = discriminating_inputs.join(" "))?;
    }

    write!(buffer, r#"        return 0
    fi
"#)?;

    // We're matching $COMP_WORDS[$i] against the DFA
    writeln!(buffer, r#"    case ${{COMP_WORDS[$i]}} in"#)?;

    let mut transitions: Vec<(Input, StateId)> = dfa.get_transitions_from(state).into_iter().collect();
    transitions.sort_by_key(|(input, _)| input.clone());
    for (input, to) in transitions {
        writeln!(buffer, r#"        {input}) {state_name} $((i + 1)); return 0;;"#, state_name = make_state_name(command, to), input = input)?;
    }

    write!(buffer, r#"    esac
    return 1
}}

"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    for state in dfa.get_all_states() {
        write_dfa_state_function(buffer, command, dfa, state)?;
    }

    write!(buffer, r#"
_{command} () {{
    {state_name} 1
}}

complete -F _{command} {command}
"#, state_name = make_state_name(command, dfa.starting_state), command = command)?;

    Ok(())
}
