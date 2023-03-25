use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;
use crate::nfa::Input;


fn make_state_name(command: &str, state: StateId) -> String {
    format!("_{command}_{state}", command = command, state = state)
}


fn write_dfa_state_function<W: Write>(buffer: &mut W, command: &str, dfa: &DFA, state: StateId) -> Result<()> {
    let transitions = dfa.get_transitions_from(state);

    let discriminating_inputs: Vec<String> = transitions.iter().filter_map(|(input, _)| match input {
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

    if !transitions.is_empty() {
        // We're matching $COMP_WORDS[$i] against the DFA
        writeln!(buffer, r#"    case ${{COMP_WORDS[$i]}} in"#)?;

        // XXX Input::Any should be the last case branch to use it as a fallback when everything more
        // specific failed
        for (input, to) in transitions {
            writeln!(buffer, r#"        {input}) {state_name} $((i + 1)); return;;"#, state_name = make_state_name(command, to), input = input)?;
        }

        writeln!(buffer, r#"    esac"#)?;
    }

    writeln!(buffer, r#"    return 1
}}
"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    for state in dfa.get_all_states() {
        write_dfa_state_function(buffer, command, dfa, state)?;
    }

    // XXX Should be exactly one, not many
    let start_state = dfa.starting_states.iter().next().unwrap();

    write!(buffer, r#"
_{command} () {{
    {state_name} 1
}}

complete -F _{command} {command}
"#, state_name = make_state_name(command, start_state), command = command)?;

    Ok(())
}
