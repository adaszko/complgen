use std::fmt::Write;

use complgen::{StateId, Result};
use crate::dfa::DFA;
use crate::nfa::Input;


// TODO https://stackoverflow.com/a/40019138


fn make_state_name(command: &str, state: StateId) -> String {
    format!("_{command}_{state}", command = command, state = state)
}


fn write_dfa_state_function<W: Write>(buffer: &mut W, command: &str, dfa: &DFA, state: StateId) -> Result<()> {
    let mut discriminating_inputs: Vec<String> = dfa.get_transitions_from(state).into_iter().filter_map(|(input, _)| match input {
        Input::Literal(s) => Some(s.to_string()),
        Input::Any => None,
    }).collect();
    discriminating_inputs.sort();

    write!(buffer, r#"function {state_name}
    set i $argv[1]
    if test $i -eq $COMP_CWORD
"#, state_name = make_state_name(command, state))?;

    // We're generating completions for the current state
    if !discriminating_inputs.is_empty() {
        writeln!(buffer, r#"        echo "{inputs}""#, inputs = discriminating_inputs.join("\n"))?;
    }

    write!(buffer, r#"        return 0
    end
"#)?;

    // We're matching $COMP_WORDS[$i] against the DFA
    writeln!(buffer, r#"    switch $COMP_WORDS[$i]"#)?;

    let mut transitions: Vec<(Input, StateId)> = dfa.get_transitions_from(state).into_iter().collect();
    transitions.sort_by_key(|(input, _)| input.clone());
    for (input, to) in transitions {
        writeln!(buffer, r#"        case '{input}'
            {state_name} (math $i + 1)
            return 0"#, state_name = make_state_name(command, to), input = input)?;
    }

    write!(buffer, r#"    end
    return 1
end

"#)?;
    Ok(())
}


pub fn write_completion_script<W: Write>(buffer: &mut W, command: &str, dfa: &DFA) -> Result<()> {
    for state in dfa.get_all_states() {
        write_dfa_state_function(buffer, command, dfa, state)?;
    }

    write!(buffer, r#"
function _{command}
    set COMP_LINE (commandline --cut-at-cursor)
    set --global COMP_WORDS
    echo $COMP_LINE | read --tokenize --array COMP_WORDS
    if string match --quiet --regex '.*\s$' $COMP_LINE
        set --global COMP_CWORD (math (count $COMP_WORDS) + 1)
    else
        set --global COMP_CWORD (count $COMP_WORDS)
    end
    {state_name} 2
end

complete --command {command} --no-files --arguments "(_{command})"
"#, state_name = make_state_name(command, dfa.starting_state), command = command)?;

    Ok(())
}
