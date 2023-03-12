use std::fmt::Write;

use crate::error::Result;
use crate::{dfa::DFA, parser::Grammar};


pub fn write_completion_script<W: Write>(buffer: &mut W, grammar: &Grammar, dfa: &DFA) -> Result<()> {
    write!(buffer, r#"
_{command}_completions() {{
  COMPREPLY+=("now")
  COMPREPLY+=("tomorrow")
  COMPREPLY+=("never")
}}

complete -F _{command}_completions {command}
"#, command = grammar.command);

    Ok(())
}
