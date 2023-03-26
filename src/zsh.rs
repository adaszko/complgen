use std::fmt::Write;

use crate::parser::{Grammar, Expr};
use crate::error::Result;


struct GenerationState {
    sym: usize,
}

impl GenerationState {
    fn new() -> Self {
        Self {
            sym: 0,
        }
    }

    fn gensym(&mut self) -> String {
        let result = format!("_{}", self.sym);
        self.sym += 1;
        result
    }
}


fn write_alternative<W: Write>(buffer: &mut W, state: &mut GenerationState, _alternatives: &[Expr]) -> Result<()> {
    write!(buffer, r#"
function _{name} () {{
    local ret=1

    local -a args
    args=(
    )

    return ret
}}
"#, name = state.gensym());
    todo!();
}

fn write_grammar<W: Write>(buffer: &mut W, state: &mut GenerationState, grammar: &Grammar) -> Result<()> {
    write!(buffer, r#"
#compdef _{command}
"#, command = grammar.command);

    write_alternative(buffer, state, &grammar.args)?;

    write!(buffer, r#"
_{command} "$*"
"#, command = grammar.command)?;
    Ok(())
}

fn generate_completion_script(grammar: Grammar) -> Result<String> {
    let mut result = String::new();
    let mut state = GenerationState::new();
    write_grammar(&mut result, &mut state, &grammar)?;
    Ok(result)
}
