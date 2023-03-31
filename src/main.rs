use std::io::Write;

use complgen::Error;

use crate::dfa::DFA;
use crate::nfa::NFA;
use crate::grammar::parse;
use crate::epsilon_nfa::EpsilonNFA;

mod grammar;
mod epsilon_nfa;
mod nfa;
mod dfa;
mod bash;
mod fish;


fn main() {
    let input = std::io::read_to_string(std::io::stdin()).unwrap();
    let grammar = match parse(&input) {
        Ok(g) => g,
        Err(Error::ParsingError(e)) => {
            eprintln!("Unable to parse grammar: {:?}", e);
            return;
        },
        Err(e) => {
            eprintln!("{:?}", e);
            return;
        }
    };
    let (command, expr) = grammar.into_command_expr();
    let epsilon_nfa = EpsilonNFA::from_expr(&expr);
    let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
    let dfa = DFA::from_nfa(&nfa);
    dfa.to_dot_file("dfa.dot").unwrap();
    let mut output = String::default();

    {
        bash::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut bash_completion_script = std::fs::File::create("completion.bash").unwrap();
        bash_completion_script.write_all(output.as_bytes()).unwrap();
    }

    {
        fish::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut fish_completion_script = std::fs::File::create("completion.fish").unwrap();
        fish_completion_script.write_all(output.as_bytes()).unwrap();
    }
}
