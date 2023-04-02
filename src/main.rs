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

    println!("Grammar -> EpsilonNFA");
    let epsilon_nfa = EpsilonNFA::from_expr(&expr);

    println!("EpsilonNFA -> NFA");
    let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);

    println!("NFA -> DFA");
    let dfa = DFA::from_nfa(&nfa);

    let mut output = String::default();

    {
        println!("Writing Bash completion script");
        bash::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut bash_completion_script = std::fs::File::create(format!("{command}.bash")).unwrap();
        bash_completion_script.write_all(output.as_bytes()).unwrap();
    }

    output.clear();

    {
        println!("Writing Fish completion script");
        fish::write_completion_script(&mut output, &command, &dfa).unwrap();
        let mut fish_completion_script = std::fs::File::create(format!("{command}.fish")).unwrap();
        fish_completion_script.write_all(output.as_bytes()).unwrap();
    }
}
