use crate::dfa::DFA;
use crate::nfa::NFA;
use crate::{bash::write_completion_script, parser::parse};
use crate::epsilon_nfa::NFA as EpsilonNFA;

mod parser;
mod error;
mod automata;
mod epsilon_nfa;
mod nfa;
mod dfa;
mod bash;


fn main() {
    let input = std::io::read_to_string(std::io::stdin()).unwrap();
    let grammar = parse(&input).unwrap();
    let (command, expr) = grammar.into_command_expr();
    let epsilon_nfa = EpsilonNFA::from_expr(&expr);
    let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
    let dfa = DFA::from_nfa(nfa);
    let mut output = String::default();
    write_completion_script(&mut output, &command, &dfa).unwrap();
    print!("{}", output);
}
