use crate::dfa::DFA;

pub mod bash;
pub mod fish;
pub mod zsh;


pub fn get_max_fallback_level(dfa: &DFA) -> Option<usize> {
    dfa.iter_inputs().map(|(_, input)| input.get_fallback_level()).max()
}
