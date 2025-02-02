use crate::StateId;

use hashbrown::HashMap;
use ustr::{Ustr, UstrMap};

use crate::grammar::{DFARef, Shell};
use crate::{dfa::DFA, regex::Input};

pub mod bash;
pub mod fish;
pub mod zsh;

pub fn get_match_final_state(dfa: &DFA, words: &[&str], shell: Shell) -> Option<StateId> {
    // Match words up to `completed_word_index`
    let mut word_index = 0;
    let mut state = dfa.starting_state;
    'outer: while word_index < words.len() {
        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if let Input::Literal(s, ..) = transition_input {
                if words[word_index] == s.as_str() {
                    word_index += 1;
                    state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if let Input::Subword(dfa, ..) = transition_input {
                if dfa.as_ref().accepts_str(words[word_index], shell) {
                    word_index += 1;
                    state = to;
                    continue 'outer;
                }
            }
        }

        for (transition_input, to) in dfa.iter_transitions_from(state) {
            if transition_input.matches_anything(shell) {
                word_index += 1;
                state = to;
                continue 'outer;
            }
        }

        return None;
    }

    Some(state)
}

pub fn get_transitions(dfa: &DFA, words_before_cursor: &[&str], shell: Shell) -> Vec<Input> {
    let Some(state) = get_match_final_state(dfa, words_before_cursor, shell) else {
        return vec![];
    };
    let inputs = dfa
        .iter_transitions_from(state)
        .map(|(input, _)| input)
        .collect();
    inputs
}

// Returns a map: command -> command id
pub fn get_command_transitions(transitions: &[Input]) -> UstrMap<usize> {
    let mut id = 0;
    let mut top_level: UstrMap<usize> = Default::default();
    for input in transitions {
        let cmd = match input {
            Input::Command(cmd, ..) => *cmd,
            Input::Subword(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Literal(..) => continue,
        };
        top_level.entry(cmd).or_insert_with(|| {
            let sav = id;
            id += 1;
            sav
        });
    }
    top_level
}

pub fn get_subword_transitions(transitions: &[Input]) -> HashMap<DFARef, usize> {
    let mut id = 1;
    let mut id_from_dfa: HashMap<DFARef, usize> = Default::default();
    for input in transitions {
        let subdfa = match input {
            Input::Subword(subdfa, ..) => subdfa,
            Input::Command(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Literal(..) => continue,
        };
        id_from_dfa.entry(subdfa.clone()).or_insert_with(|| {
            let save = id;
            id += 1;
            save
        });
    }
    id_from_dfa
}

pub fn get_subword_commands(transitions: &[Input]) -> HashMap<DFARef, Vec<(StateId, Ustr)>> {
    let mut subdfas: HashMap<DFARef, Vec<(StateId, Ustr)>> = Default::default();
    for input in transitions {
        match input {
            Input::Subword(subdfa, ..) => {
                if subdfas.contains_key(subdfa) {
                    continue;
                }
                let transitions = subdfas.entry(subdfa.clone()).or_default();
                for (from, tos) in &subdfa.as_ref().transitions {
                    for (input, _) in tos {
                        let cmd = match input {
                            Input::Command(cmd, ..) => *cmd,
                            Input::Subword(..) => unreachable!(),
                            Input::Nonterminal(..) => continue,
                            Input::Literal(..) => continue,
                        };
                        transitions.push((*from, cmd));
                    }
                }
            }
            Input::Command(..) => continue,
            Input::Nonterminal(..) => continue,
            Input::Literal(..) => continue,
        }
    }
    subdfas
}

fn make_external_command_fn_name(command: &str, id: usize) -> String {
    format!("_{command}_cmd_{id}")
}

fn make_specialized_external_command_fn_name(command: &str, id: usize) -> String {
    format!("_{command}_spec_{id}")
}

fn make_subword_fn_name(command: &str, id: usize) -> String {
    format!("_{command}_subword_{id}")
}
