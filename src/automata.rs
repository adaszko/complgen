use std::collections::{HashMap, HashSet};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Word {
    Any,
    Literal(String),
}


#[derive(Debug, PartialEq)]
pub enum Pattern {
    Word(Word),
    Sequence(Vec<Pattern>),
    Choice(Vec<Pattern>),
    Many1(Box<Pattern>),
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct StateId(usize);

impl StateId {
    fn start() -> Self {
        StateId(0)
    }

    fn next(&self) -> Self {
        StateId(self.0 + 1)
    }
}

impl From<usize> for StateId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<StateId> for usize {
    fn from(value: StateId) -> Self {
        value.0
    }
}


fn is_deduped(v: &[StateId]) -> bool {
    let mut copy = v.to_vec();
    copy.sort();
    copy.dedup();
    copy == v
}

fn nfa_from_pattern<'a>(nfa: &mut NFA, current_states: &[StateId], p: &'a Pattern) -> Vec<StateId> {
    match p {
        Pattern::Word(lookahead_word) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, lookahead_word.clone(), to_state_id);
            }
            return vec![to_state_id];
        },
        Pattern::Sequence(v) => {
            // Compile into multiple NFA states stringed together by transitions
            let mut states = current_states.to_vec();
            for p in v {
                states = nfa_from_pattern(nfa, &states, p);
            }
            states
        },
        Pattern::Choice(v) => {
            let mut result: Vec<StateId> = Default::default();
            for p in v {
                result.extend(nfa_from_pattern(nfa, current_states, p));
            }
            assert!(is_deduped(&result));
            result
        },
        Pattern::Many1(p) => {
            let ending_states = nfa_from_pattern(nfa, current_states, p);
            for ending_state in &ending_states {
                for current_state in current_states {
                    let (lookahead_word, subpattern_begining_state_id) = nfa.walk_transitions_backwards(*ending_state, *current_state);
                    // loop from the last states in `states`
                    nfa.add_transition(*ending_state, lookahead_word, subpattern_begining_state_id);
                }
            }
            ending_states
        },
    }
}

// Transform Optionals into Alternatives
fn pattern_from_expr<'a>(_e: &'a crate::parser::Expr) -> Pattern {
    todo!();
}


struct NFA {
    start_state: StateId,
    next_state_id: StateId,
    transitions: HashMap<(StateId, StateId), Word>,
    accepting_states: HashSet<StateId>,
}

impl<'a> Default for NFA {
    fn default() -> Self {
        Self {
            next_state_id: 0.into(),
            transitions: Default::default(),
            start_state: StateId::start(),
            accepting_states: Default::default(),
        }
    }
}

impl<'a> NFA {
    fn from_expr(p: &'a Pattern) -> Self {
        let mut result = Self::default();
        let start_state = result.start_state;
        nfa_from_pattern(&mut result, &[start_state], p);
        result
    }

    fn add_state(&mut self) -> StateId {
        let result = self.next_state_id;
        self.next_state_id = self.next_state_id.next();
        result
    }

    fn add_transition(&mut self, from: StateId, word: Word, to: StateId) {
        self.transitions.insert((from, to), word);
    }

    fn mark_state_accepting(&mut self, state: StateId) {
        self.accepting_states.insert(state);
    }

    fn walk_transitions_backwards(&self, ending_state: StateId, starting_state: StateId) -> (Word, StateId) {
        loop {
            let transitions: Vec<((StateId, StateId), Word)> = self.transitions.iter().filter(|((_, to), _)| *to == ending_state).map(|(t, w)| (*t, w.clone())).collect();
            assert_eq!(transitions.len(), 1);
            let ((from, to), w) = &transitions[0];
            if *from == starting_state {
                return (w.clone(), *to);
            }
        }
    }

    // Dump to a GraphViz dot format.
    fn to_dot(&self) -> String {
        todo!();
    }
}
