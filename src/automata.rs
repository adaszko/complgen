use std::collections::{HashSet, BTreeMap};

use crate::parser::Expr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Input<'a> {
    Literal(&'a str),
    Any,
    Epsilon,
}


impl<'a> Input<'a> {
    fn matches(&self, actual: &str) -> bool {
        match self {
            Self::Literal(expected) => actual == *expected,
            Self::Any => true,
            Self::Epsilon => false,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct StateId(usize);

impl StateId {
    fn start() -> Self {
        StateId(0)
    }

    fn advance(&mut self) {
        self.0 += 1;
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

fn nfa_from_expr<'a>(nfa: &mut NFA<'a>, current_states: &[StateId], e: &Expr<'a>) -> Vec<StateId> {
    match e {
        Expr::Literal(input) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, Input::Literal(input), to_state_id);
            }
            return vec![to_state_id];
        },
        Expr::Variable(_) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, Input::Any, to_state_id);
            }
            return vec![to_state_id];
        },
        Expr::Sequence(v) => {
            // Compile into multiple NFA states stringed together by transitions
            let mut states = current_states.to_vec();
            for p in v {
                states = nfa_from_expr(nfa, &states, p);
            }
            states
        },
        Expr::Alternative(v) => {
            let mut result: Vec<StateId> = Default::default();
            for p in v {
                result.extend(nfa_from_expr(nfa, current_states, p));
            }
            assert!(is_deduped(&result));
            result
        },
        Expr::Optional(e) => {
            let ending_states = nfa_from_expr(nfa, current_states, e);
            for current_state in current_states {
                for ending_state in &ending_states {
                    nfa.add_transition(*current_state, Input::Epsilon, *ending_state)
                }
            }
            ending_states
        },
        Expr::Many1(e) => {
            let ending_states = nfa_from_expr(nfa, current_states, e);
            for ending_state in &ending_states {
                for current_state in current_states {
                    // loop from the ending state to the current one
                    nfa.add_transition(*ending_state, Input::Epsilon, *current_state);
                }
            }
            ending_states
        },
    }
}

struct NFA<'a> {
    start_state: StateId,
    unallocated_state_id: StateId,
    transitions: BTreeMap<StateId, HashSet<(Input<'a>, StateId)>>,
    accepting_states: HashSet<StateId>,
}

impl<'a> Default for NFA<'a> {
    fn default() -> Self {
        Self {
            unallocated_state_id: StateId::start(),
            transitions: Default::default(),
            start_state: StateId::start(),
            accepting_states: Default::default(),
        }
    }
}

impl<'a> NFA<'a> {
    fn from_expr(p: &'a Expr) -> Self {
        let mut result = Self::default();
        let start_state = result.start_state;
        nfa_from_expr(&mut result, &[start_state], p);
        result
    }

    fn is_accepting_state(&self, state: StateId) -> bool {
        self.accepting_states.contains(&state)
    }

    fn accepts(&self, inputs: &[&str]) -> bool {
        let mut visited: HashSet<StateId> = Default::default();
        let mut backtracking_stack: Vec<(usize, StateId)> = Default::default();

        let mut input_index = 0;
        let mut current_state = self.start_state;
        loop {
            if input_index >= inputs.len() {
                break;
            }

            let transitions_from_current_state = self.transitions.get(&current_state).cloned().unwrap_or(HashSet::default());
            let matching_consuming_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input.matches(inputs[input_index]) {
                false => None,
                true => Some(*to),
            }).collect();

            let epsilon_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input {
               Input::Epsilon => Some(*to),
                _ => None,
            }).collect();

            if !visited.contains(&current_state) {
                backtracking_stack.extend(epsilon_transitions.into_iter().map(|state| (input_index, state)));
                backtracking_stack.extend(matching_consuming_transitions[1..].iter().map(|state| (input_index, *state)));
                visited.insert(current_state);
            }
            if let Some(state) = matching_consuming_transitions.first() {
                current_state = *state;
                input_index += 1;
            }
            else {
                // backtrack
                if let Some((index, state)) = backtracking_stack.pop() {
                    input_index = index;
                    current_state = state;
                }
                else {
                    break;
                }
            }
        }
        self.is_accepting_state(current_state)
    }

    fn add_state(&mut self) -> StateId {
        let result = self.unallocated_state_id;
        self.unallocated_state_id.advance();
        result
    }

    fn add_transition(&mut self, from: StateId, input: Input<'a>, to: StateId) {
        self.transitions.entry(from).or_default().insert((input, to));
    }

    fn mark_state_accepting(&mut self, state: StateId) {
        self.accepting_states.insert(state);
    }

    // Dump to a GraphViz dot format.
    fn to_dot(&self) -> String {
        // https://graphviz.org/Gallery/directed/fsm.html
        todo!();
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_any_word() {
        let pattern = Expr::Variable("dummy");
        let nfa = NFA::from_expr(&pattern);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let pattern = Expr::Sequence(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let nfa = NFA::from_expr(&pattern);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let pattern = Expr::Alternative(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let nfa = NFA::from_expr(&pattern);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let pattern = Expr::Optional(Box::new(Expr::Literal("foo")));
        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let pattern = Expr::Many1(Box::new(Expr::Literal("foo")));
        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["foo", "foo"]));
    }

    // The most complicated part is how complex patterns combine with each other.

    #[test]
    fn accepts_sequence_containing_a_choice() {
        let pattern = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Alternative(vec![
                Expr::Literal("foo"),
                Expr::Literal("bar"),
            ]),
            Expr::Literal("last"),
        ]);

        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
    }

    #[test]
    fn accepts_choice_containing_a_sequence() {
        let pattern = Expr::Alternative(vec![
            Expr::Sequence(vec![
                Expr::Literal("foo"),
                Expr::Literal("bar"),
            ]),
            Expr::Sequence(vec![
                Expr::Literal("foo"),
                Expr::Literal("baz"),
            ]),
        ]);
        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&["foo", "bar"]));
        assert!(nfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn accepts_optional_containing_a_choice() {
        let pattern = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Optional(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo"),
                    Expr::Literal("bar"),
            ]))),
            Expr::Literal("last"),
        ]);

        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
        assert!(nfa.accepts(&["first", "last"]));
    }

    #[test]
    fn accepts_repetition_of_a_choice() {
        let pattern = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Many1(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo"),
                    Expr::Literal("bar"),
            ]))),
            Expr::Literal("first"),
        ]);

        let nfa = NFA::from_expr(&pattern);

        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }
}
