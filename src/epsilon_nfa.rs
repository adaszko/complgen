use std::{collections::{HashSet, BTreeMap}, io::Write, fmt::Display};

use crate::{parser::Expr, automata::StateId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Input<'a> {
    Literal(&'a str),
    Any,
    Epsilon,
}

impl<'a> Display for Input<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Literal(s) => write!(f, "{}", s),
            Input::Any => write!(f, "*"),
            Input::Epsilon => write!(f, "epsilon"),
        }
    }
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


fn is_deduped(v: &[StateId]) -> bool {
    let mut copy = v.to_vec();
    copy.sort();
    copy.dedup();
    copy == v
}

fn do_nfa_from_expr<'a>(nfa: &mut NFA<'a>, current_states: &[StateId], e: &Expr<'a>) -> Vec<StateId> {
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
                states = do_nfa_from_expr(nfa, &states, p);
            }
            states
        },
        Expr::Alternative(v) => {
            let mut result: Vec<StateId> = Default::default();
            for p in v {
                result.extend(do_nfa_from_expr(nfa, current_states, p));
            }
            assert!(is_deduped(&result));
            result
        },
        Expr::Optional(e) => {
            let ending_states = do_nfa_from_expr(nfa, current_states, e);
            for current_state in current_states {
                for ending_state in &ending_states {
                    nfa.add_transition(*current_state, Input::Epsilon, *ending_state)
                }
            }
            ending_states
        },
        Expr::Many1(e) => {
            let ending_states = do_nfa_from_expr(nfa, current_states, e);
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


fn nfa_from_expr<'a>(e: &Expr<'a>) -> NFA<'a> {
    let mut nfa = NFA::default();
    let start_state = nfa.start_state;
    let ending_states = do_nfa_from_expr(&mut nfa, &[start_state], e);
    for state in ending_states {
        nfa.mark_state_accepting(state);
    }
    nfa
}

pub struct NFA<'a> {
    start_state: StateId,
    unallocated_state_id: StateId,
    transitions: BTreeMap<StateId, HashSet<(Input<'a>, StateId)>>,
    accepting_states: HashSet<StateId>,
}

impl<'a> Default for NFA<'a> {
    fn default() -> Self {
        let start_state = StateId::start();
        let mut unallocated_state_id = start_state;
        unallocated_state_id.advance();
        Self {
            start_state: StateId::start(),
            unallocated_state_id,
            transitions: Default::default(),
            accepting_states: Default::default(),
        }
    }
}

impl<'a> NFA<'a> {
    pub fn from_expr(e: &'a Expr) -> Self {
        nfa_from_expr(e)
    }

    fn is_accepting_state(&self, state: StateId) -> bool {
        self.accepting_states.contains(&state)
    }

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut visited_epsilons: HashSet<StateId> = Default::default();
        let mut visited_matching: HashSet<StateId> = Default::default();
        let mut backtracking_stack: Vec<(usize, StateId)> = Default::default();

        let mut input_index = 0;
        let mut current_state = self.start_state;
        loop {
            let transitions_from_current_state = self.transitions.get(&current_state).cloned().unwrap_or(HashSet::default());

            if !visited_epsilons.contains(&current_state) {
                let epsilon_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input {
                    Input::Epsilon => Some(*to),
                    _ => None,
                }).collect();
                backtracking_stack.extend(epsilon_transitions.into_iter().map(|state| (input_index, state)));
                visited_epsilons.insert(current_state);
            }

            if input_index >= inputs.len() {
                if self.is_accepting_state(current_state) {
                    return true;
                }
                // backtrack
                if let Some((index, state)) = backtracking_stack.pop() {
                    input_index = index;
                    current_state = state;
                    continue;
                }
                else {
                    break;
                }
            }

            let matching_consuming_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input.matches(inputs[input_index]) {
                false => None,
                true => Some(*to),
            }).collect();

            if !visited_matching.contains(&current_state) {
                if let Some(rest) = matching_consuming_transitions.get(1..) {
                    backtracking_stack.extend(rest.iter().map(|state| (input_index + 1, *state)));
                }
                visited_matching.insert(current_state);
            }
            if let Some(state) = matching_consuming_transitions.first() {
                current_state = *state;
                input_index += 1;
                continue;
            }
            else {
                // backtrack
                if let Some((index, state)) = backtracking_stack.pop() {
                    input_index = index;
                    current_state = state;
                    continue;
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

    pub fn to_dot<W: Write>(&self, output: &mut W) -> std::result::Result<(), std::io::Error> {
        writeln!(output, "digraph nfa {{")?;
        writeln!(output, "\trankdir=LR;")?;

        let nonaccepting_states: HashSet<StateId> = {
            let mut states: HashSet<StateId> = Default::default();
            for (from, to) in &self.transitions {
                states.insert(*from);
                to.iter().for_each(|(_, to)| {
                    states.insert(*to);
                });
            }
            states.difference(&self.accepting_states).copied().collect()
        };

        writeln!(output, "\tnode [shape = circle];")?;
        for state in nonaccepting_states {
            writeln!(output, "\t_{}[label=\"{}\"];", state, state)?;
        }

        write!(output, "\n")?;

        writeln!(output, "\tnode [shape = doublecircle];")?;
        for state in &self.accepting_states {
            writeln!(output, "\t_{}[label=\"{}\"];", state, state)?;
        }

        write!(output, "\n")?;

        for (from, to) in &self.transitions {
            for (input, to) in to {
                writeln!(output, "\t_{} -> _{} [label = \"{}\"];", from, to, input)?;
            }
        }

        writeln!(output, "}}")?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn to_dot_file<P: AsRef<std::path::Path>>(
        &self,
        path: P,
    ) -> std::result::Result<(), std::io::Error> {
        let mut file = std::fs::File::create(path)?;
        self.to_dot(&mut file)?;
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy");
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo")));
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo")));
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["foo", "foo"]));
    }

    // The most complicated part is how complex patterns combine with each other.

    #[test]
    fn accepts_sequence_containing_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Alternative(vec![
                Expr::Literal("foo"),
                Expr::Literal("bar"),
            ]),
            Expr::Literal("last"),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
    }

    #[test]
    fn accepts_choice_containing_a_sequence() {
        let expr = Expr::Alternative(vec![
            Expr::Sequence(vec![
                Expr::Literal("foo"),
                Expr::Literal("bar"),
            ]),
            Expr::Sequence(vec![
                Expr::Literal("foo"),
                Expr::Literal("baz"),
            ]),
        ]);
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["foo", "bar"]));
        assert!(nfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn accepts_optional_containing_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Optional(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo"),
                    Expr::Literal("bar"),
            ]))),
            Expr::Literal("last"),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
        assert!(nfa.accepts(&["first", "last"]));
    }

    #[test]
    fn accepts_repetition_of_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first"),
            Expr::Many1(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo"),
                    Expr::Literal("bar"),
            ]))),
            Expr::Literal("last"),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }
}
