use std::{collections::{HashSet, BTreeMap}, io::Write, fmt::Display};

use crate::{grammar::Expr, automata::StateId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal(String),
    Any,
    Epsilon,
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Literal(s) => write!(f, "{}", s),
            Input::Any => write!(f, "*"),
            Input::Epsilon => write!(f, "epsilon"),
        }
    }
}


impl Input {
    fn matches(&self, actual: &str) -> bool {
        match self {
            Self::Literal(expected) => actual == *expected,
            Self::Any => true,
            Self::Epsilon => false,
        }
    }

    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }
}


fn is_deduped(v: &[StateId]) -> bool {
    let mut copy = v.to_vec();
    copy.sort();
    copy.dedup();
    copy == v
}

fn do_nfa_from_expr(nfa: &mut NFA, current_states: &[StateId], e: &Expr) -> Vec<StateId> {
    match e {
        Expr::Literal(input) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, Input::Literal(input.to_string()), to_state_id);
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


fn nfa_from_expr(e: &Expr) -> NFA {
    let mut nfa = NFA::default();
    let start_state = nfa.start_state;
    let ending_states = do_nfa_from_expr(&mut nfa, &[start_state], e);
    for state in ending_states {
        nfa.mark_state_accepting(state);
    }
    nfa
}

pub struct NFA {
    pub start_state: StateId,
    pub unallocated_state_id: StateId,
    pub transitions: BTreeMap<StateId, HashSet<(Input, StateId)>>,
    pub accepting_states: HashSet<StateId>,
}

impl Default for NFA {
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

impl NFA {
    pub fn from_expr(e: &Expr) -> Self {
        nfa_from_expr(e)
    }

    pub fn is_accepting_state(&self, state: StateId) -> bool {
        self.accepting_states.contains(&state)
    }

    // TODO Handle epsilon loops: https://cs.stackexchange.com/questions/88185/nfa-string-acceptance-with-loop-of-epsilon-transitions
    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut backtracking_stack: Vec<(usize, StateId)> = vec![(0, self.start_state)];
        while let Some((input_index, current_state)) = backtracking_stack.pop() {
            if self.is_accepting_state(current_state) {
                return true;
            }

            let transitions_from_current_state = self.get_transitions_from(current_state);

            let epsilon_transitions: Vec<StateId> = transitions_from_current_state.iter().filter(|(input, _)| input.is_epsilon()).map(|(_, to)| *to).collect();
            backtracking_stack.extend(epsilon_transitions.into_iter().map(|state| (input_index, state)));

            if input_index < inputs.len() {
                let matching_consuming_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input.matches(inputs[input_index]) {
                    false => None,
                    true => Some(*to),
                }).collect();
                backtracking_stack.extend(matching_consuming_transitions.iter().map(|state| (input_index + 1, *state)));
            }
        }
        false
    }

    pub fn add_state(&mut self) -> StateId {
        let result = self.unallocated_state_id;
        self.unallocated_state_id.advance();
        result
    }

    pub fn add_transition(&mut self, from: StateId, input: Input, to: StateId) {
        self.transitions.entry(from).or_default().insert((input, to));
    }

    pub fn mark_state_accepting(&mut self, state: StateId) {
        self.accepting_states.insert(state);
    }

    pub fn get_transitions_from(&self, from: StateId) -> HashSet<(Input, StateId)> {
        self.transitions.get(&from).cloned().unwrap_or(HashSet::default())
    }

    pub fn get_transitions_to(&self, desired_to: StateId) -> HashSet<(StateId, Input)> {
        let mut result: HashSet<(StateId, Input)> = Default::default();
        for (from, tos) in &self.transitions {
            for (input, to) in tos {
                if desired_to == *to {
                    result.insert((*from, input.clone()));
                }
            }
        }
        result
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
    use std::rc::Rc;

    use crate::grammar::{arb_expr_match};

    use super::*;
    use proptest::prelude::*;

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo".to_string())));
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo".to_string())));
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["foo", "foo"]));
    }

    // The most complicated part is how complex patterns combine with each other.

    #[test]
    fn accepts_sequence_containing_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first".to_string()),
            Expr::Alternative(vec![
                Expr::Literal("foo".to_string()),
                Expr::Literal("bar".to_string()),
            ]),
            Expr::Literal("last".to_string()),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
    }

    #[test]
    fn accepts_choice_containing_a_sequence() {
        let expr = Expr::Alternative(vec![
            Expr::Sequence(vec![
                Expr::Literal("foo".to_string()),
                Expr::Literal("bar".to_string()),
            ]),
            Expr::Sequence(vec![
                Expr::Literal("foo".to_string()),
                Expr::Literal("baz".to_string()),
            ]),
        ]);
        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["foo", "bar"]));
        assert!(nfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn accepts_optional_containing_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first".to_string()),
            Expr::Optional(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo".to_string()),
                    Expr::Literal("bar".to_string()),
            ]))),
            Expr::Literal("last".to_string()),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "last"]));
        assert!(nfa.accepts(&["first", "bar", "last"]));
        assert!(nfa.accepts(&["first", "last"]));
    }

    #[test]
    fn accepts_repetition_of_a_choice() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("first".to_string()),
            Expr::Many1(
                Box::new(Expr::Alternative(vec![
                    Expr::Literal("foo".to_string()),
                    Expr::Literal("bar".to_string()),
            ]))),
            Expr::Literal("last".to_string()),
        ]);

        let nfa = NFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }

    #[test]
    fn accept_does_not_hang_on_epsilon_loop() {
        use Expr::*;
        let expr = Sequence(vec![Optional(Box::new(Many1(Box::new(Literal("bar".to_string()))))), Literal("bar".to_string())]);
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&[]));

        let expr = Optional(Box::new(Sequence(vec![Many1(Box::new(Optional(Box::new(Literal("bar".to_string()))))), Variable("DIRECTORY".to_string())])));
        let nfa = NFA::from_expr(&expr);
        assert!(nfa.accepts(&[]));
    }

    const INPUTS_ALPHABET: &[&str] = &["foo", "bar", "--baz", "--quux"];
    const VARIABLES_ALPHABET: &[&str] = &["FILE", "DIRECTORY", "PATH"];

    proptest! {
        #[test]
        fn accepts_arb_expr_input((expr, input) in arb_expr_match(Rc::new(INPUTS_ALPHABET.iter().map(|s|s.to_string()).collect()), Rc::new(VARIABLES_ALPHABET.iter().map(|s|s.to_string()).collect()), 10, 3)) {
            println!("{:?}", expr);
            println!("{:?}", input);
            let nfa = NFA::from_expr(&expr);
            let input: Vec<&str> = input.iter().map(|s| {
                let s: &str = s;
                s
            }).collect();
            assert!(nfa.accepts(&input));
        }
    }
}
