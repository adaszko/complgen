use std::collections::{BTreeMap, HashSet};
use std::fmt::Display;
use std::io::Write;

use complgen::{StateId, START_STATE_ID};
use roaring::{RoaringBitmap, MultiOps};
use crate::epsilon_nfa::NFA as EpsilonNFA;
use crate::epsilon_nfa::Input as EpsilonInput;


#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Input {
    Literal(String),
    Any,
}


impl Input {
    fn from_epsilon_input(value: EpsilonInput) -> Option<Self> {
        match value {
            EpsilonInput::Literal(s) => Some(Self::Literal(s)),
            EpsilonInput::Any => Some(Self::Any),
            EpsilonInput::Epsilon => None,
        }
    }

    pub fn matches(&self, actual: &str) -> bool {
        match self {
            Self::Literal(expected) => actual == *expected,
            Self::Any => true,
        }
    }
}


impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Literal(s) => write!(f, "{}", s),
            Input::Any => write!(f, "*"),
        }
    }
}



pub struct NFA {
    pub start_state: StateId,
    pub unallocated_state_id: StateId,
    pub transitions: BTreeMap<StateId, HashSet<(Input, StateId)>>,
    pub accepting_states: RoaringBitmap,
}


impl Default for NFA {
    fn default() -> Self {
        Self {
            start_state: START_STATE_ID,
            unallocated_state_id: START_STATE_ID + 1,
            transitions: Default::default(),
            accepting_states: Default::default(),
        }
    }
}


fn nfa_from_epsilon_nfa<'a>(epsilon_nfa: &'a EpsilonNFA) -> NFA {
    let mut nfa = NFA::default();
    nfa.accepting_states = epsilon_nfa.accepting_states.clone();
    nfa.start_state = epsilon_nfa.start_state;
    nfa.unallocated_state_id = epsilon_nfa.unallocated_state_id;
    let mut visited: HashSet<StateId> = Default::default();
    let mut to_visit: Vec<StateId> = vec![nfa.start_state];
    while let Some(current_state) = to_visit.pop() {
        if visited.contains(&current_state) {
            continue;
        }

        let transitions_from_current_state = epsilon_nfa.get_transitions_from(current_state);
        to_visit.extend(transitions_from_current_state.iter().map(|(_, to)| *to));

        for (input, to) in transitions_from_current_state {
            if input.is_epsilon() {
                for (from_prime, input_prime) in epsilon_nfa.get_transitions_to(current_state) {
                    nfa.add_transition(from_prime, Input::from_epsilon_input(input_prime).unwrap(), to);
                }
                if epsilon_nfa.is_accepting_state(to) {
                    nfa.accepting_states.insert(current_state);
                }
            }
            else {
                nfa.add_transition(current_state, Input::from_epsilon_input(input).unwrap(), to);
            }
        }

        visited.insert(current_state);
    }
    nfa
}


impl NFA {
    pub fn from_epsilon_nfa(epsilon_nfa: &EpsilonNFA) -> Self {
        nfa_from_epsilon_nfa(&epsilon_nfa)
    }

    pub fn get_all_states(&self) -> RoaringBitmap {
        let mut states: RoaringBitmap = Default::default();
        for (from, to) in &self.transitions {
            states.insert(*from);
            to.iter().for_each(|(_, to)| {
                states.insert(*to);
            });
        }
        states
    }

    pub fn add_state(&mut self) -> StateId {
        let result = self.unallocated_state_id;
        self.unallocated_state_id += 1;
        result
    }

    pub fn mark_state_accepting(&mut self, state: StateId) {
        self.accepting_states.insert(state);
    }

    pub fn remove_transition(&mut self, from: StateId, input: Input, to: StateId) {
        self.transitions.entry(from).or_default().remove(&(input, to));
    }

    pub fn add_transition(&mut self, from: StateId, input: Input, to: StateId) {
        self.transitions.entry(from).or_default().insert((input, to));
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

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut backtracking_stack: Vec<(usize, StateId)> = vec![(0, self.start_state)];
        loop {
            let Some((input_index, current_state)) = backtracking_stack.pop() else {
                return false;
            };

            if self.accepting_states.contains(current_state) {
                return true;
            }

            let transitions_from_current_state = self.get_transitions_from(current_state);

            if input_index < inputs.len() {
                let matching_consuming_transitions: Vec<StateId> = transitions_from_current_state.iter().filter_map(|(expected_input, to)| match expected_input.matches(inputs[input_index]) {
                    false => None,
                    true => Some(*to),
                }).collect();
                backtracking_stack.extend(matching_consuming_transitions.iter().map(|state| (input_index + 1, *state)));
            }
        }
    }

    pub fn to_dot<W: Write>(&self, output: &mut W) -> std::result::Result<(), std::io::Error> {
        writeln!(output, "digraph nfa {{")?;
        writeln!(output, "\trankdir=LR;")?;

        let nonaccepting_states = [&self.get_all_states(), &self.accepting_states].difference();

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
    use crate::grammar::Expr;

    use super::*;

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
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
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }

    #[test]
    fn eliminates_double_epsilon_transition() {
        let mut epsilon_nfa = EpsilonNFA::default();
        let first_state = epsilon_nfa.add_state();
        let second_state = epsilon_nfa.add_state();
        epsilon_nfa.add_transition(first_state, EpsilonInput::Epsilon, second_state);
        let third_state = epsilon_nfa.add_state();
        epsilon_nfa.add_transition(second_state, EpsilonInput::Epsilon, third_state);
        epsilon_nfa.mark_state_accepting(third_state);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&[]));
    }
}
