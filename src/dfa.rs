use std::{collections::{HashSet, BTreeMap, HashMap}, io::Write};

use crate::nfa::NFA;
use crate::{automata::StateId, nfa::Input};

pub struct DFA<'a> {
    start_state: StateId,
    unallocated_state_id: StateId,
    transitions: BTreeMap<StateId, HashMap<Input<'a>, StateId>>,
    accepting_states: HashSet<StateId>,
}


impl<'a> Default for DFA<'a> {
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


fn dfa_from_nfa<'a>(_nfa: &'a NFA<'a>) -> DFA<'a> {
    todo!();
}


impl<'a> DFA<'a> {
    pub fn from_nfa(nfa: &'a NFA<'a>) -> Self {
        dfa_from_nfa(&nfa)
    }

    pub fn get_transitions_from(&self, from: StateId) -> HashMap<Input, StateId> {
        self.transitions.get(&from).cloned().unwrap_or(HashMap::default())
    }

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut backtracking_stack: Vec<(usize, StateId)> = vec![(0, self.start_state)];
        while let Some((input_index, current_state)) = backtracking_stack.pop() {
            if self.accepting_states.contains(&current_state) {
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
        false
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
    use crate::parser::Expr;
    use crate::epsilon_nfa::NFA as EpsilonNFA;

    use super::*;

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy");
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo")));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&[]));
        assert!(dfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo")));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "foo"]));
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "last"]));
        assert!(dfa.accepts(&["first", "bar", "last"]));
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
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
        assert!(dfa.accepts(&["foo", "baz"]));
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "last"]));
        assert!(dfa.accepts(&["first", "bar", "last"]));
        assert!(dfa.accepts(&["first", "last"]));
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "bar", "last"]));
    }
}
