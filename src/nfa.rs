use std::collections::{BTreeMap, HashSet};
use std::fmt::Display;

use crate::automata::StateId;
use crate::parser::Expr;
use crate::epsilon_nfa::NFA as EpsilonNFA;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Input<'a> {
    Literal(&'a str),
    Any,
}


impl<'a> Display for Input<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Literal(s) => write!(f, "{}", s),
            Input::Any => write!(f, "*"),
        }
    }
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
    pub fn from_epsilon_nfa(nfa: EpsilonNFA) -> Self {
        todo!();
    }

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        todo!();
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy");
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo"), Expr::Literal("bar")]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo")));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo")));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
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
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
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

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(epsilon_nfa);
        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }
}
