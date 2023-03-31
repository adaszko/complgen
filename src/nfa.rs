use std::cmp::Ordering;
use std::collections::{BTreeMap, HashSet};
use std::fmt::Display;
use std::io::Write;

use crate::epsilon_nfa::EpsilonInput;
use crate::epsilon_nfa::EpsilonNFA;
use complgen::{StateId, START_STATE_ID};
use roaring::{MultiOps, RoaringBitmap};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd)]
pub enum Input {
    Literal(String),
    Any,
}

impl Ord for Input {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Input::Literal(left), Input::Literal(right)) => left.cmp(right),
            (Input::Literal(_), Input::Any) => Ordering::Less,
            (Input::Any, Input::Literal(_)) => Ordering::Greater,
            (Input::Any, Input::Any) => Ordering::Equal,
        }
    }
}

impl Input {
    fn from_epsilon_input(value: EpsilonInput) -> Option<Self> {
        match value {
            EpsilonInput::Literal(s) => Some(Self::Literal(s)),
            EpsilonInput::Any => Some(Self::Any),
            EpsilonInput::Epsilon => None,
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

#[derive(Clone)]
pub struct NFA {
    pub starting_states: RoaringBitmap,
    pub unallocated_state_id: StateId,
    pub transitions: BTreeMap<StateId, HashSet<(Input, StateId)>>,
    pub accepting_states: RoaringBitmap,
}

impl Default for NFA {
    fn default() -> Self {
        Self {
            starting_states: RoaringBitmap::from_iter(&[START_STATE_ID]),
            unallocated_state_id: START_STATE_ID + 1,
            transitions: Default::default(),
            accepting_states: Default::default(),
        }
    }
}

// https://www.javatpoint.com/automata-eliminating-null-transitions
// https://cstaleem.com/elimination-of-epsilon-%CE%B5-from-nfa
fn nfa_from_epsilon_nfa<'a>(epsilon_nfa: &'a EpsilonNFA) -> NFA {
    let mut result = NFA::default();

    result.accepting_states = epsilon_nfa.accepting_states.clone();
    result.starting_states = RoaringBitmap::from_iter(&[epsilon_nfa.start_state]);
    result.unallocated_state_id = epsilon_nfa.unallocated_state_id;

    for (from, tos) in &epsilon_nfa.transitions {
        for (input, to) in tos {
            if !input.is_epsilon() {
                result.add_transition(
                    *from,
                    Input::from_epsilon_input(input.clone()).unwrap(),
                    *to,
                );
            }
        }
    }

    for from in epsilon_nfa.get_all_states() {
        let mut epsilon_closure = epsilon_nfa.get_epsilon_closure_dfs(from);
        epsilon_closure.reverse();
        for to in epsilon_closure {
            for (from_prime, input_prime) in epsilon_nfa.get_transitions_to(from) {
                if input_prime.is_epsilon() {
                    continue;
                }
                result.add_transition(
                    from_prime,
                    Input::from_epsilon_input(input_prime).unwrap(),
                    to,
                );
            }
            if epsilon_nfa.is_starting_state(from) {
                result.starting_states.insert(to);
            }
            if epsilon_nfa.is_accepting_state(to) {
                result.accepting_states.insert(from);
            }
        }
    }

    result
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

    pub fn add_transition(&mut self, from: StateId, input: Input, to: StateId) {
        self.transitions
            .entry(from)
            .or_default()
            .insert((input, to));
    }

    pub fn get_transitions_from(&self, from: StateId) -> HashSet<(Input, StateId)> {
        self.transitions
            .get(&from)
            .cloned()
            .unwrap_or(HashSet::default())
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
    use std::rc::Rc;

    use crate::grammar::Expr;
    use crate::grammar::tests::arb_expr_match;

    use super::*;

    use proptest::prelude::*;

    impl Input {
        pub fn matches(&self, actual: &str) -> bool {
            match self {
                Self::Literal(expected) => actual == *expected,
                Self::Any => true,
            }
        }
    }

    impl NFA {
        pub fn mark_state_accepting(&mut self, state: StateId) {
            self.accepting_states.insert(state);
        }

        pub fn accepts(&self, inputs: &[&str]) -> bool {
            let mut to_visit: Vec<(usize, StateId)> = self
                .starting_states
                .iter()
                .map(|state| (0, state))
                .collect();
            loop {
                let Some((input_index, current_state)) = to_visit.pop() else {
                    return false;
                };

                if input_index == inputs.len() && self.accepting_states.contains(current_state) {
                    return true;
                }

                let transitions_from_current_state = self.get_transitions_from(current_state);

                if input_index < inputs.len() {
                    let matching_consuming_transitions: Vec<StateId> = transitions_from_current_state
                        .iter()
                        .filter_map(|(expected_input, to)| {
                            match expected_input.matches(inputs[input_index]) {
                                false => None,
                                true => Some(*to),
                            }
                        })
                        .collect();
                    to_visit.extend(
                        matching_consuming_transitions
                            .iter()
                            .map(|state| (input_index + 1, *state)),
                    );
                }
            }
        }
    }

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
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
            Expr::Optional(Box::new(Expr::Alternative(vec![
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
            Expr::Many1(Box::new(Expr::Alternative(vec![
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
        epsilon_nfa.add_transition(0, EpsilonInput::Epsilon, 1);
        epsilon_nfa.add_transition(1, EpsilonInput::Epsilon, 2);
        epsilon_nfa.mark_state_accepting(2);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&[]));
    }

    const LITERALS: &[&str] = &["foo", "bar", "--baz", "--quux"];
    const VARIABLES: &[&str] = &["FILE", "DIRECTORY", "PATH"];

    proptest! {
        #[test]
        fn accepts_arb_expr_input((expr, input) in arb_expr_match(Rc::new(LITERALS.iter().map(|s|s.to_string()).collect()), Rc::new(VARIABLES.iter().map(|s|s.to_string()).collect()), 10, 3)) {
            println!("{:?}", expr);
            println!("{:?}", input);
            let epsilon_nfa = EpsilonNFA::from_expr(&expr);
            let input: Vec<&str> = input.iter().map(|s| {
                let s: &str = s;
                s
            }).collect();
            prop_assert!(epsilon_nfa.accepts(&input));
            let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
            prop_assert!(nfa.accepts(&input));
        }
    }

    #[test]
    fn proptest_failure_one() {
        use Expr::*;
        let expr = Many1(Box::new(Sequence(vec![
            Alternative(vec![
                Many1(Box::new(Sequence(vec![
                    Alternative(vec![
                        Many1(Box::new(Optional(Box::new(Sequence(vec![
                            Literal("--baz".to_string()),
                            Literal("foo".to_string()),
                        ]))))),
                        Optional(Box::new(Literal("--baz".to_string()))),
                    ]),
                    Literal("foo".to_string()),
                ]))),
                Sequence(vec![
                    Optional(Box::new(Literal("foo".to_string()))),
                    Literal("foo".to_string()),
                ]),
            ]),
            Literal("foo".to_string()),
        ])));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        nfa.accepts(&["foo", "foo"]);
    }
}
