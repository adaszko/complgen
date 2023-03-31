use std::{
    collections::{BTreeMap, HashSet},
    fmt::Display,
    io::Write,
};

use crate::grammar::Expr;
use complgen::{StateId, START_STATE_ID};
use roaring::{MultiOps, RoaringBitmap};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EpsilonInput {
    Literal(String),
    Any,
    Epsilon,
}

impl Display for EpsilonInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EpsilonInput::Literal(s) => write!(f, "{}", s),
            EpsilonInput::Any => write!(f, "*"),
            EpsilonInput::Epsilon => write!(f, "epsilon"),
        }
    }
}

impl EpsilonInput {
    pub fn is_epsilon(&self) -> bool {
        matches!(self, Self::Epsilon)
    }
}

fn is_deduped(v: &[StateId]) -> bool {
    let mut copy = v.to_vec();
    copy.sort();
    copy.dedup();
    copy == v
}

fn do_nfa_from_expr(nfa: &mut EpsilonNFA, current_states: &[StateId], e: &Expr) -> Vec<StateId> {
    match e {
        Expr::Literal(input) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, EpsilonInput::Literal(input.to_string()), to_state_id);
            }
            return vec![to_state_id];
        }
        Expr::Variable(_) => {
            let to_state_id = nfa.add_state();
            for state in current_states {
                nfa.add_transition(*state, EpsilonInput::Any, to_state_id);
            }
            return vec![to_state_id];
        }
        Expr::Sequence(v) => {
            // Compile into multiple NFA states stringed together by transitions
            let mut states = current_states.to_vec();
            for p in v {
                states = do_nfa_from_expr(nfa, &states, p);
            }
            states
        }
        Expr::Alternative(v) => {
            let mut result: Vec<StateId> = Default::default();
            for p in v {
                result.extend(do_nfa_from_expr(nfa, current_states, p));
            }
            assert!(is_deduped(&result));
            result
        }
        Expr::Optional(e) => {
            let ending_states = do_nfa_from_expr(nfa, current_states, e);
            for current_state in current_states {
                for ending_state in &ending_states {
                    nfa.add_transition(*current_state, EpsilonInput::Epsilon, *ending_state)
                }
            }
            ending_states
        }
        Expr::Many1(e) => {
            let ending_states = do_nfa_from_expr(nfa, current_states, e);
            for ending_state in &ending_states {
                for current_state in current_states {
                    // loop from the ending state to the current one
                    nfa.add_transition(*ending_state, EpsilonInput::Epsilon, *current_state);
                }
            }
            ending_states
        }
    }
}

fn nfa_from_expr(e: &Expr) -> EpsilonNFA {
    let mut nfa = EpsilonNFA::default();
    let start_state = nfa.start_state;
    let ending_states = do_nfa_from_expr(&mut nfa, &[start_state], e);
    for state in ending_states {
        nfa.mark_state_accepting(state);
    }
    nfa
}

pub struct EpsilonNFA {
    pub start_state: StateId,
    pub unallocated_state_id: StateId,
    pub transitions: BTreeMap<StateId, HashSet<(EpsilonInput, StateId)>>,
    pub accepting_states: RoaringBitmap,
}

impl Default for EpsilonNFA {
    fn default() -> Self {
        Self {
            start_state: START_STATE_ID,
            unallocated_state_id: START_STATE_ID + 1,
            transitions: Default::default(),
            accepting_states: Default::default(),
        }
    }
}

impl EpsilonNFA {
    pub fn from_expr(e: &Expr) -> Self {
        nfa_from_expr(e)
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

    pub fn is_starting_state(&self, state: StateId) -> bool {
        state == 0
    }

    pub fn is_accepting_state(&self, state: StateId) -> bool {
        self.accepting_states.contains(state)
    }

    // A set of states reachable from `state` via ϵ-transitions
    // Note: The return value does *not* include `start_state`.
    pub fn get_epsilon_closure_dfs(&self, start_state: StateId) -> Vec<StateId> {
        let mut result: Vec<StateId> = Default::default();
        let mut visited: RoaringBitmap = Default::default();
        let mut to_visit: Vec<StateId> = vec![start_state];
        while let Some(current_state) = to_visit.pop() {
            if visited.contains(current_state) {
                continue;
            }

            for (input, to) in self.get_transitions_from(current_state) {
                if !input.is_epsilon() {
                    continue;
                }

                if visited.contains(to) {
                    continue;
                }

                to_visit.push(to);
            }

            visited.insert(current_state);
            result.push(current_state);
        }
        debug_assert_eq!(result.first(), Some(&start_state));
        result.remove(0);
        result
    }

    pub fn add_state(&mut self) -> StateId {
        let result = self.unallocated_state_id;
        self.unallocated_state_id += 1;
        result
    }

    pub fn add_transition(&mut self, from: StateId, input: EpsilonInput, to: StateId) {
        self.transitions
            .entry(from)
            .or_default()
            .insert((input, to));
    }

    pub fn mark_state_accepting(&mut self, state: StateId) {
        self.accepting_states.insert(state);
    }

    pub fn get_transitions_from(&self, from: StateId) -> HashSet<(EpsilonInput, StateId)> {
        self.transitions
            .get(&from)
            .cloned()
            .unwrap_or(HashSet::default())
    }

    pub fn get_transitions_to(&self, desired_to: StateId) -> HashSet<(StateId, EpsilonInput)> {
        let mut result: HashSet<(StateId, EpsilonInput)> = Default::default();
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

    use crate::grammar::tests::arb_expr_match;

    use super::*;
    use proptest::prelude::*;

    impl EpsilonInput {
        fn matches(&self, actual: &str) -> bool {
            match self {
                Self::Literal(expected) => actual == *expected,
                Self::Any => true,
                Self::Epsilon => false,
            }
        }
    }


    impl EpsilonNFA {
        pub fn get_transitions_from_matching(&self, from: StateId, input: &str) -> RoaringBitmap {
            let empty = HashSet::default();
            let tos = self.transitions.get(&from).unwrap_or_else(|| &empty);
            tos.iter()
                .filter_map(|(transition_input, to)| {
                    if transition_input.matches(input) {
                        Some(*to)
                    } else {
                        None
                    }
                })
                .collect()
        }

        // https://cs.stackexchange.com/questions/88185/nfa-string-acceptance-with-loop-of-epsilon-transitions
        pub fn accepts(&self, inputs: &[&str]) -> bool {
            let mut reachable_states: HashSet<(StateId, usize)> = Default::default();
            for state in self.get_epsilon_closure_dfs(self.start_state) {
                reachable_states.insert((state, 0));
            }
            reachable_states.insert((self.start_state, 0));

            loop {
                if reachable_states.is_empty() {
                    return false;
                }

                for (state, input_index) in &reachable_states {
                    if *input_index == inputs.len() && self.accepting_states.contains(*state) {
                        return true;
                    }
                }

                let mut current_reachable_states: HashSet<(StateId, usize)> = Default::default();
                for (from, input_index) in &reachable_states {
                    for to in self.get_epsilon_closure_dfs(*from) {
                        current_reachable_states.insert((to, *input_index));
                    }
                }

                for (from, input_index) in &reachable_states {
                    if *input_index < inputs.len() {
                        for to in self.get_transitions_from_matching(*from, inputs[*input_index]) {
                            current_reachable_states.insert((to, input_index + 1));
                        }
                    }
                }

                reachable_states = current_reachable_states;
            }
        }
    }

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo"]));
        assert!(nfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo".to_string())));
        let nfa = EpsilonNFA::from_expr(&expr);

        assert!(nfa.accepts(&[]));
        assert!(nfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo".to_string())));
        let nfa = EpsilonNFA::from_expr(&expr);
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

        let nfa = EpsilonNFA::from_expr(&expr);

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
        let nfa = EpsilonNFA::from_expr(&expr);

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

        let nfa = EpsilonNFA::from_expr(&expr);
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

        let nfa = EpsilonNFA::from_expr(&expr);

        assert!(nfa.accepts(&["first", "foo", "bar", "last"]));
    }

    const LITERALS: &[&str] = &["foo", "bar", "--baz", "--quux"];
    const VARIABLES: &[&str] = &["FILE", "DIRECTORY", "PATH"];

    proptest! {
        #[test]
        fn accepts_arb_expr_input((expr, input) in arb_expr_match(Rc::new(LITERALS.iter().map(|s|s.to_string()).collect()), Rc::new(VARIABLES.iter().map(|s|s.to_string()).collect()), 10, 3)) {
            println!("{:?}", expr);
            println!("{:?}", input);
            let nfa = EpsilonNFA::from_expr(&expr);
            let input: Vec<&str> = input.iter().map(|s| {
                let s: &str = s;
                s
            }).collect();
            prop_assert!(nfa.accepts(&input));
        }
    }

    #[test]
    fn accept_does_not_hang_on_epsilon_loop() {
        use Expr::*;
        let expr = Optional(Box::new(Sequence(vec![
            Many1(Box::new(Optional(Box::new(Literal("bar".to_string()))))),
            Variable("DIRECTORY".to_string()),
        ])));
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&[]));
    }

    #[test]
    fn proptest_case_one() {
        use Expr::*;
        let expr = Sequence(vec![
            Sequence(vec![Literal("foo".to_string()), Literal("foo".to_string())]),
            Optional(Box::new(Literal("foo".to_string()))),
        ]);
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "foo"]));
    }

    #[test]
    fn proptest_case_two() {
        use Expr::*;
        let expr = Optional(Box::new(Optional(Box::new(Optional(Box::new(Sequence(
            vec![
                Literal("foo".to_string()),
                Optional(Box::new(Many1(Box::new(Literal("foo".to_string()))))),
            ],
        )))))));
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "foo"]));
    }

    #[test]
    fn proptest_case_three() {
        use Expr::*;
        let expr = Sequence(vec![
            Many1(Box::new(Literal("foo".to_string()))),
            Literal("foo".to_string()),
        ]);
        let nfa = EpsilonNFA::from_expr(&expr);
        assert!(nfa.accepts(&["foo", "foo", "foo"]));
    }
}
