use std::{
    collections::{BTreeMap, HashMap, HashSet, BTreeSet},
    io::Write
};

use roaring::{MultiOps, RoaringBitmap};

use crate::nfa::{Input, NFA};
use complgen::{StateId, START_STATE_ID};

#[derive(Clone)]
pub struct DFA {
    pub starting_states: RoaringBitmap,
    transitions: BTreeMap<StateId, HashMap<Input, StateId>>,
    accepting_states: RoaringBitmap,
    unallocated_state_id: StateId,
}

impl Default for DFA {
    fn default() -> Self {
        Self {
            starting_states: RoaringBitmap::from_iter([START_STATE_ID]),
            transitions: Default::default(),
            accepting_states: Default::default(),
            unallocated_state_id: START_STATE_ID + 1,
        }
    }
}

// Assumption: `v` is already sorted by key
// Vec::partition_dedup_by_key() is unstable at the time of writing this
fn group_by_key<T, K, F>(v: &[T], get_key_fn: F) -> Vec<(K, &[T])>
where
    T: Clone,
    K: Eq,
    F: Fn(T) -> K,
{
    let mut result: Vec<(K, &[T])> = Default::default();
    let mut i = 0;
    loop {
        if i >= v.len() {
            break;
        }
        let slice_start = i;
        let group_representant = get_key_fn(v[i].clone());
        i += 1;
        while i < v.len() && get_key_fn(v[i].clone()) == group_representant {
            i += 1;
        }
        result.push((group_representant, &v[slice_start..i]));
    }
    result
}

fn cleanup_dfa(dfa: &DFA) -> DFA {
    let reachable_states = dfa.get_reachable_states();
    let mut result: DFA = Default::default();
    result.starting_states = dfa.starting_states.clone();
    for (from, tos) in &dfa.transitions {
        for (input, to) in tos {
            if reachable_states.contains(*from) && reachable_states.contains(*to) {
                result.add_transition(*from, input.clone(), *to);
            }
        }
    }
    for state in &dfa.accepting_states {
        if reachable_states.contains(state) {
            result.accepting_states.insert(state);
        }
    }
    result.unallocated_state_id = reachable_states.iter().max().map(|m| m + 1).unwrap_or(0);
    result
}

// This is just a simple but non-exhaustive minimization technique that shaves ~500 lines out the
// resulting Bash script.
// TODO Implement a full Hopcroft algorithm: https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft's_algorithm
fn collapse_equivalent_states(dfa: &DFA) -> DFA {
    let mut result: DFA = dfa.clone();

    let mut equivalent_states_from_outgoing_transitions: HashMap<(bool, BTreeSet<(Input, StateId)>), RoaringBitmap> = Default::default();
    for from in dfa.get_all_states() {
        let outgoing_transitions: BTreeSet<(Input, StateId)> = dfa.get_transitions_from(from).iter().map(|(input, to)| (input.clone(), *to)).collect();
        let is_accepting_state = dfa.accepting_states.contains(from);
        equivalent_states_from_outgoing_transitions.entry((is_accepting_state, outgoing_transitions)).or_default().insert(from);
    }

    let mut representant_state: HashMap<StateId, StateId> = Default::default();
    for (_, equivalent_states) in equivalent_states_from_outgoing_transitions {
        let representant = equivalent_states.iter().next().unwrap();
        for state in equivalent_states {
            representant_state.insert(state, representant);
        }
    }

    for (from, tos) in &dfa.transitions {
        for (_, to) in tos {
            if let Some(representant) = representant_state.get(&to) {
                if *representant != *to {
                    result.replace_transition_target_state(*from, *to, *representant);
                }
            }
        }
    }

    result
}

fn dfa_from_determinized_nfa(nfa: &NFA, transitions: &HashSet<(BTreeSet<StateId>, Input, BTreeSet<StateId>)>, accepting_states: &HashSet<BTreeSet<StateId>>) -> DFA {
    let mut result: DFA = Default::default();
    result.unallocated_state_id = nfa.unallocated_state_id;
    result.starting_states = nfa.starting_states.clone();

    let mut new_states: HashMap<BTreeSet<StateId>, StateId> = Default::default();
    for (from, input, to) in transitions {
        let from_state_id = if from.len() == 1 {
            *from.iter().next().unwrap()
        }
        else {
            *new_states.entry(from.clone()).or_insert_with(|| result.add_state())
        };
        let to_state_id = if to.len() == 1 {
            *to.iter().next().unwrap()
        }
        else {
            *new_states.entry(to.clone()).or_insert_with(|| result.add_state())
        };
        result.add_transition(from_state_id, input.clone(), to_state_id);
    }

    result.accepting_states = nfa.accepting_states.clone();
    for to in accepting_states {
        if to.len() == 1 {
            continue;
        }
        let to_state_id = *new_states.entry(to.clone()).or_insert_with(|| result.add_state());
        result.accepting_states.insert(to_state_id);
    }

    result = collapse_equivalent_states(&result);
    cleanup_dfa(&result)
}

// https://www.gatevidyalay.com/converting-nfa-to-dfa-solved-examples/
// Approach used: Induction on the transitions table.
fn dfa_from_nfa(nfa: &NFA) -> DFA {
    let mut accepting_states: HashSet<BTreeSet<StateId>> = Default::default();

    let mut transitions: HashSet<(BTreeSet<StateId>, Input, BTreeSet<StateId>)> = Default::default();
    let mut scalar_transitions: HashSet<(StateId, Input, StateId)> = Default::default();
    for from in &nfa.starting_states {
        scalar_transitions.extend(nfa.get_transitions_from(from).into_iter().map(|(input, to)| (from, input, to)));
    }
    let mut scalar_transitions: Vec<(StateId, Input, StateId)> = scalar_transitions.into_iter().collect();
    scalar_transitions.sort_by_key(|(from, input, _)| (*from, input.clone()));
    for (_, common_input_transitions) in group_by_key(&scalar_transitions, |(from, input, _)| (from, input)) {
        let combined_state: BTreeSet<StateId> = common_input_transitions.iter().map(|(_, _, to)| *to).collect();
        for (from, input, q) in common_input_transitions {
            let from_combined_state = BTreeSet::from_iter([*from]);
            transitions.insert((from_combined_state, input.clone(), combined_state.clone()));
            if nfa.accepting_states.contains(*q) {
                accepting_states.insert(combined_state.clone());
            }
        }
    }

    let mut result_transitions: HashSet<(BTreeSet<StateId>, Input, BTreeSet<StateId>)> = transitions.clone();
    while !transitions.is_empty() {
        let mut new_transitions: HashSet<(BTreeSet<StateId>, Input, BTreeSet<StateId>)> = Default::default();

        for (_, _, to) in transitions {
            let mut scalar_transitions: HashSet<(StateId, Input, StateId)> = Default::default();
            for to_prime in &to {
                scalar_transitions.extend(nfa.get_transitions_from(*to_prime).into_iter().map(|(input, to)| (*to_prime, input, to)));
            }
            let mut scalar_transitions: Vec<(StateId, Input, StateId)> = scalar_transitions.into_iter().collect();
            scalar_transitions.sort_by_key(|(_, input, _)| input.clone());
            for (_, common_input_transitions) in group_by_key(&scalar_transitions, |(_, input, _)| input) {
                let combined_state: BTreeSet<StateId> = common_input_transitions.iter().map(|(_, _, to)| *to).collect();
                for (_, input, q) in common_input_transitions {
                    if result_transitions.contains(&(to.clone(), input.clone(), combined_state.clone())) {
                        continue;
                    }
                    new_transitions.insert((to.clone(), input.clone(), combined_state.clone()));
                    if nfa.accepting_states.contains(*q) {
                        accepting_states.insert(combined_state.clone());
                    }
                }
            }
        }

        result_transitions.extend(new_transitions.clone());
        transitions = new_transitions;
    }

    dfa_from_determinized_nfa(nfa, &result_transitions, &accepting_states)
}

impl DFA {
    pub fn from_nfa(nfa: &NFA) -> Self {
        dfa_from_nfa(&nfa)
    }

    pub fn add_state(&mut self) -> StateId {
        let result = self.unallocated_state_id;
        self.unallocated_state_id += 1;
        result
    }

    pub fn add_transition(&mut self, from: StateId, input: Input, to: StateId) {
        self.transitions
            .entry(from)
            .or_default()
            .insert(input, to);
    }

    pub fn replace_transition_target_state(&mut self, from: StateId, to: StateId, replacement: StateId) {
        self.transitions.entry(from).and_modify(|e| {
            e.iter_mut().for_each(|(_, v)| {
                if *v == to {
                    *v = replacement
                }
            });
        });
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

    pub fn get_reachable_states(&self) -> RoaringBitmap {
        let mut visited: RoaringBitmap = Default::default();
        let mut to_visit: Vec<StateId> = self.starting_states.iter().collect();
        while let Some(current_state) = to_visit.pop() {
            if visited.contains(current_state) {
                continue;
            }

            for (_, to) in self.get_transitions_from(current_state) {
                if !visited.contains(to) {
                    to_visit.push(to);
                }
            }

            visited.insert(current_state);
        }
        visited
    }

    pub fn get_transitions_from(&self, from: StateId) -> HashMap<Input, StateId> {
        self.transitions
            .get(&from)
            .cloned()
            .unwrap_or(HashMap::default())
    }

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut backtracking_stack: Vec<(usize, StateId)> =
            self.starting_states.iter().map(|state| (0, state)).collect();
        while let Some((input_index, current_state)) = backtracking_stack.pop() {
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
                backtracking_stack.extend(
                    matching_consuming_transitions
                        .iter()
                        .map(|state| (input_index + 1, *state)),
                );
            }
        }
        false
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

    use crate::epsilon_nfa::NFA as EpsilonNFA;
    use crate::grammar::{parse, Expr};
    use crate::grammar::tests::arb_expr_match;

    use super::*;

    use proptest::prelude::*;

    #[test]
    fn groups_by_key() {
        let input = vec![0, 1, 1, 2, 2, 2];
        let output = group_by_key(&input, |x| x);
        assert_eq!(output.len(), 3);
        assert!(matches!(output[0], (0, &[0])));
        assert!(matches!(output[1], (1, &[1, 1])));
        assert!(matches!(output[2], (2, &[2, 2, 2])));
    }

    #[test]
    fn dfa_from_simple_nfa() {
        let mut nfa = NFA::default();
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.mark_state_accepting(1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.mark_state_accepting(2);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
    }

    #[test]
    fn dfa_from_diamond_nfa() {
        let mut nfa = NFA::default();
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.add_transition(1, Input::Literal("bar".to_string()), 3);
        nfa.add_transition(2, Input::Literal("baz".to_string()), 3);
        nfa.mark_state_accepting(3);
        nfa.unallocated_state_id = 4;
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
        assert!(dfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn dfa_from_nfa_with_simple_loop() {
        let mut nfa = NFA::default();
        nfa.mark_state_accepting(1);
        nfa.mark_state_accepting(2);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.add_transition(1, Input::Literal("bar".to_string()), 1);
        nfa.unallocated_state_id = 3;
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "bar"]));
        assert!(!dfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn dfa_from_nfa_with_bigger_loop() {
        let mut nfa = NFA::default();
        nfa.mark_state_accepting(1);
        nfa.mark_state_accepting(2);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.add_transition(1, Input::Literal("bar".to_string()), 2);
        nfa.unallocated_state_id = 3;
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![
            Expr::Literal("foo".to_string()),
            Expr::Literal("bar".to_string()),
        ]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&[]));
        assert!(dfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "foo"]));
    }

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
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "last"]));
        assert!(dfa.accepts(&["first", "bar", "last"]));
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
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
        assert!(dfa.accepts(&["foo", "baz"]));
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
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "last"]));
        assert!(dfa.accepts(&["first", "bar", "last"]));
        assert!(dfa.accepts(&["first", "last"]));
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
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first", "foo", "bar", "last"]));
    }

    #[test]
    fn gatevidyalay_problem1() {
        let mut nfa = NFA::default();
        nfa.add_transition(0, Input::Literal("a".to_string()), 0);
        nfa.add_transition(0, Input::Literal("b".to_string()), 0);
        nfa.add_transition(0, Input::Literal("b".to_string()), 1);
        nfa.add_transition(1, Input::Literal("b".to_string()), 2);
        nfa.mark_state_accepting(2);
        nfa.unallocated_state_id = 3;

        assert!(nfa.accepts(&["b", "b"]));
        assert!(nfa.accepts(&["a", "b", "b"]));
        assert!(nfa.accepts(&["a", "a", "b", "b"]));
        assert!(nfa.accepts(&["a", "b", "b", "b"]));

        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["b", "b"]));
        assert!(dfa.accepts(&["a", "b", "b"]));
        assert!(dfa.accepts(&["a", "a", "b", "b"]));
        assert!(dfa.accepts(&["a", "b", "b", "b"]));
    }

    #[test]
    fn defeats_naive_nfa_to_dfa() {
        let expr = Expr::Sequence(vec![
            Expr::Many1(Box::new(Expr::Literal("foo".to_string()))),
            Expr::Literal("foo".to_string()),
        ]);

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "foo"]));
        assert!(dfa.accepts(&["foo", "foo", "foo"]));
    }

    #[test]
    fn does_not_accept_a_prefix() {
        let expr = Expr::Literal("first".to_string());
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        assert!(!epsilon_nfa.accepts(&["first", "first"]));
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(!nfa.accepts(&["first", "first"]));
        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["first"]));
        assert!(!dfa.accepts(&["first", "first"]));
    }

    #[test]
    fn does_not_loop_at_the_last_word() {
        const INPUT: &str = "foo <id> [--help];";
        let g = parse(INPUT).unwrap();
        let (_, expr) = g.into_command_expr();

        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        assert!(epsilon_nfa.accepts(&["foo", "--help"]));
        assert!(!epsilon_nfa.accepts(&["foo", "--help", "--help"]));

        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        assert!(nfa.accepts(&["foo", "--help"]));
        assert!(!nfa.accepts(&["foo", "--help", "--help"]));

        let dfa = DFA::from_nfa(&nfa);
        assert!(dfa.accepts(&["foo", "--help"]));
        assert!(!dfa.accepts(&["foo", "--help", "--help"]));
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
            let dfa = DFA::from_nfa(&nfa);
            prop_assert!(dfa.accepts(&input));
        }
    }
}
