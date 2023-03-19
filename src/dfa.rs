use std::{collections::{HashSet, BTreeMap, HashMap}, io::Write};

use roaring::{RoaringBitmap, MultiOps};

use crate::nfa::{NFA, Input};
use complgen::{StateId, START_STATE_ID};


pub struct DFA {
    start_states: RoaringBitmap,
    unallocated_state_id: StateId,
    transitions: BTreeMap<StateId, HashMap<Input, StateId>>,
    accepting_states: RoaringBitmap,
}


impl Default for DFA {
    fn default() -> Self {
        Self {
            start_states: RoaringBitmap::from_iter([START_STATE_ID]),
            unallocated_state_id: START_STATE_ID + 1,
            transitions: Default::default(),
            accepting_states: Default::default(),
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


// https://github.com/caleb531/automata/blob/b9c195a80a5aa5977c7c0b81d5a72ba56bb12551/automata/fa/dfa.py#L1224
fn determinize_nfa(nfa: &mut NFA) {
    let mut visited: HashSet<StateId> = Default::default();
    let mut to_visit: HashSet<StateId> = HashSet::from_iter(nfa.starting_states.iter());
    while !to_visit.is_empty() {
        // popping from a hashset is somewhat involved
        let current_state = to_visit.iter().next().copied().unwrap();
        let current_state = to_visit.take(&current_state).unwrap();

        if visited.contains(&current_state) {
            continue;
        }

        let mut transitions_from_current_state: Vec<(Input, StateId)> = nfa.get_transitions_from(current_state).into_iter().collect();
        to_visit.extend(transitions_from_current_state.iter().map(|(_, to)| *to));

        transitions_from_current_state.sort_by_key(|(input, _)| input.clone());
        let groups = group_by_key(&transitions_from_current_state, |(input, _)| input);
        for (input, nondeterministic_transitions) in groups.into_iter().filter(|(_, g)| g.len() > 1) {
            let new_state = nfa.add_state();
            nfa.add_transition(current_state, input.clone(), new_state);
            for (_, to) in nondeterministic_transitions {
                if nfa.accepting_states.contains(*to) {
                    nfa.accepting_states.insert(new_state);
                }
                for (input_prime, to_prime) in nfa.get_transitions_from(*to) {
                    nfa.remove_transition(current_state, input.clone(), *to);
                    nfa.add_transition(new_state, input_prime, to_prime);
                }
                for (from_prime, input_prime) in nfa.get_transitions_to(*to) {
                    nfa.remove_transition(from_prime, input_prime.clone(), *to);
                    nfa.add_transition(from_prime, input_prime, new_state);
                }
                if to_visit.contains(to) {
                    to_visit.remove(to);
                    to_visit.insert(new_state);
                }
            }
        }

        visited.insert(current_state);
    }
}


fn dfa_from_determinized_nfa(nfa: &NFA) -> DFA {
    let mut dfa = DFA::default();
    dfa.accepting_states = nfa.accepting_states.clone();
    dfa.start_states = nfa.starting_states.clone();
    dfa.unallocated_state_id = nfa.unallocated_state_id;
    for (from, tos) in &nfa.transitions {
        let mut ts: HashMap<Input, StateId> = Default::default();
        for (input, to) in tos {
            ts.insert(input.clone(), *to);
        }
        dfa.transitions.insert(*from, ts);
    }
    dfa
}


impl DFA {
    pub fn from_nfa(mut nfa: NFA) -> Self {
        determinize_nfa(&mut nfa);
        dfa_from_determinized_nfa(&nfa)
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

    pub fn get_transitions_from(&self, from: StateId) -> HashMap<Input, StateId> {
        self.transitions.get(&from).cloned().unwrap_or(HashMap::default())
    }

    pub fn accepts(&self, inputs: &[&str]) -> bool {
        let mut backtracking_stack: Vec<(usize, StateId)> = self.start_states.iter().map(|state| (0, state)).collect();
        while let Some((input_index, current_state)) = backtracking_stack.pop() {
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
        false
    }

    pub fn get_live_states(&self) -> RoaringBitmap {
        let mut visited: RoaringBitmap = Default::default();
        let mut to_visit: Vec<StateId> = self.start_states.iter().collect();
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
    use crate::epsilon_nfa::NFA as EpsilonNFA;

    use super::*;

    /*
    #[test]
    fn groups_by_key() {
        let input = vec![0, 1, 1, 2, 2, 2];
        let output = group_by_key(&input, |x| x);
        assert_eq!(output.len(), 3);
        assert_eq!(output[0], (0, &[0]));
        assert_eq!(output[1], (1, &[1, 1]));
        assert_eq!(output[2], (2, &[2, 2, 2]));
    }
    */

    #[test]
    fn dfa_from_simple_nfa() {
        let mut nfa = NFA::default();
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.mark_state_accepting(1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.mark_state_accepting(2);
        let dfa = DFA::from_nfa(nfa);
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
        let dfa = DFA::from_nfa(nfa);
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
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "baz"]));
    }

    #[test]
    fn dfa_from_nfa_with_bigger_loop() {
        let mut nfa = NFA::default();
        nfa.mark_state_accepting(1);
        nfa.mark_state_accepting(2);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 1);
        nfa.add_transition(0, Input::Literal("foo".to_string()), 2);
        nfa.add_transition(1, Input::Literal("bar".to_string()), 2);
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_any_word() {
        let expr = Expr::Variable("dummy".to_string());
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["anything"]));
    }

    #[test]
    fn accepts_two_words_sequence_pattern() {
        let expr = Expr::Sequence(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
    }

    #[test]
    fn accepts_two_words_choice_pattern() {
        let expr = Expr::Alternative(vec![Expr::Literal("foo".to_string()), Expr::Literal("bar".to_string())]);
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["foo"]));
        assert!(dfa.accepts(&["bar"]));
    }

    #[test]
    fn accepts_optional_word_pattern() {
        let expr = Expr::Optional(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&[]));
        assert!(dfa.accepts(&["foo"]));
    }

    #[test]
    fn accepts_many1_words_pattern() {
        let expr = Expr::Many1(Box::new(Expr::Literal("foo".to_string())));
        let epsilon_nfa = EpsilonNFA::from_expr(&expr);
        let nfa = NFA::from_epsilon_nfa(&epsilon_nfa);
        let dfa = DFA::from_nfa(nfa);
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
        let dfa = DFA::from_nfa(nfa);
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
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["foo", "bar"]));
        assert!(dfa.accepts(&["foo", "baz"]));
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
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["first", "foo", "last"]));
        assert!(dfa.accepts(&["first", "bar", "last"]));
        assert!(dfa.accepts(&["first", "last"]));
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
        let dfa = DFA::from_nfa(nfa);
        assert!(dfa.accepts(&["first", "foo", "bar", "last"]));
    }
}
