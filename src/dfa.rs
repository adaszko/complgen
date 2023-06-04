use std::{
    collections::BTreeSet,
    io::Write, cmp::Ordering, rc::Rc
};
use hashbrown::{HashMap, HashSet};

use roaring::{MultiOps, RoaringBitmap};

use crate::regex::{Position, AugmentedRegex, Input};
use complgen::StateId;


#[derive(Debug, Clone)]
pub struct DirectDFA {
    pub starting_state: StateId,
    pub transitions: HashMap<StateId, HashMap<Input, StateId>>,
    pub accepting_states: RoaringBitmap,
}


fn dfa_from_regex(regex: &AugmentedRegex) -> DirectDFA {
    let mut unallocated_state_id = 0;
    let combined_starting_state: BTreeSet<Position> = regex.firstpos();
    let combined_starting_state_id = unallocated_state_id;
    unallocated_state_id += 1;

    let mut dstates: HashMap<BTreeSet<Position>, StateId> = HashMap::from_iter([(combined_starting_state.clone(), combined_starting_state_id)]);

    let followpos = regex.followpos();

    let mut dtran: HashMap<StateId, HashMap<Input, StateId>> = Default::default();
    let mut unmarked_states: HashSet<BTreeSet<Position>> = Default::default();
    unmarked_states.insert(combined_starting_state.clone());
    loop {
        let combined_state = match unmarked_states.iter().next() {
            Some(state) => state.clone(),
            None => break,
        };
        unmarked_states.remove(&combined_state);
        let from_combined_state_id = *dstates.get(&combined_state).unwrap();
        let from_entry = dtran.entry(from_combined_state_id).or_default();
        for input in &regex.input_symbols {
            let mut u = RoaringBitmap::new();
            for pos in &combined_state {
                let pos_usize = usize::try_from(*pos).unwrap();
                if regex.input_from_position.get(pos_usize) == Some(input) {
                    if let Some(positions) = followpos.get(&pos) {
                        u |= positions;
                    }
                }
            }
            let u = BTreeSet::from_iter(u);
            if !dstates.contains_key(&u) {
                dstates.insert(u.clone(), unallocated_state_id);
                unallocated_state_id += 1;
                unmarked_states.insert(u.clone());
            }
            let to_combined_state_id = dstates.get(&u).unwrap();
            from_entry.insert(input.clone(), *to_combined_state_id);
        }
    }

    // The accepting states are those containing the position for the endmarker symbol #.
    let accepting_states: RoaringBitmap = {
        let mut accepting_states = RoaringBitmap::default();
        for (combined_state, state_id) in &dstates {
            if combined_state.contains(&regex.endmarker_position) {
                accepting_states.insert((*state_id).into());
            }
        }
        accepting_states
    };

    DirectDFA {
        starting_state: *dstates.get(&combined_starting_state).unwrap(),
        transitions: dtran,
        accepting_states,
    }
}


struct HashableRoaringBitmap(Rc<RoaringBitmap>);


impl PartialEq for HashableRoaringBitmap {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0.iter().zip(other.0.iter()).all(|(left, right)| left == right)
    }
}

impl Eq for HashableRoaringBitmap {
}


impl std::hash::Hash for HashableRoaringBitmap {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for elem in self.0.iter() {
            elem.hash(state);
        }
    }
}


#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct SetInternId(usize);


#[derive(Default)]
struct SetInternPool {
    pool: Vec<Rc<RoaringBitmap>>,
    id_from_set: HashMap<HashableRoaringBitmap, usize>,
}

impl SetInternPool {
    fn intern(&mut self, set: RoaringBitmap) -> SetInternId {
        let rc = Rc::new(set);
        let id = *self.id_from_set.entry(HashableRoaringBitmap(Rc::clone(&rc))).or_insert_with(|| {
            let id = self.pool.len();
            self.pool.push(rc);
            id
        });
        SetInternId(id)
    }

    fn get(&self, id: SetInternId) -> Option<Rc<RoaringBitmap>> {
        self.pool.get(id.0).cloned()
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Transition {
    from: StateId,
    to: StateId,
    input: Input,
}


fn make_inverse_transitions_lookup_table(transitions: &HashMap<StateId, HashMap<Input, StateId>>) -> Vec<Transition> {
    let mut result: Vec<Transition> = Default::default();
    for (from, tos) in transitions {
        for (input, to) in tos {
            result.push(Transition {
                from: *from,
                to: *to,
                input: *input,
            });
        }
    }
    result.sort_unstable_by_key(|transition| transition.to);
    result.dedup(); // should be redundant
    result
}

// Hopcroft's DFA minimization algorithm.
// References:
//  * The Dragon Book: Minimizing the Number of states of a DFA
//  * Engineering a Compiler, 3rd ed, 2.4.4 DFA to Minimal DFA
//  * https://github.com/BurntSushi/regex-automata/blob/master/src/dfa/minimize.rs
//
// TODO Use https://docs.rs/nohash-hasher/ for Hash{Map,Set}<StateId, ...>?
fn do_minimize(dfa: &DirectDFA) -> DirectDFA {
    let mut pool = SetInternPool::default();
    let mut partition: HashSet<SetInternId> = {
        let all_states = dfa.get_all_states();
        let nonaccepting_states = [&all_states, &dfa.accepting_states].difference();
        if nonaccepting_states.is_empty() {
            return dfa.clone();
        }
        let nonaccepting_states_id = pool.intern(nonaccepting_states);
        let accepting_states_id = pool.intern(dfa.accepting_states.clone());
        HashSet::from_iter([accepting_states_id, nonaccepting_states_id])
    };
    let mut worklist = partition.clone();
    let inverse_transitions = make_inverse_transitions_lookup_table(&dfa.transitions);
    loop {
        let group_id = match worklist.iter().next() {
            Some(group_id) => *group_id,
            None => break,
        };
        worklist.remove(&group_id);
        let group = pool.get(group_id).unwrap(); // group is a better name for 's' in the book
        let group_min = group.min().unwrap();
        let group_max = group.max().unwrap();
        let inbetween_index = match inverse_transitions.binary_search_by(|transition| {
            let to: u32 = transition.to.into();
            if to < group_min {
                return Ordering::Less;
            }
            if to > group_max {
                return Ordering::Greater;
            }
            Ordering::Equal
        }) {
            Ok(index) => index,
            Err(_) => continue,
        };
        let lower_bound = {
            let mut lower_bound = inbetween_index;
            while lower_bound > 0 && u32::from(inverse_transitions[lower_bound-1].to) >= group_min {
                lower_bound -= 1;
            }
            lower_bound
        };
        let upper_bound = {
            let mut upper_bound = inbetween_index;
            while upper_bound < inverse_transitions.len() - 1 && u32::from(inverse_transitions[upper_bound+1].to) <= group_max {
                upper_bound += 1;
            }
            upper_bound
        };

        let group_transitions: Vec<Transition> = inverse_transitions[lower_bound..=upper_bound].iter().filter(|transition| group.contains(transition.to.into())).copied().collect();

        let inputs: HashSet<Input> = group_transitions.iter().map(|transition| transition.input).collect();
        for input in inputs {
            let image: RoaringBitmap = group_transitions.iter().filter(|transition| transition.input == input).map(|transition| u32::from(transition.from)).collect();
            let qs: Vec<SetInternId> = partition.iter().filter(|q_id| pool.get(**q_id).unwrap().intersection_len(&image) > 0).cloned().collect();
            for q_id in qs {
                let q = pool.get(q_id).unwrap();
                let q1 = [&q, &image].intersection(); // elements to remove from q and put into a separate set in a partiton
                let q2 = [&q, &q1].difference(); // what to replace q with
                if q2.is_empty() {
                    continue;
                }

                let q1_len = q1.len();
                let q2_len = q2.len();

                partition.remove(&q_id);
                let q1_id = pool.intern(q1);
                let q2_id = pool.intern(q2);
                partition.insert(q1_id);
                partition.insert(q2_id);

                if worklist.contains(&q_id) {
                    worklist.remove(&q_id);
                    worklist.insert(q1_id);
                    worklist.insert(q2_id);
                }
                else if q1_len <= q2_len {
                    worklist.insert(q1_id);
                }
                else {
                    worklist.insert(q2_id);
                }
                if group_id == q_id {
                    break;
                }
            }
        }
    }

    let representative_id_from_state_id = {
        let mut representative_id_from_state_id: HashMap<StateId, StateId> = Default::default();
        for q_id in &partition {
            let q = pool.get(*q_id).unwrap();
            let representative_state_id = q.min().unwrap();
            for state_id in q.iter() {
                representative_id_from_state_id.insert(StateId::try_from(state_id).unwrap(), StateId::try_from(representative_state_id).unwrap());
            }
        }
        representative_id_from_state_id
    };

    let starting_state = *representative_id_from_state_id.get(&dfa.starting_state).unwrap();

    let accepting_states = {
        let mut accepting_states: RoaringBitmap = Default::default();
        for state_id in &dfa.accepting_states {
            accepting_states.insert((*representative_id_from_state_id.get(&u16::try_from(state_id).unwrap()).unwrap()).into());
        }
        accepting_states
    };

    let transitions = {
        let mut transitions: HashMap<StateId, HashMap<Input, StateId>> = Default::default();
        for (from, tos) in &dfa.transitions {
            let entry = transitions.entry(*from).or_default();
            for (input, to) in tos {
                let representative = representative_id_from_state_id.get(to).unwrap();
                entry.insert(*input, *representative);
            }
        }
        transitions
    };

    DirectDFA { starting_state, transitions, accepting_states }
}


impl DirectDFA {
    pub fn from_regex(regex: &AugmentedRegex) -> Self {
        dfa_from_regex(regex)
    }

    pub fn minimize(&self) -> Self {
        do_minimize(self)
    }

    pub fn get_all_states(&self) -> RoaringBitmap {
        let mut states: RoaringBitmap = Default::default();
        for (from, to) in &self.transitions {
            states.insert((*from).into());
            to.iter().for_each(|(_, to)| {
                states.insert((*to).into());
            });
        }
        states
    }

    pub fn get_asterisk_transitions(&self) -> Vec<(StateId, StateId)> {
        let mut result: Vec<(StateId, StateId)> = Default::default();
        for (from, tos) in &self.transitions {
            for (input, to) in tos {
                if input.is_any() {
                    result.push((*from, *to));
                }
            }
        }
        result
    }

    pub fn to_dot<W: Write>(&self, output: &mut W) -> std::result::Result<(), std::io::Error> {
        writeln!(output, "digraph nfa {{")?;
        writeln!(output, "\trankdir=LR;")?;

        if self.accepting_states.contains(self.starting_state.into()) {
            writeln!(output, "\tnode [shape = doubleoctagon];")?;
        }
        else {
            writeln!(output, "\tnode [shape = octagon];")?;
        }
        writeln!(output, "\t_{}[label=\"{}\"];", self.starting_state, self.starting_state)?;

        let regular_states = {
            let mut states = [&self.get_all_states(), &self.accepting_states].difference();
            states.remove(self.starting_state.into());
            states
        };

        writeln!(output, "\tnode [shape = circle];")?;
        for state in regular_states {
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
    use crate::regex::AugmentedRegex;

    use super::*;

    use bumpalo::Bump;
    use ustr::ustr as u;
    use proptest::prelude::*;

    impl DirectDFA {
        pub fn accepts(&self, inputs: &[&str]) -> bool {
            let mut backtracking_stack = Vec::from_iter([(0, self.starting_state)]);
            while let Some((input_index, current_state)) = backtracking_stack.pop() {
                if input_index == inputs.len() && self.accepting_states.contains(current_state.into()) {
                    return true;
                }

                for (transition_input, to) in self.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
                    if let Input::Any = transition_input {
                        backtracking_stack.push((input_index + 1, *to));
                    }
                }

                for (transition_input, to) in self.transitions.get(&current_state).unwrap_or(&HashMap::default()) {
                    if let Input::Literal(s) = transition_input {
                        if s.as_str() == inputs[input_index] {
                            backtracking_stack.push((input_index + 1, *to));
                        }
                    }
                }
            }
            false
        }

        pub fn has_transition(&self, from: StateId, input: Input, to: StateId) -> bool {
            *self.transitions.get(&from).unwrap().get(&input).unwrap() == to
        }

        pub fn has_literal_transition(&self, from: StateId, input: &str, to: StateId) -> bool {
            self.has_transition(from, Input::Literal(ustr::ustr(input)), to)
        }
    }

    const LITERALS: &[&str] = &["foo", "bar", "--baz", "--quux"];
    const VARIABLES: &[&str] = &["FILE", "DIRECTORY", "PATH"];

    proptest! {
        #[test]
        fn accepts_arb_expr_input_from_regex((expr, input) in arb_expr_match(Rc::new(LITERALS.iter().map(|s| u(s)).collect()), Rc::new(VARIABLES.iter().map(|s| u(s)).collect()), 10, 3)) {
            // println!("{:?}", expr);
            // println!("{:?}", input);
            let arena = Bump::new();
            let regex = AugmentedRegex::from_expr(&expr, &arena);
            let dfa = DirectDFA::from_regex(&regex);
            let input: Vec<&str> = input.iter().map(|s| {
                let s: &str = s;
                s
            }).collect();
            prop_assert!(dfa.accepts(&input));
        }

        #[test]
        fn minimized_dfa_equivalent_to_input_one((expr, input) in arb_expr_match(Rc::new(LITERALS.iter().map(|s| u(s)).collect()), Rc::new(VARIABLES.iter().map(|s| u(s)).collect()), 10, 3)) {
            println!("{:?}", expr);
            println!("{:?}", input);
            let arena = Bump::new();
            let regex = AugmentedRegex::from_expr(&expr, &arena);
            let dfa = DirectDFA::from_regex(&regex);
            let input: Vec<&str> = input.iter().map(|s| {
                let s: &str = s;
                s
            }).collect();
            prop_assert!(dfa.accepts(&input));
            let minimimal_dfa = dfa.minimize();
            prop_assert!(minimimal_dfa.accepts(&input));
        }
    }

    #[test]
    fn accept_hangs() {
        use Expr::*;
        let expr = Sequence(vec![Alternative(vec![Sequence(vec![Optional(Box::new(Alternative(vec![Many1(Box::new(Optional(Box::new(Many1(Box::new(Sequence(vec![Literal(u("foo")), Literal(u("foo"))]))))))), Literal(u("bar"))]))), Variable(u("DIRECTORY"))]), Many1(Box::new(Literal(u("--quux"))))]), Sequence(vec![Sequence(vec![Many1(Box::new(Many1(Box::new(Many1(Box::new(Literal(u("bar")))))))), Many1(Box::new(Sequence(vec![Many1(Box::new(Many1(Box::new(Literal(u("--baz")))))), Sequence(vec![Alternative(vec![Variable(u("DIRECTORY")), Variable(u("PATH"))]), Alternative(vec![Literal(u("--baz")), Sequence(vec![Sequence(vec![Literal(u("--baz")), Variable(u("FILE"))]), Sequence(vec![Literal(u("foo")), Variable(u("FILE"))])])])])])))]), Literal(u("bar"))])]);
        let input = [
            "--quux",
            "--quux",
            "--quux",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "bar",
            "--baz",
            "--baz",
            "--baz",
            "--baz",
            "--baz",
            "--baz",
            "anything",
            "--baz",
            "--baz",
            "--baz",
            "--baz",
            "anything",
            "--baz",
            "--baz",
            "--baz",
            "--baz",
            "anything",
            "--baz",
            "anything",
            "foo",
            "anything",
            "bar",
        ];
        let arena = Bump::new();
        let regex = AugmentedRegex::from_expr(&expr, &arena);
        let dfa = DirectDFA::from_regex(&regex);
        let input: Vec<&str> = input.iter().map(|s| {
            let s: &str = s;
            s
        }).collect();
        assert!(dfa.accepts(&input));
    }

    #[test]
    fn engineering_a_compiler_book_dfa_minimization_example() {
        use ustr::ustr;
        let dfa = {
            let starting_state = 0;
            let mut transitions: HashMap<StateId, HashMap<Input, StateId>> = Default::default();
            transitions.entry(0).or_default().insert(Input::Literal(ustr("f")), 1);
            transitions.entry(1).or_default().insert(Input::Literal(ustr("e")), 2);
            transitions.entry(1).or_default().insert(Input::Literal(ustr("i")), 4);
            transitions.entry(2).or_default().insert(Input::Literal(ustr("e")), 3);
            transitions.entry(4).or_default().insert(Input::Literal(ustr("e")), 5);
            let accepting_states = RoaringBitmap::from_iter([3,5]);
            DirectDFA { starting_state, transitions, accepting_states }
        };
        let minimized = dfa.minimize();
        assert_eq!(minimized.starting_state, 0);
        assert_eq!(minimized.accepting_states, RoaringBitmap::from_iter([3]));
        assert!(minimized.has_literal_transition(0, "f", 1));
        assert!(minimized.has_literal_transition(1, "e", 2));
        assert!(minimized.has_literal_transition(1, "i", 2));
        assert!(minimized.has_literal_transition(2, "e", 3));
    }
}
