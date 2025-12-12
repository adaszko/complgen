use hashbrown::{HashMap, HashSet};
use indexmap::{IndexMap, IndexSet};
use std::{cmp::Ordering, collections::BTreeSet, hash::Hash, io::Write, rc::Rc};

use roaring::{MultiOps, RoaringBitmap};
use ustr::{Ustr, ustr};

use crate::grammar::{DFAId, DFAInternPool};
use crate::regex::{Inp, Position, Regex, diagnostic_display_input};
use crate::{Error, Result, StateId};

// Every state in a DFA is formally defined to have a transition on *every* input symbol.  In
// our applications that's not the case, so we add artificial transitions to a special, designated
// "dead" state.  Otherwise the minimization algorithm doesn't work as intended.
pub const DEAD_STATE_ID: StateId = 0;
pub const FIRST_STATE_ID: StateId = 1;

#[derive(Debug, Clone)]
pub struct DFA {
    pub starting_state: StateId,

    // IndexMap is used to keep complgen deterministic
    // https://github.com/adaszko/complgen/issues/60
    pub transitions: IndexMap<StateId, IndexMap<InpId, StateId>>,
    pub accepting_states: RoaringBitmap,

    pub inputs: InpInternPool,
    pub subdfas: DFAInternPool,
}

impl DFA {
    pub(crate) fn get_input(&self, id: InpId) -> &Inp {
        self.inputs.lookup(id)
    }
}

impl PartialEq for DFA {
    fn eq(&self, other: &Self) -> bool {
        self.starting_state == other.starting_state
            && self.transitions == other.transitions
            && self.accepting_states == other.accepting_states
            && self.inputs == other.inputs
    }
}

impl Eq for DFA {}

impl std::hash::Hash for DFA {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.starting_state.hash(state);
        for (from, tos) in &self.transitions {
            from.hash(state);
            for to in tos {
                to.hash(state);
            }
        }
        for elem in &self.accepting_states {
            elem.hash(state);
        }
        self.inputs.hash(state);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InpId(u32);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct InpInternPool {
    store: IndexSet<Inp>,
}

impl std::hash::Hash for InpInternPool {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for elem in &self.store {
            elem.hash(state);
        }
    }
}

impl InpInternPool {
    #[allow(dead_code)]
    fn intern(&mut self, value: Inp) -> InpId {
        let (id, _) = self.store.insert_full(value);
        InpId(id as _)
    }

    fn lookup(&self, id: InpId) -> &Inp {
        self.store.get_index(id.0 as _).unwrap()
    }

    #[allow(dead_code)]
    fn find(&self, inp: &Inp) -> Option<InpId> {
        self.store.get_index_of(inp).map(|i| InpId(i as _))
    }

    fn ids(&self) -> impl Iterator<Item = InpId> {
        (0..self.store.len()).map(|i| InpId(i as _))
    }

    fn elems(&self) -> impl Iterator<Item = &Inp> {
        self.store.iter()
    }

    fn pairs(&self) -> impl Iterator<Item = (InpId, &Inp)> {
        self.store
            .iter()
            .enumerate()
            .map(|(i, inp)| (InpId(i as _), inp))
    }
}

impl FromIterator<Inp> for InpInternPool {
    fn from_iter<T: IntoIterator<Item = Inp>>(iter: T) -> Self {
        Self {
            store: IndexSet::from_iter(iter),
        }
    }
}

// Reference:
//  * The Dragon Book: 3.9.5 Converting a Regular Expression Directly to a DFA
fn dfa_from_regex(regex: Regex, subdfas: DFAInternPool) -> DFA {
    let mut unallocated_state_id = FIRST_STATE_ID;
    let combined_starting_state: BTreeSet<Position> = regex.firstpos();
    let combined_starting_state_id = unallocated_state_id;
    unallocated_state_id += 1;

    let mut state_id_from_set_of_positions: IndexMap<BTreeSet<Position>, StateId> =
        IndexMap::from_iter([(combined_starting_state.clone(), combined_starting_state_id)]);

    let followpos = regex.followpos();
    let inputs = InpInternPool::from_iter(regex.input_from_position.iter().map(Inp::from_input));

    let mut transitions: IndexMap<StateId, IndexMap<InpId, StateId>> = Default::default();
    let mut unmarked_states: HashSet<BTreeSet<Position>> = Default::default();
    unmarked_states.insert(combined_starting_state.clone());
    while let Some(state) = unmarked_states.iter().next() {
        let combined_state = state.clone();
        unmarked_states.remove(&combined_state);
        let from_combined_state_id = *state_id_from_set_of_positions.get(&combined_state).unwrap();
        let state_transitions = transitions.entry(from_combined_state_id).or_default();
        for (inp_id, inp) in inputs.pairs() {
            let mut set_of_positions = RoaringBitmap::new();
            for pos in &combined_state {
                if let Some(input) = regex.input_from_position.get(*pos as usize)
                    && Inp::from_input(input) == *inp
                    && let Some(positions) = followpos.get(pos)
                {
                    set_of_positions |= positions;
                }
            }
            if !set_of_positions.is_empty() {
                let set_of_positions = BTreeSet::from_iter(set_of_positions);
                if !state_id_from_set_of_positions.contains_key(&set_of_positions) {
                    state_id_from_set_of_positions
                        .insert(set_of_positions.clone(), unallocated_state_id);
                    unallocated_state_id += 1;
                    unmarked_states.insert(set_of_positions.clone());
                }
                let to_combined_state_id = state_id_from_set_of_positions
                    .get(&set_of_positions)
                    .unwrap();
                state_transitions.insert(inp_id, *to_combined_state_id);
            }
        }
    }

    // The accepting states are those containing the position for the endmarker symbol #.
    let accepting_states: RoaringBitmap = {
        let mut accepting_states = RoaringBitmap::default();
        for (combined_state, state_id) in &state_id_from_set_of_positions {
            if combined_state.contains(&regex.endmarker_position) {
                accepting_states.insert(*state_id);
            }
        }
        accepting_states
    };

    DFA {
        starting_state: *state_id_from_set_of_positions
            .get(&combined_starting_state)
            .unwrap(),
        transitions,
        accepting_states,
        inputs,
        subdfas,
    }
}

struct HashableRoaringBitmap(Rc<RoaringBitmap>);

impl PartialEq for HashableRoaringBitmap {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0
            .iter()
            .zip(other.0.iter())
            .all(|(left, right)| left == right)
    }
}

impl Eq for HashableRoaringBitmap {}

impl std::hash::Hash for HashableRoaringBitmap {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for elem in self.0.iter() {
            elem.hash(state);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct SetId(usize);

#[derive(Default)]
struct SetInternPool {
    pool: Vec<Rc<RoaringBitmap>>,
    id_from_set: HashMap<HashableRoaringBitmap, usize>,
}

impl SetInternPool {
    fn intern(&mut self, set: RoaringBitmap) -> SetId {
        let rc = Rc::new(set);
        let id = *self
            .id_from_set
            .entry(HashableRoaringBitmap(Rc::clone(&rc)))
            .or_insert_with(|| {
                let id = self.pool.len();
                self.pool.push(rc);
                id
            });
        SetId(id)
    }

    fn lookup(&self, id: SetId) -> Option<Rc<RoaringBitmap>> {
        self.pool.get(id.0).cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Transition {
    from: StateId,
    to: StateId,
    input: InpId,
}

fn keep_only_states_with_input_transitions(
    starting_state: StateId,
    transitions: &[Transition],
    accepting_states: &RoaringBitmap,
) -> (Vec<Transition>, RoaringBitmap) {
    let states_with_input_transition =
        RoaringBitmap::from_iter(transitions.iter().map(|transition| transition.to));

    let alive_accepting_states =
        RoaringBitmap::from_sorted_iter(accepting_states.iter().filter(|state| {
            *state == starting_state || states_with_input_transition.contains(*state)
        }))
        .unwrap();

    let alive_transitions: Vec<Transition> = transitions
        .iter()
        .filter(|transition| {
            if transition.from == starting_state {
                return true;
            }
            if !states_with_input_transition.contains(transition.from)
                || !states_with_input_transition.contains(transition.to)
            {
                return false;
            }
            true
        })
        .cloned()
        .collect();

    (alive_transitions, alive_accepting_states)
}

fn eliminate_nonaccepting_states_without_output_transitions(
    transitions: &[Transition],
    accepting_states: &RoaringBitmap,
) -> Vec<Transition> {
    let states_with_output_transition =
        RoaringBitmap::from_iter(transitions.iter().map(|transition| transition.from));
    let alive_transitions: Vec<Transition> = transitions
        .iter()
        .filter(|transition| {
            accepting_states.contains(transition.to)
                || states_with_output_transition.contains(transition.to)
        })
        .cloned()
        .collect();
    alive_transitions
}

fn renumber_states(
    starting_state: StateId,
    transitions: &[Transition],
    accepting_states: &RoaringBitmap,
) -> (StateId, Vec<Transition>, RoaringBitmap) {
    let new_from_old_state_id = {
        let mut new_from_old_state_id: HashMap<StateId, StateId> = Default::default();
        let mut unallocated_state_id = 0;

        new_from_old_state_id
            .entry(starting_state)
            .or_insert_with(|| {
                let result = unallocated_state_id;
                unallocated_state_id += 1;
                result
            });

        for transition in transitions {
            new_from_old_state_id
                .entry(transition.from)
                .or_insert_with(|| {
                    let result = unallocated_state_id;
                    unallocated_state_id += 1;
                    result
                });
            new_from_old_state_id
                .entry(transition.to)
                .or_insert_with(|| {
                    let result = unallocated_state_id;
                    unallocated_state_id += 1;
                    result
                });
        }
        new_from_old_state_id
    };

    let new_starting_state = *new_from_old_state_id.get(&starting_state).unwrap();

    let new_transitions: Vec<Transition> = transitions
        .iter()
        .map(|Transition { from, to, input }| Transition {
            from: *new_from_old_state_id.get(from).unwrap(),
            to: *new_from_old_state_id.get(to).unwrap(),
            input: *input,
        })
        .collect();

    let new_accepting_states: RoaringBitmap = RoaringBitmap::from_iter(
        accepting_states
            .iter()
            .map(|old| *new_from_old_state_id.get(&old).unwrap()),
    );

    (new_starting_state, new_transitions, new_accepting_states)
}

fn hashmap_transitions_from_vec(
    transitions: &[Transition],
) -> IndexMap<StateId, IndexMap<InpId, StateId>> {
    let mut result: IndexMap<StateId, IndexMap<InpId, StateId>> = Default::default();

    for transition in transitions {
        result
            .entry(transition.from)
            .or_default()
            .insert(transition.input, transition.to);
    }

    result
}

fn find_bounds(
    transitions: &[Transition],
    group_min: u32,
    group_max: u32,
) -> Option<&[Transition]> {
    let inbetween_index = match transitions.binary_search_by(|transition| {
        if transition.to < group_min {
            return Ordering::Less;
        }
        if transition.to > group_max {
            return Ordering::Greater;
        }
        Ordering::Equal
    }) {
        Ok(index) => index,
        Err(_) => return None,
    };
    let lower_bound = {
        let mut lower_bound = inbetween_index;
        while lower_bound > 0 && transitions[lower_bound - 1].to >= group_min {
            lower_bound -= 1;
        }
        lower_bound
    };
    let upper_bound = {
        let mut upper_bound = inbetween_index;
        while upper_bound < transitions.len() - 1 && transitions[upper_bound + 1].to <= group_max {
            upper_bound += 1;
        }
        upper_bound
    };
    Some(&transitions[lower_bound..=upper_bound])
}

// Hopcroft's DFA minimization algorithm.
// References:
//  * The Dragon Book: Minimizing the Number of states of a DFA
//  * Engineering a Compiler, 3rd ed, 2.4.4 DFA to Minimal DFA
//  * https://github.com/BurntSushi/regex-automata/blob/c61a6d0f19b013dc832755375709023dfb9d5a8f/src/dfa/minimize.rs#L87
fn do_minimize(dfa: DFA) -> DFA {
    let mut pool = SetInternPool::default();
    let mut partitions: HashSet<SetId> = {
        let dead_state_group = RoaringBitmap::from_iter([DEAD_STATE_ID]);
        let all_states = dfa.get_all_states();
        let nonaccepting_states =
            [&all_states, &dfa.accepting_states, &dead_state_group].difference();
        if nonaccepting_states.is_empty() {
            // Nothing to minimize
            return dfa;
        }
        let nonaccepting_states_intern_id = pool.intern(nonaccepting_states);
        let accepting_states_intern_id = pool.intern(dfa.accepting_states.clone());
        let dead_state_intern_id = pool.intern(dead_state_group);
        HashSet::from_iter([
            dead_state_intern_id,
            accepting_states_intern_id,
            nonaccepting_states_intern_id,
        ])
    };
    let mut worklist = partitions.clone();
    let transitions_image = dfa.make_transitions_image();
    while let Some(group_id) = worklist.iter().next() {
        let group_id = *group_id;
        worklist.remove(&group_id);
        let group = pool.lookup(group_id).unwrap();
        let group_min = group.min().unwrap();
        let group_max = group.max().unwrap();

        let transitions = match find_bounds(&transitions_image, group_min, group_max) {
            Some(t) => t,
            None => continue,
        };

        let transitions_to_group: HashMap<InpId, RoaringBitmap> = {
            let mut group_transitions: HashMap<InpId, RoaringBitmap> = Default::default();
            for transition in transitions {
                if group.contains(transition.to) {
                    group_transitions
                        .entry(transition.input)
                        .or_default()
                        .insert(transition.from);
                }
            }
            group_transitions
        };
        for from_states in transitions_to_group.values() {
            let overlapping_sets: Vec<SetId> = partitions
                .iter()
                .filter(|set_id| !pool.lookup(**set_id).unwrap().is_disjoint(from_states))
                .copied()
                .collect();
            for intern_id in overlapping_sets {
                let states = pool.lookup(intern_id).unwrap();
                let states_to_remove = [&states, from_states].intersection();
                let remaining_states = [&states, &states_to_remove].difference();
                if remaining_states.is_empty() {
                    continue;
                }

                let num_states_to_remove = states_to_remove.len();
                let num_remaining_states = remaining_states.len();

                partitions.remove(&intern_id);
                let states_to_remove_intern_id = pool.intern(states_to_remove);
                let remaining_states_intern_id = pool.intern(remaining_states);
                partitions.insert(states_to_remove_intern_id);
                partitions.insert(remaining_states_intern_id);

                if worklist.contains(&intern_id) {
                    worklist.remove(&intern_id);
                    worklist.insert(states_to_remove_intern_id);
                    worklist.insert(remaining_states_intern_id);
                } else if num_states_to_remove <= num_remaining_states {
                    worklist.insert(states_to_remove_intern_id);
                } else {
                    worklist.insert(remaining_states_intern_id);
                }
                if group_id == intern_id {
                    break;
                }
            }
        }
    }

    let representative_id_from_state_id = {
        let mut representative_id_from_state_id: HashMap<StateId, StateId> = Default::default();
        for intern_id in &partitions {
            let partition_element = pool.lookup(*intern_id).unwrap();
            let representative_state_id = partition_element.min().unwrap();
            for state_id in partition_element.iter() {
                representative_id_from_state_id.insert(
                    StateId::try_from(state_id).unwrap(),
                    StateId::try_from(representative_state_id).unwrap(),
                );
            }
        }
        representative_id_from_state_id
    };

    let starting_state = *representative_id_from_state_id
        .get(&dfa.starting_state)
        .unwrap();

    let accepting_states = {
        let mut accepting_states: RoaringBitmap = Default::default();
        for state_id in &dfa.accepting_states {
            accepting_states.insert(*representative_id_from_state_id.get(&state_id).unwrap());
        }
        accepting_states
    };

    let transitions = {
        let mut transitions: Vec<Transition> = Default::default();
        for (from, tos) in &dfa.transitions {
            for (input, to) in tos {
                let representative = representative_id_from_state_id.get(to).unwrap();
                transitions.push(Transition {
                    from: *from,
                    to: *representative,
                    input: *input,
                });
            }
        }
        transitions
    };

    let (transitions, accepting_states) =
        keep_only_states_with_input_transitions(starting_state, &transitions, &accepting_states);
    let transitions =
        eliminate_nonaccepting_states_without_output_transitions(&transitions, &accepting_states);
    let (starting_state, transitions, accepting_states) =
        renumber_states(starting_state, &transitions, &accepting_states);
    let transitions = hashmap_transitions_from_vec(&transitions);
    DFA {
        starting_state,
        transitions,
        accepting_states,
        inputs: dfa.inputs,
        subdfas: dfa.subdfas,
    }
}

fn do_to_dot<W: Write>(
    output: &mut W,
    array_start: u32,
    dfa: &DFA,
    identifiers_prefix: &str,
    recursion_level: usize,
) -> Result<()> {
    let indentation = format!("\t{}", str::repeat("\t", recursion_level));

    let id_from_dfa = dfa.get_subwords(array_start as usize);

    if dfa.accepting_states.contains(dfa.starting_state) {
        writeln!(output, "{indentation}node [shape=doubleoctagon];")?;
    } else {
        writeln!(output, "{indentation}node [shape=octagon];")?;
    }
    writeln!(
        output,
        "{indentation}_{identifiers_prefix}{}[label=\"{identifiers_prefix}{}\"];",
        dfa.starting_state + array_start,
        dfa.starting_state + array_start
    )?;

    let regular_states = {
        let mut states = [&dfa.get_all_states(), &dfa.accepting_states].difference();
        states.remove(dfa.starting_state);
        states
    };

    writeln!(output, "{indentation}node [shape=circle];")?;
    for state in regular_states {
        writeln!(
            output,
            "{indentation}_{identifiers_prefix}{}[label=\"{identifiers_prefix}{}\"];",
            state + array_start,
            state + array_start
        )?;
    }

    writeln!(output)?;

    writeln!(output, "{indentation}node [shape=doublecircle];")?;
    for state in &dfa.accepting_states {
        writeln!(
            output,
            "{indentation}_{identifiers_prefix}{}[label=\"{identifiers_prefix}{}\"];",
            state + array_start,
            state + array_start
        )?;
    }

    writeln!(output)?;

    for (subdfaid, subdfa_id) in &id_from_dfa {
        writeln!(
            output,
            "{indentation}subgraph cluster_{identifiers_prefix}{subdfa_id} {{"
        )?;
        writeln!(output, "{indentation}\tlabel=\"subword {subdfa_id}\";")?;
        writeln!(output, "{indentation}\tcolor=grey91;")?;
        writeln!(output, "{indentation}\tstyle=filled;")?;
        let subdfa_identifiers_prefix = &format!("{subdfa_id}_");
        let subdfa = dfa.subdfas.lookup(*subdfaid);
        do_to_dot(
            output,
            array_start,
            subdfa,
            subdfa_identifiers_prefix,
            recursion_level + 1,
        )?;
        writeln!(output, "{indentation}}}")?;
    }

    for (from, tos) in &dfa.transitions {
        for (input_id, to) in tos {
            let input = dfa.get_input(*input_id);
            match input {
                Inp::Literal { .. } | Inp::Star | Inp::Command { .. } => {
                    let label = {
                        let mut buffer = String::new();
                        diagnostic_display_input(&mut buffer, input)?;
                        buffer.replace('\"', "\\\"")
                    };
                    writeln!(
                        output,
                        "{indentation}_{identifiers_prefix}{} -> _{identifiers_prefix}{} [label=\"{}\"];",
                        from + array_start,
                        to + array_start,
                        label
                    )?;
                }
                Inp::Subword {
                    subdfa: subdfaid, ..
                } => {
                    let subdfa = dfa.subdfas.lookup(*subdfaid);
                    let subdfa_id = *id_from_dfa.get(subdfaid).unwrap();
                    let subdfa_identifiers_prefix = &format!("{subdfa_id}_");
                    writeln!(
                        output,
                        r#"{indentation}_{identifiers_prefix}{} -> _{subdfa_identifiers_prefix}{} [style="dashed"];"#,
                        from + array_start,
                        subdfa.starting_state + array_start
                    )?;
                    for subdfa_accepting_state in &subdfa.accepting_states {
                        writeln!(
                            output,
                            r#"{indentation}_{subdfa_identifiers_prefix}{} -> _{identifiers_prefix}{} [style="dashed"];"#,
                            subdfa_accepting_state,
                            to + array_start
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

impl DFA {
    pub fn from_regex(regex: Regex, subdfas: DFAInternPool) -> Result<Self> {
        let dfa = dfa_from_regex(regex, subdfas);
        dfa.check_ambiguity_best_effort()?;
        Ok(dfa)
    }

    fn make_transitions_image(&self) -> Vec<Transition> {
        let mut result: Vec<Transition> = Default::default();
        for (from, tos) in &self.transitions {
            let meaningful_inputs: IndexSet<InpId> = tos.keys().cloned().collect();
            for (input, to) in tos {
                result.push(Transition {
                    from: *from,
                    to: *to,
                    input: *input,
                });
            }
            for inp_id in self.inputs.ids() {
                if meaningful_inputs.contains(&inp_id) {
                    continue;
                }
                result.push(Transition {
                    from: *from,
                    to: DEAD_STATE_ID,
                    input: inp_id,
                });
            }
        }

        result.sort_unstable_by_key(|transition| transition.to);
        result.dedup(); // should be redundant
        result
    }

    pub fn minimize(self) -> Self {
        do_minimize(self)
    }

    fn do_check_ambiguity_best_effort(
        &self,
        state: StateId,
        visited: &mut RoaringBitmap,
        path: &mut Vec<Inp>,
    ) -> Result<()> {
        let mut ambiguous_inputs: Vec<Inp> = Default::default();
        for (input_id, _) in self.iter_transitions_from(state) {
            let input = self.get_input(input_id);
            match input {
                Inp::Literal { .. } => {}
                Inp::Star => ambiguous_inputs.push(input.clone()),
                Inp::Subword {
                    subdfa: subdfa_id, ..
                } => {
                    let subdfa = self.subdfas.lookup(*subdfa_id);
                    subdfa.check_ambiguity_best_effort()?
                }
                Inp::Command { regex: None, .. } => ambiguous_inputs.push(input.clone()),
                Inp::Command {
                    regex: Some(..), ..
                } => {}
            }
        }
        if ambiguous_inputs.len() >= 2 {
            return Err(Error::AmbiguousDFA(
                path.to_owned().into_boxed_slice(),
                ambiguous_inputs.into_boxed_slice(),
            ));
        }
        for (input_id, to) in self.iter_transitions_from(state) {
            if !visited.contains(to) {
                visited.insert(to);
                let input = self.get_input(input_id);
                path.push(input.clone());
                self.do_check_ambiguity_best_effort(to, visited, path)?;
                path.pop();
            }
        }
        Ok(())
    }

    fn check_ambiguity_best_effort(&self) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        let mut path: Vec<Inp> = Default::default();
        self.do_check_ambiguity_best_effort(self.starting_state, &mut visited, &mut path)?;
        Ok(())
    }

    pub(crate) fn iter_inputs(&self) -> impl Iterator<Item = &Inp> + '_ {
        self.iter_transitions()
            .map(|(_, input_id, _)| self.get_input(input_id))
    }

    fn iter_transitions_from(&self, from: StateId) -> impl Iterator<Item = (InpId, StateId)> {
        match self.transitions.get(&from) {
            Some(transitions) => transitions.clone().into_iter(),
            None => IndexMap::<InpId, StateId>::default().into_iter(),
        }
    }

    pub(crate) fn iter_leaders(&self) -> impl Iterator<Item = InpId> {
        self.iter_transitions_from(self.starting_state)
            .map(|(inp_id, _)| inp_id)
    }

    pub(crate) fn iter_transitions(&self) -> impl Iterator<Item = (StateId, InpId, StateId)> + '_ {
        self.transitions
            .iter()
            .flat_map(|(from, tos)| tos.iter().map(|(input, to)| (*from, *input, *to)))
    }

    pub(crate) fn get_all_literals(&self) -> Vec<(Ustr, Option<Ustr>)> {
        self.inputs
            .elems()
            .filter_map(|input| match input {
                Inp::Literal {
                    literal,
                    description,
                    ..
                } => Some((*literal, *description)),
                Inp::Subword { .. } => None,
                Inp::Star => None,
                Inp::Command { .. } => None,
            })
            .collect()
    }

    pub(crate) fn get_literal_transitions_from(&self, from: StateId) -> Vec<(Ustr, Ustr, StateId)> {
        let map = match self.transitions.get(&from) {
            Some(map) => map,
            None => return vec![],
        };
        let transitions: Vec<(Ustr, Ustr, StateId)> = map
            .iter()
            .filter_map(|(input_id, to)| {
                let input = self.get_input(*input_id);
                match input {
                    Inp::Literal {
                        literal: input,
                        description,
                        ..
                    } => Some((*input, description.unwrap_or(ustr("")), *to)),
                    Inp::Subword { .. } => None,
                    Inp::Star => None,
                    Inp::Command { .. } => None,
                }
            })
            .collect();
        transitions
    }

    pub(crate) fn get_nontail_transitions_from(&self, from: StateId) -> Vec<(Ustr, StateId)> {
        let map = match self.transitions.get(&from) {
            Some(map) => map,
            None => return vec![],
        };
        let transitions: Vec<(Ustr, StateId)> = map
            .iter()
            .filter_map(|(input_id, to)| {
                let input = self.get_input(*input_id);
                match input {
                    Inp::Command {
                        regex: Some(regex), ..
                    } => Some((*regex, *to)),
                    Inp::Command { regex: None, .. } => None,
                    Inp::Literal { .. } => None,
                    Inp::Subword { .. } => None,
                    Inp::Star => None,
                }
            })
            .collect();
        transitions
    }

    pub(crate) fn get_subword_transitions_from(&self, from: StateId) -> Vec<(DFAId, StateId)> {
        let map = match self.transitions.get(&from) {
            Some(map) => map,
            None => return vec![],
        };
        let transitions: Vec<(DFAId, StateId)> = map
            .iter()
            .filter_map(|(input_id, to)| match self.get_input(*input_id) {
                Inp::Subword { subdfa: dfa, .. } => Some((*dfa, *to)),
                Inp::Literal { .. } => None,
                Inp::Star => None,
                Inp::Command { .. } => None,
            })
            .collect();
        transitions
    }

    pub(crate) fn get_all_states(&self) -> RoaringBitmap {
        let mut states: RoaringBitmap = Default::default();
        self.iter_transitions().for_each(|(from, _, to)| {
            states.insert(from);
            states.insert(to);
        });
        states.insert(DEAD_STATE_ID);
        states
    }

    pub(crate) fn iter_match_anything_transitions(
        &self,
    ) -> impl Iterator<Item = (StateId, StateId)> + '_ {
        self.iter_transitions()
            .filter_map(move |(from, input_id, to)| {
                let input = self.get_input(input_id);
                if input.is_star(&self.subdfas) {
                    Some((from, to))
                } else {
                    None
                }
            })
    }

    pub(crate) fn get_subwords(&self, first_id: usize) -> IndexMap<DFAId, usize> {
        let mut unallocated_id = first_id;
        let mut result: IndexMap<DFAId, usize> = Default::default();
        for (_, tos) in &self.transitions {
            for (input_id, _) in tos {
                let dfa = match self.get_input(*input_id) {
                    Inp::Subword { subdfa, .. } => subdfa,
                    Inp::Star => continue,
                    Inp::Command { .. } => continue,
                    Inp::Literal { .. } => continue,
                };
                result.entry(*dfa).or_insert_with(|| {
                    let save = unallocated_id;
                    unallocated_id += 1;
                    save
                });
            }
        }
        result
    }

    pub(crate) fn get_max_fallback_level(&self) -> Option<usize> {
        self.iter_inputs()
            .filter_map(|input| input.get_fallback_level())
            .max()
    }

    pub(crate) fn needs_subwords_code(&self) -> bool {
        self.iter_inputs().any(|input| match input {
            Inp::Subword { .. } => true,
            Inp::Literal { .. } | Inp::Star | Inp::Command { .. } => false,
        })
    }

    pub(crate) fn needs_nontails_code(&self) -> bool {
        self.iter_inputs().any(|input| match input {
            Inp::Subword { subdfa, .. } => self.subdfas.lookup(*subdfa).needs_nontails_code(),
            Inp::Literal { .. } | Inp::Star => false,
            Inp::Command { regex, .. } => regex.is_some(),
        })
    }

    pub(crate) fn needs_subword_nontails_code(&self) -> bool {
        self.iter_inputs().any(|input| match input {
            Inp::Subword { subdfa, .. } => self.subdfas.lookup(*subdfa).needs_nontails_code(),
            Inp::Literal { .. } | Inp::Star | Inp::Command { .. } => false,
        })
    }

    pub fn to_dot<W: Write>(&self, output: &mut W, array_start: u32) -> Result<()> {
        writeln!(output, "digraph dfa {{")?;
        writeln!(output, "\trankdir=LR;")?;
        do_to_dot(output, array_start, self, "", 0)?;
        writeln!(output, "}}")?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn to_dot_file<P: AsRef<std::path::Path>>(&self, path: P, array_start: u32) -> Result<()> {
        let mut file = std::fs::File::create(path)?;
        self.to_dot(&mut file, array_start)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::ops::Rem;
    use std::rc::Rc;

    use crate::grammar::{
        Expr, ExprId, Grammar, HumanSpan, Shell, SubwordCompilationPhase, ValidGrammar, alloc,
    };
    use crate::regex::Regex;
    use Expr::*;
    use itertools::Itertools;
    use proptest::bits::{u64, usize};
    use proptest::test_runner::TestRng;

    use super::*;

    use proptest::prelude::*;

    impl Inp {
        fn literal(s: &str) -> Self {
            Inp::Literal {
                literal: ustr(s),
                description: None,
                fallback_level: 0,
            }
        }
    }

    impl DFA {
        fn from_regex_lenient(regex: Regex, subdfas: DFAInternPool) -> Self {
            dfa_from_regex(regex, subdfas)
        }

        fn accepts_str(&self, mut input: &str) -> bool {
            let mut current_state = self.starting_state;
            'outer: while !input.is_empty() {
                for (transition_input_id, to) in self.iter_transitions_from(current_state) {
                    let transition_input = self.get_input(transition_input_id);
                    if let Inp::Literal { literal, .. } = transition_input {
                        let s = literal.as_str();
                        if input.starts_with(s) {
                            input = &input[s.len()..];
                            current_state = to;
                            continue 'outer;
                        }
                    }
                }

                for (transition_input_id, to) in self.iter_transitions_from(current_state) {
                    let transition_input = self.get_input(transition_input_id);
                    if transition_input.is_star(&self.subdfas) {
                        current_state = to;
                        break 'outer;
                    }
                }

                break;
            }
            self.accepting_states.contains(current_state)
        }

        fn accepts(&self, inputs: &[&str]) -> std::result::Result<bool, TestCaseError> {
            let mut input_index = 0;
            let mut current_state = self.starting_state;
            'outer: loop {
                if input_index == inputs.len() {
                    break;
                }

                for (transition_input_id, to) in self.iter_transitions_from(current_state) {
                    let transition_input = self.get_input(transition_input_id);
                    if let Inp::Literal { literal, .. } = transition_input {
                        if inputs[input_index] == literal.as_str() {
                            input_index += 1;
                            current_state = to;
                            continue 'outer;
                        }
                    }
                }

                for (transition_input_id, to) in self.iter_transitions_from(current_state) {
                    let transition_input = self.get_input(transition_input_id);
                    if let Inp::Subword { subdfa, .. } = transition_input {
                        let dfa = self.subdfas.lookup(*subdfa);
                        if dfa.accepts_str(inputs[input_index]) {
                            input_index += 1;
                            current_state = to;
                            continue 'outer;
                        }
                    }
                }

                let anys: Vec<(InpId, StateId)> = self
                    .iter_transitions_from(current_state)
                    .filter(|(input_id, _)| self.get_input(*input_id).is_star(&self.subdfas))
                    .map(|(k, v)| (k.clone(), v))
                    .collect();
                // It's ambiguous which transition to take if there are two transitions
                // representing a nonterminal.
                assert!(anys.len() <= 1);

                for (transition_input_id, to) in anys {
                    let transition_input = self.get_input(transition_input_id);
                    if transition_input.is_star(&self.subdfas) {
                        input_index += 1;
                        current_state = to;
                        continue 'outer;
                    }
                }

                break;
            }
            Ok(self.accepting_states.contains(current_state.into()))
        }

        fn has_transition(&self, from: StateId, input: InpId, to: StateId) -> bool {
            *self.transitions.get(&from).unwrap().get(&input).unwrap() == to
        }
    }

    #[test]
    fn minimal_example() {
        let mut arena: Vec<Expr> = Default::default();
        let expr = alloc(&mut arena, Expr::term("foo"));
        let regex = Regex::from_expr(expr, &arena, Shell::Bash).unwrap();
        let subdfas = DFAInternPool::default();
        assert!(regex.check_ambiguities(&subdfas).is_ok());
        let dfa = DFA::from_regex(regex, DFAInternPool::default()).unwrap();
        let foo = Inp::literal("foo");
        let foo_id = dfa.inputs.find(&foo).unwrap();
        assert_eq!(dfa.transitions.len(), 2);
        let tos: IndexMap<InpId, StateId> = IndexMap::from_iter([(foo_id, 2)]);
        assert_eq!(*dfa.transitions.get(&1).unwrap(), tos);
        assert_eq!(dfa.accepting_states, RoaringBitmap::from_iter([2]));
        assert_eq!(dfa.starting_state, 1);
    }

    const TERMINALS: &[&str] = &["foo", "bar", "--baz", "--quux"];
    const NONTERMINALS: &[&str] = &["FILE", "DIRECTORY", "PATH"];

    fn arb_literal(arena: Rc<RefCell<Vec<Expr>>>, inputs: Rc<Vec<Ustr>>) -> BoxedStrategy<ExprId> {
        (0..inputs.len())
            .prop_map(move |index| {
                let e = Expr::term(&inputs[index]);
                alloc(&mut *arena.borrow_mut(), e)
            })
            .boxed()
    }

    fn arb_nonterminal(
        arena: Rc<RefCell<Vec<Expr>>>,
        nonterminals: Rc<Vec<Ustr>>,
    ) -> BoxedStrategy<ExprId> {
        (0..nonterminals.len())
            .prop_map(move |index| {
                let e = Expr::nontermref(&nonterminals[index]);
                alloc(&mut *arena.borrow_mut(), e)
            })
            .boxed()
    }

    fn arb_optional(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth - 1,
            max_width,
        )
        .prop_map(move |child| {
            let e = Optional {
                child,
                span: Default::default(),
            };
            alloc(&mut *arena.borrow_mut(), e)
        })
        .boxed()
    }

    fn arb_many1(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth - 1,
            max_width,
        )
        .prop_map(move |child| {
            let e = Many1 {
                child,
                span: Default::default(),
            };
            alloc(&mut *arena.borrow_mut(), e)
        })
        .boxed()
    }

    fn arb_sequence(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                let a = Rc::clone(&arena);
                prop::collection::vec(e, width).prop_map(move |children| {
                    let e = Sequence {
                        children,
                        span: HumanSpan::default(),
                    };
                    alloc(&mut *a.borrow_mut(), e)
                })
            })
            .boxed()
    }

    fn arb_alternative(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                let a = Rc::clone(&arena);
                prop::collection::vec(e, width).prop_map(move |children| {
                    let e = Alternative {
                        children,
                        span: HumanSpan::default(),
                    };
                    alloc(&mut *a.borrow_mut(), e)
                })
            })
            .boxed()
    }

    fn arb_expr(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        if remaining_depth <= 1 {
            prop_oneof![
                arb_literal(Rc::clone(&arena), Rc::clone(&inputs)),
                arb_nonterminal(Rc::clone(&arena), nonterminals),
            ]
            .boxed()
        } else {
            prop_oneof![
                arb_literal(Rc::clone(&arena), inputs.clone()),
                arb_nonterminal(Rc::clone(&arena), nonterminals.clone()),
                arb_optional(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_many1(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_sequence(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_alternative(
                    Rc::clone(&arena),
                    inputs,
                    nonterminals,
                    remaining_depth,
                    max_width
                ),
            ]
            .boxed()
        }
    }

    fn do_arb_match(
        arena: Rc<RefCell<Vec<Expr>>>,
        expr: ExprId,
        rng: &mut TestRng,
        max_width: usize,
        output: &mut Vec<Ustr>,
    ) {
        match &arena.borrow()[expr] {
            Terminal { term, .. } => output.push(*term),
            Subword {
                phase: SubwordCompilationPhase::Expr(e),
                ..
            } => {
                let mut out: Vec<Ustr> = Default::default();
                do_arb_match(Rc::clone(&arena), *e, rng, max_width, &mut out);
                let joined = out.into_iter().join("");
                output.push(ustr(&joined));
            }
            Subword { .. } => unreachable!(),
            NontermRef { .. } => output.push(ustr("anything")),
            Command { .. } => output.push(ustr("anything")),
            Sequence { children, .. } => {
                for subexpr in children {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            Alternative { children, .. } => {
                let chosen_branch = rng.next_u64().rem(children.len() as u64) as usize;
                do_arb_match(
                    Rc::clone(&arena),
                    children[chosen_branch],
                    rng,
                    max_width,
                    output,
                );
            }
            Optional { child: subexpr, .. } => {
                if rng.next_u64() % 2 == 0 {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            Many1 { child: subexpr, .. } => {
                let n = rng.next_u64();
                let chosen_len = n % (max_width as u64) + 1;
                for _ in 0..chosen_len {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            DistributiveDescription { child, descr, .. } => {
                do_arb_match(Rc::clone(&arena), *child, rng, max_width, output);
                output.push(ustr(&format!(r#""{descr}""#)));
            }
            Fallback { children, .. } => {
                let chosen_branch =
                    usize::try_from(rng.next_u64().rem(u64::try_from(children.len()).unwrap()))
                        .unwrap();
                do_arb_match(
                    Rc::clone(&arena),
                    children[chosen_branch],
                    rng,
                    max_width,
                    output,
                );
            }
        }
    }

    fn arb_match(
        arena: Rc<RefCell<Vec<Expr>>>,
        e: ExprId,
        mut rng: TestRng,
        max_width: usize,
    ) -> (ExprId, Rc<RefCell<Vec<Expr>>>, Vec<Ustr>) {
        let mut output: Vec<Ustr> = Default::default();
        do_arb_match(Rc::clone(&arena), e, &mut rng, max_width, &mut output);
        (e, arena, output)
    }

    // Produce an arbitrary sequence matching `e`.
    fn arb_expr_match(
        remaining_depth: usize,
        max_width: usize,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
    ) -> BoxedStrategy<(ExprId, Rc<RefCell<Vec<Expr>>>, Vec<Ustr>)> {
        let arena = Rc::new(RefCell::new(vec![]));
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth,
            max_width,
        )
        .prop_perturb(move |e, rng| arb_match(Rc::clone(&arena), e, rng, max_width))
        .boxed()
    }

    proptest! {
        #[test]
        fn accepts_arb_expr_input_from_regex((expr, arena, input) in arb_expr_match(3, 3, Rc::new(TERMINALS.iter().map(|s| ustr(s)).collect()), Rc::new(NONTERMINALS.iter().map(|s| ustr(s)).collect()))) {
            //println!("{:?}", expr);
            //println!("{:?}", arena);
            //println!("{:?}", input);
            let regex = Regex::from_expr(expr, &arena.borrow(), Shell::Bash).unwrap();
            //dbg!(&regex.input_from_position);
            let subdfas = DFAInternPool::default();
            prop_assume!(regex.check_ambiguities(&subdfas).is_ok());
            let dfa = DFA::from_regex_lenient(regex, DFAInternPool::default());
            prop_assume!(dfa.check_ambiguity_best_effort().is_ok());
            let input: Vec<&str> = input.iter().map(|s| s.as_str()).collect();
            prop_assert!(dfa.accepts(&input)?);
        }

        #[test]
        fn minimized_dfa_equivalent_to_input_one((expr, arena, input) in arb_expr_match(10, 3, Rc::new(TERMINALS.iter().map(|s| ustr(s)).collect()), Rc::new(NONTERMINALS.iter().map(|s| ustr(s)).collect()))) {
            println!("{:?}", expr);
            println!("{:?}", input);
            let regex = Regex::from_expr(expr, &arena.borrow(), Shell::Bash).unwrap();
            let subdfas = DFAInternPool::default();
            prop_assume!(regex.check_ambiguities(&subdfas).is_ok());
            let dfa = DFA::from_regex_lenient(regex, DFAInternPool::default());
            prop_assume!(dfa.check_ambiguity_best_effort().is_ok());
            let input: Vec<&str> = input.iter().map(|s| s.as_str()).collect();
            prop_assert!(dfa.accepts(&input)?);
            let minimal_dfa = dfa.minimize();
            prop_assert!(minimal_dfa.accepts(&input)?);
        }
    }

    #[test]
    fn engineering_a_compiler_book_dfa_minimization_example() {
        let mut inputs: InpInternPool = Default::default();
        let f_id = inputs.intern(Inp::literal("f"));
        let e_id = inputs.intern(Inp::literal("e"));
        let i_id = inputs.intern(Inp::literal("i"));
        let dfa = {
            let starting_state = 0;
            let mut transitions: IndexMap<StateId, IndexMap<InpId, StateId>> = Default::default();
            transitions.entry(0).or_default().insert(f_id, 1);
            transitions.entry(1).or_default().insert(e_id, 2);
            transitions.entry(1).or_default().insert(i_id, 4);
            transitions.entry(2).or_default().insert(e_id, 3);
            transitions.entry(4).or_default().insert(e_id, 5);
            let accepting_states = RoaringBitmap::from_iter([3, 5]);
            DFA {
                starting_state,
                transitions,
                accepting_states,
                inputs,
                subdfas: DFAInternPool::default(),
            }
        };
        let minimized = dfa.minimize();
        assert_eq!(minimized.starting_state, 0);
        assert_eq!(minimized.accepting_states, RoaringBitmap::from_iter([3]));
        assert!(minimized.has_transition(0, f_id, 1));
        assert!(minimized.has_transition(1, e_id, 2));
        assert!(minimized.has_transition(1, i_id, 2));
        assert!(minimized.has_transition(2, e_id, 3));
    }

    #[test]
    fn grammar_terms_get_deduped() {
        const INPUT: &str = r#"
foo DUPLICATED_TERM;
foo DUPLICATED_TERM;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        let vg = ValidGrammar::from_grammar(g, Shell::Bash).unwrap();
        let regex = Regex::from_valid_grammar(&vg, Shell::Bash).unwrap();
        let dfa = DFA::from_regex(regex, vg.subdfas).unwrap();

        // There should be only one tansition on input Term("DUPLICATED_TERM")
        let transitions = dfa.get_literal_transitions_from(dfa.starting_state);
        assert_eq!(transitions.len(), 1);
        assert_eq!(transitions[0].0, "DUPLICATED_TERM");
    }
}
