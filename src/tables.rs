use std::hash::Hasher;
use std::{collections::BTreeMap, hash::DefaultHasher};

use hashbrown::HashMap;
use indexmap::IndexSet;
use roaring::RoaringBitmap;
use ustr::Ustr;

use crate::dfa::DFA;
use crate::{CommandId, LiteralId, StateId};

pub(crate) struct MatchTransitions {
    pub(crate) literal: BTreeMap<StateId, BTreeMap<LiteralId, StateId>>,
    pub(crate) command: Option<BTreeMap<StateId, BTreeMap<CommandId, StateId>>>,
    pub(crate) compadd: Option<BTreeMap<StateId, BTreeMap<CommandId, StateId>>>,
    pub(crate) star: Option<Vec<(StateId, StateId)>>,
}

pub(crate) struct CompletionTransitions {
    pub(crate) max_fallback_level: usize,
    pub(crate) literal: Vec<BTreeMap<StateId, RoaringBitmap>>,
    pub(crate) command: Option<Vec<BTreeMap<StateId, RoaringBitmap>>>,
    pub(crate) compadd: Option<Vec<BTreeMap<StateId, Vec<usize>>>>,
}

pub(crate) struct LookupTables {
    pub(crate) all_literals: Vec<(LiteralId, Ustr, Ustr)>,
    pub(crate) match_transitions: MatchTransitions,
    pub(crate) completion_transitions: CompletionTransitions,
}

impl LookupTables {
    // Excludes exact literal values
    pub(crate) fn shape_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();

        let Self {
            all_literals: _,
            match_transitions,
            completion_transitions,
        } = self;

        let MatchTransitions {
            literal,
            command,
            compadd,
            star,
        } = match_transitions;

        for (from, tos) in literal {
            hasher.write_u32(*from);
            for (lit_id, to) in tos {
                hasher.write_u32(*lit_id);
                hasher.write_u32(*to);
            }
        }

        if let Some(command) = command {
            for (from, tos) in command {
                hasher.write_u32(*from);
                for (cmd_id, to) in tos {
                    hasher.write_u32(*cmd_id);
                    hasher.write_u32(*to);
                }
            }
        }

        if let Some(compadd) = compadd {
            for (from, tos) in compadd {
                hasher.write_u32(*from);
                for (cmd_id, to) in tos {
                    hasher.write_u32(*cmd_id);
                    hasher.write_u32(*to);
                }
            }
        }

        if let Some(star) = star {
            for (from, to) in star {
                hasher.write_u32(*from);
                hasher.write_u32(*to);
            }
        }

        let CompletionTransitions {
            max_fallback_level,
            literal,
            command,
            compadd: _,
        } = completion_transitions;

        hasher.write_usize(*max_fallback_level);

        for level in literal {
            for (from, lit_ids) in level {
                hasher.write_u32(*from);
                for id in lit_ids {
                    hasher.write_u32(id);
                }
            }
        }

        if let Some(command) = command {
            for level in command {
                for (from, cmd_ids) in level {
                    hasher.write_u32(*from);
                    for id in cmd_ids {
                        hasher.write_u32(id);
                    }
                }
            }
        }

        hasher.finish()
    }

    // Equivalent sub-DFAs, ignoring literal values, IOW: "same-shape"
    pub(crate) fn isomorphic_to(&self, other: &LookupTables) -> bool {
        let LookupTables {
            all_literals: _,
            match_transitions: left_match_transitions,
            completion_transitions: left_completion_transitions,
        } = self;

        let LookupTables {
            all_literals: _,
            match_transitions: right_match_transitions,
            completion_transitions: right_completion_transitions,
        } = other;

        let MatchTransitions {
            literal: left_literal,
            command: left_command,
            compadd: left_compadd,
            star: left_star,
        } = &left_match_transitions;

        let MatchTransitions {
            literal: right_literal,
            command: right_command,
            compadd: right_compadd,
            star: right_star,
        } = &right_match_transitions;

        if left_literal != right_literal {
            return false;
        }

        if left_command != right_command {
            return false;
        }

        if left_compadd != right_compadd {
            return false;
        }

        if left_star != right_star {
            return false;
        }

        let CompletionTransitions {
            max_fallback_level: left_max_fallback_level,
            literal: left_literal,
            command: left_command,
            compadd: _,
        } = left_completion_transitions;

        let CompletionTransitions {
            max_fallback_level: right_max_fallback_level,
            literal: right_literal,
            command: right_command,
            compadd: _,
        } = right_completion_transitions;

        if left_max_fallback_level != right_max_fallback_level {
            return false;
        }

        if left_literal != right_literal {
            return false;
        }

        if left_command != right_command {
            return false;
        }

        true
    }
}

pub(crate) fn get_match_transitions(
    dfa: &DFA,
    id_from_literal_description: &HashMap<(Ustr, Ustr), LiteralId>,
    id_from_cmd: &IndexSet<Ustr>,
    needs_commands_code: bool,
    needs_compadds_code: bool,
    needs_star_code: bool,
) -> MatchTransitions {
    let all_states = dfa.get_all_states();

    let literal_transitions = dfa.get_literal_transitions(&all_states, id_from_literal_description);

    let command_transitions = 'commands: {
        if !needs_commands_code {
            break 'commands None;
        }
        Some(dfa.get_command_transitions(&all_states, id_from_cmd))
    };

    let compadd_transitions = 'compadds: {
        if !needs_compadds_code {
            break 'compadds None;
        }
        Some(dfa.get_compadd_transitions(&all_states, id_from_cmd))
    };

    let star_transitions = 'stars: {
        if !needs_star_code {
            break 'stars None;
        }
        Some(dfa.iter_top_level_star_transitions().collect())
    };

    MatchTransitions {
        literal: literal_transitions,
        command: command_transitions,
        compadd: compadd_transitions,
        star: star_transitions,
    }
}

pub(crate) fn get_completion_transitions(
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    needs_commands_code: bool,
    needs_compadds_code: bool,
    id_from_literal_description: &HashMap<(Ustr, Ustr), LiteralId>,
    max_fallback_level: usize,
) -> CompletionTransitions {
    let literal_completions =
        dfa.get_literal_completions(id_from_literal_description, max_fallback_level);

    let command_completions = 'commands: {
        if !needs_commands_code {
            break 'commands None;
        }
        Some(dfa.get_command_completions(id_from_cmd, max_fallback_level))
    };

    let compadd_completions = 'compadds: {
        if !needs_compadds_code {
            break 'compadds None;
        }
        Some(dfa.get_completion_compadds(id_from_cmd, max_fallback_level))
    };

    CompletionTransitions {
        max_fallback_level,
        literal: literal_completions,
        command: command_completions,
        compadd: compadd_completions,
    }
}

pub(crate) fn get_lookup_tables(
    dfa: &DFA,
    id_from_cmd: &IndexSet<Ustr>,
    array_start: usize,
    needs_commands_code: bool,
    needs_compadds_code: bool,
    needs_star_code: bool,
) -> LookupTables {
    let all_literals = dfa.get_all_literals(array_start);

    let id_from_literal_description: HashMap<(Ustr, Ustr), LiteralId> = all_literals
        .iter()
        .map(|(id, input, description)| ((*input, *description), *id))
        .collect();

    let match_transitions = get_match_transitions(
        dfa,
        &id_from_literal_description,
        id_from_cmd,
        needs_commands_code,
        needs_compadds_code,
        needs_star_code,
    );
    let max_fallback_level = dfa.get_max_fallback_level().unwrap_or(array_start);
    let completion_transitions = get_completion_transitions(
        dfa,
        id_from_cmd,
        needs_commands_code,
        needs_compadds_code,
        &id_from_literal_description,
        max_fallback_level,
    );
    LookupTables {
        all_literals,
        match_transitions,
        completion_transitions,
    }
}
