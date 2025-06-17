use crate::{Error, Result};
use hashbrown::HashSet;
use indexmap::IndexSet;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use roaring::RoaringBitmap;
use ustr::{Ustr, UstrMap};

use crate::grammar::{
    ChicSpan, CmdRegexDecl, DFAId, Expr, Shell, Specialization, SubwordCompilationPhase,
};

pub type Position = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal {
        literal: Ustr,
        description: Option<Ustr>,
        fallback_level: usize,
        span: ChicSpan,
    },
    Subword {
        subdfa: DFAId,
        fallback_level: usize,
        span: ChicSpan,
    },
    Nonterminal {
        nonterm: Ustr,
        spec: Option<Specialization>,
        fallback_level: usize,
        span: ChicSpan,
    },
    Command {
        cmd: Ustr,
        regex: Option<CmdRegexDecl>,
        fallback_level: usize,
        span: ChicSpan,
    },
}

impl Input {
    pub fn is_ambiguous(&self, shell: Shell) -> bool {
        match self {
            Self::Literal { .. } => false,
            Self::Subword { .. } => false,
            Self::Nonterminal { .. } => true,
            Self::Command { regex: None, .. } => true,
            Self::Command {
                regex: Some(regex), ..
            } => regex.matches_anything(shell),
        }
    }

    pub fn get_fallback_level(&self) -> usize {
        match self {
            Self::Literal {
                fallback_level: level,
                ..
            } => *level,
            Self::Subword {
                fallback_level: level,
                ..
            } => *level,
            Self::Nonterminal {
                fallback_level: level,
                ..
            } => *level,
            Self::Command {
                fallback_level: level,
                ..
            } => *level,
        }
    }
}

impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword {
                subdfa: subword, ..
            } => write!(f, r#"{subword:?}"#),
            Self::Literal { literal, .. } => write!(f, r#"{literal}"#),
            Self::Nonterminal {
                nonterm: nonterminal,
                ..
            } => write!(f, r#"<{nonterminal}>"#),
            Self::Command { cmd: command, .. } => write!(f, r#"{{{{{{ {command} }}}}}}"#),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum RegexNode {
    Epsilon,
    Subword(Position),
    Terminal(Ustr, usize, Position), // terminal, fallback level, position
    Nonterminal(Position),
    Command(Ustr, Position),
    Cat(usize, usize),
    Or(Box<[usize]>, bool), // is fallback
    Star(usize),
    EndMarker(Position),
}

impl std::fmt::Debug for RegexNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(position) => f.write_fmt(format_args!(r#"Subword({position})"#)),
            Self::Terminal(term, level, position) => f.write_fmt(format_args!(
                r#"Terminal({:?}.to_string(), {level}, {position})"#,
                term
            )),
            Self::Nonterminal(position) => {
                f.write_fmt(format_args!(r#"Nonterminal({})"#, position))
            }
            Self::Command(code, position) => f.write_fmt(format_args!(
                r#"Command({:?}.to_string(), {})"#,
                code, position
            )),
            Self::Cat(left, right) => f.write_fmt(format_args!(r#"Cat({:?}, {:?})"#, left, right)),
            Self::Or(arg0, is_fallback) => {
                f.write_fmt(format_args!(r#"Or(vec!{:?}, {is_fallback})"#, arg0))
            }
            Self::Star(arg0) => f.write_fmt(format_args!(r#"Star({:?})"#, arg0)),
            Self::EndMarker(position) => f.write_fmt(format_args!(r#"EndMarker({})"#, position)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}

fn do_firstpos(re: &RegexNode, arena: &[RegexNode], result: &mut BTreeSet<Position>) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(_, _, position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(_, position) => {
            result.insert(*position);
        }
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_firstpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            if left.nullable(arena) {
                do_firstpos(left, arena, result);
                do_firstpos(right, arena, result);
            } else {
                do_firstpos(left, arena, result);
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            do_firstpos(subregex, arena, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_lastpos(re: &RegexNode, arena: &[RegexNode], result: &mut HashSet<Position>) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(position) => {
            result.insert(*position);
        }
        RegexNode::Terminal(_, _, position) => {
            result.insert(*position);
        }
        RegexNode::Nonterminal(position) => {
            result.insert(*position);
        }
        RegexNode::Command(_, position) => {
            result.insert(*position);
        }
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_lastpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            if right.nullable(arena) {
                do_lastpos(right, arena, result);
                do_lastpos(left, arena, result);
            } else {
                do_lastpos(right, arena, result);
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            do_lastpos(subregex, arena, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_followpos(
    re: &RegexNode,
    arena: &[RegexNode],
    result: &mut BTreeMap<Position, RoaringBitmap>,
) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(..) => {}
        RegexNode::Terminal(..) => {}
        RegexNode::Nonterminal(..) => {}
        RegexNode::Command(..) => {}
        RegexNode::Or(subregexes, _) => {
            for subreid in subregexes {
                let subre = &arena[*subreid];
                do_followpos(subre, arena, result);
            }
        }
        RegexNode::Cat(leftid, rightid) => {
            let left = &arena[*leftid];
            let right = &arena[*rightid];
            do_followpos(left, arena, result);
            do_followpos(right, arena, result);
            let fp = right.firstpos(arena);
            for i in left.lastpos(arena) {
                for j in &fp {
                    result.entry(i).or_default().insert(*j);
                }
            }
        }
        RegexNode::Star(subregexid) => {
            let subregex = &arena[*subregexid];
            let first = subregex.firstpos(arena);
            let last = subregex.lastpos(arena);
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(*j);
                }
            }
        }
        RegexNode::EndMarker(_) => {}
    }
}

impl RegexNode {
    fn nullable(&self, arena: &[RegexNode]) -> bool {
        match self {
            RegexNode::Epsilon => true,
            RegexNode::Subword(..) => false,
            RegexNode::Terminal(..) => false,
            RegexNode::Nonterminal(..) => false,
            RegexNode::Command(..) => false,
            RegexNode::Or(children, _) => children.iter().any(|child_id| {
                let child = &arena[*child_id];
                child.nullable(arena)
            }),
            RegexNode::Cat(leftid, rightid) => {
                let left = &arena[*leftid];
                let right = &arena[*rightid];
                left.nullable(arena) && right.nullable(arena)
            }
            RegexNode::Star(_) => true,
            RegexNode::EndMarker(_) => true,
        }
    }

    pub fn firstpos(&self, arena: &[RegexNode]) -> BTreeSet<Position> {
        let mut result: BTreeSet<Position> = Default::default();
        do_firstpos(self, arena, &mut result);
        result
    }

    fn lastpos(&self, arena: &[RegexNode]) -> HashSet<Position> {
        let mut result: HashSet<Position> = Default::default();
        do_lastpos(self, arena, &mut result);
        result
    }

    pub fn followpos(&self, arena: &[RegexNode]) -> BTreeMap<Position, RoaringBitmap> {
        let mut result: BTreeMap<Position, RoaringBitmap> = Default::default();
        do_followpos(self, arena, &mut result);
        result
    }
}

fn do_from_expr<'a>(
    e: &Expr,
    specs: &UstrMap<Specialization>,
    arena: &mut Vec<RegexNode>,
    symbols: &mut IndexSet<Input>,
    input_from_position: &mut Vec<Input>,
) -> usize {
    match e {
        Expr::Terminal(term, description, level, span) => {
            let result = RegexNode::Terminal(
                *term,
                *level,
                Position::try_from(input_from_position.len()).unwrap(),
            );
            let input = Input::Literal {
                literal: *term,
                description: *description,
                fallback_level: *level,
                span: span.clone(),
            };
            input_from_position.push(input.clone());
            symbols.insert(input);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::Subword(subword, fallback_level, span) => {
            let result = RegexNode::Subword(Position::try_from(input_from_position.len()).unwrap());
            let dfa = match subword {
                SubwordCompilationPhase::DFA(dfa) => dfa,
                SubwordCompilationPhase::Expr(_) => unreachable!(),
            };
            let input = Input::Subword {
                subdfa: dfa.clone(),
                fallback_level: *fallback_level,
                span: span.clone(),
            };
            input_from_position.push(input.clone());
            symbols.insert(input);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::NontermRef(name, fallback_level, span) => {
            let result =
                RegexNode::Nonterminal(Position::try_from(input_from_position.len()).unwrap());
            let specialization = specs.get(name);
            let input = Input::Nonterminal {
                nonterm: *name,
                spec: specialization.copied(),
                fallback_level: *fallback_level,
                span: span.clone(),
            };
            input_from_position.push(input.clone());
            symbols.insert(input);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::Command(code, cmd_regex_decl, fallback_level, span) => {
            let result = RegexNode::Command(
                *code,
                Position::try_from(input_from_position.len()).unwrap(),
            );
            let input = Input::Command {
                cmd: *code,
                regex: *cmd_regex_decl,
                fallback_level: *fallback_level,
                span: span.clone(),
            };
            input_from_position.push(input.clone());
            symbols.insert(input);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::Sequence(subexprs) => {
            let mut left_regex_id =
                do_from_expr(&subexprs[0], specs, arena, symbols, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex_id =
                    do_from_expr(right_expr, specs, arena, symbols, input_from_position);
                let left_regex = RegexNode::Cat(left_regex_id, right_regex_id);
                left_regex_id = {
                    let id = arena.len();
                    arena.push(left_regex);
                    id
                };
            }
            left_regex_id
        }
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<usize> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, specs, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), false);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(subexpr, specs, arena, symbols, input_from_position);
            let epsid = {
                let id = arena.len();
                arena.push(RegexNode::Epsilon);
                id
            };
            let result = RegexNode::Or(Box::new([subregex, epsid]), false);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::Many1(subexpr) => {
            let subregex_id = do_from_expr(subexpr, specs, arena, symbols, input_from_position);
            let star = RegexNode::Star(subregex_id);
            let starid = {
                let id = arena.len();
                arena.push(star);
                id
            };
            let result = RegexNode::Cat(subregex_id, starid);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
        Expr::DistributiveDescription(_, _) => unreachable!(
            "DistributiveDescription Expr type should have been erased before compilation to regex"
        ),
        Expr::Fallback(subexprs) => {
            let mut subregexes: Vec<usize> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, specs, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            let result = RegexNode::Or(subregexes.into_boxed_slice(), true);
            let result_id = {
                let id = arena.len();
                arena.push(result);
                id
            };
            result_id
        }
    }
}

#[derive(Debug)]
pub struct Regex {
    pub root: RegexNode,
    pub input_symbols: Rc<IndexSet<Input>>,
    pub input_from_position: Vec<Input>,
    pub endmarker_position: Position,
    pub myarena: Vec<RegexNode>,
}

/*

Ambiguous matching arises from:

1) ({{{ ... }}} | foo), i.e. commands at non-tail position; (foo | {{{ ... }}}) is fine
2) Same with <NONTERM>, where <NONTERM> is undefined, because it matches anything then

*/
fn do_ensure_ambiguous_inputs_tail_only(
    firstpos: &RoaringBitmap,
    followpos: &BTreeMap<Position, RoaringBitmap>,
    input_from_position: &Vec<Input>,
    shell: Shell,
    visited: &mut RoaringBitmap,
) -> Result<()> {
    let unvisited = firstpos - visited.clone();
    if unvisited.len() == 0 {
        return Ok(());
    }
    if unvisited.len() == 1 {
        let pos = unvisited.select(0).unwrap();
        let Some(follow) = followpos.get(&pos) else {
            return Ok(());
        };
        visited.insert(pos);
        return do_ensure_ambiguous_inputs_tail_only(
            follow,
            followpos,
            input_from_position,
            shell,
            visited,
        );
    }
    let inputs: Vec<Input> = unvisited
        .iter()
        .map(|pos| input_from_position[pos as usize].clone())
        .collect();

    let mut prev_ambiguous: Option<Input> = None;
    for inp in inputs {
        if inp.is_ambiguous(shell) {
            if let Some(prev_inp) = prev_ambiguous {
                return Err(Error::AmbiguousMatchable(prev_inp, inp));
            }
            prev_ambiguous = Some(inp);
        }
    }

    for pos in unvisited {
        let Some(follow) = followpos.get(&pos) else {
            continue;
        };
        visited.insert(pos);
        do_ensure_ambiguous_inputs_tail_only(
            follow,
            followpos,
            input_from_position,
            shell,
            visited,
        )?;
    }
    Ok(())
}

fn do_ensure_ambiguous_inputs_tail_only_subword(
    firstpos: &RoaringBitmap,
    followpos: &BTreeMap<Position, RoaringBitmap>,
    input_from_position: &Vec<Input>,
    shell: Shell,
    path_prev_ambiguous: Option<Input>,
    visited: &mut RoaringBitmap,
) -> Result<()> {
    let unvisited = firstpos - visited.clone();
    if unvisited.len() == 0 {
        return Ok(());
    }
    if unvisited.len() == 1 {
        let pos = unvisited.select(0).unwrap();
        let Some(follow) = followpos.get(&pos) else {
            return Ok(());
        };
        let inp = input_from_position[pos as usize].clone();
        let this_ambiguous = if inp.is_ambiguous(shell) {
            Some(inp)
        } else {
            None
        };
        match (&path_prev_ambiguous, &this_ambiguous) {
            (Some(prev_inp), Some(inp)) => {
                return Err(Error::AmbiguousMatchable(prev_inp.clone(), inp.clone()));
            }
            _ => (),
        };
        visited.insert(pos);
        return do_ensure_ambiguous_inputs_tail_only_subword(
            follow,
            followpos,
            input_from_position,
            shell,
            path_prev_ambiguous.clone().or(this_ambiguous),
            visited,
        );
    }
    let inputs: Vec<Input> = unvisited
        .iter()
        .filter_map(|pos| input_from_position.get(pos as usize).cloned())
        .collect();

    let mut prev_ambiguous: Option<Input> = None;
    for inp in inputs {
        if let Some(ref prev_inp) = path_prev_ambiguous {
            return Err(Error::AmbiguousMatchable(prev_inp.clone(), inp));
        }

        if let Some(prev_inp) = prev_ambiguous {
            return Err(Error::AmbiguousMatchable(prev_inp, inp));
        }
        if inp.is_ambiguous(shell) {
            prev_ambiguous = Some(inp);
        }
    }

    for pos in unvisited {
        let Some(follow) = followpos.get(&pos) else {
            continue;
        };
        visited.insert(pos);
        do_ensure_ambiguous_inputs_tail_only_subword(
            follow,
            followpos,
            input_from_position,
            shell,
            path_prev_ambiguous.clone().or(prev_ambiguous.clone()),
            visited,
        )?;
    }
    Ok(())
}

impl Regex {
    pub fn from_expr(e: &Expr, specs: &UstrMap<Specialization>) -> Result<Self> {
        let mut input_symbols: IndexSet<Input> = Default::default();
        let mut input_from_position: Vec<Input> = Default::default();
        let mut myarena: Vec<RegexNode> = Default::default();
        let regex = do_from_expr(
            e,
            specs,
            &mut myarena,
            &mut input_symbols,
            &mut input_from_position,
        );
        let endmarker_position = input_from_position.len() as Position;
        let endmarkerid = {
            let id = myarena.len();
            myarena.push(RegexNode::EndMarker(endmarker_position));
            id
        };
        let root = RegexNode::Cat(regex, endmarkerid);

        let retval = Self {
            root,
            input_symbols: Rc::new(input_symbols),
            endmarker_position,
            input_from_position,
            myarena,
        };
        Ok(retval)
    }

    pub fn ensure_ambiguous_inputs_tail_only(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_ensure_ambiguous_inputs_tail_only(
            &RoaringBitmap::from_iter(&self.root.firstpos(&self.myarena)),
            &self.root.followpos(&self.myarena),
            &self.input_from_position,
            shell,
            &mut visited,
        )
    }

    pub fn ensure_ambiguous_inputs_tail_only_subword(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_ensure_ambiguous_inputs_tail_only_subword(
            &RoaringBitmap::from_iter(&self.root.firstpos(&self.myarena)),
            &self.root.followpos(&self.myarena),
            &self.input_from_position,
            shell,
            None,
            &mut visited,
        )
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        self.root.firstpos(&self.myarena)
    }

    pub fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
        self.root.followpos(&self.myarena)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Error, Result};
    use ustr::ustr;

    fn make_sample_star_regex(arena: &mut Vec<RegexNode>) -> usize {
        let t1 = {
            let id = arena.len();
            arena.push(RegexNode::Terminal(ustr("a"), 0, 1));
            id
        };
        let t2 = {
            let id = arena.len();
            arena.push(RegexNode::Terminal(ustr("b"), 0, 2));
            id
        };
        let or_ = RegexNode::Or(Box::new([t1, t2]), false);
        let or_id = {
            let id = arena.len();
            arena.push(or_);
            id
        };
        let star = RegexNode::Star(or_id);
        let star_id = {
            let id = arena.len();
            arena.push(star);
            id
        };
        star_id
    }

    fn make_sample_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        // (a|b)*a
        let t = {
            let id = arena.len();
            arena.push(RegexNode::Terminal(ustr("a"), 0, 3));
            id
        };
        RegexNode::Cat(make_sample_star_regex(arena), t)
    }

    #[test]
    fn nullable() {
        let mut arena = Default::default();
        let star_regex_id = make_sample_star_regex(&mut arena);
        let star_regex = &arena[star_regex_id];
        assert!(star_regex.nullable(&arena));

        let regex = make_sample_regex(&mut arena);
        assert!(!regex.nullable(&arena));
    }

    #[test]
    fn firstpos() {
        let mut arena = Default::default();
        let regex = make_sample_regex(&mut arena);
        assert_eq!(regex.firstpos(&arena), BTreeSet::from([1, 2, 3]));
    }

    #[test]
    fn lastpos() {
        let mut arena = Default::default();
        let regex = make_sample_regex(&mut arena);
        assert_eq!(regex.lastpos(&arena), HashSet::from([3]));
    }

    fn alloc(arena: &mut Vec<RegexNode>, node: RegexNode) -> usize {
        let id = arena.len();
        arena.push(node);
        id
    }

    fn make_followpos_regex(arena: &mut Vec<RegexNode>) -> RegexNode {
        use RegexNode::*;

        let c0 = make_sample_star_regex(arena);
        let c1 = alloc(arena, Terminal(ustr("a"), 0, 3));
        let c2 = alloc(arena, Cat(c0, c1));
        let c3 = alloc(arena, Terminal(ustr("b"), 0, 4));
        let c4 = alloc(arena, Cat(c2, c3));
        let c5 = alloc(arena, Terminal(ustr("b"), 0, 5));

        // (a|b)*abb#
        RegexNode::Cat(alloc(arena, Cat(c4, c5)), alloc(arena, EndMarker(6)))
    }

    #[test]
    fn followpos() {
        let mut arena = Default::default();
        let regex = make_followpos_regex(&mut arena);
        let fp = regex.followpos(&mut arena);
        assert_eq!(fp.get(&6), None);
        assert_eq!(fp.get(&5), Some(&RoaringBitmap::from_iter([6])));
        assert_eq!(fp.get(&4), Some(&RoaringBitmap::from_iter([5])));
        assert_eq!(fp.get(&3), Some(&RoaringBitmap::from_iter([4])));
        assert_eq!(fp.get(&2), Some(&RoaringBitmap::from_iter([1, 2, 3])));
        assert_eq!(fp.get(&1), Some(&RoaringBitmap::from_iter([1, 2, 3])));
    }

    fn get_validated_grammar(input: &str) -> Result<crate::grammar::ValidGrammar> {
        let g = crate::grammar::Grammar::parse(input)
            .map_err(|e| e.to_string())
            .unwrap();
        let validated = crate::grammar::ValidGrammar::from_grammar(g, Shell::Bash)?;
        let specs = UstrMap::default();
        let regex = Regex::from_expr(&validated.expr, &specs).unwrap();
        regex.ensure_ambiguous_inputs_tail_only(Shell::Bash)?;
        Ok(validated)
    }

    #[test]
    fn detects_nontail_command_subword() {
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ git tag }}}..{{{ git tag }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option={{{ echo foo }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ echo foo }}}{{{ echo bar }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/49
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable(_, _))
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> ::= {{{ echo dummy }}};"#),
            Err(Error::AmbiguousMatchable(_, _))
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option=<FOO>; <FOO@bash> ::= {{{ echo bash }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"
duf -hide-fs <FS>[,<FS>]...;
<FS@fish> ::= {{{ string split ' ' --fields 3 </proc/mounts | sort --unique }}};
"#
            ),
            Err(Error::AmbiguousMatchable(_, _))
        ));
    }
}
