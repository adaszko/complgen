use crate::{Error, Result};
use hashbrown::HashSet;
use indexmap::IndexSet;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use bumpalo::Bump;
use roaring::RoaringBitmap;
use ustr::{Ustr, UstrMap};

use crate::grammar::{CmdRegexDecl, DFARef, Expr, Shell, Specialization, SubwordCompilationPhase};

pub type Position = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal(Ustr, Option<Ustr>, usize), // literal, optional description, fallback level
    Subword(DFARef, usize),
    Nonterminal(Ustr, Option<Specialization>, usize), // name, specialization, fallback level
    Command(Ustr, Option<CmdRegexDecl>, usize),       // command, fallback level
}

impl Input {
    pub fn is_ambiguous(&self, shell: Shell) -> bool {
        match self {
            Self::Literal(..) => false,
            Self::Subword(..) => false,
            Self::Nonterminal(..) => true,
            Self::Command(_, None, _) => true,
            Self::Command(_, Some(regex), _) => regex.matches_anything(shell),
        }
    }

    pub fn get_fallback_level(&self) -> usize {
        match self {
            Self::Literal(_, _, level) => *level,
            Self::Subword(_, level) => *level,
            Self::Nonterminal(_, _, level) => *level,
            Self::Command(_, _, level) => *level,
        }
    }
}

impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(subword, _) => write!(f, r#"{subword:?}"#),
            Self::Literal(literal, ..) => write!(f, r#"{literal}"#),
            Self::Nonterminal(nonterminal, ..) => write!(f, r#"<{nonterminal}>"#),
            Self::Command(command, ..) => write!(f, r#"{{{{{{ {command} }}}}}}"#),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum RegexNode<'a> {
    Epsilon,
    Subword(Position),
    Terminal(Ustr, usize, Position), // terminal, fallback level, position
    Nonterminal(Position),
    Command(Ustr, Position),
    Cat(&'a RegexNode<'a>, &'a RegexNode<'a>),
    Or(Vec<RegexNode<'a>>, bool), // is fallback
    Star(&'a RegexNode<'a>),
    EndMarker(Position),
}

impl<'a> std::fmt::Debug for RegexNode<'a> {
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

fn do_firstpos(re: &RegexNode, result: &mut BTreeSet<Position>) {
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
            for subre in subregexes {
                do_firstpos(subre, result);
            }
        }
        RegexNode::Cat(left, right) => {
            if left.nullable() {
                do_firstpos(left, result);
                do_firstpos(right, result);
            } else {
                do_firstpos(left, result);
            }
        }
        RegexNode::Star(subregex) => {
            do_firstpos(subregex, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_lastpos(re: &RegexNode, result: &mut HashSet<Position>) {
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
            for subre in subregexes {
                do_lastpos(subre, result);
            }
        }
        RegexNode::Cat(left, right) => {
            if right.nullable() {
                do_lastpos(right, result);
                do_lastpos(left, result);
            } else {
                do_lastpos(right, result);
            }
        }
        RegexNode::Star(subregex) => {
            do_lastpos(subregex, result);
        }
        RegexNode::EndMarker(position) => {
            result.insert(*position);
        }
    }
}

fn do_followpos(re: &RegexNode, result: &mut BTreeMap<Position, RoaringBitmap>) {
    match re {
        RegexNode::Epsilon => {}
        RegexNode::Subword(..) => {}
        RegexNode::Terminal(..) => {}
        RegexNode::Nonterminal(..) => {}
        RegexNode::Command(..) => {}
        RegexNode::Or(subregexes, _) => {
            for subre in subregexes {
                do_followpos(subre, result);
            }
        }
        RegexNode::Cat(left, right) => {
            do_followpos(left, result);
            do_followpos(right, result);
            let fp = right.firstpos();
            for i in left.lastpos() {
                for j in &fp {
                    result.entry(i).or_default().insert(*j);
                }
            }
        }
        RegexNode::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(*j);
                }
            }
        }
        RegexNode::EndMarker(_) => {}
    }
}

impl<'a> RegexNode<'a> {
    fn nullable(&self) -> bool {
        match self {
            RegexNode::Epsilon => true,
            RegexNode::Subword(..) => false,
            RegexNode::Terminal(..) => false,
            RegexNode::Nonterminal(..) => false,
            RegexNode::Command(..) => false,
            RegexNode::Or(children, _) => children.iter().any(|child| child.nullable()),
            RegexNode::Cat(left, right) => left.nullable() && right.nullable(),
            RegexNode::Star(_) => true,
            RegexNode::EndMarker(_) => true,
        }
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        let mut result: BTreeSet<Position> = Default::default();
        do_firstpos(self, &mut result);
        result
    }

    fn lastpos(&self) -> HashSet<Position> {
        let mut result: HashSet<Position> = Default::default();
        do_lastpos(self, &mut result);
        result
    }

    pub fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
        let mut result: BTreeMap<Position, RoaringBitmap> = Default::default();
        do_followpos(self, &mut result);
        result
    }
}

fn do_from_expr<'a>(
    e: &Expr,
    specs: &UstrMap<Specialization>,
    arena: &'a Bump,
    symbols: &mut IndexSet<Input>,
    input_from_position: &mut Vec<Input>,
) -> RegexNode<'a> {
    match e {
        Expr::Terminal(term, description, level, _) => {
            let result = RegexNode::Terminal(
                *term,
                *level,
                Position::try_from(input_from_position.len()).unwrap(),
            );
            let input = Input::Literal(*term, *description, *level);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        }
        Expr::Subword(subword, fallback_level) => {
            let result = RegexNode::Subword(Position::try_from(input_from_position.len()).unwrap());
            let dfa = match subword {
                SubwordCompilationPhase::DFA(dfa) => dfa,
                SubwordCompilationPhase::Expr(_) => unreachable!(),
            };
            let input = Input::Subword(dfa.clone(), *fallback_level);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        }
        Expr::NontermRef(name, fallback_level, _) => {
            let result =
                RegexNode::Nonterminal(Position::try_from(input_from_position.len()).unwrap());
            let specialization = specs.get(name);
            let input = Input::Nonterminal(*name, specialization.copied(), *fallback_level);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        }
        Expr::Command(code, cmd_regex_decl, fallback_level, ..) => {
            let result = RegexNode::Command(
                *code,
                Position::try_from(input_from_position.len()).unwrap(),
            );
            let input = Input::Command(*code, *cmd_regex_decl, *fallback_level);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        }
        Expr::Sequence(subexprs) => {
            let mut left_regex =
                do_from_expr(&subexprs[0], specs, arena, symbols, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex = arena.alloc(do_from_expr(
                    right_expr,
                    specs,
                    arena,
                    symbols,
                    input_from_position,
                ));
                left_regex = RegexNode::Cat(arena.alloc(left_regex), right_regex);
            }
            left_regex
        }
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<RegexNode> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, specs, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            RegexNode::Or(subregexes, false)
        }
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(subexpr, specs, arena, symbols, input_from_position);
            RegexNode::Or(vec![subregex, RegexNode::Epsilon], false)
        }
        Expr::Many1(subexpr) => {
            let subregex = arena.alloc(do_from_expr(
                subexpr,
                specs,
                arena,
                symbols,
                input_from_position,
            ));
            let star = arena.alloc(RegexNode::Star(subregex));
            RegexNode::Cat(subregex, star)
        }
        Expr::DistributiveDescription(_, _) => unreachable!(
            "DistributiveDescription Expr type should have been erased before compilation to regex"
        ),
        Expr::Fallback(subexprs) => {
            let mut subregexes: Vec<RegexNode> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, specs, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            RegexNode::Or(subregexes, true)
        }
    }
}

#[derive(Debug)]
pub struct Regex<'a> {
    pub root: RegexNode<'a>,
    pub input_symbols: Rc<IndexSet<Input>>,
    pub input_from_position: Vec<Input>,
    pub endmarker_position: Position,
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
        if let Some(..) = prev_ambiguous {
            return Err(Error::AmbiguousMatchable);
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
    had_ambiguous: bool,
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
        let is_amb = input_from_position[pos as usize].is_ambiguous(shell);
        if is_amb && had_ambiguous {
            return Err(Error::AmbiguousMatchable);
        }
        visited.insert(pos);
        return do_ensure_ambiguous_inputs_tail_only_subword(
            follow,
            followpos,
            input_from_position,
            shell,
            had_ambiguous || is_amb,
            visited,
        );
    }
    let inputs: Vec<Input> = unvisited
        .iter()
        .filter_map(|pos| input_from_position.get(pos as usize).cloned())
        .collect();

    let mut prev_ambiguous: Option<Input> = None;
    for inp in inputs {
        if had_ambiguous {
            return Err(Error::AmbiguousMatchable);
        }

        if let Some(..) = prev_ambiguous {
            return Err(Error::AmbiguousMatchable);
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
            had_ambiguous || prev_ambiguous.is_some(),
            visited,
        )?;
    }
    Ok(())
}

impl<'a> Regex<'a> {
    pub fn from_expr(e: &Expr, specs: &UstrMap<Specialization>, arena: &'a Bump) -> Result<Self> {
        let mut input_symbols: IndexSet<Input> = Default::default();
        let mut input_from_position: Vec<Input> = Default::default();
        let regex = arena.alloc(do_from_expr(
            e,
            specs,
            arena,
            &mut input_symbols,
            &mut input_from_position,
        ));
        let endmarker_position = input_from_position.len() as Position;
        let endmarker = arena.alloc(RegexNode::EndMarker(endmarker_position));
        let root = RegexNode::Cat(regex, endmarker);

        let retval = Self {
            root,
            input_symbols: Rc::new(input_symbols),
            endmarker_position,
            input_from_position,
        };
        Ok(retval)
    }

    pub fn ensure_ambiguous_inputs_tail_only(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_ensure_ambiguous_inputs_tail_only(
            &RoaringBitmap::from_iter(&self.root.firstpos()),
            &self.root.followpos(),
            &self.input_from_position,
            shell,
            &mut visited,
        )
    }

    pub fn ensure_ambiguous_inputs_tail_only_subword(&self, shell: Shell) -> Result<()> {
        let mut visited: RoaringBitmap = Default::default();
        visited.insert(self.endmarker_position);
        do_ensure_ambiguous_inputs_tail_only_subword(
            &RoaringBitmap::from_iter(&self.root.firstpos()),
            &self.root.followpos(),
            &self.input_from_position,
            shell,
            false,
            &mut visited,
        )
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        self.root.firstpos()
    }

    pub fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
        self.root.followpos()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Error, Result};
    use ustr::ustr;

    fn make_sample_star_regex(arena: &Bump) -> RegexNode {
        RegexNode::Star(arena.alloc(RegexNode::Or(
            vec![
                RegexNode::Terminal(ustr("a"), 0, 1),
                RegexNode::Terminal(ustr("b"), 0, 2),
            ],
            false,
        )))
    }

    fn make_sample_regex(arena: &Bump) -> RegexNode {
        // (a|b)*a
        RegexNode::Cat(
            arena.alloc(make_sample_star_regex(arena)),
            arena.alloc(RegexNode::Terminal(ustr("a"), 0, 3)),
        )
    }

    #[test]
    fn nullable() {
        let arena = Bump::new();
        let star_regex = make_sample_star_regex(&arena);
        assert!(star_regex.nullable());

        let regex = make_sample_regex(&arena);
        assert!(!regex.nullable());
    }

    #[test]
    fn firstpos() {
        let arena = Bump::new();
        let regex = make_sample_regex(&arena);
        assert_eq!(regex.firstpos(), BTreeSet::from([1, 2, 3]));
    }

    #[test]
    fn lastpos() {
        let arena = Bump::new();
        let regex = make_sample_regex(&arena);
        assert_eq!(regex.lastpos(), HashSet::from([3]));
    }

    fn make_followpos_regex(arena: &Bump) -> RegexNode {
        use RegexNode::*;
        // (a|b)*abb#
        RegexNode::Cat(
            arena.alloc(Cat(
                arena.alloc(Cat(
                    arena.alloc(Cat(
                        arena.alloc(make_sample_star_regex(&arena)),
                        arena.alloc(Terminal(ustr("a"), 0, 3)),
                    )),
                    arena.alloc(Terminal(ustr("b"), 0, 4)),
                )),
                arena.alloc(Terminal(ustr("b"), 0, 5)),
            )),
            arena.alloc(EndMarker(6)),
        )
    }

    #[test]
    fn followpos() {
        let arena = Bump::new();
        let regex = make_followpos_regex(&arena);
        let fp = regex.followpos();
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
        let arena = Bump::new();
        let specs = UstrMap::default();
        let regex = Regex::from_expr(&validated.expr, &specs, &arena).unwrap();
        regex.ensure_ambiguous_inputs_tail_only(Shell::Bash)?;
        Ok(validated)
    }

    #[test]
    fn detects_nontail_command_alternative() {
        assert!(matches!(
            get_validated_grammar(r#"cmd ({{{ echo foo }}} | bar);"#),
            Err(Error::AmbiguousMatchable)
        ));
    }

    #[test]
    fn detects_nontail_command_fallback() {
        assert!(matches!(
            get_validated_grammar(r#"cmd ({{{ echo foo }}} || bar);"#),
            Err(Error::AmbiguousMatchable)
        ));
    }

    #[test]
    fn detects_nontail_command_subword() {
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ git tag }}}..{{{ git tag }}};"#),
            Err(Error::AmbiguousMatchable)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd --option={{{ echo foo }}};"#),
            Ok(_)
        ));
        assert!(matches!(
            get_validated_grammar(r#"cmd {{{ echo foo }}}{{{ echo bar }}};"#),
            Err(Error::AmbiguousMatchable)
        ));

        // https://github.com/adaszko/complgen/issues/49
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>-(amd64|arm64); <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable)
        ));
        assert!(matches!(
            get_validated_grammar(
                r#"build <PLATFORM>[-(amd64|arm64)]; <PLATFORM> ::= {{{ echo foo }}};"#
            ),
            Err(Error::AmbiguousMatchable)
        ));

        // https://github.com/adaszko/complgen/issues/53
        assert!(matches!(
            get_validated_grammar(r#"cmd <DUMMY>,<DUMMY>; <DUMMY@bash> ::= {{{ echo dummy }}};"#),
            Err(Error::AmbiguousMatchable)
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
            Err(Error::AmbiguousMatchable)
        ));
    }
}
