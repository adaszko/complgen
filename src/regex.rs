use hashbrown::HashSet;
use std::rc::Rc;
use std::collections::{BTreeMap, BTreeSet};

use bumpalo::Bump;
use ustr::{Ustr, UstrMap};
use roaring::RoaringBitmap;

use crate::grammar::{Expr, Specialization, SubwordCompilationPhase, DFARef};

pub type Position = u32;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Literal(Ustr, Option<Ustr>),
    Subword(DFARef, Option<Ustr>),
    Nonterminal(Ustr, Option<Specialization>),
    Command(Ustr),
}


impl Input {
    pub fn matches_anything(&self) -> bool {
        match self {
            Self::Literal(..) => false,
            Self::Subword(..) => false,
            Self::Nonterminal(..) => true,
            Self::Command(..) => true,
        }
    }
}


impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(subword, _) => write!(f, r#"{subword:?}"#),
            Self::Literal(literal, _) => write!(f, r#"{literal}"#),
            Self::Nonterminal(nonterminal, _) => write!(f, r#"{nonterminal}"#),
            Self::Command(command) => write!(f, r#"{command}"#),
        }
    }
}


#[derive(Clone, PartialEq)]
pub enum AugmentedRegexNode<'a> {
    Epsilon,
    Subword(Position),
    Terminal(Ustr, Position),
    Nonterminal(Position),
    Command(Ustr, Position),
    Cat(&'a AugmentedRegexNode<'a>, &'a AugmentedRegexNode<'a>),
    Or(Vec<AugmentedRegexNode<'a>>),
    Star(&'a AugmentedRegexNode<'a>),
    EndMarker(Position),
}


impl<'a> std::fmt::Debug for AugmentedRegexNode<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword(position) => f.write_fmt(format_args!(r#"Subword({position})"#)),
            Self::Terminal(term, position) => f.write_fmt(format_args!(r#"Terminal({:?}.to_string(), {position})"#, term)),
            Self::Nonterminal(position) => f.write_fmt(format_args!(r#"Nonterminal({})"#, position)),
            Self::Command(code, position) => f.write_fmt(format_args!(r#"Command({:?}.to_string(), {})"#, code, position)),
            Self::Cat(left, right) => f.write_fmt(format_args!(r#"Cat({:?}, {:?})"#, left, right)),
            Self::Or(arg0) => f.write_fmt(format_args!(r#"Or(vec!{:?})"#, arg0)),
            Self::Star(arg0) => f.write_fmt(format_args!(r#"Star({:?})"#, arg0)),
            Self::EndMarker(position) => f.write_fmt(format_args!(r#"EndMarker({})"#, position)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}


fn do_firstpos(re: &AugmentedRegexNode, result: &mut BTreeSet<Position>) {
    match re {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Subword(position) => { result.insert(*position); },
        AugmentedRegexNode::Terminal(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Nonterminal(position) => { result.insert(*position); },
        AugmentedRegexNode::Command(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Or(subregexes) => {
            for subre in subregexes {
                do_firstpos(subre, result);
            }
        },
        AugmentedRegexNode::Cat(left, right) => {
            if left.nullable() {
                do_firstpos(left, result);
                do_firstpos(right, result);
            }
            else {
                do_firstpos(left, result);
            }
        },
        AugmentedRegexNode::Star(subregex) => { do_firstpos(subregex, result); },
        AugmentedRegexNode::EndMarker(position) => { result.insert(*position); },
    }
}


fn do_lastpos(re: &AugmentedRegexNode, result: &mut HashSet<Position>) {
    match re {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Subword(position) => { result.insert(*position); },
        AugmentedRegexNode::Terminal(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Nonterminal(position) => { result.insert(*position); },
        AugmentedRegexNode::Command(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Or(subregexes) => {
            for subre in subregexes {
                do_lastpos(subre, result);
            }
        },
        AugmentedRegexNode::Cat(left, right) => {
            if right.nullable() {
                do_lastpos(right, result);
                do_lastpos(left, result);
            }
            else {
                do_lastpos(right, result);
            }
        },
        AugmentedRegexNode::Star(subregex) => { do_lastpos(subregex, result); },
        AugmentedRegexNode::EndMarker(position) => { result.insert(*position); },
    }
}


fn do_followpos(re: &AugmentedRegexNode, result: &mut BTreeMap<Position, RoaringBitmap>) {
    match re {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Subword(..) => {},
        AugmentedRegexNode::Terminal(..) => {},
        AugmentedRegexNode::Nonterminal(..) => {},
        AugmentedRegexNode::Command(..) => {},
        AugmentedRegexNode::Or(subregexes) => {
            for subre in subregexes {
                do_followpos(subre, result);
            }
        },
        AugmentedRegexNode::Cat(left, right) => {
            do_followpos(left, result);
            do_followpos(right, result);
            let fp = right.firstpos();
            for i in left.lastpos() {
                for j in &fp {
                    result.entry(i).or_default().insert(*j);
                }
            }
        },
        AugmentedRegexNode::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(*j);
                }
            }
        },
        AugmentedRegexNode::EndMarker(_) => {},
    }
}


impl<'a> AugmentedRegexNode<'a> {
    fn nullable(&self) -> bool {
        match self {
            AugmentedRegexNode::Epsilon => true,
            AugmentedRegexNode::Subword(..) => false,
            AugmentedRegexNode::Terminal(..) => false,
            AugmentedRegexNode::Nonterminal(..) => false,
            AugmentedRegexNode::Command(..) => false,
            AugmentedRegexNode::Or(children) => children.iter().any(|child| child.nullable()),
            AugmentedRegexNode::Cat(left, right) => left.nullable() && right.nullable(),
            AugmentedRegexNode::Star(_) => true,
            AugmentedRegexNode::EndMarker(_) => true,
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


fn do_from_expr<'a>(e: &Expr, specs: &UstrMap<Specialization>, arena: &'a Bump, symbols: &mut HashSet<Input>, input_from_position: &mut Vec<Input>) -> AugmentedRegexNode<'a> {
    match e {
        Expr::Terminal(term, description) => {
            let result = AugmentedRegexNode::Terminal(*term, Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Literal(*term, *description);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Subword(subword, descr) => {
            let result = AugmentedRegexNode::Subword(Position::try_from(input_from_position.len()).unwrap());
            let dfa = match subword {
                SubwordCompilationPhase::DFA(dfa) => dfa,
                SubwordCompilationPhase::Expr(_) => unreachable!(),
            };
            let input = Input::Subword(dfa.clone(), *descr);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Nonterminal(name) => {
            let result = AugmentedRegexNode::Nonterminal(Position::try_from(input_from_position.len()).unwrap());
            let specialization = specs.get(name);
            let input = Input::Nonterminal(*name, specialization.copied());
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Command(code) => {
            let result = AugmentedRegexNode::Command(*code, Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Command(*code);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Sequence(subexprs) => {
            let mut left_regex = do_from_expr(&subexprs[0], specs, arena, symbols, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex = arena.alloc(do_from_expr(right_expr, specs, arena, symbols, input_from_position));
                left_regex = AugmentedRegexNode::Cat(arena.alloc(left_regex), right_regex);
            }
            left_regex
        },
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<AugmentedRegexNode> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, specs, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            AugmentedRegexNode::Or(subregexes)
        },
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(subexpr, specs, arena, symbols, input_from_position);
            AugmentedRegexNode::Or(vec![subregex, AugmentedRegexNode::Epsilon])
        }
        Expr::Many1(subexpr) => {
            let subregex = arena.alloc(do_from_expr(subexpr, specs, arena, symbols, input_from_position));
            let star = arena.alloc(AugmentedRegexNode::Star(subregex));
            AugmentedRegexNode::Cat(subregex, star)
        },
    }
}


#[derive(Debug)]
pub struct AugmentedRegex<'a> {
    pub root: AugmentedRegexNode<'a>,
    pub input_symbols: Rc<HashSet<Input>>,
    pub input_from_position: Vec<Input>,
    pub endmarker_position: Position,
}


impl<'a> AugmentedRegex<'a> {
    pub fn from_expr(e: &Expr, specs: &UstrMap<Specialization>, arena: &'a Bump) -> Self {
        let mut input_symbols: HashSet<Input> = Default::default();
        let mut input_from_position: Vec<Input> = Default::default();
        let regex = arena.alloc(do_from_expr(e, specs, arena, &mut input_symbols, &mut input_from_position));
        let endmarker_position = Position::try_from(input_from_position.len()).unwrap();
        let endmarker = arena.alloc(AugmentedRegexNode::EndMarker(endmarker_position));
        let root = AugmentedRegexNode::Cat(regex, endmarker);
        Self {
            root,
            input_symbols: Rc::new(input_symbols),
            endmarker_position,
            input_from_position,
        }
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
    use ustr::ustr;

    fn make_sample_star_regex(arena: &Bump) -> AugmentedRegexNode {
        AugmentedRegexNode::Star(arena.alloc(AugmentedRegexNode::Or(vec![AugmentedRegexNode::Terminal(ustr("a"), 1), AugmentedRegexNode::Terminal(ustr("b"), 2),])))
    }

    fn make_sample_regex(arena: &Bump) -> AugmentedRegexNode {
        // (a|b)*a
        AugmentedRegexNode::Cat(
            arena.alloc(make_sample_star_regex(arena)),
            arena.alloc(AugmentedRegexNode::Terminal(ustr("a"), 3)),
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
        assert_eq!(regex.firstpos(), BTreeSet::from([1,2,3]));
    }

    #[test]
    fn lastpos() {
        let arena = Bump::new();
        let regex = make_sample_regex(&arena);
        assert_eq!(regex.lastpos(), HashSet::from([3]));
    }

    fn make_followpos_regex(arena: &Bump) -> AugmentedRegexNode {
        // (a|b)*abb#
        AugmentedRegexNode::Cat(
            arena.alloc(AugmentedRegexNode::Cat(
                arena.alloc(AugmentedRegexNode::Cat(
                    arena.alloc(AugmentedRegexNode::Cat(
                        arena.alloc(make_sample_star_regex(&arena)),
                        arena.alloc(AugmentedRegexNode::Terminal(ustr("a"), 3)),
                    )),
                    arena.alloc(AugmentedRegexNode::Terminal(ustr("b"), 4)),
                )),
                arena.alloc(AugmentedRegexNode::Terminal(ustr("b"), 5)),
            )),
            arena.alloc(AugmentedRegexNode::EndMarker(6)),
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
        assert_eq!(fp.get(&2), Some(&RoaringBitmap::from_iter([1,2,3])));
        assert_eq!(fp.get(&1), Some(&RoaringBitmap::from_iter([1,2,3])));
    }
}
