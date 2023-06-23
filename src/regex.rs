use hashbrown::HashSet;
use std::rc::Rc;
use std::collections::{BTreeMap, BTreeSet};

use bumpalo::Bump;
use ustr::Ustr;
use roaring::RoaringBitmap;

use crate::grammar::Expr;

pub type Position = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AnyInput {
    Any,
    Command(Ustr),
}

impl std::fmt::Display for AnyInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyInput::Any => write!(f, "*"),
            AnyInput::Command(cmd) => write!(f, "{{{}}}", cmd),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Input {
    Literal(Ustr),
    Any(AnyInput),
}


impl Input {
    pub fn matches_anything(&self) -> bool {
        match self {
            Input::Literal(_) => false,
            Input::Any(_) => true,
        }
    }
}


impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Literal(s) => write!(f, "{}", s),
            Input::Any(any) => write!(f, "{}", any),
        }
    }
}


#[derive(Clone, PartialEq)]
pub enum AugmentedRegexNode<'a> {
    Epsilon,
    Terminal(Ustr, Option<Ustr>, Position),
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
            Self::Terminal(term, descr, position) => f.write_fmt(format_args!(r#"Terminal({:?}.to_string(), {:?}, {position})"#, term, descr)),
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
        AugmentedRegexNode::Terminal(_, _, position) => { result.insert(*position); },
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
        AugmentedRegexNode::Terminal(_, _, position) => { result.insert(*position); },
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


fn do_from_expr<'a>(e: &Expr, arena: &'a Bump, symbols: &mut HashSet<Input>, input_from_position: &mut Vec<Input>) -> AugmentedRegexNode<'a> {
    match e {
        Expr::Terminal(term, descr) => {
            let result = AugmentedRegexNode::Terminal(*term, *descr, Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Literal(*term);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Nonterminal(_) => {
            let result = AugmentedRegexNode::Nonterminal(Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Any(AnyInput::Any);
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Command(code) => {
            let result = AugmentedRegexNode::Command(*code, Position::try_from(input_from_position.len()).unwrap());
            let input = Input::Any(AnyInput::Command(*code));
            input_from_position.push(input.clone());
            symbols.insert(input);
            result
        },
        Expr::Sequence(subexprs) => {
            let mut left_regex = do_from_expr(&subexprs[0], arena, symbols, input_from_position);
            for right_expr in &subexprs[1..] {
                let right_regex = arena.alloc(do_from_expr(right_expr, arena, symbols, input_from_position));
                left_regex = AugmentedRegexNode::Cat(arena.alloc(left_regex), right_regex);
            }
            left_regex
        },
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<AugmentedRegexNode> = Default::default();
            for e in subexprs {
                let subregex = do_from_expr(e, arena, symbols, input_from_position);
                subregexes.push(subregex);
            }
            AugmentedRegexNode::Or(subregexes)
        },
        Expr::Optional(subexpr) => {
            let subregex = do_from_expr(subexpr, arena, symbols, input_from_position);
            AugmentedRegexNode::Or(vec![subregex, AugmentedRegexNode::Epsilon])
        }
        Expr::Many1(subexpr) => {
            let subregex = arena.alloc(do_from_expr(subexpr, arena, symbols, input_from_position));
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
    pub fn from_expr(e: &Expr, arena: &'a Bump) -> Self {
        let mut input_symbols: HashSet<Input> = Default::default();
        let mut input_from_position: Vec<Input> = Default::default();
        let regex = arena.alloc(do_from_expr(e, arena, &mut input_symbols, &mut input_from_position));
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
        AugmentedRegexNode::Star(arena.alloc(AugmentedRegexNode::Or(vec![AugmentedRegexNode::Terminal(ustr("a"), None, 1), AugmentedRegexNode::Terminal(ustr("b"), None, 2),])))
    }

    fn make_sample_regex(arena: &Bump) -> AugmentedRegexNode {
        // (a|b)*a
        AugmentedRegexNode::Cat(
            arena.alloc(make_sample_star_regex(arena)),
            arena.alloc(AugmentedRegexNode::Terminal(ustr("a"), None, 3)),
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
                        arena.alloc(AugmentedRegexNode::Terminal(ustr("a"), None, 3)),
                    )),
                    arena.alloc(AugmentedRegexNode::Terminal(ustr("b"), None, 4)),
                )),
                arena.alloc(AugmentedRegexNode::Terminal(ustr("b"), None, 5)),
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
