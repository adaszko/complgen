use std::collections::{HashSet, BTreeMap};

use itertools::Itertools;
use roaring::RoaringBitmap;

use crate::grammar::Expr;

#[derive(Clone, PartialEq)]
pub enum Regex {
    Literal(String, usize),
    Variable(String, usize),
    Cat(Box<Regex>, Box<Regex>),
    Or(Vec<Regex>),
    Star(Box<Regex>),
    Epsilon,
    RightmostLeaf(usize),
}


impl std::fmt::Debug for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0, position) => f.write_fmt(format_args!(r#"Literal("{}".to_string(), {})"#, arg0, position)),
            Self::Variable(arg0, position) => f.write_fmt(format_args!(r#"Variable("{}".to_string(), {})"#, arg0, position)),
            Self::Cat(left, right) => f.write_fmt(format_args!(r#"Cat(Box::new({:?}), Box::new({:?}))"#, left, right)),
            Self::Or(arg0) => f.write_fmt(format_args!(r#"Or(vec!{:?})"#, arg0)),
            Self::Star(arg0) => f.write_fmt(format_args!(r#"Star(Box::new({:?}))"#, arg0)),
            Self::RightmostLeaf(position) => f.write_fmt(format_args!(r#"RightmostLeaf({})"#, position)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}


fn do_from_expr(e: &Expr, mut position: usize) -> (Regex, usize) {
    match e {
        Expr::Literal(s) => (Regex::Literal(s.to_string(), position), position + 1),
        Expr::Variable(s) => (Regex::Variable(s.to_string(), position), position + 1),
        Expr::Sequence(subexprs) => {
            let mut left = {
                let (re, pos) = do_from_expr(&subexprs[0], position);
                position = pos;
                re
            };
            for right in &subexprs[1..] {
                let (re, pos) = do_from_expr(right, position);
                position = pos;
                left = Regex::Cat(Box::new(left), Box::new(re))
            }
            (left, position)
        },
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<Regex> = Default::default();
            for e in subexprs {
                let (subregex, pos) = do_from_expr(e, position);
                subregexes.push(subregex);
                position = pos;
            }
            (Regex::Or(subregexes), position)
        },
        Expr::Optional(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (Regex::Or(vec![subregex, Regex::Epsilon]), position)
        }
        Expr::Many1(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (Regex::Cat(Box::new(subregex.clone()), Box::new(Regex::Star(Box::new(subregex)))), position)
        },
    }
}


fn do_firstpos(re: &Regex, result: &mut HashSet<usize>) {
    match re {
        Regex::Epsilon => {},
        Regex::Literal(_, position) => { result.insert(*position); },
        Regex::Variable(_, position) => { result.insert(*position); },
        Regex::Or(subregexes) => {
            for subre in subregexes {
                do_firstpos(subre, result);
            }
        },
        Regex::Cat(left, right) => {
            if left.nullable() {
                do_firstpos(left, result);
                do_firstpos(right, result);
            }
            else {
                do_firstpos(left, result);
            }
        },
        Regex::Star(subregex) => { do_firstpos(subregex, result); },
        Regex::RightmostLeaf(position) => { result.insert(*position); },
    }
}


fn do_lastpos(re: &Regex, result: &mut HashSet<usize>) {
    match re {
        Regex::Epsilon => {},
        Regex::Literal(_, position) => { result.insert(*position); },
        Regex::Variable(_, position) => { result.insert(*position); },
        Regex::Or(subregexes) => {
            for subre in subregexes {
                do_lastpos(subre, result);
            }
        },
        Regex::Cat(left, right) => {
            if right.nullable() {
                do_lastpos(right, result);
                do_lastpos(left, result);
            }
            else {
                do_lastpos(right, result);
            }
        },
        Regex::Star(subregex) => { do_lastpos(subregex, result); },
        Regex::RightmostLeaf(position) => { result.insert(*position); },
    }
}


fn do_followpos(re: &Regex, result: &mut BTreeMap<usize, RoaringBitmap>) {
    match re {
        Regex::Epsilon => {},
        Regex::Literal(_, _) => {},
        Regex::Variable(_, _) => {},
        Regex::Or(subregexes) => {
            for subre in subregexes {
                do_followpos(subre, result);
            }
        },
        Regex::Cat(left, right) => {
            do_followpos(left, result);
            do_followpos(right, result);
            let fp = right.firstpos();
            for i in left.lastpos() {
                for j in &fp {
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        Regex::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        Regex::RightmostLeaf(_) => {},
    }
}


impl Regex {
    fn from_expr(e: &Expr) -> Self {
        let (regex, _) = do_from_expr(e, 0);
        regex
    }

    fn nullable(&self) -> bool {
        match self {
            Regex::Epsilon => true,
            Regex::Literal(_, _) => false,
            Regex::Variable(_, _) => false,
            Regex::Or(children) => children.iter().any(|child| child.nullable()),
            Regex::Cat(left, right) => left.nullable() && right.nullable(),
            Regex::Star(_) => true,
            Regex::RightmostLeaf(_) => true,
        }
    }

    fn firstpos(&self) -> HashSet<usize> {
        let mut result: HashSet<usize> = Default::default();
        do_firstpos(self, &mut result);
        result
    }

    fn lastpos(&self) -> HashSet<usize> {
        let mut result: HashSet<usize> = Default::default();
        do_lastpos(self, &mut result);
        result
    }

    fn followpos(&self) -> BTreeMap<usize, RoaringBitmap> {
        let mut result: BTreeMap<usize, RoaringBitmap> = Default::default();
        do_followpos(self, &mut result);
        result
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn make_sample_star_regex() -> Regex {
        Regex::Star(Box::new(Regex::Or(vec![Regex::Literal("a".to_string(), 1), Regex::Literal("b".to_string(), 2),])))
    }

    fn make_sample_regex() -> Regex {
        // (a|b)*a
        Regex::Cat(
            Box::new(make_sample_star_regex()),
            Box::new(Regex::Literal("a".to_string(), 3)),
        )
    }

    #[test]
    fn nullable() {
        let star_regex = make_sample_star_regex();
        assert!(star_regex.nullable());

        let regex = make_sample_regex();
        assert!(!regex.nullable());
    }

    #[test]
    fn firstpos() {
        let regex = make_sample_regex();
        assert_eq!(regex.firstpos(), HashSet::from([1,2,3]));
    }

    #[test]
    fn lastpos() {
        let regex = make_sample_regex();
        assert_eq!(regex.lastpos(), HashSet::from([3]));
    }

    fn make_followpos_regex() -> Regex {
        // (a|b)*abb#
        Regex::Cat(
            Box::new(Regex::Cat(
                Box::new(Regex::Cat(
                    Box::new(Regex::Cat(
                        Box::new(make_sample_star_regex()),
                        Box::new(Regex::Literal("a".to_string(), 3)),
                    )),
                    Box::new(Regex::Literal("b".to_string(), 4)),
                )),
                Box::new(Regex::Literal("b".to_string(), 5)),
            )),
            Box::new(Regex::RightmostLeaf(6)),
        )
    }

    #[test]
    fn followpos() {
        let regex = make_followpos_regex();
        let fp = regex.followpos();
        assert_eq!(fp.get(&6), None);
        assert_eq!(fp.get(&5), Some(&RoaringBitmap::from_iter([6])));
        assert_eq!(fp.get(&4), Some(&RoaringBitmap::from_iter([5])));
        assert_eq!(fp.get(&3), Some(&RoaringBitmap::from_iter([4])));
        assert_eq!(fp.get(&2), Some(&RoaringBitmap::from_iter([1,2,3])));
        assert_eq!(fp.get(&1), Some(&RoaringBitmap::from_iter([1,2,3])));
    }
}
