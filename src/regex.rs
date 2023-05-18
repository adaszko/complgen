use std::collections::{HashSet, BTreeMap, BTreeSet};

use roaring::RoaringBitmap;

use crate::{grammar::Expr, nfa::Input};

pub type Position = u32;

#[derive(Clone, PartialEq)]
pub enum AugmentedRegex {
    Literal(String, Position),
    Variable(String, Position),
    Cat(Box<AugmentedRegex>, Box<AugmentedRegex>),
    Or(Vec<AugmentedRegex>),
    Star(Box<AugmentedRegex>),
    Epsilon,
    EndMarker(Position),
}


impl std::fmt::Debug for AugmentedRegex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0, position) => f.write_fmt(format_args!(r#"Literal("{}".to_string(), {})"#, arg0, position)),
            Self::Variable(arg0, position) => f.write_fmt(format_args!(r#"Variable("{}".to_string(), {})"#, arg0, position)),
            Self::Cat(left, right) => f.write_fmt(format_args!(r#"Cat(Box::new({:?}), Box::new({:?}))"#, left, right)),
            Self::Or(arg0) => f.write_fmt(format_args!(r#"Or(vec!{:?})"#, arg0)),
            Self::Star(arg0) => f.write_fmt(format_args!(r#"Star(Box::new({:?}))"#, arg0)),
            Self::EndMarker(position) => f.write_fmt(format_args!(r#"RightmostLeaf({})"#, position)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}


fn do_from_expr(e: &Expr, mut position: Position) -> (AugmentedRegex, Position) {
    match e {
        Expr::Literal(s) => (AugmentedRegex::Literal(s.to_string(), position), position + 1),
        Expr::Variable(s) => (AugmentedRegex::Variable(s.to_string(), position), position + 1),
        Expr::Sequence(subexprs) => {
            let mut left = {
                let (re, pos) = do_from_expr(&subexprs[0], position);
                position = pos;
                re
            };
            for right in &subexprs[1..] {
                let (re, pos) = do_from_expr(right, position);
                position = pos;
                left = AugmentedRegex::Cat(Box::new(left), Box::new(re))
            }
            (left, position)
        },
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<AugmentedRegex> = Default::default();
            for e in subexprs {
                let (subregex, pos) = do_from_expr(e, position);
                subregexes.push(subregex);
                position = pos;
            }
            (AugmentedRegex::Or(subregexes), position)
        },
        Expr::Optional(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (AugmentedRegex::Or(vec![subregex, AugmentedRegex::Epsilon]), position)
        }
        Expr::Many1(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (AugmentedRegex::Cat(Box::new(subregex.clone()), Box::new(AugmentedRegex::Star(Box::new(subregex)))), position)
        },
    }
}


fn do_firstpos(re: &AugmentedRegex, result: &mut BTreeSet<Position>) {
    match re {
        AugmentedRegex::Epsilon => {},
        AugmentedRegex::Literal(_, position) => { result.insert(*position); },
        AugmentedRegex::Variable(_, position) => { result.insert(*position); },
        AugmentedRegex::Or(subregexes) => {
            for subre in subregexes {
                do_firstpos(subre, result);
            }
        },
        AugmentedRegex::Cat(left, right) => {
            if left.nullable() {
                do_firstpos(left, result);
                do_firstpos(right, result);
            }
            else {
                do_firstpos(left, result);
            }
        },
        AugmentedRegex::Star(subregex) => { do_firstpos(subregex, result); },
        AugmentedRegex::EndMarker(position) => { result.insert(*position); },
    }
}


fn do_lastpos(re: &AugmentedRegex, result: &mut HashSet<u32>) {
    match re {
        AugmentedRegex::Epsilon => {},
        AugmentedRegex::Literal(_, position) => { result.insert(*position); },
        AugmentedRegex::Variable(_, position) => { result.insert(*position); },
        AugmentedRegex::Or(subregexes) => {
            for subre in subregexes {
                do_lastpos(subre, result);
            }
        },
        AugmentedRegex::Cat(left, right) => {
            if right.nullable() {
                do_lastpos(right, result);
                do_lastpos(left, result);
            }
            else {
                do_lastpos(right, result);
            }
        },
        AugmentedRegex::Star(subregex) => { do_lastpos(subregex, result); },
        AugmentedRegex::EndMarker(position) => { result.insert(*position); },
    }
}


fn do_followpos(re: &AugmentedRegex, result: &mut BTreeMap<Position, RoaringBitmap>) {
    match re {
        AugmentedRegex::Epsilon => {},
        AugmentedRegex::Literal(_, _) => {},
        AugmentedRegex::Variable(_, _) => {},
        AugmentedRegex::Or(subregexes) => {
            for subre in subregexes {
                do_followpos(subre, result);
            }
        },
        AugmentedRegex::Cat(left, right) => {
            do_followpos(left, result);
            do_followpos(right, result);
            let fp = right.firstpos();
            for i in left.lastpos() {
                for j in &fp {
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        AugmentedRegex::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        AugmentedRegex::EndMarker(_) => {},
    }
}


fn do_get_input_symbols(regex: &AugmentedRegex, output: &mut HashSet<Input>) {
    match regex {
        AugmentedRegex::Epsilon => {},
        AugmentedRegex::Literal(s, _) => { output.insert(Input::Literal(s.to_string())); },
        AugmentedRegex::Variable(_, _) => { output.insert(Input::Any); },
        AugmentedRegex::Cat(left, right) => {
            do_get_input_symbols(&left, output);
            do_get_input_symbols(&right, output);
        },
        AugmentedRegex::Or(subregexes) => {
            for re in subregexes {
                do_get_input_symbols(&re, output);
            }
        },
        AugmentedRegex::Star(re) => {
            do_get_input_symbols(&re, output);
        },
        AugmentedRegex::EndMarker(_) => {},
    }
}


impl AugmentedRegex {
    pub fn from_expr(e: &Expr) -> Self {
        let (regex, _) = do_from_expr(e, 0);
        regex
    }

    pub fn get_rightmost_leaf_position(&self) -> Option<Position> {
        match self {
            AugmentedRegex::Epsilon => None,
            AugmentedRegex::Literal(_, _) => None,
            AugmentedRegex::Variable(_, _) => None,
            AugmentedRegex::Cat(_, right) => right.get_rightmost_leaf_position(),
            AugmentedRegex::Or(v) => v.last().and_then(|re| re.get_rightmost_leaf_position()),
            AugmentedRegex::Star(re) => re.get_rightmost_leaf_position(),
            AugmentedRegex::EndMarker(position) => Some(*position),
        }
    }

    // TODO This has to be precomputed in Self::from_expr() for efficiency.
    pub fn get_input_symbols(&self) -> HashSet<Input> {
        let mut output: HashSet<Input> = Default::default();
        do_get_input_symbols(self, &mut output);
        output
    }

    pub fn get_position_input(&self, position: u32) -> Input {
        // TODO This has to be precomputed in Self::from_expr() for efficiency.
        todo!();
    }

    pub fn get_endmarker_position(&self) -> Position {
        // TODO This has to be precomputed in Self::from_expr() for efficiency.
        todo!();
    }

    fn nullable(&self) -> bool {
        match self {
            AugmentedRegex::Epsilon => true,
            AugmentedRegex::Literal(_, _) => false,
            AugmentedRegex::Variable(_, _) => false,
            AugmentedRegex::Or(children) => children.iter().any(|child| child.nullable()),
            AugmentedRegex::Cat(left, right) => left.nullable() && right.nullable(),
            AugmentedRegex::Star(_) => true,
            AugmentedRegex::EndMarker(_) => true,
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
        let mut result: BTreeMap<u32, RoaringBitmap> = Default::default();
        do_followpos(self, &mut result);
        result
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn make_sample_star_regex() -> AugmentedRegex {
        AugmentedRegex::Star(Box::new(AugmentedRegex::Or(vec![AugmentedRegex::Literal("a".to_string(), 1), AugmentedRegex::Literal("b".to_string(), 2),])))
    }

    fn make_sample_regex() -> AugmentedRegex {
        // (a|b)*a
        AugmentedRegex::Cat(
            Box::new(make_sample_star_regex()),
            Box::new(AugmentedRegex::Literal("a".to_string(), 3)),
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
        assert_eq!(regex.firstpos(), BTreeSet::from([1,2,3]));
    }

    #[test]
    fn lastpos() {
        let regex = make_sample_regex();
        assert_eq!(regex.lastpos(), HashSet::from([3]));
    }

    fn make_followpos_regex() -> AugmentedRegex {
        // (a|b)*abb#
        AugmentedRegex::Cat(
            Box::new(AugmentedRegex::Cat(
                Box::new(AugmentedRegex::Cat(
                    Box::new(AugmentedRegex::Cat(
                        Box::new(make_sample_star_regex()),
                        Box::new(AugmentedRegex::Literal("a".to_string(), 3)),
                    )),
                    Box::new(AugmentedRegex::Literal("b".to_string(), 4)),
                )),
                Box::new(AugmentedRegex::Literal("b".to_string(), 5)),
            )),
            Box::new(AugmentedRegex::EndMarker(6)),
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
