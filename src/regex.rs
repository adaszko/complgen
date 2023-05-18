use std::collections::{HashSet, BTreeMap, BTreeSet, HashMap};

use roaring::RoaringBitmap;

use crate::{grammar::Expr, nfa::Input};

pub type Position = u32;

#[derive(Clone, PartialEq)]
pub enum AugmentedRegexNode {
    Literal(String, Position),
    Variable(String, Position),
    Cat(Box<AugmentedRegexNode>, Box<AugmentedRegexNode>),
    Or(Vec<AugmentedRegexNode>),
    Star(Box<AugmentedRegexNode>),
    Epsilon,
    EndMarker(Position),
}


impl std::fmt::Debug for AugmentedRegexNode {
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


fn do_firstpos(re: &AugmentedRegexNode, result: &mut BTreeSet<Position>) {
    match re {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Literal(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Variable(_, position) => { result.insert(*position); },
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


fn do_lastpos(re: &AugmentedRegexNode, result: &mut HashSet<u32>) {
    match re {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Literal(_, position) => { result.insert(*position); },
        AugmentedRegexNode::Variable(_, position) => { result.insert(*position); },
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
        AugmentedRegexNode::Literal(_, _) => {},
        AugmentedRegexNode::Variable(_, _) => {},
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
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        AugmentedRegexNode::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.entry(i).or_default().insert(u32::try_from(*j).unwrap());
                }
            }
        },
        AugmentedRegexNode::EndMarker(_) => {},
    }
}


fn do_get_input_symbols(regex: &AugmentedRegexNode, output: &mut HashSet<Input>) {
    match regex {
        AugmentedRegexNode::Epsilon => {},
        AugmentedRegexNode::Literal(s, _) => { output.insert(Input::Literal(s.to_string())); },
        AugmentedRegexNode::Variable(_, _) => { output.insert(Input::Any); },
        AugmentedRegexNode::Cat(left, right) => {
            do_get_input_symbols(&left, output);
            do_get_input_symbols(&right, output);
        },
        AugmentedRegexNode::Or(subregexes) => {
            for re in subregexes {
                do_get_input_symbols(&re, output);
            }
        },
        AugmentedRegexNode::Star(re) => {
            do_get_input_symbols(&re, output);
        },
        AugmentedRegexNode::EndMarker(_) => {},
    }
}


impl AugmentedRegexNode {
    pub fn get_rightmost_leaf_position(&self) -> Option<Position> {
        match self {
            AugmentedRegexNode::Epsilon => None,
            AugmentedRegexNode::Literal(_, _) => None,
            AugmentedRegexNode::Variable(_, _) => None,
            AugmentedRegexNode::Cat(_, right) => right.get_rightmost_leaf_position(),
            AugmentedRegexNode::Or(v) => v.last().and_then(|re| re.get_rightmost_leaf_position()),
            AugmentedRegexNode::Star(re) => re.get_rightmost_leaf_position(),
            AugmentedRegexNode::EndMarker(position) => Some(*position),
        }
    }

    fn nullable(&self) -> bool {
        match self {
            AugmentedRegexNode::Epsilon => true,
            AugmentedRegexNode::Literal(_, _) => false,
            AugmentedRegexNode::Variable(_, _) => false,
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


fn do_from_expr(e: &Expr, mut position: Position) -> (AugmentedRegexNode, Position) {
    match e {
        Expr::Literal(s) => (AugmentedRegexNode::Literal(s.to_string(), position), position + 1),
        Expr::Variable(s) => (AugmentedRegexNode::Variable(s.to_string(), position), position + 1),
        Expr::Sequence(subexprs) => {
            let mut left = {
                let (re, pos) = do_from_expr(&subexprs[0], position);
                position = pos;
                re
            };
            for right in &subexprs[1..] {
                let (re, pos) = do_from_expr(right, position);
                position = pos;
                left = AugmentedRegexNode::Cat(Box::new(left), Box::new(re))
            }
            (left, position)
        },
        Expr::Alternative(subexprs) => {
            let mut subregexes: Vec<AugmentedRegexNode> = Default::default();
            for e in subexprs {
                let (subregex, pos) = do_from_expr(e, position);
                subregexes.push(subregex);
                position = pos;
            }
            (AugmentedRegexNode::Or(subregexes), position)
        },
        Expr::Optional(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (AugmentedRegexNode::Or(vec![subregex, AugmentedRegexNode::Epsilon]), position)
        }
        Expr::Many1(subexpr) => {
            let (subregex, position) = do_from_expr(subexpr, position);
            (AugmentedRegexNode::Cat(Box::new(subregex.clone()), Box::new(AugmentedRegexNode::Star(Box::new(subregex)))), position)
        },
    }
}


pub struct AugmentedRegex {
    root: AugmentedRegexNode,
    input_symbols: HashSet<Input>,
    input_from_position: HashMap<Position, Input>,
    endmarker_position: Position,
}


impl AugmentedRegex {
    pub fn from_expr(e: &Expr) -> Self {
        let (regex, _) = do_from_expr(e, 0);
        Self {
            root: regex,
            input_symbols: todo!(),
            input_from_position: todo!(),
            endmarker_position: todo!(),
        }
    }

    pub fn firstpos(&self) -> BTreeSet<Position> {
        self.root.firstpos()
    }

    fn lastpos(&self) -> HashSet<Position> {
        self.root.lastpos()
    }

    pub fn followpos(&self) -> BTreeMap<Position, RoaringBitmap> {
        self.root.followpos()
    }

    // TODO This has to be precomputed in Self::from_expr() for efficiency.
    pub fn get_input_symbols(&self) -> HashSet<Input> {
        let mut output: HashSet<Input> = Default::default();
        do_get_input_symbols(&self.root, &mut output);
        output
    }

    pub fn get_input_from_position(&self, position: u32) -> Option<Input> {
        self.input_from_position.get(&position).map(|input| input.clone())
    }

    pub fn get_endmarker_position(&self) -> Position {
        self.endmarker_position
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn make_sample_star_regex() -> AugmentedRegexNode {
        AugmentedRegexNode::Star(Box::new(AugmentedRegexNode::Or(vec![AugmentedRegexNode::Literal("a".to_string(), 1), AugmentedRegexNode::Literal("b".to_string(), 2),])))
    }

    fn make_sample_regex() -> AugmentedRegexNode {
        // (a|b)*a
        AugmentedRegexNode::Cat(
            Box::new(make_sample_star_regex()),
            Box::new(AugmentedRegexNode::Literal("a".to_string(), 3)),
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

    fn make_followpos_regex() -> AugmentedRegexNode {
        // (a|b)*abb#
        AugmentedRegexNode::Cat(
            Box::new(AugmentedRegexNode::Cat(
                Box::new(AugmentedRegexNode::Cat(
                    Box::new(AugmentedRegexNode::Cat(
                        Box::new(make_sample_star_regex()),
                        Box::new(AugmentedRegexNode::Literal("a".to_string(), 3)),
                    )),
                    Box::new(AugmentedRegexNode::Literal("b".to_string(), 4)),
                )),
                Box::new(AugmentedRegexNode::Literal("b".to_string(), 5)),
            )),
            Box::new(AugmentedRegexNode::EndMarker(6)),
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
