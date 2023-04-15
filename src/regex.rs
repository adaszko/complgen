use std::collections::{HashSet, BTreeSet};

use crate::grammar::Expr;

#[derive(Clone, PartialEq)]
pub enum Regex {
    Literal(String, usize),
    Variable(String, usize),
    Cat(Vec<Regex>),
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
            Self::Cat(arg0) => f.write_fmt(format_args!(r#"Cat(vec!{:?})"#, arg0)),
            Self::Or(arg0) => f.write_fmt(format_args!(r#"Or(vec!{:?})"#, arg0)),
            Self::Star(arg0) => f.write_fmt(format_args!(r#"Star(Box::new({:?}))"#, arg0)),
            Self::RightmostLeaf(position) => f.write_fmt(format_args!(r#"Accept({})"#, position)),
            Self::Epsilon => f.write_fmt(format_args!(r#"Epsilon"#)),
        }
    }
}


fn do_from_expr(e: &Expr, mut position: usize) -> (Regex, usize) {
    match e {
        Expr::Literal(s) => (Regex::Literal(s.to_string(), position), position + 1),
        Expr::Variable(s) => (Regex::Variable(s.to_string(), position), position + 1),
        Expr::Sequence(subexprs) => {
            let mut subregexes: Vec<Regex> = Default::default();
            for e in subexprs {
                let (subregex, pos) = do_from_expr(e, position);
                subregexes.push(subregex);
                position = pos;
            }
            (Regex::Cat(subregexes), position)
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
            (Regex::Cat(vec![subregex.clone(), Regex::Star(Box::new(subregex))]), position)
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
        Regex::Cat(subregexes) => {
            for subre in subregexes {
                do_firstpos(subre, result);
                if !subre.nullable() {
                    break;
                }
            }
        },
        Regex::Star(subregex) => { do_firstpos(subregex, result); },
        Regex::RightmostLeaf(_) => {},
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
        Regex::Cat(subregexes) => {
            for subre in subregexes.iter().rev() {
                do_lastpos(subre, result);
                if !subre.nullable() {
                    break;
                }
            }
        },
        Regex::Star(subregex) => { do_lastpos(subregex, result); },
        Regex::RightmostLeaf(_) => {},
    }
}


fn do_followpos(re: &Regex, result: &mut BTreeSet<(usize, usize)>) {
    match re {
        Regex::Epsilon => {},
        Regex::Literal(_, _) => {},
        Regex::Variable(_, _) => {},
        Regex::Or(subregexes) => {
            for subre in subregexes {
                do_followpos(subre, result);
            }
        },
        Regex::Cat(subregexes) => {
            for pair in subregexes.windows(2) {
                let [left, right] = pair else { unreachable!() };
                for i in left.lastpos() {
                    for j in right.firstpos() {
                        result.insert((i, j));
                    }
                }
            }
        },
        Regex::Star(subregex) => {
            let first = subregex.firstpos();
            let last = subregex.lastpos();
            for i in last {
                for j in &first {
                    result.insert((i, *j));
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
            Regex::Cat(children) => children.iter().all(|child| child.nullable()),
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

    fn followpos(&self) -> BTreeSet<(usize, usize)> {
        let mut result: BTreeSet<(usize, usize)> = Default::default();
        do_followpos(self, &mut result);
        result
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nullable() {
        let regex = Regex::RightmostLeaf(1);
        assert!(regex.nullable());
    }
}
