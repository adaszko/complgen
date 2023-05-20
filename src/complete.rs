use std::{rc::Rc, cell::RefCell};

use crate::grammar::Expr;


fn match_against_regex<'a, 'b>(expr: &'a Expr, mut words: &'b [&'a str], completions: Rc<RefCell<Vec<&'a str>>>) -> Option<&'b [&'a str]> {
    match expr {
        Expr::Literal(s) => {
            return if words[0] == s.as_str() {
                Some(&words[1..])
            }
            else {
                None
            }
        },
        Expr::Variable(_) => Some(&words[1..]),
        Expr::Sequence(subexprs) => {
            let mut rest = words;
            for e in subexprs {
                if let Some(r) = do_get_completions(e, rest, completions.clone()) {
                    rest = r;
                }
                else {
                    return None;
                }
            }
            return Some(rest);
        },
        Expr::Alternative(subexpr) => {
            for e in subexpr {
                if let Some(r) = do_get_completions(e, words, completions.clone()) {
                    return Some(r);
                }
            }
            return None;
        },
        Expr::Optional(subexpr) => {
            if let Some(rest) = do_get_completions(subexpr, words, completions) {
                return Some(rest);
            }
            else {
                return Some(words);
            }
        },
        Expr::Many1(subexpr) => {
            let mut matched_count = 0;
            while let Some(rest) = do_get_completions(subexpr, words, completions.clone()) {
                words = rest;
                matched_count += 1;
            }
            if matched_count >= 1 {
                return Some(words);
            }
            else {
                return None;
            }
        },
    }
}


fn generate_completions<'a, 'b>(expr: &'a Expr, completions: Rc<RefCell<Vec<&'a str>>>) {
    match expr {
        Expr::Literal(s) => completions.borrow_mut().push(s),
        Expr::Variable(_) => (),
        Expr::Sequence(subexpr) => generate_completions(&subexpr[0], completions),
        Expr::Alternative(subexprs) => {
            for e in subexprs {
                generate_completions(e, completions.clone());
            }
        },
        Expr::Optional(subexpr) => generate_completions(subexpr, completions),
        Expr::Many1(subexpr) => generate_completions(subexpr, completions),
    }
}


pub fn do_get_completions<'a, 'b>(expr: &'a Expr, words_before_cursor: &'b [&'a str], completions: Rc<RefCell<Vec<&'a str>>>) -> Option<&'b [&'a str]> {
    if !words_before_cursor.is_empty() {
        match_against_regex(expr, words_before_cursor, completions)
    }
    else {
        generate_completions(expr, completions);
        None
    }
}


pub fn get_completions<'a, 'b>(expr: &'a Expr, words_before_cursor: &'b [&'a str]) -> Vec<&'a str> {
    // The borrow checker isn't happy with passing a mut ref in a loop inside of
    // match_against_regex().  The reason for that is we're using indirect recursion and borrow
    // checker's scope is limited to a single function so it isn't aware of the indirect recursion.
    // We work around it using Rc<RefCell<>>.
    let completions: Rc<RefCell<Vec<&'a str>>> = Default::default();
    do_get_completions(expr, words_before_cursor, completions.clone());
    let mut result = Rc::<RefCell<Vec<&str>>>::try_unwrap(completions).unwrap().into_inner();
    result.sort();
    result
}


#[cfg(test)]
mod tests {
    use hashbrown::HashSet;

    use crate::grammar::parse;

    use super::*;

    #[test]
    fn completes_darcs_add() {
        const GRAMMAR: &str = r#"darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;"#;
        let g = parse(GRAMMAR).unwrap();
        let (_, expr) = g.into_command_expr();
        assert_eq!(get_completions(&expr, &vec![]), vec!["add"]);

        let input = vec!["add"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&expr, &input));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"]);
        assert_eq!(generated, expected);
    }
}
