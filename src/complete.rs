use crate::grammar::Expr;


fn do_get_completions<'a, 'b>(expr: &'a Expr, args: &'b [&'a str], i: usize, completed_word_index: usize, completions: &'b mut Vec<&'a str>) -> (bool, usize) {
    if i < completed_word_index {
        // We're matching words in `args` against the grammar up to `completed_word_index`.
        match expr {
            Expr::Literal(s) => {
                if args[i] == s {
                    return (true, i + 1);
                }
                else {
                    return (false, i);
                }
            },
            Expr::Variable(_) => return (true, i + 1),
            Expr::Sequence(subexpr) => {
                let mut j = i;
                for e in subexpr {
                    let (matched, k) = do_get_completions(e, args, j, completed_word_index, completions);
                    if matched {
                        j = k;
                    }
                    else {
                        return (false, i);
                    }
                }
                return (true, j);
            },
            Expr::Alternative(subexpr) => {
                for e in subexpr {
                    let (matched, rest) = do_get_completions(e, args, i, completed_word_index, completions);
                    if matched {
                        return (true, rest);
                    }
                }
                return (false, i);
            },
            Expr::Optional(subexpr) => {
                let (matched, rest) = do_get_completions(subexpr, args, i, completed_word_index, completions);
                if matched {
                    return (true, rest);
                }
                else {
                    return (true, i);
                }
            },
            Expr::Many1(subexpr) => {
                let mut j = i;
                let mut matched_count = 0;
                loop {
                    let (matched, k) = do_get_completions(subexpr, args, j, completed_word_index, completions);
                    if !matched {
                        break;
                    }
                    j = k;
                    matched_count += 1;
                }
                if matched_count >= 1 {
                    return (true, j);
                }
                else {
                    return (false, i);
                }
            },
        }
    }
    else if i == completed_word_index {
        // We're generating completions for args[completed_word_index].
        match expr {
            Expr::Literal(s) => {
                completions.push(s);
                return (true, i + 1);
            },
            Expr::Variable(_) => {
                return (true, i + 1);
            },
            Expr::Sequence(subexpr) => {
                return do_get_completions(&subexpr[0], args, i, completed_word_index, completions);
            },
            Expr::Alternative(subexprs) => {
                let mut any_matched = false;
                for e in subexprs {
                    let (matched, _) = do_get_completions(e, args, i, completed_word_index, completions);
                    any_matched = any_matched || matched;
                }
                return (any_matched, i + 1);
            },
            Expr::Optional(subexpr) => {
                let (matched, rest) = do_get_completions(subexpr, args, i, completed_word_index, completions);
                if matched {
                    return (true, rest);
                }
                else {
                    return (false, rest);
                }
            },
            Expr::Many1(subexpr) => {
                let mut j = i;
                let mut matched_count = 0;
                loop {
                    if j > completed_word_index {
                        return (true, j);
                    }
                    let (matched, k) = do_get_completions(subexpr, args, j, completed_word_index, completions);
                    if !matched {
                        break;
                    }
                    j = k;
                    matched_count += 1;
                }
                if matched_count >= 1 {
                    return (true, j);
                }
                else {
                    return (false, i);
                }
            },
        }
    }
    else {
        return (true, i);
    }
}


pub fn get_completions<'a>(expr: &'a Expr, args: &'a [&'a str], completed_word_index: usize) -> Vec<&'a str> {
    assert!(completed_word_index <= args.len());
    let mut completions: Vec<&'a str> = Default::default();
    do_get_completions(&expr, args, 0, completed_word_index, &mut completions);
    completions
}


#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::grammar::parse;

    use super::*;

    #[test]
    fn completes_darcs_add() {
        const GRAMMAR: &str = r#"darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;"#;
        let g = parse(GRAMMAR).unwrap();
        let (_, expr) = g.into_command_expr();
        assert_eq!(get_completions(&expr, &vec![], 0), vec!["add"]);

        let input = vec!["add"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&expr, &input, 1));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"]);
        assert_eq!(generated, expected);
    }
}
