use crate::grammar::Expr;


fn do_get_completions<'a, 'b>(expr: &'a Expr, args: &'b [&'a str], mut i: usize, completed_word_index: usize, completions: &'b mut Vec<&'a str>) {
    match expr {
        Expr::Literal(s) => completions.push(s),
        Expr::Variable(_) => {},
        Expr::Sequence(subexpr) => {
            for e in subexpr {
                do_get_completions(e, args, i, completed_word_index, completions);
                i += 1;
            }
        },
        Expr::Alternative(subexpr) => {
            for e in subexpr {
                do_get_completions(e, args, i, completed_word_index, completions);
            }
        },
        Expr::Optional(_) => todo!(),
        Expr::Many1(_) => todo!(),
    }
}


pub fn get_completions<'a>(expr: &'a Expr, args: &'a [&'a str], completed_word_index: usize) -> Vec<&'a str> {
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
        assert_eq!(get_completions(&expr, &vec!["darcs"], 1), vec!["add"]);
        let input = vec!["darcs", "add"];
        let generated: HashSet<&str> = HashSet::from_iter(get_completions(&expr, &input, 1));
        let expected = HashSet::from_iter(["--boring", "--debug", "--dry-run", "--no-prehook", "--prehook", "--quiet", "--reserved-ok", "--standard-verbosity", "--verbose", "-v", "--case-ok", "--debug-http", "--no-date-trick", "--not-recursive", "--prompt-posthook", "--recursive", "--run-posthook", "--timings", "-q", "--date-trick", "--debug-verbose", "--no-posthook", "--posthook", "--prompt-prehook", "--repodir", "--run-prehook", "--umask", "-r"]);
        assert_eq!(generated, expected);
    }
}
