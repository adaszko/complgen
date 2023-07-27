use std::rc::Rc;

use nom::{IResult, character::complete::{char, anychar, multispace1}, bytes::complete::{tag_no_case, tag, take_till, take_while1, is_not, take_while}, error::context, branch::alt, combinator::fail, multi::many1};
use ustr::ustr;

use crate::grammar::Expr;


#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    UsageLine(Rc<Expr>),
    OptionLine(Rc<Expr>),
    OptionList(Vec<Rc<Expr>>),
}


fn multispace0_except_newline(input: &str) -> IResult<&str, &str> {
    let (input, spaces) = take_while(|c| c == ' ' || c == '\t')(input)?;
    Ok((input, spaces))
}


fn multispace1_except_newline(input: &str) -> IResult<&str, &str> {
    let (input, spaces) = take_while1(|c| c == ' ' || c == '\t')(input)?;
    Ok((input, spaces))
}

fn newline_or_eof(input: &str) -> IResult<&str, &str> {
    if let Ok((input, _)) = tag::<&str, &str, nom::error::Error<_>>("\n")(input) {
        Ok((input, "\n"))
    } else if input.is_empty() {
        Ok((input, ""))
    }
    else {
        fail("newline_or_eof")
    }
}


fn terminal(input: &str) -> IResult<&str, &str> {
    fn is_terminal_char(c: char) -> bool {
        c.is_ascii_alphanumeric() && c.is_ascii_lowercase() || c == '-' || c == '+' || c == '_' || c == '.'
    }
    let (input, term) = take_while1(is_terminal_char)(input)?;
    Ok((input, term))
}


fn terminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, term) = context("terminal", terminal)(input)?;
    let expr = Expr::Terminal(ustr(term), None);
    Ok((input, expr))
}


fn uppercase_nonterminal(input: &str) -> IResult<&str, &str> {
    fn is_nonterm_char(c: char) -> bool {
        c.is_ascii_uppercase()
    }

    take_while1(is_nonterm_char)(input)
}


fn angle_brackets_nonterminal(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}


fn nonterminal(input: &str) -> IResult<&str, &str> {
    alt((
        angle_brackets_nonterminal,
        uppercase_nonterminal
    ))(input)
}


fn nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, nonterm) = context("nonterminal", nonterminal)(input)?;
    Ok((input, Expr::Nonterminal(ustr(nonterm))))
}


fn optional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, expr) = usage_expr(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(expr))))
}


fn many1_tag(input: &str) -> IResult<&str, ()> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = tag("...")(input)?;
    Ok((input, ()))
}


fn unary_expr(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((
        optional_expr,
        terminal_expr,
        nonterminal_expr,
    ))(input)?;

    if let Ok((input, ())) = many1_tag(input) {
        return Ok((input, Expr::Many1(Rc::new(e))));
    }

    Ok((input, e))
}


fn sequence_expr(input: &str) -> IResult<&str, Expr> {
    fn do_sequence_expr(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multispace0_except_newline(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = unary_expr(input)?;
    let mut factors: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = do_sequence_expr(input) else { break };
        if let Expr::Sequence(v) = right {
            factors.extend(v.into_iter().map(|rc| Rc::into_inner(rc).unwrap()));
        }
        else {
            factors.push(right);
        }
        input = pos;
    }
    let result = if factors.len() == 1 {
        factors.into_iter().next().unwrap()
    } else {
        Expr::Sequence(factors.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}


fn usage_expr(input: &str) -> IResult<&str, Expr> {
    sequence_expr(input)
}


fn usage_line(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = tag_no_case("usage:")(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, expr) = usage_expr(input)?;
    Ok((input, expr))
}

fn short_option(input: &str) -> IResult<&str, &str> {
    let (rest, _) = char('-')(input)?;
    let (rest, _) = anychar(rest)?;
    Ok((rest, &input[0..2]))
}

fn long_option(input: &str) -> IResult<&str, &str> {
    fn is_long_option_char(c: char) -> bool {
        c.is_alphanumeric() || c == '-'
    }

    let (rest, _) = tag("--")(input)?;
    let (rest, long) = take_while1(is_long_option_char)(rest)?;
    Ok((rest, &input[0..2+long.len()]))
}


fn description(input: &str) -> IResult<&str, &str> {
    take_till(|c| c == '\n')(input)
}

// e.g. " -i, --ignore-case         ignore case distinctions in patterns and data"
fn short_option_long_option_description(input: &str) -> IResult<&str, (&str, &str, &str)> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    Ok((input, (short, long, description)))
}

fn short_option_long_option_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, (short, long, description)) = short_option_long_option_description(input)?;
    let d = ustr(description);
    let s = Expr::Terminal(ustr(short), Some(d));
    let l = Expr::Terminal(ustr(long), Some(d));
    let a = Expr::Alternative(vec![Rc::new(s), Rc::new(l)]);
    Ok((input, a))
}

fn short_option_long_option_argument_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = alt((
        tag("="),
        multispace0_except_newline,
    ))(input)?;
    let (input, arg) = nonterminal(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;

    let d = ustr(description);
    let s = Expr::Terminal(ustr(short), Some(d));
    let l = Expr::Terminal(ustr(long), Some(d));
    let alt = Expr::Alternative(vec![Rc::new(s), Rc::new(l)]);
    let arg = Expr::Nonterminal(ustr(arg));
    let s = Expr::Sequence(vec![Rc::new(alt), Rc::new(arg)]);

    Ok((input, s))
}

// e.g. "  -I                        equivalent to --binary-files=without-match"
fn short_option_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, descr) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    Ok((input, Expr::Terminal(ustr(short), Some(ustr(descr)))))
}


//   -Z <FLAG>                 Unstable (nightly-only) flags to Cargo, see 'cargo -Z help' for details
fn short_option_argument_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, arg) = nonterminal(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, descr) = description(input)?;
    let (input, _) = newline_or_eof(input)?;

    let t = Expr::Terminal(ustr(short), Some(ustr(descr)));
    let a = Expr::Nonterminal(ustr(arg));
    let s = Expr::Sequence(vec![Rc::new(t), Rc::new(a)]);

    Ok((input, s))
}


// e.g. "--no-ignore-case      do not ignore case distinctions (default)"
fn long_option_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, descr) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    let expr = Expr::Terminal(ustr(long), Some(ustr(descr)));
    Ok((input, expr))
}

// e.g. "--foo=BAR Description"
fn long_option_argument_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = alt((
        tag("="),
        multispace0_except_newline,
    ))(input)?;
    let (input, arg) = nonterminal(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    let expr = Expr::Sequence(vec![Rc::new(Expr::Terminal(ustr(long), Some(ustr(description)))), Rc::new(Expr::Nonterminal(ustr(arg)))]);
    Ok((input, expr))
}


fn optional_nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, nonterm) = nonterminal_expr(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(nonterm))))
}

fn optional_equals_nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, nonterm) = nonterminal_expr(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(nonterm))))
}


fn short_option_long_option_optional_argument_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = alt((
        tag("="),
        multispace1_except_newline,
    ))(input)?;
    let (input, opt_arg) = optional_nonterminal_expr(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    let s = Rc::new(Expr::Terminal(ustr(short), Some(ustr(description))));
    let l = Rc::new(Expr::Terminal(ustr(long), Some(ustr(description))));
    let o = Rc::new(opt_arg);
    let expr = Expr::Sequence(vec![Rc::new(Expr::Alternative(vec![s, l])), o]);
    Ok((input, expr))
}


fn short_option_long_option_many1_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, short) = short_option(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = many1_tag(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    let s = Rc::new(Expr::Terminal(ustr(short), Some(ustr(description))));
    let l = Rc::new(Expr::Many1(Rc::new(Expr::Terminal(ustr(long), Some(ustr(description))))));
    let expr = Expr::Alternative(vec![s, l]);
    Ok((input, expr))
}

fn long_option_optional_argument_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, long) = long_option(input)?;
    let (input, _) = multispace0_except_newline(input)?;
    let (input, opt_arg) = alt((
        optional_equals_nonterminal_expr,
        optional_nonterminal_expr,
    ))(input)?;
    let (input, _) = multispace1_except_newline(input)?;
    let (input, description) = description(input)?;
    let (input, _) = newline_or_eof(input)?;
    let l = Rc::new(Expr::Terminal(ustr(long), Some(ustr(description))));
    let a = Rc::new(opt_arg);
    let expr = Expr::Sequence(vec![l, a]);
    Ok((input, expr))
}

fn option_line(input: &str) -> IResult<&str, Expr> {
    alt((
        short_option_long_option_optional_argument_description_expr,
        short_option_long_option_many1_description_expr,
        short_option_long_option_argument_description_expr,
        short_option_long_option_description_expr,
        long_option_optional_argument_description_expr,
        long_option_argument_description_expr,
        long_option_description_expr,
        short_option_argument_description_expr,
        short_option_description_expr,
    ))(input)
}


fn options_list(input: &str) -> IResult<&str, Statement> {
    let (input, _) = multispace0_except_newline(input)?;
    let (input, _) = tag_no_case("options:")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, options) = many1(option_line)(input)?;
    let refcounted_options = options.into_iter().map(|e| Rc::new(e)).collect();
    Ok((input, Statement::OptionList(refcounted_options)))
}


fn fluff_line(input: &str) -> IResult<&str, ()> {
    let (input, _) = take_till(|c| c == '\n')(input)?;
    let (input, _) = newline_or_eof(input)?;
    Ok((input, ()))
}


pub fn usage(mut input: &str) -> IResult<&str, Vec<Statement>> {
    let mut result: Vec<Statement> = Default::default();
    while !input.is_empty() {
        if let Ok((rest, expr)) = usage_line(input) {
            result.push(Statement::UsageLine(Rc::new(expr)));
            input = rest;
        }
        else if let Ok((rest, expr)) = option_line(input) {
            result.push(Statement::OptionLine(Rc::new(expr)));
            input = rest;
        }
        else if let Ok((rest, stmt)) = options_list(input) {
            result.push(stmt);
            input = rest;
        }
        else if let Ok((rest, ())) = fluff_line(input) {
            input = rest;
        }
        else {
            return Ok((input, result))
        }
    }
    Ok(("", result))
}


pub fn scrape(input: &str) -> complgen::Result<Vec<Statement>> {
    let (input, expr) = match usage(input) {
        Ok((input, expr)) => (input, expr),
        Err(e) => return Err(complgen::Error::ParsingError(e.to_string())),
    };

    if !input.is_empty() {
        return Err(complgen::Error::ParsingError(input.to_owned()));
    }

    Ok(expr)
}


fn escape_description(s: &str) -> String {
    s.replace("\"", "\\\"")
}


fn do_pretty_print(e: &Expr) -> String {
    match e {
        Expr::Subword(dfa, None) => format!(r#"{dfa:?}"#),
        Expr::Subword(dfa, Some(descr)) => format!(r#"{dfa:?} {descr:?}"#),
        Expr::Terminal(term, None) => format!(r#"{term}"#),
        Expr::Terminal(term, Some(descr)) => format!(r#"{term} "{}""#, escape_description(descr)),
        Expr::Nonterminal(nonterm) => format!(r#"<{nonterm}>"#),
        Expr::Command(cmd) => format!(r#"{{{{{{ {cmd} }}}}}}"#),
        Expr::Sequence(subexprs) => itertools::join(subexprs.iter().map(|e| do_pretty_print(e)), " "),
        Expr::Alternative(subexprs) => itertools::join(subexprs.iter().map(|e| do_pretty_print(e)), " | "),
        Expr::Optional(subexpr) => format!(r#"[{}]"#, do_pretty_print(subexpr)),
        Expr::Many1(subexpr) => format!(r#"{} ..."#, do_pretty_print(subexpr)),
    }
}


pub fn pretty_print(exprs: &[Statement]) {
    fn pp_option_line(e: &Expr) {
        match e {
            Expr::Alternative(subexprs) => {
                for subexpr in subexprs {
                    println!(" | {}", do_pretty_print(subexpr));
                }
            },
            _ => println!(" | {}", do_pretty_print(e)),
        }
    }

    for e in exprs {
        match e {
            Statement::UsageLine(e) => println!("{};", do_pretty_print(e)),
            Statement::OptionLine(e) => pp_option_line(e),
            Statement::OptionList(opts) => {
                println!("<OPTION> ::= ");
                for e in opts {
                    pp_option_line(e);
                }
                println!(" ;");
            },
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_short_option() {
        const INPUT: &str = r#"-i"#;
        let (rest, short_option) = short_option(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(short_option, "-i");
    }

    #[test]
    fn parses_long_option() {
        const INPUT: &str = r#"--ignore-case"#;
        let (rest, long_option) = long_option(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(long_option, "--ignore-case");
    }

    #[test]
    fn parses_short_option_long_option_description() {
        const INPUT: &str = r#"-i, --ignore-case         ignore case distinctions in patterns and data"#;
        let (rest, (short_option, long_option, description)) = short_option_long_option_description(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(short_option, "-i");
        assert_eq!(long_option, "--ignore-case");
        assert_eq!(description, "ignore case distinctions in patterns and data");
    }

    #[test]
    fn parses_long_option_argument_description() {
        use Expr::*;
        const INPUT: &str = r#"--name <string> Name of the project"#;
        let (rest, expr) = long_option_argument_description_expr(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Sequence(vec![Rc::new(Terminal(ustr("--name"), Some(ustr("Name of the project")))), Rc::new(Nonterminal(ustr("string")))]));
    }

    #[test]
    fn parses_short_option_long_option_argument_description() {
        use Expr::*;
        const INPUT: &str = r#"-n, --name <string> Name of the project"#;
        let (rest, expr) = option_line(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-n"), Some(ustr("Name of the project")))), Rc::new(Terminal(ustr("--name"), Some(ustr("Name of the project"))))])), Rc::new(Nonterminal(ustr("string")))]));
    }

    #[test]
    fn parses_long_option_optional_argument_description() {
        use Expr::*;
        const INPUT: &str = r#"--name [<string>] Name of the project"#;
        let (rest, expr) = option_line(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Sequence(vec![Rc::new(Terminal(ustr("--name"), Some(ustr("Name of the project")))), Rc::new(Optional(Rc::new(Nonterminal(ustr("string")))))]));
    }

    #[test]
    fn parses_long_option_optional_equals_argument_description() {
        use Expr::*;
        const INPUT: &str = r#"      --timings[=<FMTS>]        Timing output formats (unstable) (comma separated): html, json"#;
        let (rest, expr) = option_line(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(expr, Sequence(vec![Rc::new(Terminal(ustr("--timings"), Some(ustr("Timing output formats (unstable) (comma separated): html, json")))), Rc::new(Optional(Rc::new(Nonterminal(ustr("FMTS")))))]));
    }

    #[test]
    fn parses_usage_line() {
        use Expr::*;
        const INPUT: &str = r#"Usage: grep [OPTION]... PATTERNS [FILE]..."#;
        let (rest, e) = usage_line(INPUT).unwrap();
        assert_eq!(rest, "");
        assert_eq!(e, Sequence(vec![Rc::new(Terminal(ustr("grep"), None)), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTION"))))))), Rc::new(Nonterminal(ustr("PATTERNS"))), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("FILE")))))))]));
    }

    #[test]
    fn parses_grep_help() {
        use Expr::*;
        use Statement::*;
        const INPUT: &str = r#"
Usage: ggrep [OPTION]... PATTERNS [FILE]...
Search for PATTERNS in each FILE.
Example: ggrep -i 'hello world' menu.h main.c
PATTERNS can contain multiple patterns separated by newlines.

Pattern selection and interpretation:
  -E, --extended-regexp     PATTERNS are extended regular expressions
  -F, --fixed-strings       PATTERNS are strings
  -G, --basic-regexp        PATTERNS are basic regular expressions
  -P, --perl-regexp         PATTERNS are Perl regular expressions
  -e, --regexp=PATTERNS     use PATTERNS for matching
  -f, --file=FILE           take PATTERNS from FILE
  -i, --ignore-case         ignore case distinctions in patterns and data
      --no-ignore-case      do not ignore case distinctions (default)
  -w, --word-regexp         match only whole words
  -x, --line-regexp         match only whole lines
  -z, --null-data           a data line ends in 0 byte, not newline
"#;
        let Ok(("", expr)) = usage(INPUT) else { panic!("parse error") };
        assert_eq!(expr, [
            UsageLine(
                Rc::new(Sequence(vec![Rc::new(Terminal(ustr("ggrep"), None)), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTION"))))))), Rc::new(Nonterminal(ustr("PATTERNS"))), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("FILE")))))))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-E"), Some(ustr("are extended regular expressions")))), Rc::new(Terminal(ustr("--extended-regexp"), Some(ustr("are extended regular expressions"))))])), Rc::new(Nonterminal(ustr("PATTERNS")))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-F"), Some(ustr("are strings")))), Rc::new(Terminal(ustr("--fixed-strings"), Some(ustr("are strings"))))])), Rc::new(Nonterminal(ustr("PATTERNS")))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-G"), Some(ustr("are basic regular expressions")))), Rc::new(Terminal(ustr("--basic-regexp"), Some(ustr("are basic regular expressions"))))])), Rc::new(Nonterminal(ustr("PATTERNS")))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-P"), Some(ustr("are Perl regular expressions")))), Rc::new(Terminal(ustr("--perl-regexp"), Some(ustr("are Perl regular expressions"))))])), Rc::new(Nonterminal(ustr("PATTERNS")))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-e"), Some(ustr("use PATTERNS for matching")))), Rc::new(Terminal(ustr("--regexp"), Some(ustr("use PATTERNS for matching"))))])), Rc::new(Nonterminal(ustr("PATTERNS")))])),
            ),
            OptionLine(
                Rc::new(Sequence(vec![Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-f"), Some(ustr("take PATTERNS from FILE")))), Rc::new(Terminal(ustr("--file"), Some(ustr("take PATTERNS from FILE"))))])), Rc::new(Nonterminal(ustr("FILE")))])),
            ),
            OptionLine(
                Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-i"), Some(ustr("ignore case distinctions in patterns and data")))), Rc::new(Terminal(ustr("--ignore-case"), Some(ustr("ignore case distinctions in patterns and data"))))])),
            ),
            OptionLine(
                Rc::new(Terminal(ustr("--no-ignore-case"), Some(ustr("do not ignore case distinctions (default)")))),
            ),
            OptionLine(
                Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-w"), Some(ustr("match only whole words")))), Rc::new(Terminal(ustr("--word-regexp"), Some(ustr("match only whole words"))))])),
            ),
            OptionLine(
                Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-x"), Some(ustr("match only whole lines")))), Rc::new(Terminal(ustr("--line-regexp"), Some(ustr("match only whole lines"))))])),
            ),
            OptionLine(
                Rc::new(Alternative(vec![Rc::new(Terminal(ustr("-z"), Some(ustr("a data line ends in 0 byte, not newline")))), Rc::new(Terminal(ustr("--null-data"), Some(ustr("a data line ends in 0 byte, not newline"))))])),
            ),
        ]);
    }
}
