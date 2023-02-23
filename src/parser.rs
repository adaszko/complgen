// nom-based parser for extended form of BNF
// Can steal some stuff from https://docs.rs/clap_complete/latest/clap_complete/index.html
// Drawing of a railroad diagram for a grammar.

use nom::{character::complete::{alphanumeric1, char, one_of, multispace0}, bytes::complete::{tag, escaped, is_not}, IResult, branch::alt};

#[derive(Debug, PartialEq)]
struct Production<'a> {
    lhs: &'a str,
    rhs: Expr<'a>,
}


#[derive(Debug, PartialEq)]
enum Expr<'a> {
    Terminal(&'a str),
    Nonterminal(&'a str),
    Optional(Box<Expr<'a>>),
    ShellOut(&'a str),
    Sequence(Box<Expr<'a>>),
    OneOrMore(Box<Expr<'a>>),
    Alternative(Vec<Box<Expr<'a>>>),
}


fn terminal(input: &str) -> IResult<&str, &str> {
    let (input, inner) = escaped(is_not("\\"), '\\', one_of("\""))(input)?;
    Ok((input, inner))
}


fn terminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, literal) = terminal(input)?;
    Ok((input, Expr::Terminal(literal)))
}


fn nonterminal(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('<')(input)?;
    let (input, name) = alphanumeric1(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}


fn nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, nonterm) = nonterminal(input)?;
    Ok((input, Expr::Nonterminal(nonterm)))
}


fn optional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Box::new(expr))))
}


fn shell_out_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("$(")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, cmd) = terminal(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, Expr::ShellOut(cmd)))
}


fn expr(input: &str) -> IResult<&str, Expr> {
    let (input, expr) = alt((
        terminal_expr,
        nonterminal_expr,
        optional_expr,
    ))(input)?;
    Ok((input, expr))
}


fn production(input: &str) -> IResult<&str, Production> {
    let (input, name) = nonterminal(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = expr(input)?;
    let production = Production {
        lhs: name,
        rhs: expr,
    };
    Ok((input, production))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_terminal_expr() {
        const INPUT: &str = r#""foo""#;
        assert_eq!(nonterminal_expr(INPUT).unwrap(), ("", Expr::Terminal("foo")));
    }

    #[test]
    fn parses_nonterminal_expr() {
        const INPUT: &str = "<foo-bar>";
        assert_eq!(nonterminal_expr(INPUT).unwrap(), ("", Expr::Nonterminal("foo-bar")));
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo bar>]";
        assert_eq!(optional_expr(INPUT).unwrap(), ("", Expr::Nonterminal("foo bar")));
    }

    #[test]
    fn parses_shell_out_expr() {
        const INPUT: &str = "$(foo)";
        assert_eq!(shell_out_expr(INPUT).unwrap(), ("", Expr::Nonterminal("cli")));
    }

    #[test]
    fn parses_sequence_expr() {
        const INPUT: &str = "<first-nonterminal> <second nonterminal>";
        assert_eq!(shell_out_expr(INPUT).unwrap(), ("", Expr::Nonterminal("cli")));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<nonterminal>...";
        assert_eq!(shell_out_expr(INPUT).unwrap(), ("", Expr::Nonterminal("cli")));
    }

    #[test]
    fn parses_alternative_expr() {
        const INPUT: &str = "<first-nonterminal> | <second nonterminal>";
        assert_eq!(shell_out_expr(INPUT).unwrap(), ("", Expr::Nonterminal("cli")));
    }

    #[test]
    fn parses_parenthesised_production() {
        const INPUT: &str = r#"<clone> ::= "clone" (<urn> | <url>) [<clone-option>...]"#;
        // assert_eq!(production(INPUT).unwrap(), ("", Expr::Nonterminal("cli")));
    }
}
