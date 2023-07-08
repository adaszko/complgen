use std::{rc::Rc, debug_assert, borrow::Borrow};

use bumpalo::Bump;
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while1, escaped, take_till, take_while, take_until, escaped_transform},
    character::{complete::{char, multispace1, one_of}},
    multi::{many0, many1},
    IResult, combinator::{fail, opt}, error::context, sequence::preceded,
};

use complgen::{Error, Result};
use ustr::{Ustr, ustr, UstrMap, UstrSet};

use crate::{dfa::DFA, regex::AugmentedRegex};


#[derive(Debug, Clone, PartialEq)]
pub enum Subword {
    Expr(Rc<Expr>),
    DFA(Rc<DFA>),
}


// Can't use an arena here until proptest supports non-owned types: https://github.com/proptest-rs/proptest/issues/9
#[derive(Clone, PartialEq)]
pub enum Expr {
    Prefix(Ustr, Subword, Option<Ustr>), // e.g. --color=(always | never | auto)
    Terminal(Ustr, Option<Ustr>), // e.g. --help
    Nonterminal(Ustr), // e.g. <PATH>, <DIRECTORY>, etc.
    Command(Ustr), // e.g. { ls }
    Sequence(Vec<Rc<Expr>>),
    Alternative(Vec<Rc<Expr>>),
    Optional(Rc<Expr>),
    Many1(Rc<Expr>),
}


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Specialization {
    pub bash: Option<Ustr>,
    pub fish: Option<Ustr>,
    pub zsh: Option<Ustr>,
    pub generic: Option<Ustr>,
}


impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prefix(term, expr, Some(descr)) => f.write_fmt(format_args!(r#"Rc::new(Prefix(ustr("{term}"), {expr:?}, Some(ustr("{descr}"))))"#)),
            Self::Prefix(term, expr, None) => f.write_fmt(format_args!(r#"Rc::new(Prefix(ustr("{term}"), {expr:?}, None))"#)),
            Self::Terminal(term, Some(descr)) => f.write_fmt(format_args!(r#"Rc::new(Terminal(ustr("{term}"), Some(ustr("{}"))))"#, descr)),
            Self::Terminal(term, None) => f.write_fmt(format_args!(r#"Rc::new(Terminal(ustr("{term}"), None))"#)),
            Self::Nonterminal(nonterm) => f.write_fmt(format_args!(r#"Rc::new(Nonterminal(ustr("{nonterm}")))"#)),
            Self::Command(arg0) => f.write_fmt(format_args!(r#"Rc::new(Command(ustr("{}")))"#, arg0)),
            Self::Sequence(arg0) => f.write_fmt(format_args!(r#"Rc::new(Sequence(vec!{:?}))"#, arg0)),
            Self::Alternative(arg0) => f.write_fmt(format_args!(r#"Rc::new(Alternative(vec!{:?}))"#, arg0)),
            Self::Optional(arg0) => f.write_fmt(format_args!(r#"Rc::new(Optional({:?}))"#, arg0)),
            Self::Many1(arg0) => f.write_fmt(format_args!(r#"Rc::new(Many1({:?}))"#, arg0)),
        }
    }
}


fn do_to_railroad_diagram(expr: Rc<Expr>) -> Box<dyn railroad::Node> {
    match expr.as_ref() {
        Expr::Prefix(prefix, _, _) => Box::new(railroad::Terminal::new(format!("{prefix}*"))),
        Expr::Terminal(s, _) => Box::new(railroad::Terminal::new(s.as_str().to_string())),
        Expr::Nonterminal(s) => Box::new(railroad::NonTerminal::new(s.as_str().to_string())),
        Expr::Command(s) => Box::new(railroad::Comment::new(s.as_str().to_string())),
        Expr::Sequence(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs.iter().map(|e| do_to_railroad_diagram(Rc::clone(e))).collect();
            Box::new(railroad::Sequence::new(subnodes))
        },
        Expr::Alternative(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs.iter().map(|e| do_to_railroad_diagram(Rc::clone(e))).collect();
            Box::new(railroad::Choice::new(subnodes))
        },
        Expr::Optional(subexpr) => Box::new(railroad::Optional::new(do_to_railroad_diagram(Rc::clone(subexpr)))),
        Expr::Many1(subexpr) => {
            let subnode = do_to_railroad_diagram(Rc::clone(subexpr));
            Box::new(railroad::Repeat::new(subnode, Box::new(railroad::Empty)))
        },
    }
}

pub fn to_railroad_diagram<W: std::io::Write>(expr: Rc<Expr>, output: &mut W) -> std::result::Result<(), std::io::Error> {
    let root = {
        let node = do_to_railroad_diagram(expr);
        let mut seq: railroad::Sequence<Box<dyn railroad::Node>> = Default::default();
        seq.push(Box::new(railroad::Start));
        seq.push(node);
        seq.push(Box::new(railroad::End));
        seq
    };
    let mut dia = railroad::Diagram::new(root);
    dia.add_element(railroad::svg::Element::new("style").set("type", "text/css").text(railroad::DEFAULT_CSS));
    dia.write(output)
}

pub fn to_railroad_diagram_file<P: AsRef<std::path::Path>>(
    expr: Rc<Expr>,
    path: P,
) -> std::result::Result<(), std::io::Error> {
    let mut file = std::fs::File::create(path)?;
    to_railroad_diagram(expr, &mut file)?;
    Ok(())
}


fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('#')(input)?;
    let (input, content) = take_till(|c| c == '\n')(input)?;
    Ok((input, content))
}

fn blanks(input: &str) -> IResult<&str, ()> {
    let (input, _) = alt((multispace1, comment))(input)?;
    Ok((input, ()))
}

fn multiblanks0(mut input: &str) -> IResult<&str, ()> {
    while let Ok((rest, _)) = blanks(input) {
        input = rest;
    }
    Ok((input, ()))
}

fn multiblanks1(input: &str) -> IResult<&str, ()> {
    let (input, _) = blanks(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, ()))
}


const RESERVED_CHARACTERS: &str = r#"()[]{}<>.|;"#;


fn is_terminal_char(c: char) -> bool {
    if c == '\\' {
        return false;
    }

    if RESERVED_CHARACTERS.find(c).is_some() {
        return false;
    }

    c.is_ascii_alphanumeric() || c.is_ascii_punctuation()
}


fn prefix(input: &str) -> IResult<&str, String> {
    let (input, term) = escaped_transform(take_while1(is_terminal_char), '\\', one_of(RESERVED_CHARACTERS))(input)?;
    if term.is_empty() {
        return fail(input);
    }
    Ok((input, term))
}

fn subword_optional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, expr) = subword_expr(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(expr))))
}

fn subword_parenthesized_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, e) = subword_expr(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn subword_unary_expr(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((
        nonterminal_expr,
        subword_optional_expr,
        subword_parenthesized_expr,
        command_expr,
        terminal_opt_description_expr,
    ))(input)?;

    if let Ok((input, ())) = many1_tag(input) {
        return Ok((input, Expr::Many1(Rc::new(e))));
    }

    Ok((input, e))
}

fn subword_sequence_expr(input: &str) -> IResult<&str, Expr> {
    let (input, exprs) = many1(subword_unary_expr)(input)?;
    let result = if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    }
    else {
        Expr::Sequence(exprs.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn subword_alternative_expr(input: &str) -> IResult<&str, Expr> {
    let (mut input, left) = subword_sequence_expr(input)?;
    let mut elems: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = preceded(char('|'), subword_sequence_expr)(input) else { break };
        elems.push(right);
        input = pos;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        Expr::Alternative(elems.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn subword_expr(input: &str) -> IResult<&str, Expr> {
    subword_alternative_expr(input)
}

fn prefix_opt_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, term) = prefix(input)?;
    let (input, expr) = subword_expr(input)?;
    let (input, descr) = opt(multiblanks1_description)(input)?;
    let expr = Expr::Prefix(ustr(&term), Subword::Expr(Rc::new(expr)), descr.map(ustr));
    Ok((input, expr))
}

fn terminal(input: &str) -> IResult<&str, String> {
    let (input, term) = escaped_transform(take_while1(is_terminal_char), '\\', one_of(RESERVED_CHARACTERS))(input)?;
    if term.is_empty() {
        return fail(input);
    }
    Ok((input, term))
}

fn description(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('"')(input)?;
    let (input, descr) = escaped(take_till(|c| c == '"'), '\\', char('"'))(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, descr))
}

fn multiblanks1_description(input: &str) -> IResult<&str, &str> {
    let (input, _) = multiblanks1(input)?;
    let (input, descr) = description(input)?;
    Ok((input, descr))
}

fn terminal_opt_description_expr(input: &str) -> IResult<&str, Expr> {
    let (input, term) = terminal(input)?;
    let (input, descr) = opt(multiblanks1_description)(input)?;
    let expr = Expr::Terminal(ustr(&term), descr.map(ustr));
    Ok((input, expr))
}

fn nonterminal(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}

fn nonterminal_expr(input: &str) -> IResult<&str, Expr> {
    let (input, nonterm) = context("nonterminal", nonterminal)(input)?;
    Ok((input, Expr::Nonterminal(ustr(nonterm))))
}

fn single_bracket_command(input: &str) -> IResult<&str, &str> {
    fn is_command_char(c: char) -> bool {
        c != '}'
    }

    let (input, _) = char('{')(input)?;
    let (input, cmd) = escaped(take_while(is_command_char), '\\', one_of("{}"))(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, cmd.trim()))
}

fn triple_bracket_command(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("{{{")(input)?;
    let (input, cmd) = take_until("}}}")(input)?;
    let (input, _) = tag("}}}")(input)?;
    Ok((input, cmd.trim()))
}

fn command(input: &str) -> IResult<&str, &str> {
    alt((triple_bracket_command, single_bracket_command))(input)
}

fn command_expr(input: &str) -> IResult<&str, Expr> {
    let (input, cmd) = command(input)?;
    Ok((input, Expr::Command(ustr(cmd))))
}

fn optional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(expr))))
}

fn parenthesized_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn many1_tag(input: &str) -> IResult<&str, ()> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("...")(input)?;
    Ok((input, ()))
}

fn unary_expr(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((
        nonterminal_expr,
        optional_expr,
        parenthesized_expr,
        command_expr,
        prefix_opt_description_expr,
        terminal_opt_description_expr,
    ))(input)?;

    if let Ok((input, ())) = many1_tag(input) {
        return Ok((input, Expr::Many1(Rc::new(e))));
    }

    Ok((input, e))
}

fn sequence_expr(input: &str) -> IResult<&str, Expr> {
    fn do_sequence_expr(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = unary_expr(input)?;
    let mut factors: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = do_sequence_expr(input) else { break };
        factors.push(right);
        input = pos;
    }
    let result = if factors.len() == 1 {
        factors.drain(..).next().unwrap()
    } else {
        Expr::Sequence(factors.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn alternative_expr(input: &str) -> IResult<&str, Expr> {
    fn do_alternative_expr(input: &str) -> IResult<&str, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, _) = char('|')(input)?;
        let (input, _) = multiblanks0(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = sequence_expr(input)?;
    let mut elems: Vec<Expr> = vec![left];
    loop {
        let Ok((pos, right)) = do_alternative_expr(input) else { break };
        elems.push(right);
        input = pos;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        Expr::Alternative(elems.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    alternative_expr(input)
}


#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    CallVariant {
        head: Ustr,
        expr: Rc<Expr>,
    },
    NonterminalDefinition {
        symbol: Ustr,
        shell: Option<Ustr>,
        expr: Rc<Expr>,
    },
}


fn call_variant(input: &str) -> IResult<&str, Statement> {
    let (input, name) = terminal(input)?;
    let (input, _) = multiblanks1(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let production = Statement::CallVariant {
        head: ustr(&name),
        expr: Rc::new(expr),
    };

    Ok((input, production))
}


fn specialized_nonterminal(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">@")(input)?;
    let (input, _) = char('@')(input)?;
    let (input, shell) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, (name, shell)))
}


fn optionally_specialized_nonterminal(input: &str) -> IResult<&str, (&str, Option<&str>)> {
    if let Ok((input, (name, shell))) = specialized_nonterminal(input) {
        return Ok((input, (name, Some(shell))));
    }
    if let Ok((input, name)) = nonterminal(input) {
        return Ok((input, (name, None)));
    }
    fail(input)
}

fn nonterminal_definition(input: &str) -> IResult<&str, Statement> {
    let (input, (name, shell)) = optionally_specialized_nonterminal(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let stmt = Statement::NonterminalDefinition {
        symbol: ustr(name),
        shell: shell.map(|s| ustr(s)),
        expr: Rc::new(e),
    };

    Ok((input, stmt))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, stmt) = alt((call_variant, nonterminal_definition))(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, stmt))
}

fn grammar(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, _) = multiblanks0(input)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, statements))
}


#[derive(Debug, PartialEq, Clone)]
pub struct Grammar {
    statements: Vec<Statement>,
}


pub struct ValidGrammar {
    pub command: Ustr,
    pub expr: Rc<Expr>,
    pub specializations: UstrMap<Specialization>,
    pub undefined_nonterminals: UstrSet,
    pub unused_nonterminals: UstrSet,
}


fn make_specializations_map(statements: &[Statement]) -> Result<UstrMap<Specialization>> {
    let mut specializations: UstrMap<Specialization> = Default::default();
    for definition in statements {
        let (name, shell, expr) = match definition {
            Statement::NonterminalDefinition { symbol, shell: Some(shell), expr } => (*symbol, shell, expr),
            Statement::NonterminalDefinition { shell: None, .. } => continue,
            Statement::CallVariant { .. } => continue,
        };
        let command = match expr.borrow() {
            Expr::Command(cmd) => cmd,
            _ => return Err(Error::NonCommandSpecialization(name, Some(*shell))),
        };
        let known_shell = match shell.as_str() {
            "bash" | "fish" | "zsh" => true,
            _ => false,
        };
        if !known_shell {
            return Err(Error::UnknownShell(*shell));
        }
        let spec = specializations.entry(name).or_default();
        match shell.as_str() {
            "bash" => {
                if let Some(_) = spec.bash {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.bash = Some(*command);
            },
            "fish" => {
                if let Some(_) = spec.fish {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.fish = Some(*command);
            },
            "zsh" => {
                if let Some(_) = spec.zsh {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.zsh = Some(*command);
            },
            _ => unreachable!(),
        }
    }

    for definition in statements {
        let (name, expr) = match definition {
            Statement::NonterminalDefinition { symbol, shell: None, expr } => (symbol, expr),
            _ => continue,
        };
        let Some(spec) = specializations.get_mut(name) else { continue };
        let Expr::Command(command) = expr.borrow() else {
            return Err(Error::NonCommandSpecialization(*name, None));
        };
        if let Some(_) = spec.generic {
            return Err(Error::DuplicateNonterminalDefinition(*name, None));
        }
        spec.generic = Some(*command);
    }

    specializations.entry(ustr("PATH")).or_insert_with(||
        Specialization {
            bash: Some(ustr(&format!(r#"compgen -A file "$1""#))),
            fish: Some(ustr(&format!(r#"__fish_complete_path "$1""#))),
            zsh: Some(ustr("_path_files")),
            generic: None,
        }
    );

    specializations.entry(ustr("DIRECTORY")).or_insert_with(||
        Specialization {
            bash: Some(ustr(&format!(r#"compgen -A directory "$1""#))),
            fish: Some(ustr(&format!(r#"__fish_complete_directories "$1""#))),
            zsh: Some(ustr("_path_files -/")),
            generic: None,
        }
    );

    Ok(specializations)
}


fn flatten_expr(expr: Rc<Expr>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) | Expr::Nonterminal(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Prefix(prefix, child, description) => {
            let child = match child {
                Subword::Expr(e) => Rc::clone(&e),
                Subword::DFA(_) => unreachable!(),
            };
            let new_child = flatten_expr(Rc::clone(&child));
            if Rc::ptr_eq(&child, &new_child) {
                Rc::new(Expr::Sequence(vec![Rc::new(Expr::Terminal(*prefix, *description)), child]))
            }
            else {
                Rc::new(Expr::Sequence(vec![Rc::new(Expr::Terminal(*prefix, *description)), new_child]))
            }
        },
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children.iter().map(|e| flatten_expr(Rc::clone(&e))).collect();
            if children.iter().zip_eq(new_children.iter()).all(|(left, right)| Rc::ptr_eq(left, &right)) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Sequence(new_children))
            }
        },
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children.iter().map(|e| flatten_expr(Rc::clone(&e))).collect();
            if children.iter().zip_eq(new_children.iter()).all(|(left, right)| Rc::ptr_eq(left, &right)) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Alternative(new_children))
            }
        },
        Expr::Optional(child) => {
            let new_child = flatten_expr(Rc::clone(&child));
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Optional(new_child))
            }
        },
        Expr::Many1(child) => {
            let new_child = flatten_expr(Rc::clone(&child));
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Many1(new_child))
            }
        },
    }
}


fn compile_subword_exprs(expr: Rc<Expr>, specs: &UstrMap<Specialization>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Prefix(prefix, subword_expr, description) => {
            let arena = Bump::new();
            let subword_expr = match subword_expr {
                Subword::Expr(e) => Rc::clone(&e),
                Subword::DFA(_) => unreachable!(),
            };
            let subword_expr = flatten_expr(subword_expr);
            let regex = AugmentedRegex::from_expr(&subword_expr, &specs, &arena);
            let dfa = DFA::from_regex(&regex);
            Rc::new(Expr::Prefix(*prefix, Subword::DFA(Rc::new(dfa)), *description))
        },
        Expr::Terminal(..) | Expr::Nonterminal(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children.iter().map(|e| compile_subword_exprs(Rc::clone(&e), specs)).collect();
            if children.iter().zip_eq(new_children.iter()).all(|(left, right)| Rc::ptr_eq(left, &right)) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Sequence(new_children))
            }
        },
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children.iter().map(|e| compile_subword_exprs(Rc::clone(&e), specs)).collect();
            if children.iter().zip_eq(new_children.iter()).all(|(left, right)| Rc::ptr_eq(left, &right)) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Alternative(new_children))
            }
        },
        Expr::Optional(child) => {
            let new_child = compile_subword_exprs(Rc::clone(&child), specs);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Optional(new_child))
            }
        },
        Expr::Many1(child) => {
            let new_child = compile_subword_exprs(Rc::clone(&child), specs);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Many1(new_child))
            }
        }
    }
}


impl ValidGrammar {
    pub fn from_grammar(grammar: Grammar) -> Result<Self> {
        let command = {
            let mut commands: Vec<Ustr> = grammar.statements.iter().filter_map(|v|
                match v {
                    Statement::CallVariant { head: lhs, .. } => Some(*lhs),
                    Statement::NonterminalDefinition { .. } => None,
                }
            ).collect();

            if commands.is_empty() {
                return Err(Error::MissingCallVariants);
            }

            commands.sort_unstable();
            commands.dedup();

            if commands.len() > 1 {
                return Err(Error::VaryingCommandNames(
                    commands.into_iter().collect(),
                ));
            }
            commands[0]
        };

        let expr = {
            let call_variants: Vec<Rc<Expr>> = grammar.statements.iter().filter_map(|v|
                match v {
                    Statement::CallVariant { expr: rhs, .. } => Some(rhs.clone()),
                    Statement::NonterminalDefinition { .. } => None,
                }
            ).collect();

            if call_variants.len() == 1 {
                Rc::clone(&call_variants[0])
            }
            else {
                Rc::new(Expr::Alternative(call_variants))
            }
        };

        let specializations = make_specializations_map(&grammar.statements)?;

        let mut nonterminal_definitions: UstrMap<Rc<Expr>> = {
            let mut nonterminal_definitions: UstrMap<Rc<Expr>> = Default::default();
            for definition in &grammar.statements {
                let (symbol, expr) = match definition {
                    Statement::NonterminalDefinition { symbol, expr, shell: None, } => (*symbol, expr),
                    _ => continue,
                };
                if nonterminal_definitions.contains_key(&symbol) {
                    return Err(Error::DuplicateNonterminalDefinition(symbol, None));
                }
                nonterminal_definitions.insert(symbol, Rc::clone(&expr));
            }
            nonterminal_definitions
        };

        let mut unused_nonterminals: UstrSet = nonterminal_definitions.keys().copied().collect();

        for nonterminal in get_nonterminals_resolution_order(&nonterminal_definitions)? {
            let e = Rc::clone(nonterminal_definitions.get(&nonterminal).unwrap());
            *nonterminal_definitions.get_mut(&nonterminal).unwrap() = resolve_nonterminals(e, &nonterminal_definitions, &specializations, &mut unused_nonterminals);
        }
        let expr = resolve_nonterminals(expr, &nonterminal_definitions, &specializations, &mut unused_nonterminals);

        let undefined_nonterminals = {
            let mut nonterms = get_expression_nonterminals(Rc::clone(&expr));
            nonterms.retain(|n| !specializations.contains_key(n));
            nonterms
        };

        let expr = compile_subword_exprs(Rc::clone(&expr), &specializations);

        let g = ValidGrammar {
            command,
            expr,
            undefined_nonterminals,
            specializations,
            unused_nonterminals,
        };
        Ok(g)
    }
}


fn resolve_nonterminals(expr: Rc<Expr>, vars: &UstrMap<Rc<Expr>>, specializations: &UstrMap<Specialization>, unused_nonterminals: &mut UstrSet) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) => Rc::clone(&expr),
        Expr::Prefix(prefix, child, description) => {
            let child = match child {
                Subword::Expr(e) => Rc::clone(&e),
                Subword::DFA(..) => unreachable!(),
            };
            let new_child = resolve_nonterminals(Rc::clone(&child), vars, specializations, unused_nonterminals);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Prefix(*prefix, Subword::Expr(new_child), *description))
            }
        },
        Expr::Nonterminal(name) => {
            if specializations.contains_key(name) {
                // Specialized nonterminals are resolved when the target shell is known, not earlier.
                return Rc::clone(&expr);
            }
            match vars.get(&name) {
                Some(replacement) => {
                    unused_nonterminals.remove(name);
                    Rc::clone(&replacement)
                },
                None => {
                    Rc::clone(&expr)
                },
            }
        },
        Expr::Command(_) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
                if !Rc::ptr_eq(&child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Sequence(new_children))
            } else {
                Rc::clone(&expr)
            }
        },
        Expr::Alternative(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
                if !Rc::ptr_eq(&child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Alternative(new_children))
            } else {
                Rc::clone(&expr)
            }
        },
        Expr::Optional(child) => {
            let new_child = resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Optional(new_child))
            }
        },
        Expr::Many1(child) => {
            let new_child = resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            }
            else {
                Rc::new(Expr::Many1(new_child))
            }
        },
    }
}


fn do_get_expression_nonterminals(expr: Rc<Expr>, deps: &mut UstrSet) {
    match expr.as_ref() {
        Expr::Terminal(..) => {},
        Expr::Prefix(_, subexpr, _) => {
            let subexpr = match subexpr {
                Subword::Expr(e) => Rc::clone(&e),
                Subword::DFA(..) => unreachable!(),
            };
            do_get_expression_nonterminals(Rc::clone(&subexpr), deps);
        },
        Expr::Nonterminal(varname) => {
            deps.insert(*varname);
        },
        Expr::Command(_) => {},
        Expr::Sequence(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(&child), deps);
            }
        },
        Expr::Alternative(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(&child), deps);
            }
        },
        Expr::Optional(child) => { do_get_expression_nonterminals(Rc::clone(&child), deps); }
        Expr::Many1(child) => { do_get_expression_nonterminals(Rc::clone(&child), deps); }
    }
}


fn get_expression_nonterminals(expr: Rc<Expr>) -> UstrSet {
    let mut result: UstrSet = Default::default();
    do_get_expression_nonterminals(expr, &mut result);
    result
}


pub fn get_not_depended_on_nonterminals(dependency_graph: &UstrMap<UstrSet>) -> UstrSet {
    let num_depending_nonterminals = {
        let mut num_depending_nonterminals: UstrMap<usize> = dependency_graph.keys().map(|vertex| (*vertex, 0)).collect();
        for (_, nonterminal_depependencies) in dependency_graph.iter() {
            for dep in nonterminal_depependencies {
                *num_depending_nonterminals.get_mut(dep).unwrap() += 1;
            }
        }
        num_depending_nonterminals
    };

    let vertices_without_incoming_edges: UstrSet = num_depending_nonterminals
        .into_iter()
        .filter(|(_, indegree)| *indegree == 0)
        .map(|(vertex, _)| vertex)
        .collect();

    vertices_without_incoming_edges
}


fn traverse_nonterminal_dependencies_dfs(vertex: Ustr, graph: &UstrMap<UstrSet>, path: &mut Vec<Ustr>, visited: &mut UstrSet, result: &mut Vec<Ustr>) -> Result<()> {
    visited.insert(vertex);
    let dummy = UstrSet::default();
    for child in graph.get(&vertex).unwrap_or_else(|| &dummy) {
        if path.contains(child) {
            let mut path = path.clone();
            path.push(vertex);
            return Err(Error::NonterminalDefinitionsCycle(Some(path)));
        }
        if visited.contains(child) {
            continue;
        }
        path.push(*child);
        traverse_nonterminal_dependencies_dfs(*child, graph, path, visited, result)?;
        path.pop().unwrap();
        result.push(*child);
    }
    Ok(())
}


// A topological order but without the initial nonterminals that don't depend on any other
// nonterminals.
fn get_nonterminals_resolution_order(nonterminal_definitions: &UstrMap<Rc<Expr>>) -> Result<Vec<Ustr>> {
    if nonterminal_definitions.is_empty() {
        return Ok(Vec::default());
    }

    let mut dependency_graph: UstrMap<UstrSet> = Default::default();
    for (varname, expr) in nonterminal_definitions {
        let mut referenced_nonterminals = get_expression_nonterminals(Rc::clone(&expr));
        referenced_nonterminals.retain(|var| nonterminal_definitions.contains_key(var));
        dependency_graph.insert(*varname, referenced_nonterminals);
    }

    let not_depended_on_vars = get_not_depended_on_nonterminals(&dependency_graph);
    if not_depended_on_vars.is_empty() {
        return Err(Error::NonterminalDefinitionsCycle(None));
    }

    let mut visited: UstrSet = Default::default();
    let mut result: Vec<Ustr> = Default::default();
    let mut path: Vec<Ustr> = Default::default();
    for vertex in not_depended_on_vars {
        debug_assert!(!visited.contains(&vertex));
        traverse_nonterminal_dependencies_dfs(vertex, &dependency_graph, &mut path, &mut visited, &mut result)?;
        result.push(vertex);
        debug_assert!(path.is_empty());
    }

    // Filter out nonterminals that don't depend on any other as they are already fully resolved.
    result.retain(|vertex| dependency_graph.get(vertex).map(|children| !children.is_empty()).unwrap_or(true));

    log::debug!("nonterminals expansion order: {:?}", result);
    Ok(result)
}


impl Grammar {
    pub fn parse(input: &str) -> Result<Self> {
        let (input, statements) = match grammar(input) {
            Ok((input, statements)) => (input, statements),
            Err(e) => return Err(Error::ParsingError(e.to_string())),
        };

        if !input.is_empty() {
            return Err(Error::ParsingError(input.to_owned()));
        }

        let g = Grammar {
            statements,
        };

        Ok(g)
    }
}


#[cfg(test)]
pub mod tests {
    use std::rc::Rc;
    use std::ops::Rem;
    use proptest::{strategy::BoxedStrategy, test_runner::TestRng};
    use proptest::prelude::*;
    use ustr::ustr as u;

    use super::*;
    use Expr::*;

    fn arb_literal(inputs: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..inputs.len()).prop_map(move |index| Rc::new(Terminal(ustr(&inputs[index]), None))).boxed()
    }

    fn arb_nonterminal(nonterminals: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..nonterminals.len()).prop_map(move |index| Rc::new(Nonterminal(ustr(&nonterminals[index])))).boxed()
    }

    fn arb_optional(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth-1, max_width).prop_map(|e| Rc::new(Optional(e))).boxed()
    }

    fn arb_many1(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth-1, max_width).prop_map(|e| Rc::new(Many1(e))).boxed()
    }

    fn arb_sequence(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width).prop_flat_map(move |width| {
            let e = arb_expr(inputs.clone(), nonterminals.clone(), remaining_depth-1, max_width);
            prop::collection::vec(e, width).prop_map(|v| Rc::new(Sequence(v)))
        }).boxed()
    }

    fn arb_alternative(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width).prop_flat_map(move |width| {
            let e = arb_expr(inputs.clone(), nonterminals.clone(), remaining_depth-1, max_width);
            prop::collection::vec(e, width).prop_map(|v| Rc::new(Alternative(v)))
        }).boxed()
    }

    pub fn arb_expr(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<Rc<Expr>> {
        if remaining_depth <= 1 {
            prop_oneof![
                arb_literal(Rc::clone(&inputs)),
                arb_nonterminal(nonterminals),
            ].boxed()
        }
        else {
            prop_oneof![
                arb_literal(inputs.clone()),
                arb_nonterminal(nonterminals.clone()),
                arb_optional(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_many1(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_sequence(inputs.clone(), nonterminals.clone(), remaining_depth, max_width),
                arb_alternative(inputs, nonterminals, remaining_depth, max_width),
            ].boxed()
        }
    }

    pub fn do_arb_match(e: Rc<Expr>, rng: &mut TestRng, max_width: usize, output: &mut Vec<Ustr>) {
        match e.as_ref() {
            Terminal(s, _) => output.push(*s),
            Prefix(s, _, _) => output.push(ustr(&format!("{}{}", s, "anything"))),
            Nonterminal(_) => output.push(ustr("anything")),
            Command(_) => output.push(ustr("anything")),
            Sequence(v) => {
                for subexpr in v {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
            Alternative(v) => {
                let chosen_alternative = usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(Rc::clone(&v[chosen_alternative]), rng, max_width, output);
            },
            Optional(subexpr) => {
                if rng.next_u64() % 2 == 0 {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
            Many1(subexpr) => {
                let n = rng.next_u64();
                let chosen_len = n % u64::try_from(max_width).unwrap() + 1;
                for _ in 0..chosen_len {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            },
        }
    }

    pub fn arb_match(e: Rc<Expr>, mut rng: TestRng, max_width: usize) -> (Rc<Expr>, Vec<Ustr>) {
        let mut output: Vec<Ustr> = Default::default();
        do_arb_match(Rc::clone(&e), &mut rng, max_width, &mut output);
        (e, output)
    }

    // Produce an arbitrary sequence matching `e`.
    pub fn arb_expr_match(inputs: Rc<Vec<Ustr>>, nonterminals: Rc<Vec<Ustr>>, remaining_depth: usize, max_width: usize) -> BoxedStrategy<(Rc<Expr>, Vec<Ustr>)> {
        arb_expr(inputs, nonterminals, remaining_depth, max_width).prop_perturb(move |e, rng| arb_match(e, rng, max_width)).boxed()
    }

    #[test]
    fn parses_prefix() {
        const INPUT: &str = r#"--color=<WHEN>"#;
        let ("<WHEN>", e) = prefix(INPUT).unwrap() else { unreachable!() };
        assert_eq!(e, "--color=");
    }

    #[test]
    fn parses_prefix_expr() {
        const INPUT: &str = r#"--color=<WHEN>"#;
        let ("", e) = prefix_opt_description_expr(INPUT).unwrap() else { unreachable!() };
        assert_eq!(e, Prefix(ustr("--color="), Subword::Expr(Rc::new(Expr::Nonterminal(ustr("WHEN")))), None));
    }

    #[test]
    fn parses_prefix_description_expr() {
        const INPUT: &str = r#"--color=<WHEN> "use markers to highlight the matching strings""#;
        let ("", e) = prefix_opt_description_expr(INPUT).unwrap() else { unreachable!() };
        assert_eq!(e, Prefix(ustr("--color="), Subword::Expr(Rc::new(Expr::Nonterminal(ustr("WHEN")))), Some(ustr("use markers to highlight the matching strings"))));
    }

    #[test]
    fn parses_word_terminal() {
        const INPUT: &str = r#"foo\.bar"#;
        let ("", e) = terminal_opt_description_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("foo.bar"), None));
    }

    #[test]
    fn parses_short_option_terminal() {
        const INPUT: &str = r#"-f"#;
        let ("", e) = terminal_opt_description_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("-f"), None));
    }

    #[test]
    fn parses_long_option_terminal() {
        const INPUT: &str = r#"--foo"#;
        let ("", e) = terminal_opt_description_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Terminal(u("--foo"), None));
    }

    #[test]
    fn parses_symbol() {
        const INPUT: &str = "<FILE>";
        let ("", e) = nonterminal_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Nonterminal(u("FILE")));
    }

    #[test]
    fn parses_command() {
        const INPUT: &str = "{ rustup toolchain list | cut -d' ' -f1 }";
        let ("", e) = command_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Command(u("rustup toolchain list | cut -d' ' -f1")));
    }

    #[test]
    fn parses_triple_brackets_command() {
        const INPUT: &str = "{{{ rad patch list | awk '{print $3}' | grep . | grep -vw ID }}}";
        let ("", e) = command_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Command(u("rad patch list | awk '{print $3}' | grep . | grep -vw ID")));
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo>]";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Optional(Rc::new(Nonterminal(u("foo")))));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Many1(Rc::new(Nonterminal(u("foo")))));
    }

    #[test]
    fn parses_sequence_expr() {
        const INPUT: &str = "<first-symbol> <second symbol>";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Nonterminal(u("first-symbol"))),
                Rc::new(Nonterminal(u("second symbol"))),
            ])
        );
    }

    #[test]
    fn parses_alternative_expr() {
        const INPUT: &str = "a b | c";
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Alternative(vec![
                Rc::new(Sequence(vec![Rc::new(Terminal(u("a"), None)), Rc::new(Terminal(u("b"), None))])),
                Rc::new(Terminal(u("c"), None))
            ])
        );
    }

    #[test]
    fn parses_parenthesised_expr() {
        const INPUT: &str = r#"a (b | c)"#;
        let ("", e) = expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(u("a"), None)),
                Rc::new(Alternative(vec![Rc::new(Terminal(u("b"), None)), Rc::new(Terminal(u("c"), None))])),
            ])
        );
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let ("", v) = call_variant(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(
            v,
            Statement::CallVariant {
                head: u("foo"),
                expr: Rc::new(Terminal(u("bar"), None))
            }
        );
    }

    #[test]
    fn parses_grammar() {
        const INPUT: &str = r#"
foo bar;
foo baz;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("foo"), expr: Rc::new(Terminal(u("bar"), None)) },
                    Statement::CallVariant { head: u("foo"), expr: Rc::new(Terminal(u("baz"), None)) }
                ],
            }
        );
    }

    #[test]
    fn bug1() {
        // Did not consider whitespace before ...
        const INPUT: &str = "darcs help ( ( -v | --verbose ) | ( -q | --quiet ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;";
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("darcs"), expr: Rc::new(Sequence(vec![
                    Rc::new(Terminal(u("help"), None)),
                    Rc::new(Sequence(vec![
                        Rc::new(Many1(Rc::new(Alternative(vec![
                            Rc::new(Alternative(vec![Rc::new(Terminal(u("-v"), None)), Rc::new(Terminal(u("--verbose"), None))])),
                            Rc::new(Alternative(vec![Rc::new(Terminal(u("-q"), None)), Rc::new(Terminal(u("--quiet"), None))])),
                        ],)),)),
                        Rc::new(Optional(Rc::new(Sequence(vec![
                            Rc::new(Nonterminal(u("DARCS_COMMAND"))),
                            Rc::new(Optional(Rc::new(Terminal(u("DARCS_SUBCOMMAND"), None)))),
                        ])))),
                    ])),
                ])) },
                ],
            }
        );
    }

    #[test]
    fn parses_darcs_grammar() {
        // Source: https://github.com/mbrubeck/compleat/blob/56dd9761cdbb07de674947b129192cd8043cda8a/examples/darcs.usage
        const INPUT: &str = r#"
darcs help ( ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;
darcs add ( --boring | ( --case-ok | --reserved-ok ) | ( ( -r | --recursive ) | --not-recursive ) | ( --date-trick | --no-date-trick ) | --repodir <DIRECTORY> | --dry-run | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;
darcs remove ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ( <FILE> | <DIRECTORY> )...;
darcs move ( ( --case-ok | --reserved-ok ) | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <SOURCE> ... <DESTINATION>;
darcs replace ( --token-chars <"[CHARS]"> | ( ( -f | --force ) | --no-force ) | --repodir <DIRECTORY> | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <OLD> <NEW> <FILE> ...;
darcs revert ( ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs unrevert ( --ignore-times | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs whatsnew ( ( ( -s | --summary ) | --no-summary ) | ( -u | --unified ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | --ignore-times | --boring | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs record ( ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( -A <EMAIL> | --author <EMAIL> ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | ( ( -a | --all ) | --pipe | ( -i | --interactive ) ) | ( --ask-deps | --no-ask-deps ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | --logfile <FILE> | --delete-logfile | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs unrecord ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs amend-record ( ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -n <N> | --index <N> ) ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( -A <EMAIL> | --author <EMAIL> ) | ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( ( -l | --look-for-adds ) | --dont-look-for-adds ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs mark-conflicts ( --ignore-times | --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs tag ( ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( -A <EMAIL> | --author <EMAIL> ) | ( --pipe | ( -i | --interactive ) ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [TAGNAME];
darcs setpref ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <PREF> <VALUE>;
darcs diff ( ( --to-match <PATTERN> | --to-patch <REGEXP> | --to-tag <REGEXP> | --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | --last <NUMBER> | ( -n <N-M> | --index <N-M> ) ) | --diff-command <COMMAND> | --diff-opts <OPTIONS> | ( -u | --unified ) | --repodir <DIRECTORY> | --store-in-memory | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs changes ( ( --to-match <PATTERN> | --to-patch <REGEXP> | --to-tag <REGEXP> | --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | ( -n <N-M> | --index <N-M> ) | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | --max-count <NUMBER> | --only-to-files | ( --context | --xml-output | --human-readable | --number | --count ) | ( ( -s | --summary ) | --no-summary ) | --reverse | --repo <URL> | --repodir <DIRECTORY> | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs annotate ( ( ( -s | --summary ) | --no-summary ) | ( -u | --unified ) | --human-readable | --xml-output | ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --creator-hash <HASH> | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs dist ( ( -d <DISTNAME> | --dist-name <DISTNAME> ) | --repodir <DIRECTORY> | ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --store-in-memory | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs trackdown ( --repodir <DIRECTORY> | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [[INITIALIZATION] COMMAND];
darcs show ( contents ( ( --match <PATTERN> | ( -p <REGEXP> | --patch <REGEXP> ) | ( -t <REGEXP> | --tag <REGEXP> ) | ( -n <N> | --index <N> ) ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [FILE]... | files ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( --pending | --no-pending ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | index ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | pristine ( ( --files | --no-files ) | ( --directories | --no-directories ) | ( -0 | --null ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | repo ( --repodir <DIRECTORY> | ( --files | --no-files ) | --xml-output | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | authors ( --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  | tags ( --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ...  );
darcs pull ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --mark-conflicts | --allow-conflicts | --dont-allow-conflicts | --skip-conflicts ) | --external-merge <COMMAND> | ( --test | --no-test ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | --ignore-unrelated-repos | ( --intersection | --union | --complement ) | ( --compress | --dont-compress ) | --nolinks | --ignore-times | --remote-repo <URL> | ( --set-scripts-executable | --dont-set-scripts-executable ) | --umask <UMASK> | ( --restrict-paths | --dont-restrict-paths ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY]...;
darcs obliterate ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --repodir <DIRECTORY> | ( ( -s | --summary ) | --no-summary ) | ( --compress | --dont-compress ) | --ignore-times | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs rollback ( ( --from-match <PATTERN> | --from-patch <REGEXP> | --from-tag <REGEXP> | --last <NUMBER> | --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( -A <EMAIL> | --author <EMAIL> ) | ( -m <PATCHNAME> | --patch-name <PATCHNAME> ) | ( --edit-long-comment | --skip-long-comment | --prompt-long-comment ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | ( --compress | --dont-compress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [ ( <FILE> | <DIRECTORY> ) ]...;
darcs push ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | ( --sign | --sign-as <KEYID> | --sign-ssl <IDFILE> | --dont-sign ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | --repodir <DIRECTORY> | ( --set-default | --no-set-default ) | --ignore-unrelated-repos | ( --apply-as <USERNAME> | --apply-as-myself ) | --nolinks | --remote-repo <URL> | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY];
darcs send ( ( --matches <PATTERN> | ( -p <REGEXP> | --patches <REGEXP> ) | ( -t <REGEXP> | --tags <REGEXP> ) ) | ( --no-deps | --dont-prompt-for-dependencies | --prompt-for-dependencies ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --from <EMAIL> | ( -A <EMAIL> | --author <EMAIL> ) | --to <EMAIL> | --cc <EMAIL> | --subject <SUBJECT> | --in-reply-to <EMAIL> | ( -o <FILE> | --output <FILE> ) | ( -O [<DIRECTORY>] | --output-auto-name [<DIRECTORY>] ) | ( --sign | --sign-as <KEYID> | --sign-ssl <IDFILE> | --dont-sign ) | --dry-run | --xml-output | ( ( -s | --summary ) | --no-summary ) | ( --edit-description | --dont-edit-description ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | --sendmail-command <COMMAND> | --ignore-unrelated-repos | --logfile <FILE> | --delete-logfile | --remote-repo <URL> | --context <FILENAME> | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... [REPOSITORY];
darcs apply ( ( --verify <PUBRING> | --verify-ssl <KEYS> | --no-verify ) | ( ( -a | --all ) | ( -i | --interactive ) ) | --dry-run | --xml-output | ( --mark-conflicts | --allow-conflicts | --no-resolve-conflicts | --dont-allow-conflicts | --skip-conflicts ) | --external-merge <COMMAND> | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | --reply <FROM> | --cc <EMAIL> | --happy-forwarding | --sendmail-command <COMMAND> | --ignore-times | ( --compress | --dont-compress ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | --umask <UMASK> | ( --restrict-paths | --dont-restrict-paths ) | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <PATCHFILE>;
darcs get ( ( --repo-name <DIRECTORY> | --repodir <DIRECTORY> ) | ( --partial | --lazy | --ephemeral | --complete ) | ( --to-match <PATTERN> | --to-patch <REGEXP> | ( -t <REGEXP> | --tag <REGEXP> ) | --context <FILENAME> ) | ( --set-default | --no-set-default ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | --nolinks | ( --hashed | --old-fashioned-inventory ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <REPOSITORY> [<DIRECTORY>];
darcs put ( ( --to-match <PATTERN> | --to-patch <REGEXP> | ( -t <REGEXP> | --tag <REGEXP> ) | --context <FILENAME> ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --hashed | --old-fashioned-inventory ) | ( --set-default | --no-set-default ) | --repodir <DIRECTORY> | ( --apply-as <USERNAME> | --apply-as-myself ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <NEW_REPOSITORY>;
darcs initialize ( ( --hashed | --darcs-2 | --old-fashioned-inventory ) | --repodir <DIRECTORY> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs optimize ( --repodir <DIRECTORY> | --reorder-patches | --sibling <URL> | --relink | --relink-pristine | --upgrade | --pristine | ( --compress | --dont-compress | --uncompress ) | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs check ( ( --complete | --partial ) | ( --no-test | --test ) | ( --leave-test-directory | --remove-test-directory ) | --repodir <DIRECTORY> | --ignore-times | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs repair ( --repodir <DIRECTORY> | --umask <UMASK> | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... ;
darcs convert ( ( --repo-name <DIRECTORY> | --repodir <DIRECTORY> ) | ( --set-scripts-executable | --dont-set-scripts-executable ) | ( --ssh-cm | --no-ssh-cm ) | ( --http-pipelining | --no-http-pipelining ) | --no-cache | ( --debug | --debug-verbose | --debug-http | ( -v | --verbose ) | ( -q | --quiet ) | --standard-verbosity ) | --timings | ( --posthook <COMMAND> | --no-posthook ) | ( --prompt-posthook | --run-posthook ) | ( --prehook <COMMAND> | --no-prehook ) | ( --prompt-prehook | --run-prehook ) ) ... <SOURCE> [<DESTINATION>];
"#;
        let _ = Grammar::parse(INPUT).unwrap();
    }

    #[test]
    fn parses_nonterminal_definition() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> ::= --color <WHEN>;
<WHEN> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant { head: u("grep"), expr: Rc::new(Sequence(vec![Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTION"))))))), Rc::new(Sequence(vec![Rc::new(Nonterminal(ustr("PATTERNS"))), Rc::new(Many1(Rc::new(Optional(Rc::new(Nonterminal(ustr("FILE")))))))]))])) },
                    Statement::NonterminalDefinition { symbol: u("OPTION"), shell: None, expr: Rc::new(Sequence(vec![Rc::new(Terminal(ustr("--color"), None)), Rc::new(Nonterminal(ustr("WHEN")))])) },
                    Statement::NonterminalDefinition { symbol: u("WHEN"), shell: None, expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("always"), None)), Rc::new(Terminal(ustr("never"), None)), Rc::new(Terminal(ustr("auto"), None))])) },
                ],
            }
        );
    }

    #[test]
    fn skips_comment() {
        const INPUT: &str = r#"#foo"#;
        assert_eq!(comment(INPUT).unwrap(), ("", "foo"));
    }

    #[test]
    fn parses_comments() {
        const INPUT: &str = r#"
# sample comment
<OPTION> ::= --extended-regexp                      # "PATTERNS are extended regular expressions"
           | --fixed-strings                        # "PATTERNS are strings"
           | --basic-regexp                         # "PATTERNS are basic regular expressions"
           | --perl-regexp                          # "PATTERNS are Perl regular expressions"
# another comment
           ;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::NonterminalDefinition { symbol: u("OPTION"), shell: None, expr: Rc::new(Alternative(vec![
                        Rc::new(Terminal(u("--extended-regexp"), None)),
                        Rc::new(Terminal(u("--fixed-strings"), None)),
                        Rc::new(Terminal(u("--basic-regexp"), None)),
                        Rc::new(Terminal(u("--perl-regexp"), None)),
                    ]))},
                ],
            }
        );
    }

    #[test]
    fn nonterminal_resolution_order_detects_trivial_cycle() {
        let nonterminal_definitions = UstrMap::from_iter([
            (u("FOO"), Rc::new(Nonterminal(u("BAR")))),
            (u("BAR"), Rc::new(Nonterminal(u("FOO")))),
        ]);
        assert!(matches!(get_nonterminals_resolution_order(&nonterminal_definitions), Err(Error::NonterminalDefinitionsCycle(None))));
    }

    #[test]
    fn nonterminal_resolution_order_detects_simple_cycle() {
        let nonterminal_definitions = UstrMap::from_iter([
            (u("FOO"), Rc::new(Nonterminal(u("BAR")))),
            (u("BAR"), Rc::new(Nonterminal(u("BAR")))),
        ]);
        assert!(matches!(&get_nonterminals_resolution_order(&nonterminal_definitions), Err(Error::NonterminalDefinitionsCycle(Some(path))) if path == &[u("BAR"), u("BAR")]));
    }

    #[test]
    fn computes_nonterminals_resolution_order() {
        let nonterminal_definitions = UstrMap::from_iter([
            (u("WHEN"), Rc::new(Alternative(vec![Rc::new(Terminal(u("always"), None)), Rc::new(Terminal(u("never"), None)), Rc::new(Terminal(u("auto"), None))]))),
            (u("FOO"), Rc::new(Nonterminal(u("WHEN")))),
            (u("OPTION"), Rc::new(Sequence(vec![Rc::new(Terminal(u("--color"), None)), Rc::new(Nonterminal(u("FOO")))]))),
        ]);
        assert_eq!(get_nonterminals_resolution_order(&nonterminal_definitions).unwrap(), vec![u("FOO"), u("OPTION")]);
    }

    #[test]
    fn parses_inline_shell_command() {
        const INPUT: &str = r#"
cargo [+{ rustup toolchain list | cut -d' ' -f1 }] [<OPTIONS>] [<COMMAND>];
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g.statements,
            [
                Statement::CallVariant { head: ustr("cargo"), expr: Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Prefix(ustr("+"), Subword::Expr(Rc::new(Command(ustr("rustup toolchain list | cut -d' ' -f1")))), None)))), Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTIONS"))))), Rc::new(Optional(Rc::new(Nonterminal(ustr("COMMAND")))))]))])) }
            ],
        );
    }

    #[test]
    fn parses_prefix_grammar() {
        const INPUT: &str = r#"
grep --color=<WHEN> --version;
<WHEN> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g.statements,
            [
                Statement::CallVariant {
                    head: ustr("grep"),
                    expr: Rc::new(Sequence(vec![Rc::new(Prefix(ustr("--color="), Subword::Expr(Rc::new(Expr::Nonterminal(ustr("WHEN")))), None)), Rc::new(Terminal(ustr("--version"), None))])),
                },

                Statement::NonterminalDefinition { symbol: ustr("WHEN"), shell: None, expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("always"), None)), Rc::new(Terminal(ustr("never"), None)), Rc::new(Terminal(ustr("auto"), None))])) },
            ],
        );
    }


    #[test]
    fn parses_strace_expr_grammar() {
        use Statement::*;
        const INPUT: &str = r#"
strace -e <EXPR>;
<EXPR> ::= [<qualifier>=][!]<value>[,<value>]...;
<qualifier> ::= trace | read | write | fault;
<value> ::= %file | file | all;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(g.statements, [
            CallVariant {
                head: ustr("strace"),
                expr: Rc::new(Sequence(vec![Rc::new(Terminal(ustr("-e"), None)), Rc::new(Nonterminal(ustr("EXPR")))])),
            },
            NonterminalDefinition {
                symbol: ustr("EXPR"),
                shell: None,
                expr: Rc::new(Sequence(vec![
                    Rc::new(Optional(Rc::new(Sequence(vec![Rc::new(Nonterminal(ustr("qualifier"))), Rc::new(Terminal(ustr("="), None))])))),
                    Rc::new(Optional(Rc::new(Terminal(ustr("!"), None)))),
                    Rc::new(Nonterminal(ustr("value"))),
                    Rc::new(Many1(Rc::new(Optional(Rc::new(Sequence(vec![Rc::new(Terminal(ustr(","), None)), Rc::new(Nonterminal(ustr("value")))])))))),
                ])),
            },
            NonterminalDefinition {
                symbol: ustr("qualifier"),
                shell: None,
                expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("trace"), None)), Rc::new(Terminal(ustr("read"), None)), Rc::new(Terminal(ustr("write"), None)), Rc::new(Terminal(ustr("fault"), None))])),
            },
            NonterminalDefinition {
                symbol: ustr("value"),
                shell: None,
                expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("%file"), None)), Rc::new(Terminal(ustr("file"), None)), Rc::new(Terminal(ustr("all"), None))])),
            },
        ]);
    }


    #[test]
    fn parses_lsof_filter_grammar() {
        use Statement::*;
        const INPUT: &str = r#"
lsof -s<PROTOCOL>:<STATE-SPEC>[,<STATE-SPEC>]...;
<PROTOCOL> ::= TCP | UDP;
<STATE-SPEC> ::= [^]<STATE>;
<STATE> ::= LISTEN | CLOSED;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(g.statements, [
            CallVariant {
                head: ustr("lsof"),
                expr: Rc::new(Prefix(ustr("-s"), Subword::Expr(Rc::new(Sequence(vec![
                    Rc::new(Nonterminal(ustr("PROTOCOL"))),
                    Rc::new(Terminal(ustr(":"), None)),
                    Rc::new(Nonterminal(ustr("STATE-SPEC"))),
                    Rc::new(Many1(Rc::new(Optional(Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr(","), None)),
                        Rc::new(Nonterminal(ustr("STATE-SPEC")))
                    ])))))),
                ]))), None)),
            },
            NonterminalDefinition {
                symbol: ustr("PROTOCOL"),
                shell: None,
                expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("TCP"), None)), Rc::new(Terminal(ustr("UDP"), None))])),
            },
            NonterminalDefinition {
                symbol: ustr("STATE-SPEC"),
                shell: None,
                expr: Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Terminal(ustr("^"), None)))), Rc::new(Nonterminal(ustr("STATE")))])),
            },
            NonterminalDefinition {
                symbol: ustr("STATE"),
                shell: None,
                expr: Rc::new(Alternative(vec![Rc::new(Terminal(ustr("LISTEN"), None)), Rc::new(Terminal(ustr("CLOSED"), None))])),
            },
        ],
        );
    }


    #[test]
    fn parses_shell_command_nonterminal_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> ::= { rustup toolchain list | cut -d' ' -f1 };
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant {
                        head: u("cargo"),
                        expr: Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Prefix(ustr("+"), Subword::Expr(Rc::new(Expr::Nonterminal(ustr("toolchain")))), None)))), Rc::new(Sequence(vec![Rc::new(Optional(Rc::new(Nonterminal(ustr("OPTIONS"))))), Rc::new(Optional(Rc::new(Nonterminal(ustr("COMMAND")))))]))])),
                    },
                    Statement::NonterminalDefinition {
                        symbol: u("toolchain"),
                        shell: None,
                        expr: Rc::new(Command(u("rustup toolchain list | cut -d' ' -f1"))),
                    },
                ],
            }
        );
    }

    #[test]
    fn parses_descr() {
        const INPUT: &str = r#""PATTERNS are extended regular expressions""#;
        let ("", e) = description(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, "PATTERNS are extended regular expressions");
    }

    #[test]
    fn parses_term_descr() {
        const INPUT: &str = r#"--extended-regexp "PATTERNS are extended regular expressions""#;
        let ("", e) = terminal_opt_description_expr(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(e, Expr::Terminal(ustr("--extended-regexp"), Some(ustr("PATTERNS are extended regular expressions"))));
    }

    #[test]
    fn parses_description() {
        const INPUT: &str = r#"
grep --extended-regexp "PATTERNS are extended regular expressions";
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g.statements,
            vec![
                Statement::CallVariant { head: u("grep"), expr: Rc::new(Terminal(ustr("--extended-regexp"), Some(u("PATTERNS are extended regular expressions")))) }
            ],
        );
    }

    #[test]
    fn detects_duplicated_nonterminals() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> ::= --color <WHEN>;
<OPTION> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert!(matches!(ValidGrammar::from_grammar(g), Err(Error::DuplicateNonterminalDefinition(nonterm, None)) if nonterm == "OPTION"));
    }

    #[test]
    fn issue_15() {
        const INPUT: &str = r#"foo\.sh [-h] ;"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(g.statements, vec![
            Statement::CallVariant {
                head: u("foo.sh"),
                expr: Rc::new(Optional(Rc::new(Terminal(ustr("-h"), None)))),
            },
        ]);
    }

    #[test]
    fn parses_nonterminal_shell_specific() {
        const INPUT: &str = r#"<FILE@bash>"#;
        let ("", (nonterm, shell)) = specialized_nonterminal(INPUT).unwrap() else { panic!("parsing error"); };
        assert_eq!(nonterm, "FILE");
        assert_eq!(shell, "bash");
    }

    #[test]
    fn parses_specialized_nonterminals() {
        use Statement::*;
        const INPUT: &str = r#"
ls <FILE>;
<FILE@bash> ::= { compgen -A file "$1" };
<FILE@fish> ::= { __fish_complete_path "$1" };
"#;
        let g = Grammar::parse(INPUT).unwrap();
        assert_eq!(
            g.statements,
            vec![
                Statement::CallVariant {
                    head: u("ls"),
                    expr: Rc::new(Nonterminal(ustr("FILE"))), // should not get expanded because it's specialized
                },
                NonterminalDefinition { symbol: ustr("FILE"), shell: Some(ustr("bash")), expr: Rc::new(Command(ustr(r#"compgen -A file "$1""#))) },
                NonterminalDefinition { symbol: ustr("FILE"), shell: Some(ustr("fish")), expr: Rc::new(Command(ustr(r#"__fish_complete_path "$1""#))) },
            ],
        );
        let v = ValidGrammar::from_grammar(g).unwrap();
        let spec = v.specializations.get(&ustr("FILE")).unwrap();
        assert_eq!(spec.bash, Some(ustr(r#"compgen -A file "$1""#)));
        assert_eq!(spec.fish, Some(ustr(r#"__fish_complete_path "$1""#)));
        assert_eq!(spec.zsh, None);
    }
}
