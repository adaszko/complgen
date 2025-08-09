use std::{borrow::Borrow, debug_assert, rc::Rc};

use hashbrown::HashMap;
use itertools::Itertools;
use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till, take_until, take_while1},
    character::complete::{char, multispace1, one_of},
    combinator::{fail, map, opt, value, verify},
    error::context,
    multi::{fold_many0, many0},
    sequence::preceded,
};

use crate::{Error, Result};
use ustr::{Ustr, UstrMap, UstrSet, ustr};

use crate::{dfa::DFA, regex::Regex};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DFAId(u32);

impl From<u32> for DFAId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone)]
pub struct DFAInterner {
    map: HashMap<DFA, u32>,
    vec: Vec<DFA>,
}

impl Default for DFAInterner {
    fn default() -> Self {
        Self {
            map: HashMap::default(),
            vec: Default::default(),
        }
    }
}

impl DFAInterner {
    pub fn intern(&mut self, val: DFA) -> DFAId {
        if let Some(&idx) = self.map.get(&val) {
            return DFAId(idx);
        }
        let idx = self.map.len() as u32;
        self.map.insert(val.clone(), idx);
        self.vec.push(val);
        DFAId::from(idx)
    }

    pub fn lookup(&self, idx: DFAId) -> &DFA {
        &self.vec[idx.0 as usize]
    }
}

#[derive(Clone, PartialEq)]
pub enum SubwordCompilationPhase {
    Expr(Rc<Expr>),
    DFA(DFAId),
}

impl std::fmt::Debug for SubwordCompilationPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => {
                f.write_fmt(format_args!(r#"SubwordCompilationPhase::Expr({expr:?})"#))
            }
            Self::DFA(dfa) => f.write_fmt(format_args!(r#"SubwordCompilationPhase::DFA({dfa:?})"#)),
        }
    }
}

// Can't use an arena here until proptest supports non-owned types: https://github.com/proptest-rs/proptest/issues/9
#[derive(Clone, PartialEq)]
pub enum Expr {
    // `--help`
    Terminal(Ustr, Option<Ustr>, usize, ChicSpan), // terminal, optional description, fallback level

    // `--option=argument`
    Subword(SubwordCompilationPhase, usize, ChicSpan),

    // `<PATH>`, `<DIRECTORY>`, etc.
    NontermRef(Ustr, usize, ChicSpan), // name, fallback level

    // `{{{ ls }}}`
    // or
    // `{{{ ls }}}@bash"foo"@fish"bar"`
    Command(Ustr, Option<CmdRegexDecl>, usize, ChicSpan), // command, [regex], fallback level

    // `foo bar`
    Sequence(Vec<Rc<Expr>>),

    // `foo | bar`
    Alternative(Vec<Rc<Expr>>),

    // `[EXPR]`
    Optional(Rc<Expr>),

    // `EXPR...`
    Many1(Rc<Expr>),

    // `(b | build) "Compile the current package"` means the description applies to both `b` and
    // `build`. `(b build) "Compile the current package"` means means the description applies just
    // to `b` (i.e. the first literal)
    DistributiveDescription(Rc<Expr>, Ustr),

    // `foo || bar`
    Fallback(Vec<Rc<Expr>>),
}

// Invariant: At least one field must be Some(_)
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CmdRegexDecl {
    pub bash: Option<Ustr>,
    pub fish: Option<Ustr>,
    pub zsh: Option<Ustr>,
}

#[derive(Copy, Clone)]
pub enum Shell {
    Bash,
    Fish,
    Zsh,
}

impl CmdRegexDecl {
    pub fn matches_anything(&self, shell: Shell) -> bool {
        match shell {
            Shell::Bash => self.bash.is_none(),
            Shell::Fish => self.fish.is_none(),
            Shell::Zsh => self.zsh.is_none(),
        }
    }
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
            Self::Subword(subword, level, ..) => {
                f.write_fmt(format_args!(r#"Rc::new(Subword({subword:?}, {level}))"#))
            }
            Expr::Terminal(term, Some(descr), level, ..) => f.write_fmt(format_args!(
                r#"Rc::new(Terminal(ustr("{term}"), Some(ustr("{}"))), {level})"#,
                descr
            )),
            Expr::Terminal(term, None, level, ..) => f.write_fmt(format_args!(
                r#"Rc::new(Terminal(ustr("{term}"), None, {level}))"#
            )),
            Expr::NontermRef(nonterm, level, _) => f.write_fmt(format_args!(
                r#"Rc::new(Nonterminal(ustr("{nonterm}"), {level}))"#
            )),
            Self::Command(cmd, regex, level, span) => f.write_fmt(format_args!(
                r#"Rc::new(Command(ustr({cmd:?}), {regex:?}, {level}, {span:?}))"#
            )),
            Self::Sequence(arg0) => {
                f.write_fmt(format_args!(r#"Rc::new(Sequence(vec!{:?}))"#, arg0))
            }
            Self::Alternative(arg0) => {
                f.write_fmt(format_args!(r#"Rc::new(Alternative(vec!{:?}))"#, arg0))
            }
            Self::Optional(arg0) => f.write_fmt(format_args!(r#"Rc::new(Optional({:?}))"#, arg0)),
            Self::Many1(arg0) => f.write_fmt(format_args!(r#"Rc::new(Many1({:?}))"#, arg0)),
            Self::DistributiveDescription(expr, descr) => f.write_fmt(format_args!(
                r#"Rc::new(DistributiveDescription({expr:?}, {descr:?}))"#
            )),
            Self::Fallback(arg0) => {
                f.write_fmt(format_args!(r#"Rc::new(Fallback(vec!{:?}))"#, arg0))
            }
        }
    }
}

fn railroad_node_from_expr(expr: Rc<Expr>) -> Box<dyn railroad::Node> {
    match expr.as_ref() {
        Expr::Subword(subword, ..) => {
            let expr = match subword {
                SubwordCompilationPhase::Expr(expr) => expr,
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            let mut seq: Box<railroad::Sequence<Box<dyn railroad::Node>>> = Default::default();
            seq.push(Box::new(railroad::SimpleStart));
            seq.push(railroad_node_from_expr(Rc::clone(expr)));
            seq.push(Box::new(railroad::SimpleEnd));
            seq
        }
        Expr::Terminal(s, ..) => Box::new(railroad::Terminal::new(s.as_str().to_string())),
        Expr::NontermRef(s, ..) => Box::new(railroad::NonTerminal::new(s.as_str().to_string())),
        Expr::Command(s, ..) => Box::new(railroad::Comment::new(s.as_str().to_string())),
        Expr::Sequence(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(Rc::clone(e)))
                .collect();
            Box::new(railroad::Sequence::new(subnodes))
        }
        Expr::Alternative(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(Rc::clone(e)))
                .collect();
            Box::new(railroad::Choice::new(subnodes))
        }
        Expr::Optional(subexpr) => Box::new(railroad::Optional::new(railroad_node_from_expr(
            Rc::clone(subexpr),
        ))),
        Expr::Many1(subexpr) => {
            let subnode = railroad_node_from_expr(Rc::clone(subexpr));
            Box::new(railroad::Repeat::new(subnode, Box::new(railroad::Empty)))
        }
        Expr::DistributiveDescription(subexpr, description) => {
            let inner = railroad_node_from_expr(Rc::clone(subexpr));
            let label = railroad::Comment::new(description.to_string());
            Box::new(railroad::LabeledBox::new(inner, label))
        }
        Expr::Fallback(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(Rc::clone(e)))
                .collect();
            Box::new(railroad::Choice::new(subnodes))
        }
    }
}

pub fn to_railroad_diagram<W: std::io::Write>(
    grammar: &Grammar,
    output: &mut W,
) -> std::result::Result<(), std::io::Error> {
    let mut vertical: railroad::VerticalGrid<Box<dyn railroad::Node>> = Default::default();

    for stmt in &grammar.statements {
        let node: Box<dyn railroad::Node> = match stmt {
            Statement::CallVariant { head, expr } => {
                let mut seq: Box<railroad::Sequence<Box<dyn railroad::Node>>> = Default::default();
                seq.push(Box::new(railroad::Start));
                seq.push(Box::new(railroad::Terminal::new(head.to_string())));
                seq.push(railroad_node_from_expr(Rc::clone(expr)));
                seq.push(Box::new(railroad::End));
                seq
            }
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => {
                let inner = railroad_node_from_expr(Rc::clone(expr));
                let label = if let Some(shell) = shell {
                    format!("{}@{}", symbol, shell)
                } else {
                    symbol.to_string()
                };
                let label = railroad::Comment::new(label);
                Box::new(railroad::LabeledBox::new(inner, label))
            }
        };
        vertical.push(node);
    }

    let mut dia = railroad::Diagram::new(vertical);
    dia.add_element(
        railroad::svg::Element::new("style")
            .set("type", "text/css")
            .text(railroad::DEFAULT_CSS),
    );
    dia.write(output)
}

pub fn to_railroad_diagram_file<P: AsRef<std::path::Path>>(
    grammar: &Grammar,
    path: P,
) -> std::result::Result<(), std::io::Error> {
    let mut file = std::fs::File::create(path)?;
    to_railroad_diagram(grammar, &mut file)?;
    Ok(())
}

use nom_locate::LocatedSpan;
pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, Eq, Hash)]
pub enum ChicSpan {
    Significant {
        line_start: usize,
        start: usize,
        end: usize,
    },
    Dummy, // For tests
}

impl PartialEq for ChicSpan {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Dummy {}, _) => true,
            (_, Self::Dummy {}) => true,
            (
                Self::Significant {
                    line_start: l_line_start,
                    start: l_start,
                    end: l_end,
                },
                Self::Significant {
                    line_start: r_line_start,
                    start: r_start,
                    end: r_end,
                },
            ) => l_line_start == r_line_start && l_start == r_start && l_end == r_end,
        }
    }
}

impl ChicSpan {
    fn new(before: Span, after: Span) -> Self {
        // XXX Doesn't handle tabs
        Self::Significant {
            line_start: before.location_line() as usize - 1,
            start: before.get_column() - 1,
            end: after.get_column() - 1,
        }
    }
}

fn comment(input: Span) -> IResult<Span, Span> {
    let (input, _) = char('#')(input)?;
    let (input, content) = take_till(|c| c == '\n')(input)?;
    Ok((input, content))
}

fn form_feed(input: Span) -> IResult<Span, Span> {
    let (input, _) = char('\u{000C}')(input)?;
    Ok((input, input))
}

fn blanks(input: Span) -> IResult<Span, ()> {
    let (input, _) = alt((multispace1, comment, form_feed))(input)?;
    Ok((input, ()))
}

fn multiblanks0(mut input: Span) -> IResult<Span, ()> {
    while let Ok((rest, _)) = blanks(input) {
        input = rest;
    }
    Ok((input, ()))
}

fn multiblanks1(input: Span) -> IResult<Span, ()> {
    let (input, _) = blanks(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, ()))
}

const ESCAPE_CHARACTER: char = '\\';
const RESERVED_CHARACTERS: &str = r#"()[]{}<>|;""#;

fn is_terminal_char(c: char) -> bool {
    if c == ESCAPE_CHARACTER {
        return false;
    }

    if RESERVED_CHARACTERS.find(c).is_some() {
        return false;
    }

    c.is_ascii_alphanumeric() || c.is_ascii_punctuation()
}

fn terminal(input: Span) -> IResult<Span, String> {
    let (input, term) = escaped_transform(
        take_while1(is_terminal_char),
        ESCAPE_CHARACTER,
        one_of(RESERVED_CHARACTERS),
    )(input)?;
    if term.is_empty() {
        return fail(input);
    }
    Ok((input, term))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
    EscapedWS,
}

fn parse_literal(input: Span) -> IResult<Span, Span> {
    verify(is_not("\"\\"), |s: &Span| !s.is_empty()).parse(input)
}

fn parse_escaped_char(input: Span) -> IResult<Span, char> {
    preceded(
        char('\\'),
        alt((value('\\', char('\\')), value('"', char('"')))),
    )
    .parse(input)
}

fn parse_escaped_whitespace(input: Span) -> IResult<Span, Span> {
    preceded(char('\\'), multispace1).parse(input)
}

fn parse_fragment(input: Span) -> IResult<Span, StringFragment> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))
    .parse(input)
}

fn description_inner(input: Span) -> IResult<Span, String> {
    let (input, inner) = fold_many0(parse_fragment, String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(&String::from_utf8_lossy(s.as_bytes())),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        Span::new(&string).to_string()
    })(input)?;
    Ok((input, inner))
}

fn description(input: Span) -> IResult<Span, String> {
    let (input, _) = char('"')(input)?;
    let (input, descr) = description_inner(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, descr))
}

fn terminal_opt_description_expr(input: Span) -> IResult<Span, Expr> {
    let (after, term) = terminal(input)?;
    let (after, descr) = opt(preceded(multiblanks0, description))(after)?;
    let expr = Expr::Terminal(
        ustr(&term),
        descr.map(|span| ustr(&span)),
        0,
        ChicSpan::new(input, after),
    );
    Ok((after, expr))
}

fn nonterm(input: Span) -> IResult<Span, Span> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}

fn nonterm_expr(input: Span) -> IResult<Span, Expr> {
    let (after, nonterm) = context("nonterminal", nonterm)(input)?;
    let diagnostic_span = ChicSpan::new(input, after);
    Ok((
        after,
        Expr::NontermRef(ustr(nonterm.into_fragment()), 0, diagnostic_span),
    ))
}

fn triple_bracket_command(input: Span) -> IResult<Span, Span> {
    let (input, _) = tag("{{{")(input)?;
    let (input, cmd) = take_until("}}}")(input)?;
    let (input, _) = tag("}}}")(input)?;
    Ok((input, Span::new(cmd.into_fragment().trim())))
}

fn command_expr(input: Span) -> IResult<Span, Expr> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = ChicSpan::new(input, after);
    Ok((
        after,
        Expr::Command(ustr(cmd.into_fragment()), None, 0, command_span),
    ))
}

fn at_shell_regex(input: Span) -> IResult<Span, (String, String)> {
    let (input, _) = char('@')(input)?;
    let (input, shell) = terminal(input)?;
    let shell = match shell.as_ref() {
        "bash" | "fish" | "zsh" => shell,
        _ => return fail(input),
    };
    let (input, regex) = description(input)?;
    Ok((input, (shell, regex)))
}

fn cmd_regex_decl(mut input: Span) -> IResult<Span, CmdRegexDecl> {
    let mut spec = CmdRegexDecl::default();
    while let Ok((rest, (shell, regex))) = at_shell_regex(input) {
        match shell.as_ref() {
            "bash" => spec.bash = Some(ustr(regex.as_ref())),
            "fish" => spec.fish = Some(ustr(regex.as_ref())),
            "zsh" => spec.zsh = Some(ustr(regex.as_ref())),
            _ => unreachable!(),
        }
        input = rest;
    }

    if let CmdRegexDecl {
        bash: None,
        fish: None,
        zsh: None,
    } = spec
    {
        return fail(input);
    }

    Ok((input, spec))
}

fn nontail_command_expr(mut input: Span) -> IResult<Span, Expr> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = ChicSpan::new(input, after);
    input = after;
    let (input, regex_decl) = cmd_regex_decl(input)?;
    Ok((
        input,
        Expr::Command(ustr(cmd.into_fragment()), Some(regex_decl), 0, command_span),
    ))
}

fn optional_expr(input: Span) -> IResult<Span, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::Optional(Rc::new(expr))))
}

fn parenthesized_expr(input: Span) -> IResult<Span, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn many1_tag(input: Span) -> IResult<Span, ()> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("...")(input)?;
    Ok((input, ()))
}

fn unary_expr(input: Span) -> IResult<Span, Expr> {
    let (input, e) = alt((
        nonterm_expr,
        optional_expr,
        parenthesized_expr,
        nontail_command_expr,
        command_expr,
        terminal_opt_description_expr,
    ))(input)?;

    if let Ok((input, ())) = many1_tag(input) {
        return Ok((input, Expr::Many1(Rc::new(e))));
    }

    Ok((input, e))
}

fn subword_sequence_expr(input: Span) -> IResult<Span, Expr> {
    let (mut after, left) = unary_expr(input)?;
    let mut factors: Vec<Expr> = vec![left];
    while let Ok((rest, right)) = unary_expr(after) {
        factors.push(right);
        after = rest;
    }
    let result = if factors.len() == 1 {
        factors.into_iter().next().unwrap()
    } else {
        let flattened_factors: Vec<Rc<Expr>> = factors
            .into_iter()
            .map(|e| flatten_expr(Rc::new(e)))
            .collect();
        let e = Expr::Sequence(flattened_factors);
        let span = ChicSpan::new(input, after);
        Expr::Subword(SubwordCompilationPhase::Expr(Rc::new(e)), 0, span)
    };
    Ok((after, result))
}

fn subword_sequence_expr_opt_description(input: Span) -> IResult<Span, Expr> {
    let (input, expr) = subword_sequence_expr(input)?;
    let (input, description) = opt(preceded(multiblanks0, description))(input)?;
    let result = match description {
        Some(descr) => Expr::DistributiveDescription(Rc::new(expr), ustr(&descr)),
        None => expr,
    };
    Ok((input, result))
}

fn sequence_expr(input: Span) -> IResult<Span, Expr> {
    let (mut input, left) = subword_sequence_expr_opt_description(input)?;
    let mut factors: Vec<Expr> = vec![left];
    while let Ok((rest, right)) =
        preceded(multiblanks1, subword_sequence_expr_opt_description)(input)
    {
        factors.push(right);
        input = rest;
    }
    let result = if factors.len() == 1 {
        factors.drain(..).next().unwrap()
    } else {
        Expr::Sequence(factors.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn alternative_expr(input: Span) -> IResult<Span, Expr> {
    fn do_alternative_expr(input: Span) -> IResult<Span, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, _) = char('|')(input)?;
        let (input, _) = multiblanks0(input)?;
        let (input, right) = sequence_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = sequence_expr(input)?;
    let mut elems: Vec<Expr> = vec![left];
    while let Ok((rest, right)) = do_alternative_expr(input) {
        elems.push(right);
        input = rest;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        Expr::Alternative(elems.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn fallback_expr(input: Span) -> IResult<Span, Expr> {
    fn do_fallback_expr(input: Span) -> IResult<Span, Expr> {
        let (input, _) = multiblanks0(input)?;
        let (input, _) = tag("||")(input)?;
        let (input, _) = multiblanks0(input)?;
        let (input, right) = alternative_expr(input)?;
        Ok((input, right))
    }

    let (mut input, left) = alternative_expr(input)?;
    let mut fallbacks: Vec<Expr> = vec![left];
    while let Ok((rest, right)) = do_fallback_expr(input) {
        fallbacks.push(right);
        input = rest;
    }
    let result = if fallbacks.len() == 1 {
        fallbacks.drain(..).next().unwrap()
    } else {
        Expr::Fallback(fallbacks.into_iter().map(Rc::new).collect())
    };
    Ok((input, result))
}

fn expr(input: Span) -> IResult<Span, Expr> {
    fallback_expr(input)
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

fn call_variant(input: Span) -> IResult<Span, Statement> {
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

fn nonterm_specialization(input: Span) -> IResult<Span, (Span, Span)> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">@")(input)?;
    let (input, _) = char('@')(input)?;
    let (input, shell) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, (name, shell)))
}

fn nonterm_def(input: Span) -> IResult<Span, (Span, Option<Span>)> {
    if let Ok((input, (name, shell))) = nonterm_specialization(input) {
        return Ok((input, (name, Some(shell))));
    }
    if let Ok((input, name)) = nonterm(input) {
        return Ok((input, (name, None)));
    }
    fail(input)
}

fn nonterm_def_statement(input: Span) -> IResult<Span, Statement> {
    let (input, (name, shell)) = nonterm_def(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let stmt = Statement::NonterminalDefinition {
        symbol: ustr(name.into_fragment()),
        shell: shell.map(|span| ustr(span.into_fragment())),
        expr: Rc::new(e),
    };

    Ok((input, stmt))
}

fn statement(input: Span) -> IResult<Span, Statement> {
    let (input, stmt) = alt((call_variant, nonterm_def_statement))(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, stmt))
}

fn grammar(input: Span) -> IResult<Span, Vec<Statement>> {
    let (input, _) = multiblanks0(input)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, statements))
}

#[derive(Debug, PartialEq, Clone)]
pub struct Grammar {
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ValidGrammar {
    pub command: Ustr,
    pub expr: Rc<Expr>,
    pub specializations: UstrMap<Specialization>,
    pub undefined_nonterminals: UstrSet,
    pub unused_nonterminals: UstrSet,
    pub subdfa_interner: DFAInterner,
}

fn make_specializations_map(statements: &[Statement]) -> Result<UstrMap<Specialization>> {
    let mut specializations: UstrMap<Specialization> = Default::default();
    for definition in statements {
        let (name, shell, expr) = match definition {
            Statement::NonterminalDefinition {
                symbol,
                shell: Some(shell),
                expr,
            } => (*symbol, shell, expr),
            Statement::NonterminalDefinition { shell: None, .. } => continue,
            Statement::CallVariant { .. } => continue,
        };
        let command = match expr.borrow() {
            Expr::Command(cmd, ..) => cmd,
            _ => return Err(Error::NonCommandSpecialization(name, Some(*shell))),
        };
        let known_shell = matches!(shell.as_str(), "bash" | "fish" | "zsh");
        if !known_shell {
            return Err(Error::UnknownShell(*shell));
        }
        let spec = specializations.entry(name).or_default();
        match shell.as_str() {
            "bash" => {
                if spec.bash.is_some() {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.bash = Some(*command);
            }
            "fish" => {
                if spec.fish.is_some() {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.fish = Some(*command);
            }
            "zsh" => {
                if spec.zsh.is_some() {
                    return Err(Error::DuplicateNonterminalDefinition(name, Some(*shell)));
                }
                spec.zsh = Some(*command);
            }
            _ => unreachable!(),
        }
    }

    for definition in statements {
        let (name, expr) = match definition {
            Statement::NonterminalDefinition {
                symbol,
                shell: None,
                expr,
            } => (symbol, expr),
            _ => continue,
        };
        let Some(spec) = specializations.get_mut(name) else {
            continue;
        };
        let Expr::Command(command, ..) = expr.borrow() else {
            return Err(Error::NonCommandSpecialization(*name, None));
        };
        if spec.generic.is_some() {
            return Err(Error::DuplicateNonterminalDefinition(*name, None));
        }
        spec.generic = Some(*command);
    }

    specializations
        .entry(ustr("PATH"))
        .or_insert_with(|| Specialization {
            bash: Some(ustr(r#"compgen -A file "$1""#)),
            fish: Some(ustr(r#"__fish_complete_path "$1""#)),
            zsh: Some(ustr("_path_files")),
            generic: None,
        });

    specializations
        .entry(ustr("DIRECTORY"))
        .or_insert_with(|| Specialization {
            bash: Some(ustr(r#"compgen -A directory "$1""#)),
            fish: Some(ustr(r#"__fish_complete_directories "$1""#)),
            zsh: Some(ustr("_path_files -/")),
            generic: None,
        });

    specializations
        .entry(ustr("PID"))
        .or_insert_with(|| Specialization {
            bash: None,
            fish: Some(ustr(r#"__fish_complete_pids"#)),
            zsh: Some(ustr(r#"_pids"#)),
            generic: None,
        });

    specializations
        .entry(ustr("USER"))
        .or_insert_with(|| Specialization {
            bash: Some(ustr(
                r#"compgen -A user | while read line; do echo "$line "; done"#,
            )),
            fish: Some(ustr(r#"__fish_complete_users"#)),
            zsh: Some(ustr(r#"_users"#)),
            generic: None,
        });

    specializations
        .entry(ustr("GROUP"))
        .or_insert_with(|| Specialization {
            bash: Some(ustr(
                r#"compgen -A group | while read line; do echo "$line "; done"#,
            )),
            fish: Some(ustr(r#"__fish_complete_groups"#)),
            zsh: Some(ustr(r#"_groups"#)),
            generic: None,
        });

    specializations
        .entry(ustr("HOST"))
        .or_insert_with(|| Specialization {
            bash: Some(ustr(
                r#"compgen -A hostname | while read line; do echo "$line "; done"#,
            )),
            fish: Some(ustr(r#"__fish_complete_hostnames"#)),
            zsh: Some(ustr(r#"_hosts"#)),
            generic: None,
        });

    specializations
        .entry(ustr("INTERFACE"))
        .or_insert_with(|| Specialization {
            bash: None,
            fish: Some(ustr(r#"__fish_complete_interfaces"#)),
            zsh: Some(ustr(r#"_net_interfaces"#)),
            generic: None,
        });

    specializations
        .entry(ustr("PACKAGE"))
        .or_insert_with(|| Specialization {
            bash: None,
            fish: Some(ustr(r#"__fish_complete_packages"#)),
            zsh: None,
            generic: None,
        });

    Ok(specializations)
}

// Used in subword mode, when we know there won't be any sub-DFAs needed, just one big one.
// Substitutes Expr::Subword with Expr to make AST simpler to process.
fn flatten_expr(expr: Rc<Expr>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) | Expr::NontermRef(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Subword(child, ..) => {
            let child = match child {
                SubwordCompilationPhase::Expr(e) => Rc::clone(e),
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            let new_child = flatten_expr(Rc::clone(&child));
            if Rc::ptr_eq(&child, &new_child) {
                child
            } else {
                new_child
            }
        }
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| flatten_expr(Rc::clone(e)))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| flatten_expr(Rc::clone(e)))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = flatten_expr(Rc::clone(child));
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = flatten_expr(Rc::clone(child));
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription(child, description) => {
            let new_child = flatten_expr(Rc::clone(child));
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::DistributiveDescription(new_child, *description))
            }
        }
        Expr::Fallback(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| flatten_expr(Rc::clone(e)))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Fallback(new_children))
            }
        }
    }
}

fn compile_subword_exprs(
    expr: Rc<Expr>,
    specs: &UstrMap<Specialization>,
    shell: Shell,
    subdfa_interner: &mut DFAInterner,
) -> Result<Rc<Expr>> {
    let retval = match expr.as_ref() {
        Expr::Subword(subword_expr, fallback_level, span) => {
            let subword_expr = match subword_expr {
                SubwordCompilationPhase::Expr(e) => Rc::clone(e),
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            let subword_expr = flatten_expr(subword_expr);
            let regex = Regex::from_expr(&subword_expr, specs).unwrap();
            regex.ensure_ambiguous_inputs_tail_only_subword(shell)?;
            regex.check_clashing_variants()?;
            let dfa = DFA::from_regex(&regex, DFAInterner::default());
            let dfa = dfa.minimize();
            let subdfaid = subdfa_interner.intern(dfa);
            Rc::new(Expr::Subword(
                SubwordCompilationPhase::DFA(subdfaid),
                *fallback_level,
                span.clone(),
            ))
        }
        Expr::Terminal(..) | Expr::NontermRef(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| compile_subword_exprs(Rc::clone(e), specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| compile_subword_exprs(Rc::clone(e), specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = compile_subword_exprs(Rc::clone(child), specs, shell, subdfa_interner)?;
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = compile_subword_exprs(Rc::clone(child), specs, shell, subdfa_interner)?;
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription(..) => unreachable!(
            "DistributiveDescription Expr type should have been erased by the time subwords are being compiled"
        ),
        Expr::Fallback(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| compile_subword_exprs(Rc::clone(e), specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Fallback(new_children))
            }
        }
    };
    Ok(retval)
}

// Move descriptions to their corresponding terminals.
fn do_distribute_descriptions(expr: Rc<Expr>, description: &mut Option<Ustr>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::DistributiveDescription(child, descr) => {
            let new_child = do_distribute_descriptions(Rc::clone(child), &mut Some(*descr));
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&child)
            } else {
                new_child
            }
        }
        Expr::Terminal(term, None, level, span) if description.is_some() => {
            let result = Rc::new(Expr::Terminal(*term, *description, *level, span.clone()));
            *description = None; // spend it
            result
        }
        Expr::Terminal(..) => Rc::clone(&expr),
        Expr::NontermRef(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| do_distribute_descriptions(Rc::clone(e), description))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| do_distribute_descriptions(Rc::clone(e), &mut description.clone()))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = do_distribute_descriptions(Rc::clone(child), description);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = do_distribute_descriptions(Rc::clone(child), description);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Many1(new_child))
            }
        }
        Expr::Subword(SubwordCompilationPhase::Expr(child), fallback_level, span) => {
            let new_child = do_distribute_descriptions(Rc::clone(&child), description);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Subword(
                    SubwordCompilationPhase::Expr(new_child),
                    *fallback_level,
                    span.clone(),
                ))
            }
        }
        Expr::Subword(SubwordCompilationPhase::DFA(_), ..) => unreachable!(),
        Expr::Fallback(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| do_distribute_descriptions(Rc::clone(e), description))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Fallback(new_children))
            }
        }
    }
}

fn distribute_descriptions(expr: Rc<Expr>) -> Rc<Expr> {
    let mut description = None;
    do_distribute_descriptions(expr, &mut description)
}

// Propagate fallback levels of fallback alternatives down to literals.
fn do_propagate_fallback_levels(expr: Rc<Expr>, fallback_level: usize) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(_, _, level, _) if *level == fallback_level => Rc::clone(&expr),
        Expr::Terminal(term, descr, _, span) => {
            Rc::new(Expr::Terminal(*term, *descr, fallback_level, span.clone()))
        }
        Expr::Fallback(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .enumerate()
                .map(|(i, e)| do_propagate_fallback_levels(Rc::clone(e), i))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Fallback(new_children))
            }
        }
        Expr::NontermRef(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Sequence(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(Rc::clone(e), fallback_level))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<Rc<Expr>> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(Rc::clone(e), fallback_level))
                .collect();
            if children
                .iter()
                .zip_eq(new_children.iter())
                .all(|(left, right)| Rc::ptr_eq(left, right))
            {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = do_propagate_fallback_levels(Rc::clone(child), fallback_level);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = do_propagate_fallback_levels(Rc::clone(child), fallback_level);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Many1(new_child))
            }
        }
        Expr::Subword(SubwordCompilationPhase::Expr(child), _, span) => {
            let new_child = do_propagate_fallback_levels(Rc::clone(&child), fallback_level);
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Subword(
                    SubwordCompilationPhase::Expr(new_child),
                    fallback_level,
                    span.clone(),
                ))
            }
        }
        Expr::Subword(SubwordCompilationPhase::DFA(..), ..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!(),
    }
}

fn propagate_fallback_levels(expr: Rc<Expr>) -> Rc<Expr> {
    do_propagate_fallback_levels(expr, 0)
}

impl ValidGrammar {
    pub fn from_grammar(grammar: Grammar, shell: Shell) -> Result<Self> {
        let command = {
            let mut commands: Vec<Ustr> = grammar
                .statements
                .iter()
                .filter_map(|v| match v {
                    Statement::CallVariant { head: lhs, .. } => Some(*lhs),
                    Statement::NonterminalDefinition { .. } => None,
                })
                .collect();

            if commands.is_empty() {
                return Err(Error::MissingCallVariants);
            }

            commands.sort_unstable();
            commands.dedup();

            if commands.len() > 1 {
                return Err(Error::VaryingCommandNames(commands.into_iter().collect()));
            }
            commands[0]
        };

        let expr = {
            let call_variants: Vec<Rc<Expr>> = grammar
                .statements
                .iter()
                .filter_map(|v| match v {
                    Statement::CallVariant { expr: rhs, .. } => Some(rhs.clone()),
                    Statement::NonterminalDefinition { .. } => None,
                })
                .collect();

            if call_variants.len() == 1 {
                Rc::clone(&call_variants[0])
            } else {
                Rc::new(Expr::Alternative(call_variants))
            }
        };

        let mut nonterminal_definitions: UstrMap<Rc<Expr>> = {
            let mut nonterminal_definitions: UstrMap<Rc<Expr>> = Default::default();
            for definition in &grammar.statements {
                let (symbol, expr) = match definition {
                    Statement::NonterminalDefinition {
                        symbol,
                        expr,
                        shell: None,
                    } => (*symbol, expr),
                    _ => continue,
                };
                if nonterminal_definitions.contains_key(&symbol) {
                    return Err(Error::DuplicateNonterminalDefinition(symbol, None));
                }
                let expr = distribute_descriptions(Rc::clone(&expr));
                nonterminal_definitions.insert(symbol, expr);
            }
            nonterminal_definitions
        };

        let expr = distribute_descriptions(expr);

        let specializations = make_specializations_map(&grammar.statements)?;

        let mut unused_nonterminals: UstrSet = nonterminal_definitions.keys().copied().collect();

        for nonterminal in get_nonterminals_resolution_order(&nonterminal_definitions)? {
            let e = Rc::clone(nonterminal_definitions.get(&nonterminal).unwrap());
            *nonterminal_definitions.get_mut(&nonterminal).unwrap() = resolve_nonterminals(
                e,
                &nonterminal_definitions,
                &specializations,
                &mut unused_nonterminals,
            );
        }

        check_subword_spaces(Rc::clone(&expr), &nonterminal_definitions)?;

        let expr = resolve_nonterminals(
            expr,
            &nonterminal_definitions,
            &specializations,
            &mut unused_nonterminals,
        );

        let expr = propagate_fallback_levels(expr);

        let undefined_nonterminals = {
            let mut nonterms = get_expression_nonterminals(Rc::clone(&expr));
            nonterms.retain(|n| !specializations.contains_key(n));
            nonterms
        };

        let mut subdfa_interner = DFAInterner::default();
        let expr = compile_subword_exprs(
            Rc::clone(&expr),
            &specializations,
            shell,
            &mut subdfa_interner,
        )?;

        let g = ValidGrammar {
            command,
            expr,
            undefined_nonterminals,
            specializations,
            unused_nonterminals,
            subdfa_interner,
        };
        Ok(g)
    }
}

fn expr_get_head(expr: Rc<Expr>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) => expr,
        Expr::NontermRef(..) => expr,
        Expr::Command(..) => expr,
        Expr::Alternative(..) => expr,
        Expr::Fallback(..) => expr,
        Expr::Optional(..) => expr,
        Expr::Many1(..) => expr,
        Expr::Sequence(children) => expr_get_head(children.first().unwrap().clone()),
        Expr::Subword(..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
    }
}

fn expr_get_tail(expr: Rc<Expr>) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) => expr,
        Expr::NontermRef(..) => expr,
        Expr::Command(..) => expr,
        Expr::Alternative(..) => expr,
        Expr::Fallback(..) => expr,
        Expr::Optional(..) => expr,
        Expr::Many1(..) => expr,
        Expr::Sequence(children) => expr_get_head(children.last().unwrap().clone()),
        Expr::Subword(..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
    }
}

fn check_subword_spaces(expr: Rc<Expr>, nonterms: &UstrMap<Rc<Expr>>) -> Result<()> {
    let mut nonterm_expn_trace: Vec<ChicSpan> = Default::default();
    do_check_subword_spaces(expr, nonterms, &mut nonterm_expn_trace, false)
}

// Disallows spaces in subword expressions, e.g.
//
// aerc :<COMMAND>;
// <COMMAND> ::= quit -f;
//
// On deeply nested <NONTERM>s, this can lead to [surprising spaces
// removal](https://github.com/adaszko/complgen/issues/63) so forbid it completely.
fn do_check_subword_spaces(
    expr: Rc<Expr>,
    nonterms: &UstrMap<Rc<Expr>>,
    nonterm_expn_trace: &mut Vec<ChicSpan>,
    within_subword: bool,
) -> Result<()> {
    match expr.as_ref() {
        Expr::Sequence(children) if within_subword => {
            for child in children {
                do_check_subword_spaces(
                    Rc::clone(&child),
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            // Error out on two adjacent Expr::Terminal()s
            for pair in children.windows(2) {
                let [left, right] = pair else { unreachable!() };
                match (
                    expr_get_tail(Rc::clone(&left)).as_ref(),
                    expr_get_head(Rc::clone(&right)).as_ref(),
                ) {
                    (Expr::Terminal(_, _, _, left_span), Expr::Terminal(_, _, _, right_span)) => {
                        return Err(Error::SubwordSpaces(
                            left_span.to_owned(),
                            right_span.to_owned(),
                            nonterm_expn_trace.to_owned(),
                        ));
                    }
                    _ => {}
                }
            }
            Ok(())
        }
        Expr::Sequence(children) => {
            for child in children {
                do_check_subword_spaces(
                    Rc::clone(&child),
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Terminal(..) => Ok(()),
        Expr::NontermRef(name, _, span) => {
            let Some(expn) = nonterms.get(name) else {
                return Ok(());
            };
            nonterm_expn_trace.push(span.clone());
            do_check_subword_spaces(
                Rc::clone(&expn),
                nonterms,
                nonterm_expn_trace,
                within_subword,
            )?;
            nonterm_expn_trace.pop();
            Ok(())
        }
        Expr::Command(..) => Ok(()),
        Expr::Subword(SubwordCompilationPhase::Expr(child), ..) => {
            do_check_subword_spaces(Rc::clone(&child), nonterms, nonterm_expn_trace, true)
        }
        Expr::Subword(SubwordCompilationPhase::DFA(..), ..) => {
            unreachable!("wrong compilation phases order")
        }
        Expr::Alternative(children) => {
            for child in children {
                do_check_subword_spaces(
                    Rc::clone(&child),
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Fallback(children) => {
            for child in children {
                do_check_subword_spaces(
                    Rc::clone(&child),
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Optional(child) => do_check_subword_spaces(
            Rc::clone(&child),
            nonterms,
            nonterm_expn_trace,
            within_subword,
        ),
        Expr::Many1(child) => do_check_subword_spaces(
            Rc::clone(&child),
            nonterms,
            nonterm_expn_trace,
            within_subword,
        ),
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
    }
}

fn resolve_nonterminals(
    expr: Rc<Expr>,
    vars: &UstrMap<Rc<Expr>>,
    specializations: &UstrMap<Specialization>,
    unused_nonterminals: &mut UstrSet,
) -> Rc<Expr> {
    match expr.as_ref() {
        Expr::Terminal(..) | Expr::Command(..) => Rc::clone(&expr),
        Expr::Subword(child, fallback_level, span) => {
            let child = match child {
                SubwordCompilationPhase::Expr(e) => Rc::clone(e),
                SubwordCompilationPhase::DFA(..) => unreachable!(),
            };
            let new_child = resolve_nonterminals(
                Rc::clone(&child),
                vars,
                specializations,
                unused_nonterminals,
            );
            if Rc::ptr_eq(&child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Subword(
                    SubwordCompilationPhase::Expr(new_child),
                    *fallback_level,
                    span.clone(),
                ))
            }
        }
        Expr::NontermRef(name, ..) => {
            if specializations.contains_key(name) {
                // Specialized nonterminals are resolved when the target shell is known, not earlier.
                return Rc::clone(&expr);
            }
            match vars.get(name) {
                Some(replacement) => {
                    unused_nonterminals.remove(name);
                    Rc::clone(replacement)
                }
                None => Rc::clone(&expr),
            }
        }
        Expr::Sequence(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(
                    Rc::clone(child),
                    vars,
                    specializations,
                    unused_nonterminals,
                );
                if !Rc::ptr_eq(child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Sequence(new_children))
            } else {
                Rc::clone(&expr)
            }
        }
        Expr::Alternative(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(
                    Rc::clone(child),
                    vars,
                    specializations,
                    unused_nonterminals,
                );
                if !Rc::ptr_eq(child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Alternative(new_children))
            } else {
                Rc::clone(&expr)
            }
        }
        Expr::Optional(child) => {
            let new_child =
                resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child =
                resolve_nonterminals(Rc::clone(child), vars, specializations, unused_nonterminals);
            if Rc::ptr_eq(child, &new_child) {
                Rc::clone(&expr)
            } else {
                Rc::new(Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription(..) => unreachable!(
            "Expr::DistributiveDescription should have been erased by the time nonterminals are being resolved"
        ),
        Expr::Fallback(children) => {
            let mut new_children: Vec<Rc<Expr>> = Default::default();
            let mut any_child_replaced = false;
            for child in children {
                let new_child = resolve_nonterminals(
                    Rc::clone(child),
                    vars,
                    specializations,
                    unused_nonterminals,
                );
                if !Rc::ptr_eq(child, &new_child) {
                    any_child_replaced = true;
                }
                new_children.push(new_child);
            }
            if any_child_replaced {
                Rc::new(Expr::Fallback(new_children))
            } else {
                Rc::clone(&expr)
            }
        }
    }
}

fn do_get_expression_nonterminals(expr: Rc<Expr>, deps: &mut UstrSet) {
    match expr.as_ref() {
        Expr::Terminal(..) | Expr::Command(..) => {}
        Expr::Subword(subexpr, ..) => {
            let subexpr = match subexpr {
                SubwordCompilationPhase::Expr(e) => Rc::clone(e),
                SubwordCompilationPhase::DFA(..) => unreachable!(),
            };
            do_get_expression_nonterminals(Rc::clone(&subexpr), deps);
        }
        Expr::NontermRef(varname, ..) => {
            deps.insert(*varname);
        }
        Expr::Sequence(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(child), deps);
            }
        }
        Expr::Alternative(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(child), deps);
            }
        }
        Expr::Optional(child) => {
            do_get_expression_nonterminals(Rc::clone(child), deps);
        }
        Expr::Many1(child) => {
            do_get_expression_nonterminals(Rc::clone(child), deps);
        }
        Expr::DistributiveDescription(..) => unreachable!(
            "Expr::DistributiveDescription should have been erased by the time nonterminals are being collected"
        ),
        Expr::Fallback(children) => {
            for child in children {
                do_get_expression_nonterminals(Rc::clone(child), deps);
            }
        }
    }
}

fn get_expression_nonterminals(expr: Rc<Expr>) -> UstrSet {
    let mut result: UstrSet = Default::default();
    do_get_expression_nonterminals(expr, &mut result);
    result
}

pub fn get_not_depended_on_nonterminals(dependency_graph: &UstrMap<UstrSet>) -> UstrSet {
    let num_depending_nonterminals = {
        let mut num_depending_nonterminals: UstrMap<usize> =
            dependency_graph.keys().map(|vertex| (*vertex, 0)).collect();
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

fn traverse_nonterminal_dependencies_dfs(
    vertex: Ustr,
    graph: &UstrMap<UstrSet>,
    path: &mut Vec<Ustr>,
    visited: &mut UstrSet,
    result: &mut Vec<Ustr>,
) -> Result<()> {
    visited.insert(vertex);
    let dummy = UstrSet::default();
    for child in graph.get(&vertex).unwrap_or(&dummy) {
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
fn get_nonterminals_resolution_order(
    nonterminal_definitions: &UstrMap<Rc<Expr>>,
) -> Result<Vec<Ustr>> {
    if nonterminal_definitions.is_empty() {
        return Ok(Vec::default());
    }

    let mut dependency_graph: UstrMap<UstrSet> = Default::default();
    for (varname, expr) in nonterminal_definitions {
        let mut referenced_nonterminals = get_expression_nonterminals(Rc::clone(expr));
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
        traverse_nonterminal_dependencies_dfs(
            vertex,
            &dependency_graph,
            &mut path,
            &mut visited,
            &mut result,
        )?;
        result.push(vertex);
        debug_assert!(path.is_empty());
    }

    // Filter out nonterminals that don't depend on any other as they are already fully resolved.
    result.retain(|vertex| {
        dependency_graph
            .get(vertex)
            .map(|children| !children.is_empty())
            .unwrap_or(true)
    });

    log::debug!("nonterminals expansion order: {:?}", result);
    Ok(result)
}

impl Grammar {
    pub fn parse(input_before: &str) -> std::result::Result<Self, chic::Error> {
        let (input_after, statements) = match grammar(Span::new(input_before)).finish() {
            Ok((input, statements)) => (input, statements),
            Err(e) => {
                let line_start = e.input.location_line() as usize - 1;
                let start = e.input.get_column() - 1;
                let end = start + 1;
                let error = chic::Error::new(e.to_string()).error(
                    line_start,
                    start,
                    end,
                    input_before.lines().nth(line_start).unwrap(),
                    "",
                );
                return Err(error);
            }
        };

        if !input_after.is_empty() {
            let line_start = input_after.location_line() as usize - 1;
            let start = input_after.get_column() - 1;
            let end = start + 1;
            let error = chic::Error::new("Parsing failed").error(
                line_start,
                start,
                end,
                input_before.lines().nth(line_start).unwrap(),
                "",
            );
            return Err(error);
        }

        let g = Grammar { statements };

        Ok(g)
    }
}

#[cfg(test)]
pub mod tests {
    use proptest::prelude::*;
    use proptest::{strategy::BoxedStrategy, test_runner::TestRng};
    use std::ops::Rem;
    use std::rc::Rc;
    use ustr::ustr as u;

    use super::*;
    use Expr::*;

    impl ChicSpan {
        pub fn dummy() -> Self {
            Self::Dummy {}
        }
    }

    fn arb_literal(inputs: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..inputs.len())
            .prop_map(move |index| {
                Rc::new(Terminal(ustr(&inputs[index]), None, 0, ChicSpan::Dummy))
            })
            .boxed()
    }

    fn arb_nonterminal(nonterminals: Rc<Vec<Ustr>>) -> BoxedStrategy<Rc<Expr>> {
        (0..nonterminals.len())
            .prop_map(move |index| {
                Rc::new(NontermRef(ustr(&nonterminals[index]), 0, ChicSpan::dummy()))
            })
            .boxed()
    }

    fn arb_optional(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth - 1, max_width)
            .prop_map(|e| Rc::new(Optional(e)))
            .boxed()
    }

    fn arb_many1(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<Rc<Expr>> {
        arb_expr(inputs, nonterminals, remaining_depth - 1, max_width)
            .prop_map(|e| Rc::new(Many1(e)))
            .boxed()
    }

    fn arb_sequence(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                prop::collection::vec(e, width).prop_map(|v| Rc::new(Sequence(v)))
            })
            .boxed()
    }

    fn arb_alternative(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<Rc<Expr>> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                prop::collection::vec(e, width).prop_map(|v| Rc::new(Alternative(v)))
            })
            .boxed()
    }

    pub fn arb_expr(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<Rc<Expr>> {
        if remaining_depth <= 1 {
            prop_oneof![
                arb_literal(Rc::clone(&inputs)),
                arb_nonterminal(nonterminals),
            ]
            .boxed()
        } else {
            prop_oneof![
                arb_literal(inputs.clone()),
                arb_nonterminal(nonterminals.clone()),
                arb_optional(
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_many1(
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_sequence(
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_alternative(inputs, nonterminals, remaining_depth, max_width),
            ]
            .boxed()
        }
    }

    pub fn do_arb_match(e: Rc<Expr>, rng: &mut TestRng, max_width: usize, output: &mut Vec<Ustr>) {
        match e.as_ref() {
            Terminal(s, _, _, _) => output.push(*s),
            Subword(sw, ..) => {
                let e = match sw {
                    SubwordCompilationPhase::Expr(e) => e,
                    SubwordCompilationPhase::DFA(_) => unreachable!(),
                };
                let mut out: Vec<Ustr> = Default::default();
                do_arb_match(Rc::clone(e), rng, max_width, &mut out);
                let joined = out.into_iter().join("");
                output.push(ustr(&joined));
            }
            NontermRef(..) => output.push(ustr("anything")),
            Command(..) => output.push(ustr("anything")),
            Sequence(v) => {
                for subexpr in v {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            }
            Alternative(v) => {
                let chosen_alternative =
                    usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(Rc::clone(&v[chosen_alternative]), rng, max_width, output);
            }
            Optional(subexpr) => {
                if rng.next_u64() % 2 == 0 {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            }
            Many1(subexpr) => {
                let n = rng.next_u64();
                let chosen_len = n % u64::try_from(max_width).unwrap() + 1;
                for _ in 0..chosen_len {
                    do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                }
            }
            DistributiveDescription(subexpr, description) => {
                do_arb_match(Rc::clone(&subexpr), rng, max_width, output);
                output.push(ustr(&format!(r#""{description}""#)));
            }
            Fallback(v) => {
                let chosen_alternative =
                    usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(Rc::clone(&v[chosen_alternative]), rng, max_width, output);
            }
        }
    }

    pub fn arb_match(e: Rc<Expr>, mut rng: TestRng, max_width: usize) -> (Rc<Expr>, Vec<Ustr>) {
        let mut output: Vec<Ustr> = Default::default();
        do_arb_match(Rc::clone(&e), &mut rng, max_width, &mut output);
        (e, output)
    }

    // Produce an arbitrary sequence matching `e`.
    pub fn arb_expr_match(
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<(Rc<Expr>, Vec<Ustr>)> {
        arb_expr(inputs, nonterminals, remaining_depth, max_width)
            .prop_perturb(move |e, rng| arb_match(e, rng, max_width))
            .boxed()
    }

    #[test]
    fn parses_subword_expr() {
        const INPUT: &str = r#"--color=<WHEN>"#;
        let (s, e) = subword_sequence_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Expr::Subword(
                SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                    Rc::new(Terminal(ustr("--color="), None, 0, ChicSpan::Dummy)),
                    Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                ]))),
                0,
                ChicSpan::Dummy,
            )
        );
    }

    #[test]
    fn parses_prefix_description_expr() {
        const INPUT: &str = r#"--color=<WHEN> "use markers to highlight the matching strings""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            DistributiveDescription(
                Rc::new(Subword(
                    SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr("--color="), None, 0, ChicSpan::Dummy)),
                        Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                    ]))),
                    0,
                    ChicSpan::Dummy,
                )),
                ustr("use markers to highlight the matching strings")
            )
        );
    }

    #[test]
    fn parses_option_argument_alternative_description_expr() {
        const INPUT: &str =
            r#"(--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            DistributiveDescription(
                Rc::new(Alternative(vec![
                    Rc::new(Subword(
                        SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                            Rc::new(Terminal(ustr("--color="), None, 0, ChicSpan::Dummy)),
                            Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                        ]))),
                        0,
                        ChicSpan::Dummy,
                    )),
                    Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr("--color"), None, 0, ChicSpan::Dummy)),
                        Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                    ]))
                ])),
                ustr("use markers to highlight the matching strings")
            )
        );
    }

    #[test]
    fn parses_word_terminal() {
        const INPUT: &str = r#"foo.bar"#;
        let (s, e) = terminal_opt_description_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, Terminal(u("foo.bar"), None, 0, ChicSpan::Dummy));
    }

    #[test]
    fn parses_short_option_terminal() {
        const INPUT: &str = r#"-f"#;
        let (s, e) = terminal_opt_description_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, Terminal(u("-f"), None, 0, ChicSpan::Dummy));
    }

    #[test]
    fn parses_long_option_terminal() {
        const INPUT: &str = r#"--foo"#;
        let (s, e) = terminal_opt_description_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, Terminal(u("--foo"), None, 0, ChicSpan::Dummy));
    }

    #[test]
    fn parses_symbol() {
        const INPUT: &str = "<FILE>";
        let (s, e) = nonterm_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, NontermRef(u("FILE"), 0, ChicSpan::dummy()));
    }

    #[test]
    fn parses_command() {
        const INPUT: &str = "{{{ rustup toolchain list | cut -d' ' -f1 }}}";
        let (s, e) = command_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert!(
            matches!(e, Command(s, None, 0, ..) if s == "rustup toolchain list | cut -d' ' -f1")
        );
    }

    #[test]
    fn parses_nontail_command() {
        const INPUT: &str = r#"{{{ foo }}}@bash"bar""#;
        let (s, e) = nontail_command_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty(), "{:?}", s.fragment());
        assert!(matches!(e, Command(s, _, 0, ..) if s == "foo"));
    }

    #[test]
    fn parses_triple_brackets_command() {
        const INPUT: &str = "{{{ rad patch list | awk '{print $3}' | grep . | grep -vw ID }}}";
        let (s, e) = command_expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert!(
            matches!(e, Command(s, None, 0, ..) if s == "rad patch list | awk '{print $3}' | grep . | grep -vw ID")
        );
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo>]";
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Optional(Rc::new(NontermRef(u("foo"), 0, ChicSpan::dummy())))
        );
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Many1(Rc::new(NontermRef(u("foo"), 0, ChicSpan::dummy())))
        );
    }

    #[test]
    fn parses_sequence_expr() {
        const INPUT: &str = "<first-symbol> <second symbol>";
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(NontermRef(u("first-symbol"), 0, ChicSpan::dummy())),
                Rc::new(NontermRef(u("second symbol"), 0, ChicSpan::dummy())),
            ])
        );
    }

    #[test]
    fn parses_alternative_expr() {
        const INPUT: &str = "a b | c";
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Alternative(vec![
                Rc::new(Sequence(vec![
                    Rc::new(Terminal(u("a"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(u("b"), None, 0, ChicSpan::Dummy))
                ])),
                Rc::new(Terminal(u("c"), None, 0, ChicSpan::Dummy))
            ])
        );
    }

    #[test]
    fn parses_parenthesised_expr() {
        const INPUT: &str = r#"a (b | c)"#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(u("a"), None, 0, ChicSpan::Dummy)),
                Rc::new(Alternative(vec![
                    Rc::new(Terminal(u("b"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(u("c"), None, 0, ChicSpan::Dummy))
                ])),
            ])
        );
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let (s, v) = call_variant(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            v,
            Statement::CallVariant {
                head: u("foo"),
                expr: Rc::new(Terminal(u("bar"), None, 0, ChicSpan::Dummy))
            }
        );
    }

    #[test]
    fn parses_grammar() {
        const INPUT: &str = r#"
foo bar;
foo baz;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![
                    Statement::CallVariant {
                        head: u("foo"),
                        expr: Rc::new(Terminal(u("bar"), None, 0, ChicSpan::Dummy))
                    },
                    Statement::CallVariant {
                        head: u("foo"),
                        expr: Rc::new(Terminal(u("baz"), None, 0, ChicSpan::Dummy))
                    }
                ],
            }
        );
    }

    #[test]
    fn bug1() {
        // Did not consider whitespace before ...
        const INPUT: &str = "darcs help ( ( -v | --verbose ) | ( -q | --quiet ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;";
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            vec![Statement::CallVariant {
                head: u("darcs"),
                expr: Rc::new(Sequence(vec![
                    Rc::new(Terminal(u("help"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Many1(Rc::new(Alternative(vec![
                        Rc::new(Alternative(vec![
                            Rc::new(Terminal(u("-v"), None, 0, ChicSpan::Dummy)),
                            Rc::new(Terminal(u("--verbose"), None, 0, ChicSpan::Dummy))
                        ])),
                        Rc::new(Alternative(vec![
                            Rc::new(Terminal(u("-q"), None, 0, ChicSpan::Dummy)),
                            Rc::new(Terminal(u("--quiet"), None, 0, ChicSpan::Dummy))
                        ])),
                    ],)),)),
                    Rc::new(Optional(Rc::new(Sequence(vec![
                        Rc::new(NontermRef(u("DARCS_COMMAND"), 0, ChicSpan::dummy())),
                        Rc::new(Optional(Rc::new(Terminal(
                            u("DARCS_SUBCOMMAND"),
                            None,
                            0,
                            ChicSpan::Dummy
                        )))),
                    ])))),
                ]))
            },],
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
        let _ = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
    }

    #[test]
    fn parses_nonterminal_definition() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> ::= --color <WHEN>;
<WHEN> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            [
                Statement::CallVariant {
                    head: u("grep"),
                    expr: Rc::new(Sequence(vec![
                        Rc::new(Many1(Rc::new(Optional(Rc::new(NontermRef(
                            ustr("OPTION"),
                            0,
                            ChicSpan::dummy()
                        )))))),
                        Rc::new(NontermRef(ustr("PATTERNS"), 0, ChicSpan::dummy())),
                        Rc::new(Many1(Rc::new(Optional(Rc::new(NontermRef(
                            ustr("FILE"),
                            0,
                            ChicSpan::dummy()
                        )))))),
                    ]))
                },
                Statement::NonterminalDefinition {
                    symbol: u("OPTION"),
                    shell: None,
                    expr: Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr("--color"), None, 0, ChicSpan::Dummy)),
                        Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                    ]))
                },
                Statement::NonterminalDefinition {
                    symbol: u("WHEN"),
                    shell: None,
                    expr: Rc::new(Alternative(vec![
                        Rc::new(Terminal(ustr("always"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(ustr("never"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(ustr("auto"), None, 0, ChicSpan::Dummy))
                    ]))
                },
            ],
        );
    }

    #[test]
    fn skips_comment() {
        const INPUT: &str = r#"#foo"#;
        let (s, e) = comment(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e.into_fragment(), "foo");
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
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g,
            Grammar {
                statements: vec![Statement::NonterminalDefinition {
                    symbol: u("OPTION"),
                    shell: None,
                    expr: Rc::new(Alternative(vec![
                        Rc::new(Terminal(u("--extended-regexp"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(u("--fixed-strings"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(u("--basic-regexp"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(u("--perl-regexp"), None, 0, ChicSpan::Dummy)),
                    ]))
                },],
            }
        );
    }

    #[test]
    fn nonterminal_resolution_order_detects_trivial_cycle() {
        let nonterminal_definitions = UstrMap::from_iter([
            (
                u("FOO"),
                Rc::new(NontermRef(u("BAR"), 0, ChicSpan::dummy())),
            ),
            (
                u("BAR"),
                Rc::new(NontermRef(u("FOO"), 0, ChicSpan::dummy())),
            ),
        ]);
        assert!(matches!(
            get_nonterminals_resolution_order(&nonterminal_definitions),
            Err(Error::NonterminalDefinitionsCycle(None))
        ));
    }

    #[test]
    fn nonterminal_resolution_order_detects_simple_cycle() {
        let nonterminal_definitions = UstrMap::from_iter([
            (
                u("FOO"),
                Rc::new(NontermRef(u("BAR"), 0, ChicSpan::dummy())),
            ),
            (
                u("BAR"),
                Rc::new(NontermRef(u("BAR"), 0, ChicSpan::dummy())),
            ),
        ]);
        assert!(
            matches!(&get_nonterminals_resolution_order(&nonterminal_definitions), Err(Error::NonterminalDefinitionsCycle(Some(path))) if path == &[u("BAR"), u("BAR")])
        );
    }

    #[test]
    fn computes_nonterminals_resolution_order() {
        let nonterminal_definitions = UstrMap::from_iter([
            (
                u("WHEN"),
                Rc::new(Alternative(vec![
                    Rc::new(Terminal(u("always"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(u("never"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(u("auto"), None, 0, ChicSpan::Dummy)),
                ])),
            ),
            (
                u("FOO"),
                Rc::new(NontermRef(u("WHEN"), 0, ChicSpan::dummy())),
            ),
            (
                u("OPTION"),
                Rc::new(Sequence(vec![
                    Rc::new(Terminal(u("--color"), None, 0, ChicSpan::Dummy)),
                    Rc::new(NontermRef(u("FOO"), 0, ChicSpan::dummy())),
                ])),
            ),
        ]);
        assert_eq!(
            get_nonterminals_resolution_order(&nonterminal_definitions).unwrap(),
            vec![u("FOO"), u("OPTION")]
        );
    }

    #[test]
    fn parses_inline_shell_command() {
        const INPUT: &str = r#"
cargo [+{{{ rustup toolchain list | cut -d' ' -f1 }}}] [<OPTIONS>] [<COMMAND>];
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        assert_eq!(
            g.statements[0],
            Statement::CallVariant {
                head: ustr("cargo"),
                expr: Rc::new(Sequence(vec![
                    Rc::new(Optional(Rc::new(Subword(
                        SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                            Rc::new(Terminal(ustr("+"), None, 0, ChicSpan::Dummy)),
                            Rc::new(Command(
                                ustr("rustup toolchain list | cut -d' ' -f1"),
                                None,
                                0,
                                ChicSpan::dummy()
                            ))
                        ]))),
                        0,
                        ChicSpan::Dummy,
                    )))),
                    Rc::new(Optional(Rc::new(NontermRef(
                        ustr("OPTIONS"),
                        0,
                        ChicSpan::dummy()
                    )))),
                    Rc::new(Optional(Rc::new(NontermRef(
                        ustr("COMMAND"),
                        0,
                        ChicSpan::dummy()
                    )))),
                ])),
            }
        );
    }

    #[test]
    fn parses_prefix_grammar() {
        const INPUT: &str = r#"
grep --color=<WHEN> --version;
<WHEN> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            [
                Statement::CallVariant {
                    head: ustr("grep"),
                    expr: Rc::new(Sequence(vec![
                        Rc::new(Subword(
                            SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                                Rc::new(Terminal(ustr("--color="), None, 0, ChicSpan::Dummy)),
                                Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                            ]))),
                            0,
                            ChicSpan::Dummy,
                        )),
                        Rc::new(Terminal(ustr("--version"), None, 0, ChicSpan::Dummy))
                    ])),
                },
                Statement::NonterminalDefinition {
                    symbol: ustr("WHEN"),
                    shell: None,
                    expr: Rc::new(Alternative(vec![
                        Rc::new(Terminal(ustr("always"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(ustr("never"), None, 0, ChicSpan::Dummy)),
                        Rc::new(Terminal(ustr("auto"), None, 0, ChicSpan::Dummy))
                    ]))
                },
            ],
        );
    }

    #[test]
    fn parens_are_enough_to_parse_as_subword() {
        const INPUT: &str = r#"
grep --color=(always | never | auto);
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            [Statement::CallVariant {
                head: ustr("grep"),
                expr: Rc::new(Subword(
                    SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr("--color="), None, 0, ChicSpan::Dummy)),
                        Rc::new(Alternative(vec![
                            Rc::new(Terminal(ustr("always"), None, 0, ChicSpan::Dummy)),
                            Rc::new(Terminal(ustr("never"), None, 0, ChicSpan::Dummy)),
                            Rc::new(Terminal(ustr("auto"), None, 0, ChicSpan::Dummy))
                        ]))
                    ]))),
                    0,
                    ChicSpan::Dummy,
                )),
            },],
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
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);
        assert_eq!(
            g.statements[0],
            CallVariant {
                head: ustr("strace"),
                expr: Rc::new(Sequence(vec![
                    Rc::new(Terminal(ustr("-e"), None, 0, ChicSpan::Dummy)),
                    Rc::new(NontermRef(ustr("EXPR"), 0, ChicSpan::dummy()))
                ])),
            }
        );
        assert_eq!(
            g.statements[1],
            NonterminalDefinition {
                symbol: ustr("EXPR"),
                shell: None,
                expr: Rc::new(Subword(
                    SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                        Rc::new(Optional(Rc::new(Sequence(vec![
                            Rc::new(NontermRef(ustr("qualifier"), 0, ChicSpan::dummy())),
                            Rc::new(Terminal(ustr("="), None, 0, ChicSpan::Dummy))
                        ])))),
                        Rc::new(Optional(Rc::new(Terminal(
                            ustr("!"),
                            None,
                            0,
                            ChicSpan::Dummy
                        )))),
                        Rc::new(NontermRef(ustr("value"), 0, ChicSpan::dummy())),
                        Rc::new(Many1(Rc::new(Optional(Rc::new(Sequence(vec![
                            Rc::new(Terminal(ustr(","), None, 0, ChicSpan::Dummy)),
                            Rc::new(NontermRef(ustr("value"), 0, ChicSpan::dummy()))
                        ]))))))
                    ]))),
                    0,
                    ChicSpan::Dummy,
                )),
            }
        );
        assert_eq!(
            g.statements[2],
            NonterminalDefinition {
                symbol: ustr("qualifier"),
                shell: None,
                expr: Rc::new(Alternative(vec![
                    Rc::new(Terminal(ustr("trace"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("read"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("write"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("fault"), None, 0, ChicSpan::Dummy))
                ])),
            }
        );
        assert_eq!(
            g.statements[3],
            NonterminalDefinition {
                symbol: ustr("value"),
                shell: None,
                expr: Rc::new(Alternative(vec![
                    Rc::new(Terminal(ustr("%file"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("file"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("all"), None, 0, ChicSpan::Dummy))
                ])),
            }
        );
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
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);
        assert_eq!(
            g.statements[0],
            CallVariant {
                head: ustr("lsof"),
                expr: Rc::new(Subword(
                    SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                        Rc::new(Terminal(ustr("-s"), None, 0, ChicSpan::Dummy)),
                        Rc::new(NontermRef(ustr("PROTOCOL"), 0, ChicSpan::dummy())),
                        Rc::new(Terminal(ustr(":"), None, 0, ChicSpan::Dummy)),
                        Rc::new(NontermRef(ustr("STATE-SPEC"), 0, ChicSpan::dummy())),
                        Rc::new(Many1(Rc::new(Optional(Rc::new(Sequence(vec![
                            Rc::new(Terminal(ustr(","), None, 0, ChicSpan::Dummy)),
                            Rc::new(NontermRef(ustr("STATE-SPEC"), 0, ChicSpan::dummy()))
                        ]))))))
                    ]))),
                    0,
                    ChicSpan::Dummy,
                )),
            }
        );
        assert_eq!(
            g.statements[1],
            NonterminalDefinition {
                symbol: ustr("PROTOCOL"),
                shell: None,
                expr: Rc::new(Alternative(vec![
                    Rc::new(Terminal(ustr("TCP"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("UDP"), None, 0, ChicSpan::Dummy))
                ])),
            }
        );
        assert_eq!(
            g.statements[2],
            NonterminalDefinition {
                symbol: ustr("STATE-SPEC"),
                shell: None,
                expr: Rc::new(Subword(
                    SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                        Rc::new(Optional(Rc::new(Terminal(
                            ustr("^"),
                            None,
                            0,
                            ChicSpan::Dummy
                        )))),
                        Rc::new(NontermRef(ustr("STATE"), 0, ChicSpan::dummy()))
                    ]))),
                    0,
                    ChicSpan::Dummy,
                )),
            }
        );
        assert_eq!(
            g.statements[3],
            NonterminalDefinition {
                symbol: ustr("STATE"),
                shell: None,
                expr: Rc::new(Alternative(vec![
                    Rc::new(Terminal(ustr("LISTEN"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("CLOSED"), None, 0, ChicSpan::Dummy))
                ])),
            }
        );
    }

    #[test]
    fn parses_shell_command_nonterminal_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> ::= {{{ rustup toolchain list | cut -d' ' -f1 }}};
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            vec![
                Statement::CallVariant {
                    head: u("cargo"),
                    expr: Rc::new(Sequence(vec![
                        Rc::new(Optional(Rc::new(Subword(
                            SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                                Rc::new(Terminal(ustr("+"), None, 0, ChicSpan::Dummy)),
                                Rc::new(NontermRef(ustr("toolchain"), 0, ChicSpan::dummy()))
                            ]))),
                            0,
                            ChicSpan::Dummy,
                        )))),
                        Rc::new(Optional(Rc::new(NontermRef(
                            ustr("OPTIONS"),
                            0,
                            ChicSpan::dummy()
                        )))),
                        Rc::new(Optional(Rc::new(NontermRef(
                            ustr("COMMAND"),
                            0,
                            ChicSpan::dummy()
                        ))))
                    ]))
                },
                Statement::NonterminalDefinition {
                    symbol: u("toolchain"),
                    shell: None,
                    expr: Rc::new(Command(
                        u("rustup toolchain list | cut -d' ' -f1"),
                        None,
                        0,
                        ChicSpan::dummy()
                    )),
                },
            ],
        );
    }

    #[test]
    fn parses_descr() {
        const INPUT: &str = r#""PATTERNS are extended regular expressions""#;
        let (s, e) = description(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, "PATTERNS are extended regular expressions");
    }

    #[test]
    fn parses_special_characters_descr() {
        const INPUT: &str = r#""$f\"\\""#;
        let (s, e) = description(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(e, r#"$f"\"#);
    }

    #[test]
    fn parses_term_descr() {
        const INPUT: &str = r#"--extended-regexp "PATTERNS are extended regular expressions""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Terminal(
                ustr("--extended-regexp"),
                Some(ustr("PATTERNS are extended regular expressions")),
                0,
                ChicSpan::Dummy
            )
        );
    }

    #[test]
    fn parses_term_descr_arg() {
        const INPUT: &str = r#"--context "print NUM lines of output context" <NUM>"#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(
                    ustr("--context"),
                    Some(ustr("print NUM lines of output context")),
                    0,
                    ChicSpan::Dummy
                )),
                Rc::new(NontermRef(ustr("NUM"), 0, ChicSpan::dummy()))
            ])
        );
    }

    #[test]
    fn parses_description() {
        const INPUT: &str = r#"
grep --extended-regexp "PATTERNS are extended regular expressions";
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            vec![Statement::CallVariant {
                head: u("grep"),
                expr: Rc::new(Terminal(
                    ustr("--extended-regexp"),
                    Some(ustr("PATTERNS are extended regular expressions")),
                    0,
                    ChicSpan::Dummy
                ))
            }],
        );
    }

    #[test]
    fn detects_duplicated_nonterminals() {
        const INPUT: &str = r#"
grep [<OPTION>]... <PATTERNS> [<FILE>]...;
<OPTION> ::= --color <WHEN>;
<OPTION> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert!(
            matches!(ValidGrammar::from_grammar(g, Shell::Bash), Err(Error::DuplicateNonterminalDefinition(nonterm, None)) if nonterm == "OPTION")
        );
    }

    #[test]
    fn issue_15() {
        const INPUT: &str = r#"foo.sh [-h] ;"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            vec![Statement::CallVariant {
                head: u("foo.sh"),
                expr: Rc::new(Optional(Rc::new(Terminal(
                    ustr("-h"),
                    None,
                    0,
                    ChicSpan::Dummy
                )))),
            },]
        );
    }

    #[test]
    fn parses_nonterminal_shell_specific() {
        const INPUT: &str = r#"<FILE@bash>"#;
        let (s, (nonterm, shell)) = nonterm_specialization(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(nonterm.into_fragment(), "FILE");
        assert_eq!(shell.into_fragment(), "bash");
    }

    #[test]
    fn parses_specialized_nonterminals() {
        use Statement::*;
        const INPUT: &str = r#"
ls <FILE>;
<FILE@bash> ::= {{{ compgen -A file "$1" }}};
<FILE@fish> ::= {{{ __fish_complete_path "$1" }}};
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(
            g.statements,
            vec![
                Statement::CallVariant {
                    head: u("ls"),
                    expr: Rc::new(NontermRef(ustr("FILE"), 0, ChicSpan::dummy())), // should not get expanded because it's specialized
                },
                NonterminalDefinition {
                    symbol: ustr("FILE"),
                    shell: Some(ustr("bash")),
                    expr: Rc::new(Command(
                        ustr(r#"compgen -A file "$1""#),
                        None,
                        0,
                        ChicSpan::dummy()
                    ))
                },
                NonterminalDefinition {
                    symbol: ustr("FILE"),
                    shell: Some(ustr("fish")),
                    expr: Rc::new(Command(
                        ustr(r#"__fish_complete_path "$1""#),
                        None,
                        0,
                        ChicSpan::dummy()
                    ))
                },
            ],
        );
        let v = ValidGrammar::from_grammar(g, Shell::Bash).unwrap();
        let spec = v.specializations.get(&ustr("FILE")).unwrap();
        assert_eq!(spec.bash, Some(ustr(r#"compgen -A file "$1""#)));
        assert_eq!(spec.fish, Some(ustr(r#"__fish_complete_path "$1""#)));
        assert_eq!(spec.zsh, None);
    }

    #[test]
    fn distributes_descriptions() {
        const INPUT: &str = r#"mygrep (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed = distribute_descriptions(Rc::new(e));
        assert_eq!(
            distributed,
            Rc::new(Sequence(vec![
                Rc::new(Terminal(ustr("mygrep"), None, 0, ChicSpan::Dummy)),
                Rc::new(Alternative(vec![
                    Rc::new(Subword(
                        SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                            Rc::new(Terminal(
                                ustr("--color="),
                                Some(ustr("use markers to highlight the matching strings")),
                                0,
                                ChicSpan::Dummy
                            )),
                            Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                        ]))),
                        0,
                        ChicSpan::Dummy,
                    )),
                    Rc::new(Sequence(vec![
                        Rc::new(Terminal(
                            ustr("--color"),
                            Some(ustr("use markers to highlight the matching strings")),
                            0,
                            ChicSpan::Dummy
                        )),
                        Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                    ]))
                ]))
            ]))
        );
    }

    #[test]
    fn spends_distributed_description() {
        const INPUT: &str = r#"mygrep --help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed = distribute_descriptions(Rc::new(e));
        assert_eq!(
            distributed,
            Rc::new(Alternative(vec![
                Rc::new(Sequence(vec![
                    Rc::new(Terminal(ustr("mygrep"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("--help"), None, 0, ChicSpan::Dummy))
                ])),
                Rc::new(Alternative(vec![
                    Rc::new(Subword(
                        SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                            Rc::new(Terminal(
                                ustr("--color="),
                                Some(ustr("use markers to highlight the matching strings")),
                                0,
                                ChicSpan::Dummy
                            )),
                            Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                        ]))),
                        0,
                        ChicSpan::Dummy,
                    )),
                    Rc::new(Sequence(vec![
                        Rc::new(Terminal(
                            ustr("--color"),
                            Some(ustr("use markers to highlight the matching strings")),
                            0,
                            ChicSpan::Dummy
                        )),
                        Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                    ]))
                ]))
            ]))
        );
    }

    #[test]
    fn spends_distributed_description2() {
        const INPUT: &str = r#"mygrep (--help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings")"#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed = distribute_descriptions(Rc::new(e));
        assert_eq!(
            distributed,
            Rc::new(Sequence(vec![
                Rc::new(Terminal(ustr("mygrep"), None, 0, ChicSpan::Dummy)),
                Rc::new(Alternative(vec![
                    Rc::new(Terminal(ustr("--help"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Alternative(vec![
                        Rc::new(Subword(
                            SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                                Rc::new(Terminal(
                                    ustr("--color="),
                                    Some(ustr("use markers to highlight the matching strings")),
                                    0,
                                    ChicSpan::Dummy
                                )),
                                Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                            ]))),
                            0,
                            ChicSpan::Dummy,
                        )),
                        Rc::new(Sequence(vec![
                            Rc::new(Terminal(
                                ustr("--color"),
                                Some(ustr("use markers to highlight the matching strings")),
                                0,
                                ChicSpan::Dummy
                            )),
                            Rc::new(NontermRef(ustr("WHEN"), 0, ChicSpan::dummy()))
                        ]))
                    ]))
                ]))
            ]))
        );
    }

    #[test]
    fn parses_fallback_expr() {
        const INPUT: &str = r#"cmd (foo || bar)"#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(ustr("cmd"), None, 0, ChicSpan::Dummy)),
                Rc::new(Fallback(vec![
                    Rc::new(Terminal(ustr("foo"), None, 0, ChicSpan::Dummy)),
                    Rc::new(Terminal(ustr("bar"), None, 0, ChicSpan::Dummy))
                ]))
            ])
        );
    }

    #[test]
    fn parses_git_commit_range_subword() {
        const INPUT: &str = r#"{{{ git tag }}}..{{{ git tag }}}"#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Subword(
                SubwordCompilationPhase::Expr(Rc::new(Sequence(vec![
                    Rc::new(Command(ustr("git tag"), None, 0, ChicSpan::dummy())),
                    Rc::new(Terminal(ustr(".."), None, 0, ChicSpan::Dummy)),
                    Rc::new(Command(ustr("git tag"), None, 0, ChicSpan::dummy()))
                ]))),
                0,
                ChicSpan::Dummy,
            )
        );
    }

    #[test]
    fn ignores_form_feed() {
        const INPUT: &str = "cmd \u{000C} \u{000C} \u{000C}\u{000C} foo";
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(ustr("cmd"), None, 0, ChicSpan::Dummy)),
                Rc::new(Terminal(ustr("foo"), None, 0, ChicSpan::Dummy)),
            ])
        );
    }

    #[test]
    fn parses_nontail_command_script() {
        const INPUT: &str = r#"cmd {{{ echo foo }}}@bash"bar"@fish"baz"@zsh"quux""#;
        let (s, e) = expr(Span::new(INPUT)).unwrap();
        assert!(s.is_empty(), "{:?}", s.fragment());
        assert_eq!(
            e,
            Sequence(vec![
                Rc::new(Terminal(ustr("cmd"), None, 0, ChicSpan::Dummy)),
                Rc::new(Command(
                    ustr("echo foo"),
                    Some(CmdRegexDecl {
                        bash: Some(ustr("bar")),
                        fish: Some(ustr("baz")),
                        zsh: Some(ustr("quux"))
                    }),
                    0,
                    ChicSpan::dummy(),
                )),
            ])
        );
    }
}
