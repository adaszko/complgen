use std::debug_assert;

use hashbrown::HashMap;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till, take_until, take_while1},
    character::complete::{char, multispace1, one_of},
    combinator::{fail, map, opt, value, verify},
    error::context,
    multi::{fold_many0, many0},
    sequence::preceded,
    Finish, IResult, Parser,
};

use crate::{Error, Result};
use ustr::{ustr, Ustr, UstrMap, UstrSet};

use crate::{dfa::DFA, regex::Regex};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DFAId(u32);

impl From<u32> for DFAId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Default)]
pub struct DFAInterner {
    map: HashMap<DFA, u32>,
    vec: Vec<DFA>,
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
    Expr(ExprId),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExprId(usize);

impl ExprId {
    pub fn to_index(&self) -> usize {
        self.0
    }
}

#[derive(Clone)]
pub enum Expr {
    // `--help`
    Terminal {
        term: Ustr,
        descr: Option<Ustr>,
        fallback: usize,
        span: Option<HumanSpan>,
    },

    // `<PATH>`, `<DIRECTORY>`, etc.
    NontermRef {
        nonterm: Ustr,
        fallback: usize,
        span: Option<HumanSpan>,
    },

    // `{{{ ls }}}`
    // or
    // `{{{ ls }}}@bash"foo"@fish"bar"`
    Command {
        cmd: Ustr,
        regex: Option<CmdRegex>,
        fallback: usize,
        span: Option<HumanSpan>,
    },

    // `foo bar`
    Sequence(Vec<ExprId>),

    // `foo | bar`
    Alternative(Vec<ExprId>),

    // `[EXPR]`
    Optional(ExprId),

    // `EXPR...`
    Many1(ExprId),

    // `(b | build) "Compile the current package"` means the description applies to both `b` and
    // `build`. `(b build) "Compile the current package"` means means the description applies just
    // to `b` (i.e. the first literal)
    DistributiveDescription {
        child: ExprId,
        descr: Ustr,
    },

    // `foo || bar`
    Fallback(Vec<ExprId>),

    // `--option=argument`
    Subword {
        phase: SubwordCompilationPhase,
        fallback: usize,
        span: Option<HumanSpan>,
    },
}

// Invariant: At least one field must be Some(_)
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CmdRegex {
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

impl CmdRegex {
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
            Self::Subword {
                phase, fallback, ..
            } => f.write_fmt(format_args!(r#"Subword({phase:?}, {fallback})"#)),
            Expr::Terminal {
                term,
                descr: Some(descr),
                fallback,
                ..
            } => f.write_fmt(format_args!(
                r#"Terminal(ustr(\"{term}\"), Some(ustr(\"{}\"))), {fallback})"#,
                descr
            )),
            Expr::Terminal {
                term,
                descr: None,
                fallback,
                ..
            } => f.write_fmt(format_args!(
                r#"Terminal(ustr(\"{term}\"), None, {fallback})"#
            )),
            Expr::NontermRef {
                nonterm,
                fallback,
                span,
            } => f.write_fmt(format_args!(
                r#"Nonterminal(ustr(\"{nonterm}\"), {fallback}, {span:?})"#
            )),
            Self::Command {
                cmd,
                regex,
                fallback,
                span,
            } => f.write_fmt(format_args!(
                r#"Command(ustr({cmd:?}), {regex:?}, {fallback}, {span:?})"#
            )),
            Self::Sequence(children) => {
                f.write_fmt(format_args!(r#"Sequence(vec!{:?})"#, children))
            }
            Self::Alternative(children) => {
                f.write_fmt(format_args!(r#"Alternative(vec!{:?})"#, children))
            }
            Self::Optional(child) => f.write_fmt(format_args!(r#"Optional({:?})"#, child)),
            Self::Many1(child) => f.write_fmt(format_args!(r#"Many1({:?})"#, child)),
            Self::DistributiveDescription { child, descr } => f.write_fmt(format_args!(
                r#"DistributiveDescription({child:?}, {descr:?})"#
            )),
            Self::Fallback(children) => {
                f.write_fmt(format_args!(r#"Fallback(vec!{:?})"#, children))
            }
        }
    }
}

pub fn alloc<T>(arena: &mut Vec<T>, elem: T) -> ExprId {
    let id = arena.len();
    arena.push(elem);
    ExprId(id)
}

fn railroad_node_from_expr(arena: &[Expr], expr_id: ExprId) -> Box<dyn railroad::Node> {
    match &arena[expr_id.to_index()] {
        Expr::Subword {
            phase: SubwordCompilationPhase::Expr(expr),
            ..
        } => {
            let mut seq: Box<railroad::Sequence<Box<dyn railroad::Node>>> = Default::default();
            seq.push(Box::new(railroad::SimpleStart));
            seq.push(railroad_node_from_expr(arena, *expr));
            seq.push(Box::new(railroad::SimpleEnd));
            seq
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::DFA(..),
            ..
        } => unreachable!(),
        Expr::Terminal { term, .. } => Box::new(railroad::Terminal::new(term.as_str().to_string())),
        Expr::NontermRef { nonterm, .. } => {
            Box::new(railroad::NonTerminal::new(nonterm.as_str().to_string()))
        }
        Expr::Command { cmd, .. } => Box::new(railroad::Comment::new(cmd.as_str().to_string())),
        Expr::Sequence(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(arena, *e))
                .collect();
            Box::new(railroad::Sequence::new(subnodes))
        }
        Expr::Alternative(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(arena, *e))
                .collect();
            Box::new(railroad::Choice::new(subnodes))
        }
        Expr::Optional(subexpr) => Box::new(railroad::Optional::new(railroad_node_from_expr(
            arena, *subexpr,
        ))),
        Expr::Many1(subexpr) => {
            let subnode = railroad_node_from_expr(arena, *subexpr);
            Box::new(railroad::Repeat::new(subnode, Box::new(railroad::Empty)))
        }
        Expr::DistributiveDescription { child, descr } => {
            let inner = railroad_node_from_expr(arena, *child);
            let label = railroad::Comment::new(descr.to_string());
            Box::new(railroad::LabeledBox::new(inner, label))
        }
        Expr::Fallback(subexprs) => {
            let subnodes: Vec<Box<dyn railroad::Node>> = subexprs
                .iter()
                .map(|e| railroad_node_from_expr(arena, *e))
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
                seq.push(railroad_node_from_expr(&grammar.arena, *expr));
                seq.push(Box::new(railroad::End));
                seq
            }
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => {
                let inner = railroad_node_from_expr(&grammar.arena, *expr);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HumanSpan {
    pub line_start: usize,
    pub start: usize,
    pub end: usize,
}

impl HumanSpan {
    fn new(before: Span, after: Span) -> Self {
        // XXX Doesn't handle tabs
        Self {
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

fn terminal_opt_description_expr<'s>(
    arena: &mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (after, term) = terminal(input)?;
    let (after, descr) = opt(preceded(multiblanks0, description))(after)?;
    let expr = Expr::Terminal {
        term: ustr(&term),
        descr: descr.map(|span| ustr(&span)),
        fallback: 0,
        span: Some(HumanSpan::new(input, after)),
    };
    let id = alloc(arena, expr);
    Ok((after, id))
}

fn nonterm(input: Span) -> IResult<Span, Span> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}

fn nonterm_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, nonterm) = context("nonterminal", nonterm)(input)?;
    let diagnostic_span = HumanSpan::new(input, after);
    let e = Expr::NontermRef {
        nonterm: ustr(nonterm.into_fragment()),
        fallback: 0,
        span: Some(diagnostic_span),
    };
    let id = alloc(arena, e);
    Ok((after, id))
}

fn triple_bracket_command(input: Span) -> IResult<Span, Span> {
    let (input, _) = tag("{{{")(input)?;
    let (input, cmd) = take_until("}}}")(input)?;
    let (input, _) = tag("}}}")(input)?;
    Ok((input, Span::new(cmd.into_fragment().trim())))
}

fn command_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = HumanSpan::new(input, after);
    let e = Expr::Command {
        cmd: ustr(cmd.into_fragment()),
        regex: None,
        fallback: 0,
        span: Some(command_span),
    };
    let id = alloc(arena, e);
    Ok((after, id))
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

fn cmd_regex_decl(mut input: Span) -> IResult<Span, CmdRegex> {
    let mut spec = CmdRegex::default();
    while let Ok((rest, (shell, regex))) = at_shell_regex(input) {
        match shell.as_ref() {
            "bash" => spec.bash = Some(ustr(regex.as_ref())),
            "fish" => spec.fish = Some(ustr(regex.as_ref())),
            "zsh" => spec.zsh = Some(ustr(regex.as_ref())),
            _ => unreachable!(),
        }
        input = rest;
    }

    if let CmdRegex {
        bash: None,
        fish: None,
        zsh: None,
    } = spec
    {
        return fail(input);
    }

    Ok((input, spec))
}

fn nontail_command_expr<'s>(
    arena: &mut Vec<Expr>,
    mut input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = HumanSpan::new(input, after);
    input = after;
    let (input, regex_decl) = cmd_regex_decl(input)?;
    let e = Expr::Command {
        cmd: ustr(cmd.into_fragment()),
        regex: Some(regex_decl),
        fallback: 0,
        span: Some(command_span),
    };
    let id = alloc(arena, e);
    Ok((input, id))
}

fn optional_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, expr) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(']')(input)?;
    let id = alloc(arena, Expr::Optional(expr));
    Ok((input, id))
}

fn parenthesized_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn many1_tag(input: Span) -> IResult<Span, ()> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("...")(input)?;
    Ok((input, ()))
}

fn unary_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, e) = 'alt: {
        if let Ok((input, e)) = nonterm_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = optional_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = parenthesized_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = nontail_command_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = command_expr(arena, input) {
            break 'alt (input, e);
        }

        terminal_opt_description_expr(arena, input)?
    };

    if let Ok((input, ())) = many1_tag(input) {
        let e = Expr::Many1(e);
        let id = alloc(arena, e);
        return Ok((input, id));
    }

    Ok((input, e))
}

fn subword_sequence_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut after, left) = unary_expr(arena, input)?;
    let mut factors: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = unary_expr(arena, after) {
        factors.push(right);
        after = rest;
    }
    let result = if factors.len() == 1 {
        factors.into_iter().next().unwrap()
    } else {
        let flattened_factors: Vec<ExprId> = factors
            .into_iter()
            .map(|e| flatten_expr(arena, e))
            .collect();
        let e = Expr::Sequence(flattened_factors);
        let span = HumanSpan::new(input, after);
        let subword_id = alloc(arena, e);
        let subword_expr = Expr::Subword {
            phase: SubwordCompilationPhase::Expr(subword_id),
            fallback: 0,
            span: Some(span),
        };
        alloc(arena, subword_expr)
    };
    Ok((after, result))
}

fn subword_sequence_expr_opt_description<'s>(
    arena: &mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (input, expr_id) = subword_sequence_expr(arena, input)?;
    let (input, description) = opt(preceded(multiblanks0, description))(input)?;
    let result = match description {
        Some(descr) => {
            let e = Expr::DistributiveDescription {
                child: expr_id,
                descr: ustr(&descr),
            };
            alloc(arena, e)
        }
        None => expr_id,
    };
    Ok((input, result))
}

fn sequence_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut input, left) = subword_sequence_expr_opt_description(arena, input)?;
    let mut factors: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = preceded(multiblanks1, |i| {
        subword_sequence_expr_opt_description(arena, i)
    })(input)
    {
        factors.push(right);
        input = rest;
    }
    let result = if factors.len() == 1 {
        factors.drain(..).next().unwrap()
    } else {
        alloc(arena, Expr::Sequence(factors))
    };
    Ok((input, result))
}

fn do_alternative_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char('|')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = sequence_expr(arena, input)?;
    Ok((input, right))
}

fn alternative_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut input, left) = sequence_expr(arena, input)?;
    let mut elems: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = do_alternative_expr(arena, input) {
        elems.push(right);
        input = rest;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        alloc(arena, Expr::Alternative(elems))
    };
    Ok((input, result))
}

fn do_fallback_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("||")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = alternative_expr(arena, input)?;
    Ok((input, right))
}

fn fallback_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut input, left) = alternative_expr(arena, input)?;
    let mut fallbacks: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = do_fallback_expr(arena, input) {
        fallbacks.push(right);
        input = rest;
    }
    let result = if fallbacks.len() == 1 {
        fallbacks.drain(..).next().unwrap()
    } else {
        alloc(arena, Expr::Fallback(fallbacks))
    };
    Ok((input, result))
}

fn expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    fallback_expr(arena, input)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    CallVariant {
        head: Ustr,
        expr: ExprId,
    },
    NonterminalDefinition {
        symbol: Ustr,
        shell: Option<Ustr>,
        expr: ExprId,
    },
}

fn call_variant<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, Statement> {
    let (input, name) = terminal(input)?;
    let (input, _) = multiblanks1(input)?;
    let (input, expr) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let production = Statement::CallVariant {
        head: ustr(&name),
        expr,
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

fn nonterm_def_statement<'s>(
    arena: &mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, Statement> {
    let (input, (name, shell)) = nonterm_def(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, e) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(';')(input)?;

    let stmt = Statement::NonterminalDefinition {
        symbol: ustr(name.into_fragment()),
        shell: shell.map(|span| ustr(span.into_fragment())),
        expr: e,
    };

    Ok((input, stmt))
}

fn statement<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, Statement> {
    let (input, stmt) = 'alt: {
        if let Ok((input, stmt)) = call_variant(arena, input) {
            break 'alt (input, stmt);
        }

        nonterm_def_statement(arena, input)?
    };

    let (input, _) = multiblanks0(input)?;
    Ok((input, stmt))
}

fn grammar(input: Span) -> IResult<Span, (Vec<Expr>, Vec<Statement>)> {
    let mut arena = Vec::new();
    let (input, _) = multiblanks0(input)?;
    let (input, statements) = many0(|i| statement(&mut arena, i))(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, (arena, statements)))
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub arena: Vec<Expr>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ValidGrammar {
    pub arena: Vec<Expr>,
    pub command: Ustr,
    pub expr: ExprId,
    pub specializations: UstrMap<Specialization>,
    pub undefined_nonterminals: UstrSet,
    pub unused_nonterminals: UstrSet,
    pub subdfa_interner: DFAInterner,
}

fn make_specializations_map(
    arena: &[Expr],
    statements: &[Statement],
) -> Result<UstrMap<Specialization>> {
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
        let command = match &arena[expr.to_index()] {
            Expr::Command { cmd, .. } => cmd,
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
        let Expr::Command { cmd: command, .. } = &arena[expr.to_index()] else {
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
fn flatten_expr(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    match arena[expr_id.to_index()].clone() {
        Expr::Terminal { .. } | Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Subword { phase: child, .. } => {
            let child = match child {
                SubwordCompilationPhase::Expr(e) => e,
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            flatten_expr(arena, child)
        }
        Expr::Sequence(children) => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription {
            child,
            descr: description,
        } => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::DistributiveDescription {
                        child: new_child,
                        descr: description,
                    },
                )
            }
        }
        Expr::Fallback(children) => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Fallback(new_children))
            }
        }
    }
}

fn compile_subword_exprs(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    specs: &UstrMap<Specialization>,
    shell: Shell,
    subdfa_interner: &mut DFAInterner,
) -> Result<ExprId> {
    let retval = match arena[expr_id.to_index()].clone() {
        Expr::Subword {
            phase: subword_expr,
            fallback,
            span,
        } => {
            let subword_expr = match subword_expr {
                SubwordCompilationPhase::Expr(e) => e,
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            let subword_expr = flatten_expr(arena, subword_expr);
            let regex = Regex::from_expr(subword_expr, arena, specs).unwrap();
            regex.ensure_ambiguous_inputs_tail_only_subword(shell)?;
            regex.check_clashing_variants()?;
            let dfa = DFA::from_regex(&regex, DFAInterner::default());
            let dfa = dfa.minimize();
            let subdfaid = subdfa_interner.intern(dfa);
            alloc(
                arena,
                Expr::Subword {
                    phase: SubwordCompilationPhase::DFA(subdfaid),
                    fallback,
                    span,
                },
            )
        }
        Expr::Terminal { .. } | Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| compile_subword_exprs(arena, *e, specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| compile_subword_exprs(arena, *e, specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = compile_subword_exprs(arena, child, specs, shell, subdfa_interner)?;
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = compile_subword_exprs(arena, child, specs, shell, subdfa_interner)?;
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription { .. } => unreachable!(
            "DistributiveDescription Expr type should have been erased by the time subwords are being compiled"
        ),
        Expr::Fallback(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| compile_subword_exprs(arena, *e, specs, shell, subdfa_interner))
                .collect::<Result<_>>()?;
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Fallback(new_children))
            }
        }
    };
    Ok(retval)
}

// Move descriptions to their corresponding terminals.
fn do_distribute_descriptions(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    description: &mut Option<Ustr>,
) -> ExprId {
    match arena[expr_id.to_index()].clone() {
        Expr::DistributiveDescription { child, descr } => {
            let new_child = do_distribute_descriptions(arena, child, &mut Some(descr));
            if child == new_child { child } else { new_child }
        }
        Expr::Terminal {
            term,
            descr: None,
            fallback: level,
            span,
        } if description.is_some() => {
            let result = Expr::Terminal {
                term,
                descr: *description,
                fallback: level,
                span,
            };
            *description = None; // spend it
            alloc(arena, result)
        }
        Expr::Terminal { .. } => expr_id,
        Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, description))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, &mut description.clone()))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Many1(new_child))
            }
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::Expr(child),
            fallback,
            span,
        } => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        phase: SubwordCompilationPhase::Expr(new_child),
                        fallback,
                        span,
                    },
                )
            }
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::DFA(_),
            ..
        } => unreachable!(),
        Expr::Fallback(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_distribute_descriptions(arena, *e, description))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Fallback(new_children))
            }
        }
    }
}

fn distribute_descriptions(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    let mut description = None;
    do_distribute_descriptions(arena, expr_id, &mut description)
}

// Propagate fallback levels of fallback alternatives down to literals.
fn do_propagate_fallback_levels(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    fallback_level: usize,
) -> ExprId {
    match arena[expr_id.to_index()].clone() {
        Expr::Terminal { fallback, .. } if fallback == fallback_level => expr_id,
        Expr::Terminal {
            term, descr, span, ..
        } => alloc(
            arena,
            Expr::Terminal {
                term,
                descr,
                fallback: fallback_level,
                span,
            },
        ),
        Expr::Fallback(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .enumerate()
                .map(|(i, e)| do_propagate_fallback_levels(arena, *e, i))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Fallback(new_children))
            }
        }
        Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Sequence(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(arena, *e, fallback_level))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|e| do_propagate_fallback_levels(arena, *e, fallback_level))
                .collect();
            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Many1(new_child))
            }
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::Expr(child),
            fallback: _,
            span,
        } => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        phase: SubwordCompilationPhase::Expr(new_child),
                        fallback: fallback_level,
                        span,
                    },
                )
            }
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::DFA(..),
            ..
        } => unreachable!(),
        Expr::DistributiveDescription { .. } => unreachable!(),
    }
}

fn propagate_fallback_levels(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    do_propagate_fallback_levels(arena, expr_id, 0)
}

impl ValidGrammar {
    pub fn from_grammar(mut grammar: Grammar, shell: Shell) -> Result<Self> {
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
            let call_variants: Vec<ExprId> = grammar
                .statements
                .iter()
                .filter_map(|v| match v {
                    Statement::CallVariant { expr: rhs, .. } => Some(*rhs),
                    Statement::NonterminalDefinition { .. } => None,
                })
                .collect();

            if call_variants.len() == 1 {
                call_variants[0]
            } else {
                alloc(&mut grammar.arena, Expr::Alternative(call_variants))
            }
        };

        let mut nonterminal_definitions: UstrMap<ExprId> = {
            let mut nonterminal_definitions: UstrMap<ExprId> = Default::default();
            for definition in &grammar.statements {
                let (symbol, expr) = match definition {
                    Statement::NonterminalDefinition {
                        symbol,
                        expr,
                        shell: None,
                    } => (*symbol, *expr),
                    _ => continue,
                };
                if nonterminal_definitions.contains_key(&symbol) {
                    return Err(Error::DuplicateNonterminalDefinition(symbol, None));
                }
                let expr = distribute_descriptions(&mut grammar.arena, expr);
                nonterminal_definitions.insert(symbol, expr);
            }
            nonterminal_definitions
        };

        let expr = distribute_descriptions(&mut grammar.arena, expr);

        let specializations = make_specializations_map(&grammar.arena, &grammar.statements)?;

        let mut unused_nonterminals: UstrSet = nonterminal_definitions.keys().copied().collect();

        for nonterminal in
            get_nonterminals_resolution_order(&grammar.arena, &nonterminal_definitions)?
        {
            let e = *nonterminal_definitions.get(&nonterminal).unwrap();
            let new_e = resolve_nonterminals(
                &mut grammar.arena,
                e,
                &nonterminal_definitions,
                &specializations,
                &mut unused_nonterminals,
            );
            *nonterminal_definitions.get_mut(&nonterminal).unwrap() = new_e;
        }

        check_subword_spaces(&grammar.arena, expr, &nonterminal_definitions)?;

        let expr = resolve_nonterminals(
            &mut grammar.arena,
            expr,
            &nonterminal_definitions,
            &specializations,
            &mut unused_nonterminals,
        );

        let expr = propagate_fallback_levels(&mut grammar.arena, expr);

        let undefined_nonterminals = {
            let mut nonterms = get_expression_nonterminals(&grammar.arena, expr);
            nonterms.retain(|n| !specializations.contains_key(n));
            nonterms
        };

        let mut subdfa_interner = DFAInterner::default();
        let expr = compile_subword_exprs(
            &mut grammar.arena,
            expr,
            &specializations,
            shell,
            &mut subdfa_interner,
        )?;

        let g = ValidGrammar {
            arena: grammar.arena,
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

fn expr_get_head(arena: &[Expr], expr_id: ExprId) -> ExprId {
    match &arena[expr_id.to_index()] {
        Expr::Terminal { .. }
        | Expr::NontermRef { .. }
        | Expr::Command { .. }
        | Expr::Alternative(..)
        | Expr::Fallback(..)
        | Expr::Optional(..)
        | Expr::Many1(..) => expr_id,
        Expr::Sequence(children) => expr_get_head(arena, *children.first().unwrap()),
        Expr::Subword { .. } => unreachable!(),
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn expr_get_tail(arena: &[Expr], expr_id: ExprId) -> ExprId {
    match &arena[expr_id.to_index()] {
        Expr::Terminal { .. }
        | Expr::NontermRef { .. }
        | Expr::Command { .. }
        | Expr::Alternative(..)
        | Expr::Fallback(..)
        | Expr::Optional(..)
        | Expr::Many1(..) => expr_id,
        Expr::Sequence(children) => expr_get_head(arena, *children.last().unwrap()),
        Expr::Subword { .. } => unreachable!(),
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn check_subword_spaces(arena: &[Expr], expr_id: ExprId, nonterms: &UstrMap<ExprId>) -> Result<()> {
    let mut nonterm_expn_trace: Vec<HumanSpan> = Default::default();
    do_check_subword_spaces(arena, expr_id, nonterms, &mut nonterm_expn_trace, false)
}

// Disallows spaces in subword expressions, e.g.
//
// aerc :<COMMAND>;
// <COMMAND> ::= quit -f;
//
// On deeply nested <NONTERM>s, this can lead to [surprising spaces
// removal](https://github.com/adaszko/complgen/issues/63) so forbid it completely.
fn do_check_subword_spaces(
    arena: &[Expr],
    expr_id: ExprId,
    nonterms: &UstrMap<ExprId>,
    nonterm_expn_trace: &mut Vec<HumanSpan>,
    within_subword: bool,
) -> Result<()> {
    match &arena[expr_id.to_index()] {
        Expr::Sequence(children) if within_subword => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            // Error out on two adjacent Expr::Terminal()s
            for pair in children.windows(2) {
                let [left, right] = pair else { unreachable!() };
                let left_tail = expr_get_tail(arena, *left);
                let right_head = expr_get_head(arena, *right);
                if let (
                    Expr::Terminal {
                        span: left_span, ..
                    },
                    Expr::Terminal {
                        span: right_span, ..
                    },
                ) = (&arena[left_tail.to_index()], &arena[right_head.to_index()])
                {
                    return Err(Error::SubwordSpaces(
                        left_span.to_owned(),
                        right_span.to_owned(),
                        nonterm_expn_trace.to_owned(),
                    ));
                }
            }
            Ok(())
        }
        Expr::Sequence(children) => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Terminal { .. } => Ok(()),
        Expr::NontermRef { nonterm, span, .. } => {
            let Some(expn) = nonterms.get(nonterm) else {
                return Ok(());
            };
            if let Some(span) = *span {
                nonterm_expn_trace.push(span);
            }
            do_check_subword_spaces(arena, *expn, nonterms, nonterm_expn_trace, within_subword)?;
            if span.is_some() {
                nonterm_expn_trace.pop();
            }
            Ok(())
        }
        Expr::Command { .. } => Ok(()),
        Expr::Subword {
            phase: SubwordCompilationPhase::Expr(child),
            ..
        } => do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, true),
        Expr::Subword {
            phase: SubwordCompilationPhase::DFA(..),
            ..
        } => {
            unreachable!("wrong compilation phases order")
        }
        Expr::Alternative(children) => {
            for child in children {
                do_check_subword_spaces(
                    arena,
                    *child,
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
                    arena,
                    *child,
                    nonterms,
                    nonterm_expn_trace,
                    within_subword,
                )?;
            }
            Ok(())
        }
        Expr::Optional(child) => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, within_subword)
        }
        Expr::Many1(child) => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, within_subword)
        }
        Expr::DistributiveDescription { .. } => {
            unreachable!("wrong compilation phases order")
        }
    }
}

fn resolve_nonterminals(
    arena: &mut Vec<Expr>,
    expr_id: ExprId,
    vars: &UstrMap<ExprId>,
    specializations: &UstrMap<Specialization>,
    unused_nonterminals: &mut UstrSet,
) -> ExprId {
    match arena[expr_id.to_index()].clone() {
        Expr::Terminal { .. } | Expr::Command { .. } => expr_id,
        Expr::Subword {
            phase: SubwordCompilationPhase::Expr(child),
            fallback,
            span,
        } => {
            let new_child =
                resolve_nonterminals(arena, child, vars, specializations, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword {
                        phase: SubwordCompilationPhase::Expr(new_child),
                        fallback,
                        span,
                    },
                )
            }
        }
        Expr::Subword {
            phase: SubwordCompilationPhase::DFA(..),
            ..
        } => unreachable!(),
        Expr::NontermRef { nonterm: name, .. } => {
            if specializations.contains_key(&name) {
                // Specialized nonterminals are resolved when the target shell is known, not earlier.
                return expr_id;
            }
            match vars.get(&name) {
                Some(replacement) => {
                    unused_nonterminals.remove(&name);
                    *replacement
                }
                None => expr_id,
            }
        }
        Expr::Sequence(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    resolve_nonterminals(arena, *child, vars, specializations, unused_nonterminals)
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Sequence(new_children))
            }
        }
        Expr::Alternative(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    resolve_nonterminals(arena, *child, vars, specializations, unused_nonterminals)
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Alternative(new_children))
            }
        }
        Expr::Optional(child) => {
            let new_child =
                resolve_nonterminals(arena, child, vars, specializations, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Optional(new_child))
            }
        }
        Expr::Many1(child) => {
            let new_child =
                resolve_nonterminals(arena, child, vars, specializations, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::Many1(new_child))
            }
        }
        Expr::DistributiveDescription { .. } => unreachable!(
            "Expr::DistributiveDescription should have been erased by the time nonterminals are being resolved"
        ),
        Expr::Fallback(children) => {
            let new_children: Vec<ExprId> = children
                .iter()
                .map(|child| {
                    resolve_nonterminals(arena, *child, vars, specializations, unused_nonterminals)
                })
                .collect();

            if children == new_children {
                expr_id
            } else {
                alloc(arena, Expr::Fallback(new_children))
            }
        }
    }
}

fn do_get_expression_nonterminals(arena: &[Expr], expr_id: ExprId, deps: &mut UstrSet) {
    match &arena[expr_id.to_index()] {
        Expr::Terminal { .. } | Expr::Command { .. } => {}
        Expr::Subword { phase, .. } => {
            let subexpr = match phase {
                SubwordCompilationPhase::Expr(e) => *e,
                SubwordCompilationPhase::DFA(..) => unreachable!(),
            };
            do_get_expression_nonterminals(arena, subexpr, deps);
        }
        Expr::NontermRef {
            nonterm: varname, ..
        } => {
            deps.insert(*varname);
        }
        Expr::Sequence(children) => {
            for child in children {
                do_get_expression_nonterminals(arena, *child, deps);
            }
        }
        Expr::Alternative(children) => {
            for child in children {
                do_get_expression_nonterminals(arena, *child, deps);
            }
        }
        Expr::Optional(child) => {
            do_get_expression_nonterminals(arena, *child, deps);
        }
        Expr::Many1(child) => {
            do_get_expression_nonterminals(arena, *child, deps);
        }
        Expr::DistributiveDescription { .. } => unreachable!(
            "Expr::DistributiveDescription should have been erased by the time nonterminals are being collected"
        ),
        Expr::Fallback(children) => {
            for child in children {
                do_get_expression_nonterminals(arena, *child, deps);
            }
        }
    }
}

fn get_expression_nonterminals(arena: &[Expr], expr_id: ExprId) -> UstrSet {
    let mut result: UstrSet = Default::default();
    do_get_expression_nonterminals(arena, expr_id, &mut result);
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
    arena: &[Expr],
    nonterminal_definitions: &UstrMap<ExprId>,
) -> Result<Vec<Ustr>> {
    if nonterminal_definitions.is_empty() {
        return Ok(Vec::default());
    }

    let mut dependency_graph: UstrMap<UstrSet> = Default::default();
    for (varname, expr_id) in nonterminal_definitions {
        let mut referenced_nonterminals = get_expression_nonterminals(arena, *expr_id);
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

    Ok(result)
}

impl Grammar {
    pub fn parse(input_before: &str) -> std::result::Result<Self, chic::Error> {
        let (input_after, (arena, statements)) = match grammar(Span::new(input_before)).finish() {
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

        let g = Grammar { arena, statements };

        Ok(g)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use Expr::*;

    impl Expr {
        pub fn term(s: &str) -> Self {
            Self::Terminal {
                term: ustr(s),
                descr: None,
                fallback: 0,
                span: None,
            }
        }

        fn term_descr(s: &str, d: &str) -> Self {
            Self::Terminal {
                term: ustr(s),
                descr: Some(ustr(d)),
                fallback: 0,
                span: None,
            }
        }

        pub fn nontermref(s: &str) -> Self {
            Self::NontermRef {
                nonterm: ustr(s),
                fallback: 0,
                span: None,
            }
        }

        fn subword(expr: ExprId) -> Self {
            Self::Subword {
                phase: SubwordCompilationPhase::Expr(expr),
                fallback: 0,
                span: None,
            }
        }
    }

    // Extensional equality.  Ignores spans
    fn teq(left: ExprId, right: ExprId, arena: &[Expr]) -> bool {
        if left.to_index() == right.to_index() {
            return true;
        }

        match (&arena[left.to_index()], &arena[right.to_index()]) {
            (
                Expr::Terminal {
                    term: l,
                    descr: l_descr,
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Terminal {
                    term: r,
                    descr: r_descr,
                    fallback: r_fallback,
                    span: _,
                },
            ) => l == r && l_descr == r_descr && l_fallback == r_fallback,
            (
                Expr::NontermRef {
                    nonterm: l,
                    fallback: l_fallback,
                    span: _,
                },
                Expr::NontermRef {
                    nonterm: r,
                    fallback: r_fallback,
                    span: _,
                },
            ) => l == r && l_fallback == r_fallback,
            (
                Expr::Command {
                    cmd: l,
                    regex: l_regex,
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Command {
                    cmd: r,
                    regex: r_regex,
                    fallback: r_fallback,
                    span: _,
                },
            ) => l == r && l_regex == r_regex && l_fallback == r_fallback,
            (Expr::Sequence(l), Expr::Sequence(r))
            | (Expr::Alternative(l), Expr::Alternative(r))
            | (Expr::Fallback(l), Expr::Fallback(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                l.iter().zip(r.iter()).all(|(le, re)| teq(*le, *re, arena))
            }
            (Expr::Optional(l), Expr::Optional(r)) => teq(*l, *r, arena),
            (Expr::Many1(l), Expr::Many1(r)) => teq(*l, *r, arena),
            (
                Expr::DistributiveDescription {
                    child: l,
                    descr: l_descr,
                },
                Expr::DistributiveDescription {
                    child: r,
                    descr: r_descr,
                },
            ) => l_descr == r_descr && teq(*l, *r, arena),
            (
                Expr::Subword {
                    phase: SubwordCompilationPhase::Expr(l),
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Subword {
                    phase: SubwordCompilationPhase::Expr(r),
                    fallback: r_fallback,
                    span: _,
                },
            ) => l_fallback == r_fallback && teq(*l, *r, arena),
            (
                Expr::Subword {
                    phase: SubwordCompilationPhase::DFA(l),
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Subword {
                    phase: SubwordCompilationPhase::DFA(r),
                    fallback: r_fallback,
                    span: _,
                },
            ) => l_fallback == r_fallback && l == r,
            _ => false,
        }
    }

    #[test]
    fn parses_subword_expr() {
        const INPUT: &str = r#"--color=<WHEN>"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = subword_sequence_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let e2 = alloc(&mut arena, Expr::term("--color="));
        let e3 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let e1 = alloc(&mut arena, Sequence(vec![e2, e3]));
        let expected = alloc(
            &mut arena,
            Expr::Subword {
                phase: SubwordCompilationPhase::Expr(e1),
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_prefix_description_expr() {
        const INPUT: &str = r#"--color=<WHEN> "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let terminal_expr = Expr::term("--color=");
        let terminal_id = alloc(&mut arena, terminal_expr);
        let nonterm_expr = Expr::nontermref("WHEN");
        let nonterm_id = alloc(&mut arena, nonterm_expr);
        let sequence_expr = Sequence(vec![terminal_id, nonterm_id]);
        let sequence_id = alloc(&mut arena, sequence_expr);
        let subword_expr = Subword {
            phase: SubwordCompilationPhase::Expr(sequence_id),
            fallback: 0,
            span: None,
        };
        let subword_id = alloc(&mut arena, subword_expr);
        let expected = alloc(
            &mut arena,
            DistributiveDescription {
                child: subword_id,
                descr: ustr("use markers to highlight the matching strings"),
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_option_argument_alternative_description_expr() {
        const INPUT: &str =
            r#"(--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let terminal1_id = alloc(&mut arena, Expr::term("--color="));
        let nonterm1_id = alloc(&mut arena, Expr::nontermref("WHEN"));
        let sequence1_id = alloc(&mut arena, Sequence(vec![terminal1_id, nonterm1_id]));
        let subword_id = alloc(&mut arena, Expr::subword(sequence1_id));
        let terminal2_id = alloc(&mut arena, Expr::term("--color"));
        let nonterm2_id = alloc(&mut arena, Expr::nontermref("WHEN"));
        let sequence2_id = alloc(&mut arena, Sequence(vec![terminal2_id, nonterm2_id]));
        let alternative_id = alloc(&mut arena, Alternative(vec![subword_id, sequence2_id]));
        let expected = alloc(
            &mut arena,
            DistributiveDescription {
                child: alternative_id,
                descr: ustr("use markers to highlight the matching strings"),
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_word_terminal() {
        const INPUT: &str = r#"foo.bar"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("foo.bar"));
        assert!(teq(e, expected, &arena));
    }

    #[test]
    fn parses_short_option_terminal() {
        const INPUT: &str = r#"-f"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("-f"));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_long_option_terminal() {
        const INPUT: &str = r#"--foo"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("--foo"));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_symbol() {
        const INPUT: &str = "<FILE>";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = nonterm_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::nontermref("FILE"));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_command() {
        const INPUT: &str = "{{{ rustup toolchain list | cut -d' ' -f1 }}}";
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = command_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert!(
            matches!(arena[e.to_index()], Command { cmd: s, regex: None, fallback: 0, .. } if s == "rustup toolchain list | cut -d' ' -f1")
        );
    }

    #[test]
    fn parses_nontail_command() {
        const INPUT: &str = r#"{{{ foo }}}@bash"bar""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = nontail_command_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty(), "{:?}", s.fragment());
        let expected = alloc(
            &mut arena,
            Command {
                cmd: ustr("foo"),
                regex: Some(CmdRegex {
                    bash: Some(ustr("bar")),
                    fish: None,
                    zsh: None,
                }),
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_triple_brackets_command() {
        const INPUT: &str = "{{{ rad patch list | awk '{print $3}' | grep . | grep -vw ID }}}";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = command_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(
            &mut arena,
            Command {
                cmd: ustr("rad patch list | awk '{print $3}' | grep . | grep -vw ID"),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo>]";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm_id = alloc(&mut arena, Expr::nontermref("foo"));
        let expected = alloc(&mut arena, Optional(nonterm_id));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm_id = alloc(&mut arena, Expr::nontermref("foo"));
        let expected = alloc(&mut arena, Many1(nonterm_id));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_sequence_expr() {
        const INPUT: &str = "<first-symbol> <second symbol>";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm1_id = alloc(&mut arena, Expr::nontermref("first-symbol"));
        let nonterm2_id = alloc(&mut arena, Expr::nontermref("second symbol"));
        let expected = alloc(&mut arena, Sequence(vec![nonterm1_id, nonterm2_id]));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_alternative_expr() {
        const INPUT: &str = "a b | c";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let a_id = alloc(&mut arena, Expr::term("a"));
        let b_id = alloc(&mut arena, Expr::term("b"));
        let seq_id = alloc(&mut arena, Sequence(vec![a_id, b_id]));
        let c_id = alloc(&mut arena, Expr::term("c"));
        let expected = alloc(&mut arena, Alternative(vec![seq_id, c_id]));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_parenthesised_expr() {
        const INPUT: &str = r#"a (b | c)"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let a_id = alloc(&mut arena, Expr::term("a"));
        let b_id = alloc(&mut arena, Expr::term("b"));
        let c_id = alloc(&mut arena, Expr::term("c"));
        let alt_id = alloc(&mut arena, Alternative(vec![b_id, c_id]));
        let expected = alloc(&mut arena, Sequence(vec![a_id, alt_id]));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, v) = call_variant(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let Statement::CallVariant { head, expr: actual } = v else {
            unreachable!()
        };
        assert_eq!(head, ustr("foo"));
        let expected = alloc(&mut arena, Expr::term("bar"));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_grammar() {
        const INPUT: &str = r#"
foo bar;
foo baz;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1
        let (head, actual_expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("foo"));
        let expected_bar_id = alloc(&mut g.arena, Expr::term("bar"));
        assert!(teq(*actual_expr_id, expected_bar_id, &g.arena));

        // Statement 2
        let (head, actual_expr_id) = match &g.statements[1] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("foo"));
        let expected_baz_id = alloc(&mut g.arena, Expr::term("baz"));
        assert!(teq(*actual_expr_id, expected_baz_id, &g.arena));
    }

    #[test]
    fn bug1() {
        // Did not consider whitespace before ...
        const INPUT: &str = "darcs help ( ( -v | --verbose ) | ( -q | --quiet ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;";
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (*head, *expr),
            _ => panic!("Expected CallVariant"),
        };

        assert_eq!(head, ustr("darcs"));

        let help_id = alloc(&mut g.arena, Expr::term("help"));

        let v_id = alloc(&mut g.arena, Expr::term("-v"));
        let verbose_id = alloc(&mut g.arena, Expr::term("--verbose"));
        let v_alt_id = alloc(&mut g.arena, Alternative(vec![v_id, verbose_id]));

        let q_id = alloc(&mut g.arena, Expr::term("-q"));
        let quiet_id = alloc(&mut g.arena, Expr::term("--quiet"));
        let q_alt_id = alloc(&mut g.arena, Alternative(vec![q_id, quiet_id]));

        let top_alt_id = alloc(&mut g.arena, Alternative(vec![v_alt_id, q_alt_id]));
        let many1_id = alloc(&mut g.arena, Many1(top_alt_id));

        let darcs_subcommand_id = alloc(&mut g.arena, Expr::term("DARCS_SUBCOMMAND"));
        let optional_subcommand_id = alloc(&mut g.arena, Optional(darcs_subcommand_id));
        let darcs_command_id = alloc(&mut g.arena, Expr::nontermref("DARCS_COMMAND"));
        let seq_command_id = alloc(
            &mut g.arena,
            Sequence(vec![darcs_command_id, optional_subcommand_id]),
        );
        let optional_command_id = alloc(&mut g.arena, Optional(seq_command_id));

        let expected_expr_id = alloc(
            &mut g.arena,
            Sequence(vec![help_id, many1_id, optional_command_id]),
        );

        assert!(teq(expr_id, expected_expr_id, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 3);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let option_nonterm_id = alloc(&mut g.arena, Expr::nontermref("OPTION"));
        let optional_option_id = alloc(&mut g.arena, Optional(option_nonterm_id));
        let many1_option_id = alloc(&mut g.arena, Many1(optional_option_id));
        let patterns_nonterm_id = alloc(&mut g.arena, Expr::nontermref("PATTERNS"));
        let file_nonterm_id = alloc(&mut g.arena, Expr::nontermref("FILE"));
        let optional_file_id = alloc(&mut g.arena, Optional(file_nonterm_id));
        let many1_file_id = alloc(&mut g.arena, Many1(optional_file_id));
        let expected_expr1_id = alloc(
            &mut g.arena,
            Sequence(vec![many1_option_id, patterns_nonterm_id, many1_file_id]),
        );
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2: <OPTION> ::= ...
        let (symbol, shell, expr_id2) = match &g.statements[1] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("OPTION"));
        assert!(shell.is_none());
        let color_term_id = alloc(&mut g.arena, Expr::term("--color"));
        let when_nonterm_id = alloc(&mut g.arena, Expr::nontermref("WHEN"));
        let expected_expr2_id = alloc(&mut g.arena, Sequence(vec![color_term_id, when_nonterm_id]));
        assert!(teq(*expr_id2, expected_expr2_id, &g.arena));

        // Statement 3: <WHEN> ::= ...
        let (symbol, shell, expr_id3) = match &g.statements[2] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("WHEN"));
        assert!(shell.is_none());
        let always_term_id = alloc(&mut g.arena, Expr::term("always"));
        let never_term_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_term_id = alloc(&mut g.arena, Expr::term("auto"));
        let expected_expr3_id = alloc(
            &mut g.arena,
            Alternative(vec![always_term_id, never_term_id, auto_term_id]),
        );
        assert!(teq(*expr_id3, expected_expr3_id, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (symbol, shell, expr_id) = match &g.statements[0] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };

        assert_eq!(*symbol, ustr("OPTION"));
        assert!(shell.is_none());

        let extended_regexp_id = alloc(&mut g.arena, Expr::term("--extended-regexp"));
        let fixed_strings_id = alloc(&mut g.arena, Expr::term("--fixed-strings"));
        let basic_regexp_id = alloc(&mut g.arena, Expr::term("--basic-regexp"));
        let perl_regexp_id = alloc(&mut g.arena, Expr::term("--perl-regexp"));

        let expected_expr_id = alloc(
            &mut g.arena,
            Alternative(vec![
                extended_regexp_id,
                fixed_strings_id,
                basic_regexp_id,
                perl_regexp_id,
            ]),
        );

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
    }

    #[test]
    fn nonterminal_resolution_order_detects_trivial_cycle() {
        let mut arena: Vec<Expr> = vec![];
        let foo_expr = Expr::nontermref("BAR");
        let foo_id = alloc(&mut arena, foo_expr);
        let bar_expr = Expr::nontermref("FOO");
        let bar_id = alloc(&mut arena, bar_expr);
        let nonterminal_definitions =
            UstrMap::from_iter([(ustr("FOO"), foo_id), (ustr("BAR"), bar_id)]);
        assert!(matches!(
            get_nonterminals_resolution_order(&arena, &nonterminal_definitions),
            Err(Error::NonterminalDefinitionsCycle(None))
        ));
    }

    #[test]
    fn nonterminal_resolution_order_detects_simple_cycle() {
        let mut arena: Vec<Expr> = vec![];
        let foo_expr = Expr::nontermref("BAR");
        let foo_id = alloc(&mut arena, foo_expr);
        let bar_expr = Expr::nontermref("BAR");
        let bar_id = alloc(&mut arena, bar_expr);
        let nonterminal_definitions =
            UstrMap::from_iter([(ustr("FOO"), foo_id), (ustr("BAR"), bar_id)]);
        assert!(
            matches!(&get_nonterminals_resolution_order(&arena, &nonterminal_definitions), Err(Error::NonterminalDefinitionsCycle(Some(path))) if path == &[ustr("BAR"), ustr("BAR")])
        );
    }

    #[test]
    fn computes_nonterminals_resolution_order() {
        let mut arena: Vec<Expr> = vec![];
        let always_id = alloc(&mut arena, Expr::term("always"));
        let never_id = alloc(&mut arena, Expr::term("never"));
        let auto_id = alloc(&mut arena, Expr::term("auto"));
        let when_id = alloc(&mut arena, Alternative(vec![always_id, never_id, auto_id]));
        let foo_id = alloc(&mut arena, Expr::nontermref("WHEN"));
        let color_id = alloc(&mut arena, Expr::term("--color"));
        let option_foo_ref_id = alloc(&mut arena, Expr::nontermref("FOO"));
        let option_id = alloc(&mut arena, Sequence(vec![color_id, option_foo_ref_id]));
        let nonterminal_definitions = UstrMap::from_iter([
            (ustr("WHEN"), when_id),
            (ustr("FOO"), foo_id),
            (ustr("OPTION"), option_id),
        ]);
        assert_eq!(
            get_nonterminals_resolution_order(&arena, &nonterminal_definitions).unwrap(),
            vec![ustr("FOO"), ustr("OPTION")]
        );
    }

    #[test]
    fn parses_inline_shell_command() {
        const INPUT: &str = r#"
cargo [+{{{ rustup toolchain list | cut -d' ' -f1 }}}]
[<OPTIONS>] [<COMMAND>];
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("cargo"));

        let plus_id = alloc(&mut g.arena, Expr::term("+"));
        let cmd_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr("rustup toolchain list | cut -d' ' -f1"),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        let subword_seq_id = alloc(&mut g.arena, Sequence(vec![plus_id, cmd_id]));
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let optional1_id = alloc(&mut g.arena, Optional(subword_id));

        let options_id = alloc(&mut g.arena, Expr::nontermref("OPTIONS"));
        let optional2_id = alloc(&mut g.arena, Optional(options_id));

        let command_id = alloc(&mut g.arena, Expr::nontermref("COMMAND"));
        let optional3_id = alloc(&mut g.arena, Optional(command_id));

        let expected_expr_id = alloc(
            &mut g.arena,
            Sequence(vec![optional1_id, optional2_id, optional3_id]),
        );

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
    }

    #[test]
    fn parses_prefix_grammar() {
        const INPUT: &str = r#"
grep --color=<WHEN> --version;
<WHEN> ::= always | never | auto;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let color_eq_id = alloc(&mut g.arena, Expr::term("--color="));
        let when_nonterm_id = alloc(&mut g.arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(&mut g.arena, Sequence(vec![color_eq_id, when_nonterm_id]));
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let version_id = alloc(&mut g.arena, Expr::term("--version"));
        let expected_expr1_id = alloc(&mut g.arena, Sequence(vec![subword_id, version_id]));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2: <WHEN> ::= ...
        let (symbol, shell, expr_id2) = match &g.statements[1] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("WHEN"));
        assert!(shell.is_none());

        let always_id = alloc(&mut g.arena, Expr::term("always"));
        let never_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_id = alloc(&mut g.arena, Expr::term("auto"));
        let expected_expr2_id = alloc(
            &mut g.arena,
            Alternative(vec![always_id, never_id, auto_id]),
        );
        assert!(teq(*expr_id2, expected_expr2_id, &g.arena));
    }

    #[test]
    fn parens_suffice_to_parse_as_subword() {
        const INPUT: &str = r#"
grep --color=(always | never | auto);
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let color_eq_id = alloc(&mut g.arena, Expr::term("--color="));
        let always_id = alloc(&mut g.arena, Expr::term("always"));
        let never_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_id = alloc(&mut g.arena, Expr::term("auto"));
        let alt_id = alloc(
            &mut g.arena,
            Alternative(vec![always_id, never_id, auto_id]),
        );
        let seq_id = alloc(&mut g.arena, Sequence(vec![color_eq_id, alt_id]));
        let expected_expr_id = alloc(&mut g.arena, Expr::subword(seq_id));

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("strace"));
        let e_id = alloc(&mut g.arena, Expr::term("-e"));
        let expr_nonterm_id = alloc(&mut g.arena, Expr::nontermref("EXPR"));
        let expected_expr1_id = alloc(&mut g.arena, Sequence(vec![e_id, expr_nonterm_id]));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("EXPR"));
        assert!(shell.is_none());
        let qualifier_nonterm_id = alloc(&mut g.arena, Expr::nontermref("qualifier"));
        let eq_id = alloc(&mut g.arena, Expr::term("="));
        let seq1_id = alloc(&mut g.arena, Sequence(vec![qualifier_nonterm_id, eq_id]));
        let opt1_id = alloc(&mut g.arena, Optional(seq1_id));
        let bang_id = alloc(&mut g.arena, Expr::term("!"));
        let opt2_id = alloc(&mut g.arena, Optional(bang_id));
        let value_nonterm_id = alloc(&mut g.arena, Expr::nontermref("value"));
        let comma_id = alloc(&mut g.arena, Expr::term(","));
        let value_nonterm2_id = alloc(&mut g.arena, Expr::nontermref("value"));
        let seq2_id = alloc(&mut g.arena, Sequence(vec![comma_id, value_nonterm2_id]));
        let opt3_id = alloc(&mut g.arena, Optional(seq2_id));
        let many1_id = alloc(&mut g.arena, Many1(opt3_id));
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence(vec![opt1_id, opt2_id, value_nonterm_id, many1_id]),
        );
        let expected_expr2_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        // Statement 3
        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("qualifier"));
        assert!(shell.is_none());
        let trace_id = alloc(&mut g.arena, Expr::term("trace"));
        let read_id = alloc(&mut g.arena, Expr::term("read"));
        let write_id = alloc(&mut g.arena, Expr::term("write"));
        let fault_id = alloc(&mut g.arena, Expr::term("fault"));
        let expected_expr3_id = alloc(
            &mut g.arena,
            Alternative(vec![trace_id, read_id, write_id, fault_id]),
        );
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        // Statement 4
        let (symbol, shell, expr_id) = match &g.statements[3] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("value"));
        assert!(shell.is_none());
        let file_percent_id = alloc(&mut g.arena, Expr::term("%file"));
        let file_id = alloc(&mut g.arena, Expr::term("file"));
        let all_id = alloc(&mut g.arena, Expr::term("all"));
        let expected_expr4_id = alloc(
            &mut g.arena,
            Alternative(vec![file_percent_id, file_id, all_id]),
        );
        assert!(teq(*expr_id, expected_expr4_id, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("lsof"));
        let s_id = alloc(&mut g.arena, Expr::term("-s"));
        let protocol_id = alloc(&mut g.arena, Expr::nontermref("PROTOCOL"));
        let colon_id = alloc(&mut g.arena, Expr::term(":"));
        let state_spec_id = alloc(&mut g.arena, Expr::nontermref("STATE-SPEC"));
        let comma_id = alloc(&mut g.arena, Expr::term(","));
        let state_spec2_id = alloc(&mut g.arena, Expr::nontermref("STATE-SPEC"));
        let seq_id = alloc(&mut g.arena, Sequence(vec![comma_id, state_spec2_id]));
        let opt_id = alloc(&mut g.arena, Optional(seq_id));
        let many1_id = alloc(&mut g.arena, Many1(opt_id));
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence(vec![s_id, protocol_id, colon_id, state_spec_id, many1_id]),
        );
        let expected_expr1_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("PROTOCOL"));
        assert!(shell.is_none());
        let tcp_id = alloc(&mut g.arena, Expr::term("TCP"));
        let udp_id = alloc(&mut g.arena, Expr::term("UDP"));
        let expected_expr2_id = alloc(&mut g.arena, Alternative(vec![tcp_id, udp_id]));
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        // Statement 3
        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("STATE-SPEC"));
        assert!(shell.is_none());
        let caret_id = alloc(&mut g.arena, Expr::term("^"));
        let opt_caret_id = alloc(&mut g.arena, Optional(caret_id));
        let state_id = alloc(&mut g.arena, Expr::nontermref("STATE"));
        let subword_seq2_id = alloc(&mut g.arena, Sequence(vec![opt_caret_id, state_id]));
        let expected_expr3_id = alloc(&mut g.arena, Expr::subword(subword_seq2_id));
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        // Statement 4
        let (symbol, shell, expr_id) = match &g.statements[3] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("STATE"));
        assert!(shell.is_none());
        let listen_id = alloc(&mut g.arena, Expr::term("LISTEN"));
        let closed_id = alloc(&mut g.arena, Expr::term("CLOSED"));
        let expected_expr4_id = alloc(&mut g.arena, Alternative(vec![listen_id, closed_id]));
        assert!(teq(*expr_id, expected_expr4_id, &g.arena));
    }

    #[test]
    fn parses_shell_command_nonterminal_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> ::= {{{ rustup toolchain list | cut -d' ' -f1 }}};
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("cargo"));

        let plus_id = alloc(&mut g.arena, Expr::term("+"));
        let toolchain_id = alloc(&mut g.arena, Expr::nontermref("toolchain"));
        let subword_seq_id = alloc(&mut g.arena, Sequence(vec![plus_id, toolchain_id]));
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let opt1_id = alloc(&mut g.arena, Optional(subword_id));

        let options_id = alloc(&mut g.arena, Expr::nontermref("OPTIONS"));
        let opt2_id = alloc(&mut g.arena, Optional(options_id));

        let command_id = alloc(&mut g.arena, Expr::nontermref("COMMAND"));
        let opt3_id = alloc(&mut g.arena, Optional(command_id));

        let expected_expr1_id = alloc(&mut g.arena, Sequence(vec![opt1_id, opt2_id, opt3_id]));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("toolchain"));
        assert!(shell.is_none());
        let expected_expr2_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr("rustup toolchain list | cut -d' ' -f1"),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));
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
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(
            &mut arena,
            Expr::term_descr(
                "--extended-regexp",
                "PATTERNS are extended regular expressions",
            ),
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_term_descr_arg() {
        const INPUT: &str = r#"--context "print NUM lines of output context" <NUM>"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let term_id = alloc(
            &mut arena,
            Expr::term_descr("--context", "print NUM lines of output context"),
        );
        let nonterm_id = alloc(&mut arena, Expr::nontermref("NUM"));
        let expected = alloc(&mut arena, Sequence(vec![term_id, nonterm_id]));
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_description() {
        const INPUT: &str = r#"
grep --extended-regexp "PATTERNS are extended regular expressions";
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        let (head, actual) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, *expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));
        let expected = alloc(
            &mut g.arena,
            Expr::term_descr(
                "--extended-regexp",
                "PATTERNS are extended regular expressions",
            ),
        );
        assert!(teq(actual, expected, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("foo.sh"));
        let h_id = alloc(&mut g.arena, Expr::term("-h"));
        let expected_expr_id = alloc(&mut g.arena, Optional(h_id));
        assert!(teq(*expr_id, expected_expr_id, &g.arena));
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
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 3);

        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("ls"));
        let expected_expr1_id = alloc(&mut g.arena, Expr::nontermref("FILE"));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("FILE"));
        assert_eq!(*shell, Some(ustr("bash")));
        let expected_expr2_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr(r#"compgen -A file "$1""#),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("FILE"));
        assert_eq!(*shell, Some(ustr("fish")));
        let expected_expr3_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr(r#"__fish_complete_path "$1""#),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        let v = ValidGrammar::from_grammar(g, Shell::Bash).unwrap();
        let spec = v.specializations.get(&ustr("FILE")).unwrap();
        assert_eq!(spec.bash, Some(ustr(r#"compgen -A file "$1""#)));
        assert_eq!(spec.fish, Some(ustr(r#"__fish_complete_path "$1""#)));
        assert_eq!(spec.zsh, None);
    }

    #[test]
    fn distributes_descriptions() {
        const INPUT: &str = r#"mygrep (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(&mut arena, Sequence(vec![color_eq_id, when_id1]));
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(&mut arena, Sequence(vec![color_id, when_id2]));

        let alt_id = alloc(&mut arena, Alternative(vec![subword_id, seq2_id]));
        let expected_expr_id = alloc(&mut arena, Sequence(vec![mygrep_id, alt_id]));

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn spends_distributed_description() {
        const INPUT: &str = r#"mygrep --help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let help_id = alloc(&mut arena, Expr::term("--help"));
        let seq1_id = alloc(&mut arena, Sequence(vec![mygrep_id, help_id]));

        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(&mut arena, Sequence(vec![color_eq_id, when_id1]));
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(&mut arena, Sequence(vec![color_id, when_id2]));

        let alt2_id = alloc(&mut arena, Alternative(vec![subword_id, seq2_id]));

        let expected_expr_id = alloc(&mut arena, Alternative(vec![seq1_id, alt2_id]));

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn spends_distributed_description2() {
        const INPUT: &str = r#"mygrep (--help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings")"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let mygrep_id = alloc(&mut arena, Expr::term("mygrep"));
        let help_id = alloc(&mut arena, Expr::term("--help"));

        let color_eq_id = alloc(
            &mut arena,
            Expr::term_descr("--color=", "use markers to highlight the matching strings"),
        );
        let when_id1 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(&mut arena, Sequence(vec![color_eq_id, when_id1]));
        let subword_id = alloc(&mut arena, Expr::subword(subword_seq_id));

        let color_id = alloc(
            &mut arena,
            Expr::term_descr("--color", "use markers to highlight the matching strings"),
        );
        let when_id2 = alloc(&mut arena, Expr::nontermref("WHEN"));
        let seq2_id = alloc(&mut arena, Sequence(vec![color_id, when_id2]));

        let alt2_id = alloc(&mut arena, Alternative(vec![subword_id, seq2_id]));

        let alt1_id = alloc(&mut arena, Alternative(vec![help_id, alt2_id]));

        let expected_expr_id = alloc(&mut arena, Sequence(vec![mygrep_id, alt1_id]));

        assert!(teq(distributed_id, expected_expr_id, &arena));
    }

    #[test]
    fn parses_fallback_expr() {
        const INPUT: &str = r#"cmd (foo || bar)"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());

        let cmd_id = alloc(&mut arena, Expr::term("cmd"));
        let foo_id = alloc(&mut arena, Expr::term("foo"));
        let bar_id = alloc(&mut arena, Expr::term("bar"));
        let fallback_id = alloc(&mut arena, Fallback(vec![foo_id, bar_id]));
        let expected_expr_id = alloc(&mut arena, Sequence(vec![cmd_id, fallback_id]));

        assert!(teq(e, expected_expr_id, &arena));
    }

    #[test]
    fn parses_git_commit_range_subword() {
        const INPUT: &str = r#"{{{ git tag }}}..{{{ git tag }}}"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());

        let git_tag1_id = alloc(
            &mut arena,
            Command {
                cmd: ustr("git tag"),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        let dotdot_id = alloc(&mut arena, Expr::term(".."));
        let git_tag2_id = alloc(
            &mut arena,
            Command {
                cmd: ustr("git tag"),
                regex: None,
                fallback: 0,
                span: None,
            },
        );
        let seq_id = alloc(
            &mut arena,
            Sequence(vec![git_tag1_id, dotdot_id, git_tag2_id]),
        );
        let expected_expr_id = alloc(&mut arena, Expr::subword(seq_id));

        assert!(teq(e, expected_expr_id, &arena));
    }

    #[test]
    fn ignores_form_feed() {
        const INPUT: &str = "cmd \u{000C} \u{000C} \u{000C}\u{000C} foo";
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());

        let cmd_id = alloc(&mut arena, Expr::term("cmd"));
        let foo_id = alloc(&mut arena, Expr::term("foo"));
        let expected_expr_id = alloc(&mut arena, Sequence(vec![cmd_id, foo_id]));

        assert!(teq(e, expected_expr_id, &arena));
    }

    #[test]
    fn parses_nontail_command_script() {
        const INPUT: &str = r#"cmd {{{ echo foo }}}@bash"bar"@fish"baz"@zsh"quux""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty(), "{:?}", s.fragment());

        let cmd_id = alloc(&mut arena, Expr::term("cmd"));
        let echo_id = alloc(
            &mut arena,
            Command {
                cmd: ustr("echo foo"),
                regex: Some(CmdRegex {
                    bash: Some(ustr("bar")),
                    fish: Some(ustr("baz")),
                    zsh: Some(ustr("quux")),
                }),
                fallback: 0,
                span: None,
            },
        );
        let expected_expr_id = alloc(&mut arena, Sequence(vec![cmd_id, echo_id]));

        assert!(teq(e, expected_expr_id, &arena));
    }
}
