use std::debug_assert;

use hashbrown::HashMap;
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
    DistributiveDescription(ExprId, Ustr),

    // `foo || bar`
    Fallback(Vec<ExprId>),
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
                f.write_fmt(format_args!(r#"Subword({subword:?}, {level})"#))
            }
            Expr::Terminal(term, Some(descr), level, ..) => f.write_fmt(format_args!(
                r#"Terminal(ustr(\"{term}\"), Some(ustr(\"{}\"))), {level})"#,
                descr
            )),
            Expr::Terminal(term, None, level, ..) => {
                f.write_fmt(format_args!(r#"Terminal(ustr(\"{term}\"), None, {level})"#))
            }
            Expr::NontermRef(nonterm, level, _) => {
                f.write_fmt(format_args!(r#"Nonterminal(ustr(\"{nonterm}\"), {level})"#))
            }
            Self::Command(cmd, regex, level, span) => f.write_fmt(format_args!(
                r#"Command(ustr({cmd:?}), {regex:?}, {level}, {span:?})"#
            )),
            Self::Sequence(arg0) => f.write_fmt(format_args!(r#"Sequence(vec!{:?})"#, arg0)),
            Self::Alternative(arg0) => f.write_fmt(format_args!(r#"Alternative(vec!{:?})"#, arg0)),
            Self::Optional(arg0) => f.write_fmt(format_args!(r#"Optional({:?})"#, arg0)),
            Self::Many1(arg0) => f.write_fmt(format_args!(r#"Many1({:?})"#, arg0)),
            Self::DistributiveDescription(expr, descr) => f.write_fmt(format_args!(
                r#"DistributiveDescription({expr:?}, {descr:?})"#
            )),
            Self::Fallback(arg0) => f.write_fmt(format_args!(r#"Fallback(vec!{:?})"#, arg0)),
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
        Expr::Subword(subword, ..) => {
            let expr = match subword {
                SubwordCompilationPhase::Expr(expr) => *expr,
                SubwordCompilationPhase::DFA(_) => unreachable!(),
            };
            let mut seq: Box<railroad::Sequence<Box<dyn railroad::Node>>> = Default::default();
            seq.push(Box::new(railroad::SimpleStart));
            seq.push(railroad_node_from_expr(arena, expr));
            seq.push(Box::new(railroad::SimpleEnd));
            seq
        }
        Expr::Terminal(s, ..) => Box::new(railroad::Terminal::new(s.as_str().to_string())),
        Expr::NontermRef(s, ..) => Box::new(railroad::NonTerminal::new(s.as_str().to_string())),
        Expr::Command(s, ..) => Box::new(railroad::Comment::new(s.as_str().to_string())),
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
        Expr::DistributiveDescription(subexpr, description) => {
            let inner = railroad_node_from_expr(arena, *subexpr);
            let label = railroad::Comment::new(description.to_string());
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

fn terminal_opt_description_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (after, term) = terminal(input)?;
    let (after, descr) = opt(preceded(multiblanks0, description))(after)?;
    let expr = Expr::Terminal(
        ustr(&term),
        descr.map(|span| ustr(&span)),
        0,
        ChicSpan::new(input, after),
    );
    let id = alloc(arena, expr);
    Ok((after, id))
}

fn nonterm(input: Span) -> IResult<Span, Span> {
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    Ok((input, name))
}

fn nonterm_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, nonterm) = context("nonterminal", nonterm)(input)?;
    let diagnostic_span = ChicSpan::new(input, after);
    let e = Expr::NontermRef(ustr(nonterm.into_fragment()), 0, diagnostic_span);
    let id = alloc(arena, e);
    Ok((after, id))
}

fn triple_bracket_command(input: Span) -> IResult<Span, Span> {
    let (input, _) = tag("{{{")(input)?;
    let (input, cmd) = take_until("}}}")(input)?;
    let (input, _) = tag("}}}")(input)?;
    Ok((input, Span::new(cmd.into_fragment().trim())))
}

fn command_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = ChicSpan::new(input, after);
    let e = Expr::Command(ustr(cmd.into_fragment()), None, 0, command_span);
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

fn nontail_command_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    mut input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (after, cmd) = triple_bracket_command(input)?;
    let command_span = ChicSpan::new(input, after);
    input = after;
    let (input, regex_decl) = cmd_regex_decl(input)?;
    let e = Expr::Command(ustr(cmd.into_fragment()), Some(regex_decl), 0, command_span);
    let id = alloc(arena, e);
    Ok((input, id))
}

fn optional_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = char('[')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, expr) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char(']')(input)?;
    let id = alloc(arena, Expr::Optional(expr));
    Ok((input, id))
}

fn parenthesized_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
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

fn unary_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
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

fn subword_sequence_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
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
        let span = ChicSpan::new(input, after);
        let subword_id = alloc(arena, e);
        let subword_expr = Expr::Subword(SubwordCompilationPhase::Expr(subword_id), 0, span);
        alloc(arena, subword_expr)
    };
    Ok((after, result))
}

fn subword_sequence_expr_opt_description<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (input, expr_id) = subword_sequence_expr(arena, input)?;
    let (input, description) = opt(preceded(multiblanks0, description))(input)?;
    let result = match description {
        Some(descr) => {
            let e = Expr::DistributiveDescription(expr_id, ustr(&descr));
            alloc(arena, e)
        }
        None => expr_id,
    };
    Ok((input, result))
}

fn sequence_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
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

fn do_alternative_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char('|')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = sequence_expr(arena, input)?;
    Ok((input, right))
}

fn alternative_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
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

fn do_fallback_expr<'a, 's>(
    arena: &'a mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("||")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = alternative_expr(arena, input)?;
    Ok((input, right))
}

fn fallback_expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
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

fn expr<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
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

fn call_variant<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, Statement> {
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

fn nonterm_def_statement<'a, 's>(
    arena: &'a mut Vec<Expr>,
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

fn statement<'a, 's>(arena: &'a mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, Statement> {
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

#[derive(Debug, PartialEq, Clone)]
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
        let Expr::Command(command, ..) = &arena[expr.to_index()] else {
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
        Expr::Terminal(..) | Expr::NontermRef(..) | Expr::Command(..) => expr_id,
        Expr::Subword(child, ..) => {
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
        Expr::DistributiveDescription(child, description) => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(arena, Expr::DistributiveDescription(new_child, description))
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
        Expr::Subword(subword_expr, fallback_level, span) => {
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
                Expr::Subword(SubwordCompilationPhase::DFA(subdfaid), fallback_level, span),
            )
        }
        Expr::Terminal(..) | Expr::NontermRef(..) | Expr::Command(..) => expr_id,
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
        Expr::DistributiveDescription(..) => unreachable!(
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
        Expr::DistributiveDescription(child, descr) => {
            let new_child = do_distribute_descriptions(arena, child, &mut Some(descr));
            if child == new_child { child } else { new_child }
        }
        Expr::Terminal(term, None, level, span) if description.is_some() => {
            let result = Expr::Terminal(term, *description, level, span);
            *description = None; // spend it
            alloc(arena, result)
        }
        Expr::Terminal(..) => expr_id,
        Expr::NontermRef(..) | Expr::Command(..) => expr_id,
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
        Expr::Subword(SubwordCompilationPhase::Expr(child), fallback_level, span) => {
            let new_child = do_distribute_descriptions(arena, child, description);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword(
                        SubwordCompilationPhase::Expr(new_child),
                        fallback_level,
                        span,
                    ),
                )
            }
        }
        Expr::Subword(SubwordCompilationPhase::DFA(_), ..) => unreachable!(),
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
        Expr::Terminal(_, _, level, _) if level == fallback_level => expr_id,
        Expr::Terminal(term, descr, _, span) => {
            alloc(arena, Expr::Terminal(term, descr, fallback_level, span))
        }
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
        Expr::NontermRef(..) | Expr::Command(..) => expr_id,
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
        Expr::Subword(SubwordCompilationPhase::Expr(child), _, span) => {
            let new_child = do_propagate_fallback_levels(arena, child, fallback_level);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword(
                        SubwordCompilationPhase::Expr(new_child),
                        fallback_level,
                        span,
                    ),
                )
            }
        }
        Expr::Subword(SubwordCompilationPhase::DFA(..), ..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!(),
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
        Expr::Terminal(..)
        | Expr::NontermRef(..)
        | Expr::Command(..)
        | Expr::Alternative(..)
        | Expr::Fallback(..)
        | Expr::Optional(..)
        | Expr::Many1(..) => expr_id,
        Expr::Sequence(children) => expr_get_head(arena, *children.first().unwrap()),
        Expr::Subword(..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
    }
}

fn expr_get_tail(arena: &[Expr], expr_id: ExprId) -> ExprId {
    match &arena[expr_id.to_index()] {
        Expr::Terminal(..)
        | Expr::NontermRef(..)
        | Expr::Command(..)
        | Expr::Alternative(..)
        | Expr::Fallback(..)
        | Expr::Optional(..)
        | Expr::Many1(..) => expr_id,
        Expr::Sequence(children) => expr_get_head(arena, *children.last().unwrap()),
        Expr::Subword(..) => unreachable!(),
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
    }
}

fn check_subword_spaces(arena: &[Expr], expr_id: ExprId, nonterms: &UstrMap<ExprId>) -> Result<()> {
    let mut nonterm_expn_trace: Vec<ChicSpan> = Default::default();
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
    nonterm_expn_trace: &mut Vec<ChicSpan>,
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
                match (&arena[left_tail.to_index()], &arena[right_head.to_index()]) {
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
                    arena,
                    *child,
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
            nonterm_expn_trace.push(*span);
            do_check_subword_spaces(arena, *expn, nonterms, nonterm_expn_trace, within_subword)?;
            nonterm_expn_trace.pop();
            Ok(())
        }
        Expr::Command(..) => Ok(()),
        Expr::Subword(SubwordCompilationPhase::Expr(child), ..) => {
            do_check_subword_spaces(arena, *child, nonterms, nonterm_expn_trace, true)
        }
        Expr::Subword(SubwordCompilationPhase::DFA(..), ..) => {
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
        Expr::DistributiveDescription(..) => unreachable!("wrong compilation phases order"),
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
        Expr::Terminal(..) | Expr::Command(..) => expr_id,
        Expr::Subword(child, fallback_level, span) => {
            let child = match child {
                SubwordCompilationPhase::Expr(e) => e,
                SubwordCompilationPhase::DFA(..) => unreachable!(),
            };
            let new_child =
                resolve_nonterminals(arena, child, vars, specializations, unused_nonterminals);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Subword(
                        SubwordCompilationPhase::Expr(new_child),
                        fallback_level,
                        span,
                    ),
                )
            }
        }
        Expr::NontermRef(name, ..) => {
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
        Expr::DistributiveDescription(..) => unreachable!(
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
        Expr::Terminal(..) | Expr::Command(..) => {}
        Expr::Subword(subexpr, ..) => {
            let subexpr = match subexpr {
                SubwordCompilationPhase::Expr(e) => *e,
                SubwordCompilationPhase::DFA(..) => unreachable!(),
            };
            do_get_expression_nonterminals(arena, subexpr, deps);
        }
        Expr::NontermRef(varname, ..) => {
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
        Expr::DistributiveDescription(..) => unreachable!(
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

    log::debug!("nonterminals expansion order: {:?}", result);
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
    use itertools::Itertools;
    use proptest::prelude::*;
    use proptest::{strategy::BoxedStrategy, test_runner::TestRng};
    use std::cell::RefCell;
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

    impl Expr {
        pub fn term(s: &str) -> Self {
            Self::Terminal(ustr(s), None, 0, ChicSpan::Dummy)
        }

        fn term_descr(s: &str, d: &str) -> Self {
            Self::Terminal(ustr(s), Some(ustr(d)), 0, ChicSpan::Dummy)
        }

        pub fn nontermref(s: &str) -> Self {
            Self::NontermRef(ustr(s), 0, ChicSpan::dummy())
        }

        fn subword(expr: ExprId) -> Self {
            Self::Subword(SubwordCompilationPhase::Expr(expr), 0, ChicSpan::Dummy)
        }
    }

    fn expr_extensionally_eq(left: ExprId, right: ExprId, arena: &[Expr]) -> bool {
        if left.to_index() == right.to_index() {
            return true;
        }

        match (&arena[left.to_index()], &arena[right.to_index()]) {
            (Expr::Terminal(l, l_descr, _, _), Expr::Terminal(r, r_descr, _, _)) => {
                l == r && l_descr == r_descr
            }
            (
                Expr::Subword(SubwordCompilationPhase::Expr(l), _, _),
                Expr::Subword(SubwordCompilationPhase::Expr(r), _, _),
            ) => expr_extensionally_eq(*l, *r, arena),
            (
                Expr::Subword(SubwordCompilationPhase::DFA(l), _, _),
                Expr::Subword(SubwordCompilationPhase::DFA(r), _, _),
            ) => l == r,
            (Expr::NontermRef(l, _, _), Expr::NontermRef(r, _, _)) => l == r,
            (Expr::Command(l, l_regex, _, _), Expr::Command(r, r_regex, _, _)) => {
                l == r && l_regex == r_regex
            }
            (Expr::Sequence(l), Expr::Sequence(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                l.iter()
                    .zip(r.iter())
                    .all(|(le, re)| expr_extensionally_eq(*le, *re, arena))
            }
            (Expr::Alternative(l), Expr::Alternative(r))
            | (Expr::Fallback(l), Expr::Fallback(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                let mut r_used = vec![false; r.len()];
                for le in l.iter() {
                    if r.iter()
                        .enumerate()
                        .find(|(i, re)| !r_used[*i] && expr_extensionally_eq(*le, **re, arena))
                        .map(|(i, _)| r_used[i] = true)
                        .is_none()
                    {
                        return false;
                    }
                }
                true
            }
            (Expr::Optional(l), Expr::Optional(r)) => expr_extensionally_eq(*l, *r, arena),
            (Expr::Many1(l), Expr::Many1(r)) => expr_extensionally_eq(*l, *r, arena),
            (
                Expr::DistributiveDescription(l, l_descr),
                Expr::DistributiveDescription(r, r_descr),
            ) => l_descr == r_descr && expr_extensionally_eq(*l, *r, arena),
            _ => false,
        }
    }

    fn arb_literal(arena: Rc<RefCell<Vec<Expr>>>, inputs: Rc<Vec<Ustr>>) -> BoxedStrategy<ExprId> {
        (0..inputs.len())
            .prop_map(move |index| {
                let e = Expr::term(&inputs[index]);
                let id = ExprId(arena.borrow().len());
                arena.borrow_mut().push(e);
                id
            })
            .boxed()
    }

    fn arb_nonterminal(
        arena: Rc<RefCell<Vec<Expr>>>,
        nonterminals: Rc<Vec<Ustr>>,
    ) -> BoxedStrategy<ExprId> {
        (0..nonterminals.len())
            .prop_map(move |index| {
                let e = Expr::nontermref(&nonterminals[index]);
                let id = ExprId(arena.borrow().len());
                arena.borrow_mut().push(e);
                id
            })
            .boxed()
    }

    fn arb_optional(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth - 1,
            max_width,
        )
        .prop_map(move |e| {
            let e = Optional(e);
            let id = ExprId(arena.borrow().len());
            arena.borrow_mut().push(e);
            id
        })
        .boxed()
    }

    fn arb_many1(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth - 1,
            max_width,
        )
        .prop_map(move |e| {
            let e = Many1(e);
            let id = ExprId(arena.borrow().len());
            arena.borrow_mut().push(e);
            id
        })
        .boxed()
    }

    fn arb_sequence(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                let a = Rc::clone(&arena);
                prop::collection::vec(e, width).prop_map(move |v| {
                    let e = Sequence(v);
                    let id = ExprId(a.borrow().len());
                    a.borrow_mut().push(e);
                    id
                })
            })
            .boxed()
    }

    fn arb_alternative(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        (2..max_width)
            .prop_flat_map(move |width| {
                let e = arb_expr(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth - 1,
                    max_width,
                );
                let a = Rc::clone(&arena);
                prop::collection::vec(e, width).prop_map(move |v| {
                    let e = Alternative(v);
                    let id = ExprId(a.borrow().len());
                    a.borrow_mut().push(e);
                    id
                })
            })
            .boxed()
    }

    pub fn arb_expr(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<ExprId> {
        if remaining_depth <= 1 {
            prop_oneof![
                arb_literal(Rc::clone(&arena), Rc::clone(&inputs)),
                arb_nonterminal(Rc::clone(&arena), nonterminals),
            ]
            .boxed()
        } else {
            prop_oneof![
                arb_literal(Rc::clone(&arena), inputs.clone()),
                arb_nonterminal(Rc::clone(&arena), nonterminals.clone()),
                arb_optional(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_many1(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_sequence(
                    Rc::clone(&arena),
                    inputs.clone(),
                    nonterminals.clone(),
                    remaining_depth,
                    max_width
                ),
                arb_alternative(
                    Rc::clone(&arena),
                    inputs,
                    nonterminals,
                    remaining_depth,
                    max_width
                ),
            ]
            .boxed()
        }
    }

    pub fn do_arb_match(
        arena: Rc<RefCell<Vec<Expr>>>,
        expr: ExprId,
        rng: &mut TestRng,
        max_width: usize,
        output: &mut Vec<Ustr>,
    ) {
        match &arena.borrow()[expr.to_index()] {
            Terminal(s, ..) => output.push(*s),
            Subword(sw, ..) => {
                let e = match sw {
                    SubwordCompilationPhase::Expr(e) => e,
                    SubwordCompilationPhase::DFA(_) => unreachable!(),
                };
                let mut out: Vec<Ustr> = Default::default();
                do_arb_match(Rc::clone(&arena), *e, rng, max_width, &mut out);
                let joined = out.into_iter().join("");
                output.push(ustr(&joined));
            }
            NontermRef(..) => output.push(ustr("anything")),
            Command(..) => output.push(ustr("anything")),
            Sequence(v) => {
                for subexpr in v {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            Alternative(v) => {
                let chosen_alternative =
                    usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(
                    Rc::clone(&arena),
                    v[chosen_alternative],
                    rng,
                    max_width,
                    output,
                );
            }
            Optional(subexpr) => {
                if rng.next_u64() % 2 == 0 {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            Many1(subexpr) => {
                let n = rng.next_u64();
                let chosen_len = n % u64::try_from(max_width).unwrap() + 1;
                for _ in 0..chosen_len {
                    do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                }
            }
            DistributiveDescription(subexpr, description) => {
                do_arb_match(Rc::clone(&arena), *subexpr, rng, max_width, output);
                output.push(ustr(&format!(r#""{description}""#)));
            }
            Fallback(v) => {
                let chosen_alternative =
                    usize::try_from(rng.next_u64().rem(u64::try_from(v.len()).unwrap())).unwrap();
                do_arb_match(
                    Rc::clone(&arena),
                    v[chosen_alternative],
                    rng,
                    max_width,
                    output,
                );
            }
        }
    }

    pub fn arb_match(
        arena: Rc<RefCell<Vec<Expr>>>,
        e: ExprId,
        mut rng: TestRng,
        max_width: usize,
    ) -> (ExprId, Rc<RefCell<Vec<Expr>>>, Vec<Ustr>) {
        let mut output: Vec<Ustr> = Default::default();
        do_arb_match(Rc::clone(&arena), e, &mut rng, max_width, &mut output);
        (e, arena, output)
    }

    // Produce an arbitrary sequence matching `e`.
    pub fn arb_expr_match(
        arena: Rc<RefCell<Vec<Expr>>>,
        inputs: Rc<Vec<Ustr>>,
        nonterminals: Rc<Vec<Ustr>>,
        remaining_depth: usize,
        max_width: usize,
    ) -> BoxedStrategy<(ExprId, Rc<RefCell<Vec<Expr>>>, Vec<Ustr>)> {
        arb_expr(
            Rc::clone(&arena),
            inputs,
            nonterminals,
            remaining_depth,
            max_width,
        )
        .prop_perturb(move |e, rng| arb_match(Rc::clone(&arena), e, rng, max_width))
        .boxed()
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
            Expr::Subword(SubwordCompilationPhase::Expr(e1), 0, ChicSpan::Dummy),
        );
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        let subword_expr = Subword(
            SubwordCompilationPhase::Expr(sequence_id),
            0,
            ChicSpan::Dummy,
        );
        let subword_id = alloc(&mut arena, subword_expr);
        let expected = alloc(
            &mut arena,
            DistributiveDescription(
                subword_id,
                ustr("use markers to highlight the matching strings"),
            ),
        );
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
            DistributiveDescription(
                alternative_id,
                ustr("use markers to highlight the matching strings"),
            ),
        );
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_word_terminal() {
        const INPUT: &str = r#"foo.bar"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("foo.bar"));
        assert!(expr_extensionally_eq(e, expected, &arena));
    }

    #[test]
    fn parses_short_option_terminal() {
        const INPUT: &str = r#"-f"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("-f"));
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_long_option_terminal() {
        const INPUT: &str = r#"--foo"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = terminal_opt_description_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::term("--foo"));
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_symbol() {
        const INPUT: &str = "<FILE>";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = nonterm_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(&mut arena, Expr::nontermref("FILE"));
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_command() {
        const INPUT: &str = "{{{ rustup toolchain list | cut -d' ' -f1 }}}";
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = command_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert!(
            matches!(arena[e.to_index()], Command(s, None, 0, ..) if s == "rustup toolchain list | cut -d' ' -f1")
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
            Command(
                ustr("foo"),
                Some(CmdRegexDecl {
                    bash: Some(ustr("bar")),
                    fish: None,
                    zsh: None,
                }),
                0,
                ChicSpan::dummy(),
            ),
        );
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_triple_brackets_command() {
        const INPUT: &str = "{{{ rad patch list | awk '{print $3}' | grep . | grep -vw ID }}}";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = command_expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let expected = alloc(
            &mut arena,
            Command(
                ustr("rad patch list | awk '{print $3}' | grep . | grep -vw ID"),
                None,
                0,
                ChicSpan::dummy(),
            ),
        );
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_optional_expr() {
        const INPUT: &str = "[<foo>]";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm_id = alloc(&mut arena, Expr::nontermref("foo"));
        let expected = alloc(&mut arena, Optional(nonterm_id));
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm_id = alloc(&mut arena, Expr::nontermref("foo"));
        let expected = alloc(&mut arena, Many1(nonterm_id));
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        assert!(expr_extensionally_eq(actual, expected, &arena));
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, v) = call_variant(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        match v {
            Statement::CallVariant { head, expr } => {
                assert_eq!(head, u("foo"));
                let expected = Expr::term("bar");
                assert_eq!(arena[expr.to_index()], expected);
            }
            _ => panic!("Expected a call variant"),
        };
    }

    #[test]
    fn parses_grammar() {
        const INPUT: &str = r#"
foo bar;
foo baz;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);
        let bar_expr = Expr::term("bar");
        let baz_expr = Expr::term("baz");
        match &g.statements[0] {
            Statement::CallVariant { head, expr } => {
                assert_eq!(*head, u("foo"));
                assert_eq!(g.arena[expr.to_index()], bar_expr);
            }
            _ => panic!("Expected a call variant"),
        }
        match &g.statements[1] {
            Statement::CallVariant { head, expr } => {
                assert_eq!(*head, u("foo"));
                assert_eq!(g.arena[expr.to_index()], baz_expr);
            }
            _ => panic!("Expected a call variant"),
        }
    }

    #[test]
    fn bug1() {
        // Did not consider whitespace before ...
        const INPUT: &str = "darcs help ( ( -v | --verbose ) | ( -q | --quiet ) ) ... [<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  ;";
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };

        assert_eq!(*head, u("darcs"));

        let top_level_seq_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(top_level_seq_children.len(), 3);

        // 1. Terminal("help")
        assert_eq!(
            g.arena[top_level_seq_children[0].to_index()],
            Expr::term("help")
        );

        // 2. Many1(...)
        let many1_child_id = match &g.arena[top_level_seq_children[1].to_index()] {
            Expr::Many1(child) => *child,
            _ => panic!("Expected Many1"),
        };

        let alt1_children = match &g.arena[many1_child_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt1_children.len(), 2);

        let alt1_1_children = match &g.arena[alt1_children[0].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt1_1_children.len(), 2);
        assert_eq!(g.arena[alt1_1_children[0].to_index()], Expr::term("-v"));
        assert_eq!(
            g.arena[alt1_1_children[1].to_index()],
            Expr::term("--verbose")
        );

        let alt1_2_children = match &g.arena[alt1_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt1_2_children.len(), 2);
        assert_eq!(g.arena[alt1_2_children[0].to_index()], Expr::term("-q"));
        assert_eq!(
            g.arena[alt1_2_children[1].to_index()],
            Expr::term("--quiet")
        );

        // 3. Optional(...)
        let optional1_child_id = match &g.arena[top_level_seq_children[2].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };

        let seq2_children = match &g.arena[optional1_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);

        assert_eq!(
            g.arena[seq2_children[0].to_index()],
            Expr::nontermref("DARCS_COMMAND")
        );

        let optional2_child_id = match &g.arena[seq2_children[1].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };

        assert_eq!(
            g.arena[optional2_child_id.to_index()],
            Expr::term("DARCS_SUBCOMMAND")
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
        assert_eq!(g.statements.len(), 3);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, u("grep"));

        let seq1_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 3);

        let many1_child_id = match &g.arena[seq1_children[0].to_index()] {
            Expr::Many1(child) => *child,
            _ => panic!("Expected Many1"),
        };
        let optional_child_id = match &g.arena[many1_child_id.to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional_child_id.to_index()],
            Expr::nontermref("OPTION")
        );

        assert_eq!(
            g.arena[seq1_children[1].to_index()],
            Expr::nontermref("PATTERNS")
        );

        let many1_child_id2 = match &g.arena[seq1_children[2].to_index()] {
            Expr::Many1(child) => *child,
            _ => panic!("Expected Many1"),
        };
        let optional_child_id2 = match &g.arena[many1_child_id2.to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional_child_id2.to_index()],
            Expr::nontermref("FILE")
        );

        // Statement 2: <OPTION> ::= ...
        let (symbol, shell, expr_id2) = match &g.statements[1] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, u("OPTION"));
        assert!(shell.is_none());
        let seq2_children = match &g.arena[expr_id2.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(g.arena[seq2_children[0].to_index()], Expr::term("--color"));
        assert_eq!(
            g.arena[seq2_children[1].to_index()],
            Expr::nontermref("WHEN")
        );

        // Statement 3: <WHEN> ::= ...
        let (symbol, shell, expr_id3) = match &g.statements[2] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, u("WHEN"));
        assert!(shell.is_none());
        let alt3_children = match &g.arena[expr_id3.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt3_children.len(), 3);
        assert_eq!(g.arena[alt3_children[0].to_index()], Expr::term("always"));
        assert_eq!(g.arena[alt3_children[1].to_index()], Expr::term("never"));
        assert_eq!(g.arena[alt3_children[2].to_index()], Expr::term("auto"));
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
        assert_eq!(g.statements.len(), 1);

        let (symbol, shell, expr_id) = match &g.statements[0] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };

        assert_eq!(*symbol, u("OPTION"));
        assert!(shell.is_none());

        let children = match &g.arena[expr_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };

        assert_eq!(children.len(), 4);
        assert_eq!(
            g.arena[children[0].to_index()],
            Expr::term("--extended-regexp")
        );
        assert_eq!(
            g.arena[children[1].to_index()],
            Expr::term("--fixed-strings")
        );
        assert_eq!(
            g.arena[children[2].to_index()],
            Expr::term("--basic-regexp")
        );
        assert_eq!(g.arena[children[3].to_index()], Expr::term("--perl-regexp"));
    }

    #[test]
    fn nonterminal_resolution_order_detects_trivial_cycle() {
        let mut arena: Vec<Expr> = vec![];
        let foo_expr = Expr::nontermref("BAR");
        let foo_id = alloc(&mut arena, foo_expr);
        let bar_expr = Expr::nontermref("FOO");
        let bar_id = alloc(&mut arena, bar_expr);
        let nonterminal_definitions = UstrMap::from_iter([(u("FOO"), foo_id), (u("BAR"), bar_id)]);
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
        let nonterminal_definitions = UstrMap::from_iter([(u("FOO"), foo_id), (u("BAR"), bar_id)]);
        assert!(
            matches!(&get_nonterminals_resolution_order(&arena, &nonterminal_definitions), Err(Error::NonterminalDefinitionsCycle(Some(path))) if path == &[u("BAR"), u("BAR")])
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
            (u("WHEN"), when_id),
            (u("FOO"), foo_id),
            (u("OPTION"), option_id),
        ]);
        assert_eq!(
            get_nonterminals_resolution_order(&arena, &nonterminal_definitions).unwrap(),
            vec![u("FOO"), u("OPTION")]
        );
    }

    #[test]
    fn parses_inline_shell_command() {
        const INPUT: &str = r#"
cargo [+{{{ rustup toolchain list | cut -d' ' -f1 }}}]
[<OPTIONS>] [<COMMAND>];
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("cargo"));
        let seq1_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 3);
        let optional1_child_id = match &g.arena[seq1_children[0].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        let subword_phase = match &g.arena[optional1_child_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq2_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(g.arena[seq2_children[0].to_index()], Expr::term("+"));
        assert_eq!(
            g.arena[seq2_children[1].to_index()],
            Command(
                ustr("rustup toolchain list | cut -d' ' -f1"),
                None,
                0,
                ChicSpan::dummy()
            )
        );
        let optional2_child_id = match &g.arena[seq1_children[1].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional2_child_id.to_index()],
            Expr::nontermref("OPTIONS")
        );
        let optional3_child_id = match &g.arena[seq1_children[2].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional3_child_id.to_index()],
            Expr::nontermref("COMMAND")
        );
    }

    #[test]
    fn parses_prefix_grammar() {
        const INPUT: &str = r#"
grep --color=<WHEN> --version;
<WHEN> ::= always | never | auto;
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let seq1_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 2);

        let subword_phase = match &g.arena[seq1_children[0].to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq2_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(g.arena[seq2_children[0].to_index()], Expr::term("--color="));
        assert_eq!(
            g.arena[seq2_children[1].to_index()],
            Expr::nontermref("WHEN")
        );

        assert_eq!(
            g.arena[seq1_children[1].to_index()],
            Expr::term("--version")
        );

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
        let alt2_children = match &g.arena[expr_id2.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt2_children.len(), 3);
        assert_eq!(g.arena[alt2_children[0].to_index()], Expr::term("always"));
        assert_eq!(g.arena[alt2_children[1].to_index()], Expr::term("never"));
        assert_eq!(g.arena[alt2_children[2].to_index()], Expr::term("auto"));
    }

    #[test]
    fn parens_are_enough_to_parse_as_subword() {
        const INPUT: &str = r#"
grep --color=(always | never | auto);
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let subword_phase = match &g.arena[expr_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };

        let seq_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);

        assert_eq!(g.arena[seq_children[0].to_index()], Expr::term("--color="));

        let alt_children = match &g.arena[seq_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt_children.len(), 3);
        assert_eq!(g.arena[alt_children[0].to_index()], Expr::term("always"));
        assert_eq!(g.arena[alt_children[1].to_index()], Expr::term("never"));
        assert_eq!(g.arena[alt_children[2].to_index()], Expr::term("auto"));
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

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("strace"));
        let seq_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);
        assert_eq!(g.arena[seq_children[0].to_index()], Expr::term("-e"));
        assert_eq!(
            g.arena[seq_children[1].to_index()],
            Expr::nontermref("EXPR")
        );

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
        let subword_phase = match &g.arena[expr_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq2_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 4);

        let optional1_child_id = match &g.arena[seq2_children[0].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        let seq3_children = match &g.arena[optional1_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq3_children.len(), 2);
        assert_eq!(
            g.arena[seq3_children[0].to_index()],
            Expr::nontermref("qualifier")
        );
        assert_eq!(g.arena[seq3_children[1].to_index()], Expr::term("="));

        let optional2_child_id = match &g.arena[seq2_children[1].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(g.arena[optional2_child_id.to_index()], Expr::term("!"));

        assert_eq!(
            g.arena[seq2_children[2].to_index()],
            Expr::nontermref("value")
        );

        let many1_child_id = match &g.arena[seq2_children[3].to_index()] {
            Expr::Many1(child) => *child,
            _ => panic!("Expected Many1"),
        };
        let optional3_child_id = match &g.arena[many1_child_id.to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        let seq4_children = match &g.arena[optional3_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq4_children.len(), 2);
        assert_eq!(g.arena[seq4_children[0].to_index()], Expr::term(","));
        assert_eq!(
            g.arena[seq4_children[1].to_index()],
            Expr::nontermref("value")
        );

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
        let alt_children = match &g.arena[expr_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt_children.len(), 4);
        assert_eq!(g.arena[alt_children[0].to_index()], Expr::term("trace"));
        assert_eq!(g.arena[alt_children[1].to_index()], Expr::term("read"));
        assert_eq!(g.arena[alt_children[2].to_index()], Expr::term("write"));
        assert_eq!(g.arena[alt_children[3].to_index()], Expr::term("fault"));

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
        let alt_children = match &g.arena[expr_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt_children.len(), 3);
        assert_eq!(g.arena[alt_children[0].to_index()], Expr::term("%file"));
        assert_eq!(g.arena[alt_children[1].to_index()], Expr::term("file"));
        assert_eq!(g.arena[alt_children[2].to_index()], Expr::term("all"));
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

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("lsof"));
        let subword_phase = match &g.arena[expr_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 5);
        assert_eq!(g.arena[seq_children[0].to_index()], Expr::term("-s"));
        assert_eq!(
            g.arena[seq_children[1].to_index()],
            Expr::nontermref("PROTOCOL")
        );
        assert_eq!(g.arena[seq_children[2].to_index()], Expr::term(":"));
        assert_eq!(
            g.arena[seq_children[3].to_index()],
            Expr::nontermref("STATE-SPEC")
        );
        let many1_child_id = match &g.arena[seq_children[4].to_index()] {
            Expr::Many1(child) => *child,
            _ => panic!("Expected Many1"),
        };
        let optional_child_id = match &g.arena[many1_child_id.to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        let seq2_children = match &g.arena[optional_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(g.arena[seq2_children[0].to_index()], Expr::term(","));
        assert_eq!(
            g.arena[seq2_children[1].to_index()],
            Expr::nontermref("STATE-SPEC")
        );

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
        let alt_children = match &g.arena[expr_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt_children.len(), 2);
        assert_eq!(g.arena[alt_children[0].to_index()], Expr::term("TCP"));
        assert_eq!(g.arena[alt_children[1].to_index()], Expr::term("UDP"));

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
        let subword_phase = match &g.arena[expr_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq3_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq3_children.len(), 2);
        let optional2_child_id = match &g.arena[seq3_children[0].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(g.arena[optional2_child_id.to_index()], Expr::term("^"));
        assert_eq!(
            g.arena[seq3_children[1].to_index()],
            Expr::nontermref("STATE")
        );

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
        let alt2_children = match &g.arena[expr_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt2_children.len(), 2);
        assert_eq!(g.arena[alt2_children[0].to_index()], Expr::term("LISTEN"));
        assert_eq!(g.arena[alt2_children[1].to_index()], Expr::term("CLOSED"));
    }

    #[test]
    fn parses_shell_command_nonterminal_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> ::= {{{ rustup toolchain list | cut -d' ' -f1 }}};
"#;
        let g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, u("cargo"));
        let seq1_children = match &g.arena[expr_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 3);

        let optional1_child_id = match &g.arena[seq1_children[0].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        let subword_phase = match &g.arena[optional1_child_id.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq2_children = match &g.arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(g.arena[seq2_children[0].to_index()], Expr::term("+"));
        assert_eq!(
            g.arena[seq2_children[1].to_index()],
            Expr::nontermref("toolchain")
        );

        let optional2_child_id = match &g.arena[seq1_children[1].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional2_child_id.to_index()],
            Expr::nontermref("OPTIONS")
        );

        let optional3_child_id = match &g.arena[seq1_children[2].to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(
            g.arena[optional3_child_id.to_index()],
            Expr::nontermref("COMMAND")
        );

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            Statement::NonterminalDefinition {
                symbol,
                shell,
                expr,
            } => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, u("toolchain"));
        assert!(shell.is_none());
        assert_eq!(
            g.arena[expr_id.to_index()],
            Command(
                u("rustup toolchain list | cut -d' ' -f1"),
                None,
                0,
                ChicSpan::dummy()
            )
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
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        assert!(expr_extensionally_eq(actual, expected, &arena));
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
        assert_eq!(*head, u("grep"));
        let expected = alloc(
            &mut g.arena,
            Expr::term_descr(
                "--extended-regexp",
                "PATTERNS are extended regular expressions",
            ),
        );
        assert!(expr_extensionally_eq(actual, expected, &g.arena));
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
        assert_eq!(g.statements.len(), 1);
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, u("foo.sh"));
        let optional_child_id = match &g.arena[expr_id.to_index()] {
            Expr::Optional(child) => *child,
            _ => panic!("Expected Optional"),
        };
        assert_eq!(g.arena[optional_child_id.to_index()], Expr::term("-h"));
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
        assert_eq!(g.statements.len(), 3);

        let (head, expr_id) = match &g.statements[0] {
            CallVariant { head, expr } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, u("ls"));
        assert_eq!(g.arena[expr_id.to_index()], Expr::nontermref("FILE"));

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
        assert_eq!(
            g.arena[expr_id.to_index()],
            Command(ustr(r#"compgen -A file "$1""#), None, 0, ChicSpan::dummy())
        );

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
        assert_eq!(
            g.arena[expr_id.to_index()],
            Command(
                ustr(r#"__fish_complete_path "$1""#),
                None,
                0,
                ChicSpan::dummy()
            )
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
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let seq_children = match &arena[distributed_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);
        assert_eq!(arena[seq_children[0].to_index()], Expr::term("mygrep"));

        let alt_children = match &arena[seq_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt_children.len(), 2);

        // First alternative
        let subword_phase = match &arena[alt_children[0].to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let subword_seq_children = match &arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(subword_seq_children.len(), 2);
        assert_eq!(
            arena[subword_seq_children[0].to_index()],
            Expr::term_descr("--color=", "use markers to highlight the matching strings")
        );
        assert_eq!(
            arena[subword_seq_children[1].to_index()],
            Expr::nontermref("WHEN")
        );

        // Second alternative
        let seq2_children = match &arena[alt_children[1].to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(
            arena[seq2_children[0].to_index()],
            Expr::term_descr("--color", "use markers to highlight the matching strings")
        );
        assert_eq!(arena[seq2_children[1].to_index()], Expr::nontermref("WHEN"));
    }

    #[test]
    fn spends_distributed_description() {
        const INPUT: &str = r#"mygrep --help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let alt1_children = match &arena[distributed_id.to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt1_children.len(), 2);

        let seq1_children = match &arena[alt1_children[0].to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 2);
        assert_eq!(arena[seq1_children[0].to_index()], Expr::term("mygrep"));
        assert_eq!(arena[seq1_children[1].to_index()], Expr::term("--help"));

        let alt2_children = match &arena[alt1_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt2_children.len(), 2);

        // First alternative of second alternative
        let subword_phase = match &arena[alt2_children[0].to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let subword_seq_children = match &arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(subword_seq_children.len(), 2);
        assert_eq!(
            arena[subword_seq_children[0].to_index()],
            Expr::term_descr("--color=", "use markers to highlight the matching strings")
        );
        assert_eq!(
            arena[subword_seq_children[1].to_index()],
            Expr::nontermref("WHEN")
        );

        // Second alternative of second alternative
        let seq2_children = match &arena[alt2_children[1].to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(
            arena[seq2_children[0].to_index()],
            Expr::term_descr("--color", "use markers to highlight the matching strings")
        );
        assert_eq!(arena[seq2_children[1].to_index()], Expr::nontermref("WHEN"));
    }

    #[test]
    fn spends_distributed_description2() {
        const INPUT: &str = r#"mygrep (--help | (--color=<WHEN> | --color <WHEN>) "use markers to highlight the matching strings")"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let distributed_id = distribute_descriptions(&mut arena, e);

        let seq1_children = match &arena[distributed_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq1_children.len(), 2);
        assert_eq!(arena[seq1_children[0].to_index()], Expr::term("mygrep"));

        let alt1_children = match &arena[seq1_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt1_children.len(), 2);
        assert_eq!(arena[alt1_children[0].to_index()], Expr::term("--help"));

        let alt2_children = match &arena[alt1_children[1].to_index()] {
            Expr::Alternative(children) => children,
            _ => panic!("Expected Alternative"),
        };
        assert_eq!(alt2_children.len(), 2);

        // First alternative of second alternative
        let subword_phase = match &arena[alt2_children[0].to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let subword_seq_children = match &arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(subword_seq_children.len(), 2);
        assert_eq!(
            arena[subword_seq_children[0].to_index()],
            Expr::term_descr("--color=", "use markers to highlight the matching strings")
        );
        assert_eq!(
            arena[subword_seq_children[1].to_index()],
            Expr::nontermref("WHEN")
        );

        // Second alternative of second alternative
        let seq2_children = match &arena[alt2_children[1].to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq2_children.len(), 2);
        assert_eq!(
            arena[seq2_children[0].to_index()],
            Expr::term_descr("--color", "use markers to highlight the matching strings")
        );
        assert_eq!(arena[seq2_children[1].to_index()], Expr::nontermref("WHEN"));
    }

    #[test]
    fn parses_fallback_expr() {
        const INPUT: &str = r#"cmd (foo || bar)"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let seq_children = match &arena[e.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);
        assert_eq!(arena[seq_children[0].to_index()], Expr::term("cmd"));
        let fallback_children = match &arena[seq_children[1].to_index()] {
            Expr::Fallback(children) => children,
            _ => panic!("Expected Fallback"),
        };
        assert_eq!(fallback_children.len(), 2);
        assert_eq!(arena[fallback_children[0].to_index()], Expr::term("foo"));
        assert_eq!(arena[fallback_children[1].to_index()], Expr::term("bar"));
    }

    #[test]
    fn parses_git_commit_range_subword() {
        const INPUT: &str = r#"{{{ git tag }}}..{{{ git tag }}}"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let subword_phase = match &arena[e.to_index()] {
            Expr::Subword(phase, 0, _) => phase,
            _ => panic!("Expected Subword"),
        };
        let subword_child_id = match subword_phase {
            SubwordCompilationPhase::Expr(expr_id) => *expr_id,
            _ => panic!("Expected SubwordCompilationPhase::Expr"),
        };
        let seq_children = match &arena[subword_child_id.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 3);
        assert_eq!(
            arena[seq_children[0].to_index()],
            Command(ustr("git tag"), None, 0, ChicSpan::dummy())
        );
        assert_eq!(arena[seq_children[1].to_index()], Expr::term(".."));
        assert_eq!(
            arena[seq_children[2].to_index()],
            Command(ustr("git tag"), None, 0, ChicSpan::dummy())
        );
    }

    #[test]
    fn ignores_form_feed() {
        const INPUT: &str = "cmd \u{000C} \u{000C} \u{000C}\u{000C} foo";
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let seq_children = match &arena[e.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);
        assert_eq!(arena[seq_children[0].to_index()], Expr::term("cmd"));
        assert_eq!(arena[seq_children[1].to_index()], Expr::term("foo"));
    }

    #[test]
    fn parses_nontail_command_script() {
        const INPUT: &str = r#"cmd {{{ echo foo }}}@bash"bar"@fish"baz"@zsh"quux""#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty(), "{:?}", s.fragment());
        let seq_children = match &arena[e.to_index()] {
            Expr::Sequence(children) => children,
            _ => panic!("Expected Sequence"),
        };
        assert_eq!(seq_children.len(), 2);
        assert_eq!(arena[seq_children[0].to_index()], Expr::term("cmd"));
        assert_eq!(
            arena[seq_children[1].to_index()],
            Command(
                ustr("echo foo"),
                Some(CmdRegexDecl {
                    bash: Some(ustr("bar")),
                    fish: Some(ustr("baz")),
                    zsh: Some(ustr("quux"))
                }),
                0,
                ChicSpan::dummy(),
            )
        );
    }
}
