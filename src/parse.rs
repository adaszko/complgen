use std::io::Write;

use hashbrown::HashSet;
use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_until, take_while},
    character::complete::{char, multispace1},
    combinator::{eof, fail, map, opt, value, verify},
    error::context,
    multi::{fold_many0, many0},
    sequence::preceded,
};
use nom_locate::LocatedSpan;

use crate::{Error, Result};
use ustr::{Ustr, UstrMap, UstrSet, ustr};

#[derive(Debug, Clone)]
pub struct Grammar {
    pub arena: Vec<Expr>,
    pub statements: Vec<Statement>,
}

impl Grammar {
    pub fn parse(input_before: &str) -> Result<Self> {
        let (input_after, (arena, statements)) = match grammar(Span::new(input_before)).finish() {
            Ok((input, statements)) => (input, statements),
            Err(e) => {
                let span = HumanSpan::from_machine(e.input);
                return Err(Error::ParseError(span));
            }
        };

        if !input_after.is_empty() {
            let span = HumanSpan::from_machine(input_after);
            return Err(Error::ParseError(span));
        }

        let g = Grammar { arena, statements };

        Ok(g)
    }

    pub(crate) fn iter_call_variants(&self) -> impl Iterator<Item = (Ustr, HumanSpan, ExprId)> {
        self.statements.iter().filter_map(|v| match v {
            Statement::CallVariant {
                name,
                name_span,
                expr,
            } => Some((*name, *name_span, *expr)),
            Statement::NonterminalDefinition { .. } => None,
        })
    }

    pub(crate) fn iter_nonterm_defns(&self) -> impl Iterator<Item = &NontermDefn> {
        self.statements.iter().filter_map(|v| match v {
            Statement::NonterminalDefinition(defn) => Some(defn),
            Statement::CallVariant { .. } => None,
        })
    }

    pub(crate) fn get_specializations(
        &self,
        target_shell: Shell,
    ) -> Result<(UstrMap<UserSpec>, UstrMap<(Ustr, HumanSpan)>)> {
        let mut specializations: UstrMap<UserSpec> = Default::default();
        let mut fallbacks: UstrMap<(Ustr, HumanSpan)> = Default::default();
        let mut specialized_nonterminals: UstrSet = Default::default();
        for defn in self.iter_nonterm_defns() {
            let NontermDefn {
                shell: Some((shell_name, shell_span)),
                ..
            } = defn
            else {
                continue;
            };
            let rhs = &self.arena[defn.rhs_expr_id];
            let command = match rhs {
                Expr::Command { cmd, .. } => cmd,
                _ => {
                    return Err(Error::NonCommandSpecialization(*rhs.get_span()));
                }
            };
            if Shell::from_str(shell_name, *shell_span)? != target_shell {
                continue;
            }
            if let Some(UserSpec { span, .. }) = specializations.get(&defn.lhs_name) {
                return Err(Error::DuplicateNonterminalDefinition(*span, defn.lhs_span));
            }
            match target_shell {
                Shell::Bash => {
                    specializations.entry(defn.lhs_name).insert_entry(UserSpec {
                        cmd: *command,
                        span: defn.lhs_span,
                        used: false,
                    });
                }
                Shell::Fish => {
                    specializations.entry(defn.lhs_name).insert_entry(UserSpec {
                        cmd: *command,
                        span: defn.lhs_span,
                        used: false,
                    });
                }
                Shell::Zsh => {
                    specializations.entry(defn.lhs_name).insert_entry(UserSpec {
                        cmd: *command,
                        span: defn.lhs_span,
                        used: false,
                    });
                }
                Shell::Pwsh => {
                    specializations.entry(defn.lhs_name).insert_entry(UserSpec {
                        cmd: *command,
                        span: defn.lhs_span,
                        used: false,
                    });
                }
            }
            specialized_nonterminals.insert(defn.lhs_name);
        }

        for defn in self.iter_nonterm_defns() {
            let NontermDefn { shell: None, .. } = defn else {
                continue;
            };
            if !specialized_nonterminals.contains(&defn.lhs_name) {
                continue;
            }
            let rhs = &self.arena[defn.rhs_expr_id];
            let Expr::Command { cmd, .. } = rhs else {
                return Err(Error::NonCommandSpecialization(*rhs.get_span()));
            };
            if let Some((_, span)) = fallbacks.get(&defn.lhs_name) {
                return Err(Error::DuplicateNonterminalDefinition(*span, defn.lhs_span));
            }
            fallbacks.insert(defn.lhs_name, (*cmd, defn.lhs_span));
        }

        Ok((specializations, fallbacks))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExprId(pub usize);

impl std::fmt::Display for ExprId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Index<ExprId> for Vec<Expr> {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self[index.0]
    }
}

impl std::ops::Index<ExprId> for [Expr] {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self[index.0]
    }
}

pub(crate) fn alloc(arena: &mut Vec<Expr>, elem: Expr) -> ExprId {
    let id = arena.len();
    arena.push(elem);
    ExprId(id)
}

#[derive(Clone)]
pub enum Expr {
    // `--help`
    Terminal {
        term: Ustr,
        descr: Option<Ustr>,
        fallback: usize,
        span: HumanSpan,
    },

    // `<PATH>`, `<DIRECTORY>`, etc.
    NontermRef {
        nonterm: Ustr,
        fallback: usize,
        span: HumanSpan,
    },

    // `{{{ ls }}}`
    Command {
        cmd: Ustr,
        zsh_compadd: bool,
        fallback: usize,
        span: HumanSpan,
    },

    // `foo bar`
    Sequence {
        children: Vec<ExprId>,
        span: HumanSpan,
    },

    // `foo | bar`
    Alternative {
        children: Vec<ExprId>,
        span: HumanSpan,
    },

    // `[EXPR]`
    Optional {
        child: ExprId,
        span: HumanSpan,
    },

    // `EXPR...`
    Many1 {
        child: ExprId,
        span: HumanSpan,
    },

    // `(b | build) "Compile the current package"` means the description applies to both `b` and
    // `build`. `(b build) "Compile the current package"` means means the description applies just
    // to `b` (i.e. the first literal)
    DistributiveDescription {
        child: ExprId,
        descr: Ustr,
        span: HumanSpan,
    },

    // `foo || bar`
    Fallback {
        children: Vec<ExprId>,
        span: HumanSpan,
    },

    // `--option=argument`
    Subword {
        root_id: ExprId,
        fallback: usize,
        span: HumanSpan,
    },
}

impl Expr {
    pub(crate) fn get_span(&self) -> &HumanSpan {
        match self {
            Expr::Terminal { span, .. } => span,
            Expr::NontermRef { span, .. } => span,
            Expr::Command { span, .. } => span,
            Expr::Sequence { span, .. } => span,
            Expr::Alternative { span, .. } => span,
            Expr::Optional { span, .. } => span,
            Expr::Many1 { span, .. } => span,
            Expr::DistributiveDescription { span, .. } => span,
            Expr::Fallback { span, .. } => span,
            Expr::Subword { span, .. } => span,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Shell {
    Bash,
    Fish,
    Zsh,
    Pwsh,
}

impl Shell {
    fn from_str(shell: &str, span: HumanSpan) -> Result<Self> {
        match shell {
            "bash" => Ok(Self::Bash),
            "fish" => Ok(Self::Fish),
            "zsh" => Ok(Self::Zsh),
            "pwsh" => Ok(Self::Pwsh),
            _ => Err(Error::UnknownShell(span)),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UserSpec {
    pub cmd: Ustr,
    pub span: HumanSpan,
    pub used: bool,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BuiltinSpec {
    pub cmd: Ustr,
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Subword {
                root_id, fallback, ..
            } => f.write_fmt(format_args!(r#"Subword {{ root_id: {root_id}, fallback: {fallback} }}"#)),
            Expr::Terminal {
                term,
                descr: Some(descr),
                fallback,
                ..
            } => f.write_fmt(format_args!(
                r#"Terminal {{ term: ustr("{term}"), descr: Some(ustr("{}"))), fallback: {fallback}, span: None }}"#,
                descr
            )),
            Expr::Terminal {
                term,
                descr: None,
                fallback,
                ..
            } => f.write_fmt(format_args!(
                r#"Terminal {{ term: ustr("{term}"), descr: None, fallback: {fallback}, span: None }}"#
            )),
            Expr::NontermRef {
                nonterm,
                fallback,
                ..
            } => f.write_fmt(format_args!(
                r#"NontermRef {{ nonterm: ustr("{nonterm}"), fallback: {fallback}, span: None }}"#
            )),
            Self::Command {
                cmd,
                zsh_compadd,
                fallback,
                span,
            } => f.write_fmt(format_args!(
                r#"Command(ustr({cmd:?}), {zsh_compadd}, {fallback}, {span:?})"#
            )),
            Self::Sequence { children, span } => {
                f.write_fmt(format_args!(r#"Sequence(vec!{:?}, {span:?})"#, children))
            }
            Self::Alternative { children, span } => {
                f.write_fmt(format_args!(r#"Alternative(vec!{:?}, {span:?})"#, children))
            }
            Self::Optional { child, span } => f.write_fmt(format_args!(r#"Optional({:?}, {span:?})"#, child)),
            Self::Many1 { child, span } => f.write_fmt(format_args!(r#"Many1({:?}, {span:?})"#, child)),
            Self::DistributiveDescription { child, descr, span } => f.write_fmt(format_args!(
                r#"DistributiveDescription({child:?}, {descr:?}, {span:?})"#
            )),
            Self::Fallback { children, span } => {
                f.write_fmt(format_args!(r#"Fallback(vec!{:?}, {span:?})"#, children))
            }
        }
    }
}

fn dot_escape(s: &str) -> String {
    if s.is_empty() {
        return s.to_string();
    }
    s.replace('\"', "\\\"")
        .replace('`', "\\`")
        .replace('$', "\\$")
}

fn do_expr_to_dot<W: Write>(
    output: &mut W,
    expr_id: ExprId,
    visited: &mut HashSet<ExprId>,
    arena: &[Expr],
    identifier_prefix: &str,
    recursion_level: usize,
) -> std::result::Result<(), std::io::Error> {
    if visited.contains(&expr_id) {
        // One nonterminal may be referenced multiple times resulting in traversing one expr_id
        // many times.  Let's detect that to avoid producing redudant .dot graph edges.
        return Ok(());
    }
    visited.insert(expr_id);
    let indent = format!("\t{}", str::repeat("\t", recursion_level));
    match arena[expr_id].clone() {
        Expr::Terminal { term, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="\"{term}\""];"#
            )?;
        }
        Expr::NontermRef { nonterm, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="<{nonterm}>"];"#
            )?;
        }
        Expr::Command { cmd, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="{}"];"#,
                dot_escape(&cmd)
            )?;
        }
        Expr::Sequence { children, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="Sequence"];"#
            )?;
            for child_id in &children {
                match arena[*child_id] {
                    Expr::Terminal { .. }
                    | Expr::NontermRef { .. }
                    | Expr::Command { .. }
                    | Expr::Sequence { .. }
                    | Expr::Alternative { .. }
                    | Expr::Optional { .. }
                    | Expr::Many1 { .. }
                    | Expr::DistributiveDescription { .. }
                    | Expr::Fallback { .. } => {
                        writeln!(
                            output,
                            r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child_id};"#
                        )?;
                    }
                    Expr::Subword { root_id, .. } => {
                        writeln!(
                            output,
                            r#"{indent}_{identifier_prefix}{expr_id} -> _{child_id}_{root_id};"#
                        )?;
                    }
                }
            }
            for child in children {
                do_expr_to_dot(
                    output,
                    child,
                    visited,
                    arena,
                    identifier_prefix,
                    recursion_level,
                )?;
            }
        }
        Expr::Alternative { children, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="Alternative"];"#
            )?;
            for child in &children {
                writeln!(
                    output,
                    r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child};"#
                )?;
            }
            for child in children {
                do_expr_to_dot(
                    output,
                    child,
                    visited,
                    arena,
                    identifier_prefix,
                    recursion_level,
                )?;
            }
        }
        Expr::Fallback { children, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="Fallback"];"#
            )?;
            for child in &children {
                writeln!(
                    output,
                    r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child};"#
                )?;
            }
            for child in children {
                do_expr_to_dot(
                    output,
                    child,
                    visited,
                    arena,
                    identifier_prefix,
                    recursion_level,
                )?;
            }
        }
        Expr::Optional { child, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="Optional"];"#
            )?;
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child};"#
            )?;
            do_expr_to_dot(
                output,
                child,
                visited,
                arena,
                identifier_prefix,
                recursion_level,
            )?;
        }
        Expr::Many1 { child, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="Many1"];"#
            )?;
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child};"#
            )?;
            do_expr_to_dot(
                output,
                child,
                visited,
                arena,
                identifier_prefix,
                recursion_level,
            )?;
        }
        Expr::DistributiveDescription { child, .. } => {
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id}[label="DistributiveDescription"];"#
            )?;
            writeln!(
                output,
                r#"{indent}_{identifier_prefix}{expr_id} -> _{identifier_prefix}{child};"#
            )?;
        }
        Expr::Subword { root_id, .. } => {
            writeln!(output, "{indent}subgraph cluster_{expr_id} {{")?;
            let subword_identifier_prefix = &format!("{expr_id}_");
            do_expr_to_dot(
                output,
                root_id,
                visited,
                arena,
                subword_identifier_prefix,
                recursion_level + 1,
            )?;
            writeln!(output, "{indent}}}")?;
        }
    }
    Ok(())
}

fn expr_to_dot<W: Write>(
    output: &mut W,
    root_id: ExprId,
    arena: &[Expr],
) -> std::result::Result<(), std::io::Error> {
    writeln!(output, "digraph rx {{")?;
    let mut visited: HashSet<ExprId> = Default::default();
    do_expr_to_dot(output, root_id, &mut visited, arena, "", 0)?;
    writeln!(output, "}}")?;
    Ok(())
}

#[allow(dead_code)]
pub fn expr_to_dot_file<P: AsRef<std::path::Path>>(
    path: P,
    root_id: ExprId,
    arena: &[Expr],
) -> std::result::Result<(), std::io::Error> {
    let mut file = std::fs::File::create(path)?;
    expr_to_dot(&mut file, root_id, arena)?;
    Ok(())
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct HumanSpan {
    pub line: usize,
    pub column_start: usize,
    pub column_end: usize,
}

impl HumanSpan {
    fn from_range(before: Span, after: Span) -> Self {
        Self {
            line: before.location_line() as usize,
            column_start: before.get_column(),
            column_end: after.get_column(),
        }
    }

    fn from_machine(span: Span) -> Self {
        Self {
            line: span.location_line() as usize,
            column_start: span.get_column(),
            column_end: span.get_column() + 1,
        }
    }

    pub fn line_machine(&self) -> usize {
        self.line - 1
    }

    pub fn column_start_machine(&self) -> usize {
        self.column_start - 1
    }

    pub fn column_end_machine(&self) -> usize {
        self.column_end - 1
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
    let (input, _) = alt((multispace1, comment, form_feed)).parse(input)?;
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

// A sequence of alphanumeric/punctuation characters that ends at first reserved character
// (i.e. operators like ()[]{}<>|;").
// There's special case for '.' since it's the only reserved character that is repeated (i.e. in
// "...") in order to gain special meaning of denoting "one or more".
fn terminal(mut input: Span) -> IResult<Span, String> {
    fn is_regular_terminal_char(c: char) -> bool {
        c.is_ascii_alphanumeric()
            || matches!(
                c,
                '!' | '#'
                    | '$'
                    | '%'
                    | '&'
                    | '\''
                    | '*'
                    | '+'
                    | ','
                    | '-'
                    | '/'
                    | ':'
                    | '='
                    | '?'
                    | '@'
                    | '^'
                    | '_'
                    | '`'
                    | '~',
            )
    }

    let mut term = String::new();
    loop {
        let mut consumed = 0;

        // an optional sequence of regular characters
        let (after, part) = take_while(is_regular_terminal_char)(input)?;
        term.push_str(&part);
        consumed += part.len();
        input = after;

        // an optional sequence of escaped characters
        while let Some(after) = input.strip_prefix('\\') {
            input = after.into();
            if input.starts_with(&[
                '(', ')', '[', ']', '<', '>', '|', ';', '"', '{', '}', '\\', '.',
            ]) {
                let mut chars = input.chars();
                term.push(chars.next().unwrap());
                consumed += 1;
                input = chars.as_str().into();
            } else {
                // escaped non-special character
                return fail().parse(input);
            }
        }

        // an optional sequence of fewer than 3 '.'
        if input.starts_with("...") {
            break;
        }
        let (after, part) = take_while(|c| c == '.')(input)?;
        term.push_str(&part);
        consumed += part.len();
        input = after;

        if consumed == 0 {
            break;
        }
    }

    if term.is_empty() {
        return fail().parse(input);
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

// https://github.com/rust-bakery/nom/blob/51c3c4e44fa78a8a09b413419372b97b2cc2a787/examples/string.rs
fn description_inner(input: Span) -> IResult<Span, String> {
    let (input, inner) = fold_many0(parse_fragment, String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(&String::from_utf8_lossy(s.as_bytes())),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        Span::new(&string).to_string()
    })
    .parse(input)?;
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
    let (after, descr) = opt(preceded(multiblanks0, description)).parse(after)?;
    let expr = Expr::Terminal {
        term: ustr(&term),
        descr: descr.map(|span| ustr(&span)),
        fallback: 0,
        span: HumanSpan::from_range(input, after),
    };
    let id = alloc(arena, expr);
    Ok((after, id))
}

fn nonterm(input: Span) -> IResult<Span, (Ustr, HumanSpan)> {
    let before_nonterm = input;
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">")(input)?;
    let (input, _) = char('>')(input)?;
    let nonterm_span = HumanSpan::from_range(before_nonterm, input);
    Ok((input, (ustr(name.into_fragment()), nonterm_span)))
}

fn nonterm_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, (nonterm, _)) = context("nonterminal", nonterm).parse(input)?;
    let diagnostic_span = HumanSpan::from_range(input, after);
    let e = Expr::NontermRef {
        nonterm,
        fallback: 0,
        span: diagnostic_span,
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
    let command_span = HumanSpan::from_range(input, after);
    let e = Expr::Command {
        cmd: ustr(cmd.into_fragment()),
        zsh_compadd: false,
        fallback: 0,
        span: command_span,
    };
    let id = alloc(arena, e);
    Ok((after, id))
}

fn optional_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (after, _) = char('[')(input)?;
    let (after, _) = multiblanks0(after)?;
    let (after, expr) = expr(arena, after)?;
    let (after, _) = multiblanks0(after)?;
    let (after, _) = char(']')(after)?;
    let id = alloc(
        arena,
        Expr::Optional {
            child: expr,
            span: HumanSpan::from_range(input, after),
        },
    );
    Ok((after, id))
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
    let (after, e) = 'alt: {
        if let Ok((input, e)) = nonterm_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = optional_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = parenthesized_expr(arena, input) {
            break 'alt (input, e);
        }

        if let Ok((input, e)) = command_expr(arena, input) {
            break 'alt (input, e);
        }

        terminal_opt_description_expr(arena, input)?
    };

    if let Ok((after, ())) = many1_tag(after) {
        let e = Expr::Many1 {
            child: e,
            span: HumanSpan::from_range(input, after),
        };
        let id = alloc(arena, e);
        return Ok((after, id));
    }

    Ok((after, e))
}

// Used in subword mode, when we know there won't be any sub-DFAs needed, just one big one.
// Substitutes Expr::Subword with Expr to make AST simpler to process.
pub(crate) fn flatten_expr(arena: &mut Vec<Expr>, expr_id: ExprId) -> ExprId {
    match arena[expr_id].clone() {
        Expr::Terminal { .. } | Expr::NontermRef { .. } | Expr::Command { .. } => expr_id,
        Expr::Subword { root_id: child, .. } => flatten_expr(arena, child),
        Expr::Sequence { children, span } => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Sequence {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Alternative { children, span } => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Alternative {
                        children: new_children,
                        span,
                    },
                )
            }
        }
        Expr::Optional { child, span } => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Optional {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::Many1 { child, span } => {
            let new_child = flatten_expr(arena, child);
            if child == new_child {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Many1 {
                        child: new_child,
                        span,
                    },
                )
            }
        }
        Expr::DistributiveDescription {
            child,
            descr: description,
            span,
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
                        span,
                    },
                )
            }
        }
        Expr::Fallback { children, span } => {
            let new_children: Vec<ExprId> =
                children.iter().map(|e| flatten_expr(arena, *e)).collect();
            if children == new_children {
                expr_id
            } else {
                alloc(
                    arena,
                    Expr::Fallback {
                        children: new_children,
                        span,
                    },
                )
            }
        }
    }
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
        let span = HumanSpan::from_range(input, after);
        let e = Expr::Sequence {
            children: flattened_factors,
            span,
        };
        let root_id = alloc(arena, e);
        let subword_expr = Expr::Subword {
            root_id,
            fallback: 0,
            span,
        };
        alloc(arena, subword_expr)
    };
    Ok((after, result))
}

fn subword_sequence_expr_opt_description<'s>(
    arena: &mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, ExprId> {
    let (after, expr_id) = subword_sequence_expr(arena, input)?;
    let (after, description) = opt(preceded(multiblanks0, description)).parse(after)?;
    let result = match description {
        Some(descr) => {
            let e = Expr::DistributiveDescription {
                child: expr_id,
                descr: ustr(&descr),
                span: HumanSpan::from_range(input, after),
            };
            alloc(arena, e)
        }
        None => expr_id,
    };
    Ok((after, result))
}

fn sequence_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut after, left) = subword_sequence_expr_opt_description(arena, input)?;
    let mut factors: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = preceded(multiblanks1, |i| {
        subword_sequence_expr_opt_description(arena, i)
    })
    .parse(after)
    {
        factors.push(right);
        after = rest;
    }
    let result = if factors.len() == 1 {
        factors.drain(..).next().unwrap()
    } else {
        alloc(
            arena,
            Expr::Sequence {
                children: factors,
                span: HumanSpan::from_range(input, after),
            },
        )
    };
    Ok((after, result))
}

fn do_alternative_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = char('|')(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = sequence_expr(arena, input)?;
    Ok((input, right))
}

fn alternative_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut after, left) = sequence_expr(arena, input)?;
    let mut elems: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = do_alternative_expr(arena, after) {
        elems.push(right);
        after = rest;
    }
    let result = if elems.len() == 1 {
        elems.drain(..).next().unwrap()
    } else {
        alloc(
            arena,
            Expr::Alternative {
                children: elems,
                span: HumanSpan::from_range(input, after),
            },
        )
    };
    Ok((after, result))
}

fn do_fallback_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (input, _) = multiblanks0(input)?;
    let (input, _) = tag("||")(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, right) = alternative_expr(arena, input)?;
    Ok((input, right))
}

fn fallback_expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    let (mut after, left) = alternative_expr(arena, input)?;
    let mut fallbacks: Vec<ExprId> = vec![left];
    while let Ok((rest, right)) = do_fallback_expr(arena, after) {
        fallbacks.push(right);
        after = rest;
    }
    let result = if fallbacks.len() == 1 {
        fallbacks.drain(..).next().unwrap()
    } else {
        alloc(
            arena,
            Expr::Fallback {
                children: fallbacks,
                span: HumanSpan::from_range(input, after),
            },
        )
    };
    Ok((after, result))
}

pub(crate) fn expr<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, ExprId> {
    fallback_expr(arena, input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct NontermDefn {
    pub(crate) lhs_name: Ustr,
    pub(crate) lhs_span: HumanSpan,
    pub(crate) shell: Option<(Ustr, HumanSpan)>,
    pub(crate) rhs_expr_id: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    CallVariant {
        name: Ustr,
        name_span: HumanSpan,
        expr: ExprId,
    },
    NonterminalDefinition(NontermDefn),
}

fn end_of_statement<'s>(input: Span<'s>) -> IResult<Span<'s>, ()> {
    alt((map(char(';'), |_| ()), map(eof, |_| ()))).parse(input)
}

fn call_variant<'s>(arena: &mut Vec<Expr>, input: Span<'s>) -> IResult<Span<'s>, Statement> {
    let (after, name) = terminal(input)?;
    let name_span = HumanSpan::from_range(input, after);
    let (after, _) = multiblanks1(after)?;
    let (after, expr) = expr(arena, after)?;
    let (after, _) = multiblanks0(after)?;
    let (after, _) = end_of_statement(after)?;

    let production = Statement::CallVariant {
        name: ustr(&name),
        name_span,
        expr,
    };

    Ok((after, production))
}

fn nonterm_specialization(input: Span) -> IResult<Span, (Ustr, HumanSpan, Ustr, HumanSpan)> {
    let before_name = input;
    let (input, _) = char('<')(input)?;
    let (input, name) = is_not(">@")(input)?;
    let (input, _) = char('@')(input)?;
    let before_shell = input;
    let (input, shell) = is_not(">")(input)?;
    let shell_span = HumanSpan::from_range(before_shell, input);
    let (input, _) = char('>')(input)?;
    let nonterm_span = HumanSpan::from_range(before_name, input);
    Ok((
        input,
        (
            ustr(name.into_fragment()),
            nonterm_span,
            ustr(shell.into_fragment()),
            shell_span,
        ),
    ))
}

fn nonterm_def(input: Span) -> IResult<Span, (Ustr, HumanSpan, Option<(Ustr, HumanSpan)>)> {
    if let Ok((input, (name, nonterm_span, shell, shell_span))) = nonterm_specialization(input) {
        return Ok((input, (name, nonterm_span, Some((shell, shell_span)))));
    }
    if let Ok((input, (name, nonterm_span))) = nonterm(input) {
        return Ok((input, (name, nonterm_span, None)));
    }
    fail().parse(input)
}

fn nonterm_def_statement<'s>(
    arena: &mut Vec<Expr>,
    input: Span<'s>,
) -> IResult<Span<'s>, Statement> {
    let (input, (name, nonterm_span, shell)) = nonterm_def(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = alt((tag("::="), tag("="))).parse(input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, rhs_expr_id) = expr(arena, input)?;
    let (input, _) = multiblanks0(input)?;
    let (input, _) = end_of_statement(input)?;

    let stmt = Statement::NonterminalDefinition(NontermDefn {
        lhs_name: name,
        lhs_span: nonterm_span,
        shell,
        rhs_expr_id,
    });

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
    let (input, statements) = many0(|i| statement(&mut arena, i)).parse(input)?;
    let (input, _) = multiblanks0(input)?;
    Ok((input, (arena, statements)))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use Expr::*;

    impl Expr {
        pub(crate) fn term(s: &str) -> Self {
            Self::Terminal {
                term: ustr(s),
                descr: None,
                fallback: 0,
                span: HumanSpan::default(),
            }
        }

        pub(crate) fn term_descr(s: &str, d: &str) -> Self {
            Self::Terminal {
                term: ustr(s),
                descr: Some(ustr(d)),
                fallback: 0,
                span: HumanSpan::default(),
            }
        }

        pub(crate) fn nontermref(s: &str) -> Self {
            Self::NontermRef {
                nonterm: ustr(s),
                fallback: 0,
                span: HumanSpan::default(),
            }
        }

        pub(crate) fn subword(expr: ExprId) -> Self {
            Self::Subword {
                root_id: expr,
                fallback: 0,
                span: HumanSpan::default(),
            }
        }
    }

    // Extensional equality.  Ignores spans
    pub(crate) fn teq(left: ExprId, right: ExprId, arena: &[Expr]) -> bool {
        if left == right {
            return true;
        }

        match (&arena[left], &arena[right]) {
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
                    zsh_compadd: l_zsh_compadd,
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Command {
                    cmd: r,
                    zsh_compadd: r_zsh_compadd,
                    fallback: r_fallback,
                    span: _,
                },
            ) => l == r && l_zsh_compadd == r_zsh_compadd && l_fallback == r_fallback,
            (
                Expr::Sequence {
                    children: l,
                    span: _,
                },
                Expr::Sequence {
                    children: r,
                    span: _,
                },
            )
            | (
                Expr::Alternative {
                    children: l,
                    span: _,
                },
                Expr::Alternative {
                    children: r,
                    span: _,
                },
            )
            | (
                Expr::Fallback {
                    children: l,
                    span: _,
                },
                Expr::Fallback {
                    children: r,
                    span: _,
                },
            ) => {
                if l.len() != r.len() {
                    return false;
                }
                l.iter().zip(r.iter()).all(|(le, re)| teq(*le, *re, arena))
            }
            (Expr::Optional { child: l, span: _ }, Expr::Optional { child: r, span: _ }) => {
                teq(*l, *r, arena)
            }
            (Expr::Many1 { child: l, span: _ }, Expr::Many1 { child: r, span: _ }) => {
                teq(*l, *r, arena)
            }
            (
                Expr::DistributiveDescription {
                    child: l,
                    descr: l_descr,
                    span: _,
                },
                Expr::DistributiveDescription {
                    child: r,
                    descr: r_descr,
                    span: _,
                },
            ) => l_descr == r_descr && teq(*l, *r, arena),
            (
                Expr::Subword {
                    root_id: l,
                    fallback: l_fallback,
                    span: _,
                },
                Expr::Subword {
                    root_id: r,
                    fallback: r_fallback,
                    span: _,
                },
            ) => l_fallback == r_fallback && teq(*l, *r, arena),
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
        let e1 = alloc(
            &mut arena,
            Sequence {
                children: vec![e2, e3],
                span: HumanSpan::default(),
            },
        );
        let expected = alloc(
            &mut arena,
            Expr::Subword {
                root_id: e1,
                fallback: 0,
                span: HumanSpan::default(),
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
        let sequence_expr = Sequence {
            children: vec![terminal_id, nonterm_id],
            span: HumanSpan::default(),
        };
        let sequence_id = alloc(&mut arena, sequence_expr);
        let subword_expr = Subword {
            root_id: sequence_id,
            fallback: 0,
            span: HumanSpan::default(),
        };
        let subword_id = alloc(&mut arena, subword_expr);
        let expected = alloc(
            &mut arena,
            DistributiveDescription {
                child: subword_id,
                descr: ustr("use markers to highlight the matching strings"),
                span: Default::default(),
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
        let sequence1_id = alloc(
            &mut arena,
            Sequence {
                children: vec![terminal1_id, nonterm1_id],
                span: HumanSpan::default(),
            },
        );
        let subword_id = alloc(&mut arena, Expr::subword(sequence1_id));
        let terminal2_id = alloc(&mut arena, Expr::term("--color"));
        let nonterm2_id = alloc(&mut arena, Expr::nontermref("WHEN"));
        let sequence2_id = alloc(
            &mut arena,
            Sequence {
                children: vec![terminal2_id, nonterm2_id],
                span: Default::default(),
            },
        );
        let alternative_id = alloc(
            &mut arena,
            Alternative {
                children: vec![subword_id, sequence2_id],
                span: Default::default(),
            },
        );
        let expected = alloc(
            &mut arena,
            DistributiveDescription {
                child: alternative_id,
                descr: ustr("use markers to highlight the matching strings"),
                span: Default::default(),
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
    fn parses_escaped_terminal() {
        let (s, t) = terminal(Span::new("\\...")).unwrap();
        assert!(s.is_empty(), "{s}");
        assert_eq!(t, "...", "{t}");

        let (s, t) = terminal(Span::new(".\\..")).unwrap();
        assert!(s.is_empty(), "{s}");
        assert_eq!(t, "...", "{t}");

        let (s, t) = terminal(Span::new("..\\.")).unwrap();
        assert!(s.is_empty(), "{s}");
        assert_eq!(t, "...", "{t}");
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
            matches!(arena[e], Command { cmd, fallback: 0, .. } if cmd == "rustup toolchain list | cut -d' ' -f1")
        );
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
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
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
        let expected = alloc(
            &mut arena,
            Optional {
                child: nonterm_id,
                span: Default::default(),
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_one_or_more_expr() {
        const INPUT: &str = "<foo>...";
        let mut arena: Vec<Expr> = Default::default();
        let (s, actual) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let nonterm_id = alloc(&mut arena, Expr::nontermref("foo"));
        let expected = alloc(
            &mut arena,
            Many1 {
                child: nonterm_id,
                span: Default::default(),
            },
        );
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
        let expected = alloc(
            &mut arena,
            Sequence {
                children: vec![nonterm1_id, nonterm2_id],
                span: Default::default(),
            },
        );
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
        let seq_id = alloc(
            &mut arena,
            Sequence {
                children: vec![a_id, b_id],
                span: Default::default(),
            },
        );
        let c_id = alloc(&mut arena, Expr::term("c"));
        let expected = alloc(
            &mut arena,
            Alternative {
                children: vec![seq_id, c_id],
                span: Default::default(),
            },
        );
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
        let alt_id = alloc(
            &mut arena,
            Alternative {
                children: vec![b_id, c_id],
                span: Default::default(),
            },
        );
        let expected = alloc(
            &mut arena,
            Sequence {
                children: vec![a_id, alt_id],
                span: Default::default(),
            },
        );
        assert!(teq(actual, expected, &arena));
    }

    #[test]
    fn parses_variant() {
        const INPUT: &str = r#"foo bar;"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, v) = call_variant(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        let Statement::CallVariant {
            name: head,
            expr: actual,
            name_span: _,
        } = v
        else {
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
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("foo"));
        let expected_bar_id = alloc(&mut g.arena, Expr::term("bar"));
        assert!(teq(*actual_expr_id, expected_bar_id, &g.arena));

        // Statement 2
        let (head, actual_expr_id) = match &g.statements[1] {
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
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
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (*head, *expr),
            _ => panic!("Expected CallVariant"),
        };

        assert_eq!(head, ustr("darcs"));

        let help_id = alloc(&mut g.arena, Expr::term("help"));

        let v_id = alloc(&mut g.arena, Expr::term("-v"));
        let verbose_id = alloc(&mut g.arena, Expr::term("--verbose"));
        let v_alt_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![v_id, verbose_id],
                span: Default::default(),
            },
        );

        let q_id = alloc(&mut g.arena, Expr::term("-q"));
        let quiet_id = alloc(&mut g.arena, Expr::term("--quiet"));
        let q_alt_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![q_id, quiet_id],
                span: Default::default(),
            },
        );

        let top_alt_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![v_alt_id, q_alt_id],
                span: Default::default(),
            },
        );
        let many1_id = alloc(
            &mut g.arena,
            Many1 {
                child: top_alt_id,
                span: Default::default(),
            },
        );

        let darcs_subcommand_id = alloc(&mut g.arena, Expr::term("DARCS_SUBCOMMAND"));
        let optional_subcommand_id = alloc(
            &mut g.arena,
            Optional {
                child: darcs_subcommand_id,
                span: Default::default(),
            },
        );
        let darcs_command_id = alloc(&mut g.arena, Expr::nontermref("DARCS_COMMAND"));
        let seq_command_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![darcs_command_id, optional_subcommand_id],
                span: Default::default(),
            },
        );
        let optional_command_id = alloc(
            &mut g.arena,
            Optional {
                child: seq_command_id,
                span: Default::default(),
            },
        );

        let expected_expr_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![help_id, many1_id, optional_command_id],
                span: Default::default(),
            },
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
<OPTION> = --color <WHEN>;
<WHEN> = always | never | auto;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 3);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let option_nonterm_id = alloc(&mut g.arena, Expr::nontermref("OPTION"));
        let optional_option_id = alloc(
            &mut g.arena,
            Optional {
                child: option_nonterm_id,
                span: Default::default(),
            },
        );
        let many1_option_id = alloc(
            &mut g.arena,
            Many1 {
                child: optional_option_id,
                span: Default::default(),
            },
        );
        let patterns_nonterm_id = alloc(&mut g.arena, Expr::nontermref("PATTERNS"));
        let file_nonterm_id = alloc(&mut g.arena, Expr::nontermref("FILE"));
        let optional_file_id = alloc(
            &mut g.arena,
            Optional {
                child: file_nonterm_id,
                span: Default::default(),
            },
        );
        let many1_file_id = alloc(
            &mut g.arena,
            Many1 {
                child: optional_file_id,
                span: Default::default(),
            },
        );
        let expected_expr1_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![many1_option_id, patterns_nonterm_id, many1_file_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2: <OPTION> = ...
        let (symbol, shell, expr_id2) = match &g.statements[1] {
            Statement::NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("OPTION"));
        assert!(shell.is_none());
        let color_term_id = alloc(&mut g.arena, Expr::term("--color"));
        let when_nonterm_id = alloc(&mut g.arena, Expr::nontermref("WHEN"));
        let expected_expr2_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![color_term_id, when_nonterm_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id2, expected_expr2_id, &g.arena));

        // Statement 3: <WHEN> = ...
        let (symbol, shell, expr_id3) = match &g.statements[2] {
            Statement::NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("WHEN"));
        assert!(shell.is_none());
        let always_term_id = alloc(&mut g.arena, Expr::term("always"));
        let never_term_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_term_id = alloc(&mut g.arena, Expr::term("auto"));
        let expected_expr3_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![always_term_id, never_term_id, auto_term_id],
                span: Default::default(),
            },
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
<OPTION> = --extended-regexp                      # "PATTERNS are extended regular expressions"
           | --fixed-strings                        # "PATTERNS are strings"
           | --basic-regexp                         # "PATTERNS are basic regular expressions"
           | --perl-regexp                          # "PATTERNS are Perl regular expressions"
# another comment
           ;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);

        let (symbol, shell, expr_id) = match &g.statements[0] {
            Statement::NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
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
            Alternative {
                children: vec![
                    extended_regexp_id,
                    fixed_strings_id,
                    basic_regexp_id,
                    perl_regexp_id,
                ],
                span: Default::default(),
            },
        );

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
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
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("cargo"));

        let plus_id = alloc(&mut g.arena, Expr::term("+"));
        let cmd_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr("rustup toolchain list | cut -d' ' -f1"),
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
            },
        );
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![plus_id, cmd_id],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let optional1_id = alloc(
            &mut g.arena,
            Optional {
                child: subword_id,
                span: Default::default(),
            },
        );

        let options_id = alloc(&mut g.arena, Expr::nontermref("OPTIONS"));
        let optional2_id = alloc(
            &mut g.arena,
            Optional {
                child: options_id,
                span: Default::default(),
            },
        );

        let command_id = alloc(&mut g.arena, Expr::nontermref("COMMAND"));
        let optional3_id = alloc(
            &mut g.arena,
            Optional {
                child: command_id,
                span: Default::default(),
            },
        );

        let expected_expr_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![optional1_id, optional2_id, optional3_id],
                span: Default::default(),
            },
        );

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
    }

    #[test]
    fn parses_prefix_grammar() {
        const INPUT: &str = r#"
grep --color=<WHEN> --version;
<WHEN> = always | never | auto;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1: grep ...
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let color_eq_id = alloc(&mut g.arena, Expr::term("--color="));
        let when_nonterm_id = alloc(&mut g.arena, Expr::nontermref("WHEN"));
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![color_eq_id, when_nonterm_id],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let version_id = alloc(&mut g.arena, Expr::term("--version"));
        let expected_expr1_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![subword_id, version_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2: <WHEN> = ...
        let (symbol, shell, expr_id2) = match &g.statements[1] {
            Statement::NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("WHEN"));
        assert!(shell.is_none());

        let always_id = alloc(&mut g.arena, Expr::term("always"));
        let never_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_id = alloc(&mut g.arena, Expr::term("auto"));
        let expected_expr2_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![always_id, never_id, auto_id],
                span: Default::default(),
            },
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
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("grep"));

        let color_eq_id = alloc(&mut g.arena, Expr::term("--color="));
        let always_id = alloc(&mut g.arena, Expr::term("always"));
        let never_id = alloc(&mut g.arena, Expr::term("never"));
        let auto_id = alloc(&mut g.arena, Expr::term("auto"));
        let alt_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![always_id, never_id, auto_id],
                span: Default::default(),
            },
        );
        let seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![color_eq_id, alt_id],
                span: Default::default(),
            },
        );
        let expected_expr_id = alloc(&mut g.arena, Expr::subword(seq_id));

        assert!(teq(*expr_id, expected_expr_id, &g.arena));
    }

    #[test]
    fn parses_strace_expr_grammar() {
        use Statement::*;
        const INPUT: &str = r#"
strace -e <EXPR>;
<EXPR> = [<qualifier>=][!]<value>[,<value>]...;
<qualifier> = trace | read | write | fault;
<value> = %file | file | all;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("strace"));
        let e_id = alloc(&mut g.arena, Expr::term("-e"));
        let expr_nonterm_id = alloc(&mut g.arena, Expr::nontermref("EXPR"));
        let expected_expr1_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![e_id, expr_nonterm_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("EXPR"));
        assert!(shell.is_none());
        let qualifier_nonterm_id = alloc(&mut g.arena, Expr::nontermref("qualifier"));
        let eq_id = alloc(&mut g.arena, Expr::term("="));
        let seq1_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![qualifier_nonterm_id, eq_id],
                span: Default::default(),
            },
        );
        let opt1_id = alloc(
            &mut g.arena,
            Optional {
                child: seq1_id,
                span: Default::default(),
            },
        );
        let bang_id = alloc(&mut g.arena, Expr::term("!"));
        let opt2_id = alloc(
            &mut g.arena,
            Optional {
                child: bang_id,
                span: Default::default(),
            },
        );
        let value_nonterm_id = alloc(&mut g.arena, Expr::nontermref("value"));
        let comma_id = alloc(&mut g.arena, Expr::term(","));
        let value_nonterm2_id = alloc(&mut g.arena, Expr::nontermref("value"));
        let seq2_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![comma_id, value_nonterm2_id],
                span: Default::default(),
            },
        );
        let opt3_id = alloc(
            &mut g.arena,
            Optional {
                child: seq2_id,
                span: Default::default(),
            },
        );
        let many1_id = alloc(
            &mut g.arena,
            Many1 {
                child: opt3_id,
                span: Default::default(),
            },
        );
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![opt1_id, opt2_id, value_nonterm_id, many1_id],
                span: Default::default(),
            },
        );
        let expected_expr2_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        // Statement 3
        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
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
            Alternative {
                children: vec![trace_id, read_id, write_id, fault_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        // Statement 4
        let (symbol, shell, expr_id) = match &g.statements[3] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("value"));
        assert!(shell.is_none());
        let file_percent_id = alloc(&mut g.arena, Expr::term("%file"));
        let file_id = alloc(&mut g.arena, Expr::term("file"));
        let all_id = alloc(&mut g.arena, Expr::term("all"));
        let expected_expr4_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![file_percent_id, file_id, all_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr4_id, &g.arena));
    }

    #[test]
    fn parses_lsof_filter_grammar() {
        use Statement::*;
        const INPUT: &str = r#"
lsof -s<PROTOCOL>:<STATE-SPEC>[,<STATE-SPEC>]...;
<PROTOCOL> = TCP | UDP;
<STATE-SPEC> = [^]<STATE>;
<STATE> = LISTEN | CLOSED;
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 4);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("lsof"));
        let s_id = alloc(&mut g.arena, Expr::term("-s"));
        let protocol_id = alloc(&mut g.arena, Expr::nontermref("PROTOCOL"));
        let colon_id = alloc(&mut g.arena, Expr::term(":"));
        let state_spec_id = alloc(&mut g.arena, Expr::nontermref("STATE-SPEC"));
        let comma_id = alloc(&mut g.arena, Expr::term(","));
        let state_spec2_id = alloc(&mut g.arena, Expr::nontermref("STATE-SPEC"));
        let seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![comma_id, state_spec2_id],
                span: Default::default(),
            },
        );
        let opt_id = alloc(
            &mut g.arena,
            Optional {
                child: seq_id,
                span: Default::default(),
            },
        );
        let many1_id = alloc(
            &mut g.arena,
            Many1 {
                child: opt_id,
                span: Default::default(),
            },
        );
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![s_id, protocol_id, colon_id, state_spec_id, many1_id],
                span: Default::default(),
            },
        );
        let expected_expr1_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("PROTOCOL"));
        assert!(shell.is_none());
        let tcp_id = alloc(&mut g.arena, Expr::term("TCP"));
        let udp_id = alloc(&mut g.arena, Expr::term("UDP"));
        let expected_expr2_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![tcp_id, udp_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr2_id, &g.arena));

        // Statement 3
        let (symbol, shell, expr_id) = match &g.statements[2] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("STATE-SPEC"));
        assert!(shell.is_none());
        let caret_id = alloc(&mut g.arena, Expr::term("^"));
        let opt_caret_id = alloc(
            &mut g.arena,
            Optional {
                child: caret_id,
                span: Default::default(),
            },
        );
        let state_id = alloc(&mut g.arena, Expr::nontermref("STATE"));
        let subword_seq2_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![opt_caret_id, state_id],
                span: Default::default(),
            },
        );
        let expected_expr3_id = alloc(&mut g.arena, Expr::subword(subword_seq2_id));
        assert!(teq(*expr_id, expected_expr3_id, &g.arena));

        // Statement 4
        let (symbol, shell, expr_id) = match &g.statements[3] {
            NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("STATE"));
        assert!(shell.is_none());
        let listen_id = alloc(&mut g.arena, Expr::term("LISTEN"));
        let closed_id = alloc(&mut g.arena, Expr::term("CLOSED"));
        let expected_expr4_id = alloc(
            &mut g.arena,
            Alternative {
                children: vec![listen_id, closed_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr4_id, &g.arena));
    }

    #[test]
    fn parses_shell_command_nonterminal_definition() {
        const INPUT: &str = r#"
cargo [+<toolchain>] [<OPTIONS>] [<COMMAND>];
<toolchain> = {{{ rustup toolchain list | cut -d' ' -f1 }}};
"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 2);

        // Statement 1
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("cargo"));

        let plus_id = alloc(&mut g.arena, Expr::term("+"));
        let toolchain_id = alloc(&mut g.arena, Expr::nontermref("toolchain"));
        let subword_seq_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![plus_id, toolchain_id],
                span: Default::default(),
            },
        );
        let subword_id = alloc(&mut g.arena, Expr::subword(subword_seq_id));
        let opt1_id = alloc(
            &mut g.arena,
            Optional {
                child: subword_id,
                span: Default::default(),
            },
        );

        let options_id = alloc(&mut g.arena, Expr::nontermref("OPTIONS"));
        let opt2_id = alloc(
            &mut g.arena,
            Optional {
                child: options_id,
                span: Default::default(),
            },
        );

        let command_id = alloc(&mut g.arena, Expr::nontermref("COMMAND"));
        let opt3_id = alloc(
            &mut g.arena,
            Optional {
                child: command_id,
                span: Default::default(),
            },
        );

        let expected_expr1_id = alloc(
            &mut g.arena,
            Sequence {
                children: vec![opt1_id, opt2_id, opt3_id],
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr1_id, &g.arena));

        // Statement 2
        let (symbol, shell, expr_id) = match &g.statements[1] {
            Statement::NonterminalDefinition(NontermDefn {
                lhs_name: symbol,
                shell,
                rhs_expr_id: expr,
                lhs_span: _,
            }) => (symbol, shell, expr),
            _ => panic!("Expected NonterminalDefinition"),
        };
        assert_eq!(*symbol, ustr("toolchain"));
        assert!(shell.is_none());
        let expected_expr2_id = alloc(
            &mut g.arena,
            Command {
                cmd: ustr("rustup toolchain list | cut -d' ' -f1"),
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
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
    fn parses_descr_escaped_chars() {
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
        let expected = alloc(
            &mut arena,
            Sequence {
                children: vec![term_id, nonterm_id],
                span: Default::default(),
            },
        );
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
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, *expr),
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
    fn issue_15() {
        const INPUT: &str = r#"foo.sh [-h] ;"#;
        let mut g = Grammar::parse(INPUT).map_err(|e| e.to_string()).unwrap();
        assert_eq!(g.statements.len(), 1);
        let (head, expr_id) = match &g.statements[0] {
            Statement::CallVariant {
                name: head,
                expr,
                name_span: _,
            } => (head, expr),
            _ => panic!("Expected CallVariant"),
        };
        assert_eq!(*head, ustr("foo.sh"));
        let h_id = alloc(&mut g.arena, Expr::term("-h"));
        let expected_expr_id = alloc(
            &mut g.arena,
            Optional {
                child: h_id,
                span: Default::default(),
            },
        );
        assert!(teq(*expr_id, expected_expr_id, &g.arena));
    }

    #[test]
    fn parses_nonterminal_shell_specific() {
        const INPUT: &str = r#"<FILE@bash>"#;
        let (s, (nonterm, _, shell, _)) = nonterm_specialization(Span::new(INPUT)).unwrap();
        assert!(s.is_empty());
        assert_eq!(nonterm, "FILE");
        assert_eq!(shell, "bash");
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
        let fallback_id = alloc(
            &mut arena,
            Fallback {
                children: vec![foo_id, bar_id],
                span: Default::default(),
            },
        );
        let expected_expr_id = alloc(
            &mut arena,
            Sequence {
                children: vec![cmd_id, fallback_id],
                span: Default::default(),
            },
        );

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
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
            },
        );
        let dotdot_id = alloc(&mut arena, Expr::term(".."));
        let git_tag2_id = alloc(
            &mut arena,
            Command {
                cmd: ustr("git tag"),
                zsh_compadd: false,
                fallback: 0,
                span: HumanSpan::default(),
            },
        );
        let seq_id = alloc(
            &mut arena,
            Sequence {
                children: vec![git_tag1_id, dotdot_id, git_tag2_id],
                span: Default::default(),
            },
        );
        let expected_expr_id = alloc(&mut arena, Expr::subword(seq_id));

        assert!(teq(e, expected_expr_id, &arena));
    }

    #[test]
    fn parses_quoted_operators() {
        const INPUT: &str = r#"<_>\...<_>"#;
        let mut arena: Vec<Expr> = Default::default();
        let (s, e) = expr(&mut arena, Span::new(INPUT)).unwrap();
        assert!(s.is_empty());

        let left = alloc(&mut arena, Expr::nontermref("_"));
        let sep = alloc(&mut arena, Expr::term("..."));
        let right = alloc(&mut arena, Expr::nontermref("_"));
        let seq = alloc(
            &mut arena,
            Sequence {
                children: vec![left, sep, right],
                span: Default::default(),
            },
        );
        let expected_expr_id = alloc(&mut arena, Expr::subword(seq));

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
        let expected_expr_id = alloc(
            &mut arena,
            Sequence {
                children: vec![cmd_id, foo_id],
                span: Default::default(),
            },
        );

        assert!(teq(e, expected_expr_id, &arena));
    }
}
