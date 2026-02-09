use grammar::HumanSpan;
use std::string::FromUtf8Error;
use ustr::Ustr;

use crate::dfa::Inp;

pub mod bash;
pub mod dfa;
pub mod fish;
pub mod grammar;
pub mod pwsh;
pub mod regex;
pub mod scrape;
pub mod zsh;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parse error")]
    ParseError(HumanSpan),

    #[error("Grammar needs to contain at least one call variant, e.g. grep;")]
    MissingCallVariants,

    #[error("Invalid command name")]
    InvalidCommandName(HumanSpan),

    #[error("Multiple commands")]
    VaryingCommandNames(Box<[HumanSpan]>),

    #[error("Nonterminal definitions depend on each other cyclically")]
    NonterminalDefinitionsCycle(Box<[HumanSpan]>),

    #[error("Duplicate nonterminal definition")]
    DuplicateNonterminalDefinition(HumanSpan, HumanSpan),

    #[error("Unknown shell")]
    UnknownShell(HumanSpan),

    #[error("Can only specialize external commands")]
    NonCommandSpecialization(HumanSpan),

    #[error("Ambiguity in matching: {:?} {:?}", .0, .1)]
    AmbiguousMatchable(HumanSpan, HumanSpan),

    #[error("Ambiguity in matching")]
    UnboundedMatchable(HumanSpan, HumanSpan),

    #[error("Clashing variants")]
    ConflictingDescriptions(Box<[Inp]>, Ustr, Ustr, Ustr),

    #[error("Two adjacent terminals in a subword expression")]
    SubwordSpaces(HumanSpan, HumanSpan, Box<[HumanSpan]>),

    #[error("Ambiguous DFA: {:?} {:?}", .0, .1)]
    AmbiguousDFA(Box<[Inp]>, Box<[Inp]>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u32;

pub(crate) fn make_dot_string_constant(s: &str) -> String {
    let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
    format!(r#""{escaped}""#)
}
