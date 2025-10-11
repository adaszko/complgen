use grammar::HumanSpan;
use std::string::FromUtf8Error;
use ustr::Ustr;

use crate::regex::Inp;

pub mod bash;
pub mod dfa;
pub mod fish;
pub mod grammar;
pub mod regex;
pub mod scrape;
pub mod zsh;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parse error")]
    ParseError(HumanSpan),

    #[error("Grammar needs to contain at least one call variant, e.g. grep;")]
    MissingCallVariants,

    #[error("Multiple commands: {:?}", .0)]
    VaryingCommandNames(Vec<Ustr>),

    #[error("Invalid command name: {}", .0)]
    InvalidCommandName(String),

    #[error("Nonterminal definitions depend on each other cyclically")]
    NonterminalDefinitionsCycle(Option<Vec<Ustr>>),

    #[error("Duplicate nonterminal definition")]
    DuplicateNonterminalDefinition(Ustr, Option<Ustr>),

    #[error("Unknown shell: {}", .0)]
    UnknownShell(Ustr),

    #[error("Can only specialize external commands: {}@{:?}", .0, .1)]
    NonCommandSpecialization(Ustr, Option<Ustr>),

    #[error("Ambiguity in matching: {:?} {:?}", .0, .1)]
    AmbiguousMatchable(HumanSpan, HumanSpan),

    #[error("Ambiguity in matching: {:?} {:?}", .0, .1)]
    UnboundedMatchable(HumanSpan, HumanSpan),

    #[error("Ambiguous DFA: {:?} {:?}", .0, .1)]
    AmbiguousDFA(Box<[Inp]>, Box<[Inp]>),

    #[error("Clashing variants: {:?} {:?}", .0, .1)]
    ClashingVariants(HumanSpan, HumanSpan),

    #[error("Clashing subword leaders: {:?} {:?}", .0, .1)]
    ClashingSubwordLeaders(HumanSpan, HumanSpan),

    #[error("Two adjacent terminals in a subword expression: {:?}", .0)]
    SubwordSpaces(HumanSpan, HumanSpan, Vec<HumanSpan>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u32;
