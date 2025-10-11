use grammar::HumanSpan;
use regex::Input;
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
    AmbiguousMatchable(Box<Input>, Box<Input>),

    #[error("Ambiguity in matching: {:?} {:?}", .0, .1)]
    UnboundedMatchable(Box<Input>, Box<Input>),

    #[error("Ambiguous DFA: {:?} {:?}", .0, .1)]
    AmbiguousDFA(Box<[Inp]>, Box<[Inp]>),

    #[error("Clashing variants: {:?} {:?}", .0, .1)]
    ClashingVariants(Option<HumanSpan>, Option<HumanSpan>),

    #[error("Clashing subword leaders: {:?} {:?}", .0, .1)]
    ClashingSubwordLeaders(Option<HumanSpan>, Option<HumanSpan>),

    #[error("Two adjacent terminals in a subword expression: {:?}", .0)]
    SubwordSpaces(Option<HumanSpan>, Option<HumanSpan>, Vec<HumanSpan>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u32;
