use grammar::ChicSpan;
use std::string::FromUtf8Error;
use ustr::Ustr;

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

    #[error("One command is allowed in completions definition")]
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

    #[error("Commands are only allowed at a tail position to avoid ambiguities")]
    NontailCommand(Ustr, ChicSpan),

    #[error("Undefined nonterminal at a non-tail position")]
    NontailUndefNonterm(Ustr, ChicSpan),

    #[error("Two adjacent terminals in a subword expression: {:?}", .0)]
    SubwordSpaces(ChicSpan, ChicSpan, Vec<ChicSpan>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u16;
