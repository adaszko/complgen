use std::string::FromUtf8Error;
use ustr::Ustr;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parsing error: {:?}", .0)]
    ParsingError(String),

    #[error("Empty grammar")]
    EmptyGrammar,

    #[error("Trailing input")]
    TrailingInput(String),

    #[error("One one command is allowed in completions definition")]
    VaryingCommandNames(Vec<Ustr>),

    #[error("Variable definitions depend on each other cyclically")]
    NonterminalDefinitionsCycle(Option<Vec<Ustr>>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u16;
