use std::string::FromUtf8Error;
use ustr::Ustr;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parsing error: {:?}", .0)]
    ParsingError(String),

    #[error("Grammar needs to contain at least one call variant, e.g. grep;")]
    MissingCallVariants,

    #[error("One one command is allowed in completions definition")]
    VaryingCommandNames(Vec<Ustr>),

    #[error("Nonterminal definitions depend on each other cyclically")]
    NonterminalDefinitionsCycle(Option<Vec<Ustr>>),

    #[error("Duplicate nonterminal definition")]
    DuplicateNonterminalDefinition(Ustr, Option<Ustr>),

    #[error("Unknown shell: {}", .0)]
    UnknownShell(Ustr),

    #[error("Can only specialize external commands: {}@{:?}", .0, .1)]
    NonCommandSpecialization(Ustr, Option<Ustr>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u16;
