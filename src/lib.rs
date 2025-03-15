use dfa::DFA;
use grammar::ChicSpan;
use std::{process::exit, string::FromUtf8Error};
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

    #[error("Two adjacent terminals in a subword expression: {:?}", .0)]
    SubwordSpaces(Vec<ChicSpan>),

    #[error("UTF-8 conversion error")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("Formatting error")]
    FmtError(#[from] std::fmt::Error),

    #[error("IO error")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = u16;

pub fn check_dfa_ambiguity(dfa: &DFA) {
    if let Some(inputs) = dfa.find_ambiguous_transition() {
        eprintln!("Error: Final DFA contains ambiguous transition(s).");
        eprintln!(
            "Ambiguous transition requires matching against: {:?}",
            inputs
        );
        eprintln!(
            "They arise when there's a need to match a shell word against the output of more than one external shell command."
        );
        eprintln!(
            "The grammar needs to be modified to match against at most one external command output to avoid ambiguities."
        );
        exit(1);
    }
}
