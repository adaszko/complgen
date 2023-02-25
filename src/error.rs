#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parsing error")]
    ParsingError,

    #[error("Empty grammar")]
    EmptyGrammar,

    #[error("One one command is allowed in completions definition")]
    VaryingCommandNames(Vec<String>),
}

pub type Result<T> = std::result::Result<T, Error>;
