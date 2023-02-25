#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parsing error: {:?}", .0)]
    ParsingError(String),

    #[error("Empty grammar")]
    EmptyGrammar,

    #[error("One one command is allowed in completions definition")]
    VaryingCommandNames(Vec<String>),
}

pub type Result<T> = std::result::Result<T, Error>;
