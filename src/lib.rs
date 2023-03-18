mod error;

pub use error::Error;
pub type Result<T> = std::result::Result<T, Error>;

pub type StateId = usize;
pub const START_STATE_ID: StateId = 0;
