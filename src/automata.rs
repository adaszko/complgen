use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StateId(usize);

impl Display for StateId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl StateId {
    pub fn start() -> Self {
        StateId(0)
    }

    pub fn advance(&mut self) {
        self.0 += 1;
    }
}

impl From<usize> for StateId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<StateId> for usize {
    fn from(value: StateId) -> Self {
        value.0
    }
}
