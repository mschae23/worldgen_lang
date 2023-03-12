use crate::compiler::lexer::{Token, TokenPos};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32, // Uses byte indices
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Span {
            start, end,
        }
    }

    pub fn from_pos(start: &TokenPos, end: &TokenPos) -> Self {
        Self::new(start.index, end.index)
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn end(&self) -> u32 {
        self.end
    }

    /// Combines the spans by using the start of `self` and the end of `other`.
    pub fn mix(self, other: Self) -> Self {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    pub fn merge(self, other: Self) -> Self {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn overlaps(&self, other: &Self) -> bool {
        (self.start <= other.start && self.end > other.start)
            || (other.start <= self.start && other.end > self.start)
    }

    pub fn includes(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

impl<'source> From<Token<'source>> for Span {
    fn from(value: Token<'source>) -> Self {
        Self::from_pos(&value.start, &value.end)
    }
}

impl<'source> From<&Token<'source>> for Span {
    fn from(value: &Token<'source>) -> Self {
        Self::new(value.start.index, value.end.index)
    }
}
