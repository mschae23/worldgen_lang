use crate::compiler::lexer::{Token, TokenPos};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
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
