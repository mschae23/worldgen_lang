use std::fmt::{Debug, Formatter};

#[derive(Clone, Copy, PartialEq, Eq)]
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

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}..{})", self.start, self.end)
    }
}
