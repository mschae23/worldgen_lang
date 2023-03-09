use crate::compiler::error::ErrorReporting;
use crate::compiler::lexer::Lexer;

impl CompileState {
    pub fn new(source: &str) -> ReadState {
        ReadState {
            source,
        }
    }
}

pub struct ReadState<'source> {
    pub source: &'source str,
}

impl<'source> ReadState<'source> {
    pub fn tokenize(self) -> TokenizedState<'source> {
        let reporting = ErrorReporting::new(self.source);
        let lexer = Lexer::new(self.source);

        TokenizedState {
            reporting, lexer,
        }
    }
}

pub struct TokenizedState<'source> {
    pub reporting: ErrorReporting<'source>,
    pub lexer: Lexer<'source>,
}

pub struct CompileState {
}
