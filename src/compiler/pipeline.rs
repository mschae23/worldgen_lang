use std::rc::Rc;
use crate::compiler::error::ErrorReporting;
use crate::compiler::lexer::Lexer;
use crate::Config;

impl CompileState {
    pub fn new(config: Rc<Config>, source: &str) -> ReadState {
        ReadState {
            config, source,
        }
    }
}

pub struct ReadState<'source> {
    pub config: Rc<Config>,
    pub source: &'source str,
}

impl<'source> ReadState<'source> {
    pub fn tokenize(self) -> TokenizedState<'source> {
        let reporting = ErrorReporting::new(Rc::clone(&self.config), self.source, Rc::new(self.config.input.clone()));
        let lexer = Lexer::new(self.source);

        TokenizedState {
            config: self.config, reporting, lexer,
        }
    }
}

pub struct TokenizedState<'source> {
    pub config: Rc<Config>,
    pub reporting: ErrorReporting<'source>,
    pub lexer: Lexer<'source>,
}

impl<'source> TokenizedState<'source> {
    /* pub fn parse(self) -> ParsedState<'source> {
        ParsedState {
            reporting, // ...
        }
    } */
}

pub struct CompileState {
}
