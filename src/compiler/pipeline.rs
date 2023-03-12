use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{CompileStage, ErrorReporting};
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
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
    pub fn parse(mut self) -> ParsedState<'source> {
        let mut lexer_reporter = self.reporting.create_for_stage(CompileStage::Lexer, ());
        let mut parser_reporter = self.reporting.create_for_stage(CompileStage::Parser, ());

        let mut parser = Parser::new(self.lexer);
        let declarations = parser.parse(&mut parser_reporter, &mut lexer_reporter);

        self.reporting.submit(lexer_reporter);
        self.reporting.submit(parser_reporter);

        ParsedState {
            config: self.config, reporting: self.reporting,
            declarations,
        }
    }
}

pub struct ParsedState<'source> {
    pub config: Rc<Config>,
    pub reporting: ErrorReporting<'source>,
    pub declarations: Vec<Decl<'source>>,
}

pub struct CompileState {
}
