use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{CompileStage, ErrorReporting};
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
use crate::Config;

impl CompileState {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(config: Rc<Config>, source: &str, input: Rc<PathBuf>) -> ReadState {
        ReadState {
            config, source,  input,
        }
    }
}

pub struct ReadState<'source> {
    pub config: Rc<Config>,
    pub source: &'source str, input: Rc<PathBuf>,
}

impl<'source> ReadState<'source> {
    pub fn tokenize(self) -> TokenizedState<'source> {
        let lexer = Lexer::new(self.source);

        TokenizedState {
            config: self.config, input: Rc::clone(&self.input), lexer,
        }
    }
}

pub struct TokenizedState<'source> {
    pub config: Rc<Config>,  input: Rc<PathBuf>,
    pub lexer: Lexer<'source>,
}

impl<'source> TokenizedState<'source> {
    pub fn parse(self, reporting: &mut ErrorReporting) -> ParsedState<'source> {
        let mut lexer_reporter = reporting.create_for_stage(CompileStage::Lexer, Rc::clone(&self.input), ());
        let mut parser_reporter = reporting.create_for_stage(CompileStage::Parser,  Rc::clone(&self.input), ());

        let mut parser = Parser::new(self.lexer);
        let declarations = parser.parse(&mut parser_reporter, &mut lexer_reporter);

        reporting.submit(lexer_reporter);
        reporting.submit(parser_reporter);

        ParsedState {
            config: self.config,
            declarations,
        }
    }
}

pub struct ParsedState<'source> {
    pub config: Rc<Config>,
    pub declarations: Vec<Decl<'source>>,
}

pub struct CompileState {
}
