use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::ForwardDeclStorage;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{CompileStage, ErrorReporting, FileId};
use crate::compiler::forward_declare::ForwardDeclarer;
use crate::compiler::lexer::Lexer;
use crate::compiler::name::TypeStorage;
use crate::compiler::parser::Parser;
use crate::compiler::type_checker::TypeChecker;
use crate::Config;

impl CompileState {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(config: Rc<Config>, source: &str, input: Rc<PathBuf>, file_id: FileId) -> ReadState {
        ReadState {
            config, source,  input, file_id,
        }
    }
}

pub struct ReadState<'source> {
    pub config: Rc<Config>,
    pub source: &'source str, input: Rc<PathBuf>, file_id: FileId,
}

impl<'source> ReadState<'source> {
    pub fn tokenize(self) -> TokenizedState<'source> {
        let lexer = Lexer::new(self.source);

        TokenizedState {
            config: self.config, input: Rc::clone(&self.input), file_id: self.file_id,
            lexer,
        }
    }
}

pub struct TokenizedState<'source> {
    pub config: Rc<Config>,  input: Rc<PathBuf>, file_id: FileId,
    pub lexer: Lexer<'source>,
}

impl<'source> TokenizedState<'source> {
    pub fn parse(self, reporting: &mut ErrorReporting) -> ParsedState<'source> {
        let mut lexer_reporter = reporting.create_for_stage(CompileStage::Lexer, self.file_id, ());
        let mut parser_reporter = reporting.create_for_stage(CompileStage::Parser,  self.file_id, ());

        let mut parser = Parser::new(self.lexer);
        let declarations = parser.parse(&mut parser_reporter, &mut lexer_reporter);

        reporting.submit(lexer_reporter);
        reporting.submit(parser_reporter);

        ParsedState {
            config: self.config, input: self.input, file_id: self.file_id,
            declarations,
        }
    }
}

pub struct ParsedState<'source> {
    pub config: Rc<Config>, input: Rc<PathBuf>, file_id: FileId,
    pub declarations: Vec<Decl<'source>>,
}

impl<'source> ParsedState<'source> {
    pub fn forward_declare(self, reporting: &mut ErrorReporting) -> ForwardDeclaredState<'source> {
        let mut reporter = reporting.create_for_stage(CompileStage::ForwardDeclarer,  self.file_id, ());

        let forward_declarer = ForwardDeclarer::new(Rc::clone(&self.config),
            Rc::clone(&self.input), self.file_id);
        let result = forward_declarer.forward_declare(self.declarations, &mut reporter);

        reporting.submit(reporter);

        ForwardDeclaredState {
            config: self.config, input: self.input, file_id: self.file_id,
            declarations: result.declarations,
            type_storage: result.types,
            forward_declaration_storage: result.storage,
        }
    }
}

pub struct ForwardDeclaredState<'source> {
    pub config: Rc<Config>, input: Rc<PathBuf>, file_id: FileId,
    pub declarations: Vec<Decl<'source>>,
    pub type_storage: TypeStorage,
    pub forward_declaration_storage: ForwardDeclStorage,
}

impl<'source> ForwardDeclaredState<'source> {
    pub fn check_types(self, reporting: &mut ErrorReporting) -> TypeCheckedState {
        let mut reporter = reporting.create_for_stage(CompileStage::TypeChecker,  self.file_id, ());

        let type_checker = TypeChecker::new(Rc::clone(&self.config),
            Rc::clone(&self.input), self.file_id, self.type_storage, self.forward_declaration_storage);
        type_checker.check_types(self.declarations, &mut reporter);

        reporting.submit(reporter);

        TypeCheckedState {
            config: self.config, _input: self.input, _file_id: self.file_id,
        }
    }
}

pub struct TypeCheckedState {
    pub config: Rc<Config>, _input: Rc<PathBuf>, _file_id: FileId,
    // TODO Result from type checker
}

pub struct CompileState {
}
