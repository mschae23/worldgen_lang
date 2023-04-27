use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::{ForwardDeclaredDecl, ForwardDeclStorage};
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{CompileStage, ErrorReporting, FileId};
use crate::compiler::error::span::SpanWithFile;
use crate::compiler::forward_declare::ForwardDeclarer;
use crate::compiler::lexer::Lexer;
use crate::compiler::name::{NameResolution, TypeStorage};
use crate::compiler::parser::Parser;
use crate::compiler::type_checker::TypeChecker;
use crate::Config;

impl CompileState {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(config: Rc<Config>, source: &str, paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, file_id: FileId, type_id_offset: usize, decl_id_offset: usize) -> ReadState {
        ReadState {
            config, source,  paths, file_id,
            type_id_offset, decl_id_offset,
        }
    }
}

pub struct ReadState<'source> {
    pub config: Rc<Config>,
    pub source: &'source str, pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, pub file_id: FileId,
    pub type_id_offset: usize, pub decl_id_offset: usize,
}

impl<'source> ReadState<'source> {
    pub fn tokenize(self) -> TokenizedState<'source> {
        let lexer = Lexer::new(self.source, self.file_id);

        TokenizedState {
            config: self.config, paths: self.paths, file_id: self.file_id,
            type_id_offset: self.type_id_offset, decl_id_offset: self.decl_id_offset,
            lexer,
        }
    }
}

pub struct TokenizedState<'source> {
    pub config: Rc<Config>,  pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, pub file_id: FileId,
    pub type_id_offset: usize, pub decl_id_offset: usize,
    pub lexer: Lexer<'source>,
}

impl<'source> TokenizedState<'source> {
    pub fn parse(self, reporting: &mut ErrorReporting) -> ParsedState<'source> {
        let mut lexer_reporter = reporting.create_for_stage(CompileStage::Lexer, self.file_id, ());
        let mut parser_reporter = reporting.create_for_stage(CompileStage::Parser,  self.file_id, ());

        let mut parser = Parser::new(self.lexer, self.file_id);
        let declarations = parser.parse(&mut parser_reporter, &mut lexer_reporter);

        reporting.submit(lexer_reporter);
        reporting.submit(parser_reporter);

        ParsedState {
            config: self.config, paths: self.paths, file_id: self.file_id,
            type_id_offset: self.type_id_offset, decl_id_offset: self.decl_id_offset,
            declarations,
        }
    }
}

pub struct ParsedState<'source> {
    pub config: Rc<Config>, pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, pub file_id: FileId,
    pub type_id_offset: usize, pub decl_id_offset: usize,
    pub declarations: Vec<Decl<'source>>,
}

impl<'source> ParsedState<'source> {
    pub fn forward_declare(self, reporting: &mut ErrorReporting) -> ForwardDeclaredState<'source> {
        let mut reporter = reporting.create_for_stage(CompileStage::ForwardDeclarer,  self.file_id, ());

        let forward_declarer = ForwardDeclarer::new(Rc::clone(&self.config),
            Rc::clone(&self.paths[self.paths.len() - 1].0), self.file_id, self.type_id_offset, self.decl_id_offset);
        let result = forward_declarer.forward_declare(self.declarations, &mut reporter);

        reporting.submit(reporter);

        ForwardDeclaredState {
            config: self.config, paths: self.paths, file_id: self.file_id,
            type_id_offset: self.type_id_offset, decl_id_offset: self.decl_id_offset,
            declarations: result.declarations,
            type_storage: result.types,
            forward_declaration_storage: result.storage,
        }
    }
}

pub struct ForwardDeclaredState<'source> {
    pub config: Rc<Config>, pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, pub file_id: FileId,
    pub type_id_offset: usize, pub decl_id_offset: usize,
    pub declarations: Vec<ForwardDeclaredDecl<'source>>,
    pub type_storage: TypeStorage,
    pub forward_declaration_storage: ForwardDeclStorage,
}

impl<'source> ForwardDeclaredState<'source> {
    pub fn check_types(self, reporting: &mut ErrorReporting) -> TypeCheckedState {
        let mut reporter = reporting.create_for_stage(CompileStage::TypeChecker,  self.file_id, ());

        let mut type_checker = TypeChecker::new(Rc::clone(&self.config),
            self.paths, self.file_id, self.type_id_offset, self.decl_id_offset,
            self.type_storage.get_type_count(), self.forward_declaration_storage, reporting);
        type_checker.check_types(self.declarations, &mut reporter);

        let paths = type_checker.paths;
        let names = type_checker.names;

        reporting.submit(reporter);

        TypeCheckedState {
            config: self.config, paths, _file_id: self.file_id,
            names,
        }
    }
}

pub struct TypeCheckedState {
    pub config: Rc<Config>, pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, pub _file_id: FileId,
    pub names: NameResolution,
}

pub struct CompileState {
}
