use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::ForwardDeclStorage;
use crate::compiler::ast::simple::Decl;
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{NameResolution, TypeStorage};
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
    DeclAlreadyDeclared(&'source str, Option<Span>),
    TypeNotFound(String),
    ConversionParameterCount(usize),
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::TypeAlreadyDeclared(_, _) => "type/already_declared_type",
            Self::DeclAlreadyDeclared(_, _) => "type/already_declared_decl",
            Self::TypeNotFound(_) => "type/not_found",
            Self::ConversionParameterCount(_) => "type/conversion_parameter_count",
        }
    }

    fn severity(&self) -> Severity {
        #[allow(clippy::match_single_binding)]
        match self {
            _ => Severity::Error,
        }
    }

    fn message(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> String {
        match self {
            Self::TypeAlreadyDeclared(name, _) => format!("type `{}` was already declared", name),
            Self::DeclAlreadyDeclared(name, _) => format!("`{}` was already declared", name),
            Self::TypeNotFound(name) => format!("type `{}` not found", name),
            Self::ConversionParameterCount(count) => if *count == 0 {
                String::from("conversion template requires exactly one parameter")
            } else {
                format!("conversion template has more than one parameter: {}", count)
            },
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::TypeAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
            Self::DeclAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
            Self::TypeNotFound(name) => Some(format!("`{}` referenced here", name)),
            Self::ConversionParameterCount(count) => Some(format!("{} parameters declared here", count)),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
            Self::TypeAlreadyDeclared(_, span) => if let Some(&span) = span.as_ref() {
                vec![(span, Some(String::from("first declared here")))]
            } else { vec![] },
            Self::DeclAlreadyDeclared(_, span) => if let Some(&span) = span.as_ref() {
                vec![(span, Some(String::from("first declared here")))]
            } else { vec![] },
            _ => Vec::new(),
        }
    }

    fn primary_note(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<(NoteKind, String)> {
        None
    }

    fn additional_notes(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
        Vec::new()
    }
}

pub type TypeErrorReporter<'source> = ErrorReporter<MessageMarker, TypeError<'source>>;

pub struct TypeChecker {
    _config: Rc<Config>,
    _path: Rc<PathBuf>, file_id: FileId,

    types: TypeStorage, forward_decls: ForwardDeclStorage,
    names: NameResolution,
}

impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId, type_storage: TypeStorage, forward_decls: ForwardDeclStorage) -> Self {
        TypeChecker {
            _config: config, _path: path, file_id,
            types: type_storage, forward_decls,
            names: NameResolution::new(),
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl<'source>>, reporter: &mut TypeErrorReporter<'source>) -> Vec<TypedDecl<'source>> {
        eprintln!("[debug] Name resolution state:\n{:#?}\n{:#?}\n{:#?}", &self.types, &self.forward_decls, &self.names);

        let /* mut */ typed_declarations = Vec::new();

        for _decl in declarations {
            // typed_declarations.push(self.check_declaration(decl));
        }

        typed_declarations
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
