use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
    DeclAlreadyDeclared(&'source str, Option<Span>),
    TypeNotFound(String),
    ConversionParameterCount(usize),
}

impl<'source> Diagnostic<MessageMarker> for DeclError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::TypeAlreadyDeclared(_, _) => "decl/already_declared_type",
            Self::DeclAlreadyDeclared(_, _) => "decl/already_declared_decl",
            Self::TypeNotFound(_) => "decl/type_not_found",
            Self::ConversionParameterCount(_) => "decl/conversion_parameter_count",
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
            Self::TypeAlreadyDeclared(name, _) => format!("Type `{}` was already declared", name),
            Self::DeclAlreadyDeclared(name, _) => format!("`{}` was already declared", name),
            Self::TypeNotFound(name) => format!("Type `{}` not found", name),
            Self::ConversionParameterCount(count) => if *count == 0 {
                String::from("Conversion template requires exactly one parameter")
            } else {
                format!("Conversion template has more than one parameter: {}", count)
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

pub type DeclErrorReporter<'source> = ErrorReporter<MessageMarker, DeclError<'source>>;

pub struct ForwardDeclarer {
    _config: Rc<Config>,
    path: Rc<PathBuf>, file_id: FileId,
}

impl<'source> ForwardDeclarer {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId) -> Self {
        ForwardDeclarer {
            _config: config, path, file_id,
        }
    }

    pub fn forward_declare(&mut self, _declarations: Vec<Decl<'source>>, _reporter: &mut DeclErrorReporter<'source>) /* -> ForwardDeclarationResult */ {
        // TODO
    }

    fn error(&self, span: Span, message: DeclError<'source>, reporter: &mut DeclErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
