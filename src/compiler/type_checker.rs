use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
}

impl Diagnostic<MessageMarker> for TypeError {
    fn name(&self) -> &'static str {
        todo!()
    }

    fn severity(&self) -> Severity {
        todo!()
    }

    fn message(&self, context: &DiagnosticContext<'_, MessageMarker>) -> String {
        todo!()
    }

    fn primary_annotation(&self, context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        todo!()
    }

    fn additional_annotations(&self, context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        todo!()
    }

    fn primary_note(&self, context: &DiagnosticContext<'_, MessageMarker>) -> Option<(NoteKind, String)> {
        todo!()
    }

    fn additional_notes(&self, context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
        todo!()
    }
}

pub type TypeErrorReporter = ErrorReporter<MessageMarker, TypeError>;

pub struct TypeChecker {
    _config: Rc<Config>,
    had_error: bool,
}

impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>) -> Self {
        TypeChecker {
            _config: config,
            had_error: false,
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl<'source>>) -> Vec<TypedDecl<'source>> {
        for decl in &declarations {
            self.declare_declaration(decl);
        }

        let mut typed_declarations = Vec::new();

        for decl in declarations {
            // typed_declarations.push(self.check_declaration(decl));
        }

        typed_declarations
    }
    fn declare_declaration(&mut self, decl: &Decl) {
        todo!()
    }
}
