use std::collections::VecDeque;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{NameResolution, SimpleTypeInfo};
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::TypeAlreadyDeclared(_, _) => "type/already_declared",
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
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            TypeError::TypeAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
            TypeError::TypeAlreadyDeclared(_, span) => if let Some(&span) = span.as_ref() {
                vec![(span, Some(String::from("first declared here")))]
            } else { vec![] },
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

    names: NameResolution,
}

impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>) -> Self {
        TypeChecker {
            _config: config,
            names: NameResolution::new(),
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl<'source>>, reporter: &mut TypeErrorReporter<'source>) -> Vec<TypedDecl<'source>> {
        self.declare_types(&declarations, reporter);
        self.declare_declarations(&declarations);

        let /* mut */ typed_declarations = Vec::new();

        for _decl in declarations {
            // typed_declarations.push(self.check_declaration(decl));
        }

        typed_declarations
    }

    fn declare_types(&mut self, decls: &[Decl<'source>], reporter: &mut TypeErrorReporter<'source>) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).collect::<VecDeque<_>>();
        let mut path = PathBuf::new();

        while let Some(variant) = to_process.pop_front() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations } => {
                            to_process.push_back(ProcessVariant::ModuleStart(name.source()));
                            to_process.extend(declarations.iter().map(ProcessVariant::Decl));
                            to_process.push_back(ProcessVariant::ModuleEnd);
                        },
                        Decl::Interface { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = self.names.get_simple_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_type_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.insert_simple_type(path.clone(), name.span(), /* TODO */ 0, SimpleTypeInfo::Class { interface: true, });
                            }

                            path.pop();
                        },
                        Decl::Class { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = self.names.get_simple_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_type_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.insert_simple_type(path.clone(), name.span(), 0, SimpleTypeInfo::Class { interface: false, });
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = self.names.get_simple_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_type_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.insert_simple_type(path.clone(), name.span(), 0, SimpleTypeInfo::TypeAlias);
                            }

                            path.pop();
                        },
                        // The remaining declarations don't create new types
                        _ => {},
                    }
                },
                ProcessVariant::ModuleStart(name) => {
                    path.push(name);
                },
                ProcessVariant::ModuleEnd => {
                    path.pop();
                },
            };
        }

        eprintln!("[debug] Name resolution state:\n{:#?}", &self.names);
    }

    fn declare_declarations(&mut self, decls: &[Decl]) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).collect::<VecDeque<_>>();
        let mut path = PathBuf::new();

        while let Some(variant) = to_process.pop_front() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations } => {
                            to_process.push_back(ProcessVariant::ModuleStart(name.source()));
                            to_process.extend(declarations.iter().map(ProcessVariant::Decl));
                            to_process.push_back(ProcessVariant::ModuleEnd);
                        },
                        Decl::Interface { .. } => {
                        },
                        Decl::Class { .. } => {},
                        Decl::TypeAlias { .. } => {},
                        Decl::Template { .. } => {},
                        Decl::Include { .. } => {},
                        Decl::Import { .. } => {},
                        Decl::Variable { .. } => {},
                        Decl::Error => {},
                    }
                },
                ProcessVariant::ModuleStart(name) => {
                    path.push(name);
                },
                ProcessVariant::ModuleEnd => {
                    path.pop();
                },
            };
        }

        eprintln!("[debug] Name resolution state:\n{:#?}", &self.names);
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
