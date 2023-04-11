use std::collections::VecDeque;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::{ForwardClassDecl, ForwardDecl, ForwardDeclareResult, ForwardDeclStorage};
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{SimpleTypeInfo, TypeStorage};
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
    DeclAlreadyDeclared(&'source str, Option<Span>),
    UnresolvedType(String),
    ConversionParameterCount(usize),
}

impl<'source> Diagnostic<MessageMarker> for DeclError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::TypeAlreadyDeclared(_, _) => "decl/already_declared_type",
            Self::DeclAlreadyDeclared(_, _) => "decl/already_declared_decl",
            Self::UnresolvedType(_) => "decl/unresolved_type",
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
            Self::UnresolvedType(name) => format!("Unresolved type: `{}` not found", name),
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
            Self::UnresolvedType(name) => Some(format!("`{}` referenced here", name)),
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

    pub fn forward_declare(self, declarations: Vec<Decl<'source>>, reporter: &mut DeclErrorReporter<'source>) -> ForwardDeclareResult<'source> {
        let types = self.declare_types(&declarations, reporter);

        let mut storage = ForwardDeclStorage::new();
        self.declare_forward_decls(&mut storage, &types, &declarations, reporter);

        ForwardDeclareResult::new(types, storage, declarations)
    }

    fn declare_types(&self, decls: &[Decl<'source>], reporter: &mut DeclErrorReporter<'source>) -> TypeStorage {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut storage = TypeStorage::new();
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

                            if let Some(previous_id) = storage.get_type_id_by_path(&path) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                storage.insert(path.clone(), name.span(), self.file_id, SimpleTypeInfo::Class { interface: true, });
                            }

                            path.pop();
                        },
                        Decl::Class { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = storage.get_type_id_by_path(&path) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                storage.insert(path.clone(), name.span(), self.file_id, SimpleTypeInfo::Class { interface: false, });
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = storage.get_type_id_by_path(&path) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                storage.insert(path.clone(), name.span(), self.file_id, SimpleTypeInfo::TypeAlias);
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
        };

        storage
    }

    fn declare_forward_decls(&self, storage: &mut ForwardDeclStorage, types: &TypeStorage, decls: &[Decl<'source>], reporter: &mut DeclErrorReporter) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).collect::<VecDeque<_>>();
        let mut path = Vec::new();

        while let Some(variant) = to_process.pop_front() {
            match variant {
                ProcessVariant::Decl(decl) => match decl {
                    Decl::Module { name, declarations, } => {
                        to_process.push_back(ProcessVariant::ModuleStart(name.source()));
                        to_process.extend(declarations.iter().map(ProcessVariant::Decl));
                        to_process.push_back(ProcessVariant::ModuleEnd);
                    },
                    Decl::Interface { name, parameters, .. } => {
                        path.push(name.source());

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for interface decl can't be found");

                        let parameter_types = parameters.iter().map(|part| {
                            let id = types.get_type_id_by_type_reference_part(&path, part);
                        }).collect();

                        storage.insert(&path, ForwardDecl::Class(ForwardClassDecl {
                            type_id,
                            name_span: name.span(),
                            parameters: parameter_types,
                        }));

                        path.pop();
                    },
                    Decl::Class { .. } => {},
                    Decl::TypeAlias { .. } => {},
                    Decl::Template { .. } => {},
                    Decl::Include { .. } => {},
                    Decl::Import { .. } => {},
                    Decl::Variable { .. } => {},
                    Decl::Error => {},
                },
                ProcessVariant::ModuleStart(name) => {
                    path.push(name);
                },
                ProcessVariant::ModuleEnd => {
                    path.pop();
                },
            }
        }
    }

    fn error(&self, span: Span, message: DeclError<'source>, reporter: &mut DeclErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
