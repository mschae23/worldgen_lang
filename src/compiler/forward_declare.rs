use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::{ForwardClassDecl, ForwardConversionDecl, ForwardDecl, ForwardDeclareResult, ForwardDeclStorage, ForwardOptimizeDecl, ForwardTemplateDecl, ForwardTypeAliasDecl, ForwardVariableDecl};
use crate::compiler::ast::simple::{Decl, TemplateKind};
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{self, SimpleTypeInfo, TypeStorage};
use crate::Config;
#[allow(unused)]
use crate::println_debug;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
    DeclAlreadyDeclared(String, Option<Span>), // the string has to contain the `` part if it's a name
    UnresolvedType(&'source str),
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
            Self::TypeAlreadyDeclared(name, _) => format!("type `{}` was already declared", name),
            Self::DeclAlreadyDeclared(name, _) => format!("{} was already declared", name),
            Self::UnresolvedType(name) => format!("unresolved type: `{}` not found", name),
            Self::ConversionParameterCount(count) => if *count == 0 {
                String::from("type conversion template requires exactly one parameter")
            } else {
                format!("type conversion template has more than one parameter: {}", count)
            },
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::TypeAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
            Self::DeclAlreadyDeclared(name, _) => Some(format!("{} declared again here", name)),
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
    _path: Rc<PathBuf>, file_id: FileId,
}

impl<'source> ForwardDeclarer {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId) -> Self {
        ForwardDeclarer {
            _config: config, _path: path, file_id,
        }
    }

    pub fn forward_declare(self, declarations: Vec<Decl<'source>>, reporter: &mut DeclErrorReporter<'source>) -> ForwardDeclareResult<'source> {
        let mut types = self.declare_types(&declarations, reporter);

        let mut storage = ForwardDeclStorage::new();
        self.declare_forward_decls(&mut storage, &mut types, &declarations, reporter);

        ForwardDeclareResult::new(types, storage, declarations)
    }

    fn declare_types(&self, decls: &[Decl<'source>], reporter: &mut DeclErrorReporter<'source>) -> TypeStorage {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut storage = TypeStorage::new();
        let mut to_process = decls.iter().map(ProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut path = Vec::new();

        while let Some(variant) = to_process.pop() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations } => {
                            to_process.push(ProcessVariant::ModuleEnd);
                            to_process.extend(declarations.iter().map(ProcessVariant::Decl).rev());
                            to_process.push(ProcessVariant::ModuleStart(name.source()));
                        },
                        Decl::Interface { name, .. } => {
                            path.push(name.source());

                            if let Err(previous_id) = storage.insert(&path, name.span(), self.file_id, SimpleTypeInfo::Class { interface: true, }) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            }

                            path.pop();
                        },
                        Decl::Class { name, .. } => {
                            path.push(name.source());

                            if let Err(previous_id) = storage.insert(&path, name.span(), self.file_id, SimpleTypeInfo::Class { interface: false, }) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Err(previous_id) = storage.insert(&path, name.span(), self.file_id, SimpleTypeInfo::TypeAlias) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
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

    fn declare_forward_decls(&self, storage: &mut ForwardDeclStorage, types: &mut TypeStorage, decls: &[Decl<'source>], reporter: &mut DeclErrorReporter<'source>) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut path = Vec::new();

        while let Some(variant) = to_process.pop() {
            match variant {
                ProcessVariant::Decl(decl) => match decl {
                    Decl::Module { name, declarations, } => {
                        to_process.push(ProcessVariant::ModuleEnd);
                        to_process.extend(declarations.iter().map(ProcessVariant::Decl).rev());
                        to_process.push(ProcessVariant::ModuleStart(name.source()));
                    },
                    Decl::Interface { name, parameters, .. } => {
                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], false, |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                        }

                        path.push(name.source());

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for interface decl can't be found");

                        let parameter_type_results = parameters.iter().map(|part| {
                            Ok(types.get_type_id_by_type_part(&path[0..path.len() - 1], &part.parameter_type, self.file_id)?)
                        });

                        let mut parameter_types = Vec::with_capacity(parameter_type_results.len());

                        for result in parameter_type_results {
                            match result {
                                Ok(id) => parameter_types.push(id),
                                Err((source, span)) => {
                                    self.error(span, DeclError::UnresolvedType(source), reporter);
                                    parameter_types.push(name::ERROR_TYPE_ID);
                                },
                            }
                        }

                        storage.insert(&path, ForwardDecl::Class(ForwardClassDecl {
                            type_id,
                            name_span: name.span(),
                            interface: true,
                            parameters: parameter_types,
                        }), name.span());
                        path.pop();
                    },
                    Decl::Class { name, parameters, .. } => {
                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], false, |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                        }

                        path.push(name.source());

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for class decl can't be found");

                        let parameter_type_results = parameters.iter().map(|part| {
                            Ok(types.get_type_id_by_type_part(&path[0..path.len() - 1], &part.parameter_type, self.file_id)?)
                        });

                        let mut parameter_types = Vec::with_capacity(parameter_type_results.len());

                        for result in parameter_type_results {
                            match result {
                                Ok(id) => parameter_types.push(id),
                                Err((source, span)) => {
                                    self.error(span, DeclError::UnresolvedType(source), reporter);
                                    parameter_types.push(name::ERROR_TYPE_ID);
                                },
                            }
                        }

                        storage.insert(&path, ForwardDecl::Class(ForwardClassDecl {
                            type_id,
                            name_span: name.span(),
                            interface: false,
                            parameters: parameter_types,
                        }), name.span());
                        path.pop();
                    },
                    Decl::TypeAlias { name, to, .. } => {
                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], false, |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                        }

                        path.push(name.source());

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for type alias decl can't be found");
                        let to = match types.get_type_id_by_type_part(&path[0..path.len() - 1], to, self.file_id) {
                            Ok(id) => id,
                            Err((source, span)) => {
                                self.error(span, DeclError::UnresolvedType(source), reporter);
                                name::ERROR_TYPE_ID
                            },
                        };

                        storage.insert(&path, ForwardDecl::TypeAlias(ForwardTypeAliasDecl {
                            type_id,
                            reference: to,
                        }), name.span());
                        path.pop();
                    },
                    Decl::Template { kind, parameters, return_type, parameter_span, .. } => {
                        let parameter_type_results = parameters.iter().map(|part| {
                            Ok(types.get_type_id_by_type_part(&path, &part.parameter_type, self.file_id)?)
                        });

                        let mut parameter_types = Vec::with_capacity(parameter_type_results.len());

                        for result in parameter_type_results {
                            match result {
                                Ok(id) => parameter_types.push(id),
                                Err((source, span)) => {
                                    self.error(span, DeclError::UnresolvedType(source), reporter);
                                    parameter_types.push(name::ERROR_TYPE_ID);
                                },
                            }
                        }

                        let return_type = match types.get_type_id_by_type_part(&path, return_type, self.file_id) {
                            Ok(id) => id,
                            Err((source, span)) => {
                                self.error(span, DeclError::UnresolvedType(source), reporter);
                                name::ERROR_TYPE_ID
                            },
                        };

                        let optimize_on = match kind {
                            TemplateKind::Optimize { on } => Some(match types.get_type_id_by_type_part(&path, on, self.file_id) {
                                Ok(id) => id,
                                Err((source, span)) => {
                                    self.error(span, DeclError::UnresolvedType(source), reporter);
                                    name::ERROR_TYPE_ID
                                },
                            }),
                            _ => None,
                        };

                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], false, |_, decl| match decl {
                            ForwardDecl::Template(decl) => matches!(kind, TemplateKind::Template { .. })
                                && decl.parameters == parameter_types && decl.return_type == return_type,
                            ForwardDecl::Conversion(decl) => matches!(kind, TemplateKind::Conversion { .. })
                                && parameter_types.len() == 1 && &decl.from == &parameter_types[0] && decl.to == return_type,
                            ForwardDecl::Optimize(decl) => if let Some(optimize_on) = optimize_on {
                                decl.on == optimize_on
                            } else { false },
                            _ => false,
                        }) {
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(kind.span(), DeclError::DeclAlreadyDeclared(kind.name_for_error().into_owned(), Some(previous_span)), reporter);
                        } else {
                            match kind {
                                TemplateKind::Template { name } => {
                                    path.push(name.source());
                                    storage.insert(&path, ForwardDecl::Template(ForwardTemplateDecl {
                                        parameters: parameter_types,
                                        return_type,
                                    }), name.span());
                                    path.pop();
                                },
                                TemplateKind::Conversion { span } => {
                                    path.push("<type conversion template>");

                                    if parameter_types.len() != 1 {
                                        self.error(*parameter_span, DeclError::ConversionParameterCount(parameter_types.len()), reporter);
                                    } else {
                                        storage.insert(&path, ForwardDecl::Conversion(ForwardConversionDecl {
                                            from: parameter_types[0],
                                            to: return_type,
                                        }), *span);
                                    }

                                    path.pop();
                                },
                                TemplateKind::Optimize { on } => {
                                    path.push("<optimization template>");
                                    storage.insert(&path, ForwardDecl::Optimize(ForwardOptimizeDecl {
                                        on: optimize_on.expect("No type ID for optimization template target despite previous check"),
                                        parameters: parameter_types,
                                        return_type,
                                    }), on.span());
                                    path.pop();
                                },
                            }
                        }
                    },
                    Decl::Variable { kind, name, .. } => {
                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], false, |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                        }

                        path.push(name.source());
                        storage.insert(&path, ForwardDecl::Variable(ForwardVariableDecl {
                            kind: *kind,
                        }), name.span());
                        path.pop();
                    },
                    // These don't need forward declarations
                    Decl::Include { .. } => {},
                    Decl::Import { .. } => {},
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
