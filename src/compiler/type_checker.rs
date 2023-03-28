use std::collections::{HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::Decl;
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{NameResolution, PositionedName, SimpleClassDecl, SimpleDecl, SimpleModuleDecl, SimpleTypeAliasDecl, SimpleTypeInfo};
use crate::Config;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<'source> {
    TypeAlreadyDeclared(&'source str, Option<Span>),
    DeclAlreadyDeclared(&'source str, Option<Span>),
    TypeNotFound(String),
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::TypeAlreadyDeclared(_, _) => "type/already_declared_type",
            Self::DeclAlreadyDeclared(_, _) => "type/already_declared_decl",
            Self::TypeNotFound(_) => "type/not_found",
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
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            TypeError::TypeAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
            TypeError::DeclAlreadyDeclared(name, _) => Some(format!("`{}` declared again here", name)),
            TypeError::TypeNotFound(name) => Some(format!("`{}` referenced here", name)),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
            TypeError::TypeAlreadyDeclared(_, span) => if let Some(&span) = span.as_ref() {
                vec![(span, Some(String::from("first declared here")))]
            } else { vec![] },
            TypeError::DeclAlreadyDeclared(_, span) => if let Some(&span) = span.as_ref() {
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
        self.declare_declarations(&declarations, reporter);

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

                            if let Some(previous_id) = self.names.get_simple_types().get_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_types().get_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.get_simple_types_mut().insert(path.clone(), name.span(), /* TODO */ 0, SimpleTypeInfo::Class { interface: true, });
                            }

                            path.pop();
                        },
                        Decl::Class { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = self.names.get_simple_types().get_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_types().get_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.get_simple_types_mut().insert(path.clone(), name.span(), 0, SimpleTypeInfo::Class { interface: false, });
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Some(previous_id) = self.names.get_simple_types().get_type_id_by_path(&path) {
                                let previous_span = self.names.get_simple_types().get_span(previous_id);
                                self.error(name.span(), TypeError::TypeAlreadyDeclared(name.source(), previous_span.map(|(span, _)| span)), reporter);
                            } else {
                                self.names.get_simple_types_mut().insert(path.clone(), name.span(), 0, SimpleTypeInfo::TypeAlias);
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
    }

    fn declare_declarations(&mut self, decls: &[Decl<'source>], reporter: &mut TypeErrorReporter<'source>) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str, FileId, Span),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).collect::<VecDeque<_>>();
        let mut decl_stack = vec![SimpleModuleDecl {
            name: PositionedName::new(String::from("<global>"), 0, Span::new(0, 0)),
            declarations: HashMap::new(),
        }];
        let mut path = PathBuf::new();

        while let Some(variant) = to_process.pop_front() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations } => {
                            to_process.push_back(ProcessVariant::ModuleStart(name.source(), /* TODO */ 0, name.span()));
                            to_process.extend(declarations.iter().map(ProcessVariant::Decl));
                            to_process.push_back(ProcessVariant::ModuleEnd);
                        },
                        Decl::Interface { name, parameters, .. } => {
                            path.push(name.source());

                            if let Some(top_module) = decl_stack.last_mut() {
                                match top_module.declarations.entry(name.source().to_owned()) {
                                    Entry::Occupied(entry) => {
                                        self.error(name.span(), TypeError::DeclAlreadyDeclared(name.source(), Some(entry.get().span())), reporter);
                                    },
                                    Entry::Vacant(entry) => {
                                        let parameters = parameters.iter().map(|part| {
                                            let type_id = match self.names.get_simple_types_mut().get_type_id_by_type_part(path.parent().expect("Path does not have a parent despite previous push"), &part.parameter_type) {
                                                None => {
                                                    self.error(part.parameter_type.span(), TypeError::TypeNotFound(format!("{:?}", &part.parameter_type)), reporter);
                                                    0
                                                },
                                                Some(id) => id,
                                            };

                                            (PositionedName::new(part.name.source().to_owned(), /* TODO */ 0, part.name.span()), type_id)
                                        }).collect();

                                        entry.insert(SimpleDecl::Class(SimpleClassDecl {
                                            name: PositionedName::new(name.source().to_owned(), /* TODO */ 0, name.span()),
                                            type_id: self.names.get_simple_types().get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
                                            interface: true,
                                            parameters,
                                        }));
                                    },
                                };
                            }

                            path.pop();
                        },
                        Decl::Class { name, parameters, .. } => {
                            path.push(name.source());

                            if let Some(top_module) = decl_stack.last_mut() {
                                match top_module.declarations.entry(name.source().to_owned()) {
                                    Entry::Occupied(entry) => {
                                        self.error(name.span(), TypeError::DeclAlreadyDeclared(name.source(), Some(entry.get().span())), reporter);
                                    },
                                    Entry::Vacant(entry) => {
                                        let parameters = parameters.iter().map(|part| {
                                            let type_id = match self.names.get_simple_types_mut().get_type_id_by_type_part(path.parent().expect("Path does not have a parent despite previous push"), &part.parameter_type) {
                                                None => {
                                                    self.error(part.parameter_type.span(), TypeError::TypeNotFound(format!("{:?}", &part.parameter_type)), reporter);
                                                    0
                                                },
                                                Some(id) => id,
                                            };

                                            (PositionedName::new(part.name.source().to_owned(), /* TODO */ 0, part.name.span()), type_id)
                                        }).collect();

                                        entry.insert(SimpleDecl::Class(SimpleClassDecl {
                                            name: PositionedName::new(name.source().to_owned(), /* TODO */ 0, name.span()),
                                            type_id: self.names.get_simple_types().get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
                                            interface: false,
                                            parameters,
                                        }));
                                    },
                                };
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Some(top_module) = decl_stack.last_mut() {
                                match top_module.declarations.entry(name.source().to_owned()) {
                                    Entry::Occupied(entry) => {
                                        self.error(name.span(), TypeError::DeclAlreadyDeclared(name.source(), Some(entry.get().span())), reporter);
                                    },
                                    Entry::Vacant(entry) => {
                                        entry.insert(SimpleDecl::TypeAlias(SimpleTypeAliasDecl {
                                            name: PositionedName::new(name.source().to_owned(), /* TODO */ 0, name.span()),
                                            type_id: self.names.get_simple_types().get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
                                        }));
                                    },
                                };
                            }

                            path.pop();
                        },
                        // TODO
                        Decl::Template { .. } => {},
                        Decl::Variable { .. } => {},
                        // These don't need forward declarations
                        Decl::Import { .. } => {},
                        Decl::Include { .. } => {},
                        Decl::Error => {},
                    }
                },
                ProcessVariant::ModuleStart(name, file_id, span) => {
                    path.push(name);
                    decl_stack.push(SimpleModuleDecl {
                        name: PositionedName::new(name.to_owned(), file_id, span),
                        declarations: HashMap::new(),
                    })
                },
                ProcessVariant::ModuleEnd => {
                    path.pop();

                    let previous_len = decl_stack.len();
                    let top = decl_stack.pop().expect("Compiler bug: Last module does not exist");

                    if let Some(next_top_module) = decl_stack.get_mut(previous_len - 2) {
                        next_top_module.declarations.insert(top.name.source.clone(), SimpleDecl::Module(top));
                    }
                },
            };
        }

        assert_eq!(decl_stack.len(), 1);

        for (_, decl) in decl_stack.pop().expect("No module in decl stack despite previous check").declarations {
            self.names.get_simple_decls_mut().insert(decl);
        }

        eprintln!("[debug] Name resolution state:\n{:#?}", &self.names);
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
