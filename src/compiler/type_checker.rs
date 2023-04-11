use std::collections::{HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::simple::{Decl, TemplateKind};
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::name::{NameResolution, PositionedName, SimpleClassDecl, SimpleDecl, SimpleModuleDecl, SimpleTypeAliasDecl, SimpleUnnamedTemplateData, SimpleUnnamedTemplateDecl, TypeStorage};
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

pub type TypeErrorReporter<'source> = ErrorReporter<MessageMarker, TypeError<'source>>;

pub struct TypeChecker {
    _config: Rc<Config>,
    path: Rc<PathBuf>, file_id: FileId,

    types: TypeStorage,
    names: NameResolution,
}

impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId, type_storage: TypeStorage) -> Self {
        TypeChecker {
            _config: config, path, file_id,
            types: type_storage, names: NameResolution::new(),
        }
    }

    pub fn check_types(&mut self, declarations: Vec<Decl<'source>>, reporter: &mut TypeErrorReporter<'source>) -> Vec<TypedDecl<'source>> {
        self.declare_declarations(&declarations, reporter);

        eprintln!("[debug] Name resolution state:\n{:#?}", &self.names);

        let /* mut */ typed_declarations = Vec::new();

        for _decl in declarations {
            // typed_declarations.push(self.check_declaration(decl));
        }

        typed_declarations
    }

    fn declare_declarations(&mut self, decls: &[Decl<'source>], reporter: &mut TypeErrorReporter<'source>) {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str, FileId, Span),
            ModuleEnd,
        }

        let mut to_process = decls.iter().map(ProcessVariant::Decl).collect::<VecDeque<_>>();
        let mut decl_stack = vec![SimpleModuleDecl {
            name: PositionedName::new(String::from("<global>"), self.file_id, Span::new(0, 0)),
            declarations: HashMap::new(),
            unnamed_templates: Vec::new(),
        }];
        let mut path = PathBuf::new();

        while let Some(variant) = to_process.pop_front() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations } => {
                            to_process.push_back(ProcessVariant::ModuleStart(name.source(), self.file_id, name.span()));
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
                                            let type_id = match self.types.get_type_id_by_type_part(path.parent().expect("Path does not have a parent despite previous push"), &part.parameter_type) {
                                                None => {
                                                    self.error(part.parameter_type.span(), TypeError::TypeNotFound(format!("{:?}", &part.parameter_type)), reporter);
                                                    0
                                                },
                                                Some(id) => id,
                                            };

                                            (PositionedName::new(part.name.source().to_owned(), self.file_id, part.name.span()), type_id)
                                        }).collect();

                                        entry.insert(SimpleDecl::Class(SimpleClassDecl {
                                            name: PositionedName::new(name.source().to_owned(), self.file_id, name.span()),
                                            type_id: self.types.get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
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
                                            let type_id = match self.types.get_type_id_by_type_part(path.parent().expect("Path does not have a parent despite previous push"), &part.parameter_type) {
                                                None => {
                                                    self.error(part.parameter_type.span(), TypeError::TypeNotFound(format!("{:?}", &part.parameter_type)), reporter);
                                                    0
                                                },
                                                Some(id) => id,
                                            };

                                            (PositionedName::new(part.name.source().to_owned(), self.file_id, part.name.span()), type_id)
                                        }).collect();

                                        entry.insert(SimpleDecl::Class(SimpleClassDecl {
                                            name: PositionedName::new(name.source().to_owned(), self.file_id, name.span()),
                                            type_id: self.types.get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
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
                                            name: PositionedName::new(name.source().to_owned(), self.file_id, name.span()),
                                            type_id: self.types.get_type_id_by_path(&path).expect("Bug in compiler: Type for declaration does not exist"),
                                        }));
                                    },
                                };
                            }

                            path.pop();
                        },
                        Decl::Template { kind, parameters, return_type, parameter_span, .. } => {
                            match kind {
                                TemplateKind::Template { name } => {
                                    path.push(name.source());

                                    if let Some(top_module) = decl_stack.last_mut() {
                                        match top_module.declarations.entry(name.source().to_owned()) {
                                            Entry::Occupied(entry) => {
                                                self.error(name.span(), TypeError::DeclAlreadyDeclared(name.source(), Some(entry.get().span())), reporter);
                                            },
                                            Entry::Vacant(_entry) => {
                                                // TODO
                                                // entry.insert(SimpleDecl::Template);
                                            },
                                        };
                                    }

                                    path.pop();
                                },
                                TemplateKind::Conversion { span } => {
                                    if parameters.len() != 1 {
                                        self.error(*parameter_span, TypeError::ConversionParameterCount(parameters.len()), reporter);
                                    } else {
                                        let from = &parameters[0];

                                        if let Some(top_module) = decl_stack.last_mut() {
                                            top_module.unnamed_templates.push(SimpleUnnamedTemplateDecl {
                                                name_span: *span,
                                                data: SimpleUnnamedTemplateData::Conversion {
                                                },
                                            });
                                        }
                                    }
                                },
                                TemplateKind::Optimize { on } => {
                                    if let Some(top_module) = decl_stack.last_mut() {
                                        top_module.unnamed_templates.push(SimpleUnnamedTemplateDecl {
                                            name_span: on.span(),
                                            data: SimpleUnnamedTemplateData::Optimize {
                                            },
                                        });
                                    }
                                },
                            }
                        },
                        // TODO
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
                        unnamed_templates: Vec::new(),
                    })
                },
                ProcessVariant::ModuleEnd => {
                    path.pop();

                    let previous_len = decl_stack.len();
                    let top = decl_stack.pop().expect("Compiler bug: Last module does not exist");

                    if let Some(next_top_module) = decl_stack.get_mut(previous_len - 2) {
                        // TODO Fix multiple module decls with the same name being allowed, but overriding the last one
                        next_top_module.declarations.insert(top.name.source.clone(), SimpleDecl::Module(top));
                    }
                },
            };
        }

        assert_eq!(decl_stack.len(), 1);

        for (_, decl) in decl_stack.pop().expect("No module in decl stack despite previous check").declarations {
            self.names.get_simple_decls_mut().insert(decl);
        }
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
