use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::ast::forward::{ForwardClassDecl, ForwardConversionDecl, ForwardDecl, ForwardDeclaredClassImplementsPart, ForwardDeclaredDecl, ForwardDeclaredParameterPart, ForwardDeclareResult, ForwardDeclStorage, ForwardOptimizeDecl, ForwardTemplateDecl, ForwardTypeAliasDecl, ForwardVariableDecl};
use crate::compiler::ast::simple::{Decl, TemplateKind};
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::Token;
use crate::compiler::name::{self, SimpleTypeInfo, TypeStorage};
use crate::Config;
#[allow(unused)]
use crate::println_debug;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclError<'source> {
    TypeAlreadyDeclared(&'source str, Option<SpanWithFile>),
    DeclAlreadyDeclared(String, Option<SpanWithFile>), // the string has to contain the `` part if it's a name
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

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(SpanWithFile, Option<String>)> {
        match self {
            Self::TypeAlreadyDeclared(_, first) => if let Some(&span) = first.as_ref() {
                vec![(span, Some(String::from("first declared here")))]
            } else { vec![] },
            Self::DeclAlreadyDeclared(_, first) => if let Some(&span) = first.as_ref() {
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

impl ForwardDeclarer {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId) -> Self {
        ForwardDeclarer {
            _config: config, _path: path, file_id,
        }
    }

    pub fn forward_declare<'source>(self, declarations: Vec<Decl<'source>>, reporter: &mut DeclErrorReporter<'source>) -> ForwardDeclareResult<'source> {
        let mut types = self.declare_types(&declarations, reporter);

        let mut storage = ForwardDeclStorage::new(types.get_type_count());
        let declarations = self.declare_forward_decls(&mut storage, &mut types, declarations, reporter);

        storage.assert_complete_type_mappings();

        ForwardDeclareResult::new(types, storage, declarations)
    }

    fn declare_types<'source>(&self, decls: &[Decl<'source>], reporter: &mut DeclErrorReporter<'source>) -> TypeStorage {
        enum ProcessVariant<'decl, 'source> {
            Decl(&'decl Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd,
        }

        let mut storage = TypeStorage::new();
        let mut input_stack = decls.iter().map(ProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut path = Vec::new();

        while let Some(variant) = input_stack.pop() {
            match variant {
                ProcessVariant::Decl(decl) => {
                    match decl {
                        Decl::Module { name, declarations, key_span: _key_span } => {
                            input_stack.push(ProcessVariant::ModuleEnd);
                            input_stack.extend(declarations.iter().map(ProcessVariant::Decl).rev());
                            input_stack.push(ProcessVariant::ModuleStart(name.source()));
                        },
                        Decl::Class { interface, name, .. } => {
                            path.push(name.source());

                            if let Err(previous_id) = storage.insert(&path, name.span(), self.file_id, SimpleTypeInfo::Class { interface: *interface, }) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span), reporter);
                            }

                            path.pop();
                        },
                        Decl::TypeAlias { name, .. } => {
                            path.push(name.source());

                            if let Err(previous_id) = storage.insert(&path, name.span(), self.file_id, SimpleTypeInfo::TypeAlias) {
                                let previous_span = storage.get_span(previous_id);
                                self.error(name.span(), DeclError::TypeAlreadyDeclared(name.source(), previous_span), reporter);
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

    fn declare_forward_decls<'source>(&self, storage: &mut ForwardDeclStorage, types: &mut TypeStorage, decls: Vec<Decl<'source>>, reporter: &mut DeclErrorReporter<'source>) -> Vec<ForwardDeclaredDecl<'source>> {
        enum ProcessVariant<'source> {
            Decl(Decl<'source>),
            ModuleStart(&'source str),
            ModuleEnd {
                key_span: SpanWithFile,
                name: Token<'source>,
                decl_count: usize,
            },
        }

        let mut input_stack = decls.into_iter().map(ProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut output_stack = Vec::new();
        let mut path = Vec::new();

        while let Some(variant) = input_stack.pop() {
            match variant {
                ProcessVariant::Decl(decl) => match decl {
                    Decl::Module { key_span, name, declarations, } => {
                        input_stack.push(ProcessVariant::ModuleEnd { key_span, name: name.clone(), decl_count: declarations.len(), });
                        input_stack.extend(declarations.into_iter().map(ProcessVariant::Decl).rev());
                        input_stack.push(ProcessVariant::ModuleStart(name.source()));
                    },
                    Decl::Class { key_span, interface, name, parameters, implements, class_repr, parameter_span, } => {
                        let parameter_type_results = parameters.iter().map(|part| {
                            types.get_type_id_by_type_part(&path, &part.parameter_type, self.file_id)
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

                        let duplicate = if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], |name2, decl2| if let ForwardDecl::Template(template_decl) = decl2 {
                            template_decl.parameters == parameter_types
                        } else { false } && name.source() == name2) {
                            storage.mark_duplicate(previous_id);
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                            true
                        } else { false };

                        path.push(name.source());

                        let implements_id = implements.as_ref().map(|part| match types.get_type_id_by_type_reference_part(&path[0..path.len() - 1], &part.name) {
                            Ok(id) => id,
                            Err((source, span)) => {
                                self.error(span, DeclError::UnresolvedType(source), reporter);
                                name::ERROR_TYPE_ID
                            },
                        });

                        if duplicate {
                            output_stack.push(ForwardDeclaredDecl::Error);
                            path.pop();
                            continue;
                        }

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for interface decl can't be found");

                        let decl_id = storage.insert_with_type(&path, type_id, ForwardDecl::Class(ForwardClassDecl {
                            key_span,
                            type_id,
                            name_span: SpanWithFile::new(self.file_id, name.span()),
                            interface,
                            parameters: parameter_types.clone(),
                            implements: implements_id,
                            repr: class_repr.is_some(),
                        }), name.file_id(), name.span());

                        output_stack.push(ForwardDeclaredDecl::Class {
                            key_span, decl_id,
                            interface,
                            name,
                            parameters: parameters.into_iter().zip(parameter_types.into_iter()).map(|(part, id)| ForwardDeclaredParameterPart {
                                name: part.name,
                                parameter_type: id,
                                type_span: SpanWithFile::new(key_span.file_id(), part.parameter_type.span()),
                            }).collect(),
                            implements: implements.zip(implements_id).map(|(part, id)| ForwardDeclaredClassImplementsPart {
                                name: part.name,
                                implements_id: id,
                                parameters: part.parameters,
                                span: part.span,
                                parameter_span: part.parameter_span,
                            }),
                            class_repr,
                            parameter_span,
                        });
                        path.pop();
                    },
                    Decl::TypeAlias { key_span, name, to: to_part, condition } => {
                        let duplicate = if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            storage.mark_duplicate(previous_id);
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                            true
                        } else { false };

                        path.push(name.source());

                        let type_id = types.get_type_id_by_path(&path).expect("Internal compiler error: type for type alias decl can't be found");
                        let to = match types.get_type_id_by_type_part(&path[0..path.len() - 1], &to_part, self.file_id) {
                            Ok(id) => id,
                            Err((source, span)) => {
                                self.error(span, DeclError::UnresolvedType(source), reporter);
                                name::ERROR_TYPE_ID
                            },
                        };

                        if duplicate {
                            output_stack.push(ForwardDeclaredDecl::Error);
                            path.pop();
                            continue;
                        }

                        let decl_id = storage.insert_with_type(&path, type_id, ForwardDecl::TypeAlias(ForwardTypeAliasDecl {
                            key_span,
                            type_id,
                            name_span: SpanWithFile::new(self.file_id, name.span()),
                            reference: to,
                        }), name.file_id(), name.span());

                        output_stack.push(ForwardDeclaredDecl::TypeAlias {
                            key_span, decl_id,
                            name,
                            to,
                            condition,
                            to_span: SpanWithFile::new(key_span.file_id(), to_part.span()),
                        });
                        path.pop();
                    },
                    Decl::Template { key_span, kind, parameters, return_type: return_type_part, expr, parameter_span, expr_span } => {
                        let parameter_type_results = parameters.iter().map(|part| {
                            types.get_type_id_by_type_part(&path, &part.parameter_type, self.file_id)
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

                        let return_type = match types.get_type_id_by_type_part(&path, &return_type_part, self.file_id) {
                            Ok(id) => id,
                            Err((source, span)) => {
                                self.error(span, DeclError::UnresolvedType(source), reporter);
                                name::ERROR_TYPE_ID
                            },
                        };

                        let optimize_on = match &kind {
                            TemplateKind::Optimize { on } => Some(match types.get_type_id_by_type_part(&path, on, self.file_id) {
                                Ok(id) => id,
                                Err((source, span)) => {
                                    self.error(span, DeclError::UnresolvedType(source), reporter);
                                    name::ERROR_TYPE_ID
                                },
                            }),
                            _ => None,
                        };

                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], |decl_name, decl| match decl {
                            ForwardDecl::Template(decl) => matches!(&kind, TemplateKind::Template { .. })
                                && decl.parameters == parameter_types && decl.return_type == return_type,
                            ForwardDecl::Conversion(decl) => matches!(&kind, TemplateKind::Conversion { .. })
                                && parameter_types.len() == 1 && decl.from == parameter_types[0] && decl.to == return_type,
                            ForwardDecl::Optimize(decl) => if let Some(optimize_on) = optimize_on {
                                decl.on == optimize_on
                            } else { false },
                            ForwardDecl::Class(decl) => if let TemplateKind::Template { name, } = &kind {
                                decl_name == name.source() && decl.parameters == parameter_types
                            } else { false },
                            _ => false,
                        }) {
                            storage.mark_duplicate(previous_id);
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(kind.span(), DeclError::DeclAlreadyDeclared(kind.name_for_error().into_owned(), Some(previous_span)), reporter);
                            output_stack.push(ForwardDeclaredDecl::Error);
                        } else {
                            match kind {
                                TemplateKind::Template { name } => {
                                    path.push(name.source());
                                    let decl_id = storage.insert(&path, ForwardDecl::Template(ForwardTemplateDecl {
                                        key_span,
                                        name_span: SpanWithFile::new(self.file_id, name.span()),
                                        parameters: parameter_types.clone(),
                                        return_type,
                                    }), name.file_id(), name.span());

                                    output_stack.push(ForwardDeclaredDecl::Template {
                                        key_span, decl_id,
                                        kind: TemplateKind::Template { name, },
                                        parameters: parameters.into_iter().zip(parameter_types.into_iter()).map(|(part, id)| ForwardDeclaredParameterPart {
                                            name: part.name,
                                            parameter_type: id,
                                            type_span: SpanWithFile::new(key_span.file_id(), part.parameter_type.span()),
                                        }).collect(),
                                        return_type,
                                        expr,
                                        parameter_span,
                                        return_type_span: return_type_part.span(),
                                        expr_span,
                                    });
                                    path.pop();
                                },
                                TemplateKind::Conversion { span } => {
                                    path.push("<type conversion template>");

                                    if parameter_types.len() != 1 {
                                        self.error(parameter_span, DeclError::ConversionParameterCount(parameter_types.len()), reporter);
                                        output_stack.push(ForwardDeclaredDecl::Error);
                                    } else {
                                        let decl_id = storage.insert(&path, ForwardDecl::Conversion(ForwardConversionDecl {
                                            key_span,
                                            from: parameter_types[0],
                                            to: return_type,
                                        }), self.file_id, span);
                                        output_stack.push(ForwardDeclaredDecl::Template {
                                            key_span, decl_id,
                                            kind: TemplateKind::Conversion { span, },
                                            parameters: parameters.into_iter().zip(parameter_types.into_iter()).map(|(part, id)| ForwardDeclaredParameterPart {
                                                name: part.name,
                                                parameter_type: id,
                                                type_span: SpanWithFile::new(key_span.file_id(), part.parameter_type.span()),
                                            }).collect(),
                                            return_type,
                                            expr,
                                            parameter_span,
                                            return_type_span: return_type_part.span(),
                                            expr_span,
                                        });
                                    }

                                    path.pop();
                                },
                                TemplateKind::Optimize { on } => {
                                    path.push("<optimization template>");
                                    let decl_id = storage.insert(&path, ForwardDecl::Optimize(ForwardOptimizeDecl {
                                        key_span,
                                        on: optimize_on.expect("No type ID for optimization template target despite previous check"),
                                        parameters: parameter_types.clone(),
                                        return_type,
                                    }), self.file_id, on.span());
                                    output_stack.push(ForwardDeclaredDecl::Template {
                                        key_span, decl_id,
                                        kind: TemplateKind::Optimize { on, },
                                        parameters: parameters.into_iter().zip(parameter_types.into_iter()).map(|(part, id)| ForwardDeclaredParameterPart {
                                            name: part.name,
                                            parameter_type: id,
                                            type_span: SpanWithFile::new(key_span.file_id(), part.parameter_type.span()),
                                        }).collect(),
                                        return_type,
                                        expr,
                                        parameter_span,
                                        return_type_span: return_type_part.span(),
                                        expr_span,
                                    });
                                    path.pop();
                                },
                            }
                        }
                    },
                    Decl::Variable { key_span, kind, name, expr, span } => {
                        if let Some(previous_id) = storage.find_decl_id_for_duplicate(&path, &[], |name2, decl2| !matches!(decl2, ForwardDecl::Template(_)) && name.source() == name2) {
                            storage.mark_duplicate(previous_id);
                            let previous_span = storage.get_span_by_id(previous_id);
                            self.error(name.span(), DeclError::DeclAlreadyDeclared(format!("`{}`", name.source()), Some(previous_span)), reporter);
                        }

                        path.push(name.source());
                        let decl_id = storage.insert(&path, ForwardDecl::Variable(ForwardVariableDecl {
                            key_span,
                            name_span: SpanWithFile::new(self.file_id, name.span()),
                            kind,
                        }), name.file_id(), name.span());
                        output_stack.push(ForwardDeclaredDecl::Variable {
                            key_span, decl_id,
                            kind,
                            name,
                            expr,
                            span,
                        });
                        path.pop();
                    },
                    // These don't need forward declarations
                    Decl::Include { key_span, path } => output_stack.push(ForwardDeclaredDecl::Include { key_span, path, }),
                    Decl::Import { key_span, path, selector, span } => output_stack.push(ForwardDeclaredDecl::Import { key_span, path, selector, span, }),
                    Decl::Error => output_stack.push(ForwardDeclaredDecl::Error),
                },
                ProcessVariant::ModuleStart(name) => {
                    path.push(name);
                },
                ProcessVariant::ModuleEnd { key_span, name, decl_count, } => {
                    path.pop();

                    let declarations = if decl_count == 0 { Vec::new() } else {
                        output_stack.drain(output_stack.len() - decl_count .. output_stack.len())
                            .filter(|decl| *decl != ForwardDeclaredDecl::Error).collect()
                    };

                    output_stack.push(ForwardDeclaredDecl::Module {
                        key_span,
                        name,
                        declarations,
                    });
                },
            }
        }

        assert!(input_stack.is_empty(), "Input stack is not empty after forward declaring declarations: {}", input_stack.len());

        output_stack.into_iter().filter(|decl| *decl != ForwardDeclaredDecl::Error).collect()
    }

    fn error<'source>(&self, span: Span, message: DeclError<'source>, reporter: &mut DeclErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
