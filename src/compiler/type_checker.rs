use std::borrow::Cow;
use std::path::PathBuf;
use std::rc::Rc;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::forward::{DeclId, ForwardDecl, ForwardDeclaredClassImplementsPart, ForwardDeclaredDecl, ForwardDeclaredParameterPart, ForwardDeclStorage};
use crate::compiler::ast::simple::{ClassReprPart, Expr, TemplateExpr, TemplateKind, VariableKind};
use crate::compiler::ast::typed::{OwnedToken, TypedClassImplementsPart, TypedClassReprPart, TypedDecl, TypedDefinedClassReprPart, TypedExpr, TypedParameterPart, TypedTemplateDecl, TypedTemplateExpr};
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, ErrorReporting, FileId, NoteKind, Severity};
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::Token;
use crate::compiler::name;
use crate::compiler::name::{NameResolution, TypeId, TypeStorage};
use crate::compiler::pipeline::CompileState;
use crate::compiler::type_checker::hint::TypeHint;
use crate::Config;
#[allow(unused)]
use crate::println_debug;

pub mod hint;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<'source> {
    ImplementsNotClass {
        name: &'source str,
        actual_kind: &'source str,
        defined_at: SpanWithFile,
    },
    ClassMissingDef {
        clause: &'static str,
        name: &'source str,
    },
    MismatchedTypes {
        expected: Cow<'source, str>,
        found: Cow<'source, str>,
        message: Option<Cow<'source, str>>,
        additional_annotation: Option<(SpanWithFile, Option<Cow<'source, str>>)>,
    },
    RepeatedInclude {
        path: String,
        previous_span: Option<SpanWithFile>,
    },
    DeclAlreadyDeclared(String, Option<SpanWithFile>), // the string has to contain the `` part if it's a name (Same error as in ForwardDeclarer)
    Io {
        message: &'source str,
        io_message: String,
    },
    Unimplemented(&'source str),
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::ImplementsNotClass { .. } => "type/implements_not_class",
            Self::ClassMissingDef { .. } => "type/class_missing_def",
            Self::MismatchedTypes { .. } => "type/mismatched_types",
            Self::RepeatedInclude { .. } => "type/repeated_include",
            Self::DeclAlreadyDeclared(_, _) => "type/already_declared_decl",
            Self::Io { .. } => "type/io",
            Self::Unimplemented(_) => "type/unimplemented",
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
            Self::ImplementsNotClass { name, actual_kind, .. } => format!("expected class or interface, but `{}` is {}", name, actual_kind),
            Self::ClassMissingDef { clause, name } => format!("missing {} definition for class `{}`", clause, name),
            Self::MismatchedTypes { expected, found, .. } => format!("mismatched types: expected {}, found {}", expected, found),
            Self::RepeatedInclude { path, .. } => format!("tried to include file twice: {}", path),
            Self::DeclAlreadyDeclared(name, _) => format!("{} was already declared", name),
            Self::Io { message, io_message } => format!("{} (IO error: \"{}\")", message, io_message),
            Self::Unimplemented(msg) => format!("not implemented yet: {}", msg),
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::ImplementsNotClass { name, .. } => Some(format!("`{}` referenced here", name)),
            Self::ClassMissingDef { name, .. } => Some(format!("`{}` is declared here", name)),
            Self::MismatchedTypes { message, .. } => message.as_ref().map(|cow| cow.clone().into_owned()),
            Self::RepeatedInclude { .. } => Some(String::from("file included again here")),
            Self::DeclAlreadyDeclared(name, _) => Some(format!("{} declared again here", name)),
            Self::Io { .. } => Some(String::from("IO error occurred here")),
            Self::Unimplemented(_) => Some(String::from("unimplemented feature used here")),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(SpanWithFile, Option<String>)> {
        match self {
            Self::ImplementsNotClass { name, defined_at, .. } => vec![(*defined_at, Some(format!("`{}` originally defined here", name)))],
            Self::MismatchedTypes { additional_annotation, .. } => additional_annotation.as_ref().map(|&(span, ref label)| (span, label.as_ref()
                .map(|label| label.clone().into_owned())))
                .into_iter().collect(),
            Self::RepeatedInclude { previous_span, .. } => previous_span.as_ref()
                .map(|&previous_span| (previous_span, Some(String::from("file included here before"))))
                .into_iter().collect(),
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

pub type TypeErrorReporter<'source> = ErrorReporter<MessageMarker, TypeError<'source>>;

enum DeclProcessVariant<'source> {
    Decl(ForwardDeclaredDecl<'source>),
    ModuleStart(&'source str),
    ModuleEnd,
}

enum ExprProcessVariant<'source> {
    Expr(Expr<'source>),
    Object { merge_expr: Option<Span>, fields: Vec<Token<'source>>, span: Span, },
    Array { elements: usize, span: Span, },
}

pub struct TypeChecker<'reporting> {
    config: Rc<Config>, reporting: &'reporting mut ErrorReporting,
    pub paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, own_path_index: usize, file_id: FileId,
    type_id_offset: usize, decl_id_offset: usize,

    type_count: usize,
    pub names: NameResolution,
}

#[allow(clippy::too_many_arguments)]
impl<'reporting, 'source> TypeChecker<'reporting> {
    // path should already be canonicalized
    pub fn new(config: Rc<Config>, paths: Vec<(Rc<PathBuf>, Option<SpanWithFile>)>, file_id: FileId, type_id_offset: usize, decl_id_offset: usize, types: TypeStorage, forward_decls: ForwardDeclStorage, reporting: &'reporting mut ErrorReporting) -> Self {
        let own_path_index = paths.len() - 1;

        TypeChecker {
            config, reporting, paths, own_path_index, file_id,
            type_id_offset, decl_id_offset,
            type_count: types.get_type_count(),
            names: NameResolution::new(types, forward_decls),
        }
    }

    pub fn check_types(&mut self, declarations: Vec<ForwardDeclaredDecl<'source>>, reporter: &mut TypeErrorReporter<'source>) {
        println_debug!("Name resolution state: {:#?}", &self.names.forward_decls);

        let mut process_stack = declarations.into_iter()
            .map(DeclProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut module_path = Vec::new();

        while let Some(process) = process_stack.pop() {
            match process {
                DeclProcessVariant::Decl(decl) => match decl {
                    ForwardDeclaredDecl::Module { name, declarations, key_span: _key_span } => {
                        process_stack.push(DeclProcessVariant::ModuleEnd);
                        process_stack.extend(declarations.into_iter()
                            .map(DeclProcessVariant::Decl).rev());
                        process_stack.push(DeclProcessVariant::ModuleStart(name.source()));
                    },
                    ForwardDeclaredDecl::Class { key_span, decl_id, interface, name, parameters, implements, class_repr, parameter_span } =>
                        self.check_class_decl(key_span, decl_id, interface, name, parameters, implements, class_repr, parameter_span, &module_path, reporter),
                    ForwardDeclaredDecl::TypeAlias { key_span, decl_id, name, to, condition, to_span } =>
                        self.check_type_alias_decl(key_span, decl_id, name, to, condition, to_span, &module_path, reporter),
                    ForwardDeclaredDecl::Template { key_span, decl_id, kind, parameters, return_type, expr, parameter_span, return_type_span, expr_span } =>
                        self.check_template_decl(key_span, decl_id, kind, parameters, return_type, expr, parameter_span, return_type_span, expr_span, &module_path, reporter),
                    ForwardDeclaredDecl::Include { key_span, path } =>
                        self.check_include(key_span, path, reporter),
                    ForwardDeclaredDecl::Import { key_span, path, selector, span } =>
                        self.check_import(key_span, path, selector, span, reporter),
                    ForwardDeclaredDecl::Variable { key_span, decl_id, kind, name, expr, span } =>
                        self.check_variable(key_span, decl_id, kind, name, expr, span, reporter),
                    ForwardDeclaredDecl::Error => {},
                },
                DeclProcessVariant::ModuleStart(name) => {
                    self.names.open_type_environment(name.to_owned());
                    module_path.push(name);
                },
                DeclProcessVariant::ModuleEnd => {
                    self.names.close_type_environment();
                    module_path.pop();
                },
            }
        }
    }

    fn check_class_decl(&mut self, _key_span: SpanWithFile, decl_id: DeclId, interface: bool, name: Token<'source>, parameters: Vec<ForwardDeclaredParameterPart<'source>>, implements: Option<ForwardDeclaredClassImplementsPart<'source>>, class_repr: Option<ClassReprPart<'source>>, parameter_span: Span, _module_path: &[&'source str], reporter: &mut TypeErrorReporter<'source>) {
        if self.names.forward_decls.has_duplicate(decl_id) {
            return;
        }

        let forward_decl = self.names.forward_decls.get_decl_by_id(decl_id);

        match forward_decl {
            ForwardDecl::Class(decl) => {
                let typed_parameters = parameters.into_iter().enumerate().map(|(i, part)| TypedParameterPart {
                    name: OwnedToken::from_token(&part.name),
                    parameter_type: decl.parameters[i],
                }).collect::<Vec<_>>();

                let typed_implements = implements.map(|implements| {
                    let parameter_types = self.names.forward_decls.get_decl_by_type_id(implements.implements_id).and_then(|referenced_decl| {
                        match referenced_decl {
                            ForwardDecl::Class(decl) => Some(decl.parameters.clone()),
                            _ => {
                                self.error(implements.span, TypeError::ImplementsNotClass { name: implements.name.0.last().source(),
                                    actual_kind: referenced_decl.kind_with_indefinite_article(),
                                    defined_at: referenced_decl.name_span(),
                                }, reporter);
                                None
                            }
                        }
                    });

                    TypedClassImplementsPart {
                        reference: implements.implements_id,
                        parameters: implements.parameters.into_iter().enumerate()
                            .map(|(i, parameter)| self.check_expr(parameter,
                                parameter_types.as_ref().map(|types| TypeHint::Options(NonEmpty::new(types[i]))).unwrap_or(TypeHint::Any),
                                    reporter)).collect(),
                        span: implements.span,
                        parameter_span: implements.parameter_span,
                        file_id: self.file_id,
                    }
                });

                let typed_class_repr = class_repr.map(|part| TypedClassReprPart::Defined(TypedDefinedClassReprPart {
                    expr: self.check_expr(part.expr, TypeHint::Any, reporter),
                    span: part.span,
                    file_id: self.file_id,
                })).unwrap_or_else(|| {
                    let parent = typed_implements.as_ref().and_then(|implements| self.names.forward_decls.get_decl_id_from_type_id(implements.reference));
                    let mut parent = parent.and_then(|parent| match self.names.forward_decls.get_decl_by_id(parent) {
                        ForwardDecl::Class(decl) => Some((parent, decl)),
                        _ => None,
                    });

                    while let Some((parent_id, decl)) = parent {
                        if decl.repr {
                            return TypedClassReprPart::Inherited(parent_id);
                        } else {
                            parent = parent.and_then(|(_, parent)| parent.implements
                                .and_then(|parent| self.names.forward_decls.get_decl_id_from_type_id(parent))
                                .and_then(|parent| match self.names.forward_decls.get_decl_by_id(parent) {
                                ForwardDecl::Class(decl) => Some((parent, decl)),
                                _ => None,
                            }));
                        }
                    }

                    TypedClassReprPart::None
                });

                if !interface {
                    // Classes need an implements clause and a repr

                    if typed_implements.is_none() {
                        self.error(name.span(), TypeError::ClassMissingDef { clause: "implements", name: name.source(), }, reporter);
                    }

                    if typed_class_repr == TypedClassReprPart::None {
                        self.error(name.span(), TypeError::ClassMissingDef { clause: "representation", name: name.source(), }, reporter);
                    }
                }

                self.names.insert_declaration(name.source().to_owned(), TypedDecl::Class {
                    name: OwnedToken::from_token(&name),
                    parameters: typed_parameters,
                    implements: typed_implements,
                    class_repr: typed_class_repr,
                    parameter_span,
                });
            },
            _ => panic!("Internal compiler error: forward decl for `{}` is not a class", name.source()),
        }
    }

    fn check_type_alias_decl(&mut self, key_span: SpanWithFile, decl_id: DeclId, name: Token<'source>, to: TypeId, condition: Option<(Expr<'source>, Span)>, _to_span: SpanWithFile, _module_path: &[&'source str], reporter: &mut TypeErrorReporter<'source>) {
        if self.names.forward_decls.has_duplicate(decl_id) {
            return;
        }

        let forward_decl = self.names.forward_decls.get_decl_by_id(decl_id);

        match forward_decl {
            ForwardDecl::TypeAlias(_decl) => {
                let typed_condition = condition.map(|(condition, condition_span)| {
                    let typed = self.check_expr(condition, TypeHint::new(name::BOOLEAN_TYPE_ID), reporter);

                    match typed.convert_to(&TypeHint::new(name::BOOLEAN_TYPE_ID), &self.names) {
                        Ok(typed) => typed,
                        Err(typed) => {
                            self.error(condition_span, TypeError::MismatchedTypes {
                                expected: Cow::Borrowed("boolean"),
                                found: Cow::Owned(self.names.type_name(typed.type_id()).to_owned()),
                                message: Some(Cow::Borrowed("type alias condition must be of type boolean")),
                                additional_annotation: Some((key_span, None)),
                            }, reporter);
                            typed
                        },
                    }
                });

                self.names.insert_declaration(name.source().to_owned(), TypedDecl::TypeAlias {
                    name: OwnedToken::from_token(&name),
                    to,
                    condition: typed_condition,
                });
            },
            _ => panic!("Internal compiler error: forward decl for `{}` is not a type alias", name.source()),
        }
    }

    fn check_template_decl(&mut self, key_span: SpanWithFile, decl_id: DeclId, kind: TemplateKind<'source>, parameters: Vec<ForwardDeclaredParameterPart<'source>>, return_type: TypeId, expr: TemplateExpr<'source>, parameter_span: Span, return_type_span: Span, expr_span: Span, _module_path: &[&'source str], reporter: &mut TypeErrorReporter<'source>) {
        if self.names.forward_decls.has_duplicate(decl_id) {
            return;
        }

        let forward_decl = self.names.forward_decls.get_decl_by_id(decl_id);

        match kind {
            TemplateKind::Template { name, .. } => match forward_decl {
                ForwardDecl::Template(decl) => {
                    let typed_parameters = decl.parameters.iter().enumerate().map(|(i, id)| TypedParameterPart {
                        name: OwnedToken::from_token(&parameters[i].name), parameter_type: *id }).collect();
                    let typed_expr = self.check_template_expr(expr, TypeHint::new(return_type), reporter);

                    match typed_expr.convert_to(&TypeHint::new(return_type), &self.names) {
                        Ok(typed_expr) => self.names.insert_template_declaration(TypedTemplateDecl {
                            name: OwnedToken::from_token(&name),
                            parameters: typed_parameters,
                            return_type,
                            expr: typed_expr,
                            parameter_span,
                        }),
                        Err(typed_expr) => self.error(expr_span, TypeError::MismatchedTypes {
                            expected: Cow::Owned(self.names.type_name(return_type).to_owned()),
                            found: Cow::Owned(self.names.type_name(typed_expr.type_id()).to_owned()),
                            message: Some(Cow::Borrowed("template expression must have the template's return type")),
                            additional_annotation: Some((SpanWithFile::new(self.file_id, return_type_span), Some(Cow::Borrowed("return type defined here")))),
                        }, reporter),
                    }
                },
                _ => panic!("Internal compiler error: forward decl for `{}` is not a template", name.source()),
            },
            TemplateKind::Conversion { .. } => self.unimplemented(key_span.span(), "check type conversion decl", reporter),
            TemplateKind::Optimize { .. } => self.unimplemented(key_span.span(), "check optimization decl", reporter),
        }
    }

    fn check_include(&mut self, _key_span: SpanWithFile, path_token: Token<'source>, reporter: &mut TypeErrorReporter<'source>) {
        let path = self.paths[self.own_path_index].0.parent().expect("path has no parent, even though it is definitely the path of a file").join(path_token.source()).canonicalize();
        let path = match path {
            Ok(path) => Rc::new(path),
            Err(err) => {
                self.error(path_token.span(), TypeError::Io { message: "failed to canonicalize path", io_message: format!("{}", err), }, reporter);
                return;
            },
        };

        if let Some((_, previous_span)) = self.paths.iter().rev().find(|(previous, _)| *previous == path) {
            let formatted_path = path.strip_prefix(&**reporter.working_dir()).unwrap_or(&**path).to_string_lossy().into_owned();
            self.error(path_token.span(), TypeError::RepeatedInclude { path: formatted_path, previous_span: *previous_span, }, reporter);

            // Still push the path, for more useful error messages in case it is included yet another time
            self.paths.push((path, Some(SpanWithFile::new(path_token.file_id(), path_token.span()))));
            return;
        }

        self.paths.push((Rc::clone(&path), Some(SpanWithFile::new(path_token.file_id(), path_token.span()))));

        let file = std::fs::read_to_string(&**path);
        let file = match file {
            Ok(file) => file,
            Err(err) => {
                self.error(path_token.span(), TypeError::Io { message: "failed to read file", io_message: format!("{}", err), }, reporter);
                return;
            },
        };
        let file_id = self.reporting.get_file_id(Rc::clone(&path), file.clone());

        let paths = std::mem::take(&mut self.paths);
        let type_checked = CompileState::new(Rc::clone(&self.config), &file, paths, file_id,
            self.type_id_offset + self.type_count - name::PRIMITIVE_TYPE_COUNT, self.decl_id_offset + self.names.forward_decls.get_decl_count())
            .tokenize()
            .parse(self.reporting)
            .forward_declare(self.reporting)
            .check_types(self.reporting);

        self.paths = type_checked.paths;
        let names = type_checked.names;
        println_debug!("Included name resolution: {:#?}", &names);

        // TODO Check that these names don't already exist in this environment

        self.names.include(names, reporter);
    }

    #[allow(unused_variables)]
    fn check_import(&mut self, key_span: SpanWithFile, path: NonEmpty<Token<'source>>, selector: Option<NonEmpty<Token<'source>>>, span: Span, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check import decl", reporter);
    }

    #[allow(unused_variables)]
    fn check_variable(&mut self, key_span: SpanWithFile, decl_id: DeclId, kind: VariableKind, name: Token<'source>, expr: Expr<'source>, span: Span, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check variable decl", reporter);
    }

    fn check_template_expr(&mut self, expr: TemplateExpr<'source>, type_hint: TypeHint, reporter: &mut TypeErrorReporter) -> TypedTemplateExpr {
        // This is fine as a recursive function, and would be easy to refactor to non-recursive anyway
        match expr {
            TemplateExpr::Block { expressions, span } => {
                let typed_expressions = expressions.into_iter().map(|expr| self.check_template_expr(expr, type_hint.clone(), reporter))
                    .collect::<Vec<_>>();

                // SAFETY: typed_expressions should have the same length as expressions, so it should
                //         also never be empty.

                TypedTemplateExpr::Block {
                    expressions: unsafe { NonEmpty::new_unchecked(typed_expressions) },
                    span,
                }
            },
            TemplateExpr::If { key_span, condition, then, otherwise, condition_span } => {
                let typed_condition = self.check_expr(condition, TypeHint::new(name::BOOLEAN_TYPE_ID), reporter);
                let typed_then = Box::new(self.check_template_expr(*then, type_hint.clone(), reporter));
                let typed_otherwise = Box::new(self.check_template_expr(*otherwise, type_hint, reporter));

                let typed_condition = match typed_condition.convert_to(&TypeHint::new(name::BOOLEAN_TYPE_ID), &self.names) {
                    Ok(typed_condition) => typed_condition,
                    Err(typed_condition) => {
                        self.error(condition_span, TypeError::MismatchedTypes {
                            expected: Cow::Borrowed("boolean"),
                            found: Cow::Owned(self.names.type_name(typed_condition.type_id()).to_owned()),
                            message: Some(Cow::Borrowed("condition of `if` expression needs to be of type boolean")),
                            additional_annotation: Some((SpanWithFile::new(self.file_id, key_span), None)),
                        }, reporter);
                        typed_condition
                    },
                };

                TypedTemplateExpr::If {
                    key_span: SpanWithFile::new(self.file_id, key_span),
                    condition: typed_condition,
                    then: typed_then,
                    otherwise: typed_otherwise,
                    condition_span,
                }
            },
            TemplateExpr::Simple(expr) => TypedTemplateExpr::Simple(self.check_expr(expr, type_hint, reporter)),
        }
    }

    fn check_expr(&mut self, expr: Expr<'source>, _type_hint: TypeHint, reporter: &mut TypeErrorReporter) -> TypedExpr {
        let mut input_stack = vec![ExprProcessVariant::Expr(expr)];
        let mut output_stack = Vec::new();

        while let Some(input) = input_stack.pop() {
            match input {
                ExprProcessVariant::Expr(expr) => {
                    match expr {
                        Expr::ConstantInt(value, span) => output_stack.push(TypedExpr::ConstantInt(value, SpanWithFile::new(self.file_id, span))),
                        Expr::ConstantFloat(value, span) => output_stack.push(TypedExpr::ConstantFloat(value, SpanWithFile::new(self.file_id, span))),
                        Expr::ConstantBoolean(value, span) => output_stack.push(TypedExpr::ConstantBoolean(value, SpanWithFile::new(self.file_id, span))),
                        Expr::ConstantString(value, span) => output_stack.push(TypedExpr::ConstantString(value, SpanWithFile::new(self.file_id, span))),
                        Expr::Identifier(name) => {
                            self.unimplemented(name.span(), "check identifier", reporter);
                            output_stack.push(TypedExpr::Identifier(OwnedToken::from_token(&name), name::ERROR_TYPE_ID));
                        },
                        Expr::Replacement(span) => {
                            self.unimplemented(span, "check replacement expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::UnaryOperator { operator, .. } => {
                            self.unimplemented(operator.span(), "check operator expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::BinaryOperator { operator, .. } => {
                            self.unimplemented(operator.span(), "check operator expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::FunctionCall { args_span, .. } => {
                            self.unimplemented(args_span, "check function call expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::Member { name, .. } => {
                            self.unimplemented(name.span(), "check member expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::Receiver { name, .. } => {
                            self.unimplemented(name.span(), "check receiver expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::Index { operator_span, .. } => {
                            self.unimplemented(operator_span, "check index expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::BuiltinFunctionCall { name, .. } => {
                            self.unimplemented(name.span(), "check built-in function call expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::BuiltinType(part) => {
                            self.unimplemented(part.span(), "check type expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::TypeCast { operator_span, .. } => {
                            self.unimplemented(operator_span, "check type cast expr", reporter);
                            output_stack.push(TypedExpr::Error);
                        },
                        Expr::Object { fields, merge_expr, span } => {
                            let (keys, exprs): (Vec<_>, Vec<_>) = fields.into_iter().unzip();

                            input_stack.push(ExprProcessVariant::Object { merge_expr: merge_expr.as_ref().map(|(_, span)| *span), fields: keys, span, });
                            input_stack.extend(exprs.into_iter().map(ExprProcessVariant::Expr));

                            if let Some((merge_expr, _)) = merge_expr {
                                input_stack.push(ExprProcessVariant::Expr(*merge_expr));
                            }
                        },
                        Expr::Array { elements, span } => {
                            input_stack.push(ExprProcessVariant::Array { elements: elements.len(), span, });
                            input_stack.extend(elements.into_iter().map(ExprProcessVariant::Expr));
                        },
                        Expr::Error => output_stack.push(TypedExpr::Error),
                    }
                },
                ExprProcessVariant::Object { merge_expr, fields, span } => {
                    let merge_expr = merge_expr.map(|merge_expr_span|
                        (output_stack.pop().expect("Internal compiler error: empty output stack"), merge_expr_span))
                        .map(|(expr, span)| (Box::new(expr), SpanWithFile::new(self.file_id, span)));

                    let merge_expr = if let Some((typed_merge_expr, merge_expr_span)) = merge_expr {
                        let typed_merge_expr = match typed_merge_expr.convert_to(&TypeHint::new(name::OBJECT_TYPE_ID), &self.names) {
                            Ok(typed_merge_expr) => typed_merge_expr,
                            Err(typed_merge_expr) => {
                                self.error(merge_expr_span.span(), TypeError::MismatchedTypes {
                                    expected: Cow::Borrowed("object"),
                                    found: Cow::Owned(self.names.type_name(typed_merge_expr.type_id()).to_owned()),
                                    message: Some(Cow::Borrowed("merge expression needs to be of type object")),
                                    additional_annotation: Some((SpanWithFile::new(self.file_id, span), None)),
                                }, reporter);
                                typed_merge_expr
                            },
                        };

                        Some((Box::new(typed_merge_expr), merge_expr_span))
                    } else { None };

                    let fields_len = fields.len();

                    let fields = if !fields.is_empty() { fields.into_iter().map(|token| OwnedToken::from_token(&token)).zip(
                        output_stack.drain((output_stack.len() - fields_len)..output_stack.len()).rev()).collect() } else { Vec::new() };

                    output_stack.push(TypedExpr::Object {
                        fields,
                        merge_expr,
                        span,
                    });
                },
                ExprProcessVariant::Array { elements, span } => {
                    let elements = if elements > 0 {
                        output_stack.drain((output_stack.len() - elements)..output_stack.len()).rev().collect()
                    } else { Vec::new() };

                    output_stack.push(TypedExpr::Array {
                        elements,
                        span,
                    });
                },
            }
        }

        assert!(input_stack.is_empty(), "Input stack is not empty after type check expr call: {}", input_stack.len());
        assert_eq!(1, output_stack.len(), "Output stack does not have exactly one element after type check expr call: {}", output_stack.len());

        output_stack.swap_remove(0)
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }

    fn unimplemented(&self, span: Span, message: &'source str, reporter: &mut TypeErrorReporter<'source>) {
        self.error(span, TypeError::Unimplemented(message), reporter);
    }
}
