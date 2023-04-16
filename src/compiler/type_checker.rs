use std::borrow::Cow;
use std::path::PathBuf;
use std::rc::Rc;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::forward::{ForwardDecl, ForwardDeclStorage};
use crate::compiler::ast::simple::{ClassImplementsPart, ClassReprPart, Decl, Expr, ParameterPart, TemplateExpr, TemplateKind, TypePart, VariableKind};
use crate::compiler::ast::typed::{OwnedToken, TypedClassImplementsPart, TypedClassReprPart, TypedDecl, TypedDefinedClassReprPart, TypedExpr, TypedParameterPart};
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::Token;
use crate::compiler::name;
use crate::compiler::name::{NameResolution, TypeStorage};
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
    Unimplemented(&'source str),
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            Self::ImplementsNotClass { .. } => "type/implements_not_class",
            Self::ClassMissingDef { .. } => "type/class_missing_def",
            Self::MismatchedTypes { .. } => "type/mismatched_types",
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
            Self::Unimplemented(msg) => format!("not implemented yet: {}", msg),
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::ImplementsNotClass { name, .. } => Some(format!("`{}` referenced here", name)),
            Self::ClassMissingDef { name, .. } => Some(format!("`{}` is declared here", name)),
            Self::MismatchedTypes { message, .. } => message.as_ref().map(|cow| cow.clone().into_owned()),
            Self::Unimplemented(_) => Some(String::from("unimplemented feature used here")),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(SpanWithFile, Option<String>)> {
        match self {
            Self::ImplementsNotClass { name, defined_at, .. } => vec![(*defined_at, Some(format!("`{}` originally defined here", name)))],
            Self::MismatchedTypes { additional_annotation, .. } => additional_annotation.as_ref().map(|&(span, ref label)| (span, label.as_ref()
                .map(|label| label.clone().into_owned())))
                .into_iter().collect(),
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
    Decl(Decl<'source>),
    ModuleStart(&'source str),
    ModuleEnd,
}

enum ExprProcessVariant<'source> {
    Expr(Expr<'source>),
    Object { merge_expr: Option<Span>, fields: Vec<Token<'source>>, span: Span, },
    Array { elements: usize, span: Span, },
}

pub struct TypeChecker {
    _config: Rc<Config>,
    _path: Rc<PathBuf>, file_id: FileId,

    types: TypeStorage, forward_decls: ForwardDeclStorage,
    names: NameResolution,
}

#[allow(clippy::too_many_arguments)]
impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId, type_storage: TypeStorage, forward_decls: ForwardDeclStorage) -> Self {
        TypeChecker {
            _config: config, _path: path, file_id,
            types: type_storage, forward_decls,
            names: NameResolution::new(),
        }
    }

    pub fn check_types(mut self, declarations: Vec<Decl<'source>>, reporter: &mut TypeErrorReporter<'source>) {
        println_debug!("Name resolution state:\n{:#?}\n{:#?}", &self.types, &self.forward_decls);

        let mut process_stack = declarations.into_iter()
            .map(DeclProcessVariant::Decl).rev().collect::<Vec<_>>();
        let mut module_path = Vec::new();

        while let Some(process) = process_stack.pop() {
            match process {
                DeclProcessVariant::Decl(decl) => match decl {
                    Decl::Module { name, declarations, key_span: _key_span } => {
                        process_stack.push(DeclProcessVariant::ModuleEnd);
                        process_stack.extend(declarations.into_iter()
                            .map(DeclProcessVariant::Decl).rev());
                        process_stack.push(DeclProcessVariant::ModuleStart(name.source()));
                    },
                    Decl::Class { key_span, interface, name, parameters, implements, class_repr, parameter_span } =>
                        self.check_class_decl(key_span, interface, name, parameters, implements, class_repr, parameter_span, &module_path, reporter),
                    Decl::TypeAlias { key_span, name, to, condition } =>
                        self.check_type_alias_decl(key_span, name, to, condition, &module_path, reporter),
                    Decl::Template { key_span, kind, parameters, return_type, expr, parameter_span } =>
                        self.check_template_decl(key_span, kind, parameters, return_type, expr, parameter_span, reporter),
                    Decl::Include { key_span, path } =>
                        self.check_include(key_span, path, &mut process_stack, reporter),
                    Decl::Import { key_span, path, selector, span } =>
                        self.check_import(key_span, path, selector, span, reporter),
                    Decl::Variable { key_span, kind, name, expr, span } =>
                        self.check_variable(key_span, kind, name, expr, span, reporter),
                    Decl::Error => {},
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

    fn check_class_decl(&mut self, _key_span: SpanWithFile, interface: bool, name: Token<'source>, parameters: Vec<ParameterPart<'source>>, implements: Option<ClassImplementsPart<'source>>, class_repr: Option<ClassReprPart<'source>>, parameter_span: Span, module_path: &[&'source str], reporter: &mut TypeErrorReporter<'source>) {
        let forward_decl_id = self.forward_decls.find_decl_id_for_duplicate(module_path, &[], |name2, decl|
            !matches!(decl, ForwardDecl::Template(_)) && name.source() == name2)
            .expect("Internal compiler error: Missing forward declaration for class");

        if self.forward_decls.has_duplicate(forward_decl_id) {
            return;
        }

        let forward_decl = self.forward_decls.get_decl_by_id(forward_decl_id);

        match forward_decl {
            ForwardDecl::Class(decl) => {
                let implements_type = decl.implements;

                let typed_parameters = parameters.into_iter().enumerate().map(|(i, part)| TypedParameterPart {
                    name: OwnedToken::from_token(&part.name),
                    parameter_type: decl.parameters[i],
                }).collect::<Vec<_>>();

                let typed_implements = implements.zip(implements_type).map(|(part, implements_type)| {
                    let parameter_types = self.forward_decls.get_decl_by_type_id(implements_type).and_then(|referenced_decl| {
                        match referenced_decl {
                            ForwardDecl::Class(decl) => Some(decl.parameters.clone()),
                            _ => {
                                self.error(part.span, TypeError::ImplementsNotClass { name: part.name.0.last().source(),
                                    actual_kind: referenced_decl.kind_with_indefinite_article(),
                                    defined_at: referenced_decl.span(),
                                }, reporter);
                                None
                            }
                        }
                    });

                    TypedClassImplementsPart {
                        reference: implements_type,
                        parameters: part.parameters.into_iter().enumerate()
                            .map(|(i, parameter)| self.check_expr(parameter,
                                parameter_types.as_ref().map(|types| TypeHint::Options(NonEmpty::new(types[i]))).unwrap_or(TypeHint::Any),
                                    reporter)).collect(),
                        span: part.span,
                        parameter_span: part.parameter_span,
                        file_id: self.file_id,
                    }
                });

                let typed_class_repr = class_repr.map(|part| TypedClassReprPart::Defined(TypedDefinedClassReprPart {
                    expr: self.check_expr(part.expr, TypeHint::Any, reporter),
                    span: part.span,
                    file_id: self.file_id,
                })).unwrap_or_else(|| {
                    let parent = implements_type.and_then(|id| self.forward_decls.get_decl_id_from_type_id(id));
                    let mut parent = parent.and_then(|parent| match self.forward_decls.get_decl_by_id(parent) {
                        ForwardDecl::Class(decl) => Some((parent, decl)),
                        _ => None,
                    });

                    while let Some((parent_id, decl)) = parent {
                        if decl.repr {
                            return TypedClassReprPart::Inherited(parent_id);
                        } else {
                            parent = parent.and_then(|(_, parent)| parent.implements
                                .and_then(|parent| self.forward_decls.get_decl_id_from_type_id(parent))
                                .and_then(|parent| match self.forward_decls.get_decl_by_id(parent) {
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

    fn check_type_alias_decl(&mut self, key_span: SpanWithFile, name: Token<'source>, _to: TypePart<'source>, condition: Option<(Expr<'source>, Span)>, module_path: &[&'source str], reporter: &mut TypeErrorReporter<'source>) {
        let forward_decl_id = self.forward_decls.find_decl_id_for_duplicate(module_path, &[], |name2, decl|
            !matches!(decl, ForwardDecl::Template(_)) && name.source() == name2)
            .expect("Internal compiler error: Missing forward declaration for class");

        if self.forward_decls.has_duplicate(forward_decl_id) {
            return;
        }

        let forward_decl = self.forward_decls.get_decl_by_id(forward_decl_id);

        match forward_decl {
            ForwardDecl::TypeAlias(decl) => {
                let to = decl.reference;

                let typed_condition = condition.map(|(condition, condition_span)| {
                    let typed = self.check_expr(condition, TypeHint::new(name::BOOLEAN_TYPE_ID), reporter);

                    if !TypeHint::new(name::BOOLEAN_TYPE_ID).can_convert_from(&TypeHint::new(typed.type_id()), true, &self.names) {
                        self.error(condition_span, TypeError::MismatchedTypes {
                            expected: Cow::Borrowed("boolean"),
                            found: Cow::Owned(self.names.type_name(typed.type_id()).to_owned()),
                            message: Some(Cow::Borrowed("type alias condition must be of type boolean")),
                            additional_annotation: Some((key_span, None)),
                        }, reporter);
                    }

                    typed
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

    fn check_template_decl(&mut self, key_span: SpanWithFile, kind: TemplateKind<'source>, parameters: Vec<ParameterPart<'source>>, return_type: TypePart<'source>, expr: TemplateExpr<'source>, parameter_span: Span, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check template decl", reporter);
    }

    fn check_include(&mut self, key_span: SpanWithFile, path: Token<'source>, process_stack: &mut Vec<DeclProcessVariant>, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check include decl", reporter);
    }

    fn check_import(&mut self, key_span: SpanWithFile, path: NonEmpty<Token<'source>>, selector: Option<NonEmpty<Token<'source>>>, span: Span, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check import decl", reporter);
    }

    fn check_variable(&mut self, key_span: SpanWithFile, kind: VariableKind, name: Token<'source>, expr: Expr<'source>, span: Span, reporter: &mut TypeErrorReporter<'source>) {
        self.unimplemented(key_span.span(), "check variable decl", reporter);
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

                    if let Some((typed_merge_expr, merge_expr_span)) = &merge_expr {
                        // TODO insert "convert type" AST node
                        if !TypeHint::new(name::OBJECT_TYPE_ID).can_convert_from(&TypeHint::new(typed_merge_expr.type_id()), true, &self.names) {
                            self.error(merge_expr_span.span(), TypeError::MismatchedTypes {
                                expected: Cow::Borrowed("object"),
                                found: Cow::Owned(self.names.type_name(typed_merge_expr.type_id()).to_owned()),
                                message: Some(Cow::Borrowed("merge expression needs to be of type object")),
                                additional_annotation: Some((SpanWithFile::new(self.file_id, span), None)),
                            }, reporter);
                        }
                    }

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

    /* fn check_expr(&mut self, expr: Expr<'source>, _type_hint: TypeHint, reporter: &mut TypeErrorReporter<'source>) -> TypedExpr {
        match expr {
            Expr::ConstantInt(value, span) => TypedExpr::ConstantInt(value, SpanWithFile::new(self.file_id, span)),
            Expr::ConstantFloat(value, span) => TypedExpr::ConstantFloat(value, SpanWithFile::new(self.file_id, span)),
            Expr::ConstantBoolean(value, span) => TypedExpr::ConstantBoolean(value, SpanWithFile::new(self.file_id, span)),
            Expr::ConstantString(value, span) => TypedExpr::ConstantString(value, SpanWithFile::new(self.file_id, span)),
            Expr::Identifier(name) => {
                self.unimplemented(name.span(), "check identifier", reporter);
                TypedExpr::Identifier(OwnedToken::from_token(&name), name::ERROR_TYPE_ID)
            },
            Expr::Replacement(span) => {
                self.unimplemented(span, "check replacement expr", reporter);
                TypedExpr::Error
            },
            Expr::UnaryOperator { operator, .. } => {
                self.unimplemented(operator.span(), "check operator expr", reporter);
                TypedExpr::Error
            },
            Expr::BinaryOperator { operator, .. } => {
                self.unimplemented(operator.span(), "check operator expr", reporter);
                TypedExpr::Error
            },
            Expr::FunctionCall { args_span, .. } => {
                self.unimplemented(args_span, "check function call expr", reporter);
                TypedExpr::Error
            },
            Expr::Member { name, .. } => {
                self.unimplemented(name.span(), "check member expr", reporter);
                TypedExpr::Error
            },
            Expr::Receiver { name, .. } => {
                self.unimplemented(name.span(), "check receiver expr", reporter);
                TypedExpr::Error
            },
            Expr::Index { operator_span, .. } => {
                self.unimplemented(operator_span, "check index expr", reporter);
                TypedExpr::Error
            },
            Expr::BuiltinFunctionCall { name, .. } => {
                self.unimplemented(name.span(), "check built-in function call expr", reporter);
                TypedExpr::Error
            },
            Expr::BuiltinType(part) => {
                self.unimplemented(part.span(), "check type expr", reporter);
                TypedExpr::Error
            },
            Expr::TypeCast { operator_span, .. } => {
                self.unimplemented(operator_span, "check type cast expr", reporter);
                TypedExpr::Error
            },
            Expr::Object { fields, merge_expr, span } => {
                let typed_merge_expr = merge_expr.map(|(merge_expr, span)|
                    (Box::new(self.check_expr(*merge_expr, TypeHint::new(name::OBJECT_TYPE_ID), reporter)), SpanWithFile::new(self.file_id, span)));

                if let Some((typed_merge_expr, merge_expr_span)) = &typed_merge_expr {
                    // TODO insert "convert type" AST node
                    if !TypeHint::new(name::OBJECT_TYPE_ID).can_convert_from(&TypeHint::new(typed_merge_expr.type_id()), true, &self.names) {
                        self.error(merge_expr_span.span(), TypeError::MismatchedTypes {
                            expected: Cow::Borrowed("object"),
                            found: Cow::Owned(self.names.type_name(typed_merge_expr.type_id()).to_owned()),
                            message: Some(Cow::Borrowed("merge expression needs to be of type object")),
                            additional_annotation: Some((SpanWithFile::new(self.file_id, span), None)),
                        }, reporter);
                    }
                }

                TypedExpr::Object {
                    fields: fields.into_iter().map(|(key, field)|
                        (OwnedToken::from_token(&key), self.check_expr(field, TypeHint::Any, reporter))
                    ).collect(),
                    merge_expr: typed_merge_expr,
                    span,
                }
            },
            Expr::Array { elements, span } => {
                TypedExpr::Array {
                    elements: elements.into_iter().map(|element|
                        self.check_expr(element, TypeHint::Any, reporter)
                    ).collect(),
                    span,
                }
            },
            Expr::Error => TypedExpr::Error,
        }
    } */

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }

    fn unimplemented(&self, span: Span, message: &'source str, reporter: &mut TypeErrorReporter<'source>) {
        self.error(span, TypeError::Unimplemented(message), reporter);
    }
}
