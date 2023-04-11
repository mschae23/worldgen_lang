use std::path::PathBuf;
use std::rc::Rc;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::forward::ForwardDeclStorage;
use crate::compiler::ast::simple::{ClassImplementsPart, ClassReprPart, Decl, Expr, ParameterPart, TemplateExpr, TemplateKind, TypePart, VariableKind};
use crate::compiler::ast::typed::{TypedDecl, TypedExpr};
use crate::compiler::error::{Diagnostic, DiagnosticContext, ErrorReporter, FileId, NoteKind, Severity};
use crate::compiler::error::span::Span;
use crate::compiler::lexer::Token;
use crate::compiler::name::{NameResolution, TypeStorage};
use crate::Config;
#[allow(unused)]
use crate::println_debug;

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<'source> {
    Unknown(&'source str), // Because there are no errors yet
}

impl<'source> Diagnostic<MessageMarker> for TypeError<'source> {
    fn name(&self) -> &'static str {
        match self {
            _ => "type/unknown",
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
            _ => String::from("no message"),
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            _ => None,
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
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

enum ProcessVariant<'source> {
    Decl(Decl<'source>),
    ModuleStart(&'source str),
    ModuleEnd,
}

pub struct TypeChecker {
    _config: Rc<Config>,
    _path: Rc<PathBuf>, file_id: FileId,

    types: TypeStorage, forward_decls: ForwardDeclStorage,
    names: NameResolution,
}

impl<'source> TypeChecker {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, file_id: FileId, type_storage: TypeStorage, forward_decls: ForwardDeclStorage) -> Self {
        TypeChecker {
            _config: config, _path: path, file_id,
            types: type_storage, forward_decls,
            names: NameResolution::new(),
        }
    }

    pub fn check_types(mut self, declarations: Vec<Decl<'source>>, reporter: &mut TypeErrorReporter<'source>) -> Vec<TypedDecl<'source>> {
        // println_debug!("Name resolution state:\n{:#?}\n{:#?}", &self.types, &self.forward_decls);

        let mut typed_declarations = Vec::new();
        let mut process_stack = declarations.into_iter()
            .map(ProcessVariant::Decl).rev().collect::<Vec<_>>();

        while let Some(process) = process_stack.pop() {
            match process {
                ProcessVariant::Decl(decl) => match decl {
                    Decl::Module { name, declarations } => {
                        process_stack.push(ProcessVariant::ModuleEnd);
                        process_stack.extend(declarations.into_iter()
                            .map(ProcessVariant::Decl).rev());
                        process_stack.push(ProcessVariant::ModuleStart(name.source()));
                    },
                    Decl::Interface { name, parameters, implements, class_repr, parameter_span } =>
                        typed_declarations.push(self.check_interface_decl(name, parameters, implements, class_repr, parameter_span)),
                    Decl::Class { name, parameters, implements, class_repr, parameter_span } =>
                        typed_declarations.push(self.check_class_decl(name, parameters, implements, class_repr, parameter_span)),
                    Decl::TypeAlias { name, to, condition } =>
                        typed_declarations.push(self.check_type_alias_decl(name, to, condition)),
                    Decl::Template { kind, parameters, return_type, expr, parameter_span } =>
                        typed_declarations.push(self.check_template_decl(kind, parameters, return_type, expr, parameter_span)),
                    Decl::Include { path } =>
                        typed_declarations.push(self.check_include(path, &mut process_stack)),
                    Decl::Import { path, selector, span } =>
                        typed_declarations.push(self.check_import(path, selector, span)),
                    Decl::Variable { kind, name, expr, span } =>
                        typed_declarations.push(self.check_variable(kind, name, expr, span)),
                    Decl::Error =>
                        typed_declarations.push(TypedDecl::Error),
                },
                ProcessVariant::ModuleStart(name) => {
                    self.names.open_type_environment(name.to_owned());
                },
                ProcessVariant::ModuleEnd => {
                    self.names.close_type_environment();
                },
            }
        }

        typed_declarations
    }

    fn check_interface_decl(&mut self, name: Token<'source>, parameters: Vec<ParameterPart<'source>>, implements: Option<ClassImplementsPart<'source>>, class_repr: Option<ClassReprPart<'source>>, parameter_span: Span) -> TypedDecl {
        todo!()
    }

    fn check_class_decl(&mut self, name: Token<'source>, parameters: Vec<ParameterPart<'source>>, implements: ClassImplementsPart<'source>, class_repr: Option<ClassReprPart<'source>>, parameter_span: Span) -> TypedDecl {
        todo!()
    }

    fn check_type_alias_decl(&mut self, name: Token<'source>, to: TypePart<'source>, condition: Option<Expr<'source>>) -> TypedDecl {
        todo!()
    }

    fn check_template_decl(&mut self, kind: TemplateKind<'source>, parameters: Vec<ParameterPart<'source>>, return_type: TypePart<'source>, expr: TemplateExpr<'source>, parameter_span: Span) -> TypedDecl {
        todo!()
    }

    fn check_include(&mut self, path: Token<'source>, process_stack: &mut Vec<ProcessVariant>) -> TypedDecl {
        todo!()
    }

    fn check_import(&mut self, path: NonEmpty<Token<'source>>, selector: Option<NonEmpty<Token<'source>>>, span: Span) -> TypedDecl {
        todo!()
    }

    fn check_variable(&mut self, kind: VariableKind, name: Token<'source>, expr: Expr<'source>, span: Span) -> TypedDecl {
        todo!()
    }

    fn error(&self, span: Span, message: TypeError<'source>, reporter: &mut TypeErrorReporter<'source>) {
        reporter.report(span, message, false);
    }
}
