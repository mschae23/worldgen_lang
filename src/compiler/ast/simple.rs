use std::fmt::{Debug, Formatter};
use non_empty_vec::NonEmpty;
use crate::compiler::error::span::Span;
use crate::compiler::lexer::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeReferencePart<'source>(pub NonEmpty<Token<'source>>, pub Span);

#[derive(Clone, PartialEq, Eq)]
pub enum TypePart<'source> {
    Name(TypeReferencePart<'source>),
    Template {
        args: Vec<TypePart<'source>>,
        return_type: Box<TypePart<'source>>,
        args_span: Span,
    },
}

impl<'source> Debug for TypePart<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(tokens) => write!(f, "{}", tokens.0.iter().map(|token| token.source().clone()).collect::<Vec<_>>().join("::")),
            Self::Template { args, return_type, .. } => write!(f, "({}): {:?}", args.iter()
                .map(|arg| format!("{:?}", arg)).collect::<Vec<_>>().join(", "), return_type),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterPart<'source> {
    pub name: Token<'source>,
    pub parameter_type: TypePart<'source>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleImplementsPart<'source> {
    pub name: TypeReferencePart<'source>,
    pub parameters: Vec<Expr<'source>>,
    pub span: Span, pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassImplementsPart<'source> {
    pub parts: NonEmpty<SingleImplementsPart<'source>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassReprPart<'source> {
    pub expr: Expr<'source>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TemplateDeclKind {
    Template,
    Optimize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TemplateKind<'source> {
    Template {
        name: Token<'source>,
    },
    Conversion {
        // Doesn't have the type it converts from here because it's a parameter
        span: Span,
    },
    Optimize {
        on: TypePart<'source>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableKind {
    Auto,
    Inline,
    Export,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<'source> {
    Module {
        name: Token<'source>,
        declarations: Vec<Decl<'source>>,
    },
    Interface {
        name: Token<'source>,
        parameters: Vec<ParameterPart<'source>>,
        implements: Option<ClassImplementsPart<'source>>,
        class_repr: Option<ClassReprPart<'source>>,
        parameter_span: Span,
    },
    Class {
        name: Token<'source>,
        parameters: Vec<ParameterPart<'source>>,
        implements: ClassImplementsPart<'source>,
        class_repr: Option<ClassReprPart<'source>>,
        parameter_span: Span,
    },
    TypeAlias {
        name: Token<'source>,
        to: TypePart<'source>,
        condition: Option<Expr<'source>>,
    },
    Template {
        kind: TemplateKind<'source>,
        parameters: Vec<ParameterPart<'source>>,
        return_type: TypePart<'source>,
        expr: TemplateExpr<'source>,
        parameter_span: Span,
    },
    Include {
        path: Token<'source>,
    },
    Import {
        path: NonEmpty<Token<'source>>,
        selector: Option<NonEmpty<Token<'source>>>, // None represents star import (import something::*)
        span: Span, // only path and selector, doesn't include the "import" keyword or semicolon
    },
    Variable {
        kind: VariableKind,
        name: Token<'source>,
        expr: Expr<'source>,
        span: Span,
    },

    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TemplateExpr<'source> {
    Block {
        expressions: NonEmpty<TemplateExpr<'source>>,
        span: Span,
    },
    If {
        condition: Expr<'source>,
        then: Box<TemplateExpr<'source>>,
        otherwise: Box<TemplateExpr<'source>>,
        condition_span: Span,
    },
    Simple(Expr<'source>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'source> {
    ConstantFloat(f64, Span),
    ConstantInt(i32, Span),
    ConstantBoolean(bool, Span),
    ConstantString(String, Span),
    Identifier(Token<'source>),

    UnaryOperator {
        operator_span: Span,
        expr: Box<Expr<'source>>,
    },
    BinaryOperator {
        left: Box<Expr<'source>>,
        operator_span: Span,
        right: Box<Expr<'source>>,
    },
    FunctionCall {
        callee: Box<Expr<'source>>,
        args: Vec<Expr<'source>>,
        args_span: Span,
    },
    Member { // something::member
        receiver: Box<Expr<'source>>,
        name: Token<'source>,
    },
    Receiver { // something.receiver_template
        receiver: Box<Expr<'source>>,
        name: Token<'source>,
    },
    Index {
        receiver: Box<Expr<'source>>,
        index: Box<Expr<'source>>,
        operator_span: Span,
    },
    BuiltinFunctionCall {
        name: Token<'source>,
        args: Vec<Expr<'source>>,
        args_span: Span,
    },
    BuiltinType(TypePart<'source>),
    TypeCast {
        expr: Box<Expr<'source>>,
        operator_span: Span,
        to: TypePart<'source>,
    },

    Object {
        fields: Vec<(Token<'source>, Expr<'source>)>,
        span: Span,
    },
    Array {
        elements: Vec<Expr<'source>>,
        span: Span,
    },

    Error,
}
