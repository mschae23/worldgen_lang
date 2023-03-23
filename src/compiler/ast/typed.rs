use std::fmt::Debug;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::simple::{ClassImplementsPart, ClassReprPart, ParameterPart, TemplateKind, TypePart, VariableKind};
use crate::compiler::error::span::Span;
use crate::compiler::lexer::Token;

// TODO This isn't actually typed yet

#[derive(Clone, Debug, PartialEq)]
pub enum TypedDecl<'source> {
    Module {
        name: Token<'source>,
        declarations: Vec<TypedDecl<'source>>,
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
        condition: Option<TypedExpr<'source>>,
    },
    Template {
        kind: TemplateKind<'source>,
        parameters: Vec<ParameterPart<'source>>,
        return_type: TypePart<'source>,
        expr: TypedTemplateExpr<'source>,
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
        expr: TypedExpr<'source>,
        span: Span,
    },

    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedTemplateExpr<'source> {
    Block {
        expressions: NonEmpty<TypedTemplateExpr<'source>>,
        span: Span,
    },
    If {
        condition: TypedExpr<'source>,
        then: Box<TypedTemplateExpr<'source>>,
        otherwise: Box<TypedTemplateExpr<'source>>,
        condition_span: Span,
    },
    Simple(TypedExpr<'source>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedExpr<'source> {
    ConstantFloat(f64, Span),
    ConstantInt(i32, Span),
    ConstantBoolean(bool, Span),
    ConstantString(String, Span),
    Identifier(Token<'source>),

    Replacement(Span), // `_` token as expression, for type alias conditions

    UnaryOperator {
        operator: Token<'source>,
        expr: Box<TypedExpr<'source>>,
    },
    BinaryOperator {
        left: Box<TypedExpr<'source>>,
        operator: Token<'source>,
        right: Box<TypedExpr<'source>>,
    },
    FunctionCall {
        callee: Box<TypedExpr<'source>>,
        args: Vec<TypedExpr<'source>>,
        args_span: Span,
    },
    Member { // something::member
        receiver: Box<TypedExpr<'source>>,
        name: Token<'source>,
    },
    Receiver { // something.receiver_template
        receiver: Box<TypedExpr<'source>>,
        name: Token<'source>,
    },
    Index {
        receiver: Box<TypedExpr<'source>>,
        index: Box<TypedExpr<'source>>,
        operator_span: Span,
    },
    BuiltinFunctionCall {
        name: Token<'source>,
        args: Vec<TypedExpr<'source>>,
        args_span: Span,
    },
    BuiltinType(TypePart<'source>),
    TypeCast {
        expr: Box<TypedExpr<'source>>,
        operator_span: Span,
        to: TypePart<'source>,
    },

    Object {
        fields: Vec<(Token<'source>, TypedExpr<'source>)>,
        merge_expr: Option<Box<TypedExpr<'source>>>,
        span: Span,
    },
    Array {
        elements: Vec<TypedExpr<'source>>,
        span: Span,
    },

    Error,
}
