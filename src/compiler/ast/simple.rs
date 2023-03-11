use std::fmt::{Debug, Formatter};
use non_empty_vec::NonEmpty;
use crate::compiler::error::span::Span;
use crate::compiler::lexer::{Token, TokenPos};

#[derive(Clone, PartialEq, Eq)]
pub enum TypePart<'source> {
    Name(NonEmpty<Token<'source>>, Span),
    Template {
        args: Vec<TypePart<'source>>,
        return_type: Box<TypePart<'source>>,
        args_span: Span,
    },
}

impl<'source> Debug for TypePart<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypePart::Name(tokens, _) => write!(f, "{}", tokens.iter().map(|token| token.source().clone()).collect::<Vec<_>>().join("::")),
            TypePart::Template { args, return_type, .. } => write!(f, "({}): {:?}", args.iter()
                .map(|arg| format!("{:?}", arg)).collect::<Vec<_>>().join(", "), return_type)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleImplementsPart<'source> {
    pub name: Token<'source>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TemplateKind<'source> {
    Template {
        name: Token<'source>,
    },
    Conversion {
        from: TypePart<'source>,
    },
    Optimize {
        on: TypePart<'source>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableKind {
    Simple,
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
        parameters: Vec<(Token<'source>, TypePart<'source>)>,
        implements: Option<ClassImplementsPart<'source>>,
        class_repr: Option<ClassReprPart<'source>>,
        parameter_span: Span,
    },
    Class {
        name: Token<'source>,
        parameters: Vec<(Token<'source>, TypePart<'source>)>,
        implements: ClassImplementsPart<'source>, // TODO should this be optional or required?
        class_repr: ClassReprPart<'source>,
        parameter_span: Span,
    },
    Template {
        kind: TemplateKind<'source>,
        parameters: Vec<(Token<'source>, TypePart<'source>)>,
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
        expressions: Vec<TemplateExpr<'source>>,
        last: Box<TemplateExpr<'source>>,
        start_token: TokenPos, end_token: TokenPos,
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
        operator: TokenPos,
        expr: Box<Expr<'source>>,
    },
    BinaryOperator {
        left: Box<Expr<'source>>,
        operator: TokenPos,
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
        operator_start: TokenPos,
        index: Box<Expr<'source>>,
        operator_end: TokenPos,
    },
    BuiltinFunctionCall {
        name: Token<'source>,
        args: Vec<Expr<'source>>,
        args_span: Span,
    },
    BuiltinType(TypePart<'source>),
    TypeCast {
        expr: Box<Expr<'source>>,
        operator: TokenPos,
        to: TypePart<'source>,
    },

    Object {
        fields: Vec<(Token<'source>, Expr<'source>)>,
        start_token: TokenPos, end_token: TokenPos,
    },
    Array {
        elements: Vec<Expr<'source>>,
        start_token: TokenPos, end_token: TokenPos,
    },

    Error,
}
