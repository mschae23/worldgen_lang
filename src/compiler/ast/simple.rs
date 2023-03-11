use std::fmt::{Debug, Formatter};
use non_empty_vec::NonEmpty;
use crate::compiler::error::span::Span;
use crate::compiler::lexer::Token;

#[derive(Clone, PartialEq, Eq)]
pub enum TypePart<'source> {
    Name(NonEmpty<Token<'source>>, Span),
    // Template { ... },
}

impl<'source> Debug for TypePart<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypePart::Name(tokens, _) => write!(f, "{}", tokens.iter().map(|token| token.source().clone()).collect::<Vec<_>>().join("::")),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleImplementsPart<'source> {
    pub name: Token<'source>,
    // pub parameters: Vec<Expr>,
    pub span: Span, pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassImplementsPart<'source> {
    pub parts: NonEmpty<SingleImplementsPart<'source>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassReprPart {
    // pub expr: Expr,
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
        class_repr: Option<ClassReprPart>,
        parameter_span: Span,
    },
    Class {
        name: Token<'source>,
        parameters: Vec<(Token<'source>, TypePart<'source>)>,
        implements: ClassImplementsPart<'source>, // TODO should this be optional or required?
        class_repr: ClassReprPart,
        parameter_span: Span,
    },
    Template {
        kind: TemplateKind<'source>,
        parameters: Vec<(Token<'source>, TypePart<'source>)>,
        return_type: TypePart<'source>,
        // expr: TemplateExpr,
        parameter_span: Span,
    },
    Import {
        path: NonEmpty<Token<'source>>,
        selector: Option<NonEmpty<Token<'source>>>, // None represents star import (import something::*)
        span: Span, // only path and selector, doesn't include the "import" keyword or semicolon
    },
    Variable {
        kind: VariableKind,
        name: Token<'source>,
        // expr: Expr,
        span: Span,
    },
}
