use std::fmt::Debug;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::forward::DeclId;
use crate::compiler::ast::simple::VariableKind;
use crate::compiler::error::FileId;
use crate::compiler::error::span::Span;
use crate::compiler::lexer::{Token, TokenType};
use crate::compiler::name::TypeId;

// TODO This isn't actually typed yet

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OwnedToken {
    pub source: String,
    pub token_type: TokenType,
    pub file_id: FileId, pub span: Span,
}

impl OwnedToken {
    pub fn new(source: String, token_type: TokenType, file_id: FileId, span: Span) -> Self {
        OwnedToken {
            source, token_type, file_id, span,
        }
    }

    pub fn from_token(token: &Token<'_>) -> Self {
        Self::new(token.source().to_owned(), token.token_type(), token.file_id(), token.span())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedParameterPart {
    pub name: OwnedToken,
    pub parameter_type: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedClassImplementsPart {
    pub reference: TypeId,
    pub parameters: Vec<TypedExpr>,
    pub span: Span, pub parameter_span: Span, pub file_id: FileId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedDefinedClassReprPart {
    pub expr: TypedExpr,
    pub span: Span, pub file_id: FileId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedClassReprPart {
    Defined(TypedDefinedClassReprPart),
    Inherited(DeclId),
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedDecl {
    Interface {
        name: OwnedToken,
        parameters: Vec<TypedParameterPart>,
        implements: Option<TypedClassImplementsPart>,
        class_repr: Option<TypedClassReprPart>,
        parameter_span: Span,
    },
    Class {
        name: OwnedToken,
        parameters: Vec<TypedParameterPart>,
        implements: TypedClassImplementsPart,
        class_repr: Option<TypedClassReprPart>,
        parameter_span: Span,
    },
    TypeAlias {
        name: OwnedToken,
        to: TypeId,
        condition: Option<TypedExpr>,
    },
    Variable {
        kind: VariableKind,
        name: OwnedToken,
        expr: TypedExpr,
        span: Span,
    },

    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedTemplateDecl {
    pub name: OwnedToken,
    pub parameters: Vec<TypedParameterPart>,
    pub return_type: TypeId,
    pub expr: TypedTemplateExpr,
    pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedConversionDecl {
    pub from: TypedParameterPart,
    pub to: TypeId,
    pub expr: TypedTemplateExpr,
    pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedOptimizeDecl {
    pub on: TypeId,
    pub parameters: Vec<TypedParameterPart>,
    pub return_type: TypeId,
    pub expr: TypedTemplateExpr,
    pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedTemplateExpr {
    Block {
        expressions: NonEmpty<TypedTemplateExpr>,
        span: Span,
    },
    If {
        condition: TypedExpr,
        then: Box<TypedTemplateExpr>,
        otherwise: Box<TypedTemplateExpr>,
        condition_span: Span,
    },
    Simple(TypedExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedExpr {
    ConstantFloat(f64, FileId, Span),
    ConstantInt(i32, FileId, Span),
    ConstantBoolean(bool, FileId, Span),
    ConstantString(String, FileId, Span),
    Identifier(OwnedToken),

    Replacement(Span), // `_` token as expression, for type alias conditions

    UnaryOperator {
        operator: OwnedToken,
        expr: Box<TypedExpr>,
    },
    BinaryOperator {
        left: Box<TypedExpr>,
        operator: OwnedToken,
        right: Box<TypedExpr>,
    },
    FunctionCall {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
        args_span: Span,
    },
    Member { // something::member
        receiver: Box<TypedExpr>,
        name: OwnedToken,
    },
    Receiver { // something.receiver_template
        receiver: Box<TypedExpr>,
        name: OwnedToken,
    },
    Index {
        receiver: Box<TypedExpr>,
        index: Box<TypedExpr>,
        operator_span: Span,
    },
    BuiltinFunctionCall {
        name: OwnedToken,
        args: Vec<TypedExpr>,
        args_span: Span,
    },
    BuiltinType(TypeId),
    TypeCast {
        expr: Box<TypedExpr>,
        operator_span: Span,
        to: TypeId,
    },

    Object {
        fields: Vec<(OwnedToken, TypedExpr)>,
        merge_expr: Option<Box<TypedExpr>>,
        span: Span,
    },
    Array {
        elements: Vec<TypedExpr>,
        span: Span,
    },

    Error,
}
