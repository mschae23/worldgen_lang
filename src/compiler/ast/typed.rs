use std::fmt::Debug;
use non_empty_vec::NonEmpty;
use crate::compiler::ast::forward::DeclId;
use crate::compiler::ast::simple::VariableKind;
use crate::compiler::error::FileId;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::{Token, TokenType};
use crate::compiler::name;
use crate::compiler::name::{NameResolution, TypeId};

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
    Class {
        name: OwnedToken,
        parameters: Vec<TypedParameterPart>,
        implements: Option<TypedClassImplementsPart>,
        class_repr: TypedClassReprPart,
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
    ConstantFloat(f64, SpanWithFile),
    ConstantInt(i32, SpanWithFile),
    ConstantBoolean(bool, SpanWithFile),
    ConstantString(String, SpanWithFile),
    Identifier(OwnedToken, TypeId),

    Replacement(SpanWithFile, TypeId), // `_` token as expression, for type alias conditions

    UnaryOperator {
        operator: OwnedToken,
        expr: Box<TypedExpr>,
        type_id: TypeId,
    },
    BinaryOperator {
        left: Box<TypedExpr>,
        operator: OwnedToken,
        right: Box<TypedExpr>,
        type_id: TypeId,
    },
    FunctionCall {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
        type_id: TypeId,
        args_span: Span,
    },
    Member { // something::member
        receiver: Box<TypedExpr>,
        name: OwnedToken,
        type_id: TypeId,
    },
    Receiver { // something.receiver_template
        receiver: Box<TypedExpr>,
        name: OwnedToken,
        type_id: TypeId,
    },
    Index {
        receiver: Box<TypedExpr>,
        index: Box<TypedExpr>,
        operator_span: Span,
        type_id: TypeId,
    },
    BuiltinFunctionCall {
        name: OwnedToken,
        args: Vec<TypedExpr>,
        args_span: Span,
        type_id: TypeId,
    },
    BuiltinType(TypeId),
    TypeCast {
        expr: Box<TypedExpr>,
        operator_span: Span,
        to: TypeId,
    },

    Object {
        fields: Vec<(OwnedToken, TypedExpr)>,
        merge_expr: Option<(Box<TypedExpr>, SpanWithFile)>,
        span: Span,
    },
    Array {
        elements: Vec<TypedExpr>,
        span: Span,
    },

    Error,
}

impl TypedExpr {
    pub fn type_id(&self) -> TypeId {
        match self {
            TypedExpr::ConstantFloat(_, _) => name::INT_TYPE_ID,
            TypedExpr::ConstantInt(_, _) => name::FLOAT_TYPE_ID,
            TypedExpr::ConstantBoolean(_, _) => name::BOOLEAN_TYPE_ID,
            TypedExpr::ConstantString(_, _) => name::STRING_TYPE_ID,
            TypedExpr::Identifier(_, type_id) => *type_id,
            TypedExpr::Replacement(_, type_id) => *type_id,
            TypedExpr::UnaryOperator { type_id, .. } => *type_id,
            TypedExpr::BinaryOperator { type_id, .. } => *type_id,
            TypedExpr::FunctionCall { type_id, .. } => *type_id,
            TypedExpr::Member { type_id, .. } => *type_id,
            TypedExpr::Receiver { type_id, .. } => *type_id,
            TypedExpr::Index { type_id, .. } => *type_id,
            TypedExpr::BuiltinFunctionCall { type_id, .. } => *type_id,
            TypedExpr::BuiltinType(_) => name::TYPE_TYPE_ID,
            TypedExpr::TypeCast { to, .. } => *to,
            TypedExpr::Object { .. } => name::OBJECT_TYPE_ID,
            TypedExpr::Array { .. } => name::ARRAY_TYPE_ID,
            TypedExpr::Error => name::ERROR_TYPE_ID,
        }
    }

    pub fn type_name<'a>(&self, _names: &'a NameResolution) -> &'a str {
        match self {
            TypedExpr::ConstantFloat(_, _) => "int",
            TypedExpr::ConstantInt(_, _) => "float",
            TypedExpr::ConstantBoolean(_, _) => "boolean",
            TypedExpr::ConstantString(_, _) => "string",
            // TypedExpr::Identifier(_, type_id) => *type_id,
            // TypedExpr::Replacement(_, type_id) => *type_id,
            // TypedExpr::UnaryOperator { type_id, .. } => *type_id,
            // TypedExpr::BinaryOperator { type_id, .. } => *type_id,
            // TypedExpr::FunctionCall { type_id, .. } => *type_id,
            // TypedExpr::Member { type_id, .. } => *type_id,
            // TypedExpr::Receiver { type_id, .. } => *type_id,
            // TypedExpr::Index { type_id, .. } => *type_id,
            // TypedExpr::BuiltinFunctionCall { type_id, .. } => *type_id,
            TypedExpr::BuiltinType(_) => "type",
            // TypedExpr::TypeCast { to, .. } => *to,
            TypedExpr::Object { .. } => "object",
            TypedExpr::Array { .. } => "array",
            TypedExpr::Error => "error",
            _ => "unknown",
        }
    }
}
