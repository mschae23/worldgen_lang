use std::fmt::Debug;

use non_empty_vec::NonEmpty;

use crate::compiler::ast::forward::DeclId;
use crate::compiler::ast::simple::VariableKind;
use crate::compiler::error::FileId;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::{Token, TokenType};
use crate::compiler::name;
use crate::compiler::name::{NameResolution, TypeId};
use crate::compiler::type_checker::hint::TypeHint;

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
}

impl TypedDecl {
    pub fn name_span(&self) -> SpanWithFile {
        match self {
            TypedDecl::Class { name, .. } => SpanWithFile::new(name.file_id, name.span),
            TypedDecl::TypeAlias { name, .. } => SpanWithFile::new(name.file_id, name.span),
            TypedDecl::Variable { name, .. } => SpanWithFile::new(name.file_id, name.span),
        }
    }
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
        key_span: SpanWithFile,
        condition: TypedExpr,
        then: Box<TypedTemplateExpr>,
        otherwise: Box<TypedTemplateExpr>,
        condition_span: Span,
    },
    Simple(TypedExpr),
}

impl TypedTemplateExpr {
    pub fn type_id(&self) -> TypeId {
        match self {
            TypedTemplateExpr::Block { expressions, .. } => expressions.last().type_id(),
            TypedTemplateExpr::If { then, .. } => then.type_id(),
            TypedTemplateExpr::Simple(expr) => expr.type_id(),
        }
    }

    fn convert_to_type(self, to: &[TypeId], names: &NameResolution) -> Result<TypedTemplateExpr, TypedTemplateExpr> {
        let from = self.type_id();

        if to.contains(&from) {
            return Ok(self);
        }

        match self {
            TypedTemplateExpr::Block { expressions, span, } => {
                let mut expressions = <NonEmpty<_> as Into<Vec<_>>>::into(expressions);

                match expressions.swap_remove(expressions.len() - 1).convert_to_type(to, names) {
                    Ok(last) => {
                        Ok(TypedTemplateExpr::Block { expressions: (expressions, last).into(), span, })
                    },
                    Err(last) => {
                        Err(TypedTemplateExpr::Block { expressions: (expressions, last).into(), span, })
                    },
                }
            },
            TypedTemplateExpr::If { key_span, condition, then, otherwise, condition_span } => {
                match then.convert_to_type(to, names) {
                    Ok(then) => match otherwise.convert_to_type(to, names) {
                        Ok(otherwise) => Ok(TypedTemplateExpr::If { key_span, condition, then: Box::new(then), otherwise: Box::new(otherwise), condition_span }),
                        Err(otherwise) => Err(TypedTemplateExpr::If { key_span, condition, then: Box::new(then), otherwise: Box::new(otherwise), condition_span }),
                    },
                    Err(then) => Err(TypedTemplateExpr::If { key_span, condition, then: Box::new(then), otherwise, condition_span }),
                }
            },
            TypedTemplateExpr::Simple(expr) => match expr.convert_to_type(to, names) {
                Ok(expr) => Ok(TypedTemplateExpr::Simple(expr)),
                Err(expr) => Err(TypedTemplateExpr::Simple(expr)),
            },
        }
    }

    pub fn convert_to(self, to: &TypeHint, names: &NameResolution) -> Result<TypedTemplateExpr, TypedTemplateExpr> {
        match to {
            TypeHint::Options(options) => self.convert_to_type(options, names),
            TypeHint::Any => Ok(self),
        }
    }
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

    // Type conversions
    ConvertIntToFloat {
        from: Box<TypedExpr>,
    },
    ConvertToString {
        from: Box<TypedExpr>,
    },

    Error,
}

impl TypedExpr {
    pub fn type_id(&self) -> TypeId {
        match self {
            TypedExpr::ConstantInt(_, _) => name::INT_TYPE_ID,
            TypedExpr::ConstantFloat(_, _) => name::FLOAT_TYPE_ID,
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
            TypedExpr::ConvertIntToFloat { .. } => name::FLOAT_TYPE_ID,
            TypedExpr::ConvertToString { .. } => name::STRING_TYPE_ID,
            TypedExpr::Error => name::ERROR_TYPE_ID,
        }
    }

    fn convert_to_type(self, to: &[TypeId], _names: &NameResolution) -> Result<TypedExpr, TypedExpr> {
        let from = self.type_id();

        if to.contains(&from) {
            return Ok(self);
        } else if from == name::INT_TYPE_ID && to.contains(&name::FLOAT_TYPE_ID) {
            return Ok(TypedExpr::ConvertIntToFloat { from: Box::new(self), });
        } else if to.contains(&name::STRING_TYPE_ID) && name::CONVERTIBLE_TO_STRING.contains(&from) {
            return Ok(TypedExpr::ConvertToString { from: Box::new(self), });
        }

        // TODO lookup type conversion templates

        if from == name::ERROR_TYPE_ID {
            return Ok(TypedExpr::Error); // Error should've already been reported
        }

        Err(self)
    }

    pub fn convert_to(self, to: &TypeHint, names: &NameResolution) -> Result<TypedExpr, TypedExpr> {
        match to {
            TypeHint::Options(options) => {
                self.convert_to_type(options, names)
            },
            TypeHint::Any => Ok(self),
        }
    }
}
