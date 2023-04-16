use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter};
use non_empty_vec::NonEmpty;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::Token;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveTypeKind {
    Int, Float, Boolean, String, Object, Array, Type,
}

impl Display for PrimitiveTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveTypeKind::Int => write!(f, "int"),
            PrimitiveTypeKind::Float => write!(f, "float"),
            PrimitiveTypeKind::Boolean => write!(f, "boolean"),
            PrimitiveTypeKind::String => write!(f, "string"),
            PrimitiveTypeKind::Object => write!(f, "object"),
            PrimitiveTypeKind::Array => write!(f, "array"),
            PrimitiveTypeKind::Type => write!(f, "type"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeReferencePart<'source>(pub NonEmpty<Token<'source>>, pub Span);

#[derive(Clone, PartialEq, Eq)]
pub enum TypePart<'source> {
    Primitive(PrimitiveTypeKind, Span),
    Name(TypeReferencePart<'source>),
    Template {
        args: Vec<TypePart<'source>>,
        return_type: Box<TypePart<'source>>,
        args_span: Span,
    },
}

impl<'source> TypePart<'source> {
    pub fn span(&self) -> Span {
        match self {
            TypePart::Primitive(_, span) => *span,
            TypePart::Name(name) => name.1,
            TypePart::Template { return_type, args_span, .. } => args_span.merge(return_type.span()),
        }
    }
}

impl<'source> Debug for TypePart<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(kind, _) => write!(f, "{}", kind),
            Self::Name(tokens) => write!(f, "{}", tokens.0.iter().map(|token| token.source()).collect::<Vec<_>>().join("::")),
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
pub struct ClassImplementsPart<'source> {
    pub name: TypeReferencePart<'source>,
    pub parameters: Vec<Expr<'source>>,
    pub span: Span, pub parameter_span: Span,
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

impl<'source> TemplateKind<'source> {
    pub fn span(&self) -> Span {
        match self {
            TemplateKind::Template { name } => name.span(),
            TemplateKind::Conversion { span } => *span,
            TemplateKind::Optimize { on } => on.span(),
        }
    }

    pub fn name_for_error(&self) -> Cow<'source, str> {
        match self {
            TemplateKind::Template { name } => Cow::Owned(format!("`{}`", name.source())),
            TemplateKind::Conversion { .. } => Cow::Borrowed("type conversion template"),
            TemplateKind::Optimize { .. } => Cow::Borrowed("optimization template"),
        }
    }
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
        key_span: SpanWithFile,
        name: Token<'source>,
        declarations: Vec<Decl<'source>>,
    },
    Class {
        key_span: SpanWithFile,
        interface: bool,
        name: Token<'source>,
        parameters: Vec<ParameterPart<'source>>,
        implements: Option<ClassImplementsPart<'source>>,
        class_repr: Option<ClassReprPart<'source>>,
        parameter_span: Span,
    },
    TypeAlias {
        key_span: SpanWithFile,
        name: Token<'source>,
        to: TypePart<'source>,
        condition: Option<Expr<'source>>,
    },
    Template {
        key_span: SpanWithFile,
        kind: TemplateKind<'source>,
        parameters: Vec<ParameterPart<'source>>,
        return_type: TypePart<'source>,
        expr: TemplateExpr<'source>,
        parameter_span: Span,
    },
    Include {
        key_span: SpanWithFile,
        path: Token<'source>,
    },
    Import {
        key_span: SpanWithFile,
        path: NonEmpty<Token<'source>>,
        selector: Option<NonEmpty<Token<'source>>>, // None represents star import (import something::*)
        span: Span, // only path and selector, doesn't include the "import" keyword or semicolon
    },
    Variable {
        key_span: SpanWithFile,
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
    ConstantInt(i32, Span),
    ConstantFloat(f64, Span),
    ConstantBoolean(bool, Span),
    ConstantString(String, Span),
    Identifier(Token<'source>),

    Replacement(Span), // `_` token as expression, for type alias conditions

    UnaryOperator {
        operator: Token<'source>,
        expr: Box<Expr<'source>>,
    },
    BinaryOperator {
        left: Box<Expr<'source>>,
        operator: Token<'source>,
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
        merge_expr: Option<(Box<Expr<'source>>, Span)>,
        span: Span,
    },
    Array {
        elements: Vec<Expr<'source>>,
        span: Span,
    },

    Error,
}
