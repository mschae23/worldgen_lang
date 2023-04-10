use crate::compiler::error::span::Span;
use crate::compiler::name::TypeId;

pub type DeclId = usize;

#[derive(Debug)]
pub struct ForwardDeclStorage {
}

#[derive(Debug)]
pub struct ForwardClassDecl {
    pub type_id: TypeId, pub name_span: Span,
    pub parameters: Vec<TypeId>,
}

#[derive(Debug)]
pub struct ForwardTypeAliasDecl {
    pub type_id: TypeId,
    pub reference: TypeId,
}

#[derive(Debug)]
pub struct ForwardTemplateDecl {
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug)]
pub struct ForwardConversionDecl {
    pub from: TypeId,
    pub to: TypeId,
}

#[derive(Debug)]
pub struct ForwardOptimizeDecl {
    pub on: TypeId,
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug)]
pub enum ForwardDecl {
    Class(ForwardClassDecl),
    TypeAlias(ForwardTypeAliasDecl),
    Template(ForwardTemplateDecl),
    Conversion(ForwardConversionDecl),
    Optimize(ForwardOptimizeDecl),
}
