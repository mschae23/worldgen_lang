use std::collections::HashMap;
use non_empty_vec::{ne_vec, NonEmpty};
use crate::compiler::ast::simple::{ClassReprPart, Expr, TemplateExpr, TemplateKind, TypeReferencePart, VariableKind};
use crate::compiler::error::FileId;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::Token;
use crate::compiler::name;
use crate::compiler::name::{TypeId, TypeStorage};
#[allow(unused)]
use crate::println_debug;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DeclId(usize);

#[derive(Debug)]
pub struct ForwardDeclStorage {
    type_id_offset: usize, decl_id_offset: usize,
    declarations: Vec<ForwardDecl>,
    declaration_spans: Vec<SpanWithFile>,
    declaration_has_duplicate: Vec<bool>,
    top_level_module: ForwardModule,

    type_to_decl_mapping: Vec<TypeToDeclMapping>,
}

impl ForwardDeclStorage {
    pub fn new(type_id_offset: usize, decl_id_offset: usize, type_count: usize) -> Self {
        let mut type_to_decl_mapping = vec![TypeToDeclMapping::Unknown; type_count];
        type_to_decl_mapping[0..name::PRIMITIVE_TYPE_COUNT].fill(TypeToDeclMapping::Primitive);

        ForwardDeclStorage {
            type_id_offset, decl_id_offset,
            declarations: Vec::new(),
            declaration_spans: Vec::new(),
            declaration_has_duplicate: Vec::new(),
            top_level_module: ForwardModule::new(),
            type_to_decl_mapping,
        }
    }

    #[inline]
    fn next_decl_id(&self) -> DeclId {
        DeclId(self.declarations.len() + self.decl_id_offset)
    }

    #[inline]
    fn type_id_to_array_index(&self, id: TypeId) -> usize {
        if id.0 < name::PRIMITIVE_TYPE_COUNT {
            id.0
        } else {
            id.0 - self.type_id_offset
        }
    }

    #[inline]
    pub fn get_decl_count(&self) -> usize {
        self.declarations.len()
    }

    pub fn declarations(&self) -> &[ForwardDecl] {
        &self.declarations
    }

    pub fn get_decl_by_id(&self, id: DeclId) -> &ForwardDecl {
        &self.declarations[id.0 - self.decl_id_offset]
    }

    pub fn get_span_by_id(&self, id: DeclId) -> SpanWithFile {
        self.declaration_spans[id.0 - self.decl_id_offset]
    }

    pub fn get_decl_id_from_type_id(&self, id: TypeId) -> Option<DeclId> {
        if let TypeToDeclMapping::Forward(decl_id) = self.type_to_decl_mapping[self.type_id_to_array_index(id)] {
            Some(decl_id)
        } else {
            None
        }
    }

    pub fn get_decl_by_type_id(&self, id: TypeId) -> Option<&ForwardDecl> {
        self.get_decl_id_from_type_id(id).map(|id| self.get_decl_by_id(id))
    }

    pub fn has_duplicate(&self, id: DeclId) -> bool {
        self.declaration_has_duplicate[id.0 - self.decl_id_offset]
    }

    pub fn top_level_module(&self) -> &ForwardModule {
        &self.top_level_module
    }

    fn find_module_mut<'a, P: IntoIterator<Item=&'a str>>(&mut self, path: P) -> &mut ForwardModule {
        let mut module = &mut self.top_level_module;

        for component in path.into_iter() {
            module = module.sub_modules.entry(component.to_owned()).or_default();
        }

        module
    }

    fn find_module<'a, 'b, P: IntoIterator<Item=&'b (&'a str, Span)>>(&self, path: P) -> Result<&ForwardModule, (&'a str, Span)> where 'a: 'b {
        let mut module = &self.top_level_module;

        for &(component, span) in path.into_iter() {
            module = module.sub_modules.get(component).ok_or((component, span))?;
        }

        Ok(module)
    }

    pub fn insert(&mut self, path: &[&str], decl: ForwardDecl, file_id: FileId, span: Span) -> DeclId {
        assert!(!path.is_empty(), "Cannot insert forward declaration without a name");

        let id: DeclId = self.next_decl_id();
        self.declarations.push(decl);
        self.declaration_spans.push(SpanWithFile::new(file_id, span));
        self.declaration_has_duplicate.push(false);

        let name = *path.last().expect("Path has no last element despite previous check");
        let module = self.find_module_mut(path.iter().take(path.len().saturating_sub(1)).copied());

        module.declarations.push((name.to_owned(), id));
        id
    }

    pub fn insert_with_type(&mut self, path: &[&str], type_id: TypeId, decl: ForwardDecl, file_id: FileId, span: Span) -> DeclId {
        let decl_id = self.insert(path, decl, file_id, span);
        self.insert_type_to_decl_mapping(type_id, decl_id);
        decl_id
    }

    pub fn mark_duplicate(&mut self, original_id: DeclId) {
        self.declaration_has_duplicate[original_id.0 - self.decl_id_offset] = true;
    }

    pub fn insert_type_to_decl_mapping(&mut self, type_id: TypeId, decl_id: DeclId) {
        let index = self.type_id_to_array_index(type_id);
        self.type_to_decl_mapping[index] = TypeToDeclMapping::Forward(decl_id);
    }

    pub fn find_decl_direct<'a, F: Fn(&str, &ForwardDecl) -> bool>(&self, module_path: &[(&'a str, Span)], predicate: F, name_debug_info: (&'a str, Span)) -> Result<&ForwardDecl, (&'a str, Span)> {
        let module = self.find_module(module_path)?;

        for (name, id) in module.declarations.iter() {
            let id = *id;
            let decl = &self.declarations[id.0 - self.decl_id_offset];

            if predicate(name, decl) {
                return Ok(decl);
            }
        }

        Err(name_debug_info)
    }

    pub fn find_decl_id_for_duplicate<F: Fn(&str, &ForwardDecl) -> bool>(&self, prefix: &[&str], module_path: &[&str], predicate: F) -> Option<DeclId> {
        // Keep in sync with TypeStorage::get_type_id_by_type_reference_part
        let mut module = &self.top_level_module;

        for &component in prefix.iter() {
            if let Some(sub_module) = module.sub_modules.get(component) {
                module = sub_module;
            } else { // if !recurse_to_parent {
                return None;
            } /* else {
                // There is the possibility that no types have been defined in the current module,
                // but in outer ones. Just go as far as possible, and search there.
                break;
            } */
        }

        let mut current = module;

        for &component in module_path.iter() {
            if let Some(sub_module) = current.sub_modules.get(component) {
                current = sub_module;
            } else {
                return None;
            }
        }

        for (name, id) in current.declarations.iter() {
            let id = *id;
            let decl = &self.declarations[id.0 - self.decl_id_offset];

            if predicate(name, decl) {
                return Some(id);
            }
        }

        None
    }

    pub fn find_decl_id<F: Fn(&str, &ForwardDecl) -> bool>(&self, prefix: &[&str], module_path: &[(&str, Span)], predicate: F, span: Span) -> Result<DeclId, Span> {
        // Keep in sync with TypeStorage::get_type_id_by_type_reference_part
        let mut module_stack = ne_vec![&self.top_level_module];

        for &component in prefix.iter() {
            if let Some(sub_module) = module_stack.last().sub_modules.get(component) {
                module_stack.push(sub_module);
            } else { // if recurse_to_parent {
                // There is the possibility that no types have been defined in the current module,
                // but in outer ones. Just go as far as possible, and search there.
                break;
            }
        }

        'outer:
        for module in module_stack.iter().copied().rev() {
            let mut current = module;

            for (i, (component, component_span)) in module_path.iter().enumerate() {
                if let Some(sub_module) = current.sub_modules.get(*component) {
                    current = sub_module;
                } else if i == 0 {
                    // If this is the first component that is not found in this module, keep
                    // searching in the parent module.
                    continue 'outer;
                } else {
                    return Err(*component_span);
                }
            }

            for (name, id) in current.declarations.iter() {
                let id = *id;
                let decl = &self.declarations[id.0 - self.decl_id_offset];

                if predicate(name, decl) {
                    return Ok(id);
                }
            }
        }

        Err(span)
    }

    pub fn include(&mut self, mut other: ForwardDeclStorage) {
        self.declarations.append(&mut other.declarations);
        self.declaration_spans.append(&mut other.declaration_spans);
        self.declaration_has_duplicate.append(&mut other.declaration_has_duplicate);
        self.top_level_module.merge(other.top_level_module);
        self.type_to_decl_mapping.append(&mut other.type_to_decl_mapping);
    }

    pub fn assert_complete_type_mappings(&self) {
        if self.type_to_decl_mapping.iter().any(|mapping| matches!(mapping, TypeToDeclMapping::Unknown)) {
            eprintln!("[debug] incomplete mappings from type ID to forward decl ID");
        }
    }
}

pub struct ForwardDeclareResult<'source> {
    pub types: TypeStorage,
    pub storage: ForwardDeclStorage,
    pub declarations: Vec<ForwardDeclaredDecl<'source>>,
}

impl<'source> ForwardDeclareResult<'source> {
    pub fn new(types: TypeStorage, storage: ForwardDeclStorage, declarations: Vec<ForwardDeclaredDecl<'source>>) -> Self {
        ForwardDeclareResult {
            types, storage, declarations,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardClassDecl {
    pub key_span: SpanWithFile,
    pub type_id: TypeId, pub name_span: SpanWithFile,
    pub interface: bool,
    pub parameters: Vec<TypeId>,
    pub implements: Option<TypeId>,
    pub repr: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardTypeAliasDecl {
    pub key_span: SpanWithFile,
    pub type_id: TypeId, pub name_span: SpanWithFile,
    pub reference: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardTemplateDecl {
    pub key_span: SpanWithFile, pub name_span: SpanWithFile,
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardConversionDecl {
    pub key_span: SpanWithFile,
    pub from: TypeId,
    pub to: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardOptimizeDecl {
    pub key_span: SpanWithFile,
    pub on: TypeId,
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardVariableDecl {
    pub key_span: SpanWithFile, pub name_span: SpanWithFile,
    // Can't put expr type here, because of type inference,
    // which happens later
    pub kind: VariableKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForwardDecl {
    Class(ForwardClassDecl),
    TypeAlias(ForwardTypeAliasDecl),
    Template(ForwardTemplateDecl),
    Conversion(ForwardConversionDecl),
    Optimize(ForwardOptimizeDecl),
    Variable(ForwardVariableDecl),
}

impl ForwardDecl {
    pub fn kind(&self) -> &'static str {
        match self {
            ForwardDecl::Class(_) => "class",
            ForwardDecl::TypeAlias(_) => "type alias",
            ForwardDecl::Template(_) => "template",
            ForwardDecl::Conversion(_) => "type conversion",
            ForwardDecl::Optimize(_) => "optimization",
            ForwardDecl::Variable(_) => "variable",
        }
    }

    pub fn kind_with_indefinite_article(&self) -> &'static str {
        match self {
            ForwardDecl::Class(_) => "a class",
            ForwardDecl::TypeAlias(_) => "a type alias",
            ForwardDecl::Template(_) => "a template",
            ForwardDecl::Conversion(_) => "a type conversion",
            ForwardDecl::Optimize(_) => "an optimization",
            ForwardDecl::Variable(_) => "a variable",
        }
    }

    pub fn key_span(&self) -> SpanWithFile {
        match self {
            ForwardDecl::Class(decl) => decl.key_span,
            ForwardDecl::TypeAlias(decl) => decl.key_span,
            ForwardDecl::Template(decl) => decl.key_span,
            ForwardDecl::Conversion(decl) => decl.key_span,
            ForwardDecl::Optimize(decl) => decl.key_span,
            ForwardDecl::Variable(decl) => decl.key_span,
        }
    }

    pub fn span(&self) -> SpanWithFile {
        match self {
            ForwardDecl::Class(decl) => decl.name_span,
            ForwardDecl::TypeAlias(decl) => decl.name_span,
            ForwardDecl::Template(decl) => decl.name_span,
            ForwardDecl::Conversion(decl) => decl.key_span,
            ForwardDecl::Optimize(decl) => decl.key_span,
            ForwardDecl::Variable(decl) => decl.name_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardModule {
    pub sub_modules: HashMap<String, ForwardModule>,
    pub declarations: Vec<(String, DeclId)>,
}

impl ForwardModule {
    pub fn new() -> Self {
        ForwardModule {
            sub_modules: HashMap::new(),
            declarations: Vec::new(),
        }
    }

    pub fn merge(&mut self, mut other: ForwardModule) {
        for (name, sub_module) in other.sub_modules.into_iter() {
            self.sub_modules.entry(name).or_default().merge(sub_module);
        }

        self.declarations.append(&mut other.declarations);
    }
}

impl Default for ForwardModule {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeToDeclMapping {
    Unknown,
    Primitive,
    Forward(DeclId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForwardDeclaredParameterPart<'source> {
    pub name: Token<'source>,
    pub parameter_type: TypeId,
    pub type_span: SpanWithFile,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForwardDeclaredClassImplementsPart<'source> {
    pub name: TypeReferencePart<'source>,
    pub implements_id: TypeId,
    pub parameters: Vec<Expr<'source>>,
    pub span: Span, pub parameter_span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForwardDeclaredDecl<'source> {
    Module {
        key_span: SpanWithFile,
        name: Token<'source>,
        declarations: Vec<ForwardDeclaredDecl<'source>>,
    },
    Class {
        key_span: SpanWithFile, decl_id: DeclId,
        interface: bool,
        name: Token<'source>,
        parameters: Vec<ForwardDeclaredParameterPart<'source>>,
        implements: Option<ForwardDeclaredClassImplementsPart<'source>>,
        class_repr: Option<ClassReprPart<'source>>,
        parameter_span: Span,
    },
    TypeAlias {
        key_span: SpanWithFile, decl_id: DeclId,
        name: Token<'source>,
        to: TypeId,
        condition: Option<(Expr<'source>, Span)>,
        to_span: SpanWithFile,
    },
    Template {
        key_span: SpanWithFile, decl_id: DeclId,
        kind: TemplateKind<'source>,
        parameters: Vec<ForwardDeclaredParameterPart<'source>>,
        return_type: TypeId,
        expr: TemplateExpr<'source>,
        parameter_span: Span, return_type_span: Span, expr_span: Span,
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
        key_span: SpanWithFile, decl_id: DeclId,
        kind: VariableKind,
        name: Token<'source>,
        expr: Expr<'source>,
        span: Span,
    },

    Error,
}
