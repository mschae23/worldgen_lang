use std::collections::HashMap;
use non_empty_vec::ne_vec;
use crate::compiler::ast::simple::{Decl, VariableKind};
use crate::compiler::error::FileId;
use crate::compiler::error::span::Span;
use crate::compiler::name::{PRIMITIVE_TYPE_COUNT, TypeId, TypeStorage};

pub type DeclId = usize;

#[derive(Debug)]
pub struct ForwardDeclStorage {
    declarations: Vec<ForwardDecl>,
    declaration_spans: Vec<(FileId, Span)>,
    declaration_has_duplicate: Vec<bool>,
    top_level_module: ForwardModule,

    type_to_decl_mapping: Vec<TypeToDeclMapping>,
}

impl ForwardDeclStorage {
    pub fn new(type_count: usize) -> Self {
        let mut type_to_decl_mapping = vec![TypeToDeclMapping::Unknown; type_count];
        type_to_decl_mapping[0..PRIMITIVE_TYPE_COUNT].fill(TypeToDeclMapping::Primitive);

        ForwardDeclStorage {
            declarations: Vec::new(),
            declaration_spans: Vec::new(),
            declaration_has_duplicate: Vec::new(),
            top_level_module: ForwardModule::new(),
            type_to_decl_mapping,
        }
    }

    pub fn declarations(&self) -> &[ForwardDecl] {
        &self.declarations
    }

    pub fn get_decl_by_id(&self, id: DeclId) -> &ForwardDecl {
        &self.declarations[id]
    }

    pub fn get_span_by_id(&self, id: DeclId) -> (FileId, Span) {
        self.declaration_spans[id]
    }

    pub fn get_decl_id_from_type_id(&self, id: TypeId) -> Option<DeclId> {
        if let TypeToDeclMapping::Forward(decl_id) = self.type_to_decl_mapping[id] {
            Some(decl_id)
        } else {
            None
        }
    }

    pub fn get_decl_by_type_id(&self, id: TypeId) -> Option<&ForwardDecl> {
        self.get_decl_id_from_type_id(id).map(|id| self.get_decl_by_id(id))
    }

    pub fn has_duplicate(&self, id: DeclId) -> bool {
        self.declaration_has_duplicate[id]
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

        let id: DeclId = self.declarations.len();
        self.declarations.push(decl);
        self.declaration_spans.push((file_id, span));
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
        self.declaration_has_duplicate[original_id] = true;
    }

    pub fn insert_type_to_decl_mapping(&mut self, type_id: TypeId, decl_id: DeclId) {
        self.type_to_decl_mapping[type_id] = TypeToDeclMapping::Forward(decl_id);
    }

    pub fn find_decl_direct<'a, F: Fn(&str, &ForwardDecl) -> bool>(&self, module_path: &[(&'a str, Span)], predicate: F, name_debug_info: (&'a str, Span)) -> Result<&ForwardDecl, (&'a str, Span)> {
        let module = self.find_module(module_path)?;

        for (name, id) in module.declarations.iter() {
            let id = *id;
            let decl = &self.declarations[id];

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
            let decl = &self.declarations[id];

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
                let decl = &self.declarations[id];

                if predicate(name, decl) {
                    return Ok(id);
                }
            }
        }

        Err(span)
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
    pub declarations: Vec<Decl<'source>>,
}

impl<'source> ForwardDeclareResult<'source> {
    pub fn new(types: TypeStorage, storage: ForwardDeclStorage, declarations: Vec<Decl<'source>>) -> Self {
        ForwardDeclareResult {
            types, storage, declarations,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardClassDecl {
    pub type_id: TypeId, pub name_span: Span,
    pub interface: bool,
    pub parameters: Vec<TypeId>,
    pub implements: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardTypeAliasDecl {
    pub type_id: TypeId,
    pub reference: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardTemplateDecl {
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardConversionDecl {
    pub from: TypeId,
    pub to: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardOptimizeDecl {
    pub on: TypeId,
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardVariableDecl {
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
