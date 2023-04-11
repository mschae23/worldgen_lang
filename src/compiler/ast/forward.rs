use std::collections::HashMap;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::span::Span;
use crate::compiler::name::{TypeId, TypeStorage};

pub type DeclId = usize;

#[derive(Debug)]
pub struct ForwardDeclStorage {
    declarations: Vec<ForwardDecl>,
    top_level_module: ForwardModule,
}

impl ForwardDeclStorage {
    pub fn new() -> Self {
        ForwardDeclStorage {
            declarations: Vec::new(),
            top_level_module: ForwardModule::new(),
        }
    }

    pub fn declarations(&self) -> &[ForwardDecl] {
        &self.declarations
    }

    pub fn get_decl_by_id(&self, id: DeclId) -> &ForwardDecl {
        &self.declarations[id]
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

    pub fn insert(&mut self, path: &[&str], decl: ForwardDecl) {
        assert!(path.len() >= 1, "Cannot insert forward declaration without a name");

        let id: DeclId = self.declarations.len();
        self.declarations.push(decl);

        let name = *path.last().expect("Path has no last element despite previous check");
        let module = self.find_module_mut(path.iter().take(path.len().saturating_sub(1)).copied());

        module.declarations.push((name.to_owned(), id));
    }

    pub fn find_decl<'a, F: Fn(&str, &ForwardDecl) -> bool>(&self, module_path: &[(&'a str, Span)], predicate: F, name_debug_info: (&'a str, Span)) -> Result<&ForwardDecl, (&'a str, Span)> {
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
}

impl Default for ForwardDeclStorage {
    fn default() -> Self {
        Self::new()
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
    pub parameters: Vec<TypeId>,
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
pub enum ForwardDecl {
    Class(ForwardClassDecl),
    TypeAlias(ForwardTypeAliasDecl),
    Template(ForwardTemplateDecl),
    Conversion(ForwardConversionDecl),
    Optimize(ForwardOptimizeDecl),
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
