use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use non_empty_vec::{ne_vec, NonEmpty};
use crate::compiler::ast::simple::{PrimitiveTypeKind, TypePart, TypeReferencePart};
use crate::compiler::ast::typed::TypedDecl;
use crate::compiler::error::FileId;
use crate::compiler::error::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PositionedName {
    pub source: String,
    pub file_id: FileId,
    pub span: Span,
}

impl PositionedName {
    pub fn new(source: String, file_id: FileId, span: Span) -> Self {
        PositionedName {
            source, file_id, span,
        }
    }
}

pub type TypeId = usize;

#[derive(Debug)]
pub enum SimpleTypeInfo {
    Primitive {
        kind: PrimitiveTypeKind,
    },
    Class {
        interface: bool,
    },
    TypeAlias,
    Template,
    Error,
}

pub const INT_TYPE_ID: TypeId = 0;
pub const FLOAT_TYPE_ID: TypeId = 1;
pub const BOOLEAN_TYPE_ID: TypeId = 2;
pub const STRING_TYPE_ID: TypeId = 3;
pub const OBJECT_TYPE_ID: TypeId = 4;
pub const ARRAY_TYPE_ID: TypeId = 5;
pub const TYPE_TYPE_ID: TypeId = 6;
pub const ERROR_TYPE_ID: TypeId = 7;
pub const PRIMITIVE_TYPE_COUNT: usize = 8;

#[derive(Debug)]
pub struct TypeStorage {
    types: Vec<SimpleTypeInfo>,
    top_level_module: TypeModule,
    type_path_lookup: Vec<Option<PathBuf>>,
    type_span_lookup: Vec<Option<(FileId, Span)>>,
}

impl TypeStorage {
    pub fn new() -> Self {
        TypeStorage {
            types: vec![
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Int },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Float },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Boolean },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::String },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Object },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Array },
                SimpleTypeInfo::Primitive { kind: PrimitiveTypeKind::Type },
                SimpleTypeInfo::Error,
            ],
            top_level_module: TypeModule::new(),
            type_path_lookup: Vec::new(),
            type_span_lookup: Vec::new(),
        }
    }

    #[inline]
    pub fn get(&self, type_id: TypeId) -> Option<&SimpleTypeInfo> {
        self.types.get(type_id)
    }

    #[inline]
    pub fn get_type_count(&self) -> usize {
        self.types.len()
    }

    pub fn get_path(&self, type_id: TypeId) -> Option<&Path> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            use std::borrow::Borrow;
            self.type_path_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).and_then(|path| path.as_ref().map(PathBuf::borrow))
        }
    }

    pub fn get_type_id_by_path(&self, path: &[&str]) -> Option<TypeId> {
        assert!(!path.is_empty(), "Cannot get type ID for empty path");

        let mut module = &self.top_level_module;

        for &component in path.iter().take(path.len().saturating_sub(1)) {
            module = module.sub_modules.get(component)?;
        }

        module.types.get(path.last().copied().expect("Empty path despite previous check")).copied()
    }

    pub fn get_type_id_by_type_part<'source>(&mut self, prefix: &[&str], part: &TypePart<'source>, file_id: FileId) -> Result<TypeId, (&'source str, Span)> {
        match part {
            TypePart::Primitive(kind, _) => Ok(match kind {
                PrimitiveTypeKind::Int => INT_TYPE_ID,
                PrimitiveTypeKind::Float => FLOAT_TYPE_ID,
                PrimitiveTypeKind::Boolean => BOOLEAN_TYPE_ID,
                PrimitiveTypeKind::String => STRING_TYPE_ID,
                PrimitiveTypeKind::Object => OBJECT_TYPE_ID,
                PrimitiveTypeKind::Array => ARRAY_TYPE_ID,
                PrimitiveTypeKind::Type => TYPE_TYPE_ID,
            }),
            TypePart::Name(name) => self.get_type_id_by_type_reference_part(prefix, name),
            TypePart::Template { args_span, return_type, .. } => {
                let id = self.types.len();
                self.types.push(SimpleTypeInfo::Template);
                self.type_path_lookup.push(None);
                self.type_span_lookup.push(Some((file_id, args_span.mix(return_type.span()))));
                Ok(id)
            },
        }
    }

    pub fn get_type_id_by_type_reference_part<'source>(&self, prefix: &[&str], part: &TypeReferencePart<'source>) -> Result<TypeId, (&'source str, Span)> {
        // Keep in sync with ForwardDeclStorage::find_decl_id
        let mut module_stack = ne_vec![&self.top_level_module];

        for &component in prefix.iter() {
            if let Some(sub_module) = module_stack.last().sub_modules.get(component) {
                module_stack.push(sub_module);
            } else {
                // There is the possibility that no types have been defined in the current module,
                // but in outer ones. Just go as far as possible, and search there.
                break;
            }
        }

        'outer:
        for module in module_stack.iter().copied().rev() {
            let mut current = module;

            for (i, component) in part.0.iter().take(<NonZeroUsize as Into<usize>>::into(part.0.len()).saturating_sub(1)).enumerate() {
                if let Some(sub_module) = current.sub_modules.get(component.source()) {
                    current = sub_module;
                } else if i == 0 {
                    // If this is the first component that is not found in this module, keep
                    // searching in the parent module.
                    continue 'outer;
                } else {
                    return Err((component.source(), component.span()));
                }
            }

            if let Some(&id) = current.types.get(part.0.last().source()) {
                return Ok(id);
            } else if <NonZeroUsize as Into<usize>>::into(part.0.len()) == 1 {
                // If the part has only this component, it will be the first one,
                // so just continue searching.
                continue 'outer;
            } else {
                return Err((part.0.last().source(), part.0.last().span()));
            }
        }

        let first = part.0.first();
        Err((first.source(), first.span()))
    }

    pub fn get_span(&self, type_id: TypeId) -> Option<(FileId, Span)> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_span_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).and_then(|o| o.as_ref()).copied()
        }
    }

    pub fn insert(&mut self, path: &[&str], span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> Result<TypeId, TypeId> {
        assert!(!path.is_empty(), "Cannot insert a type with an empty path");

        let id: TypeId = self.types.len();

        let mut module = &mut self.top_level_module;

        for &component in path.iter().take(path.len().saturating_sub(1)) {
            module = module.sub_modules.entry(component.to_owned()).or_default();
        }

        match module.types.entry(path.last().copied().expect("Empty path despite previous check").to_owned()) {
            Entry::Occupied(entry) => return Err(*entry.get()),
            Entry::Vacant(entry) => entry.insert(id),
        };

        self.types.push(type_info);
        self.type_path_lookup.push(Some(path.iter().collect()));
        self.type_span_lookup.push(Some((file_id, span)));

        Ok(id)
    }
}

impl Default for TypeStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct TypeModule {
    pub sub_modules: HashMap<String, TypeModule>,
    pub types: HashMap<String, TypeId>,
}

impl TypeModule {
    pub fn new() -> Self {
        TypeModule {
            sub_modules: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

impl Default for TypeModule {
    fn default() -> Self {
        Self::new()
    }
}

pub type EnvironmentId = usize;

pub const GLOBAL_ENVIRONMENT_ID: EnvironmentId = 0;
pub const GLOBAL_IMPORT_ENVIRONMENT_ID: EnvironmentId = 1;

#[derive(Debug)]
pub struct TypeEnvironment {
    pub sub_environments: HashMap<String, EnvironmentId>,
    pub declarations: HashMap<String, TypedDecl>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            sub_environments: HashMap::new(),
            declarations: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct NameResolution {
    // Type environments always occupy 2 IDs and 2 slots in the stack:
    // a normal type environment and an import type environment
    type_environments: NonEmpty<TypeEnvironment>, // by ID
    type_environment_stack: NonEmpty<EnvironmentId>,
}

impl NameResolution {
    pub fn new() -> Self {
        NameResolution {
            type_environments: ne_vec![TypeEnvironment::new(), TypeEnvironment::new()],
            type_environment_stack: NonEmpty::new(GLOBAL_ENVIRONMENT_ID),
        }
    }

    #[inline]
    pub fn get_type_environment_by_id(&self, id: EnvironmentId) -> &TypeEnvironment {
        &self.type_environments[id]
    }

    #[inline]
    pub fn get_type_environment_by_id_mut(&mut self, id: EnvironmentId) -> &mut TypeEnvironment {
        &mut self.type_environments[id]
    }

    #[inline]
    pub fn get_current_type_environment(&self) -> &TypeEnvironment {
        self.get_type_environment_by_id(*self.type_environment_stack.last())
    }

    #[inline]
    pub fn get_current_type_environment_mut(&mut self) -> &mut TypeEnvironment {
        self.get_type_environment_by_id_mut(*self.type_environment_stack.last())
    }

    #[inline]
    pub fn get_current_import_environment(&self) -> &TypeEnvironment {
        assert!(self.is_normal_environment(*self.type_environment_stack.last()), "An import environment is on the type environment stack");

        self.get_type_environment_by_id(*self.type_environment_stack.last() + 1)
    }

    #[inline]
    pub fn is_normal_environment(&self, id: EnvironmentId) -> bool {
        id % 2 == 0
    }

    #[inline]
    pub fn is_import_environment(&self, id: EnvironmentId) -> bool {
        id % 2 == 1
    }

    pub fn open_type_environment(&mut self, name: String) -> EnvironmentId {
        let current = self.get_current_type_environment();

        // Don't recurse into parent environments here; nested modules with the same name are different
        let existing_id = current.sub_environments.get(&name).copied()
            .or_else(|| self.get_current_import_environment().sub_environments.get(&name).copied());

        let id = if let Some(existing_id) = existing_id {
            existing_id
        } else {
            let next_id: EnvironmentId = self.type_environments.len().into();
            assert!(self.is_normal_environment(next_id), "New type environment `{}` gets an odd ID: {}", &name, next_id);

            self.type_environments.push(TypeEnvironment::new()); // the new type environment
            self.type_environments.push(TypeEnvironment::new()); // the new import environment

            next_id
        };

        // This should be done in both cases (already existing or new environment).
        // If the environment already existed, it needs to be added to the main environment
        // in case the module is from the import environment, or it will be unimportable.
        self.get_current_type_environment_mut().sub_environments.insert(name, id);
        self.type_environment_stack.push(id);
        id
    }

    pub fn insert_declaration(&mut self, name: String, decl: TypedDecl) {
        self.get_current_type_environment_mut().declarations.insert(name, decl);
    }

    pub fn close_type_environment(&mut self) {
        // This assert is actually redundant, because type_environment_stack is a NonEmpty,
        // but it's probably better to do it anyway, and it also has a better panic message.
        assert!(<NonZeroUsize as Into<usize>>::into(self.type_environment_stack.len()) > 1, "Tried to close the global type environment");

        self.type_environment_stack.pop();
    }
}

impl Default for NameResolution {
    fn default() -> Self {
        NameResolution::new()
    }
}
