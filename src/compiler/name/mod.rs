use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::num::NonZeroUsize;
use non_empty_vec::{ne_vec, NonEmpty};
use crate::compiler::ast::forward::ForwardDeclStorage;
use crate::compiler::ast::simple::{PrimitiveTypeKind, TypePart, TypeReferencePart};
use crate::compiler::ast::typed::{TypedConversionDecl, TypedDecl, TypedOptimizeDecl, TypedTemplateDecl};
use crate::compiler::error::FileId;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::compiler::lexer::TokenType;
use crate::compiler::type_checker::TypeErrorReporter;
use crate::println_debug;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeId(pub(crate) usize);

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

pub const INT_TYPE_ID: TypeId = TypeId(0);
pub const FLOAT_TYPE_ID: TypeId = TypeId(1);
pub const BOOLEAN_TYPE_ID: TypeId = TypeId(2);
pub const STRING_TYPE_ID: TypeId = TypeId(3);
pub const OBJECT_TYPE_ID: TypeId = TypeId(4);
pub const ARRAY_TYPE_ID: TypeId = TypeId(5);
pub const TYPE_TYPE_ID: TypeId = TypeId(6);
pub const ERROR_TYPE_ID: TypeId = TypeId(7);
pub const PRIMITIVE_TYPE_COUNT: usize = 8;

pub const CONVERTIBLE_TO_STRING: [TypeId; 3] = [
    INT_TYPE_ID,
    FLOAT_TYPE_ID,
    BOOLEAN_TYPE_ID,
];

#[derive(Debug)]
pub struct TypeStorage {
    type_id_offset: usize,
    types: Vec<SimpleTypeInfo>,
    top_level_module: TypeModule,
    type_path_lookup: Vec<Option<NonEmpty<String>>>,
    type_span_lookup: Vec<Option<SpanWithFile>>,
}

impl TypeStorage {
    pub fn new(type_id_offset: usize) -> Self {
        TypeStorage {
            type_id_offset,
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
    fn next_type_id(&self) -> TypeId {
        TypeId(self.types.len() + self.type_id_offset)
    }

    #[inline]
    pub fn get(&self, type_id: TypeId) -> Option<&SimpleTypeInfo> {
        self.types.get(type_id.0)
    }

    #[inline]
    pub fn get_type_count(&self) -> usize {
        self.types.len()
    }

    pub fn get_path(&self, type_id: TypeId) -> Option<&NonEmpty<String>> {
        if type_id.0 < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            use std::borrow::Borrow;
            self.type_path_lookup.get(type_id.0 - PRIMITIVE_TYPE_COUNT - self.type_id_offset).and_then(|path| path.as_ref().map(NonEmpty::borrow))
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
                let id = self.next_type_id();
                self.types.push(SimpleTypeInfo::Template);
                self.type_path_lookup.push(None);
                self.type_span_lookup.push(Some(SpanWithFile::new(file_id, args_span.mix(return_type.span()))));
                Ok(id)
            },
        }
    }

    pub fn get_type_id_by_type_reference_part<'source>(&self, prefix: &[&str], part: &TypeReferencePart<'source>) -> Result<TypeId, (&'source str, Span)> {
        if <NonZeroUsize as Into<usize>>::into(part.0.len()) == 1 {
            match part.0.first().token_type() {
                TokenType::Int => return Ok(INT_TYPE_ID),
                TokenType::Float => return Ok(FLOAT_TYPE_ID),
                TokenType::Boolean => return Ok(BOOLEAN_TYPE_ID),
                TokenType::String => return Ok(STRING_TYPE_ID),
                TokenType::Object => return Ok(OBJECT_TYPE_ID),
                TokenType::Array => return Ok(ARRAY_TYPE_ID),
                TokenType::Type => return Ok(TYPE_TYPE_ID),
                _ => {},
            }
        }

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

    pub fn get_span(&self, type_id: TypeId) -> Option<SpanWithFile> {
        if type_id.0 < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_span_lookup.get(type_id.0 - PRIMITIVE_TYPE_COUNT - self.type_id_offset).and_then(|o| o.as_ref()).copied()
        }
    }

    pub fn insert(&mut self, path: &[&str], span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> Result<TypeId, TypeId> {
        assert!(!path.is_empty(), "Cannot insert a type with an empty path");

        let id: TypeId = self.next_type_id();

        let mut module = &mut self.top_level_module;

        for &component in path.iter().take(path.len().saturating_sub(1)) {
            module = module.sub_modules.entry(component.to_owned()).or_default();
        }

        match module.types.entry(path.last().copied().expect("Empty path despite previous check").to_owned()) {
            Entry::Occupied(entry) => return Err(*entry.get()),
            Entry::Vacant(entry) => entry.insert(id),
        };

        self.types.push(type_info);
        // SAFETY: path was asserted to not be empty
        self.type_path_lookup.push(Some(unsafe { NonEmpty::new_unchecked(path.iter().copied().map(str::to_owned).collect()) }));
        self.type_span_lookup.push(Some(SpanWithFile::new(file_id, span)));

        Ok(id)
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct EnvironmentId(usize);

impl Debug for EnvironmentId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub const GLOBAL_ENVIRONMENT_ID: EnvironmentId = EnvironmentId(0);
pub const GLOBAL_IMPORT_ENVIRONMENT_ID: EnvironmentId = EnvironmentId(1);

#[derive(Debug)]
pub struct TypeEnvironment {
    pub sub_environments: HashMap<String, EnvironmentId>,
    pub declarations: HashMap<String, TypedDecl>,
    pub templates: Vec<TypedTemplateDecl>,
    pub conversions: Vec<TypedConversionDecl>,
    pub optimizations: Vec<TypedOptimizeDecl>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            sub_environments: HashMap::new(),
            declarations: HashMap::new(),
            templates: Vec::new(),
            conversions: Vec::new(),
            optimizations: Vec::new(),
        }
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct NameResolution {
    pub types: TypeStorage,
    pub forward_decls: ForwardDeclStorage,

    // Type environments always occupy 2 IDs and 2 slots in the stack:
    // a normal type environment and an import type environment
    type_environments: Vec<TypeEnvironment>, // by ID
    type_environment_stack: NonEmpty<EnvironmentId>,
}

impl NameResolution {
    pub fn new(types: TypeStorage, forward_decls: ForwardDeclStorage) -> Self {
        NameResolution {
            types, forward_decls,
            type_environments: vec![TypeEnvironment::new(), TypeEnvironment::new()],
            type_environment_stack: NonEmpty::new(GLOBAL_ENVIRONMENT_ID),
        }
    }

    #[inline]
    pub fn get_current_environment_id(&self) -> EnvironmentId {
        *self.type_environment_stack.last()
    }

    #[inline]
    pub fn get_import_environment_id(&self) -> EnvironmentId {
        EnvironmentId(self.type_environment_stack.last().0 + 1)
    }

    #[inline]
    pub fn get_type_environment_by_id(&self, id: EnvironmentId) -> &TypeEnvironment {
        &self.type_environments[id.0]
    }

    #[inline]
    pub fn get_type_environment_by_id_mut(&mut self, id: EnvironmentId) -> &mut TypeEnvironment {
        &mut self.type_environments[id.0]
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

        self.get_type_environment_by_id(self.get_import_environment_id())
    }

    #[inline]
    pub fn is_normal_environment(&self, id: EnvironmentId) -> bool {
        id.0 % 2 == 0
    }

    #[inline]
    pub fn is_import_environment(&self, id: EnvironmentId) -> bool {
        id.0 % 2 == 1
    }

    pub fn open_type_environment(&mut self, name: String) -> EnvironmentId {
        let current = self.get_current_type_environment();

        // Don't recurse into parent environments here; nested modules with the same name are different
        let existing_id = current.sub_environments.get(&name).copied()
            .or_else(|| self.get_current_import_environment().sub_environments.get(&name).copied());

        let id = if let Some(existing_id) = existing_id {
            existing_id
        } else {
            let next_id: EnvironmentId = EnvironmentId(self.type_environments.len());
            assert!(self.is_normal_environment(next_id), "New type environment `{}` gets an odd ID: {:?}", &name, next_id);

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

    pub fn insert_template_declaration(&mut self, decl: TypedTemplateDecl) {
        self.get_current_type_environment_mut().templates.push(decl);
    }

    pub fn insert_conversion_declaration(&mut self, decl: TypedConversionDecl) {
        self.get_current_type_environment_mut().conversions.push(decl);
    }

    pub fn insert_optimize_declaration(&mut self, decl: TypedOptimizeDecl) {
        self.get_current_type_environment_mut().optimizations.push(decl);
    }

    pub fn include(&mut self, mut other: NameResolution, reporter: &mut TypeErrorReporter<'_>) {
        let id_offset = self.type_environments.len();

        // TODO Include prefix from current module.
        //      It always includes into the global environment right now, even if
        //      the include decl is inside a module (get path from current environment stack)
        self.forward_decls.include(other.forward_decls, reporter);
        self.type_environments.append(&mut other.type_environments);

        self.merge_type_environment(self.get_import_environment_id(), EnvironmentId(GLOBAL_ENVIRONMENT_ID.0 + id_offset), id_offset);
    }

    fn merge_type_environment(&mut self, id: EnvironmentId, other_id: EnvironmentId, id_offset: usize) {
        println_debug!("TODO: merge type environment {:?} into {:?} (offset {})", other_id, id, id_offset);
    }

    pub fn close_type_environment(&mut self) {
        // This assert is actually redundant, because type_environment_stack is a NonEmpty,
        // but it's probably better to do it anyway, and it also has a better panic message.
        assert!(<NonZeroUsize as Into<usize>>::into(self.type_environment_stack.len()) > 1, "Tried to close the global type environment");

        self.type_environment_stack.pop();
    }

    pub fn type_name(&self, type_id: TypeId) -> &str {
        match type_id {
            INT_TYPE_ID => "int",
            FLOAT_TYPE_ID => "float",
            BOOLEAN_TYPE_ID => "boolean",
            STRING_TYPE_ID => "string",
            OBJECT_TYPE_ID => "object",
            ARRAY_TYPE_ID => "array",
            TYPE_TYPE_ID => "type",
            ERROR_TYPE_ID => "error",
            _ => self.types.get_path(type_id).map_or("unknown", |path| path.last()),
        }
    }
}
