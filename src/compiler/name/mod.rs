use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use non_empty_vec::ne_vec;
use crate::compiler::ast::simple::{PrimitiveTypeKind, TypePart, TypeReferencePart};
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
    type_span_lookup: Vec<Option<(Span, FileId)>>,
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

    pub fn get(&self, type_id: TypeId) -> Option<&SimpleTypeInfo> {
        self.types.get(type_id)
    }

    pub fn get_path(&self, type_id: TypeId) -> Option<&Path> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_path_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).and_then(|path| path.as_ref().map(|path| path.as_path()))
        }
    }

    pub fn get_type_id_by_path(&self, path: &[&str]) -> Option<TypeId> {
        assert!(path.len() >= 1, "Cannot get type ID for empty path");

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
                self.type_span_lookup.push(Some((args_span.mix(return_type.span()), file_id)));
                Ok(id)
            },
        }
    }

    pub fn get_type_id_by_type_reference_part<'source>(&self, prefix: &[&str], part: &TypeReferencePart<'source>) -> Result<TypeId, (&'source str, Span)> {
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

    pub fn get_span(&self, type_id: TypeId) -> Option<(Span, FileId)> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_span_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).and_then(|span| span.as_ref().map(|&(span, file)| (span, file)))
        }
    }

    pub fn insert(&mut self, path: &[&str], span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> Result<TypeId, TypeId> {
        assert!(path.len() >= 1, "Cannot insert a type with an empty path");

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
        self.type_span_lookup.push(Some((span, file_id)));

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

#[derive(Debug)]
pub struct SimpleDeclStorage {
    decls: Vec<SimpleDecl>,
}

impl SimpleDeclStorage {
    pub fn new() -> Self {
        SimpleDeclStorage {
            decls: Vec::new(),
        }
    }

    pub fn insert(&mut self, decl: SimpleDecl) {
        self.decls.push(decl);
    }
}

impl Default for SimpleDeclStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct NameResolution {
    decls: SimpleDeclStorage,
}

impl NameResolution {
    pub fn new() -> Self {
        NameResolution {
            decls: SimpleDeclStorage::new(),
        }
    }

    pub fn get_simple_decls(&self) -> &SimpleDeclStorage {
        &self.decls
    }

    pub fn get_simple_decls_mut(&mut self) -> &mut SimpleDeclStorage {
        &mut self.decls
    }
}

impl Default for NameResolution {
    fn default() -> Self {
        NameResolution::new()
    }
}

#[derive(Debug)]
pub struct SimpleModuleDecl {
    pub name: PositionedName,
    pub declarations: HashMap<String, SimpleDecl>,
    pub unnamed_templates: Vec<SimpleUnnamedTemplateDecl>,
}

#[derive(Debug)]
pub struct SimpleClassDecl {
    pub name: PositionedName,
    pub type_id: TypeId,
    pub interface: bool,
    pub parameters: Vec<(PositionedName, TypeId)>,
}

#[derive(Debug)]
pub struct SimpleTypeAliasDecl {
    pub name: PositionedName,
    pub type_id: TypeId,
}

#[derive(Debug)]
pub enum SimpleUnnamedTemplateData {
    Conversion {
        // TODO
    },
    Optimize {
        // TODO
    },
}

#[derive(Debug)]
pub struct SimpleUnnamedTemplateDecl {
    pub name_span: Span, // span of what would be the name for normal templates
    pub data: SimpleUnnamedTemplateData,
}

#[derive(Debug)]
pub enum SimpleDecl {
    Module(SimpleModuleDecl),
    Class(SimpleClassDecl),
    TypeAlias(SimpleTypeAliasDecl),
}

impl SimpleDecl {
    pub fn name(&self) -> &PositionedName {
        match self {
            Self::Module(decl) => &decl.name,
            Self::Class(decl) => &decl.name,
            Self::TypeAlias(decl) => &decl.name,
        }
    }

    pub fn span(&self) -> Span {
        self.name().span
    }
}
