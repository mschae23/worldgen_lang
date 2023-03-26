use std::collections::HashMap;
use std::path::{Path, PathBuf};
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

#[derive(Clone, Copy, Debug)]
pub enum PrimitiveTypeKind {
    Int, Float, Boolean, String, Object, Array, Type,
}

#[derive(Debug)]
pub enum SimpleTypeInfo {
    Primitive {
        kind: PrimitiveTypeKind,
    },
    Class {
        interface: bool,
    },
    TypeAlias,
}

pub const INT_TYPE_ID: TypeId = 0;
pub const FLOAT_TYPE_ID: TypeId = 1;
pub const BOOLEAN_TYPE_ID: TypeId = 2;
pub const STRING_TYPE_ID: TypeId = 3;
pub const OBJECT_TYPE_ID: TypeId = 4;
pub const ARRAY_TYPE_ID: TypeId = 5;
pub const TYPE_TYPE_ID: TypeId = 6;
pub const PRIMITIVE_TYPE_COUNT: usize = 7;

#[derive(Debug)]
struct TypeStorage {
    types: Vec<SimpleTypeInfo>,
    type_id_lookup: HashMap<PathBuf, TypeId>,
    type_path_lookup: Vec<PathBuf>,
    type_span_lookup: Vec<(Span, FileId)>,
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
            ],
            type_id_lookup: HashMap::new(),
            type_path_lookup: Vec::new(),
            type_span_lookup: Vec::new(),
        }
    }

    pub fn get(&self, type_id: TypeId) -> Option<&SimpleTypeInfo> {
        self.types.get(type_id)
    }

    pub fn get_type_id_by_path<P: AsRef<Path>>(&self, path: P) -> Option<TypeId> {
        fn _get_by_path_impl(this: &TypeStorage, path: &Path) -> Option<TypeId> {
            this.type_id_lookup.get(path).map(|&id| id)
        }

        _get_by_path_impl(self, path.as_ref())
    }

    pub fn get_path(&self, type_id: TypeId) -> Option<&Path> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_path_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).map(|path| path.as_path())
        }
    }

    pub fn get_span(&self, type_id: TypeId) -> Option<(Span, FileId)> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_span_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).map(|&(span, file)| (span, file))
        }
    }

    pub fn insert(&mut self, path: PathBuf, span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> TypeId {
        let id = self.types.len();

        self.types.push(type_info);
        // TODO Is this clone okay? Maybe use Rc?
        self.type_id_lookup.insert(path.clone(), id);
        self.type_path_lookup.push(path);
        self.type_span_lookup.push((span, file_id));

        id
    }
}

impl Default for TypeStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct NameResolution {
    types: TypeStorage,
}

impl NameResolution {
    pub fn new() -> Self {
        NameResolution {
            types: TypeStorage::new(),
        }
    }

    pub fn insert_simple_type(&mut self, path: PathBuf, span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> TypeId {
        self.types.insert(path, span, file_id, type_info)
    }

    pub fn get_simple_type(&self, type_id: TypeId) -> Option<&SimpleTypeInfo> {
        self.types.get(type_id)
    }

    pub fn get_simple_type_id_by_path<P: AsRef<Path>>(&self, path: P) -> Option<TypeId> {
        self.types.get_type_id_by_path(path)
    }

    pub fn get_simple_type_span(&self, type_id: TypeId) -> Option<(Span, FileId)> {
        self.types.get_span(type_id)
    }
}

impl Default for NameResolution {
    fn default() -> Self {
        NameResolution::new()
    }
}

#[derive(Debug)]
struct SimpleModuleDecl {
    pub name: PositionedName,
    pub declarations: HashMap<String, SimpleDecl>,
}

#[derive(Debug)]
struct SimpleClassDecl {
    pub name: PositionedName,
    pub type_id: TypeId,
    pub interface: bool,
    pub parameters: Vec<(PositionedName, TypeId)>,
}

#[derive(Debug)]
enum SimpleDecl {
    Module(SimpleModuleDecl),
    Class(SimpleClassDecl),
}
