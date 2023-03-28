use std::collections::HashMap;
use std::path::{Path, PathBuf};
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
pub struct TypeStorage {
    types: Vec<SimpleTypeInfo>,
    type_id_lookup: HashMap<PathBuf, TypeId>,
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
            ],
            type_id_lookup: HashMap::new(),
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

    pub fn get_type_id_by_path<P: AsRef<Path>>(&self, path: P) -> Option<TypeId> {
        fn _get_by_path_impl(this: &TypeStorage, path: &Path) -> Option<TypeId> {
            this.type_id_lookup.get(path).copied()
        }

        _get_by_path_impl(self, path.as_ref())
    }

    pub fn get_type_id_by_type_part(&mut self, prefix: &Path, part: &TypePart) -> Option<TypeId> {
        match part {
            TypePart::Primitive(kind, _) => Some(match kind {
                PrimitiveTypeKind::Int => INT_TYPE_ID,
                PrimitiveTypeKind::Float => FLOAT_TYPE_ID,
                PrimitiveTypeKind::Boolean => BOOLEAN_TYPE_ID,
                PrimitiveTypeKind::String => STRING_TYPE_ID,
                PrimitiveTypeKind::Object => OBJECT_TYPE_ID,
                PrimitiveTypeKind::Array => ARRAY_TYPE_ID,
                PrimitiveTypeKind::Type => TYPE_TYPE_ID,
            }),
            TypePart::Name(name) => self.get_type_id_by_type_reference_part(prefix, name),
            TypePart::Template { .. } => {
                let id = self.types.len();
                self.types.push(SimpleTypeInfo::Template);
                self.type_path_lookup.push(None);
                self.type_span_lookup.push(None);
                Some(id)
            },
        }
    }

    pub fn get_type_id_by_type_reference_part(&self, prefix: &Path, part: &TypeReferencePart) -> Option<TypeId> {
        self.get_type_id_by_path([prefix, &part.0.iter().map(|token| token.source()).collect::<PathBuf>()].into_iter().collect::<PathBuf>())
    }

    pub fn get_span(&self, type_id: TypeId) -> Option<(Span, FileId)> {
        if type_id < PRIMITIVE_TYPE_COUNT {
            None
        } else {
            self.type_span_lookup.get(type_id - PRIMITIVE_TYPE_COUNT).and_then(|span| span.as_ref().map(|&(span, file)| (span, file)))
        }
    }

    pub fn insert(&mut self, path: PathBuf, span: Span, file_id: FileId, type_info: SimpleTypeInfo) -> TypeId {
        let id = self.types.len();

        self.types.push(type_info);
        // TODO Is this clone okay? Maybe use Rc?
        self.type_id_lookup.insert(path.clone(), id);
        self.type_path_lookup.push(Some(path));
        self.type_span_lookup.push(Some((span, file_id)));

        id
    }
}

impl Default for TypeStorage {
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
    types: TypeStorage,
    decls: SimpleDeclStorage,
}

impl NameResolution {
    pub fn new() -> Self {
        NameResolution {
            types: TypeStorage::new(),
            decls: SimpleDeclStorage::new(),
        }
    }

    pub fn get_simple_types(&self) -> &TypeStorage {
        &self.types
    }

    pub fn get_simple_types_mut(&mut self) -> &mut TypeStorage {
        &mut self.types
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
