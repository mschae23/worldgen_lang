use non_empty_vec::NonEmpty;
use crate::compiler::name;
use crate::compiler::name::{NameResolution, TypeId};

#[derive(Clone, PartialEq, Eq)]
pub enum TypeHint {
    Options(NonEmpty<TypeId>),
    Any,
}

impl TypeHint {
    fn one_way_can_type_convert_to(from: TypeId, to: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        match to {
            TypeHint::Options(options) => match from {
                _ if options.contains(&from) => true,
                name::INT_TYPE_ID if allow_coerce && options.contains(&name::FLOAT_TYPE_ID) => true,
                _ => false,
            },
            TypeHint::Any => true,
        }
    }

    fn one_way_can_type_convert_from(to: TypeId, from: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        match from {
            TypeHint::Options(from_options) => match to {
                _ if from_options.contains(&to) => true,
                name::FLOAT_TYPE_ID if allow_coerce && from_options.contains(&name::INT_TYPE_ID) => true,
                _ if allow_coerce && from_options.contains(&name::ERROR_TYPE_ID) => true,
                _ => false,
            },
            TypeHint::Any => false,
        }
    }

    fn one_way_can_convert_to(&self, to: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        match self {
            TypeHint::Options(from_options) => {
                for from_type in from_options {
                    if Self::one_way_can_type_convert_to(*from_type, to, allow_coerce, names) {
                        return true;
                    }
                }

                false
            },
            TypeHint::Any => false,
        }
    }

    fn one_way_can_convert_from(&self, from: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        match self {
            TypeHint::Options(to_options) => {
                for to_type in to_options {
                    if Self::one_way_can_type_convert_from(*to_type, from, allow_coerce, names) {
                        return true;
                    }
                }

                false
            }
            TypeHint::Any => true,
        }
    }

    pub fn can_convert_to(&self, other: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        self.one_way_can_convert_to(other, allow_coerce, names)
            || other.one_way_can_convert_from(self, allow_coerce, names)
    }

    pub fn can_convert_from(&self, other: &Self, allow_coerce: bool, names: &NameResolution) -> bool {
        self.one_way_can_convert_from(other, allow_coerce, names)
            || other.one_way_can_convert_to(self, allow_coerce, names)
    }
}
