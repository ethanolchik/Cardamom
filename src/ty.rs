use crate::ast::{Attribute, Derived};
use crate::token::Token;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type {
    pub attributes: Vec<Attribute>,
    pub derived: Vec<Derived>,
    pub generics: Vec<Box<Type>>,
    pub name: Token,

    pub kind: TypeKind,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeKind {
    Int,
    Float,
    String,
    Array(Box<Type>, usize),
    Function(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
    Reference(Box<Type>),
    MutRef(Box<Type>),
    Struct(Vec<Type>),
    Tuple(Vec<Type>),
    Void,
    User(String),
    Generic(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl TypeKind {
    pub fn inner_type(&self) -> Option<&Type> {
        match self {
            TypeKind::Pointer(inner) => Some(inner),
            TypeKind::Reference(inner) => Some(inner),
            TypeKind::MutRef(inner) => Some(inner),
            TypeKind::Array(inner, _) => Some(inner),
            _ => None,
        }
    }

    pub fn from_name(name: &str) -> TypeKind {
        match name {
            "int" => TypeKind::Int,
            "float" => TypeKind::Float,
            "string" => TypeKind::String,
            "void" => TypeKind::Void,
            _ => TypeKind::User(name.to_string()),
        }
    }

    /// Returns `true` if the `TypeKind` is a primitive type.
    pub fn is_primitive(&self) -> bool {
        matches!(self, TypeKind::Int | TypeKind::Float | TypeKind::String | TypeKind::Void)
    }

    /// Returns `true` if the `TypeKind` represents a pointer.
    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeKind::Pointer(_))
    }

    /// Returns `true` if the `TypeKind` represents a reference.
    pub fn is_reference(&self) -> bool {
        matches!(self, TypeKind::Reference(_))
    }

    /// Returns `true` if the `TypeKind` represents a mutable reference.
    pub fn is_mutable_ref(&self) -> bool {
        matches!(self, TypeKind::MutRef(_))
    }

    /// Returns `true` if the `TypeKind` represents an array.
    pub fn is_array(&self) -> bool {
        matches!(self, TypeKind::Array(_, _))
    }

    /// Returns `true` if the `TypeKind` represents a function.
    pub fn is_function(&self) -> bool {
        matches!(self, TypeKind::Function(_, _))
    }

    /// Returns `true` if the `TypeKind` represents a tuple.
    pub fn is_tuple(&self) -> bool {
        matches!(self, TypeKind::Tuple(_))
    }

    /// Returns `true` if the `TypeKind` represents a user-defined type.
    pub fn is_user(&self) -> bool {
        matches!(self, TypeKind::User(_))
    }

    /// Returns `true` if the `TypeKind` represents a generic type.
    pub fn is_generic(&self) -> bool {
        matches!(self, TypeKind::Generic(_))
    }
}

impl Type {
    pub fn new(name: Token, kind: TypeKind) -> Type {
        Type {
            attributes: Vec::new(),
            derived: Vec::new(),
            generics: Vec::new(),
            name,
            kind
        }
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        if self.kind == other.kind {
            return true;
        }

        match (&self.kind, &other.kind) {
            (TypeKind::Int, TypeKind::Float) => true,
            (TypeKind::Float, TypeKind::Int) => true,
            _ => false,
        }
    }

    /// 1: Strict compatibility required
    /// 2: Looser compatibility required
    /// 3: No compatibility required
    pub fn get_compatibility_strictness_for_operator(&self, op: String) -> u8 {
        match op.as_str() {
            "+" | "-" | "*" | "/" | "%" => 2,
            "==" | "!=" | "<" | ">" | "<=" | ">=" => 1,
            _ => 3,
        }
    }

    /// Should only be used on primitive types.
    pub fn get_types_from_compatibility_strictness(&self, strictness: u8) -> Vec<String> {
        match strictness {
            1 => vec![self.clone().name.lexeme],
            2 => match self.kind {
                TypeKind::Int => vec![String::from("int"), String::from("float")],
                TypeKind::Float => vec![String::from("int"), String::from("float")],
                _ => vec![],
            },
            _ => vec![],
        }
    }

    pub fn to_string(&self) -> String {
        let mut s = format!("{} {{ ", self.name.lexeme);
    
        for attr in &self.attributes {
            s.push_str(&format!("{} ", attr.to_string()));
        }
    
        for gen in &self.generics {
            s.push_str(&format!("{} ", gen));
        }
    
        s.push_str(&format!("{} ", self.kind.to_string())); // Updated to use `kind`.
    
        for der in &self.derived {
            s.push_str(&format!("{} ", der.to_string()));
        }
    
        s.push_str("}");
        s
    }
    
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }
    
    pub fn is_pointer(&self) -> bool {
        self.kind.is_pointer()
    }
    
    pub fn is_reference(&self) -> bool {
        self.kind.is_reference()
    }
    
    pub fn is_mutable_ref(&self) -> bool {
        self.kind.is_mutable_ref()
    }
    
    pub fn is_array(&self) -> bool {
        self.kind.is_array()
    }
    
    pub fn is_function(&self) -> bool {
        self.kind.is_function()
    }
    
    pub fn is_tuple(&self) -> bool {
        self.kind.is_tuple()
    }
    
    pub fn is_user(&self) -> bool {
        self.kind.is_user()
    }
    
    pub fn is_generic(&self) -> bool {
        self.kind.is_generic()
    }

    pub fn has_generic(&self) -> bool {
        !self.generics.is_empty()
    }

    pub fn is_const(&self) -> bool {
        self.derived.iter().any(|a| a == &Derived::Const)
    }
}