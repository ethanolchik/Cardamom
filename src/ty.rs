use crate::ast::{Attribute, Derived};
use crate::token::Token;
use std::fmt::Display;
use std::collections::HashMap;

pub type SubstitutionMap = HashMap<String, TypeKind>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Type {
    pub attributes: Vec<Attribute>,
    pub derived: Vec<Derived>,
    pub generics: Vec<Box<Type>>,
    pub name: Token,

    pub kind: TypeKind,

    pub is_constructor: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Int,
    Float,
    String,
    Array(Box<Type>, usize),
    Function(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
    Reference(Box<Type>),
    MutRef(Box<Type>),
    Tuple(Vec<Type>),
    Void,
    User(String),
    GenericParam(String),
    GenericInstance(String, Vec<Type>),
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

/// A unique “signature” that identifies a single instantiation, e.g. "Vec<int>".
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct MonomorphSignature {
    pub name: String,       // "Vec"
    pub arg_types: Vec<Type>, // [int]
}

/// Tracks each known instantiation of a generic type.
/// For classes, you'd store Symbol::Class; for functions, Symbol::Function, etc.
#[derive(Clone, Debug)]
pub struct MonomorphTable {
    pub instances: HashMap<MonomorphSignature, Type>,
}

impl MonomorphTable {
    pub fn new() -> Self {
        MonomorphTable {
            instances: HashMap::new(),
        }
    }

    /// Insert or retrieve a generic instantiation.
    pub fn get_or_insert(&mut self, sig: MonomorphSignature, specialized_type: Type) -> Type {
        if let Some(existing) = self.instances.get(&sig) {
            existing.clone()
        } else {
            self.instances.insert(sig.clone(), specialized_type.clone());
            specialized_type
        }
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
        match self {
            TypeKind::Int | TypeKind::Float | TypeKind::String | TypeKind::Void => true,
            TypeKind::Array(inner, _) => inner.is_primitive(),
            TypeKind::Pointer(inner) => inner.is_primitive(),
            TypeKind::Reference(inner) => inner.is_primitive(),
            TypeKind::MutRef(inner) => inner.is_primitive(),
            TypeKind::Function(_, ret) => ret.is_primitive(),
            _ => false,
        }
    }

    /// Returns `true` if the `TypeKind` represents a pointer.
    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeKind::Pointer(inner) if inner.is_primitive())
    }

    /// Returns `true` if the `TypeKind` represents a reference.
    pub fn is_reference(&self) -> bool {
        matches!(self, TypeKind::Reference(inner) if inner.is_primitive())
    }

    /// Returns `true` if the `TypeKind` represents a mutable reference.
    pub fn is_mutable_ref(&self) -> bool {
        matches!(self, TypeKind::MutRef(inner) if inner.is_primitive())
    }

    /// Returns `true` if the `TypeKind` represents an array.
    pub fn is_array(&self) -> bool {
        matches!(self, TypeKind::Array(inner, _) if inner.is_primitive())
    }

    /// Returns `true` if the `TypeKind` represents a function.
    pub fn is_function(&self) -> bool {
        matches!(self, TypeKind::Function(params, ret) if params.iter().all(|p| p.is_primitive()) && ret.is_primitive())
    }

    /// Returns `true` if the `TypeKind` represents a tuple.
    pub fn is_tuple(&self) -> bool {
        matches!(self, TypeKind::Tuple(types) if types.iter().all(|t| t.is_primitive()))
    }

    /// Returns `true` if the `TypeKind` represents a user-defined type.
    pub fn is_user(&self) -> bool {
        matches!(self, TypeKind::User(_) | TypeKind::GenericParam(_))
    }

    /// Returns `true` if the `TypeKind` represents a generic type.
    pub fn is_generic(&self) -> bool {
        matches!(self, TypeKind::GenericParam(_))
    }

    pub fn to_string(&self) -> String {
        match self {
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::String => "string".to_string(),
            TypeKind::Array(ty, size) => format!("{}[{}]", ty.kind.to_string(), size),
            TypeKind::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|p| p.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", params_str, ret.kind.to_string())
            },
            TypeKind::Pointer(ty) => format!("*{}", ty.kind.to_string()),
            TypeKind::Reference(ty) => format!("&{}", ty.kind.to_string()),
            TypeKind::MutRef(ty) => format!("&mut {}", ty.kind.to_string()),
            TypeKind::Tuple(types) => {
                let types_str = types
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", types_str)
            },
            TypeKind::Void => "void".to_string(),
            TypeKind::User(name) => name.clone(),
            TypeKind::GenericParam(name) => name.clone(),
            TypeKind::GenericInstance(name, types) => {
                let types_str = types
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{}>", name, types_str)
            },
        }
    }
}

impl Type {
    pub fn new(name: Token, kind: TypeKind) -> Type {
        Type {
            attributes: Vec::new(),
            derived: Vec::new(),
            generics: Vec::new(),
            name,
            kind,
            is_constructor: false,
        }
    }

    /// Returns true if `self` is compatible with `other`.
    /// This covers:
    /// - pointer/ref/mutref with identical inner type
    /// - arrays with same inner type & length
    /// - user-defined class equality (by name)
    /// - function types with matching parameter lists & return type
    /// - tuple types if each element matches
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        // Exact equality check
        //      If exactly the same type kind (and name, etc.), trivially compatible
        if self.kind == other.kind {
            return true;
        }
        // Auto-deref for primitives (example):
        //    if function param is `int` and argument is `&int`, allow it.
        //    (Or you could do the inverse, or also handle `MutRef(int)`. 
        //    Adjust to match your language rules.)
        match (&self.kind, &other.kind) {
            (TypeKind::Int, TypeKind::Reference(inner)) 
            | (TypeKind::Float, TypeKind::Reference(inner)) 
            | (TypeKind::String, TypeKind::Reference(inner)) => {
                // e.g. param = int, arg = &int => good
                if inner.kind == self.kind {
                    return true;
                }
            }
            _ => {}
        }

        // Same as above but for mutref
        match (&self.kind, &other.kind) {
            (TypeKind::Int, TypeKind::MutRef(inner)) 
            | (TypeKind::Float, TypeKind::MutRef(inner)) 
            | (TypeKind::String, TypeKind::MutRef(inner)) => {
                // e.g. param = int, arg = &int => good
                if inner.kind == self.kind {
                    return true;
                }
            }
            _ => {}
        }

        // Same as above but for pointer
        match (&self.kind, &other.kind) {
            (TypeKind::Int, TypeKind::Pointer(inner)) 
            | (TypeKind::Float, TypeKind::Pointer(inner)) 
            | (TypeKind::String, TypeKind::Pointer(inner)) => {
                // e.g. param = int, arg = &int => good
                if inner.kind == self.kind {
                    return true;
                }
            }
            _ => {}
        }

        // Pointer, reference, or mutref with identical subtypes
        match (&self.kind, &other.kind) {
            (TypeKind::Pointer(a), TypeKind::Pointer(b))
            | (TypeKind::Reference(a), TypeKind::Reference(b))
            | (TypeKind::MutRef(a), TypeKind::MutRef(b)) => {
                return a.is_compatible_with(b);
            }
            _ => {}
        }

        // Arrays: check if length matches and element type matches
        match (&self.kind, &other.kind) {
            (TypeKind::Array(elem1, len1), TypeKind::Array(elem2, len2)) => {
                if len1 == len2 && elem1.is_compatible_with(elem2) {
                    return true;
                }
            }
            _ => {}
        }

        // Tuples
        match (&self.kind, &other.kind) {
            (TypeKind::Tuple(v1), TypeKind::Tuple(v2)) => {
                if v1.len() == v2.len() {
                    for (t1, t2) in v1.iter().zip(v2.iter()) {
                        if !t1.is_compatible_with(t2) {
                            return false;
                        }
                    }
                    return true;
                }
            }
            _ => {}
        }

        // User-defined: check name equality
        match (&self.kind, &other.kind) {
            (TypeKind::User(u1), TypeKind::User(u2)) => {
                return u1 == u2;
            }
            _ => {}
        }

        // Function types
        match (&self.kind, &other.kind) {
            (TypeKind::Function(params1, ret1), TypeKind::Function(params2, ret2)) => {
                if params1.len() == params2.len() {
                    // check each param
                    for (p1, p2) in params1.iter().zip(params2.iter()) {
                        if !p1.is_compatible_with(p2) {
                            return false;
                        }
                    }
                    return ret1.is_compatible_with(ret2);
                }
            }
            _ => {}
        }

        // Otherwise, not compatible
        false
    }

    /// Attempt to unify `self` with `other`. 
    /// If successful, record T -> TypeKind mappings in `subs`.
    /// If there's a conflict, you might produce an error or return false.
    pub fn unify(&self, other: &Type, subs: &mut SubstitutionMap) -> bool {
        match (&self.kind, &other.kind) {
            // 1) If `self` is a generic parameter, record the substitution
            (TypeKind::GenericParam(ref param_name), _) => {
                // If there's already a substitution for `param_name`, ensure it matches
                if let Some(existing) = subs.get(param_name) {
                    // We already have T -> something. Ensure it's compatible with `other`.
                    // E.g. T -> int, but now we're unifying T with float => error
                    if &other.kind != existing {
                        return false; // or produce an error
                    }
                } else {
                    subs.insert(param_name.clone(), other.kind.clone());
                }
                true
            },

            // 2) If both are GenericInstance with same base name, unify their type arguments
            (
                TypeKind::GenericInstance(base1, args1),
                TypeKind::GenericInstance(base2, args2),
            ) if base1 == base2 && args1.len() == args2.len() => {
                // unify each pair
                for (a, b) in args1.iter().zip(args2.iter()) {
                    if !a.unify(b, subs) {
                        return false;
                    }
                }
                true
            },

            // 3) If they're the same type (int vs int, etc.), success
            _ if self == other => true,

            // 4) Otherwise, unify fails
            _ => false,
        }
    }

    /// Apply a SubstitutionMap to create a fully concrete type.
    pub fn apply_substitution(&self, subs: &SubstitutionMap) -> Type {
        match &self.kind {
            // If it's a GenericParam, replace it if we have a mapping
            TypeKind::GenericParam(param_name) => {
                if let Some(replacement) = subs.get(param_name) {
                    // Create a new Type with the replaced kind
                    Type {
                        kind: replacement.clone(),
                        ..self.clone()
                    }
                } else {
                    // no mapping => remain generic
                    self.clone()
                }
            },

            // If it's a GenericInstance, apply_substitution to all arguments
            TypeKind::GenericInstance(base, args) => {
                let new_args: Vec<Type> = args
                    .iter()
                    .map(|arg| arg.apply_substitution(subs))
                    .collect();
                let new_kind = TypeKind::GenericInstance(base.clone(), new_args);
                Type {
                    kind: new_kind,
                    ..self.clone()
                }
            },

            // Arrays, pointers, references, etc.: apply to subtypes if needed
            TypeKind::Pointer(inner) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::Pointer(Box::new(new_inner)),
                    ..self.clone()
                }
            },
            TypeKind::Reference(inner) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::Reference(Box::new(new_inner)),
                    ..self.clone()
                }
            },
            TypeKind::Array(inner, size) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::Array(Box::new(new_inner), *size),
                    ..self.clone()
                }
            },
            TypeKind::Function(params, ret) => {
                let new_params: Vec<Type> = params
                    .iter()
                    .map(|param| param.apply_substitution(subs))
                    .collect();
                let new_ret = ret.apply_substitution(subs);
                Type {
                    kind: TypeKind::Function(new_params, Box::new(new_ret)),
                    ..self.clone()
                }
            },
            TypeKind::MutRef(inner) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::MutRef(Box::new(new_inner)),
                    ..self.clone()
                }
            },
            

            // If it's not generic, just return itself
            _ => self.clone(),
        }
    }

    pub fn to_string(&self) -> String {
        let mut s = format!("{}", self.name.lexeme);

        if !self.generics.is_empty() {
            s.push('<');
            for gen in &self.generics {
                s.push_str(&format!("{}, ", gen));
            }
            s.pop();
            s.pop();
            s.push('>');
        }

        s
    }

    pub fn get_array_depth(&self) -> usize {
        match &self.kind {
            TypeKind::Array(_, depth) => *depth,
            _ => 0
        }
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