use crate::ast::{Attribute, Derived};
use crate::token::Token;
use std::collections::HashMap;
use std::fmt::Display;

pub type SubstitutionMap = HashMap<String, TypeKind>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Type {
    pub attributes: Vec<Attribute>,
    pub derived: Vec<Derived>,
    pub generics: Vec<Box<Type>>,
    pub name: Token,

    pub kind: TypeKind,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    String,
    Array(Box<Type>, usize),
    Function(Vec<Type>, Box<Type>),
    Reference(Box<Type>),
    MutRef(Box<Type>),
    Tuple(Vec<Type>),
    Void,
    User(String, String), // (module, name)
    GenericParam(String),
    GenericInstance(String, String, Vec<Type>), // (module, name, type arguments)
    /// An imported standard library module, e.g. the `io` in `io.println(..)`.
    /// Modules are namespaces, not values, so this type only ever appears as the
    /// type of the name to the left of a `.`.
    Module(String),
    /// A trait whose implementation is carried by a borrowed runtime value.
    DynTrait(Box<Type>),
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
    /// Type identity ignores source tokens, but never numeric conversions or borrows.
    pub fn same_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Array(a, ad), Self::Array(b, bd)) => ad == bd && a.kind.same_type(&b.kind),
            (Self::Reference(a), Self::Reference(b))
            | (Self::MutRef(a), Self::MutRef(b))
            | (Self::DynTrait(a), Self::DynTrait(b)) => a.kind.same_type(&b.kind),
            (Self::GenericInstance(am, an, aa), Self::GenericInstance(bm, bn, ba)) => {
                am == bm
                    && an == bn
                    && aa.len() == ba.len()
                    && aa.iter().zip(ba).all(|(a, b)| a.kind.same_type(&b.kind))
            }
            (Self::Tuple(a), Self::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.kind.same_type(&b.kind))
            }
            (Self::Function(a, ar), Self::Function(b, br)) => {
                ar.kind.same_type(&br.kind)
                    && a.len() == b.len()
                    && a.iter().zip(b).all(|(a, b)| a.kind.same_type(&b.kind))
            }
            _ => self == other,
        }
    }

    pub fn contains_generics(&self) -> bool {
        match self {
            Self::GenericParam(_) => true,
            Self::Array(t, _) | Self::Reference(t) | Self::MutRef(t) | Self::DynTrait(t) => {
                t.kind.contains_generics()
            }
            Self::GenericInstance(_, _, args) | Self::Tuple(args) => {
                args.iter().any(|a| a.kind.contains_generics())
            }
            Self::Function(args, ret) => {
                ret.kind.contains_generics() || args.iter().any(|a| a.kind.contains_generics())
            }
            _ => false,
        }
    }

    pub fn contains_dynamic(&self) -> bool {
        match self {
            Self::DynTrait(_) => true,
            Self::Array(t, _) | Self::Reference(t) | Self::MutRef(t) => t.kind.contains_dynamic(),
            Self::GenericInstance(_, _, args) | Self::Tuple(args) => {
                args.iter().any(|arg| arg.kind.contains_dynamic())
            }
            Self::Function(args, ret) => {
                ret.kind.contains_dynamic() || args.iter().any(|arg| arg.kind.contains_dynamic())
            }
            _ => false,
        }
    }

    /// Only shared references can carry dynamic trait objects in the initial ABI.
    pub fn dynamic_trait(&self) -> Option<&Type> {
        match self {
            Self::Reference(inner) => match &inner.kind {
                Self::DynTrait(trait_type) => Some(trait_type),
                _ => None,
            },
            _ => None,
        }
    }

    /// Match an impl's type pattern, not assignment compatibility. In particular,
    /// `impl Trait for int` must never select an implementation for `bool`.
    pub fn match_pattern(&self, actual: &Self, subs: &mut SubstitutionMap) -> bool {
        match (self, actual) {
            (Self::GenericParam(name), actual) => {
                if let Some(existing) = subs.get(name) {
                    existing.same_type(actual)
                } else {
                    subs.insert(name.clone(), actual.clone());
                    true
                }
            }
            (Self::GenericInstance(pm, pn, pa), Self::GenericInstance(am, an, aa)) => {
                pm == am
                    && pn == an
                    && pa.len() == aa.len()
                    && pa
                        .iter()
                        .zip(aa)
                        .all(|(p, a)| p.kind.match_pattern(&a.kind, subs))
            }
            (Self::Array(p, pd), Self::Array(a, ad)) => {
                pd == ad && p.kind.match_pattern(&a.kind, subs)
            }
            (Self::Reference(p), Self::Reference(a))
            | (Self::MutRef(p), Self::MutRef(a))
            | (Self::DynTrait(p), Self::DynTrait(a)) => p.kind.match_pattern(&a.kind, subs),
            _ => self.same_type(actual),
        }
    }

    pub fn inner_type(&self) -> Option<&Type> {
        match self {
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
            "bool" => TypeKind::Bool,
            "string" => TypeKind::String,
            "void" => TypeKind::Void,
            _ => TypeKind::User("".to_string(), name.to_string()),
        }
    }

    /// Returns `true` if the `TypeKind` is a primitive type.
    pub fn is_primitive(&self) -> bool {
        match self {
            TypeKind::Int
            | TypeKind::Float
            | TypeKind::Bool
            | TypeKind::String
            | TypeKind::Void => true,
            TypeKind::Array(inner, _) => inner.is_primitive(),
            TypeKind::Reference(inner) => inner.is_primitive(),
            TypeKind::MutRef(inner) => inner.is_primitive(),
            TypeKind::Function(_, ret) => ret.is_primitive(),
            _ => false,
        }
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
        matches!(self, TypeKind::User(_, _) | TypeKind::GenericParam(_))
    }

    /// Returns `true` if the `TypeKind` represents a generic type.
    pub fn is_generic(&self) -> bool {
        matches!(self, TypeKind::GenericParam(_))
    }

    pub fn to_string(&self) -> String {
        match self {
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::String => "string".to_string(),
            // Arrays print in the same postfix form the parser accepts. Element types
            // written with a prefix (`&T`, `#T`, `*T`) or a function type are
            // parenthesised, so the result parses back to the same type: without this
            // `(&int[])[]` would print as `&int[][]`, which means something else.
            TypeKind::Array(ty, _) => match &ty.kind {
                TypeKind::Function(..) | TypeKind::Reference(_) | TypeKind::MutRef(_) => {
                    format!("({})[]", ty.kind.to_string())
                }
                _ => format!("{}[]", ty.kind.to_string()),
            },
            TypeKind::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|p| p.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", params_str, ret.kind.to_string())
            }
            TypeKind::Reference(ty) => format!("&{}", ty.kind.to_string()),
            // Printed with the syntax that produces them: `&T` and `&mut T`.
            TypeKind::MutRef(ty) => format!("&mut {}", ty.kind.to_string()),
            TypeKind::Tuple(types) => {
                let types_str = types
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", types_str)
            }
            TypeKind::Void => "void".to_string(),
            TypeKind::Module(name) => format!("module {}", name),
            TypeKind::User(module, name) => {
                if module.is_empty() {
                    name.clone()
                } else {
                    format!("{}.{}", module, name)
                }
            }
            TypeKind::GenericParam(name) => name.clone(),
            TypeKind::GenericInstance(module, name, types) => {
                let types_str = types
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let base = if module.is_empty() {
                    name.clone()
                } else {
                    format!("{}.{}", module, name)
                };
                format!("{}<{}>", base, types_str)
            }
            TypeKind::DynTrait(ty) => {
                format!("dynamic {}", ty.kind)
            }
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
        }
    }

    /// Returns true if a value of type `self` can be used where `other` is expected.
    ///
    /// The direction matters: `self` is the type a value actually has, and `other` is
    /// the type the surrounding code requires.
    ///
    /// This covers:
    /// - pointer/ref/mutref with identical inner type
    /// - arrays with same inner type & length
    /// - user-defined class equality (by name)
    /// - function types with matching parameter lists & return type
    /// - tuple types if each element matches
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        // Error types are compatible with everything so one earlier diagnostic does not
        // cause a cascade merely because it was produced while checking another module.
        let is_error = |kind: &TypeKind| matches!(kind, TypeKind::User(_, name) if name == "error");
        if is_error(&self.kind) || is_error(&other.kind) {
            return true;
        }

        // Erasure needs an explicit conversion. In particular, ordinary borrow
        // coercions and int/bool compatibility cannot change a vtable's identity.
        if self.kind.contains_dynamic() || other.kind.contains_dynamic() {
            return self.kind.same_type(&other.kind);
        }

        // Exact equality check
        //      If exactly the same type kind (and name, etc.), trivially compatible
        if self.kind == other.kind {
            return true;
        }
        // Preserve compatibility with the language's historical 0/1 truth values.
        if matches!(
            (&self.kind, &other.kind),
            (TypeKind::Bool, TypeKind::Int) | (TypeKind::Int, TypeKind::Bool)
        ) {
            return true;
        }
        // Borrows.
        //
        // `&T` is an immutable borrow and `#T` a mutable one. The rules mirror what the
        // generated C++ (`const T&` and `T&`) will actually accept:
        //
        //   - reading through a borrow yields the borrowed type, so `&T` and `#T` are
        //     both usable where `T` is wanted;
        //   - anything can be borrowed immutably, since `const T&` binds to temporaries;
        //   - a mutable borrow can be used where an immutable one is wanted, but not
        //     the other way round, which would discard the immutability.
        //
        // Whether a value may be borrowed *mutably* also depends on it being an lvalue,
        // which is a property of the expression rather than the type, so the type
        // checker enforces that separately at each call site.
        match (&self.kind, &other.kind) {
            // An immutable borrow cannot stand in for a mutable one; that would let the
            // callee write through a reference the caller only lent out for reading.
            (TypeKind::Reference(_), TypeKind::MutRef(_)) => return false,

            // Two borrows of the same kind agree if their pointees do.
            (TypeKind::Reference(a), TypeKind::Reference(b))
            | (TypeKind::MutRef(a), TypeKind::MutRef(b)) => return a.is_compatible_with(b),

            // A mutable borrow may be used where an immutable one is expected.
            (TypeKind::MutRef(a), TypeKind::Reference(b)) => return a.is_compatible_with(b),

            // Reading through a borrow yields the borrowed type.
            (TypeKind::Reference(inner), _) | (TypeKind::MutRef(inner), _) => {
                if inner.is_compatible_with(other) {
                    return true;
                }
            }

            // Borrowing a value. A mutable borrow additionally requires an lvalue,
            // which is a property of the expression and is checked at the call site.
            (_, TypeKind::Reference(inner)) | (_, TypeKind::MutRef(inner)) => {
                if self.is_compatible_with(inner) {
                    return true;
                }
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
            (TypeKind::User(m1, n1), TypeKind::User(m2, n2)) => {
                return m1 == m2 && n1 == n2;
            }
            // Two instantiations of a generic class agree if they are the same class
            // with matching type arguments. Structural comparison is required because
            // `Type` equality includes the token a type was written at.
            (TypeKind::GenericInstance(m1, n1, a1), TypeKind::GenericInstance(m2, n2, a2)) => {
                return m1 == m2
                    && n1 == n2
                    && a1.len() == a2.len()
                    && a1
                        .iter()
                        .zip(a2.iter())
                        .all(|(x, y)| x.is_compatible_with(y));
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
            }

            // 2) If both are GenericInstance with same base name, unify their type arguments
            (
                TypeKind::GenericInstance(module1, base1, args1),
                TypeKind::GenericInstance(module2, base2, args2),
            ) if module1 == module2 && base1 == base2 && args1.len() == args2.len() => {
                // unify each pair
                for (a, b) in args1.iter().zip(args2.iter()) {
                    if !a.unify(b, subs) {
                        return false;
                    }
                }
                true
            }

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
            }

            // If it's a GenericInstance, apply_substitution to all arguments
            TypeKind::GenericInstance(module, base, args) => {
                let new_args: Vec<Type> = args
                    .iter()
                    .map(|arg| arg.apply_substitution(subs))
                    .collect();
                let new_kind = TypeKind::GenericInstance(module.clone(), base.clone(), new_args);
                Type {
                    kind: new_kind,
                    ..self.clone()
                }
            }

            // Arrays, pointers, references, etc.: apply to subtypes if needed
            TypeKind::Reference(inner) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::Reference(Box::new(new_inner)),
                    ..self.clone()
                }
            }
            TypeKind::Array(inner, size) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::Array(Box::new(new_inner), *size),
                    ..self.clone()
                }
            }
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
            }
            TypeKind::MutRef(inner) => {
                let new_inner = inner.apply_substitution(subs);
                Type {
                    kind: TypeKind::MutRef(Box::new(new_inner)),
                    ..self.clone()
                }
            }
            TypeKind::DynTrait(inner) => Type {
                kind: TypeKind::DynTrait(Box::new(inner.apply_substitution(subs))),
                ..self.clone()
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
            _ => 0,
        }
    }

    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
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
