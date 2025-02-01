use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ty::{Type, TypeKind};
use crate::token::Token;

// Add a static counter for generating unique IDs
static NEXT_SYMBOL_ID: AtomicUsize = AtomicUsize::new(0);

// Add visibility enum
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected,
    Static,
}

// Add ID field to Symbol
#[derive(Clone, Debug)]
pub enum Symbol {
    Variable(SymbolId, Token, Type, Option<Visibility>, bool),
    Function {
        id: SymbolId,
        name: Token,
        params: Vec<Type>,
        return_type: Type,
        is_method: bool,
        visibility: Option<Visibility>,
        is_static: bool,
    },
    Class {
        id: SymbolId,
        name: Token,
        fields: HashMap<String, (Type, Visibility, bool)>,
        methods: HashMap<String, Symbol>,
        fully_defined: bool,
        constructor_param_count: usize,
    },
}

// Add a type alias for symbol IDs
pub type SymbolId = usize;

impl Symbol {
    // Add helper to get ID from any symbol variant
    pub fn id(&self) -> SymbolId {
        match self {
            Symbol::Variable(id, ..) => *id,
            Symbol::Function { id, .. } => *id,
            Symbol::Class { id, .. } => *id,
        }
    }

    // Add constructors for each variant that automatically assign IDs
    pub fn new_variable(name: Token, ty: Type) -> Self {
        Self::new_variable_with_visibility(name, ty, None, false)
    }

    pub fn new_function(name: Token, params: Vec<Type>, return_type: Type, is_method: bool) -> Self {
        Self::new_function_with_visibility(name, params, return_type, is_method, None, false)
    }

    pub fn new_class(name: Token) -> Self {
        let id = NEXT_SYMBOL_ID.fetch_add(1, Ordering::SeqCst);
        Symbol::Class {
            id,
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
            fully_defined: false,
            constructor_param_count: 0,
        }
    }

    // New constructor for variables with visibility
    pub fn new_variable_with_visibility(name: Token, ty: Type, visibility: Option<Visibility>, is_static: bool) -> Self {
        let id = NEXT_SYMBOL_ID.fetch_add(1, Ordering::SeqCst);
        Symbol::Variable(id, name, ty, visibility, is_static)
    }

    // Add helper methods forvisibility checks
    pub fn is_visible_from(&self, current_class: Option<&str>, accessing_class: Option<&str>) -> bool {
        match self {
            Symbol::Function { visibility, .. } | Symbol::Variable(_, _, _, visibility, _) => {
                match visibility {
                    Some(Visibility::Public) => true,
                    Some(Visibility::Private) => current_class == accessing_class,
                    Some(Visibility::Protected) => {
                        if let (Some(current), Some(accessing)) = (current_class, accessing_class) {
                            current == accessing // TODO: Add inheritance check here when implemented
                        } else {
                            false
                        }
                    }
                    Some(Visibility::Static) => true, // Static members are always visible
                    None => true, // No visibility = public
                }
            }
            _ => true,
        }
    }

    // New constructor with full visibility options
    pub fn new_function_with_visibility(
        name: Token,
        params: Vec<Type>,
        return_type: Type,
        is_method: bool,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Self {
        let id = NEXT_SYMBOL_ID.fetch_add(1, Ordering::SeqCst);
        Symbol::Function {
            id,
            name,
            params,
            return_type,
            is_method,
            visibility,
            is_static,
        }
    }

    pub fn new_generic_param(name: Token) -> Self {
        Self::new_variable(name.clone(), Type::new(name.clone(), TypeKind::GenericParam(name.lexeme.clone())))
    }
}

/// A multi-scope SymbolTable with separate global tables for classes & functions if needed.
/// One approach is to store all top-level declarations in global maps, and local variables
/// in a stack of scopes (for name resolution within blocks, functions, etc.).
#[derive(Debug)]
pub struct SymbolTable {
    /// Stack of local scopes: each is a map from variable name -> Symbol::Variable
    scopes: Vec<HashMap<String, Symbol>>,

    /// Global classes by name
    pub classes: HashMap<String, Symbol>,
    /// Global functions by name
    pub functions: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            classes: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Enter a new lexical scope
    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current lexical scope
    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    /// Declare a symbol (usually a variable) in the current local scope
    pub fn declare_symbol(&mut self, name: &str, sym: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), sym);
        }
    }

    /// Look up a symbol in local scopes from innermost to outermost
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }


    /// Store a top-level function
    pub fn declare_function(&mut self, name: &str, sym: Symbol) {
        self.functions.insert(name.to_string(), sym);
    }

    /// Lookup a top-level function
    pub fn lookup_function(&self, name: &str) -> Option<&Symbol> {
        self.functions.get(name)
    }

    /// Mutably lookup a function (if needed to mark something)
    pub fn lookup_function_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.functions.get_mut(name)
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Symbol> {
        self.classes.get(name)
    }

    pub fn user_defined_type_exists(&self, ty: &Type) -> bool {
        match &ty.kind {
            TypeKind::Reference(inner) | TypeKind::Pointer(inner) | TypeKind::MutRef(inner) | TypeKind::Array(inner, _) => self.user_defined_type_exists(inner),
            TypeKind::Function(params, ret) => params.iter().any(|p| self.user_defined_type_exists(p)) || self.user_defined_type_exists(ret),
            _ => self.classes.contains_key(&ty.name.lexeme),
        }
    }

    /// Store a class
    pub fn declare_class(&mut self, name: &str, sym: Symbol) {
        self.classes.insert(name.to_string(), sym);
    }

    /// Lookup a class
    pub fn lookup_class(&self, name: &str) -> Option<&Symbol> {
        self.classes.get(name)
    }

    /// Mutably lookup a class
    pub fn lookup_class_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.classes.get_mut(name)
    }
}