use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::*;
use crate::errors::{Error, Help, Note};
use crate::token::{Token, TokenKind};
use crate::ty::{Type, TypeKind};
use crate::utils::symtable::{Symbol, SymbolTable, Visibility};

/// A map from expressions to their inferred types.
type ExprTypeMap<'a> = HashMap<*const Expr, Type>;

/// Main TypeChecker structure.
/// - Contains a mutable reference to the `SymbolTable`.
/// - Maintains a list of `Error` objects.
/// - Stores an `ExprTypeMap` so we know each expression's resulting `Type`.
/// - Tracks the current filename and source code for better error messages.
/// - Also tracks the current class or function context (for `this`, access checks, etc.).
pub struct TypeChecker<'a> {
    pub symtable: &'a mut SymbolTable,
    pub expr_types: ExprTypeMap<'a>,
    pub errors: RefCell<Vec<Error>>,

    /// The name of the file we're currently checking, so we can attach it to errors.
    pub filename: String,
    /// The entire source code of the file, used for showing the line with an error.
    pub source: String,

    // Class & function context
    current_class: Option<String>,
    class_stack: Vec<String>,
    current_function_return_type: Option<Type>,
    function_has_valid_return: bool,
    current_assignment: Option<Expr>,
    current_initialiser: Option<Expr>,
    /// The type an expression is expected to produce, used to infer things that carry
    /// no type of their own: empty array literals and unannotated closure parameters.
    current_expected_type: Option<Type>,
    in_static_context: bool,
    in_call: bool,
    /// The name of the function being checked, for diagnostics and call-site metadata.
    current_function: Option<String>,
    /// The generic function or class whose specialisation causes dependencies found in
    /// the current body. Keys share the instantiation namespace (`foo`, `class Foo`).
    current_generic_owner: Option<String>,
    /// Type parameters of the function or class being checked. A use made inside a generic
    /// function is not a concrete instantiation, so these names mark types that are
    /// still standing in for something else.
    current_function_generics: Vec<String>,

    /// Public exports of every module compiled so far, keyed by module name.
    pub module_exports: HashMap<String, ModuleExports>,
    /// The type arguments each generic call site resolved to, keyed by AST node.
    pub call_instantiations: HashMap<*const Expr, Vec<TypeKind>>,
    /// Every distinct instantiation of each generic function, in first-use order so the
    /// generated code is deterministic.
    pub instantiations: HashMap<String, HashMap<String, Vec<Vec<TypeKind>>>>,
    /// Generic calls made from inside a generic function, with their type arguments
    /// still expressed in terms of the caller's type parameters. Expanding these is
    /// what makes instantiation transitive.
    pub generic_call_sites: Vec<GenericCallSite>,
    /// Type parameter names of each generic function, keyed by module then name.
    pub function_generics: HashMap<String, HashMap<String, Vec<String>>>,
    /// The name of the module being checked.
    module_name: String,
    /// True when checking an imported module rather than the program itself, which
    /// relaxes the requirement to have a `main`.
    is_library: bool,
}

/// A generic dependency appearing inside another generic owner.
#[derive(Clone, Debug, PartialEq)]
pub struct GenericCallSite {
    pub caller_module: String,
    /// The generic function or class containing the dependency.
    pub caller: String,
    pub callee_module: String,
    /// The generic function being called.
    pub callee: String,
    /// The callee's type arguments, which may mention the caller's type parameters.
    pub arguments: Vec<TypeKind>,
}

/// The names a module makes available to anything that imports it.
#[derive(Clone, Debug, Default)]
pub struct ModuleExports {
    pub functions: HashMap<String, ExportedFunction>,
    pub classes: HashMap<String, ExportedClass>,
}

/// The signature an importing module sees.
#[derive(Clone, Debug)]
pub struct ExportedFunction {
    pub params: Vec<Type>,
    pub return_type: Type,
    /// Names of the function's type parameters, in declaration order.
    pub generics: Vec<String>,
}

/// The members of a class that are visible to importing modules.
#[derive(Clone, Debug)]
pub struct ExportedClass {
    /// Names of the class's type parameters, in declaration order.
    pub generics: Vec<String>,
    pub fields: HashMap<String, (Type, Visibility, bool)>,
    pub methods: HashMap<String, ExportedMethod>,
    pub constructor_params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct ExportedMethod {
    pub params: Vec<Type>,
    pub return_type: Type,
    /// Type parameters declared by the method itself; class parameters are stored on
    /// `ExportedClass` and are supplied by the receiver.
    pub generics: Vec<String>,
    pub visibility: Visibility,
    pub is_static: bool,
}

#[derive(Clone, Debug)]
struct MethodSignature {
    module: String,
    class_name: String,
    class_generics: Vec<String>,
    class_arguments: Vec<Type>,
    method_name: String,
    method_generics: Vec<String>,
    params: Vec<Type>,
    return_type: Type,
}

impl ModuleExports {
    pub fn function_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.functions.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn class_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.classes.keys().cloned().collect();
        names.sort();
        names
    }
}

impl<'a> TypeChecker<'a> {
    /// Creates a new TypeChecker.
    pub fn new(symtable: &'a mut SymbolTable, filename: String, source: String) -> Self {
        Self {
            symtable,
            expr_types: HashMap::new(),
            errors: RefCell::new(Vec::new()),

            filename,
            source,

            current_class: None,
            class_stack: Vec::new(),
            current_function_return_type: None,
            function_has_valid_return: false,
            current_assignment: None,
            current_initialiser: None,
            current_expected_type: None,
            in_static_context: false,
            in_call: false,
            current_function: None,
            current_generic_owner: None,
            current_function_generics: Vec::new(),

            module_exports: HashMap::new(),
            call_instantiations: HashMap::new(),
            instantiations: HashMap::new(),
            generic_call_sites: Vec::new(),
            function_generics: HashMap::new(),
            module_name: "main".to_string(),
            is_library: false,
        }
    }

    /// Makes the exports of already-compiled modules visible to this one.
    pub fn set_module_exports(&mut self, exports: HashMap<String, ModuleExports>) {
        self.module_exports = exports;
    }

    /// Sets the name of the module being checked, used to attribute instantiations.
    pub fn set_module_name(&mut self, name: String) {
        self.module_name = name;
    }

    /// Marks this as an imported module, which is not required to define `main`.
    pub fn set_is_library(&mut self, is_library: bool) {
        self.is_library = is_library;
    }

    /// Main entry point for type-checking a module.
    pub fn check_module(&mut self, module: &Module) {
        self.resolve_imports(module);
        self.collect_declarations(module);
        self.define_class_members(module);
        self.register_extensions(module);

        module.accept(self);

        self.check_entry_point(module);
    }

    /// Collects the `public` functions and classes of the module just checked, so importers can
    /// resolve references against them.
    pub fn exports(&self, module: &Module) -> ModuleExports {
        let mut exports = ModuleExports::default();

        for stmt in &module.statements {
            if let Stmt::Function {
                name,
                params,
                return_type,
                modifiers,
                generics,
                ..
            } = &**stmt
            {
                let function =
                    self.collect_exported_function(params, return_type, modifiers, generics);
                if let Some(function) = function {
                    exports.functions.insert(name.lexeme.clone(), function);
                }
            } else if let Stmt::Class {
                name,
                generics,
                fields,
                methods,
                modifier,
                ..
            } = &**stmt
            {
                let class = self.collect_exported_class(generics, fields, methods, modifier);
                if let Some(class) = class {
                    exports.classes.insert(name.lexeme.clone(), class);
                }
            };
        }

        exports
    }

    fn collect_exported_function(
        &self,
        params: &[Box<Stmt>],
        return_type: &Type,
        modifiers: &[Modifier],
        generics: &[Token],
    ) -> Option<ExportedFunction> {
        if !modifiers.contains(&Modifier::Public) {
            return None;
        }

        let generic_names: Vec<String> = generics.iter().map(|g| g.lexeme.clone()).collect();

        let param_types = params
            .iter()
            .filter_map(|p| {
                if let Stmt::Variable { type_, .. } = &**p {
                    let generalised = Self::generalise(type_, &generic_names);
                    Some(self.qualify_type(&generalised))
                } else {
                    None
                }
            })
            .collect();

        Some(ExportedFunction {
            params: param_types,
            return_type: self.qualify_type(&Self::generalise(return_type, &generic_names)),
            generics: generic_names,
        })
    }

    fn collect_exported_class(
        &self,
        generics: &[Token],
        fields: &[Box<Stmt>],
        methods: &[Box<Stmt>],
        modifier: &[Modifier],
    ) -> Option<ExportedClass> {
        if !modifier.contains(&Modifier::Public) {
            return None;
        }

        let generic_names: Vec<String> = generics.iter().map(|g| g.lexeme.clone()).collect();

        let mut field_exports = HashMap::new();
        let mut constructor_params = Vec::new();
        for f in fields {
            if let Stmt::Variable {
                name: field_name,
                type_,
                modifiers,
                ..
            } = &**f
            {
                if modifiers.contains(&Modifier::Public) {
                    let qualified = self.qualify_type(&Self::generalise(type_, &generic_names));
                    field_exports.insert(
                        field_name.lexeme.clone(),
                        (
                            qualified,
                            self.visibility_of(modifiers),
                            is_static_member(modifiers),
                        ),
                    );
                }
                if is_constructor_field(modifiers) {
                    constructor_params
                        .push(self.qualify_type(&Self::generalise(type_, &generic_names)));
                }
            }
        }

        let mut method_exports = HashMap::new();
        for m in methods {
            if let Stmt::Function {
                name: method_name,
                params,
                return_type,
                modifiers,
                generics: method_generics,
                ..
            } = &**m
            {
                if modifiers.contains(&Modifier::Public) {
                    let method_generic_names: Vec<String> = method_generics
                        .iter()
                        .map(|generic| generic.lexeme.clone())
                        .collect();
                    let mut all_generics = generic_names.clone();
                    all_generics.extend(method_generic_names.iter().cloned());
                    let param_types = params
                        .iter()
                        .filter_map(|p| {
                            if let Stmt::Variable { type_, .. } = &**p {
                                Some(self.qualify_type(&Self::generalise(type_, &all_generics)))
                            } else {
                                None
                            }
                        })
                        .collect();

                    method_exports.insert(
                        method_name.lexeme.clone(),
                        ExportedMethod {
                            params: param_types,
                            return_type: self
                                .qualify_type(&Self::generalise(return_type, &all_generics)),
                            generics: method_generic_names,
                            visibility: self.visibility_of(modifiers),
                            is_static: is_static_member(modifiers),
                        },
                    );
                }
            }
        }

        Some(ExportedClass {
            generics: generic_names,
            fields: field_exports,
            methods: method_exports,
            constructor_params,
        })
    }

    /// `main` is the program entry point, so it has a fixed shape: it takes no
    /// parameters and returns either `int` or `void`. Anything else cannot be lowered
    /// to a valid C++ `main`, so reject it here instead of emitting broken C++.
    fn check_entry_point(&mut self, module: &Module) {
        // Imported modules are libraries; only the program itself needs an entry point.
        if self.is_library {
            return;
        }

        for stmt in &module.statements {
            let Stmt::Function {
                name,
                params,
                return_type,
                modifiers,
                generics,
                ..
            } = &**stmt
            else {
                continue;
            };

            if name.lexeme != "main" {
                continue;
            }

            if modifiers.contains(&Modifier::Extern) {
                self.error_token(name, "`main` cannot be declared `extern`");
            }

            if !generics.is_empty() {
                self.error_token(name, "`main` cannot be generic");
            }

            if !params.is_empty() {
                self.error_with_notes(
                    name.clone(),
                    &format!("`main` must take no parameters, but takes {}", params.len()),
                    vec![Note::new(
                        "`main` is the entry point of the program".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        "Remove the parameters from `main`".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                );
            }

            if return_type.kind != TypeKind::Int && return_type.kind != TypeKind::Void {
                self.error_with_notes(
                    return_type.name.clone(),
                    &format!(
                        "`main` must return `int` or `void`, but returns `{}`",
                        return_type.kind
                    ),
                    vec![Note::new(
                        "`main` is the entry point of the program".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        "Change the return type of `main` to `int` or `void`".to_string(),
                        return_type.name.line,
                        return_type.name.span.clone(),
                        self.filename.clone(),
                    )],
                );
            }
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.borrow().len()
    }

    pub fn emit_errors(&self) {
        for err in self.errors.borrow().iter() {
            eprintln!("{}", err.to_string());
        }
    }

    /// Binds each `import <module>;` to a module that has already been type checked.
    ///
    /// The imported name becomes a symbol of module type, so `io.println` resolves
    /// through the normal member-access path.
    fn resolve_imports(&mut self, module: &Module) {
        let mut imported: HashMap<String, Token> = HashMap::new();

        for stmt in &module.statements {
            let Stmt::Import { name, alias } = &**stmt else {
                continue;
            };

            // The loader has already resolved and compiled every import, so a missing
            // entry here means loading failed and has been reported already.
            if !self.module_exports.contains_key(&name.lexeme) {
                continue;
            }

            if let Some(previous) = imported.get(&alias.lexeme) {
                self.error_with_notes(
                    alias.clone(),
                    &format!("`{}` is imported more than once", alias.lexeme),
                    vec![Note::new(
                        format!("`{}` was first imported here", alias.lexeme),
                        previous.line,
                        previous.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        "Use `import <module> as <name>;` to bind it to a different name"
                            .to_string(),
                        alias.line,
                        alias.span.clone(),
                        self.filename.clone(),
                    )],
                );
                continue;
            }

            imported.insert(alias.lexeme.clone(), alias.clone());

            let module_type = Type::new(alias.clone(), TypeKind::Module(name.lexeme.clone()));

            self.symtable.declare_module(
                &alias.lexeme,
                Symbol::new_variable(alias.clone(), module_type),
            );
        }
    }

    // Forward declarations
    fn collect_declarations(&mut self, module: &Module) {
        for stmt in &module.statements {
            match &**stmt {
                Stmt::Class { name, generics, .. } => {
                    let class_name = name.lexeme.clone();
                    let generic_names = generics.iter().map(|g| g.lexeme.clone()).collect();
                    let sym = Symbol::new_generic_class(name.clone(), generic_names);
                    self.symtable.declare_class(&class_name, sym);
                }
                Stmt::Function {
                    name,
                    params,
                    return_type,
                    generics,
                    ..
                } => {
                    let fn_name = name.lexeme.clone();
                    let generic_names: Vec<String> =
                        generics.iter().map(|g| g.lexeme.clone()).collect();

                    // A parameter written `x: T` parses as the user type `T`; knowing
                    // the function's type parameters is what turns it into a generic.
                    let mut param_types = Vec::new();
                    for p in params {
                        if let Stmt::Variable { type_, .. } = &**p {
                            let generalised = Self::generalise(type_, &generic_names);
                            param_types.push(self.qualify_type(&generalised));
                        }
                    }

                    let return_type =
                        self.qualify_type(&Self::generalise(return_type, &generic_names));
                    let sym = Symbol::new_generic_function(
                        name.clone(),
                        param_types,
                        return_type,
                        false,
                        generic_names,
                    );
                    self.symtable.declare_function(&fn_name, sym);
                }
                _ => {}
            }
        }
    }

    // Class fields & methods
    fn define_class_members(&mut self, module: &Module) {
        for stmt in &module.statements {
            if let Stmt::Class {
                name,
                generics,
                fields,
                methods,
                ..
            } = &**stmt
            {
                let class_name = name.lexeme.clone();
                let class_generics: Vec<String> =
                    generics.iter().map(|g| g.lexeme.clone()).collect();

                // Visibility and `static` are read off each member's own modifiers, so
                // any combination (e.g. `private static`) is representable.
                let mut field_declarations = Vec::new();
                for f in fields {
                    if let Stmt::Variable {
                        name: field_name,
                        type_,
                        modifiers,
                        ..
                    } = &**f
                    {
                        let qualified =
                            self.qualify_type(&Self::generalise(type_, &class_generics));
                        field_declarations.push((
                            field_name.clone(),
                            qualified,
                            self.visibility_of(modifiers),
                            is_static_member(modifiers),
                        ));
                    }
                }

                let mut method_declarations = Vec::new();
                for m in methods {
                    if let Stmt::Function {
                        name: method_name,
                        params,
                        return_type,
                        modifiers,
                        generics: method_generics,
                        ..
                    } = &**m
                    {
                        let method_generic_names: Vec<String> = method_generics
                            .iter()
                            .map(|generic| generic.lexeme.clone())
                            .collect();
                        let mut all_generics = class_generics.clone();
                        all_generics.extend(method_generic_names.iter().cloned());
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(
                                    self.qualify_type(&Self::generalise(type_, &all_generics)),
                                );
                            }
                        }
                        let qualified_return =
                            self.qualify_type(&Self::generalise(return_type, &all_generics));
                        method_declarations.push((
                            method_name.clone(),
                            param_types,
                            qualified_return,
                            self.visibility_of(modifiers),
                            is_static_member(modifiers),
                            method_generic_names,
                        ));
                    }
                }

                // Now update the class symbol with all collected declarations
                // Constructor parameters are the header fields, in source order.
                let constructor_param_types: Vec<Type> = fields
                    .iter()
                    .filter_map(|f| match &**f {
                        Stmt::Variable {
                            type_, modifiers, ..
                        } if is_constructor_field(modifiers) => {
                            Some(self.qualify_type(&Self::generalise(type_, &class_generics)))
                        }
                        _ => None,
                    })
                    .collect();

                let mut symbol_declarations = Vec::new();
                if let Some(class_sym) = self.symtable.lookup_class_mut(&class_name) {
                    if let Symbol::Class {
                        fields,
                        methods,
                        fully_defined,
                        constructor_params,
                        ..
                    } = class_sym
                    {
                        *constructor_params = constructor_param_types;
                        for (field_name, field_type, visibility, is_static) in field_declarations {
                            fields.insert(
                                field_name.lexeme.clone(),
                                (field_type.clone(), visibility.clone(), is_static),
                            );

                            symbol_declarations.push((
                                field_name.lexeme.clone(),
                                Symbol::new_variable_with_visibility(
                                    field_name,
                                    field_type,
                                    Some(visibility),
                                    is_static,
                                ),
                            ));
                        }

                        for (
                            method_name,
                            param_types,
                            return_type,
                            visibility,
                            is_static,
                            method_generics,
                        ) in method_declarations
                        {
                            methods.insert(
                                method_name.lexeme.clone(),
                                Symbol::new_generic_function_with_visibility(
                                    method_name,
                                    param_types,
                                    return_type,
                                    true,
                                    Some(visibility),
                                    is_static,
                                    method_generics,
                                ),
                            );
                        }

                        *fully_defined = true;
                    }
                }

                // Now declare the symbols after the class symbol borrow is dropped
                for (name, symbol) in symbol_declarations {
                    self.symtable.declare_symbol(&name, symbol);
                }
            }
        }
    }

    /// Resolves a call against a known signature, reporting any mismatch and returning
    /// the result type.
    ///
    /// Shared by every call form (bare name or module member, with or without explicit
    /// type arguments) so they all check and instantiate generics the same way.
    fn check_call_signature(
        &mut self,
        expr: &Expr,
        token: &Token,
        module: &str,
        name: &str,
        generics: &[String],
        params: &[Type],
        return_type: &Type,
        explicit: &[Type],
        arguments: &[Box<Expr>],
        arg_tys: &[Type],
    ) -> Type {
        if params.len() != arg_tys.len() {
            self.error_with_notes(
                token.clone(),
                &format!(
                    "Function `{}` expects {} args, got {}",
                    name,
                    params.len(),
                    arg_tys.len()
                ),
                vec![Note::new(
                    format!("`{}` takes {} parameter(s)", name, params.len()),
                    token.line,
                    token.span.clone(),
                    self.filename.clone(),
                )],
                vec![],
            );
            return return_type.clone();
        }

        let subs = self.resolve_generics(token, name, generics, params, explicit, arg_tys);

        for (i, (expected, actual)) in params.iter().zip(arg_tys.iter()).enumerate() {
            let expected = expected.apply_substitution(&subs);

            if let Some(argument) = arguments.get(i) {
                self.check_mutable_borrow(&expected, argument, actual);
            }

            if !actual.is_compatible_with(&expected) {
                let argument_token = arguments
                    .get(i)
                    .map(|argument| get_token(argument))
                    .unwrap_or_else(|| token.clone());

                self.error_with_notes(
                    argument_token,
                    &format!(
                        "Argument mismatch in call to `{}`: expected `{}`, got `{}`",
                        name, expected.kind, actual.kind
                    ),
                    vec![Note::new(
                        format!("Parameter declared as `{}`", expected.kind),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![],
                );
            }
        }

        self.record_instantiation(expr, module, name, generics, &subs);
        return_type.apply_substitution(&subs)
    }

    /// If `callee` names a function of an imported module, returns its module and
    /// exported signature.
    fn module_callee(&self, callee: &Expr) -> Option<(String, String, ExportedFunction)> {
        let Expr::MemberAccess { object, name } = callee else {
            return None;
        };
        let object_ty = self.get_expr_type(object)?;
        let TypeKind::Module(module) = &object_ty.kind else {
            return None;
        };

        let function = self
            .module_exports
            .get(module)?
            .functions
            .get(&name.lexeme)?;
        Some((module.clone(), name.lexeme.clone(), function.clone()))
    }

    /// If `callee` names a class exported by an imported module, returns its module
    /// and exported declaration.
    fn module_class(&self, callee: &Expr) -> Option<(String, String, ExportedClass)> {
        let Expr::MemberAccess { object, name } = callee else {
            return None;
        };
        let object_ty = self.get_expr_type(object)?;
        let TypeKind::Module(module) = &object_ty.kind else {
            return None;
        };

        let class = self.module_exports.get(module)?.classes.get(&name.lexeme)?;
        Some((module.clone(), name.lexeme.clone(), class.clone()))
    }

    /// Resolves a method call while preserving both the receiver's class arguments and
    /// the type parameters declared by the method itself.
    fn method_callee(&self, callee: &Expr) -> Option<MethodSignature> {
        let Expr::MemberAccess { object, name } = callee else {
            return None;
        };
        let object_type = Self::without_borrows(self.get_expr_type(object)?);
        let (module, class_name, class_arguments) = match object_type.kind {
            TypeKind::User(module, class_name) => (module, class_name, Vec::new()),
            TypeKind::GenericInstance(module, class_name, arguments) => {
                (module, class_name, arguments)
            }
            _ => return None,
        };
        let module = if module.is_empty() {
            self.module_name.clone()
        } else {
            module
        };

        if module == self.module_name {
            let Symbol::Class {
                generics: class_generics,
                methods,
                ..
            } = self.symtable.lookup_class(&class_name)?
            else {
                return None;
            };
            let Symbol::Function {
                params,
                return_type,
                generics: method_generics,
                ..
            } = methods.get(&name.lexeme)?
            else {
                return None;
            };
            Some(MethodSignature {
                module,
                class_name,
                class_generics: class_generics.clone(),
                class_arguments,
                method_name: name.lexeme.clone(),
                method_generics: method_generics.clone(),
                params: params.clone(),
                return_type: return_type.clone(),
            })
        } else {
            let class = self.module_exports.get(&module)?.classes.get(&class_name)?;
            let method = class.methods.get(&name.lexeme)?;
            Some(MethodSignature {
                module,
                class_name,
                class_generics: class.generics.clone(),
                class_arguments,
                method_name: name.lexeme.clone(),
                method_generics: method.generics.clone(),
                params: method.params.clone(),
                return_type: method.return_type.clone(),
            })
        }
    }

    /// Resolves a call to a generic function, reporting any problem with its type
    /// arguments and returning the substitution to use.
    ///
    /// Type arguments are either written out (`identity<int>(5)`) or inferred from the
    /// arguments (`identity(5)`).
    fn resolve_generics(
        &self,
        token: &Token,
        name: &str,
        generics: &[String],
        params: &[Type],
        explicit: &[Type],
        arg_tys: &[Type],
    ) -> HashMap<String, TypeKind> {
        let mut subs: HashMap<String, TypeKind> = HashMap::new();

        if !explicit.is_empty() {
            if explicit.len() != generics.len() {
                self.error_with_notes(
                    token.clone(),
                    &format!(
                        "`{}` takes {} type argument(s), but {} were given",
                        name,
                        generics.len(),
                        explicit.len()
                    ),
                    vec![Note::new(
                        format!(
                            "`{}` is declared as `{}<{}>`",
                            name,
                            name,
                            generics.join(", ")
                        ),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![],
                );
            }

            for (parameter, argument) in generics.iter().zip(explicit.iter()) {
                subs.insert(parameter.clone(), argument.kind.clone());
            }
        } else {
            for (param, actual) in params.iter().zip(arg_tys.iter()) {
                Self::infer_substitution(param, actual, &mut subs);
            }
        }

        // Anything still unbound could not be worked out from the call.
        for parameter in generics {
            if !subs.contains_key(parameter) {
                self.error_with_notes(
                    token.clone(),
                    &format!("Cannot infer type parameter `{}` of `{}`", parameter, name),
                    vec![Note::new(
                        "It does not appear in any parameter, so it cannot be deduced from the arguments".to_string(),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        format!("Give it explicitly, e.g. `{}<int>(..)`", name),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                );

                subs.insert(
                    parameter.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                );
            }
        }

        subs
    }

    /// Generalises explicitly written type arguments against the enclosing function or
    /// class, so that `new Box<T>(..)` inside `wrap<T>` refers to `wrap`'s `T` rather
    /// than to a class called `T`.
    fn generalise_explicit(&self, types: &[Type]) -> Vec<Type> {
        types
            .iter()
            .map(|ty| {
                let generalised = Self::generalise(ty, &self.current_function_generics);
                self.qualify_type(&generalised)
            })
            .collect()
    }

    /// Resolves an import alias to the imported module's canonical name.
    fn resolve_module_name(&self, qualifier: &str) -> String {
        match self.symtable.lookup_module(qualifier) {
            Some(Symbol::Variable(_, _, ty, ..)) => match &ty.kind {
                TypeKind::Module(module) => module.clone(),
                _ => qualifier.to_string(),
            },
            _ => qualifier.to_string(),
        }
    }

    /// Resolves module ownership for user-written types.
    ///
    /// Parsed unqualified names intentionally have an empty owner until type checking,
    /// where they become local to the current module. Explicit qualifiers are also
    /// canonicalised so `import foo as f; f.Box` is owned by `foo`, not by the alias.
    fn qualify_type(&self, ty: &Type) -> Type {
        let kind = match &ty.kind {
            TypeKind::User(module, name) => {
                if name == "error" || name == "bool" {
                    TypeKind::User(module.clone(), name.clone())
                } else if module.is_empty() {
                    let written = if ty.name.lexeme.contains('.') {
                        ty.name.lexeme.as_str()
                    } else {
                        name.as_str()
                    };

                    if let Some((owner, base)) = written.rsplit_once('.') {
                        TypeKind::User(self.resolve_module_name(owner), base.to_string())
                    } else {
                        TypeKind::User(self.module_name.clone(), name.clone())
                    }
                } else {
                    TypeKind::User(self.resolve_module_name(module), name.clone())
                }
            }
            TypeKind::GenericInstance(module, name, args) => {
                let args = args.iter().map(|arg| self.qualify_type(arg)).collect();
                if module.is_empty() {
                    let written = if ty.name.lexeme.contains('.') {
                        ty.name.lexeme.as_str()
                    } else {
                        name.as_str()
                    };

                    if let Some((owner, base)) = written.rsplit_once('.') {
                        TypeKind::GenericInstance(
                            self.resolve_module_name(owner),
                            base.to_string(),
                            args,
                        )
                    } else {
                        TypeKind::GenericInstance(self.module_name.clone(), name.clone(), args)
                    }
                } else {
                    TypeKind::GenericInstance(
                        self.resolve_module_name(module),
                        name.clone(),
                        args,
                    )
                }
            }
            TypeKind::Array(inner, depth) => {
                TypeKind::Array(Box::new(self.qualify_type(inner)), *depth)
            }
            TypeKind::Reference(inner) => TypeKind::Reference(Box::new(self.qualify_type(inner))),
            TypeKind::MutRef(inner) => TypeKind::MutRef(Box::new(self.qualify_type(inner))),
            TypeKind::Function(params, ret) => TypeKind::Function(
                params
                    .iter()
                    .map(|param| self.qualify_type(param))
                    .collect(),
                Box::new(self.qualify_type(ret)),
            ),
            TypeKind::Tuple(elements) => {
                TypeKind::Tuple(elements.iter().map(|e| self.qualify_type(e)).collect())
            }
            other => other.clone(),
        };

        Type { kind, ..ty.clone() }
    }

    /// Records generic classes mentioned by a type declaration.
    ///
    /// This catches dependencies that have no constructor expression to visit, such as
    /// `class Outer<T> { private values: Inner<T>[]; }`.
    fn record_type_instantiations(&mut self, ty: &Type) {
        match &ty.kind {
            TypeKind::GenericInstance(module, name, arguments) => {
                for argument in arguments {
                    self.record_type_instantiations(argument);
                }

                let owner = if module.is_empty() {
                    self.module_name.clone()
                } else {
                    module.clone()
                };
                let generics = if owner == self.module_name {
                    match self.symtable.lookup_class(name) {
                        Some(Symbol::Class { generics, .. }) => generics.clone(),
                        _ => return,
                    }
                } else {
                    match self
                        .module_exports
                        .get(&owner)
                        .and_then(|exports| exports.classes.get(name))
                    {
                        Some(class) => class.generics.clone(),
                        None => return,
                    }
                };
                let substitutions: HashMap<String, TypeKind> = generics
                    .iter()
                    .cloned()
                    .zip(arguments.iter().map(|argument| argument.kind.clone()))
                    .collect();

                self.record_class_instantiation(&owner, name, &generics, &substitutions);
            }
            TypeKind::Array(inner, _)
            | TypeKind::Reference(inner)
            | TypeKind::MutRef(inner) => self.record_type_instantiations(inner),
            TypeKind::Function(params, return_type) => {
                for param in params {
                    self.record_type_instantiations(param);
                }
                self.record_type_instantiations(return_type);
            }
            TypeKind::Tuple(elements) => {
                for element in elements {
                    self.record_type_instantiations(element);
                }
            }
            _ => {}
        }
    }

    /// Binds a class's type parameters to the arguments of one of its instantiations.
    fn class_substitution(
        &self,
        module: &str,
        class_name: &str,
        arguments: &[Type],
    ) -> HashMap<String, TypeKind> {
        let generics = if module.is_empty() || module == self.module_name {
            match self.symtable.lookup_class(class_name) {
                Some(Symbol::Class { generics, .. }) => generics.clone(),
                _ => return HashMap::new(),
            }
        } else {
            match self
                .module_exports
                .get(module)
                .and_then(|exports| exports.classes.get(class_name))
            {
                Some(class) => class.generics.clone(),
                None => return HashMap::new(),
            }
        };

        generics
            .into_iter()
            .zip(arguments.iter().map(|argument| argument.kind.clone()))
            .collect()
    }

    /// Key under which a class's instantiations are recorded.
    ///
    /// Functions and classes share one instantiation graph so that transitive uses are
    /// resolved together, so class entries are namespaced to keep them distinct.
    fn class_key(name: &str) -> String {
        format!("class {}", name)
    }

    /// Key under which a function's instantiations are recorded.
    fn function_key(name: &str) -> String {
        name.to_string()
    }

    /// Key under which a generic method's instantiations are recorded. Its argument
    /// list contains class arguments followed by method arguments.
    fn method_key(class_name: &str, method_name: &str) -> String {
        format!("method {}.{}", class_name, method_name)
    }

    /// Records that a generic class is used at a particular instantiation.
    fn record_class_instantiation(
        &mut self,
        module_name: &str,
        name: &str,
        generics: &[String],
        subs: &HashMap<String, TypeKind>,
    ) {
        if generics.is_empty() {
            return;
        }

        let arguments: Vec<TypeKind> = generics
            .iter()
            .map(|parameter| {
                subs.get(parameter)
                    .cloned()
                    .unwrap_or(TypeKind::User("".to_string(), "error".to_string()))
            })
            .collect();

        // A use inside a generic function is not concrete yet; the fixpoint resolves it
        // once the enclosing function's own instantiations are known.
        if arguments
            .iter()
            .any(|argument| self.is_unresolved(argument))
        {
            if let Some(caller) = self.current_generic_owner.clone() {
                let site = GenericCallSite {
                    caller_module: self.module_name.clone(),
                    caller,
                    callee_module: module_name.to_string(),
                    callee: Self::class_key(name),
                    arguments,
                };

                if !self.generic_call_sites.contains(&site) {
                    self.generic_call_sites.push(site);
                }
            }
            return;
        }

        let instantiations = self
            .instantiations
            .entry(module_name.to_string())
            .or_default()
            .entry(Self::class_key(name))
            .or_default();

        if !instantiations.contains(&arguments) {
            instantiations.push(arguments);
        }
    }

    /// Records one generic method specialisation. Class arguments come first so the
    /// code generator can select the method instances belonging to each class instance.
    fn record_method_instantiation(
        &mut self,
        expr: &Expr,
        signature: &MethodSignature,
        method_substitutions: &HashMap<String, TypeKind>,
    ) {
        let method_arguments: Vec<TypeKind> = signature
            .method_generics
            .iter()
            .map(|parameter| {
                method_substitutions
                    .get(parameter)
                    .cloned()
                    .unwrap_or(TypeKind::User(String::new(), "error".to_string()))
            })
            .collect();
        self.call_instantiations
            .insert(expr as *const Expr, method_arguments.clone());

        let mut arguments: Vec<TypeKind> = signature
            .class_arguments
            .iter()
            .map(|argument| argument.kind.clone())
            .collect();
        arguments.extend(method_arguments);
        let callee = Self::method_key(&signature.class_name, &signature.method_name);

        if arguments.iter().any(|argument| self.is_unresolved(argument)) {
            if let Some(caller) = self.current_generic_owner.clone() {
                let site = GenericCallSite {
                    caller_module: self.module_name.clone(),
                    caller,
                    callee_module: signature.module.clone(),
                    callee,
                    arguments,
                };
                if !self.generic_call_sites.contains(&site) {
                    self.generic_call_sites.push(site);
                }
            }
            return;
        }

        let instances = self
            .instantiations
            .entry(signature.module.clone())
            .or_default()
            .entry(callee)
            .or_default();
        if !instances.contains(&arguments) {
            instances.push(arguments);
        }
    }

    fn check_method_signature(
        &mut self,
        expr: &Expr,
        token: &Token,
        signature: &MethodSignature,
        explicit: &[Type],
        arguments: &[Box<Expr>],
        argument_types: &[Type],
    ) -> Type {
        let class_substitutions: HashMap<String, TypeKind> = signature
            .class_generics
            .iter()
            .cloned()
            .zip(
                signature
                    .class_arguments
                    .iter()
                    .map(|argument| argument.kind.clone()),
            )
            .collect();
        let params: Vec<Type> = signature
            .params
            .iter()
            .map(|param| param.apply_substitution(&class_substitutions))
            .collect();
        let return_type = signature
            .return_type
            .apply_substitution(&class_substitutions);

        if params.len() != argument_types.len() {
            self.error_token(
                token,
                &format!(
                    "Method `{}` expects {} args, got {}",
                    signature.method_name,
                    params.len(),
                    argument_types.len()
                ),
            );
            return return_type;
        }

        let method_substitutions = self.resolve_generics(
            token,
            &signature.method_name,
            &signature.method_generics,
            &params,
            explicit,
            argument_types,
        );
        for (index, (expected, actual)) in params.iter().zip(argument_types).enumerate() {
            let expected = expected.apply_substitution(&method_substitutions);
            if let Some(argument) = arguments.get(index) {
                self.check_mutable_borrow(&expected, argument, actual);
            }
            if !actual.is_compatible_with(&expected) {
                self.error_token(
                    &get_token(&arguments[index]),
                    &format!(
                        "Argument mismatch in call to `{}`: expected `{}`, got `{}`",
                        signature.method_name, expected.kind, actual.kind
                    ),
                );
            }
        }

        self.record_method_instantiation(expr, signature, &method_substitutions);
        return_type.apply_substitution(&method_substitutions)
    }

    /// Records that `expr` calls a generic function at a particular instantiation, so
    /// code generation knows which specialisation to emit and to call.
    fn record_instantiation(
        &mut self,
        expr: &Expr,
        module: &str,
        name: &str,
        generics: &[String],
        subs: &HashMap<String, TypeKind>,
    ) {
        if generics.is_empty() {
            return;
        }

        let module = if module.is_empty() {
            self.module_name.clone()
        } else {
            module.to_string()
        };

        let arguments: Vec<TypeKind> = generics
            .iter()
            .map(|parameter| {
                subs.get(parameter)
                    .cloned()
                    .unwrap_or(TypeKind::User("".to_string(), "error".to_string()))
            })
            .collect();

        // Code generation needs the type arguments at every generic call site, even
        // ones that are still symbolic: when the enclosing generic function is emitted
        // for a particular instantiation, they get substituted then.
        self.call_instantiations
            .insert(expr as *const Expr, arguments.clone());

        // A call inside a generic function is not a concrete instantiation yet. Record
        // it so `expand_instantiations` can resolve it once the caller's own
        // instantiations are known.
        if arguments
            .iter()
            .any(|argument| self.is_unresolved(argument))
        {
            if let Some(caller) = self.current_generic_owner.clone() {
                let site = GenericCallSite {
                    caller_module: self.module_name.clone(),
                    caller,
                    callee_module: module.clone(),
                    callee: name.to_string(),
                    arguments,
                };

                if !self.generic_call_sites.contains(&site) {
                    self.generic_call_sites.push(site);
                }
            }
            return;
        }

        let instantiations = self
            .instantiations
            .entry(module)
            .or_default()
            .entry(name.to_string())
            .or_default();

        if !instantiations.contains(&arguments) {
            instantiations.push(arguments);
        }
    }

    /// Whether a type argument is still generic or came from an earlier error.
    fn is_unresolved(&self, kind: &TypeKind) -> bool {
        match kind {
            TypeKind::GenericParam(_) => true,
            // A user type named after an enclosing type parameter is that parameter,
            // not a concrete type: the call is inside a generic function.
            TypeKind::User(_, name) => {
                name == "error" || self.current_function_generics.contains(name)
            }
            TypeKind::Array(inner, _) => self.is_unresolved(&inner.kind),
            TypeKind::Reference(inner) | TypeKind::MutRef(inner) => self.is_unresolved(&inner.kind),
            TypeKind::Function(params, ret) => {
                params.iter().any(|p| self.is_unresolved(&p.kind)) || self.is_unresolved(&ret.kind)
            }
            TypeKind::Tuple(elements) => elements.iter().any(|e| self.is_unresolved(&e.kind)),
            TypeKind::GenericInstance(_, _, args) => {
                args.iter().any(|a| self.is_unresolved(&a.kind))
            }
            _ => false,
        }
    }

    /// Rewrites the named types that refer to a function's type parameters into
    /// `GenericParam`s.
    ///
    /// A parameter written `x: T` parses as the ordinary user type `T`, because the
    /// parser has no idea which names are type parameters. Substitution only acts on
    /// `GenericParam`, so this conversion is what makes a signature actually generic.
    fn generalise(ty: &Type, generics: &[String]) -> Type {
        let kind = match &ty.kind {
            TypeKind::User(_, name) if generics.contains(name) => {
                TypeKind::GenericParam(name.clone())
            }
            TypeKind::Array(inner, depth) => {
                TypeKind::Array(Box::new(Self::generalise(inner, generics)), *depth)
            }
            TypeKind::Reference(inner) => {
                TypeKind::Reference(Box::new(Self::generalise(inner, generics)))
            }
            TypeKind::MutRef(inner) => {
                TypeKind::MutRef(Box::new(Self::generalise(inner, generics)))
            }
            TypeKind::Function(params, ret) => TypeKind::Function(
                params
                    .iter()
                    .map(|p| Self::generalise(p, generics))
                    .collect(),
                Box::new(Self::generalise(ret, generics)),
            ),
            TypeKind::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(|e| Self::generalise(e, generics))
                    .collect(),
            ),
            // `Box<T>` mentions the type parameter in its arguments.
            TypeKind::GenericInstance(module, name, args) => TypeKind::GenericInstance(
                module.clone(),
                name.clone(),
                args.iter().map(|a| Self::generalise(a, generics)).collect(),
            ),
            other => other.clone(),
        };

        Type { kind, ..ty.clone() }
    }

    /// Matches a parameter type against the type of the argument supplied for it,
    /// recording what each type parameter must be.
    ///
    /// The first binding for a parameter wins; a later conflicting one is reported by
    /// the ordinary argument check once the substitution has been applied.
    fn infer_substitution(param: &Type, argument: &Type, subs: &mut HashMap<String, TypeKind>) {
        match (&param.kind, &argument.kind) {
            (TypeKind::GenericParam(name), _) => {
                // Borrows are transparent for inference: passing an `int` to a `&T`
                // should infer `T = int`, not `T = &int`.
                let resolved = Self::without_borrows(argument);
                subs.entry(name.clone()).or_insert(resolved.kind);
            }
            (TypeKind::Array(p, _), TypeKind::Array(a, _)) => Self::infer_substitution(p, a, subs),
            (TypeKind::Reference(p), TypeKind::Reference(a))
            | (TypeKind::MutRef(p), TypeKind::MutRef(a))
            | (TypeKind::Reference(p), TypeKind::MutRef(a)) => Self::infer_substitution(p, a, subs),
            // A borrowed parameter matched against a plain value: look through it.
            (TypeKind::Reference(p), _) | (TypeKind::MutRef(p), _) => {
                Self::infer_substitution(p, argument, subs)
            }
            (TypeKind::Function(p_params, p_ret), TypeKind::Function(a_params, a_ret)) => {
                for (p, a) in p_params.iter().zip(a_params.iter()) {
                    Self::infer_substitution(p, a, subs);
                }
                Self::infer_substitution(p_ret, a_ret, subs);
            }
            (TypeKind::Tuple(p_elements), TypeKind::Tuple(a_elements)) => {
                for (p, a) in p_elements.iter().zip(a_elements.iter()) {
                    Self::infer_substitution(p, a, subs);
                }
            }
            // `Box<T>` matched against `Box<int>` infers `T = int`.
            (
                TypeKind::GenericInstance(p_module, p_name, p_args),
                TypeKind::GenericInstance(a_module, a_name, a_args),
            ) if p_module == a_module && p_name == a_name => {
                for (p, a) in p_args.iter().zip(a_args.iter()) {
                    Self::infer_substitution(p, a, subs);
                }
            }
            _ => {}
        }
    }

    /// Looks through any borrows to the type actually being operated on.
    ///
    /// Members and indexing reach through a borrow: given `xs: &int[]`, `xs.len()` and
    /// `xs[0]` operate on the array. C++ references behave the same way, so the
    /// generated code needs no explicit dereference.
    fn without_borrows(ty: &Type) -> Type {
        match &ty.kind {
            TypeKind::Reference(inner) | TypeKind::MutRef(inner) => Self::without_borrows(inner),
            _ => ty.clone(),
        }
    }

    /// Whether `expr` denotes a place in memory, rather than a temporary value.
    ///
    /// Only an lvalue can be borrowed mutably: `#x` names something the callee can
    /// write back into, whereas `#(a + b)` or `#f()` would write into a value that is
    /// about to disappear. C++ enforces the same rule for binding `T&`.
    fn is_lvalue(expr: &Expr) -> bool {
        match expr {
            Expr::Variable { .. }
            | Expr::Index { .. }
            | Expr::MemberAccess { .. }
            | Expr::StaticAccess { .. } => true,
            Expr::Grouping { expression } => Self::is_lvalue(expression),
            Expr::Reference { object } | Expr::MutReference { object } => Self::is_lvalue(object),
            _ => false,
        }
    }

    /// Reports an error if `argument` cannot be borrowed mutably for a `#T` parameter.
    fn check_mutable_borrow(&self, expected: &Type, argument: &Expr, actual: &Type) {
        if !matches!(expected.kind, TypeKind::MutRef(_)) {
            return;
        }

        // Passing an existing mutable borrow along is fine whatever it came from.
        if matches!(actual.kind, TypeKind::MutRef(_)) {
            return;
        }

        if Self::is_lvalue(argument) {
            return;
        }

        let token = get_token(argument);
        self.error_with_notes(
            token.clone(),
            &format!(
                "Cannot mutably borrow a temporary value for a `{}` parameter",
                expected.kind
            ),
            vec![Note::new(
                "A `#` parameter writes back through the reference, so it needs a variable to write into".to_string(),
                token.line,
                token.span.clone(),
                self.filename.clone(),
            )],
            vec![Help::new(
                "Assign the value to a variable first, then pass that".to_string(),
                token.line,
                token.span.clone(),
                self.filename.clone(),
            )],
        );
    }

    /// Reports an error if `target_ty` is an immutable borrow being written through.
    fn check_assignable(&self, target_ty: &Type, token: &Token, description: &str) {
        if matches!(target_ty.kind, TypeKind::Reference(_)) {
            self.error_with_notes(
                token.clone(),
                &format!(
                    "Cannot assign through immutable borrow `{}`",
                    target_ty.kind
                ),
                vec![Note::new(
                    format!("{} is borrowed immutably with `&`", description),
                    token.line,
                    token.span.clone(),
                    self.filename.clone(),
                )],
                vec![Help::new(
                    format!(
                        "Use a mutable borrow instead: `&mut {}`",
                        target_ty.kind.to_string().trim_start_matches('&')
                    ),
                    token.line,
                    token.span.clone(),
                    self.filename.clone(),
                )],
            );
        }
    }

    /// The parameter types of whatever `callee` refers to, if it can be resolved.
    fn callee_param_types(&self, callee: &Expr) -> Option<Vec<Type>> {
        let callee_ty = self.get_expr_type(callee)?;

        match &callee_ty.kind {
            TypeKind::Function(params, _) => Some(params.clone()),
            // A bare function name in callee position is typed as `User(name)` and
            // resolved against the function table.
            TypeKind::User(_, name) => match self.symtable.lookup_function(name) {
                Some(Symbol::Function { params, .. }) => Some(params.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// The type the expression currently being checked is expected to have, if known.
    ///
    /// This comes either from an explicit expectation pushed by the surrounding
    /// expression (a declared variable type, or a parameter type at a call site), or
    /// from the variable being assigned to.
    fn expected_type(&self) -> Option<Type> {
        if let Some(ty) = &self.current_expected_type {
            return Some(ty.clone());
        }

        match &self.current_assignment {
            Some(Expr::Assignment { name, .. }) | Some(Expr::MemberAssignment { name, .. }) => {
                match self.symtable.lookup_symbol(&name.lexeme) {
                    Some(Symbol::Variable(_, _, ty, ..)) => Some(ty.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Runs `f` with `expected` as the expected type, restoring the previous one after.
    fn with_expected_type<R>(
        &mut self,
        expected: Option<Type>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let previous = std::mem::replace(&mut self.current_expected_type, expected);
        let result = f(self);
        self.current_expected_type = previous;
        result
    }

    /// Whether a member of `class_name` with the given visibility is reachable from the
    /// code currently being checked.
    fn is_member_visible(&self, visibility: &Visibility, class_name: &str) -> bool {
        match visibility {
            Visibility::Public => true,
            Visibility::Private => Some(class_name) == self.current_class.as_deref(),
            Visibility::Protected => {
                // TODO: Add inheritance check once subclassing is implemented.
                self.current_class
                    .as_deref()
                    .map_or(false, |current| current == class_name)
            }
        }
    }

    /// Maps a member's modifiers onto its symbol-table visibility.
    fn visibility_of(&self, modifiers: &[Modifier]) -> Visibility {
        match member_visibility(modifiers) {
            Modifier::Public => Visibility::Public,
            Modifier::Protected => Visibility::Protected,
            _ => Visibility::Private,
        }
    }

    /// Creates an `Error` object based on a `Token` (for line/col info) and appends it to `self.errors`.
    fn error_token(&self, token: &Token, message: &str) {
        let mut err = Error::new(
            message.to_string(),
            token.line,
            token.span.clone(),
            self.filename.clone(),
        );
        err.add_source(self.source.clone()); // attach entire source for caret display

        self.errors.borrow_mut().push(err);
    }

    fn error_with_notes(&self, token: Token, message: &str, notes: Vec<Note>, helps: Vec<Help>) {
        let mut err = Error::new(
            message.to_string(),
            token.line,
            token.span,
            self.filename.clone(),
        );
        err.add_source(self.source.clone());

        for note in notes {
            err.add_note(note);
        }

        for help in helps {
            err.add_help(help);
        }

        self.errors.borrow_mut().push(err);
    }

    /// Store `ty` in the map for `expr`.
    fn set_expr_type(&mut self, expr: &Expr, ty: Type) {
        self.expr_types.insert(expr as *const Expr, ty);
    }

    /// Retrieve the stored type for `expr` (if any).
    fn get_expr_type(&self, expr: &Expr) -> Option<&Type> {
        self.expr_types.get(&(expr as *const Expr))
    }

    fn type_exists(&self, ty: &Type) -> bool {
        match &ty.kind {
            TypeKind::Int | TypeKind::Float | TypeKind::String | TypeKind::Void => true,
            // A module is a namespace, not something a variable can be declared as.
            TypeKind::Module(_) => false,
            TypeKind::User(module, name) => {
                // Keep cascaded diagnostics readable once an earlier expression has failed.
                if name == "error" || name == "bool" {
                    return true;
                }

                let local = module.is_empty() || module == &self.module_name;
                let is_generic_param = local
                    && matches!(
                        self.symtable.lookup_symbol(name),
                        Some(Symbol::Variable(_, _, ty, ..))
                            if matches!(ty.kind, TypeKind::GenericParam(_))
                    );
                let class_exists = if local {
                    self.symtable.lookup_type(name).is_some()
                } else {
                    self.module_exports
                        .get(module)
                        .is_some_and(|exports| exports.classes.contains_key(name))
                };

                (class_exists || is_generic_param)
                    && ty.generics.iter().all(|arg| self.type_exists(arg))
            }
            TypeKind::GenericParam(name) => self.symtable.lookup_symbol(name).is_some(),
            TypeKind::GenericInstance(module, name, args) => {
                let local = module.is_empty() || module == &self.module_name;
                let class_exists = if local {
                    self.symtable.lookup_type(name).is_some()
                } else {
                    self.module_exports
                        .get(module)
                        .is_some_and(|exports| exports.classes.contains_key(name))
                };
                class_exists && args.iter().all(|arg| self.type_exists(arg))
            }
            TypeKind::Reference(inner) | TypeKind::MutRef(inner) => {
                // Check if the inner type exists
                self.type_exists(inner)
            }
            TypeKind::Array(inner, _) => {
                // Check if the inner type exists
                self.type_exists(inner)
            }
            TypeKind::Function(params, return_ty) => {
                // Check if the return type exists
                if !self.type_exists(return_ty) {
                    return false;
                }

                // Check if all parameter types exist
                params.iter().all(|p| self.type_exists(p))
            }
            TypeKind::Tuple(types) => types.iter().all(|ty| self.type_exists(ty)),
        }
    }

    fn error_type(&self, token: &Token) -> Type {
        Type::new(
            token.clone(),
            TypeKind::User("".to_string(), "error".to_string()),
        )
    }

    fn int_type(&self, token: &Token) -> Type {
        Type::new(token.clone(), TypeKind::Int)
    }

    fn string_type(&self, token: &Token) -> Type {
        Type::new(token.clone(), TypeKind::String)
    }

    fn void_type(&self, token: &Token) -> Type {
        Type::new(token.clone(), TypeKind::Void)
    }

    fn function_type(&self, token: &Token, params: Vec<Type>, return_type: Type) -> Type {
        Type::new(
            token.clone(),
            TypeKind::Function(params, Box::new(return_type)),
        )
    }

    fn statements_guarantee_return(&self, statements: &[Box<Stmt>]) -> bool {
        statements
            .iter()
            .any(|stmt| self.statement_guarantees_return(stmt))
    }

    fn statement_guarantees_return(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return { .. } => true,
            Stmt::Block { statements } => self.statements_guarantee_return(statements),
            Stmt::If {
                then_branch,
                else_branch: Some(else_branch),
                ..
            } => {
                self.statement_guarantees_return(then_branch)
                    && self.statement_guarantees_return(else_branch)
            }
            // `@cpp("..")` is opaque to the checker: the spliced C++ may well return, so
            // treat it as satisfying the return requirement rather than reporting a
            // false positive on every standard library primitive.
            Stmt::Expression { expression } => {
                matches!(&**expression, Expr::Intrinsic { name, .. } if name.lexeme == "cpp")
            }
            _ => false,
        }
    }

    fn register_extensions(&mut self, module: &Module) {
        for stmt in &module.statements {
            if let Stmt::Extension { target, methods } = &**stmt {
                let target_name = target.name.lexeme.clone();
                for method in methods {
                    if let Stmt::Function {
                        name,
                        params,
                        return_type,
                        ..
                    } = &**method
                    {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(self.qualify_type(type_));
                            }
                        }
                        // Create a symbol for the extension method.
                        let sym = Symbol::new_function_with_visibility(
                            name.clone(),
                            param_types,
                            self.qualify_type(return_type),
                            true, // mark as method (or extension)
                            Some(Visibility::Public),
                            false,
                        );
                        // Register the method for the target type.
                        self.symtable.register_extension_method(&target_name, sym);
                    }
                }
            }
        }
    }
}

fn get_token(expr: &Expr) -> Token {
    match expr {
        Expr::Binary { op, .. }
        | Expr::Unary { op, .. }
        | Expr::Literal { value: op, .. }
        | Expr::Variable { name: op }
        | Expr::Assignment { name: op, .. }
        | Expr::IndexAssignment { op, .. }
        | Expr::MemberAssignment { name: op, .. }
        | Expr::MemberAccess { name: op, .. }
        | Expr::Call { paren: op, .. } => op.clone(),
        Expr::Grouping { expression } => get_token(expression),
        Expr::Array { token, .. } => token.clone(),
        Expr::Tuple { elements } => elements
            .first()
            .map(|expr| get_token(expr))
            .unwrap_or_else(|| Token::dummy("()")),
        _ => Token::dummy("unknown"),
    }
}

fn get_token_s(stmt: &Stmt) -> Token {
    match stmt {
        Stmt::Return { token, value } => {
            if let Some(expr) = value {
                get_token(expr)
            } else {
                token.clone()
            }
        }
        Stmt::Expression { expression: expr } => get_token(expr),
        _ => Token::dummy("unknown"),
    }
}

impl<'a> Visitor for TypeChecker<'a> {
    fn visit_binary(&mut self, expr: &Expr) {
        if let Expr::Binary { left, op, right } = expr {
            left.accept(self);
            right.accept(self);

            let left_ty = self.get_expr_type(left).cloned().unwrap_or_else(|| {
                self.error_token(&op, "Left operand has unknown type in binary operation");

                let token = get_token(left);

                Type::new(
                    token.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            let right_ty = self.get_expr_type(right).cloned().unwrap_or_else(|| {
                self.error_token(&op, "Right operand has unknown type in binary operation");

                let token = get_token(right);

                Type::new(token, TypeKind::User("".to_string(), "error".to_string()))
            });

            // Example: handle arithmetic vs. comparison operators
            let result_ty = if left_ty.is_compatible_with(&right_ty) {
                match op.kind {
                    TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div => {
                        if left_ty.kind == TypeKind::Float || right_ty.kind == TypeKind::Float {
                            if left_ty.kind == TypeKind::Float {
                                left_ty
                            } else {
                                right_ty
                            }
                        } else {
                            left_ty
                        }
                    }
                    TokenKind::EqEq
                    | TokenKind::Neq
                    | TokenKind::Lt
                    | TokenKind::Gt
                    | TokenKind::Lte
                    | TokenKind::Gte => {
                        Type::new(Token::dummy("int"), TypeKind::Int) // booleans are just integers.
                    }
                    _ => {
                        // fallback: just assume same as left
                        left_ty
                    }
                }
            } else {
                self.error_token(
                    &op,
                    &format!(
                        "Type mismatch in binary operation: `{}` vs `{}` using `{}`",
                        left_ty.kind, right_ty.kind, op.lexeme
                    ),
                );

                Type::new(
                    op.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            };

            self.set_expr_type(expr, result_ty);
        }
    }

    fn visit_unary(&mut self, expr: &Expr) {
        if let Expr::Unary { op, right } = expr {
            right.accept(self);
            let right_ty = self.get_expr_type(right).cloned().unwrap_or_else(|| {
                self.error_token(&op, "Unknown type for operand in unary expression");
                Type::new(
                    op.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            let result_ty = match op.kind {
                TokenKind::Minus => {
                    // numeric only
                    if right_ty.is_primitive() {
                        right_ty.clone()
                    } else {
                        self.error_token(
                            &op,
                            &format!("Cannot apply unary minus to `{}`", right_ty.kind),
                        );
                        Type::new(
                            op.clone(),
                            TypeKind::User("".to_string(), "error".to_string()),
                        )
                    }
                }
                _ => right_ty.clone(),
            };

            self.set_expr_type(expr, result_ty);
        }
    }

    fn visit_literal(&mut self, expr: &Expr) {
        if let Expr::Literal { value } = expr {
            // Infer type from token kind
            let lit_ty = match value.kind {
                TokenKind::Integer => Type::new(value.clone(), TypeKind::Int),
                TokenKind::Float => Type::new(value.clone(), TypeKind::Float),
                TokenKind::String => Type::new(value.clone(), TypeKind::String),
                _ => {
                    // fallback
                    Type::new(
                        value.clone(),
                        TypeKind::User("".to_string(), "bool".to_string()),
                    )
                }
            };
            self.set_expr_type(expr, lit_ty);
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) {
        if let Expr::Grouping { expression } = expr {
            expression.accept(self);
            if let Some(ty) = self.get_expr_type(expression).cloned() {
                self.set_expr_type(expr, ty);
            }
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr) {
        if let Expr::Variable { name } = expr {
            // An imported module name shadows nothing and is resolved first, so that
            // `io.println(..)` works anywhere in the file.
            if let Some(Symbol::Variable(_, _, module_ty, ..)) =
                self.symtable.lookup_module(&name.lexeme)
            {
                let module_ty = module_ty.clone();
                self.set_expr_type(expr, module_ty);
                return;
            }

            // Look in local scopes
            if let Some(sym) = self.symtable.lookup_symbol(&name.lexeme) {
                match sym {
                    Symbol::Variable(_, _, var_ty, ..) => {
                        self.set_expr_type(expr, var_ty.clone());
                    }
                    Symbol::Function { .. } => {
                        if self.in_call {
                            // Allow function references in call expressions
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    name.clone(),
                                    TypeKind::User("".to_string(), name.lexeme.clone()),
                                ),
                            );
                        } else {
                            self.error_token(
                                name,
                                &format!("`{}` is a function, not a variable", name.lexeme),
                            );
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    name.clone(),
                                    TypeKind::User("".to_string(), "error".to_string()),
                                ),
                            );
                        }
                    }
                    Symbol::Class { .. } => {
                        // Allow class references for static access
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("".to_string(), name.lexeme.clone()),
                            ),
                        );
                    }
                }
            } else {
                // Check if it's a class name (for static access) or global function
                if let Some(Symbol::Class { .. }) = self.symtable.lookup_class(&name.lexeme) {
                    // It's a valid class reference
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("".to_string(), name.lexeme.clone()),
                        ),
                    );
                } else if let Some(symbol) = self.symtable.lookup_function(&name.lexeme) {
                    if self.in_call {
                        // In callee position the name is resolved by `visit_call`, which
                        // looks the function up again to report better diagnostics.
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("".to_string(), name.lexeme.clone()),
                            ),
                        );
                    } else if let Symbol::Function {
                        params,
                        return_type,
                        ..
                    } = symbol
                    {
                        // Used as a value, a function has its own function type, so it
                        // can be stored in a variable or passed to another function.
                        let function_ty =
                            self.function_type(name, params.clone(), return_type.clone());
                        self.set_expr_type(expr, function_ty);
                    } else {
                        self.set_expr_type(expr, self.error_type(name));
                    }
                } else {
                    self.error_token(name, &format!("Unknown variable `{}`", name.lexeme));
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("".to_string(), "error".to_string()),
                        ),
                    );
                }
            }
        }
    }

    fn visit_assignment(&mut self, expr: &Expr) {
        if let Expr::Assignment { name, value, .. } = expr {
            self.current_assignment = Some(expr.clone());
            value.accept(self);
            self.current_assignment = None;

            let rhs_ty = self.get_expr_type(value).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            // Check if the type of the right-hand side exists
            if !self.type_exists(&rhs_ty) {
                self.error_token(name, &format!("Type `{}` does not exist", rhs_ty.kind));
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("".to_string(), "error".to_string()),
                    ),
                );
                return;
            }

            // Check if the variable being assigned exists
            if let Some(sym) = self.symtable.lookup_symbol(&name.lexeme) {
                if let Symbol::Variable(_, _, var_ty, ..) = sym {
                    // Writing through an immutable borrow is not allowed; the generated
                    // `const T&` would reject it too.
                    self.check_assignable(var_ty, name, &format!("`{}`", name.lexeme));

                    // Check if the type of the variable exists
                    if !self.type_exists(var_ty) {
                        self.error_token(name, &format!("Type `{}` does not exist", var_ty.kind));
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("".to_string(), "error".to_string()),
                            ),
                        );
                        return;
                    }

                    // Assignment writes a value through a borrow; it does not rebind
                    // the reference. An immutable source may therefore be copied into
                    // a mutable destination without converting &T into &mut T.
                    let target_ty = Self::without_borrows(var_ty);
                    if !rhs_ty.is_compatible_with(&target_ty) {
                        self.error_with_notes(
                            name.clone(),
                            &format!(
                                "Cannot assign `{}` to variable of type `{}`",
                                rhs_ty.kind, var_ty.kind
                            ),
                            vec![Note::new(
                                format!(
                                    "Variable `{}` is declared here with type `{}`",
                                    name.lexeme, var_ty.kind
                                ),
                                var_ty.name.line,
                                var_ty.name.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                format!("Try using a value of type `{}`", var_ty.kind),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                    }
                    // Assignment expression type => var's type
                    self.set_expr_type(expr, var_ty.clone());
                } else {
                    self.error_token(name, &format!("Symbol `{}` is not a variable", name.lexeme));
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("".to_string(), "error".to_string()),
                        ),
                    );
                }
            } else {
                self.error_token(name, &format!("Unknown variable `{}`", name.lexeme));
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("".to_string(), "error".to_string()),
                    ),
                );
            }
        }
    }

    fn visit_member_assignment(&mut self, expr: &Expr) {
        if let Expr::MemberAssignment {
            object,
            name,
            value,
            ..
        } = expr
        {
            object.accept(self);

            self.current_assignment = Some(expr.clone());
            value.accept(self);
            self.current_assignment = None;

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let rhs_ty = self.get_expr_type(value).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            let obj_ty = Self::without_borrows(&obj_ty);

            if let TypeKind::User(_, ref class_name) = obj_ty.kind {
                if let Some(Symbol::Class { fields, .. }) = self.symtable.lookup_class(class_name) {
                    if let Some(field_ty) = fields.get(&name.lexeme) {
                        if !rhs_ty.is_compatible_with(&field_ty.0) {
                            self.error_with_notes(
                                name.clone(),
                                &format!(
                                    "Type mismatch in member assignment. Expected `{}`, got `{}`",
                                    field_ty.0.kind, rhs_ty.kind
                                ),
                                vec![Note::new(
                                    format!(
                                        "Field `{}` is declared here with type `{}`",
                                        name.lexeme, field_ty.0.kind
                                    ),
                                    field_ty.0.name.line,
                                    field_ty.0.name.span.clone(),
                                    self.filename.clone(),
                                )],
                                vec![Help::new(
                                    format!("Try using a value of type `{}`", field_ty.0.kind),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone(),
                                )],
                            );
                        }
                        self.set_expr_type(expr, field_ty.0.clone());
                    } else {
                        self.error_token(
                            name,
                            &format!("No field `{}` in class `{}`", name.lexeme, class_name),
                        );
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("".to_string(), "error".to_string()),
                            ),
                        );
                    }
                } else {
                    self.error_token(
                        name,
                        &format!("`{}` is not a defined class type", class_name),
                    );
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("".to_string(), "error".to_string()),
                        ),
                    );
                }
            } else {
                self.error_token(
                    name,
                    &format!(
                        "Cannot do member assignment on non-class type `{}`",
                        obj_ty.kind
                    ),
                );
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("".to_string(), "error".to_string()),
                    ),
                );
            }
        }
    }

    fn visit_static_access(&mut self, expr: &Expr) {
        if let Expr::StaticAccess { object, name } = expr {
            let old_static_context = self.in_static_context;
            self.in_static_context = true;

            // First visit the object expression to type check it
            object.accept(self);

            // Get the class name from the object expression
            let class_name = match &**object {
                Expr::Variable { name: class_name } => &class_name.lexeme,
                _ => {
                    self.error_token(name, "Static access must be on a class name");
                    self.set_expr_type(expr, self.error_type(name));
                    self.in_static_context = old_static_context;
                    return;
                }
            };

            // Look up the class and verify static member access
            if let Some(Symbol::Class {
                fields, methods, ..
            }) = self.symtable.lookup_class(class_name)
            {
                // Check static fields first
                if let Some((field_ty, visibility, is_static)) = fields.get(&name.lexeme) {
                    if !self.is_member_visible(visibility, class_name) {
                        self.error_token(
                            name,
                            &format!(
                                "Cannot access {} static field `{}` of class `{}`",
                                visibility.to_string().to_lowercase(),
                                name.lexeme,
                                class_name
                            ),
                        );
                    }
                    if !is_static {
                        self.error_with_notes(
                            name.clone(),
                            &format!(
                                "Cannot access non-static field `{}` in static context",
                                name.lexeme
                            ),
                            vec![Note::new(
                                format!("Field `{}` is defined here as non-static", name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                "Try accessing the field using an instance of the class"
                                    .to_string(),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                    }
                    self.set_expr_type(expr, field_ty.clone());
                } else if let Some(Symbol::Function {
                    params,
                    return_type,
                    is_static,
                    visibility,
                    ..
                }) = methods.get(&name.lexeme)
                {
                    if let Some(visibility) = visibility {
                        if !self.is_member_visible(visibility, class_name) {
                            self.error_token(
                                name,
                                &format!(
                                    "Cannot access {} static method `{}` of class `{}`",
                                    visibility.to_string().to_lowercase(),
                                    name.lexeme,
                                    class_name
                                ),
                            );
                        }
                    }
                    if !is_static {
                        self.error_with_notes(
                            name.clone(),
                            &format!(
                                "Cannot access non-static method `{}` in static context",
                                name.lexeme
                            ),
                            vec![Note::new(
                                format!("Method `{}` is defined here as non-static", name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                "Try accessing the method using an instance of the class"
                                    .to_string(),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                    }
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::Function(params.clone(), Box::new(return_type.clone())),
                        ),
                    );
                } else {
                    self.error_token(
                        name,
                        &format!(
                            "No static member `{}` found in class `{}`",
                            name.lexeme, class_name
                        ),
                    );
                    self.set_expr_type(expr, self.error_type(name));
                }
            } else {
                self.error_token(
                    name,
                    &format!("Unknown class `{}` in static access", class_name),
                );
                self.set_expr_type(expr, self.error_type(name));
            }

            self.in_static_context = old_static_context;
        }
    }

    fn visit_static_assignment(&mut self, expr: &Expr) {
        if let Expr::StaticAssignment {
            object,
            name,
            value,
            ..
        } = expr
        {
            object.accept(self);
            self.current_assignment = Some(expr.clone());
            value.accept(self);
            self.current_assignment = None;

            let rhs_ty = self.get_expr_type(value).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            let class_name = match &**object {
                Expr::Variable { name: class_name } => class_name.lexeme.clone(),
                _ => {
                    self.error_token(name, "Static assignment must be on a class name");
                    self.set_expr_type(expr, self.error_type(name));
                    return;
                }
            };

            // Look up the static field in the class
            if let Some(Symbol::Class { fields, .. }) = self.symtable.lookup_class(&class_name) {
                if let Some((field_ty, visibility, is_static)) = fields.get(&name.lexeme) {
                    if !self.is_member_visible(visibility, &class_name) {
                        self.error_token(
                            name,
                            &format!(
                                "Cannot assign to {} static field `{}` of class `{}`",
                                visibility.to_string().to_lowercase(),
                                name.lexeme,
                                class_name
                            ),
                        );
                    }
                    if !is_static {
                        self.error_token(
                            name,
                            &format!(
                                "Cannot assign non-static field `{}` using static access",
                                name.lexeme
                            ),
                        );
                    }
                    if !rhs_ty.is_compatible_with(field_ty) {
                        self.error_token(
                            name,
                            &format!(
                                "Cannot assign value of type `{}` to static field `{}` of type `{}`",
                                rhs_ty.kind, name.lexeme, field_ty.kind
                            ),
                        );
                    }
                    self.set_expr_type(expr, field_ty.clone());
                    return;
                }

                self.error_token(
                    name,
                    &format!(
                        "No static field `{}` found in class `{}`",
                        name.lexeme, class_name
                    ),
                );
            } else {
                self.error_token(
                    name,
                    &format!("Cannot find class `{}` for static assignment", class_name),
                );
            }

            // Set error type if we couldn't resolve the static field
            self.set_expr_type(
                expr,
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                ),
            );
        }
    }

    fn visit_index_assignment(&mut self, expr: &Expr) {
        if let Expr::IndexAssignment {
            object,
            index,
            value,
            token,
            ..
        } = expr
        {
            object.accept(self);
            index.accept(self);
            value.accept(self);

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    token.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let idx_ty = self.get_expr_type(index).cloned().unwrap_or_else(|| {
                Type::new(
                    token.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let rhs_ty = self.get_expr_type(value).cloned().unwrap_or_else(|| {
                Type::new(
                    token.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let obj_ty = Self::without_borrows(&obj_ty);

            if let TypeKind::Array(elem_ty, _) = obj_ty.kind {
                // index must be int
                if idx_ty.kind != TypeKind::Int {
                    self.error_with_notes(
                        token.clone(),
                        "Array index must be an integer",
                        vec![Note::new(
                            format!("Found index of type `{}`", idx_ty.kind),
                            token.line,
                            token.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![Help::new(
                            "Try using an integer expression for the index".to_string(),
                            token.line,
                            token.span.clone(),
                            self.filename.clone(),
                        )],
                    );
                }
                if !rhs_ty.is_compatible_with(&elem_ty) {
                    self.error_token(
                        &token,
                        &format!(
                            "Array assignment mismatch. Expected `{}`, got `{}`",
                            elem_ty.kind, rhs_ty.kind
                        ),
                    );
                }
                self.set_expr_type(expr, *elem_ty.clone());
            } else {
                self.error_with_notes(
                    token.clone(),
                    &format!("Cannot index into non-array type `{}`", obj_ty.kind),
                    vec![Note::new(
                        "Double check the type definitions and try again".to_string(),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![],
                );
            }
        }
    }

    fn visit_call(&mut self, expr: &Expr) {
        if let Expr::Call {
            callee,
            arguments,
            paren,
        } = expr
        {
            self.in_call = true;
            callee.accept(self);
            self.in_call = false;

            // Knowing the parameter types up front lets each argument be checked against
            // what it is expected to be, so `f([])` can infer the empty literal.
            let expected_params = self.callee_param_types(callee);

            let arg_tys: Vec<Type> = arguments
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let expected = expected_params
                        .as_ref()
                        .and_then(|params| params.get(i).cloned());

                    self.with_expected_type(expected, |checker| {
                        a.accept(checker);
                    });

                    self.get_expr_type(a).cloned().unwrap_or_else(|| {
                        Type::new(
                            paren.clone(),
                            TypeKind::User("".to_string(), "error".to_string()),
                        )
                    })
                })
                .collect();

            // A call through an imported module is resolved against that module's
            // exported signature, so its type parameters are still known here.
            if let Some((module, name, function)) = self.module_callee(callee) {
                let result = self.check_call_signature(
                    expr,
                    &paren,
                    &module,
                    &name,
                    &function.generics,
                    &function.params,
                    &function.return_type,
                    &[],
                    arguments,
                    &arg_tys,
                );
                self.set_expr_type(expr, result);
                return;
            }

            if let Some(signature) = self.method_callee(callee) {
                if !signature.method_generics.is_empty() {
                    let result = self.check_method_signature(
                        expr,
                        &get_token(callee),
                        &signature,
                        &[],
                        arguments,
                        &arg_tys,
                    );
                    self.set_expr_type(expr, result);
                    return;
                }
            }

            let callee_ty = self.get_expr_type(callee).cloned().unwrap_or_else(|| {
                Type::new(
                    paren.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            match callee_ty.kind {
                TypeKind::Function(param_tys, ret_ty) => {
                    if param_tys.len() != arg_tys.len() {
                        self.error_with_notes(
                            paren.clone(),
                            &format!(
                                "Function expects {} args, found {}",
                                param_tys.len(),
                                arg_tys.len()
                            ),
                            vec![Note::new(
                                format!("Function declared with {} parameter(s)", param_tys.len()),
                                paren.line,
                                paren.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                if param_tys.len() > arg_tys.len() {
                                    "Try adding the missing argument(s)"
                                } else {
                                    "Try removing the extra argument(s)"
                                }
                                .to_string(),
                                paren.line,
                                paren.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                    } else {
                        // Create a substitution map for monomorphization
                        let mut subs: HashMap<String, TypeKind> = HashMap::new();
                        for (i, (expected, actual)) in
                            param_tys.iter().zip(arg_tys.iter()).enumerate()
                        {
                            if let Some(argument) = arguments.get(i) {
                                self.check_mutable_borrow(expected, argument, actual);
                            }

                            if !actual.is_compatible_with(expected) {
                                self.error_with_notes(
                                    actual.name.clone(),
                                    &format!(
                                        "Argument type mismatch: expected `{}`, got `{}`",
                                        expected.kind, actual.kind
                                    ),
                                    vec![Note::new(
                                        format!(
                                            "Function was defined with argument type `{}`",
                                            expected.kind
                                        ),
                                        expected.name.line,
                                        expected.name.span.clone(),
                                        self.filename.clone(),
                                    )],
                                    vec![],
                                );
                            } else {
                                // If the expected type is a generic parameter, record the substitution
                                if let TypeKind::GenericParam(param_name) = &expected.kind {
                                    subs.insert(param_name.clone(), actual.kind.clone());
                                }
                            }
                        }

                        // Instantiate the function type with the substitutions
                        let instantiated_ret_ty = ret_ty.apply_substitution(&subs);
                        self.set_expr_type(expr, instantiated_ret_ty);
                    }
                }
                TypeKind::User(ref module, ref name) => {
                    if let Some(sym) = self.symtable.lookup_function(name) {
                        if let Symbol::Function {
                            params,
                            return_type,
                            generics,
                            ..
                        } = sym
                        {
                            let (params, return_type, generics) =
                                (params.clone(), return_type.clone(), generics.clone());

                            let result = self.check_call_signature(
                                expr,
                                &paren,
                                &module,
                                name,
                                &generics,
                                &params,
                                &return_type,
                                &[],
                                arguments,
                                &arg_tys,
                            );
                            self.set_expr_type(expr, result);
                        } else {
                            self.error_token(
                                &paren,
                                &format!("`{}` is not a function symbol", name),
                            );
                            let token = get_token(callee);
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    token.clone(),
                                    TypeKind::User("".to_string(), "error".to_string()),
                                ),
                            );
                        }
                    } else {
                        self.error_token(
                            &paren,
                            &format!("Cannot call object of type `{}`", callee_ty.kind),
                        );
                        let token = get_token(callee);
                        self.set_expr_type(
                            expr,
                            Type::new(
                                token.clone(),
                                TypeKind::User("".to_string(), "error".to_string()),
                            ),
                        );
                    }
                }
                TypeKind::Int | TypeKind::Float | TypeKind::String => {
                    self.error_token(
                        &paren,
                        &format!("Cannot call non-function type `{}`", callee_ty.kind),
                    );
                    let token = get_token(callee);
                    self.set_expr_type(expr, self.error_type(&token));
                }
                _ => {
                    self.error_token(
                        &paren,
                        &format!("Cannot call non-function type `{}`", callee_ty.kind),
                    );
                    let token = get_token(callee);
                    self.set_expr_type(expr, self.error_type(&token));
                }
            }
        }
    }

    fn visit_generic_call(&mut self, expr: &Expr) {
        if let Expr::GenericCall {
            callee,
            arguments,
            generics,
            ..
        } = expr
        {
            self.in_call = true;
            callee.accept(self);
            self.in_call = false;

            let arg_tys: Vec<Type> = arguments
                .iter()
                .map(|a| {
                    a.accept(self);
                    self.get_expr_type(a).cloned().unwrap_or_else(|| {
                        Type::new(
                            get_token(a),
                            TypeKind::User("".to_string(), "error".to_string()),
                        )
                    })
                })
                .collect();

            // `mod.generic<int>(..)` resolves against the module's exported signature.
            if let Some((module, name, function)) = self.module_callee(callee) {
                let token = get_token(callee);
                let explicit: &[Type] = if function.generics.is_empty() {
                    self.error_token(
                        &token,
                        &format!("`{}` is not generic, so it takes no type arguments", name),
                    );
                    &[]
                } else {
                    generics
                };
                let explicit = self.generalise_explicit(explicit);

                let result = self.check_call_signature(
                    expr,
                    &token,
                    &module,
                    &name,
                    &function.generics,
                    &function.params,
                    &function.return_type,
                    &explicit,
                    arguments,
                    &arg_tys,
                );
                self.set_expr_type(expr, result);
                return;
            }

            if let Some(signature) = self.method_callee(callee) {
                let token = get_token(callee);
                let explicit: &[Type] = if signature.method_generics.is_empty() {
                    self.error_token(
                        &token,
                        &format!(
                            "`{}` is not generic, so it takes no type arguments",
                            signature.method_name
                        ),
                    );
                    &[]
                } else {
                    generics
                };
                let explicit = self.generalise_explicit(explicit);
                let result = self.check_method_signature(
                    expr,
                    &token,
                    &signature,
                    &explicit,
                    arguments,
                    &arg_tys,
                );
                self.set_expr_type(expr, result);
                return;
            }

            let callee_ty = self.get_expr_type(callee).cloned().unwrap_or_else(|| {
                Type::new(
                    get_token(callee),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            match callee_ty.kind {
                TypeKind::Function(param_tys, ret_ty) => {
                    if param_tys.len() != arg_tys.len() {
                        self.error_token(
                            &get_token(callee),
                            &format!(
                                "Function expects {} args, found {}",
                                param_tys.len(),
                                arg_tys.len()
                            ),
                        );
                    } else {
                        // Create a substitution map for monomorphization
                        let mut subs: HashMap<String, TypeKind> = HashMap::new();

                        // Fill substitution map with provided generic types
                        for (i, param) in param_tys.iter().enumerate() {
                            if let TypeKind::GenericParam(param_name) = &param.kind {
                                if let Some(generic_type) = generics.get(i) {
                                    subs.insert(param_name.clone(), generic_type.kind.clone());
                                }
                            }
                        }

                        // Now check arguments against substituted parameter types
                        for (expected, actual) in param_tys.iter().zip(arg_tys.iter()) {
                            let substituted_type = expected.apply_substitution(&subs);

                            if !actual.is_compatible_with(&substituted_type) {
                                self.error_token(
                                    &get_token(callee),
                                    &format!(
                                        "Argument mismatch in call to `{}`: expected `{}`, got `{}`",
                                        get_token(callee).lexeme, substituted_type.kind, actual.kind
                                    ),
                                );
                            }
                        }

                        let instantiated_ret_ty = ret_ty.apply_substitution(&subs);
                        self.set_expr_type(expr, instantiated_ret_ty);
                    }
                }
                TypeKind::User(ref module, ref name) => {
                    if let Some(sym) = self.symtable.lookup_function(name) {
                        // `generics` here is the *call's* explicit type arguments; the
                        // function's own type parameter names come from the symbol.
                        if let Symbol::Function {
                            params,
                            return_type,
                            generics: declared,
                            ..
                        } = sym
                        {
                            let (params, return_type, fn_generics) =
                                (params.clone(), return_type.clone(), declared.clone());
                            let token = get_token(callee);

                            // Reporting "not generic" is clearer than an arity
                            // complaint about a list of no type parameters, so the
                            // explicit arguments are dropped in that case.
                            let explicit: &[Type] = if fn_generics.is_empty() {
                                self.error_token(
                                    &token,
                                    &format!(
                                        "`{}` is not generic, so it takes no type arguments",
                                        name
                                    ),
                                );
                                &[]
                            } else {
                                generics
                            };
                            let explicit = self.generalise_explicit(explicit);

                            let result = self.check_call_signature(
                                expr,
                                &token,
                                &module,
                                name,
                                &fn_generics,
                                &params,
                                &return_type,
                                &explicit,
                                arguments,
                                &arg_tys,
                            );
                            self.set_expr_type(expr, result);
                        } else {
                            self.error_token(
                                &get_token(callee),
                                &format!("`{}` is not a function symbol", name),
                            );
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    get_token(callee),
                                    TypeKind::User("".to_string(), "error".to_string()),
                                ),
                            );
                        }
                    } else {
                        self.error_token(
                            &get_token(callee),
                            &format!("Cannot call object of type `{}`", callee_ty.kind),
                        );
                        self.set_expr_type(
                            expr,
                            Type::new(
                                get_token(callee),
                                TypeKind::User("".to_string(), "error".to_string()),
                            ),
                        );
                    }
                }
                _ => {
                    self.error_token(
                        &get_token(callee),
                        &format!("Cannot call non-function type `{}`", callee_ty.kind),
                    );
                    self.set_expr_type(
                        expr,
                        Type::new(
                            get_token(callee),
                            TypeKind::User("".to_string(), "error".to_string()),
                        ),
                    );
                }
            }
        }
    }

    fn visit_member_access(&mut self, expr: &Expr) {
        if let Expr::MemberAccess { object, name } = expr {
            object.accept(self);
            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let obj_ty = Self::without_borrows(&obj_ty);

            // A member of `Box<int>` is a member of `Box` with `T` bound to `int`, so
            // reduce to the class and remember the substitution to apply.
            let (obj_ty, class_subs) = match &obj_ty.kind {
                TypeKind::GenericInstance(module_name, class_name, arguments) => {
                    let subs = self.class_substitution(module_name, class_name, arguments);
                    (
                        Type::new(
                            obj_ty.name.clone(),
                            TypeKind::User(module_name.clone(), class_name.clone()),
                        ),
                        subs,
                    )
                }
                _ => (obj_ty, HashMap::new()),
            };

            match &obj_ty.kind {
                TypeKind::Module(module_name) => {
                    // `io.println` and friends: resolve against the module's exports.
                    let exports = self
                        .module_exports
                        .get(module_name)
                        .cloned()
                        .unwrap_or_default();

                    if let Some((module_name, class_name, _)) = self.module_class(expr) {
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User(module_name.clone(), class_name.clone()),
                            ),
                        );
                        return;
                    }

                    match exports.functions.get(&name.lexeme) {
                        Some(function) => {
                            let member_ty = self.function_type(
                                name,
                                function.params.clone(),
                                function.return_type.clone(),
                            );
                            self.set_expr_type(expr, member_ty);
                        }
                        None => {
                            self.error_with_notes(
                                name.clone(),
                                &format!(
                                    "No public function `{}` in module `{}`",
                                    name.lexeme, module_name
                                ),
                                vec![Note::new(
                                    format!(
                                        "`{}` provides: {}",
                                        module_name,
                                        exports.function_names().join(", ")
                                    ),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone(),
                                )],
                                vec![Help::new(
                                    "Functions must be declared `public` to be importable"
                                        .to_string(),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone(),
                                )],
                            );
                            self.set_expr_type(expr, self.error_type(name));
                        }
                    }
                    return;
                }
                TypeKind::String => {
                    let member_ty = match name.lexeme.as_str() {
                        "len" => Some(self.function_type(name, vec![], self.int_type(name))),
                        "charAt" => Some(self.function_type(
                            name,
                            vec![self.int_type(name)],
                            self.string_type(name),
                        )),
                        "charCodeAt" => Some(self.function_type(
                            name,
                            vec![self.int_type(name)],
                            self.int_type(name),
                        )),
                        _ => None,
                    };

                    if let Some(member_ty) = member_ty {
                        self.set_expr_type(expr, member_ty);
                    } else {
                        self.error_token(
                            name,
                            &format!("No member `{}` in type `string`", name.lexeme),
                        );
                        self.set_expr_type(expr, self.error_type(name));
                    }
                    return;
                }
                TypeKind::Array(elem_ty, _) => {
                    let member_ty = match name.lexeme.as_str() {
                        "len" => Some(self.function_type(name, vec![], self.int_type(name))),
                        "push" => Some(self.function_type(
                            name,
                            vec![*elem_ty.clone()],
                            self.void_type(name),
                        )),
                        "pop" => Some(self.function_type(name, vec![], self.void_type(name))),
                        _ => None,
                    };

                    if let Some(member_ty) = member_ty {
                        self.set_expr_type(expr, member_ty);
                    } else {
                        self.error_token(
                            name,
                            &format!(
                                "No member `{}` in array type `{}`",
                                name.lexeme, obj_ty.kind
                            ),
                        );
                        self.set_expr_type(expr, self.error_type(name));
                    }
                    return;
                }
                TypeKind::User(module_name, class_name) => {
                    if !module_name.is_empty() && module_name != &self.module_name {
                        let class = self
                            .module_exports
                            .get(module_name)
                            .and_then(|exports| exports.classes.get(class_name))
                            .cloned();

                        if let Some(class) = class {
                            if let Some((field_ty, _, is_static)) = class.fields.get(&name.lexeme) {
                                if *is_static && !self.in_static_context {
                                    self.error_token(
                                    name,
                                    &format!(
                                        "Static field `{}` must be accessed using static access syntax",
                                        name.lexeme
                                    ),
                                );
                                }
                                self.set_expr_type(expr, field_ty.apply_substitution(&class_subs));
                            } else if let Some(method) = class.methods.get(&name.lexeme) {
                                if method.is_static && !self.in_static_context {
                                    self.error_token(
                                    name,
                                    &format!(
                                        "Static method `{}` must be accessed using static access syntax",
                                        name.lexeme
                                    ),
                                );
                                }
                                self.set_expr_type(
                                    expr,
                                    Type::new(
                                        name.clone(),
                                        TypeKind::Function(
                                            method
                                                .params
                                                .iter()
                                                .map(|p| p.apply_substitution(&class_subs))
                                                .collect(),
                                            Box::new(
                                                method.return_type.apply_substitution(&class_subs),
                                            ),
                                        ),
                                    ),
                                );
                            } else {
                                self.error_token(
                                    name,
                                    &format!(
                                        "No public member `{}` in class `{}`",
                                        name.lexeme, class_name
                                    ),
                                );
                                self.set_expr_type(expr, self.error_type(name));
                            }
                        } else {
                            self.error_token(
                                name,
                                &format!("Unknown class `{}.{}`", module_name, class_name),
                            );
                            self.set_expr_type(expr, self.error_type(name));
                        }
                    } else if let Some(Symbol::Class {
                        fields, methods, ..
                    }) = self.symtable.lookup_class(class_name)
                    {
                        // Check if we're in a static context
                        if !self.in_static_context {
                            // Only allow static access through StaticAccess expression
                            if let Some((_, _, is_static)) = fields.get(&name.lexeme) {
                                if *is_static {
                                    self.error_with_notes(
                                    name.clone(),
                                    &format!("Static field `{}` must be accessed using static access syntax", name.lexeme),
                                    vec![Note::new(
                                        format!("Field `{}` is defined here as static", name.lexeme),
                                        name.line,
                                        name.span.clone(),
                                        self.filename.clone()
                                    )],
                                    vec![Help::new(
                                        format!("Try using static access syntax: `Class::{}`", name.lexeme),
                                        name.line,
                                        name.span.clone(),
                                        self.filename.clone()
                                    )]
                                );
                                }
                            }
                            if let Some(Symbol::Function {
                                is_static: true, ..
                            }) = methods.get(&name.lexeme)
                            {
                                self.error_with_notes(
                                name.clone(),
                                &format!("Static method `{}` must be accessed using static access syntax", name.lexeme),
                                vec![Note::new(
                                    format!("Method `{}` is defined here as static", name.lexeme),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone()
                                )],
                                vec![Help::new(
                                    format!("Try using static access syntax: `Class::{}`", name.lexeme),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone()
                                )]
                            );
                            }
                        }
                        // Check fields
                        if let Some((field_ty, visibility, ..)) = fields.get(&name.lexeme) {
                            // Check visibility
                            let is_visible = self.is_member_visible(visibility, &class_name);

                            if !is_visible {
                                self.error_token(
                                    name,
                                    &format!(
                                        "Cannot access {} field `{}` of class `{}`",
                                        visibility.to_string().to_lowercase(),
                                        name.lexeme,
                                        class_name
                                    ),
                                );
                            }

                            self.set_expr_type(expr, field_ty.apply_substitution(&class_subs));
                        } else if let Some(method) = methods.get(&name.lexeme) {
                            if let Symbol::Function {
                                params,
                                return_type,
                                visibility,
                                ..
                            } = method
                            {
                                // Check method visibility
                                let is_visible = visibility
                                    .as_ref()
                                    .map_or(true, |v| self.is_member_visible(v, &class_name));

                                if !is_visible {
                                    self.error_token(
                                        name,
                                        &format!(
                                            "Cannot access {} method `{}` of class `{}`",
                                            visibility.as_ref().map_or("".to_string(), |v| v
                                                .to_string()
                                                .to_lowercase()),
                                            name.lexeme,
                                            class_name
                                        ),
                                    );
                                }

                                let fn_ty = Type::new(
                                    name.clone(),
                                    TypeKind::Function(
                                        params
                                            .iter()
                                            .map(|p| p.apply_substitution(&class_subs))
                                            .collect(),
                                        Box::new(return_type.apply_substitution(&class_subs)),
                                    ),
                                );
                                self.set_expr_type(expr, fn_ty);
                            }
                        } else {
                            self.error_token(
                                name,
                                &format!("No member `{}` in class `{}`", name.lexeme, class_name),
                            );
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    name.clone(),
                                    TypeKind::User("".to_string(), "error".to_string()),
                                ),
                            );
                        }
                    }
                }
                _ => {
                    self.error_token(
                        name,
                        &format!(
                            "Cannot access member `{}` on type `{}`",
                            name.lexeme, obj_ty.kind
                        ),
                    );
                    self.set_expr_type(expr, self.error_type(name));
                }
            }
        }
    }

    fn visit_index(&mut self, expr: &Expr) {
        if let Expr::Index {
            object,
            index,
            token,
        } = expr
        {
            object.accept(self);
            index.accept(self);

            let name = get_token(object);

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let idx_ty = self.get_expr_type(index).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            let obj_ty = Self::without_borrows(&obj_ty);

            if let TypeKind::Array(elem_ty, _) = obj_ty.kind {
                if idx_ty.kind != TypeKind::Int {
                    self.error_with_notes(
                        token.clone(),
                        "Array index must be an integer",
                        vec![Note::new(
                            format!("Found index of type `{}`", idx_ty.kind),
                            token.line,
                            token.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![Help::new(
                            "Try using an integer expression for the index".to_string(),
                            token.line,
                            token.span.clone(),
                            self.filename.clone(),
                        )],
                    );
                }
                self.set_expr_type(expr, *elem_ty.clone());
            } else {
                self.error_with_notes(
                    token.clone(),
                    &format!("Cannot index into non-array type `{}`", obj_ty.kind),
                    vec![Note::new(
                        "Double check the type definitions and try again".to_string(),
                        token.line,
                        token.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![],
                );
            }
        }
    }

    fn visit_cast(&mut self, expr: &Expr) {
        if let Expr::Cast { object, type_ } = expr {
            object.accept(self);
            let target_ty = self.qualify_type(type_);

            let name = get_token(object);

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });
            // Simplistic cast logic
            if !obj_ty.is_compatible_with(&target_ty) {
                // Maybe it's int->float or vice versa, or same user type, etc.
                // We'll do a minimal check
                match (&obj_ty.kind, &target_ty.kind) {
                    (TypeKind::Int, TypeKind::Float) | (TypeKind::Float, TypeKind::Int) => {
                        // allowed
                    }
                    (TypeKind::User(m1, u1), TypeKind::User(m2, u2)) if u1 == u2 && m1 == m2 => {
                        // same user type
                    }
                    _ => {
                        self.error_token(
                            &name,
                            &format!(
                                "Invalid cast from `{}` to `{}`",
                                obj_ty.kind, target_ty.kind
                            ),
                        );
                    }
                }
            }
            self.set_expr_type(expr, target_ty);
        }
    }

    fn visit_class_init(&mut self, expr: &Expr) {
        if let Expr::ClassInit {
            name,
            generics,
            arguments,
        } = expr
        {
            for arg in arguments {
                arg.accept(self);
            }

            let arg_tys: Vec<Type> = arguments
                .iter()
                .map(|a| {
                    self.get_expr_type(a)
                        .cloned()
                        .unwrap_or_else(|| self.error_type(&get_token(a)))
                })
                .collect();

            let (owner, class_name) = match name.lexeme.rsplit_once('.') {
                Some((module, class)) => {
                    (self.resolve_module_name(module), class.to_string())
                }
                None => (self.module_name.clone(), name.lexeme.clone()),
            };

            let (class_generics, constructor_params, fully_defined) = if owner == self.module_name {
                match self.symtable.lookup_class(&class_name) {
                    Some(Symbol::Class {
                        generics,
                        constructor_params,
                        fully_defined,
                        ..
                    }) => (generics.clone(), constructor_params.clone(), *fully_defined),
                    _ => {
                        self.error_token(name, &format!("Unknown class `{}`", name.lexeme));
                        self.set_expr_type(expr, self.error_type(name));
                        return;
                    }
                }
            } else {
                match self
                    .module_exports
                    .get(&owner)
                    .and_then(|exports| exports.classes.get(&class_name))
                {
                    Some(class) => (
                        class.generics.clone(),
                        class.constructor_params.clone(),
                        true,
                    ),
                    None => {
                        self.error_token(name, &format!("Unknown public class `{}`", name.lexeme));
                        self.set_expr_type(expr, self.error_type(name));
                        return;
                    }
                }
            };

            if !fully_defined {
                self.error_token(
                    name,
                    &format!("Class `{}` is not fully defined yet", name.lexeme),
                );
            }

            if arg_tys.len() != constructor_params.len() {
                self.error_token(
                    name,
                    &format!(
                        "Constructor for `{}` expects {} argument(s), but {} provided",
                        name.lexeme,
                        constructor_params.len(),
                        arg_tys.len()
                    ),
                );
                self.set_expr_type(expr, self.error_type(name));
                return;
            }

            // Type arguments are either written out or deduced from the constructor
            // arguments, exactly as for a generic function call.
            let explicit = self.generalise_explicit(generics);
            let subs = self.resolve_generics(
                name,
                &name.lexeme,
                &class_generics,
                &constructor_params,
                &explicit,
                &arg_tys,
            );

            for (i, (expected, actual)) in constructor_params.iter().zip(arg_tys.iter()).enumerate()
            {
                let expected = expected.apply_substitution(&subs);

                if !actual.is_compatible_with(&expected) {
                    let token = arguments
                        .get(i)
                        .map(|argument| get_token(argument))
                        .unwrap_or_else(|| name.clone());

                    self.error_token(
                        &token,
                        &format!(
                            "Constructor argument mismatch for `{}`: expected `{}`, got `{}`",
                            name.lexeme, expected.kind, actual.kind
                        ),
                    );
                }
            }

            let class_ty = if class_generics.is_empty() {
                Type::new(
                    name.clone(),
                    TypeKind::User(owner.clone(), class_name.clone()),
                )
            } else {
                let arguments: Vec<Type> = class_generics
                    .iter()
                    .map(|parameter| {
                        Type::new(
                            name.clone(),
                            subs.get(parameter)
                                .cloned()
                                .unwrap_or(TypeKind::User(String::new(), "error".to_string())),
                        )
                    })
                    .collect();

                Type::new(
                    name.clone(),
                    TypeKind::GenericInstance(owner.clone(), class_name.clone(), arguments),
                )
            };

            self.record_class_instantiation(&owner, &class_name, &class_generics, &subs);
            self.set_expr_type(expr, class_ty);
        }
    }

    fn visit_reference(&mut self, expr: &Expr) {
        if let Expr::Reference { object } = expr {
            object.accept(self);

            let name = get_token(object);

            let mut obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            obj_ty.derived.push(Derived::Ref);

            let ref_ty = Type::new(obj_ty.name.clone(), TypeKind::Reference(Box::new(obj_ty)));

            self.set_expr_type(expr, ref_ty);
        }
    }

    fn visit_mut_reference(&mut self, expr: &Expr) {
        if let Expr::MutReference { object } = expr {
            object.accept(self);

            let name = get_token(object);
            let mut obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("".to_string(), "error".to_string()),
                )
            });

            obj_ty.derived.push(Derived::MutRef);

            let ref_ty = Type::new(obj_ty.name.clone(), TypeKind::MutRef(Box::new(obj_ty)));
            self.set_expr_type(expr, ref_ty);
        }
    }

    fn visit_closure(&mut self, expr: &Expr) {
        if let Expr::Closure {
            name,
            parameters,
            body,
            return_type,
            param_types,
        } = expr
        {
            // Enter a new scope for the closure body
            self.symtable.begin_scope();

            // Save the old function return type context, then set the closure's
            // declared return type as the "current function return type"
            // (so `return` statements inside the closure are checked).
            let old_ret = self.current_function_return_type.take();
            let old_has_valid_return = self.function_has_valid_return;
            self.current_function_return_type = Some(return_type.clone());
            self.function_has_valid_return = false;

            // Check if we are currently in an assignment.
            // If we are, then we can lookup the variable we are assigning to
            // and check if the type of the closure matches the expected type
            // If we are not in an assignment, then we should expect type annotations on each parameter.
            let expected_type: Option<Type> = self.expected_type();

            // Work out the type of every closure parameter.
            //
            // A parameter is either annotated (`|x: int|`) or inferred from the type the
            // closure is being assigned to (`let f: fn(int) -> void = |x| void -> {..}`).
            // Codegen relies on these types to emit the C++ lambda signature, so a
            // parameter we cannot resolve has to be a reported error, not a panic.
            let expected_params: Option<Vec<Type>> = match expected_type.as_ref().map(|t| &t.kind) {
                Some(TypeKind::Function(params, _)) => Some(params.clone()),
                Some(other) => {
                    self.error_token(
                        name,
                        &format!(
                            "Cannot assign a closure to a value of non-function type `{}`",
                            other
                        ),
                    );
                    None
                }
                None => None,
            };

            if let Some(params) = expected_params.as_ref() {
                if params.len() != parameters.len() {
                    self.error_token(
                        name,
                        &format!(
                            "Closure takes {} parameter(s) but `{}` expects {}",
                            parameters.len(),
                            expected_type
                                .as_ref()
                                .map(|t| t.kind.to_string())
                                .unwrap_or_default(),
                            params.len()
                        ),
                    );
                }
            }

            // Declare each closure parameter in the symbol table.
            let mut tys: Vec<Type> = Vec::new();
            for (i, param_token) in parameters.iter().enumerate() {
                let resolved = expected_params
                    .as_ref()
                    .and_then(|params| params.get(i).cloned())
                    .or_else(|| param_types.get(i).cloned());

                let param_ty = match resolved {
                    Some(ty) => ty,
                    None => {
                        self.error_with_notes(
                            param_token.clone(),
                            &format!(
                                "Cannot infer the type of closure parameter `{}`",
                                param_token.lexeme
                            ),
                            vec![Note::new(
                                "Closure parameter types are only inferred when the closure is assigned to a variable with a declared function type".to_string(),
                                param_token.line,
                                param_token.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                format!("Annotate the parameter, e.g. `|{}: int|`", param_token.lexeme),
                                param_token.line,
                                param_token.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                        Type::new(
                            param_token.clone(),
                            TypeKind::User(self.module_name.clone(), "error".to_string()),
                        )
                    }
                };

                self.symtable.declare_symbol(
                    &param_token.lexeme,
                    Symbol::new_variable(param_token.clone(), param_ty.clone()),
                );
                tys.push(param_ty);
            }

            // Visit the closure body statement (which can contain returns)
            body.accept(self);
            let closure_has_return = self.statement_guarantees_return(body);

            if return_type.kind != TypeKind::Void && !closure_has_return {
                self.error_with_notes(
                    name.clone(),
                    &format!(
                        "Closure (return type `{}`) does not return a value on all paths",
                        return_type.kind
                    ),
                    vec![Note::new(
                        "Closure return type is declared here".to_string(),
                        return_type.name.line,
                        return_type.name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        "Add a return statement to the closure body".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                );
            }

            // End the closure scope
            self.symtable.end_scope();

            // Restore the old function return type
            self.current_function_return_type = old_ret;
            self.function_has_valid_return = old_has_valid_return;

            // Finally, set the closure's type. We treat the closure
            // as a function with `param_types -> return_type`.
            // `tys` already holds the resolved parameter types, whether they came from
            // annotations or were inferred, so it is the single source of truth here.
            let closure_type = Type::new(
                name.clone(),
                TypeKind::Function(tys, Box::new(return_type.clone())),
            );
            self.set_expr_type(expr, closure_type);
        }
    }

    fn visit_array(&mut self, expr: &Expr) {
        if let Expr::Array { elements, .. } = expr {
            let name = get_token(expr);

            // If we know what the array is expected to be, its element type tells the
            // elements what they should be. That is what lets `[[], []]` work when the
            // declared type is `int[][]`.
            let expected = self.expected_type();
            let expected_element = match expected.as_ref().map(|t| &t.kind) {
                Some(TypeKind::Array(element, _)) => Some((**element).clone()),
                _ => None,
            };

            let mut elem_tys = Vec::new();
            for e in elements {
                self.with_expected_type(expected_element.clone(), |checker| {
                    e.accept(checker);
                });

                let ty = self.get_expr_type(e).cloned().unwrap_or_else(|| {
                    Type::new(
                        name.clone(),
                        TypeKind::User(self.module_name.clone(), "error".to_string()),
                    )
                });
                elem_tys.push(ty);
            }

            if elem_tys.is_empty() {
                // An empty literal carries no element type, so it can only be checked
                // against an expected one.
                match expected {
                    Some(expected) if matches!(expected.kind, TypeKind::Array(..)) => {
                        self.set_expr_type(expr, expected);
                    }
                    _ => {
                        self.error_with_notes(
                            name.clone(),
                            "Cannot infer the type of an empty array literal",
                            vec![Note::new(
                                "There are no elements to infer an element type from".to_string(),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![Help::new(
                                "Annotate the target, e.g. `let xs: int[] = [];`".to_string(),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                        );
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name,
                                TypeKind::User(self.module_name.clone(), "error".to_string()),
                            ),
                        );
                    }
                }
            } else {
                let first_ty = &elem_tys[0];

                for other in &elem_tys[1..] {
                    if !other.is_compatible_with(first_ty) {
                        self.error_token(
                            &name,
                            &format!(
                                "Inconsistent array element types: `{}` vs `{}`",
                                first_ty.kind, other.kind
                            ),
                        );
                    }
                }

                let arr_ty = Type::new(
                    first_ty.name.clone(),
                    TypeKind::Array(Box::new(first_ty.clone()), 1),
                );

                self.set_expr_type(expr, arr_ty);
            }
        }
    }

    fn visit_tuple(&mut self, expr: &Expr) {
        if let Expr::Tuple { elements } = expr {
            let mut tuple_elems = Vec::new();

            let name = get_token(expr);
            for e in elements {
                e.accept(self);
                tuple_elems.push(self.get_expr_type(e).cloned().unwrap_or_else(|| {
                    Type::new(
                        name.clone(),
                        TypeKind::User(self.module_name.clone(), "error".to_string()),
                    )
                }));
            }
            if tuple_elems.is_empty() {
                self.error_token(&name, "Cannot infer the type of an empty tuple literal");
                self.set_expr_type(expr, self.error_type(&name));
                return;
            }
            let tuple_ty = Type::new(tuple_elems[0].name.clone(), TypeKind::Tuple(tuple_elems));
            self.set_expr_type(expr, tuple_ty);
        }
    }

    fn visit_expression(&mut self, stmt: &Stmt) {
        if let Stmt::Expression { expression } = stmt {
            expression.accept(self);
        }
    }

    fn visit_block(&mut self, stmt: &Stmt) {
        if let Stmt::Block { statements } = stmt {
            self.symtable.begin_scope();
            for s in statements {
                s.accept(self);
            }
            self.symtable.end_scope();
        }
    }

    fn visit_if(&mut self, stmt: &Stmt) {
        if let Stmt::If {
            condition,
            then_branch,
            else_branch,
        } = stmt
        {
            condition.accept(self);
            then_branch.accept(self);
            if let Some(e) = else_branch {
                e.accept(self);
            }
        }
    }

    fn visit_while(&mut self, stmt: &Stmt) {
        if let Stmt::While { condition, body } = stmt {
            condition.accept(self);
            body.accept(self);
        }
    }

    fn visit_for(&mut self, stmt: &Stmt) {
        if let Stmt::For {
            initialiser,
            condition,
            increment,
            body,
        } = stmt
        {
            self.symtable.begin_scope();
            if let Some(init) = initialiser {
                init.accept(self);
            }
            if let Some(cond) = condition {
                cond.accept(self);
            }
            if let Some(inc) = increment {
                inc.accept(self);
            }
            body.accept(self);
            self.symtable.end_scope();
        }
    }

    fn visit_return(&mut self, stmt: &Stmt) {
        if let Stmt::Return { value, .. } = stmt {
            let ret_ty = if let Some(val) = value {
                val.accept(self);
                self.get_expr_type(val).cloned().unwrap_or_else(|| {
                    Type::new(
                        get_token(val),
                        TypeKind::User(self.module_name.clone(), "error".to_string()),
                    )
                })
            } else {
                Type::new(Token::dummy("void"), TypeKind::Void)
            };

            if let Some(expected) = &self.current_function_return_type {
                if !ret_ty.is_compatible_with(expected) {
                    self.error_with_notes(
                        get_token_s(stmt),
                        &format!(
                            "Return type mismatch: expected `{}`, got `{}`",
                            expected.kind, ret_ty.kind
                        ),
                        vec![Note::new(
                            format!("Function declares return type `{}`", expected.kind),
                            expected.name.line,
                            expected.name.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![Help::new(
                            format!("Try returning a value of type `{}`", expected.kind),
                            get_token_s(stmt).line,
                            get_token_s(stmt).span.clone(),
                            self.filename.clone(),
                        )],
                    );
                } else {
                    self.function_has_valid_return = true;
                }
            } else {
                self.error_token(
                    &get_token_s(stmt),
                    "Return statement outside of a function or closure",
                );
            }
        }
    }

    fn visit_break(&mut self, _stmt: &Stmt) {
        // Could check if inside loop
    }

    fn visit_continue(&mut self, _stmt: &Stmt) {
        // Could check if inside loop
    }

    fn visit_function(&mut self, stmt: &Stmt) {
        if let Stmt::Function {
            name,
            params,
            body,
            return_type,
            generics,
            modifiers,
            ..
        } = stmt
        {
            // Begin scope, set up parameters, etc. (same as before)
            self.symtable.begin_scope();

            let old_ret = self.current_function_return_type.take();
            let old_has_valid_return = self.function_has_valid_return;

            // We'll track if we ever see a matching return
            self.function_has_valid_return = false;

            // Insert generics. A method also sees the type parameters of the class it
            // belongs to, which are already in `current_function_generics`.
            let mut generic_names: Vec<String> = self.current_function_generics.clone();
            for g in generics {
                if !generic_names.contains(&g.lexeme) {
                    generic_names.push(g.lexeme.clone());
                }
            }

            // The body is checked with type parameters marked as such, so the return
            // type has to be generalised the same way for `return` to match.
            self.current_function_return_type =
                Some(self.qualify_type(&Self::generalise(return_type, &generic_names)));
            for g in generics {
                self.symtable
                    .declare_symbol(&g.lexeme, Symbol::new_generic_param(g.clone()));
            }

            let old_generics =
                std::mem::replace(&mut self.current_function_generics, generic_names.clone());
            let old_function =
                std::mem::replace(&mut self.current_function, Some(name.lexeme.clone()));

            // A free generic function owns dependencies found in its body. An ordinary
            // method of a generic class keeps the class as its owner, because all such
            // methods are emitted as part of each class specialisation.
            let owner = if !generics.is_empty() {
                match &self.current_class {
                    Some(class_name) => Some(Self::method_key(class_name, &name.lexeme)),
                    None => Some(Self::function_key(&name.lexeme)),
                }
            } else {
                self.current_generic_owner.clone()
            };
            let old_generic_owner =
                std::mem::replace(&mut self.current_generic_owner, owner.clone());

            if let Some(owner) = owner {
                if !generics.is_empty() {
                    self.function_generics
                        .entry(self.module_name.clone())
                        .or_default()
                        .insert(owner, generic_names.clone());
                }
            }

            // Insert parameters. Their declared types are generalised so that a type
            // parameter is recognised as such inside the body, rather than looking like
            // an ordinary user type named `T`.
            for p in params {
                if let Stmt::Variable {
                    name: param_name,
                    type_,
                    ..
                } = &**p
                {
                    let param_type = self.qualify_type(&Self::generalise(type_, &generic_names));
                    self.symtable.declare_symbol(
                        &param_name.lexeme,
                        Symbol::new_variable(param_name.clone(), param_type),
                    );
                }
            }

            // Now visit each statement in the body,
            for b in body {
                b.accept(self);
            }

            // End scope
            self.current_function_generics = old_generics;
            self.current_function = old_function;
            self.current_generic_owner = old_generic_owner;
            self.symtable.end_scope();

            // restore old function return
            self.current_function_return_type = old_ret;
            let has_guaranteed_return = self.statements_guarantee_return(body);
            self.function_has_valid_return = old_has_valid_return;

            if modifiers.contains(&Modifier::Extern) {
                // If the function is extern, we don't need a return statement.
                return;
            }

            // If the function returns void, we don't need a return statement.
            if return_type.kind == TypeKind::Void {
                // add the function to the current scope
                self.symtable.declare_symbol(
                    &name.lexeme,
                    Symbol::new_function(
                        name.clone(),
                        params
                            .iter()
                            .map(|p| {
                                if let Stmt::Variable { type_, .. } = &**p {
                                    self.qualify_type(type_)
                                } else {
                                    Type::new(
                                        name.clone(),
                                        TypeKind::User(
                                            self.module_name.clone(),
                                            "error".to_string(),
                                        ),
                                    )
                                }
                            })
                            .collect(),
                        self.qualify_type(return_type),
                        self.current_class.is_some(),
                    ),
                );
                return;
            }

            // If we haven't seen a valid return statement, produce an error
            if !has_guaranteed_return {
                self.error_with_notes(
                    name.clone(),
                    &format!(
                        "Function `{}` (return type `{}`) does not return a value on all paths",
                        name.lexeme, return_type.kind
                    ),
                    vec![Note::new(
                        format!("Function declares non-void return type here"),
                        return_type.name.line,
                        return_type.name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        "Add a return statement at the end of the function".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                );
            }
        }
    }

    fn visit_variable(&mut self, stmt: &Stmt) {
        if let Stmt::Variable {
            name,
            initialiser,
            type_,
            ..
        } = stmt
        {
            // Inside a generic function or class, a declared type may name one of the
            // type parameters in scope, so mark those before anything compares against
            // it.
            let declared =
                self.qualify_type(&Self::generalise(type_, &self.current_function_generics));
            let type_ = &declared;

            if let Some(init) = initialiser {
                // Temporarily define the variable
                self.symtable.begin_scope();
                self.symtable.declare_symbol(
                    &name.lexeme,
                    Symbol::new_variable(name.clone(), type_.clone()),
                );

                // check if the type exists
                if !self.type_exists(type_) {
                    self.error_with_notes(
                        type_.name.clone(),
                        &format!("Unknown type `{}`", type_.name.lexeme),
                        vec![Note::new(
                            "Double check the type definition".to_string(),
                            type_.name.line,
                            type_.name.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![],
                    );
                }

                // Give the initialiser the declared type as its expected type (this is how
                // closure parameter types get inferred), then restore the previous context so
                // an unrelated expression later on cannot pick up a stale expectation.
                let old_assignment = self.current_assignment.take();
                self.current_assignment = Some(Expr::Assignment {
                    name: name.clone(),
                    value: init.clone(),
                    op: Token::dummy("="),
                });
                self.with_expected_type(Some(type_.clone()), |checker| {
                    init.accept(checker);
                });
                self.current_assignment = old_assignment;
                self.current_initialiser = None;
                self.symtable.end_scope();
                let init_ty = self.get_expr_type(init).cloned().unwrap_or_else(|| {
                    Type::new(
                        name.clone(),
                        TypeKind::User(self.module_name.clone(), "error".to_string()),
                    )
                });
                if !init_ty.is_compatible_with(type_) {
                    self.error_with_notes(
                        name.clone(),
                        &format!(
                            "Cannot initialise variable `{}` of type `{}` with `{}`",
                            name.lexeme, type_.kind, init_ty.kind
                        ),
                        vec![Note::new(
                            format!(
                                "Variable `{}` is declared here with type `{}`",
                                name.lexeme, type_.kind
                            ),
                            name.line,
                            name.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![Help::new(
                            format!("Try initializing with a value of type `{}`", type_.kind),
                            name.line,
                            name.span.clone(),
                            self.filename.clone(),
                        )],
                    );
                }
            }

            // A borrow has to refer to something from the moment it exists, so it
            // cannot be declared and assigned later.
            if initialiser.is_none()
                && matches!(type_.kind, TypeKind::Reference(_) | TypeKind::MutRef(_))
            {
                self.error_with_notes(
                    name.clone(),
                    &format!("Borrow `{}` must be initialised when declared", name.lexeme),
                    vec![Note::new(
                        format!(
                            "`{}` borrows, so it needs something to borrow from",
                            type_.kind
                        ),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![Help::new(
                        format!(
                            "Give it a value, e.g. `let {}: {} = &other;`",
                            name.lexeme, type_.kind
                        ),
                        name.line,
                        name.span.clone(),
                        self.filename.clone(),
                    )],
                );
            }

            // check if the type exists
            if !self.type_exists(type_) {
                self.error_with_notes(
                    type_.name.clone(),
                    &format!("Unknown type `{}`", type_.name.lexeme),
                    vec![Note::new(
                        "Double check the type definition".to_string(),
                        type_.name.line,
                        type_.name.span.clone(),
                        self.filename.clone(),
                    )],
                    vec![],
                );

                self.symtable.declare_symbol(
                    &name.lexeme,
                    Symbol::new_variable(
                        name.clone(),
                        Type::new(
                            name.clone(),
                            TypeKind::User(self.module_name.clone(), "error".to_string()),
                        ),
                    ),
                );
            } else {
                self.symtable.declare_symbol(
                    &name.lexeme,
                    Symbol::new_variable(name.clone(), type_.clone()),
                );
            }
        }
    }

    fn visit_intrinsic(&mut self, expr: &Expr) {
        if let Expr::Intrinsic { name, arguments } = expr {
            // Intrinsics are the compiler's own escape hatch, so their arguments must be
            // literals it can act on at compile time rather than arbitrary expressions.
            let literal_argument = |arg: &Expr| matches!(arg, Expr::Literal { value } if value.kind == TokenKind::String);

            match name.lexeme.as_str() {
                "cpp" | "include" => {
                    if arguments.len() != 1 || !literal_argument(&arguments[0]) {
                        self.error_with_notes(
                            name.clone(),
                            &format!("`@{}` takes exactly one string literal", name.lexeme),
                            vec![Note::new(
                                format!("For example: `@{}(\"...\");`", name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone(),
                            )],
                            vec![],
                        );
                    }
                }
                _ => {
                    self.error_with_notes(
                        name.clone(),
                        &format!("Unknown intrinsic `@{}`", name.lexeme),
                        vec![Note::new(
                            "Available intrinsics: @cpp, @include".to_string(),
                            name.line,
                            name.span.clone(),
                            self.filename.clone(),
                        )],
                        vec![],
                    );
                }
            }

            // An intrinsic is a statement-like escape hatch, so it has no useful type.
            self.set_expr_type(expr, Type::new(name.clone(), TypeKind::Void));
        }
    }

    fn visit_import(&mut self, _stmt: &Stmt) {
        // Not doing anything special with imports here
    }

    fn visit_module(&mut self, module: &Module) {
        // Typically do nothing here if we've already visited statements in `check_module`.
        // Or you could recursively visit each statement.
        for stmt in &module.statements {
            stmt.accept(self);
        }
    }

    fn visit_class(&mut self, stmt: &Stmt) {
        if let Stmt::Class {
            name,
            generics,
            fields,
            methods,
            ..
        } = stmt
        {
            // Set the current_class so we know which class we're in
            let class_name = name.lexeme.clone();
            self.current_class = Some(class_name.clone());
            self.class_stack.push(class_name.clone());

            // The class's type parameters are in scope throughout its body, so `T` is a
            // usable type name in fields and methods.
            let generic_names: Vec<String> = generics.iter().map(|g| g.lexeme.clone()).collect();
            self.symtable.begin_scope();
            for g in generics {
                self.symtable
                    .declare_symbol(&g.lexeme, Symbol::new_generic_param(g.clone()));
            }
            let old_generics =
                std::mem::replace(&mut self.current_function_generics, generic_names.clone());
            let class_owner = if generic_names.is_empty() {
                None
            } else {
                Some(Self::class_key(&class_name))
            };
            let old_generic_owner =
                std::mem::replace(&mut self.current_generic_owner, class_owner.clone());

            if let Some(owner) = class_owner {
                self.function_generics
                    .entry(self.module_name.clone())
                    .or_default()
                    .insert(owner, generic_names.clone());
            }

            for field in fields {
                if let Stmt::Variable { type_, .. } = &**field {
                    let field_type =
                        self.qualify_type(&Self::generalise(type_, &generic_names));
                    self.record_type_instantiations(&field_type);
                }
                field.accept(self);
            }

            // `this` carries the symbolic class arguments while a generic class body is
            // checked, allowing calls to generic methods to retain the class part of
            // their eventual specialisation.
            let class_type = if generic_names.is_empty() {
                Type::new(
                    name.clone(),
                    TypeKind::User(self.module_name.clone(), class_name.clone()),
                )
            } else {
                let arguments = generic_names
                    .iter()
                    .map(|generic| {
                        Type::new(name.clone(), TypeKind::GenericParam(generic.clone()))
                    })
                    .collect();
                Type::new(
                    name.clone(),
                    TypeKind::GenericInstance(
                        self.module_name.clone(),
                        class_name.clone(),
                        arguments,
                    ),
                )
            };

            for method in methods {
                // Start a new scope for the method
                self.symtable.begin_scope();

                // Declare `this` in the symbol table, pointing to `class_type`
                let this_symbol = Symbol::new_variable(name.clone(), class_type.clone());

                self.symtable.declare_symbol("this", this_symbol);
                // Now visit the method AST node itself
                method.accept(self);

                // End the scope
                self.symtable.end_scope();
            }

            self.current_function_generics = old_generics;
            self.current_generic_owner = old_generic_owner;
            self.symtable.end_scope();
            self.class_stack.pop();
            self.current_class = self.class_stack.last().cloned();
        }
    }

    fn visit_extension(&mut self, _stmt: &Stmt) {
        // skip
    }
}

// Add ToString for Visibility
impl ToString for Visibility {
    fn to_string(&self) -> String {
        match self {
            Visibility::Public => "Public".to_string(),
            Visibility::Private => "Private".to_string(),
            Visibility::Protected => "Protected".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Type checks a fixture and everything it imports, returning the error count.
    fn typecheck_fixture(relative_path: &str) -> usize {
        let filename = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), relative_path);

        let program = match crate::modules::load(std::path::Path::new(&filename)) {
            Ok(program) => program,
            // A module that fails to load (unknown import, cycle) is itself an error.
            Err(errors) => return errors.len().max(1),
        };

        let root = program.root().name.clone();
        let mut exports: HashMap<String, ModuleExports> = HashMap::new();
        let mut errors = 0;

        for module in &program.modules {
            let mut symtable = SymbolTable::new();
            let mut checker = TypeChecker::new(
                &mut symtable,
                module.path.to_string_lossy().to_string(),
                module.source.clone(),
            );
            checker.set_module_name(module.name.clone());
            checker.set_module_exports(exports.clone());
            checker.set_is_library(module.name != root);
            checker.check_module(&module.ast);

            // The standard library must always be clean, whatever the fixture does.
            if module.name != root {
                assert_eq!(
                    checker.error_count(),
                    0,
                    "standard library module `{}` should type check cleanly",
                    module.name
                );
            }

            errors += checker.error_count();
            exports.insert(module.name.clone(), checker.exports(&module.ast));
        }

        errors
    }

    /// Collects every `.crdm` fixture in a directory, so new fixtures are picked up
    /// automatically instead of needing to be listed by hand.
    fn fixtures_in(directory: &str) -> Vec<String> {
        let mut fixtures: Vec<String> =
            std::fs::read_dir(format!("{}/{}", env!("CARGO_MANIFEST_DIR"), directory))
                .expect("fixture directory should be readable")
                .filter_map(|entry| {
                    let path = entry.expect("directory entry should be readable").path();
                    if path.extension().and_then(|e| e.to_str()) != Some("crdm") {
                        return None;
                    }
                    Some(format!(
                        "{}/{}",
                        directory,
                        path.file_name().unwrap().to_string_lossy()
                    ))
                })
                .collect();

        fixtures.sort();
        assert!(
            !fixtures.is_empty(),
            "`{}` should contain fixtures",
            directory
        );
        fixtures
    }

    #[test]
    fn pass_fixtures_typecheck_cleanly() {
        for fixture in fixtures_in("tests/pass") {
            assert_eq!(typecheck_fixture(&fixture), 0, "`{}` should pass", fixture);
        }
    }

    #[test]
    fn fail_fixtures_report_type_errors() {
        for fixture in fixtures_in("tests/fail") {
            assert!(
                typecheck_fixture(&fixture) > 0,
                "`{}` should report at least one error",
                fixture
            );
        }
    }
}

/// Instantiations of generic functions, keyed by module then function name.
pub type Instantiations = HashMap<String, HashMap<String, Vec<Vec<TypeKind>>>>;

/// Type parameter names of generic functions, keyed by module then function name.
pub type FunctionGenerics = HashMap<String, HashMap<String, Vec<String>>>;

/// Closes the set of instantiations under calls between generic functions.
///
/// Instantiating `wrapper<int>` requires whatever `wrapper` calls, instantiated the
/// same way. Each round substitutes a caller's type arguments into the calls it makes,
/// which may reveal further instantiations, so this repeats until nothing new appears.
///
/// This runs once over the whole program rather than per module, because a module can
/// be instantiated by something compiled after it.
pub fn expand_instantiations(
    instantiations: &mut Instantiations,
    call_sites: &[GenericCallSite],
    function_generics: &FunctionGenerics,
) {
    loop {
        let mut discovered: Vec<(String, String, Vec<TypeKind>)> = Vec::new();

        for site in call_sites {
            let Some(caller_generics) = function_generics
                .get(&site.caller_module)
                .and_then(|module| module.get(&site.caller))
            else {
                continue;
            };

            let Some(caller_instantiations) = instantiations
                .get(&site.caller_module)
                .and_then(|module| module.get(&site.caller))
            else {
                continue;
            };

            for caller_arguments in caller_instantiations {
                let subs: HashMap<String, TypeKind> = caller_generics
                    .iter()
                    .cloned()
                    .zip(caller_arguments.iter().cloned())
                    .collect();

                let resolved: Vec<TypeKind> = site
                    .arguments
                    .iter()
                    .map(|argument| {
                        Type::new(Token::dummy("<type>"), argument.clone())
                            .apply_substitution(&subs)
                            .kind
                    })
                    .collect();

                // Still symbolic: the caller is itself only used generically so far.
                if resolved
                    .iter()
                    .any(|argument| matches!(argument, TypeKind::GenericParam(_)))
                {
                    continue;
                }

                let already = instantiations
                    .get(&site.callee_module)
                    .and_then(|module| module.get(&site.callee))
                    .map_or(false, |existing| existing.contains(&resolved));

                let entry = (site.callee_module.clone(), site.callee.clone(), resolved);
                if !already && !discovered.contains(&entry) {
                    discovered.push(entry);
                }
            }
        }

        if discovered.is_empty() {
            return;
        }

        for (module, callee, arguments) in discovered {
            instantiations
                .entry(module)
                .or_default()
                .entry(callee)
                .or_default()
                .push(arguments);
        }
    }
}
