use std::collections::HashMap;

use crate::ast::*;
use crate::token::{Token, TokenKind};
use crate::ty::{Type, TypeKind, MonomorphSignature, MonomorphTable};
use crate::utils::symtable::{SymbolTable, Symbol, Visibility};
use crate::errors::{Error, Note, Help};

/// A map from expressions to their inferred types.
type ExprTypeMap<'a> = HashMap<*const Expr, Type>;


/// Main TypeChecker structure.
/// - Contains a mutable reference to the `SymbolTable`.
/// - Maintains a list of `Error` objects.
/// - Stores an `ExprTypeMap` so we know each expression's resulting `Type`.
/// - Tracks the current filename and source code for better error messages.
/// - Also tracks the current class or function context (for `this`, access checks, etc.).
/// - Contains a `MonomorphTable` for storing specialized generic types.
pub struct TypeChecker<'a> {
    pub symtable: &'a mut SymbolTable,
    pub expr_types: ExprTypeMap<'a>,

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
    in_static_context: bool,
    in_call: bool,

    pub monomorph_table: MonomorphTable,
}

impl<'a> TypeChecker<'a> {
    /// Creates a new TypeChecker.
    pub fn new(symtable: &'a mut SymbolTable, filename: String, source: String) -> Self {
        Self {
            symtable,
            expr_types: HashMap::new(),

            filename,
            source,

            current_class: None,
            class_stack: Vec::new(),
            current_function_return_type: None,
            function_has_valid_return: false,
            current_assignment: None,
            current_initialiser: None,
            in_static_context: false,
            in_call: false,

            monomorph_table: MonomorphTable::new(),
        }
    }

    /// Main entry point for type-checking a module.
    pub fn check_module(&mut self, module: &Module) {
        self.collect_declarations(module);
        self.define_class_members(module);
        self.register_extensions(module);

        module.accept(self);
    }

    // Forward declarations
    fn collect_declarations(&mut self, module: &Module) {
        for stmt in &module.statements {
            match &**stmt {
                Stmt::Class { name, .. } => {
                    let class_name = name.lexeme.clone();
                    let sym = Symbol::new_class(name.clone());
                    self.symtable.declare_class(&class_name, sym);
                }
                Stmt::Function {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    let fn_name = name.lexeme.clone();
                    let mut param_types = Vec::new();
                    for p in params {
                        if let Stmt::Variable { type_, .. } = &**p {
                            param_types.push(type_.clone());
                        }
                    }
                    let sym = Symbol::new_function(
                        name.clone(),
                        param_types,
                        return_type.clone(),
                        false,
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
                public_fields,
                private_fields,
                protected_fields,
                static_fields,
                public_methods,
                private_methods,
                protected_methods,
                static_methods,
                ..
            } = &**stmt
            {
                let class_name = name.lexeme.clone();

                // Collect all field declarations first
                let mut field_declarations = Vec::new();
                
                // Process all fields and collect their declarations
                for f in public_fields {
                    if let Stmt::Variable { name: field_name, type_, .. } = &**f {
                        field_declarations.push((
                            field_name.clone(),
                            type_.clone(),
                            Visibility::Public,
                            false
                        ));
                    }
                }
                
                for f in private_fields {
                    if let Stmt::Variable { name: field_name, type_, .. } = &**f {
                        field_declarations.push((
                            field_name.clone(),
                            type_.clone(),
                            Visibility::Private,
                            false
                        ));
                    }
                }
                
                for f in protected_fields {
                    if let Stmt::Variable { name: field_name, type_, .. } = &**f {
                        field_declarations.push((
                            field_name.clone(),
                            type_.clone(),
                            Visibility::Protected,
                            false
                        ));
                    }
                }
                
                for f in static_fields {
                    if let Stmt::Variable { name: field_name, type_, .. } = &**f {
                        field_declarations.push((
                            field_name.clone(),
                            type_.clone(),
                            Visibility::Static,
                            true
                        ));
                    }
                }

                // Now process all method declarations
                let mut method_declarations = Vec::new();
                
                for m in public_methods {
                    if let Stmt::Function { name: method_name, params, return_type, .. } = &**m {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(type_.clone());
                            }
                        }
                        method_declarations.push((
                            method_name.clone(),
                            param_types,
                            return_type.clone(),
                            Visibility::Public,
                            false
                        ));
                    }
                }

                for m in private_methods {
                    if let Stmt::Function { name: method_name, params, return_type, .. } = &**m {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(type_.clone());
                            }
                        }
                        method_declarations.push((
                            method_name.clone(),
                            param_types,
                            return_type.clone(),
                            Visibility::Private,
                            false
                        ));
                    }
                }

                for m in protected_methods {
                    if let Stmt::Function { name: method_name, params, return_type, .. } = &**m {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(type_.clone());
                            }
                        }
                        method_declarations.push((
                            method_name.clone(),
                            param_types,
                            return_type.clone(),
                            Visibility::Protected,
                            false
                        ));
                    }
                }

                for m in static_methods {
                    if let Stmt::Function { name: method_name, params, return_type, .. } = &**m {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(type_.clone());
                            }
                        }
                        method_declarations.push((
                            method_name.clone(),
                            param_types,
                            return_type.clone(),
                            Visibility::Static,
                            true
                        ));
                    }
                }

                // Now update the class symbol with all collected declarations
                let mut symbol_declarations = Vec::new();
                if let Some(class_sym) = self.symtable.lookup_class_mut(&class_name) {
                    if let Symbol::Class { fields, methods, fully_defined, .. } = class_sym {
                        // Add all fields
                        for (field_name, field_type, visibility, is_static) in field_declarations {
                            fields.insert(
                                field_name.lexeme.clone(),
                                (field_type.clone(), visibility.clone(), is_static)
                            );
                            
                            symbol_declarations.push((
                                field_name.lexeme.clone(),
                                Symbol::new_variable_with_visibility(
                                    field_name,
                                    field_type,
                                    Some(visibility),
                                    is_static
                                )
                            ));
                        }

                        // Add all methods
                        for (method_name, param_types, return_type, visibility, is_static) in method_declarations {
                            methods.insert(
                                method_name.lexeme.clone(),
                                Symbol::new_function_with_visibility(
                                    method_name,
                                    param_types,
                                    return_type,
                                    true,
                                    Some(visibility),
                                    is_static
                                )
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

    /// Given a generic type like `Vec<T>` plus a substitution map T->int,
    /// produce a specialized type `Vec<int>`, and record it in the monomorph table.
    fn instantiate_generic_type(&mut self, original: &Type, subs: &HashMap<String, TypeKind>) -> Type {
        // 1) Apply the substitution e.g. `Vec<T>` => `Vec<int>`.
        let specialized = original.apply_substitution(subs);

        // 2) If specialized is a GenericInstance("Vec", [int]), store it in monomorph_table
        if let TypeKind::GenericInstance(ref base, ref args) = specialized.kind {
            let sig = MonomorphSignature {
                name: base.clone(),
                arg_types: args.clone(),
            };
            return self.monomorph_table.get_or_insert(sig, specialized);
        }

        // If it's not actually a generic instance (like T -> int but original was T),
        // just return the substituted type.
        specialized
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

        println!("{}", err.to_string());
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

        println!("{}", err.to_string());
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
            TypeKind::User(name) => {
                // Check if the base type exists in the symbol table
                self.symtable.lookup_type(name).is_some()
            }
            TypeKind::Reference(inner) | TypeKind::Pointer(inner) => {
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
            // Add checks for other derived types as needed
            _ => true, // For primitive types, we assume they exist
        }
    }

    fn register_extensions(&mut self, module: &Module) {
        for stmt in &module.statements {
            if let Stmt::Extension { target, methods } = &**stmt {
                let target_name = target.name.lexeme.clone();
                for method in methods {
                    if let Stmt::Function { name, params, return_type, .. } = &**method {
                        let mut param_types = Vec::new();
                        for p in params {
                            if let Stmt::Variable { type_, .. } = &**p {
                                param_types.push(type_.clone());
                            }
                        }
                        // Create a symbol for the extension method.
                        let sym = Symbol::new_function_with_visibility(
                            name.clone(),
                            param_types,
                            return_type.clone(),
                            true, // mark as method (or extension)
                            Some(Visibility::Public),
                            false
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
        | Expr::PtrAssignment { op, .. }
        | Expr::MemberAssignment { name: op, .. }
        | Expr::MemberAccess { name: op, .. }
        | Expr::Call { paren: op, ..} => op.clone(),
        Expr::Grouping { expression } => get_token(expression),
        Expr::Array { elements } => get_token(&elements[0]),
        Expr::Tuple { elements } => get_token(&elements[0]),
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
        _ => Token::dummy("unknown")
    }
}

impl<'a> Visitor for TypeChecker<'a> {
    fn visit_binary(&mut self, expr: &Expr) {
        if let Expr::Binary { left, op, right } = expr {
            left.accept(self);
            right.accept(self);

            let left_ty = self.get_expr_type(left).cloned().unwrap_or_else(|| {
                self.error_token(
                    &op,
                    "Left operand has unknown type in binary operation",
                );

                let token = get_token(left);

                Type::new(
                    token.clone(),
                    TypeKind::User("error".to_string()),
                )
            });

            let right_ty = self.get_expr_type(right).cloned().unwrap_or_else(|| {
                self.error_token(
                    &op,
                    "Right operand has unknown type in binary operation",
                );

                let token = get_token(right);

                Type::new(
                    token,
                    TypeKind::User("error".to_string()),
                )
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
                    TokenKind::EqEq | TokenKind::Neq | TokenKind::Lt | TokenKind::Gt | TokenKind::Lte | TokenKind::Gte => {
                        Type::new(
                            Token::dummy("int"),
                            TypeKind::Int
                        ) // booleans are just integers.
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
                    TypeKind::User("error".to_string()),
                )
            };

            self.set_expr_type(expr, result_ty);
        }
    }

    fn visit_unary(&mut self, expr: &Expr) {
        if let Expr::Unary { op, right } = expr {
            right.accept(self);
            let right_ty = self.get_expr_type(right).cloned().unwrap_or_else(|| {
                self.error_token(
                    &op,
                    "Unknown type for operand in unary expression",
                );
                Type::new(
                    op.clone(),
                    TypeKind::User("error".to_string()),
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
                            TypeKind::User("error".to_string()),
                        )
                    }
                }
                TokenKind::Mul => {
                    // dereference
                    if let TypeKind::Pointer(inner) = right_ty.kind {
                        *inner.clone()
                    } else {
                        self.error_token(
                            &op,
                            &format!("Cannot dereference non-pointer `{}`", right_ty.kind),
                        );
                        Type::new(
                            op.clone(),
                            TypeKind::User("error".to_string()),
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
                TokenKind::Integer => {
                    Type::new(value.clone(), TypeKind::Int)
                }
                TokenKind::Float => {
                    Type::new(value.clone(), TypeKind::Float)
                }
                TokenKind::String => {
                    Type::new(value.clone(), TypeKind::String)
                }
                // Possibly booleans if your lexer sets them differently,
                // but let's handle them as `Identifier("true"/"false")` or a special token kind.
                _ => {
                    // fallback
                    Type::new(value.clone(), TypeKind::User("bool".to_string()))
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
                                    TypeKind::User(name.lexeme.clone()),
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
                                    TypeKind::User("error".to_string()),
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
                                TypeKind::User(name.lexeme.clone()),
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
                            TypeKind::User(name.lexeme.clone()),
                        ),
                    );
                } else if let Some(_) = self.symtable.lookup_function(&name.lexeme) {
                    // It's a function
                    if self.in_call {
                        // Allow function references in call expressions
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User(name.lexeme.clone()),
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
                                TypeKind::User("error".to_string()),
                            ),
                        );
                    }
                } else {
                    self.error_token(
                        name,
                        &format!("Unknown variable `{}`", name.lexeme),
                    );
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("error".to_string()),
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
                    TypeKind::User("error".to_string()),
                )
            });

            // Check if the type of the right-hand side exists
            if !self.type_exists(&rhs_ty) {
                self.error_token(
                    name,
                    &format!("Type `{}` does not exist", rhs_ty.kind),
                );
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
                    ),
                );
                return;
            }

            // Check if the variable being assigned exists
            if let Some(sym) = self.symtable.lookup_symbol(&name.lexeme) {
                if let Symbol::Variable(_, _, var_ty, ..) = sym {
                    // Check if the type of the variable exists
                    if !self.type_exists(var_ty) {
                        self.error_token(
                            name,
                            &format!("Type `{}` does not exist", var_ty.kind),
                        );
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("error".to_string()),
                            ),
                        );
                        return;
                    }

                    if !rhs_ty.is_compatible_with(var_ty) {
                        self.error_with_notes(
                            name.clone(),
                            &format!(
                                "Cannot assign `{}` to variable of type `{}`",
                                rhs_ty.kind, var_ty.kind
                            ),
                            vec![Note::new(
                                format!("Variable `{}` is declared here with type `{}`", 
                                    name.lexeme, var_ty.kind),
                                var_ty.name.line,
                                var_ty.name.span.clone(),
                                self.filename.clone()
                            )],
                            vec![Help::new(
                                format!("Try using a value of type `{}`", var_ty.kind),
                                name.line,
                                name.span.clone(),
                                self.filename.clone()
                            )]
                        );
                    }
                    // Assignment expression type => var's type
                    self.set_expr_type(expr, var_ty.clone());
                } else {
                    self.error_token(
                        name,
                        &format!("Symbol `{}` is not a variable", name.lexeme),
                    );
                    self.set_expr_type(
                        expr,
                        Type::new(
                            name.clone(),
                            TypeKind::User("error".to_string()),
                        ),
                    );
                }
            } else {
                self.error_token(
                    name,
                    &format!("Unknown variable `{}`", name.lexeme),
                );
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
                    ),
                );
            }
        }
    }

    fn visit_member_assignment(&mut self, expr: &Expr) {
        if let Expr::MemberAssignment {
            object, name, value, ..
        } = expr
        {
            object.accept(self);

            self.current_assignment = Some(expr.clone());
            value.accept(self);
            self.current_assignment = None;

            let obj_ty = self
                .get_expr_type(object)
                .cloned()
                .unwrap_or_else(|| Type::new(name.clone(), TypeKind::User("error".to_string())));
            let rhs_ty = self
                .get_expr_type(value)
                .cloned()
                .unwrap_or_else(|| Type::new(name.clone(), TypeKind::User("error".to_string())));

            if let TypeKind::User(ref class_name) = obj_ty.kind {
                if let Some(Symbol::Class { fields, .. }) = self.symtable.lookup_class(class_name)
                {
                    if let Some(field_ty) = fields.get(&name.lexeme) {
                        if !rhs_ty.is_compatible_with(&field_ty.0) {
                            self.error_with_notes(
                                name.clone(),
                                &format!(
                                    "Type mismatch in member assignment. Expected `{}`, got `{}`",
                                    field_ty.0.kind, rhs_ty.kind
                                ),
                                vec![Note::new(
                                    format!("Field `{}` is declared here with type `{}`", 
                                        name.lexeme, field_ty.0.kind),
                                    field_ty.0.name.line,
                                    field_ty.0.name.span.clone(),
                                    self.filename.clone()
                                )],
                                vec![Help::new(
                                    format!("Try using a value of type `{}`", field_ty.0.kind),
                                    name.line,
                                    name.span.clone(),
                                    self.filename.clone()
                                )]
                            );
                        }
                        self.set_expr_type(expr, field_ty.0.clone());
                    } else {
                        self.error_token(
                            name,
                            &format!(
                                "No field `{}` in class `{}`",
                                name.lexeme, class_name
                            ),
                        );
                        self.set_expr_type(
                            expr,
                            Type::new(
                                name.clone(),
                                TypeKind::User("error".to_string()),
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
                            TypeKind::User("error".to_string()),
                        ),
                    );
                }
            } else {
                self.error_token(
                    name,
                    &format!("Cannot do member assignment on non-class type `{}`", obj_ty.kind),
                );
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
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
                    self.in_static_context = old_static_context;
                    return;
                }
            };

            // Look up the class and verify static member access
            if let Some(Symbol::Class { fields, methods, .. }) = self.symtable.lookup_class(class_name) {
                // Check static fields first
                if let Some((field_ty, _, is_static)) = fields.get(&name.lexeme) {
                    if !is_static {
                        self.error_with_notes(
                            name.clone(),
                            &format!("Cannot access non-static field `{}` in static context", name.lexeme),
                            vec![Note::new(
                                format!("Field `{}` is defined here as non-static", name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone()
                            )],
                            vec![Help::new(
                                "Try accessing the field using an instance of the class".to_string(),
                                name.line,
                                name.span.clone(),
                                self.filename.clone()
                            )]
                        );
                    }
                    self.set_expr_type(expr, field_ty.clone());
                } else if let Some(Symbol::Function { is_static, .. }) = methods.get(&name.lexeme) {
                    if !is_static {
                        self.error_with_notes(
                            name.clone(),
                            &format!("Static method `{}` must be accessed using a static access `{}::{}`", 
                                name.lexeme, class_name, name.lexeme),
                            vec![Note::new(
                                format!("Method `{}` is defined here as static", name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone()
                            )],
                            vec![Help::new(
                                format!("Try accessing the method using a static access `{}::{}`", class_name, name.lexeme),
                                name.line,
                                name.span.clone(),
                                self.filename.clone()
                            )]
                        );
                    }
                    // ... rest of method handling ...
                } else {
                    self.error_token(
                        name,
                        &format!("No static member `{}` found in class `{}`", name.lexeme, class_name),
                    );
                }
            }

            self.in_static_context = old_static_context;
        }
    }

    fn visit_static_assignment(&mut self, expr: &Expr) {
        if let Expr::StaticAssignment { name, value, .. } = expr {
            self.current_assignment = Some(expr.clone());
            value.accept(self);
            self.current_assignment = None;

            let rhs_ty = self.get_expr_type(value).cloned().unwrap_or_else(|| {
                Type::new(name.clone(), TypeKind::User("error".to_string()))
            });

            // Look up the static field in the class
            if let Some(Symbol::Class { fields, .. }) = 
                self.symtable.lookup_class(&name.lexeme)
            {
                if let Some((field_ty, _, _)) = fields.get(&name.lexeme) {
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
                    &format!("No static field `{}` found", name.lexeme),
                );
            } else {
                self.error_token(
                    name,
                    &format!("Cannot find class for static assignment `{}`", name.lexeme),
                );
            }

            // Set error type if we couldn't resolve the static field
            self.set_expr_type(
                expr,
                Type::new(
                    name.clone(),
                    TypeKind::User("error".to_string()),
                ),
            );
        }
    }

    fn visit_index_assignment(&mut self, expr: &Expr) {
        if let Expr::IndexAssignment {
            object, index, value, token, ..
        } = expr
        {
            object.accept(self);
            index.accept(self);
            value.accept(self);

            let obj_ty = self
                .get_expr_type(object)
                .cloned()
                .unwrap_or_else(|| Type::new(token.clone(), TypeKind::User("error".to_string())));
            let idx_ty = self
                .get_expr_type(index)
                .cloned()
                .unwrap_or_else(|| Type::new(token.clone(), TypeKind::User("error".to_string())));
            let rhs_ty = self
                .get_expr_type(value)
                .cloned()
                .unwrap_or_else(|| Type::new(token.clone(), TypeKind::User("error".to_string())));

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
                            self.filename.clone()
                        )],
                        vec![Help::new(
                            "Try using an integer expression for the index".to_string(),
                            token.line,
                            token.span.clone(),
                            self.filename.clone()
                        )]
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
                        self.filename.clone()
                    )],
                    vec![]
                );
            }
        }
    }

    fn visit_ptr_assignment(&mut self, expr: &Expr) {
        if let Expr::PtrAssignment { object, value, op } = expr {
            object.accept(self);
            value.accept(self);

            let token = get_token(object);

            let obj_ty = self
                .get_expr_type(object)
                .cloned()
                .unwrap_or_else(|| Type::new(token.clone(), TypeKind::User("error".to_string())));
            let rhs_ty = self
                .get_expr_type(value)
                .cloned()
                .unwrap_or_else(|| Type::new(token.clone(), TypeKind::User("error".to_string())));

            if let TypeKind::Pointer(inner_ty) = obj_ty.kind {
                if !rhs_ty.is_compatible_with(&inner_ty) {
                    self.error_token(
                        &op,
                        &format!(
                            "Pointer assignment mismatch. Expected `{}`, got `{}`",
                            inner_ty.kind, rhs_ty.kind
                        ),
                    );
                }
                self.set_expr_type(expr, *inner_ty.clone());
            } else {
                self.error_token(
                    &op,
                    "Cannot pointer-assign to non-pointer type",
                );
                self.set_expr_type(
                    expr,
                    Type::new(token.clone(), TypeKind::User("error".to_string())),
                );
            }
        }
    }

    fn visit_call(&mut self, expr: &Expr) {
        if let Expr::Call { callee, arguments, paren } = expr {
            self.in_call = true;
            callee.accept(self);
            self.in_call = false;
            let arg_tys: Vec<Type> = arguments
                .iter()
                .map(|a| {
                    a.accept(self);
                    self.get_expr_type(a).cloned().unwrap_or_else(|| {
                        Type::new(
                            paren.clone(),
                            TypeKind::User("error".to_string()),
                        )
                    })
                })
                .collect();

            let callee_ty = self.get_expr_type(callee).cloned().unwrap_or_else(|| {
                Type::new(
                    paren.clone(),
                    TypeKind::User("error".to_string()),
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
                                self.filename.clone()
                            )],
                        );
                    } else {
                        // Create a substitution map for monomorphization
                        let mut subs: HashMap<String, TypeKind> = HashMap::new();
                        for (expected, actual) in param_tys.iter().zip(arg_tys.iter()) {
                            if !actual.is_compatible_with(expected) {
                                self.error_with_notes(
                                    actual.name.clone(),
                                    &format!(
                                        "Argument type mismatch: expected `{}`, got `{}`",
                                        expected.kind, actual.kind
                                    ),
                                    vec![Note::new(
                                        format!("Function was defined with argument type `{}`", expected.kind),
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
                TypeKind::User(ref name) => {
                    if let Some(sym) = self.symtable.lookup_function(name) {
                        if let Symbol::Function { params, return_type, .. } = sym {
                            if params.len() != arg_tys.len() {
                                self.error_token(
                                    &paren,
                                    &format!(
                                        "Function `{}` expects {} args, got {}",
                                        name, params.len(), arg_tys.len()
                                    ),
                                );
                            } else {
                                for (expected, actual) in params.iter().zip(arg_tys.iter()) {
                                    if !actual.is_compatible_with(expected) {
                                        self.error_token(&expected.name, &format!(
                                            "Argument mismatch in call to `{}`: expected `{}`, got `{}`",
                                            name, expected.kind, actual.kind
                                        ));
                                    }
                                }
                                self.set_expr_type(expr, return_type.clone());
                            }
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
                                    TypeKind::User("error".to_string()),
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
                                TypeKind::User("error".to_string()),
                            ),
                        );
                    }
                }
                TypeKind::Int
                | TypeKind::Float
                | TypeKind::String => {
                }
                _ => {
                    self.error_token(
                        &paren,
                        &format!("Cannot call non-function type `{}`", callee_ty.kind),
                    );
                    let token = get_token(callee);
                    self.set_expr_type(
                        expr,
                        Type::new(
                            token.clone(),
                            TypeKind::User("error".to_string()),
                        ),
                    );
                }
            }
        }
    }

    fn visit_generic_call(&mut self, expr: &Expr) {
        if let Expr::GenericCall { callee, arguments, generics, .. } = expr {
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
                            TypeKind::User("error".to_string()),
                        )
                    })
                })
                .collect();

            let callee_ty = self.get_expr_type(callee).cloned().unwrap_or_else(|| {
                Type::new(
                    get_token(callee),
                    TypeKind::User("error".to_string()),
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
                TypeKind::User(ref name) => {
                    if let Some(sym) = self.symtable.lookup_function(name) {
                        if let Symbol::Function { params, return_type, .. } = sym {
                            if params.len() != arg_tys.len() {
                                self.error_token(
                                    &get_token(callee),
                                    &format!(
                                        "Function `{}` expects {} args, got {}",
                                        name, params.len(), arg_tys.len()
                                    ),
                                );
                            } else {
                                for (i, (_, actual)) in params.iter().zip(arg_tys.iter()).enumerate() {
                                    if !actual.is_compatible_with(&generics[i]) {
                                        self.error_token(&generics[i].name, &format!(
                                            "Argument mismatch in call to `{}`: expected `{}`, got `{}`",
                                            name, generics[i].kind, actual.kind
                                        ));
                                    }
                                }
                                self.set_expr_type(expr, return_type.clone());
                            }
                        } else {
                            self.error_token(
                                &get_token(callee),
                                &format!("`{}` is not a function symbol", name),
                            );
                            self.set_expr_type(
                                expr,
                                Type::new(
                                    get_token(callee),
                                    TypeKind::User("error".to_string()),
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
                                TypeKind::User("error".to_string()),
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
                            TypeKind::User("error".to_string()),
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
                Type::new(name.clone(), TypeKind::User("error".to_string()))
            });

            if let TypeKind::User(ref class_name) = obj_ty.kind {
                if let Some(Symbol::Class { fields, methods, .. }) = self.symtable.lookup_class(class_name) {
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
                        if let Some(Symbol::Function { is_static: true, .. }) = methods.get(&name.lexeme) {
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
                        let is_visible = match visibility {
                            Visibility::Public => true,
                            Visibility::Static => true,
                            Visibility::Private => Some(class_name.as_str()) == self.current_class.as_deref(),
                            Visibility::Protected => {
                                self.current_class.as_deref().map_or(false, |current| {
                                    current == class_name // TODO: Add inheritance check
                                })
                            }
                        };

                        if !is_visible {
                            self.error_token(
                                name,
                                &format!("Cannot access {} field `{}` of class `{}`", 
                                    visibility.to_string().to_lowercase(),
                                    name.lexeme, 
                                    class_name
                                ),
                            );
                        }

                        self.set_expr_type(expr, field_ty.clone());
                    } else if let Some(method) = methods.get(&name.lexeme) {
                        if let Symbol::Function {
                            params,
                            return_type,
                            visibility,
                            ..
                        } = method {
                            // Check method visibility
                            let is_visible = match visibility {
                                Some(Visibility::Public) => true,
                                Some(Visibility::Static) => true,
                                Some(Visibility::Private) => Some(class_name.as_str()) == self.current_class.as_deref(),
                                Some(Visibility::Protected) => {
                                    self.current_class.as_deref().map_or(false, |current| {
                                        current == class_name // TODO: Add inheritance check
                                    })
                                },
                                None => true,
                            };

                            if !is_visible {
                                self.error_token(
                                    name,
                                    &format!("Cannot access {} method `{}` of class `{}`",
                                        visibility.as_ref().map_or("".to_string(), |v| v.to_string().to_lowercase()),
                                        name.lexeme,
                                        class_name
                                    ),
                                );
                            }

                            let fn_ty = Type::new(
                                name.clone(),
                                TypeKind::Function(params.clone(), Box::new(return_type.clone())),
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
                                TypeKind::User("error".to_string()),
                            ),
                        );
                    }
                }
            }
        }
    }

    fn visit_index(&mut self, expr: &Expr) {
        if let Expr::Index { object, index, token } = expr {
            object.accept(self);
            index.accept(self);

            let name = get_token(object);

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(name.clone(), TypeKind::User("error".to_string()))
            });
            let idx_ty = self.get_expr_type(index).cloned().unwrap_or_else(|| {
                Type::new(name.clone(), TypeKind::User("error".to_string()))
            });

            if let TypeKind::Array(elem_ty, _) = obj_ty.kind {
                if idx_ty.kind != TypeKind::Int {
                    self.error_with_notes(
                        token.clone(),
                        "Array index must be an integer",
                        vec![Note::new(
                            format!("Found index of type `{}`", idx_ty.kind),
                            token.line,
                            token.span.clone(),
                            self.filename.clone()
                        )],
                        vec![Help::new(
                            "Try using an integer expression for the index".to_string(),
                            token.line,
                            token.span.clone(),
                            self.filename.clone()
                        )]
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
                        self.filename.clone()
                    )],
                    vec![]
                );
            }
        }
    }

    fn visit_cast(&mut self, expr: &Expr) {
        if let Expr::Cast { object, type_ } = expr {
            object.accept(self);

            let name = get_token(object);

            let obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("error".to_string()),
                )
            });
            // Simplistic cast logic
            if !obj_ty.is_compatible_with(type_) {
                // Maybe it's int->float or vice versa, or same user type, etc.
                // We'll do a minimal check
                match (&obj_ty.kind, &type_.kind) {
                    (TypeKind::Int, TypeKind::Float)
                    | (TypeKind::Float, TypeKind::Int) => {
                        // allowed
                    }
                    (TypeKind::User(u1), TypeKind::User(u2)) if u1 == u2 => {
                        // same user type
                    }
                    _ => {
                        self.error_token(&name, &format!(
                            "Invalid cast from `{}` to `{}`",
                            obj_ty.kind, type_.kind
                        ));
                    }
                }
            }
            self.set_expr_type(expr, type_.clone());
        }
    }

    fn visit_class_init(&mut self, expr: &Expr) {
        if let Expr::ClassInit { name, arguments } = expr {
            // Look up the class
            if let Some(Symbol::Class { fields, fully_defined, .. }) =
                self.symtable.lookup_class(&name.lexeme)
            {
                // If not fully defined => error
                if !fully_defined {
                    self.error_token(
                        name,
                        &format!("Class `{}` is not fully defined yet", name.lexeme),
                    );
                }
    
                // Check argument count
                // If we stored `constructor_param_count`, do:
                //   if arguments.len() != constructor_param_count => error
                //
                // But if you're just using the length of `fields`,
                // we can do:
                let expected_args = fields.iter().filter(|(_, ty)| ty.0.is_constructor).count();
                let actual_args = arguments.len();
                if actual_args != expected_args {
                    self.error_token(
                        name,
                        &format!(
                            "Constructor for `{}` expects {} arguments, but {} provided",
                            name.lexeme, expected_args, actual_args
                        ),
                    );
                }
    
                // Type check each argument
                for arg in arguments {
                    arg.accept(self);
                }
    
                // Finally, set the expression type to the class type
                let class_ty = Type::new(
                    name.clone(),
                    TypeKind::User(name.lexeme.clone()),
                );
                self.set_expr_type(expr, class_ty);
    
            } else {
                self.error_token(
                    name,
                    &format!("Unknown class `{}`", name.lexeme),
                );
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
                    ),
                );
            }
        }
    }    

    fn visit_dereference(&mut self, expr: &Expr) {
        // Similar to unary star
        if let Expr::Dereference { object } = expr {
            object.accept(self);

            let name = get_token(object);
            let mut obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("error".to_string()),
                )
            });

            obj_ty.derived.push(Derived::Ptr);

            if let TypeKind::Pointer(inner) = obj_ty.kind {
                self.set_expr_type(expr, *inner.clone());
            } else {
                self.error_token(&name,&format!(
                    "Cannot dereference non-pointer type `{}`",
                    obj_ty.kind
                ));
                self.set_expr_type(
                    expr,
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
                    ),
                );
            }
        }
    }

    fn visit_reference(&mut self, expr: &Expr) {
        if let Expr::Reference { object } = expr {
            object.accept(self);

            let name = get_token(object);

            let mut obj_ty = self.get_expr_type(object).cloned().unwrap_or_else(|| {
                Type::new(
                    name.clone(),
                    TypeKind::User("error".to_string()),
                )
            });

            obj_ty.derived.push(Derived::Ref);

            let ref_ty = Type::new(
                obj_ty.name.clone(),
                TypeKind::Reference(Box::new(obj_ty)),
            );

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
                    TypeKind::User("error".to_string()),
                )
            });

            obj_ty.derived.push(Derived::MutRef);

            let ref_ty = Type::new(
                obj_ty.name.clone(),
                TypeKind::MutRef(Box::new(obj_ty)),
            );
            self.set_expr_type(expr, ref_ty);
        }
    }

    fn visit_closure(&mut self, expr: &Expr) {
        if let Expr::Closure {
            name,
            parameters,
            body,
            return_type,
            param_types
        } = expr
        {
            // Enter a new scope for the closure body
            self.symtable.begin_scope();
    
            // Save the old function return type context, then set the closure's
            // declared return type as the "current function return type" 
            // (so `return` statements inside the closure are checked).
            let old_ret = self.current_function_return_type.take();
            self.current_function_return_type = Some(return_type.clone());
    
            // Check if we are currently in an assignment.
            // If we are, then we can lookup the variable we are assigning to
            // and check if the type of the closure matches the expected type
            // If we are not in an assignment, then we should expect type annotations on each parameter.
            let expected_type: Option<Type>;
            if let Some(expr) = &self.current_assignment {
                expected_type = match expr {
                    Expr::Assignment { name, .. } => {
                        let sym = self.symtable.lookup_symbol(&name.lexeme);

                        if let Some(x) = sym {
                            match x {
                                Symbol::Variable(_, _, ty, ..) => Some(ty.clone()),
                                _ => unreachable!()
                            }
                        } else {
                            None
                        }
                    }
                    Expr::MemberAssignment { name, ..} => {
                        let sym = self.symtable.lookup_symbol(&name.lexeme);

                        if let Some(x) = sym {
                            match x {
                                Symbol::Variable(_, _, ty, ..) => Some(ty.clone()),
                                _ => unreachable!()
                            }
                        } else {
                            None
                        }
                    }
                    _ => None
                }
            } else {
                expected_type = None;
            }

            let mut tys: Vec<Type> = Vec::new();
            // Declare each closure parameter in the symbol table
            if let Some(ref ty) = expected_type {
                tys = match ty.kind {
                    TypeKind::Function(ref params, _) => params.clone(),
                    _ => unreachable!()
                };

                for (i, param_token) in parameters.iter().enumerate() {
                    // Declare in the symbol table
                    self.symtable.declare_symbol(
                        &param_token.lexeme, 
                        Symbol::new_variable(param_token.clone(), tys[i].clone())
                    );
                }
            } else {
                for (i, param_token) in parameters.iter().enumerate() {
                    self.symtable.declare_symbol(
                        &param_token.lexeme,
                        Symbol::new_variable(param_token.clone(), param_types[i].clone())
                    );

                    // No need to push the type of the parameter anymore
                }
            }

            // Visit the closure body statement (which can contain returns)
            body.accept(self);
    
            // End the closure scope
            self.symtable.end_scope();
    
            // Restore the old function return type
            self.current_function_return_type = old_ret;
    
            // Finally, set the closure's type. We treat the closure 
            // as a function with `param_types -> return_type`.
            let closure_type: Type;
            if expected_type.is_none() {
                closure_type = Type::new(
                    name.clone(),
                    // The function type: (param_types) -> return_type
                    TypeKind::Function(param_types.clone(), Box::new(return_type.clone())),
                );
            } else {
                closure_type = Type::new(
                    name.clone(),
                    TypeKind::Function(tys, Box::new(return_type.clone()))
                )
            }
            self.set_expr_type(expr, closure_type);
        }
    }    

    fn visit_array(&mut self, expr: &Expr) {
        if let Expr::Array { elements } = expr {
            let mut elem_tys = Vec::new();

            let name = get_token(expr);
            for e in elements {
                e.accept(self);

                let ty = self.get_expr_type(e).cloned().unwrap_or_else(|| {
                    Type::new(name.clone(), TypeKind::User("error".to_string()))
                });
                elem_tys.push(ty);
            }

            if elem_tys.is_empty() {
                self.set_expr_type(
                    expr,
                    Type::new(name, TypeKind::User("error".to_string())),
                );
            } else {
                let first_ty = &elem_tys[0];

                for other in &elem_tys[1..] {
                    if !other.is_compatible_with(first_ty) {
                        self.error_token(&name, &format!(
                            "Inconsistent array element types: `{}` vs `{}`",
                            first_ty.kind, other.kind
                        ));
                    }
                }

                let arr_ty = Type::new(
                    first_ty.name.clone(),
                    TypeKind::Array(Box::new(first_ty.clone()), first_ty.get_array_depth() + 1),
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
                tuple_elems.push(
                    self.get_expr_type(e).cloned().unwrap_or_else(|| {
                        Type::new(name.clone(), TypeKind::User("error".to_string()))
                    })
                );
            }
            let tuple_ty = Type::new(
                tuple_elems[0].name.clone(),
                TypeKind::Tuple(tuple_elems),
            );
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
                        TypeKind::User("error".to_string()),
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
                            self.filename.clone()
                        )],
                        vec![Help::new(
                            format!("Try returning a value of type `{}`", expected.kind),
                            get_token_s(stmt).line,
                            get_token_s(stmt).span.clone(),
                            self.filename.clone()
                        )]
                    );
                } else {
                    self.function_has_valid_return = true;
                }
            } else {
                self.error_token(&get_token_s(stmt), "Return statement outside of a function or closure");
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
            modifiers
        } = stmt
        {
            // Begin scope, set up parameters, etc. (same as before)
            self.symtable.begin_scope();
    
            let old_ret = self.current_function_return_type.take();
            self.current_function_return_type = Some(return_type.clone());
    
            // We'll track if we ever see a matching return
            self.function_has_valid_return = false;

            // Insert generics
            for g in generics {
                self.symtable.declare_symbol(
                    &g.lexeme,
                    Symbol::new_generic_param(g.clone())
                );
            }
    
            // Insert parameters
            for p in params {
                if let Stmt::Variable { name: param_name, type_, .. } = &**p {
                    self.symtable.declare_symbol(
                        &param_name.lexeme,
                        Symbol::new_variable(param_name.clone(), type_.clone()),
                    );
                }
            }
    
            // Now visit each statement in the body, 
            for b in body {
                b.accept(self);
            }
    
            // End scope
            self.symtable.end_scope();
    
            // restore old function return
            self.current_function_return_type = old_ret;

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
                        params.iter().map(|p| {
                            if let Stmt::Variable { type_, .. } = &**p {
                                type_.clone()
                            } else {
                                Type::new(
                                    name.clone(),
                                    TypeKind::User("error".to_string()),
                                )
                            }
                        }).collect(),
                        return_type.clone(),
                        self.current_class.is_some(),
                    )
                );
                return;
            }
    
            // If we haven't seen a valid return statement, produce an error
            if !self.function_has_valid_return {
                self.error_with_notes(
                    name.clone(),
                    &format!(
                        "Function `{}` (return type `{}`) does not return a value on all paths",
                        name.lexeme,
                        return_type.kind
                    ),
                    vec![Note::new(
                        format!("Function declares non-void return type here"),
                        return_type.name.line,
                        return_type.name.span.clone(),
                        self.filename.clone()
                    )],
                    vec![Help::new(
                        "Add a return statement at the end of the function".to_string(),
                        name.line,
                        name.span.clone(),
                        self.filename.clone()
                    )]
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
            if let Some(init) = initialiser {
                // Temporarily define the variable
                self.symtable.begin_scope();
                self.symtable.declare_symbol(&name.lexeme, Symbol::new_variable(name.clone(), type_.clone()));

                // check if the type exists
                if !type_.is_primitive() && !self.symtable.user_defined_type_exists(type_) {
                    self.error_with_notes(
                        type_.name.clone(),
                        &format!("Unknown type `{}`", type_.name.lexeme),
                        vec![Note::new(
                            "Double check the type definition".to_string(),
                            type_.name.line,
                            type_.name.span.clone(),
                            self.filename.clone()
                        )],
                        vec![]
                    );
                }

                self.current_assignment = Some(Expr::Assignment { name: name.clone(), value: init.clone(), op: Token::dummy("=") });
                init.accept(self);
                self.current_initialiser = None;
                self.symtable.end_scope();
                let init_ty = self.get_expr_type(init).cloned().unwrap_or_else(|| {
                    Type::new(
                        name.clone(),
                        TypeKind::User("error".to_string()),
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
                            format!("Variable `{}` is declared here with type `{}`", 
                                name.lexeme, type_.kind),
                            name.line,
                            name.span.clone(),
                            self.filename.clone()
                        )],
                        vec![Help::new(
                            format!("Try initializing with a value of type `{}`", type_.kind),
                            name.line,
                            name.span.clone(),
                            self.filename.clone()
                        )]
                    );
                }
            }

            // check if the type exists
            if !type_.is_primitive() && !self.symtable.user_defined_type_exists(type_) {
                self.error_with_notes(
                    type_.name.clone(),
                    &format!("Unknown type `{}`", type_.name.lexeme),
                    vec![Note::new(
                        "Double check the type definition".to_string(),
                        type_.name.line,
                        type_.name.span.clone(),
                        self.filename.clone()
                    )],
                    vec![]
                );

                self.symtable.declare_symbol(&name.lexeme, Symbol::new_variable(name.clone(), Type::new(name.clone(), TypeKind::User("error".to_string()))));
            } else {
                self.symtable.declare_symbol(&name.lexeme, Symbol::new_variable(name.clone(), type_.clone()));
            }
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
            public_fields,
            private_fields,
            protected_fields,
            static_fields,
            public_methods,
            private_methods,
            protected_methods,
            static_methods,
            ..
        } = stmt
        {
            // Set the current_class so we know which class we're in
            let class_name = name.lexeme.clone();
            self.current_class = Some(class_name.clone());
            self.class_stack.push(class_name.clone());
    
            for field in public_fields
                .iter()
                .chain(private_fields)
                .chain(protected_fields)
                .chain(static_fields)
            {
                field.accept(self); 
            }
    
            // Construct a Type that represents this class, e.g. `User(className)`
            let class_type = Type::new(name.clone(), TypeKind::User(class_name.clone()));
    
            for method in public_methods
                .iter()
                .chain(private_methods)
                .chain(protected_methods)
                .chain(static_methods)
            {
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
            Visibility::Static => "Static".to_string(),
        }
    }
}