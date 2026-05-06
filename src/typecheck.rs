use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::*;
use crate::token::{Token, TokenKind};
use crate::ty::{Type, TypeKind};
use crate::utils::symtable::{SymbolTable, Symbol};
use crate::errors::{Error, Note, Help};

/// A map from expressions to their inferred types.
type ExprTypeMap<'a> = HashMap<*const Expr, Type>;


/// Main TypeChecker structure.
/// - Contains a mutable reference to the `SymbolTable`.
/// - Maintains a list of `Error` objects.
/// - Stores an `ExprTypeMap` so we know each expression's resulting `Type`.
/// - Tracks the current filename and source code for better error messages.
pub struct TypeChecker<'a> {
    pub symtable: &'a mut SymbolTable,
    pub expr_types: ExprTypeMap<'a>,
    pub errors: RefCell<Vec<Error>>,

    /// The name of the file we're currently checking, so we can attach it to errors.
    pub filename: String,
    /// The entire source code of the file, used for showing the line with an error.
    pub source: String,

    current_function_return_type: Option<Type>,
    function_has_valid_return: bool,
    current_assignment: Option<Expr>,
    current_initialiser: Option<Expr>,
    in_call: bool,
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

            current_function_return_type: None,
            function_has_valid_return: false,
            current_assignment: None,
            current_initialiser: None,
            in_call: false,
        }
    }

    /// Main entry point for type-checking a module.
    pub fn check_module(&mut self, module: &Module) {
        self.collect_declarations(module);
        module.accept(self);
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

    // Forward declarations
    fn collect_declarations(&mut self, module: &Module) {
        for stmt in &module.statements {
            match &**stmt {
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
            TypeKind::User(name) => {
                // Keep cascaded diagnostics readable once an earlier expression has failed.
                let is_generic_param = matches!(
                    self.symtable.lookup_symbol(name),
                    Some(Symbol::Variable(_, _, ty, ..))
                        if matches!(ty.kind, TypeKind::GenericParam(_))
                );

                (name == "error" || self.symtable.lookup_type(name).is_some() || is_generic_param)
                    && ty.generics.iter().all(|arg| self.type_exists(arg))
            }
            TypeKind::GenericParam(name) => self.symtable.lookup_symbol(name).is_some(),
            TypeKind::GenericInstance(name, args) => {
                self.symtable.lookup_type(name).is_some()
                    && args.iter().all(|arg| self.type_exists(arg))
            }
            TypeKind::Reference(inner) | TypeKind::Pointer(inner) | TypeKind::MutRef(inner) => {
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
        Type::new(token.clone(), TypeKind::User("error".to_string()))
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

    fn unsupported_expr(&mut self, expr: &Expr, token: &Token, message: &str) {
        self.error_token(token, message);
        self.set_expr_type(expr, self.error_type(token));
    }

    fn function_type(&self, token: &Token, params: Vec<Type>, return_type: Type) -> Type {
        Type::new(
            token.clone(),
            TypeKind::Function(params, Box::new(return_type)),
        )
    }

    fn statements_guarantee_return(&self, statements: &[Box<Stmt>]) -> bool {
        statements.iter().any(|stmt| self.statement_guarantees_return(stmt))
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
            _ => false,
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
        Expr::Array { elements } => elements
            .first()
            .map(|expr| get_token(expr))
            .unwrap_or_else(|| Token::dummy("[]")),
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
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Member assignment is not part of Cardamom V1.");
    }

    fn visit_static_access(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Static access is not part of Cardamom V1.");
    }

    fn visit_static_assignment(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Static assignment is not part of Cardamom V1.");
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
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Pointer assignment is not part of Cardamom V1.");
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
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Generic function calls are not part of Cardamom V1.");
    }

    fn visit_member_access(&mut self, expr: &Expr) {
        if let Expr::MemberAccess { object, name } = expr {
            object.accept(self);
            let obj_ty = self
                .get_expr_type(object)
                .cloned()
                .unwrap_or_else(|| self.error_type(name));

            let member_ty = match &obj_ty.kind {
                TypeKind::String => match name.lexeme.as_str() {
                    "len" => Some(self.function_type(name, vec![], self.int_type(name))),
                    "charAt" => Some(self.function_type(name, vec![self.int_type(name)], self.string_type(name))),
                    "charCodeAt" => Some(self.function_type(name, vec![self.int_type(name)], self.int_type(name))),
                    _ => None,
                },
                TypeKind::Array(elem_ty, _) => match name.lexeme.as_str() {
                    "len" => Some(self.function_type(name, vec![], self.int_type(name))),
                    "push" => Some(self.function_type(name, vec![*elem_ty.clone()], self.void_type(name))),
                    "pop" => Some(self.function_type(name, vec![], self.void_type(name))),
                    _ => None,
                },
                _ => None,
            };

            if let Some(member_ty) = member_ty {
                self.set_expr_type(expr, member_ty);
            } else {
                self.error_token(name, &format!("No member `{}` in type `{}`", name.lexeme, obj_ty.kind));
                self.set_expr_type(expr, self.error_type(name));
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
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Constructors are not part of Cardamom V1.");
    }

    fn visit_dereference(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Pointer dereference is not part of Cardamom V1.");
    }

    fn visit_reference(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "References are not part of Cardamom V1.");
    }

    fn visit_mut_reference(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Mutable references are not part of Cardamom V1.");
    }

    fn visit_closure(&mut self, expr: &Expr) {
        let token = get_token(expr);
        self.unsupported_expr(expr, &token, "Closures are not part of Cardamom V1.");
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
                self.error_token(&name, "Cannot infer the type of an empty array literal");
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
                tuple_elems.push(
                    self.get_expr_type(e).cloned().unwrap_or_else(|| {
                        Type::new(name.clone(), TypeKind::User("error".to_string()))
                    })
                );
            }
            if tuple_elems.is_empty() {
                self.error_token(&name, "Cannot infer the type of an empty tuple literal");
                self.set_expr_type(expr, self.error_type(&name));
                return;
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
            let old_has_valid_return = self.function_has_valid_return;
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
                        false,
                    )
                );
                return;
            }
    
            // If we haven't seen a valid return statement, produce an error
            if !has_guaranteed_return {
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
            if type_.kind == TypeKind::User("infer".to_string()) {
                if let Some(init) = initialiser {
                    init.accept(self);
                    let inferred_ty = self
                        .get_expr_type(init)
                        .cloned()
                        .unwrap_or_else(|| self.error_type(name));
                    self.symtable.declare_symbol(
                        &name.lexeme,
                        Symbol::new_variable(name.clone(), inferred_ty),
                    );
                } else {
                    self.error_token(name, "Cannot infer the type of a variable without an initializer");
                    self.symtable.declare_symbol(
                        &name.lexeme,
                        Symbol::new_variable(name.clone(), self.error_type(name)),
                    );
                }
                return;
            }

            if let Some(init) = initialiser {
                // Temporarily define the variable
                self.symtable.begin_scope();
                self.symtable.declare_symbol(&name.lexeme, Symbol::new_variable(name.clone(), type_.clone()));

                // check if the type exists
                if !self.type_exists(type_) {
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
            if !self.type_exists(type_) {
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

    fn visit_class(&mut self, _stmt: &Stmt) {
    }

    fn visit_extension(&mut self, _stmt: &Stmt) {
        // skip
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn typecheck_fixture(relative_path: &str) -> usize {
        let filename = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), relative_path);
        let source = std::fs::read_to_string(&filename).expect("fixture should be readable");

        let mut lexer = Lexer::new(source.clone(), filename.clone());
        lexer.scan_tokens();
        assert!(
            !lexer.had_error,
            "fixture `{}` should lex cleanly",
            relative_path
        );

        let mut parser = Parser::new(lexer.tokens.clone(), source.clone(), filename.clone());
        let module = match parser.parse() {
            Ok(module) => module,
            Err(err) => panic!("fixture `{}` should parse: {}", relative_path, err.to_string()),
        };
        assert!(
            !parser.had_error,
            "fixture `{}` should parse cleanly",
            relative_path
        );

        let mut symtable = SymbolTable::new();
        let mut checker = TypeChecker::new(&mut symtable, filename, source);
        checker.check_module(&module);
        checker.error_count()
    }

    #[test]
    fn pass_fixtures_typecheck_cleanly() {
        for fixture in [
            "tests/pass/empty.crdm",
            "tests/pass/function_1.crdm",
            "tests/pass/function_2.crdm",
            "tests/pass/function_3.crdm",
            "tests/pass/comparison_1.crdm",
            "tests/pass/variable_1.crdm",
            "tests/pass/variable_2.crdm",
            "tests/pass/array_1.crdm",
            "tests/pass/array_2.crdm",
            "tests/pass/inference_1.crdm",
            "tests/pass/main_args.crdm",
        ] {
            assert_eq!(typecheck_fixture(fixture), 0, "`{}` should pass", fixture);
        }
    }

    #[test]
    fn fail_fixtures_report_type_errors() {
        for fixture in [
            "tests/fail/function_1.crdm",
            "tests/fail/function_2.crdm",
        ] {
            assert!(
                typecheck_fixture(fixture) > 0,
                "`{}` should report at least one type error",
                fixture
            );
        }
    }
}
