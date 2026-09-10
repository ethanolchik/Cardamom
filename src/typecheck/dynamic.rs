//! Borrowed trait objects. Until Cardamom has lifetimes, handles may only be
//! parameters or immediate call arguments/receivers, never stored or returned.

use super::*;
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct DynamicTrait {
    pub trait_type: Type,
    /// Canonical slot order, independent of declaration and HashMap iteration order.
    pub methods: Vec<(String, ExportedMethod)>,
}

pub type DynamicTraits = BTreeMap<String, DynamicTrait>;

#[derive(Clone, Debug)]
pub struct DynamicCast {
    pub concrete: Type,
    pub trait_type: Type,
    /// Module of the explicit impl; `None` selects structural class methods.
    pub implementation: Option<String>,
}

pub type DynamicCasts = HashMap<*const Expr, DynamicCast>;

enum Usage<'a> {
    Value,
    Callee,
    Receiver,
    Argument(&'a Type),
}

fn ungroup(expr: &Expr) -> &Expr {
    match expr {
        Expr::Grouping { expression } => ungroup(expression),
        _ => expr,
    }
}

fn contains_borrow(kind: &TypeKind) -> bool {
    match kind {
        TypeKind::Reference(_) | TypeKind::MutRef(_) => true,
        TypeKind::Array(inner, _) | TypeKind::DynTrait(inner) => contains_borrow(&inner.kind),
        TypeKind::Tuple(types) | TypeKind::GenericInstance(_, _, types) => {
            types.iter().any(|ty| contains_borrow(&ty.kind))
        }
        TypeKind::Function(params, ret) => {
            params.iter().any(|ty| contains_borrow(&ty.kind)) || contains_borrow(&ret.kind)
        }
        _ => false,
    }
}

impl TypeChecker<'_> {
    /// `type_exists` accepts generic class names without checking arity. A table
    /// needs a complete type at every position, even when no cast instantiates it.
    fn dynamic_concrete_type(&self, ty: &Type) -> bool {
        match &ty.kind {
            TypeKind::User(module, name) | TypeKind::GenericInstance(module, name, _) => {
                let arity = if module.is_empty() || module == &self.module_name {
                    match self.symtable.lookup_class(name) {
                        Some(Symbol::Class { generics, .. }) => Some(generics.len()),
                        _ => None,
                    }
                } else {
                    self.module_exports
                        .get(module)
                        .and_then(|exports| exports.classes.get(name))
                        .map(|class| class.generics.len())
                };
                let arguments = match &ty.kind {
                    TypeKind::GenericInstance(_, _, arguments) => arguments.as_slice(),
                    _ => &[],
                };
                arity == Some(arguments.len())
                    && arguments.iter().all(|ty| self.dynamic_concrete_type(ty))
            }
            TypeKind::Array(inner, _) | TypeKind::Reference(inner) | TypeKind::MutRef(inner) => {
                inner.kind != TypeKind::Void && self.dynamic_concrete_type(inner)
            }
            TypeKind::Tuple(types) => types
                .iter()
                .all(|ty| ty.kind != TypeKind::Void && self.dynamic_concrete_type(ty)),
            TypeKind::Function(params, ret) => {
                params
                    .iter()
                    .all(|ty| ty.kind != TypeKind::Void && self.dynamic_concrete_type(ty))
                    && self.dynamic_concrete_type(ret)
            }
            TypeKind::Int
            | TypeKind::Float
            | TypeKind::String
            | TypeKind::Bool
            | TypeKind::Void => true,
            _ => false,
        }
    }

    pub(super) fn dynamic_trait(&mut self, trait_type: &Type) -> Option<DynamicTrait> {
        let key = trait_type.kind.to_string();
        if let Some(descriptor) = self.dynamic_traits.get(&key) {
            return Some(descriptor.clone());
        }
        let Some(declaration) = self.trait_declaration(trait_type).cloned() else {
            self.error_token(&trait_type.name, &format!("Unknown dynamic trait `{key}`"));
            return None;
        };
        let arguments = match &trait_type.kind {
            TypeKind::GenericInstance(_, _, arguments) => arguments.as_slice(),
            _ => &[],
        };
        if arguments.len() != declaration.generics.len() {
            self.error_token(
                &trait_type.name,
                &format!(
                    "Dynamic trait `{key}` expects {} type arguments, got {}",
                    declaration.generics.len(),
                    arguments.len()
                ),
            );
            return None;
        }
        for argument in arguments {
            if argument.kind.contains_dynamic()
                || !self.dynamic_concrete_type(argument)
                || contains_borrow(&argument.kind)
                || argument.kind == TypeKind::Void
            {
                self.error_token(
                    &argument.name,
                    "Dynamic trait arguments must be concrete owned types",
                );
                return None;
            }
            self.record_type_instantiations(argument);
        }
        let substitutions: HashMap<String, TypeKind> = declaration
            .generics
            .iter()
            .cloned()
            .zip(arguments.iter().map(|ty| ty.kind.clone()))
            .collect();
        let mut methods: Vec<_> = declaration.methods.into_iter().collect();
        methods.sort_by(|a, b| a.0.cmp(&b.0));
        for (name, method) in &mut methods {
            method.params = method
                .params
                .iter()
                .map(|ty| ty.apply_substitution(&substitutions))
                .collect();
            method.return_type = method.return_type.apply_substitution(&substitutions);
            let reason = if method.is_static {
                Some("static methods have no dynamic receiver")
            } else if !method.generics.is_empty() || !method.constraints.is_empty() {
                Some("generic methods cannot be placed in a method table")
            } else if method.params.iter().any(|ty| ty.kind == TypeKind::Void) {
                Some("method parameters cannot have type void")
            } else if method
                .params
                .iter()
                .chain(std::iter::once(&method.return_type))
                .any(|ty| ty.kind.contains_generics())
            {
                Some("method signatures cannot use Self or unresolved type parameters")
            } else if contains_borrow(&method.return_type.kind) {
                Some("borrowed results are not supported by dynamic dispatch")
            } else if method
                .params
                .iter()
                .chain(std::iter::once(&method.return_type))
                .any(|ty| ty.kind.contains_dynamic())
            {
                Some("dynamic method signatures cannot contain other dynamic objects")
            } else {
                None
            };
            if let Some(reason) = reason {
                self.error_token(
                    &trait_type.name,
                    &format!("Trait `{key}` cannot be used dynamically: method `{name}`: {reason}"),
                );
                return None;
            }
            for ty in method
                .params
                .iter()
                .chain(std::iter::once(&method.return_type))
            {
                if !self.dynamic_concrete_type(ty) {
                    self.error_token(&trait_type.name, &format!("Dynamic trait method `{name}` requires concrete valid signature types; got `{}`", ty.kind));
                    return None;
                }
                self.record_type_instantiations(ty);
            }
        }
        let descriptor = DynamicTrait {
            trait_type: trait_type.clone(),
            methods,
        };
        self.dynamic_traits.insert(key, descriptor.clone());
        Some(descriptor)
    }

    /// Returns true if this was a dynamic cast, including one with diagnostics.
    pub(super) fn check_dynamic_cast(
        &mut self,
        expr: &Expr,
        object: &Expr,
        source: &Type,
        target: &Type,
    ) -> bool {
        if !source.kind.contains_dynamic() && !target.kind.contains_dynamic() {
            return false;
        }
        let Some(trait_type) = target.kind.dynamic_trait() else {
            self.error_token(
                &target.name,
                "Dynamic casts require an `&dynamic Trait` target",
            );
            return true;
        };
        if self.dynamic_trait(trait_type).is_none() {
            return true;
        }
        if source.kind.same_type(&target.kind) {
            return true;
        }
        let Expr::Reference { object: borrowed } = ungroup(object) else {
            self.error_token(&get_token(object), "Dynamic casts require an explicit immutable borrow of a named value, e.g. `as &dynamic Trait (&value)`");
            return true;
        };
        let borrowed = ungroup(borrowed);
        let Expr::Variable { name } = borrowed else {
            self.error_token(&get_token(borrowed), "Dynamic casts can only borrow owned named values, not temporaries, fields or indexes");
            return true;
        };
        let Some(concrete) = self.get_expr_type(borrowed).cloned() else {
            return true;
        };
        if name.lexeme == "this"
            || !matches!(
                self.symtable.lookup_symbol(&name.lexeme),
                Some(Symbol::Variable(..))
            )
            || contains_borrow(&concrete.kind)
            || concrete.kind.contains_dynamic()
            || concrete.kind == TypeKind::Void
            || !self.dynamic_concrete_type(&concrete)
        {
            self.error_token(
                name,
                "Dynamic casts require an owned named value with a concrete type",
            );
            return true;
        }
        if let Err(reason) = self.type_satisfies_trait(&concrete.kind, trait_type) {
            self.error_token(
                name,
                &format!(
                    "Type `{}` does not satisfy dynamic trait `{}`: {reason}",
                    concrete.kind, trait_type.kind
                ),
            );
            return true;
        }
        let implementation = self
            .matching_explicit_impl(&concrete.kind, trait_type)
            .map(|(implementation, _)| implementation.module);
        self.record_type_instantiations(&concrete);
        self.record_explicit_impl_instantiations(&concrete.kind, trait_type);
        self.dynamic_casts.insert(
            expr as *const Expr,
            DynamicCast {
                concrete,
                trait_type: trait_type.clone(),
                implementation,
            },
        );
        true
    }

    fn check_dynamic_readonly(&self, cast: &DynamicCast, token: &Token) {
        if cast.implementation.is_none() {
            let readonly = match &cast.concrete.kind {
                TypeKind::User(module, name) | TypeKind::GenericInstance(module, name, _) => {
                    if module.is_empty() || module == &self.module_name {
                        self.class_const_methods.get(name)
                    } else {
                        self.module_exports
                            .get(module)
                            .and_then(|exports| exports.classes.get(name))
                            .map(|class| &class.const_methods)
                    }
                }
                _ => None,
            };
            for (method, _) in &self.dynamic_traits[&cast.trait_type.kind.to_string()].methods {
                if !readonly.is_some_and(|methods| methods.contains(method)) {
                    self.error_token(
                        token,
                        &format!(
                            "Dynamic trait method `{method}` must be read-only when borrowing `{}`",
                            cast.concrete.kind
                        ),
                    );
                    return;
                }
            }
        }
    }

    fn check_dynamic_type(&mut self, ty: &Type, generics: &[String], parameter: bool) {
        if !ty.kind.contains_dynamic() {
            return;
        }
        let ty = self.qualify_type(&Self::generalise(ty, generics));
        if parameter {
            if let Some(trait_type) = ty.kind.dynamic_trait() {
                self.dynamic_trait(trait_type);
                return;
            }
        }
        self.error_token(&ty.name, "Dynamic objects are only supported as direct `&dynamic Trait` parameters and immediate call arguments/receivers; they cannot be stored or returned");
    }

    pub(super) fn check_dynamic_uses(&mut self, module: &Module) {
        // All method bodies must be checked first, including classes declared
        // after a cast. Const analysis uses the resolved mutable call arguments.
        for stmt in &module.statements {
            if let Stmt::Class { name, methods, .. } = &**stmt {
                self.class_const_methods.insert(
                    name.lexeme.clone(),
                    const_methods(methods, &self.expr_types, &self.mutable_arguments.borrow()),
                );
            }
        }
        for stmt in &module.statements {
            self.dynamic_stmt(stmt, &[], false);
        }
    }

    fn dynamic_stmt(&mut self, stmt: &Stmt, generics: &[String], closure: bool) {
        match stmt {
            Stmt::Function {
                params,
                body,
                return_type,
                modifiers,
                generics: own,
                ..
            } => {
                let mut scope = generics.to_vec();
                scope.extend(own.iter().map(|g| g.lexeme.clone()));
                for param in params {
                    if let Stmt::Variable { type_, .. } = &**param {
                        self.check_dynamic_type(
                            type_,
                            &scope,
                            !modifiers.contains(&Modifier::Extern),
                        );
                    }
                }
                self.check_dynamic_type(return_type, &scope, false);
                for stmt in body {
                    self.dynamic_stmt(stmt, &scope, closure);
                }
            }
            Stmt::Class {
                fields,
                methods,
                generics: own,
                ..
            } => {
                let mut scope = generics.to_vec();
                scope.extend(own.iter().map(|g| g.lexeme.clone()));
                for stmt in fields.iter().chain(methods) {
                    self.dynamic_stmt(stmt, &scope, closure);
                }
            }
            Stmt::Impl {
                target,
                trait_type,
                methods,
                generics: own,
                ..
            } => {
                let mut scope = generics.to_vec();
                scope.extend(own.iter().map(|g| g.lexeme.clone()));
                self.check_dynamic_type(target, &scope, false);
                self.check_dynamic_type(trait_type, &scope, false);
                for stmt in methods {
                    self.dynamic_stmt(stmt, &scope, closure);
                }
            }
            Stmt::Trait {
                methods,
                generics: own,
                ..
            } => {
                let mut scope = generics.to_vec();
                scope.extend(own.iter().map(|g| g.lexeme.clone()));
                for stmt in methods {
                    self.dynamic_stmt(stmt, &scope, closure);
                }
            }
            Stmt::Extension { target, methods } => {
                self.check_dynamic_type(target, generics, false);
                for stmt in methods {
                    self.dynamic_stmt(stmt, generics, closure);
                }
            }
            Stmt::Variable {
                type_, initialiser, ..
            } => {
                self.check_dynamic_type(type_, generics, false);
                if let Some(expr) = initialiser {
                    self.dynamic_expr(expr, Usage::Value, generics, closure);
                }
            }
            Stmt::Expression { expression } => {
                self.dynamic_expr(expression, Usage::Value, generics, closure)
            }
            Stmt::Block { statements } => {
                for stmt in statements {
                    self.dynamic_stmt(stmt, generics, closure);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.dynamic_expr(condition, Usage::Value, generics, closure);
                self.dynamic_stmt(then_branch, generics, closure);
                if let Some(stmt) = else_branch {
                    self.dynamic_stmt(stmt, generics, closure);
                }
            }
            Stmt::While { condition, body } => {
                self.dynamic_expr(condition, Usage::Value, generics, closure);
                self.dynamic_stmt(body, generics, closure);
            }
            Stmt::For {
                initialiser,
                condition,
                increment,
                body,
            } => {
                if let Some(stmt) = initialiser {
                    self.dynamic_stmt(stmt, generics, closure);
                }
                for expr in condition.iter().chain(increment) {
                    self.dynamic_expr(expr, Usage::Value, generics, closure);
                }
                self.dynamic_stmt(body, generics, closure);
            }
            Stmt::Return {
                value: Some(expr), ..
            } => self.dynamic_expr(expr, Usage::Value, generics, closure),
            Stmt::Import { .. }
            | Stmt::Return { .. }
            | Stmt::Break { .. }
            | Stmt::Continue { .. } => {}
        }
    }

    /// Only declared parameter types can accept a handle. Inferring T = &dynamic
    /// Trait or calling through a function value must not circumvent escape checks.
    fn dynamic_parameters(&self, callee: &Expr) -> Option<Vec<Type>> {
        let callee = ungroup(callee);
        if self.function_refs.contains_key(&(callee as *const Expr)) {
            return self.callee_param_types(callee);
        }
        self.method_callee(callee).map(|signature| signature.params)
    }

    fn dynamic_expr(&mut self, expr: &Expr, usage: Usage<'_>, generics: &[String], closure: bool) {
        // Grouping preserves whether this is a direct argument, receiver or callee.
        if let Expr::Grouping { expression } = expr {
            self.dynamic_expr(expression, usage, generics, closure);
            return;
        }
        if let Some(ty) = self.get_expr_type(expr) {
            if ty.kind.contains_dynamic() {
                let allowed = if ty.kind.dynamic_trait().is_some() {
                    match &usage {
                        Usage::Receiver => true,
                        Usage::Argument(expected) => {
                            expected.kind.dynamic_trait().is_some()
                                && ty.kind.same_type(&expected.kind)
                        }
                        _ => false,
                    }
                } else {
                    matches!(usage, Usage::Callee)
                        && (self.function_refs.contains_key(&(expr as *const Expr))
                            || self.method_callee(expr).is_some())
                };
                if !allowed {
                    self.error_token(&get_token(expr), "Dynamic objects cannot escape: use them only as direct dynamic parameters or immediate call arguments/receivers");
                }
                if closure
                    && matches!(expr, Expr::Variable { .. })
                    && ty.kind.dynamic_trait().is_some()
                {
                    self.error_token(
                        &get_token(expr),
                        "A closure cannot capture a dynamic trait parameter",
                    );
                }
            }
        }
        if self
            .call_instantiations
            .get(&(expr as *const Expr))
            .is_some_and(|types| types.iter().any(TypeKind::contains_dynamic))
        {
            self.error_token(
                &get_token(expr),
                "Dynamic objects cannot be used as generic type arguments",
            );
        }
        match expr {
            Expr::Call {
                callee, arguments, ..
            }
            | Expr::GenericCall {
                callee, arguments, ..
            } => {
                if let Expr::GenericCall {
                    generics: arguments,
                    ..
                } = expr
                {
                    for ty in arguments {
                        self.check_dynamic_type(ty, generics, false);
                    }
                    if let Expr::MemberAccess { object, .. } = ungroup(callee) {
                        if self
                            .get_expr_type(object)
                            .is_some_and(|ty| ty.kind.dynamic_trait().is_some())
                        {
                            self.error_token(
                                &get_token(callee),
                                "Dynamic trait methods do not accept type arguments",
                            );
                        }
                    }
                }
                let params = self.dynamic_parameters(callee);
                self.dynamic_expr(callee, Usage::Callee, generics, closure);
                for (i, argument) in arguments.iter().enumerate() {
                    let usage = params
                        .as_ref()
                        .and_then(|params| params.get(i))
                        .map(Usage::Argument)
                        .unwrap_or(Usage::Value);
                    self.dynamic_expr(argument, usage, generics, closure);
                }
            }
            Expr::MemberAccess { object, .. } | Expr::StaticAccess { object, .. } => {
                if self
                    .get_expr_type(object)
                    .is_some_and(|ty| ty.kind.dynamic_trait().is_some())
                    && !matches!(usage, Usage::Callee)
                {
                    self.error_token(
                        &get_token(expr),
                        "Dynamic trait methods must be called directly",
                    );
                }
                self.dynamic_expr(object, Usage::Receiver, generics, closure);
            }
            Expr::Cast { object, .. } => {
                if let Some(cast) = self.dynamic_casts.get(&(expr as *const Expr)) {
                    self.check_dynamic_readonly(cast, &get_token(object));
                }
                // An identity cast forwards the same borrowed handle.
                let identity = self
                    .get_expr_type(expr)
                    .zip(self.get_expr_type(object))
                    .is_some_and(|(target, source)| {
                        target.kind.dynamic_trait().is_some() && target.kind.same_type(&source.kind)
                    });
                self.dynamic_expr(
                    object,
                    if identity { usage } else { Usage::Value },
                    generics,
                    closure,
                );
            }
            Expr::Closure {
                body,
                param_types,
                return_type,
                ..
            } => {
                for ty in param_types.iter().chain(std::iter::once(return_type)) {
                    self.check_dynamic_type(ty, generics, false);
                }
                self.dynamic_stmt(body, generics, true);
            }
            Expr::ClassInit {
                arguments,
                generics: types,
                ..
            } => {
                for ty in types {
                    self.check_dynamic_type(ty, generics, false);
                }
                for arg in arguments {
                    self.dynamic_expr(arg, Usage::Value, generics, closure);
                }
            }
            Expr::Intrinsic { arguments, .. } => {
                for arg in arguments {
                    self.dynamic_expr(arg, Usage::Value, generics, closure);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.dynamic_expr(left, Usage::Value, generics, closure);
                self.dynamic_expr(right, Usage::Value, generics, closure);
            }
            Expr::Array { elements, .. } | Expr::Tuple { elements } => {
                for expr in elements {
                    self.dynamic_expr(expr, Usage::Value, generics, closure);
                }
            }
            Expr::Assignment { value, .. } => {
                self.dynamic_expr(value, Usage::Value, generics, closure)
            }
            Expr::MemberAssignment { object, value, .. }
            | Expr::StaticAssignment { object, value, .. } => {
                self.dynamic_expr(object, Usage::Value, generics, closure);
                self.dynamic_expr(value, Usage::Value, generics, closure);
            }
            Expr::IndexAssignment {
                object,
                index,
                value,
                ..
            } => {
                self.dynamic_expr(object, Usage::Value, generics, closure);
                self.dynamic_expr(index, Usage::Value, generics, closure);
                self.dynamic_expr(value, Usage::Value, generics, closure);
            }
            Expr::Index { object, index, .. } => {
                self.dynamic_expr(object, Usage::Value, generics, closure);
                self.dynamic_expr(index, Usage::Value, generics, closure);
            }
            Expr::Reference { object }
            | Expr::MutReference { object }
            | Expr::Unary { right: object, .. } => {
                self.dynamic_expr(object, Usage::Value, generics, closure);
            }
            Expr::Grouping { .. } | Expr::Literal { .. } | Expr::Variable { .. } => {}
        }
    }
}
