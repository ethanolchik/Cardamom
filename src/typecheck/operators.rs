//! Operator resolution uses the same trait contracts and impl instantiations as
//! explicit bounded method calls; there is no fallback to arbitrary C++ operators.
use super::*;
use crate::operators::{self, Operator};

impl TypeChecker<'_> {
    pub(super) fn check_compound_assignment(&mut self, expr: &Expr) -> bool {
        let (op, value) = match expr {
            Expr::Assignment { op, value, .. }
            | Expr::MemberAssignment { op, value, .. }
            | Expr::StaticAssignment { op, value, .. }
            | Expr::IndexAssignment { op, value, .. }
                if op.kind != TokenKind::Eq =>
            {
                (op, value)
            }
            _ => return false,
        };
        let target = match expr {
            Expr::Assignment { name, .. } => match self.symtable.lookup_symbol(&name.lexeme) {
                Some(Symbol::Variable(_, _, ty, ..)) => Some(ty.clone()),
                _ => None,
            },
            Expr::MemberAssignment { object, name, .. }
            | Expr::StaticAssignment { object, name, .. } => {
                object.accept(self);
                let ty = self.get_expr_type(object).cloned();
                if let Some(ty) = ty {
                    self.check_assignable(&ty, op, "assignment receiver");
                    self.assignment_field(
                        &Self::without_borrows(&ty).kind,
                        name,
                        matches!(expr, Expr::StaticAssignment { .. }),
                    )
                } else {
                    None
                }
            }
            Expr::IndexAssignment { object, index, .. } => {
                object.accept(self);
                index.accept(self);
                if !self
                    .get_expr_type(index)
                    .is_some_and(|ty| Self::without_borrows(ty).kind == TypeKind::Int)
                {
                    self.error_token(op, "Array index must be an integer");
                }
                let ty = self.get_expr_type(object).cloned();
                ty.and_then(|ty| {
                    self.check_assignable(&ty, op, "indexed assignment receiver");
                    match Self::without_borrows(&ty).kind {
                        TypeKind::Array(inner, _) => Some(*inner),
                        _ => None,
                    }
                })
            }
            _ => None,
        };
        let Some(target) = target else {
            self.error_token(op, "Invalid compound assignment target");
            self.set_expr_type(expr, self.error_type(op));
            return true;
        };
        self.check_assignable(&target, op, "compound assignment target");
        self.with_expected_type(None, |checker| value.accept(checker));
        let rhs = self
            .get_expr_type(value)
            .cloned()
            .unwrap_or_else(|| self.error_type(op));
        let result = self.check_operator(expr, op, &target, Some(&rhs));
        let target = Self::without_borrows(&target);
        if !result.is_compatible_with(&target) {
            self.error_token(
                op,
                &format!(
                    "Operator result `{}` cannot be assigned back to `{}`",
                    result.kind, target.kind
                ),
            );
        }
        self.set_expr_type(expr, target);
        true
    }

    fn assignment_field(
        &self,
        receiver: &TypeKind,
        name: &Token,
        static_access: bool,
    ) -> Option<Type> {
        let (module, class, arguments) = match receiver {
            TypeKind::User(module, class) => (module, class, Vec::new()),
            TypeKind::GenericInstance(module, class, args) => (module, class, args.clone()),
            _ => return None,
        };
        let field = if module.is_empty() || module == &self.module_name {
            let Symbol::Class { fields, .. } = self.symtable.lookup_class(class)? else {
                return None;
            };
            let field = fields.get(&name.lexeme)?;
            if !self.is_member_visible(&field.1, class) {
                self.error_token(name, "Cannot assign to a private field");
            }
            field
        } else {
            self.module_exports
                .get(module)?
                .classes
                .get(class)?
                .fields
                .get(&name.lexeme)?
        };
        if field.2 != static_access {
            self.error_token(name, "Incorrect static/instance field access");
        }
        Some(
            field
                .0
                .apply_substitution(&self.class_substitution(module, class, &arguments)),
        )
    }

    pub(super) fn check_operator(
        &mut self,
        expr: &Expr,
        token: &Token,
        receiver: &Type,
        rhs: Option<&Type>,
    ) -> Type {
        let receiver = Self::without_borrows(receiver);
        let rhs = rhs.map(Self::without_borrows);
        let builtin = match &rhs {
            Some(rhs) => operators::builtin_binary(&token.kind, &receiver.kind, &rhs.kind),
            None => operators::builtin_unary(&token.kind, &receiver.kind),
        };
        if let Some(kind) = builtin {
            return Type::new(token.clone(), kind);
        }
        let contract = if rhs.is_some() {
            operators::binary(&token.kind)
        } else {
            operators::unary(&token.kind)
        };
        let Some(contract) = contract else {
            self.error_token(
                token,
                &format!(
                    "Operator `{}` is not defined for `{}`{}",
                    token.lexeme,
                    receiver.kind,
                    rhs.as_ref()
                        .map(|t| format!(" and `{}`", t.kind))
                        .unwrap_or_default()
                ),
            );
            return self.error_type(token);
        };
        match self.resolve_operator(contract, &receiver, rhs.as_ref()) {
            Ok((trait_type, result)) => {
                self.record_explicit_impl_instantiations(&receiver.kind, &trait_type);
                self.record_type_instantiations(&result);
                self.trait_call_sites.insert(
                    expr as *const Expr,
                    TraitCallSite {
                        trait_type,
                        method: contract.method.to_string(),
                    },
                );
                if contract.has_output {
                    result
                } else {
                    Type::new(token.clone(), TypeKind::Bool)
                }
            }
            Err(reason) => {
                self.error_token(
                    token,
                    &format!(
                        "Operator `{}` requires `{}.{}` for `{}`: {}",
                        token.lexeme, contract.module, contract.trait_name, receiver.kind, reason
                    ),
                );
                self.error_type(token)
            }
        }
    }

    fn resolve_operator(
        &self,
        op: Operator,
        receiver: &Type,
        rhs: Option<&Type>,
    ) -> Result<(Type, Type), String> {
        let matches_contract = |ty: &Type| match &ty.kind {
            TypeKind::User(module, name) | TypeKind::GenericInstance(module, name, _) => {
                module == op.module && name == op.trait_name
            }
            _ => false,
        };
        let mut candidates = Vec::new();
        if let TypeKind::GenericParam(parameter) = &receiver.kind {
            candidates.extend(
                self.current_constraints
                    .get(parameter)
                    .into_iter()
                    .flatten()
                    .filter(|ty| matches_contract(ty))
                    .cloned(),
            );
        } else {
            for implementation in self.explicit_impls.iter().chain(
                self.module_exports
                    .values()
                    .flat_map(|m| &m.implementations),
            ) {
                if !matches_contract(&implementation.trait_type) {
                    continue;
                }
                let mut subs = HashMap::new();
                if !implementation
                    .target
                    .kind
                    .match_pattern(&receiver.kind, &mut subs)
                {
                    continue;
                }
                if op.has_output {
                    if let (Some(rhs), TypeKind::GenericInstance(_, _, arguments)) =
                        (rhs, &implementation.trait_type.kind)
                    {
                        if !arguments
                            .first()
                            .is_some_and(|arg| arg.kind.match_pattern(&rhs.kind, &mut subs))
                        {
                            continue;
                        }
                    }
                }
                candidates.push(implementation.trait_type.apply_substitution(&subs));
            }
            // Structural satisfaction remains available without an explicit impl.
            if candidates.is_empty() {
                let kind = if op.has_output {
                    let method = self.operator_method(&receiver.kind, op.method)
                        .ok_or_else(|| format!("missing public method `{}` (import `{}` and add a matching method or impl)", op.method, op.module))?;
                    let mut arguments: Vec<Type> = rhs.cloned().into_iter().collect();
                    arguments.push(method.return_type);
                    TypeKind::GenericInstance(op.module.into(), op.trait_name.into(), arguments)
                } else {
                    TypeKind::User(op.module.into(), op.trait_name.into())
                };
                candidates.push(Type::new(Token::dummy(op.trait_name), kind));
            }
        }
        let mut valid = Vec::new();
        let mut reason = format!(
            "missing `{}` bound; import `{}` and constrain this type parameter",
            op.trait_name, op.module
        );
        for candidate in candidates {
            let checked = (|| {
                self.type_satisfies_trait(&receiver.kind, &candidate)?;
                let declaration = self
                    .trait_declaration(&candidate)
                    .ok_or("trait is not imported")?;
                let arguments = match &candidate.kind {
                    TypeKind::GenericInstance(_, _, arguments) => arguments.as_slice(),
                    _ => &[],
                };
                let expected_arity = if op.has_output {
                    1 + usize::from(rhs.is_some())
                } else {
                    0
                };
                if arguments.len() != expected_arity || declaration.generics.len() != expected_arity
                {
                    return Err("invalid operator trait type arguments".to_string());
                }
                let mut subs = HashMap::from([("Self".into(), receiver.kind.clone())]);
                subs.extend(
                    declaration
                        .generics
                        .iter()
                        .cloned()
                        .zip(arguments.iter().map(|a| a.kind.clone())),
                );
                let method = declaration
                    .methods
                    .get(op.method)
                    .ok_or("operator trait method is missing")?;
                if method.is_static
                    || !method.generics.is_empty()
                    || method.params.len() != usize::from(rhs.is_some())
                {
                    return Err("invalid operator method signature".to_string());
                }
                if let Some(rhs) = rhs {
                    let param = method.params[0].apply_substitution(&subs);
                    if matches!(param.kind, TypeKind::MutRef(_)) || !rhs.is_compatible_with(&param)
                    {
                        return Err(format!(
                            "right operand `{}` does not match `{}`",
                            rhs.kind, param.kind
                        ));
                    }
                }
                let result = method.return_type.apply_substitution(&subs);
                if op.has_output {
                    if !result.kind.same_type(&arguments.last().unwrap().kind)
                        || result.kind == TypeKind::Void
                    {
                        return Err(
                            "operator result must match its non-void Output type".to_string()
                        );
                    }
                } else if !matches!(result.kind, TypeKind::Bool | TypeKind::Int) {
                    return Err("comparison methods must return bool (or legacy int)".to_string());
                }
                Ok(result)
            })();
            match checked {
                Ok(result) => valid.push((candidate, result)),
                Err(error) => reason = error,
            }
        }
        match valid.len() {
            1 => Ok(valid.pop().unwrap()),
            0 => Err(reason),
            _ => Err(
                "ambiguous operator: multiple matching trait bounds or implementations".to_string(),
            ),
        }
    }

    fn operator_method(&self, receiver: &TypeKind, name: &str) -> Option<ExportedMethod> {
        let (module, class, arguments) = match receiver {
            TypeKind::User(module, class) => (module, class, Vec::new()),
            TypeKind::GenericInstance(module, class, arguments) => {
                (module, class, arguments.clone())
            }
            _ => return None,
        };
        let mut method = if module.is_empty() || module == &self.module_name {
            let Symbol::Class { methods, .. } = self.symtable.lookup_class(class)? else {
                return None;
            };
            let Symbol::Function {
                params,
                return_type,
                generics,
                visibility: Some(Visibility::Public),
                is_static,
                ..
            } = methods.get(name)?
            else {
                return None;
            };
            ExportedMethod {
                params: params.clone(),
                return_type: return_type.clone(),
                generics: generics.clone(),
                visibility: Visibility::Public,
                is_static: *is_static,
                constraints: Vec::new(),
            }
        } else {
            self.module_exports
                .get(module)?
                .classes
                .get(class)?
                .methods
                .get(name)?
                .clone()
        };
        let subs = self.class_substitution(module, class, &arguments);
        method.params = method
            .params
            .iter()
            .map(|p| p.apply_substitution(&subs))
            .collect();
        method.return_type = method.return_type.apply_substitution(&subs);
        Some(method)
    }
}
