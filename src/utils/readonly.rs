use std::collections::{HashMap, HashSet};

use crate::ast::{is_static_member, Expr, Stmt};
use crate::ty::{Type, TypeKind};

/// Works out which methods can be marked `const` in the generated C++.
///
/// A method is const unless it writes to a member of `this` or calls another
/// method on `this` that is not itself const. The second condition makes this a
/// fixpoint: assume every method is const, then repeatedly drop the ones that turn
/// out not to be, until nothing changes.
pub fn const_methods(
    methods: &[Box<Stmt>],
    expr_types: &HashMap<*const Expr, Type>,
    mutable_arguments: &HashSet<*const Expr>,
) -> HashSet<String> {
    let mut candidates: HashSet<String> = methods
        .iter()
        .filter_map(|method| match &**method {
            // A static method has no `this`, so `const` does not apply.
            Stmt::Function {
                name, modifiers, ..
            } if !is_static_member(modifiers) => Some(name.lexeme.clone()),
            _ => None,
        })
        .collect();

    loop {
        let mut changed = false;

        for method in methods {
            let Stmt::Function { name, body, .. } = &**method else {
                continue;
            };
            if !candidates.contains(&name.lexeme) {
                continue;
            }

            let mutates = {
                let mut detector = MutationDetector {
                    const_methods: &candidates,
                    expr_types,
                    mutable_arguments,
                    mutates: false,
                };
                for stmt in body {
                    detector.walk_stmt(stmt);
                }
                detector.mutates
            };

            if mutates {
                candidates.remove(&name.lexeme);
                changed = true;
            }
        }

        if !changed {
            return candidates;
        }
    }
}

/// Detects whether a method body mutates the object it is called on.
///
/// Used to decide if a method can be `const` in the generated C++.
struct MutationDetector<'a> {
    /// Methods currently believed to be const, for resolving calls on `this`.
    const_methods: &'a HashSet<String>,
    expr_types: &'a HashMap<*const Expr, Type>,
    mutable_arguments: &'a HashSet<*const Expr>,
    mutates: bool,
}

impl<'a> MutationDetector<'a> {
    fn is_this(expr: &Expr) -> bool {
        match expr {
            Expr::Variable { name } => name.lexeme == "this",
            Expr::Grouping { expression } => Self::is_this(expression),
            _ => false,
        }
    }

    fn readonly_builtin(&self, object: &Expr, method: &str) -> bool {
        let Some(ty) = self.expr_types.get(&(object as *const Expr)) else {
            return false;
        };
        let mut kind = &ty.kind;
        while let TypeKind::Reference(inner) | TypeKind::MutRef(inner) = kind {
            kind = &inner.kind;
        }
        match kind {
            TypeKind::Array(..) => method == "len",
            TypeKind::String => matches!(method, "len" | "charAt" | "charCodeAt"),
            _ => false,
        }
    }

    /// Whether an expression ultimately refers to `this`.
    fn targets_this(expr: &Expr) -> bool {
        match expr {
            Expr::Variable { name } => name.lexeme == "this",
            Expr::MemberAccess { object, .. } | Expr::Index { object, .. } => {
                Self::targets_this(object)
            }
            Expr::Grouping { expression } => Self::targets_this(expression),
            _ => false,
        }
    }

    fn walk_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => self.walk_expr(expression),
            Stmt::Block { statements } => statements.iter().for_each(|s| self.walk_stmt(s)),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(condition);
                self.walk_stmt(then_branch);
                if let Some(else_branch) = else_branch {
                    self.walk_stmt(else_branch);
                }
            }
            Stmt::While { condition, body } => {
                self.walk_expr(condition);
                self.walk_stmt(body);
            }
            Stmt::For {
                initialiser,
                condition,
                increment,
                body,
            } => {
                if let Some(initialiser) = initialiser {
                    self.walk_stmt(initialiser);
                }
                if let Some(condition) = condition {
                    self.walk_expr(condition);
                }
                if let Some(increment) = increment {
                    self.walk_expr(increment);
                }
                self.walk_stmt(body);
            }
            Stmt::Return {
                value: Some(value), ..
            } => self.walk_expr(value),
            Stmt::Variable {
                initialiser: Some(initialiser),
                type_,
                ..
            } => {
                if matches!(type_.kind, TypeKind::MutRef(_)) && Self::targets_this(initialiser) {
                    self.mutates = true;
                }
                self.walk_expr(initialiser);
            }
            _ => {}
        }
    }

    fn walk_expr(&mut self, expr: &Expr) {
        // Implicit mutable borrows (e.g. change(this.field)) need the resolved
        // parameter type, which is recorded at call checking time.
        if self.mutable_arguments.contains(&(expr as *const Expr)) && Self::targets_this(expr) {
            self.mutates = true;
        }
        match expr {
            // Any write reaching `this` rules the method out.
            Expr::MemberAssignment { object, value, .. } => {
                if Self::targets_this(object) {
                    self.mutates = true;
                }
                self.walk_expr(value);
            }
            Expr::IndexAssignment {
                object,
                index,
                value,
                ..
            } => {
                if Self::targets_this(object) {
                    self.mutates = true;
                }
                self.walk_expr(object);
                self.walk_expr(index);
                self.walk_expr(value);
            }
            Expr::Call {
                callee, arguments, ..
            }
            | Expr::GenericCall {
                callee, arguments, ..
            } => {
                // Calling a non-const method on `this` mutates it transitively.
                if let Expr::MemberAccess { object, name } = &**callee {
                    let readonly = self.readonly_builtin(object, &name.lexeme)
                        || (Self::is_this(object) && self.const_methods.contains(&name.lexeme));
                    if Self::targets_this(object) && !readonly {
                        self.mutates = true;
                    }
                }
                self.walk_expr(callee);
                arguments.iter().for_each(|a| self.walk_expr(a));
            }
            Expr::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            Expr::Unary { right, .. } => self.walk_expr(right),
            Expr::Grouping { expression } => self.walk_expr(expression),
            Expr::Array { elements, .. } | Expr::Tuple { elements } => {
                elements.iter().for_each(|e| self.walk_expr(e))
            }
            Expr::Assignment { name, value, .. } => {
                if name.lexeme == "this" {
                    self.mutates = true;
                }
                self.walk_expr(value);
            }
            Expr::StaticAssignment { value, .. } => self.walk_expr(value),
            Expr::MutReference { object } => {
                if Self::targets_this(object) {
                    self.mutates = true;
                }
                self.walk_expr(object);
            }
            Expr::MemberAccess { object, .. }
            | Expr::StaticAccess { object, .. }
            | Expr::Cast { object, .. }
            | Expr::Reference { object } => self.walk_expr(object),
            Expr::Index { object, index, .. } => {
                self.walk_expr(object);
                self.walk_expr(index);
            }
            Expr::ClassInit { arguments, .. } => arguments.iter().for_each(|a| self.walk_expr(a)),
            Expr::Closure { body, .. } => self.walk_stmt(body),
            Expr::Intrinsic { .. } | Expr::Literal { .. } | Expr::Variable { .. } => {}
        }
    }
}
