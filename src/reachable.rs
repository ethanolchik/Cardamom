//! Reachability analysis over the call graph.
//!
//! A program that imports `str` should not pay for every function `str` defines. This
//! walks outwards from the program's own code, following calls across module
//! boundaries, and reports which functions are actually needed. Code generation then
//! emits only those.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Stmt};
use crate::modules::Program;
use crate::typecheck::FunctionRefs;

/// A function identified by the module that defines it and its name.
pub type FunctionRef = (String, String);

/// Per-module information needed to resolve a call to the function it refers to.
struct ModuleContext<'a> {
    /// Functions defined in this module.
    locals: HashSet<String>,
    /// Function name -> body, so the walk can follow calls into it.
    bodies: HashMap<String, &'a [Box<Stmt>]>,
    /// Bodies that are always considered live, such as class methods.
    always_live: Vec<&'a [Box<Stmt>]>,
}

/// Returns every function reachable from the program's own code.
///
/// The program's own module is treated as entirely live, since a user does not expect
/// their own functions to vanish; imported modules are reduced to what is used.
pub fn analyse(program: &Program, function_refs: &FunctionRefs) -> HashSet<FunctionRef> {
    let mut contexts: HashMap<String, ModuleContext> = HashMap::new();

    for module in &program.modules {
        let mut locals = HashSet::new();
        let mut bodies = HashMap::new();
        let mut always_live = Vec::new();

        for stmt in &module.ast.statements {
            match &**stmt {
                Stmt::Function { name, body, .. } => {
                    locals.insert(name.lexeme.clone());
                    bodies.insert(name.lexeme.clone(), body.as_slice());
                }
                // Classes are always emitted in full, so their methods are always live
                // and anything they call must be kept too.
                Stmt::Class { methods, .. } => {
                    for method in methods {
                        if let Stmt::Function { body, .. } = &**method {
                            always_live.push(body.as_slice());
                        }
                    }
                }
                Stmt::Extension { methods, .. } | Stmt::Impl { methods, .. } => {
                    for method in methods {
                        if let Stmt::Function { body, .. } = &**method {
                            always_live.push(body.as_slice());
                        }
                    }
                }
                _ => {}
            }
        }

        contexts.insert(
            module.name.clone(),
            ModuleContext {
                locals,
                bodies,
                always_live,
            },
        );
    }

    let root = program.root().name.clone();

    // Seed the walk with everything that is live by definition.
    let mut worklist: Vec<FunctionRef> = Vec::new();
    let mut reachable: HashSet<FunctionRef> = HashSet::new();

    if let Some(root_context) = contexts.get(&root) {
        for name in &root_context.locals {
            worklist.push((root.clone(), name.clone()));
        }
    }

    // Class and extension method bodies are emitted regardless, in every module.
    for context in contexts.values() {
        for body in &context.always_live {
            for called in calls_in(body, function_refs) {
                worklist.push(called);
            }
        }
    }

    while let Some(function) = worklist.pop() {
        if !reachable.insert(function.clone()) {
            continue;
        }

        let (module_name, function_name) = &function;
        let Some(context) = contexts.get(module_name) else {
            continue;
        };
        let Some(body) = context.bodies.get(function_name) else {
            continue;
        };

        for called in calls_in(body, function_refs) {
            if !reachable.contains(&called) {
                worklist.push(called);
            }
        }
    }

    reachable
}

/// Includes functions passed or stored as values, as well as direct calls. The
/// checker resolves each reference against lexical scopes and selective aliases.
fn calls_in(body: &[Box<Stmt>], function_refs: &FunctionRefs) -> Vec<FunctionRef> {
    let mut collector = CallCollector {
        function_refs,
        found: Vec::new(),
    };

    for stmt in body {
        collector.walk_stmt(stmt);
    }

    collector.found
}

struct CallCollector<'a> {
    function_refs: &'a FunctionRefs,
    found: Vec<FunctionRef>,
}

impl<'a> CallCollector<'a> {
    fn walk_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => self.walk_expr(expression),
            Stmt::Block { statements } => {
                for s in statements {
                    self.walk_stmt(s);
                }
            }
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
            Stmt::Return { value, .. } => {
                if let Some(value) = value {
                    self.walk_expr(value);
                }
            }
            Stmt::Variable { initialiser, .. } => {
                if let Some(initialiser) = initialiser {
                    self.walk_expr(initialiser);
                }
            }
            Stmt::Function { body, .. } => {
                for s in body {
                    self.walk_stmt(s);
                }
            }
            Stmt::Break { .. }
            | Stmt::Continue { .. }
            | Stmt::Import { .. }
            | Stmt::Class { .. }
            | Stmt::Extension { .. }
            | Stmt::Trait { .. }
            | Stmt::Impl { .. } => {}
        }
    }

    fn walk_expr(&mut self, expr: &Expr) {
        if let Some(function) = self.function_refs.get(&(expr as *const Expr)) {
            self.found.push(function.clone());
        }
        match expr {
            Expr::Call {
                callee, arguments, ..
            } => {
                self.walk_expr(callee);
                for argument in arguments {
                    self.walk_expr(argument);
                }
            }
            Expr::GenericCall {
                callee, arguments, ..
            } => {
                self.walk_expr(callee);
                for argument in arguments {
                    self.walk_expr(argument);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            Expr::Unary { right, .. } => self.walk_expr(right),
            Expr::Grouping { expression } => self.walk_expr(expression),
            Expr::Array { elements, .. } | Expr::Tuple { elements } => {
                for element in elements {
                    self.walk_expr(element);
                }
            }
            Expr::Assignment { value, .. } => self.walk_expr(value),
            Expr::MemberAssignment { object, value, .. }
            | Expr::StaticAssignment { object, value, .. } => {
                self.walk_expr(object);
                self.walk_expr(value);
            }
            Expr::IndexAssignment {
                object,
                index,
                value,
                ..
            } => {
                self.walk_expr(object);
                self.walk_expr(index);
                self.walk_expr(value);
            }
            Expr::MemberAccess { object, .. } | Expr::StaticAccess { object, .. } => {
                self.walk_expr(object)
            }
            Expr::Index { object, index, .. } => {
                self.walk_expr(object);
                self.walk_expr(index);
            }
            Expr::Cast { object, .. }
            | Expr::Reference { object }
            | Expr::MutReference { object } => self.walk_expr(object),
            Expr::ClassInit { arguments, .. } => {
                for argument in arguments {
                    self.walk_expr(argument);
                }
            }
            Expr::Closure { body, .. } => self.walk_stmt(body),
            Expr::Intrinsic { arguments, .. } => {
                for argument in arguments {
                    self.walk_expr(argument);
                }
            }
            Expr::Literal { .. } | Expr::Variable { .. } => {}
        }
    }
}
