//! Reachability analysis over the call graph.
//!
//! A program that imports `str` should not pay for every function `str` defines. This
//! walks outwards from the program's own code, following calls across module
//! boundaries, and reports which functions are actually needed. Code generation then
//! emits only those.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Stmt};
use crate::modules::Program;

/// A function identified by the module that defines it and its name.
pub type FunctionRef = (String, String);

/// Per-module information needed to resolve a call to the function it refers to.
struct ModuleContext {
    /// Functions defined in this module.
    locals: HashSet<String>,
    /// `alias -> module name` for this module's imports.
    imports: HashMap<String, String>,
    /// Function name -> body, so the walk can follow calls into it.
    bodies: HashMap<String, Vec<Box<Stmt>>>,
    /// Bodies that are always considered live, such as class methods.
    always_live: Vec<Vec<Box<Stmt>>>,
}

/// Returns every function reachable from the program's own code.
///
/// The program's own module is treated as entirely live, since a user does not expect
/// their own functions to vanish; imported modules are reduced to what is used.
pub fn analyse(program: &Program) -> HashSet<FunctionRef> {
    let mut contexts: HashMap<String, ModuleContext> = HashMap::new();

    for module in &program.modules {
        let mut locals = HashSet::new();
        let mut bodies = HashMap::new();
        let mut always_live = Vec::new();

        for stmt in &module.ast.statements {
            match &**stmt {
                Stmt::Function { name, body, .. } => {
                    locals.insert(name.lexeme.clone());
                    bodies.insert(name.lexeme.clone(), body.clone());
                }
                // Classes are always emitted in full, so their methods are always live
                // and anything they call must be kept too.
                Stmt::Class { methods, .. } => {
                    for method in methods {
                        if let Stmt::Function { body, .. } = &**method {
                            always_live.push(body.clone());
                        }
                    }
                }
                Stmt::Extension { methods, .. } => {
                    for method in methods {
                        if let Stmt::Function { body, .. } = &**method {
                            always_live.push(body.clone());
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
                imports: module.imports.clone(),
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
    for (module_name, context) in &contexts {
        for body in &context.always_live {
            for called in calls_in(body, module_name, context) {
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

        for called in calls_in(body, module_name, context) {
            if !reachable.contains(&called) {
                worklist.push(called);
            }
        }
    }

    reachable
}

/// Every function call appearing anywhere in `body`, resolved to a `FunctionRef`.
fn calls_in(body: &[Box<Stmt>], module: &str, context: &ModuleContext) -> Vec<FunctionRef> {
    let mut collector = CallCollector {
        module,
        context,
        found: Vec::new(),
    };

    for stmt in body {
        collector.walk_stmt(stmt);
    }

    collector.found
}

struct CallCollector<'a> {
    module: &'a str,
    context: &'a ModuleContext,
    found: Vec<FunctionRef>,
}

impl<'a> CallCollector<'a> {
    /// Records a call if the callee resolves to a known function.
    fn record_callee(&mut self, callee: &Expr) {
        match callee {
            // `foo()` refers to a function of the module being walked.
            Expr::Variable { name } => {
                if self.context.locals.contains(&name.lexeme) {
                    self.found.push((self.module.to_string(), name.lexeme.clone()));
                }
            }
            // `io.println()` refers to a function of an imported module.
            Expr::MemberAccess { object, name } => {
                if let Expr::Variable { name: object_name } = &**object {
                    if let Some(imported) = self.context.imports.get(&object_name.lexeme) {
                        self.found.push((imported.clone(), name.lexeme.clone()));
                    }
                }
            }
            _ => {}
        }
    }

    fn walk_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => self.walk_expr(expression),
            Stmt::Block { statements } => {
                for s in statements {
                    self.walk_stmt(s);
                }
            }
            Stmt::If { condition, then_branch, else_branch } => {
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
            Stmt::For { initialiser, condition, increment, body } => {
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
            | Stmt::Extension { .. } => {}
        }
    }

    fn walk_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Call { callee, arguments, .. } => {
                self.record_callee(callee);
                self.walk_expr(callee);
                for argument in arguments {
                    self.walk_expr(argument);
                }
            }
            Expr::GenericCall { callee, arguments, .. } => {
                self.record_callee(callee);
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
            Expr::Array { elements } | Expr::Tuple { elements } => {
                for element in elements {
                    self.walk_expr(element);
                }
            }
            Expr::Assignment { value, .. }
            | Expr::MemberAssignment { value, .. }
            | Expr::StaticAssignment { value, .. }
            | Expr::PtrAssignment { value, .. } => self.walk_expr(value),
            Expr::IndexAssignment { object, index, value, .. } => {
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
            | Expr::Dereference { object }
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
