use crate::ast::*;
use crate::ty::{Type, TypeKind};

pub struct AstPrinter {
    indent: usize,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }
}

impl Visitor for AstPrinter {
    fn visit_binary(&mut self, expr: &Expr) {
        println!("{}Binary: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Binary { left, right, .. } = expr {
            left.accept(self);
            right.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_unary(&mut self, expr: &Expr) {
        println!("{}Unary: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Unary { right, .. } = expr {
            right.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_literal(&mut self, expr: &Expr) {
        println!("{}Literal: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Literal { value, .. } = expr {
            println!("{}Value: {}", String::from("\t").repeat(self.indent), value.to_string());
        }
        self.indent -= 1;
    }

    fn visit_grouping(&mut self, expr: &Expr) {
        println!("{}Grouping: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Grouping { expression, .. } = expr {
            expression.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_variable_expr(&mut self, expr: &Expr) {
        println!("{}Variable: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Variable { name, .. } = expr {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
        }
        self.indent -= 1;
    }

    fn visit_assignment(&mut self, expr: &Expr) {
        println!("{}Assignment: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Assignment { name, value, .. } = expr {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            value.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_call(&mut self, expr: &Expr) {
        println!("{}Call: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Call { callee, arguments, .. } = expr {
            callee.accept(self);
            for arg in arguments.iter() {
                arg.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_member_access(&mut self, expr: &Expr) {
        println!("{}MemberAccess: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::MemberAccess { object, name, .. } = expr {
            object.accept(self);
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
        }
        self.indent -= 1;
    }

    fn visit_index(&mut self, expr: &Expr) {
        println!("{}Index: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Index { object, index, .. } = expr {
            object.accept(self);
            index.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_cast(&mut self, expr: &Expr) {
        println!("{}Cast: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Cast { object, type_, .. } = expr {
            object.accept(self);
            println!("{}Type: ", String::from("\t").repeat(self.indent));
            printtype(type_.clone(), self.indent+1);
        }
        self.indent -= 1;
    }

    fn visit_class_init(&mut self, expr: &Expr) {
        println!("{}ClassInit: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::ClassInit { name, arguments, .. } = expr {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            for arg in arguments.iter() {
                arg.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_dereference(&mut self, expr: &Expr) {
        println!("{}Dereference: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Dereference { object, .. } = expr {
            object.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_reference(&mut self, expr: &Expr) {
        println!("{}Reference: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Reference { object, .. } = expr {
            object.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_mut_reference(&mut self, expr: &Expr) {
        println!("{}MutReference: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::MutReference { object, .. } = expr {
            object.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_array(&mut self, expr: &Expr) {
        println!("{}Array: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Array { elements, .. } = expr {
            for element in elements.iter() {
                element.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_tuple(&mut self, expr: &Expr) {
        println!("{}Tuple: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::Tuple { elements, .. } = expr {
            for element in elements.iter() {
                element.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_member_assignment(&mut self, stmt: &Expr) {
        println!("{}MemberAssignment: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::MemberAssignment { object, name, value, .. } = stmt {
            object.accept(self);
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            value.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_index_assignment(&mut self, stmt: &Expr) {
        println!("{}IndexAssignment: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::IndexAssignment { object, index, value, .. } = stmt {
            object.accept(self);
            index.accept(self);
            value.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_ptr_assignment(&mut self, stmt: &Expr) {
        println!("{}PtrAssignment: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Expr::PtrAssignment { object, value, .. } = stmt {
            object.accept(self);
            value.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_expression(&mut self, stmt: &Stmt) {
        println!("{}Expression: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Expression { expression, .. } = stmt {
            expression.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_block(&mut self, stmt: &Stmt) {
        println!("{}Block: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Block { statements, .. } = stmt {
            for statement in statements.iter() {
                statement.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_if(&mut self, stmt: &Stmt) {
        println!("{}If: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::If { condition, then_branch, else_branch, .. } = stmt {
            condition.accept(self);
            then_branch.accept(self);
            if let Some(else_branch) = else_branch {
                else_branch.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_while(&mut self, stmt: &Stmt) {
        println!("{}While: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::While { condition, body, .. } = stmt {
            condition.accept(self);
            body.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_for(&mut self, stmt: &Stmt) {
        println!("{}For: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::For { initialiser, condition, increment, body, .. } = stmt {
            if let Some(initialiser) = initialiser {
                initialiser.accept(self);
            }
            if let Some(condition) = condition {
                condition.accept(self);
            }
            if let Some(increment) = increment {
                increment.accept(self);
            }
            body.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_return(&mut self, stmt: &Stmt) {
        println!("{}Return: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Return { value, .. } = stmt {
            if let Some(value) = value {
                value.accept(self);
            }
        }
        self.indent -= 1;
    }

    fn visit_break(&mut self, _stmt: &Stmt) {
        println!("{}Break: ", String::from("\t").repeat(self.indent));
    }

    fn visit_continue(&mut self, _stmt: &Stmt) {
        println!("{}Continue: ", String::from("\t").repeat(self.indent));
    }

    fn visit_function(&mut self, stmt: &Stmt) {
        println!("{}Function: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Function { name, params, body, return_type, .. } = stmt {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            for param in params.iter() {
                param.accept(self);
            }
            for stmt in body.iter() {
                stmt.accept(self);
            }
            println!("{}Type: ", String::from("\t").repeat(self.indent));
            printtype(return_type.clone(), self.indent+1);
        }
        self.indent -= 1;
    }

    fn visit_variable(&mut self, stmt: &Stmt) {
        println!("{}Variable: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Variable { name, initialiser, type_, modifiers, derived, .. } = stmt {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            if let Some(initialiser) = initialiser {
                initialiser.accept(self);
            }
            println!("{}Type: ", String::from("\t").repeat(self.indent));
            printtype(type_.clone(), self.indent+1);
            for modifier in modifiers.iter() {
                println!("{}Modifier: {}", String::from("\t").repeat(self.indent), modifier.to_string());
            }
            for der in derived.iter() {
                println!("{}Derived: {}", String::from("\t").repeat(self.indent), der.to_string());
            }
        }
        self.indent -= 1;
    }

    fn visit_import(&mut self, stmt: &Stmt) {
        println!("{}Import: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Import { path, alias, .. } = stmt {
            path.accept(self);
            println!("{}Alias: {}", String::from("\t").repeat(self.indent), alias.lexeme);
        }
        self.indent -= 1;
    }

    fn visit_module(&mut self, stmt: &Module) {
        println!("{}Module: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        for statement in stmt.statements.iter() {
            statement.accept(self);
        }
        self.indent -= 1;
    }

    fn visit_class(&mut self, stmt: &Stmt) {
        println!("{}Class: ", String::from("\t").repeat(self.indent));
        self.indent += 1;
        if let Stmt::Class { name, generics, modifier, public_methods, private_methods, protected_methods, static_methods, public_fields, private_fields, protected_fields, static_fields, .. } = stmt {
            println!("{}Name: {}", String::from("\t").repeat(self.indent), name.lexeme);
            for g in generics.iter() {
                println!("{}Generic: {}", String::from("\t").repeat(self.indent), g.lexeme);
            }
            for modi in modifier.iter() {
                println!("{}Modifier: {:?}", String::from("\t").repeat(self.indent), modi);
            }
            for field in public_fields.iter() {
                field.accept(self);
            }
            for field in private_fields.iter() {
                field.accept(self);
            }
            for field in protected_fields.iter() {
                field.accept(self);
            }
            for field in static_fields.iter() {
                field.accept(self);
            }
            for method in public_methods.iter() {
                method.accept(self);
            }
            for method in private_methods.iter() {
                method.accept(self);
            }
            for method in protected_methods.iter() {
                method.accept(self);
            }
            for method in static_methods.iter() {
                method.accept(self);
            }
        }
        self.indent -= 1;
    }
}

fn printtype(type_: Type, indent: usize) {
    match type_.kind {
        TypeKind::Int => {
            println!("{}Int", String::from("\t").repeat(indent));
        }
        TypeKind::Float => {
            println!("{}Float", String::from("\t").repeat(indent));
        }
        TypeKind::String => {
            println!("{}String", String::from("\t").repeat(indent));
        }
        TypeKind::Array(ref inner, size) => {
            println!("{}Array: ", String::from("\t").repeat(indent));
            printtype(*inner.clone(), indent + 1);
            println!("{}Size: {}", String::from("\t").repeat(indent+1), size);
        }
        TypeKind::Function(ref params, ref return_type) => {
            println!("{}Function: ", String::from("\t").repeat(indent));
            for param in params.iter() {
                printtype(param.clone(), indent + 1);
            }
            printtype(*return_type.clone(), indent + 1);
        }
        TypeKind::Pointer(ref inner) => {
            println!("{}Pointer: ", String::from("\t").repeat(indent));
            printtype(*inner.clone(), indent + 1);
        }
        TypeKind::Reference(ref inner) => {
            println!("{}Reference: ", String::from("\t").repeat(indent));
            printtype(*inner.clone(), indent + 1);
        }
        TypeKind::MutRef(ref inner) => {
            println!("{}MutRef: ", String::from("\t").repeat(indent));
            printtype(*inner.clone(), indent + 1);
        }
        TypeKind::Struct(ref fields) => {
            println!("{}Struct: ", String::from("\t").repeat(indent));
            for field in fields.iter() {
                printtype(field.clone(), indent + 1);
            }
        }
        TypeKind::Tuple(ref fields) => {
            println!("{}Tuple: ", String::from("\t").repeat(indent));
            for field in fields.iter() {
                printtype(field.clone(), indent + 1);
            }
        }
        TypeKind::Void => {
            println!("{}Void", String::from("\t").repeat(indent));
        }
        TypeKind::User(ref name) => {
            println!("{}User: ", String::from("\t").repeat(indent));
            println!("{}Name: {}", String::from("\t").repeat(indent+1), name);
        }
        TypeKind::Generic(ref name) => {
            println!("{}Generic: ", String::from("\t").repeat(indent));
            println!("{}Name: {}", String::from("\t").repeat(indent+1), name);
        }
    }

    if type_.has_generic() {
        println!("{}Generics: ", String::from("\t").repeat(indent));
        for g in type_.generics.iter() {
            printtype(*g.clone(), indent + 1);
        }
    }
}