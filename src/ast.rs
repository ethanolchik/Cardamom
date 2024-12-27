use crate::token::Token;
use crate::ty::Type;

/// A modifier for a class or function
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Modifier {
    Public,
    Private,
    Protected,
    Static,
    Builtin,
    Method,
    Constructor,
    None,
}

/// An attribute for a class
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Attribute {
    Generic,
    Class,
}

/// A derived type
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Derived {
    Const,
    Ref,
    Ptr,
    Array,
    Lambda,
    Mut,
}

/// AST node
pub trait Node {
    fn accept(&self, visitor: &mut dyn Visitor);
}

/// An expression
#[derive(Clone, Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Token,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Array {
        elements: Vec<Box<Expr>>,
    },
    Tuple {
        elements: Vec<Box<Expr>>,
    },
    Assignment {
        name: Token,
        value: Box<Expr>,
        op: Token,
    },
    MemberAssignment {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
        op: Token,
    },
    IndexAssignment {
        object: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
        op: Token,
    },
    PtrAssignment {
        object: Box<Expr>,
        value: Box<Expr>,
        op: Token,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Box<Expr>>,
    },
    MemberAccess {
        object: Box<Expr>,
        name: Token,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    Cast {
        object: Box<Expr>,
        type_: Type,
    },
    ClassInit {
        name: Token,
        arguments: Vec<Box<Expr>>,
    },
    Dereference {
        object: Box<Expr>,
    },
    Reference {
        object: Box<Expr>,
    },
    MutReference {
        object: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Expression {
        expression: Box<Expr>,
    },
    Block {
        statements: Vec<Box<Stmt>>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    For {
        initialiser: Option<Box<Stmt>>,
        condition: Option<Box<Expr>>,
        increment: Option<Box<Expr>>,
        body: Box<Stmt>,
    },
    Return {
        value: Option<Box<Expr>>,
    },
    Break,
    Continue,
    Variable {
        name: Token,
        initialiser: Option<Box<Expr>>,
        type_: Type,
        modifiers: Vec<Modifier>,
        derived: Vec<Derived>,
    },
    Function {
        name: Token,
        params: Vec<Box<Stmt>>,
        body: Vec<Box<Stmt>>,
        return_type: Type,
        modifiers: Vec<Modifier>,
        generics: Vec<Token>,
    },
    Import {
        path: Box<Expr>,
        alias: Token,
    },
    Class {
        name: Token,
        generics: Vec<Token>,
        modifier: Vec<Modifier>,
        public_methods: Vec<Box<Stmt>>,
        private_methods: Vec<Box<Stmt>>,
        protected_methods: Vec<Box<Stmt>>,
        static_methods: Vec<Box<Stmt>>,
        public_fields: Vec<Box<Stmt>>,
        private_fields: Vec<Box<Stmt>>,
        protected_fields: Vec<Box<Stmt>>,
        static_fields: Vec<Box<Stmt>>,
    },
}

pub struct Module {
    pub statements: Vec<Box<Stmt>>,
}

impl Node for Module {
    fn accept(&self, visitor: &mut dyn Visitor) {
        visitor.visit_module(self);
    }
}

pub trait Visitor {
    fn visit_binary(&mut self, expr: &Expr);
    fn visit_unary(&mut self, expr: &Expr);
    fn visit_literal(&mut self, expr: &Expr);
    fn visit_grouping(&mut self, expr: &Expr);
    fn visit_variable_expr(&mut self, expr: &Expr);
    fn visit_assignment(&mut self, expr: &Expr);
    fn visit_call(&mut self, expr: &Expr);
    fn visit_member_access(&mut self, expr: &Expr);
    fn visit_index(&mut self, expr: &Expr);
    fn visit_cast(&mut self, expr: &Expr);
    fn visit_class_init(&mut self, expr: &Expr);
    fn visit_dereference(&mut self, expr: &Expr);
    fn visit_reference(&mut self, expr: &Expr);
    fn visit_mut_reference(&mut self, expr: &Expr);
    fn visit_array(&mut self, expr: &Expr);
    fn visit_tuple(&mut self, expr: &Expr);
    fn visit_member_assignment(&mut self, stmt: &Expr);
    fn visit_index_assignment(&mut self, stmt: &Expr);
    fn visit_ptr_assignment(&mut self, stmt: &Expr);
    fn visit_expression(&mut self, stmt: &Stmt);
    fn visit_block(&mut self, stmt: &Stmt);
    fn visit_if(&mut self, stmt: &Stmt);
    fn visit_while(&mut self, stmt: &Stmt);
    fn visit_for(&mut self, stmt: &Stmt);
    fn visit_return(&mut self, stmt: &Stmt);
    fn visit_break(&mut self, stmt: &Stmt);
    fn visit_continue(&mut self, stmt: &Stmt);
    fn visit_function(&mut self, stmt: &Stmt);
    fn visit_variable(&mut self, stmt: &Stmt);
    fn visit_import(&mut self, stmt: &Stmt);
    fn visit_module(&mut self, stmt: &Module);
    fn visit_class(&mut self, stmt: &Stmt);
}


impl Node for Expr {
    fn accept(&self, visitor: &mut dyn Visitor) {
        match self {
            Expr::Binary { .. } => visitor.visit_binary(self),
            Expr::Unary { .. } => visitor.visit_unary(self),
            Expr::Literal { .. } => visitor.visit_literal(self),
            Expr::Grouping { .. } => visitor.visit_grouping(self),
            Expr::Variable { .. } => visitor.visit_variable_expr(self),
            Expr::Array { .. } => visitor.visit_array(self),
            Expr::Tuple { .. } => visitor.visit_tuple(self),
            Expr::Assignment { .. } => visitor.visit_assignment(self),
            Expr::MemberAssignment { .. } => visitor.visit_member_assignment(self),
            Expr::IndexAssignment { .. } => visitor.visit_index_assignment(self),
            Expr::PtrAssignment { .. } => visitor.visit_ptr_assignment(self),
            Expr::Call { .. } => visitor.visit_call(self),
            Expr::MemberAccess { .. } => visitor.visit_member_access(self),
            Expr::Index { .. } => visitor.visit_index(self),
            Expr::Cast { .. } => visitor.visit_cast(self),
            Expr::ClassInit { .. } => visitor.visit_class_init(self),
            Expr::Dereference { .. } => visitor.visit_dereference(self),
            Expr::Reference { .. } => visitor.visit_reference(self),
            Expr::MutReference { .. } => visitor.visit_mut_reference(self),
        }
    }
}

impl Stmt {
    pub fn accept(&self, visitor: &mut dyn Visitor) {
        match self {
            Stmt::Expression { .. } => visitor.visit_expression(self),
            Stmt::Block { .. } => visitor.visit_block(self),
            Stmt::If { .. } => visitor.visit_if(self),
            Stmt::While { .. } => visitor.visit_while(self),
            Stmt::For { .. } => visitor.visit_for(self),
            Stmt::Return { .. } => visitor.visit_return(self),
            Stmt::Break => visitor.visit_break(self),
            Stmt::Continue => visitor.visit_continue(self),
            Stmt::Variable { .. } => visitor.visit_variable(self),
            Stmt::Function { .. } => visitor.visit_function(self),
            Stmt::Import { .. } => visitor.visit_import(self),
            Stmt::Class { .. } => visitor.visit_class(self),
        }
    }
}

impl Modifier {
    pub fn to_string(&self) -> String {
        match self {
            Modifier::Public => "Public".to_string(),
            Modifier::Private => "Private".to_string(),
            Modifier::Protected => "Protected".to_string(),
            Modifier::Static => "Static".to_string(),
            Modifier::Builtin => "Builtin".to_string(),
            Modifier::Method => "Method".to_string(),
            Modifier::Constructor => "Constructor".to_string(),
            Modifier::None => "None".to_string(),
        }
    }
}

impl Attribute {
    pub fn to_string(&self) -> String {
        match self {
            Attribute::Generic => "Generic".to_string(),
            Attribute::Class => "Class".to_string(),
        }
    }
}

impl Derived {
    pub fn to_string(&self) -> String {
        match self {
            Derived::Const => "Const".to_string(),
            Derived::Ref => "Ref".to_string(),
            Derived::Ptr => "Ptr".to_string(),
            Derived::Array => "Array".to_string(),
            Derived::Lambda => "Lambda".to_string(),
            Derived::Mut => "Mut".to_string(),
        }
    }
}