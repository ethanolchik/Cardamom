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
    Extern,
    Extension,
    None,
}

/// The visibility of a class member.
///
/// Visibility and storage (`static`) are orthogonal, so they are tracked separately:
/// a member can be any combination of the two, e.g. `private static`.
pub fn member_visibility(modifiers: &[Modifier]) -> Modifier {
    modifiers
        .iter()
        .find(|m| {
            matches!(
                m,
                Modifier::Public | Modifier::Private | Modifier::Protected
            )
        })
        .cloned()
        // Members are private unless they say otherwise.
        .unwrap_or(Modifier::Private)
}

/// Whether a class member was declared `static`.
pub fn is_static_member(modifiers: &[Modifier]) -> bool {
    modifiers.contains(&Modifier::Static)
}

/// Whether a field came from the class header, and so is also a constructor parameter.
///
/// `class Person(private name: string)` declares `name` as both a field and the first
/// constructor parameter; this is what distinguishes it from a plain field.
pub fn is_constructor_field(modifiers: &[Modifier]) -> bool {
    modifiers.contains(&Modifier::Constructor)
}

/// The modifiers attached to a class member (`Stmt::Variable` or `Stmt::Function`).
pub fn member_modifiers(stmt: &Stmt) -> &[Modifier] {
    match stmt {
        Stmt::Variable { modifiers, .. } => modifiers,
        Stmt::Function { modifiers, .. } => modifiers,
        _ => &[],
    }
}

/// An attribute for a class
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Attribute {
    Generic,
    Class,
}

/// A derived type
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Derived {
    Const,
    Ref,
    Array,
    Lambda,
    MutRef,
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
        /// The `[` token, so an empty literal still has a source location.
        token: Token,
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
    StaticAssignment {
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
        token: Token,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Box<Expr>>,
    },
    GenericCall {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Box<Expr>>,
        generics: Vec<Type>,
    },
    MemberAccess {
        object: Box<Expr>,
        name: Token,
    },
    StaticAccess {
        object: Box<Expr>,
        name: Token,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        token: Token,
    },
    Cast {
        object: Box<Expr>,
        type_: Type,
    },
    ClassInit {
        name: Token,
        /// Explicit type arguments, as in `new Box<int>(..)`. Empty when they are to be
        /// inferred from the constructor arguments.
        generics: Vec<Type>,
        arguments: Vec<Box<Expr>>,
    },
    Reference {
        object: Box<Expr>,
    },
    MutReference {
        object: Box<Expr>,
    },
    Closure {
        name: Token,
        parameters: Vec<Token>,
        param_types: Vec<Type>,
        body: Box<Stmt>,
        return_type: Type,
    },
    /// A compiler intrinsic, written `@name(args)`.
    ///
    /// Intrinsics are the escape hatch that lets the standard library be written in
    /// Cardamom: `@cpp("..")` splices raw C++ into the generated function body, and
    /// `@include("<x>")` adds a header to the generated file.
    Intrinsic {
        name: Token,
        arguments: Vec<Box<Expr>>,
    },
}

#[derive(Clone, Debug)]
pub struct GenericConstraint {
    pub parameter: Token,
    pub traits: Vec<Type>,
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
        token: Token,
        value: Option<Box<Expr>>,
    },
    Break {
        token: Token,
    },
    Continue {
        token: Token,
    },
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
        constraints: Vec<GenericConstraint>,
    },
    Import {
        /// The module being imported, e.g. `io` in `import io;`.
        name: Token,
        /// The name it is bound to, which is the module name unless `as` was used.
        alias: Token,
    },
    Class {
        name: Token,
        generics: Vec<Token>,
        constraints: Vec<GenericConstraint>,
        modifier: Vec<Modifier>,
        /// Every field, in source order. Visibility and `static` live in each
        /// field's own `modifiers`, because they are independent properties.
        fields: Vec<Box<Stmt>>,
        /// Every method, in source order, annotated the same way as `fields`.
        methods: Vec<Box<Stmt>>,
    },
    Extension {
        target: Box<Type>,
        methods: Vec<Box<Stmt>>,
    },
    Trait {
        name: Token,
        generics: Vec<Token>,
        methods: Vec<Box<Stmt>>,
        modifier: Vec<Modifier>,
    },
    /// Marker implementation: required methods are ordinary methods on `target`.
    Impl {
        trait_type: Type,
        target: Type,
        generics: Vec<Token>,
        constraints: Vec<GenericConstraint>,
        methods: Vec<Box<Stmt>>,
        modifier: Vec<Modifier>,
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
    fn visit_generic_call(&mut self, expr: &Expr);
    fn visit_member_access(&mut self, expr: &Expr);
    fn visit_static_access(&mut self, expr: &Expr);
    fn visit_index(&mut self, expr: &Expr);
    fn visit_cast(&mut self, expr: &Expr);
    fn visit_class_init(&mut self, expr: &Expr);
    fn visit_reference(&mut self, expr: &Expr);
    fn visit_mut_reference(&mut self, expr: &Expr);
    fn visit_closure(&mut self, expr: &Expr);
    fn visit_intrinsic(&mut self, expr: &Expr);
    fn visit_array(&mut self, expr: &Expr);
    fn visit_tuple(&mut self, expr: &Expr);
    fn visit_member_assignment(&mut self, stmt: &Expr);
    fn visit_static_assignment(&mut self, stmt: &Expr);
    fn visit_index_assignment(&mut self, stmt: &Expr);
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
    fn visit_extension(&mut self, stmt: &Stmt);
    fn visit_trait(&mut self, _stmt: &Stmt) {}
    fn visit_impl(&mut self, _stmt: &Stmt) {}
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
            Expr::StaticAssignment { .. } => visitor.visit_static_assignment(self),
            Expr::IndexAssignment { .. } => visitor.visit_index_assignment(self),
            Expr::Call { .. } => visitor.visit_call(self),
            Expr::GenericCall { .. } => visitor.visit_generic_call(self),
            Expr::MemberAccess { .. } => visitor.visit_member_access(self),
            Expr::StaticAccess { .. } => visitor.visit_static_access(self),
            Expr::Index { .. } => visitor.visit_index(self),
            Expr::Cast { .. } => visitor.visit_cast(self),
            Expr::ClassInit { .. } => visitor.visit_class_init(self),
            Expr::Reference { .. } => visitor.visit_reference(self),
            Expr::MutReference { .. } => visitor.visit_mut_reference(self),
            Expr::Closure { .. } => visitor.visit_closure(self),
            Expr::Intrinsic { .. } => visitor.visit_intrinsic(self),
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
            Stmt::Break { .. } => visitor.visit_break(self),
            Stmt::Continue { .. } => visitor.visit_continue(self),
            Stmt::Variable { .. } => visitor.visit_variable(self),
            Stmt::Function { .. } => visitor.visit_function(self),
            Stmt::Import { .. } => visitor.visit_import(self),
            Stmt::Class { .. } => visitor.visit_class(self),
            Stmt::Extension { .. } => visitor.visit_extension(self),
            Stmt::Trait { .. } => visitor.visit_trait(self),
            Stmt::Impl { .. } => visitor.visit_impl(self),
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
            Modifier::Extern => "Extern".to_string(),
            Modifier::Extension => "Extension".to_string(),
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
            Derived::Array => "Array".to_string(),
            Derived::Lambda => "Lambda".to_string(),
            Derived::MutRef => "Mut".to_string(),
        }
    }
}
