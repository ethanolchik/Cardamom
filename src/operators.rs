//! The language's operator contracts. Implementations live in ordinary standard
//! modules; this table is the only compiler knowledge of their names.
use crate::token::TokenKind;
use crate::ty::TypeKind;

#[derive(Clone, Copy)]
pub struct Operator {
    pub module: &'static str,
    pub trait_name: &'static str,
    pub method: &'static str,
    /// `ops` contracts take an Output parameter (and Rhs for binary operators).
    pub has_output: bool,
}

pub fn binary(kind: &TokenKind) -> Option<Operator> {
    use TokenKind::*;
    let (module, trait_name, method) = match kind {
        EqEq | Neq => ("cmp", "Eq", "equals"),
        Lt | Gte => ("cmp", "Comparable", "less"),
        Gt | Lte => ("cmp", "Comparable", "greater"),
        Plus | PlusEq => ("ops", "Add", "add"),
        Minus | MinusEq => ("ops", "Sub", "sub"),
        Mul | MulEq => ("ops", "Mul", "mul"),
        Div | DivEq => ("ops", "Div", "div"),
        Mod | ModEq => ("ops", "Rem", "rem"),
        Amp | AmpEq => ("ops", "BitAnd", "bitAnd"),
        Pipe | PipeEq => ("ops", "BitOr", "bitOr"),
        Caret | CaretEq => ("ops", "BitXor", "bitXor"),
        LShift | LShiftEq => ("ops", "Shl", "shl"),
        RShift | RShiftEq => ("ops", "Shr", "shr"),
        _ => return None,
    };
    Some(Operator {
        module,
        trait_name,
        method,
        has_output: module == "ops",
    })
}

pub fn unary(kind: &TokenKind) -> Option<Operator> {
    let (trait_name, method) = match kind {
        TokenKind::Minus => ("Neg", "neg"),
        TokenKind::Bang => ("Not", "not"),
        TokenKind::Tilde => ("BitNot", "bitNot"),
        _ => return None,
    };
    Some(Operator {
        module: "ops",
        trait_name,
        method,
        has_output: true,
    })
}

/// Built-ins never dispatch to trait methods (including inside primitive impls).
/// This both prevents recursion and keeps scalar operations independent of imports.
pub fn builtin_binary(op: &TokenKind, left: &TypeKind, right: &TypeKind) -> Option<TypeKind> {
    use TokenKind::*;
    use TypeKind as T;
    let integral = matches!(left, T::Int | T::Bool) && matches!(right, T::Int | T::Bool);
    let numeric = integral || matches!((left, right), (T::Float, T::Float));
    let strings = matches!((left, right), (T::String, T::String));
    match op {
        EqEq | Neq | Lt | Gt | Lte | Gte if numeric || strings => Some(T::Bool),
        And | Or if integral => Some(T::Bool),
        Plus | PlusEq if strings => Some(T::String),
        Plus | Minus | Mul | Div | PlusEq | MinusEq | MulEq | DivEq if numeric => {
            Some(if integral { T::Int } else { T::Float })
        }
        Mod | Amp | Pipe | Caret | LShift | RShift | ModEq | AmpEq | PipeEq | CaretEq
        | LShiftEq | RShiftEq
            if integral =>
        {
            Some(T::Int)
        }
        _ => None,
    }
}

pub fn builtin_unary(op: &TokenKind, operand: &TypeKind) -> Option<TypeKind> {
    match (op, operand) {
        (TokenKind::Minus, TypeKind::Int | TypeKind::Float) => Some(operand.clone()),
        (TokenKind::Bang, TypeKind::Int | TypeKind::Bool) => Some(TypeKind::Bool),
        (TokenKind::Tilde, TypeKind::Int) => Some(TypeKind::Int),
        _ => None,
    }
}
