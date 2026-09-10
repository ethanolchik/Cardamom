# Operators and traits

Operators participate in Cardamom's type system. For user-defined types they
resolve to public trait methods, using the same structural checking and static
monomorphisation as bounded method calls. Merely having two values of the same
type no longer makes an arbitrary operator legal.

Import `cmp` for comparison contracts and `ops` for arithmetic/bitwise contracts.
Aliases work too: `import ops as arithmetic;` refers to the same contracts.

## Contracts

| Syntax | Contract | Method |
| --- | --- | --- |
| `a == b`, `a != b` | `cmp.Eq` | `equals(other: &Self) -> bool` |
| `a < b`, `a >= b` | `cmp.Comparable` | `less(other: &Self) -> bool` |
| `a > b`, `a <= b` | `cmp.Comparable` | `greater(other: &Self) -> bool` |
| `a + b` | `ops.Add<Rhs, Output>` | `add(other: &Rhs) -> Output` |
| `a - b` | `ops.Sub<Rhs, Output>` | `sub(other: &Rhs) -> Output` |
| `a * b` | `ops.Mul<Rhs, Output>` | `mul(other: &Rhs) -> Output` |
| `a / b` | `ops.Div<Rhs, Output>` | `div(other: &Rhs) -> Output` |
| `a % b` | `ops.Rem<Rhs, Output>` | `rem(other: &Rhs) -> Output` |
| `a & b`, `a \| b`, `a ^ b` | `ops.BitAnd<Rhs, Output>`, `BitOr<Rhs, Output>`, `BitXor<Rhs, Output>` | `bitAnd`, `bitOr`, `bitXor` |
| `a << b`, `a >> b` | `ops.Shl<Rhs, Output>`, `Shr<Rhs, Output>` | `shl`, `shr` |
| `-a`, `!a`, `~a` | `ops.Neg<Output>`, `Not<Output>`, `BitNot<Output>` | `neg()`, `not()`, `bitNot()` |

Compound assignments (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`,
`>>=`) use the corresponding binary contract and assign its result back. The
result must fit the destination type, and the destination must be writable.
Variables, fields, static fields, and array elements are supported. An overloaded
compound assignment evaluates its destination once, not once to read and again
to write.

Comparisons always produce `bool`; legacy `int`-returning comparison methods
remain accepted. For custom types, `!=` negates `equals`, `<=` negates `greater`,
and `>=` negates `less`. `Comparable` is a **total-order contract**: its two methods
must agree, and the compiler cannot prove the ordering laws. An unordered custom
type should not claim this contract. Primitive floating-point operators retain
IEEE unordered/NaN behaviour, including inside generic functions.

## Structural implementations

A class with compatible public methods satisfies these traits without an `impl`:

```cpp
import cmp;
import ops;

class Distance(public metres: int) {
    public equals(other: &Distance) -> bool {
        return this.metres == other.metres;
    }
    public add(other: &Distance) -> Distance {
        return new Distance(this.metres + other.metres);
    }
    public mul(scale: &int) -> int {
        return this.metres * scale;
    }
}

fn main() {
    let a: Distance = new Distance(3);
    let b: Distance = a + new Distance(4);
    let same: bool = b == new Distance(7);
    let scaled: int = b * 2;
    a += b;
}
```

`Rhs` and `Output` need not be `Self`: the multiplication above satisfies
`ops.Mul<int, int>`. The compiler infers a concrete structural operator's output
from its method signature. Operator methods borrow their receiver and should not
mutate it; use the result or a compound assignment to update a value.

## Explicit and generic implementations

Explicit implementations can adapt existing/foreign classes or primitives.
They take precedence over structural methods for the same contract. Multiple
applicable implementations or output contracts are errors rather than depending
on hash-map iteration order.

```cpp
import cmp;

class Box<T>(public value: T) {}

impl<T> cmp.Eq for Box<T> where T: cmp.Eq {
    public equals(other: &Box<T>) -> bool {
        return this.value == other.value;
    }
}

fn equal<T>(left: &T, right: &T) -> bool where T: cmp.Eq {
    return left == right;
}
```

Generic bodies must declare the capabilities they use. An unconstrained `T` does
not support equality or arithmetic merely because a particular caller uses `int`.
Forwarding `T` to another constrained function must also prove that function's
bounds. Nested and cross-module generic impl dependencies are specialised along
with their callers.

Separate input/output types can be expressed in bounds:

```cpp
import ops;

fn combine<L, R, O>(left: &L, right: &R) -> O where L: ops.Add<R, O> {
    return left + right;
}
```

Call as `combine<int, int, int>(2, 3)`, for example. Output-only type parameters
currently need explicit type arguments; these are ordinary generic parameters,
not associated types. This change does not add associated types, supertraits,
dynamic trait objects, implicit conversions, or new operator spellings.

## Standard implementations and evaluation

- Integers have arithmetic, remainder, bitwise, shift, negation, and logical-not
  implementations. Floats have arithmetic and negation. Strings have addition;
  booleans have logical-not. Primitives implement the comparison traits.
- Arrays implement `Eq` when their elements do, and lexicographic `Comparable`
  when their elements do. This recurses through nested arrays and calls element
  trait methods rather than requiring C++ operators on the element class.
- `cmp.equal`, `cmp.less`, `cmp.greater`, `cmp.min`, and `cmp.max` use these contracts.
  `collections.Hashmap` key comparisons now use `==` under its `cmp.Eq` bound.
- Native primitive operations remain built-ins and do not need imports. They
  cannot be replaced by an impl. This also avoids recursion when a primitive
  trait implementation uses the operator it implements.
- Overloaded operands are evaluated left-to-right, once each, and borrowed for
  the method call. Short-circuit `&&` and `||` remain built-ins and are **not**
  overloadable. String literals compare by value, including grouped literals.
- Unsupported operations are diagnosed by Cardamom rather than blindly emitted
  as C++. For example, subtracting strings and taking a floating-point remainder
  are not native operations.

See `tests/pass/operators_1.crdm` and `operators_2.crdm` for executable examples,
and `tests/fail/operator_*.crdm` for rejected programs.
