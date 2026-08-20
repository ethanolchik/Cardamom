# Cardamom
Another programming language implementation (hopefully this time will be better).


## Example code
```cpp
import io;

public class Person(private name: string, private age: int) {
    private address: string = "1 High Road";

    public getAddress() -> string {
        return this.address;
    }

    public moveHouse(address: Option<string>) -> void {
        this.address = address.value() ?? this.address; // either new address or old address
    }

    public birthday() -> int {
        this.age += 1;

        return this.age;
    }

    public getName() -> string {
        return this.name;
    }
}

fn main() -> void {
    let p: Person = new Person("Ethan", 17);

    io.println("Hello, $1! You live at $2.", p.getName(), p.getAddress());

    io.println("Happy $1 birthday, $2!", p.birthday(), p.getName());
}
```

> Note: `Option` and `??` in the example above are parsed but not yet implemented
> end to end.

## Modules

`import <name>;` looks for a module as `<name>.crdm` or `<name>/main.crdm`, searching
next to the importing file first and then the standard library. Only `public` functions
are visible to importers.

```cpp
// geometry/main.crdm
import math;

public fn clamped(v: int, lo: int, hi: int) -> int {
    return math.min(math.max(v, lo), hi);
}

fn helper() -> int { return 1; }   // private to this module
```

```cpp
// app.crdm
import io;
import str;
import geometry;

fn main() {
    io.println(str.fromInt(geometry.clamped(99, 0, 10)));
}
```

Use `import <module> as <name>;` to bind a module to a different name. Import cycles are
detected and reported with the full chain.

## Standard library

The standard library is just a set of modules that happen to live on the search path.
It is written in Cardamom, in `std/`:

| Module | Provides |
| --- | --- |
| `io` | `print`, `println`, `input` |
| `str` | `len`, `charAt`, `charCodeAt`, `fromASCII`, `fromInt`, `fromFloat`, `toInt`, `toFloat`, `substring`, `repeat`, `contains` |
| `math` | `abs`, `min`, `max`, `pow`, `sqrt` |

Adding a function means editing `std/<module>/main.crdm` — no compiler changes.

Only the functions a program actually calls are emitted, so importing a module costs
nothing for the parts you do not use.

The search path is, in order: the importing file's directory, `$CARDAMOM_STD`, `std/`
next to the compiler binary, and the source checkout.

### Intrinsics

Most of the library is ordinary Cardamom, but the leaves have to reach C++ eventually.
`@cpp` splices code into the generated function body and `@include` adds a header:

```cpp
public fn println(content: string) {
    @include("<iostream>");
    @cpp("std::cout << content << std::endl;");
}

// Built on top, in plain Cardamom:
public fn repeat(s: string, times: int) -> string {
    let out: string = "";
    let i: int = 0;
    while (i < times) {
        out += s;
        i += 1;
    }
    return out;
}
```

`fn extern name(..) -> T {}` remains available to declare a function you link yourself.

## Generics

Functions can take type parameters. Type arguments are inferred from the call, or given
explicitly:

```cpp
fn identity<T>(x: T) -> T {
    return x;
}

fn firstOr<T>(xs: T[], fallback: T) -> T {
    if (xs.len() > 0) {
        return xs[0];
    }
    return fallback;
}

fn main() {
    identity(5);            // T inferred as int
    identity("hello");      // a second instantiation
    identity<int>(7);       // explicit, reuses the first

    let xs: int[] = [1, 2];
    firstOr(xs, 0);
}
```

Generics are monomorphised: each distinct set of type arguments produces its own
specialised function, so the generated C++ contains no templates and type errors are
reported by Cardamom rather than by the C++ compiler.

Classes take type parameters too:

```cpp
public class Option<T>(private value: T, private present: int) {
    public unwrapOr(fallback: T) -> T {
        if (this.present == 1) {
            return this.value;
        }
        return fallback;
    }
}

fn some<T>(v: T) -> Option<T> {
    return new Option<T>(v, 1);
}

fn main() {
    let a: Option<int> = some(41);
    let b: Option<string> = new Option("", 0);   // type argument inferred

    a.unwrapOr(0);
    b.unwrapOr("empty");
}
```

Instantiation is transitive and only the instantiations a program actually uses are
emitted, so `Box<T>` used inside `wrap<T>` produces exactly the specialisations `wrap`
is called at.

Generic functions cross module boundaries; classes do not yet, since only functions are
exported.

## Functions as values

A named function can be used wherever a `fn` type is expected:

```cpp
fn twice(x: int) -> int { return x * 2; }

fn apply(f: fn(int) -> int, v: int) -> int { return f(v); }

fn main() {
    let f: fn(int) -> int = twice;
    let fs: (fn(int) -> int)[] = [twice, f];

    apply(twice, 5);
}
```

## References

`&T` is an immutable borrow and `&mut T` a mutable one. They lower to `const T&` and
`T&`:

```cpp
fn bump(x: &mut int) -> void {
    x += 1;              // visible to the caller
}

fn readonly(x: &int) -> int {
    return x + 1;        // reading only
}

fn main() {
    let n: int = 1;
    bump(n);             // n is now 2
    readonly(n);
    readonly(5);         // an immutable borrow accepts a temporary
}
```

The rules the checker enforces:

- assigning through a `&T` is an error; use `&mut T`
- a `&mut T` argument must be a variable, index or field, not a temporary
- a `&T` cannot be passed where a `&mut T` is required (the reverse is fine)
- members and indexing reach through a borrow, so `xs.len()` works for `xs: &int[]`

Class methods that never write to `this` are emitted as `const`, which is what lets
them be called through a `&T`.

## Notes

Empty array literals take their type from the context they appear in, so `let xs: int[] = [];`
and `total([])` both work; a literal with nothing to infer from is an error.

Names that are C++ keywords but not Cardamom keywords (`double`, `template`, `union`, ...)
are usable as ordinary identifiers and renamed during code generation.

I am currently developing this programming language as a hobby

## Building
```sh
cargo build --release
cp ./target/release/cardamom ./cardamom
```

## Usage
```sh
./cardamom <file>       # compile the file and generate ./output
./cardamom <file> -out  # compile the file and generate ./output and ./output.cpp
```