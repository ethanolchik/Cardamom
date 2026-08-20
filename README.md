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