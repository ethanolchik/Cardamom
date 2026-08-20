# Cardamom
Another programming language implementation (hopefully this time will be better).


## Example code
```cpp
import "std.io" as io;

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

> Note: `import`, `Option` and `??` in the example above are parsed but not yet
> implemented end to end.

## Class members

Visibility (`public`/`private`/`protected`) and `static` are independent properties, so
members are annotated individually rather than grouped into `public:` style sections.
That is what makes combinations like `private static` expressible:

```cpp
public class Registry(private name: string) {
    private static instances: int = 0;  // private *and* static
    public static limit: int = 10;

    public static describe() -> string {
        return "registry";
    }
}
```

Modifiers may appear in either order (`public static` or `static public`), and a member
with no visibility modifier is `private`.

## Array types

Array types are written postfix, and repeat for extra dimensions:

```cpp
let a: int[] = [1, 2, 3];
let b: int[][] = [[1, 2], [3, 4]];
let s: string[] = ["x", "y"];
```

A postfix `[]` binds tighter than the prefix modifiers `&`, `#` and `*`, so `&int[]` is a
reference to an array. Parenthesise to group the other way:

```cpp
let a: &int[];              // reference to an array of int
let b: (&int)[];            // array of references to int
let f: (fn(int) -> int)[];  // array of functions
```

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