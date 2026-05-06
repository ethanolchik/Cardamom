# CLI And Data Processing Roadmap

This branch redirects Cardamom toward fast command-line tools and practical
data-processing programs while keeping the existing C++ backend.

## 1. Define The Core Identity

Cardamom should optimise for small native programs that read, transform, and
write data.

V1 should include:

- functions
- variables
- arrays
- strings
- loops
- conditionals
- imports
- simple errors
- file IO
- command-line arguments

V1 should not center:

- inheritance
- complex generics
- garbage collection
- async runtimes
- macro systems
- low-level pointer programming

## 2. Simplify The Surface Language

The everyday syntax should make small tools direct to write:

```crdm
import fs;

fn main(args: [string]) -> int {
    let path = args.get(0) ?? "input.txt";

    for line in fs.lines(path)? {
        if line.contains("ERROR") {
            print(line);
        }
    }

    return 0;
}
```

Priority syntax changes:

- `main(args: [string]) -> int`
- `for item in collection`
- string interpolation with named values
- postfix `?` for propagating recoverable errors
- `??` for option fallback

### Option And Result Operators

Cardamom should distinguish absence from failure:

- `?` is for `Option<T>`, where the missing case is `None`
- `!` is for `Result<T, E>`, where the failure case is `Err(E)`

Chaining should preserve the wrapper instead of panicking:

```crdm
let name: Option<string> = user?.profile?.name;
let size: Result<int, FsError> = file!.metadata!.size;
```

For `Option<T>`:

- `Some(x)?.field` evaluates to `Some(x.field)`
- `None?.field` evaluates to `None`

For `Result<T, E>`:

- `Ok(x)!.field` evaluates to `Ok(x.field)`
- `Err(e)!.field` evaluates to `Err(e)`

Postfix propagation should return from the current function:

```crdm
fn first_arg(args: [string]) -> Option<string> {
    let arg = args.get(0)?;
    return some(arg);
}

fn load(path: string) -> Result<string, FsError> {
    let text = fs.read(path)!;
    return ok(text);
}
```

Forced unwrap should be explicit through methods such as `.unwrap()`. The `?`
and `!` operators should not panic silently.

## 3. Build A Practical Standard Library

The standard library should come before large language features because it
defines whether Cardamom is useful for its target purpose.

Initial modules:

- `std.io`: `print`, `println`, `eprint`, `eprintln`
- `std.fs`: `read`, `write`, `lines`, `exists`
- `std.str`: `split`, `trim`, `contains`, `starts_with`, `ends_with`
- `std.cli`: arguments and basic flag parsing
- `std.conv`: `parse_int`, `parse_float`, `to_string`

File APIs should support streaming so large inputs do not need to be loaded
into memory.

## 4. Keep The Type System Focused

The initial type system should be small and predictable.

Core types:

- `int`
- `float`
- `bool`
- `string`
- `[T]`
- `Option<T>`
- `Result<T, E>`

Likely follow-up types:

- `Map<K, V>`
- `struct`
- tuples
- iterators

References, pointers, classes, and closures can exist experimentally, but they
should not drive the main user-facing design until the data-tool workflow is
solid.

## 5. Validate With Real Tools

Each compiler change should be tested against small practical programs.

Target examples:

- `wc.crdm`: count lines, words, and characters
- `grep.crdm`: print lines containing a substring
- `head.crdm`: print the first N lines
- `csv_filter.crdm`: filter rows by column value
- `log_errors.crdm`: extract error lines from logs

The branch should be considered successful when these examples feel natural,
compile reliably, and produce straightforward C++.
