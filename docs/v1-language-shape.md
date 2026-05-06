# V1 Language Shape

This document sketches the first practical version of Cardamom after the
CLI/data-processing redesign. It is a target design, not a statement that every
example already compiles.

## Purpose

Cardamom V1 should make these programs straightforward:

- read command-line arguments
- read files as text or lines
- transform strings
- parse numbers
- filter data
- write results
- report recoverable errors clearly

## Program Entry

The preferred entry point is:

```crdm
fn main(args: [string]) -> int {
    return 0;
}
```

Returning `0` means success. Non-zero values mean failure, matching normal CLI
conventions.

## Imports

The target import style should be simple:

```crdm
import std.fs as fs;
import std.io as io;
```

The standard library should expose focused modules rather than one large global
namespace.

## Variables

Cardamom should support explicit types and local inference:

```crdm
let count: int = 0;
let name = "input.txt";
```

Inference should be local and obvious. Public APIs should keep explicit
parameter and return types.

## Core Types

Initial built-in types:

```text
int
float
bool
string
[T]
Option<T>
Result<T, E>
```

These are enough to build useful file-processing tools without forcing the
compiler to support a large object system.

## Loops

Collection iteration should be the normal way to process data:

```crdm
for line in fs.lines(path)! {
    io.println(line);
}
```

Index-based loops can still exist, but data tools should mostly use iterators.

## Strings

Strings need to be strong because most CLI tools spend their time handling text.

Target operations:

```crdm
line.contains("ERROR")
line.trim()
line.split(",")
line.starts_with("#")
line.ends_with(".txt")
```

String interpolation should use named expressions:

```crdm
io.println("processed {count} lines from {path}");
```

## Option

`Option<T>` represents absence.

```crdm
let first: Option<string> = args.get(0);
let path: string = args.get(0) ?? "input.txt";
```

`?.` chains through optional values:

```crdm
let name: Option<string> = user?.profile?.name;
```

Postfix `?` propagates `None` from the current function:

```crdm
fn first_arg(args: [string]) -> Option<string> {
    let arg = args.get(0)?;
    return some(arg);
}
```

## Result

`Result<T, E>` represents recoverable failure.

```crdm
let text: Result<string, FsError> = fs.read(path);
```

`!.` chains through successful values:

```crdm
let size: Result<int, FsError> = file!.metadata!.size;
```

Postfix `!` propagates `Err(e)` from the current function:

```crdm
fn load(path: string) -> Result<string, FsError> {
    let text = fs.read(path)!;
    return ok(text);
}
```

Forced unwrap should be explicit:

```crdm
let value = maybe_value.unwrap();
```

## Structs Before Classes

For data-processing work, records are more important than inheritance.

Prefer a small `struct` feature before expanding class support:

```crdm
struct User {
    name: string,
    email: string,
    active: bool,
}
```

Methods can come later if the plain data shape works well.

## Example Tool

```crdm
import std.fs as fs;
import std.io as io;

fn main(args: [string]) -> int {
    let path = args.get(0) ?? "input.log";

    for line in fs.lines(path)! {
        if line.contains("ERROR") {
            io.println(line);
        }
    }

    return 0;
}
```

The first implementation milestone should be making this kind of program feel
natural, even if the initial standard library is small.
