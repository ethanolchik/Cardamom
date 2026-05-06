# Cardamom

Cardamom is a small statically typed language for writing fast command-line
tools and data-processing programs.

The compiler currently targets C++. That backend is a practical bootstrap
target: it keeps Cardamom native, gives the standard library access to proven
C++ facilities, and leaves room for another backend later once the language is
more stable.

## Design Goals

- Make common CLI and file-processing programs concise.
- Keep performance predictable by compiling ahead of time.
- Prefer explicit, simple types over dynamic runtime behavior.
- Build a useful standard library before adding large language features.

## Example Direction

```crdm
import fs;

fn main(args: [string]) -> int {
    let path = args.get(0) ?? "input.txt";
    let lines = fs.lines(path)?;

    for line in lines {
        if line.contains("ERROR") {
            print(line);
        }
    }

    return 0;
}
```

This is target syntax for the redesign. Some of it may not be implemented yet.

## Near-Term Scope

Core language features:

- functions
- variables
- conditionals
- loops
- arrays
- strings
- simple imports
- basic static type checking

Data-tooling features to add:

- `main(args: [string]) -> int`
- string interpolation
- `for item in collection`
- `Option<T>` for missing values
- `Result<T, E>` or `?` for recoverable errors
- file IO helpers
- string helpers such as `split`, `trim`, and `contains`

Features to postpone:

- classes as the main abstraction
- inheritance
- complex generics
- macros
- async
- garbage collection
- direct pointer-heavy programming

## Roadmap

See:

- [docs/cli-data-roadmap.md](docs/cli-data-roadmap.md)
- [docs/simplification-plan.md](docs/simplification-plan.md)
- [docs/v1-language-shape.md](docs/v1-language-shape.md)
