# Simplification Plan

Cardamom is being redirected toward fast CLI tools and data-processing programs.
The implementation should shrink to match that purpose.

## Keep In The V1 Core

These features directly support the new direction:

- modules and imports
- functions
- typed parameters and return values
- local variables
- `if` / `else`
- `while`
- C-style `for` loops until `for item in collection` exists
- arrays
- strings
- basic numeric and comparison operators
- C++ code generation
- extern declarations for the bootstrap standard library

## Add For The V1 Direction

These features should be added before expanding the language again:

- `main(args: [string]) -> int`
- local type inference for `let`
- `for item in collection`
- `Option<T>` and `Result<T, E>`
- `?` for option propagation and chaining
- `!` for result propagation and chaining
- string interpolation
- file, string, CLI, and conversion helpers in the standard library

## Postpone Or Remove From The Active Compiler

These features are not part of the CLI/data-processing core and currently add
large amounts of implementation complexity:

- classes
- visibility modifiers
- static members
- extensions
- constructors and `new`
- inheritance-oriented member access rules
- closures
- raw pointers
- raw references and mutable references
- generic functions beyond the minimal needs of `Option<T>` and `Result<T, E>`
- monomorphization scaffolding that is not used by the V1 examples

Some syntax may return later in a smaller form. For example, plain `struct`
records are more useful for data work than classes with visibility and methods.

## Recommended Removal Order

1. Freeze the V1 test baseline around functions, variables, arrays, comparisons,
   and `main(args: [string]) -> int`.
2. Mark non-V1 fixtures as legacy so they stop forcing the compiler to preserve
   old behavior.
3. Remove class and extension parsing before removing the associated typecheck
   and codegen paths.
4. Remove closure parsing and closure typechecking.
5. Remove raw pointer/reference expression support.
6. Replace broad generic support with focused generic instances for standard
   library types.
7. Revisit the AST and visitor traits after the old syntax is gone.

The safest approach is to delete one feature family at a time and keep tests
green after each cut.

## Current Branch State

The first simplification pass has been applied:

- class and extension declarations are rejected by the parser
- visibility and static modifiers are rejected by the parser
- constructors and closures are rejected by the parser
- generic function call syntax has been removed from the parser
- raw reference and dereference expression parsing has been removed
- class and extension registration passes have been removed from the typechecker
- class, static access, closure, reference, and pointer typechecking paths have
  been collapsed to unsupported-feature stubs
- the active pass/fail fixture lists now describe the V1 baseline

This pass also added two V1 syntax improvements:

- dotted imports, such as `import std.io as io;`
- local inference for initialized variables, such as `let count = 1;`
