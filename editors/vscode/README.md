# Cardamom for VS Code and Cursor

Syntax highlighting and editing support for `.crdm` files. The extension uses your
editor's colour theme and runs without a language server.

- Keywords, functions, methods, types, generics, references, and closures.
- Strings, comments, numbers, operators, and C++ inside `@cpp(...)`.
- Comment toggling, bracket matching, automatic pairs, and brace indentation.
- Snippets for functions, classes, imports, and native intrinsics.

## Install

From this directory, with Node.js 20 or newer:

```sh
npm ci --ignore-scripts
npm test
npm run package
code --install-extension cardamom-0.1.0.vsix
```

For Cursor, use `cursor --install-extension cardamom-0.1.0.vsix`. If the editor's
command is not on your PATH, open the Command Palette, run **Extensions: Install
from VSIX...**, and select the generated file. VS Code 1.85 or newer is required.

Open a `.crdm` file; its language mode should be **Cardamom**. If you previously
associated these files with another language, change that association to `cardamom`
in your editor settings. Reload the editor window if an already-open file keeps
its old highlighting.

## Snippets

Type a prefix and select the snippet from the completion menu. Tab moves between
placeholders.

| Prefix | Inserts |
| --- | --- |
| `main` | Program entry point |
| `fn` | Function declaration |
| `fn-generic` | Generic identity function |
| `class` | Class with a constructor field and getter |
| `import` | Module import |
| `cpp` | Embedded C++ statement |
| `include` | Native header include |

## Language details

The grammar follows Cardamom's lexer: comments start with `//`; both single and
double quotes delimit strings, which may span lines. Backslashes do not escape a
closing quote. C++ keywords such as `template` remain ordinary identifiers outside
`@cpp(...)`.

Selective imports such as `import math.{sin, sqrt as root};` highlight module names,
members, and aliases, including multiline lists. Use the `import-members` snippet
to insert one.

Embedded C++ uses the editor's built-in C++ grammar. Its state is reset on each
line and at the closing Cardamom quote, so incomplete C++ cannot colour the rest
of the Cardamom file. Multiline C++ block comments and raw strings may therefore
have limited highlighting. Use the other outer quote style when the C++ contains
quoted text, for example:

```text
@cpp('std::cout << "Hello" << std::endl;');
```

Type colours are based on declaration patterns, primitive names, and the
capitalised type-name convention. This extension provides syntax highlighting
and snippets; compiler diagnostics and symbol navigation would require language
server support.

## Development

Open `editors/vscode` as a workspace and press **F5** to launch an Extension
Development Host with the showcase file. Use **Developer: Inspect Editor Tokens
and Scopes** to inspect the grammar's scopes.

`npm test` runs the grammar through VS Code's TextMate tokenizer and Oniguruma
engine. It checks language boundaries and tokenizes the repository's standard
library, test programs, and showcase. When VS Code or Cursor is installed in a
standard macOS or Linux location, the tests also load its C++ grammar. Set
`CARDAMOM_CPP_GRAMMAR` to a `cpp.tmLanguage.json` path for another installation.
Without one, embedding boundaries use a small test grammar and the editor C++
integration test is skipped.

The bounded C++ captures have a following `source.cpp` include because TextMate's
dependency discovery does not inspect captures. Keep this include when editing
the grammar, or C++ tokenization can silently disappear in a fresh editor session.

The VSIX contains only the extension metadata, grammar, language configuration,
snippets, documentation, and GPL-3.0 license. Development dependencies and tests
are excluded.
