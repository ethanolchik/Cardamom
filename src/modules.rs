//! Module resolution and loading.
//!
//! `import io;` resolves to a Cardamom source file, which is parsed and compiled
//! alongside the program that imported it. The same mechanism serves the standard
//! library and a user's own files: the standard library is simply a set of modules that
//! happen to live on the standard library search path.
//!
//! A module named `foo` is looked for as `foo.crdm` or `foo/main.crdm`, first next to
//! the importing file (so local modules win) and then on the standard library path.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::{Module, Stmt};
use crate::errors::{Error, Note};
use crate::lexer::Lexer;
use crate::parser::Parser;

/// A module that has been found, read and parsed.
pub struct LoadedModule {
    /// The name used to import it, e.g. `io`. The program being compiled is `main`.
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub ast: Module,
    /// `alias -> module name` for everything this module imports.
    pub imports: HashMap<String, String>,
}

/// Every module making up one compilation, in dependency order: a module always appears
/// after everything it imports, and the program being compiled is last.
pub struct Program {
    pub modules: Vec<LoadedModule>,
}

impl Program {
    /// The module being compiled.
    pub fn root(&self) -> &LoadedModule {
        self.modules
            .last()
            .expect("a program always contains at least its root module")
    }
}

/// Directories searched for standard library modules, most specific first.
fn std_search_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // An explicit override always wins, which is what tests and installs use.
    if let Ok(dir) = std::env::var("CARDAMOM_STD") {
        paths.push(PathBuf::from(dir));
    }

    // Next to the compiler binary, which is how an installed copy finds its library.
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            paths.push(dir.join("std"));
            // `target/debug/cardamom` -> repository root, for running from a build tree.
            if let Some(root) = dir.parent().and_then(|p| p.parent()) {
                paths.push(root.join("std"));
            }
        }
    }

    // Finally the source checkout, so `cargo run` works with no setup.
    paths.push(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std"));

    paths
}

/// The directories a module's own imports are resolved against, before the standard
/// library. For `foo/main.crdm` this includes `foo/`'s parent, so that a module laid out
/// as a directory can still import its siblings.
fn local_roots(module_path: &Path) -> Vec<PathBuf> {
    let mut roots = Vec::new();

    let Some(dir) = module_path.parent() else {
        return vec![PathBuf::from(".")];
    };

    roots.push(dir.to_path_buf());

    if module_path.file_name().and_then(|f| f.to_str()) == Some("main.crdm") {
        if let Some(parent) = dir.parent() {
            roots.push(parent.to_path_buf());
        }
    }

    roots
}

/// Resolves a module name to a file, searching `local_dirs` before the standard library.
fn resolve(name: &str, local_dirs: &[PathBuf]) -> Option<PathBuf> {
    let mut roots = local_dirs.to_vec();
    roots.extend(std_search_paths());

    for root in roots {
        // `foo/main.crdm` is preferred so a module can grow into a directory without
        // its importers having to change.
        let as_directory = root.join(name).join("main.crdm");
        if as_directory.is_file() {
            return Some(as_directory);
        }

        let as_file = root.join(format!("{}.crdm", name));
        if as_file.is_file() {
            return Some(as_file);
        }
    }

    None
}

fn available_modules() -> Vec<String> {
    let mut names = HashSet::new();

    for root in std_search_paths() {
        let Ok(entries) = std::fs::read_dir(&root) else {
            continue;
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() && path.join("main.crdm").is_file() {
                if let Some(name) = path.file_name() {
                    names.insert(name.to_string_lossy().to_string());
                }
            } else if path.extension().and_then(|e| e.to_str()) == Some("crdm") {
                if let Some(name) = path.file_stem() {
                    names.insert(name.to_string_lossy().to_string());
                }
            }
        }
    }

    let mut names: Vec<String> = names.into_iter().collect();
    names.sort();
    names
}

/// Reads and parses one file into a `LoadedModule`, without following its imports.
fn parse_module(name: &str, path: &Path) -> Result<LoadedModule, Vec<Error>> {
    let filename = path.to_string_lossy().to_string();

    let source = match std::fs::read_to_string(path) {
        Ok(source) => source,
        Err(e) => {
            return Err(vec![Error::new(
                format!("Could not read module `{}`: {}", name, e),
                0,
                crate::token::Span::new(0, 0),
                filename,
            )]);
        }
    };

    let mut lexer = Lexer::new(source.clone(), filename.clone());
    lexer.scan_tokens();
    if lexer.had_error {
        // The lexer has already reported its own errors.
        return Err(Vec::new());
    }

    let mut parser = Parser::new(lexer.tokens.clone(), source.clone(), filename.clone());
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(error) => return Err(vec![error]),
    };

    if parser.had_error {
        return Err(Vec::new());
    }

    let mut imports = HashMap::new();
    for stmt in &ast.statements {
        if let Stmt::Import { name, alias } = &**stmt {
            imports.insert(alias.lexeme.clone(), name.lexeme.clone());
        }
    }

    Ok(LoadedModule {
        name: name.to_string(),
        path: path.to_path_buf(),
        source,
        ast,
        imports,
    })
}

/// Loads `entry` and everything it imports, transitively.
///
/// The returned modules are in dependency order, so each can be type checked and
/// generated knowing that everything it depends on has already been processed.
pub fn load(entry: &Path) -> Result<Program, Vec<Error>> {
    let mut modules: Vec<LoadedModule> = Vec::new();
    let mut loaded: HashSet<String> = HashSet::new();
    let mut errors: Vec<Error> = Vec::new();

    // `visiting` is the current import chain, used to detect and report cycles.
    let mut visiting: Vec<String> = Vec::new();

    fn visit(
        name: &str,
        path: &Path,
        modules: &mut Vec<LoadedModule>,
        loaded: &mut HashSet<String>,
        visiting: &mut Vec<String>,
        errors: &mut Vec<Error>,
    ) {
        if loaded.contains(name) {
            return;
        }

        let module = match parse_module(name, path) {
            Ok(module) => module,
            Err(mut e) => {
                errors.append(&mut e);
                // Mark as loaded so one broken module does not cascade.
                loaded.insert(name.to_string());
                return;
            }
        };

        visiting.push(name.to_string());

        let local_dirs = local_roots(&module.path);

        for stmt in &module.ast.statements {
            let Stmt::Import { name: import_name, .. } = &**stmt else {
                continue;
            };

            let imported = &import_name.lexeme;

            if visiting.iter().any(|m| m == imported) {
                let chain = visiting
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(" -> ");

                let mut error = Error::new(
                    format!("Import cycle: {} -> {}", chain, imported),
                    import_name.line,
                    import_name.span.clone(),
                    module.path.to_string_lossy().to_string(),
                );
                error.add_source(module.source.clone());
                errors.push(error);
                continue;
            }

            match resolve(imported, &local_dirs) {
                Some(resolved) => {
                    visit(imported, &resolved, modules, loaded, visiting, errors);
                }
                None => {
                    let mut error = Error::new(
                        format!("Cannot find module `{}`", imported),
                        import_name.line,
                        import_name.span.clone(),
                        module.path.to_string_lossy().to_string(),
                    );
                    error.add_source(module.source.clone());
                    error.add_note(Note::new(
                        format!("Available modules: {}", available_modules().join(", ")),
                        import_name.line,
                        import_name.span.clone(),
                        module.path.to_string_lossy().to_string(),
                    ));
                    errors.push(error);
                }
            }
        }

        visiting.pop();

        // Pushed after its dependencies, giving dependency order overall.
        loaded.insert(name.to_string());
        modules.push(module);
    }

    visit(
        "main",
        entry,
        &mut modules,
        &mut loaded,
        &mut visiting,
        &mut errors,
    );

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(Program { modules })
}
