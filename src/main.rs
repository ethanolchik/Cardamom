pub mod ast;
mod cli;
pub mod codegen;
pub mod errors;
pub mod lexer;
pub mod modules;
pub mod operators;
pub mod parser;
pub mod reachable;
pub mod token;
pub mod ty;
pub mod typecheck;
pub mod utils;

use std::collections::HashMap;
use std::ffi::OsString;
use std::fs;
use std::process::Command;

use crate::ast::Node;
use crate::codegen::CppCodeGenerator;
use crate::typecheck::ModuleExports;
use crate::utils::symtable::SymbolTable;

fn run_file(options: cli::Options) -> bool {
    // Load the program and everything it imports, in dependency order.
    let program = match modules::load(&options.input) {
        Ok(program) => program,
        Err(errors) => {
            for error in &errors {
                eprintln!("{}", error.to_string());
            }
            eprintln!("Program exited with {} error(s).", errors.len());
            return false;
        }
    };

    if is_flag_set_str!("debug") {
        for module in &program.modules {
            println!(
                "[DEBUG] loaded module `{}` from {}",
                module.name,
                module.path.display()
            );
        }
    }

    if is_flag_set_str!("ast") {
        for module in &program.modules {
            println!("[AST] module `{}`", module.name);
            let mut printer = utils::astprint::AstPrinter::new();
            module.ast.accept(&mut printer);
        }
    }

    // Type check each module in turn. Because they are in dependency order, everything
    // a module imports has already been checked and its exports recorded.
    let mut exports: HashMap<String, ModuleExports> = HashMap::new();
    let mut expr_types = codegen::ExprTypes::new();
    let mut call_instantiations = codegen::CallInstantiations::new();
    let mut trait_call_sites = codegen::TraitCallSites::new();
    let mut instantiations = typecheck::Instantiations::new();
    let mut generic_call_sites: Vec<typecheck::GenericCallSite> = Vec::new();
    let mut function_generics = typecheck::FunctionGenerics::new();
    let mut error_count = 0;

    let root_name = program.root().name.clone();

    for module in &program.modules {
        let symtable = &mut SymbolTable::new();
        let mut tc = typecheck::TypeChecker::new(
            symtable,
            module.path.to_string_lossy().to_string(),
            module.source.clone(),
        );
        tc.set_module_exports(exports.clone());
        tc.set_module_name(module.name.clone());
        tc.set_is_library(module.name != root_name);
        tc.check_module(&module.ast);

        if tc.has_errors() {
            tc.emit_errors();
            error_count += tc.error_count();
        }

        exports.insert(module.name.clone(), tc.exports(&module.ast));
        // Expression types are keyed by AST node address, and every module's AST is
        // kept alive by `program`, so the maps can simply be merged.
        expr_types.extend(tc.expr_types.clone());
        call_instantiations.extend(tc.call_instantiations.clone());
        trait_call_sites.extend(tc.trait_call_sites.clone());
        generic_call_sites.extend(tc.generic_call_sites.clone());

        for (module_name, functions) in tc.instantiations.clone() {
            instantiations
                .entry(module_name)
                .or_default()
                .extend(functions);
        }
        for (module_name, functions) in tc.function_generics.clone() {
            function_generics
                .entry(module_name)
                .or_default()
                .extend(functions);
        }
    }

    // Instantiation is transitive and can cross module boundaries, so the set is
    // closed once every module has been checked.
    typecheck::expand_instantiations(&mut instantiations, &generic_call_sites, &function_generics);

    if error_count > 0 {
        eprintln!("Program exited with {} error(s).", error_count);
        return false;
    }

    let mut cg = CppCodeGenerator::with_types(expr_types);
    cg.set_instantiations(call_instantiations, instantiations);
    cg.set_trait_call_sites(trait_call_sites);
    let code = cg.generate_program(&program);

    match compile_cpp(&options, &program, &code) {
        Ok(()) => true,
        Err(error) => {
            eprintln!("{error}");
            false
        }
    }
}

fn compile_cpp(
    options: &cli::Options,
    program: &modules::Program,
    code: &str,
) -> Result<(), String> {
    // Absolute paths cannot be mistaken for compiler flags, even if the filename
    // begins with a dash. Append rather than replace an extension: app.exe.cpp.
    let cwd =
        std::env::current_dir().map_err(|e| format!("Could not read working directory: {e}"))?;
    let binary = cwd.join(&options.output);
    let mut cpp_name = binary.as_os_str().to_os_string();
    cpp_name.push(".cpp");
    let cpp = std::path::PathBuf::from(cpp_name);

    // A mistyped -o must never overwrite an input or one of its imported modules.
    for path in [&binary, &cpp] {
        if let Ok(existing) = fs::canonicalize(path) {
            if program
                .modules
                .iter()
                .any(|module| fs::canonicalize(&module.path).ok().as_ref() == Some(&existing))
            {
                return Err(format!(
                    "Refusing to overwrite source file `{}`.",
                    path.display()
                ));
            }
        }
    }

    fs::write(&cpp, code).map_err(|e| format!("Could not write `{}`: {e}", cpp.display()))?;

    let cxx = options
        .cxx
        .clone()
        .or_else(|| std::env::var_os("CXX"))
        .unwrap_or_else(|| OsString::from("g++"));
    let mut command = Command::new(&cxx);
    command
        .arg("-std=c++17")
        .arg(&cpp)
        // Libraries follow the translation unit, as required by static linkers.
        .args(&options.cxx_args)
        .arg("-o")
        .arg(&binary);
    if is_flag_set_str!("debug") {
        println!("[DEBUG] {command:?}");
    }

    let result = command.output().map_err(|e| {
        format!(
            "Could not run C++ compiler `{}`: {e}. Use --cxx to select one.\nGenerated C++: {}",
            cxx.to_string_lossy(),
            cpp.display()
        )
    })?;
    if !result.status.success() {
        return Err(format!(
            "C++ compilation failed ({}):\n{}\nGenerated C++: {}",
            result.status,
            String::from_utf8_lossy(&result.stderr),
            cpp.display()
        ));
    }
    if !is_flag_set_str!("show output") {
        let _ = fs::remove_file(&cpp);
    }
    print!("{}", String::from_utf8_lossy(&result.stdout));
    eprint!("{}", String::from_utf8_lossy(&result.stderr));
    println!("Compiled {}", options.output.display());
    Ok(())
}

fn main() {
    match cli::parse(std::env::args_os().skip(1)) {
        Ok(cli::Invocation::Help) => println!("{}", cli::HELP),
        Ok(cli::Invocation::Compile(options)) => {
            // The command line is parsed once, before any compiler work begins.
            unsafe {
                utils::flags::FLAGS.0 = options.flags;
            }
            if !run_file(options) {
                std::process::exit(1);
            }
        }
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}
