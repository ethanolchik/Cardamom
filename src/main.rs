pub mod lexer;
pub mod token;
pub mod errors;
pub mod parser;
pub mod ast;
pub mod ty;
pub mod utils;
pub mod typecheck;
pub mod codegen;
pub mod modules;
pub mod reachable;

use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use crate::ast::Node;
use crate::codegen::CppCodeGenerator;
use crate::typecheck::ModuleExports;
use crate::utils::symtable::SymbolTable;

const PRINT_HELP: fn() -> () = || {
    println!("Usage: cardamom [options] <filename>");
    println!("Options:");
    println!("\t-d, --debug\tEnable debug mode");
    println!("\t-nc, --no-colour\tDisable coloured output");
    println!("\t-h, --help\tDisplay this help message");
    println!("\t-ast\tDisplay the AST");
    println!("\t-out\tDisplay the output of the generated C code");
};

fn run_file(filename: String) -> bool {
    // Load the program and everything it imports, in dependency order.
    let program = match modules::load(Path::new(&filename)) {
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
            println!("[DEBUG] loaded module `{}` from {}", module.name, module.path.display());
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
    }

    if error_count > 0 {
        eprintln!("Program exited with {} error(s).", error_count);
        return false;
    }

    let mut cg = CppCodeGenerator::with_types(expr_types);
    let code = cg.generate_program(&program);

    let mut output = File::create("output.cpp").unwrap();
    output.write_all(code.as_bytes()).unwrap();

    let result = std::process::Command::new("g++")
        .arg("output.cpp")
        .arg("-o")
        .arg("output")
        .output()
        .expect("Failed to compile the generated C++ code.");

    if !is_flag_set_str!("show output") {
        let _ = std::fs::remove_file("output.cpp");
    }

    if result.status.success() {
        println!("Successfully compiled the generated C++ code.");
    } else {
        eprintln!("Failed to compile the generated C++ code.");
        eprintln!("{}", String::from_utf8_lossy(&result.stderr));
        return false;
    }

    true
}

fn main() {
    let mut filename: Option<String> = None;
    let args: Vec<String> = args().collect();

    for arg in args.iter() {
        match arg.as_str() {
            "-d" | "--debug" => set_flag_str!("debug"),
            "-nc" | "--no-colour" => set_flag_str!("no colour"),
            "-ast" => set_flag_str!("ast"),
            "-out" => set_flag_str!("show output"),
            "-h" | "--help" => {
                PRINT_HELP();
                return;
            }
            _ => {
                filename = Some(arg.clone());
            }
        }
    }

    if let Some(filename) = filename {
        if !run_file(filename) {
            std::process::exit(1);
        }
    }
}
