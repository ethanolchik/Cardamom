pub mod lexer;
pub mod token;
pub mod errors;
pub mod parser;
pub mod ast;
pub mod ty;
pub mod utils;
pub mod typecheck;
pub mod codegen;

use std::fs::{read_to_string, File};
use std::env::args;
use std::io::Write;

use crate::ast::Node;
use crate::utils::symtable::SymbolTable;

use crate::codegen::CppCodeGenerator;

const PRINT_HELP: fn() -> () = || {
    println!("Usage: cardamom [options] <filename>");
    println!("Options:");
    println!("\t-d, --debug\tEnable debug mode");
    println!("\t-nc, --no-colour\tDisable coloured output");
    println!("\t-h, --help\tDisplay this help message");
    println!("\t-ast\tDisplay the AST");
    println!("\t-out\tDisplay the output of the generated C code");
};

fn parse_module(source: String, filename: String) -> Result<ast::Module, String> {
    let mut l = lexer::Lexer::new(source.clone(), filename.clone());
    l.scan_tokens();
    if l.had_error {
        return Err(format!(
            "Program exited with {} error(s) while lexing {}.",
            l.error_tokens.len(),
            filename
        ));
    }

    let mut p = parser::Parser::new(l.tokens.clone(), source.clone(), filename.clone());
    let module = p.parse();
    if p.had_error {
        return Err(format!("Program exited with {} error(s) while parsing {}.", p.errors, filename));
    }

    module.map_err(|err| err.to_string())
}

fn load_core_prelude() -> Result<ast::Module, String> {
    let mut statements = Vec::new();

    for core_file in ["option.crdm", "result.crdm"] {
        let core_path = format!("{}/core/{}", env!("CARGO_MANIFEST_DIR"), core_file);
        let source = read_to_string(&core_path)
            .map_err(|e| format!("Error reading core prelude `{}`: {}", core_path, e))?;
        let mut module = parse_module(source, core_path)?;
        statements.append(&mut module.statements);
    }

    Ok(ast::Module { statements })
}

fn run_file(filename: String) -> bool {
    let source = match read_to_string(filename.clone()) {
        Ok(source) => source,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return false;
        }
    };

    let mut module = match parse_module(source.clone(), filename.clone()) {
        Ok(module) => module,
        Err(err) => {
            eprintln!("{}", err);
            return false;
        }
    };

    let core = match load_core_prelude() {
        Ok(module) => module,
        Err(err) => {
            eprintln!("{}", err);
            return false;
        }
    };

    if is_flag_set_str!("debug") {
        let mut v = utils::astprint::AstPrinter::new();
        println!("[DEBUG] core");
        core.accept(&mut v);
    }

    let mut all_statements = core.statements;
    all_statements.extend(module.statements.drain(..));
    module.statements = all_statements;

    if is_flag_set_str!("debug") {
        let mut v = utils::astprint::AstPrinter::new();
        println!("[DEBUG] combined");
        module.accept(&mut v);
    }

    let symtable = &mut SymbolTable::new();
    let mut tc = typecheck::TypeChecker::new(symtable, filename.clone(), source.clone());
    tc.check_module(&module);

    if tc.has_errors() {
        tc.emit_errors();
        eprintln!("Program exited with {} error(s).", tc.error_count());
        return false;
    }

    let mut cg = CppCodeGenerator::with_variable_types(tc.variable_types.clone());
    let code = cg.generate(&module);

    let mut output = File::create("output.cpp").unwrap();
    output.write_all(code.as_bytes()).unwrap();

    let output = std::process::Command::new("g++")
        .arg("output.cpp")
        .arg("-o")
        .arg("output")
        .output()
        .expect("Failed to compile the generated C++ code.");

    if !is_flag_set_str!("show output") {
        let _ = std::fs::remove_file("output.cpp");
    }

    if output.status.success() {
        println!("Successfully compiled the generated C++ code.");
    } else {
        eprintln!("Failed to compile the generated C++ code.");
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
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
