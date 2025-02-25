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

fn run_file(filename: String) {
    let source = match read_to_string(filename.clone()) {
        Ok(source) => source,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    let mut l = lexer::Lexer::new(source.clone(), filename.clone());

    l.scan_tokens();

    if is_flag_set_str!("debug") {
        for token in l.tokens.iter() {
            println!("[DEBUG]{}", token.to_string());
        }
    }

    let mut p = parser::Parser::new(l.tokens.clone(), source.clone(), filename.clone());

    let module = p.parse();

    if is_flag_set_str!("debug") {
        if let Ok(module) = &module {
            let mut v = utils::astprint::AstPrinter::new();
            println!("[DEBUG]");
            module.accept(&mut v);
        }
    }

    let symtable = &mut SymbolTable::new();
    let mut tc = typecheck::TypeChecker::new(symtable, filename.clone(), source.clone());
    let mut cg = CppCodeGenerator::new();

    if let Ok(module) = &module {
        tc.check_module(module);
    
        let code = cg.generate(module);

        let mut output = File::create("output.cpp").unwrap();
        output.write_all(code.as_bytes()).unwrap();

        // compile the generated C code
        let output = std::process::Command::new("g++")
            .arg("output.cpp")
            .arg("-o")
            .arg("output")
            .output()
            .expect("Failed to compile the generated C++ code.");

        if !is_flag_set_str!("show output") {
            std::fs::remove_file("output.cpp").unwrap();
        }

        if output.status.success() {
            println!("Successfully compiled the generated C++ code.");
        } else {
            eprintln!("Failed to compile the generated C++ code.");
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        }
    }

    if l.had_error || p.had_error {
        println!("Program exited with {} error(s).", l.error_tokens.len() + p.errors);
    }
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
        run_file(filename);
    }
}