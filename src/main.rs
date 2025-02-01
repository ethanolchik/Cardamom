pub mod lexer;
pub mod token;
pub mod errors;
pub mod parser;
pub mod ast;
pub mod ty;
pub mod utils;
pub mod typecheck;

use std::fs::read_to_string;
use std::env::args;

use crate::ast::Node;
use crate::utils::symtable::SymbolTable;

const PRINT_HELP: fn() -> () = || {
    println!("Usage: cardamom [options] <filename>");
    println!("Options:");
    println!("\t-d, --debug\tEnable debug mode");
    println!("\t-nc, --no-colour\tDisable coloured output");
    println!("\t-h, --help\tDisplay this help message");
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

    if let Ok(ref module) = module {
        tc.check_module(&module);
    }

    for error in tc.errors.iter() {
        println!("{}", error.to_string());
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