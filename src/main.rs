pub mod lexer;
pub mod token;
pub mod errors;
pub mod parser;
pub mod ast;
pub mod ty;
pub mod utils;

use std::fs::read_to_string;
use std::env::args;
use crate::ast::Node;

fn main() {
    let args: Vec<String> = args().collect();
    let filename = &args[1];

    let source = match read_to_string(filename) {
        Ok(source) => source,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    let mut l = lexer::Lexer::new(source.clone(), filename.clone());

    l.scan_tokens();

    let mut p = parser::Parser::new(l.tokens.clone(), source.clone(), filename.clone());

    let module = p.parse();

    if let Ok(module) = module {
        let mut v = utils::astprint::AstPrinter::new();
        module.accept(&mut v);
    }

    if l.had_error || p.had_error {
        println!("Program exited with {} error(s).", l.error_tokens.len() + p.errors);
    }
}
