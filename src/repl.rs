// Std
use std::io;

// Internal
use crate::{
    lexer::Lexer,
    parser::Parser,
    ast::Node,
};

pub fn start() {
    loop {
        println!(">> ");
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failed to read line.");
        let l = Lexer::new(line);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if p.errors.len() != 0 {
            print_parse_errors(&p.errors);
            continue;
        }
        println!("{}", program.to_string());
    }
}

fn print_parse_errors(errors: &Vec<String>) {
    println!("Woops! parser errors:");
    for e in errors {
        println!("\t{}", e);
    }
}