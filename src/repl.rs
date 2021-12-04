// Std
use std::io;

// Internal
use crate::{
    lexer::Lexer,
    parser::Parser,
    evaluator::eval,
    object::new_environment,
};

pub fn start() {
    let env = new_environment();
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

        if let Some(evaluated) = eval(program, &env) {
            println!("{}", evaluated.inspect());
        }
    }
}

fn print_parse_errors(errors: &Vec<String>) {
    println!("Woops! parser errors:");
    for e in errors {
        println!("\t{}", e);
    }
}