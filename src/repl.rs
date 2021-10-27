// Std
use std::io;

// Internal
use crate::{
    lexer::Lexer,
    token::TokenType::*,
};

pub fn start() {
    loop {
        println!(">> ");
        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failed to read line.");
        let mut l = Lexer::new(line);

        let mut tok = l.next_token();
        while tok.typ != EOF {
            println!("{:?}", tok);
            tok = l.next_token();
        }
    }
}