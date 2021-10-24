mod lexer;
mod token;

use crate::token::TokenType::*;
use crate::lexer::Lexer;

fn main() {
    println!("Hello, world! {}", EOF);
    let mut l = Lexer::new("=".to_string());
    l.next_token();
}
