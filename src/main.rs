mod ast;
mod lexer;
mod token;
mod parser;
mod repl;
mod trace;
mod object;

fn main() {
    println!("Hello {}! This is the Monkey programming language!", whoami::username());
    println!("Feel free to type in commands");
    repl::start();
}
