// Std
use std::{
    fmt,
    result,
    cmp::PartialEq,
    collections::HashMap,
};

// Internal
use crate::{
    token::TokenType::*,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}

// #[derive(Copy, Clone)]
// pub struct TokenWrapper<'a> {
//     pub token_ref: &'a Token,
// }

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL, // "ILLEGAL"
    EOF,     // "EOF"

    IDENT, // "IDENT"
    INT,   // "INT"

    ASSIGN, // "="
    PLUS,   // "+"
    MINUS,  // "-"
    BANG,   // "!"
    ASTERISK, // "*"
    SLASH,  // "/"

    EQ,    // "=="
    NOTEQ, // "!="
    LT,    // "<"
    GT,    // ">"

    COMMA,     // ","
    SEMICOLON, // ";"

    LPAREN, // "("
    RPAREN, // ")"
    LBRACE, // "{"
    RBRACE, // "}"

    FUNCTION, // "FUNCTION"
    LET,      // "LET"
    IF,       // "IF"
    ELSE,     // "ELSE"
    TRUE,     // "TRUE"
    FALSE,    // "FALSE"
    RETURN,   // "RETURN"
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> result::Result<(), fmt::Error> {
        let t = match self {
            ILLEGAL => "ILLEGAL",
            EOF => "EOF",
            IDENT => "IDENT",
            INT => "INT",
            ASSIGN => "=",
            MINUS => "-",
            BANG => "!",
            ASTERISK => "*",
            SLASH => "/",
            EQ => "==",
            NOTEQ => "!=",
            LT => "<",
            GT => ">",
            PLUS => "+",
            COMMA => ",",
            SEMICOLON => ";",
            LPAREN => "(",
            RPAREN => ")",
            LBRACE => "{",
            RBRACE => "}",
            FUNCTION => "FUNCTION",
            LET => "LET",
            IF => "IF",
            ELSE => "ELSE",
            TRUE => "TRUE",
            FALSE => "FALSE",
            RETURN => "RETURN",
        };
        write!(f, "{}", t)
    }
}

pub fn lookup_ident(ident: &String) -> TokenType {
    let map = keywords();
    let keyword = map.get(ident);
    return  match keyword {
        None => IDENT,
        Some(k) => *k,
    }
}

fn keywords() -> HashMap<String, TokenType> {
    let mut keywords: HashMap<String, TokenType> = HashMap::new();
    keywords.insert(String::from("fn"), FUNCTION);
    keywords.insert(String::from("let"), LET);
    keywords.insert(String::from("if"), IF);
    keywords.insert(String::from("else"), ELSE);
    keywords.insert(String::from("true"), TRUE);
    keywords.insert(String::from("false"), FALSE);
    keywords.insert(String::from("return"), RETURN);
    keywords
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    #[test]
    fn it_displays_token() {
        for &(tok, lit) in [
            (ILLEGAL, "ILLEGAL"),
            (EOF, "EOF"),
            (IDENT, "IDENT"),
            (INT, "INT"),
            (ASSIGN, "="),
            (PLUS, "+"),
            (COMMA, ","),
            (SEMICOLON, ";"),
            (LPAREN, "("),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RBRACE, "}"),
            (FUNCTION, "FUNCTION"),
            (LET, "LET"),
        ].iter() {
            assert_eq!(format!("{}", tok), lit);
        }
    }
}