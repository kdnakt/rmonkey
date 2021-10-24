use std::fmt;
use std::result;
use std::cmp::PartialEq;
use std::collections::HashMap;
use crate::token::TokenType::*;

pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL, // "ILLEGAL"
    EOF,     // "EOF"

    IDENT, // "IDENT"
    INT,   // "INT"

    ASSIGN, // "="
    PLUS,   // "+"

    COMMA,     // ","
    SEMICOLON, // ";"

    LPAREN, // "("
    RPAREN, // ")"
    LBRACE, // "{"
    RBRACE, // "}"

    FUNCTION, // "FUNCTION"
    LET,      // "LET"
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> result::Result<(), fmt::Error> {
        let t = match self {
            ILLEGAL => "ILLEGAL",
            EOF => "EOF",
            IDENT => "IDENT",
            INT => "INT",
            ASSIGN => "=",
            PLUS => "+",
            COMMA => ",",
            SEMICOLON => ";",
            LPAREN => "(",
            RPAREN => ")",
            LBRACE => "{",
            RBRACE => "}",
            FUNCTION => "FUNCTION",
            LET => "LET",
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