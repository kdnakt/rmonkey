use crate::token;
use crate::token::TokenType;
use crate::token::TokenType::*;
use crate::token::Token;
use std::convert::TryInto;

pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

fn new_token(typ: TokenType, ch: char) -> Token {
    Token { typ: typ, literal: ch.to_string() }
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer { input: input, pos: 0, read_pos: 0, ch: 0 };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let ch = self.ch as char;
        let tok = match self.ch {
            0 => Token { typ: EOF, literal: "".to_string() },
            _ => match ch {
                '=' => new_token(ASSIGN, ch),
                ';' => new_token(SEMICOLON, ch),
                '(' => new_token(LPAREN, ch),
                ')' => new_token(RPAREN, ch),
                ',' => new_token(COMMA, ch),
                '+' => new_token(PLUS, ch),
                '{' => new_token(LBRACE, ch),
                '}' => new_token(RBRACE, ch),
                _ => {
                    if is_letter(ch) {
                        let ident = self.read_identifier();
                        let typ = token::lookup_ident(&ident);
                        return Token { typ: typ, literal: ident }
                    } else if is_digit(ch) {
                        let number = self.read_number();
                        return Token { typ: INT, literal: number }
                    } else {
                        return Token { typ: ILLEGAL, literal: ch.to_string() }
                    }
                }
            }
        };
        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        self.ch = if self.read_pos >= self.input.len().try_into().unwrap() {
            0
        } else {
            let mut bytes = self.input.bytes();
            bytes.nth(self.read_pos).unwrap()
        };
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn read_identifier(&mut self) -> String {
        let p = self.pos;
        while is_letter(self.ch as char) {
            self.read_char();
        }
        let chars = self.input.as_str();
        chars[p..self.pos].to_string()
    }

    fn read_number(&mut self) -> String {
        let p = self.pos;
        while is_digit(self.ch as char) {
            self.read_char();
        }
        let chars = self.input.as_str();
        chars[p..self.pos].to_string()
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch as char) {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::TokenType::*;
    #[test]
    fn it_gets_next_token() {
        let input = "\
            =+(){},;\
            let five = 5;\
        ".to_string();
        let mut l = Lexer::new(input);
        for &(expected_token, expected_literal) in [
            (ASSIGN, "="),
            (PLUS, "+"),
            (LPAREN, "("),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RBRACE, "}"),
            (COMMA, ","),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "five"),
            (ASSIGN, "="),
            (INT, "5"),
            (SEMICOLON, ";"),
            (EOF, ""),
        ].iter() {
            let tok = l.next_token();
            assert_eq!(tok.typ, expected_token);
            assert_eq!(tok.literal, expected_literal);
        }
    }
}