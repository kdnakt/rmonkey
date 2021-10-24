use std::fmt;
use std::result;

#[derive(Copy, Clone)]
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
        use TokenType::*;
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