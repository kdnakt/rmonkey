// Internal
use crate::{
    ast::*,
    lexer::Lexer,
    token::*,
    token::TokenType::*,
};

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        let cur_token = l.next_token();
        let peek_token = l.next_token();
        Parser {
            l: l,
            cur_token: cur_token,
            peek_token: peek_token,
            errors: Vec::new(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        while self.cur_token.typ != EOF {
            let stmt = self.parse_statement();
            match stmt {
                None => (),
                Some(s) => statements.push(s),
            }
            self.next_token();
        }
        Program { statements: statements }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt = match self.cur_token.typ {
            LET => self.parse_let_statement(),
            _ => None,
        };
        match stmt {
            None => None,
            Some(s) => Some(Box::new(s)),
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let cur_token = self.cur_token.clone();
        if !self.expect_peek(IDENT) {
            return None
        }

        let i_token = self.cur_token.clone();
        let i_lit = i_token.literal.clone();
        let name = Identifier { token: i_token, value: i_lit };

        if !self.expect_peek(ASSIGN) {
            return None
        }

        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        return Some(LetStatement { token: cur_token, name: name })
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.typ == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.typ == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn peek_error(&mut self, t: TokenType) {
        let msg = format!("expected next token to be {}, got {} instead", t, self.peek_token.typ);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::lexer::Lexer;
    use crate::ast::*;
    #[test]
    fn it_parses_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        for &(index, expected_identifier) in [
            (0, "x"),
            (1, "y"),
            (2, "fooba")
        ].iter() {
            let stmt = program.statements.get(index);
            test_let_statement(stmt.unwrap(), expected_identifier.to_string());
        } 

        assert_eq!(3, program.statements.len());
    }

    fn test_let_statement(s: &Box<dyn Statement>, name: String) {
        let stmt = s.as_ref();
        assert_eq!("let", stmt.token_literal());

        // let anyStmt = stmt.as_any();
        // let letStmt: &LetStatement = match anyStmt.downcast_ref::<LetStatement>() {
        //     Some(s) => s,
        //     None => panic!("stmt not &ast::LetStatement, got {:?}", stmt.token_literal()),
        // };
        // assert_eq!(name, letStmt.name.value);
        // assert_eq!(name, letStmt.name.token.literal);
    }

    fn check_parse_errors(p: &Parser) {
        if p.errors.len() != 0 {
            for err in &p.errors {
                eprintln!("Parser error: {}", err);
            }
            panic!("Parser has {} errors", p.errors.len());
        }
    }

}
