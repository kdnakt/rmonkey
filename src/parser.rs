// Internal
use crate::{
    ast::*,
    ast::StatementNode::*,
    ast::ExpressionNode::*,
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

    fn parse_expression(&mut self) -> Option<ExpressionNode> {
        return None
    }

    fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.typ {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();
        if !self.expect_peek(IDENT) {
            return None
        }

        let i_token = self.cur_token.clone();
        let i_lit = i_token.literal.clone();
        let name = Identifier { token: i_token, value: i_lit };

        if !self.expect_peek(ASSIGN) {
            return None
        }

        let value = self.parse_expression();

        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        return Some(LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression();

        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        return Some(ReturnStatement { token, return_value })
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
    use crate::ast::StatementNode::*;
    use crate::ast::ExpressionNode::*;
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

        assert_eq!(3, program.statements.len());

        for &(index, expected_identifier) in [
            (0, "x"),
            (1, "y"),
            (2, "foobar")
        ].iter() {
            let stmt = program.statements.get(index);
            test_let_statement(stmt.unwrap(), expected_identifier.to_string());
        }
    }

    fn test_let_statement(s: &StatementNode, expected: String) {
        assert_eq!("let", s.token_literal());

        let n = match s {
            LetStatement{name, ..} => name,
            _ => panic!("stmt not ast::StatementNode::LetStatement, got {}", s.token_literal()),
        };
        match n {
            Identifier{token, value} => {
                assert_eq!(expected, value.to_string());
                assert_eq!(expected, token.literal.to_string());
            },
            _ => panic!("name not ast::ExpressionNode::Identifier, got {}", n.token_literal()),
        };
    }

    #[test]
    fn it_parses_return_statements() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(3, program.statements.len());

        for stmt in program.statements {
            match stmt {
                ReturnStatement{token, ..} => {
                    assert_eq!("return", token.literal);
                },
                _ => panic!("stmt not ast::StatementNode::ReturnStatement, got={}", stmt.token_literal()),
            }
        }
    }

    #[test]
    fn it_parses_identifier_expression() {
        let input = "foobar;".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());
        match program.statements.get(0) {
            Some(s) => match s {
                ExpressionStatement{expression, ..} => {
                    match expression {
                        Some(e) => match e {
                            Identifier{value, ..} => {
                                assert_eq!("foobar", value);
                                assert_eq!("foobar", e.token_literal());
                            },
                            _ => panic!("expression is not Identifier, got={}", e.token_literal()),
                        },
                        None => panic!("expression is None"),
                    };
                },
                _ => panic!("program.statements[0] is not ast::StatementNode::ExpressionStatement. got={}", s.token_literal()),
            },
            None => panic!("program.statements[0] is None"),
        };
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
