// Internal
use crate::{
    ast::*,
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    lexer::Lexer,
    token::*,
    token::TokenType::*,
    parser::Precedence::*,
    trace::*,
};

struct ScopeCall<F: FnMut()> {
    c: F
}

impl<F: FnMut()> Drop for ScopeCall<F> {
    fn drop(&mut self) {
        (self.c)();
    }
}

macro_rules! defer {
    ($e: expr) => {
        let _scope_call = ScopeCall { c: || -> () { $e; }};
    }
}

enum Precedence {
    LOWEST = 1,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
}

fn precedence(t: TokenType) -> Precedence {
    match t {
        EQ => EQUALS,
        NOTEQ => EQUALS,
        LT => LESSGREATER,
        GT => LESSGREATER,
        PLUS => SUM,
        MINUS => SUM,
        SLASH => PRODUCT,
        ASTERISK => PRODUCT,
        _ => LOWEST,
    }
}

enum PrefixParseFn {
    ParseIdentifier,
}

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

    fn parse_expression(&mut self, p: Precedence) -> Option<ExpressionNode> {
        let trace = trace("parse_expression");
        defer!(untrace(trace));
        let prefix = self.parse_prefix(self.cur_token.typ);
        let mut left_exp = match prefix {
            Some(p) => Some(p),
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.typ);
                return None;
            }
        };

        let precedence = p as i32;
        while !self.peek_token_is(SEMICOLON) && precedence < (self.peek_precedence() as i32) {
            let left = match self.peek_token.typ {
                PLUS => left_exp,
                MINUS => left_exp,
                SLASH => left_exp,
                ASTERISK => left_exp,
                EQ => left_exp,
                NOTEQ => left_exp,
                GT => left_exp,
                LT => left_exp,
                _ => {
                    return left_exp;
                },
            };
            self.next_token();
            left_exp = self.parse_infix_expression(left);
        }
        return left_exp;
    }

    fn parse_infix_expression(&mut self, left: Option<ExpressionNode>) -> Option<ExpressionNode> {
        let trace = trace("parse_infix_expression");
        defer!(untrace(trace));
        let token = self.cur_token.clone();
        let operator = token.literal.to_string();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        Some(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn cur_precedence(&self) -> Precedence {
        precedence(self.cur_token.typ)
    }

    fn parse_prefix(&mut self, t: TokenType) -> Option<ExpressionNode> {
        match t {
            IDENT => Some(IdentifierExpression {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.to_string(),
            }),
            INT => self.parse_integer_literal(),
            BANG => self.parse_prefix_expression(),
            MINUS => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<ExpressionNode> {
        let trace = trace("parse_prefix_expression");
        defer!(untrace(trace));
        let token = self.cur_token.clone();
        let operator = token.literal.to_string();
        self.next_token();
        let right = self.parse_expression(PREFIX);
        Some(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<ExpressionNode> {
        let trace = trace("parse_integer_literal");
        defer!(untrace(trace));
        let token = self.cur_token.clone();
        let result = token.literal.parse();
        if result.is_err() {
            let msg = format!("could not parse {} as integer", token.literal);
            self.errors.push(msg);
            return None;
        }
        let value: i64 = result.unwrap();
        Some(IntegerLiteral { token, value })
    }

    fn peek_precedence(&self) -> Precedence {
        precedence(self.peek_token.typ)
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {} found", t);
        self.errors.push(msg);
    }

    fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.typ {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();
        if !self.expect_peek(IDENT) {
            return None
        }

        let i_token = self.cur_token.clone();
        let i_lit = i_token.literal.clone();
        let name = IdentifierExpression { token: i_token, value: i_lit };

        if !self.expect_peek(ASSIGN) {
            return None
        }

        self.next_token();
        let value = self.parse_expression(LOWEST);

        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        return Some(LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(LOWEST);

        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        return Some(ReturnStatement { token, return_value })
    }

    fn parse_expression_statement(&mut self) -> Option<StatementNode> {
        let trace = trace("parse_expression_statement");
        defer!(untrace(trace));
        let token = self.cur_token.clone();
        let expression = self.parse_expression(LOWEST);
        if self.peek_token_is(SEMICOLON) {
            self.next_token();
        }
        Some(ExpressionStatement { token, expression })
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
            IdentifierExpression{token, value} => {
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

        let stmt = program.statements.get(0);
        let expression = if let Some(ExpressionStatement{expression, ..}) = stmt {
            expression
        } else {
            panic!("program.statements[0] is not ExpressionStatement, got={}", stmt.unwrap().token_literal());
        };

        let e = if let Some(e) = expression {
            e
        } else {
            panic!("expression is None");
        };
        assert_eq!("foobar", e.token_literal());

        let value = if let IdentifierExpression{value, ..} = e {
            value
        } else {
            panic!("expression is not IdentifierExpression");
        };
        assert_eq!("foobar", value);
    }

    #[test]
    fn it_parses_integer_literal_expression() {
        let input = "5;".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(&p);

        assert_eq!(1, program.statements.len());

        let stmt = program.statements.get(0);
        let expression = if let Some(ExpressionStatement{expression, ..}) = stmt {
            expression
        } else {
            panic!("program.statements[0] is not ExpressionStatement, got={}", stmt.unwrap().token_literal());
        };

        let e = if let Some(e) = expression {
            e
        } else {
            panic!("expression is None");
        };
        assert_eq!("5", e.token_literal());

        let value = if let IntegerLiteral{value, ..} = e {
            value
        } else {
            panic!("expression is not IntegerLiteral");
        };
        assert_eq!(5i64, *value);
    }

    #[test]
    fn it_parses_prefix_expressions() {
        for &(input, op, value) in [
            ("!5","!",5),
            ("-15","-",15),
        ].iter() {
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(&p);

            if program.statements.len() != 1 {
                panic!("program.statements does not contain {} statements. got={}",
                        1, program.statements.len());
            }
            let stmt = program.statements.get(0);
            let expression = if let Some(ExpressionStatement{expression, ..}) = stmt {
                expression
            } else {
                panic!("program.statements[0] is not ExpressionStatement, got={}", stmt.unwrap().token_literal());
            };
            let e = if let Some(e) = expression {
                e
            } else {
                panic!("expression is None");
            };
            let (operator, right) = if let PrefixExpression{operator, right, ..} = e {
                (operator, right)
            } else {
                panic!("expression is not PrefixExpression");
            };
            assert_eq!(op, operator);
            test_integer_literal(right, value);
        }
    }

    #[test]
    fn it_parses_infix_expressions() {
        for &(input, left_val, op, right_val) in [
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ].iter() {
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(&p);

            if program.statements.len() != 1 {
                panic!("{}: program.statements does not contain {} statements. got={}",
                        input, 1, program.statements.len());
            }
            let stmt = program.statements.get(0);
            let expression = if let Some(ExpressionStatement{expression, ..}) = stmt {
                expression
            } else {
                panic!("program.statements[0] is not ExpressionStatement, got={}", stmt.unwrap().token_literal());
            };
            let e = if let Some(e) = expression {
                e
            } else {
                panic!("expression is None");
            };

            let (left, operator, right) = if let InfixExpression{left, operator, right, ..} = e {
                (left, operator, right)
            } else {
                panic!("expression is not InfixExpression");
            };
            test_integer_literal(left, left_val);
            assert_eq!(op, operator);
            test_integer_literal(right, right_val);
        }
    }

    #[test]
    fn it_parses_operator_precedence() {
        for &(input, expected) in [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)"
            ),
            (
                "3 + 4; -5 * 5",
                "(3 + 4)((-5) * 5)"
            ),
            (
                "5 > 4 == 3 < 4",
                "((5 > 4) == (3 < 4))"
            ),
            (
                "5 < 4 != 3 < 4",
                "((5 < 4) != (3 < 4))"
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
            ),
        ].iter() {
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(&p);

            assert_eq!(expected, program.to_string());
        }
    }

    fn test_integer_literal(il: &Box<Option<ExpressionNode>>, expected: i64) {
        let il = if let Some(e) = il.as_ref() { e } else { panic!("il is None") };
        assert_eq!(format!("{}", expected), il.token_literal());

        let value = if let IntegerLiteral{value, ..} = il {
            value
        } else {
            panic!("il is not IntegerLiteral");
        };
        assert_eq!(expected, *value);
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
