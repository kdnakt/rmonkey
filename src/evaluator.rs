// Internal
use crate::{
    ast::*,
    ast::AstNode::*,
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    object::Object,
};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean{value: true};
const FALSE: Object = Object::Boolean{value: false};

pub fn eval(node: AstNode) -> Option<Object> {
    match node {
        Program{statements, ..} => eval_statements(statements),
        Statement{node, ..} => match node {
            ExpressionStatement{expression, ..} => eval_expression(expression),
            _ => None,
        }
        _ => None,
    }
}

pub fn eval_expression(expression: Option<ExpressionNode>) -> Option<Object> {
    match expression {
        Some(IntegerLiteral{value, ..}) => Some(Object::Integer{ value }),
        Some(Boolean{value, ..}) => native_bool_to_boolean_object(value),
        Some(PrefixExpression{right, operator, token, ..}) => {
            let right = eval_expression(Some(right.unwrap()));
            eval_prefix_expression(operator, right)
        },
        _ => None,
    }
}

pub fn eval_statements(statements: Vec<StatementNode>) -> Option<Object> {
    let mut result: Option<Object> = None;
    for s in statements {
        result = eval(Statement{
            node: s,
        });
    }
    result
}

fn native_bool_to_boolean_object(b: bool) -> Option<Object> {
    Some(if b {
        TRUE
    } else {
        FALSE
    })
}

fn eval_prefix_expression(op: String, right: Option<Object>) -> Option<Object> {
    match op.as_ref() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Some(NULL),
    }
}

fn eval_bang_operator_expression(right: Option<Object>) -> Option<Object> {
    match right {
        Some(TRUE) => Some(FALSE),
        Some(FALSE) => Some(TRUE),
        Some(NULL) => Some(TRUE),
        _ => Some(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Option<Object>) -> Option<Object> {
    Some(if let Some(Object::Integer{value, ..}) = right {
        Object::Integer{ value: -value }
    } else {
        NULL
    })
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::object::Object;
    use crate::object::Object::*;
    use crate::lexer::*;
    use crate::parser::*;
    #[test]
    fn it_evaluates_bang_operator() {
        for &(input, expected) in [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn it_eval_boolean_expression() {
        for &(input, expected) in [
            ("true", true),
            ("false", false),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn it_eval_integer_expression() {
        for &(input, expected) in [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: String) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        eval(program)
    }

    fn test_boolean_object(evaluated: Option<Object>, expected: bool) {
        let value = if let Some(Boolean{value, ..}) = evaluated {
            value
        } else {
            panic!("object is not Boolean. got={:?}", evaluated);
        };
        assert_eq!(expected, value);
    }

    fn test_integer_object(evaluated: Option<Object>, expected: i64) {
        let value = if let Some(Integer{value, ..}) = evaluated {
            value
        } else {
            panic!("object is not Integer. got={:?}", evaluated);
        };
        assert_eq!(expected, value);
    }

}