// Internal
use crate::{
    ast::*,
    ast::AstNode::*,
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    object::*,
    object::ObjectType::*,
};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean{value: true};
const FALSE: Object = Object::Boolean{value: false};

pub fn eval(node: AstNode, env: &Environment) -> Option<Object> {
    match node {
        Program{statements, ..} => eval_program(statements, env),
        Statement{node, ..} => match node {
            ExpressionStatement{expression, ..} => eval_expression(expression, env),
            BlockStatement{statements, ..} => eval_statements(statements, env),
            ReturnStatement{return_value, ..} => {
                let value = eval_expression(return_value, env);
                Some(Object::ReturnValue{ value: Box::new(value.unwrap()) })
            },
            LetStatement{value, ..} => {
                let val = eval_expression(value, env);
                if is_error(&val) {
                    val
                } else {
                    None
                }
            },
            _ => None,
        }
        _ => None,
    }
}

pub fn eval_expression(expression: Option<ExpressionNode>, env: &Environment) -> Option<Object> {
    match expression {
        Some(IntegerLiteral{value, ..}) => Some(Object::Integer{ value }),
        Some(Boolean{value, ..}) => native_bool_to_boolean_object(value),
        Some(PrefixExpression{right, operator, ..}) => {
            let right = eval_expression(Some(right.unwrap()), env);
            eval_prefix_expression(operator, right)
        },
        Some(InfixExpression{left, right, operator, ..}) => {
            let left = eval_expression(Some(left.unwrap()), env);
            let right = eval_expression(Some(right.unwrap()), env);
            eval_infix_expression(operator, left, right)
        },
        Some(IfExpression{condition, consequence, alternative, ..}) => {
            let cond = eval_expression(Some(condition.unwrap()), env);
            if is_truthy(cond) {
                eval(Statement{
                    node: *consequence
                }, env)
            } else {
                match alternative.as_ref() {
                    Some(_) => eval(Statement{
                        node: alternative.unwrap()
                    }, env),
                    None => Some(NULL),
                }
            }
        },
        Some(IdentifierExpression{value, ..}) => {
            new_error(format!("identifier not found: {}", value))
        },
        _ => None,
    }
}

pub fn eval_program(statements: Vec<StatementNode>, env: &Environment) -> Option<Object> {
    let mut result: Option<Object> = None;
    for s in statements {
        result = eval(Statement{
            node: s,
        }, env);
        match result {
            Some(Object::ReturnValue{value, ..}) => return Some(*value),
            Some(Object::Error{..}) => return result,
            _ => (),
        }
    }
    result
}

pub fn eval_statements(statements: Vec<StatementNode>, env: &Environment) -> Option<Object> {
    let mut result: Option<Object> = None;
    for s in statements {
        result = eval(Statement{
            node: s,
        }, env);
        if let Some(o) = &result {
            match o.typ() {
                ReturnValueObj => return result,
                ErrorObj => return result,
                _ => ()
            }
        }
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

fn eval_infix_expression(op: String, left: Option<Object>, right: Option<Object>) -> Option<Object> {
    if left.as_ref().unwrap_or(&NULL).typ() == IntegerObj && right.as_ref().unwrap_or(&NULL).typ() == IntegerObj {
        eval_integer_infix_expression(op, left, right)
    } else {
        let left = left.as_ref().unwrap_or(&NULL);
        let right = right.as_ref().unwrap_or(&NULL);
        match op.as_ref() {
            "==" => native_bool_to_boolean_object(left == right),
            "!=" => native_bool_to_boolean_object(left != right),
            _ => {
                if left.typ() != right.typ() {
                    new_error(format!("type mismatch: {:?} {} {:?}", left.typ(), op, right.typ()))
                } else {
                    new_error(format!("unknown operator: {:?} {} {:?}", left.typ(), op, right.typ()))
                }
            },
        }
    }
}

fn eval_integer_infix_expression(op: String, left: Option<Object>, right: Option<Object>) -> Option<Object> {
    let left_val = if let Some(Object::Integer{value, ..}) = left { value }
        else { panic!("left is not Object::Integer"); };
    let right_val = if let Some(Object::Integer{value, ..}) = right { value }
        else { panic!("right is not Object::Integer"); };
    match op.as_ref() {
        "+" => Some(Object::Integer{value: left_val + right_val}),
        "-" => Some(Object::Integer{value: left_val - right_val}),
        "*" => Some(Object::Integer{value: left_val * right_val}),
        "/" => Some(Object::Integer{value: left_val / right_val}),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => new_error(format!("unknown operator: {:?} {} {:?}", left.unwrap().typ(), op, right.unwrap().typ())),
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
    if let Some(Object::Integer{value, ..}) = right {
        Some(Object::Integer{ value: -value })
    } else {
        new_error(format!("unknown operator: -{:?}", right.unwrap().typ()))
    }
}

fn is_truthy(obj: Option<Object>) -> bool {
    match obj {
        Some(NULL) => false,
        Some(TRUE) => true,
        Some(FALSE) => false,
        _ => true,
    }
}

fn is_error(obj: &Option<Object>) -> bool {
    match obj {
        Some(o) => o.typ() == ErrorObj,
        _ => false,
    }
}

fn new_error(message: String) -> Option<Object> {
    Some(Object::Error{ message })
}

#[cfg(test)]
mod tests {
    use super::eval;
    use super::NULL;
    use crate::object::*;
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
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
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
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2", 4),
            ("50 / 2 * 2 + 10", 60),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn it_eval_if_else_expression() {
        for &(input, expected) in [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            match expected {
                Some(i) => test_integer_object(evaluated, i),
                None => test_null_object(evaluated),
            }
        }
    }

    #[test]
    fn it_eval_return_statement() {
        for &(input, expected) in [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5;", 10),
            ("
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
            ", 10),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn it_eval_error_handling() {
        for &(input, expected) in [
            ("5 + true;", "type mismatch: IntegerObj + BooleanObj"),
            ("5 + true; 5;", "type mismatch: IntegerObj + BooleanObj"),
            ("-true;", "unknown operator: -BooleanObj"),
            ("false + true;", "unknown operator: BooleanObj + BooleanObj"),
            ("foobar", "identifier not found: foobar"),
        ].iter() {
            let evaluated = test_eval(input.to_string());
            let msg = if let Some(Error{message, ..}) = evaluated {
                message
            } else {
                panic!("no error object returned. got={:?}", evaluated);
            };
            assert_eq!(expected, msg);
        }
    }

    fn test_eval(input: String) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let env = new_environment();
        eval(program, &env)
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

    fn test_null_object(evaluated: Option<Object>) {
        let obj = evaluated.unwrap();
        assert_eq!(NULL, obj);
    }

}