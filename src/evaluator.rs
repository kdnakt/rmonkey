
// Internal
use crate::{
    ast::*,
    ast::AstNode::*,
    object::Object,
};

pub fn eval(node: AstNode) -> Option<Object> {
    match node {
        Program{statements, ..} => eval_statements(&statements),
        _ => None,
    }
}

pub fn eval_statements(statements: &Vec<StatementNode>) -> Option<Object> {
    let mut result: Option<Object> = None;
    for s in statements {
        result = eval(Statement{
            node: s,
        });
    }
    result
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
}