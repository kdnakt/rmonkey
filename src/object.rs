// Std
use std::{
    collections::HashMap,
};

// Internal
use crate::{
    ast::*,
    object::ObjectType::*,
    object::Object::*,
};

#[derive(PartialEq, Debug)]
pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
    ReturnValueObj,
    ErrorObj,
    FunctionObj,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    Null,
    ReturnValue {
        value: Box<Object>,
    },
    Error {
        message: String,
    },
    Function {
        parameters: Vec<ExpressionNode>, // Vec<IdentifierExpression>
        body: Box<StatementNode>,
        env: Environment,
    },
}

impl Object {
    pub fn typ(&self) -> ObjectType {
        match self {
            Integer{..} => IntegerObj,
            Boolean{..} => BooleanObj,
            Null => NullObj,
            ReturnValue{..} => ReturnValueObj,
            Error{..} => ErrorObj,
            Function{..} => FunctionObj,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Integer{value, ..} => format!("{}", value),
            Boolean{value, ..} => format!("{}", value),
            Null => "null".to_string(),
            ReturnValue{value, ..} => format!("{}", value.inspect()),
            Error{message, ..} => format!("ERROR:{}", message),
            Function{parameters, body, ..} => {
                "".to_string()
            },
        }
    }
}

pub fn new_environment() -> Environment {
    let store = HashMap::new();
    Environment { store }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>
}

impl Environment {
    pub fn get(&self, name: String) -> Option<&Object> {
        self.store.get(&name)
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}