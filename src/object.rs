// Internal
use crate::{
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
}

#[derive(Debug, PartialEq, Eq)]
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
}

impl Object {
    pub fn typ(&self) -> ObjectType {
        match self {
            Integer{..} => IntegerObj,
            Boolean{..} => BooleanObj,
            Null => NullObj,
            ReturnValue{..} => ReturnValueObj,
            Error{..} => ErrorObj,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Integer{value, ..} => format!("{}", value),
            Boolean{value, ..} => format!("{}", value),
            Null => "null".to_string(),
            ReturnValue{value, ..} => format!("{}", value.inspect()),
            Error{message, ..} => format!("ERROR:{}", message),
        }
    }
}