// Internal
use crate::{
    object::ObjectType::*,
    object::Object::*,
};

#[derive(PartialEq)]
pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
    ReturnValueObj,
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
    }
}

impl Object {
    pub fn typ(&self) -> ObjectType {
        match self {
            Integer{..} => IntegerObj,
            Boolean{..} => BooleanObj,
            Null => NullObj,
            ReturnValue{..} => ReturnValueObj,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Integer{value, ..} => format!("{}", value),
            Boolean{value, ..} => format!("{}", value),
            Null => "null".to_string(),
            ReturnValue{value, ..} => format!("{}", value.inspect()),
        }
    }
}