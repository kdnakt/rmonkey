// Internal
use crate::{
    object::ObjectType::*,
    object::Object::*,
};

pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
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
}

impl Object {
    fn typ(&self) -> ObjectType {
        match self {
            Integer{..} => IntegerObj,
            Boolean{..} => BooleanObj,
            Null => NullObj,
        }
    }

    fn inspect(&self) -> String {
        match self {
            Integer{value, ..} => format!("{}", value),
            Boolean{value, ..} => format!("{}", value),
            Null => "null".to_string()
        }
    }
}