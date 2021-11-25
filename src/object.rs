
pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
}

pub enum Object {
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    Null,
}
