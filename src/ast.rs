#[derive(Debug, Clone, PartialEq)]
pub enum Op
{
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr
{
    // Basic Values
    Integer(i64),
    Identifier(String),
    String(String), // "hello"
    Shell(String),  // `ls`

    // Operations
    BinaryOp
    {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },

    // Assignment: x = 10
    Assignment
    {
        name: String,
        value: Box<Expr>,
    },

    // Function calls (simplified for scripting)
    Call
    {
        function: String,
        args: Vec<Expr>,
    },
}
