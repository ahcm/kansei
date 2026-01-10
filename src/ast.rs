#[derive(Debug, Clone, PartialEq)]
pub enum Op
{
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure
{
    pub params: Vec<(String, bool)>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr
{
    // Basic Values
    Integer(i64),
    Identifier(String),
    String(String), // "hello"
    Boolean(bool),
    Nil,
    Shell(String), // `ls`

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
        function: Box<Expr>,
        args: Vec<Expr>,
        block: Option<Closure>,
    },

    Yield(Vec<Expr>),

    If
    {
        condition: Box<Expr>,
        then_branch: Box<Expr>, // For now, single expression blocks
        else_branch: Option<Box<Expr>>,
    },

    While
    {
        condition: Box<Expr>,
        body: Box<Expr>,
    },

    For
    {
        var: String,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },

    FunctionDef
    {
        name: String,
        params: Vec<(String, bool)>,
        body: Box<Expr>,
    },

    Array(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    Index
    {
        target: Box<Expr>,
        index: Box<Expr>,
    },

    // A block of code (so an 'if' can run multiple lines)
    Block(Vec<Expr>),
}