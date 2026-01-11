use std::cell::RefCell;
use std::rc::Rc;
use crate::intern::SymbolId;

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
    pub params: Vec<(SymbolId, bool)>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind
{
    // Basic Values
    Integer(i64),
    Float(f64),
    Identifier { name: SymbolId, slot: Option<usize> },
    Reference(SymbolId), // &x
    String(Rc<String>), // "hello"
    Boolean(bool),
    Nil,
    Shell(Rc<String>), // `ls`

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
        name: SymbolId,
        value: Box<Expr>,
        slot: Option<usize>,
    },
    IndexAssignment
    {
        target: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },

    // Function calls (simplified for scripting)
    Call
    {
        function: Box<Expr>,
        args: Vec<Expr>,
        block: Option<Closure>,
        inlined_body: Rc<RefCell<Option<Expr>>>,
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
        var: SymbolId,
        var_slot: Option<usize>,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
    Loop
    {
        count: Box<Expr>,
        var: Option<SymbolId>,
        var_slot: Option<usize>,
        body: Box<Expr>,
    },

    FunctionDef
    {
        name: SymbolId,
        params: Vec<(SymbolId, bool)>,
        body: Box<Expr>,
        slots: Option<Rc<Vec<Rc<String>>>>,
    },
    AnonymousFunction
    {
        params: Vec<(SymbolId, bool)>,
        body: Box<Expr>,
        slots: Option<Rc<Vec<Rc<String>>>>,
    },

    Array(Vec<Expr>),
    ArrayGenerator
    {
        generator: Box<Expr>,
        size: Box<Expr>,
    },
    Map(Vec<(Expr, Expr)>),
    Index
    {
        target: Box<Expr>,
        index: Box<Expr>,
    },

    // A block of code (so an 'if' can run multiple lines)
    Block(Vec<Expr>),
}
