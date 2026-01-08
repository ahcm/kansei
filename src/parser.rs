use crate::ast::{Expr, Op};
use crate::lexer::{Lexer, Token};

pub struct Parser
{
    lexer: Lexer,
    current_token: Token,
}

impl Parser
{
    pub fn new(mut lexer: Lexer) -> Self
    {
        let current_token = lexer.next_token();
        Self {
            lexer,
            current_token,
        }
    }

    fn eat(&mut self)
    {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Expr
    {
        self.parse_assignment()
    }

    // Handles: x = ...
    fn parse_assignment(&mut self) -> Expr
    {
        if let Token::Identifier(name) = &self.current_token
        {
            // Peek ahead cheat: we should use proper peeking, but cloning for MVP is fine
            let mut temp_lexer = self.lexer.clone(); // Note: Lexer needs Clone derive added
            if temp_lexer.next_token() == Token::Equals
            {
                let var_name = name.clone();
                self.eat(); // eat name
                self.eat(); // eat =
                return Expr::Assignment {
                    name: var_name,
                    value: Box::new(self.parse_expression()),
                };
            }
        }
        self.parse_expression()
    }

    // Handles: 1 + 2
    fn parse_expression(&mut self) -> Expr
    {
        let mut left = self.parse_term();

        while self.current_token == Token::Plus || self.current_token == Token::Minus
        {
            let op = match self.current_token
            {
                Token::Plus => Op::Add,
                Token::Minus => Op::Subtract,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_term();
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_term(&mut self) -> Expr
    {
        let mut left = self.parse_factor();

        while self.current_token == Token::Star || self.current_token == Token::Slash
        {
            let op = match self.current_token
            {
                Token::Star => Op::Multiply,
                Token::Slash => Op::Divide,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_factor();
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_factor(&mut self) -> Expr
    {
        match self.current_token.clone()
        {
            Token::Integer(i) =>
            {
                self.eat();
                Expr::Integer(i)
            }
            Token::StringLiteral(s) =>
            {
                self.eat();
                Expr::String(s)
            }
            Token::CommandLiteral(c) =>
            {
                self.eat();
                Expr::Shell(c)
            }
            Token::Identifier(name) =>
            {
                self.eat();
                // Check if it's a function call like "puts"
                if name == "puts"
                {
                    let arg = self.parse_expression();
                    return Expr::Call {
                        function: name,
                        args: vec![arg],
                    };
                }
                Expr::Identifier(name)
            }
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }
}
