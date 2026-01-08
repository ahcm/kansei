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

    fn expect(&mut self, token: Token)
    {
        if self.current_token == token
        {
            self.eat();
        }
        else
        {
            panic!("Syntax Error: Expected {:?}, but found {:?}", token, self.current_token);
        }
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

    // LEVEL 1: Equality & Comparison (==, !=, <, >)
    // This is now the "top" of the expression logic
    fn parse_expression(&mut self) -> Expr
    {
        let mut left = self.parse_math(); // Go down to math first

        while matches!(
            self.current_token,
            Token::EqualEqual | Token::BangEqual | Token::Less | Token::Greater
        )
        {
            let op = match self.current_token
            {
                Token::EqualEqual => Op::Equal,
                Token::BangEqual => Op::NotEqual,
                Token::Less => Op::LessThan,
                Token::Greater => Op::GreaterThan,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_math();
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    // LEVEL 2: Addition & Subtraction (+, -)
    // Renamed from 'parse_expression' to 'parse_math'
    fn parse_math(&mut self) -> Expr
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

    // LEVEL 3: Multiplication & Division (*, /)
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

    // LEVEL 4: Atoms (Ints, Strings, Bools, Parens)
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
            Token::True =>
            {
                self.eat();
                Expr::Boolean(true)
            }
            Token::False =>
            {
                self.eat();
                Expr::Boolean(false)
            }
            Token::Identifier(name) =>
            {
                self.eat();
                if name == "puts" || name == "print" || name == "write_file" || name == "read_file"
                {
                    let mut args = Vec::new();

                    args.push(self.parse_expression());

                    while self.current_token == Token::Comma
                    {
                        self.eat();
                        args.push(self.parse_expression());
                    }

                    return Expr::Call {
                        function: name,
                        args,
                    };
                }
                Expr::Identifier(name)
            }
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_if(&mut self) -> Expr
    {
        self.eat(); // eat 'if' (or 'elif' if called recursively)

        let condition = self.parse_expression();
        let then_branch = self.parse_block();

        let else_branch = match self.current_token
        {
            Token::Elif =>
            {
                // RECURSION MAGIC:
                // We treat 'elif' exactly like a new 'if' statement
                // that lives inside the 'else' slot of the parent.
                Some(Box::new(self.parse_if()))
            }
            Token::Else =>
            {
                self.eat();
                let block = self.parse_block();
                self.expect(Token::End); // The chain ends here
                Some(Box::new(block))
            }
            Token::End =>
            {
                self.eat(); // The chain ends here
                None
            }
            _ => panic!("Expected elif, else, or end"),
        };
        Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        }
    }

    fn parse_while(&mut self) -> Expr
    {
        self.eat(); // eat 'while'

        let condition = self.parse_expression();
        let body = self.parse_block();

        self.expect(Token::End);

        Expr::While {
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }

    // 2. The Worker: Collects lines of code until told to stop
    fn parse_block(&mut self) -> Expr
    {
        let mut statements = Vec::new();

        // Keep parsing until we hit a keyword that ends a block
        while self.current_token != Token::End
            && self.current_token != Token::Else
            && self.current_token != Token::Elif
            && self.current_token != Token::EOF
        {
            statements.push(self.parse_expression());
        }

        // Optimization: If block is just 1 line, return that line.
        // Otherwise return a Block of lines.
        if statements.len() == 1
        {
            statements.pop().unwrap()
        }
        else
        {
            Expr::Block(statements)
        }
    }
}
