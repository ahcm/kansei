use crate::ast::{Closure, Expr, Op};
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
        self.parse_block()
    }

    fn parse_assignment(&mut self) -> Expr
    {
        if let Token::Identifier(name) = &self.current_token
        {
            let mut temp_lexer = self.lexer.clone();
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

    fn parse_expression(&mut self) -> Expr
    {
        let mut left = self.parse_math(); 

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
        let mut expr = self.parse_atom();

        loop {
            if self.current_token == Token::LeftBracket {
                self.eat(); // [
                let index = self.parse_expression();
                self.expect(Token::RightBracket);
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.current_token == Token::Dot {
                self.eat(); // .
                let name = match &self.current_token {
                    Token::Identifier(n) => n.clone(),
                    _ => panic!("Expected property name after dot"),
                };
                self.eat();
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(Expr::String(name)),
                };
            } else if self.current_token == Token::LeftParen {
                self.eat(); // (
                let mut args = Vec::new();
                if self.current_token != Token::RightParen {
                    loop {
                        args.push(self.parse_expression());
                        if self.current_token == Token::Comma {
                            self.eat();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(Token::RightParen);

                // Check for Block { |params| body }
                let block = if self.current_token == Token::LeftBrace {
                    self.eat(); // {
                    let mut params = Vec::new();
                    if self.current_token == Token::Pipe {
                        self.eat(); // |
                        loop {
                            let is_ref = if self.current_token == Token::Ampersand {
                                self.eat();
                                true
                            } else {
                                false
                            };
                            match &self.current_token {
                                Token::Identifier(p) => params.push((p.clone(), is_ref)),
                                _ => panic!("Expected param name"),
                            }
                            self.eat();
                            if self.current_token == Token::Comma {
                                self.eat();
                            } else {
                                break;
                            }
                        }
                        self.expect(Token::Pipe);
                    }
                    let body = self.parse_block();
                    self.expect(Token::RightBrace);
                    Some(Closure { params, body: Box::new(body) })
                } else {
                    None
                };

                expr = Expr::Call {
                    function: Box::new(expr),
                    args,
                    block,
                };
            } else {
                break;
            }
        }
        expr
    }

    fn parse_atom(&mut self) -> Expr
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

                // Legacy built-in support
                if name == "puts" || name == "print" || name == "write_file" || name == "read_file" || name == "len"
                {
                    let mut args = Vec::new();
                    args.push(self.parse_expression());

                    while self.current_token == Token::Comma
                    {
                        self.eat();
                        args.push(self.parse_expression());
                    }

                    return Expr::Call {
                        function: Box::new(Expr::Identifier(name)),
                        args,
                        block: None,
                    };
                }
                Expr::Identifier(name)
            }
            Token::Fn => self.parse_fn(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::LeftBracket => self.parse_array(),
            Token::LeftBrace => self.parse_map(),
            Token::LeftParen => {
                self.eat();
                if self.current_token == Token::RightParen {
                    self.eat();
                    return Expr::Nil;
                }
                let expr = self.parse_expression();
                self.expect(Token::RightParen);
                expr
            },
            Token::Yield => {
                self.eat();
                let mut args = Vec::new();
                if self.current_token == Token::LeftParen {
                     self.eat();
                     if self.current_token != Token::RightParen {
                         loop {
                             args.push(self.parse_expression());
                             if self.current_token == Token::Comma {
                                 self.eat();
                             } else {
                                 break;
                             }
                         }
                     }
                     self.expect(Token::RightParen);
                } else {
                    // Variadic-ish greedy parsing
                    while self.current_token != Token::EOF &&
                          self.current_token != Token::End &&
                          self.current_token != Token::Else &&
                          self.current_token != Token::Elif &&
                          self.current_token != Token::RightBrace &&
                          self.current_token != Token::RightParen
                    {
                        args.push(self.parse_expression());
                        if self.current_token == Token::Comma {
                            self.eat();
                        } else {
                            break;
                        }
                    }
                }
                Expr::Yield(args)
            }
            Token::Ampersand => {
                self.eat();
                match &self.current_token {
                    Token::Identifier(name) => {
                        let n = name.clone();
                        self.eat();
                        Expr::Reference(n)
                    },
                    _ => panic!("Expected identifier after &"),
                }
            }
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_array(&mut self) -> Expr
    {
        self.eat(); // [
        let mut elements = Vec::new();
        if self.current_token != Token::RightBracket {
            loop {
                elements.push(self.parse_expression());
                if self.current_token == Token::Comma {
                    self.eat();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RightBracket);
        Expr::Array(elements)
    }

    fn parse_map(&mut self) -> Expr
    {
        self.eat(); // {
        let mut entries = Vec::new();
        if self.current_token != Token::RightBrace {
            loop {
                let key = self.parse_expression();
                self.expect(Token::Colon);
                let value = self.parse_expression();
                entries.push((key, value));
                
                if self.current_token == Token::Comma {
                    self.eat();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RightBrace);
        Expr::Map(entries)
    }

    fn parse_fn(&mut self) -> Expr
    {
        self.eat(); // eat 'fn'

        let name = match &self.current_token
        {
            Token::Identifier(n) => n.clone(),
            _ => panic!("Expected function name"),
        };
        self.eat();

        self.expect(Token::LeftParen);
        let mut params = Vec::new();
        if self.current_token != Token::RightParen
        {
            loop
            {
                let is_ref = if self.current_token == Token::Ampersand {
                    self.eat();
                    true
                } else {
                    false
                };

                match &self.current_token
                {
                    Token::Identifier(arg) => params.push((arg.clone(), is_ref)),
                    _ => panic!("Expected parameter name"),
                }
                self.eat();
                if self.current_token == Token::Comma
                {
                    self.eat();
                }
                else
                {
                    break;
                }
            }
        }
        self.expect(Token::RightParen);

        let body = self.parse_block();
        self.expect(Token::End);

        Expr::FunctionDef {
            name,
            params,
            body: Box::new(body),
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
                Some(Box::new(self.parse_if()))
            }
            Token::Else =>
            {
                self.eat();
                let block = self.parse_block();
                self.expect(Token::End);
                Some(Box::new(block))
            }
            Token::End =>
            {
                self.eat();
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

    fn parse_for(&mut self) -> Expr
    {
        self.eat(); // eat 'for'

        // Expect variable name
        let var_name = match &self.current_token {
            Token::Identifier(name) => name.clone(),
            _ => panic!("Expected identifier after 'for'"),
        };
        self.eat();

        self.expect(Token::In);

        let iterable = self.parse_expression();

        let body = self.parse_block();
        self.expect(Token::End);

        Expr::For {
            var: var_name,
            iterable: Box::new(iterable),
            body: Box::new(body),
        }
    }

    fn parse_block(&mut self) -> Expr
    {
        let mut statements = Vec::new();

        while self.current_token != Token::End
            && self.current_token != Token::Else
            && self.current_token != Token::Elif
            && self.current_token != Token::RightBrace // Handle block end }
            && self.current_token != Token::EOF
        {
            statements.push(self.parse_assignment());
        }

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
