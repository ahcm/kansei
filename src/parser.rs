use crate::ast::{Closure, Expr, ExprKind, Op};
use crate::intern;
use crate::lexer::{Lexer, Span, Token};
use std::rc::Rc;
use std::cell::RefCell;

pub struct Parser
{
    lexer: Lexer,
    current_token: Span,
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
        if self.current_token.token == token
        {
            self.eat();
        }
        else
        {
            panic!("Syntax Error at line {}: Expected {:?}, but found {:?}", self.current_token.line, token, self.current_token.token);
        }
    }

    fn make_expr(&self, kind: ExprKind, line: usize) -> Expr {
        Expr { kind, line }
    }

    pub fn parse(&mut self) -> Expr
    {
        self.parse_block()
    }

    fn parse_assignment(&mut self) -> Expr
    {
        let expr = self.parse_expression();

        if self.current_token.token == Token::Equals {
            let line = self.current_token.line;
            self.eat(); // =
            let value = self.parse_expression();

            return match expr.kind {
                ExprKind::Identifier { name, .. } => self.make_expr(ExprKind::Assignment {
                    name,
                    value: Box::new(value),
                    slot: None,
                }, line),
                ExprKind::Index { target, index } => self.make_expr(ExprKind::IndexAssignment {
                    target,
                    index,
                    value: Box::new(value),
                }, line),
                _ => panic!("Invalid assignment target at line {}", line),
            };
        }
        expr
    }

    fn parse_expression(&mut self) -> Expr
    {
        let mut left = self.parse_math(); 

        while matches!(
            self.current_token.token,
            Token::EqualEqual | Token::BangEqual | Token::Less | Token::Greater
        )
        {
            let line = self.current_token.line;
            let op = match self.current_token.token
            {
                Token::EqualEqual => Op::Equal,
                Token::BangEqual => Op::NotEqual,
                Token::Less => Op::LessThan,
                Token::Greater => Op::GreaterThan,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_math();
            left = self.make_expr(ExprKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }, line);
        }
        left
    }

    fn parse_math(&mut self) -> Expr
    {
        let mut left = self.parse_term();

        while self.current_token.token == Token::Plus || self.current_token.token == Token::Minus
        {
            let line = self.current_token.line;
            let op = match self.current_token.token
            {
                Token::Plus => Op::Add,
                Token::Minus => Op::Subtract,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_term();
            left = self.make_expr(ExprKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }, line);
        }
        left
    }

    fn parse_term(&mut self) -> Expr
    {
        let mut left = self.parse_factor();

        while self.current_token.token == Token::Star || self.current_token.token == Token::Slash
        {
            let line = self.current_token.line;
            let op = match self.current_token.token
            {
                Token::Star => Op::Multiply,
                Token::Slash => Op::Divide,
                _ => unreachable!(),
            };
            self.eat();
            let right = self.parse_factor();
            left = self.make_expr(ExprKind::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }, line);
        }
        left
    }

    fn parse_factor(&mut self) -> Expr
    {
        let mut expr = self.parse_atom();

        loop {
            let line = self.current_token.line;
            if self.current_token.token == Token::LeftBracket {
                self.eat(); // [
                let index = self.parse_expression();
                self.expect(Token::RightBracket);
                expr = self.make_expr(ExprKind::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                }, line);
            } else if self.current_token.token == Token::Dot || self.current_token.token == Token::ColonColon {
                self.eat(); // . or ::
                let name = match &self.current_token.token {
                    Token::Identifier(n) => intern::intern_owned(n.clone()),
                    _ => panic!("Expected property name after dot at line {}", self.current_token.line),
                };
                let name_line = self.current_token.line;
                self.eat();
                expr = self.make_expr(ExprKind::Index {
                    target: Box::new(expr),
                    index: Box::new(self.make_expr(ExprKind::String(name), name_line)),
                }, line);
            } else if self.current_token.token == Token::LeftParen {
                self.eat(); // (
                let mut args = Vec::new();
                if self.current_token.token != Token::RightParen {
                    loop {
                        args.push(self.parse_expression());
                        if self.current_token.token == Token::Comma {
                            self.eat();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(Token::RightParen);

                // Check for Block { |params| body }
                let block = if self.current_token.token == Token::LeftBrace {
                    self.eat(); // {
                    let mut params = Vec::new();
                    if self.current_token.token == Token::Pipe {
                        self.eat(); // |
                        loop {
                            let is_ref = if self.current_token.token == Token::Ampersand {
                                self.eat();
                                true
                            } else {
                                false
                            };
                            match &self.current_token.token {
                                Token::Identifier(p) => params.push((intern::intern_symbol_owned(p.clone()), is_ref)),
                                _ => panic!("Expected param name at line {}", self.current_token.line),
                            }
                            self.eat();
                            if self.current_token.token == Token::Comma {
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

                expr = self.make_expr(ExprKind::Call {
                    function: Box::new(expr),
                    args,
                    block,
                    inlined_body: Rc::new(RefCell::new(None)),
                }, line);
            } else {
                break;
            }
        }
        expr
    }

    fn parse_atom(&mut self) -> Expr
    {
        let line = self.current_token.line;
        match self.current_token.token.clone()
        {
            Token::Integer(i) =>
            {
                self.eat();
                self.make_expr(ExprKind::Integer(i), line)
            }
            Token::Float { value, kind } =>
            {
                self.eat();
                self.make_expr(ExprKind::Float { value, kind }, line)
            }
            Token::StringLiteral(s) =>
            {
                self.eat();
                self.make_expr(ExprKind::String(intern::intern_owned(s)), line)
            }
            Token::CommandLiteral(c) =>
            {
                self.eat();
                self.make_expr(ExprKind::Shell(intern::intern_owned(c)), line)
            }
            Token::True =>
            {
                self.eat();
                self.make_expr(ExprKind::Boolean(true), line)
            }
            Token::False =>
            {
                self.eat();
                self.make_expr(ExprKind::Boolean(false), line)
            }
            Token::Identifier(name) =>
            {
                let name = intern::intern_symbol_owned(name);
                self.eat();

                // Legacy built-in support
                let name_str = intern::symbol_name(name);
                if name_str.as_str() == "puts" || name_str.as_str() == "print" || name_str.as_str() == "write_file" || name_str.as_str() == "read_file" || name_str.as_str() == "len"
                {
                    let mut args = Vec::new();
                    args.push(self.parse_expression());

                    while self.current_token.token == Token::Comma
                    {
                        self.eat();
                        args.push(self.parse_expression());
                    }

                    return self.make_expr(ExprKind::Call {
                        function: Box::new(self.make_expr(ExprKind::Identifier { name, slot: None }, line)),
                        args,
                        block: None,
                        inlined_body: Rc::new(RefCell::new(None)),
                    }, line);
                }
                self.make_expr(ExprKind::Identifier { name, slot: None }, line)
            }
            Token::Fn => self.parse_fn(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Loop => self.parse_loop(),
            Token::Use => self.parse_use(),
            Token::LeftBracket => self.parse_array(),
            Token::LeftBrace => {
                let mut temp_lexer = self.lexer.clone();
                if temp_lexer.next_token().token == Token::Pipe {
                    self.parse_closure_literal()
                } else {
                    self.parse_map()
                }
            },
            Token::LeftParen => {
                self.eat();
                if self.current_token.token == Token::RightParen {
                    self.eat();
                    return self.make_expr(ExprKind::Nil, line);
                }
                let expr = self.parse_expression();
                self.expect(Token::RightParen);
                expr
            },
            Token::Yield => {
                self.eat();
                let mut args = Vec::new();
                if self.current_token.token == Token::LeftParen {
                     self.eat();
                     if self.current_token.token != Token::RightParen {
                         loop {
                             args.push(self.parse_expression());
                             if self.current_token.token == Token::Comma {
                                 self.eat();
                             } else {
                                 break;
                             }
                         }
                     }
                     self.expect(Token::RightParen);
                } else {
                    // Variadic-ish greedy parsing
                    while self.current_token.token != Token::EOF &&
                          self.current_token.token != Token::End &&
                          self.current_token.token != Token::Else &&
                          self.current_token.token != Token::Elif &&
                          self.current_token.token != Token::RightBrace &&
                          self.current_token.token != Token::RightParen
                    {
                        args.push(self.parse_expression());
                        if self.current_token.token == Token::Comma {
                            self.eat();
                        } else {
                            break;
                        }
                    }
                }
                self.make_expr(ExprKind::Yield(args), line)
            }
            Token::Ampersand => {
                self.eat();
                match &self.current_token.token {
                    Token::Identifier(name) => {
                        let n = intern::intern_symbol_owned(name.clone());
                        self.eat();
                        self.make_expr(ExprKind::Reference(n), line)
                    },
                    _ => panic!("Expected identifier after & at line {}", self.current_token.line),
                }
            }
            _ => panic!("Unexpected token: {:?} at line {}", self.current_token.token, self.current_token.line),
        }
    }

    fn parse_array(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // [
        let mut elements = Vec::new();
        if self.current_token.token != Token::RightBracket {
            let first = self.parse_expression();
            if self.current_token.token == Token::Semicolon {
                self.eat(); // ;
                let size = self.parse_expression();
                self.expect(Token::RightBracket);
                return self.make_expr(ExprKind::ArrayGenerator {
                    generator: Box::new(first),
                    size: Box::new(size),
                }, line);
            }
            
            elements.push(first);
            if self.current_token.token == Token::Comma {
                self.eat();
                loop {
                    elements.push(self.parse_expression());
                    if self.current_token.token == Token::Comma {
                        self.eat();
                    } else {
                        break;
                    }
                }
            }
        }
        self.expect(Token::RightBracket);
        self.make_expr(ExprKind::Array(elements), line)
    }

    fn parse_map(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // {
        let mut entries = Vec::new();
        if self.current_token.token != Token::RightBrace {
            loop {
                let key = self.parse_expression();
                self.expect(Token::Colon);
                let value = self.parse_expression();
                entries.push((key, value));
                
                if self.current_token.token == Token::Comma {
                    self.eat();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RightBrace);
        self.make_expr(ExprKind::Map(entries), line)
    }

    fn parse_use(&mut self) -> Expr {
        let line = self.current_token.line;
        self.eat(); // eat 'use'

        let mut path = Vec::new();
        match &self.current_token.token {
            Token::Identifier(n) => {
                path.push(intern::intern_symbol_owned(n.clone()));
                self.eat();
            }
            _ => panic!("Expected module name after use at line {}", self.current_token.line),
        }

        while self.current_token.token == Token::ColonColon {
            self.eat(); // ::
            match &self.current_token.token {
                Token::Identifier(n) => {
                    path.push(intern::intern_symbol_owned(n.clone()));
                    self.eat();
                }
                _ => panic!("Expected module name after :: at line {}", self.current_token.line),
            }
        }

        self.make_expr(ExprKind::Use(path), line)
    }

    fn parse_fn(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'fn'

        let name = if let Token::Identifier(n) = &self.current_token.token {
            let name = intern::intern_symbol_owned(n.clone());
            self.eat();
            Some(name)
        } else {
            None
        };

        self.expect(Token::LeftParen);
        let mut params = Vec::new();
        if self.current_token.token != Token::RightParen
        {
            loop
            {
                let is_ref = if self.current_token.token == Token::Ampersand {
                    self.eat();
                    true
                } else {
                    false
                };

                match &self.current_token.token
                {
                    Token::Identifier(arg) => params.push((intern::intern_symbol_owned(arg.clone()), is_ref)),
                    _ => panic!("Expected parameter name at line {}", self.current_token.line),
                }
                self.eat();
                if self.current_token.token == Token::Comma
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

        if let Some(name) = name {
            self.make_expr(ExprKind::FunctionDef {
                name,
                params,
                body: Box::new(body),
                slots: None,
            }, line)
        } else {
            self.make_expr(ExprKind::AnonymousFunction {
                params,
                body: Box::new(body),
                slots: None,
            }, line)
        }
    }

    fn parse_if(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'if' (or 'elif' if called recursively)

        let condition = self.parse_expression();
        let then_branch = self.parse_block();

        let else_branch = match self.current_token.token
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
            _ => panic!("Expected elif, else, or end at line {}", self.current_token.line),
        };
        self.make_expr(ExprKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        }, line)
    }

    fn parse_while(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'while'

        let condition = self.parse_expression();
        let body = self.parse_block();

        self.expect(Token::End);

        self.make_expr(ExprKind::While {
            condition: Box::new(condition),
            body: Box::new(body),
        }, line)
    }

    fn parse_for(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'for'

        // Expect variable name
        let var_name = match &self.current_token.token {
            Token::Identifier(name) => intern::intern_symbol_owned(name.clone()),
            _ => panic!("Expected identifier after 'for' at line {}", self.current_token.line),
        };
        self.eat();

        self.expect(Token::In);

        let iterable = self.parse_expression();

        let body = self.parse_block();
        self.expect(Token::End);

        self.make_expr(ExprKind::For {
            var: var_name,
            var_slot: None,
            iterable: Box::new(iterable),
            body: Box::new(body),
        }, line)
    }

    fn parse_loop(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'loop'

        let count = self.parse_expression();

        let var = if self.current_token.token == Token::Pipe {
            self.eat(); // |
            let name = match &self.current_token.token {
                Token::Identifier(n) => intern::intern_symbol_owned(n.clone()),
                _ => panic!("Expected loop variable name at line {}", self.current_token.line),
            };
            self.eat();
            self.expect(Token::Pipe);
            Some(name)
        } else {
            None
        };

        let body = self.parse_block();
        self.expect(Token::End);

        self.make_expr(ExprKind::Loop {
            count: Box::new(count),
            var,
            var_slot: None,
            body: Box::new(body),
        }, line)
    }

    fn parse_block(&mut self) -> Expr
    {
        let line = self.current_token.line;
        let mut statements = Vec::new();

        while self.current_token.token != Token::End
            && self.current_token.token != Token::Else
            && self.current_token.token != Token::Elif
            && self.current_token.token != Token::RightBrace // Handle block end }
            && self.current_token.token != Token::EOF
        {
            statements.push(self.parse_assignment());
        }

        if statements.len() == 1
        {
            statements.pop().unwrap()
        }
        else
        {
            self.make_expr(ExprKind::Block(statements), line)
        }
    }

    fn parse_closure_literal(&mut self) -> Expr {
        let line = self.current_token.line;
        self.eat(); // {
        let mut params = Vec::new();
        if self.current_token.token == Token::Pipe {
            self.eat(); // |
            loop {
                let is_ref = if self.current_token.token == Token::Ampersand {
                    self.eat();
                    true
                } else {
                    false
                };
                match &self.current_token.token {
                    Token::Identifier(p) => params.push((intern::intern_symbol_owned(p.clone()), is_ref)),
                    _ => panic!("Expected param name at line {}", self.current_token.line),
                }
                self.eat();
                if self.current_token.token == Token::Comma {
                    self.eat();
                } else {
                    break;
                }
            }
            self.expect(Token::Pipe);
        }
        let body = self.parse_block();
        self.expect(Token::RightBrace);
        self.make_expr(ExprKind::AnonymousFunction { params, body: Box::new(body), slots: None }, line)
    }
}
