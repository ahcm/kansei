use crate::ast::{Closure, Expr, ExprKind, FormatPart, FormatSpec, Op, Param, ParamType, TypeRef};
use crate::intern;
use crate::intern::SymbolId;
use crate::lexer::{Lexer, Span, Token};
use std::cell::RefCell;
use std::rc::Rc;

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
            panic!(
                "Syntax Error at line {}: Expected {:?}, but found {:?}",
                self.current_token.line, token, self.current_token.token
            );
        }
    }

    fn make_expr(&self, kind: ExprKind, line: usize) -> Expr
    {
        Expr { kind, line }
    }

    pub fn parse(&mut self) -> Expr
    {
        self.parse_block()
    }

    fn parse_assignment(&mut self) -> Expr
    {
        let expr = self.parse_expression();

        if self.current_token.token == Token::Equals
        {
            let line = self.current_token.line;
            self.eat(); // =
            let value = self.parse_expression();

            return match expr.kind
            {
                ExprKind::Identifier { name, .. } => self.make_expr(
                    ExprKind::Assignment {
                        name,
                        value: Box::new(value),
                        slot: None,
                    },
                    line,
                ),
                ExprKind::Index { target, index } => self.make_expr(
                    ExprKind::IndexAssignment {
                        target,
                        index,
                        value: Box::new(value),
                    },
                    line,
                ),
                _ => panic!("Invalid assignment target at line {}", line),
            };
        }
        expr
    }

    fn parse_expression(&mut self) -> Expr
    {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr
    {
        let mut left = self.parse_and();

        while matches!(self.current_token.token, Token::Or | Token::OrOr)
        {
            let line = self.current_token.line;
            let is_bool = matches!(self.current_token.token, Token::OrOr);
            self.eat();
            let right = self.parse_and();
            left = if is_bool
            {
                self.make_expr(
                    ExprKind::OrBool {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    line,
                )
            }
            else
            {
                self.make_expr(
                    ExprKind::Or {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    line,
                )
            };
        }
        left
    }

    fn parse_and(&mut self) -> Expr
    {
        let mut left = self.parse_comparison();

        while matches!(self.current_token.token, Token::And | Token::AndAnd)
        {
            let line = self.current_token.line;
            let is_bool = matches!(self.current_token.token, Token::AndAnd);
            self.eat();
            let right = self.parse_comparison();
            left = if is_bool
            {
                self.make_expr(
                    ExprKind::AndBool {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    line,
                )
            }
            else
            {
                self.make_expr(
                    ExprKind::And {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    line,
                )
            };
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr
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
            left = self.make_expr(
                ExprKind::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                line,
            );
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
            left = self.make_expr(
                ExprKind::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                line,
            );
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
            left = self.make_expr(
                ExprKind::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                line,
            );
        }
        left
    }

    fn parse_factor(&mut self) -> Expr
    {
        if self.current_token.token == Token::Not
        {
            let line = self.current_token.line;
            self.eat();
            let inner = self.parse_factor();
            return self.make_expr(ExprKind::Not(Box::new(inner)), line);
        }
        if self.current_token.token == Token::Clone
        {
            let line = self.current_token.line;
            self.eat();
            let inner = self.parse_factor();
            return self.make_expr(ExprKind::Clone(Box::new(inner)), line);
        }
        if self.current_token.token == Token::Percent
        {
            let line = self.current_token.line;
            self.eat();
            let inner = self.parse_factor();
            return self.make_expr(ExprKind::EnvFreeze(Box::new(inner)), line);
        }

        let mut expr = self.parse_atom();

        loop
        {
            let line = self.current_token.line;
            if self.current_token.token == Token::LeftBracket
            {
                self.eat(); // [
                let first = self.parse_expression();
                if self.current_token.token == Token::Comma
                {
                    // Slice syntax: target[start, end]
                    self.eat(); // ,
                    let second = self.parse_expression();
                    self.expect(Token::RightBracket);
                    expr = self.make_expr(
                        ExprKind::Slice {
                            target: Box::new(expr),
                            start: Box::new(first),
                            end: Box::new(second),
                        },
                        line,
                    );
                }
                else
                {
                    // Regular index: target[index]
                    self.expect(Token::RightBracket);
                    expr = self.make_expr(
                        ExprKind::Index {
                            target: Box::new(expr),
                            index: Box::new(first),
                        },
                        line,
                    );
                }
            }
            else if self.current_token.token == Token::Dot
                || self.current_token.token == Token::ColonColon
            {
                self.eat(); // . or ::
                let name = match &self.current_token.token
                {
                    Token::Identifier(n) => intern::intern_owned(n.clone()),
                    token =>
                    {
                        if let Some(keyword) = token.keyword_as_identifier()
                        {
                            intern::intern_owned(keyword.to_string())
                        }
                        else
                        {
                            panic!(
                                "Expected property name after dot at line {}",
                                self.current_token.line
                            );
                        }
                    }
                };
                let name_line = self.current_token.line;
                self.eat();
                expr = self.make_expr(
                    ExprKind::Index {
                        target: Box::new(expr),
                        index: Box::new(self.make_expr(ExprKind::String(name), name_line)),
                    },
                    line,
                );
            }
            else if self.current_token.token == Token::LeftParen
            {
                self.eat(); // (
                let mut args = Vec::new();
                if self.current_token.token != Token::RightParen
                {
                    loop
                    {
                        args.push(self.parse_expression());
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

                // Check for Block { |params| body }
                let block = if self.current_token.token == Token::LeftBrace
                {
                    self.eat(); // {
                    let params = self.parse_pipe_params();
                    let body = self.parse_block();
                    self.expect(Token::RightBrace);
                    Some(Closure {
                        params,
                        body: Box::new(body),
                    })
                }
                else
                {
                    None
                };

                expr = self.make_expr(
                    ExprKind::Call {
                        function: Box::new(expr),
                        args,
                        block,
                        inlined_body: Rc::new(RefCell::new(None)),
                    },
                    line,
                );
            }
            else if self.current_token.token == Token::LeftBrace
            {
                if let ExprKind::Identifier { name, .. } = &expr.kind
                {
                    let mut temp_lexer = self.lexer.clone();
                    let next = temp_lexer.next_token().token;
                    let is_struct = if next == Token::Pipe
                    {
                        false
                    }
                    else if next == Token::RightBrace
                    {
                        true
                    }
                    else if matches!(next, Token::Identifier(_))
                    {
                        temp_lexer.next_token().token == Token::Colon
                    }
                    else
                    {
                        false
                    };
                    if is_struct
                    {
                        self.eat(); // {
                        let fields = self.parse_struct_literal_fields();
                        self.expect(Token::RightBrace);
                        expr = self.make_expr(
                            ExprKind::StructLiteral {
                                name: *name,
                                fields,
                            },
                            line,
                        );
                        continue;
                    }
                }

                // Call with block and no parentheses: foo { |x| ... }
                self.eat(); // {
                let params = self.parse_pipe_params();
                let body = self.parse_block();
                self.expect(Token::RightBrace);
                expr = self.make_expr(
                    ExprKind::Call {
                        function: Box::new(expr),
                        args: Vec::new(),
                        block: Some(Closure {
                            params,
                            body: Box::new(body),
                        }),
                        inlined_body: Rc::new(RefCell::new(None)),
                    },
                    line,
                );
            }
            else if matches!(self.current_token.token, Token::Identifier(_))
            {
                let mut temp_lexer = self.lexer.clone();
                if temp_lexer.next_token().token == Token::LeftBrace
                {
                    let name = match &self.current_token.token
                    {
                        Token::Identifier(n) => intern::intern_owned(n.clone()),
                        _ => unreachable!(),
                    };
                    self.eat(); // identifier
                    self.eat(); // {
                    let params = self.parse_pipe_params();
                    let body = self.parse_block();
                    self.expect(Token::RightBrace);
                    let index = self.make_expr(ExprKind::String(name), line);
                    let function = self.make_expr(
                        ExprKind::Index {
                            target: Box::new(expr),
                            index: Box::new(index),
                        },
                        line,
                    );
                    expr = self.make_expr(
                        ExprKind::Call {
                            function: Box::new(function),
                            args: Vec::new(),
                            block: Some(Closure {
                                params,
                                body: Box::new(body),
                            }),
                            inlined_body: Rc::new(RefCell::new(None)),
                        },
                        line,
                    );
                }
                else if let Token::Identifier(n) = &self.current_token.token
                {
                    if self.current_token.line == expr.line && (n == "keys" || n == "values")
                    {
                        let name = intern::intern_owned(n.clone());
                        self.eat(); // identifier
                        expr = self.make_expr(
                            ExprKind::Index {
                                target: Box::new(expr),
                                index: Box::new(self.make_expr(ExprKind::String(name), line)),
                            },
                            line,
                        );
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
            else
            {
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
            Token::Integer { value, kind } =>
            {
                self.eat();
                self.make_expr(ExprKind::Integer { value, kind }, line)
            }
            Token::Unsigned { value, kind } =>
            {
                self.eat();
                self.make_expr(ExprKind::Unsigned { value, kind }, line)
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
            Token::FormatString(s) =>
            {
                self.eat();
                self.parse_format_string(s, line)
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
            Token::Nil =>
            {
                self.eat();
                self.make_expr(ExprKind::Nil, line)
            }
            Token::Identifier(name) =>
            {
                let name = intern::intern_symbol_owned(name);
                self.eat();

                // Legacy built-in support (for builtins that don't require parentheses)
                let name_str = intern::symbol_name(name);
                if name_str.as_str() == "puts"
                    || name_str.as_str() == "print"
                    || name_str.as_str() == "write_file"
                    || name_str.as_str() == "read_file"
                {
                    let mut args = Vec::new();
                    args.push(self.parse_expression());

                    while self.current_token.token == Token::Comma
                    {
                        self.eat();
                        args.push(self.parse_expression());
                    }

                    return self.make_expr(
                        ExprKind::Call {
                            function: Box::new(
                                self.make_expr(ExprKind::Identifier { name, slot: None }, line),
                            ),
                            args,
                            block: None,
                            inlined_body: Rc::new(RefCell::new(None)),
                        },
                        line,
                    );
                }
                self.make_expr(ExprKind::Identifier { name, slot: None }, line)
            }
            Token::Fn => self.parse_fn(),
            Token::Struct => self.parse_struct_def(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Loop => self.parse_loop(),
            Token::Collect => self.parse_collect(),
            Token::Use => self.parse_use(),
            Token::Import => self.parse_import(),
            Token::Load => self.parse_load(),
            Token::At => self.parse_scoped_public(),
            Token::LeftBracket => self.parse_array(),
            Token::LeftBrace =>
            {
                let mut temp_lexer = self.lexer.clone();
                if temp_lexer.next_token().token == Token::Pipe
                {
                    self.parse_closure_literal()
                }
                else
                {
                    self.parse_map()
                }
            }
            Token::LeftParen =>
            {
                self.eat();
                if self.current_token.token == Token::RightParen
                {
                    self.eat();
                    return self.make_expr(ExprKind::Nil, line);
                }
                let expr = self.parse_expression();
                self.expect(Token::RightParen);
                expr
            }
            Token::Yield =>
            {
                self.eat();
                let mut args = Vec::new();
                if self.current_token.token == Token::LeftParen
                {
                    self.eat();
                    if self.current_token.token != Token::RightParen
                    {
                        loop
                        {
                            args.push(self.parse_expression());
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
                }
                else
                {
                    // Variadic-ish greedy parsing
                    while self.current_token.token != Token::EOF
                        && self.current_token.token != Token::End
                        && self.current_token.token != Token::Else
                        && self.current_token.token != Token::Elif
                        && self.current_token.token != Token::RightBrace
                        && self.current_token.token != Token::RightParen
                    {
                        args.push(self.parse_expression());
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
                self.make_expr(ExprKind::Yield(args), line)
            }
            Token::Return =>
            {
                self.eat();
                // Check if there's an expression to return on the same line
                let value = if self.current_token.line == line
                    && self.current_token.token != Token::EOF
                    && self.current_token.token != Token::End
                    && self.current_token.token != Token::Else
                    && self.current_token.token != Token::Elif
                    && self.current_token.token != Token::RightBrace
                    && self.current_token.token != Token::RightParen
                {
                    Some(Box::new(self.parse_expression()))
                }
                else
                {
                    None
                };
                self.make_expr(ExprKind::Return(value), line)
            }
            Token::Ampersand =>
            {
                self.eat();
                match &self.current_token.token
                {
                    Token::Identifier(name) =>
                    {
                        let n = intern::intern_symbol_owned(name.clone());
                        self.eat();
                        self.make_expr(ExprKind::Reference(n), line)
                    }
                    _ => panic!("Expected identifier after & at line {}", self.current_token.line),
                }
            }
            _ => panic!(
                "Unexpected token: {:?} at line {}",
                self.current_token.token, self.current_token.line
            ),
        }
    }

    fn parse_array(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // [
        let mut elements = Vec::new();
        if self.current_token.token != Token::RightBracket
        {
            let first = self.parse_expression();
            if self.current_token.token == Token::Semicolon
            {
                self.eat(); // ;
                let size = self.parse_expression();
                self.expect(Token::RightBracket);
                return self.make_expr(
                    ExprKind::ArrayGenerator {
                        generator: Box::new(first),
                        size: Box::new(size),
                    },
                    line,
                );
            }

            elements.push(first);
            if self.current_token.token == Token::Comma
            {
                self.eat();
                loop
                {
                    if self.current_token.token == Token::RightBracket
                    {
                        break;
                    }
                    elements.push(self.parse_expression());
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
        }
        self.expect(Token::RightBracket);
        self.make_expr(ExprKind::Array(elements), line)
    }

    fn parse_map(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // {
        let mut entries = Vec::new();
        if self.current_token.token != Token::RightBrace
        {
            loop
            {
                let key = self.parse_expression();
                self.expect(Token::Colon);
                let value = self.parse_expression();
                entries.push((key, value));

                if self.current_token.token == Token::Comma
                {
                    self.eat();
                    if self.current_token.token == Token::RightBrace
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        }
        self.expect(Token::RightBrace);
        self.make_expr(ExprKind::Map(entries), line)
    }

    fn parse_struct_type_fields(&mut self) -> Vec<(SymbolId, TypeRef)>
    {
        let mut fields = Vec::new();
        if self.current_token.token != Token::RightBrace
        {
            loop
            {
                let field_name = match &self.current_token.token
                {
                    Token::Identifier(n) =>
                    {
                        let field = intern::intern_symbol_owned(n.clone());
                        self.eat();
                        field
                    }
                    _ => panic!("Expected field name at line {}", self.current_token.line),
                };
                self.expect(Token::Colon);
                let type_ref = self.parse_type_ref();
                fields.push((field_name, type_ref));
                if self.current_token.token == Token::Comma
                {
                    self.eat();
                    if self.current_token.token == Token::RightBrace
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        }
        fields
    }

    fn parse_struct_def(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // struct
        let name = match &self.current_token.token
        {
            Token::Identifier(n) =>
            {
                let name = intern::intern_symbol_owned(n.clone());
                self.eat();
                name
            }
            _ => panic!("Expected struct name at line {}", self.current_token.line),
        };
        self.expect(Token::LeftBrace);
        let fields = self.parse_struct_type_fields();
        self.expect(Token::RightBrace);
        self.make_expr(ExprKind::StructDef { name, fields }, line)
    }

    fn parse_struct_literal_fields(&mut self) -> Vec<(SymbolId, Expr)>
    {
        let mut fields = Vec::new();
        if self.current_token.token != Token::RightBrace
        {
            loop
            {
                let field_name = match &self.current_token.token
                {
                    Token::Identifier(n) =>
                    {
                        let field = intern::intern_symbol_owned(n.clone());
                        self.eat();
                        field
                    }
                    _ => panic!("Expected field name at line {}", self.current_token.line),
                };
                self.expect(Token::Colon);
                let value = self.parse_expression();
                fields.push((field_name, value));
                if self.current_token.token == Token::Comma
                {
                    self.eat();
                    if self.current_token.token == Token::RightBrace
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        }
        fields
    }

    fn parse_type_ref(&mut self) -> TypeRef
    {
        let mut path = Vec::new();
        match &self.current_token.token
        {
            Token::Identifier(n) =>
            {
                path.push(intern::intern_symbol_owned(n.clone()));
                self.eat();
            }
            _ => panic!("Expected type name at line {}", self.current_token.line),
        }
        while self.current_token.token == Token::ColonColon
        {
            self.eat();
            match &self.current_token.token
            {
                Token::Identifier(n) =>
                {
                    path.push(intern::intern_symbol_owned(n.clone()));
                    self.eat();
                }
                _ => panic!("Expected type name after :: at line {}", self.current_token.line),
            }
        }
        TypeRef { path }
    }

    fn parse_use(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'use'

        let mut path = Vec::new();
        match &self.current_token.token
        {
            Token::Identifier(n) =>
            {
                path.push(intern::intern_symbol_owned(n.clone()));
                self.eat();
            }
            _ => panic!("Expected module name after use at line {}", self.current_token.line),
        }

        while self.current_token.token == Token::ColonColon
        {
            self.eat(); // ::
            match &self.current_token.token
            {
                Token::Identifier(n) =>
                {
                    path.push(intern::intern_symbol_owned(n.clone()));
                    self.eat();
                }
                _ => panic!("Expected module name after :: at line {}", self.current_token.line),
            }
        }

        self.make_expr(ExprKind::Use(path), line)
    }

    fn parse_scoped_public(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat '@'
        let is_file = match &self.current_token.token
        {
            Token::Identifier(n) if n == "file" =>
            {
                self.eat();
                true
            }
            Token::Identifier(n) if n == "function" =>
            {
                self.eat();
                false
            }
            _ =>
            {
                panic!("Expected 'file' or 'function' after @ at line {}", self.current_token.line)
            }
        };
        let expr = self.parse_assignment();
        if is_file
        {
            self.make_expr(ExprKind::FilePublic(Box::new(expr)), line)
        }
        else
        {
            self.make_expr(ExprKind::FunctionPublic(Box::new(expr)), line)
        }
    }

    fn parse_import(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'import'

        let path = match &self.current_token.token
        {
            Token::StringLiteral(content) =>
            {
                let value = Rc::new(content.clone());
                self.eat();
                value
            }
            _ => panic!("Expected string literal after import at line {}", self.current_token.line),
        };

        let mut alias = None;
        if self.current_token.token == Token::As
        {
            self.eat(); // eat 'as'
            match &self.current_token.token
            {
                Token::Identifier(name) =>
                {
                    alias = Some(intern::intern_symbol_owned(name.clone()));
                    self.eat();
                }
                _ => panic!("Expected identifier after as at line {}", self.current_token.line),
            }
        }

        self.make_expr(ExprKind::Import { path, alias }, line)
    }

    fn parse_load(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'load'

        let mut path = Vec::new();
        match &self.current_token.token
        {
            Token::Identifier(n) =>
            {
                path.push(intern::intern_symbol_owned(n.clone()));
                self.eat();
            }
            _ => panic!("Expected module name after load at line {}", self.current_token.line),
        }

        while self.current_token.token == Token::ColonColon
        {
            self.eat(); // ::
            match &self.current_token.token
            {
                Token::Identifier(n) =>
                {
                    path.push(intern::intern_symbol_owned(n.clone()));
                    self.eat();
                }
                _ => panic!("Expected module name after :: at line {}", self.current_token.line),
            }
        }

        self.make_expr(ExprKind::Load(path), line)
    }

    fn parse_export(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'export'

        let mut namespace = Vec::new();
        match &self.current_token.token
        {
            Token::Identifier(n) =>
            {
                namespace.push(intern::intern_symbol_owned(n.clone()));
                self.eat();
            }
            _ => panic!("Expected namespace after export at line {}", self.current_token.line),
        }

        loop
        {
            if self.current_token.token != Token::ColonColon
            {
                panic!("Expected :: after export namespace at line {}", self.current_token.line);
            }
            self.eat(); // ::
            if self.current_token.token == Token::LeftBracket
            {
                break;
            }
            match &self.current_token.token
            {
                Token::Identifier(n) =>
                {
                    namespace.push(intern::intern_symbol_owned(n.clone()));
                    self.eat();
                }
                _ => panic!("Expected namespace segment at line {}", self.current_token.line),
            }
        }

        self.expect(Token::LeftBracket);
        let mut names = Vec::new();
        if self.current_token.token != Token::RightBracket
        {
            loop
            {
                match &self.current_token.token
                {
                    Token::Identifier(n) =>
                    {
                        names.push(intern::intern_symbol_owned(n.clone()));
                        self.eat();
                    }
                    _ => panic!("Expected export name at line {}", self.current_token.line),
                }
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
        self.expect(Token::RightBracket);

        self.make_expr(ExprKind::Export { namespace, names }, line)
    }

    fn parse_fn(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'fn'

        let base_name = if let Token::Identifier(n) = &self.current_token.token
        {
            let name = intern::intern_symbol_owned(n.clone());
            self.eat();
            Some(name)
        }
        else
        {
            None
        };

        let mut method_target = None;
        let mut method_name = None;
        let name = if let Some(name) = base_name
        {
            if self.current_token.token == Token::Dot
            {
                self.eat();
                let method = if let Token::Identifier(n) = &self.current_token.token
                {
                    let method = intern::intern_symbol_owned(n.clone());
                    self.eat();
                    method
                }
                else
                {
                    panic!("Expected method name after '.' at line {}", self.current_token.line);
                };
                method_target = Some(name);
                method_name = Some(method);
                None
            }
            else
            {
                Some(name)
            }
        }
        else
        {
            None
        };

        self.expect(Token::LeftParen);
        let mut params = Vec::new();
        if self.current_token.token != Token::RightParen
        {
            loop
            {
                let param = self.parse_param();
                params.push(param);
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

        if let Some(type_name) = method_target
        {
            let method_name = method_name.expect("Expected method name");
            self.make_expr(
                ExprKind::MethodDef {
                    type_name,
                    name: method_name,
                    params,
                    body: Box::new(body),
                    slots: None,
                },
                line,
            )
        }
        else if let Some(name) = name
        {
            self.make_expr(
                ExprKind::FunctionDef {
                    name,
                    params,
                    body: Box::new(body),
                    slots: None,
                },
                line,
            )
        }
        else
        {
            self.make_expr(
                ExprKind::AnonymousFunction {
                    params,
                    body: Box::new(body),
                    slots: None,
                },
                line,
            )
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
            Token::Elif => Some(Box::new(self.parse_if())),
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
        self.make_expr(
            ExprKind::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            },
            line,
        )
    }

    fn parse_while(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'while'

        let condition = self.parse_expression();
        let body = self.parse_block();

        self.expect(Token::End);

        self.make_expr(
            ExprKind::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            line,
        )
    }

    fn parse_for(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'for'

        // Expect variable name
        let var_name = match &self.current_token.token
        {
            Token::Identifier(name) => intern::intern_symbol_owned(name.clone()),
            _ => panic!("Expected identifier after 'for' at line {}", self.current_token.line),
        };
        self.eat();

        self.expect(Token::In);

        let iterable = self.parse_expression();

        let body = self.parse_block();
        self.expect(Token::End);

        self.make_expr(
            ExprKind::For {
                var: var_name,
                var_slot: None,
                iterable: Box::new(iterable),
                body: Box::new(body),
            },
            line,
        )
    }

    fn parse_loop(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'loop'

        let count = self.parse_expression();

        let var = if self.current_token.token == Token::Pipe
        {
            self.eat(); // |
            let name = match &self.current_token.token
            {
                Token::Identifier(n) => intern::intern_symbol_owned(n.clone()),
                _ => panic!("Expected loop variable name at line {}", self.current_token.line),
            };
            self.eat();
            self.expect(Token::Pipe);
            Some(name)
        }
        else
        {
            None
        };

        let body = self.parse_block();
        self.expect(Token::End);

        self.make_expr(
            ExprKind::Loop {
                count: Box::new(count),
                var,
                var_slot: None,
                body: Box::new(body),
            },
            line,
        )
    }

    fn parse_collect(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // eat 'collect'

        let count = self.parse_expression();

        let into = if self.current_token.token == Token::Into
        {
            self.eat(); // eat 'into'
            Some(Box::new(self.parse_expression()))
        }
        else
        {
            None
        };

        let var = if self.current_token.token == Token::Pipe
        {
            self.eat(); // |
            let name = match &self.current_token.token
            {
                Token::Identifier(n) => intern::intern_symbol_owned(n.clone()),
                _ => panic!("Expected collect variable name at line {}", self.current_token.line),
            };
            self.eat();
            self.expect(Token::Pipe);
            Some(name)
        }
        else
        {
            None
        };

        let body = self.parse_block();
        self.expect(Token::End);

        self.make_expr(
            ExprKind::Collect {
                count: Box::new(count),
                into,
                var,
                var_slot: None,
                body: Box::new(body),
            },
            line,
        )
    }

    fn parse_block(&mut self) -> Expr
    {
        let line = self.current_token.line;
        let mut statements = Vec::new();
        let mut seen_export = false;

        while self.current_token.token != Token::End
            && self.current_token.token != Token::Else
            && self.current_token.token != Token::Elif
            && self.current_token.token != Token::RightBrace // Handle block end }
            && self.current_token.token != Token::EOF
        {
            if self.current_token.token == Token::Export
            {
                if seen_export || !statements.is_empty()
                {
                    panic!(
                        "export must appear at the top of the file at line {}",
                        self.current_token.line
                    );
                }
                seen_export = true;
                statements.push(self.parse_export());
                continue;
            }
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

    fn parse_closure_literal(&mut self) -> Expr
    {
        let line = self.current_token.line;
        self.eat(); // {
        let params = self.parse_pipe_params();
        let body = self.parse_block();
        self.expect(Token::RightBrace);
        self.make_expr(
            ExprKind::AnonymousFunction {
                params,
                body: Box::new(body),
                slots: None,
            },
            line,
        )
    }

    fn parse_pipe_params(&mut self) -> Vec<Param>
    {
        let mut params = Vec::new();
        if self.current_token.token == Token::Pipe
        {
            self.eat(); // |
            loop
            {
                let param = self.parse_param();
                params.push(param);
                if self.current_token.token == Token::Comma
                {
                    self.eat();
                }
                else
                {
                    break;
                }
            }
            self.expect(Token::Pipe);
        }
        params
    }

    fn parse_param(&mut self) -> Param
    {
        let is_ref = if self.current_token.token == Token::Ampersand
        {
            self.eat();
            true
        }
        else
        {
            false
        };

        let name = match &self.current_token.token
        {
            Token::Identifier(arg) => intern::intern_symbol_owned(arg.clone()),
            _ => panic!("Expected parameter name at line {}", self.current_token.line),
        };
        self.eat();

        let type_ann = if self.current_token.token == Token::LeftBrace
        {
            self.eat(); // {
            let fields = self.parse_struct_type_fields();
            self.expect(Token::RightBrace);
            Some(ParamType::Struct(fields))
        }
        else
        {
            None
        };

        Param {
            name,
            is_ref,
            type_ann,
        }
    }

    fn parse_format_string(&mut self, content: String, line: usize) -> Expr
    {
        let mut parts: Vec<FormatPart> = Vec::new();
        let mut literal = String::new();
        let chars: Vec<char> = content.chars().collect();
        let mut i = 0;
        while i < chars.len()
        {
            let ch = chars[i];
            if ch == '{'
            {
                if i + 1 < chars.len() && chars[i + 1] == '{'
                {
                    literal.push('{');
                    i += 2;
                    continue;
                }
                if !literal.is_empty()
                {
                    parts.push(FormatPart::Literal(intern::intern_owned(literal.clone())));
                    literal.clear();
                }
                let start = i + 1;
                let mut end = start;
                while end < chars.len() && chars[end] != '}'
                {
                    end += 1;
                }
                if end >= chars.len()
                {
                    panic!("Unclosed format string expression at line {}", line);
                }
                let expr_slice: String = chars[start..end].iter().collect();
                let (expr_str, spec) = self.split_format_expr(&expr_slice, line);
                if expr_str.trim().is_empty()
                {
                    panic!("Empty format string expression at line {}", line);
                }
                let lexer = Lexer::new(&expr_str);
                let mut parser = Parser::new(lexer);
                let expr = parser.parse();
                parts.push(FormatPart::Expr {
                    expr: Box::new(expr),
                    spec,
                });
                i = end + 1;
            }
            else if ch == '}'
            {
                if i + 1 < chars.len() && chars[i + 1] == '}'
                {
                    literal.push('}');
                    i += 2;
                }
                else
                {
                    panic!("Unmatched '}}' in format string at line {}", line);
                }
            }
            else
            {
                literal.push(ch);
                i += 1;
            }
        }
        if !literal.is_empty()
        {
            parts.push(FormatPart::Literal(intern::intern_owned(literal)));
        }
        self.make_expr(ExprKind::FormatString(parts), line)
    }

    fn split_format_expr(&self, input: &str, line: usize) -> (String, Option<FormatSpec>)
    {
        let mut depth_paren = 0usize;
        let mut depth_brack = 0usize;
        let mut depth_brace = 0usize;
        let mut in_string = false;
        let mut string_delim = '\0';
        let mut split_at: Option<usize> = None;
        for (idx, ch) in input.chars().enumerate()
        {
            if in_string
            {
                if ch == string_delim
                {
                    in_string = false;
                }
                continue;
            }
            match ch
            {
                '"' | '`' =>
                {
                    in_string = true;
                    string_delim = ch;
                }
                '(' => depth_paren += 1,
                ')' => depth_paren = depth_paren.saturating_sub(1),
                '[' => depth_brack += 1,
                ']' => depth_brack = depth_brack.saturating_sub(1),
                '{' => depth_brace += 1,
                '}' => depth_brace = depth_brace.saturating_sub(1),
                ':' if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
                {
                    split_at = Some(idx);
                    break;
                }
                _ =>
                {}
            }
        }

        if let Some(idx) = split_at
        {
            let expr = input[..idx].trim().to_string();
            let spec_str = input[idx + 1..].trim();
            if spec_str.is_empty()
            {
                panic!("Empty format specifier at line {}", line);
            }
            let spec = self.parse_format_spec(spec_str, line);
            (expr, Some(spec))
        }
        else
        {
            (input.trim().to_string(), None)
        }
    }

    fn parse_format_spec(&self, spec: &str, line: usize) -> FormatSpec
    {
        if let Some(rest) = spec.strip_prefix('.')
        {
            if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit())
            {
                panic!("Invalid format precision at line {}", line);
            }
            let precision = rest
                .parse::<usize>()
                .unwrap_or_else(|_| panic!("Invalid format precision at line {}", line));
            return FormatSpec {
                precision: Some(precision),
            };
        }
        panic!("Unsupported format specifier at line {}", line);
    }
}
