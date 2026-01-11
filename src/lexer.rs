#[derive(Debug, Clone, PartialEq)]
pub enum Token
{
    Integer(i64),
    Float(f64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    StringLiteral(String),  // "hello"
    CommandLiteral(String), // `ls`
    EOF,
    EqualEqual,
    BangEqual,
    Less,
    Greater,
    If,
    Else,
    Elif,
    While,
    For,
    In,
    Yield,
    Pipe,
    End,
    True,
    False,
    Comma,
    Fn,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Colon,
    Dot,
    Ampersand,
    Semicolon,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub token: Token,
    pub line: usize,
}

#[derive(Clone)]
pub struct Lexer
{
    input: Vec<char>,
    position: usize,
    pub line: usize,
}

impl Lexer
{
    pub fn new(input: &str) -> Self
    {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
        }
    }

    pub fn next_token(&mut self) -> Span
    {
        self.skip_whitespace();

        let start_line = self.line;

        if self.position >= self.input.len()
        {
            return Span { token: Token::EOF, line: start_line };
        }

        let ch = self.input[self.position];

        let token = match ch
        {
            '#' =>
            {
                self.skip_comment();
                return self.next_token(); // Recursively call to get the *actual* next token
            }
            '"' => self.read_string('"'), // standard strings
            '`' => self.read_string('`'), // shell commands
            '+' =>
            {
                self.position += 1;
                Token::Plus
            }
            '-' =>
            {
                self.position += 1;
                Token::Minus
            }
            '*' =>
            {
                self.position += 1;
                Token::Star
            }
            '/' =>
            {
                self.position += 1;
                Token::Slash
            }
            '=' =>
            {
                // Check for '=='
                if self.peek() == '='
                {
                    self.position += 2;
                    Token::EqualEqual
                }
                else
                {
                    self.position += 1;
                    Token::Equals
                }
            }
            '!' =>
            {
                if self.peek() == '='
                {
                    self.position += 2;
                    Token::BangEqual
                }
                else
                {
                    panic!("Unexpected char: !")
                }
            }
            '<' =>
            {
                self.position += 1;
                Token::Less
            }
            '>' =>
            {
                self.position += 1;
                Token::Greater
            }
            '0'..='9' => self.read_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),
            ',' =>
            {
                self.position += 1;
                Token::Comma
            }
            '(' =>
            {
                self.position += 1;
                Token::LeftParen
            }
            ')' =>
            {
                self.position += 1;
                Token::RightParen
            }
            '[' =>
            {
                self.position += 1;
                Token::LeftBracket
            }
            ']' =>
            {
                self.position += 1;
                Token::RightBracket
            }
            '{' =>
            {
                self.position += 1;
                Token::LeftBrace
            }
            '}' =>
            {
                self.position += 1;
                Token::RightBrace
            }
            ':' =>
            {
                self.position += 1;
                Token::Colon
            }
            '.' =>
            {
                self.position += 1;
                Token::Dot
            }
            '|' =>
            {
                self.position += 1;
                Token::Pipe
            }
            '&' =>
            {
                self.position += 1;
                Token::Ampersand
            }
            ';' =>
            {
                self.position += 1;
                Token::Semicolon
            }
            _ =>
            {
                // Ignore unknown chars for this MVP
                self.position += 1;
                return self.next_token();
            }
        };
        
        Span { token, line: start_line }
    }

    fn skip_comment(&mut self)
    {
        while self.position < self.input.len() && self.input[self.position] != '\n'
        {
            self.position += 1;
        }
    }

    fn skip_whitespace(&mut self)
    {
        while self.position < self.input.len() && self.input[self.position].is_whitespace()
        {
            if self.input[self.position] == '\n' {
                self.line += 1;
            }
            self.position += 1;
        }
    }

    fn read_number(&mut self) -> Token
    {
        let mut int_val: i64 = 0;
        let mut overflowed = false;
        while self.position < self.input.len() && self.input[self.position].is_digit(10)
        {
            let digit = (self.input[self.position] as u8 - b'0') as i64;
            if let Some(next) = int_val.checked_mul(10).and_then(|v| v.checked_add(digit)) {
                int_val = next;
            } else {
                overflowed = true;
            }
            self.position += 1;
        }

        if self.position < self.input.len() && self.input[self.position] == '.' {
            if self.position + 1 < self.input.len() && self.input[self.position + 1].is_digit(10) {
                self.position += 1; // Consume dot
                let mut frac_val: i64 = 0;
                let mut divisor: f64 = 1.0;
                while self.position < self.input.len() && self.input[self.position].is_digit(10) {
                    let digit = (self.input[self.position] as u8 - b'0') as i64;
                    if let Some(next) = frac_val.checked_mul(10).and_then(|v| v.checked_add(digit)) {
                        frac_val = next;
                    }
                    divisor *= 10.0;
                    self.position += 1;
                }
                let int_part = if overflowed { 0 } else { int_val };
                return Token::Float(int_part as f64 + (frac_val as f64 / divisor));
            }
        }

        if overflowed {
            Token::Integer(0)
        } else {
            Token::Integer(int_val)
        }
    }

    fn read_identifier(&mut self) -> Token
    {
        let start = self.position;
        while self.position < self.input.len()
            && (self.input[self.position].is_alphanumeric() || self.input[self.position] == '_')
        {
            self.position += 1;
        }
        let ident: String = self.input[start..self.position].iter().collect();
        match ident.as_str()
        {
            "if" => Token::If,
            "else" => Token::Else,
            "elif" => Token::Elif,
            "end" => Token::End,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "yield" => Token::Yield,
            "true" => Token::True,
            "false" => Token::False,
            "fn" => Token::Fn,
            _ => Token::Identifier(ident),
        }
    }

    // read until the closing quote/backtick
    fn read_string(&mut self, quote_char: char) -> Token
    {
        self.position += 1; // Skip the opening quote
        let start = self.position;

        while self.position < self.input.len() && self.input[self.position] != quote_char
        {
            if self.input[self.position] == '\n' {
                self.line += 1;
            }
            self.position += 1;
        }

        let content: String = self.input[start..self.position].iter().collect();

        // Consume the closing quote if we aren't at EOF
        if self.position < self.input.len()
        {
            self.position += 1;
        }

        if quote_char == '`'
        {
            Token::CommandLiteral(content)
        }
        else
        {
            Token::StringLiteral(content)
        }
    }

    fn peek(&self) -> char
    {
        if self.position + 1 >= self.input.len()
        {
            return '\0';
        }
        self.input[self.position + 1]
    }
}
