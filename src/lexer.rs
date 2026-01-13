use crate::ast::{FloatKind, IntKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Token
{
    Integer
    {
        value: i128,
        kind: IntKind,
    },
    Unsigned
    {
        value: u128,
        kind: IntKind,
    },
    Float
    {
        value: f64,
        kind: FloatKind,
    },
    Identifier(String),
    FormatString(String),
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
    Loop,
    Use,
    Load,
    In,
    Yield,
    Clone,
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
    ColonColon,
    Dot,
    Ampersand,
    Semicolon,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span
{
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
        loop
        {
            self.skip_whitespace();

            let start_line = self.line;

            if self.position >= self.input.len()
            {
                return Span {
                    token: Token::EOF,
                    line: start_line,
                };
            }

            let ch = self.input[self.position];

            if ch == '#'
            {
                self.skip_comment();
                continue;
            }

            let token = match ch
            {
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
                'a'..='z' | 'A'..='Z' | '_' =>
                {
                    if ch == 'f' && self.peek() == '"'
                    {
                        self.position += 1; // consume 'f'
                        let token = self.read_string('"');
                        match token
                        {
                            Token::StringLiteral(s) => Token::FormatString(s),
                            _ => token,
                        }
                    }
                    else
                    {
                        self.read_identifier()
                    }
                }
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
                    if self.peek() == ':'
                    {
                        self.position += 2;
                        Token::ColonColon
                    }
                    else
                    {
                        self.position += 1;
                        Token::Colon
                    }
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
                    continue;
                }
            };

            return Span {
                token,
                line: start_line,
            };
        }
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
            if self.input[self.position] == '\n'
            {
                self.line += 1;
            }
            self.position += 1;
        }
    }

    fn read_number(&mut self) -> Token
    {
        let mut int_val: u128 = 0;
        let mut overflowed = false;
        let mut saw_fraction = false;
        while self.position < self.input.len() && self.input[self.position].is_digit(10)
        {
            let digit = (self.input[self.position] as u8 - b'0') as u128;
            if let Some(next) = int_val.checked_mul(10).and_then(|v| v.checked_add(digit))
            {
                int_val = next;
            }
            else
            {
                overflowed = true;
            }
            self.position += 1;
        }

        let mut frac_val: u128 = 0;
        let mut divisor: f64 = 1.0;
        if self.position < self.input.len() && self.input[self.position] == '.'
        {
            if self.position + 1 < self.input.len() && self.input[self.position + 1].is_digit(10)
            {
                self.position += 1; // Consume dot
                while self.position < self.input.len() && self.input[self.position].is_digit(10)
                {
                    let digit = (self.input[self.position] as u8 - b'0') as u128;
                    if let Some(next) = frac_val.checked_mul(10).and_then(|v| v.checked_add(digit))
                    {
                        frac_val = next;
                    }
                    divisor *= 10.0;
                    self.position += 1;
                }
                saw_fraction = true;
            }
        }

        let mut kind = FloatKind::F64;
        let mut has_suffix = false;
        if self.position + 1 < self.input.len()
            && self.input[self.position] == 'f'
            && self.input[self.position + 1].is_digit(10)
        {
            has_suffix = true;
            self.position += 1; // Consume 'f'
            let suffix_start = self.position;
            while self.position < self.input.len() && self.input[self.position].is_digit(10)
            {
                self.position += 1;
            }
            let suffix: String = self.input[suffix_start..self.position].iter().collect();
            kind = match suffix.as_str()
            {
                "32" => FloatKind::F32,
                "64" => FloatKind::F64,
                "128" => FloatKind::F128,
                _ => panic!("Unknown float suffix: f{}", suffix),
            };
        }

        if saw_fraction || has_suffix
        {
            let int_part = if overflowed { 0 } else { int_val };
            let value = if saw_fraction
            {
                int_part as f64 + (frac_val as f64 / divisor)
            }
            else
            {
                int_part as f64
            };
            Token::Float { value, kind }
        }
        else
        {
            let (int_kind, is_signed) = self.read_int_suffix();
            if overflowed
            {
                return if is_signed
                {
                    Token::Integer {
                        value: 0,
                        kind: int_kind,
                    }
                }
                else
                {
                    Token::Unsigned {
                        value: 0,
                        kind: int_kind,
                    }
                };
            }
            if is_signed
            {
                let max = signed_int_max(int_kind);
                if int_val > max as u128
                {
                    panic!("Integer literal out of range for {:?}", int_kind);
                }
                Token::Integer {
                    value: int_val as i128,
                    kind: int_kind,
                }
            }
            else
            {
                let max = unsigned_int_max(int_kind);
                if int_val > max
                {
                    panic!("Unsigned literal out of range for {:?}", int_kind);
                }
                Token::Unsigned {
                    value: int_val,
                    kind: int_kind,
                }
            }
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
            "loop" => Token::Loop,
            "use" => Token::Use,
            "load" => Token::Load,
            "in" => Token::In,
            "yield" => Token::Yield,
            "clone" => Token::Clone,
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
            if self.input[self.position] == '\n'
            {
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

    fn read_int_suffix(&mut self) -> (IntKind, bool)
    {
        if self.position + 1 < self.input.len()
        {
            let ch = self.input[self.position];
            if ch == 'i' || ch == 'u'
            {
                let is_signed = ch == 'i';
                self.position += 1; // consume i/u
                let start = self.position;
                while self.position < self.input.len() && self.input[self.position].is_digit(10)
                {
                    self.position += 1;
                }
                let suffix: String = self.input[start..self.position].iter().collect();
                let kind = match suffix.as_str()
                {
                    "8" =>
                    {
                        if is_signed
                        {
                            IntKind::I8
                        }
                        else
                        {
                            IntKind::U8
                        }
                    }
                    "16" =>
                    {
                        if is_signed
                        {
                            IntKind::I16
                        }
                        else
                        {
                            IntKind::U16
                        }
                    }
                    "32" =>
                    {
                        if is_signed
                        {
                            IntKind::I32
                        }
                        else
                        {
                            IntKind::U32
                        }
                    }
                    "64" =>
                    {
                        if is_signed
                        {
                            IntKind::I64
                        }
                        else
                        {
                            IntKind::U64
                        }
                    }
                    "128" =>
                    {
                        if is_signed
                        {
                            IntKind::I128
                        }
                        else
                        {
                            IntKind::U128
                        }
                    }
                    _ => panic!(
                        "Unknown integer suffix: {}{}",
                        if is_signed { "i" } else { "u" },
                        suffix
                    ),
                };
                return (kind, is_signed);
            }
        }
        (IntKind::I64, true)
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

fn signed_int_max(kind: IntKind) -> i128
{
    match kind
    {
        IntKind::I8 => i8::MAX as i128,
        IntKind::I16 => i16::MAX as i128,
        IntKind::I32 => i32::MAX as i128,
        IntKind::I64 => i64::MAX as i128,
        IntKind::I128 => i128::MAX,
        _ => panic!("Expected signed int kind, got {:?}", kind),
    }
}

fn unsigned_int_max(kind: IntKind) -> u128
{
    match kind
    {
        IntKind::U8 => u8::MAX as u128,
        IntKind::U16 => u16::MAX as u128,
        IntKind::U32 => u32::MAX as u128,
        IntKind::U64 => u64::MAX as u128,
        IntKind::U128 => u128::MAX,
        _ => panic!("Expected unsigned int kind, got {:?}", kind),
    }
}
