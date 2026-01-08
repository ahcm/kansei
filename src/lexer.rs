#[derive(Debug, Clone, PartialEq)]
pub enum Token
{
    Integer(i64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    StringLiteral(String),  // "hello"
    CommandLiteral(String), // `ls`
    EOF,
}

#[derive(Clone)]
pub struct Lexer
{
    input: Vec<char>,
    position: usize,
}

impl Lexer
{
    pub fn new(input: &str) -> Self
    {
        Self {
            input: input.chars().collect(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token
    {
        self.skip_whitespace();

        if self.position >= self.input.len()
        {
            return Token::EOF;
        }

        let ch = self.input[self.position];

        match ch
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
                self.position += 1;
                Token::Equals
            }
            '0'..='9' => self.read_integer(),
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),
            _ =>
            {
                // Ignore unknown chars for this MVP
                self.position += 1;
                self.next_token()
            }
        }
    }

    fn skip_whitespace(&mut self)
    {
        while self.position < self.input.len() && self.input[self.position].is_whitespace()
        {
            self.position += 1;
        }
    }

    fn read_integer(&mut self) -> Token
    {
        let start = self.position;
        while self.position < self.input.len() && self.input[self.position].is_digit(10)
        {
            self.position += 1;
        }
        let num_str: String = self.input[start..self.position].iter().collect();
        Token::Integer(num_str.parse().unwrap_or(0))
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
        Token::Identifier(ident)
    }

    // read until the closing quote/backtick
    fn read_string(&mut self, quote_char: char) -> Token
    {
        self.position += 1; // Skip the opening quote
        let start = self.position;

        while self.position < self.input.len() && self.input[self.position] != quote_char
        {
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
}
