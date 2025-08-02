use std::fmt::Debug;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Const,
    Var,
    If,
    Else,
    Fun,
    While,
    Print,
    Return,
    For,
    Class,
    Nil,
    True,
    False
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
    Identifier(String),
    Keyword(Keyword),
    Dot,
    Semicolon,
    SingleQuotes(char),
    DoubleQuotes(String),
    Minus,
    Plus,
    Star,
    And,
    Or,
    Percent,
    Dollar,
    At,
    Exclamation,
    Caret,
    Tilde,
    Grave,
    Pound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Colon,
    Left,
    Right,
    Comma,
    Slash,
    Question,
    Assignment,
    Equals,
    Less,
    Greater,
    LessEquals,
    GreaterEquals,
    NotEquals,
    Integer(String),
    Decimal(String),
    EOF,
}

#[derive(Clone)]
pub struct Token {
    pub line: u64,
    pub character_in_line: u64,
    pub token_type: Lexeme,
}

impl Debug for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "TokenData({:?})", self.token_type)
    }
}

pub struct Tokenizer<'a> {
    pub tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    current_line: u64,
    current_char: u64,
    current_string: String,
}

impl<'a> Tokenizer<'a> {
    pub fn new(content: &'a str) -> Self {
        return Self {
            tokens: Vec::new(),
            chars: content.chars().peekable(),
            current_line: 1,
            current_char: 0,
            current_string: String::new(),
        };
    }

    pub fn iter(&self) -> core::slice::Iter<Token> {
        self.tokens.iter()
    }

    fn token(&self, token: Lexeme) -> Token {
        Token {
            line: self.current_line,
            character_in_line: self.current_char,
            token_type: token,
        }
    }

    fn consume_identifier(&mut self) {
        while let Some(char) = self.chars.peek() {
            match char {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.current_string.push(*char);
                }
                _ => {
                    let result = match &self.current_string[..] {
                        "const" => Some(Keyword::Const),
                        "var" => Some(Keyword::Var),
                        "if" => Some(Keyword::If),
                        "else" => Some(Keyword::Else),
                        "fun" => Some(Keyword::Fun),
                        "while" => Some(Keyword::While),
                        "print" => Some(Keyword::Print),
                        "return" => Some(Keyword::Return),
                        "for" => Some(Keyword::For),
                        "class" => Some(Keyword::Class),
                        "nil" => Some(Keyword::Nil),
                        "true" => Some(Keyword::True),
                        "false" => Some(Keyword::False),
                        _ => None,
                    };
                    match result {
                        Some(key) => self.tokens.push(self.token(Lexeme::Keyword(key))),
                        None => {
                            self.tokens
                                .push(self.token(Lexeme::Identifier(self.current_string.clone())));
                        }
                    }
                    self.current_string.clear();
                    break;
                }
            }
            self.chars.next();
        }
    }

    pub fn tokenize(&mut self) {
        while let Some(char) = self.chars.next() {
            self.current_char += 1;
            match char {
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.current_string.push(char);
                    self.consume_identifier();
                }
                '0'..='9' => {
                    let mut number_type = false;
                    self.current_string.push(char);
                    while let Some(char) = self.chars.peek() {
                        match char {
                            '0'..='9' => {
                                self.current_string.push(self.chars.next().unwrap());
                            }
                            '.' => {
                                self.current_string.push(self.chars.next().unwrap());
                                number_type = true;
                            }
                            _ => {
                                if number_type {
                                    self.tokens.push(
                                        self.token(Lexeme::Decimal(self.current_string.clone())),
                                    );
                                } else {
                                    self.tokens.push(
                                        self.token(Lexeme::Integer(self.current_string.clone())),
                                    );
                                }
                                self.current_string.clear();
                                break;
                            }
                        }
                    }
                }
                '.' => {
                    self.current_string.push(char);

                    if let Some(char) = self.chars.peek() {
                        match char {
                            '0'..='9' => {}
                            _ => {
                                self.current_string.clear();
                                self.tokens.push(self.token(Lexeme::Dot));
                                continue;
                            }
                        }
                    }

                    while let Some(char) = self.chars.peek() {
                        match char {
                            '0'..='9' => {
                                self.current_string.push(self.chars.next().unwrap());
                            }
                            _ => {
                                self.tokens
                                    .push(self.token(Lexeme::Decimal(self.current_string.clone())));
                                self.current_string.clear();
                                break;
                            }
                        }
                    }
                }
                '\'' => {
                    let char = self.chars.next().expect(&format!(
                        "Expected character inside single quotes at '{}:{}'",
                        self.current_line, self.current_char
                    ));
                    self.tokens.push(self.token(Lexeme::SingleQuotes(char)));
                    let char = self.chars.next();
                    unsafe {
                        if char.is_none() || char.unwrap_unchecked() != '\'' {
                            panic!(
                                "Expected ending quote for text '{:?}' at '{}:{}'",
                                self.tokens.iter().nth_back(0).unwrap(),
                                self.current_line,
                                self.current_char
                            );
                        }
                    }
                }
                '"' => {
                    while let Some(char) = self.chars.peek() {
                        match char {
                            '"' => {
                                self.chars.next();
                                self.tokens.push(
                                    self.token(Lexeme::DoubleQuotes(self.current_string.clone())),
                                );
                                self.current_string.clear();
                                break;
                            }
                            _ => unsafe {
                                self.current_string
                                    .push(self.chars.next().unwrap_unchecked());
                            },
                        }
                    }
                }
                '=' => {
                    if let Some(next) = self.chars.peek() {
                        if let Some(token) = match next {
                            '=' => Some(Lexeme::Equals),
                            '<' => Some(Lexeme::LessEquals),
                            '>' => Some(Lexeme::GreaterEquals),
                            '!' => Some(Lexeme::NotEquals),
                            _ => None,
                        } {
                            self.tokens.push(self.token(token));
                        } else {
                            self.tokens.push(self.token(Lexeme::Assignment));
                        }
                    } else {
                        self.tokens.push(self.token(Lexeme::Assignment));
                    }
                }
                '<' => {
                    if let Some(next) = self.chars.peek() {
                        if let Some(token) = match next {
                            '=' => Some(Lexeme::LessEquals),
                            _ => None,
                        } {
                            self.tokens.push(self.token(token));
                        } else {
                            self.tokens.push(self.token(Lexeme::Less));
                        }
                    } else {
                        self.tokens.push(self.token(Lexeme::Less));
                    }
                }
                '>' => {
                    if let Some(next) = self.chars.peek() {
                        if let Some(token) = match next {
                            '=' => Some(Lexeme::GreaterEquals),
                            _ => None,
                        } {
                            self.tokens.push(self.token(token));
                        } else {
                            self.tokens.push(self.token(Lexeme::Greater));
                        }
                    } else {
                        self.tokens.push(self.token(Lexeme::Greater));
                    }
                }
                '!' => {
                    if let Some(next) = self.chars.peek() {
                        if let Some(token) = match next {
                            '=' => Some(Lexeme::NotEquals),
                            _ => None,
                        } {
                            self.tokens.push(self.token(token));
                        } else {
                            self.tokens.push(self.token(Lexeme::Exclamation));
                        }
                    } else {
                        self.tokens.push(self.token(Lexeme::Exclamation));
                    }
                }
                '/' => {
                    if let Some(next) = self.chars.peek() {
                        match next {
                            '/' => {
                                while let Some(char) = self.chars.peek() {
                                    if *char == '\n' {
                                        break;
                                    }
                                    self.chars.next();
                                }
                            }
                            _ => {
                                self.tokens.push(self.token(Lexeme::Slash));
                            }
                        }
                    } else {
                        self.tokens.push(self.token(Lexeme::Slash));
                    }
                }
                ' ' | '\r' | '\t' => {}
                '\n' => {
                    self.current_char = 0;
                    self.current_line += 1;
                }
                _ => {
                    self.tokens.push(match char {
                        ';' => self.token(Lexeme::Semicolon),
                        '+' => self.token(Lexeme::Plus),
                        '-' => self.token(Lexeme::Minus),
                        '&' => self.token(Lexeme::And),
                        '|' => self.token(Lexeme::Or),
                        '^' => self.token(Lexeme::Caret),
                        '$' => self.token(Lexeme::Dollar),
                        '~' => self.token(Lexeme::Tilde),
                        '@' => self.token(Lexeme::At),
                        '#' => self.token(Lexeme::Pound),
                        '%' => self.token(Lexeme::Percent),
                        '*' => self.token(Lexeme::Star),
                        '`' => self.token(Lexeme::Grave),
                        '[' => self.token(Lexeme::OpenSquare),
                        ']' => self.token(Lexeme::CloseSquare),
                        '{' => self.token(Lexeme::OpenCurly),
                        '}' => self.token(Lexeme::CloseCurly),
                        '(' => self.token(Lexeme::OpenParen),
                        ')' => self.token(Lexeme::CloseParen),
                        '<' => self.token(Lexeme::Left),
                        '>' => self.token(Lexeme::Right),
                        '?' => self.token(Lexeme::Question),
                        ',' => self.token(Lexeme::Comma),
                        ':' => self.token(Lexeme::Colon),
                        _ => {
                            panic!(
                                "Unidentified character type '{char}' at '{}:{}'",
                                self.current_line, self.current_char
                            );
                        }
                    });
                }
            }
        }
        self.tokens.push(self.token(Lexeme::EOF));
    }
}
