use std::fmt::{Debug, Formatter};
use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;

macro_rules! compare_operator {
    ($self:ident, $single:ident, $double:ident) => {
        if let Some(next) = $self.iter.peek() {
            if let Some(token) = match next {
                '=' => Some(Lexeme::$double),
                _ => None,
            } {
                $self.iter.next();
                $self.token(token);
            } else {
                $self.token(Lexeme::$single);
            }
        } else {
            $self.token(Lexeme::$single);
        }
    };
}

pub trait Keyword: Debug + Clone + PartialEq + Eq {
    fn lookup(str: &str) -> Option<Self>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme<Keywords: Keyword> {
    Identifier(String),
    Keyword(Keywords),
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
    DivEquals,
    MulEquals,
    PlusEquals,
    MinusEquals,
    Integer(String),
    Decimal(String),
    Unknown
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token<Keywords: Keyword> {
    pub start_index: u64,
    pub end_index: u64,
    pub token_type: Lexeme<Keywords>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TokenBuilder<Keywords> {
    pub start_index: u64,
    unused: PhantomData<Keywords>,
}

impl<Keywords: Keyword> TokenBuilder<Keywords> {
    fn new(start_index: u64) -> Self {
        Self {
            start_index,
            unused: PhantomData,
        }
    }

    fn finish(&self, token_type: Lexeme<Keywords>, end_index: u64) -> Token<Keywords> {
        Token {
            start_index: self.start_index,
            end_index,
            token_type,
        }
    }
}

impl<Keywords: Keyword> Debug for Token<Keywords> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Token({:?})", self.token_type)
    }
}

pub struct TokenizerError {
    pub token_index: usize,
    pub msg: String,
}

pub struct Tokenizer<'a, Keywords: Keyword> {
    pub tokens: Vec<Token<Keywords>>,
    iter: TokenizerStringIter<'a>,
    current_string: String,
    current_token: TokenBuilder<Keywords>,
    errors: Vec<TokenizerError>,
}

struct TokenizerStringIter<'a> {
    chars: Peekable<Chars<'a>>,
    current_pos: u64,
}

impl<'a> TokenizerStringIter<'a> {
    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(char) => Some(*char),
            None => None,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.current_pos += 1;
        self.chars.next()
    }
}

impl<'a, Keywords: Keyword> Tokenizer<'a, Keywords> {
    pub fn new(content: &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            iter: TokenizerStringIter {
                chars: content.chars().peekable(),
                current_pos: 0,
            },
            current_string: String::new(),
            current_token: TokenBuilder::new(0),
            errors: Vec::new(),
        }
    }

    fn err(&mut self, token: Lexeme<Keywords>, msg: &str) {
        self.token(token);
        self.errors.push(TokenizerError {
            token_index: self.tokens.len() - 1,
            msg: String::from(msg),
        })
    }

    fn token(&mut self, token: Lexeme<Keywords>) {
        self.tokens
            .push(self.current_token.finish(token, self.iter.current_pos));
        self.current_token = TokenBuilder::new(self.iter.current_pos + 1);
    }

    fn consume_identifier(&mut self) {
        while let Some(char) = self.iter.peek() {
            match char {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.current_string.push(char);
                }
                _ => {
                    let result = Keywords::lookup(&self.current_string[..]);
                    match result {
                        Some(key) => self.token(Lexeme::Keyword(key)),
                        None => self.token(Lexeme::Identifier(self.current_string.clone())),
                    }
                    self.current_string.clear();
                    break;
                }
            }
            self.iter.next();
        }
    }

    fn equal_type(c: char) -> Option<Lexeme<Keywords>> {
        match c {
            '=' => Some(Lexeme::Equals),
            '<' => Some(Lexeme::LessEquals),
            '>' => Some(Lexeme::GreaterEquals),
            '!' => Some(Lexeme::NotEquals),
            '+' => Some(Lexeme::PlusEquals),
            '-' => Some(Lexeme::MinusEquals),
            '*' => Some(Lexeme::MulEquals),
            '/' => Some(Lexeme::DivEquals),
            _ => None,
        }
    }

    pub fn tokenize(&mut self) {
        while let Some(char) = self.iter.next() {
            match char {
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.current_string.push(char);
                    self.consume_identifier();
                }
                '0'..='9' => {
                    let mut number_type = false;
                    self.current_string.push(char);
                    while let Some(char) = self.iter.peek() {
                        match char {
                            '0'..='9' => {
                                self.current_string.push(self.iter.next().unwrap());
                            }
                            '.' => {
                                self.current_string.push(self.iter.next().unwrap());
                                number_type = true;
                            }
                            _ => {
                                if number_type {
                                    self.token(Lexeme::Decimal(self.current_string.clone()));
                                } else {
                                    self.token(Lexeme::Integer(self.current_string.clone()));
                                }
                                self.current_string.clear();
                                break;
                            }
                        }
                    }
                }
                '.' => {
                    self.current_string.push(char);

                    if let Some(char) = self.iter.peek() {
                        match char {
                            '0'..='9' => {}
                            _ => {
                                self.current_string.clear();
                                self.token(Lexeme::Dot);
                                continue;
                            }
                        }
                    }

                    while let Some(char) = self.iter.peek() {
                        match char {
                            '0'..='9' => {
                                self.iter.next().unwrap();
                            }
                            _ => {
                                self.token(Lexeme::Decimal(self.current_string.clone()));
                                self.current_string.clear();
                                break;
                            }
                        }
                    }
                }
                '\'' => {
                    let Some(char) = self.iter.next() else {
                        self.err(
                            Lexeme::SingleQuotes(char),
                            "Expected character inside single quotes",
                        );
                        continue;
                    };
                    self.token(Lexeme::SingleQuotes(char));
                    let char = self.iter.next();
                    unsafe {
                        if char.is_none() || char.unwrap_unchecked() != '\'' {
                            self.err(
                                Lexeme::SingleQuotes(char.unwrap_unchecked()),
                                "No matching single quote found!",
                            );
                        }
                    }
                }
                '"' => {
                    while let Some(char) = self.iter.peek() {
                        match char {
                            '"' => {
                                self.iter.next();

                                self.token(Lexeme::DoubleQuotes(self.current_string.clone()));
                                self.current_string.clear();
                                break;
                            }
                            _ => {
                                self.current_string.push(char);
                                self.iter.next();
                            }
                        }
                    }
                }
                '=' => {
                    if let Some(next) = self.iter.peek() {
                        if let Some(token) = Self::equal_type(next) {
                            self.token(token);
                        } else {
                            self.token(Lexeme::Assignment);
                        }
                    } else {
                        self.token(Lexeme::Assignment);
                    }
                }
                '<' => compare_operator!(self, Less, LessEquals),
                '>' => compare_operator!(self, Greater, GreaterEquals),
                '!' => compare_operator!(self, Exclamation, NotEquals),
                '/' => {
                    if let Some(next) = self.iter.peek() {
                        match next {
                            '/' => {
                                while let Some(char) = self.iter.peek() {
                                    if char == '\n' {
                                        break;
                                    }
                                    self.iter.next();
                                }
                            }
                            '=' => {
                                self.iter.next();
                                self.token(Lexeme::DivEquals);
                            }
                            _ => {
                                self.token(Lexeme::Slash);
                            }
                        }
                    } else {
                        self.token(Lexeme::Slash);
                    }
                }
                ' ' | '\r' | '\t' => {}
                _ => {
                    match char {
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
                            self.err(Lexeme::Unknown, "Unidentified character type");
                        }
                    };
                }
            }
        }
    }
}
