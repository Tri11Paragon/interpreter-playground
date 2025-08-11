use crate::tokenizer::{
    Keyword, Lexeme, Token, TokenBuilder, Tokenizer, TokenizerError, TokenizerStringIter,
};
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

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

impl<Keywords: Keyword> TokenBuilder<Keywords> {
    fn new(start_index: usize) -> Self {
        Self {
            start_index,
            unused: PhantomData,
        }
    }

    fn finish(
        &self,
        token_type: Lexeme<Keywords>,
        end_index: usize,
        file: Option<String>,
    ) -> Token<Keywords> {
        Token {
            start_index: self.start_index,
            end_index,
            token_type,
            file,
        }
    }
}

impl<Keywords: Keyword> Debug for Token<Keywords> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Token({:?})", self.token_type)
    }
}

impl<'a> TokenizerStringIter<'a> {
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn next(&mut self) -> Option<char> {
        let result = self.chars.next()?;
        self.current_pos += result.len_utf8();
        Some(result)
    }
}

impl<'a, Keywords: Keyword> Tokenizer<'a, Keywords> {
    pub fn tokenize_file(file: &str) -> Result<Vec<Token<Keywords>>, Vec<TokenizerError>> {
        let error_str = format!("Unable to read file {}!", file);
        let data = std::fs::read_to_string(file).expect(&error_str);
        let mut tokenizer = Tokenizer::new(&data);
        tokenizer.file = Some(file.into());
        tokenizer.tokenize()
    }

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
            last_char: '\0',
            file: None,
        }
    }

    fn err(&mut self, token: Lexeme<Keywords>, msg: &str) {
        self.token(token);
        let last = self.tokens.last().unwrap();
        self.errors.push(TokenizerError {
            token_start_index: last.start_index,
            token_end_index: last.end_index,
            msg: String::from(msg),
            file: self.file.clone(),
        })
    }

    fn reset_token(&mut self) {
        self.current_token = TokenBuilder::new(self.iter.current_pos);
    }

    fn token(&mut self, token: Lexeme<Keywords>) {
        self.tokens.push(self.current_token.finish(
            token,
            self.iter.current_pos,
            self.file.clone(),
        ));
        self.reset_token();
    }

    fn consume_identifier(&mut self) {
        while let Some(char) = self.iter.peek() {
            match char {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.current_string.push(char);
                }
                _ => {
                    let result = Keywords::lookup(&self.current_string);
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

    pub fn tokenize(&mut self) -> Result<Vec<Token<Keywords>>, Vec<TokenizerError>> {
        while let Some(char) = self.iter.next() {
            self.last_char = char;
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
                ' ' | '\r' | '\t' | '\n' => {
                    self.reset_token();
                }
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
        if self.errors.is_empty() {
            Ok(self.tokens.clone())
        } else {
            Err(self.errors.clone())
        }
    }
}
