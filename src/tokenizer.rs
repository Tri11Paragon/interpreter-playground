use std::fmt::{Debug};
use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;

mod tokenizer_impl;

pub trait Keyword: Debug + Clone + PartialEq + Eq {
    fn lookup(str: &str) -> Option<Self>;
}

pub trait PrettyPrint {
    fn pretty_print(&self, str: &str);
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
    pub start_index: usize,
    pub end_index: usize,
    pub token_type: Lexeme<Keywords>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TokenBuilder<Keywords> {
    pub start_index: usize,
    unused: PhantomData<Keywords>,
}

#[derive(Debug, Clone)]
pub struct TokenizerError {
    pub token_start_index: usize,
    pub token_end_index: usize,
    pub msg: String,
}

pub struct Tokenizer<'a, Keywords: Keyword> {
    tokens: Vec<Token<Keywords>>,
    iter: TokenizerStringIter<'a>,
    current_string: String,
    current_token: TokenBuilder<Keywords>,
    errors: Vec<TokenizerError>,
    last_char: char,
}

struct TokenizerStringIter<'a> {
    chars: Peekable<Chars<'a>>,
    current_pos: usize,
}
