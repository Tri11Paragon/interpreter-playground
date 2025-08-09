use std::collections::HashMap;
use parser::errors::{ParserError, PrettyPrint};
use parser::tokenizer::{Lexeme, Token, Tokenizer};

macro_rules! debug {
    ($($arg:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($arg)*));
    };
}

macro_rules! define_keywords {
    (
        $( $Variant:ident $(=> $name:literal)? ),+ $(,)?
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $( $Variant ),+
        }

        impl parser::tokenizer::Keyword for Keyword {
            fn lookup(s: &str) -> Option<Self> {
                $(
                    define_keywords!(@arm s, $Variant $(=> $name)?);
                )+
                None
            }
        }
    };

    // Arm for entries with explicit spelling: Variant => "kw"
    (@arm $s:ident, $Variant:ident => $name:literal) => {
        if $s == $name {
            return Some(Self::$Variant);
        }
    };

    // Arm for entries without explicit spelling: Variant
    // Compare case-insensitively with the variant name (e.g., "Return" matches "return").
    (@arm $s:ident, $Variant:ident) => {
        if $s.eq_ignore_ascii_case(stringify!($Variant)) {
            return Some(Self::$Variant);
        }
    };
}

define_keywords! {
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
    False,
    Import
}

enum ParseMapImpl<'a> {
    Action(fn(&Vec<Token<Keyword>>) -> ()),
    Map(&'a mut ParseMap<'a>),
}

struct ParseMap<'a> {
    tokens: Vec<Token<Keyword>>,
    map: HashMap<Lexeme<Keyword>, ParseMapImpl<'a>>,
}

impl<'a, 'b> ParseMap<'a> {
    fn new(map: HashMap<Lexeme<Keyword>, ParseMapImpl<'a>>) -> Self {
        Self {
            tokens: Vec::new(),
            map,
        }
    }

    fn parse(&mut self, parser: &mut Parser<'b>) {
        if !parser.has_token() {
            parser.errors.push(ParserError::eof(parser.file.clone()));
            return;
        }
        if self.map.contains_key(&parser.peek_token().token_type) {
            let next_action = self.map.get_mut(&parser.peek_token().token_type).expect("How did we get here?");
            self.tokens.push(parser.next_token().clone());
            match next_action {
                ParseMapImpl::Action(func) => {
                    (func)(&self.tokens);
                }
                ParseMapImpl::Map(map) => {
                    map.tokens = self.tokens.clone();
                    map.parse(parser);
                }
            }
        }
    }
}


struct Parser<'a> {
    tokens: &'a Vec<Token<Keyword>>,
    errors: Vec<parser::errors::ParserError>,
    current_token: usize,
    file: Option<String>
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token<Keyword>>, file: Option<String>) -> Self {
        Self {
            errors: Vec::new(),
            tokens,
            current_token: 0,
            file
        }
    }

    fn peek_token(&mut self) -> &Token<Keyword> {
        &self.tokens[self.current_token]
    }

    fn next_token(&mut self) -> &Token<Keyword> {
        let token = &self.tokens[self.current_token];
        self.advance();
        token
    }

    fn has_token(&self) -> bool {
        self.current_token < self.tokens.len()
    }

    fn advance(&mut self) {
        self.current_token += 1;
    }
}

#[test]
fn print_tests() {
    let code = r#"
        class meow {
            var silly = 0;
            var iamavariable = {
                var counter = 0;
                while (counter < 10) {
                    counter = counter + 1;
                }
                return counter;
            };

            fun callme(hello, there) {
                print hello;
                print there;
                print iamavariable;
            }
        }

        fun main() {
            var wow = ~meow{silly = 50};
            wow.callme(50, 10);
        }
    "#;
    let mut tokenizer = Tokenizer::<Keyword>::new(code);

    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            println!("{:?}", tokens);
        }
        Err(errors) => {
            errors.pretty_print(code);
        }
    }
}

fn main() {
    let code = "fun sillybilly () {a.b = 0;}";
    let mut tokenizer = Tokenizer::<Keyword>::new(code);

    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            println!("{:?}", tokens);
        }
        Err(errors) => {
            errors.pretty_print(code);
        }
    }
}

// fn main() {
//     let args: Vec<String> = env::args().collect();

//     if args.len() <= 1 {
//         println!("Usage: program file_path");
//         std::process::exit(64)
//     }

//     let file_path = &args[1];

//     let contents =
//         

//     let tokens = extract_tokens(&contents);
// }