use std::fmt::Display;
use std::{iter::Peekable, slice::Iter};

use tokenizer::Keyword;
use tokenizer::Lexeme;
use tokenizer::Tokenizer;

use ast_defs::*;

pub mod ast_defs;
pub mod tokenizer;

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
    let mut tokenizer = Tokenizer::new(code);

    tokenizer.tokenize();

    println!("Tokens: \n{:?}", &mut tokenizer.tokens);

    let parsed_tree = Parser::parse(tokenizer.tokens.token_iter());

    println!();
    println!("{:?}", &mut parsed_tree.unwrap().nodes);
}

fn main() {
    let mut tokenizer = Tokenizer::new("fun sillybilly () {a.b = 0;}");

    tokenizer.tokenize();

    println!("{:?}", &mut tokenizer.tokens);

    let parsed_tree = Parser::parse(tokenizer.tokens.token_iter());

    println!();
    println!(
        "{:?}",
        &mut parsed_tree.expect("Some kind of error happened lol").nodes
    );
}

// fn main() {
//     let args: Vec<String> = env::args().collect();

//     if args.len() <= 1 {
//         println!("Usage: program file_path");
//         std::process::exit(64)
//     }

//     let file_path = &args[1];

//     let contents =
//         fs::read_to_string(&file_path).expect(&format!("Unable to read file {}!", file_path));

//     let tokens = extract_tokens(&contents);
// }

trait ToTokenIter<'a> {
    type IterResult;
    fn token_iter(&'a mut self) -> Self::IterResult;
}

impl<'a> From<&'a mut Vec<tokenizer::Token>> for TokenIter<'a> {
    fn from(value: &'a mut Vec<tokenizer::Token>) -> Self {
        TokenIter::new(value)
    }
}

impl<'a> ToTokenIter<'a> for Vec<tokenizer::Token> {
    type IterResult = TokenIter<'a>;
    fn token_iter(&'a mut self) -> Self::IterResult {
        TokenIter::new(self)
    }
}
