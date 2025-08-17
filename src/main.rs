use std::thread::scope;
use crate::Keywords::Function;
use macros::build_repr_bnf;
use tokenizer::tokenizer::{Keyword, Lexeme, Tokenizer};

macro_rules! debug {
    ($($arg:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($arg)*));
    };
}

// build_repr_bnf! {
//         wow -> silly | wow "+" wow;
//         silly -> "billy" | beep;
//         beep -> "beep" | group;
//         group -> (wow "+" beep) | silly* | (beep "-" ">" beep)*;
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Keywords {
    Function,
}

impl Keyword for Keywords {
    fn lookup(str: &str) -> Option<Self> {
        match str {
            "fn" => Some(Function),
            _ => None,
        }
    }

    fn lookup_index(index: usize) -> Self {
        match index {
            0 => Function,
            _ => panic!("Unknown Keyword {}", index),
        }
    }

    fn index(&self) -> usize {
        match self {
            Function => 0,
        }
    }

    fn size() -> usize {
        1
    }
}

#[derive(Debug, Clone)]
enum AstType {
    Function(String),
    Scope,
    Expression,
    Comparison(Lexeme<Keywords>),
    Variable(String),
}

#[derive(Debug, Clone)]
enum ConstructionType {
    Function,
    Comparison,
    Variable,
    None,
}

#[derive(Debug, Clone)]
struct AstNode {
    node_type: AstType,
    children: Vec<AstNode>,
}

impl AstNode {
    fn new(node_type: AstType) -> Self {
        AstNode {
            node_type,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Action<'a> {
    SyncStart(&'a ParseMap<'a>, ConstructionType),
    SyncEnd(),
    State(&'a ParseMap<'a>),
    Panic,
}

#[derive(Debug, Clone)]
struct ParseMap<'a> {
    map: Vec<Action<'a>>,
}

impl<'a> ParseMap<'a> {
    fn new() -> Self {
        ParseMap {
            map: vec![Action::<'a>::Panic; Lexeme::<Keywords>::size()],
        }
    }
}

fn parse_test<'a>(tokenizer: &mut Tokenizer<'a, Keywords>) -> AstNode {
    let mut function_map = ParseMap::new();
    let mut scope_map = ParseMap::new();
    let mut expression_map = ParseMap::new();
    let mut comparison_map = ParseMap::new();
    let mut variable_map = ParseMap::new();

    function_map.map[Lexeme::Keyword(Function).index()] =
        Action::SyncStart(&scope_map, ConstructionType::Function);
    scope_map.map[Lexeme::<Keywords>::OpenCurly.index()] = Action::SyncStart(&expression_map,
                                                                  ConstructionType::None);
    scope_map.map[Lexeme::<Keywords>::CloseCurly.index()] = Action::SyncEnd();
    expression_map.map[Lexeme::<Keywords>::Identifier("".to_owned()).index()] = Action::SyncStart
        (&comparison_map, ConstructionType::None);


    todo!()
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
}

fn main() {}

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
