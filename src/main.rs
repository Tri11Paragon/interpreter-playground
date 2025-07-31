
use std::{iter::Peekable, slice::Iter};

use tokenizer::Tokenizer;

pub mod tokenizer;

enum ASTType {
    Program,
    Function,
    Class,
    Scope,
    ReturningScope,
    VariableDecl,
    Statement,
    Return,
    ControlFlow,
    ForExpression,
    CompareEquality,
    CompareRelationship,
    AllCompare,
    Expression,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Primary,
    Variable,
}

struct ASTError {
    line: u64,
    char_in_line: u64,
    message: String
}

impl ASTError {
    fn new(message: &str, token: tokenizer::Token) -> Self {
        ASTError {
            line: token.line,
            char_in_line: token.character_in_line,
            message: message.to_owned()
        }
    }
}

type ASTNodes = Vec<Box<ASTNode>>;

struct ASTNode {
    line: u64,
    char_in_line: u64,
    lexeme: tokenizer::Lexeme,
    ast_type: ASTType,
    children: ASTNodes,
}

impl ASTNode {
    fn new(ast_type: ASTType, token: tokenizer::Token) -> Self {
        ASTNode {
            line: token.line,
            char_in_line: token.character_in_line,
            lexeme: token.token_type,
            ast_type: ast_type,
            children: ASTNodes::new()
        }
    }
}

struct TokenIter<'a> {
    iter: Peekable<Iter<'a, tokenizer::Token>>
}

impl<'a> TokenIter<'a> {
    fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }
}

struct Parser {
    tree: ASTNodes,
}

impl Parser {
    fn program(iter: &mut TokenIter) -> Result<ASTNodes, ASTError> {
        let nodes = ASTNodes::new();

        while iter.has_next() {
            
        }

        Ok(nodes)
    }

    fn parse(&mut self, iter: &mut TokenIter) {}
}

fn main() {
    let mut tokenizer = Tokenizer::new("var silly = 32;");

    tokenizer.tokenize();

    println!("{:?}", &mut tokenizer.tokens);
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
