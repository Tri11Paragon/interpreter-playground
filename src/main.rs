
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

trait ToTokenIter<'a> {
    type IterResult;
    fn to_token_iter(&'a mut self) -> Self::IterResult;
}

impl<'a> ToTokenIter<'a> for Vec<tokenizer::Token> {
    type IterResult = TokenIter<'a>;
    fn to_token_iter(&'a mut self) -> Self::IterResult {
        TokenIter::new(self)
    }
}

impl<'a> TokenIter<'a> {
    fn new(vec: &'a mut Vec<tokenizer::Token>) -> Self {
        TokenIter {
            iter: vec.iter().peekable()
        }
    }

    fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }
}

struct Parser {
    tree: ASTNodes,
}

impl Parser {
    fn parse_function(iter: &mut TokenIter) -> Result<ASTNodes, ASTError> {

    }

    fn parse_class(iter: &mut TokenIter) -> Result<ASTNodes, ASTError> {

    }

    fn parse_program(iter: &mut TokenIter) -> Result<ASTNodes, ASTError> {
        let nodes = ASTNodes::new();

        while iter.has_next() {
            let mut results = Parser::parse_function(iter);
            match results {
                Ok(result) => nodes.append(results),
                Err(e) => return Err(e)
            }
            
        }

        Ok(nodes)
    }

    fn parse(&mut self, mut iter: TokenIter) {
        let mut results = Parser::parse_program(&mut iter);
        match &mut results {
            Ok(result) => {
                self.tree.append(result);
            }
            Err(e) => {
                println!("Parser Error: {}", e.message);
            }
        }
        
    }

    fn new() -> Self {
        Parser {
            tree: ASTNodes::new()
        }
    }
}

fn main() {
    let mut tokenizer = Tokenizer::new("var silly = 32;");

    tokenizer.tokenize();

    let mut parser = Parser::new();
    parser.parse(tokenizer.tokens.to_token_iter());

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
