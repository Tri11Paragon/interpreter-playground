
use std::{iter::Peekable, slice::Iter};

use tokenizer::Tokenizer;

pub mod tokenizer;

enum CompareEquality {
    Equals,
    NotEquals
}

enum CompareRelationship {
    Greater,
    GreaterEquals,
    Less,
    LessEquals
}

enum AllCompare {
    CompareEquality(CompareEquality),
    CompareRelationship(CompareRelationship)
}

enum ASTBlock {
    Program(Box<Class>),
    Function(Box<Function>)
}

struct Program {
    children: Vec<ASTBlock>
}

struct Function {
    identifier : String,
    scopes: Box<Scope>
}

enum ASTClass {
    VariableDecl(Box<VariableDecl>),
    Function(Box<Function>)
}

struct Class {
    identifier : String,
    children: Vec<ASTClass>
}

enum ASTScope {
    Statement(Box<Statement>),
    Scope(Box<Scope>)
}

struct Scope {
    children : Vec<ASTScope>
}

struct VariableDecl {
    identifier: String,
    assigning_expression: Option<Box<Scope>>
}

struct Statement {

}

struct Return {
    value : Variable
}

enum ASTControlFlowIfCondition {
    Expression(Box<Expression>),
    VariableDecl(Box<VariableDecl>),
    VariableDeclCompare(Box<VariableDecl>, AllCompare, Box<Expression>)
}

struct ControlFlowIf {
    condition: ASTControlFlowIfCondition,
    if_true: Box<Scope>,
    if_false: Option<Box<Scope>>
}

struct ControlFlowWhile {
    condition: ASTControlFlowIfCondition,
    scope: Box<Scope>
}

struct ControlFlowFor {
    expression: Box<ForExpression>,
    scope: Box<Scope>
}

enum ASTControlFlow {

}

struct ControlFlow {

}

enum ASTForExpression {
    VariableDecl(Box<VariableDecl>),
    None
}

struct ForExpression {
    init: ASTForExpression,
    compare_expression: Box<Expression>,
    increment_expression: Box<Expression>
}

struct Expression {

}

struct Equality {

}

struct Comparison {

}

struct Term {

}

struct Factor {

}

struct Unary {

}

struct Primary {

}

enum ASTVariable {
    Variable(Box<Variable>, Box<Variable>),
    Identifier(String)
}

struct Variable {
    variable: ASTVariable
}

enum ASTType {
    Program(Block),
    Function(Block),
    Class(Block),
    Scope(Block),
    ReturningScope(Block),
    VariableDecl(VariableDecl),
    Statement(Box<ASTNode>),
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

type ChildNodes = Vec<Box<ASTNode>>;

struct ASTNode {
    line: u64,
    char_in_line: u64,
    ast_type: ASTType,
}

impl ASTNode {
    fn new(ast_type: ASTType, token: tokenizer::Token) -> Self {
        ASTNode {
            line: token.line,
            char_in_line: token.character_in_line,
            ast_type: ast_type
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

impl<'a> From<&'a mut Vec<tokenizer::Token>> for TokenIter<'a>{
    fn from(value: &'a mut Vec<tokenizer::Token>) -> Self {
        TokenIter::new(value)
    }
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
    tree: ChildNodes,
}

impl Parser {
    fn parse_function(iter: &mut TokenIter) -> Result<ChildNodes, ASTError> {

    }

    fn parse_class(iter: &mut TokenIter) -> Result<ChildNodes, ASTError> {
        
    }

    fn parse_program(iter: &mut TokenIter) -> Result<ChildNodes, ASTError> {
        let mut nodes = ChildNodes::new();

        while iter.has_next() {
            nodes.append(&mut Parser::parse_function(iter)?);
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
            tree: ChildNodes::new()
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
