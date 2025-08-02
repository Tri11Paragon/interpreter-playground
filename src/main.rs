use std::fmt::Display;
use std::{iter::Peekable, marker::PhantomData, slice::Iter};

use tokenizer::Keyword;
use tokenizer::Lexeme;
use tokenizer::Tokenizer;

pub mod tokenizer;

#[derive(Debug, Clone)]
enum CompareOps {
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
}

#[derive(Debug, Clone)]
enum ProgramImpl {
    Class(Box<Class>),
    Function(Box<Function>),
}

#[derive(Default, Debug, Clone)]
struct Program {
    children: Vec<ProgramImpl>,
}

#[derive(Default, Debug, Clone)]
struct Function {
    identifier: String,
    scope: Box<Scope>,
}

#[derive(Debug, Clone)]
enum ClassInternals {
    VariableDecl(Box<VariableDecl>),
    Function(Box<Function>),
}

#[derive(Default, Debug, Clone)]
struct Class {
    identifier: String,
    children: Vec<ClassInternals>,
}

#[derive(Debug, Clone)]
enum ScopeImpl {
    Statement(Box<Statement>),
    Scope(Box<Scope>),
}

#[derive(Default, Debug, Clone)]
struct Scope {
    children: Vec<ScopeImpl>,
}

#[derive(Debug, Clone)]
enum ValueExpression {
    Expression(Box<Expression>),
    Scope(Box<Scope>),
}

#[derive(Debug, Clone)]
enum ArrayDecl {
    Sized(u64),
    Initialized(Vec<ValueExpression>),
}

#[derive(Debug, Clone)]
enum VariableAssignmentExpression {
    ArrayDecl(Box<ArrayDecl>),
    Expression(Box<Expression>),
    Scope(Box<Scope>),
}

#[derive(Default, Debug, Clone)]
struct VariableDecl {
    identifier: String,
    assigning_expression: Option<VariableAssignmentExpression>,
}

#[derive(Debug, Clone)]
enum AssignmentImpl {
    Expression(Box<Expression>),
    Scope(Box<Scope>),
}

#[derive(Debug, Clone)]
enum Statement {
    Return(Box<Variable>),
    VariableDecl(Box<VariableDecl>),
    ControlFlow(Box<ControlFlow>),
    Assignment(Variable, AssignmentImpl),
}

#[derive(Debug, Clone)]
enum ControlFlowConditionImpl {
    Expression(Box<Expression>),
    VariableDecl(Box<VariableDecl>),
    VariableDeclCompare(Box<VariableDecl>, CompareOps, Box<Expression>),
}

#[derive(Debug, Clone)]
enum ControlFlow {
    If(Box<ControlFlowIf>),
    While(Box<ControlFlowWhile>),
    For(Box<ControlFlowFor>),
}

#[derive(Debug, Clone)]
struct ControlFlowIf {
    condition: ControlFlowConditionImpl,
    if_true: Box<Scope>,
    if_false: Option<Box<Scope>>,
}

#[derive(Debug, Clone)]
struct ControlFlowWhile {
    condition: ControlFlowConditionImpl,
    scope: Box<Scope>,
}

#[derive(Debug, Clone)]
struct ControlFlowFor {
    expression: Box<ForExpression>,
    scope: Box<Scope>,
}

#[derive(Debug, Clone)]
enum ForInitImpl {
    VariableDecl(Box<VariableDecl>),
    ForInit(Box<VariableDecl>, Box<ForInitImpl>),
}

#[derive(Debug, Clone)]
enum ForIncrementImpl {
    Expression(Box<Expression>),
    ForIncrement(Box<Expression>, Box<ForIncrementImpl>),
}

#[derive(Debug, Clone)]
struct ForExpression {
    init: Option<ForInitImpl>,
    compare_expression: Box<Expression>,
    increment_expression: Option<ForIncrementImpl>,
}

#[derive(Debug, Clone)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    AddEquals,
    SubEquals,
    MulEquals,
    DivEquals,
}

#[derive(Debug, Clone)]
enum Expression {
    Primary(Box<Primary>),
    Unary(UnaryOp, Box<Expression>),
    Comparison(Box<Expression>, CompareOps, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
}

#[derive(Debug, Clone)]
enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, Clone)]
enum Primary {
    Variable(Box<Variable>),
    True,
    False,
    Nil,
    Expression(Box<Expression>),
    Number(String),
    String(String),
}

#[derive(Debug, Clone)]
enum Variable {
    Variable(Box<Variable>, Box<Variable>),
    Identifier(String),
}

struct ASTError {
    line: u64,
    char_in_line: u64,
    message: String,
}

impl Display for ASTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Parser error occurred at {}:{}. Diagnostic returned {}",
            self.line, self.char_in_line, &self.message
        ))
    }
}

impl ASTError {
    fn new(message: &str, token: &tokenizer::Token) -> Self {
        ASTError {
            line: token.line,
            char_in_line: token.character_in_line,
            message: message.to_owned(),
        }
    }

    fn eof() -> Self {
        ASTError{
            line: 0,
            char_in_line: 0,
            message: String::from("You have hit the end of the file. Good luck with that. Please write better code n00b")
        }
    }
}

struct TokenIter<'a> {
    iter: Peekable<Iter<'a, tokenizer::Token>>,
}

impl<'a> TokenIter<'a> {
    fn new(vec: &'a mut Vec<tokenizer::Token>) -> Self {
        TokenIter {
            iter: vec.iter().peekable(),
        }
    }

    fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    fn consume_if(&mut self, consume: Lexeme) -> bool {
        if matches!(self.iter.peek(), Some(x) if x.token_type == consume) {
            self.iter.next();
            return true;
        }
        false
    }

    fn loop_consume_if(&mut self, consume: Lexeme) -> Result<bool, ASTError> {
        if let Some(value) = self.iter.peek() {
            if value.token_type == consume {
                self.iter.next();
                return Ok(true);
            } else {
                return Ok(false);
            }
        } else {
            return Err(ASTError::eof());
        }
    }

    fn identifier(&mut self) -> Result<String, ASTError> {
        match self.iter.next() {
            Some(value) => match &value.token_type {
                Lexeme::Identifier(name) => Ok(name.clone()),
                _ => Err(ASTError::new(
                    &format!("Syntax Error. Expected Identifier but found {:?}", value),
                    value,
                )),
            },
            None => Err(ASTError::eof()),
        }
    }

    fn requires(&mut self, token: Lexeme) -> Result<(), ASTError> {
        match self.iter.next() {
            Some(value) => {
                if value.token_type == token {
                    Ok(())
                } else {
                    Err(ASTError::new(
                        &format!("Syntax Error. Expected {:?} but found {:?}", token, value),
                        value,
                    ))
                }
            }
            None => Err(ASTError::eof()),
        }
    }
}

struct Parser;

struct ParsedTree {
    nodes: Program,
}

impl Parser {
    fn variable_decl(iter: &mut TokenIter) -> Result<Scope, ASTError> {
        todo!()
    }

    fn parse_scope(iter: &mut TokenIter) -> Result<Scope, ASTError> {
        todo!()
    }

    fn parse_function(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Fun)) {
            let mut func = Function::default();
            func.identifier = iter.identifier()?;
            iter.requires(Lexeme::OpenCurly)?;

            func.scope = Parser::parse_scope(iter)?.into();

            iter.requires(Lexeme::CloseCurly)?;
            return Ok(Some(ProgramImpl::Function(func.into())));
        }
        Ok(None)
    }

    fn parse_class(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Class)) {
            let mut class = Class::default();
            class.identifier = iter.identifier()?;
            iter.requires(Lexeme::OpenCurly);

            while !iter.loop_consume_if(Lexeme::CloseCurly)? {
                
            }

            return Ok(Some(ProgramImpl::Class(class.into())));
        }
        Ok(None)
    }

    fn parse_program(iter: &mut TokenIter) -> Result<Program, ASTError> {
        let mut nodes = Program::default();

        while iter.has_next() {
            Parser::parse_function(iter)?.map(|v| nodes.children.push(v));
            Parser::parse_class(iter)?.map(|v| nodes.children.push(v));
        }

        Ok(nodes)
    }

    fn parse(mut iter: TokenIter) -> Result<ParsedTree, ASTError> {
        Ok(ParsedTree {
            nodes: Parser::parse_program(&mut iter)?,
        })
    }
}

fn main() {
    let mut tokenizer = Tokenizer::new("var silly = 32;");

    tokenizer.tokenize();

    let mut parsed_tree = Parser::parse(tokenizer.tokens.to_token_iter());

    println!("{:?}", &mut tokenizer.tokens);
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
    fn to_token_iter(&'a mut self) -> Self::IterResult;
}

impl<'a> From<&'a mut Vec<tokenizer::Token>> for TokenIter<'a> {
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
