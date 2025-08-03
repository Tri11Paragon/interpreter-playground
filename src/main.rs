use std::fmt::Display;
use std::{iter::Peekable, marker::PhantomData, slice::Iter};

use crate::Primary::Expression;
use tokenizer::Keyword;
use tokenizer::Lexeme;
use tokenizer::Tokenizer;

pub mod tokenizer;

macro_rules! syntax_error {
    ($found:expr $(,)?) => {{
        Err(ASTError::new(
            &format!("Syntax Error. Unexpected token {:?}", $found.token_type),
            $found,
        ))
    }};
    ($found:expr, $expected:literal $(,)?) => {{
        Err(ASTError::new(
            &format!("Syntax Error. Unexpected token {:?}, {}", $found.token_type, $expected),
            $found
        ))
    }};
    ($found:expr, $expected:expr $(,)?) => {{
        Err(ASTError::new(
            &format!(
                "Syntax Error. Unexpected token {:?}, expected {:?}",
                $found.token_type,
                $expected,
            ),
            &$found,
        ))
    }};
    ($found:expr, $first:expr, $($expected:expr),+ $(,)?) => {{
        // Collect all `expected` items (including `$first`) into a vec
        let expected_list = {
            let mut v = Vec::new();
            v.push(format!("{:?}", $first));
            $( v.push(format!("{:?}", $expected)); )+
            v.join(", ")
        };

        Err(ASTError::new(
            &format!(
                "Syntax Error. Unexpected token {:?}, expected one of: {}",
                $found.token_type,
                expected_list,
            ),
            &$found,
        ))
    }};
}

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
    inputs: Vec<String>,
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
enum VariableAssignmentExpression {
    ArrayDecl(Box<ArrayDecl>),
    Expression(Box<Expression>),
    Scope(Box<Scope>),
}

#[derive(Debug, Clone)]
enum ArrayDecl {
    Sized(u64),
    Initialized(ArrayLiteral),
}

#[derive(Default, Debug, Clone)]
struct ArrayLiteral {
    values: Vec<ValueExpression>,
}

#[derive(Default, Debug, Clone)]
struct VariableDecl {
    is_const: bool,
    identifier: String,
    assigning_expression: Option<VariableAssignmentExpression>,
}

#[derive(Debug, Clone)]
enum Statement {
    Return(VariableAssignmentExpression),
    VariableDecl(Box<VariableDecl>),
    ControlFlow(Box<ControlFlow>),
    Assignment(Variable, ValueExpression),
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

impl std::fmt::Debug for ASTError {
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
        ASTError {
            line: 0,
            char_in_line: 0,
            message: String::from("You have hit the end of the file. Good luck with that. Please write better code n00b"),
        }
    }
}

struct TokenIter<'a> {
    iter: Peekable<Iter<'a, tokenizer::Token>>,
}

impl<'a> TokenIter<'a> {
    fn new(vec: &'a mut [tokenizer::Token]) -> Self {
        TokenIter {
            iter: vec.iter().peekable(),
        }
    }

    fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    fn peek(&mut self) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.peek() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
    }

    fn forward(&self, amount: usize) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.clone().skip(amount).next() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
    }

    fn next(&mut self) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.next() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
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
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Err(ASTError::eof())
        }
    }

    fn identifier(&mut self) -> Result<String, ASTError> {
        match self.iter.next() {
            Some(value) => match &value.token_type {
                Lexeme::Identifier(name) => Ok(name.clone()),
                _ => syntax_error!(value, Lexeme::Identifier(String::from("?"))),
            },
            None => Err(ASTError::eof()),
        }
    }

    fn collect_identifier(&mut self) -> Result<Vec<String>, ASTError> {
        let mut idents = Vec::new();
        while let Lexeme::Identifier(name) = self.peek()?.token_type.clone() {
            idents.push(name);
            self.next()?;
            if let Lexeme::Comma = self.peek()?.token_type {
                self.next()?;
            } else {
                break;
            }
        }
        Ok(idents)
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
    fn parse_assignment(iter: &mut TokenIter) -> Result<VariableAssignmentExpression, ASTError> {
        match iter.peek()?.token_type {
            Lexeme::OpenSquare => {
                iter.next()?;
                if let Lexeme::Integer(value) = &iter.peek()?.token_type.clone() {
                    if iter.forward(1)?.token_type == Lexeme::CloseSquare {
                        return Ok(VariableAssignmentExpression::ArrayDecl(
                            ArrayDecl::Sized(value.parse().map_err(|_| {
                                ASTError::new("Failed to parse integer", iter.peek().unwrap())
                            })?)
                            .into(),
                        ));
                    }
                }
                let mut arr = ArrayLiteral::default();
                while iter.peek()?.token_type != Lexeme::CloseSquare {
                    arr.values.push(Parser::parse_value_expression(iter)?);
                    if iter.peek()?.token_type == Lexeme::Comma {
                        iter.next()?;
                    } else if iter.peek()?.token_type != Lexeme::CloseSquare {
                        return syntax_error!(&iter.peek()?, Lexeme::Comma, Lexeme::CloseSquare);
                    }
                }
                Ok(VariableAssignmentExpression::ArrayDecl(
                    ArrayDecl::Initialized(arr).into(),
                ))
            }
            Lexeme::OpenCurly => Ok(VariableAssignmentExpression::Scope(
                Parser::parse_scope(iter)?.into(),
            )),
            _ => Ok(VariableAssignmentExpression::Expression(
                Parser::parse_expression(iter)?.into(),
            )),
        }
    }
    fn parse_variable_decl(iter: &mut TokenIter) -> Result<VariableDecl, ASTError> {
        let is_const = iter.next()?.token_type == Lexeme::Keyword(Keyword::Const);
        match iter.peek()?.token_type.clone() {
            Lexeme::Identifier(name) => {
                iter.next()?;
                if iter.peek()?.token_type == Lexeme::Semicolon {
                    return Ok(VariableDecl {
                        is_const,
                        identifier: name,
                        assigning_expression: None,
                    });
                }
                Ok(VariableDecl {
                    is_const,
                    identifier: name,
                    assigning_expression: Self::parse_assignment(iter)?.into(),
                })
            }
            _ => {
                syntax_error!(&iter.peek()?, Lexeme::Identifier(String::from("?")))
            }
        }
    }

    fn parse_value_expression(iter: &mut TokenIter) -> Result<ValueExpression, ASTError> {
        match iter.peek()?.token_type {
            Lexeme::OpenCurly => Ok(ValueExpression::Scope(Parser::parse_scope(iter)?.into())),
            _ => Ok(ValueExpression::Expression(
                Parser::parse_expression(iter)?.into(),
            )),
        }
    }

    fn parse_variable(iter: &mut TokenIter) -> Result<Variable, ASTError> {
        let token = iter.next()?.clone();
        match token.token_type {
            Lexeme::Identifier(name) => {
                if iter.peek()?.token_type == Lexeme::Dot {
                    iter.next()?;
                    Ok(Variable::Variable(
                        Variable::Identifier(name).into(),
                        Parser::parse_variable(iter)?.into(),
                    ))
                } else {
                    Ok(Variable::Identifier(name))
                }
            }
            _ => syntax_error!(&token),
        }
    }

    fn parse_expression(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        match iter.peek()?.token_type.clone() {
            Lexeme::OpenParen => {
                iter.next()?;
                let expr = Parser::parse_expression(iter)?;
                iter.requires(Lexeme::CloseParen)?;
                Ok(Expression::Primary(Primary::Expression(expr.into()).into()))
            }
            Lexeme::Decimal(Val) | Lexeme::Integer(Val) => {
                Ok(Expression::Primary(Primary::Number(Val).into()))
            }
            Lexeme::Keyword(keyword) => Ok(Expression::Primary(
                match keyword {
                    Keyword::True => Primary::True,
                    Keyword::False => Primary::False,
                    Keyword::Nil => Primary::Nil,
                    _ => {
                        return syntax_error!(
                            &iter.peek()?,
                            Lexeme::Keyword(Keyword::True),
                            Lexeme::Keyword(Keyword::False),
                            Lexeme::Keyword(Keyword::Nil)
                        )
                    }
                }
                .into(),
            )),
            Lexeme::Identifier(_) => Ok(Expression::Primary(
                Primary::Variable(Parser::parse_variable(iter)?.into()).into(),
            )),
            
        }
    }

    fn parse_statement(iter: &mut TokenIter) -> Result<Statement, ASTError> {
        let token = iter.peek()?;
        match &token.token_type {
            Lexeme::Keyword(keyword) => match keyword {
                Keyword::Var | Keyword::Const => Ok(Statement::VariableDecl(
                    Parser::parse_variable_decl(iter)?.into(),
                )),
                Keyword::If => {
                    todo!()
                }
                Keyword::While => {
                    todo!()
                }
                Keyword::For => {
                    todo!()
                }
                Keyword::Return => {
                    iter.next()?;
                    Ok(Statement::Return(Parser::parse_assignment(iter)?))
                }
                _ => {
                    syntax_error!(
                        token,
                        Keyword::Var,
                        Keyword::Const,
                        Keyword::If,
                        Keyword::While,
                        Keyword::For,
                        Keyword::Return
                    )
                }
            },
            Lexeme::Identifier(_) => {
                let variable = Parser::parse_variable(iter)?;
                iter.requires(Lexeme::Assignment)?;
                let token = iter.peek()?;
                Ok(Statement::Assignment(
                    variable,
                    Parser::parse_value_expression(iter)?.into(),
                ))
            }
            _ => {
                syntax_error!(
                    token,
                    "Expected variable declaration, control flow, or variable assignment."
                )
            }
        }
    }

    fn parse_scope(iter: &mut TokenIter) -> Result<Scope, ASTError> {
        println!("Parsing scope");
        let mut scope = Scope::default();
        iter.requires(Lexeme::OpenCurly)?;

        while !iter.loop_consume_if(Lexeme::CloseCurly)? {
            if iter.peek()?.token_type == Lexeme::OpenCurly {
                println!("Found subscope");
                scope
                    .children
                    .push(ScopeImpl::Scope(Parser::parse_scope(iter)?.into()));
            } else {
                println!("found statement");
                scope
                    .children
                    .push(ScopeImpl::Statement(Parser::parse_statement(iter)?.into()));
            }
        }
        Ok(scope)
    }

    fn try_parse_function(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Fun)) {
            println!("Trying to parse function");
            let identifier = iter.identifier()?;
            iter.requires(Lexeme::OpenParen)?;
            let inputs = iter.collect_identifier()?;
            iter.requires(Lexeme::CloseParen)?;
            let scope = Parser::parse_scope(iter)?.into();
            return Ok(Some(ProgramImpl::Function(
                Function {
                    identifier,
                    inputs,
                    scope,
                }
                .into(),
            )));
        }
        Ok(None)
    }

    fn try_parse_class(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Class)) {
            println!("Trying to parse class");
            let mut class = Class {
                identifier: iter.identifier()?,
                ..Class::default()
            };
            iter.requires(Lexeme::OpenCurly)?;

            while !iter.loop_consume_if(Lexeme::CloseCurly)? {
                todo!()
            }

            return Ok(Some(ProgramImpl::Class(class.into())));
        }
        Ok(None)
    }

    fn parse_program(iter: &mut TokenIter) -> Result<Program, ASTError> {
        let mut nodes = Program::default();

        println!("Parsing program!");

        while iter.has_next() {
            println!("{:?}", iter.peek()?);
            if let Some(v) = Parser::try_parse_function(iter)? {
                nodes.children.push(v)
            }
            if let Some(v) = Parser::try_parse_class(iter)? {
                nodes.children.push(v)
            }
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
    let mut tokenizer = Tokenizer::new("fun sillybilly () {a.b = 0;}");

    tokenizer.tokenize();

    println!("{:?}", &mut tokenizer.tokens);

    let parsed_tree = Parser::parse(tokenizer.tokens.to_token_iter());

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
