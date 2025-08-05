use crate::tokenizer;
use crate::tokenizer::{Keyword, Lexeme};
use std::fmt::Display;
use std::iter::Peekable;
use std::slice::Iter;

#[macro_export]
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
pub enum CompareOps {
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
}

#[derive(Debug, Clone)]
pub enum ProgramImpl {
    Class(Box<Class>),
    Function(Box<Function>),
}

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub children: Vec<ProgramImpl>,
}

#[derive(Default, Debug, Clone)]
pub struct Function {
    pub identifier: String,
    pub inputs: Vec<String>,
    pub scope: Box<Scope>,
}

#[derive(Default, Debug, Clone)]
pub struct Class {
    pub identifier: String,
    pub functions: Vec<Function>,
    pub variables: Vec<VariableDecl>,
}

#[derive(Debug, Clone)]
pub enum ScopeImpl {
    Statement(Box<Statement>),
    Scope(Box<Scope>),
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub children: Vec<ScopeImpl>,
}

#[derive(Default, Debug, Clone)]
pub struct ClassConstruction {
    pub name: String,
    pub assignments: Vec<NamedAssignment>,
}

#[derive(Debug, Clone)]
pub struct NamedAssignment {
    pub class_member: String,
    pub value: ValueExpression,
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub callie: Variable,
    pub args: Vec<ValueExpression>,
}

#[derive(Debug, Clone)]
pub enum ValueExpression {
    ClassConstruction(Box<ClassConstruction>),
    ArrayDecl(Box<ArrayDecl>),
    Expression(Box<Expression>),
    Scope(Box<Scope>),
}

#[derive(Debug, Clone)]
pub enum ArrayDecl {
    Sized(u64),
    Initialized(ArrayLiteral),
}

#[derive(Default, Debug, Clone)]
pub struct ArrayLiteral {
    pub values: Vec<ValueExpression>,
}

#[derive(Default, Debug, Clone)]
pub struct VariableDecl {
    pub is_const: bool,
    pub identifier: String,
    pub assigning_expression: Option<ValueExpression>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(ValueExpression),
    VariableDecl(Box<VariableDecl>),
    ControlFlow(Box<ControlFlow>),
    Assignment(Variable, ValueExpression),
    FunctionCall(Box<FunctionCall>),
    Print(ValueExpression),
}

#[derive(Debug, Clone)]
pub enum ControlFlowConditionImpl {
    Expression(Box<Expression>),
    VariableDecl(Box<VariableDecl>),
    VariableDeclCompare(Box<VariableDecl>, CompareOps, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    If(Box<ControlFlowIf>),
    While(Box<ControlFlowWhile>),
    For(Box<ControlFlowFor>),
}

#[derive(Debug, Clone)]
pub struct ControlFlowIf {
    pub condition: ControlFlowConditionImpl,
    pub if_true: Box<Scope>,
    pub if_false: Option<Box<Scope>>,
}

#[derive(Debug, Clone)]
pub struct ControlFlowWhile {
    pub condition: ControlFlowConditionImpl,
    pub scope: Box<Scope>,
}

#[derive(Debug, Clone)]
pub struct ControlFlowFor {
    pub expression: Box<ForExpression>,
    pub scope: Box<Scope>,
}

#[derive(Debug, Clone)]
pub enum ForInitImpl {
    VariableDecl(Box<VariableDecl>),
    ForInit(Box<VariableDecl>, Box<ForInitImpl>),
}

#[derive(Debug, Clone)]
pub enum ForInternalImpl {
    Expression(Box<Expression>),
    ForInternal(Box<Expression>, Box<ForInternalImpl>),
}

#[derive(Debug, Clone)]
pub struct ForExpression {
    pub init: Option<ForInitImpl>,
    pub compare_expression: Option<ForInternalImpl>,
    pub increment_expression: Option<ForInternalImpl>,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
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
pub enum Expression {
    Primary(Box<Primary>),
    Unary(UnaryOp, Box<Expression>),
    Comparison(Box<Expression>, CompareOps, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, Clone)]
pub enum Primary {
    Variable(Box<Variable>),
    True,
    False,
    Nil,
    Expression(Box<Expression>),
    Number(String),
    String(String),
    FunctionCall(Box<FunctionCall>),
}

#[derive(Debug, Clone)]
pub enum ArrayAccessor {
    Number(String),
    Variable(Box<Variable>),
}

#[derive(Debug, Clone)]
pub enum Variable {
    ArrayAccess(ArrayAccessor),
    MemberAccess(Box<Variable>, Box<Variable>),
    Identifier(String),
}

pub struct ASTError {
    pub line: u64,
    pub char_in_line: u64,
    pub message: String,
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
    pub fn new(message: &str, token: &tokenizer::Token) -> Self {
        ASTError {
            line: token.line,
            char_in_line: token.character_in_line,
            message: message.to_owned(),
        }
    }

    pub fn eof() -> Self {
        ASTError {
            line: 0,
            char_in_line: 0,
            message: String::from("You have hit the end of the file. Good luck with that. Please write better code n00b"),
        }
    }
}

macro_rules! debug {
    ($($arg:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($arg)*));
    };
}

pub struct TokenIter<'a> {
    pub iter: Peekable<Iter<'a, tokenizer::Token>>,
}

impl<'a> TokenIter<'a> {
    pub fn new(vec: &'a mut [tokenizer::Token]) -> Self {
        TokenIter {
            iter: vec.iter().peekable(),
        }
    }

    pub fn has_next(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    pub fn peek(&mut self) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.peek() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
    }

    pub fn forward(&self, amount: usize) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.clone().skip(amount).next() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
    }

    pub fn next(&mut self) -> Result<&tokenizer::Token, ASTError> {
        match self.iter.next() {
            Some(val) => Ok(val),
            None => Err(ASTError::eof()),
        }
    }

    pub fn consume_if(&mut self, consume: Lexeme) -> bool {
        if matches!(self.iter.peek(), Some(x) if x.token_type == consume) {
            self.iter.next();
            return true;
        }
        false
    }

    pub fn loop_consume_if(&mut self, consume: Lexeme) -> Result<bool, ASTError> {
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

    pub fn identifier(&mut self) -> Result<String, ASTError> {
        match self.iter.next() {
            Some(value) => match &value.token_type {
                Lexeme::Identifier(name) => Ok(name.clone()),
                _ => syntax_error!(value, Lexeme::Identifier(String::from("?"))),
            },
            None => Err(ASTError::eof()),
        }
    }

    pub fn collect_identifier(&mut self) -> Result<Vec<String>, ASTError> {
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

    pub fn requires(&mut self, token: Lexeme) -> Result<(), ASTError> {
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

pub struct Parser;

pub struct ParsedTree {
    pub nodes: Program,
}

impl Parser {
    pub fn parse_variable_decl(iter: &mut TokenIter) -> Result<VariableDecl, ASTError> {
        debug!("Parsing variable decl");
        let is_const = iter.next()?.token_type == Lexeme::Keyword(Keyword::Const);
        match iter.peek()?.token_type.clone() {
            Lexeme::Identifier(name) => {
                iter.next()?;
                if iter.next()?.token_type == Lexeme::Semicolon {
                    return Ok(VariableDecl {
                        is_const,
                        identifier: name,
                        assigning_expression: None,
                    });
                }
                let result = Ok(VariableDecl {
                    is_const,
                    identifier: name,
                    assigning_expression: Self::parse_value_expression(iter)?.into(),
                });
                iter.requires(Lexeme::Semicolon)?;
                result
            }
            _ => {
                syntax_error!(&iter.peek()?, Lexeme::Identifier(String::from("?")))
            }
        }
    }

    pub fn parse_named_assignment(iter: &mut TokenIter) -> Result<NamedAssignment, ASTError> {
        debug!("Parsing named assignment");
        let ident = iter.identifier()?;
        iter.requires(Lexeme::Assignment)?;
        Ok(NamedAssignment {
            class_member: ident,
            value: Self::parse_value_expression(iter)?,
        })
    }

    pub fn parse_value_expression(iter: &mut TokenIter) -> Result<ValueExpression, ASTError> {
        debug!("Parsing value expression");
        match iter.peek()?.token_type {
            Lexeme::OpenCurly => Ok(ValueExpression::Scope(Self::parse_scope(iter)?.into())),
            Lexeme::OpenSquare => {
                iter.next()?;
                if let Lexeme::Integer(value) = &iter.peek()?.token_type.clone() {
                    if iter.forward(1)?.token_type == Lexeme::CloseSquare {
                        return Ok(ValueExpression::ArrayDecl(
                            ArrayDecl::Sized(value.parse().map_err(|_| {
                                ASTError::new("Failed to parse integer", iter.peek().unwrap())
                            })?)
                            .into(),
                        ));
                    }
                }
                let mut arr = ArrayLiteral::default();
                while iter.peek()?.token_type != Lexeme::CloseSquare {
                    arr.values.push(Self::parse_value_expression(iter)?);
                    if iter.peek()?.token_type == Lexeme::Comma {
                        iter.next()?;
                    } else if iter.peek()?.token_type != Lexeme::CloseSquare {
                        return syntax_error!(&iter.peek()?, Lexeme::Comma, Lexeme::CloseSquare);
                    }
                }
                Ok(ValueExpression::ArrayDecl(
                    ArrayDecl::Initialized(arr).into(),
                ))
            }
            Lexeme::Tilde => {
                iter.next()?;
                let class_name = iter.identifier()?;
                iter.requires(Lexeme::OpenCurly)?;
                let mut assignments = Vec::new();
                while !iter.loop_consume_if(Lexeme::CloseCurly)? {
                    assignments.push(Self::parse_named_assignment(iter)?);
                    match iter.peek()?.token_type {
                        Lexeme::Comma => {
                            iter.next()?;
                        }
                        Lexeme::CloseCurly => {
                            iter.next()?;
                            break;
                        }
                        _ => break,
                    }
                }
                Ok(ValueExpression::ClassConstruction(
                    ClassConstruction {
                        name: class_name,
                        assignments,
                    }
                    .into(),
                ))
            }
            _ => Ok(ValueExpression::Expression(
                Self::parse_expression(iter)?.into(),
            )),
        }
    }

    pub fn parse_variable(iter: &mut TokenIter) -> Result<Variable, ASTError> {
        debug!("Parsing variable");
        let token = iter.next()?.clone();
        match token.token_type {
            Lexeme::Identifier(name) => match iter.peek()?.token_type {
                Lexeme::Dot => {
                    iter.next()?;
                    Ok(Variable::MemberAccess(
                        Variable::Identifier(name).into(),
                        Self::parse_variable(iter)?.into(),
                    ))
                }
                Lexeme::OpenSquare => {
                    iter.next()?;
                    if let Lexeme::Integer(val) = iter.peek()?.token_type.clone() {
                        iter.next()?;
                        iter.requires(Lexeme::CloseSquare)?;
                        Ok(Variable::ArrayAccess(ArrayAccessor::Number(val)))
                    } else {
                        let variable = Self::parse_variable(iter)?;
                        iter.requires(Lexeme::CloseSquare)?;
                        Ok(Variable::ArrayAccess(ArrayAccessor::Variable(
                            variable.into(),
                        )))
                    }
                }
                _ => Ok(Variable::Identifier(name)),
            },
            _ => syntax_error!(&token),
        }
    }

    pub fn parse_function_arguments(
        variable: Variable,
        iter: &mut TokenIter,
    ) -> Result<FunctionCall, ASTError> {
        debug!("Parsing function arguments");
        iter.requires(Lexeme::OpenParen)?;
        let mut args = Vec::new();
        while !iter.loop_consume_if(Lexeme::CloseParen)? {
            args.push(Self::parse_value_expression(iter)?);
            if iter.peek()?.token_type == Lexeme::Comma {
                iter.next()?;
            } else {
                break;
            }
        }
        Ok(FunctionCall {
            callie: variable,
            args,
        })
    }

    pub fn parse_primary(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing primary");
        match iter.peek()?.token_type.clone() {
            Lexeme::OpenParen => {
                iter.next()?;
                let expr = Self::parse_expression(iter)?;
                iter.requires(Lexeme::CloseParen)?;
                Ok(Expression::Primary(Primary::Expression(expr.into()).into()))
            }
            Lexeme::Decimal(val) | Lexeme::Integer(val) => {
                iter.next()?;
                Ok(Expression::Primary(Primary::Number(val).into()))
            }
            Lexeme::DoubleQuotes(v) => {
                iter.next()?;
                Ok(Expression::Primary(Primary::String(v).into()))
            }
            Lexeme::SingleQuotes(c) => {
                iter.next()?;
                Ok(Expression::Primary(Primary::String(String::from(c)).into()))
            }
            Lexeme::Keyword(keyword) => {
                iter.next()?;
                Ok(Expression::Primary(
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
                ))
            }
            Lexeme::Identifier(_) => {
                let variable = Self::parse_variable(iter)?;
                if iter.peek()?.token_type == Lexeme::OpenParen {
                    Ok(Expression::Primary(
                        Primary::FunctionCall(
                            Self::parse_function_arguments(variable, iter)?.into(),
                        )
                        .into(),
                    ))
                } else {
                    Ok(Expression::Primary(
                        Primary::Variable(variable.into()).into(),
                    ))
                }
            }
            _ => syntax_error!(
                &iter.peek()?,
                Lexeme::OpenParen,
                Lexeme::Decimal(String::from("?")),
                Lexeme::Integer(String::from("?")),
                Lexeme::Keyword(Keyword::True),
                Lexeme::Keyword(Keyword::False),
                Lexeme::Keyword(Keyword::Nil),
                Lexeme::Identifier(String::from("?"))
            ),
        }
    }

    pub fn parse_unary(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing unary");
        match iter.peek()?.token_type {
            Lexeme::Exclamation => {
                iter.next()?;
                Ok(Expression::Unary(
                    UnaryOp::Not,
                    Self::parse_unary(iter)?.into(),
                ))
            }
            Lexeme::Minus => {
                iter.next()?;
                Ok(Expression::Unary(
                    UnaryOp::Negate,
                    Self::parse_unary(iter)?.into(),
                ))
            }
            _ => Self::parse_primary(iter),
        }
    }

    pub fn parse_factor(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing factor");
        let lhs = Self::parse_unary(iter)?;
        let op_type = match iter.peek()?.token_type {
            Lexeme::Slash => {
                iter.next()?;
                match iter.peek()?.token_type {
                    Lexeme::Equals => {
                        iter.next()?;
                        BinaryOp::DivEquals
                    }
                    _ => BinaryOp::Div,
                }
            }
            Lexeme::Star => {
                iter.next()?;
                match iter.peek()?.token_type {
                    Lexeme::Equals => {
                        iter.next()?;
                        BinaryOp::MulEquals
                    }
                    _ => BinaryOp::Mul,
                }
            }
            _ => return Ok(lhs),
        };
        Ok(Expression::BinaryOp(
            lhs.into(),
            op_type,
            Self::parse_factor(iter)?.into(),
        ))
    }

    pub fn parse_term(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing Term");
        let lhs = Self::parse_factor(iter)?;
        let op_type = match iter.peek()?.token_type {
            Lexeme::Plus => {
                iter.next()?;
                match iter.peek()?.token_type {
                    Lexeme::Equals => {
                        iter.next()?;
                        BinaryOp::AddEquals
                    }
                    _ => BinaryOp::Add,
                }
            }
            Lexeme::Minus => {
                iter.next()?;
                match iter.peek()?.token_type {
                    Lexeme::Equals => {
                        iter.next()?;
                        BinaryOp::SubEquals
                    }
                    _ => BinaryOp::Sub,
                }
            }
            _ => return Ok(lhs),
        };
        Ok(Expression::BinaryOp(
            lhs.into(),
            op_type,
            Self::parse_term(iter)?.into(),
        ))
    }

    pub fn parse_comparison(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing Comparision");
        let lhs = Self::parse_term(iter)?;
        let op = match iter.peek()?.token_type {
            Lexeme::Less => {
                iter.next()?;
                CompareOps::Less
            }
            Lexeme::LessEquals => {
                iter.next()?;
                CompareOps::LessEquals
            }
            Lexeme::Greater => {
                iter.next()?;
                CompareOps::Greater
            }
            Lexeme::GreaterEquals => {
                iter.next()?;
                CompareOps::GreaterEquals
            }
            _ => return Ok(lhs),
        };
        Ok(Expression::Comparison(
            lhs.into(),
            op,
            Self::parse_comparison(iter)?.into(),
        ))
    }

    pub fn parse_expression(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        debug!("Parsing expression");
        let lhs = Self::parse_comparison(iter)?;
        let op = match iter.peek()?.token_type {
            Lexeme::Equals => {
                iter.next()?;
                CompareOps::Equals
            }
            Lexeme::NotEquals => {
                iter.next()?;
                CompareOps::NotEquals
            }
            _ => return Ok(lhs),
        };
        Ok(Expression::Comparison(
            lhs.into(),
            op,
            Self::parse_expression(iter)?.into(),
        ))
    }

    pub fn parse_control_flow_conditional(
        iter: &mut TokenIter,
    ) -> Result<ControlFlowConditionImpl, ASTError> {
        debug!("Parsing control flow conditional");
        Ok(match iter.peek()?.token_type.clone() {
            Lexeme::Keyword(keyword) => match keyword {
                Keyword::Var | Keyword::Class => {
                    iter.next()?;
                    ControlFlowConditionImpl::VariableDecl(Self::parse_variable_decl(iter)?.into())
                }
                _ => return syntax_error!(iter.peek()?, "Expected variable decl"),
            },
            Lexeme::OpenParen => {
                iter.next()?;
                let variable_decl = Self::parse_variable_decl(iter)?;
                iter.requires(Lexeme::CloseParen)?;
                let token = iter.next()?;
                let compare = match token.token_type {
                    Lexeme::Equals => CompareOps::Equals,
                    Lexeme::NotEquals => CompareOps::NotEquals,
                    Lexeme::Greater => CompareOps::Greater,
                    Lexeme::GreaterEquals => CompareOps::GreaterEquals,
                    Lexeme::Less => CompareOps::Less,
                    Lexeme::LessEquals => CompareOps::LessEquals,
                    _ => return syntax_error!(token),
                };
                ControlFlowConditionImpl::VariableDeclCompare(
                    variable_decl.into(),
                    compare,
                    Self::parse_expression(iter)?.into(),
                )
            }
            _ => ControlFlowConditionImpl::Expression(Self::parse_expression(iter)?.into()),
        })
    }

    pub fn parse_if(iter: &mut TokenIter) -> Result<ControlFlowIf, ASTError> {
        debug!("Parsing IF");
        iter.requires(Lexeme::OpenParen)?;
        let condition = Self::parse_control_flow_conditional(iter)?;
        iter.requires(Lexeme::CloseParen)?;
        let true_scope = Self::parse_scope(iter)?;
        if iter.peek()?.token_type == Lexeme::Keyword(Keyword::Else) {
            let false_scope = Self::parse_scope(iter)?;
            Ok(ControlFlowIf {
                condition,
                if_true: true_scope.into(),
                if_false: Some(false_scope.into()),
            })
        } else {
            Ok(ControlFlowIf {
                condition,
                if_true: true_scope.into(),
                if_false: None,
            })
        }
    }

    pub fn parse_while(iter: &mut TokenIter) -> Result<ControlFlowWhile, ASTError> {
        debug!("Parsing while");
        iter.requires(Lexeme::OpenParen)?;
        let condition = Self::parse_control_flow_conditional(iter)?;
        iter.requires(Lexeme::CloseParen)?;
        let scope = Self::parse_scope(iter)?;
        Ok(ControlFlowWhile {
            condition,
            scope: scope.into(),
        })
    }

    pub fn parse_for_expression(iter: &mut TokenIter) -> Result<Option<ForInternalImpl>, ASTError> {
        debug!("Parsing For Expression");
        let mut for_init = Option::<ForInternalImpl>::None;
        loop {
            match iter.peek()?.token_type {
                Lexeme::Semicolon | Lexeme::CloseParen => {
                    iter.next()?;
                    break;
                }
                _ => {
                    let expression = Self::parse_expression(iter)?;
                    match for_init {
                        Some(val) => {
                            for_init =
                                Some(ForInternalImpl::ForInternal(expression.into(), val.into()));
                        }
                        None => {
                            for_init = Some(ForInternalImpl::Expression(expression.into()));
                        }
                    }
                    if iter.peek()?.token_type == Lexeme::Comma {
                        iter.next()?;
                    }
                }
            }
        }
        Ok(for_init)
    }

    pub fn parse_for(iter: &mut TokenIter) -> Result<ControlFlowFor, ASTError> {
        debug!("Parse For");
        iter.requires(Lexeme::OpenParen)?;
        let mut for_init = Option::<ForInitImpl>::None;
        while !iter.loop_consume_if(Lexeme::Semicolon)? {
            let variable_decl = Self::parse_variable_decl(iter)?;
            match for_init {
                Some(val) => {
                    for_init = Some(ForInitImpl::ForInit(variable_decl.into(), val.into()));
                }
                None => {
                    for_init = Some(ForInitImpl::VariableDecl(variable_decl.into()));
                }
            }
            if iter.peek()?.token_type == Lexeme::Comma {
                iter.next()?;
            }
        }
        let for_compare = Self::parse_for_expression(iter)?;
        let for_increment = Self::parse_for_expression(iter)?;
        let scope = Self::parse_scope(iter)?;
        Ok(ControlFlowFor {
            expression: ForExpression {
                init: for_init,
                compare_expression: for_compare,
                increment_expression: for_increment,
            }
            .into(),
            scope: scope.into(),
        })
    }

    pub fn parse_statement(iter: &mut TokenIter) -> Result<Statement, ASTError> {
        debug!("Parsing Statement");
        let token = iter.peek()?.clone();
        match token.token_type.clone() {
            Lexeme::Keyword(keyword) => match keyword {
                Keyword::Var | Keyword::Const => Ok(Statement::VariableDecl(
                    Self::parse_variable_decl(iter)?.into(),
                )),
                Keyword::If => {
                    iter.next()?;
                    Ok(Statement::ControlFlow(
                        ControlFlow::If(Self::parse_if(iter)?.into()).into(),
                    ))
                }
                Keyword::While => {
                    iter.next()?;
                    Ok(Statement::ControlFlow(
                        ControlFlow::While(Self::parse_while(iter)?.into()).into(),
                    ))
                }
                Keyword::For => {
                    iter.next()?;
                    Ok(Statement::ControlFlow(
                        ControlFlow::For(Self::parse_for(iter)?.into()).into(),
                    ))
                }
                Keyword::Return => {
                    iter.next()?;
                    let result = Ok(Statement::Return(Self::parse_value_expression(iter)?));
                    iter.requires(Lexeme::Semicolon)?;
                    result
                }
                Keyword::Print => {
                    iter.next()?;
                    let result = Ok(Statement::Print(Self::parse_value_expression(iter)?));
                    iter.requires(Lexeme::Semicolon)?;
                    result
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
                let variable = Self::parse_variable(iter)?;

                match iter.peek()?.token_type {
                    Lexeme::Assignment => {
                        iter.next()?;
                        let result = Ok(Statement::Assignment(
                            variable,
                            Self::parse_value_expression(iter)?,
                        ));
                        iter.requires(Lexeme::Semicolon)?;
                        result
                    }
                    Lexeme::OpenParen => {
                        let result = Ok(Statement::FunctionCall(
                            Self::parse_function_arguments(variable, iter)?.into(),
                        ));
                        iter.requires(Lexeme::Semicolon)?;
                        result
                    }
                    _ => syntax_error!(iter.peek()?, "Expected function arguments."),
                }
            }
            _ => {
                syntax_error!(
                    &token,
                    "Expected variable declaration, control flow, function call, or variable assignment."
                )
            }
        }
    }

    pub fn parse_scope(iter: &mut TokenIter) -> Result<Scope, ASTError> {
        debug!("Parsing scope");
        let mut scope = Scope::default();
        iter.requires(Lexeme::OpenCurly)?;

        while !iter.loop_consume_if(Lexeme::CloseCurly)? {
            if iter.peek()?.token_type == Lexeme::OpenCurly {
                scope
                    .children
                    .push(ScopeImpl::Scope(Self::parse_scope(iter)?.into()));
            } else {
                scope
                    .children
                    .push(ScopeImpl::Statement(Self::parse_statement(iter)?.into()));
            }
        }
        Ok(scope)
    }

    pub fn try_parse_function(iter: &mut TokenIter) -> Result<Option<Function>, ASTError> {
        debug!("Trying to parse function");
        if iter.consume_if(Lexeme::Keyword(Keyword::Fun)) {
            debug!("Parsing function!");
            let identifier = iter.identifier()?;
            iter.requires(Lexeme::OpenParen)?;
            let inputs = iter.collect_identifier()?;
            iter.requires(Lexeme::CloseParen)?;
            let scope = Self::parse_scope(iter)?.into();
            return Ok(Some(Function {
                identifier,
                inputs,
                scope,
            }));
        }
        Ok(None)
    }

    pub fn try_parse_class(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        debug!("Trying to parse class");
        if iter.consume_if(Lexeme::Keyword(Keyword::Class)) {
            debug!("Parsing class");
            let class_name = iter.identifier()?;
            iter.requires(Lexeme::OpenCurly)?;

            let mut functions = Vec::<Function>::new();
            let mut variables = Vec::<VariableDecl>::new();

            while !iter.loop_consume_if(Lexeme::CloseCurly)? {
                match iter.peek()?.token_type {
                    Lexeme::Keyword(Keyword::Fun) => {
                        let fun = Self::try_parse_function(iter)?;
                        functions.push(fun.unwrap());
                    }
                    _ => {
                        variables.push(Self::parse_variable_decl(iter)?);
                    }
                }
            }

            return Ok(Some(ProgramImpl::Class(
                Class {
                    identifier: class_name,
                    functions,
                    variables,
                }
                .into(),
            )));
        }
        Ok(None)
    }

    pub fn parse_program(iter: &mut TokenIter) -> Result<Program, ASTError> {
        let mut nodes = Program::default();

        debug!("Parsing program!");

        while iter.has_next() {
            if let Some(v) = Self::try_parse_function(iter)? {
                nodes.children.push(ProgramImpl::Function(v.into()))
            }
            if let Some(v) = Self::try_parse_class(iter)? {
                nodes.children.push(v)
            }
        }

        Ok(nodes)
    }

    pub fn parse(mut iter: TokenIter) -> Result<ParsedTree, ASTError> {
        Ok(ParsedTree {
            nodes: Self::parse_program(&mut iter)?,
        })
    }
}
