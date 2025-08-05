use std::fmt::Display;
use crate::tokenizer;

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
    pub variables: Vec<VariableDecl>
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
    Print(ValueExpression)
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

impl Display for crate::ASTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Parser error occurred at {}:{}. Diagnostic returned {}",
            self.line, self.char_in_line, &self.message
        ))
    }
}

impl std::fmt::Debug for crate::ASTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Parser error occurred at {}:{}. Diagnostic returned {}",
            self.line, self.char_in_line, &self.message
        ))
    }
}

impl crate::ASTError {
    pub fn new(message: &str, token: &tokenizer::Token) -> Self {
        crate::ASTError {
            line: token.line,
            char_in_line: token.character_in_line,
            message: message.to_owned(),
        }
    }

    pub fn eof() -> Self {
        crate::ASTError {
            line: 0,
            char_in_line: 0,
            message: String::from("You have hit the end of the file. Good luck with that. Please write better code n00b"),
        }
    }
}