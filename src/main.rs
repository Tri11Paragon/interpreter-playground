use std::fmt::Display;
use std::{iter::Peekable, slice::Iter};

use tokenizer::Keyword;
use tokenizer::Lexeme;
use tokenizer::Tokenizer;

use ast_defs::*;

pub mod ast_defs;
pub mod tokenizer;

struct ASTError {
    line: u64,
    char_in_line: u64,
    message: String,
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
    fn parse_variable_decl(iter: &mut TokenIter) -> Result<VariableDecl, ASTError> {
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

    fn parse_named_assignment(iter: &mut TokenIter) -> Result<NamedAssignment, ASTError> {
        let ident = iter.identifier()?;
        iter.requires(Lexeme::Assignment)?;
        Ok(NamedAssignment {
            class_member: ident,
            value: Self::parse_value_expression(iter)?,
        })
    }

    fn parse_value_expression(iter: &mut TokenIter) -> Result<ValueExpression, ASTError> {
        match iter.peek()?.token_type {
            Lexeme::OpenCurly => Ok(ValueExpression::Scope(Parser::parse_scope(iter)?.into())),
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
                    arr.values.push(Parser::parse_value_expression(iter)?);
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
                    if iter.peek()?.token_type == Lexeme::Comma {
                        iter.next()?;
                    } else {
                        break;
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
                Parser::parse_expression(iter)?.into(),
            )),
        }
    }

    fn parse_variable(iter: &mut TokenIter) -> Result<Variable, ASTError> {
        let token = iter.next()?.clone();
        match token.token_type {
            Lexeme::Identifier(name) => match iter.peek()?.token_type {
                Lexeme::Dot => {
                    iter.next()?;
                    Ok(Variable::MemberAccess(
                        Variable::Identifier(name).into(),
                        Parser::parse_variable(iter)?.into(),
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

    fn parse_function_arguments(
        variable: Variable,
        iter: &mut TokenIter,
    ) -> Result<FunctionCall, ASTError> {
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

    fn parse_primary(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        match iter.peek()?.token_type.clone() {
            Lexeme::OpenParen => {
                iter.next()?;
                let expr = Parser::parse_expression(iter)?;
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
                let variable = Parser::parse_variable(iter)?;
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

    fn parse_unary(iter: &mut TokenIter) -> Result<Expression, ASTError> {
        match iter.peek()?.token_type {
            Lexeme::Exclamation => {
                iter.next()?;
                Ok(Expression::Unary(
                    UnaryOp::Not,
                    Parser::parse_unary(iter)?.into(),
                ))
            }
            Lexeme::Minus => {
                iter.next()?;
                Ok(Expression::Unary(
                    UnaryOp::Negate,
                    Parser::parse_unary(iter)?.into(),
                ))
            }
            _ => Parser::parse_primary(iter),
        }
    }

    fn parse_factor(iter: &mut TokenIter) -> Result<Expression, ASTError> {
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

    fn parse_term(iter: &mut TokenIter) -> Result<Expression, ASTError> {
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

    fn parse_comparison(iter: &mut TokenIter) -> Result<Expression, ASTError> {
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

    fn parse_expression(iter: &mut TokenIter) -> Result<Expression, ASTError> {
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

    fn parse_control_flow_conditional(
        iter: &mut TokenIter,
    ) -> Result<ControlFlowConditionImpl, ASTError> {
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

    fn parse_if(iter: &mut TokenIter) -> Result<ControlFlowIf, ASTError> {
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

    fn parse_while(iter: &mut TokenIter) -> Result<ControlFlowWhile, ASTError> {
        iter.requires(Lexeme::OpenParen)?;
        let condition = Self::parse_control_flow_conditional(iter)?;
        iter.requires(Lexeme::CloseParen)?;
        let scope = Self::parse_scope(iter)?;
        Ok(ControlFlowWhile {
            condition,
            scope: scope.into(),
        })
    }

    fn parse_for_expression(iter: &mut TokenIter) -> Result<Option<ForInternalImpl>, ASTError> {
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

    fn parse_for(iter: &mut TokenIter) -> Result<ControlFlowFor, ASTError> {
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

    fn parse_statement(iter: &mut TokenIter) -> Result<Statement, ASTError> {
        let token = iter.peek()?.clone();
        match token.token_type.clone() {
            Lexeme::Keyword(keyword) => match keyword {
                Keyword::Var | Keyword::Const => Ok(Statement::VariableDecl(
                    Parser::parse_variable_decl(iter)?.into(),
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
                    let result = Ok(Statement::Return(Parser::parse_value_expression(iter)?));
                    iter.requires(Lexeme::Semicolon)?;
                    result
                }
                Keyword::Print => {
                    iter.next()?;
                    let result = Ok(Statement::Print(Parser::parse_value_expression(iter)?));
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
                let variable = Parser::parse_variable(iter)?;

                match iter.peek()?.token_type {
                    Lexeme::Assignment => {
                        iter.next()?;
                        let result = Ok(Statement::Assignment(
                            variable,
                            Parser::parse_value_expression(iter)?,
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

    fn try_parse_function(iter: &mut TokenIter) -> Result<Option<Function>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Fun)) {
            println!("Trying to parse function");
            let identifier = iter.identifier()?;
            iter.requires(Lexeme::OpenParen)?;
            let inputs = iter.collect_identifier()?;
            iter.requires(Lexeme::CloseParen)?;
            let scope = Parser::parse_scope(iter)?.into();
            return Ok(Some(Function {
                identifier,
                inputs,
                scope,
            }));
        }
        Ok(None)
    }

    fn try_parse_class(iter: &mut TokenIter) -> Result<Option<ProgramImpl>, ASTError> {
        if iter.consume_if(Lexeme::Keyword(Keyword::Class)) {
            println!("Trying to parse class");
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

    fn parse_program(iter: &mut TokenIter) -> Result<Program, ASTError> {
        let mut nodes = Program::default();

        println!("Parsing program!");

        while iter.has_next() {
            println!("{:?}", iter.peek()?);
            if let Some(v) = Parser::try_parse_function(iter)? {
                nodes.children.push(ProgramImpl::Function(v.into()))
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
    println!(
        "{:?}",
        &mut parsed_tree.unwrap().nodes
    );
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
