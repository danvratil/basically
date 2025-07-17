// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

use enum_map::Enum;
use pest::iterators::Pair;
use thiserror::Error;

use crate::parser::Rule;

#[derive(Error, Debug)]
pub enum AstError {
    #[error("Empty program")]
    EmptyProgram,

    #[error("Invalid statement: {0}")]
    InvalidStatement(String),

    #[error("Invalid expression: {0}")]
    InvalidExpression(String),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl TryFrom<Pair<'_, Rule>> for Program {
    type Error = AstError;

    fn try_from(program: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match program.as_rule() {
            Rule::program => Ok(Program {
                statements: program
                    .into_inner()
                    .take_while(|p| p.as_rule() != Rule::EOI)
                    .map(Statement::try_from)
                    .collect::<Result<Vec<_>, AstError>>()?,
            }),
            _ => Err(AstError::EmptyProgram),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Noop,
    Print(Expression),
    Assignment {
        variable: Variable,
        expression: Expression,
    },
    Input {
        prompt: Option<String>,
        variables: Vec<Variable>,
    },
    Metacommand(Metacommand),
}

impl TryFrom<Pair<'_, Rule>> for Statement {
    type Error = AstError;

    fn try_from(statement: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match statement.as_rule() {
            Rule::statement => {
                Ok(Statement::try_from(statement.into_inner().next().ok_or(
                    AstError::InvalidStatement("Empty statement".to_string()),
                )?)?)
            }
            Rule::print_statement => Ok(Statement::Print(Expression::try_from(
                statement
                    .into_inner()
                    .next()
                    .ok_or(AstError::InvalidStatement(
                        "Empty print statement".to_string(),
                    ))?,
            )?)),
            Rule::assignment_statement => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                if elements.len() != 3 {
                    return Err(AstError::InvalidStatement(
                        "Invalid assignment statement".to_string(),
                    ));
                }
                let variable = Variable::try_from(
                    elements
                        .pop_front()
                        .ok_or(AstError::InvalidStatement(
                            "Empty assignment variable".to_string(),
                        ))?
                        .as_str(),
                )?;
                elements.pop_front(); // pop the assignment operator
                let expression = Expression::try_from(elements.pop_front().ok_or(
                    AstError::InvalidStatement("Empty assignment expression".to_string()),
                )?)?;
                Ok(Statement::Assignment {
                    variable,
                    expression,
                })
            }
            Rule::input_statement => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                if elements.is_empty() {
                    return Err(AstError::InvalidStatement(
                        "Invalid input statement: at least one argument is required".to_string(),
                    ));
                }

                let mut prompt = None;
                let mut variables = Vec::new();

                let first_expr = Expression::try_from(elements.pop_front().ok_or(
                    AstError::InvalidStatement("Empty input expression".to_string()),
                )?)?;
                match first_expr {
                    Expression::String(s) => prompt = Some(s),
                    Expression::Variable(v) => variables.push(v),
                    _ => {
                        return Err(AstError::InvalidStatement(format!(
                            "Invalid input expression: {:?}",
                            first_expr
                        )));
                    }
                }

                while !elements.is_empty() {
                    let expr = Expression::try_from(elements.pop_front().ok_or(
                        AstError::InvalidStatement("Empty input expression".to_string()),
                    )?)?;
                    if let Expression::Variable(v) = expr {
                        variables.push(v);
                    } else {
                        return Err(AstError::InvalidStatement(format!(
                            "Invalid input expression: {:?}",
                            expr
                        )));
                    }
                }
                Ok(Statement::Input { prompt, variables })
            }
            Rule::comment => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                // Pop the remark leader (REM or ')
                let _ = elements.pop_front().ok_or(AstError::InvalidStatement(
                    "Empty comment".to_string(),
                ))?;
                // Pop the metacommand if present, otherwise this is a noop
                match elements.pop_front().map(|p| p.as_str()) {
                    Some("$STATIC") => Ok(Statement::Metacommand(Metacommand::Static)),
                    Some("$DYNAMIC") => Ok(Statement::Metacommand(Metacommand::Dynamic)),
                    _ => Ok(Statement::Noop)
                }
            }
            _ => Err(AstError::InvalidStatement(format!(
                "Expected statement, got {:?}",
                statement.as_rule()
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Variable(Variable),
}

impl TryFrom<Pair<'_, Rule>> for Expression {
    type Error = AstError;

    fn try_from(expr: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match expr.as_rule() {
            Rule::expression => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression("Empty expression".to_string()));
                }

                // Start with the first term
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining operator-term pairs
                while elements.len() >= 2 {
                    let op = BinaryOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::BinaryOp {
                        left: Box::new(result),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::term => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression("Empty term".to_string()));
                }

                // Start with the first factor
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining operator-factor pairs
                while elements.len() >= 2 {
                    let op = BinaryOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::BinaryOp {
                        left: Box::new(result),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::variable => Ok(Expression::Variable(Variable::try_from(expr.as_str())?)),
            Rule::factor => {
                let mut inner = expr.clone().into_inner();
                Ok(Expression::try_from(inner.next().unwrap())?)
            }
            Rule::number => {
                let number = expr.as_str();
                if number.contains('.') {
                    Ok(Expression::Float(number.parse::<f64>().map_err(|e| {
                        AstError::InvalidExpression(format!("Invalid float: {e}"))
                    })?))
                } else {
                    Ok(Expression::Integer(number.parse::<i32>().map_err(|e| {
                        AstError::InvalidExpression(format!("Invalid integer: {e}"))
                    })?))
                }
            }
            Rule::string => {
                // Don't store the string with the quotes
                let string = expr.as_str();
                Ok(Expression::String(string[1..string.len() - 1].to_string()))
            }
            _ => Err(AstError::InvalidExpression(format!(
                "Expected integer, got {:?}",
                expr.as_rule()
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl TryFrom<Pair<'_, Rule>> for BinaryOperator {
    type Error = AstError;

    fn try_from(op: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match op.as_rule() {
            Rule::addition_operator => Ok(BinaryOperator::Add),
            Rule::subtraction_operator => Ok(BinaryOperator::Subtract),
            Rule::multiplication_operator => Ok(BinaryOperator::Multiply),
            Rule::division_operator => Ok(BinaryOperator::Divide),
            _ => Err(AstError::InvalidExpression(format!(
                "Invalid binary operator: {:?}",
                op
            ))),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Enum)]
pub enum VariableType {
    // Suffix: $
    String,
    // Suffix !
    SinglePrecision,
    // Suffix #
    DoublePrecision,
    // Suffix %
    Integer, // default
    // Suffix &
    Long,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub name: String,
    pub r#type: VariableType,
}

impl TryFrom<&str> for Variable {
    type Error = AstError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let r#type = match value.chars().last() {
            Some('$') => Some(VariableType::String),
            Some('!') => Some(VariableType::SinglePrecision),
            Some('#') => Some(VariableType::DoublePrecision),
            Some('%') => Some(VariableType::Integer),
            Some('&') => Some(VariableType::Long),
            Some(_) => None,
            None => {
                return Err(AstError::InvalidStatement(
                    "Variable name too short".to_string(),
                ));
            }
        };

        let name = if r#type.is_some() {
            value[..value.len() - 1].to_string()
        } else {
            value.to_string()
        };

        if name.is_empty() {
            return Err(AstError::InvalidStatement(
                "Variable name cannot be empty".to_string(),
            ));
        }

        if name.len() > 40 {
            return Err(AstError::InvalidStatement(
                "Variable name too long".to_string(),
            ));
        }

        // Other variable name constraints should be covered by the grammar.

        Ok(Variable {
            name,
            r#type: r#type.unwrap_or(VariableType::Integer),
        })
    }
}

#[derive(Debug, Clone)]
pub enum Metacommand {
    Dynamic,
    Static,
}
