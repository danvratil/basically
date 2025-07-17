// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

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
    Print(Expression),
    Assignment {
        variable: String,
        expression: Expression,
    },
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
                let variable = elements
                    .pop_front()
                    .ok_or(AstError::InvalidStatement(
                        "Empty assignment variable".to_string(),
                    ))?
                    .as_str()
                    .to_string();
                elements.pop_front(); // pop the assignment operator
                let expression = Expression::try_from(elements.pop_front().ok_or(
                    AstError::InvalidStatement("Empty assignment expression".to_string()),
                )?)?;
                Ok(Statement::Assignment {
                    variable,
                    expression,
                })
            }
            _ => Err(AstError::InvalidStatement(format!(
                "Expected print statement, got {:?}",
                statement.as_rule()
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i16), // QBasic integer types were 16-bit signed integers
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Variable(String),
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
            Rule::variable => Ok(Expression::Variable(expr.as_str().to_string())),
            Rule::factor => {
                let mut inner = expr.clone().into_inner();
                Ok(Expression::try_from(inner.next().unwrap())?)
            }
            Rule::number => Ok(Expression::Integer(expr.as_str().parse::<i16>().map_err(
                |e| AstError::InvalidExpression(format!("Invalid integer: {e}")),
            )?)),
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
