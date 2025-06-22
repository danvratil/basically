// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

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
}

impl TryFrom<Pair<'_, Rule>> for Expression {
    type Error = AstError;

    fn try_from(expr: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match expr.as_rule() {
            Rule::expression => {
                Ok(Expression::try_from(expr.into_inner().next().ok_or(
                    AstError::InvalidExpression("Empty expression".to_string()),
                )?)?)
            }
            Rule::integer => Ok(Expression::Integer(expr.as_str().parse::<i16>().map_err(
                |e| AstError::InvalidExpression(format!("Invalid integer: {e}")),
            )?)),
            _ => Err(AstError::InvalidExpression(format!(
                "Expected integer, got {:?}",
                expr.as_rule()
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::BasicParser;
    use pest::Parser;

    use super::*;

    #[test]
    fn test_parse_print_42_statement() {
        let input = "PRINT 42";

        let mut pairs = BasicParser::parse(Rule::program, input).expect("Failed to parse input");
        let program =
            Program::try_from(pairs.next().expect("Empty program")).expect("Failed to get program");
        println!("{:?}", program);
    }
}
