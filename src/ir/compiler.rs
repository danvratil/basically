// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ast;
use crate::ir;

use itertools::chain;
use std::iter;

pub fn compile(program: ast::Program) -> ir::Program {
    ir::Program {
        instructions: program
            .statements
            .into_iter()
            .map(compile_statement) // compile each statement into a list of instructions
            .flatten() // flatten the list
            .chain(iter::once(ir::Instruction::Halt)) // append a halt instruction at the end
            .collect(),
    }
}

fn compile_statement(statement: ast::Statement) -> Vec<ir::Instruction> {
    match statement {
        ast::Statement::Print(expr) => {
            chain!(compile_expression(expr), iter::once(ir::Instruction::Print)).collect()
        }
        ast::Statement::Assignment {
            variable,
            expression,
        } => chain!(
            compile_expression(expression),
            iter::once(ir::Instruction::StoreVar(variable))
        )
        .collect(),
        ast::Statement::Input { prompt, variables } => chain!(
            prompt
                .map(|p| compile_statement(ast::Statement::Print(ast::Expression::String(p))))
                .into_iter()
                .flatten(),
            iter::once(ir::Instruction::Input(variables))
        )
        .collect(),
    }
}

fn compile_expression(expr: ast::Expression) -> Vec<ir::Instruction> {
    match expr {
        ast::Expression::Integer(value) => {
            vec![ir::Instruction::LoadConst(ir::Value::Number(value))]
        }
        ast::Expression::String(value) => {
            vec![ir::Instruction::LoadConst(ir::Value::String(value))]
        }
        ast::Expression::BinaryOp { left, op, right } => chain!(
            compile_expression(*left),
            compile_expression(*right),
            iter::once(match op {
                ast::BinaryOperator::Add => ir::Instruction::Add,
                ast::BinaryOperator::Subtract => ir::Instruction::Subtract,
                ast::BinaryOperator::Multiply => ir::Instruction::Multiply,
                ast::BinaryOperator::Divide => ir::Instruction::Divide,
            })
        )
        .collect(),
        ast::Expression::Variable(variable) => {
            vec![ir::Instruction::LoadVar(variable)]
        }
    }
}
