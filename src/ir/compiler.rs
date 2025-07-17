// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ast::{self, AssignmentTarget};
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
            target,
            expression,
        } => {
            match target {
                AssignmentTarget::Variable(variable) => {
                    chain!(
                        compile_expression(expression),
                        iter::once(ir::Instruction::StoreVar(variable))
                    ).collect()
                }
                AssignmentTarget::ArrayElement(array_access) => {
                    chain!(
                        // First compile index expressions (pushed first, will be popped last)
                        array_access.indices.iter()
                            .map(|expr| compile_expression(expr.clone()))
                            .flatten(),
                        // Then compile the value expression (pushed last, will be popped first)
                        compile_expression(expression),
                        // Store the array element using the value and indices from stack
                        iter::once(ir::Instruction::StoreArrayElement {
                            name: array_access.name,
                            num_indices: array_access.indices.len(),
                        })
                    ).collect()
                }
            }
        }
        ast::Statement::Input { prompt, variables } => chain!(
            prompt
                .map(|p| compile_statement(ast::Statement::Print(ast::Expression::String(p))))
                .into_iter()
                .flatten(),
            iter::once(ir::Instruction::Input(variables))
        )
        .collect(),
        ast::Statement::Metacommand(metacommand) => match metacommand {
            ast::Metacommand::Static => vec![ir::Instruction::SetStatic],
            ast::Metacommand::Dynamic => vec![ir::Instruction::SetDynamic],
        },
        ast::Statement::Dim(array_decl) => {
            // For each dimension, we need to determine the bounds
            let mut dimensions = Vec::new();
            for dim in &array_decl.dimensions {
                let lower_bound = match &dim.lower {
                    Some(ast::Expression::Integer(val)) => *val as isize,
                    None => -1, // Special sentinel value for default bounds (will be adjusted based on STATIC/DYNAMIC)
                    _ => return vec![], // For now, only support constant bounds
                };
                
                let upper_bound = match &dim.upper {
                    ast::Expression::Integer(val) => *val as isize,
                    _ => return vec![], // For now, only support constant bounds
                };
                
                dimensions.push((lower_bound, upper_bound));
            }
            
            vec![ir::Instruction::DeclareArray {
                name: array_decl.name.clone(),
                element_type: array_decl.element_type.clone(),
                dimensions,
            }]
        }
        ast::Statement::Noop => vec![],
    }
}

fn compile_expression(expr: ast::Expression) -> Vec<ir::Instruction> {
    match expr {
        ast::Expression::Integer(value) => {
            vec![ir::Instruction::LoadConst(
                TryInto::<i16>::try_into(value)
                    .map(ir::Value::Integer)
                    .unwrap_or(ir::Value::Long(value)),
            )]
        }
        ast::Expression::Float(value) => {
            vec![ir::Instruction::LoadConst(
                // TODO: Does this actually work?
                if value > f32::MAX as f64 || value < f32::MIN as f64 {
                    ir::Value::DoublePrecision(value)
                } else {
                    ir::Value::SinglePrecision(value as f32)
                },
            )]
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
        ast::Expression::ArrayAccess(array_access) => {
            let num_indices = array_access.indices.len();
            chain!(
                // Compile index expressions and push them to stack
                array_access
                    .indices
                    .into_iter()
                    .map(compile_expression)
                    .flatten(),
                // Load the array element using the indices from stack
                iter::once(ir::Instruction::LoadArrayElement {
                    name: array_access.name,
                    num_indices,
                })
            )
            .collect()
        }
    }
}
