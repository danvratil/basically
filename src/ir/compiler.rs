// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ast::{self, AssignmentTarget};
use crate::ir;

use itertools::chain;
use std::collections::HashMap;
use std::iter;

struct CompilerContext {
    instructions: Vec<ir::Instruction>,
    label_counter: usize,
    jump_patches: HashMap<String, Vec<usize>>, // label -> list of instruction indices to patch
    label_positions: HashMap<String, usize>,   // label -> instruction position
}

impl CompilerContext {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            label_counter: 0,
            jump_patches: HashMap::new(),
            label_positions: HashMap::new(),
        }
    }

    fn generate_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn emit_instruction(&mut self, instruction: ir::Instruction) {
        self.instructions.push(instruction);
    }

    fn emit_jump(&mut self, label: &str) {
        let jump_index = self.instructions.len();
        self.jump_patches
            .entry(label.to_string())
            .or_default()
            .push(jump_index);
        self.instructions.push(ir::Instruction::Jump(0)); // placeholder, will be patched
    }

    fn emit_jump_if_false(&mut self, label: &str) {
        let jump_index = self.instructions.len();
        self.jump_patches
            .entry(label.to_string())
            .or_default()
            .push(jump_index);
        self.instructions.push(ir::Instruction::JumpIfFalse(0)); // placeholder, will be patched
    }

    fn place_label(&mut self, label: &str) {
        let position = self.instructions.len();
        self.label_positions.insert(label.to_string(), position);
    }

    fn resolve_jumps(&mut self) {
        for (label, positions) in &self.label_positions {
            if let Some(jump_indices) = self.jump_patches.get(label) {
                for &jump_index in jump_indices {
                    match &mut self.instructions[jump_index] {
                        ir::Instruction::Jump(target) => *target = *positions,
                        ir::Instruction::JumpIfFalse(target) => *target = *positions,
                        ir::Instruction::JumpIfTrue(target) => *target = *positions,
                        _ => panic!("Expected jump instruction at index {jump_index}"),
                    }
                }
            }
        }
    }
}

pub fn compile(program: ast::Program) -> ir::Program {
    let mut ctx = CompilerContext::new();

    for statement in program.statements {
        compile_statement_with_context(&mut ctx, statement);
    }

    ctx.emit_instruction(ir::Instruction::Halt);
    ctx.resolve_jumps();

    ir::Program {
        instructions: ctx.instructions,
    }
}

fn compile_statement_with_context(ctx: &mut CompilerContext, statement: ast::Statement) {
    match statement {
        ast::Statement::Print(expr) => {
            for instruction in compile_expression(expr) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::Print);
        }
        ast::Statement::Assignment { target, expression } => {
            match target {
                AssignmentTarget::Variable(variable) => {
                    for instruction in compile_expression(expression) {
                        ctx.emit_instruction(instruction);
                    }
                    ctx.emit_instruction(ir::Instruction::StoreVar(variable));
                }
                AssignmentTarget::ArrayElement(array_access) => {
                    // First compile index expressions
                    for index_expr in &array_access.indices {
                        for instruction in compile_expression(index_expr.clone()) {
                            ctx.emit_instruction(instruction);
                        }
                    }
                    // Then compile the value expression
                    for instruction in compile_expression(expression) {
                        ctx.emit_instruction(instruction);
                    }
                    // Store the array element
                    ctx.emit_instruction(ir::Instruction::StoreArrayElement {
                        name: array_access.name,
                        num_indices: array_access.indices.len(),
                    });
                }
            }
        }
        ast::Statement::Input { prompt, variables } => {
            if let Some(p) = prompt {
                compile_statement_with_context(
                    ctx,
                    ast::Statement::Print(ast::Expression::String(p)),
                );
            }
            ctx.emit_instruction(ir::Instruction::Input(variables));
        }
        ast::Statement::Metacommand(metacommand) => match metacommand {
            ast::Metacommand::Static => ctx.emit_instruction(ir::Instruction::SetStatic),
            ast::Metacommand::Dynamic => ctx.emit_instruction(ir::Instruction::SetDynamic),
        },
        ast::Statement::Dim(array_decl) => {
            // For each dimension, we need to determine the bounds
            let mut dimensions = Vec::new();
            for dim in &array_decl.dimensions {
                let lower_bound = match &dim.lower {
                    Some(ast::Expression::Integer(val)) => *val as isize,
                    None => -1, // Special sentinel value for default bounds (will be adjusted based on STATIC/DYNAMIC)
                    _ => return, // For now, only support constant bounds
                };

                let upper_bound = match &dim.upper {
                    ast::Expression::Integer(val) => *val as isize,
                    _ => return, // For now, only support constant bounds
                };

                dimensions.push((lower_bound, upper_bound));
            }

            ctx.emit_instruction(ir::Instruction::DeclareArray {
                name: array_decl.name.clone(),
                element_type: array_decl.element_type.clone(),
                dimensions,
            });
        }
        ast::Statement::If { branches } => {
            // Generate labels for the IF statement structure
            let mut branch_labels = Vec::new();
            let end_label = ctx.generate_label();

            // Generate labels for each branch
            for _ in 0..branches.len() {
                branch_labels.push(ctx.generate_label());
            }

            // Compile each branch
            for (i, branch) in branches.iter().enumerate() {
                if let Some(condition) = &branch.condition {
                    // Compile condition
                    for instruction in compile_expression(condition.clone()) {
                        ctx.emit_instruction(instruction);
                    }

                    // Jump to next branch if condition is false
                    let next_label = if i + 1 < branch_labels.len() {
                        &branch_labels[i + 1]
                    } else {
                        &end_label
                    };
                    ctx.emit_jump_if_false(next_label);
                }

                // Compile statements in this branch
                for statement in &branch.statements {
                    compile_statement_with_context(ctx, statement.clone());
                }

                // Jump to end (skip other branches)
                if i < branches.len() - 1 {
                    ctx.emit_jump(&end_label);
                }

                // Place label for next branch
                if i + 1 < branch_labels.len() {
                    ctx.place_label(&branch_labels[i + 1]);
                }
            }

            // Place end label
            ctx.place_label(&end_label);
        }
        ast::Statement::Noop => {
            // Do nothing
        }
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
        ast::Expression::UnaryOp { op, operand } => chain!(
            compile_expression(*operand),
            iter::once(match op {
                ast::UnaryOperator::Plus => ir::Instruction::UnaryPlus,
                ast::UnaryOperator::Minus => ir::Instruction::UnaryMinus,
            })
        )
        .collect(),
        ast::Expression::LogicalOp { left, op, right } => {
            match left {
                Some(left_expr) => {
                    // Binary logical operation (AND, OR)
                    chain!(
                        compile_expression(*left_expr),
                        compile_expression(*right),
                        iter::once(match op {
                            ast::LogicalOperator::And => ir::Instruction::And,
                            ast::LogicalOperator::Or => ir::Instruction::Or,
                            ast::LogicalOperator::Not => panic!("NOT should not have left operand"),
                        })
                    )
                    .collect()
                }
                None => {
                    // Unary logical operation (NOT)
                    chain!(compile_expression(*right), iter::once(ir::Instruction::Not)).collect()
                }
            }
        }
        ast::Expression::RelationalOp { left, op, right } => chain!(
            compile_expression(*left),
            compile_expression(*right),
            iter::once(match op {
                ast::RelationalOperator::Equal => ir::Instruction::Equal,
                ast::RelationalOperator::NotEqual => ir::Instruction::NotEqual,
                ast::RelationalOperator::LessThan => ir::Instruction::LessThan,
                ast::RelationalOperator::LessThanEqual => ir::Instruction::LessThanEqual,
                ast::RelationalOperator::GreaterThan => ir::Instruction::GreaterThan,
                ast::RelationalOperator::GreaterThanEqual => ir::Instruction::GreaterThanEqual,
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
                    .flat_map(compile_expression),
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
