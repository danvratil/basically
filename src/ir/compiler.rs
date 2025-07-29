// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ast::{self, AssignmentTarget};
use crate::ir;

use itertools::chain;
use std::collections::HashMap;
use std::iter;

#[derive(Debug)]
enum LoopContext {
    For { end_label: String },
    Do { end_label: String },
}

struct CompilerContext {
    instructions: Vec<ir::Instruction>,
    label_counter: usize,
    jump_patches: HashMap<String, Vec<usize>>, // label -> list of instruction indices to patch
    label_positions: HashMap<String, usize>,   // label -> instruction position
    loop_stack: Vec<LoopContext>,             // track current loop context for EXIT statements
}

impl CompilerContext {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            label_counter: 0,
            jump_patches: HashMap::new(),
            label_positions: HashMap::new(),
            loop_stack: Vec::new(),
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

    fn emit_jump_if_true(&mut self, label: &str) {
        let jump_index = self.instructions.len();
        self.jump_patches
            .entry(label.to_string())
            .or_default()
            .push(jump_index);
        self.instructions.push(ir::Instruction::JumpIfTrue(0)); // placeholder, will be patched
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

    fn push_loop_context(&mut self, context: LoopContext) {
        self.loop_stack.push(context);
    }

    fn pop_loop_context(&mut self) {
        self.loop_stack.pop();
    }

    fn find_loop_end_label(&self, loop_type: &str) -> Option<&String> {
        // Find the topmost loop of the specified type
        for context in self.loop_stack.iter().rev() {
            match (context, loop_type) {
                (LoopContext::For { end_label }, "FOR") => return Some(end_label),
                (LoopContext::Do { end_label }, "DO") => return Some(end_label),
                _ => continue,
            }
        }
        None
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
        ast::Statement::For {
            counter,
            start,
            end,
            step,
            statements,
        } => {
            // Generate unique labels for this FOR loop
            let for_check_label = ctx.generate_label();
            let for_body_label = ctx.generate_label();
            let for_end_label = ctx.generate_label();
            let positive_step_label = ctx.generate_label();

            // Generate unique temporary variable names for this loop
            let loop_id = ctx.label_counter;
            let end_var_name = format!("__for_end_{loop_id}");
            let step_var_name = format!("__for_step_{loop_id}");

            // Create temporary variables for end and step values
            let end_var = ast::Variable {
                name: end_var_name,
                r#type: counter.r#type.clone(),
            };
            let step_var = ast::Variable {
                name: step_var_name,
                r#type: counter.r#type.clone(),
            };

            // Initialize counter with start value
            for instruction in compile_expression(start.clone()) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::StoreVar(counter.clone()));

            // Store end value in temporary variable
            for instruction in compile_expression(end.clone()) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::StoreVar(end_var.clone()));

            // Store step value in temporary variable (default to 1 if None)
            let step_expr = step.clone().unwrap_or(ast::Expression::Integer(1));
            for instruction in compile_expression(step_expr) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::StoreVar(step_var.clone()));

            // Jump to condition check
            ctx.emit_jump(&for_check_label);

            // Push FOR loop context for EXIT FOR statements
            ctx.push_loop_context(LoopContext::For {
                end_label: for_end_label.clone(),
            });

            // FOR loop body
            ctx.place_label(&for_body_label);
            for statement in statements {
                compile_statement_with_context(ctx, statement.clone());
            }

            // Pop FOR loop context
            ctx.pop_loop_context();

            // Increment counter by step
            ctx.emit_instruction(ir::Instruction::LoadVar(counter.clone()));
            ctx.emit_instruction(ir::Instruction::LoadVar(step_var.clone()));
            ctx.emit_instruction(ir::Instruction::Add);
            ctx.emit_instruction(ir::Instruction::StoreVar(counter.clone()));

            // FOR loop condition check
            ctx.place_label(&for_check_label);

            // Determine comparison based on step sign
            ctx.emit_instruction(ir::Instruction::LoadVar(step_var.clone()));
            ctx.emit_instruction(ir::Instruction::LoadConst(ir::Value::Integer(0)));
            ctx.emit_instruction(ir::Instruction::GreaterThanEqual);
            ctx.emit_jump_if_true(&positive_step_label);

            // Negative step: continue if counter >= end
            ctx.emit_instruction(ir::Instruction::LoadVar(counter.clone()));
            ctx.emit_instruction(ir::Instruction::LoadVar(end_var.clone()));
            ctx.emit_instruction(ir::Instruction::GreaterThanEqual);
            ctx.emit_jump_if_true(&for_body_label);
            ctx.emit_jump(&for_end_label);

            // Positive step: continue if counter <= end
            ctx.place_label(&positive_step_label);
            ctx.emit_instruction(ir::Instruction::LoadVar(counter.clone()));
            ctx.emit_instruction(ir::Instruction::LoadVar(end_var.clone()));
            ctx.emit_instruction(ir::Instruction::LessThanEqual);
            ctx.emit_jump_if_true(&for_body_label);

            // End of FOR loop
            ctx.place_label(&for_end_label);
        }
        ast::Statement::DoLoop {
            condition_type,
            condition,
            statements,
        } => {
            match condition_type {
                ast::DoConditionType::PreTestWhile => {
                    // DO WHILE condition
                    let condition_label = ctx.generate_label();
                    let body_label = ctx.generate_label();
                    let end_label = ctx.generate_label();

                    // Condition check
                    ctx.place_label(&condition_label);
                    for instruction in compile_expression(condition.expect("PreTestWhile must have condition")) {
                        ctx.emit_instruction(instruction);
                    }
                    ctx.emit_jump_if_false(&end_label);

                    // Push DO loop context for EXIT DO statements
                    ctx.push_loop_context(LoopContext::Do {
                        end_label: end_label.clone(),
                    });

                    // Loop body
                    ctx.place_label(&body_label);
                    for statement in statements {
                        compile_statement_with_context(ctx, statement);
                    }
                    ctx.emit_jump(&condition_label);

                    // Pop DO loop context
                    ctx.pop_loop_context();

                    // End of loop
                    ctx.place_label(&end_label);
                }
                ast::DoConditionType::PreTestUntil => {
                    // DO UNTIL condition
                    let condition_label = ctx.generate_label();
                    let body_label = ctx.generate_label();
                    let end_label = ctx.generate_label();

                    // Condition check
                    ctx.place_label(&condition_label);
                    for instruction in compile_expression(condition.expect("PreTestUntil must have condition")) {
                        ctx.emit_instruction(instruction);
                    }
                    ctx.emit_jump_if_true(&end_label);

                    // Push DO loop context for EXIT DO statements
                    ctx.push_loop_context(LoopContext::Do {
                        end_label: end_label.clone(),
                    });

                    // Loop body
                    ctx.place_label(&body_label);
                    for statement in statements {
                        compile_statement_with_context(ctx, statement);
                    }
                    ctx.emit_jump(&condition_label);

                    // Pop DO loop context
                    ctx.pop_loop_context();

                    // End of loop
                    ctx.place_label(&end_label);
                }
                ast::DoConditionType::PostTestWhile => {
                    // DO...LOOP WHILE condition
                    let body_label = ctx.generate_label();
                    let condition_label = ctx.generate_label();
                    let end_label = ctx.generate_label();

                    // Push DO loop context for EXIT DO statements
                    ctx.push_loop_context(LoopContext::Do {
                        end_label: end_label.clone(),
                    });

                    // Loop body
                    ctx.place_label(&body_label);
                    for statement in statements {
                        compile_statement_with_context(ctx, statement);
                    }

                    // Pop DO loop context
                    ctx.pop_loop_context();

                    // Condition check
                    ctx.place_label(&condition_label);
                    for instruction in compile_expression(condition.expect("PostTestWhile must have condition")) {
                        ctx.emit_instruction(instruction);
                    }
                    ctx.emit_jump_if_true(&body_label);
                    ctx.place_label(&end_label);
                }
                ast::DoConditionType::PostTestUntil => {
                    // DO...LOOP UNTIL condition
                    let body_label = ctx.generate_label();
                    let condition_label = ctx.generate_label();
                    let end_label = ctx.generate_label();

                    // Push DO loop context for EXIT DO statements
                    ctx.push_loop_context(LoopContext::Do {
                        end_label: end_label.clone(),
                    });

                    // Loop body
                    ctx.place_label(&body_label);
                    for statement in statements {
                        compile_statement_with_context(ctx, statement);
                    }

                    // Pop DO loop context
                    ctx.pop_loop_context();

                    // Condition check
                    ctx.place_label(&condition_label);
                    for instruction in compile_expression(condition.expect("PostTestUntil must have condition")) {
                        ctx.emit_instruction(instruction);
                    }
                    ctx.emit_jump_if_false(&body_label);

                    // End of loop
                    ctx.place_label(&end_label);
                }
                ast::DoConditionType::None => {
                    // DO...LOOP (infinite loop)
                    let body_label = ctx.generate_label();
                    let end_label = ctx.generate_label();

                    // Push DO loop context for EXIT DO statements
                    ctx.push_loop_context(LoopContext::Do {
                        end_label: end_label.clone(),
                    });

                    // Loop body
                    ctx.place_label(&body_label);
                    for statement in statements {
                        compile_statement_with_context(ctx, statement);
                    }
                    ctx.emit_jump(&body_label);

                    ctx.pop_loop_context();

                    // End of loop
                    ctx.place_label(&end_label);
                }
            }
        }
        ast::Statement::ExitDo => {
            // Find the topmost DO loop and jump to its end
            if let Some(end_label) = ctx.find_loop_end_label("DO") {
                let end_label = end_label.clone();
                ctx.emit_jump(&end_label);
            } else {
                // This should be a compile-time error, but for now we'll just ignore it
                // TODO: Add proper error handling for EXIT DO outside of DO loop
            }
        }
        ast::Statement::ExitFor => {
            // Find the topmost FOR loop and jump to its end
            if let Some(end_label) = ctx.find_loop_end_label("FOR") {
                let end_label = end_label.clone();
                ctx.emit_jump(&end_label);
            } else {
                // This should be a compile-time error, but for now we'll just ignore it
                // TODO: Add proper error handling for EXIT FOR outside of FOR loop
            }
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
