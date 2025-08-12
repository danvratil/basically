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
    loop_stack: Vec<LoopContext>,              // track current loop context for EXIT statements
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
        ast::Statement::NumberedStatement {
            line_number,
            statement,
        } => {
            // Place line number label first
            let line_label = format!("LINE_{line_number}");
            ctx.place_label(&line_label);

            // Then compile the inner statement
            compile_plain_statement_with_context(ctx, statement);
        }
        ast::Statement::PlainStatement(statement) => {
            compile_plain_statement_with_context(ctx, statement);
        }
    }
}

fn compile_plain_statement_with_context(ctx: &mut CompilerContext, statement: ast::PlainStatement) {
    match statement {
        ast::PlainStatement::Print(expr) => {
            for instruction in compile_expression(expr) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::Print);
        }
        ast::PlainStatement::Assignment { target, expression } => {
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
        ast::PlainStatement::Input { prompt, variables } => {
            if let Some(p) = prompt {
                compile_plain_statement_with_context(
                    ctx,
                    ast::PlainStatement::Print(ast::Expression::String(p)),
                );
            }
            ctx.emit_instruction(ir::Instruction::Input(variables));
        }
        ast::PlainStatement::Metacommand(metacommand) => match metacommand {
            ast::Metacommand::Static => ctx.emit_instruction(ir::Instruction::SetStatic),
            ast::Metacommand::Dynamic => ctx.emit_instruction(ir::Instruction::SetDynamic),
        },
        ast::PlainStatement::Dim(array_decl) => {
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
        ast::PlainStatement::If { branches } => {
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
        ast::PlainStatement::For {
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
        ast::PlainStatement::DoLoop {
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
                    for instruction in
                        compile_expression(condition.expect("PreTestWhile must have condition"))
                    {
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
                    for instruction in
                        compile_expression(condition.expect("PreTestUntil must have condition"))
                    {
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
                    for instruction in
                        compile_expression(condition.expect("PostTestWhile must have condition"))
                    {
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
                    for instruction in
                        compile_expression(condition.expect("PostTestUntil must have condition"))
                    {
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
        ast::PlainStatement::While {
            condition,
            statements,
        } => {
            // Generate labels for WHILE loop
            let while_check_label = ctx.generate_label();
            let while_end_label = ctx.generate_label();

            // WHILE loop condition check
            ctx.place_label(&while_check_label);
            for instruction in compile_expression(condition.clone()) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_jump_if_false(&while_end_label);

            // WHILE loop body
            for statement in statements {
                compile_statement_with_context(ctx, statement);
            }
            ctx.emit_jump(&while_check_label);

            // Note: No loop context tracking needed since WHILE loops don't support EXIT statements

            // End of WHILE loop
            ctx.place_label(&while_end_label);
        }
        ast::PlainStatement::ExitDo => {
            // Find the topmost DO loop and jump to its end
            if let Some(end_label) = ctx.find_loop_end_label("DO") {
                let end_label = end_label.clone();
                ctx.emit_jump(&end_label);
            } else {
                // This should be a compile-time error, but for now we'll just ignore it
                // TODO: Add proper error handling for EXIT DO outside of DO loop
            }
        }
        ast::PlainStatement::ExitFor => {
            // Find the topmost FOR loop and jump to its end
            if let Some(end_label) = ctx.find_loop_end_label("FOR") {
                let end_label = end_label.clone();
                ctx.emit_jump(&end_label);
            } else {
                // This should be a compile-time error, but for now we'll just ignore it
                // TODO: Add proper error handling for EXIT FOR outside of FOR loop
            }
        }
        ast::PlainStatement::Noop => {
            // Do nothing
        }
        ast::PlainStatement::Goto(target) => {
            let label = match target {
                ast::GotoTarget::LineNumber(num) => format!("LINE_{num}"),
                ast::GotoTarget::Label(name) => name.clone(),
            };
            ctx.emit_jump(&label);
        }
        ast::PlainStatement::Label(name) => {
            ctx.place_label(&name);
        }
        ast::PlainStatement::SelectCase {
            test_expression,
            case_branches,
            else_statements,
        } => {
            // Generate unique labels
            let end_label = ctx.generate_label();
            let mut case_labels = Vec::new();

            // Generate labels for each case branch
            for _ in case_branches.iter() {
                case_labels.push(ctx.generate_label());
            }

            // Generate label for else branch if present
            let else_label = if else_statements.is_some() {
                Some(ctx.generate_label())
            } else {
                None
            };

            // Compile and store test expression in a temporary variable
            // We'll determine the type based on the first case constant
            let temp_var_name = format!("__select_case_test_{}", ctx.label_counter);
            let temp_var_type = if let Some(first_branch) = case_branches.first() {
                if let Some(first_expr) = first_branch.expressions.first() {
                    match first_expr {
                        ast::CaseExpression::Exact(ast::CaseConstant::String(_)) => {
                            ast::VariableType::String
                        }
                        ast::CaseExpression::Exact(ast::CaseConstant::Float(_)) => {
                            ast::VariableType::DoublePrecision
                        }
                        ast::CaseExpression::Exact(ast::CaseConstant::Integer(_)) => {
                            ast::VariableType::Integer
                        }
                        ast::CaseExpression::Range { from, .. } => match from {
                            ast::CaseConstant::String(_) => ast::VariableType::String,
                            ast::CaseConstant::Float(_) => ast::VariableType::DoublePrecision,
                            ast::CaseConstant::Integer(_) => ast::VariableType::Integer,
                        },
                        ast::CaseExpression::Relational { value, .. } => match value {
                            ast::CaseConstant::String(_) => ast::VariableType::String,
                            ast::CaseConstant::Float(_) => ast::VariableType::DoublePrecision,
                            ast::CaseConstant::Integer(_) => ast::VariableType::Integer,
                        },
                    }
                } else {
                    ast::VariableType::Integer // Default
                }
            } else {
                ast::VariableType::Integer // Default
            };

            let temp_var = ast::Variable {
                name: temp_var_name,
                r#type: temp_var_type,
            };

            // Evaluate test expression once and store it
            for instruction in compile_expression(test_expression.clone()) {
                ctx.emit_instruction(instruction);
            }
            ctx.emit_instruction(ir::Instruction::StoreVar(temp_var.clone()));

            // Generate comparison code for each case branch
            for (branch_idx, case_branch) in case_branches.iter().enumerate() {
                // Generate comparison logic for all expressions in this case
                for case_expr in case_branch.expressions.iter() {
                    // Load test variable
                    ctx.emit_instruction(ir::Instruction::LoadVar(temp_var.clone()));

                    match case_expr {
                        ast::CaseExpression::Exact(constant) => {
                            // test_var == constant
                            ctx.emit_instruction(ir::Instruction::LoadConst(
                                case_constant_to_ir_value(constant),
                            ));
                            ctx.emit_instruction(ir::Instruction::Equal);
                        }
                        ast::CaseExpression::Range { from, to } => {
                            // test_var >= from AND test_var <= to
                            // First check: test_var >= from
                            ctx.emit_instruction(ir::Instruction::LoadConst(
                                case_constant_to_ir_value(from),
                            ));
                            ctx.emit_instruction(ir::Instruction::GreaterThanEqual);

                            // Second check: test_var <= to
                            ctx.emit_instruction(ir::Instruction::LoadVar(temp_var.clone()));
                            ctx.emit_instruction(ir::Instruction::LoadConst(
                                case_constant_to_ir_value(to),
                            ));
                            ctx.emit_instruction(ir::Instruction::LessThanEqual);

                            // AND them together
                            ctx.emit_instruction(ir::Instruction::And);
                        }
                        ast::CaseExpression::Relational { op, value } => {
                            // test_var <op> value
                            ctx.emit_instruction(ir::Instruction::LoadConst(
                                case_constant_to_ir_value(value),
                            ));
                            match op {
                                ast::RelationalOperator::Equal => {
                                    ctx.emit_instruction(ir::Instruction::Equal)
                                }
                                ast::RelationalOperator::NotEqual => {
                                    ctx.emit_instruction(ir::Instruction::NotEqual)
                                }
                                ast::RelationalOperator::LessThan => {
                                    ctx.emit_instruction(ir::Instruction::LessThan)
                                }
                                ast::RelationalOperator::LessThanEqual => {
                                    ctx.emit_instruction(ir::Instruction::LessThanEqual)
                                }
                                ast::RelationalOperator::GreaterThan => {
                                    ctx.emit_instruction(ir::Instruction::GreaterThan)
                                }
                                ast::RelationalOperator::GreaterThanEqual => {
                                    ctx.emit_instruction(ir::Instruction::GreaterThanEqual)
                                }
                            }
                        }
                    }

                    // If this is true, jump to the case body
                    ctx.emit_jump_if_true(&case_labels[branch_idx]);
                }
            }

            // If no case matched, jump to else or end
            if let Some(else_label_ref) = &else_label {
                ctx.emit_jump(else_label_ref);
            } else {
                ctx.emit_jump(&end_label);
            }

            // Compile each case branch body
            for (branch_idx, case_branch) in case_branches.iter().enumerate() {
                ctx.place_label(&case_labels[branch_idx]);

                // Compile statements in this case
                for statement in &case_branch.statements {
                    compile_statement_with_context(ctx, statement.clone());
                }

                // Jump to end (prevent fall-through)
                ctx.emit_jump(&end_label);
            }

            // Compile else branch if present
            if let (Some(else_label_ref), Some(else_stmts)) = (else_label, else_statements) {
                ctx.place_label(&else_label_ref);
                for statement in else_stmts {
                    compile_statement_with_context(ctx, statement.clone());
                }
            }

            // Place end label
            ctx.place_label(&end_label);
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

fn case_constant_to_ir_value(constant: &ast::CaseConstant) -> ir::Value {
    match constant {
        ast::CaseConstant::Integer(val) => {
            // Try to fit in i16, otherwise use i32
            TryInto::<i16>::try_into(*val)
                .map(ir::Value::Integer)
                .unwrap_or(ir::Value::Long(*val))
        }
        ast::CaseConstant::Float(val) => {
            // Use single precision if possible, otherwise double
            if *val >= f32::MIN as f64 && *val <= f32::MAX as f64 {
                ir::Value::SinglePrecision(*val as f32)
            } else {
                ir::Value::DoublePrecision(*val)
            }
        }
        ast::CaseConstant::String(val) => ir::Value::String(val.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ArrayAccess, AssignmentTarget, BinaryOperator, Expression, IfBranch, LogicalOperator,
        Metacommand, PlainStatement, RelationalOperator, UnaryOperator, Variable, VariableType,
    };
    use crate::ir::{Instruction, Value};

    // Helper functions for creating test AST nodes
    fn create_int_expr(value: i32) -> Expression {
        Expression::Integer(value)
    }

    fn create_float_expr(value: f64) -> Expression {
        Expression::Float(value)
    }

    fn create_string_expr(value: &str) -> Expression {
        Expression::String(value.to_string())
    }

    fn create_var_expr(name: &str, var_type: VariableType) -> Expression {
        Expression::Variable(Variable {
            name: name.to_string(),
            r#type: var_type,
        })
    }

    fn create_var(name: &str, var_type: VariableType) -> Variable {
        Variable {
            name: name.to_string(),
            r#type: var_type,
        }
    }

    mod expression_compilation {
        use super::*;

        #[test]
        fn test_compile_integer_literal() {
            let expr = create_int_expr(42);
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(42))
            ));
        }

        #[test]
        fn test_compile_large_integer() {
            let expr = create_int_expr(100000); // Larger than i16::MAX
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Long(100000))
            ));
        }

        #[test]
        #[allow(clippy::approx_constant)]
        fn test_compile_float_literal() {
            let expr = create_float_expr(3.14);
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(
                matches!(instructions[0], Instruction::LoadConst(Value::SinglePrecision(f)) if (f - 3.14).abs() < f32::EPSILON)
            );
        }

        #[test]
        fn test_compile_large_float() {
            let expr = create_float_expr(1e50); // Larger than f32::MAX
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(
                matches!(instructions[0], Instruction::LoadConst(Value::DoublePrecision(f)) if f == 1e50)
            );
        }

        #[test]
        fn test_compile_string_literal() {
            let expr = create_string_expr("Hello World");
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(
                matches!(&instructions[0], Instruction::LoadConst(Value::String(s)) if s == "Hello World")
            );
        }

        #[test]
        fn test_compile_variable() {
            let expr = create_var_expr("x", VariableType::Integer);
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 1);
            assert!(
                matches!(&instructions[0], Instruction::LoadVar(Variable { name, r#type }) if name == "x" && *r#type == VariableType::Integer)
            );
        }

        #[test]
        fn test_compile_binary_addition() {
            let expr = Expression::BinaryOp {
                left: Box::new(create_int_expr(2)),
                op: BinaryOperator::Add,
                right: Box::new(create_int_expr(3)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 3);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(2))
            ));
            assert!(matches!(
                instructions[1],
                Instruction::LoadConst(Value::Integer(3))
            ));
            assert!(matches!(instructions[2], Instruction::Add));
        }

        #[test]
        fn test_compile_binary_operations() {
            let test_cases = vec![
                (BinaryOperator::Subtract, Instruction::Subtract),
                (BinaryOperator::Multiply, Instruction::Multiply),
                (BinaryOperator::Divide, Instruction::Divide),
            ];

            for (ast_op, expected_instr) in test_cases {
                let expr = Expression::BinaryOp {
                    left: Box::new(create_int_expr(5)),
                    op: ast_op,
                    right: Box::new(create_int_expr(2)),
                };
                let instructions = compile_expression(expr);
                assert_eq!(instructions.len(), 3);
                assert!(
                    matches!(&instructions[2], instr if std::mem::discriminant(instr) == std::mem::discriminant(&expected_instr))
                );
            }
        }

        #[test]
        fn test_compile_unary_operations() {
            // Unary plus
            let expr = Expression::UnaryOp {
                op: UnaryOperator::Plus,
                operand: Box::new(create_int_expr(5)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 2);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(5))
            ));
            assert!(matches!(instructions[1], Instruction::UnaryPlus));

            // Unary minus
            let expr = Expression::UnaryOp {
                op: UnaryOperator::Minus,
                operand: Box::new(create_int_expr(5)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 2);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(5))
            ));
            assert!(matches!(instructions[1], Instruction::UnaryMinus));
        }

        #[test]
        fn test_compile_logical_operations() {
            // Binary AND
            let expr = Expression::LogicalOp {
                left: Some(Box::new(create_var_expr("a", VariableType::Integer))),
                op: LogicalOperator::And,
                right: Box::new(create_var_expr("b", VariableType::Integer)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 3);
            assert!(matches!(instructions[2], Instruction::And));

            // Binary OR
            let expr = Expression::LogicalOp {
                left: Some(Box::new(create_var_expr("a", VariableType::Integer))),
                op: LogicalOperator::Or,
                right: Box::new(create_var_expr("b", VariableType::Integer)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 3);
            assert!(matches!(instructions[2], Instruction::Or));

            // Unary NOT
            let expr = Expression::LogicalOp {
                left: None,
                op: LogicalOperator::Not,
                right: Box::new(create_var_expr("a", VariableType::Integer)),
            };
            let instructions = compile_expression(expr);
            assert_eq!(instructions.len(), 2);
            assert!(matches!(instructions[1], Instruction::Not));
        }

        #[test]
        fn test_compile_relational_operations() {
            let test_cases = vec![
                (RelationalOperator::Equal, Instruction::Equal),
                (RelationalOperator::NotEqual, Instruction::NotEqual),
                (RelationalOperator::LessThan, Instruction::LessThan),
                (
                    RelationalOperator::LessThanEqual,
                    Instruction::LessThanEqual,
                ),
                (RelationalOperator::GreaterThan, Instruction::GreaterThan),
                (
                    RelationalOperator::GreaterThanEqual,
                    Instruction::GreaterThanEqual,
                ),
            ];

            for (ast_op, expected_instr) in test_cases {
                let expr = Expression::RelationalOp {
                    left: Box::new(create_int_expr(5)),
                    op: ast_op,
                    right: Box::new(create_int_expr(3)),
                };
                let instructions = compile_expression(expr);
                assert_eq!(instructions.len(), 3);
                assert!(
                    matches!(&instructions[2], instr if std::mem::discriminant(instr) == std::mem::discriminant(&expected_instr))
                );
            }
        }

        #[test]
        fn test_compile_array_access() {
            let array_access = ArrayAccess {
                name: "arr".to_string(),
                indices: vec![create_int_expr(1), create_int_expr(2)],
            };
            let expr = Expression::ArrayAccess(array_access);
            let instructions = compile_expression(expr);

            assert_eq!(instructions.len(), 3);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(1))
            ));
            assert!(matches!(
                instructions[1],
                Instruction::LoadConst(Value::Integer(2))
            ));
            assert!(
                matches!(&instructions[2], Instruction::LoadArrayElement { name, num_indices } if name == "arr" && *num_indices == 2)
            );
        }

        #[test]
        fn test_compile_complex_expression() {
            // Test: (2 + 3) * 4
            let expr = Expression::BinaryOp {
                left: Box::new(Expression::BinaryOp {
                    left: Box::new(create_int_expr(2)),
                    op: BinaryOperator::Add,
                    right: Box::new(create_int_expr(3)),
                }),
                op: BinaryOperator::Multiply,
                right: Box::new(create_int_expr(4)),
            };
            let instructions = compile_expression(expr);

            // Should compile to: LoadConst(2), LoadConst(3), Add, LoadConst(4), Multiply
            assert_eq!(instructions.len(), 5);
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(2))
            ));
            assert!(matches!(
                instructions[1],
                Instruction::LoadConst(Value::Integer(3))
            ));
            assert!(matches!(instructions[2], Instruction::Add));
            assert!(matches!(
                instructions[3],
                Instruction::LoadConst(Value::Integer(4))
            ));
            assert!(matches!(instructions[4], Instruction::Multiply));
        }
    }

    mod statement_compilation {
        use super::*;
        use crate::ast::Statement;

        #[test]
        fn test_compile_print_statement() {
            let stmt = Statement::PlainStatement(PlainStatement::Print(create_int_expr(42)));
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            assert_eq!(program.instructions.len(), 3); // LoadConst, Print, Halt
            assert!(matches!(
                program.instructions[0],
                Instruction::LoadConst(Value::Integer(42))
            ));
            assert!(matches!(program.instructions[1], Instruction::Print));
            assert!(matches!(program.instructions[2], Instruction::Halt));
        }

        #[test]
        fn test_compile_variable_assignment() {
            let stmt = Statement::PlainStatement(PlainStatement::Assignment {
                target: AssignmentTarget::Variable(create_var("x", VariableType::Integer)),
                expression: create_int_expr(42),
            });
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            assert_eq!(program.instructions.len(), 3); // LoadConst, StoreVar, Halt
            assert!(matches!(
                program.instructions[0],
                Instruction::LoadConst(Value::Integer(42))
            ));
            assert!(
                matches!(&program.instructions[1], Instruction::StoreVar(Variable { name, .. }) if name == "x")
            );
            assert!(matches!(program.instructions[2], Instruction::Halt));
        }

        #[test]
        fn test_compile_array_assignment() {
            let array_access = ArrayAccess {
                name: "arr".to_string(),
                indices: vec![create_int_expr(1)],
            };
            let stmt = Statement::PlainStatement(PlainStatement::Assignment {
                target: AssignmentTarget::ArrayElement(array_access),
                expression: create_int_expr(42),
            });
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            // Should be: LoadConst(1), LoadConst(42), StoreArrayElement, Halt
            assert_eq!(program.instructions.len(), 4);
            assert!(matches!(
                program.instructions[0],
                Instruction::LoadConst(Value::Integer(1))
            ));
            assert!(matches!(
                program.instructions[1],
                Instruction::LoadConst(Value::Integer(42))
            ));
            assert!(
                matches!(&program.instructions[2], Instruction::StoreArrayElement { name, num_indices } if name == "arr" && *num_indices == 1)
            );
            assert!(matches!(program.instructions[3], Instruction::Halt));
        }

        #[test]
        fn test_compile_input_statement() {
            let stmt = Statement::PlainStatement(PlainStatement::Input {
                prompt: Some("Enter a number: ".to_string()),
                variables: vec![create_var("x", VariableType::Integer)],
            });
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            // Should be: LoadConst("Enter a number: "), Print, Input([x]), Halt
            assert_eq!(program.instructions.len(), 4);
            assert!(
                matches!(&program.instructions[0], Instruction::LoadConst(Value::String(s)) if s == "Enter a number: ")
            );
            assert!(matches!(program.instructions[1], Instruction::Print));
            assert!(
                matches!(&program.instructions[2], Instruction::Input(vars) if vars.len() == 1)
            );
            assert!(matches!(program.instructions[3], Instruction::Halt));
        }

        #[test]
        fn test_compile_input_without_prompt() {
            let stmt = Statement::PlainStatement(PlainStatement::Input {
                prompt: None,
                variables: vec![create_var("x", VariableType::Integer)],
            });
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            // Should be: Input([x]), Halt
            assert_eq!(program.instructions.len(), 2);
            assert!(
                matches!(&program.instructions[0], Instruction::Input(vars) if vars.len() == 1)
            );
            assert!(matches!(program.instructions[1], Instruction::Halt));
        }

        #[test]
        fn test_compile_metacommands() {
            let static_stmt =
                Statement::PlainStatement(PlainStatement::Metacommand(Metacommand::Static));
            let dynamic_stmt =
                Statement::PlainStatement(PlainStatement::Metacommand(Metacommand::Dynamic));
            let program = compile(ast::Program {
                statements: vec![static_stmt, dynamic_stmt],
            });

            assert_eq!(program.instructions.len(), 3); // SetStatic, SetDynamic, Halt
            assert!(matches!(program.instructions[0], Instruction::SetStatic));
            assert!(matches!(program.instructions[1], Instruction::SetDynamic));
            assert!(matches!(program.instructions[2], Instruction::Halt));
        }

        #[test]
        fn test_compile_noop_statement() {
            let stmt = Statement::PlainStatement(PlainStatement::Noop);
            let program = compile(ast::Program {
                statements: vec![stmt],
            });

            assert_eq!(program.instructions.len(), 1); // Just Halt
            assert!(matches!(program.instructions[0], Instruction::Halt));
        }
    }

    mod control_flow_compilation {
        use super::*;
        use crate::ast::Statement;

        #[test]
        fn test_compile_simple_if_statement() {
            let if_stmt = Statement::PlainStatement(PlainStatement::If {
                branches: vec![IfBranch {
                    condition: Some(create_int_expr(1)),
                    statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                        create_string_expr("True"),
                    ))],
                }],
            });
            let program = compile(ast::Program {
                statements: vec![if_stmt],
            });

            // Program structure should be:
            // LoadConst(1), JumpIfFalse(end), LoadConst("True"), Print, end:, Halt
            let instructions = &program.instructions;
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(1))
            ));
            assert!(matches!(instructions[1], Instruction::JumpIfFalse(_)));
            assert!(
                matches!(instructions[2], Instruction::LoadConst(Value::String(ref s)) if s == "True")
            );
            assert!(matches!(instructions[3], Instruction::Print));
            assert!(matches!(instructions[4], Instruction::Halt));
        }

        #[test]
        fn test_compile_if_else_statement() {
            let if_stmt = Statement::PlainStatement(PlainStatement::If {
                branches: vec![
                    IfBranch {
                        condition: Some(create_int_expr(1)),
                        statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                            create_string_expr("True"),
                        ))],
                    },
                    IfBranch {
                        condition: None, // ELSE branch
                        statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                            create_string_expr("False"),
                        ))],
                    },
                ],
            });
            let program = compile(ast::Program {
                statements: vec![if_stmt],
            });

            let instructions = &program.instructions;
            // Should have jump structure for if-else
            assert!(matches!(
                instructions[0],
                Instruction::LoadConst(Value::Integer(1))
            ));
            assert!(matches!(instructions[1], Instruction::JumpIfFalse(_)));
            assert!(
                matches!(instructions[2], Instruction::LoadConst(Value::String(ref s)) if s == "True")
            );
            assert!(matches!(instructions[3], Instruction::Print));
            assert!(matches!(instructions[4], Instruction::Jump(_))); // Jump to end
            assert!(
                matches!(instructions[5], Instruction::LoadConst(Value::String(ref s)) if s == "False")
            );
            assert!(matches!(instructions[6], Instruction::Print));
            assert!(matches!(instructions[7], Instruction::Halt));

            // Verify jump targets are reasonable (not testing exact values due to label generation)
            if let Instruction::JumpIfFalse(target) = instructions[1] {
                assert!(target > 4 && target < instructions.len());
            }
            if let Instruction::Jump(target) = instructions[4] {
                assert!(target >= 7 && target <= instructions.len());
            }
        }

        #[test]
        fn test_compile_while_statement() {
            let while_stmt = Statement::PlainStatement(PlainStatement::While {
                condition: create_var_expr("x", VariableType::Integer),
                statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                    create_string_expr("Loop"),
                ))],
            });
            let program = compile(ast::Program {
                statements: vec![while_stmt],
            });

            let instructions = &program.instructions;
            // While loop structure: condition_check:, LoadVar(x), JumpIfFalse(end), LoadConst("Loop"), Print, Jump(condition_check), end:, Halt
            assert!(matches!(instructions[0], Instruction::LoadVar(_)));
            assert!(matches!(instructions[1], Instruction::JumpIfFalse(_)));
            assert!(
                matches!(instructions[2], Instruction::LoadConst(Value::String(ref s)) if s == "Loop")
            );
            assert!(matches!(instructions[3], Instruction::Print));
            assert!(matches!(instructions[4], Instruction::Jump(_))); // Jump back to condition
            assert!(matches!(instructions[5], Instruction::Halt));
        }
    }

    mod compiler_context_tests {
        use super::*;

        #[test]
        fn test_label_generation() {
            let mut ctx = CompilerContext::new();
            let label1 = ctx.generate_label();
            let label2 = ctx.generate_label();

            assert_eq!(label1, "L0");
            assert_eq!(label2, "L1");
            assert_ne!(label1, label2);
        }

        #[test]
        fn test_instruction_emission() {
            let mut ctx = CompilerContext::new();
            ctx.emit_instruction(Instruction::LoadConst(Value::Integer(42)));
            ctx.emit_instruction(Instruction::Print);

            assert_eq!(ctx.instructions.len(), 2);
            assert!(matches!(
                ctx.instructions[0],
                Instruction::LoadConst(Value::Integer(42))
            ));
            assert!(matches!(ctx.instructions[1], Instruction::Print));
        }

        #[test]
        fn test_jump_patching() {
            let mut ctx = CompilerContext::new();
            let label = "test_label";

            // Emit a jump to a label that doesn't exist yet
            ctx.emit_jump(label);
            ctx.emit_instruction(Instruction::Print);

            // Place the label
            ctx.place_label(label);
            ctx.emit_instruction(Instruction::Halt);

            // Resolve jumps
            ctx.resolve_jumps();

            // The jump should now point to the instruction after Print (index 2)
            assert!(matches!(ctx.instructions[0], Instruction::Jump(2)));
            assert!(matches!(ctx.instructions[1], Instruction::Print));
            assert!(matches!(ctx.instructions[2], Instruction::Halt));
        }

        #[test]
        fn test_loop_context_management() {
            let mut ctx = CompilerContext::new();

            // Test empty loop stack
            assert!(ctx.find_loop_end_label("FOR").is_none());
            assert!(ctx.find_loop_end_label("DO").is_none());

            // Push a FOR loop context
            ctx.push_loop_context(LoopContext::For {
                end_label: "for_end".to_string(),
            });

            assert_eq!(ctx.find_loop_end_label("FOR"), Some(&"for_end".to_string()));
            assert!(ctx.find_loop_end_label("DO").is_none());

            // Push a DO loop context
            ctx.push_loop_context(LoopContext::Do {
                end_label: "do_end".to_string(),
            });

            // Should find the most recent DO loop
            assert_eq!(ctx.find_loop_end_label("DO"), Some(&"do_end".to_string()));
            assert_eq!(ctx.find_loop_end_label("FOR"), Some(&"for_end".to_string()));

            // Pop DO context
            ctx.pop_loop_context();
            assert!(ctx.find_loop_end_label("DO").is_none());
            assert_eq!(ctx.find_loop_end_label("FOR"), Some(&"for_end".to_string()));

            // Pop FOR context
            ctx.pop_loop_context();
            assert!(ctx.find_loop_end_label("FOR").is_none());
        }
    }

    mod integration_tests {
        use super::*;
        use crate::ast::Statement;

        #[test]
        fn test_compile_empty_program() {
            let program = compile(ast::Program { statements: vec![] });
            assert_eq!(program.instructions.len(), 1);
            assert!(matches!(program.instructions[0], Instruction::Halt));
        }

        #[test]
        fn test_compile_multi_statement_program() {
            let statements = vec![
                Statement::PlainStatement(PlainStatement::Assignment {
                    target: AssignmentTarget::Variable(create_var("x", VariableType::Integer)),
                    expression: create_int_expr(42),
                }),
                Statement::PlainStatement(PlainStatement::Print(create_var_expr(
                    "x",
                    VariableType::Integer,
                ))),
            ];
            let program = compile(ast::Program { statements });

            // Should be: LoadConst(42), StoreVar(x), LoadVar(x), Print, Halt
            assert_eq!(program.instructions.len(), 5);
            assert!(matches!(
                program.instructions[0],
                Instruction::LoadConst(Value::Integer(42))
            ));
            assert!(matches!(program.instructions[1], Instruction::StoreVar(_)));
            assert!(matches!(program.instructions[2], Instruction::LoadVar(_)));
            assert!(matches!(program.instructions[3], Instruction::Print));
            assert!(matches!(program.instructions[4], Instruction::Halt));
        }

        #[test]
        fn test_jump_resolution_integrity() {
            // Test that all jumps in a complex program resolve to valid instruction indices
            let if_stmt = Statement::PlainStatement(PlainStatement::If {
                branches: vec![
                    IfBranch {
                        condition: Some(create_int_expr(1)),
                        statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                            create_string_expr("Branch 1"),
                        ))],
                    },
                    IfBranch {
                        condition: Some(create_int_expr(2)),
                        statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                            create_string_expr("Branch 2"),
                        ))],
                    },
                    IfBranch {
                        condition: None,
                        statements: vec![ast::Statement::PlainStatement(PlainStatement::Print(
                            create_string_expr("Else"),
                        ))],
                    },
                ],
            });
            let program = compile(ast::Program {
                statements: vec![if_stmt],
            });

            // Verify all jump targets are within valid instruction range
            for (i, instruction) in program.instructions.iter().enumerate() {
                match instruction {
                    Instruction::Jump(target)
                    | Instruction::JumpIfFalse(target)
                    | Instruction::JumpIfTrue(target) => {
                        assert!(
                            *target < program.instructions.len(),
                            "Jump at index {} has invalid target {}, program has {} instructions",
                            i,
                            target,
                            program.instructions.len()
                        );
                    }
                    _ => {}
                }
            }
        }
    }
}
