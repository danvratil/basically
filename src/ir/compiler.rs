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
