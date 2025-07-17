// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::{
    InputHandler, OutputHandler,
    ast::{Variable, VariableType},
    ir,
};
use enum_map::{EnumMap, enum_map};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VMError {
    #[error("Program out of bounds")]
    ProgramOutOfBounds,

    #[error("Stack underflow")]
    StackUnderflow,

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Type mismatch: {0}, {1}")]
    TypeMismatch(String, String),

    #[error("IO error")]
    IOError,

    #[error("Overflow")]
    Overflow,
}

pub struct VM {
    stack: Vec<ir::Value>,
    pc: usize,
    instructions: Vec<ir::Instruction>,

    variables_by_type: EnumMap<VariableType, HashMap<String, ir::Value>>,

    output_handler: OutputHandler,
    input_handler: InputHandler,
}

impl VM {
    pub fn new(
        program: ir::Program,
        output_handler: OutputHandler,
        input_handler: InputHandler,
    ) -> Self {
        Self {
            stack: vec![],
            pc: 0,
            instructions: program.instructions,
            variables_by_type: enum_map! {
                VariableType::String => HashMap::new(),
                VariableType::SinglePrecision => HashMap::new(),
                VariableType::DoublePrecision => HashMap::new(),
                VariableType::Integer => HashMap::new(),
                VariableType::Long => HashMap::new(),
            },
            output_handler,
            input_handler,
        }
    }

    fn load_variable(&self, variable: &Variable) -> Option<&ir::Value> {
        self.variables_by_type[variable.r#type].get(&variable.name)
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        loop {
            if self.pc >= self.instructions.len() {
                return Err(VMError::ProgramOutOfBounds);
            }

            use ir::Instruction;
            match &self.instructions[self.pc] {
                Instruction::LoadConst(value) => {
                    self.stack.push(value.clone());
                }
                Instruction::LoadVar(variable) => {
                    if let Some(value) = self.load_variable(&variable) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(VMError::UndefinedVariable(variable.name.clone()));
                    }
                }
                Instruction::StoreVar(variable) => {
                    let value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let value = match variable.r#type {
                        VariableType::Integer => match value {
                            ir::Value::Integer(value) => ir::Value::Integer(value),
                            ir::Value::Long(value) => {
                                ir::Value::Long(value.try_into().map_err(|_| VMError::Overflow)?)
                            }
                            ir::Value::SinglePrecision(value) => ir::Value::Integer(
                                (value.round_ties_even() as i32)
                                    .try_into()
                                    .map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::DoublePrecision(value) => ir::Value::Integer(
                                (value.round_ties_even() as i64)
                                    .try_into()
                                    .map_err(|_| VMError::Overflow)?,
                            ),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "integer".to_string(),
                                ));
                            }
                        },
                        VariableType::Long => match value {
                            ir::Value::Integer(value) => {
                                ir::Value::Long(value.try_into().map_err(|_| VMError::Overflow)?)
                            }
                            ir::Value::Long(value) => ir::Value::Long(value),
                            ir::Value::SinglePrecision(value) => ir::Value::Long(
                                (value.round_ties_even() as i32)
                                    .try_into()
                                    .map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::DoublePrecision(value) => ir::Value::Long(
                                (value.round_ties_even() as i64)
                                    .try_into()
                                    .map_err(|_| VMError::Overflow)?,
                            ),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "long".to_string(),
                                ));
                            }
                        },
                        VariableType::SinglePrecision => match value {
                            ir::Value::Integer(value) => ir::Value::SinglePrecision(
                                value.try_into().map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::Long(value) => ir::Value::SinglePrecision(value as f32),
                            ir::Value::SinglePrecision(value) => ir::Value::SinglePrecision(value),
                            ir::Value::DoublePrecision(value) => ir::Value::SinglePrecision(
                                // FIXME: Does this actually work?
                                if value >= f32::MIN as f64 && value <= f32::MAX as f64 {
                                    value as f32
                                } else {
                                    return Err(VMError::Overflow);
                                },
                            ),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "single precision".to_string(),
                                ));
                            }
                        },
                        VariableType::DoublePrecision => match value {
                            ir::Value::Integer(value) => ir::Value::DoublePrecision(
                                value.try_into().map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::Long(value) => ir::Value::DoublePrecision(
                                value.try_into().map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::SinglePrecision(value) => ir::Value::DoublePrecision(
                                value.try_into().map_err(|_| VMError::Overflow)?,
                            ),
                            ir::Value::DoublePrecision(value) => ir::Value::DoublePrecision(value),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "double precision".to_string(),
                                ));
                            }
                        },
                        VariableType::String => match value {
                            ir::Value::String(value) => ir::Value::String(value),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "string".to_string(),
                                ));
                            }
                        },
                    };
                    self.variables_by_type[variable.r#type].insert(variable.name.clone(), value);
                }
                Instruction::Print => {
                    if let Some(value) = self.stack.pop() {
                        match value {
                            ir::Value::Integer(value) => (self.output_handler)(value.to_string()),
                            ir::Value::Long(value) => (self.output_handler)(value.to_string()),
                            ir::Value::SinglePrecision(value) => {
                                (self.output_handler)(value.to_string())
                            }
                            ir::Value::DoublePrecision(value) => {
                                (self.output_handler)(value.to_string())
                            }
                            ir::Value::String(value) => (self.output_handler)(value),
                        }
                    } else {
                        return Err(VMError::StackUnderflow);
                    }
                }
                Instruction::Input(variables) => {
                    let input = (self.input_handler)();
                    let values = input.split(",").map(|v| v.trim().to_string());
                    for (variable, value) in variables.iter().zip(values) {
                        self.variables_by_type[variable.r#type].insert(
                            variable.name.clone(),
                            match variable.r#type {
                                VariableType::Integer => {
                                    ir::Value::Integer(value.parse().map_err(|_| VMError::IOError)?)
                                }
                                VariableType::Long => {
                                    ir::Value::Long(value.parse().map_err(|_| VMError::IOError)?)
                                }
                                VariableType::SinglePrecision => ir::Value::SinglePrecision(
                                    value.parse().map_err(|_| VMError::IOError)?,
                                ),
                                VariableType::DoublePrecision => ir::Value::DoublePrecision(
                                    value.parse().map_err(|_| VMError::IOError)?,
                                ),
                                VariableType::String => ir::Value::String(value),
                            },
                        );
                    }
                }
                Instruction::Add => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;

                    self.stack
                        .push(a.checked_add(&b)?.ok_or(VMError::Overflow)?);
                }
                Instruction::Subtract => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;

                    self.stack
                        .push(a.checked_sub(&b)?.ok_or(VMError::Overflow)?);
                }
                Instruction::Multiply => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;

                    self.stack
                        .push(a.checked_mul(&b)?.ok_or(VMError::Overflow)?);
                }
                Instruction::Divide => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;

                    self.stack
                        .push(a.checked_div(&b)?.ok_or(VMError::Overflow)?);
                }
                Instruction::Halt => {
                    break;
                }
            }

            self.pc += 1;
        }

        return Ok(());
    }
}
