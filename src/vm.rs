// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

mod arrays;

use crate::{
    InputHandler, OutputHandler,
    ast,
    ir,
    vm::arrays::{ArrayStorage, ArrayStorageMode},
};
use enum_map::{enum_map, Enum, EnumMap};
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

    #[error("Wrong number of dimensions")]
    WrongNumberOfDimensions,

    #[error("Subscript out of range")]
    SubscriptOutOfRange,
}

pub struct VM {
    stack: Vec<ir::Value>,
    pc: usize,
    instructions: Vec<ir::Instruction>,

    variables_by_type: EnumMap<VariableType, HashMap<String, ir::Value>>,

    output_handler: OutputHandler,
    input_handler: InputHandler,

    array_storage: ArrayStorage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Enum)]
enum VariableType {
    String,
    SinglePrecision,
    DoublePrecision,
    Integer,
    Long,
}

impl From<&ast::VariableType> for VariableType {
    fn from(value: &ast::VariableType) -> Self {
        match value {
            ast::VariableType::String => VariableType::String,
            ast::VariableType::SinglePrecision => VariableType::SinglePrecision,
            ast::VariableType::DoublePrecision => VariableType::DoublePrecision,
            ast::VariableType::Integer => VariableType::Integer,
            ast::VariableType::Long => VariableType::Long,
        }
    }
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
            array_storage: ArrayStorage::new(),
        }
    }

    fn load_variable(&self, variable: &ast::Variable) -> Option<&ir::Value> {
        self.variables_by_type[(&variable.r#type).into()].get(&variable.name)
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
                        ast::VariableType::Integer => match value {
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
                        ast::VariableType::Long => match value {
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
                        ast::VariableType::SinglePrecision => match value {
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
                        ast::VariableType::DoublePrecision => match value {
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
                        ast::VariableType::String => match value {
                            ir::Value::String(value) => ir::Value::String(value),
                            _ => {
                                return Err(VMError::TypeMismatch(
                                    value.type_name().to_string(),
                                    "string".to_string(),
                                ));
                            }
                        },
                    };
                    self.variables_by_type[(&value.as_variable_type()).into()].insert(variable.name.clone(), value);
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
                            ir::Value::Null => {
                                return Err(VMError::TypeMismatch(
                                    "null".to_string(),
                                    "string".to_string(),
                                ));
                            }
                        }
                    } else {
                        return Err(VMError::StackUnderflow);
                    }
                }
                Instruction::Input(variables) => {
                    let input = (self.input_handler)();
                    let values = input.split(",").map(|v| v.trim().to_string());
                    for (variable, value) in variables.iter().zip(values) {
                        let var_type = (&variable.r#type).into();
                        self.variables_by_type[var_type].insert(
                            variable.name.clone(),
                            match variable.r#type {
                                ast::VariableType::Integer => {
                                    ir::Value::Integer(value.parse().map_err(|_| VMError::IOError)?)
                                }
                                ast::VariableType::Long => {
                                    ir::Value::Long(value.parse().map_err(|_| VMError::IOError)?)
                                }
                                ast::VariableType::SinglePrecision => ir::Value::SinglePrecision(
                                    value.parse().map_err(|_| VMError::IOError)?,
                                ),
                                ast::VariableType::DoublePrecision => ir::Value::DoublePrecision(
                                    value.parse().map_err(|_| VMError::IOError)?,
                                ),
                                ast::VariableType::String => ir::Value::String(value),
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
                Instruction::SetStatic => {
                    self.array_storage
                        .set_default_mode(ArrayStorageMode::Static);
                }
                Instruction::SetDynamic => {
                    self.array_storage
                        .set_default_mode(ArrayStorageMode::Dynamic);
                }
                Instruction::DeclareArray {
                    name,
                    element_type,
                    dimensions,
                } => {
                    self.array_storage.declare_array(
                        name.clone(),
                        element_type.clone(),
                        dimensions.clone(),
                    )?;
                }
                Instruction::LoadArrayElement { name, num_indices } => {
                    let mut indices = Vec::with_capacity(*num_indices);
                    for _ in 0..*num_indices {
                        let index_value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let index = match index_value {
                            ir::Value::Integer(i) => i as isize,
                            ir::Value::Long(i) => i as isize,
                            _ => return Err(VMError::TypeMismatch(
                                index_value.type_name().to_string(),
                                "integer".to_string(),
                            )),
                        };
                        indices.push(index);
                    }
                    indices.reverse(); // Stack is LIFO, but we want first-to-last index order
                    
                    let value = self.array_storage.get_array_element(name, &indices)?;
                    self.stack.push(value.clone());
                }
                Instruction::StoreArrayElement { name, num_indices } => {
                    let value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    
                    let mut indices = Vec::with_capacity(*num_indices);
                    for _ in 0..*num_indices {
                        let index_value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let index = match index_value {
                            ir::Value::Integer(i) => i as isize,
                            ir::Value::Long(i) => i as isize,
                            _ => return Err(VMError::TypeMismatch(
                                index_value.type_name().to_string(),
                                "integer".to_string(),
                            )),
                        };
                        indices.push(index);
                    }
                    indices.reverse(); // Stack is LIFO, but we want first-to-last index order
                    
                    self.array_storage.set_array_element(name, &indices, value)?;
                }
            }

            self.pc += 1;
        }

        return Ok(());
    }
}
