// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::{ir, InputHandler, OutputHandler};
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
}

pub struct VM {
    stack: Vec<ir::Value>,
    pc: usize,
    instructions: Vec<ir::Instruction>,

    variables: HashMap<String, ir::Value>,

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
            variables: HashMap::new(),
            output_handler,
            input_handler,
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        loop {
            if self.pc >= self.instructions.len() {
                return Err(VMError::ProgramOutOfBounds);
            }

            use ir::Instruction;
            match self.instructions[self.pc] {
                Instruction::LoadConst(ref value) => {
                    self.stack.push(value.clone());
                }
                Instruction::LoadVar(ref variable) => {
                    if let Some(value) = self.variables.get(variable) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(VMError::UndefinedVariable(variable.clone()));
                    }
                }
                Instruction::StoreVar(ref variable) => {
                    self.variables.insert(
                        variable.clone(),
                        self.stack.pop().ok_or(VMError::StackUnderflow)?,
                    );
                }
                Instruction::Print => {
                    if let Some(value) = self.stack.pop() {
                        match value {
                            ir::Value::Number(value) => (self.output_handler)(value.to_string()),
                            ir::Value::String(value) => (self.output_handler)(value),
                        }
                    } else {
                        return Err(VMError::StackUnderflow);
                    }
                }
                Instruction::Input(ref variables) => {
                    let input = (self.input_handler)();
                    let values = input.split(",").map(|v| v.trim().to_string());
                    println!("Input: {input}");
                    println!("Variables: {:?}", variables);
                    for (variable, value) in variables.iter().zip(values) {
                        self.variables.insert(
                            variable.clone(),
                            ir::Value::Number(value.parse::<i16>().map_err(|_| {
                                VMError::IOError
                            })?)
                        );
                    }
                }
                Instruction::Add => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match (&a, &b) {
                        (ir::Value::Number(a), ir::Value::Number(b)) => {
                            self.stack.push(ir::Value::Number(a + b));
                        }
                        (ir::Value::String(a), ir::Value::String(b)) => {
                            self.stack.push(ir::Value::String(format!("{a}{b}")));
                        }
                        _ => {
                            return Err(VMError::TypeMismatch(
                                a.type_name().to_string(),
                                b.type_name().to_string(),
                            ));
                        }
                    }
                }
                Instruction::Subtract => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match (&a, &b) {
                        (ir::Value::Number(a), ir::Value::Number(b)) => {
                            self.stack.push(ir::Value::Number(a - b));
                        }
                        _ => {
                            return Err(VMError::TypeMismatch(
                                a.type_name().to_string(),
                                b.type_name().to_string(),
                            ));
                        }
                    }
                }
                Instruction::Multiply => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match (&a, &b) {
                        (ir::Value::Number(a), ir::Value::Number(b)) => {
                            self.stack.push(ir::Value::Number(a * b));
                        }
                        _ => {
                            return Err(VMError::TypeMismatch(
                                a.type_name().to_string(),
                                b.type_name().to_string(),
                            ));
                        }
                    }
                }
                Instruction::Divide => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    match (&a, &b) {
                        (ir::Value::Number(a), ir::Value::Number(b)) => {
                            self.stack.push(ir::Value::Number(a / b));
                        }
                        _ => {
                            return Err(VMError::TypeMismatch(
                                a.type_name().to_string(),
                                b.type_name().to_string(),
                            ));
                        }
                    }
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
