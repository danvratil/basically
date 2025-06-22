// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ir;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VMError {
    #[error("Program out of bounds")]
    ProgramOutOfBounds,

    #[error("Stack underflow")]
    StackUnderflow,
}


pub struct VM {
    stack: Vec<i16>,
    pc: usize,
    instructions: Vec<ir::Instruction>,

    output_handler: Box<dyn FnMut(String)>,
}

impl VM {
    pub fn new(program: ir::Program, output_handler: Box<dyn FnMut(String)>) -> Self {
        Self {
            stack: vec![],
            pc: 0,
            instructions: program.instructions,
            output_handler,
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        loop {
            if self.pc >= self.instructions.len() {
                return Err(VMError::ProgramOutOfBounds);
            }

            use ir::Instruction;
            match self.instructions[self.pc] {
                Instruction::LoadConst(value) => {
                    self.stack.push(value);
                },
                Instruction::Print => {
                    if let Some(value) = self.stack.pop() {
                        (self.output_handler)(value.to_string());
                    } else {
                        return Err(VMError::StackUnderflow);
                    }
                },
                Instruction::Add => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(a + b);
                },
                Instruction::Subtract => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(a - b);
                },
                Instruction::Multiply => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(a * b);
                },
                Instruction::Divide => {
                    let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                    self.stack.push(a / b);
                },
                Instruction::Halt => {
                    break;
                }
            }

            self.pc += 1;
        }

        return Ok(());
    }
}