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
    instructions: Vec<ir::Instruction>
}

impl VM {
    pub fn new(program: ir::Program) -> Self {
        Self {
            stack: vec![],
            pc: 0,
            instructions: program.instructions,
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
                        println!("{}", value);
                    } else {
                        return Err(VMError::StackUnderflow);
                    }
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