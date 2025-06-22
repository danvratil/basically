// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(i16),

    Add,
    Subtract,
    Multiply,
    Divide,

    Print,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}