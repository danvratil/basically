// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::{ast::Variable, ir::Value};

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(Value),
    LoadVar(Variable),
    StoreVar(Variable),
    Input(Vec<Variable>),

    Add,
    Subtract,
    Multiply,
    Divide,

    Print,
    Halt,

    SetStatic,
    SetDynamic,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}