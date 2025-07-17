// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::ast::{Variable, VariableType};
use crate::ir::Value;

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(Value),
    LoadVar(Variable),
    StoreVar(Variable),
    Input(Vec<Variable>),
    DeclareArray {
        name: String,
        element_type: VariableType,
        dimensions: Vec<(isize, isize)>, // (lower_bound, upper_bound) pairs
    },
    LoadArrayElement {
        name: String,
        num_indices: usize,
    },
    StoreArrayElement {
        name: String,
        num_indices: usize,
    },

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