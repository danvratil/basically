// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

mod compiler;
mod instructions;

pub use compiler::compile;
pub use instructions::{Instruction, Program};