// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

mod compiler;
mod instructions;
mod value;

pub use compiler::compile;
pub use instructions::{Instruction, Program};
pub use value::Value;