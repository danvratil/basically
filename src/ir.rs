mod compiler;
mod instructions;

pub use compiler::compile;
pub use instructions::{Instruction, Program};