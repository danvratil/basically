use crate::ast;
use crate::ir;

use std::iter;

pub fn compile(program: ast::Program) -> ir::Program {
    ir::Program {
        instructions: program
            .statements
            .into_iter()
            .map(compile_statement) // compile each statement into a list of instructions
            .flatten() // flatten the list
            .chain(iter::once(ir::Instruction::Halt)) // append a halt instruction at the end
            .collect(),
    }
}

fn compile_statement(statement: ast::Statement) -> Vec<ir::Instruction> {
    match statement {
        ast::Statement::Print(expr) => compile_expression(expr)
            .into_iter()
            .chain(iter::once(ir::Instruction::Print))
            .collect(),
    }
}

fn compile_expression(expr: ast::Expression) -> Vec<ir::Instruction> {
    match expr {
        ast::Expression::Integer(value) => {
            vec![ir::Instruction::LoadConst(value)]
        }
    }
}
