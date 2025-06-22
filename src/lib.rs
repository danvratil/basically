// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

pub mod parser;
pub mod ast;
pub mod ir;
pub mod vm;

use pest::Parser;

use crate::parser::{BasicParser, Rule};

pub struct Basically {
    output_handler: Box<dyn FnMut(String)>,
}

impl Basically {
    pub fn new() -> Self {
        Basically {
            output_handler: Box::new(|output| println!("{}", output)),
        }
    }

    pub fn new_with_output_handler<F>(output_handler: F) -> Self
    where F: FnMut(String) + 'static {
        Basically {
            output_handler: Box::new(output_handler),
        }
    }

    pub fn run(self, program: &str) -> Result<(), Box<dyn std::error::Error>> {
        let mut parsed = BasicParser::parse(Rule::program, program)?;
        let ast = ast::Program::try_from(parsed.next().expect("Empty program"))?;
        let ir = ir::compile(ast);
        let mut vm = vm::VM::new(ir, self.output_handler);

        vm.run()?;
        Ok(())
    }
}