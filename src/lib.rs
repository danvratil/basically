// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

pub mod parser;
pub mod ast;
pub mod ir;
pub mod vm;

use std::io;

use pest::Parser;

use crate::parser::{BasicParser, Rule};

pub type OutputHandler = Box<dyn FnMut(String)>;
pub type InputHandler = Box<dyn FnMut() -> String>;


pub struct Basically {
    output_handler: OutputHandler,
    input_handler: InputHandler,
}

impl Default for Basically {
    fn default() -> Self {
        Self {
            output_handler: Box::new(default_output_handler),
            input_handler: Box::new(default_input_handler),
        }
    }
}

impl Basically {
    pub fn run(self, program: &str) -> Result<(), Box<dyn std::error::Error>> {
        let mut parsed = BasicParser::parse(Rule::program, program)?;
        let ast = ast::Program::try_from(parsed.next().expect("Empty program"))?;
        let ir = ir::compile(ast);
        let mut vm = vm::VM::new(ir, self.output_handler, self.input_handler);

        vm.run()?;
        Ok(())
    }
}

pub struct BasicallyBuilder {
    output_handler: Option<OutputHandler>,
    input_handler: Option<InputHandler>,
}

impl BasicallyBuilder {
    pub fn new() -> Self {
        BasicallyBuilder {
            output_handler: None,
            input_handler: None,
        }
    }

    pub fn output_handler<F>(mut self, output_handler: F) -> Self
    where F: FnMut(String) + 'static {
        self.output_handler = Some(Box::new(output_handler));
        self
    }

    pub fn input_handler<F>(mut self, input_handler: F) -> Self
    where F: FnMut() -> String + 'static {
        self.input_handler = Some(Box::new(input_handler));
        self
    }

    pub fn build(self) -> Basically {
        Basically {
            output_handler: self.output_handler.unwrap_or_else(|| Box::new(default_output_handler)),
            input_handler: self.input_handler.unwrap_or_else(|| Box::new(default_input_handler)),
        }
    }
}

pub fn default_output_handler(output: String) {
    println!("{}", output);
}

pub fn default_input_handler() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();

    buf.trim().to_string()
}