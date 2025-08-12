// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use basically_interpreter::ast::Program;
use basically_interpreter::parser::{BasicParser, Rule};
use basically_interpreter::vm::VM;
use basically_interpreter::{default_input_handler, default_output_handler, ir};

use pest::Parser;
use pest::iterators::Pair;
use std::io::{self, Read};

fn print_pair(pair: &Pair<Rule>, indent: usize) {
    let indent_str = " ".repeat(indent);
    println!(
        "{}Rule::{:?} -> {:?}",
        indent_str,
        pair.as_rule(),
        pair.as_str()
    );
    for inner_pair in pair.clone().into_inner() {
        print_pair(&inner_pair, indent + 2);
    }
}

fn main() {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read from stdin");

    println!("=== PROGRAM ====");
    println!("{input}");

    println!("\n=== PARSER ===");

    let mut pairs = BasicParser::parse(Rule::program, &input).expect("Failed to parse input");
    for pair in pairs.clone() {
        print_pair(&pair, 0);
    }

    println!("\n=== AST ===");

    let program =
        Program::try_from(pairs.next().expect("Empty program")).expect("Failed to get program");
    println!("{program:?}");

    println!("\n=== IR ===");

    let ir_program = ir::compile(program);
    for (idx, instruction) in ir_program.instructions.iter().enumerate() {
        println!("{idx:>3}: {instruction:?}");
    }

    println!("\n=== Executing ===");
    let mut vm = VM::new(
        ir_program,
        Box::new(default_output_handler),
        Box::new(default_input_handler),
    );
    vm.run().expect("Failed to run program");
}
