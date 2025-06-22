// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct BasicParser;
