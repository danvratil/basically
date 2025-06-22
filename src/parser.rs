// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct BasicParser;

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_parse_print_42_statement() {
        let input = "PRINT 42";

        let mut pairs = BasicParser::parse(Rule::program, input).expect("Failed to parse input");
        let program = pairs.next().expect("Failed to get program");
        let statement = program.into_inner().next().expect("Failed to get print statement");
        assert_eq!(statement.as_rule(), Rule::statement);

        let print_statement = statement.into_inner().next().expect("Failed to get print statement");
        assert_eq!(print_statement.as_rule(), Rule::print_statement);

        let expression = print_statement.into_inner().next().expect("Failed to get expression");
        assert_eq!(expression.as_rule(), Rule::expression);

        let integer = expression.into_inner().next().expect("Failed to get print arguments");
        assert_eq!(integer.as_rule(), Rule::integer);
        assert_eq!(integer.as_str(), "42")
    }
}