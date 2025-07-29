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

    // Unary operations
    UnaryPlus,
    UnaryMinus,

    // Logical operations
    And,
    Or,
    Not,

    // Relational operations
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    // Control flow instructions
    Jump(usize),        // Unconditional jump to instruction index
    JumpIfFalse(usize), // Jump if top of stack is false
    JumpIfTrue(usize),  // Jump if top of stack is true

    Print,
    Halt,

    SetStatic,
    SetDynamic,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Variable, VariableType};
    use crate::ir::Value;

    mod instruction_creation {
        use super::*;

        #[test]
        fn test_load_const_instruction() {
            let instr = Instruction::LoadConst(Value::Integer(42));
            assert!(matches!(instr, Instruction::LoadConst(Value::Integer(42))));
        }

        #[test]
        fn test_load_var_instruction() {
            let var = Variable {
                name: "test".to_string(),
                r#type: VariableType::Integer,
            };
            let instr = Instruction::LoadVar(var.clone());
            assert!(matches!(instr, Instruction::LoadVar(v) if v.name == "test"));
        }

        #[test]
        fn test_store_var_instruction() {
            let var = Variable {
                name: "result".to_string(),
                r#type: VariableType::String,
            };
            let instr = Instruction::StoreVar(var.clone());
            assert!(matches!(instr, Instruction::StoreVar(v) if v.name == "result" && v.r#type == VariableType::String));
        }

        #[test]
        fn test_input_instruction() {
            let vars = vec![
                Variable {
                    name: "x".to_string(),
                    r#type: VariableType::Integer,
                },
                Variable {
                    name: "name$".to_string(),
                    r#type: VariableType::String,
                },
            ];
            let instr = Instruction::Input(vars.clone());
            assert!(matches!(instr, Instruction::Input(v) if v.len() == 2));
        }

        #[test]
        fn test_declare_array_instruction() {
            let instr = Instruction::DeclareArray {
                name: "matrix".to_string(),
                element_type: VariableType::Integer,
                dimensions: vec![(1, 10), (1, 5)],
            };
            match instr {
                Instruction::DeclareArray { name, element_type, dimensions } => {
                    assert_eq!(name, "matrix");
                    assert_eq!(element_type, VariableType::Integer);
                    assert_eq!(dimensions.len(), 2);
                    assert_eq!(dimensions[0], (1, 10));
                    assert_eq!(dimensions[1], (1, 5));
                }
                _ => panic!("Expected DeclareArray instruction")
            }
        }

        #[test]
        fn test_array_access_instructions() {
            let load_instr = Instruction::LoadArrayElement {
                name: "arr".to_string(),
                num_indices: 2,
            };
            match load_instr {
                Instruction::LoadArrayElement { name, num_indices } => {
                    assert_eq!(name, "arr");
                    assert_eq!(num_indices, 2);
                }
                _ => panic!("Expected LoadArrayElement instruction")
            }

            let store_instr = Instruction::StoreArrayElement {
                name: "arr".to_string(),
                num_indices: 1,
            };
            match store_instr {
                Instruction::StoreArrayElement { name, num_indices } => {
                    assert_eq!(name, "arr");
                    assert_eq!(num_indices, 1);
                }
                _ => panic!("Expected StoreArrayElement instruction")
            }
        }
    }

    mod arithmetic_instructions {
        use super::*;

        #[test]
        fn test_arithmetic_instruction_variants() {
            assert!(matches!(Instruction::Add, Instruction::Add));
            assert!(matches!(Instruction::Subtract, Instruction::Subtract));
            assert!(matches!(Instruction::Multiply, Instruction::Multiply));
            assert!(matches!(Instruction::Divide, Instruction::Divide));
        }

        #[test]
        fn test_unary_instruction_variants() {
            assert!(matches!(Instruction::UnaryPlus, Instruction::UnaryPlus));
            assert!(matches!(Instruction::UnaryMinus, Instruction::UnaryMinus));
        }
    }

    mod logical_instructions {
        use super::*;

        #[test]
        fn test_logical_instruction_variants() {
            assert!(matches!(Instruction::And, Instruction::And));
            assert!(matches!(Instruction::Or, Instruction::Or));
            assert!(matches!(Instruction::Not, Instruction::Not));
        }
    }

    mod relational_instructions {
        use super::*;

        #[test]
        fn test_relational_instruction_variants() {
            assert!(matches!(Instruction::Equal, Instruction::Equal));
            assert!(matches!(Instruction::NotEqual, Instruction::NotEqual));
            assert!(matches!(Instruction::LessThan, Instruction::LessThan));
            assert!(matches!(Instruction::LessThanEqual, Instruction::LessThanEqual));
            assert!(matches!(Instruction::GreaterThan, Instruction::GreaterThan));
            assert!(matches!(Instruction::GreaterThanEqual, Instruction::GreaterThanEqual));
        }
    }

    mod control_flow_instructions {
        use super::*;

        #[test]
        fn test_jump_instructions() {
            let jump = Instruction::Jump(42);
            assert!(matches!(jump, Instruction::Jump(42)));

            let jump_if_false = Instruction::JumpIfFalse(100);
            assert!(matches!(jump_if_false, Instruction::JumpIfFalse(100)));

            let jump_if_true = Instruction::JumpIfTrue(200);
            assert!(matches!(jump_if_true, Instruction::JumpIfTrue(200)));
        }

        #[test]
        fn test_jump_instruction_targets() {
            match Instruction::Jump(42) {
                Instruction::Jump(target) => assert_eq!(target, 42),
                _ => panic!("Expected Jump instruction")
            }

            match Instruction::JumpIfFalse(100) {
                Instruction::JumpIfFalse(target) => assert_eq!(target, 100),
                _ => panic!("Expected JumpIfFalse instruction")
            }
        }
    }

    mod system_instructions {
        use super::*;

        #[test]
        fn test_system_instruction_variants() {
            assert!(matches!(Instruction::Print, Instruction::Print));
            assert!(matches!(Instruction::Halt, Instruction::Halt));
            assert!(matches!(Instruction::SetStatic, Instruction::SetStatic));
            assert!(matches!(Instruction::SetDynamic, Instruction::SetDynamic));
        }
    }

    mod program_tests {
        use super::*;

        #[test]
        fn test_empty_program() {
            let program = Program {
                instructions: vec![],
            };
            assert!(program.instructions.is_empty());
        }

        #[test]
        fn test_simple_program() {
            let program = Program {
                instructions: vec![
                    Instruction::LoadConst(Value::Integer(42)),
                    Instruction::StoreVar(Variable {
                        name: "x".to_string(),
                        r#type: VariableType::Integer,
                    }),
                    Instruction::LoadVar(Variable {
                        name: "x".to_string(),
                        r#type: VariableType::Integer,
                    }),
                    Instruction::Print,
                    Instruction::Halt,
                ],
            };
            assert_eq!(program.instructions.len(), 5);
            assert!(matches!(program.instructions[0], Instruction::LoadConst(_)));
            assert!(matches!(program.instructions[1], Instruction::StoreVar(_)));
            assert!(matches!(program.instructions[2], Instruction::LoadVar(_)));
            assert!(matches!(program.instructions[3], Instruction::Print));
            assert!(matches!(program.instructions[4], Instruction::Halt));
        }

        #[test]
        fn test_program_with_jumps() {
            let program = Program {
                instructions: vec![
                    Instruction::LoadConst(Value::Boolean(true)),
                    Instruction::JumpIfFalse(4), // Jump to instruction 4 if false
                    Instruction::LoadConst(Value::String("True branch".to_string())),
                    Instruction::Jump(5), // Jump to instruction 5
                    Instruction::LoadConst(Value::String("False branch".to_string())),
                    Instruction::Print,
                    Instruction::Halt,
                ],
            };
            assert_eq!(program.instructions.len(), 7);

            // Verify jump targets
            match &program.instructions[1] {
                Instruction::JumpIfFalse(target) => assert_eq!(*target, 4),
                _ => panic!("Expected JumpIfFalse instruction")
            }

            match &program.instructions[3] {
                Instruction::Jump(target) => assert_eq!(*target, 5),
                _ => panic!("Expected Jump instruction")
            }
        }

        #[test]
        fn test_program_cloning() {
            let original = Program {
                instructions: vec![
                    Instruction::LoadConst(Value::Integer(1)),
                    Instruction::Print,
                    Instruction::Halt,
                ],
            };

            let cloned = original.clone();
            assert_eq!(original.instructions.len(), cloned.instructions.len());

            // Verify instructions are the same
            for (orig, clone) in original.instructions.iter().zip(cloned.instructions.iter()) {
                match (orig, clone) {
                    (Instruction::LoadConst(Value::Integer(a)), Instruction::LoadConst(Value::Integer(b))) => {
                        assert_eq!(a, b);
                    }
                    (Instruction::Print, Instruction::Print) => {}
                    (Instruction::Halt, Instruction::Halt) => {}
                    _ => panic!("Instructions don't match")
                }
            }
        }
    }

    mod instruction_properties {
        use super::*;

        #[test]
        fn test_instruction_debug_format() {
            let instr = Instruction::LoadConst(Value::Integer(42));
            let debug_str = format!("{:?}", instr);
            assert!(debug_str.contains("LoadConst"));
            assert!(debug_str.contains("Integer"));
            assert!(debug_str.contains("42"));
        }

        #[test]
        fn test_program_debug_format() {
            let program = Program {
                instructions: vec![
                    Instruction::LoadConst(Value::String("Hello".to_string())),
                    Instruction::Print,
                ],
            };
            let debug_str = format!("{:?}", program);
            assert!(debug_str.contains("Program"));
            assert!(debug_str.contains("instructions"));
        }

        #[test]
        fn test_instruction_cloning() {
            let original = Instruction::DeclareArray {
                name: "test".to_string(),
                element_type: VariableType::String,
                dimensions: vec![(0, 10)],
            };
            let cloned = original.clone();

            match (&original, &cloned) {
                (
                    Instruction::DeclareArray { name: name1, element_type: type1, dimensions: dims1 },
                    Instruction::DeclareArray { name: name2, element_type: type2, dimensions: dims2 }
                ) => {
                    assert_eq!(name1, name2);
                    assert_eq!(type1, type2);
                    assert_eq!(dims1, dims2);
                }
                _ => panic!("Cloning failed")
            }
        }
    }

    mod edge_cases {
        use super::*;

        #[test]
        fn test_zero_indices_array_access() {
            // Edge case: array access with zero indices (shouldn't normally happen but test anyway)
            let instr = Instruction::LoadArrayElement {
                name: "scalar".to_string(),
                num_indices: 0,
            };
            match instr {
                Instruction::LoadArrayElement { name, num_indices } => {
                    assert_eq!(name, "scalar");
                    assert_eq!(num_indices, 0);
                }
                _ => panic!("Expected LoadArrayElement instruction")
            }
        }

        #[test]
        fn test_large_jump_targets() {
            let instr = Instruction::Jump(usize::MAX);
            match instr {
                Instruction::Jump(target) => assert_eq!(target, usize::MAX),
                _ => panic!("Expected Jump instruction")
            }
        }

        #[test]
        fn test_empty_input_variables() {
            let instr = Instruction::Input(vec![]);
            match instr {
                Instruction::Input(vars) => assert!(vars.is_empty()),
                _ => panic!("Expected Input instruction")
            }
        }

        #[test]
        fn test_empty_array_dimensions() {
            let instr = Instruction::DeclareArray {
                name: "empty".to_string(),
                element_type: VariableType::Integer,
                dimensions: vec![],
            };
            match instr {
                Instruction::DeclareArray { dimensions, .. } => assert!(dimensions.is_empty()),
                _ => panic!("Expected DeclareArray instruction")
            }
        }
    }
}
