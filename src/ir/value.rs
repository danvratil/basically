// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::{ast, vm::VMError};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    SinglePrecision(f32),
    DoublePrecision(f64),
    Integer(i16),
    Long(i32),
    /// Boolean value (QBasic uses 0 for false, -1 for true)
    Boolean(bool),
    /// A special value indicating an absence of an value (like a null pointer)
    Null,
}

impl Value {
    pub fn type_name(&self) -> &str {
        match self {
            Value::String(_) => "string",
            Value::SinglePrecision(_) => "single precision",
            Value::DoublePrecision(_) => "double precision",
            Value::Integer(_) => "integer",
            Value::Long(_) => "long",
            Value::Boolean(_) => "boolean",
            Value::Null => "null",
        }
    }

    pub fn as_variable_type(&self) -> ast::VariableType {
        match self {
            Value::String(_) => ast::VariableType::String,
            Value::SinglePrecision(_) => ast::VariableType::SinglePrecision,
            Value::DoublePrecision(_) => ast::VariableType::DoublePrecision,
            Value::Integer(_) => ast::VariableType::Integer,
            Value::Long(_) => ast::VariableType::Long,
            Value::Boolean(_) => ast::VariableType::Integer, // QBasic treats booleans as integers
            Value::Null => panic!("Null is not a valid variable type"),
        }
    }

    pub fn checked_add(self, other: &Value) -> Result<Option<Value>, VMError> {
        match (&self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.checked_add(*b).map(Value::Integer)),
            (Value::Integer(a), Value::Long(b)) => Ok((*a as i32).checked_add(*b).map(Value::Long)),
            (Value::Integer(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 + *b)))
            }
            (Value::Integer(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 + *b)))
            }
            (Value::Long(a), Value::Integer(b)) => Ok(a.checked_add(*b as i32).map(Value::Long)),
            (Value::Long(a), Value::Long(b)) => Ok(a.checked_add(*b).map(Value::Long)),
            (Value::Long(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 + *b)))
            }
            (Value::Long(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 + *b)))
            }
            (Value::SinglePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::SinglePrecision(a + *b as f32)))
            }
            (Value::SinglePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::SinglePrecision(a + *b as f32)))
            }
            (Value::SinglePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(a + b)))
            }
            (Value::SinglePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 + b)))
            }
            (Value::DoublePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::DoublePrecision(a + *b as f64)))
            }
            (Value::DoublePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::DoublePrecision(a + *b as f64)))
            }
            (Value::DoublePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a + *b as f64)))
            }
            (Value::DoublePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a + b)))
            }
            (Value::String(a), Value::String(b)) => Ok(Some(Value::String(format!("{a}{b}")))),
            _ => Err(VMError::TypeMismatch(
                self.type_name().to_string(),
                other.type_name().to_string(),
            )),
        }
    }

    pub fn checked_sub(self, other: &Value) -> Result<Option<Value>, VMError> {
        match (&self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.checked_sub(*b).map(Value::Integer)),
            (Value::Integer(a), Value::Long(b)) => Ok((*a as i32).checked_sub(*b).map(Value::Long)),
            (Value::Integer(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 - *b)))
            }
            (Value::Integer(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 - *b)))
            }
            (Value::Long(a), Value::Integer(b)) => Ok(a.checked_sub(*b as i32).map(Value::Long)),
            (Value::Long(a), Value::Long(b)) => Ok(a.checked_sub(*b).map(Value::Long)),
            (Value::Long(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 - *b)))
            }
            (Value::Long(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 - *b)))
            }
            (Value::SinglePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::SinglePrecision(a - *b as f32)))
            }
            (Value::SinglePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::SinglePrecision(a - *b as f32)))
            }
            (Value::SinglePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(a - b)))
            }
            (Value::SinglePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 - b)))
            }
            (Value::DoublePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::DoublePrecision(a - *b as f64)))
            }
            (Value::DoublePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::DoublePrecision(a - *b as f64)))
            }
            (Value::DoublePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a - *b as f64)))
            }
            (Value::DoublePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a - b)))
            }
            _ => Err(VMError::TypeMismatch(
                self.type_name().to_string(),
                other.type_name().to_string(),
            )),
        }
    }

    pub fn checked_mul(self, other: &Value) -> Result<Option<Value>, VMError> {
        match (&self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.checked_mul(*b).map(Value::Integer)),
            (Value::Integer(a), Value::Long(b)) => Ok((*a as i32).checked_mul(*b).map(Value::Long)),
            (Value::Integer(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 * *b)))
            }
            (Value::Integer(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 * *b)))
            }
            (Value::Long(a), Value::Integer(b)) => Ok(a.checked_mul(*b as i32).map(Value::Long)),
            (Value::Long(a), Value::Long(b)) => Ok(a.checked_mul(*b).map(Value::Long)),
            (Value::Long(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 * *b)))
            }
            (Value::Long(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 * *b)))
            }
            (Value::SinglePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::SinglePrecision(*a * *b as f32)))
            }
            (Value::SinglePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::SinglePrecision(*a * *b as f32)))
            }
            (Value::SinglePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(a * b)))
            }
            (Value::SinglePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 * b)))
            }
            (Value::DoublePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::DoublePrecision(*a * *b as f64)))
            }
            (Value::DoublePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::DoublePrecision(*a * *b as f64)))
            }
            (Value::DoublePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a * *b as f64)))
            }
            (Value::DoublePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a * b)))
            }
            _ => Err(VMError::TypeMismatch(
                self.type_name().to_string(),
                other.type_name().to_string(),
            )),
        }
    }

    pub fn checked_div(self, other: &Value) -> Result<Option<Value>, VMError> {
        match (&self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.checked_div(*b).map(Value::Integer)),
            (Value::Integer(a), Value::Long(b)) => Ok((*a as i32).checked_div(*b).map(Value::Long)),
            (Value::Integer(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 / *b)))
            }
            (Value::Integer(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 / *b)))
            }
            (Value::Long(a), Value::Integer(b)) => Ok(a.checked_div(*b as i32).map(Value::Long)),
            (Value::Long(a), Value::Long(b)) => Ok(a.checked_div(*b).map(Value::Long)),
            (Value::Long(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(*a as f32 / *b)))
            }
            (Value::Long(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 / *b)))
            }
            (Value::SinglePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::SinglePrecision(a / *b as f32)))
            }
            (Value::SinglePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::SinglePrecision(a / *b as f32)))
            }
            (Value::SinglePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::SinglePrecision(a / b)))
            }
            (Value::SinglePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(*a as f64 / b)))
            }
            (Value::DoublePrecision(a), Value::Integer(b)) => {
                Ok(Some(Value::DoublePrecision(a / *b as f64)))
            }
            (Value::DoublePrecision(a), Value::Long(b)) => {
                Ok(Some(Value::DoublePrecision(a / *b as f64)))
            }
            (Value::DoublePrecision(a), Value::SinglePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a / *b as f64)))
            }
            (Value::DoublePrecision(a), Value::DoublePrecision(b)) => {
                Ok(Some(Value::DoublePrecision(a / b)))
            }
            _ => Err(VMError::TypeMismatch(
                self.type_name().to_string(),
                other.type_name().to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VMError;

    mod type_operations {
        use super::*;

        #[test]
        fn test_type_name() {
            assert_eq!(Value::String("test".to_string()).type_name(), "string");
            assert_eq!(Value::SinglePrecision(1.0).type_name(), "single precision");
            assert_eq!(Value::DoublePrecision(1.0).type_name(), "double precision");
            assert_eq!(Value::Integer(1).type_name(), "integer");
            assert_eq!(Value::Long(1).type_name(), "long");
            assert_eq!(Value::Boolean(true).type_name(), "boolean");
            assert_eq!(Value::Null.type_name(), "null");
        }

        #[test]
        fn test_as_variable_type() {
            assert_eq!(Value::String("test".to_string()).as_variable_type(), ast::VariableType::String);
            assert_eq!(Value::SinglePrecision(1.0).as_variable_type(), ast::VariableType::SinglePrecision);
            assert_eq!(Value::DoublePrecision(1.0).as_variable_type(), ast::VariableType::DoublePrecision);
            assert_eq!(Value::Integer(1).as_variable_type(), ast::VariableType::Integer);
            assert_eq!(Value::Long(1).as_variable_type(), ast::VariableType::Long);
            assert_eq!(Value::Boolean(true).as_variable_type(), ast::VariableType::Integer);
        }

        #[test]
        #[should_panic(expected = "Null is not a valid variable type")]
        fn test_null_as_variable_type_panics() {
            Value::Null.as_variable_type();
        }
    }

    mod addition_tests {
        use super::*;

        #[test]
        fn test_integer_addition() {
            // Integer + Integer
            assert_eq!(
                Value::Integer(5).checked_add(&Value::Integer(3)).unwrap(),
                Some(Value::Integer(8))
            );

            // Integer + Long
            assert_eq!(
                Value::Integer(5).checked_add(&Value::Long(3)).unwrap(),
                Some(Value::Long(8))
            );

            // Integer + SinglePrecision
            assert_eq!(
                Value::Integer(5).checked_add(&Value::SinglePrecision(3.5)).unwrap(),
                Some(Value::SinglePrecision(8.5))
            );

            // Integer + DoublePrecision
            assert_eq!(
                Value::Integer(5).checked_add(&Value::DoublePrecision(3.5)).unwrap(),
                Some(Value::DoublePrecision(8.5))
            );
        }

        #[test]
        fn test_long_addition() {
            // Long + Integer
            assert_eq!(
                Value::Long(5).checked_add(&Value::Integer(3)).unwrap(),
                Some(Value::Long(8))
            );

            // Long + Long
            assert_eq!(
                Value::Long(5).checked_add(&Value::Long(3)).unwrap(),
                Some(Value::Long(8))
            );

            // Long + SinglePrecision
            assert_eq!(
                Value::Long(5).checked_add(&Value::SinglePrecision(3.5)).unwrap(),
                Some(Value::SinglePrecision(8.5))
            );

            // Long + DoublePrecision
            assert_eq!(
                Value::Long(5).checked_add(&Value::DoublePrecision(3.5)).unwrap(),
                Some(Value::DoublePrecision(8.5))
            );
        }

        #[test]
        fn test_float_addition() {
            // SinglePrecision + Integer
            assert_eq!(
                Value::SinglePrecision(5.5).checked_add(&Value::Integer(3)).unwrap(),
                Some(Value::SinglePrecision(8.5))
            );

            // SinglePrecision + Long
            assert_eq!(
                Value::SinglePrecision(5.5).checked_add(&Value::Long(3)).unwrap(),
                Some(Value::SinglePrecision(8.5))
            );

            // SinglePrecision + SinglePrecision
            assert_eq!(
                Value::SinglePrecision(5.5).checked_add(&Value::SinglePrecision(3.2)).unwrap(),
                Some(Value::SinglePrecision(8.7))
            );

            // SinglePrecision + DoublePrecision (promotes to double)
            let result = Value::SinglePrecision(5.5).checked_add(&Value::DoublePrecision(3.2)).unwrap();
            if let Some(Value::DoublePrecision(val)) = result {
                assert!((val - 8.7).abs() < 1e-6); // Account for floating point precision
            } else {
                panic!("Expected DoublePrecision result");
            }
        }

        #[test]
        fn test_double_addition() {
            // DoublePrecision + Integer
            assert_eq!(
                Value::DoublePrecision(5.5).checked_add(&Value::Integer(3)).unwrap(),
                Some(Value::DoublePrecision(8.5))
            );

            // DoublePrecision + Long
            assert_eq!(
                Value::DoublePrecision(5.5).checked_add(&Value::Long(3)).unwrap(),
                Some(Value::DoublePrecision(8.5))
            );

            // DoublePrecision + SinglePrecision
            let result = Value::DoublePrecision(5.5).checked_add(&Value::SinglePrecision(3.2)).unwrap();
            if let Some(Value::DoublePrecision(val)) = result {
                assert!((val - 8.7).abs() < 1e-6); // Account for floating point precision
            } else {
                panic!("Expected DoublePrecision result");
            }

            // DoublePrecision + DoublePrecision
            let result = Value::DoublePrecision(5.5).checked_add(&Value::DoublePrecision(3.2)).unwrap();
            if let Some(Value::DoublePrecision(val)) = result {
                assert!((val - 8.7).abs() < 1e-6); // Account for floating point precision
            } else {
                panic!("Expected DoublePrecision result");
            }
        }

        #[test]
        fn test_string_addition() {
            // String + String (concatenation)
            assert_eq!(
                Value::String("Hello".to_string()).checked_add(&Value::String(" World".to_string())).unwrap(),
                Some(Value::String("Hello World".to_string()))
            );
        }

        #[test]
        fn test_integer_overflow() {
            // Integer overflow returns None
            assert_eq!(
                Value::Integer(i16::MAX).checked_add(&Value::Integer(1)).unwrap(),
                None
            );

            // Long overflow returns None
            assert_eq!(
                Value::Long(i32::MAX).checked_add(&Value::Long(1)).unwrap(),
                None
            );
        }

        #[test]
        fn test_type_mismatch_errors() {
            // String + Integer should error
            match Value::String("test".to_string()).checked_add(&Value::Integer(5)) {
                Err(VMError::TypeMismatch(left, right)) => {
                    assert_eq!(left, "string");
                    assert_eq!(right, "integer");
                }
                _ => panic!("Expected type mismatch error")
            }

            // Boolean + Integer should error
            match Value::Boolean(true).checked_add(&Value::Integer(5)) {
                Err(VMError::TypeMismatch(left, right)) => {
                    assert_eq!(left, "boolean");
                    assert_eq!(right, "integer");
                }
                _ => panic!("Expected type mismatch error")
            }
        }
    }

    mod subtraction_tests {
        use super::*;

        #[test]
        fn test_integer_subtraction() {
            assert_eq!(
                Value::Integer(8).checked_sub(&Value::Integer(3)).unwrap(),
                Some(Value::Integer(5))
            );

            assert_eq!(
                Value::Integer(8).checked_sub(&Value::Long(3)).unwrap(),
                Some(Value::Long(5))
            );

            assert_eq!(
                Value::Integer(8).checked_sub(&Value::SinglePrecision(3.5)).unwrap(),
                Some(Value::SinglePrecision(4.5))
            );

            assert_eq!(
                Value::Integer(8).checked_sub(&Value::DoublePrecision(3.5)).unwrap(),
                Some(Value::DoublePrecision(4.5))
            );
        }

        #[test]
        fn test_float_subtraction() {
            assert_eq!(
                Value::SinglePrecision(8.5).checked_sub(&Value::Integer(3)).unwrap(),
                Some(Value::SinglePrecision(5.5))
            );

            let result = Value::DoublePrecision(8.5).checked_sub(&Value::SinglePrecision(3.2)).unwrap();
            if let Some(Value::DoublePrecision(val)) = result {
                assert!((val - 5.3).abs() < 1e-6); // Account for floating point precision
            } else {
                panic!("Expected DoublePrecision result");
            }
        }

        #[test]
        fn test_integer_underflow() {
            assert_eq!(
                Value::Integer(i16::MIN).checked_sub(&Value::Integer(1)).unwrap(),
                None
            );
        }

        #[test]
        fn test_subtraction_type_mismatch() {
            match Value::String("test".to_string()).checked_sub(&Value::Integer(5)) {
                Err(VMError::TypeMismatch(_, _)) => {}
                _ => panic!("Expected type mismatch error")
            }
        }
    }

    mod multiplication_tests {
        use super::*;

        #[test]
        fn test_integer_multiplication() {
            assert_eq!(
                Value::Integer(5).checked_mul(&Value::Integer(3)).unwrap(),
                Some(Value::Integer(15))
            );

            assert_eq!(
                Value::Integer(5).checked_mul(&Value::Long(3)).unwrap(),
                Some(Value::Long(15))
            );

            assert_eq!(
                Value::Integer(5).checked_mul(&Value::SinglePrecision(2.5)).unwrap(),
                Some(Value::SinglePrecision(12.5))
            );
        }

        #[test]
        fn test_float_multiplication() {
            assert_eq!(
                Value::SinglePrecision(2.5).checked_mul(&Value::Integer(4)).unwrap(),
                Some(Value::SinglePrecision(10.0))
            );

            assert_eq!(
                Value::DoublePrecision(2.5).checked_mul(&Value::DoublePrecision(3.0)).unwrap(),
                Some(Value::DoublePrecision(7.5))
            );
        }

        #[test]
        fn test_multiplication_overflow() {
            // Test integer overflow
            assert_eq!(
                Value::Integer(i16::MAX).checked_mul(&Value::Integer(2)).unwrap(),
                None
            );
        }

        #[test]
        fn test_multiplication_type_mismatch() {
            match Value::String("test".to_string()).checked_mul(&Value::Integer(5)) {
                Err(VMError::TypeMismatch(_, _)) => {}
                _ => panic!("Expected type mismatch error")
            }
        }
    }

    mod division_tests {
        use super::*;

        #[test]
        fn test_integer_division() {
            assert_eq!(
                Value::Integer(15).checked_div(&Value::Integer(3)).unwrap(),
                Some(Value::Integer(5))
            );

            assert_eq!(
                Value::Integer(15).checked_div(&Value::Long(3)).unwrap(),
                Some(Value::Long(5))
            );

            assert_eq!(
                Value::Integer(15).checked_div(&Value::SinglePrecision(2.5)).unwrap(),
                Some(Value::SinglePrecision(6.0))
            );
        }

        #[test]
        fn test_float_division() {
            assert_eq!(
                Value::SinglePrecision(7.5).checked_div(&Value::Integer(3)).unwrap(),
                Some(Value::SinglePrecision(2.5))
            );

            assert_eq!(
                Value::DoublePrecision(9.0).checked_div(&Value::DoublePrecision(3.0)).unwrap(),
                Some(Value::DoublePrecision(3.0))
            );
        }

        #[test]
        fn test_division_by_zero() {
            // Division by zero returns None
            assert_eq!(
                Value::Integer(5).checked_div(&Value::Integer(0)).unwrap(),
                None
            );

            assert_eq!(
                Value::Long(5).checked_div(&Value::Long(0)).unwrap(),
                None
            );
        }

        #[test]
        fn test_division_type_mismatch() {
            match Value::String("test".to_string()).checked_div(&Value::Integer(5)) {
                Err(VMError::TypeMismatch(_, _)) => {}
                _ => panic!("Expected type mismatch error")
            }
        }
    }

    mod edge_cases {
        use super::*;

        #[test]
        fn test_zero_operations() {
            // Adding zero
            assert_eq!(
                Value::Integer(5).checked_add(&Value::Integer(0)).unwrap(),
                Some(Value::Integer(5))
            );

            // Multiplying by zero
            assert_eq!(
                Value::Integer(5).checked_mul(&Value::Integer(0)).unwrap(),
                Some(Value::Integer(0))
            );

            // Subtracting zero
            assert_eq!(
                Value::Integer(5).checked_sub(&Value::Integer(0)).unwrap(),
                Some(Value::Integer(5))
            );
        }

        #[test]
        fn test_negative_numbers() {
            assert_eq!(
                Value::Integer(-5).checked_add(&Value::Integer(3)).unwrap(),
                Some(Value::Integer(-2))
            );

            assert_eq!(
                Value::Integer(-5).checked_mul(&Value::Integer(-2)).unwrap(),
                Some(Value::Integer(10))
            );
        }

        #[test]
        fn test_floating_point_precision() {
            // Test that floating point operations work as expected
            let result = Value::SinglePrecision(0.1).checked_add(&Value::SinglePrecision(0.2)).unwrap();
            if let Some(Value::SinglePrecision(val)) = result {
                assert!((val - 0.3).abs() < f32::EPSILON * 10.0); // Account for floating point precision
            } else {
                panic!("Expected SinglePrecision result");
            }
        }

        #[test]
        fn test_type_promotion_chain() {
            // Integer → Long → Single → Double promotion chain
            let int_val = Value::Integer(5);
            let long_result = int_val.checked_add(&Value::Long(3)).unwrap();
            assert!(matches!(long_result, Some(Value::Long(8))));

            let single_result = Value::Long(5).checked_add(&Value::SinglePrecision(3.0)).unwrap();
            assert!(matches!(single_result, Some(Value::SinglePrecision(_))));

            let double_result = Value::SinglePrecision(5.0).checked_add(&Value::DoublePrecision(3.0)).unwrap();
            assert!(matches!(double_result, Some(Value::DoublePrecision(_))));
        }
    }
}
