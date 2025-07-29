// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use crate::{ast, vm::VMError};

#[derive(Debug, Clone)]
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

// TODO: Missing test coverage for all the match operations
