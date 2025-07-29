// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;

use crate::{ast::VariableType, ir, vm::VMError};

const MAX_NUM_DIMENSIONS: usize = 60;
const MIN_NUM_DIMENSIONS: usize = 1;
const MAX_SUBSCRIPT: isize = 32767;
const MIN_SUBSCRIPT: isize = -32768;

#[derive(Debug, Clone)]
pub struct Dimension {
    pub lower_bound: isize,
    pub upper_bound: isize,
}

impl Dimension {
    pub fn len(&self) -> usize {
        (self.upper_bound - self.lower_bound + 1) as usize
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub dimensions: Vec<Dimension>,
    pub data: Vec<ir::Value>,
}

impl Array {
    pub fn new(element_type: VariableType, dimensions: Vec<Dimension>) -> Self {
        let total_size = dimensions.iter().map(|d| d.len()).product();
        let default_value = Self::default_value_for_type(&element_type);

        Self {
            dimensions,
            data: vec![default_value; total_size],
        }
    }

    fn default_value_for_type(var_type: &VariableType) -> ir::Value {
        match var_type {
            VariableType::String => ir::Value::String(String::new()),
            VariableType::SinglePrecision => ir::Value::SinglePrecision(0.0),
            VariableType::DoublePrecision => ir::Value::DoublePrecision(0.0),
            VariableType::Integer => ir::Value::Integer(0),
            VariableType::Long => ir::Value::Long(0),
        }
    }

    pub fn get(&self, indices: &[isize]) -> Result<&ir::Value, VMError> {
        if indices.len() != self.dimensions.len() {
            return Err(VMError::WrongNumberOfDimensions);
        }

        let offset = self.calculate_offset(indices)?;
        Ok(&self.data[offset])
    }

    pub fn set(&mut self, indices: &[isize], value: ir::Value) -> Result<(), VMError> {
        if indices.len() != self.dimensions.len() {
            return Err(VMError::WrongNumberOfDimensions);
        }

        let offset = self.calculate_offset(indices)?;
        self.data[offset] = value;
        Ok(())
    }

    fn calculate_offset(&self, indices: &[isize]) -> Result<usize, VMError> {
        let mut offset = 0;
        let mut multiplier = 1;

        // Calculate offset using row-major order
        for i in (0..indices.len()).rev() {
            let index = indices[i];
            let dimension = &self.dimensions[i];

            if index < dimension.lower_bound || index > dimension.upper_bound {
                return Err(VMError::SubscriptOutOfRange);
            }

            offset += ((index - dimension.lower_bound) as usize) * multiplier;
            multiplier *= dimension.len();
        }

        Ok(offset)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ArrayStorageMode {
    Static,
    Dynamic,
}

#[derive(Debug)]
pub struct ArrayStorage {
    arrays: HashMap<String, Array>,
    default_lower_bound: isize, // 0 for dynamic, 1 for static (default)
}

impl ArrayStorage {
    pub fn new() -> Self {
        Self {
            arrays: HashMap::new(),
            default_lower_bound: 1, // QBasic default is 1-based arrays
        }
    }

    pub fn set_default_mode(&mut self, mode: ArrayStorageMode) {
        self.default_lower_bound = match mode {
            ArrayStorageMode::Static => 1,
            ArrayStorageMode::Dynamic => 0,
        };
    }

    pub fn declare_array(
        &mut self,
        name: String,
        element_type: VariableType,
        dimension_bounds: Vec<(isize, isize)>,
    ) -> Result<(), VMError> {
        if name.is_empty() {
            return Err(VMError::UndefinedVariable(
                "Array name cannot be empty".to_string(),
            ));
        }

        if dimension_bounds.len() < MIN_NUM_DIMENSIONS
            || dimension_bounds.len() > MAX_NUM_DIMENSIONS
        {
            return Err(VMError::WrongNumberOfDimensions);
        }

        let dimensions = dimension_bounds
            .into_iter()
            .map(|(lower, upper)| {
                let actual_lower = if lower == -1 {
                    // Sentinel value for default bounds - use the mode's default
                    self.default_lower_bound
                } else {
                    // Explicit bounds - use as specified
                    lower
                };

                if !(MIN_SUBSCRIPT..=MAX_SUBSCRIPT).contains(&actual_lower)
                    || !(MIN_SUBSCRIPT..=MAX_SUBSCRIPT).contains(&upper)
                {
                    return Err(VMError::SubscriptOutOfRange);
                }

                Ok(Dimension {
                    lower_bound: actual_lower,
                    upper_bound: upper,
                })
            })
            .collect::<Result<Vec<_>, VMError>>()?;

        let array = Array::new(element_type, dimensions);
        self.arrays.insert(name, array);
        Ok(())
    }

    pub fn get_array_element(&self, name: &str, indices: &[isize]) -> Result<&ir::Value, VMError> {
        let array = self
            .arrays
            .get(name)
            .ok_or_else(|| VMError::UndefinedVariable(name.to_string()))?;
        array.get(indices)
    }

    pub fn set_array_element(
        &mut self,
        name: &str,
        indices: &[isize],
        value: ir::Value,
    ) -> Result<(), VMError> {
        let array = self
            .arrays
            .get_mut(name)
            .ok_or_else(|| VMError::UndefinedVariable(name.to_string()))?;
        array.set(indices, value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1d_array_offset_calculation() {
        // Test 1D array: A(1 TO 5) -> indices 1,2,3,4,5 should map to offsets 0,1,2,3,4
        let dimensions = vec![Dimension {
            lower_bound: 1,
            upper_bound: 5,
        }];
        let array = Array::new(VariableType::Integer, dimensions);

        assert_eq!(array.calculate_offset(&[1]).unwrap(), 0);
        assert_eq!(array.calculate_offset(&[2]).unwrap(), 1);
        assert_eq!(array.calculate_offset(&[3]).unwrap(), 2);
        assert_eq!(array.calculate_offset(&[4]).unwrap(), 3);
        assert_eq!(array.calculate_offset(&[5]).unwrap(), 4);
    }

    #[test]
    fn test_2d_array_offset_calculation() {
        // Test 2D array: A(1 TO 3, 1 TO 2)
        // This creates a 3x2 array with 6 total elements
        // In row-major order:
        // A(1,1)=0, A(1,2)=1, A(2,1)=2, A(2,2)=3, A(3,1)=4, A(3,2)=5
        let dimensions = vec![
            Dimension {
                lower_bound: 1,
                upper_bound: 3,
            }, // 3 elements
            Dimension {
                lower_bound: 1,
                upper_bound: 2,
            }, // 2 elements
        ];
        let array = Array::new(VariableType::Integer, dimensions);

        assert_eq!(array.calculate_offset(&[1, 1]).unwrap(), 0); // First row, first col
        assert_eq!(array.calculate_offset(&[1, 2]).unwrap(), 1); // First row, second col
        assert_eq!(array.calculate_offset(&[2, 1]).unwrap(), 2); // Second row, first col
        assert_eq!(array.calculate_offset(&[2, 2]).unwrap(), 3); // Second row, second col
        assert_eq!(array.calculate_offset(&[3, 1]).unwrap(), 4); // Third row, first col
        assert_eq!(array.calculate_offset(&[3, 2]).unwrap(), 5); // Third row, second col
    }

    #[test]
    fn test_3d_array_offset_calculation() {
        // Test 3D array: A(1 TO 2, 1 TO 3, 1 TO 2)
        // This creates a 2x3x2 array with 12 total elements
        // In row-major order, the offset for A(i,j,k) should be:
        // offset = (i-1)*3*2 + (j-1)*2 + (k-1)
        let dimensions = vec![
            Dimension {
                lower_bound: 1,
                upper_bound: 2,
            }, // 2 elements
            Dimension {
                lower_bound: 1,
                upper_bound: 3,
            }, // 3 elements
            Dimension {
                lower_bound: 1,
                upper_bound: 2,
            }, // 2 elements
        ];
        let array = Array::new(VariableType::Integer, dimensions);

        // Test some key positions
        assert_eq!(array.calculate_offset(&[1, 1, 1]).unwrap(), 0); // (1-1)*6 + (1-1)*2 + (1-1) = 0
        assert_eq!(array.calculate_offset(&[1, 1, 2]).unwrap(), 1); // (1-1)*6 + (1-1)*2 + (2-1) = 1
        assert_eq!(array.calculate_offset(&[1, 2, 1]).unwrap(), 2); // (1-1)*6 + (2-1)*2 + (1-1) = 2
        assert_eq!(array.calculate_offset(&[1, 3, 2]).unwrap(), 5); // (1-1)*6 + (3-1)*2 + (2-1) = 5
        assert_eq!(array.calculate_offset(&[2, 1, 1]).unwrap(), 6); // (2-1)*6 + (1-1)*2 + (1-1) = 6
        assert_eq!(array.calculate_offset(&[2, 3, 2]).unwrap(), 11); // (2-1)*6 + (3-1)*2 + (2-1) = 11
    }

    #[test]
    fn test_array_bounds_checking() {
        let dimensions = vec![Dimension {
            lower_bound: 1,
            upper_bound: 3,
        }];
        let array = Array::new(VariableType::Integer, dimensions);

        // Valid indices
        assert!(array.calculate_offset(&[1]).is_ok());
        assert!(array.calculate_offset(&[2]).is_ok());
        assert!(array.calculate_offset(&[3]).is_ok());

        // Invalid indices
        assert!(matches!(
            array.calculate_offset(&[0]),
            Err(VMError::SubscriptOutOfRange)
        ));
        assert!(matches!(
            array.calculate_offset(&[4]),
            Err(VMError::SubscriptOutOfRange)
        ));
    }

    #[test]
    fn test_array_with_custom_bounds() {
        // Test array with custom lower bounds: A(5 TO 7, 10 TO 12)
        let dimensions = vec![
            Dimension {
                lower_bound: 5,
                upper_bound: 7,
            }, // 3 elements
            Dimension {
                lower_bound: 10,
                upper_bound: 12,
            }, // 3 elements
        ];
        let array = Array::new(VariableType::Integer, dimensions);

        assert_eq!(array.calculate_offset(&[5, 10]).unwrap(), 0); // (5-5)*3 + (10-10) = 0
        assert_eq!(array.calculate_offset(&[5, 11]).unwrap(), 1); // (5-5)*3 + (11-10) = 1
        assert_eq!(array.calculate_offset(&[6, 10]).unwrap(), 3); // (6-5)*3 + (10-10) = 3
        assert_eq!(array.calculate_offset(&[7, 12]).unwrap(), 8); // (7-5)*3 + (12-10) = 8
    }
}
