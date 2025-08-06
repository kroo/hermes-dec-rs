//! Default parameter analysis for functions
//!
//! This module analyzes bytecode patterns to detect default parameters in functions.
//! Hermes compiles default parameters into runtime undefined checks at the beginning
//! of functions.

use crate::generated::unified_instructions::UnifiedInstruction;
use std::collections::HashMap;

/// Information about a parameter's default value
#[derive(Debug, Clone)]
pub struct DefaultParameterInfo {
    /// Parameter index (0-based, excluding implicit 'this')
    pub param_index: u32,
    /// The instruction that loads the default value
    pub default_value_instruction: UnifiedInstruction,
    /// The PC where the default value is loaded
    pub default_value_pc: usize,
}

/// Analyzes a function's bytecode to detect default parameters
pub struct DefaultParameterAnalyzer;

impl DefaultParameterAnalyzer {
    /// Analyze a function's instructions to find default parameters
    ///
    /// Returns a map of parameter indices to their default value information
    pub fn analyze(instructions: &[UnifiedInstruction]) -> HashMap<u32, DefaultParameterInfo> {
        let mut defaults = HashMap::new();

        // Track which registers hold parameter values
        let mut param_registers: HashMap<u8, u32> = HashMap::new();

        // Track which register holds undefined for comparison
        let mut undefined_register: Option<u8> = None;

        // Scan through instructions looking for the default parameter pattern
        let mut i = 0;
        while i < instructions.len() {
            match &instructions[i] {
                // Step 1: LoadParam loads a parameter into a register
                UnifiedInstruction::LoadParam {
                    operand_0,
                    operand_1,
                    ..
                } => {
                    // Remember which register holds which parameter
                    // Note: operand_1 is the parameter index in bytecode (includes 'this')
                    // We subtract 1 to get the user-visible parameter index
                    if *operand_1 > 0 {
                        param_registers.insert(*operand_0, *operand_1 as u32 - 1);
                    }
                }

                // Step 2: LoadConstUndefined loads undefined for comparison
                UnifiedInstruction::LoadConstUndefined { operand_0, .. } => {
                    undefined_register = Some(*operand_0);
                }

                // Step 3: JStrictNotEqual checks if parameter is undefined
                UnifiedInstruction::JStrictNotEqual {
                    operand_0: _, // target offset
                    operand_1,    // left operand (parameter)
                    operand_2,    // right operand (undefined)
                    ..
                } => {
                    // Check if this is comparing a parameter with undefined
                    if let (Some(&param_idx), Some(undef_reg)) =
                        (param_registers.get(operand_1), undefined_register)
                    {
                        if *operand_2 == undef_reg {
                            // This is a default parameter check!
                            // The next instruction should load the default value
                            if i + 1 < instructions.len() {
                                // The instruction after the jump is the default value
                                let default_instruction = instructions[i + 1].clone();

                                // Verify this looks like a value-loading instruction
                                if is_value_loading_instruction(&default_instruction) {
                                    defaults.insert(
                                        param_idx,
                                        DefaultParameterInfo {
                                            param_index: param_idx,
                                            default_value_instruction: default_instruction,
                                            default_value_pc: i + 1,
                                        },
                                    );
                                }
                                // If not, we skip this pattern as it might be a different bytecode structure
                            }
                        }
                    }
                }

                _ => {}
            }

            i += 1;
        }

        defaults
    }

    /// Extract the default value from an instruction
    /// Returns a string representation of the default value
    pub fn extract_default_value(instruction: &UnifiedInstruction) -> Option<String> {
        match instruction {
            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                // This would need access to the string table to get the actual string
                // For now, return a placeholder
                Some(format!("string_{}", operand_1))
            }
            UnifiedInstruction::LoadConstZero { .. } => Some("0".to_string()),
            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => Some(operand_1.to_string()),
            UnifiedInstruction::LoadConstInt { operand_1, .. } => Some(operand_1.to_string()),
            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                // operand_1 is index into double array
                Some(format!("double_{}", operand_1))
            }
            UnifiedInstruction::LoadConstTrue { .. } => Some("true".to_string()),
            UnifiedInstruction::LoadConstFalse { .. } => Some("false".to_string()),
            UnifiedInstruction::LoadConstNull { .. } => Some("null".to_string()),
            UnifiedInstruction::LoadConstUndefined { .. } => Some("undefined".to_string()),
            _ => None,
        }
    }
}

/// Check if an instruction is likely to be loading a default value
fn is_value_loading_instruction(instruction: &UnifiedInstruction) -> bool {
    matches!(
        instruction,
        UnifiedInstruction::LoadConstString { .. }
            | UnifiedInstruction::LoadConstStringLongIndex { .. }
            | UnifiedInstruction::LoadConstUInt8 { .. }
            | UnifiedInstruction::LoadConstInt { .. }
            | UnifiedInstruction::LoadConstDouble { .. }
            | UnifiedInstruction::LoadConstBigInt { .. }
            | UnifiedInstruction::LoadConstBigIntLongIndex { .. }
            | UnifiedInstruction::LoadConstEmpty { .. }
            | UnifiedInstruction::LoadConstUndefined { .. }
            | UnifiedInstruction::LoadConstNull { .. }
            | UnifiedInstruction::LoadConstTrue { .. }
            | UnifiedInstruction::LoadConstFalse { .. }
            | UnifiedInstruction::LoadConstZero { .. }
            | UnifiedInstruction::NewObject { .. }
            | UnifiedInstruction::NewArray { .. }
            | UnifiedInstruction::GetGlobalObject { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_single_default_parameter() {
        // Test pattern from greet(name = "World")
        let instructions = vec![
            UnifiedInstruction::LoadParam {
                operand_0: 4,
                operand_1: 1,
            },
            UnifiedInstruction::LoadConstUndefined { operand_0: 0 },
            UnifiedInstruction::JStrictNotEqual {
                operand_0: 1, // jump offset
                operand_1: 4, // parameter register
                operand_2: 0, // undefined register
            },
            UnifiedInstruction::LoadConstString {
                operand_0: 4,
                operand_1: 2, // string table index for "World"
            },
        ];

        let defaults = DefaultParameterAnalyzer::analyze(&instructions);

        assert_eq!(defaults.len(), 1);
        assert!(defaults.contains_key(&0)); // First parameter (index 0)

        let param_info = &defaults[&0];
        assert_eq!(param_info.param_index, 0);
        assert_eq!(param_info.default_value_pc, 3);
    }
}
