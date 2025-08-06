//! Function classification (constructor, method, standalone)
//!
//! This module analyzes bytecode patterns and context to classify functions.

use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::HbcFile;

/// Classification of a function's type based on its usage pattern
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    /// Constructor function (uses LoadThisNS and assigns properties)
    Constructor {
        property_count: usize,
        has_methods: bool,
    },
    /// Method (assigned as property of non-global object)
    Method {
        /// Whether this appears to be a prototype method
        is_prototype_method: bool,
    },
    /// Standalone function (global or unassigned)
    Standalone,
    /// Anonymous function used in specific contexts
    Anonymous { context: AnonymousContext },
}

/// Context for anonymous functions
#[derive(Debug, Clone, PartialEq)]
pub enum AnonymousContext {
    /// Immediately invoked function expression
    IIFE,
    /// Callback (passed as argument)
    Callback,
    /// Unspecified anonymous usage
    Unknown,
}

/// Analyzes and classifies functions
pub struct FunctionClassifier;

impl FunctionClassifier {
    /// Classify a function based on its bytecode and usage context
    pub fn classify(
        function_index: u32,
        instructions: &[UnifiedInstruction],
        hbc_file: &HbcFile,
    ) -> FunctionType {
        // First check if it's a constructor
        if let Some(constructor_info) = analyze_constructor_pattern(instructions) {
            return FunctionType::Constructor {
                property_count: constructor_info.0,
                has_methods: constructor_info.1,
            };
        }

        // Check how this function is used in its parent function
        if let Some(parent_usage) = analyze_parent_usage(function_index, hbc_file) {
            return parent_usage;
        }

        // Default to standalone
        FunctionType::Standalone
    }

    /// Check if a function name suggests it's a constructor
    pub fn is_constructor_name(name: &str) -> bool {
        name.chars().next().map_or(false, |c| c.is_uppercase())
    }

    /// Check if a function name suggests it's a method
    pub fn is_method_name(name: &str) -> bool {
        // Common method patterns: starts with lowercase, contains verbs
        let method_prefixes = ["get", "set", "is", "has", "can", "do", "on", "handle"];
        let lower = name.to_lowercase();

        // Check if starts with lowercase
        let starts_lower = name.chars().next().map_or(false, |c| c.is_lowercase());

        // Check for common method prefixes
        let has_method_prefix = method_prefixes
            .iter()
            .any(|prefix| lower.starts_with(prefix));

        starts_lower || has_method_prefix
    }
}

/// Analyze instructions for constructor patterns
fn analyze_constructor_pattern(instructions: &[UnifiedInstruction]) -> Option<(usize, bool)> {
    let mut has_load_this = false;
    let mut this_register: Option<u8> = None;
    let mut property_count = 0;
    let mut has_methods = false;
    let mut closure_registers = std::collections::HashSet::new();

    for instruction in instructions {
        match instruction {
            UnifiedInstruction::LoadThisNS { operand_0, .. } => {
                has_load_this = true;
                this_register = Some(*operand_0);
            }

            UnifiedInstruction::CreateClosure { operand_0, .. }
            | UnifiedInstruction::CreateAsyncClosure { operand_0, .. }
            | UnifiedInstruction::CreateGeneratorClosure { operand_0, .. } => {
                closure_registers.insert(*operand_0);
            }

            UnifiedInstruction::PutById {
                operand_0,
                operand_1,
                ..
            } => {
                if let Some(this_reg) = this_register {
                    if *operand_0 == this_reg {
                        property_count += 1;
                        if closure_registers.contains(operand_1) {
                            has_methods = true;
                        }
                    }
                }
            }

            _ => {}
        }
    }

    if has_load_this && property_count > 0 {
        Some((property_count, has_methods))
    } else {
        None
    }
}

/// Analyze how a function is used in its parent context
fn analyze_parent_usage(function_index: u32, hbc_file: &HbcFile) -> Option<FunctionType> {
    // Find which function creates this closure
    for parent_idx in 0..hbc_file.functions.count() {
        if let Ok(func) = hbc_file.functions.get(parent_idx, hbc_file) {
            let mut closure_register: Option<u8> = None;
            let mut found_creation = false;

            for (i, instr) in func.instructions.iter().enumerate() {
                match &instr.instruction {
                    UnifiedInstruction::CreateClosure {
                        operand_0,
                        operand_2,
                        ..
                    }
                    | UnifiedInstruction::CreateAsyncClosure {
                        operand_0,
                        operand_2,
                        ..
                    }
                    | UnifiedInstruction::CreateGeneratorClosure {
                        operand_0,
                        operand_2,
                        ..
                    } => {
                        if *operand_2 as u32 == function_index {
                            closure_register = Some(*operand_0);
                            found_creation = true;
                        }
                    }
                    _ => {}
                }

                // If we found the creation, look at the next few instructions
                if found_creation && closure_register.is_some() {
                    let reg = closure_register.unwrap();

                    // Look ahead for usage
                    for j in i..i.saturating_add(5).min(func.instructions.len()) {
                        match &func.instructions[j].instruction {
                            // Method assignment patterns
                            UnifiedInstruction::PutNewOwnByIdShort { operand_1, .. }
                            | UnifiedInstruction::PutNewOwnById { operand_1, .. } => {
                                if *operand_1 == reg {
                                    return Some(FunctionType::Method {
                                        is_prototype_method: false,
                                    });
                                }
                            }

                            UnifiedInstruction::PutById {
                                operand_0,
                                operand_1,
                                operand_3,
                                ..
                            } => {
                                if *operand_1 == reg {
                                    // Check if assigning to prototype
                                    let is_prototype = if let Ok(prop_name) =
                                        hbc_file.strings.get(*operand_3 as u32)
                                    {
                                        prop_name.contains("prototype")
                                            || prop_name == "getInstance"
                                    } else {
                                        false
                                    };

                                    // Check if target is global object (register from GetGlobalObject)
                                    let is_global = parent_idx == 0 && *operand_0 == 1; // Common pattern

                                    if !is_global {
                                        return Some(FunctionType::Method {
                                            is_prototype_method: is_prototype,
                                        });
                                    }
                                }
                            }

                            // Callback pattern - passed as argument
                            UnifiedInstruction::Call2 { operand_3, .. }
                            | UnifiedInstruction::Call3 { operand_3, .. }
                            | UnifiedInstruction::Call4 { operand_3, .. } => {
                                if *operand_3 == reg {
                                    return Some(FunctionType::Anonymous {
                                        context: AnonymousContext::Callback,
                                    });
                                }
                            }

                            _ => {}
                        }
                    }

                    break;
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constructor_name_detection() {
        assert!(FunctionClassifier::is_constructor_name("Person"));
        assert!(FunctionClassifier::is_constructor_name("MyClass"));
        assert!(!FunctionClassifier::is_constructor_name("add"));
        assert!(!FunctionClassifier::is_constructor_name("getName"));
    }

    #[test]
    fn test_method_name_detection() {
        assert!(FunctionClassifier::is_method_name("getName"));
        assert!(FunctionClassifier::is_method_name("setAge"));
        assert!(FunctionClassifier::is_method_name("isValid"));
        assert!(FunctionClassifier::is_method_name("render"));
        assert!(!FunctionClassifier::is_method_name("Person"));
        assert!(!FunctionClassifier::is_method_name("MyClass"));
    }
}
