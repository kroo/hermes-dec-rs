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
        // First check if it's a constructor using ConstructorDetector
        if let Some(constructor_info) =
            super::ConstructorDetector::analyze(instructions, Some(hbc_file))
        {
            return FunctionType::Constructor {
                property_count: constructor_info.property_count,
                has_methods: constructor_info.has_methods,
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

/// Analyze how a function is used in its parent context
fn analyze_parent_usage(function_index: u32, hbc_file: &HbcFile) -> Option<FunctionType> {
    // Find which function creates this closure
    for parent_idx in 0..hbc_file.functions.count() {
        if let Ok(func) = hbc_file.functions.get(parent_idx, hbc_file) {
            let mut closure_register: Option<u8> = None;
            let mut found_creation = false;
            let mut global_object_register: Option<u8> = None;

            for (i, instr) in func.instructions.iter().enumerate() {
                match &instr.instruction {
                    // Track GetGlobalObject instructions
                    UnifiedInstruction::GetGlobalObject { operand_0, .. } => {
                        global_object_register = Some(*operand_0);
                    }
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
                                    let is_global = global_object_register
                                        .map_or(false, |global_reg| *operand_0 == global_reg);

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
