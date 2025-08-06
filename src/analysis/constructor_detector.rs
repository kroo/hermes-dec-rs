//! Constructor function detection
//!
//! This module analyzes bytecode patterns to identify constructor functions.
//! Constructor functions typically start with LoadThisNS and contain
//! multiple PutById instructions to assign properties to 'this'.

use crate::generated::unified_instructions::UnifiedInstruction;

/// Information about a potential constructor function
#[derive(Debug, Clone)]
pub struct ConstructorInfo {
    /// Number of properties assigned to 'this'
    pub property_count: usize,
    /// Property names assigned (in order)
    pub property_names: Vec<String>,
    /// Whether the function creates methods (assigns functions to this)
    pub has_methods: bool,
}

/// Analyzes functions to detect constructor patterns
pub struct ConstructorDetector;

impl ConstructorDetector {
    /// Analyze a function's instructions to determine if it's a constructor
    ///
    /// Returns Some(ConstructorInfo) if the function appears to be a constructor,
    /// None otherwise.
    pub fn analyze(instructions: &[UnifiedInstruction]) -> Option<ConstructorInfo> {
        // Check if the function starts with LoadThisNS
        if instructions.is_empty() {
            return None;
        }

        // Track if we've seen LoadThisNS
        let mut has_load_this = false;
        let mut this_register: Option<u8> = None;

        // Track properties assigned to 'this'
        let mut property_names = Vec::new();
        let mut has_methods = false;

        // Track which registers hold closures/functions
        let mut closure_registers = std::collections::HashSet::new();

        for instruction in instructions.iter() {
            match instruction {
                UnifiedInstruction::LoadThisNS { operand_0, .. } => {
                    // Found LoadThisNS - this is a strong indicator
                    has_load_this = true;
                    this_register = Some(*operand_0);
                }

                UnifiedInstruction::CreateClosure { operand_0, .. }
                | UnifiedInstruction::CreateAsyncClosure { operand_0, .. }
                | UnifiedInstruction::CreateGeneratorClosure { operand_0, .. } => {
                    // Track registers that hold functions
                    closure_registers.insert(*operand_0);
                }

                UnifiedInstruction::PutById {
                    operand_0, // object
                    operand_1, // value
                    operand_2, // property id
                    ..
                } => {
                    // Check if this is assigning to 'this'
                    if let Some(this_reg) = this_register {
                        if *operand_0 == this_reg {
                            // This is a property assignment to 'this'
                            property_names.push(format!("prop_{}", operand_2));

                            // Check if we're assigning a function
                            if closure_registers.contains(operand_1) {
                                has_methods = true;
                            }
                        }
                    }
                }

                _ => {}
            }
        }

        // Heuristics for constructor detection:
        // 1. Must have LoadThisNS
        // 2. Should have at least one property assignment to 'this'
        // 3. Often (but not always) returns undefined

        if has_load_this && !property_names.is_empty() {
            Some(ConstructorInfo {
                property_count: property_names.len(),
                property_names,
                has_methods,
            })
        } else {
            None
        }
    }

    /// Check if a function name suggests it's a constructor
    /// (e.g., starts with uppercase letter)
    pub fn is_constructor_name(name: &str) -> bool {
        name.chars().next().map_or(false, |c| c.is_uppercase())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_constructor() {
        // Test pattern from Person constructor
        let instructions = vec![
            UnifiedInstruction::LoadThisNS { operand_0: 1 },
            UnifiedInstruction::LoadParam {
                operand_0: 0,
                operand_1: 1,
            },
            UnifiedInstruction::PutById {
                operand_0: 1, // this
                operand_1: 0, // value
                operand_2: 1, // property id
                operand_3: 0, // string index
            },
        ];

        let result = ConstructorDetector::analyze(&instructions);
        assert!(result.is_some());

        let info = result.unwrap();
        assert_eq!(info.property_count, 1);
        assert!(!info.has_methods);
    }

    #[test]
    fn test_regular_function_not_constructor() {
        // Test pattern from regular function
        let instructions = vec![
            UnifiedInstruction::LoadParam {
                operand_0: 1,
                operand_1: 1,
            },
            UnifiedInstruction::LoadParam {
                operand_0: 0,
                operand_1: 2,
            },
            UnifiedInstruction::Add {
                operand_0: 0,
                operand_1: 1,
                operand_2: 0,
            },
            UnifiedInstruction::Ret { operand_0: 0 },
        ];

        let result = ConstructorDetector::analyze(&instructions);
        assert!(result.is_none());
    }

    #[test]
    fn test_constructor_name_detection() {
        assert!(ConstructorDetector::is_constructor_name("Person"));
        assert!(ConstructorDetector::is_constructor_name("MyClass"));
        assert!(!ConstructorDetector::is_constructor_name("add"));
        assert!(!ConstructorDetector::is_constructor_name("createPoint"));
        assert!(!ConstructorDetector::is_constructor_name(""));
    }
}
