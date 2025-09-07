//! Escape analysis for tracking whether objects escape the current function scope
//!
//! This module determines if an object "escapes" - meaning it's used in ways that
//! prevent safe inlining as an object literal.

use crate::cfg::ssa::types::{RegisterDef, RegisterUse};
use crate::cfg::ssa::SSAAnalysis;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::InstructionIndex;
use std::collections::HashSet;

/// Reasons why an object might escape
#[derive(Debug, Clone, PartialEq)]
pub enum EscapeReason {
    /// Object is returned from the function
    Returned,
    /// Object is passed as an argument to a function call
    PassedToFunction { callee: String },
    /// Object is stored in another object or array
    StoredInContainer,
    /// Object is assigned to a global or outer scope
    AssignedToOuterScope,
    /// Object is used in a way we can't track
    UnknownUsage,
    /// Object is captured by a closure
    CapturedByClosure,
    /// Object is thrown as an exception
    Thrown,
}

/// Result of escape analysis for an object
#[derive(Debug, Clone)]
pub struct EscapeAnalysisResult {
    /// Whether the object escapes
    pub escapes: bool,
    /// Reasons why the object escapes (if it does)
    pub reasons: Vec<EscapeReason>,
    /// Instructions where escape happens
    pub escape_points: Vec<InstructionIndex>,
}

impl EscapeAnalysisResult {
    /// Create a result indicating the object doesn't escape
    pub fn does_not_escape() -> Self {
        Self {
            escapes: false,
            reasons: Vec::new(),
            escape_points: Vec::new(),
        }
    }

    /// Create a result indicating the object escapes
    pub fn escapes_with(reason: EscapeReason, at: InstructionIndex) -> Self {
        Self {
            escapes: true,
            reasons: vec![reason],
            escape_points: vec![at],
        }
    }

    /// Add another escape reason
    pub fn add_escape(&mut self, reason: EscapeReason, at: InstructionIndex) {
        self.escapes = true;
        self.reasons.push(reason);
        self.escape_points.push(at);
    }
}

/// Analyzer for determining if objects escape their creation scope
pub struct EscapeAnalyzer<'a> {
    ssa: &'a SSAAnalysis,
}

impl<'a> EscapeAnalyzer<'a> {
    /// Create a new escape analyzer
    pub fn new(ssa: &'a SSAAnalysis) -> Self {
        Self { ssa }
    }

    /// Analyze if an object created at the given definition escapes
    pub fn analyze_object_escape(&self, object_def: &RegisterDef) -> EscapeAnalysisResult {
        let mut result = EscapeAnalysisResult::does_not_escape();
        let mut visited = HashSet::new();
        
        // Get all uses of this object
        if let Some(uses) = self.ssa.def_use_chains.get(object_def) {
            for use_site in uses {
                if visited.insert(use_site.clone()) {
                    self.check_use_for_escape(use_site, &mut result);
                }
            }
        }
        
        result
    }

    /// Check if a specific use of the object causes it to escape
    fn check_use_for_escape(&self, _use_site: &RegisterUse, _result: &mut EscapeAnalysisResult) {
        // Note: We need access to the actual instruction at the use site
        // This would require access to the CFG, which we'll need to pass in
        // For now, we'll mark this as a TODO
        
        // TODO: Get the instruction at use_site and analyze it
        // Common escape scenarios:
        // 1. Ret instruction - object is returned
        // 2. Call/Construct with object as argument - passed to function
        // 3. StoreToEnvironment - escapes to closure
        // 4. PutById/PutByVal where object is the value (not the target)
        // 5. Throw instruction - thrown as exception
    }

    /// Check if an instruction causes escape when the object is used in a specific operand position
    pub fn instruction_causes_escape(
        instruction: &UnifiedInstruction,
        operand_position: usize,
    ) -> Option<EscapeReason> {
        use UnifiedInstruction::*;
        
        match instruction {
            // Object is returned from the function
            Ret { .. } if operand_position == 0 => {
                Some(EscapeReason::Returned)
            }
            
            // Object is thrown as an exception
            Throw { .. } if operand_position == 0 => {
                Some(EscapeReason::Thrown)
            }
            
            // Object is passed as an argument to a call
            Call { .. } | CallLong { .. } | Construct { .. } | ConstructLong { .. } 
                if operand_position > 0 => {
                Some(EscapeReason::PassedToFunction {
                    callee: "unknown".to_string(),
                })
            }
            
            // Object is stored in another object (as the value, not the target)
            PutById { .. } | PutByIdLong { .. } | 
            PutNewOwnById { .. } | PutNewOwnByIdLong { .. } |
            PutNewOwnNEById { .. } | PutNewOwnNEByIdLong { .. } |
            PutNewOwnByIdShort { .. }
                if operand_position == 1 => {
                Some(EscapeReason::StoredInContainer)
            }
            
            // Object is stored by value
            PutByVal { .. } | PutOwnByVal { .. } 
                if operand_position == 2 => {
                Some(EscapeReason::StoredInContainer)
            }
            
            // Object is stored by index
            PutOwnByIndex { .. } | PutOwnByIndexL { .. }
                if operand_position == 1 => {
                Some(EscapeReason::StoredInContainer)
            }
            
            // Object is stored to environment (closure capture)
            StoreToEnvironment { .. } | StoreToEnvironmentL { .. }
                if operand_position == 0 => {
                Some(EscapeReason::CapturedByClosure)
            }
            
            // Object is stored as an N-property
            StoreNPToEnvironment { .. } | StoreNPToEnvironmentL { .. }
                if operand_position == 0 => {
                Some(EscapeReason::CapturedByClosure)
            }
            
            // Safe operations that don't cause escape:
            // - Property reads (GetById, GetByVal, etc.) 
            // - Property writes where object is the target (not the value)
            // - Comparison operations
            // - Type checks
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return_causes_escape() {
        let ret_instr = UnifiedInstruction::Ret { operand_0: 0 };
        let reason = EscapeAnalyzer::instruction_causes_escape(&ret_instr, 0);
        assert_eq!(reason, Some(EscapeReason::Returned));
    }

    #[test]
    fn test_property_write_target_does_not_escape() {
        let put_instr = UnifiedInstruction::PutById {
            operand_0: 0, // object (target)
            operand_1: 1, // value
            operand_2: 0,
            operand_3: 0,
        };
        // Object as target (position 0) doesn't escape
        let reason = EscapeAnalyzer::instruction_causes_escape(&put_instr, 0);
        assert_eq!(reason, None);
        
        // Object as value (position 1) does escape
        let reason = EscapeAnalyzer::instruction_causes_escape(&put_instr, 1);
        assert_eq!(reason, Some(EscapeReason::StoredInContainer));
    }

    #[test]
    fn test_call_argument_causes_escape() {
        let call_instr = UnifiedInstruction::Call {
            operand_0: 0,  // result
            operand_1: 1,  // callee
            operand_2: 2,  // arg count
        };
        // As an argument (position > 0 for Call)
        let reason = EscapeAnalyzer::instruction_causes_escape(&call_instr, 2);
        assert_eq!(reason, Some(EscapeReason::PassedToFunction {
            callee: "unknown".to_string()
        }));
    }
}