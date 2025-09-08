//! Escape analysis for tracking whether objects escape the current function scope
//!
//! This module determines if an object "escapes" - meaning it's used in ways that
//! prevent safe inlining as an object literal.

use crate::cfg::ssa::types::{RegisterDef, RegisterUse};
use crate::cfg::ssa::SSAAnalysis;
use crate::cfg::Cfg;
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
    /// Whether it's safe to inline this object as a literal despite escaping
    pub safe_to_inline: bool,
}

impl EscapeAnalysisResult {
    /// Create a result indicating the object doesn't escape
    pub fn does_not_escape() -> Self {
        Self {
            escapes: false,
            reasons: Vec::new(),
            escape_points: Vec::new(),
            safe_to_inline: true,  // If it doesn't escape, it's safe to inline
        }
    }

    /// Create a result indicating the object escapes
    pub fn escapes_with(reason: EscapeReason, at: InstructionIndex) -> Self {
        // By default, assume not safe to inline when escaping
        // The analyze_object_escape method will refine this
        Self {
            escapes: true,
            reasons: vec![reason],
            escape_points: vec![at],
            safe_to_inline: false,
        }
    }

    /// Add another escape reason
    pub fn add_escape(&mut self, reason: EscapeReason, at: InstructionIndex) {
        self.escapes = true;
        self.reasons.push(reason);
        self.escape_points.push(at);
        // When adding escapes, by default mark as not safe to inline
        // The caller should update safe_to_inline based on the full analysis
        self.safe_to_inline = false;
    }
}

/// Analyzer for determining if objects escape their creation scope
pub struct EscapeAnalyzer<'a> {
    ssa: &'a SSAAnalysis,
    cfg: &'a Cfg<'a>,
}

impl<'a> EscapeAnalyzer<'a> {
    /// Create a new escape analyzer
    pub fn new(ssa: &'a SSAAnalysis, cfg: &'a Cfg<'a>) -> Self {
        Self { ssa, cfg }
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
        
        // Determine if it's safe to inline despite escaping
        if result.escapes {
            result.safe_to_inline = self.is_safe_to_inline(&result, object_def);
        }
        
        result
    }
    
    /// Determine if an escaping object is still safe to inline
    fn is_safe_to_inline(&self, escape_result: &EscapeAnalysisResult, object_def: &RegisterDef) -> bool {
        // If it only escapes via return, it's always safe to inline
        if escape_result.reasons.len() == 1 && matches!(escape_result.reasons[0], EscapeReason::Returned) {
            return true;
        }
        
        // If it escapes in other ways, we need to check that all mutations
        // happen before the first escape point
        if !escape_result.escape_points.is_empty() {
            let first_escape = escape_result.escape_points.iter().min().unwrap();
            
            // Check all uses of the object
            if let Some(uses) = self.ssa.def_use_chains.get(object_def) {
                for use_site in uses {
                    // Check if this use is a mutation (PutById, etc.)
                    if let Some(block) = self.cfg.graph().node_weight(use_site.block_id) {
                        if let Some(instr_info) = block.instructions().iter()
                            .find(|i| i.instruction_index == use_site.instruction_idx) {
                            
                            // Check if this is a mutation instruction
                            if Self::is_mutation_instruction(&instr_info.instruction, use_site.register) {
                                // If the mutation happens after the first escape, not safe
                                if use_site.instruction_idx > *first_escape {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
            
            // All mutations happen before escape
            return true;
        }
        
        false
    }
    
    /// Check if an instruction is a mutation of the given object register
    fn is_mutation_instruction(instruction: &UnifiedInstruction, obj_register: u8) -> bool {
        use UnifiedInstruction::*;
        
        match instruction {
            // PutById variants where the object is operand_0
            PutById { operand_0, .. } |
            PutByIdLong { operand_0, .. } |
            PutNewOwnById { operand_0, .. } |
            PutNewOwnByIdLong { operand_0, .. } |
            PutNewOwnByIdShort { operand_0, .. } |
            TryPutById { operand_0, .. } |
            TryPutByIdLong { operand_0, .. } => *operand_0 == obj_register,
            
            // PutByVal where the object is operand_0
            PutByVal { operand_0, .. } => *operand_0 == obj_register,
            
            _ => false,
        }
    }

    /// Check if a specific use of the object causes it to escape
    fn check_use_for_escape(&self, use_site: &RegisterUse, result: &mut EscapeAnalysisResult) {
        // Get the instruction at the use site
        let block = &self.cfg.graph()[use_site.block_id];
        
        // Find the instruction at the specified index
        if let Some(instr_info) = block.instructions().iter()
            .find(|i| i.instruction_index == use_site.instruction_idx) {
            
            // Determine which operand position the register is used in
            let operand_position = Self::find_operand_position(&instr_info.instruction, use_site.register);
            
            if let Some(pos) = operand_position {
                if let Some(reason) = Self::instruction_causes_escape(&instr_info.instruction, pos) {
                    result.add_escape(reason, use_site.instruction_idx);
                }
            }
        }
    }
    
    /// Find which operand position a register is used in an instruction
    fn find_operand_position(instruction: &UnifiedInstruction, register: u8) -> Option<usize> {
        use UnifiedInstruction::*;
        
        match instruction {
            Ret { operand_0 } if *operand_0 == register => Some(0),
            Throw { operand_0 } if *operand_0 == register => Some(0),
            
            // For PutById variants, check if register is the value (operand_1)
            PutById { operand_0, operand_1, .. } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else { None }
            }
            PutByIdLong { operand_0, operand_1, .. } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else { None }
            }
            PutNewOwnById { operand_0, operand_1, .. } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else { None }
            }
            PutNewOwnByIdLong { operand_0, operand_1, .. } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else { None }
            }
            PutNewOwnByIdShort { operand_0, operand_1, .. } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else { None }
            }
            
            // For PutByVal, check all operands
            PutByVal { operand_0, operand_1, operand_2 } => {
                if *operand_0 == register { Some(0) }
                else if *operand_1 == register { Some(1) }
                else if *operand_2 == register { Some(2) }
                else { None }
            }
            
            // For calls, need to check which argument position
            Call { operand_1, operand_2, .. } => {
                if *operand_1 == register { Some(1) }  // Callee
                else if *operand_2 == register { Some(2) }  // First argument
                else { None }
            }
            
            // Handle Call1, Call2, Call3, Call4 variants
            Call1 { operand_1, operand_2, .. } => {
                if *operand_1 == register { Some(1) }  // Callee
                else if *operand_2 == register { Some(2) }  // First argument
                else { None }
            }
            Call2 { operand_1, operand_2, operand_3, .. } => {
                if *operand_1 == register { Some(1) }  // Callee
                else if *operand_2 == register { Some(2) }  // This (first argument)
                else if *operand_3 == register { Some(3) }  // Second argument
                else { None }
            }
            Call3 { operand_1, operand_2, operand_3, operand_4, .. } => {
                if *operand_1 == register { Some(1) }  // Callee
                else if *operand_2 == register { Some(2) }  // This
                else if *operand_3 == register { Some(3) }  // First argument
                else if *operand_4 == register { Some(4) }  // Second argument
                else { None }
            }
            Call4 { operand_1, operand_2, operand_3, operand_4, operand_5, .. } => {
                if *operand_1 == register { Some(1) }  // Callee
                else if *operand_2 == register { Some(2) }  // This
                else if *operand_3 == register { Some(3) }  // First argument
                else if *operand_4 == register { Some(4) }  // Second argument
                else if *operand_5 == register { Some(5) }  // Third argument
                else { None }
            }
            
            _ => None
        }
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
            Call { .. } | CallLong { .. } | 
            Call1 { .. } | Call2 { .. } | Call3 { .. } | Call4 { .. } |
            Construct { .. } | ConstructLong { .. } 
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