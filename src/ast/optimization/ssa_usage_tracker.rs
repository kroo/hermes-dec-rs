//! SSA Usage Tracker
//!
//! Tracks which uses of SSA values have been consumed (inlined) during AST generation,
//! enabling smart decisions about variable declaration and constant folding.

use crate::analysis::{ConstantValue, FunctionAnalysis, TrackedValue};
use crate::cfg::ssa::types::{RegisterUse, SSAValue, SSAAnalysis};
use std::collections::{HashMap, HashSet};

/// Tracks the usage status of SSA values during AST generation
pub struct SSAUsageTracker<'a> {
    /// The function analysis containing SSA and CFG information
    function_analysis: &'a FunctionAnalysis<'a>,
    
    /// Track which specific uses of an SSA value have been consumed (inlined)
    consumed_uses: HashMap<SSAValue, HashSet<RegisterUse>>,
    
    /// Cache of constant values for SSA values
    /// This is populated when we first analyze an SSA value
    constant_cache: HashMap<SSAValue, ConstantValue>,
    
    /// Track SSA values that have been declared as variables
    declared_values: HashSet<SSAValue>,
}

/// The usage status of an SSA value
#[derive(Debug, Clone, PartialEq)]
pub enum UsageStatus {
    /// All uses have been consumed/inlined
    FullyConsumed,
    
    /// Some uses consumed, others remain
    PartiallyConsumed {
        consumed_count: usize,
        remaining_count: usize,
    },
    
    /// No uses have been consumed yet
    Unconsumed {
        total_uses: usize,
    },
}

/// Strategy for declaring/using an SSA value
#[derive(Debug, Clone)]
pub enum DeclarationStrategy {
    /// Don't declare - all uses have been inlined
    FullyEliminated,
    
    /// Declare as const with the literal value
    DeclareAsConst(ConstantValue),
    
    /// Normal variable declaration (non-constant or complex value)
    DeclareAsVariable,
    
    /// Inline the constant value at this use site
    InlineConstant(ConstantValue),
    
    /// Reference existing variable
    UseVariable,
}

impl<'a> SSAUsageTracker<'a> {
    /// Create a new SSA usage tracker with function analysis
    pub fn new(function_analysis: &'a FunctionAnalysis<'a>) -> Self {
        Self {
            function_analysis,
            consumed_uses: HashMap::new(),
            constant_cache: HashMap::new(),
            declared_values: HashSet::new(),
        }
    }
    
    /// Get the SSA analysis
    fn ssa(&self) -> &SSAAnalysis {
        &self.function_analysis.ssa
    }
    
    /// Get all uses of an SSA value
    fn get_all_uses(&self, ssa_value: &SSAValue) -> Vec<RegisterUse> {
        self.ssa().get_ssa_value_uses(ssa_value).into_iter().cloned().collect()
    }
    
    /// Mark a specific use of an SSA value as consumed (inlined)
    pub fn mark_use_consumed(&mut self, ssa_value: &SSAValue, use_site: &RegisterUse) {
        log::debug!(
            "Marking use consumed: SSA {} at block {} instruction {}",
            ssa_value.name(),
            use_site.block_id.index(),
            use_site.instruction_idx.value()
        );
        
        self.consumed_uses
            .entry(ssa_value.clone())
            .or_default()
            .insert(use_site.clone());
    }
    
    /// Mark all uses in a list as consumed
    pub fn mark_uses_consumed(&mut self, ssa_value: &SSAValue, use_sites: &[RegisterUse]) {
        for use_site in use_sites {
            self.mark_use_consumed(ssa_value, use_site);
        }
    }
    
    /// Check if a specific use has been consumed
    pub fn is_use_consumed(&self, ssa_value: &SSAValue, use_site: &RegisterUse) -> bool {
        self.consumed_uses
            .get(ssa_value)
            .map(|uses| uses.contains(use_site))
            .unwrap_or(false)
    }
    
    /// Get the usage status of an SSA value
    pub fn get_usage_status(&self, ssa_value: &SSAValue) -> UsageStatus {
        let all_uses = self.get_all_uses(ssa_value);
        let consumed_count = self.consumed_uses
            .get(ssa_value)
            .map(|uses| uses.len())
            .unwrap_or(0);
        
        let total_uses = all_uses.len();
        
        if consumed_count == 0 {
            UsageStatus::Unconsumed { total_uses }
        } else if consumed_count == total_uses {
            UsageStatus::FullyConsumed
        } else {
            UsageStatus::PartiallyConsumed {
                consumed_count,
                remaining_count: total_uses - consumed_count,
            }
        }
    }
    
    /// Get remaining (non-consumed) uses of an SSA value
    pub fn get_remaining_uses(&self, ssa_value: &SSAValue) -> Vec<RegisterUse> {
        let all_uses = self.get_all_uses(ssa_value);
        let consumed = self.consumed_uses.get(ssa_value);
        
        all_uses
            .into_iter()
            .filter(|use_site| {
                consumed
                    .map(|set| !set.contains(use_site))
                    .unwrap_or(true)
            })
            .collect()
    }
    
    /// Cache a constant value for an SSA value
    pub fn cache_constant_value(&mut self, ssa_value: &SSAValue, value: ConstantValue) {
        self.constant_cache.insert(ssa_value.clone(), value);
    }
    
    /// Get cached constant value
    pub fn get_constant_value(&self, ssa_value: &SSAValue) -> Option<&ConstantValue> {
        self.constant_cache.get(ssa_value)
    }
    
    /// Check if an SSA value has been fully eliminated (all uses consumed)
    /// This is a derived property based on consumed uses
    pub fn is_fully_eliminated(&self, ssa_value: &SSAValue) -> bool {
        let all_uses = self.get_all_uses(ssa_value);
        
        if all_uses.is_empty() {
            // No uses means it's effectively eliminated
            return true;
        }
        
        // Check if all uses have been consumed
        if let Some(consumed) = self.consumed_uses.get(ssa_value) {
            all_uses.iter().all(|use_site| consumed.contains(use_site))
        } else {
            // No consumed uses tracked, so not eliminated
            false
        }
    }
    
    /// Mark an SSA value as declared
    pub fn mark_declared(&mut self, ssa_value: &SSAValue) {
        self.declared_values.insert(ssa_value.clone());
    }
    
    /// Check if an SSA value has been declared
    pub fn is_declared(&self, ssa_value: &SSAValue) -> bool {
        self.declared_values.contains(ssa_value)
    }
    
    /// Determine the declaration strategy for an SSA value at a specific use site
    /// 
    /// This is called when we need to use an SSA value and must decide whether to:
    /// - Declare it as a variable/const
    /// - Inline its constant value
    /// - Reference an existing variable
    pub fn get_declaration_strategy(
        &self,
        ssa_value: &SSAValue,
        current_use: Option<&RegisterUse>,
    ) -> DeclarationStrategy {
        // If fully eliminated, we shouldn't be asking for a declaration strategy
        if self.is_fully_eliminated(ssa_value) {
            // But if we are, it means we missed a use - panic to catch the bug
            panic!(
                "Requesting declaration strategy for fully eliminated SSA value {}. This indicates a bug - all uses should have been consumed.",
                ssa_value.name()
            );
        }
        
        // If already declared, just use the variable
        if self.is_declared(ssa_value) {
            return DeclarationStrategy::UseVariable;
        }
        
        // Check if this is a constant value using our function analysis
        let value_tracker = self.function_analysis.value_tracker();
        let tracked_value = value_tracker.get_value(ssa_value);
        let constant_value = match tracked_value {
            TrackedValue::Constant(c) => Some(c),
            _ => None,
        };
        
        // Note: We can't cache the constant value here because this method takes &self
        // Caching should be done elsewhere with mutable access
        
        // Get usage status
        let usage_status = self.get_usage_status(ssa_value);
        
        match usage_status {
            UsageStatus::FullyConsumed => {
                // All uses consumed - should have been eliminated
                DeclarationStrategy::FullyEliminated
            }
            
            UsageStatus::PartiallyConsumed { remaining_count, .. } => {
                // Some uses remain
                if let Some(const_val) = constant_value {
                    if remaining_count == 1 && current_use.is_some() {
                        // Single remaining use at current site - inline it
                        DeclarationStrategy::InlineConstant(const_val)
                    } else {
                        // Multiple remaining uses - declare as const
                        DeclarationStrategy::DeclareAsConst(const_val)
                    }
                } else {
                    // Non-constant - must declare as variable
                    DeclarationStrategy::DeclareAsVariable
                }
            }
            
            UsageStatus::Unconsumed { total_uses } => {
                // No uses consumed yet
                if let Some(const_val) = constant_value {
                    if total_uses == 1 && current_use.is_some() {
                        // Single use of a constant - inline it
                        DeclarationStrategy::InlineConstant(const_val)
                    } else {
                        // Multiple uses - declare as const
                        DeclarationStrategy::DeclareAsConst(const_val)
                    }
                } else {
                    // Non-constant - declare as variable
                    DeclarationStrategy::DeclareAsVariable
                }
            }
        }
    }
    
    /// Update tracking after a switch converter has processed cases
    /// 
    /// The switch converter should call this to report which SSA values
    /// had their comparison uses inlined into the switch cases
    pub fn report_switch_inlined_uses(
        &mut self,
        inlined_comparisons: Vec<(SSAValue, Vec<RegisterUse>)>,
    ) {
        for (ssa_value, uses) in inlined_comparisons {
            self.mark_uses_consumed(&ssa_value, &uses);
        }
        
        // After marking uses as consumed, perform cascading elimination
        self.perform_cascading_elimination();
    }
    
    /// Perform cascading elimination optimization
    /// 
    /// When an SSA value becomes fully consumed, check if it was the only use of another value.
    /// If so, that value can also be eliminated. This process cascades transitively.
    pub fn perform_cascading_elimination(&mut self) {
        let mut changed = true;
        let max_iterations = 10; // Prevent infinite loops
        let mut iterations = 0;
        
        while changed && iterations < max_iterations {
            changed = false;
            iterations += 1;
            
            // Collect SSA values that are now fully eliminated
            let mut newly_eliminated = Vec::new();
            
            // Check all SSA values to see if they've become fully eliminated
            for ssa_value in self.ssa().all_ssa_values() {
                if !self.is_fully_eliminated(ssa_value) {
                    continue;
                }
                
                // This value is fully eliminated - check what it was defined by
                if let Some(def_instruction) = self.ssa().get_defining_instruction(ssa_value) {
                    let block_id = def_instruction.block_id;
                    let instr_idx = def_instruction.instruction_idx;
                    
                    // Get the instruction
                    if let Some(block) = self.function_analysis.cfg.graph().node_weight(block_id) {
                        if let Some(instr) = block.instructions().get(instr_idx.value()) {
                            // TODO: Check if this instruction is side-effect free and can be eliminated
                            // For now, treat all instructions as safe to eliminate when their results are unused
                            // In the future, we should check for instructions with side effects like:
                            // - Function calls
                            // - Property stores
                            // - Throw instructions
                            // - etc.
                            
                            // Check what SSA values this instruction uses
                            let usage = crate::generated::instruction_analysis::analyze_register_usage(
                                &instr.instruction
                            );
                            
                            // For each source register used by this instruction
                            for &source_reg in &usage.sources {
                                if let Some(source_ssa) = self.ssa().get_value_before_instruction(
                                    source_reg,
                                    instr_idx
                                ) {
                                    // Check if this was the only remaining use of the source SSA value
                                    let source_uses = self.get_remaining_uses(source_ssa);
                                    if source_uses.len() == 1 && 
                                       source_uses[0].block_id == block_id &&
                                       source_uses[0].instruction_idx == instr_idx {
                                        // This was the only use - mark it as consumed
                                        newly_eliminated.push((source_ssa.clone(), source_uses));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            // Mark newly eliminated values as consumed
            for (ssa_value, uses) in newly_eliminated {
                log::debug!(
                    "Cascading elimination: marking SSA {} as fully consumed (was only used by eliminated values)",
                    ssa_value.name()
                );
                self.mark_uses_consumed(&ssa_value, &uses);
                changed = true;
            }
        }
        
        if iterations >= max_iterations {
            log::warn!("Cascading elimination reached maximum iterations - possible cycle");
        }
    }
}