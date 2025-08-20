//! Control Flow Plan Analyzer
//!
//! Analyzes SSA value usage within the control flow plan to determine
//! declaration and use strategies for each value in each context.

use super::control_flow_plan::{ControlFlowPlan, ControlFlowKind, StructureId};
use crate::analysis::FunctionAnalysis;
use crate::analysis::ssa_usage_tracker::{
    DeclarationStrategy, SSAUsageTracker,
};
use crate::cfg::ssa::types::{DuplicatedSSAValue, RegisterUse, DuplicationContext};
use crate::generated::unified_instructions::UnifiedInstruction;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Analyzes a control flow plan to determine SSA value strategies
pub struct ControlFlowPlanAnalyzer<'a> {
    plan: &'a mut ControlFlowPlan,
    function_analysis: &'a FunctionAnalysis<'a>,
    usage_tracker: SSAUsageTracker<'a>,
}

impl<'a> ControlFlowPlanAnalyzer<'a> {
    /// Create a new analyzer
    pub fn new(
        plan: &'a mut ControlFlowPlan,
        function_analysis: &'a FunctionAnalysis<'a>,
    ) -> Self {
        let usage_tracker = SSAUsageTracker::new(function_analysis);
        Self {
            plan,
            function_analysis,
            usage_tracker,
        }
    }
    
    /// Analyze the control flow plan and compute strategies
    pub fn analyze(mut self) {
        // First pass: Mark consumed uses (e.g., switch discriminators, inlined constants)
        self.mark_consumed_uses();
        
        // Second pass: Compute declaration strategies for each structure
        self.compute_declaration_strategies();
        
        // Third pass: Compute use strategies for each use site
        self.compute_use_strategies();
        
        // Store the computed strategies in the plan
        self.store_strategies();
    }
    
    /// Mark uses that will be consumed/inlined
    fn mark_consumed_uses(&mut self) {
        self.mark_consumed_uses_in_structure(self.plan.root);
    }
    
    /// Recursively mark consumed uses in a structure
    fn mark_consumed_uses_in_structure(&mut self, structure_id: StructureId) {
        let structure = match self.plan.get_structure(structure_id) {
            Some(s) => s.clone(),
            None => return,
        };
        
        match &structure.kind {
            ControlFlowKind::Switch { info, case_groups, default_case, .. } => {
                // Mark discriminator uses as consumed (they're inlined in the switch)
                // For sparse switches, the comparison uses are inlined
                for case in &info.cases {
                    // Find the comparison instruction in this case's comparison block
                    let block = &self.function_analysis.cfg.graph()[case.comparison_block];
                    for instr in block.instructions() {
                        // Check if this is a comparison instruction
                        if matches!(
                            &instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                            | UnifiedInstruction::JStrictEqualLong { .. }
                            | UnifiedInstruction::JStrictNotEqual { .. }
                            | UnifiedInstruction::JStrictNotEqualLong { .. }
                        ) {
                            // Find the SSA value being compared
                            let discriminator_reg = info.discriminator;
                            if let Some(ssa_value) = self.find_ssa_value_for_register(
                                discriminator_reg,
                                case.comparison_block,
                            ) {
                                // Create the use site for this comparison
                                let use_site = RegisterUse {
                                    register: discriminator_reg,
                                    block_id: case.comparison_block,
                                    instruction_idx: instr.instruction_index,
                                };
                                self.usage_tracker.mark_use_consumed(&ssa_value, &use_site);
                            }
                            break; // Found the comparison instruction
                        }
                    }
                }
                
                // Recursively analyze case bodies
                for group in case_groups {
                    self.mark_consumed_uses_in_structure(group.body);
                }
                
                // Analyze default case
                if let Some(default_id) = default_case {
                    self.mark_consumed_uses_in_structure(*default_id);
                }
            }
            
            ControlFlowKind::Conditional { true_branch, false_branch, .. } => {
                // Recursively analyze branches
                self.mark_consumed_uses_in_structure(*true_branch);
                if let Some(false_id) = false_branch {
                    self.mark_consumed_uses_in_structure(*false_id);
                }
            }
            
            ControlFlowKind::Loop { body, update, break_target, continue_target, .. } => {
                // Recursively analyze loop components
                self.mark_consumed_uses_in_structure(*body);
                if let Some(update_id) = update {
                    self.mark_consumed_uses_in_structure(*update_id);
                }
                if let Some(break_id) = break_target {
                    self.mark_consumed_uses_in_structure(*break_id);
                }
                if let Some(continue_id) = continue_target {
                    self.mark_consumed_uses_in_structure(*continue_id);
                }
            }
            
            ControlFlowKind::Sequential { elements } => {
                // Analyze nested structures
                for element in elements {
                    if let super::control_flow_plan::SequentialElement::Structure(id) = element {
                        self.mark_consumed_uses_in_structure(*id);
                    }
                }
            }
            
            ControlFlowKind::TryCatch { try_body, catch_clause, finally_body } => {
                self.mark_consumed_uses_in_structure(*try_body);
                if let Some(catch) = catch_clause {
                    self.mark_consumed_uses_in_structure(catch.body);
                }
                if let Some(finally_id) = finally_body {
                    self.mark_consumed_uses_in_structure(*finally_id);
                }
            }
            
            _ => {}
        }
    }
    
    /// Find the SSA value for a register at a specific block
    fn find_ssa_value_for_register(
        &self,
        register: u8,
        block: NodeIndex,
    ) -> Option<crate::cfg::ssa::SSAValue> {
        // Look up the SSA value that's live for this register at this block
        self.function_analysis.ssa.find_live_value(register, block)
    }
    
    /// Collect all blocks that will be duplicated in the control flow plan
    fn collect_duplicated_blocks(
        &self,
        structure_id: StructureId,
        duplicated_blocks: &mut HashMap<NodeIndex, Vec<DuplicationContext>>,
    ) {
        let structure = match self.plan.get_structure(structure_id) {
            Some(s) => s.clone(),
            None => return,
        };
        
        match &structure.kind {
            ControlFlowKind::Switch { case_groups, default_case, .. } => {
                // Check each case group for fallthrough duplications
                for group in case_groups {
                    if let Some(ref fallthrough) = group.fallthrough {
                        // These blocks will be duplicated for this fallthrough
                        for &block in &fallthrough.blocks_to_duplicate {
                            duplicated_blocks
                                .entry(block)
                                .or_default()
                                .push(fallthrough.duplication_context.clone());
                        }
                    }
                    
                    // Recursively check the case body
                    self.collect_duplicated_blocks(group.body, duplicated_blocks);
                }
                
                // Check default case
                if let Some(default_id) = default_case {
                    self.collect_duplicated_blocks(*default_id, duplicated_blocks);
                }
            }
            
            ControlFlowKind::Sequential { elements } => {
                for element in elements {
                    if let super::control_flow_plan::SequentialElement::Structure(id) = element {
                        self.collect_duplicated_blocks(*id, duplicated_blocks);
                    }
                }
            }
            
            ControlFlowKind::Conditional { true_branch, false_branch, .. } => {
                self.collect_duplicated_blocks(*true_branch, duplicated_blocks);
                if let Some(false_id) = false_branch {
                    self.collect_duplicated_blocks(*false_id, duplicated_blocks);
                }
            }
            
            ControlFlowKind::Loop { body, update, break_target, continue_target, .. } => {
                self.collect_duplicated_blocks(*body, duplicated_blocks);
                if let Some(update_id) = update {
                    self.collect_duplicated_blocks(*update_id, duplicated_blocks);
                }
                if let Some(break_id) = break_target {
                    self.collect_duplicated_blocks(*break_id, duplicated_blocks);
                }
                if let Some(continue_id) = continue_target {
                    self.collect_duplicated_blocks(*continue_id, duplicated_blocks);
                }
            }
            
            ControlFlowKind::TryCatch { try_body, catch_clause, finally_body } => {
                self.collect_duplicated_blocks(*try_body, duplicated_blocks);
                if let Some(catch) = catch_clause {
                    self.collect_duplicated_blocks(catch.body, duplicated_blocks);
                }
                if let Some(finally_id) = finally_body {
                    self.collect_duplicated_blocks(*finally_id, duplicated_blocks);
                }
            }
            
            _ => {}
        }
    }
    
    /// Compute declaration strategies for all SSA values
    fn compute_declaration_strategies(&mut self) {
        // Track which SSA values need declarations and where
        let mut declaration_points: HashMap<NodeIndex, Vec<DuplicatedSSAValue>> = HashMap::new();
        
        // First, collect all blocks that will be duplicated
        let mut duplicated_blocks: HashMap<NodeIndex, Vec<DuplicationContext>> = HashMap::new();
        self.collect_duplicated_blocks(self.plan.root, &mut duplicated_blocks);
        
        // Analyze each SSA value
        for ssa_value in self.function_analysis.ssa.all_values() {
            // Always analyze the original version
            let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
            let strategy = self.usage_tracker.get_declaration_strategy(&dup_value);
            
            match &strategy {
                DeclarationStrategy::DeclareAtDominator { dominator_block, .. } => {
                    // Only DeclareAtDominator needs a separate declaration point
                    declaration_points
                        .entry(*dominator_block)
                        .or_default()
                        .push(dup_value.clone());
                }
                DeclarationStrategy::DeclareAndInitialize { .. } => {
                    // DeclareAndInitialize means the variable is declared inline
                    // with its initialization, so no separate declaration point needed
                }
                _ => {}
            }
            
            // Store the strategy in the plan
            self.plan.set_declaration_strategy(dup_value, strategy);
            
            // If this SSA value is defined in a block that will be duplicated,
            // also analyze the duplicated versions
            if let Some(contexts) = duplicated_blocks.get(&ssa_value.def_site.block_id) {
                for context in contexts {
                    let dup_value = DuplicatedSSAValue {
                        original: ssa_value.clone(),
                        duplication_context: Some(context.clone()),
                    };
                    
                    let dup_strategy = self.usage_tracker.get_declaration_strategy(&dup_value);
                    
                    // Store the strategy for the duplicated value
                    self.plan.set_declaration_strategy(dup_value, dup_strategy);
                }
            }
        }
        
        // Store declaration points in the plan
        for (block, values) in declaration_points {
            for value in values {
                self.plan.add_block_declaration(block, value);
            }
        }
    }
    
    /// Compute use strategies for all use sites
    fn compute_use_strategies(&mut self) {
        // For each SSA value, determine strategy at each use site
        for ssa_value in self.function_analysis.ssa.all_values() {
            let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
            
            for use_site in self.function_analysis.ssa.get_ssa_value_uses(&ssa_value) {
                let strategy = self.usage_tracker.get_use_strategy(&dup_value, use_site);
                self.plan.set_use_strategy(dup_value.clone(), use_site.clone(), strategy);
            }
        }
    }
    
    /// Store the computed strategies in the plan
    fn store_strategies(&mut self) {
        // The strategies have already been stored during computation
        // This method could be used for any final processing or validation
    }
}

/// Extension methods for SSAAnalysis
impl crate::cfg::ssa::SSAAnalysis {
    /// Get all SSA values
    pub fn all_values(&self) -> Vec<crate::cfg::ssa::SSAValue> {
        self.ssa_values.values().cloned().collect()
    }
    
    /// Find the SSA value that's live for a register at a block
    pub fn find_live_value(
        &self,
        register: u8,
        block: NodeIndex,
    ) -> Option<crate::cfg::ssa::SSAValue> {
        // Look for the most recent definition that dominates this block
        for value in self.ssa_values.values() {
            if value.register == register {
                // Check if this definition dominates the use block
                // For now, simple check: definition is in same or earlier block
                if value.def_site.block_id.index() <= block.index() {
                    return Some(value.clone());
                }
            }
        }
        None
    }
}