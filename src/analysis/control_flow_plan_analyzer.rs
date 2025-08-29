//! Control Flow Plan Analyzer
//!
//! Analyzes SSA value usage within the control flow plan to determine
//! declaration and use strategies for each value in each context.

use super::control_flow_plan::{
    ControlFlowKind, ControlFlowPlan, ControlFlowStructure, PhiDeconstructionInfo, StructureId,
    UpdatedPhiFunction,
};
use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, SSAUsageTracker};
use crate::analysis::FunctionAnalysis;
use crate::cfg::ssa::types::{DuplicatedSSAValue, DuplicationContext, RegisterUse};
use crate::cfg::ssa::{PhiFunction, SSAValue};
use crate::generated::unified_instructions::UnifiedInstruction;
use log::debug;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Information about a block with PHI functions that needs deconstruction
#[derive(Debug, Clone)]
struct PhiAffectedInfo {
    /// PHI functions in this block
    phis: Vec<PhiFunction>,
    /// Duplication contexts where this block will be duplicated
    duplication_contexts: Vec<DuplicationContext>,
}

/// Analyzes a control flow plan to determine SSA value strategies
pub struct ControlFlowPlanAnalyzer<'a> {
    plan: &'a mut ControlFlowPlan,
    function_analysis: &'a FunctionAnalysis<'a>,
    usage_tracker: SSAUsageTracker<'a>,
    /// Blocks that will be duplicated in various contexts
    duplicated_blocks: HashMap<NodeIndex, Vec<DuplicationContext>>,
}

impl<'a> ControlFlowPlanAnalyzer<'a> {
    /// Create a new analyzer
    pub fn new(plan: &'a mut ControlFlowPlan, function_analysis: &'a FunctionAnalysis<'a>) -> Self {
        let usage_tracker = SSAUsageTracker::new(function_analysis);
        // Note: We'll set the call site analysis from the plan later during analysis
        // to avoid borrowing issues
        Self {
            plan,
            function_analysis,
            usage_tracker,
            duplicated_blocks: HashMap::new(),
        }
    }

    /// Collect all PHI results from the SSA analysis
    fn collect_phi_results(&mut self) {
        // Iterate through all PHI functions and collect their result SSA values
        for phi_list in self.function_analysis.ssa.phi_functions.values() {
            for phi in phi_list {
                self.plan.phi_results.insert(phi.result.clone());
            }
        }
    }

    /// Analyze the control flow plan and compute strategies
    pub fn analyze(mut self) {
        let start_time = std::time::Instant::now();

        // Collect PHI results from SSA analysis
        log::debug!("Collecting PHI results...");
        self.collect_phi_results();

        // First pass: Collect duplicated blocks
        log::debug!("Collecting duplicated blocks...");
        self.collect_duplicated_blocks(self.plan.root);

        // Second pass: Mark consumed uses (e.g., switch discriminators, inlined constants)
        log::debug!("Marking consumed uses...");
        self.mark_consumed_uses();

        // Perform cascading elimination with duplication context awareness
        self.usage_tracker
            .perform_cascading_elimination(&self.duplicated_blocks);

        // Transfer consumed uses from tracker to plan before reusing the tracker
        let consumed_uses = self.usage_tracker.get_consumed_uses();
        log::info!(
            "Transferring {} consumed uses from tracker to plan",
            consumed_uses.len()
        );

        let transfer_start = std::time::Instant::now();
        // Only log details at trace level to avoid overwhelming debug output
        for (dup_ssa, uses) in consumed_uses {
            log::trace!(
                "  {} (context: {}) has {} consumed uses",
                dup_ssa.original_ssa_value().name(),
                dup_ssa.context_description(),
                uses.len()
            );
            for use_site in uses {
                log::trace!(
                    "    - Block {}, Instruction {}",
                    use_site.block_id.index(),
                    use_site.instruction_idx.value()
                );
            }
        }
        self.plan
            .consumed_uses
            .extend(self.usage_tracker.get_consumed_uses().clone());
        log::debug!("Transfer completed in {:?}", transfer_start.elapsed());

        // Third pass: Compute declaration strategies for each structure
        log::debug!("Computing declaration strategies...");
        let strategies_start = std::time::Instant::now();
        self.compute_declaration_strategies();
        log::debug!(
            "Declaration strategies computed in {:?}",
            strategies_start.elapsed()
        );

        // Fourth pass: Analyze PHI deconstruction for duplicated blocks
        log::debug!("Analyzing PHI deconstruction...");
        self.analyze_phi_deconstruction();

        // Fifth pass: Update declaration strategies based on PHI deconstruction
        log::debug!("Updating strategies for PHI deconstruction...");
        self.update_strategies_for_phi_deconstruction();

        // Sixth pass: Compute use strategies for each use site
        log::debug!("Computing use strategies...");
        let use_strategies_start = std::time::Instant::now();
        self.compute_use_strategies();
        log::debug!(
            "Use strategies computed in {:?}",
            use_strategies_start.elapsed()
        );

        // Store the computed strategies in the plan
        log::debug!("Storing strategies...");
        self.store_strategies();

        let total_time = start_time.elapsed();
        log::info!("Control flow plan analysis completed in {:?}", total_time);
    }

    /// Mark uses that will be consumed/inlined
    fn mark_consumed_uses(&mut self) {
        self.mark_consumed_uses_in_structure(self.plan.root);
    }

    /// Recursively mark consumed uses in a structure
    fn mark_consumed_uses_in_structure(&mut self, structure_id: StructureId) {
        self.mark_consumed_uses_in_structure_with_context(structure_id, None);
    }

    fn mark_consumed_uses_in_structure_with_context(
        &mut self,
        structure_id: StructureId,
        context: Option<&DuplicationContext>,
    ) {
        debug!(
            "mark_consumed_uses_in_structure called for structure {} with context {:?}",
            structure_id.0, context
        );
        let structure = match self.plan.get_structure(structure_id) {
            Some(s) => s.clone(),
            None => return,
        };

        // If the structure itself has duplication info, use that context
        let effective_context = if let Some(ref dup_info) = structure.duplication_info {
            Some(&dup_info.context)
        } else {
            context
        };

        match &structure.kind {
            ControlFlowKind::Switch {
                info,
                case_groups,
                default_case,
                ..
            } => {
                // Mark discriminator uses as consumed (they're inlined in the switch)
                // For sparse switches, the comparison uses are inlined
                debug!(
                    "Processing {} cases for switch in structure {}",
                    info.cases.len(),
                    structure_id.0
                );
                for case in &info.cases {
                    debug!(
                        "Processing case with comparison block {}",
                        case.comparison_block.index()
                    );
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
                            // Use the generated instruction analysis to get all register operands
                            let usage =
                                crate::generated::instruction_analysis::analyze_register_usage(
                                    &instr.instruction,
                                );

                            // For sparse switch comparisons, we only mark the case value register as consumed,
                            // not the discriminator. The discriminator should remain as a variable reference.
                            // We can identify the case value register as the one that contains a constant.
                            for register in usage.sources {
                                // Find the SSA value being used at this specific instruction
                                let use_site = RegisterUse {
                                    register,
                                    block_id: case.comparison_block,
                                    instruction_idx: instr.instruction_index,
                                };

                                // Look up the SSA value from the use-def chain
                                let ssa_value = if let Some(def_site) =
                                    self.function_analysis.ssa.use_def_chains.get(&use_site)
                                {
                                    // Find the SSA value with this def site
                                    self.function_analysis
                                        .ssa
                                        .ssa_values
                                        .values()
                                        .find(|v| &v.def_site == def_site)
                                        .cloned()
                                } else {
                                    None
                                };

                                if let Some(ssa_value) = ssa_value {
                                    // Check if this register contains a constant value (case value)
                                    // The discriminator will be a parameter or variable, not a constant
                                    let value_tracker = self.function_analysis.value_tracker();
                                    let tracked_value = value_tracker.get_value(&ssa_value);
                                    let is_case_value =
                                        crate::analysis::value_tracker::ValueTracker::is_constant(
                                            &tracked_value,
                                        );

                                    debug!(
                                        "Block {}, Register r{} -> SSA {} (constant: {})",
                                        case.comparison_block.index(),
                                        register,
                                        ssa_value,
                                        is_case_value
                                    );

                                    if is_case_value {
                                        debug!(
                                            "Marking {} as consumed at Block {}, Instruction {} (context: {:?})",
                                            ssa_value,
                                            use_site.block_id.index(),
                                            use_site.instruction_idx.value(),
                                            effective_context
                                        );
                                        // Create the appropriate DuplicatedSSAValue based on context
                                        let dup_value = if let Some(ctx) = effective_context {
                                            DuplicatedSSAValue {
                                                original: ssa_value.clone(),
                                                duplication_context: Some(ctx.clone()),
                                            }
                                        } else {
                                            DuplicatedSSAValue::original(ssa_value.clone())
                                        };
                                        // Mark in both the tracker and the plan
                                        self.usage_tracker
                                            .mark_duplicated_use_consumed(&dup_value, &use_site);
                                        self.plan.mark_use_consumed(dup_value, use_site.clone());
                                    }
                                }
                            }
                            break; // Found the comparison instruction
                        }
                    }
                }

                // Recursively analyze case bodies
                for group in case_groups {
                    // Check if this case group has fallthrough duplication
                    let case_context = if let Some(ref fallthrough) = group.fallthrough {
                        Some(&fallthrough.duplication_context)
                    } else {
                        effective_context
                    };
                    self.mark_consumed_uses_in_structure_with_context(group.body, case_context);
                }

                // Analyze default case
                if let Some(default_id) = default_case {
                    self.mark_consumed_uses_in_structure_with_context(
                        *default_id,
                        effective_context,
                    );
                }
            }

            ControlFlowKind::Conditional {
                true_branch,
                false_branch,
                ..
            } => {
                // Recursively analyze branches with context
                self.mark_consumed_uses_in_structure_with_context(*true_branch, effective_context);
                if let Some(false_id) = false_branch {
                    self.mark_consumed_uses_in_structure_with_context(*false_id, effective_context);
                }
            }

            ControlFlowKind::Loop {
                body,
                update,
                break_target,
                continue_target,
                ..
            } => {
                // Recursively analyze loop components with context
                self.mark_consumed_uses_in_structure_with_context(*body, effective_context);
                if let Some(update_id) = update {
                    self.mark_consumed_uses_in_structure_with_context(
                        *update_id,
                        effective_context,
                    );
                }
                if let Some(break_id) = break_target {
                    self.mark_consumed_uses_in_structure_with_context(*break_id, effective_context);
                }
                if let Some(continue_id) = continue_target {
                    self.mark_consumed_uses_in_structure_with_context(
                        *continue_id,
                        effective_context,
                    );
                }
            }

            ControlFlowKind::Sequential { elements } => {
                // Analyze nested structures with context
                for element in elements {
                    if let super::control_flow_plan::SequentialElement::Structure(id) = element {
                        self.mark_consumed_uses_in_structure_with_context(*id, effective_context);
                    }
                }
            }

            ControlFlowKind::TryCatch {
                try_body,
                catch_clause,
                finally_clause,
            } => {
                // Recursively analyze try-catch-finally with context
                self.mark_consumed_uses_in_structure_with_context(*try_body, effective_context);
                if let Some(catch) = catch_clause {
                    self.mark_consumed_uses_in_structure_with_context(
                        catch.body,
                        effective_context,
                    );
                }
                if let Some(finally) = finally_clause {
                    self.mark_consumed_uses_in_structure_with_context(
                        finally.body,
                        effective_context,
                    );
                }
            }

            ControlFlowKind::BasicBlock { block, .. } => {
                // Check if this basic block contains comparison instructions that use setup values
                // This can happen in default cases that have their own dispatch logic
                let cfg_block = &self.function_analysis.cfg.graph()[*block];
                for instr in cfg_block.instructions() {
                    // Check if this is a comparison instruction
                    if matches!(
                        &instr.instruction,
                        UnifiedInstruction::JStrictEqual { .. }
                            | UnifiedInstruction::JStrictEqualLong { .. }
                            | UnifiedInstruction::JStrictNotEqual { .. }
                            | UnifiedInstruction::JStrictNotEqualLong { .. }
                    ) {
                        // Get all register operands
                        let usage = crate::generated::instruction_analysis::analyze_register_usage(
                            &instr.instruction,
                        );

                        // Check if any of the source registers contain constants (setup values)
                        for register in usage.sources {
                            // Find the SSA value being used at this specific instruction
                            let use_site = RegisterUse {
                                register,
                                block_id: *block,
                                instruction_idx: instr.instruction_index,
                            };

                            // Look up the SSA value from the use-def chain
                            let ssa_value = if let Some(def_site) =
                                self.function_analysis.ssa.use_def_chains.get(&use_site)
                            {
                                // Find the SSA value with this def site
                                self.function_analysis
                                    .ssa
                                    .ssa_values
                                    .values()
                                    .find(|v| &v.def_site == def_site)
                                    .cloned()
                            } else {
                                None
                            };

                            if let Some(ssa_value) = ssa_value {
                                // Check if this register contains a constant value
                                let value_tracker = self.function_analysis.value_tracker();
                                let tracked_value = value_tracker.get_value(&ssa_value);
                                let is_constant =
                                    crate::analysis::value_tracker::ValueTracker::is_constant(
                                        &tracked_value,
                                    );

                                if is_constant {
                                    debug!("BasicBlock {} comparison uses constant {}, marking as consumed (context: {:?})",
                                           block.index(), ssa_value, effective_context);
                                    // Mark this constant use as consumed with appropriate context
                                    let dup_value = if let Some(ctx) = effective_context {
                                        DuplicatedSSAValue {
                                            original: ssa_value.clone(),
                                            duplication_context: Some(ctx.clone()),
                                        }
                                    } else {
                                        DuplicatedSSAValue::original(ssa_value.clone())
                                    };
                                    self.usage_tracker
                                        .mark_duplicated_use_consumed(&dup_value, &use_site);
                                    self.plan.mark_use_consumed(dup_value, use_site.clone());
                                }
                            }
                        }
                    }
                }
            }

            _ => {}
        }
    }

    /// Collect all blocks that will be duplicated in the control flow plan
    fn collect_duplicated_blocks(&mut self, structure_id: StructureId) {
        let mut temp_duplicated = HashMap::new();
        self.collect_duplicated_blocks_helper(structure_id, &mut temp_duplicated);
        self.duplicated_blocks = temp_duplicated;
    }

    /// Helper for collecting duplicated blocks
    fn collect_duplicated_blocks_helper(
        &self,
        structure_id: StructureId,
        duplicated_blocks: &mut HashMap<NodeIndex, Vec<DuplicationContext>>,
    ) {
        let structure = match self.plan.get_structure(structure_id) {
            Some(s) => s.clone(),
            None => return,
        };

        match &structure.kind {
            ControlFlowKind::Switch {
                case_groups,
                default_case,
                ..
            } => {
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
                    self.collect_duplicated_blocks_helper(group.body, duplicated_blocks);
                }

                // Check default case
                if let Some(default_id) = default_case {
                    self.collect_duplicated_blocks_helper(*default_id, duplicated_blocks);
                }
            }

            ControlFlowKind::Sequential { elements } => {
                for element in elements {
                    if let super::control_flow_plan::SequentialElement::Structure(id) = element {
                        self.collect_duplicated_blocks_helper(*id, duplicated_blocks);
                    }
                }
            }

            ControlFlowKind::Conditional {
                true_branch,
                false_branch,
                ..
            } => {
                self.collect_duplicated_blocks_helper(*true_branch, duplicated_blocks);
                if let Some(false_id) = false_branch {
                    self.collect_duplicated_blocks_helper(*false_id, duplicated_blocks);
                }
            }

            ControlFlowKind::Loop {
                body,
                update,
                break_target,
                continue_target,
                ..
            } => {
                self.collect_duplicated_blocks_helper(*body, duplicated_blocks);
                if let Some(update_id) = update {
                    self.collect_duplicated_blocks_helper(*update_id, duplicated_blocks);
                }
                if let Some(break_id) = break_target {
                    self.collect_duplicated_blocks_helper(*break_id, duplicated_blocks);
                }
                if let Some(continue_id) = continue_target {
                    self.collect_duplicated_blocks_helper(*continue_id, duplicated_blocks);
                }
            }

            ControlFlowKind::TryCatch {
                try_body,
                catch_clause,
                finally_clause,
            } => {
                self.collect_duplicated_blocks_helper(*try_body, duplicated_blocks);
                if let Some(catch) = catch_clause {
                    self.collect_duplicated_blocks_helper(catch.body, duplicated_blocks);
                }
                if let Some(finally) = finally_clause {
                    self.collect_duplicated_blocks_helper(finally.body, duplicated_blocks);
                }
            }

            _ => {}
        }
    }

    /// Analyze PHI deconstruction for all duplicated blocks
    fn analyze_phi_deconstruction(&mut self) {
        // Collect all blocks with PHI functions that need deconstruction
        let phi_affected = self.collect_phi_affected_blocks();

        // Process each affected block
        for (block_id, info) in phi_affected {
            // Process duplicated contexts
            for context in &info.duplication_contexts {
                let phi_info = self.compute_phi_replacements(block_id, context, &info.phis);
                self.plan
                    .set_phi_info(block_id, Some(context.clone()), phi_info);
            }

            // Update original block's PHIs if needed
            if !info.duplication_contexts.is_empty() {
                // Find which predecessors are being redirected to duplicated blocks
                let removed_predecessors =
                    self.find_removed_predecessors(block_id, &info.duplication_contexts);

                if !removed_predecessors.is_empty() {
                    let phi_info =
                        self.update_original_phis(block_id, &info.phis, &removed_predecessors);
                    self.plan.set_phi_info(block_id, None, phi_info);
                }
            }
        }
    }

    /// Collect blocks with PHI functions that will be affected by duplication
    fn collect_phi_affected_blocks(&self) -> HashMap<NodeIndex, PhiAffectedInfo> {
        let mut affected = HashMap::new();

        // For each block that will be duplicated
        for (block_id, contexts) in &self.duplicated_blocks {
            if let Some(phi_functions) = self.function_analysis.ssa.phi_functions.get(block_id) {
                if !phi_functions.is_empty() {
                    let info = PhiAffectedInfo {
                        phis: phi_functions.clone(),
                        duplication_contexts: contexts.clone(),
                    };
                    affected.insert(*block_id, info);
                }
            }
        }

        affected
    }

    /// Compute PHI replacements for a duplicated block
    fn compute_phi_replacements(
        &mut self,
        block: NodeIndex,
        context: &DuplicationContext,
        phi_functions: &[PhiFunction],
    ) -> PhiDeconstructionInfo {
        let mut replacements = HashMap::new();

        // Determine the predecessor for this duplication context
        let predecessor = self.determine_predecessor_for_context(block, context);
        // For each PHI in the block
        for phi in phi_functions {
            // Find the operand from this predecessor
            if let Some(operand) = phi.operands.get(&predecessor) {
                // Create replacement: phi_result → concrete_operand
                replacements.insert(phi.result.clone(), operand.clone());

                // The PHI result in the duplicated context is replaced by the concrete operand
                // The declaration strategy for the PHI result remains as-is (it describes where
                // the replacement value needs to be available)
            }
        }

        PhiDeconstructionInfo {
            replacements,
            updated_phis: vec![], // No updated PHIs for duplicated blocks
        }
    }

    /// Update PHI functions in original blocks (remove redirected predecessors)
    fn update_original_phis(
        &mut self,
        _block: NodeIndex,
        phi_functions: &[PhiFunction],
        removed_predecessors: &[NodeIndex],
    ) -> PhiDeconstructionInfo {
        let mut updated_phis = Vec::new();

        for phi in phi_functions {
            // Filter out removed predecessors
            let remaining_operands: Vec<(NodeIndex, SSAValue)> = phi
                .operands
                .iter()
                .filter(|(pred, _)| !removed_predecessors.contains(pred))
                .map(|(p, v)| (*p, v.clone()))
                .collect();

            if remaining_operands.len() > 1 {
                // Still need a PHI
                updated_phis.push(UpdatedPhiFunction {
                    result: phi.result.clone(),
                    operands: remaining_operands,
                });
            } else if remaining_operands.len() == 1 {
                // PHI degenerates to simple assignment
                // Mark the PHI result to use AssignOnly strategy
                let dup_result = DuplicatedSSAValue::original(phi.result.clone());
                self.plan
                    .set_declaration_strategy(dup_result, DeclarationStrategy::AssignOnly);
            }
            // If no operands remain, this is an error condition
        }

        PhiDeconstructionInfo {
            replacements: HashMap::new(), // No replacements for original blocks
            updated_phis,
        }
    }

    /// Determine the predecessor block for a given duplication context
    fn determine_predecessor_for_context(
        &self,
        duplicated_block: NodeIndex,
        context: &DuplicationContext,
    ) -> NodeIndex {
        match context {
            DuplicationContext::SwitchFallthrough {
                from_case_index, ..
            } => {
                // Find the block that implements the "from" case group
                // We need to look at the switch structure to find which block corresponds to case group from_case_index
                let predecessor = self.find_case_group_block(*from_case_index, duplicated_block);

                // If we can't find it systematically, fall back to checking predecessors
                if let Some(block) = predecessor {
                    return block;
                }

                // Fallback: get all predecessors and return the first one
                // This is imperfect but better than hardcoding
                self.function_analysis
                    .cfg
                    .graph()
                    .neighbors_directed(duplicated_block, petgraph::Direction::Incoming)
                    .next()
                    .unwrap_or(NodeIndex::new(0))
            }
            DuplicationContext::SwitchBlockDuplication { case_group_keys: _ } => {
                // Find which block leads to this duplication
                // This would need to look at which case groups are being duplicated
                self.function_analysis
                    .cfg
                    .graph()
                    .neighbors_directed(duplicated_block, petgraph::Direction::Incoming)
                    .next()
                    .unwrap_or(NodeIndex::new(0))
            }
        }
    }

    /// Find the block that implements a specific case group
    fn find_case_group_block(
        &self,
        case_index: usize,
        target_block: NodeIndex,
    ) -> Option<NodeIndex> {
        // Find the switch structure that contains the target block
        let switch_structure = self.find_switch_containing_block(target_block)?;

        if let ControlFlowKind::Switch { case_groups, .. } = &switch_structure.kind {
            // Get the case group at the specified index
            let case_group = case_groups.get(case_index)?;

            // Find the final block in this case group's body
            self.find_final_block_in_structure(case_group.body)
        } else {
            None
        }
    }

    /// Find the switch structure that contains a specific block
    fn find_switch_containing_block(
        &self,
        target_block: NodeIndex,
    ) -> Option<&ControlFlowStructure> {
        // Search through all structures to find a switch that has target_block
        // in one of its case groups (either as the target of duplication or as a case body)
        for structure in self.plan.structures.values() {
            if let ControlFlowKind::Switch { case_groups, .. } = &structure.kind {
                for group in case_groups {
                    // Check if this case group has fallthrough that duplicates target_block
                    if let Some(ref fallthrough) = group.fallthrough {
                        if fallthrough.blocks_to_duplicate.contains(&target_block) {
                            return Some(structure);
                        }
                    }

                    // Check if target_block is part of this case group's body
                    if self.structure_contains_block(group.body, target_block) {
                        return Some(structure);
                    }
                }
            }
        }
        None
    }

    /// Find the final block in a structure (the block that would be the predecessor for fallthrough)
    fn find_final_block_in_structure(&self, structure_id: StructureId) -> Option<NodeIndex> {
        let structure = self.plan.get_structure(structure_id)?;

        match &structure.kind {
            ControlFlowKind::BasicBlock { block, .. } => {
                // A basic block is its own final block
                Some(*block)
            }
            ControlFlowKind::Sequential { elements } => {
                // The final block is the last element's final block
                for element in elements.iter().rev() {
                    if let super::control_flow_plan::SequentialElement::Structure(id) = element {
                        if let Some(final_block) = self.find_final_block_in_structure(*id) {
                            return Some(final_block);
                        }
                    } else if let super::control_flow_plan::SequentialElement::Block(block) =
                        element
                    {
                        return Some(*block);
                    }
                }
                None
            }
            ControlFlowKind::Conditional {
                true_branch,
                false_branch,
                ..
            } => {
                // For conditionals, we'd need to determine which branch leads to the target
                // This is complex - for now, try the true branch first
                self.find_final_block_in_structure(*true_branch)
                    .or_else(|| false_branch.and_then(|fb| self.find_final_block_in_structure(fb)))
            }
            _ => {
                // For other control flow kinds, we'd need specific logic
                None
            }
        }
    }

    /// Check if a structure contains a specific block
    fn structure_contains_block(&self, structure_id: StructureId, target_block: NodeIndex) -> bool {
        let Some(structure) = self.plan.get_structure(structure_id) else {
            return false;
        };

        match &structure.kind {
            ControlFlowKind::BasicBlock { block, .. } => *block == target_block,
            ControlFlowKind::Sequential { elements } => {
                for element in elements {
                    match element {
                        super::control_flow_plan::SequentialElement::Structure(id) => {
                            if self.structure_contains_block(*id, target_block) {
                                return true;
                            }
                        }
                        super::control_flow_plan::SequentialElement::Block(block) => {
                            if *block == target_block {
                                return true;
                            }
                        }
                    }
                }
                false
            }
            ControlFlowKind::Conditional {
                true_branch,
                false_branch,
                ..
            } => {
                self.structure_contains_block(*true_branch, target_block)
                    || false_branch
                        .map_or(false, |fb| self.structure_contains_block(fb, target_block))
            }
            _ => {
                // For other kinds, we'd need specific logic
                false
            }
        }
    }

    /// Find which predecessors are being redirected to duplicated blocks
    fn find_removed_predecessors(
        &self,
        block: NodeIndex,
        duplication_contexts: &[DuplicationContext],
    ) -> Vec<NodeIndex> {
        // For each duplication context, find which predecessor is being redirected
        let mut removed = Vec::new();

        for context in duplication_contexts {
            match context {
                DuplicationContext::SwitchFallthrough { .. } => {
                    // The fallthrough predecessor is the block that was redirected to the duplicate
                    // This is the same logic we use in determine_predecessor_for_context
                    let predecessor = self.determine_predecessor_for_context(block, context);
                    removed.push(predecessor);
                }
                DuplicationContext::SwitchBlockDuplication { case_group_keys: _ } => {
                    // For block duplication, we need to determine which predecessors
                    // are associated with the duplicated case groups
                    // This needs more sophisticated analysis based on the control flow plan
                }
            }
        }

        removed
    }

    /// Update declaration strategies based on PHI deconstruction information
    fn update_strategies_for_phi_deconstruction(&mut self) {
        // Get a copy of PHI deconstructions to avoid borrow checker issues
        let phi_deconstructions = self.plan.phi_deconstructions.clone();

        for ((_block_id, context), phi_info) in &phi_deconstructions {
            // Update strategies for PHI results that are replaced
            for (phi_result, replacement_value) in &phi_info.replacements {
                if let Some(context) = context {
                    // Mark the PHI result as Skip in the duplicated context
                    let dup_phi_result = DuplicatedSSAValue {
                        original: phi_result.clone(),
                        duplication_context: Some(context.clone()),
                    };
                    self.plan
                        .set_declaration_strategy(dup_phi_result, DeclarationStrategy::Skip);

                    // Ensure the replacement value has a declaration strategy in this context
                    let dup_replacement = DuplicatedSSAValue {
                        original: replacement_value.clone(),
                        duplication_context: Some(context.clone()),
                    };

                    if self
                        .plan
                        .get_declaration_strategy(&dup_replacement)
                        .is_none()
                    {
                        let call_site_analysis = &self.plan.call_site_analysis;
                        let replacement_strategy =
                            self.usage_tracker.get_declaration_strategy_with_context(
                                &dup_replacement,
                                Some(call_site_analysis),
                            );
                        self.plan
                            .set_declaration_strategy(dup_replacement, replacement_strategy);
                    }
                }
            }
        }
    }

    /// Compute declaration strategies for all SSA values
    fn compute_declaration_strategies(&mut self) {
        log::debug!(
            "usage_tracker has {} consumed uses",
            self.usage_tracker
                .get_consumed_uses()
                .values()
                .map(|s| s.len())
                .sum::<usize>()
        );

        // Pre-compute implicit arguments for better performance
        let implicit_args_start = std::time::Instant::now();
        self.usage_tracker
            .precompute_implicit_arguments(&self.plan.call_site_analysis);
        log::debug!(
            "Pre-computed implicit arguments in {:?}",
            implicit_args_start.elapsed()
        );

        // Track which SSA values need declarations and where
        let mut declaration_points: HashMap<NodeIndex, Vec<DuplicatedSSAValue>> = HashMap::new();

        // Use the already collected duplicated blocks from analyze()
        let duplicated_blocks = self.duplicated_blocks.clone();

        let total_ssa_values = self.function_analysis.ssa.all_values().len();
        log::debug!(
            "Analyzing {} SSA values for declaration strategies",
            total_ssa_values
        );

        let mut values_processed = 0;
        // Analyze each SSA value
        for ssa_value in self.function_analysis.ssa.all_values() {
            values_processed += 1;
            if values_processed % 1000 == 0 {
                log::debug!(
                    "  Processed {}/{} SSA values",
                    values_processed,
                    total_ssa_values
                );
            }
            // Always analyze the original version
            let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
            let call_site_analysis = &self.plan.call_site_analysis;
            let strategy = self
                .usage_tracker
                .get_declaration_strategy_with_context(&dup_value, Some(call_site_analysis));

            match &strategy {
                DeclarationStrategy::DeclareAtDominator {
                    dominator_block, ..
                } => {
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

                    // Normal duplicated value
                    let call_site_analysis = &self.plan.call_site_analysis;
                    let dup_strategy = self.usage_tracker.get_declaration_strategy_with_context(
                        &dup_value,
                        Some(call_site_analysis),
                    );
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
            for use_site in self.function_analysis.ssa.get_ssa_value_uses(&ssa_value) {
                // Check if this use is in a duplicated block
                if let Some(contexts) = self.duplicated_blocks.get(&use_site.block_id) {
                    // This use is in a duplicated block
                    for context in contexts {
                        // Check if the definition is also in a duplicated block with the same context
                        let dup_value = if self
                            .duplicated_blocks
                            .get(&ssa_value.def_site.block_id)
                            .map(|ctxs| ctxs.contains(context))
                            .unwrap_or(false)
                        {
                            // Both definition and use are in the same duplicated context
                            DuplicatedSSAValue {
                                original: ssa_value.clone(),
                                duplication_context: Some(context.clone()),
                            }
                        } else {
                            // Use is in duplicated block but definition is not - use original
                            DuplicatedSSAValue::original(ssa_value.clone())
                        };

                        let strategy = self.usage_tracker.get_use_strategy(&dup_value, use_site);
                        self.plan
                            .set_use_strategy(dup_value.clone(), use_site.clone(), strategy);
                    }
                } else {
                    // Normal use, not in a duplicated block
                    let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
                    let strategy = self.usage_tracker.get_use_strategy(&dup_value, use_site);
                    self.plan
                        .set_use_strategy(dup_value.clone(), use_site.clone(), strategy);
                }
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
