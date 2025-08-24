//! Sparse switch pattern analysis  
//!
//! This module detects and analyzes sparse switch patterns in the CFG
//! where switches are implemented as chains of JStrictEqual comparisons

use super::switch_info::*;
use crate::analysis::value_tracker::{ConstantValue, TrackedValue, ValueTracker};
use crate::cfg::analysis::PostDominatorAnalysis;
use crate::cfg::ssa::{SSAAnalysis, SSAValue};
use crate::cfg::{Cfg, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::HbcFile;
use crate::hbc::InstructionIndex;
use log::debug;
use ordered_float::OrderedFloat;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};

/// Safety checker for setup instructions
pub struct SetupSafetyChecker<'a> {
    _phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> SetupSafetyChecker<'a> {
    pub fn new(
        _cfg: &'a Cfg<'a>,
        _ssa: &'a SSAAnalysis,
        _postdom: &'a PostDominatorAnalysis,
    ) -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn is_case_localizable(&self, setup_instr: &SetupInstruction) -> bool {
        // Simplified safety check - for now, allow all constant loads and moves
        // In a full implementation, this would check:
        // 1. No observable effects between setup and use
        // 2. Setup instruction is anchored to the comparison
        // 3. No interference with other register definitions
        matches!(
            &setup_instr.instruction.instruction,
            UnifiedInstruction::LoadConstString { .. }
                | UnifiedInstruction::LoadConstStringLongIndex { .. }
                | UnifiedInstruction::LoadConstUInt8 { .. }
                | UnifiedInstruction::LoadConstInt { .. }
                | UnifiedInstruction::LoadConstDouble { .. }
                | UnifiedInstruction::LoadConstZero { .. }
                | UnifiedInstruction::LoadConstNull { .. }
                | UnifiedInstruction::LoadConstUndefined { .. }
                | UnifiedInstruction::LoadConstTrue { .. }
                | UnifiedInstruction::LoadConstFalse { .. }
                | UnifiedInstruction::Mov { .. }
        )
    }
}

/// Sparse switch pattern analyzer
///
/// Analyzes chains of JStrictEqual comparisons that implement sparse switch statements
pub struct SparseSwitchAnalyzer<'a> {
    hbc_file: Option<&'a HbcFile<'a>>,
}

impl<'a> SparseSwitchAnalyzer<'a> {
    pub fn new() -> Self {
        Self { hbc_file: None }
    }

    pub fn with_hbc_file(hbc_file: &'a HbcFile) -> Self {
        Self {
            hbc_file: Some(hbc_file),
        }
    }

    /// Build the entry path from the start block to a specific case
    fn build_entry_path(
        &self,
        start_block: NodeIndex,
        target_case_block: NodeIndex,
        cfg: &Cfg<'a>,
    ) -> Vec<NodeIndex> {
        let mut path = vec![start_block];
        let mut current = start_block;
        let mut visited = HashSet::new();

        while current != target_case_block && visited.insert(current) {
            // For sparse switches, we follow the false branch until we reach our target
            if let Some(false_successor) = self.get_false_successor(current, cfg) {
                path.push(false_successor);
                current = false_successor;
            } else {
                break;
            }
        }

        path
    }

    /// Collect setup instructions along an entry path
    fn collect_setup_along_path(
        &self,
        path: &[NodeIndex],
        target_block: NodeIndex,
        discriminator: u8,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> SmallVec<[SetupInstruction; 4]> {
        let mut setup = SmallVec::new();
        let mut seen_registers = HashSet::new();

        // Process each block in the path
        for &block_id in path {
            let block = &cfg.graph()[block_id];

            for instr in block.instructions() {
                let usage = crate::generated::instruction_analysis::analyze_register_usage(
                    &instr.instruction,
                );

                if let Some(target_reg) = usage.target {
                    // Skip if we've already seen this register (later definitions override)
                    if seen_registers.contains(&target_reg) {
                        continue;
                    }

                    // Skip the discriminator register itself
                    if target_reg == discriminator {
                        continue;
                    }

                    // Check if this value is used in the target block before being redefined
                    let instr_idx = instr.instruction_index;
                    if let Some(ssa_value) = ssa.get_value_after_instruction(target_reg, instr_idx)
                    {
                        // Check if this SSA value is used in the target block before redefinition
                        if self.is_value_used_before_redefinition(ssa_value, target_block, cfg, ssa)
                        {
                            // Extract constant value if this is a constant load
                            let const_value = if let Some(hbc_file) = self.hbc_file {
                                let value_tracker = ValueTracker::new(cfg, ssa, hbc_file);
                                if let TrackedValue::Constant(c) =
                                    value_tracker.get_value(ssa_value)
                                {
                                    Some(c)
                                } else {
                                    None
                                }
                            } else {
                                None
                            };

                            setup.push(SetupInstruction {
                                instruction: std::rc::Rc::new(instr.clone()),
                                ssa_value: ssa_value.clone(),
                                value: const_value,
                            });

                            seen_registers.insert(target_reg);
                        }
                    }
                }
            }
        }

        setup
    }

    /// Check if an SSA value is used in a block before being redefined
    fn is_value_used_before_redefinition(
        &self,
        ssa_value: &SSAValue,
        block_id: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> bool {
        let register = ssa_value.register;
        let block = &cfg.graph()[block_id];

        // Check each instruction in order
        for instr in block.instructions() {
            let usage =
                crate::generated::instruction_analysis::analyze_register_usage(&instr.instruction);

            // First check if this instruction uses the register
            for src_reg in usage.sources {
                if src_reg == register {
                    // Check if it's using our specific SSA value
                    if let Some(src_value) =
                        ssa.get_value_before_instruction(src_reg, instr.instruction_index)
                    {
                        if src_value == ssa_value {
                            // The value is used before any redefinition
                            return true;
                        }
                    }
                }
            }

            // Then check if this instruction redefines the register
            if let Some(target_reg) = usage.target {
                if target_reg == register {
                    // The register is redefined, our value is no longer live
                    return false;
                }
            }
        }

        // Check PHI functions at this block
        if let Some(phis) = ssa.phi_functions.get(&block_id) {
            for phi in phis {
                // PHI functions conceptually execute at the beginning of the block
                // so they would use values before any instruction redefines them
                for (_, operand_value) in &phi.operands {
                    if operand_value == ssa_value {
                        return true;
                    }
                }
            }
        }

        // Check if the value flows to successor blocks
        // Only if the register wasn't redefined in this block
        let register_redefined_in_block = block.instructions().iter().any(|instr| {
            let usage =
                crate::generated::instruction_analysis::analyze_register_usage(&instr.instruction);
            usage.target == Some(register)
        });

        if !register_redefined_in_block {
            // Check immediate successors for PHI functions that might use this value
            for edge in cfg.graph().edges(block_id) {
                let successor = edge.target();
                if let Some(successor_phis) = ssa.phi_functions.get(&successor) {
                    for phi in successor_phis {
                        if let Some((_, operand_value)) =
                            phi.operands.iter().find(|(pred, _)| **pred == block_id)
                        {
                            if operand_value == ssa_value {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Detect a switch pattern starting from a given block
    pub fn detect_switch_pattern(
        &self,
        start_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
        postdom: &PostDominatorAnalysis,
    ) -> Option<SwitchInfo> {
        // Quick check: does this block load a parameter or have a comparison?
        let first_block = &cfg.graph()[start_block];
        let discriminator = self.find_discriminator_with_ssa(first_block, start_block, ssa, cfg)?;

        // Safety checker for setup instructions
        let safety_checker = SetupSafetyChecker::new(cfg, ssa, postdom);

        // First pass: collect all cases and their comparison blocks
        let mut cases = Vec::new();
        let mut case_blocks = Vec::new(); // Track comparison blocks for entry path building
        let mut current_block = start_block;
        let mut visited = HashSet::new();
        let mut execution_order = 0;

        loop {
            if !visited.insert(current_block) {
                break; // Avoid infinite loops
            }

            // Try to extract a case from this block
            if let Some(mut case_info) = self.extract_case_from_block_with_dispatch(
                current_block,
                discriminator,
                cfg,
                ssa,
                Some(start_block),
            ) {
                // Store the comparison block for this case
                case_blocks.push((case_info.comparison_block, case_info.target_block));

                // Set execution order
                case_info.execution_order = execution_order;
                execution_order += 1;

                cases.push(case_info);

                // Move to the false successor
                if let Some(false_successor) = self.get_false_successor(current_block, cfg) {
                    current_block = false_successor;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Second pass: build entry paths and collect setup instructions
        for case in cases.iter_mut() {
            // Build entry path from start to this case's comparison block
            let entry_path = self.build_entry_path(start_block, case.comparison_block, cfg);

            // Collect all setup instructions along the path
            let path_setup = self.collect_setup_along_path(
                &entry_path,
                case.target_block,
                discriminator,
                cfg,
                ssa,
            );

            // Replace the case's setup with our path-based setup
            case.setup = path_setup;

            // Filter setup instructions by safety
            case.setup
                .retain(|setup_instr| safety_checker.is_case_localizable(setup_instr));
        }

        // If we didn't find any cases, this isn't a switch pattern
        if cases.is_empty() {
            return None;
        }

        // Determine default case
        let default_case = if current_block != start_block {
            // If we ended up at a different block, it might be the default
            // Default cases don't have setup instructions - the block contains the actual case body
            Some(DefaultCase {
                target_block: current_block,
                setup: SmallVec::new(),
            })
        } else {
            None
        };

        // 3. Detect shared tail block
        let shared_tail = self.detect_shared_tail(&cases, &default_case, postdom, cfg, ssa);

        // If we have a shared tail, update cases that jump to it
        // They should not be marked as always_terminates
        if let Some(ref tail) = shared_tail {
            for case in &mut cases {
                if case.target_block == tail.block_id {
                    case.always_terminates = false;
                }
            }
        }

        // 4. Check PHI scenarios and control flow (more expensive)
        if self.should_bail_out_for_phi_scenarios() {
            return None;
        }

        if self.should_bail_out_for_control_flow() {
            return None;
        }

        if self.should_bail_out_for_constants(&cases) {
            return None;
        }

        // Find the first comparison instruction to get its index
        let discriminator_instruction_index = if let Some(first_case) = cases.first() {
            let block = &cfg.graph()[first_case.comparison_block];
            // Find the comparison instruction in this block
            block
                .instructions()
                .iter()
                .find(|instr| {
                    matches!(
                        &instr.instruction,
                        UnifiedInstruction::JStrictEqual { .. }
                            | UnifiedInstruction::JStrictEqualLong { .. }
                    )
                })
                .map(|instr| instr.instruction_index)
                .unwrap_or_else(|| InstructionIndex::new(0))
        } else {
            InstructionIndex::new(0)
        };

        Some(SwitchInfo {
            discriminator,
            discriminator_instruction_index,
            cases,
            default_case,
            shared_tail,
        })
    }

    /// Find what register is being used as discriminator using SSA analysis
    fn find_discriminator_with_ssa(
        &self,
        block: &crate::cfg::Block,
        block_id: NodeIndex,
        ssa: &SSAAnalysis,
        cfg: &Cfg<'a>,
    ) -> Option<u8> {
        // Look for LoadParam as first instruction
        if let Some(first) = block.instructions().first() {
            if let UnifiedInstruction::LoadParam { operand_0, .. } = &first.instruction {
                return Some(*operand_0);
            }
        }

        // Look for comparison instruction and use ValueTracker to determine discriminator
        if let Some(hbc_file) = self.hbc_file {
            let value_tracker = ValueTracker::new(cfg, ssa, hbc_file);

            for instr in block.instructions() {
                match &instr.instruction {
                    UnifiedInstruction::JStrictEqual {
                        operand_1,
                        operand_2,
                        ..
                    }
                    | UnifiedInstruction::JStrictEqualLong {
                        operand_1,
                        operand_2,
                        ..
                    } => {
                        let pc = instr.instruction_index;

                        // Use ValueTracker to analyze what each operand contains
                        let op1_value = value_tracker.get_value_at_point(*operand_1, block_id, pc);
                        let op2_value = value_tracker.get_value_at_point(*operand_2, block_id, pc);

                        // The discriminator is the one that contains a parameter or unknown value
                        // (not a constant)
                        let op1_is_constant = matches!(op1_value, TrackedValue::Constant(_));
                        let op2_is_constant = matches!(op2_value, TrackedValue::Constant(_));

                        if !op1_is_constant && op2_is_constant {
                            return Some(*operand_1);
                        } else if op1_is_constant && !op2_is_constant {
                            return Some(*operand_2);
                        } else if matches!(op1_value, TrackedValue::Parameter { .. }) {
                            return Some(*operand_1);
                        } else if matches!(op2_value, TrackedValue::Parameter { .. }) {
                            return Some(*operand_2);
                        } else {
                            // Fallback to operand_1 if both are non-constants
                            return Some(*operand_1);
                        }
                    }
                    _ => {}
                }
            }
        }

        None
    }

    /// Extract case information from a block containing a comparison
    /// dispatch_block: If provided, instructions from the dispatch block won't be included as setup
    fn extract_case_from_block_with_dispatch(
        &self,
        block_id: NodeIndex,
        discriminator: u8,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
        _dispatch_block: Option<NodeIndex>,
    ) -> Option<CaseInfo> {
        let block = &cfg.graph()[block_id];
        let instructions = block.instructions();

        // Look for comparison instruction
        let mut comparison_index = None;
        let mut keys = Vec::new();
        let mut target_block = None;
        let mut _source_pc = InstructionIndex::new(0);

        // First pass: find the comparison instruction and extract case key
        for (i, instr) in instructions.iter().enumerate() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual {
                    operand_0: _,
                    operand_1,
                    operand_2,
                }
                | UnifiedInstruction::JStrictEqualLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    // Check if this compares our discriminator
                    // The discriminator can be in either operand position
                    let (const_reg, found) = if *operand_1 == discriminator {
                        // operand_2 should be the constant we're comparing against
                        (*operand_2, true)
                    } else if *operand_2 == discriminator {
                        // operand_1 should be the constant we're comparing against
                        (*operand_1, true)
                    } else {
                        (0, false)
                    };

                    if found {
                        // Only process this comparison once
                        if comparison_index.is_some() {
                            continue;
                        }

                        comparison_index = Some(i);
                        _source_pc = instr.instruction_index;

                        // Extract the constant value for the case key using ValueTracker
                        if let Some(hbc_file) = self.hbc_file {
                            let value_tracker = ValueTracker::new(cfg, ssa, hbc_file);
                            let tracked_value = value_tracker.get_value_at_point(
                                const_reg,
                                block_id,
                                instr.instruction_index,
                            );

                            if let TrackedValue::Constant(constant_value) = tracked_value {
                                let case_key = match constant_value {
                                    ConstantValue::Number(n) => CaseKey::Number(OrderedFloat(n)),
                                    ConstantValue::String(s) => CaseKey::String(s),
                                    ConstantValue::Boolean(b) => CaseKey::Boolean(b),
                                    ConstantValue::Null => CaseKey::Null,
                                    ConstantValue::Undefined => CaseKey::Undefined,
                                };
                                keys.push(case_key);
                            }
                        }

                        // Find the target block
                        target_block = self.get_true_successor(block_id, cfg);
                        break;
                    }
                }
                _ => {}
            }
        }

        // Bail if we didn't find a comparison
        let _comparison_idx = comparison_index?;
        let target = target_block?;

        // For the initial extraction, we'll return an empty setup list
        // The actual setup instructions will be collected by the entry path algorithm
        let setup = SmallVec::new();

        // Check if this case always terminates by looking at the target block
        let always_terminates = {
            let target_block_data = &cfg.graph()[target];
            target_block_data.instructions().iter().any(|instr| {
                matches!(
                    &instr.instruction,
                    UnifiedInstruction::Ret { .. }
                        | UnifiedInstruction::Throw { .. }
                        | UnifiedInstruction::ThrowIfUndefinedInst { .. }
                )
            })
        };

        Some(CaseInfo {
            keys,
            comparison_block: block_id,
            target_block: target,
            setup,
            always_terminates,
            execution_order: 0, // Will be set by caller
        })
    }

    /// Get the true successor of a conditional jump
    fn get_true_successor(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> Option<NodeIndex> {
        for edge in cfg.graph().edges(block_id) {
            if matches!(edge.weight(), EdgeKind::True) {
                return Some(edge.target());
            }
        }
        None
    }

    /// Get the false successor of a conditional jump
    fn get_false_successor(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> Option<NodeIndex> {
        for edge in cfg.graph().edges(block_id) {
            if matches!(edge.weight(), EdgeKind::False) {
                return Some(edge.target());
            }
        }
        None
    }

    /// Detect if there's a shared tail block where all cases converge
    fn detect_shared_tail(
        &self,
        cases: &[CaseInfo],
        default_case: &Option<DefaultCase>,
        postdom: &PostDominatorAnalysis,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> Option<SharedTailInfo> {
        // Don't skip shared tail detection based on termination
        // Even if cases terminate, they might all jump to the same terminating block

        // Check if any case target block is jumped to by other case blocks
        // This would indicate it's a shared tail (break target) rather than a fallthrough
        let mut jump_targets = std::collections::HashSet::new();

        for case in cases {
            // Check what this case's target block jumps to
            let block = &cfg.graph()[case.target_block];
            if let Some(last_instr) = block.instructions().last() {
                if let UnifiedInstruction::Jmp { .. } = &last_instr.instruction {
                    // This block jumps somewhere - find where
                    use petgraph::visit::EdgeRef;
                    for edge in cfg.graph().edges(case.target_block) {
                        jump_targets.insert(edge.target());
                    }
                }
            }
        }
        if let Some(default) = default_case {
            let block = &cfg.graph()[default.target_block];
            if let Some(last_instr) = block.instructions().last() {
                if let UnifiedInstruction::Jmp { .. } = &last_instr.instruction {
                    use petgraph::visit::EdgeRef;
                    for edge in cfg.graph().edges(default.target_block) {
                        jump_targets.insert(edge.target());
                    }
                }
            }
        }

        // Check if any jump target is also a case target
        // If so, it might be a shared tail
        let mut shared_tail = None;
        for case in cases {
            if jump_targets.contains(&case.target_block) {
                // This case's target is jumped to by other cases
                // Check if it's meaningful
                if self.is_meaningful_shared_tail(case.target_block, cfg) {
                    debug!(
                        "Detected shared tail at case target block {:?}",
                        case.target_block
                    );
                    shared_tail = Some(case.target_block);
                    break;
                }
            }
        }

        // If no shared tail found, try common post-dominator
        if shared_tail.is_none() {
            let mut target_blocks = Vec::new();
            for case in cases {
                target_blocks.push(case.target_block);
            }
            if let Some(default) = default_case {
                target_blocks.push(default.target_block);
            }
            shared_tail = self.find_common_postdominator(&target_blocks, postdom);
        }

        let shared_tail = shared_tail?;

        // Check if it's a meaningful shared tail
        if !self.is_meaningful_shared_tail(shared_tail, cfg) {
            return None;
        }

        // Analyze PHI requirements
        let phi_nodes = self.analyze_phi_requirements(shared_tail, cfg, ssa);

        Some(SharedTailInfo {
            block_id: shared_tail,
            phi_nodes,
        })
    }

    /// Find common post-dominator of multiple blocks
    fn find_common_postdominator(
        &self,
        blocks: &[NodeIndex],
        postdom: &PostDominatorAnalysis,
    ) -> Option<NodeIndex> {
        if blocks.is_empty() {
            return None;
        }

        let mut common = blocks[0];

        for &block in &blocks[1..] {
            // Find the lowest common ancestor in the post-dominator tree
            let mut a = common;
            let mut b = block;

            // Get paths to root for both nodes
            let mut path_a = vec![a];
            while let Some(parent) = postdom.immediate_post_dominator(a) {
                if parent == a {
                    break;
                }
                path_a.push(parent);
                a = parent;
            }

            let mut path_b = vec![b];
            while let Some(parent) = postdom.immediate_post_dominator(b) {
                if parent == b {
                    break;
                }
                path_b.push(parent);
                b = parent;
            }

            // Find common ancestor
            let mut found = None;
            for node_a in &path_a {
                if path_b.contains(node_a) {
                    found = Some(*node_a);
                    break;
                }
            }

            common = found?;
        }

        Some(common)
    }

    /// Check if a block is a meaningful shared tail
    fn is_meaningful_shared_tail(&self, tail_block: NodeIndex, cfg: &Cfg<'a>) -> bool {
        let block = &cfg.graph()[tail_block];

        // A meaningful shared tail should have actual content
        if !block.instructions().is_empty() {
            true // Has instructions
        } else {
            false // Empty blocks are not meaningful
        }
    }

    /// Analyze which registers need PHI nodes at the shared tail
    fn analyze_phi_requirements(
        &self,
        tail_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> HashMap<u8, PhiNode> {
        let mut phi_nodes = HashMap::new();

        // Get existing PHI functions at the tail block
        let existing_phis = ssa
            .phi_functions
            .get(&tail_block)
            .map(|phis| phis.as_slice())
            .unwrap_or(&[]);

        // Create ValueTracker if we have HBC file
        if let Some(hbc_file) = self.hbc_file {
            let value_tracker = ValueTracker::new(cfg, ssa, hbc_file);

            // Convert existing PHI functions to our PhiNode representation
            for phi_func in existing_phis {
                let mut values = HashMap::new();

                // Extract constant values from each PHI operand
                for (pred_block, ssa_value) in &phi_func.operands {
                    // Use ValueTracker to get the value
                    if let TrackedValue::Constant(constant_value) =
                        value_tracker.get_value(ssa_value)
                    {
                        values.insert(*pred_block, constant_value);
                    }
                }

                // Only create our PhiNode if we have values to track
                if !values.is_empty() {
                    phi_nodes.insert(
                        phi_func.register,
                        PhiNode {
                            register: phi_func.register,
                            values,
                            ssa_phi_value: Some(phi_func.result.clone()),
                        },
                    );
                }
            }
        }

        phi_nodes
    }

    /// Check if we should bail out for PHI scenarios
    fn should_bail_out_for_phi_scenarios(&self) -> bool {
        // For now, don't bail out for PHI scenarios
        // In a full implementation, this would check for complex PHI node requirements
        false
    }

    /// Check if we should bail out for control flow issues
    fn should_bail_out_for_control_flow(&self) -> bool {
        // For now, don't bail out for control flow issues
        // In a full implementation, this would check for:
        // - Exception-throwing instructions
        // - Complex nested control flow
        // - Irreducible control flow patterns
        false
    }

    /// Check if we should bail out for constant-related issues
    fn should_bail_out_for_constants(&self, cases: &[CaseInfo]) -> bool {
        let mut constant_set = HashSet::new();

        for case in cases {
            for key in &case.keys {
                let normalized = key.normalize_for_grouping();
                if !constant_set.insert(normalized) {
                    // Duplicate constant found
                    return true;
                }
            }
        }

        false
    }
}
