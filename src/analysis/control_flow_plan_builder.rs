//! Builder for creating ControlFlowPlan from CFG analysis
//!
//! This module converts CFG analysis results into a high-level control flow plan
//! that can be used for AST generation.

use super::ControlFlowPlan;
use crate::analysis::control_flow_plan::{
    ComparisonExpression, ControlFlowKind, SequentialElement, StructureId,
};
use crate::analysis::control_flow_plan_analyzer::ControlFlowPlanAnalyzer;
use crate::analysis::FunctionAnalysis;
use crate::cfg::analysis::{ConditionalChain, PostDominatorAnalysis, SwitchRegion};
use crate::cfg::ssa::{RegisterUse, SSAValue};
use crate::cfg::switch_analysis::sparse_switch_analyzer::SparseSwitchAnalyzer;
use crate::cfg::switch_analysis::SwitchInfo;
use crate::cfg::Cfg;
use crate::decompiler::InlineConfig;
use crate::generated::generated_traits::BinaryOperator;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::instruction_types::InstructionIndex;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

/// Builder for creating ControlFlowPlan
pub struct ControlFlowPlanBuilder<'a> {
    plan: ControlFlowPlan,
    cfg: &'a Cfg<'a>,
    function_analysis: &'a FunctionAnalysis<'a>,
    /// Track which blocks have been processed
    processed_blocks: HashSet<NodeIndex>,
    /// Blocks that should stop sequential traversal (e.g., shared tails, join blocks)
    stop_blocks: HashSet<NodeIndex>,
    /// Cached post-dominator analysis for nested pattern detection
    post_dominators: Option<PostDominatorAnalysis>,
    /// Set of catch blocks to exclude from normal control flow
    catch_blocks: HashSet<NodeIndex>,
    /// Inline configuration for optimization passes
    inline_config: InlineConfig,
}

impl<'a> ControlFlowPlanBuilder<'a> {
    /// Create a new builder
    pub fn new(cfg: &'a Cfg<'a>, function_analysis: &'a FunctionAnalysis<'a>) -> Self {
        // Pre-compute post-dominators for pattern detection
        let post_dominators = cfg.analyze_post_dominators();
        
        // Identify catch blocks from exception analysis
        let catch_blocks = if let Some(exception_analysis) = cfg.analyze_exception_handlers() {
            crate::cfg::exception_analysis::get_catch_blocks(&exception_analysis)
        } else {
            HashSet::new()
        };

        Self {
            plan: ControlFlowPlan::new(),
            cfg,
            function_analysis,
            processed_blocks: HashSet::new(),
            stop_blocks: HashSet::new(),
            post_dominators,
            catch_blocks,
            inline_config: InlineConfig::default(),
        }
    }

    /// Set the inline configuration for optimization passes
    pub fn set_inline_config(&mut self, config: InlineConfig) {
        self.inline_config = config;
    }

    /// Build the control flow plan
    pub fn build(self) -> ControlFlowPlan {
        log::debug!(
            "ControlFlowPlanBuilder::build starting for function {}",
            self.cfg.function_index()
        );

        // Run call site analysis first
        log::debug!("Starting call site analysis...");
        let call_site_analysis =
            crate::analysis::call_site_analysis::CallSiteAnalysis::analyze(&self.cfg);
        log::debug!(
            "Call site analysis complete: {} call sites found",
            call_site_analysis.call_sites.len()
        );

        // First build the structure
        let function_analysis = self.function_analysis;
        let inline_config = self.inline_config.clone();
        log::debug!("Building control flow structure...");
        let mut plan = self.build_structure();
        log::debug!("Control flow structure built");

        // Add the call site analysis to the plan
        plan.call_site_analysis = call_site_analysis;

        // Then analyze it to compute SSA strategies
        log::debug!("Starting SSA strategy analysis...");
        let analyzer = ControlFlowPlanAnalyzer::with_inline_config(
            &mut plan,
            function_analysis,
            &inline_config,
        );
        analyzer.analyze();
        log::debug!("SSA strategy analysis complete");

        plan
    }

    /// Build just the control flow structure without SSA analysis
    fn build_structure(mut self) -> ControlFlowPlan {
        // Use existing CFG analyses instead of reimplementing

        // Check for exception handlers first
        if let Some(exception_analysis) = self.cfg.analyze_exception_handlers() {
            if !exception_analysis.regions.is_empty() {
                log::debug!("Found {} exception regions", exception_analysis.regions.len());
                let root = self.build_from_exception_analysis(&exception_analysis);
                self.plan.set_root(root);
                return self.plan;
            }
        } else {
            log::debug!("No exception analysis found for function {}", self.cfg.function_index());
        }

        // Get switch regions if any
        if let Some(switch_analysis) = self.cfg.analyze_switch_regions(&self.function_analysis.ssa)
        {
            // If we have switches, build from them
            if !switch_analysis.regions.is_empty() {
                let root = self.build_from_switch_analysis(&switch_analysis);
                self.plan.set_root(root);
                return self.plan;
            }
        }

        // Get conditional chains if any
        if let Some(conditional_analysis) = self.cfg.analyze_conditional_chains() {
            if !conditional_analysis.chains.is_empty() {
                let root = self.build_from_conditional_analysis(&conditional_analysis);
                self.plan.set_root(root);
                return self.plan;
            }
        }

        // Get loop analysis
        let loop_analysis = self.cfg.analyze_loops();
        if !loop_analysis.loops.is_empty() {
            let root = self.build_from_loop_analysis(&loop_analysis);
            self.plan.set_root(root);
            return self.plan;
        }

        // Fallback: build sequential structure from entry
        let entry_block = NodeIndex::new(0);
        let root_structure = self.build_sequential_structure(entry_block);
        self.plan.set_root(root_structure);

        self.plan
    }

    /// Build a sequential structure starting from a block
    fn build_sequential_structure(&mut self, start_block: NodeIndex) -> StructureId {
        // Special case: if the block is already processed or is a stop block, return empty
        if self.processed_blocks.contains(&start_block) || self.stop_blocks.contains(&start_block) {
            return self.plan.create_structure(ControlFlowKind::Empty);
        }

        let mut elements = Vec::new();
        let mut current_block = Some(start_block);

        while let Some(block) = current_block {
            if self.processed_blocks.contains(&block) || self.stop_blocks.contains(&block) {
                break;
            }
            
            // Skip catch blocks from sequential structures
            if self.catch_blocks.contains(&block) {
                break;
            }

            // Check if this block starts a control flow pattern
            if let Some(structure_id) = self.try_build_control_structure(block) {
                elements.push(SequentialElement::Structure(structure_id));
                // The control structure handles its own blocks
                current_block = self.find_next_block_after_structure(structure_id);
            } else {
                // It's a basic block
                elements.push(SequentialElement::Block(block));
                self.processed_blocks.insert(block);

                // Find the next sequential block
                current_block = self.find_next_sequential_block(block);
            }
        }

        // Create the sequential structure
        let kind = if elements.is_empty() {
            ControlFlowKind::Empty
        } else if elements.len() == 1 {
            // If there's only one element, unwrap it
            match elements.into_iter().next().unwrap() {
                SequentialElement::Block(block) => {
                    // Don't re-create the basic block, it's already processed
                    ControlFlowKind::BasicBlock {
                        block,
                        instruction_count: self.cfg.graph()[block].instructions().len(),
                        is_synthetic: false,
                    }
                }
                SequentialElement::Structure(id) => {
                    return id;
                }
            }
        } else {
            ControlFlowKind::Sequential { elements }
        };

        self.plan.create_structure(kind)
    }

    /// Build from switch analysis
    fn build_from_switch_analysis(
        &mut self,
        analysis: &crate::cfg::analysis::SwitchAnalysis,
    ) -> StructureId {
        // For now, just handle the first switch region
        // TODO: Handle multiple switches properly
        if let Some(region) = analysis.regions.first() {
            self.build_switch_from_region(region.clone())
        } else {
            self.plan.create_structure(ControlFlowKind::Empty)
        }
    }

    /// Build from conditional analysis
    fn build_from_conditional_analysis(
        &mut self,
        analysis: &crate::cfg::analysis::ConditionalAnalysis,
    ) -> StructureId {
        log::debug!("Building from {} conditional chains", analysis.chains.len());

        // Build a sequence containing all conditional chains and unprocessed blocks
        let mut structures = Vec::new();

        // Process each conditional chain
        for (i, chain) in analysis.chains.iter().enumerate() {
            log::debug!(
                "Processing conditional chain {}/{}",
                i + 1,
                analysis.chains.len()
            );

            // Skip if this chain's blocks have already been processed
            // Check the first branch's condition block since chains don't have a single condition_block
            let chain_already_processed = chain
                .branches
                .iter()
                .any(|branch| self.processed_blocks.contains(&branch.condition_block));

            if chain_already_processed {
                log::debug!("  Chain {} already processed, skipping", i);
                continue;
            }

            let structure = self.build_conditional_structure(chain.clone());
            structures.push(structure);
        }

        // Now collect any remaining unprocessed blocks
        let all_blocks: Vec<NodeIndex> = self
            .cfg
            .graph()
            .node_indices()
            .filter(|&idx| !self.cfg.graph()[idx].is_exit())
            .collect();

        log::debug!(
            "Checking {} total blocks for unprocessed ones",
            all_blocks.len()
        );

        for &block in &all_blocks {
            if !self.processed_blocks.contains(&block) && !self.stop_blocks.contains(&block) {
                log::debug!("  Found unprocessed block {}", block.index());

                // Create a basic block structure for this unprocessed block
                if let Some(structure) = self.try_build_basic_block(block) {
                    structures.push(structure);
                }
            } else if block.index() == 3 {
                log::debug!(
                    "  Block 3 status: processed={}, stop={}",
                    self.processed_blocks.contains(&block),
                    self.stop_blocks.contains(&block)
                );
            }
        }

        // If we have multiple structures, wrap them in a sequence
        match structures.len() {
            0 => self.plan.create_structure(ControlFlowKind::Empty),
            1 => structures[0],
            _ => {
                log::debug!("Creating sequence with {} structures", structures.len());
                // Convert StructureId elements to SequentialElement
                let elements: Vec<SequentialElement> = structures
                    .into_iter()
                    .map(|id| SequentialElement::Structure(id))
                    .collect();
                self.plan
                    .create_structure(ControlFlowKind::Sequential { elements })
            }
        }
    }

    /// Build from loop analysis
    fn build_from_loop_analysis(
        &mut self,
        analysis: &crate::cfg::analysis::LoopAnalysis,
    ) -> StructureId {
        // TODO: Implement loop building
        let _ = analysis;
        self.plan.create_structure(ControlFlowKind::Empty)
    }

    /// Try to build a control flow structure starting at this block
    fn try_build_control_structure(&mut self, block: NodeIndex) -> Option<StructureId> {
        // Don't try to detect patterns in already processed blocks
        if self.processed_blocks.contains(&block) {
            return None;
        }

        // 1. Check for dense switch (SwitchImm instruction)
        if let Some(region) = self.detect_dense_switch_at_block(block) {
            return Some(self.build_switch_from_region(region));
        }

        // 2. Check for sparse switch using existing analyzer
        if let Some(ref postdom) = self.post_dominators {
            let analyzer = SparseSwitchAnalyzer::with_hbc_file(self.cfg.hbc_file());
            if let Some(switch_info) = analyzer.detect_switch_pattern(
                block,
                self.cfg,
                &self.function_analysis.ssa,
                postdom,
            ) {
                // Convert switch_info to SwitchRegion and build
                let region = self.switch_info_to_region(switch_info, block);
                return Some(self.build_switch_from_region(region));
            }
        }

        // 3. Check for conditional chains starting at this block
        if let Some(conditional_analysis) = self.cfg.analyze_conditional_chains() {
            // Check all top-level chains
            for chain in &conditional_analysis.chains {
                if let Some(structure_id) = self.check_chain_at_block(chain, block) {
                    return Some(structure_id);
                }
            }
        }

        // 4. TODO: Could also check for loops here

        None
    }

    /// Check if a conditional chain (or its nested chains) starts at the given block
    fn check_chain_at_block(
        &mut self,
        chain: &ConditionalChain,
        block: NodeIndex,
    ) -> Option<StructureId> {
        // Check if this chain starts at the current block
        if let Some(first_branch) = chain.branches.first() {
            if first_branch.condition_block == block {
                return Some(self.build_conditional_structure(chain.clone()));
            }
        }

        // Recursively check nested chains
        for nested_chain in &chain.nested_chains {
            if let Some(structure_id) = self.check_chain_at_block(nested_chain, block) {
                return Some(structure_id);
            }
        }

        None
    }

    /// Build a switch structure from a region
    fn build_switch_from_region(&mut self, mut region: SwitchRegion) -> StructureId {
        self.processed_blocks.insert(region.dispatch);

        // Analyze case bodies if not already done
        if region.case_analyses.is_empty() {
            region.analyze_case_bodies(self.cfg.graph(), self.cfg, None);
        }

        // Detect the switch pattern - either dense (SwitchImm) or sparse
        let switch_info = if self.is_dense_switch(&region) {
            self.detect_dense_switch_pattern(&region)
        } else {
            self.detect_sparse_switch_pattern(&region)
        };

        // Mark all comparison blocks as processed if we have switch info
        if let Some(ref info) = switch_info {
            for case in &info.cases {
                self.processed_blocks.insert(case.comparison_block);
            }

            // Mark shared tail as a stop block
            if let Some(ref tail) = info.shared_tail {
                self.stop_blocks.insert(tail.block_id);
            }
        }

        // Mark join block as a stop block
        self.stop_blocks.insert(region.join_block);

        // Build case group structures
        let case_groups = if let Some(ref info) = switch_info {
            // Group cases by their target blocks to find case groups
            self.build_case_groups_from_switch_info(info, &region)
        } else {
            // Fallback: create one group per case from the region
            self.build_case_groups_from_region(&region)
        };

        // Build default case if present
        let default_case = if let Some(ref info) = switch_info {
            info.default_case.as_ref().map(|default| {
                // Don't mark as processed before building - build_sequential_structure will handle it
                let structure = self.build_sequential_structure(default.target_block);
                // Now mark it as processed after building
                self.processed_blocks.insert(default.target_block);
                structure
            })
        } else {
            region.default_head.map(|default_block| {
                // Don't mark as processed before building - build_sequential_structure will handle it
                let structure = self.build_sequential_structure(default_block);
                // Now mark it as processed after building
                self.processed_blocks.insert(default_block);
                structure
            })
        };

        // Mark join block as processed
        self.processed_blocks.insert(region.join_block);

        let switch_info_final =
            switch_info.expect("Switch info should be populated by this point.");

        // Extract the discriminator SSA value and RegisterUse
        // For SwitchImm, the discriminator is used in the switch instruction itself
        // However, SSA analysis might not track SwitchImm as a use, so we need to find the value differently

        let (discriminator_value, discriminator_use) = {
            // Get the SSA value that's defined for this register before the switch instruction
            // We use get_value_before_instruction to find what value reaches the switch
            let ssa_value = self
                .function_analysis
                .ssa
                .get_value_before_instruction(
                    switch_info_final.discriminator,
                    switch_info_final.discriminator_instruction_index,
                )
                .cloned();

            // Create a RegisterUse for the switch discriminator if we have the SSA value
            let register_use = if ssa_value.is_some()
                && switch_info_final.discriminator_instruction_index != InstructionIndex(0)
            {
                Some(RegisterUse::new(
                    switch_info_final.discriminator,
                    region.dispatch,
                    switch_info_final.discriminator_instruction_index,
                ))
            } else {
                None
            };

            (ssa_value, register_use)
        };

        let switch_structure = self.plan.create_structure(ControlFlowKind::Switch {
            dispatch_block: region.dispatch,
            info: switch_info_final.clone(),
            discriminator_value,
            discriminator_use,
            case_groups,
            default_case,
        });

        // If there's a shared tail, wrap the switch in a Sequential with the tail after it
        if let Some(ref tail_info) = switch_info_final.shared_tail {
            let tail_block = tail_info.block_id;

            // Don't mark the shared tail as processed yet
            self.processed_blocks.remove(&tail_block);

            // Create the shared tail structure
            let tail_structure = self.plan.create_structure(ControlFlowKind::BasicBlock {
                block: tail_block,
                instruction_count: self.cfg.graph()[tail_block].instructions().len(),
                is_synthetic: false,
            });

            // Mark it as processed now
            self.processed_blocks.insert(tail_block);

            // Wrap switch and tail in a Sequential
            let elements = vec![
                crate::analysis::control_flow_plan::SequentialElement::Structure(switch_structure),
                crate::analysis::control_flow_plan::SequentialElement::Structure(tail_structure),
            ];

            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        } else {
            switch_structure
        }
    }

    /// Build case groups from switch info
    fn build_case_groups_from_switch_info(
        &mut self,
        info: &SwitchInfo,
        region: &SwitchRegion,
    ) -> Vec<crate::analysis::control_flow_plan::CaseGroupStructure> {
        use std::collections::BTreeMap;

        // Identify the shared tail block if it exists
        let shared_tail = info.shared_tail.as_ref().map(|tail| tail.block_id);

        // Group cases by target block to identify case groups
        let mut groups_by_target: BTreeMap<NodeIndex, Vec<&crate::cfg::switch_analysis::CaseInfo>> =
            BTreeMap::new();
        for case in &info.cases {
            groups_by_target
                .entry(case.target_block)
                .or_default()
                .push(case);
        }

        // Build case group structures
        let mut case_groups = Vec::new();
        for (target_block, cases_in_group) in groups_by_target {
            // Don't mark shared tail as processed yet - it might be used by multiple groups
            let should_mark_processed = shared_tail != Some(target_block);

            if should_mark_processed {
                self.processed_blocks.insert(target_block);
            }

            // Build the body structure
            let body = if shared_tail == Some(target_block) {
                // This group goes directly to the shared tail
                // Create an empty body since the actual shared tail will come after the switch
                self.plan.create_structure(ControlFlowKind::Empty)
            } else {
                // Try to find the case analysis for this target block
                // We need to find which case index corresponds to this target
                let case_analysis = cases_in_group.iter().find_map(|case_info| {
                    // Find the corresponding case in the region
                    region
                        .cases
                        .iter()
                        .position(|c| c.case_head == case_info.target_block)
                        .and_then(|idx| region.case_analyses.get(&idx))
                });

                if let Some(analysis) = case_analysis {
                    // Use the case body analysis which can detect nested control flow
                    self.build_from_case_body_analysis(analysis)
                } else {
                    // Fallback to simple sequential structure
                    // First check if block exists in the graph
                    if target_block.index() < self.cfg.graph().node_count() {
                        // Don't mark as processed yet, let build_sequential_structure handle it
                        if should_mark_processed {
                            self.processed_blocks.remove(&target_block);
                        }
                        self.build_sequential_structure(target_block)
                    } else {
                        self.plan.create_structure(ControlFlowKind::Empty)
                    }
                }
            };

            // Create switch cases for this group
            let switch_cases: Vec<_> = cases_in_group
                .iter()
                .map(|case_info| crate::analysis::control_flow_plan::SwitchCase {
                    keys: case_info.keys.clone(),
                    comparison_block: Some(case_info.comparison_block),
                    setup_instructions: case_info.setup.iter().cloned().collect(),
                    execution_order: case_info.execution_order,
                })
                .collect();

            // Check for fallthrough - we'll detect this after all groups are built
            let fallthrough = None;

            // Determine if this case needs a break statement
            let needs_break = if fallthrough.is_some() {
                crate::analysis::control_flow_plan::BreakRequirement::FallthroughIntended
            } else {
                // Check if the body always terminates
                if let Some(termination) = self.check_structure_termination(&body) {
                    crate::analysis::control_flow_plan::BreakRequirement::NotNeeded {
                        reason: termination,
                    }
                } else {
                    crate::analysis::control_flow_plan::BreakRequirement::Required
                }
            };

            case_groups.push(crate::analysis::control_flow_plan::CaseGroupStructure {
                cases: switch_cases,
                body,
                fallthrough,
                needs_break,
            });
        }

        // Sort by execution order of first case in each group
        case_groups.sort_by_key(|group| {
            group
                .cases
                .iter()
                .map(|c| c.execution_order)
                .min()
                .unwrap_or(usize::MAX)
        });

        // Detect fallthrough patterns
        self.detect_fallthrough_patterns(&mut case_groups, info);

        case_groups
    }

    /// Build case groups from switch region (fallback when no switch info)
    fn build_case_groups_from_region(
        &mut self,
        region: &SwitchRegion,
    ) -> Vec<crate::analysis::control_flow_plan::CaseGroupStructure> {
        let mut case_groups = Vec::new();

        for case in &region.cases {
            // Use the case body analysis if available
            let body = if let Some(case_analysis) = region.case_analyses.get(&case.case_index) {
                // Mark all blocks in the case as processed
                for &block in &case_analysis.blocks {
                    self.processed_blocks.insert(block);
                }

                // Build structure from the analyzed case body
                self.build_from_case_body_analysis(case_analysis)
            } else {
                // Fallback to simple sequential structure
                self.processed_blocks.insert(case.case_head);
                self.build_sequential_structure(case.case_head)
            };

            // Create a single case for this group
            let switch_case = crate::analysis::control_flow_plan::SwitchCase {
                keys: vec![], // Unknown keys
                comparison_block: None,
                setup_instructions: vec![],
                execution_order: case.case_index,
            };

            // Determine if this case needs a break statement
            let termination = self.check_structure_termination(&body);
            let needs_break = if let Some(termination) = termination {
                log::debug!("Case terminates with {:?}, no break needed", termination);
                crate::analysis::control_flow_plan::BreakRequirement::NotNeeded {
                    reason: termination,
                }
            } else {
                log::debug!("Case does not terminate, break required");
                crate::analysis::control_flow_plan::BreakRequirement::Required
            };

            case_groups.push(crate::analysis::control_flow_plan::CaseGroupStructure {
                cases: vec![switch_case],
                body,
                fallthrough: None,
                needs_break,
            });
        }

        case_groups
    }

    /// Build structure from case body analysis
    fn build_from_case_body_analysis(
        &mut self,
        analysis: &crate::cfg::analysis::CaseBodyAnalysis,
    ) -> StructureId {
        let mut elements = Vec::new();
        let mut processed_in_body = HashSet::new();

        // Filter out catch blocks from the case body
<<<<<<< HEAD
        let filtered_blocks: Vec<_> = analysis
            .blocks
            .iter()
            .copied()
            .filter(|b| !self.catch_blocks.contains(b))
            .collect();

=======
        let filtered_blocks: Vec<_> = analysis.blocks.iter()
            .copied()
            .filter(|b| !self.catch_blocks.contains(b))
            .collect();
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Sort blocks by index to process them in order
        let mut sorted_blocks = filtered_blocks;
        sorted_blocks.sort_by_key(|b| b.index());

        // Process blocks in order, detecting nested control flow
        for &block in &sorted_blocks {
            if processed_in_body.contains(&block) {
                continue;
            }

            // Check if this block starts a nested switch
            if let Some(nested_switch) = analysis
                .nested_switches
                .iter()
                .find(|s| s.dispatch == block)
            {
                // Mark all blocks in the nested switch as processed
                processed_in_body.insert(nested_switch.dispatch);
                for case in &nested_switch.cases {
                    processed_in_body.insert(case.case_head);
                    if let Some(ref case_analysis) =
                        nested_switch.case_analyses.get(&case.case_index)
                    {
                        processed_in_body.extend(&case_analysis.blocks);
                    }
                }
                if let Some(default) = nested_switch.default_head {
                    processed_in_body.insert(default);
                }
                processed_in_body.insert(nested_switch.join_block);

                // Build the nested switch structure
                let switch_structure = self.build_switch_from_region(nested_switch.clone());
                elements.push(SequentialElement::Structure(switch_structure));
                continue;
            }

            // Check if this block starts a conditional chain
            if let Some(chain) = analysis
                .conditionals
                .chains
                .iter()
                .find(|c| c.branches.first().map(|b| b.condition_source) == Some(block))
            {
                // Mark all blocks in the conditional as processed
                for branch in &chain.branches {
                    processed_in_body.insert(branch.condition_source);
                    processed_in_body.insert(branch.condition_block);
                    processed_in_body.insert(branch.branch_entry);
                }

                // Check if this conditional chain is actually a sparse switch
                // A sparse switch has a specific pattern: sequential equality comparisons on the same variable
                if let Some(switch_structure) = self.try_build_sparse_switch_from_chain(chain) {
                    elements.push(SequentialElement::Structure(switch_structure));
                } else {
                    // Build as a regular conditional structure
                    let conditional_structure = self.build_conditional_structure(chain.clone());
                    elements.push(SequentialElement::Structure(conditional_structure));
                }
                continue;
            }

            // Check if this block starts a loop
            if let Some(loop_info) = analysis
                .loops
                .loops
                .iter()
                .find(|l| l.primary_header() == block)
            {
                // Mark all blocks in the loop as processed
                processed_in_body.extend(&loop_info.body_nodes);
                for &header in &loop_info.headers {
                    processed_in_body.insert(header);
                }

                // Build the loop structure (simplified for now)
                let loop_body = self.build_sequential_from_blocks(&loop_info.body_nodes);

                // Convert cfg LoopType to control_flow_plan LoopType
                let loop_type = match loop_info.loop_type {
                    crate::cfg::analysis::LoopType::While => {
                        crate::analysis::control_flow_plan::LoopType::While
                    }
                    crate::cfg::analysis::LoopType::DoWhile => {
                        crate::analysis::control_flow_plan::LoopType::DoWhile
                    }
                    crate::cfg::analysis::LoopType::For => {
                        crate::analysis::control_flow_plan::LoopType::For
                    }
                };

                // Extract condition info from the loop header
                let header = loop_info.primary_header();
                let (condition, condition_use) = self.extract_loop_condition_info(header);

                let loop_structure = self.plan.create_structure(ControlFlowKind::Loop {
                    loop_type,
                    header_block: header,
                    condition,
                    condition_use,
                    body: loop_body,
                    update: None,       // TODO: Detect update statements for for-loops
                    break_target: None, // TODO: Track break targets
                    continue_target: None, // TODO: Track continue targets
                });
                elements.push(SequentialElement::Structure(loop_structure));
                continue;
            }

            // Regular block
            processed_in_body.insert(block);
            elements.push(SequentialElement::Block(block));
        }

        // Create the appropriate structure based on elements
        if elements.is_empty() {
            self.plan.create_structure(ControlFlowKind::Empty)
        } else if elements.len() == 1 {
            match elements[0] {
                SequentialElement::Block(block) => {
                    self.plan.create_structure(ControlFlowKind::BasicBlock {
                        block,
                        instruction_count: self.cfg.graph()[block].instructions().len(),
                        is_synthetic: false,
                    })
                }
                SequentialElement::Structure(id) => id,
            }
        } else {
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        }
    }

    /// Build a sequential structure from a set of blocks
    fn build_sequential_from_blocks(&mut self, blocks: &HashSet<NodeIndex>) -> StructureId {
        // Sort blocks by index
        let mut sorted_blocks: Vec<_> = blocks.iter().copied().collect();
        sorted_blocks.sort_by_key(|b| b.index());

        let elements: Vec<_> = sorted_blocks
            .iter()
            .map(|&block| SequentialElement::Block(block))
            .collect();

        if elements.is_empty() {
            self.plan.create_structure(ControlFlowKind::Empty)
        } else if elements.len() == 1 {
            if let SequentialElement::Block(block) = elements[0] {
                self.plan.create_structure(ControlFlowKind::BasicBlock {
                    block,
                    instruction_count: self.cfg.graph()[block].instructions().len(),
                    is_synthetic: false,
                })
            } else {
                self.plan.create_structure(ControlFlowKind::Empty)
            }
        } else {
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        }
    }

    /// Try to build a sparse switch from a conditional chain
    /// Returns Some if the chain represents a sparse switch pattern, None otherwise
    fn try_build_sparse_switch_from_chain(
        &mut self,
        chain: &ConditionalChain,
    ) -> Option<StructureId> {
        // Check if we have a sparse switch pattern for this chain
        // We need to find a sparse switch candidate that starts at the same block
        if let Some(first_branch) = chain.branches.first() {
            // Try to detect a sparse switch starting from this block
            let mut processed = HashSet::new();
            if let Some(candidate) =
                crate::cfg::switch_analysis::sparse_switch_detector::detect_sparse_switch_chain(
                    self.cfg.graph(),
                    &self.cfg.analyze_post_dominators()?,
                    first_branch.condition_source,
                    &mut processed,
                    self.cfg.hbc_file(),
                    &self.function_analysis.ssa.ssa_values,
                    self.cfg,
                    &self.function_analysis.ssa,
                )
            {
                // Convert the sparse switch candidate to a switch region
                let mut region =
                    crate::cfg::switch_analysis::sparse_candidate_to_switch_region(&candidate);

                // Analyze case bodies
                region.analyze_case_bodies(self.cfg.graph(), self.cfg, None);

                // Build the switch structure from this region
                return Some(self.build_switch_from_region(region));
            }
        }

        None
    }

    /// Build a conditional structure
    fn build_conditional_structure(&mut self, chain: ConditionalChain) -> StructureId {
        // Build a simple if-else from the first branch, applying inversion if needed
        if let Some(first_branch) = chain.branches.first() {
            self.processed_blocks.insert(first_branch.condition_source);
            self.processed_blocks.insert(first_branch.condition_block);

            // Build the true branch - use all blocks identified in the branch
            let original_true_branch = if first_branch.branch_blocks.is_empty() {
                self.plan.create_structure(ControlFlowKind::Empty)
            } else if first_branch.branch_blocks.len() == 1 {
                // Single block branch
                let block = first_branch.branch_blocks[0];
                self.processed_blocks.insert(block);
                self.plan.create_structure(ControlFlowKind::BasicBlock {
                    block,
                    instruction_count: self.cfg.graph()[block].instructions().len(),
                    is_synthetic: false,
                })
            } else {
                // Multiple blocks - build from the identified blocks
                self.build_branch_from_blocks(&first_branch.branch_blocks)
            };

            // Build the false branch - handle else/else-if branches
            let original_false_branch = if chain.branches.len() > 1 {
                if let Some(second_branch) = chain.branches.get(1) {
                    if second_branch.branch_blocks.is_empty() {
                        Some(self.plan.create_structure(ControlFlowKind::Empty))
                    } else if second_branch.branch_blocks.len() == 1 {
                        let block = second_branch.branch_blocks[0];
                        self.processed_blocks.insert(block);
                        Some(self.plan.create_structure(ControlFlowKind::BasicBlock {
                            block,
                            instruction_count: self.cfg.graph()[block].instructions().len(),
                            is_synthetic: false,
                        }))
                    } else {
                        Some(self.build_branch_from_blocks(&second_branch.branch_blocks))
                    }
                } else {
                    None
                }
            } else {
                None
            };

            // Don't mark join block as processed here - it should be added as a separate block
            // after the conditional structure
            // self.processed_blocks.insert(chain.join_block);

            // Extract condition information from the condition block
            let condition_expr = self.extract_condition_info(first_branch.condition_block);

            // Apply conditional inversion if needed
            let (final_condition_expr, true_branch, false_branch) = if chain.should_invert {
                // Invert the condition and swap the branches
                let inverted_condition = condition_expr.map(|expr| expr.invert());
                (
                    inverted_condition,
                    original_false_branch,
                    Some(original_true_branch),
                )
            } else {
                // Keep original order
                (
                    condition_expr,
                    Some(original_true_branch),
                    original_false_branch,
                )
            };

            let kind = ControlFlowKind::Conditional {
                condition_block: first_branch.condition_block,
                condition_expr: final_condition_expr,
                true_branch: true_branch
                    .unwrap_or_else(|| self.plan.create_structure(ControlFlowKind::Empty)),
                false_branch,
            };

            self.plan.create_structure(kind)
        } else {
            // Empty conditional?
            self.plan.create_structure(ControlFlowKind::Empty)
        }
    }

    /// Extract condition expression from a condition block
    fn extract_condition_info(&self, condition_block: NodeIndex) -> Option<ComparisonExpression> {
        // Get the block
        let block = match self.cfg.graph().node_weight(condition_block) {
            Some(b) => b,
            None => return None,
        };

        // Jump instructions always terminate blocks, so look at the last instruction
        let last_inst = match block.instructions.last() {
            Some(inst) => inst,
            None => return None,
        };

        let pc = last_inst.instruction_index;

        // Helper to create SSAValue and RegisterUse for a register
        let create_ssa_info = |register: u8| -> Option<(SSAValue, RegisterUse)> {
            let register_use = RegisterUse::new(register, condition_block, pc);

            // Get the SSA value for this register use
            let ssa_value = if let Some(def_site) =
                self.function_analysis.ssa.use_def_chains.get(&register_use)
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

            ssa_value.map(|ssav| (ssav, register_use))
        };

        // Extract condition information based on the jump type
        match &last_inst.instruction {
            // Simple register condition jumps
            UnifiedInstruction::JmpTrue { operand_1, .. }
            | UnifiedInstruction::JmpTrueLong { operand_1, .. }
            | UnifiedInstruction::JmpFalse { operand_1, .. }
            | UnifiedInstruction::JmpFalseLong { operand_1, .. }
            | UnifiedInstruction::JmpUndefined { operand_1, .. }
            | UnifiedInstruction::JmpUndefinedLong { operand_1, .. } => {
                if let Some((ssa_value, register_use)) = create_ssa_info(*operand_1) {
                    Some(ComparisonExpression::SimpleCondition {
                        operand: ssa_value,
                        operand_use: register_use,
                    })
                } else {
                    None
                }
            }

            // Binary comparison jumps - now we extract both operands and the operator
            UnifiedInstruction::JEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::Equality,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::Inequality,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JStrictEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JStrictEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::StrictEquality,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JStrictNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JStrictNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::StrictInequality,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JLess {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::LessThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JNotLess {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::GreaterEqualThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JLessEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessEqualLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessEqualN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessEqualNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::LessEqualThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JNotLessEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessEqualLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessEqualN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessEqualNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::GreaterThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JGreater {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::GreaterThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JNotGreater {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::LessEqualThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JGreaterEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterEqualLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterEqualN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterEqualNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::GreaterEqualThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),
            UnifiedInstruction::JNotGreaterEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterEqualLong {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterEqualN {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterEqualNLong {
                operand_1,
                operand_2,
                ..
            } => self.create_binary_comparison(
                BinaryOperator::LessThan,
                *operand_1,
                *operand_2,
                condition_block,
                pc,
            ),

            // Unconditional jumps - no condition
            UnifiedInstruction::Jmp { .. } | UnifiedInstruction::JmpLong { .. } => None,

            // Any other instruction shouldn't be terminating a conditional block
            _ => None,
        }
    }

    /// Helper function to create a binary comparison expression
    fn create_binary_comparison(
        &self,
        operator: BinaryOperator,
        operand_1: u8,
        operand_2: u8,
        condition_block: NodeIndex,
        pc: InstructionIndex,
    ) -> Option<ComparisonExpression> {
        // Create RegisterUse for both operands
        let left_use = RegisterUse::new(operand_1, condition_block, pc);
        let right_use = RegisterUse::new(operand_2, condition_block, pc);

        // Get SSA values for both operands
        let left_ssa =
            if let Some(def_site) = self.function_analysis.ssa.use_def_chains.get(&left_use) {
                self.function_analysis
                    .ssa
                    .ssa_values
                    .values()
                    .find(|v| &v.def_site == def_site)
                    .cloned()
            } else {
                None
            };

        let right_ssa =
            if let Some(def_site) = self.function_analysis.ssa.use_def_chains.get(&right_use) {
                self.function_analysis
                    .ssa
                    .ssa_values
                    .values()
                    .find(|v| &v.def_site == def_site)
                    .cloned()
            } else {
                None
            };

        // Only create the comparison if we have both SSA values
        if let (Some(left), Some(right)) = (left_ssa, right_ssa) {
            Some(ComparisonExpression::BinaryComparison {
                operator,
                left,
                left_use,
                right,
                right_use,
            })
        } else {
            None
        }
    }

    /// Extract condition SSAValue and RegisterUse for loops (backward compatibility)
    fn extract_loop_condition_info(
        &self,
        condition_block: NodeIndex,
    ) -> (Option<SSAValue>, Option<RegisterUse>) {
        // Get comparison expression and extract the first operand for backward compatibility
        if let Some(comparison) = self.extract_condition_info(condition_block) {
            match comparison {
                ComparisonExpression::SimpleCondition {
                    operand,
                    operand_use,
                } => (Some(operand), Some(operand_use)),
                ComparisonExpression::BinaryComparison { left, left_use, .. } => {
                    // For loops, just return the left operand for now
                    (Some(left), Some(left_use))
                }
            }
        } else {
            (None, None)
        }
    }

    /// Find the next sequential block after a given block
    fn find_next_sequential_block(&self, block: NodeIndex) -> Option<NodeIndex> {
        // Get successors
        let successors: Vec<_> = self
            .cfg
            .graph()
            .edges(block)
            .map(|edge| edge.target())
            .collect();

<<<<<<< HEAD
        // If there's exactly one successor and it's not already processed or a catch block,
        // that's our next block
        if successors.len() == 1
            && !self.processed_blocks.contains(&successors[0])
            && !self.catch_blocks.contains(&successors[0])
        {
=======
        // If there's exactly one successor and it's not already processed or a catch block, 
        // that's our next block
        if successors.len() == 1 
            && !self.processed_blocks.contains(&successors[0])
            && !self.catch_blocks.contains(&successors[0]) {
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            Some(successors[0])
        } else {
            None
        }
    }

    /// Find the next block after a control flow structure
    fn find_next_block_after_structure(&self, structure_id: StructureId) -> Option<NodeIndex> {
        // Get the structure
        if let Some(structure) = self.plan.get_structure(structure_id) {
            // The exit blocks of the structure might lead to the next sequential block
            if structure.exit_blocks.len() == 1 {
                self.find_next_sequential_block(structure.exit_blocks[0])
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Detect fallthrough patterns between case groups
    fn detect_fallthrough_patterns(
        &self,
        case_groups: &mut Vec<crate::analysis::control_flow_plan::CaseGroupStructure>,
        info: &SwitchInfo,
    ) {
        // First, we need to create CaseGroup objects from our CaseGroupStructures
        // to use with the fallthrough analysis
        let switch_case_groups: Vec<crate::cfg::switch_analysis::CaseGroup> = case_groups
            .iter()
            .filter_map(|group| {
                // Get the first case to find the target block
                group.cases.first().and_then(|first_case| {
                    // Find the corresponding case info to get the target block
                    info.cases
                        .iter()
                        .find(|ci| ci.keys == first_case.keys)
                        .map(|case_info| crate::cfg::switch_analysis::CaseGroup {
                            keys: first_case.keys.clone(),
                            target_block: case_info.target_block,
                            setup: case_info.setup.clone(),
                            always_terminates: case_info.always_terminates,
                            first_execution_order: first_case.execution_order,
                            comparison_blocks: vec![case_info.comparison_block],
                        })
                })
            })
            .collect();

        // Use the proper fallthrough analysis from cfg/switch_analysis
        let fallthrough_analysis = crate::cfg::switch_analysis::FallthroughAnalysis::analyze(
            &switch_case_groups,
            info,
            self.cfg,
        );

        // Apply the fallthrough information to our case groups
        for (i, fallthrough_target) in fallthrough_analysis.fallthrough_targets.iter().enumerate() {
            if let Some(target_idx) = fallthrough_target {
                if i < case_groups.len() {
                    let duplication_context =
                        crate::cfg::ssa::types::DuplicationContext::SwitchFallthrough {
                            from_case_index: i,
                            to_case_index: *target_idx,
                        };

                    // Get blocks to duplicate from the target group
                    let blocks_to_duplicate = if *target_idx < case_groups.len() {
                        if let Some(target_structure) =
                            self.plan.get_structure(case_groups[*target_idx].body)
                        {
                            target_structure.get_all_blocks()
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    };

                    case_groups[i].fallthrough =
                        Some(crate::analysis::control_flow_plan::FallthroughInfo {
                            to_case_index: *target_idx,
                            blocks_to_duplicate,
                            duplication_context,
                        });
                    // Update needs_break to indicate fallthrough is intended
                    case_groups[i].needs_break =
                        crate::analysis::control_flow_plan::BreakRequirement::FallthroughIntended;
                }
            }
        }
    }

    /// Check if a switch region uses a dense switch (SwitchImm instruction)
    fn is_dense_switch(&self, region: &SwitchRegion) -> bool {
        let dispatch_block = &self.cfg.graph()[region.dispatch];

        // Check if the dispatch block contains a SwitchImm instruction
        dispatch_block.instructions().iter().any(|instr| {
            matches!(
                &instr.instruction,
                crate::generated::unified_instructions::UnifiedInstruction::SwitchImm { .. }
            )
        })
    }

    /// Detect dense switch pattern (SwitchImm) for a switch region
    fn detect_dense_switch_pattern(&self, region: &SwitchRegion) -> Option<SwitchInfo> {
        let dispatch_block = &self.cfg.graph()[region.dispatch];

        // Find the SwitchImm instruction
        let switch_instr = dispatch_block.instructions().iter().find(|instr| {
            matches!(
                &instr.instruction,
                crate::generated::unified_instructions::UnifiedInstruction::SwitchImm { .. }
            )
        })?;

        // Extract the SwitchImm details
        if let crate::generated::unified_instructions::UnifiedInstruction::SwitchImm {
            operand_0: switch_register,
            operand_3: _min_value,
            operand_4: _max_value,
            ..
        } = &switch_instr.instruction
        {
            // Get the switch table from the HBC file
            let switch_table = self
                .function_analysis
                .hbc_file
                .switch_tables
                .get_switch_table_by_instruction(
                    self.function_analysis.function_index,
                    switch_instr.instruction_index.into(),
                )?;

            // Build case info from the switch table
            let mut cases = Vec::new();
            let mut execution_order = 0;
            for case in &switch_table.cases {
                // Each case in a dense switch has a single key value
                let keys = vec![crate::cfg::switch_analysis::CaseKey::Number(
                    ordered_float::OrderedFloat(case.value as f64),
                )];

                // The target block is determined by the case's target instruction index
                if let Some(target_inst_idx) = case.target_instruction_index {
                    // Find the block that contains this instruction
                    let target_block = self.cfg.graph().node_indices().find(|&idx| {
                        let block = &self.cfg.graph()[idx];
                        block
                            .instructions()
                            .iter()
                            .any(|instr| instr.instruction_index == target_inst_idx.into())
                    })?;

                    cases.push(crate::cfg::switch_analysis::CaseInfo {
                        keys,
                        comparison_block: region.dispatch, // Dense switch doesn't have separate comparison blocks
                        target_block,
                        setup: smallvec::SmallVec::new(),
                        always_terminates: false,
                        execution_order,
                    });
                    execution_order += 1;
                }
            }

            // Handle default case
            let default_case =
                if let Some(default_inst_idx) = switch_table.default_instruction_index {
                    // Find the block that contains the default target instruction
                    let default_block = self.cfg.graph().node_indices().find(|&idx| {
                        let block = &self.cfg.graph()[idx];
                        block
                            .instructions()
                            .iter()
                            .any(|instr| instr.instruction_index == default_inst_idx.into())
                    })?;

                    Some(crate::cfg::switch_analysis::DefaultCase {
                        target_block: default_block,
                        setup: smallvec::SmallVec::new(),
                    })
                } else {
                    None
                };

            Some(crate::cfg::switch_analysis::SwitchInfo {
                discriminator: *switch_register,
                discriminator_instruction_index: switch_instr.instruction_index,
                cases,
                default_case,
                shared_tail: None, // Dense switches typically don't have shared tails in the same way
            })
        } else {
            None
        }
    }

    /// Detect sparse switch pattern for a switch region
    fn detect_sparse_switch_pattern(&self, region: &SwitchRegion) -> Option<SwitchInfo> {
        // Check if dispatch block contains comparisons (sparse switch)
        let dispatch_block = &self.cfg.graph()[region.dispatch];
        let mut is_sparse = false;

        for instr in dispatch_block.instructions() {
            match &instr.instruction {
                crate::generated::unified_instructions::UnifiedInstruction::JStrictEqual {
                    ..
                }
                | crate::generated::unified_instructions::UnifiedInstruction::JStrictEqualLong {
                    ..
                } => {
                    is_sparse = true;
                    break;
                }
                _ => {}
            }
        }

        if is_sparse {
            // Try to detect the full pattern
            let analyzer = crate::cfg::switch_analysis::SparseSwitchAnalyzer::with_hbc_file(
                self.function_analysis.hbc_file,
            );

            if let Some(postdom) = self.cfg.analyze_post_dominators() {
                analyzer.detect_switch_pattern(
                    region.dispatch,
                    self.cfg,
                    &self.function_analysis.ssa,
                    &postdom,
                )
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Check if a structure always terminates
    fn check_structure_termination(
        &self,
        structure_id: &StructureId,
    ) -> Option<crate::analysis::control_flow_plan::TerminationReason> {
        // First check using the plan's built-in method
        if let Some(reason) = self.plan.structure_always_terminates(*structure_id) {
            log::debug!(
                "Structure {:?} terminates via plan method: {:?}",
                structure_id,
                reason
            );
            return Some(reason);
        }

        // If that didn't work, check if it's a basic block and analyze its instructions
        if let Some(structure) = self.plan.get_structure(*structure_id) {
            log::debug!(
                "Checking termination for structure kind: {:?}",
                std::mem::discriminant(&structure.kind)
            );
            match &structure.kind {
                crate::analysis::control_flow_plan::ControlFlowKind::BasicBlock {
                    block, ..
                } => {
                    // Check the last instruction of the block
                    if let Some(last_instr) = self.cfg.graph()[*block].instructions().last() {
                        match &last_instr.instruction {
                            crate::generated::unified_instructions::UnifiedInstruction::Ret {
                                ..
                            } => {
                                return Some(
                                    crate::analysis::control_flow_plan::TerminationReason::Return,
                                );
                            }
                            crate::generated::unified_instructions::UnifiedInstruction::Throw {
                                ..
                            } => {
                                return Some(
                                    crate::analysis::control_flow_plan::TerminationReason::Throw,
                                );
                            }
                            _ => {}
                        }
                    }
                }
                crate::analysis::control_flow_plan::ControlFlowKind::Sequential { elements } => {
                    // For Sequential structures, check if any block terminates
                    log::debug!("Sequential structure has {} elements", elements.len());

                    // Check elements from the end, looking for termination
                    for element in elements.iter().rev() {
                        match element {
                            crate::analysis::control_flow_plan::SequentialElement::Block(block) => {
                                log::debug!("Checking Block({})", block.index());
                                let instructions = self.cfg.graph()[*block].instructions();
                                log::debug!("Block has {} instructions", instructions.len());

                                // Skip empty blocks (like join blocks)
                                if instructions.is_empty() {
                                    log::debug!("Block is empty, skipping");
                                    continue;
                                }

                                if let Some(last_instr) = instructions.last() {
                                    log::debug!(
                                        "Last instruction: {:?}",
                                        std::mem::discriminant(&last_instr.instruction)
                                    );
                                    match &last_instr.instruction {
                                        crate::generated::unified_instructions::UnifiedInstruction::Ret { .. } => {
                                            log::debug!("Found Ret instruction - structure terminates");
                                            return Some(crate::analysis::control_flow_plan::TerminationReason::Return);
                                        }
                                        crate::generated::unified_instructions::UnifiedInstruction::Throw { .. } => {
                                            log::debug!("Found Throw instruction - structure terminates");
                                            return Some(crate::analysis::control_flow_plan::TerminationReason::Throw);
                                        }
                                        _ => {
                                            // If we found a non-terminal non-empty block, stop checking
                                            log::debug!("Found non-terminal instruction, structure does not terminate");
                                            return None;
                                        }
                                    }
                                }
                            }
                            crate::analysis::control_flow_plan::SequentialElement::Structure(
                                sid,
                            ) => {
                                log::debug!("Checking Structure({:?})", sid);
                                // Check if this structure terminates
                                if let Some(reason) = self.check_structure_termination(sid) {
                                    return Some(reason);
                                }
                                // If the structure doesn't terminate, keep checking earlier elements
                            }
                        }
                    }

                    // No termination found
                }
                _ => {}
            }
        }

        None
    }

    /// Detect if a block contains a dense switch (SwitchImm instruction)
    fn detect_dense_switch_at_block(&self, block: NodeIndex) -> Option<SwitchRegion> {
        let block_data = &self.cfg.graph()[block];

        // Check if the block contains a SwitchImm instruction
        for instr in block_data.instructions() {
            if matches!(&instr.instruction, UnifiedInstruction::SwitchImm { .. }) {
                // For now, we'll need to analyze the switch pattern
                // This is already handled by the global analysis, so we'll skip for now
                // TODO: Implement local dense switch detection
                return None;
            }
        }

        None
    }

    /// Convert SwitchInfo to SwitchRegion for building
    fn switch_info_to_region(
        &self,
        switch_info: SwitchInfo,
        dispatch_block: NodeIndex,
    ) -> SwitchRegion {
        use crate::cfg::analysis::SwitchCase;

        // Convert SwitchInfo cases to SwitchCase format
        let cases = switch_info
            .cases
            .iter()
            .map(|case_info| SwitchCase {
                case_index: case_info.execution_order,
                case_head: case_info.target_block,
            })
            .collect();

        // Determine the join block (where control flow merges after the switch)
        // This is typically the shared tail if it exists
        let join_block = if let Some(ref tail) = switch_info.shared_tail {
            tail.block_id
        } else {
            // Find a common post-dominator or use a dummy exit block
            NodeIndex::new(self.cfg.graph().node_count())
        };

        // Determine default head
        let default_head = switch_info.default_case.as_ref().map(|dc| dc.target_block);

        SwitchRegion {
            dispatch: dispatch_block,
            cases,
            default_head,
            join_block,
            case_analyses: std::collections::HashMap::new(),
        }
    }

    /// Build control flow plan from exception handler analysis
    fn build_from_exception_analysis(
        &mut self,
        analysis: &crate::cfg::exception_analysis::ExceptionAnalysis,
    ) -> StructureId {
        // Find the earliest instruction covered by any exception region
<<<<<<< HEAD
        let earliest_try_start = analysis
            .regions
            .iter()
            .map(|r| r.try_start_idx)
            .min()
            .unwrap_or(0);

=======
        let earliest_try_start = analysis.regions.iter()
            .map(|r| r.try_start_idx)
            .min()
            .unwrap_or(0);
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Check if there are any blocks before the exception regions
        let mut preamble_blocks = Vec::new();
        for node in self.cfg.graph().node_indices() {
            let block = &self.cfg.graph()[node];
            // If this block ends before or at the earliest try start, it's a preamble block
            if block.end_pc().0 <= earliest_try_start as usize {
                preamble_blocks.push(node);
            }
        }
<<<<<<< HEAD

        // Sort preamble blocks by their start PC
        preamble_blocks.sort_by_key(|&node| self.cfg.graph()[node].start_pc());

=======
        
        // Sort preamble blocks by their start PC
        preamble_blocks.sort_by_key(|&node| self.cfg.graph()[node].start_pc());
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Build the exception structure
        let exception_structure = if analysis.regions.len() == 1 {
            let region = &analysis.regions[0];
            self.build_try_catch_structure(region)
        } else if analysis.regions.len() > 1 {
            // Build nested exception regions recursively
            self.build_nested_exception_structures(&analysis.regions)
        } else {
            // Fallback to sequential structure
            let entry_block = NodeIndex::new(0);
            self.build_sequential_structure(entry_block)
        };
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // If we have preamble blocks, create a sequential structure
        if !preamble_blocks.is_empty() {
            use crate::analysis::control_flow_plan::SequentialElement;
            let mut elements = Vec::new();
<<<<<<< HEAD

=======
            
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            // Add each preamble block as a structure
            for block in preamble_blocks {
                self.processed_blocks.insert(block);
                let block_structure = self.plan.create_structure(ControlFlowKind::BasicBlock {
                    block,
                    instruction_count: self.cfg.graph()[block].instructions().len(),
                    is_synthetic: false,
                });
                elements.push(SequentialElement::Structure(block_structure));
            }
<<<<<<< HEAD

            // Add the exception structure
            elements.push(SequentialElement::Structure(exception_structure));

            // Create and return the sequential structure
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
=======
            
            // Add the exception structure
            elements.push(SequentialElement::Structure(exception_structure));
            
            // Create and return the sequential structure
            self.plan.create_structure(ControlFlowKind::Sequential { elements })
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        } else {
            exception_structure
        }
    }
<<<<<<< HEAD

=======
    
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
    /// Build nested exception structures
    fn build_nested_exception_structures(
        &mut self,
        regions: &[crate::cfg::exception_analysis::ExceptionRegion],
    ) -> StructureId {
        // Sort regions by their start index (and reverse end index for proper nesting order)
        let mut sorted_regions = regions.to_vec();
        sorted_regions.sort_by_key(|r| (r.try_start_idx, std::cmp::Reverse(r.try_end_idx)));
<<<<<<< HEAD

        // Find the outermost region (widest range)
        let outermost = sorted_regions
            .iter()
=======
        
        // Find the outermost region (widest range)
        let outermost = sorted_regions.iter()
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            .enumerate()
            .max_by_key(|(_, r)| r.try_end_idx - r.try_start_idx)
            .map(|(idx, _)| idx)
            .unwrap_or(0);
<<<<<<< HEAD

        // Build the outermost structure
        self.build_try_catch_structure_with_nested(&sorted_regions[outermost], &sorted_regions)
    }

=======
        
        // Build the outermost structure
        self.build_try_catch_structure_with_nested(&sorted_regions[outermost], &sorted_regions)
    }
    
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
    /// Build try-catch structure with potential nested regions
    fn build_try_catch_structure_with_nested(
        &mut self,
        region: &crate::cfg::exception_analysis::ExceptionRegion,
        all_regions: &[crate::cfg::exception_analysis::ExceptionRegion],
    ) -> StructureId {
        // Find any regions nested inside this one
<<<<<<< HEAD
        let nested_regions: Vec<_> = all_regions
            .iter()
=======
        let nested_regions: Vec<_> = all_regions.iter()
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            .filter(|r| {
                // A region is nested if it's completely inside this region's try blocks
                std::ptr::eq(*r, region) == false &&  // Not the same region
                r.try_start_idx >= region.try_start_idx &&
                r.try_end_idx <= region.try_end_idx
            })
            .cloned()
            .collect();
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        if !nested_regions.is_empty() {
            // Build the nested structure recursively
            let nested_structure = if nested_regions.len() == 1 {
                self.build_try_catch_structure(&nested_regions[0])
            } else {
                self.build_nested_exception_structures(&nested_regions)
            };
<<<<<<< HEAD

            // Now build this region with the nested structure as part of the try body
            return self.build_try_catch_with_nested_structure(region, nested_structure);
        }

        // No nested regions, build normally
        self.build_try_catch_structure(region)
    }

=======
            
            // Now build this region with the nested structure as part of the try body  
            return self.build_try_catch_with_nested_structure(region, nested_structure);
        }
        
        // No nested regions, build normally
        self.build_try_catch_structure(region)
    }
    
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
    /// Build try-catch with a nested structure in the try body
    fn build_try_catch_with_nested_structure(
        &mut self,
        region: &crate::cfg::exception_analysis::ExceptionRegion,
        nested_structure: StructureId,
    ) -> StructureId {
        use crate::analysis::control_flow_plan::CatchClause;
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Find blocks that belong to the outer try but not the nested region
        // These are blocks that come before the nested try-catch
        let mut outer_only_blocks = Vec::new();
        for &block in &region.try_blocks {
            // Check if this block is part of the nested structure's try blocks
            // If not, it belongs to the outer try only
            if !self.processed_blocks.contains(&block) {
                outer_only_blocks.push(block);
                self.processed_blocks.insert(block);
            }
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Build the try body including both outer-only blocks and nested structure
        let try_body = if outer_only_blocks.is_empty() {
            nested_structure
        } else {
            use crate::analysis::control_flow_plan::SequentialElement;
<<<<<<< HEAD

            // Create a sequential structure with outer blocks followed by nested structure
            let mut elements = Vec::new();

=======
            
            // Create a sequential structure with outer blocks followed by nested structure
            let mut elements = Vec::new();
            
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            // Add outer-only blocks
            for block in outer_only_blocks {
                let block_structure = self.plan.create_structure(ControlFlowKind::BasicBlock {
                    block,
                    instruction_count: self.cfg.graph()[block].instructions().len(),
                    is_synthetic: false,
                });
                elements.push(SequentialElement::Structure(block_structure));
            }
<<<<<<< HEAD

            // Add the nested structure
            elements.push(SequentialElement::Structure(nested_structure));

            // Create sequential structure
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        };

        // Build catch clause if present
        let catch_clause = if let Some(ref catch_handler) = region.catch_handler {
            self.processed_blocks.insert(catch_handler.catch_block);

            let catch_body = self.plan.create_structure(ControlFlowKind::BasicBlock {
                block: catch_handler.catch_block,
                instruction_count: self.cfg.graph()[catch_handler.catch_block]
                    .instructions()
                    .len()
                    - 1, // Skip Catch instruction
                is_synthetic: false,
            });

            // Look up the actual SSA value for the Catch instruction
            let error_ssa_value =
                self.find_catch_ssa_value(catch_handler.catch_block, catch_handler.error_register);

=======
            
            // Add the nested structure
            elements.push(SequentialElement::Structure(nested_structure));
            
            // Create sequential structure
            self.plan.create_structure(ControlFlowKind::Sequential { elements })
        };
        
        // Build catch clause if present
        let catch_clause = if let Some(ref catch_handler) = region.catch_handler {
            self.processed_blocks.insert(catch_handler.catch_block);
            
            let catch_body = self.plan.create_structure(ControlFlowKind::BasicBlock {
                block: catch_handler.catch_block,
                instruction_count: self.cfg.graph()[catch_handler.catch_block].instructions().len() - 1, // Skip Catch instruction
                is_synthetic: false,
            });
            
            // Look up the actual SSA value for the Catch instruction
            let error_ssa_value = self.find_catch_ssa_value(
                catch_handler.catch_block,
                catch_handler.error_register
            );
            
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            Some(CatchClause {
                catch_block: catch_handler.catch_block,
                error_register: catch_handler.error_register,
                error_ssa_value,
                body: catch_body,
            })
        } else {
            None
        };
<<<<<<< HEAD

        // Build finally clause if present
        let finally_clause = region
            .finally_block
            .map(|block| self.build_finally_clause(block));

=======
        
        // Build finally clause if present
        let finally_clause = region.finally_block.map(|block| self.build_finally_clause(block));
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Create the try-catch structure
        self.plan.create_structure(ControlFlowKind::TryCatch {
            try_body,
            catch_clause,
            finally_clause,
        })
    }

    /// Build a try-catch structure from exception region
    fn build_try_catch_structure(
        &mut self,
        region: &crate::cfg::exception_analysis::ExceptionRegion,
    ) -> StructureId {
        use crate::analysis::control_flow_plan::CatchClause;

        // Mark all try blocks and catch block as processed
        for &block in &region.try_blocks {
            self.processed_blocks.insert(block);
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        if let Some(ref catch_handler) = region.catch_handler {
            self.processed_blocks.insert(catch_handler.catch_block);
        }

        // Build the try body - check for patterns within the try blocks
        let try_body = if region.try_blocks.is_empty() {
            self.plan.create_structure(ControlFlowKind::Empty)
        } else {
            // Analyze patterns within the try blocks
            self.build_structure_from_blocks(&region.try_blocks)
        };

        // Build the catch clause
        let catch_clause = region.catch_handler.as_ref().map(|handler| {
            let catch_body = self.plan.create_structure(ControlFlowKind::BasicBlock {
                block: handler.catch_block,
                instruction_count: self.cfg.graph()[handler.catch_block].instructions().len(),
                is_synthetic: false,
            });

            // Look up the actual SSA value for the Catch instruction
<<<<<<< HEAD
            let error_ssa_value =
                self.find_catch_ssa_value(handler.catch_block, handler.error_register);

=======
            let error_ssa_value = self.find_catch_ssa_value(
                handler.catch_block,
                handler.error_register
            );
            
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            CatchClause {
                catch_block: handler.catch_block,
                error_register: handler.error_register,
                error_ssa_value,
                body: catch_body,
            }
        });

        // Handle finally block if detected
        let finally_clause = if let Some(finally_block) = region.finally_block {
            // Build the finally clause
            Some(self.build_finally_clause(finally_block))
        } else {
            None
        };

        // Create the try-catch structure
        self.plan.create_structure(ControlFlowKind::TryCatch {
            try_body,
            catch_clause,
            finally_clause,
        })
    }

    /// Build a branch structure from a set of blocks  
    fn build_branch_from_blocks(&mut self, blocks: &[NodeIndex]) -> StructureId {
        use crate::analysis::control_flow_plan::SequentialElement;
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Mark all blocks as processed
        for &block in blocks {
            self.processed_blocks.insert(block);
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Build sequential structure from the blocks
        if blocks.is_empty() {
            self.plan.create_structure(ControlFlowKind::Empty)
        } else if blocks.len() == 1 {
            let block = blocks[0];
            self.plan.create_structure(ControlFlowKind::BasicBlock {
                block,
                instruction_count: self.cfg.graph()[block].instructions().len(),
                is_synthetic: false,
            })
        } else {
            // Create sequential elements for all blocks
            let elements: Vec<SequentialElement> = blocks
                .iter()
                .map(|&block| SequentialElement::Block(block))
                .collect();
<<<<<<< HEAD
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        }
    }

    /// Build structure from a set of blocks, detecting patterns like switches
    fn build_structure_from_blocks(&mut self, blocks: &[NodeIndex]) -> StructureId {
        // Check if these blocks contain a switch pattern
        if let Some(switch_analysis) = self.cfg.analyze_switch_regions(&self.function_analysis.ssa)
        {
=======
            self.plan.create_structure(ControlFlowKind::Sequential { elements })
        }
    }
    
    /// Build structure from a set of blocks, detecting patterns like switches
    fn build_structure_from_blocks(&mut self, blocks: &[NodeIndex]) -> StructureId {
        // Check if these blocks contain a switch pattern
        if let Some(switch_analysis) = self.cfg.analyze_switch_regions(&self.function_analysis.ssa) {
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
            // Check if any switch region matches our blocks
            for region in &switch_analysis.regions {
                // Check if the switch dispatch block is in our blocks
                if blocks.contains(&region.dispatch) {
                    // Mark blocks as processed
                    for &block in blocks {
                        self.processed_blocks.insert(block);
                    }
                    // Build the switch structure
                    return self.build_switch_from_region(region.clone());
                }
            }
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Check for conditional patterns
        if let Some(conditional_analysis) = self.cfg.analyze_conditional_chains() {
            for chain in &conditional_analysis.chains {
                // Check if this chain's blocks overlap with our blocks
                let has_overlap = chain.branches.iter().any(|branch| {
<<<<<<< HEAD
                    blocks.contains(&branch.condition_block)
                        || branch.branch_blocks.iter().any(|b| blocks.contains(b))
                });

=======
                    blocks.contains(&branch.condition_block) || 
                    branch.branch_blocks.iter().any(|b| blocks.contains(b))
                });
                
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
                if has_overlap {
                    // Mark blocks as processed
                    for &block in blocks {
                        self.processed_blocks.insert(block);
                    }
                    // Build the conditional structure
                    return self.build_conditional_structure(chain.clone());
                }
            }
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Fallback: build sequential structure from blocks
        if blocks.len() == 1 {
            let block = blocks[0];
            self.plan.create_structure(ControlFlowKind::BasicBlock {
                block,
                instruction_count: self.cfg.graph()[block].instructions().len(),
                is_synthetic: false,
            })
        } else {
            use crate::analysis::control_flow_plan::SequentialElement;
            let elements: Vec<SequentialElement> = blocks
                .iter()
                .map(|&block| SequentialElement::Block(block))
                .collect();
<<<<<<< HEAD
            self.plan
                .create_structure(ControlFlowKind::Sequential { elements })
        }
    }

    /// Find the SSA value defined by the Catch instruction
    fn find_catch_ssa_value(
        &self,
        catch_block: NodeIndex,
        error_register: u8,
    ) -> crate::cfg::ssa::SSAValue {
        use crate::generated::unified_instructions::UnifiedInstruction;

        let block = &self.cfg.graph()[catch_block];

=======
            self.plan.create_structure(ControlFlowKind::Sequential { elements })
        }
    }
    
    /// Find the SSA value defined by the Catch instruction
    fn find_catch_ssa_value(&self, catch_block: NodeIndex, error_register: u8) -> crate::cfg::ssa::SSAValue {
        use crate::generated::unified_instructions::UnifiedInstruction;
        
        let block = &self.cfg.graph()[catch_block];
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // The Catch instruction is always the first instruction
        if let Some(first_instr) = block.instructions().first() {
            if matches!(&first_instr.instruction, UnifiedInstruction::Catch { .. }) {
                // Look up the SSA value defined at this instruction
                for (def, ssa_value) in &self.function_analysis.ssa.ssa_values {
                    if def.block_id == catch_block
                        && def.instruction_idx == first_instr.instruction_index.into()
                        && def.register == error_register
                    {
                        return ssa_value.clone();
                    }
                }
            }
        }
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Fallback: create a synthetic SSA value if not found
        // This shouldn't normally happen if SSA analysis is complete
        let def_site = crate::cfg::ssa::RegisterDef::new(
            error_register,
            catch_block,
<<<<<<< HEAD
            block
                .instructions()
                .first()
                .map(|i| i.instruction_index.into())
                .unwrap_or(0u32.into()),
        );
        crate::cfg::ssa::SSAValue::new(error_register, 1, def_site)
    }

    /// Build the finally clause structure
    fn build_finally_clause(
        &mut self,
        finally_block: NodeIndex,
    ) -> crate::analysis::control_flow_plan::FinallyClause {
        use crate::analysis::control_flow_plan::FinallyClause;
        use crate::generated::unified_instructions::UnifiedInstruction;

=======
            block.instructions().first().map(|i| i.instruction_index.into()).unwrap_or(0u32.into()),
        );
        crate::cfg::ssa::SSAValue::new(error_register, 1, def_site)
    }
    
    /// Build the finally clause structure
    fn build_finally_clause(&mut self, finally_block: NodeIndex) -> crate::analysis::control_flow_plan::FinallyClause {
        use crate::analysis::control_flow_plan::FinallyClause;
        use crate::generated::unified_instructions::UnifiedInstruction;
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // The finally block in exception handler contains:
        // 1. Catch instruction (first)
        // 2. Finally code (middle)
        // 3. Throw instruction (last)
<<<<<<< HEAD

        self.processed_blocks.insert(finally_block);

        let block = &self.cfg.graph()[finally_block];
        let instructions = block.instructions();

=======
        
        self.processed_blocks.insert(finally_block);
        
        let block = &self.cfg.graph()[finally_block];
        let instructions = block.instructions();
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        // Determine how many instructions to skip
        let skip_start = if matches!(
            instructions.first().map(|i| &i.instruction),
            Some(UnifiedInstruction::Catch { .. })
        ) {
<<<<<<< HEAD
            1 // Skip the Catch instruction
        } else {
            0
        };

=======
            1  // Skip the Catch instruction
        } else {
            0
        };
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        let skip_end = if matches!(
            instructions.last().map(|i| &i.instruction),
            Some(UnifiedInstruction::Throw { .. })
        ) {
<<<<<<< HEAD
            1 // Skip the Throw instruction
        } else {
            0
        };

        // Calculate the actual instruction count for the finally body
        let total_instructions = instructions.len();
        let actual_instruction_count = total_instructions.saturating_sub(skip_start + skip_end);

=======
            1  // Skip the Throw instruction
        } else {
            0
        };
        
        // Calculate the actual instruction count for the finally body
        let total_instructions = instructions.len();
        let actual_instruction_count = total_instructions.saturating_sub(skip_start + skip_end);
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        let body = self.plan.create_structure(ControlFlowKind::BasicBlock {
            block: finally_block,
            instruction_count: actual_instruction_count,
            is_synthetic: false,
        });
<<<<<<< HEAD

=======
        
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
        FinallyClause {
            finally_block,
            body,
            skip_start,
            skip_end,
        }
    }
<<<<<<< HEAD

    /// Try to build a basic block structure if it hasn't been processed yet
    fn try_build_basic_block(&mut self, block: NodeIndex) -> Option<StructureId> {
        // Check if this block exists and hasn't been processed
        if block.index() < self.cfg.graph().node_count() && !self.processed_blocks.contains(&block)
        {
            self.processed_blocks.insert(block);

            let instruction_count = self.cfg.graph()[block].instructions().len();
            Some(self.plan.create_structure(ControlFlowKind::BasicBlock {
                block,
                instruction_count,
                is_synthetic: false,
            }))
        } else {
            None
        }
    }
=======
>>>>>>> 14dcaa1 (Fix SSA use strategies and cascading elimination issues)
}
