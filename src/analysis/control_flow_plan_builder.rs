//! Builder for creating ControlFlowPlan from CFG analysis
//!
//! This module converts CFG analysis results into a high-level control flow plan
//! that can be used for AST generation.

use super::ControlFlowPlan;
use crate::analysis::control_flow_plan::{ControlFlowKind, SequentialElement, StructureId};
use crate::analysis::control_flow_plan_analyzer::ControlFlowPlanAnalyzer;
use crate::analysis::FunctionAnalysis;
use crate::cfg::analysis::{ConditionalChain, SwitchRegion};
use crate::cfg::switch_analysis::SwitchInfo;
use crate::cfg::Cfg;
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
}

impl<'a> ControlFlowPlanBuilder<'a> {
    /// Create a new builder
    pub fn new(cfg: &'a Cfg<'a>, function_analysis: &'a FunctionAnalysis<'a>) -> Self {
        Self {
            plan: ControlFlowPlan::new(),
            cfg,
            function_analysis,
            processed_blocks: HashSet::new(),
            stop_blocks: HashSet::new(),
        }
    }

    /// Build the control flow plan
    pub fn build(self) -> ControlFlowPlan {
        // First build the structure
        let function_analysis = self.function_analysis;
        let mut plan = self.build_structure();

        // Then analyze it to compute SSA strategies
        let analyzer = ControlFlowPlanAnalyzer::new(&mut plan, function_analysis);
        analyzer.analyze();

        plan
    }

    /// Build just the control flow structure without SSA analysis
    fn build_structure(mut self) -> ControlFlowPlan {
        // Use existing CFG analyses instead of reimplementing

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
        // For now, just handle the first chain
        // TODO: Handle multiple chains properly
        if let Some(chain) = analysis.chains.first() {
            self.build_conditional_structure(chain.clone())
        } else {
            self.plan.create_structure(ControlFlowKind::Empty)
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
    fn try_build_control_structure(&mut self, _block: NodeIndex) -> Option<StructureId> {
        // This is now simplified - we rely on the pre-computed analyses
        // rather than detecting patterns on the fly
        None
    }

    /// Build a switch structure from a region
    fn build_switch_from_region(&mut self, mut region: SwitchRegion) -> StructureId {
        self.processed_blocks.insert(region.dispatch);

        // Analyze case bodies if not already done
        if region.case_analyses.is_empty() {
            region.analyze_case_bodies(self.cfg.graph(), self.cfg);
        }

        // Detect the full switch pattern if it's sparse
        let switch_info = self.detect_sparse_switch_pattern(&region);

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
                self.processed_blocks.insert(default.target_block);
                self.build_sequential_structure(default.target_block)
            })
        } else {
            region.default_head.map(|default_block| {
                self.processed_blocks.insert(default_block);
                self.build_sequential_structure(default_block)
            })
        };

        // Mark join block as processed
        self.processed_blocks.insert(region.join_block);

        let switch_info_final = switch_info.unwrap_or_else(|| SwitchInfo {
            discriminator: 0,
            discriminator_instruction_index: InstructionIndex(0),
            cases: vec![],
            default_case: None,
            shared_tail: None,
        });

        let switch_structure = self.plan.create_structure(ControlFlowKind::Switch {
            dispatch_block: region.dispatch,
            info: switch_info_final.clone(),
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

            case_groups.push(crate::analysis::control_flow_plan::CaseGroupStructure {
                cases: switch_cases,
                body,
                fallthrough,
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

            case_groups.push(crate::analysis::control_flow_plan::CaseGroupStructure {
                cases: vec![switch_case],
                body,
                fallthrough: None,
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

        // Sort blocks by index to process them in order
        let mut sorted_blocks: Vec<_> = analysis.blocks.iter().copied().collect();
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

                let loop_structure = self.plan.create_structure(ControlFlowKind::Loop {
                    loop_type,
                    header_block: loop_info.primary_header(),
                    condition: None, // TODO: Extract condition from header block
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
                region.analyze_case_bodies(self.cfg.graph(), self.cfg);

                // Build the switch structure from this region
                return Some(self.build_switch_from_region(region));
            }
        }

        None
    }

    /// Build a conditional structure
    fn build_conditional_structure(&mut self, chain: ConditionalChain) -> StructureId {
        // For now, just build a simple if-else from the first branch
        if let Some(first_branch) = chain.branches.first() {
            self.processed_blocks.insert(first_branch.condition_source);
            self.processed_blocks.insert(first_branch.condition_block);

            // Build true branch
            let true_branch = self.build_sequential_structure(first_branch.branch_entry);

            // Build false branch if there's another branch
            let false_branch = if chain.branches.len() > 1 {
                if let Some(second_branch) = chain.branches.get(1) {
                    Some(self.build_sequential_structure(second_branch.branch_entry))
                } else {
                    None
                }
            } else {
                None
            };

            // Mark join block as processed
            self.processed_blocks.insert(chain.join_block);

            let kind = ControlFlowKind::Conditional {
                condition_block: first_branch.condition_block,
                condition_expr: None, // TODO: Extract condition expression
                true_branch,
                false_branch,
            };

            self.plan.create_structure(kind)
        } else {
            // Empty conditional?
            self.plan.create_structure(ControlFlowKind::Empty)
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

        // If there's exactly one successor and it's not already processed, that's our next block
        if successors.len() == 1 && !self.processed_blocks.contains(&successors[0]) {
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
                }
            }
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
}
