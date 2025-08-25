//! Sparse switch pattern detection during CFG analysis
//!
//! This module provides early detection of sparse switch patterns in the CFG where the compiler
//! has converted a sparse switch statement into a series of equality comparisons.
//!
//! This detector runs during CFG analysis to identify potential switch regions by finding
//! chains of JStrictEqual comparisons. The identified regions are later analyzed in detail
//! by SparseSwitchAnalyzer during AST conversion to extract setup instructions, PHI nodes,
//! and other information needed for JavaScript generation.

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

use super::switch_info::CaseKey;
use crate::analysis::value_tracker::{ConstantValue, TrackedValue, ValueTracker};
use crate::cfg::analysis::{PostDominatorAnalysis, SwitchCase, SwitchRegion};
use crate::cfg::ssa::RegisterUse;
use crate::hbc::HbcFile;
use ordered_float::OrderedFloat;

/// Represents a comparison value that can be of various types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparisonValue {
    Number(ordered_float::OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl ComparisonValue {
    /// Convert to a CaseKey for switch analysis
    pub fn to_case_key(&self) -> CaseKey {
        match self {
            ComparisonValue::Number(n) => CaseKey::Number(*n),
            ComparisonValue::String(s) => CaseKey::String(s.clone()),
            ComparisonValue::Boolean(b) => CaseKey::Boolean(*b),
            ComparisonValue::Null => CaseKey::Null,
            ComparisonValue::Undefined => CaseKey::Undefined,
        }
    }

    /// Create from a ConstantValue
    pub fn from_constant(value: &ConstantValue) -> Self {
        match value {
            ConstantValue::Number(n) => ComparisonValue::Number(OrderedFloat(*n)),
            ConstantValue::String(s) => ComparisonValue::String(s.clone()),
            ConstantValue::Boolean(b) => ComparisonValue::Boolean(*b),
            ConstantValue::Null => ComparisonValue::Null,
            ConstantValue::Undefined => ComparisonValue::Undefined,
        }
    }
}

/// Information about a sparse switch candidate
#[derive(Debug, Clone)]
pub struct SparseSwitchCandidate {
    /// The variable being compared
    pub compared_register: u8,
    /// Blocks performing equality comparisons
    pub comparison_blocks: Vec<ComparisonBlock>,
    /// The default block (reached when no comparisons match)
    pub default_block: Option<NodeIndex>,
    /// The join block where all cases converge
    pub join_block: NodeIndex,
    /// All blocks that are part of the switch infrastructure
    /// (includes comparison blocks and any trailing conditional blocks)
    pub infrastructure_blocks: Vec<NodeIndex>,
}

/// A block performing an equality comparison
#[derive(Debug, Clone)]
pub struct ComparisonBlock {
    /// The block containing the comparison
    pub block_index: NodeIndex,
    /// The value being compared against
    pub compared_value: ComparisonValue,
    /// The target block if comparison succeeds
    pub target_block: NodeIndex,
    /// The next comparison block if this fails
    pub next_comparison: Option<NodeIndex>,
}

/// Find sparse switch patterns in the CFG
pub fn find_sparse_switch_patterns(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    globally_processed: &mut HashSet<NodeIndex>,
    hbc_file: &HbcFile,
    ssa_values: &std::collections::HashMap<crate::cfg::ssa::RegisterDef, crate::cfg::ssa::SSAValue>,
    cfg: &crate::cfg::Cfg,
    ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
) -> Vec<SparseSwitchCandidate> {
    let mut candidates = Vec::new();
    let mut processed = HashSet::new();

    log::debug!("Starting sparse switch pattern detection");
    log::debug!(
        "Initially globally processed blocks: {:?}",
        globally_processed
    );

    // Look for chains of equality comparisons
    for node in graph.node_indices() {
        if processed.contains(&node) || globally_processed.contains(&node) {
            log::trace!("Skipping node {:?} - already processed", node);
            continue;
        }
        log::trace!("Checking node {:?} for sparse switch pattern", node);

        if let Some(candidate) = detect_sparse_switch_chain(
            graph,
            post_doms,
            node,
            &mut processed,
            hbc_file,
            ssa_values,
            cfg,
            ssa_analysis,
        ) {
            // Mark all infrastructure blocks as globally processed
            // This includes comparison blocks and any trailing conditional blocks
            // Don't mark target blocks (case heads) as they might contain inner switches
            log::trace!(
                "Found sparse switch pattern with {} comparison blocks and {} infrastructure blocks",
                candidate.comparison_blocks.len(),
                candidate.infrastructure_blocks.len()
            );
            for &block in &candidate.infrastructure_blocks {
                log::trace!("Marking block {:?} as globally processed", block);
                globally_processed.insert(block);
                processed.insert(block); // Also mark in local processed set
            }
            // Don't mark case target blocks or default block - they might contain inner switches

            candidates.push(candidate);
        }
    }

    log::debug!(
        "Sparse switch detection complete. Found {} patterns",
        candidates.len()
    );
    log::debug!("Final globally processed blocks: {:?}", globally_processed);
    candidates
}

/// Detect a sparse switch chain starting from a given node
pub fn detect_sparse_switch_chain(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    start_node: NodeIndex,
    processed: &mut HashSet<NodeIndex>,
    hbc_file: &HbcFile,
    ssa_values: &std::collections::HashMap<crate::cfg::ssa::RegisterDef, crate::cfg::ssa::SSAValue>,
    cfg: &crate::cfg::Cfg,
    ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
) -> Option<SparseSwitchCandidate> {
    // If this node has already been processed, skip it
    if processed.contains(&start_node) {
        return None;
    }

    let block = &graph[start_node];

    // Check if this block contains a JStrictEqual comparison
    let comp_info = extract_comparison_info(
        block,
        graph,
        start_node,
        hbc_file,
        ssa_values,
        cfg,
        ssa_analysis,
    );
    if comp_info.is_none() {
        return None;
    }
    let (compared_register, _first_value, _is_not_equal) = comp_info.unwrap();

    // This must be a conditional block with true/false branches
    let (_true_target, _false_target) = get_conditional_targets(graph, start_node)?;

    log::debug!(
        "Starting sparse switch chain detection from block {:?}",
        start_node
    );
    log::debug!("Comparing register r{}", compared_register);

    // Initialize tracking
    let mut comparison_blocks = Vec::new();
    let mut current_node = start_node;
    let mut seen_values = HashSet::<ComparisonValue>::new();
    let mut chain_blocks = Vec::new(); // Track all blocks in this chain

    // Follow the chain of comparisons
    loop {
        // Check if the next block in the chain has already been processed
        // This prevents detecting overlapping chains
        if current_node != start_node && processed.contains(&current_node) {
            // If we haven't collected any blocks yet, this isn't a valid chain
            if comparison_blocks.is_empty() {
                return None;
            }
            // Otherwise, end the chain here
            break;
        }

        let block = &graph[current_node];
        let (reg, value, is_not_equal) = extract_comparison_info(
            block,
            graph,
            current_node,
            hbc_file,
            ssa_values,
            cfg,
            ssa_analysis,
        )?;

        // Must be comparing the same register
        if reg != compared_register {
            break;
        }

        // Check for duplicate values
        if !seen_values.insert(value.clone()) {
            break;
        }

        chain_blocks.push(current_node);

        let (true_target, false_target) = get_conditional_targets(graph, current_node)?;

        // For JStrictNotEqual, the branches are inverted
        let (case_target, next_target) = if is_not_equal {
            // JStrictNotEqual: false branch goes to case, true branch continues
            (false_target, true_target)
        } else {
            // JStrictEqual: true branch goes to case, false branch continues
            (true_target, false_target)
        };

        comparison_blocks.push(ComparisonBlock {
            block_index: current_node,
            compared_value: value,
            target_block: case_target,
            next_comparison: Some(next_target),
        });

        // Check if the next target is another comparison
        let next_block = &graph[next_target];
        if let Some((next_reg, _, _)) = extract_comparison_info(
            next_block,
            graph,
            next_target,
            hbc_file,
            ssa_values,
            cfg,
            ssa_analysis,
        ) {
            if next_reg == compared_register {
                current_node = next_target;
                continue;
            }
        }

        // We've reached the end of the comparison chain
        // Check if the next_target is also a comparison block (just not for our register)
        // If so, it's still part of the switch infrastructure and should be marked as processed
        let default_block = Some(next_target);

        // Check if next_target is a conditional block - if so, it's part of switch infrastructure
        if is_conditional_block(graph, next_target) {
            log::debug!(
                "Block {:?} is a conditional block at end of chain - marking as part of switch",
                next_target
            );
            chain_blocks.push(next_target);
            // The actual default is whatever this conditional leads to
            // We'll just keep it as next_target for now since finding the true default
            // would require more complex analysis
        }

        // Find the join point - where all cases converge
        let mut all_targets = vec![next_target];
        for comp in &comparison_blocks {
            all_targets.push(comp.target_block);
        }

        let join_block = find_common_post_dominator(post_doms, &all_targets)?;

        // We need at least 2 comparisons to consider it a switch pattern
        if comparison_blocks.len() >= 2 {
            log::debug!(
                "Found valid sparse switch chain with {} comparisons",
                comparison_blocks.len()
            );
            log::debug!("Chain blocks: {:?}", chain_blocks);
            log::debug!(
                "Comparison blocks: {:?}",
                comparison_blocks
                    .iter()
                    .map(|c| c.block_index)
                    .collect::<Vec<_>>()
            );

            // Mark all blocks in the chain as processed
            for &block in &chain_blocks {
                processed.insert(block);
            }

            return Some(SparseSwitchCandidate {
                compared_register,
                comparison_blocks,
                default_block,
                join_block,
                infrastructure_blocks: chain_blocks,
            });
        }

        break;
    }

    None
}

/// Extract comparison information from a block
/// Returns (register, value, is_not_equal)
pub fn extract_comparison_info(
    block: &Block,
    graph: &DiGraph<Block, EdgeKind>,
    node: NodeIndex,
    hbc_file: &HbcFile,
    _ssa_values: &std::collections::HashMap<
        crate::cfg::ssa::RegisterDef,
        crate::cfg::ssa::SSAValue,
    >,
    cfg: &crate::cfg::Cfg,
    ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
) -> Option<(u8, ComparisonValue, bool)> {
    log::trace!("extract_comparison_info for block {:?}", node);

    // Always use ValueTracker since all parameters are now required
    let result =
        extract_comparison_info_with_value_tracker(block, node, hbc_file, cfg, ssa_analysis);
    if result.is_some() {
        log::trace!("  Found comparison with ValueTracker: {:?}", result);
        return result;
    }

    // Fall back to legacy behavior if ValueTracker didn't find anything
    let result = extract_comparison_info_legacy(block, graph, node);

    if result.is_none() {
        log::trace!("  No comparison found in block {:?}", node);
        // Log the actual instructions to debug
        for instr in block.instructions() {
            log::trace!("    Instruction: {:?}", instr.instruction);
        }
    } else {
        log::trace!("  Found comparison: {:?}", result);
    }

    result
}

/// Extract comparison information using ValueTracker
fn extract_comparison_info_with_value_tracker(
    block: &Block,
    node: NodeIndex,
    hbc_file: &HbcFile,
    cfg: &crate::cfg::Cfg,
    ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
) -> Option<(u8, ComparisonValue, bool)> {
    let value_tracker = ValueTracker::new(cfg, ssa_analysis, hbc_file);

    // Look for JStrictEqual or JStrictNotEqual instructions
    for (idx, instr) in block.instructions().iter().enumerate() {
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
                // Get the instruction location
                let pc = block.start_pc + idx;

                // Look up SSA values for both operands
                let use1 = RegisterUse {
                    register: *operand_1,
                    block_id: node,
                    instruction_idx: pc,
                };
                let use2 = RegisterUse {
                    register: *operand_2,
                    block_id: node,
                    instruction_idx: pc,
                };

                // Try to find SSA values for these registers at this point
                let ssa_value1 = ssa_analysis
                    .use_def_chains
                    .get(&use1)
                    .and_then(|def| ssa_analysis.ssa_values.get(def));
                let ssa_value2 = ssa_analysis
                    .use_def_chains
                    .get(&use2)
                    .and_then(|def| ssa_analysis.ssa_values.get(def));

                // Track values for both operands
                if let Some(ssa1) = ssa_value1 {
                    let tracked1 = value_tracker.get_value(ssa1);
                    if let TrackedValue::Constant(const_val) = tracked1 {
                        let comp_val = ComparisonValue::from_constant(&const_val);
                        log::trace!(
                            "  Found constant {:?} in r{} via SSA tracking",
                            comp_val,
                            operand_1
                        );
                        return Some((*operand_2, comp_val, false));
                    }
                }

                if let Some(ssa2) = ssa_value2 {
                    let tracked2 = value_tracker.get_value(ssa2);
                    if let TrackedValue::Constant(const_val) = tracked2 {
                        let comp_val = ComparisonValue::from_constant(&const_val);
                        log::trace!(
                            "  Found constant {:?} in r{} via SSA tracking",
                            comp_val,
                            operand_2
                        );
                        return Some((*operand_1, comp_val, false));
                    }
                }
            }
            UnifiedInstruction::JStrictNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JStrictNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => {
                // Get the instruction location
                let pc = block.start_pc + idx;

                // Look up SSA values for both operands
                let use1 = RegisterUse {
                    register: *operand_1,
                    block_id: node,
                    instruction_idx: pc,
                };
                let use2 = RegisterUse {
                    register: *operand_2,
                    block_id: node,
                    instruction_idx: pc,
                };

                // Try to find SSA values for these registers at this point
                let ssa_value1 = ssa_analysis
                    .use_def_chains
                    .get(&use1)
                    .and_then(|def| ssa_analysis.ssa_values.get(def));
                let ssa_value2 = ssa_analysis
                    .use_def_chains
                    .get(&use2)
                    .and_then(|def| ssa_analysis.ssa_values.get(def));

                // Track values for both operands
                if let Some(ssa1) = ssa_value1 {
                    let tracked1 = value_tracker.get_value(ssa1);
                    if let TrackedValue::Constant(const_val) = tracked1 {
                        let comp_val = ComparisonValue::from_constant(&const_val);
                        return Some((*operand_2, comp_val, true));
                    }
                }

                if let Some(ssa2) = ssa_value2 {
                    let tracked2 = value_tracker.get_value(ssa2);
                    if let TrackedValue::Constant(const_val) = tracked2 {
                        let comp_val = ComparisonValue::from_constant(&const_val);
                        return Some((*operand_1, comp_val, true));
                    }
                }
            }
            _ => {}
        }
    }

    None
}

/// Legacy version of extract_comparison_info without ValueTracker
fn extract_comparison_info_legacy(
    block: &Block,
    graph: &DiGraph<Block, EdgeKind>,
    node: NodeIndex,
) -> Option<(u8, ComparisonValue, bool)> {
    // Look for JStrictEqual or JStrictNotEqual with a constant value
    for (idx, instr) in block.instructions().iter().enumerate() {
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
                // First check if either operand has a constant load in this block
                if let Some(const_val) =
                    find_constant_load_before(block, idx, *operand_2, graph, node)
                {
                    return Some((
                        *operand_1,
                        ComparisonValue::Number(OrderedFloat(const_val as f64)),
                        false,
                    ));
                }
                if let Some(const_val) =
                    find_constant_load_before(block, idx, *operand_1, graph, node)
                {
                    return Some((
                        *operand_2,
                        ComparisonValue::Number(OrderedFloat(const_val as f64)),
                        false,
                    ));
                }
            }
            UnifiedInstruction::JStrictNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JStrictNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => {
                // Check if we're comparing against a constant
                if let Some(const_val) =
                    find_constant_load_before(block, idx, *operand_2, graph, node)
                {
                    return Some((
                        *operand_1,
                        ComparisonValue::Number(OrderedFloat(const_val as f64)),
                        true,
                    ));
                }
                // Try the other operand
                if let Some(const_val) =
                    find_constant_load_before(block, idx, *operand_1, graph, node)
                {
                    return Some((
                        *operand_2,
                        ComparisonValue::Number(OrderedFloat(const_val as f64)),
                        true,
                    ));
                }
            }
            _ => {}
        }
    }
    None
}

/// Find a constant load instruction before the given index
fn find_constant_load_before(
    block: &Block,
    before_index: usize,
    register: u8,
    graph: &DiGraph<Block, EdgeKind>,
    node: NodeIndex,
) -> Option<i32> {
    // First, look backwards in the current block for a constant load into the specified register
    for i in (0..before_index).rev() {
        let instr = &block.instructions()[i];
        match &instr.instruction {
            UnifiedInstruction::LoadConstInt {
                operand_0,
                operand_1,
            } if *operand_0 == register => {
                return Some(*operand_1);
            }
            UnifiedInstruction::LoadConstUInt8 {
                operand_0,
                operand_1,
            } if *operand_0 == register => {
                return Some(*operand_1 as i32);
            }
            UnifiedInstruction::LoadConstZero { operand_0 } if *operand_0 == register => {
                return Some(0);
            }
            // If the register is reassigned, stop looking
            _ if is_register_assigned(&instr.instruction, register) => {
                return None; // Register was reassigned, value is lost
            }
            _ => {}
        }
    }

    // If not found in current block and we haven't checked from the beginning,
    // check predecessor blocks (only if there's a single predecessor)
    if before_index > 0 {
        use petgraph::Direction;
        let predecessors: Vec<_> = graph.edges_directed(node, Direction::Incoming).collect();

        if predecessors.len() == 1 {
            let pred_node = predecessors[0].source();
            let pred_block = &graph[pred_node];

            // Check the entire predecessor block
            return find_constant_load_in_block(pred_block, register);
        }
    }

    None
}

/// Find a constant load in an entire block (searching backwards)
fn find_constant_load_in_block(block: &Block, register: u8) -> Option<i32> {
    // Search backwards through the entire block
    for instr in block.instructions().iter().rev() {
        match &instr.instruction {
            UnifiedInstruction::LoadConstInt {
                operand_0,
                operand_1,
            } if *operand_0 == register => {
                return Some(*operand_1);
            }
            UnifiedInstruction::LoadConstUInt8 {
                operand_0,
                operand_1,
            } if *operand_0 == register => {
                return Some(*operand_1 as i32);
            }
            UnifiedInstruction::LoadConstZero { operand_0 } if *operand_0 == register => {
                return Some(0);
            }
            // If the register is reassigned, stop looking
            _ if is_register_assigned(&instr.instruction, register) => {
                return None;
            }
            _ => {}
        }
    }
    None
}

/// Check if an instruction assigns to a register
fn is_register_assigned(instruction: &UnifiedInstruction, register: u8) -> bool {
    // This is a simplified check - in reality we'd need to check all instructions
    // that write to registers
    match instruction {
        UnifiedInstruction::LoadConstInt { operand_0, .. }
        | UnifiedInstruction::LoadConstUInt8 { operand_0, .. }
        | UnifiedInstruction::LoadConstZero { operand_0 } => *operand_0 == register,
        _ => false, // TODO: Add more instruction types
    }
}

/// Get true and false targets from a conditional node
/// Check if a block is a conditional block (has true/false edges)
fn is_conditional_block(graph: &DiGraph<Block, EdgeKind>, node: NodeIndex) -> bool {
    let mut has_true = false;
    let mut has_false = false;

    for edge in graph.edges(node) {
        match edge.weight() {
            EdgeKind::True => has_true = true,
            EdgeKind::False => has_false = true,
            _ => {}
        }
    }

    has_true && has_false
}

fn get_conditional_targets(
    graph: &DiGraph<Block, EdgeKind>,
    node: NodeIndex,
) -> Option<(NodeIndex, NodeIndex)> {
    let mut true_target = None;
    let mut false_target = None;

    for edge in graph.edges(node) {
        match edge.weight() {
            EdgeKind::True => true_target = Some(edge.target()),
            EdgeKind::False => false_target = Some(edge.target()),
            _ => {}
        }
    }

    match (true_target, false_target) {
        (Some(t), Some(f)) => Some((t, f)),
        _ => None,
    }
}

/// Find the common post-dominator of a set of nodes
fn find_common_post_dominator(
    post_doms: &PostDominatorAnalysis,
    nodes: &[NodeIndex],
) -> Option<NodeIndex> {
    if nodes.is_empty() {
        return None;
    }

    // Start with the post-dominators of the first node
    let mut common_doms = post_doms.post_dominators.get(&nodes[0])?.clone();

    // Intersect with post-dominators of all other nodes
    for &node in &nodes[1..] {
        if let Some(node_doms) = post_doms.post_dominators.get(&node) {
            common_doms = common_doms.intersection(node_doms).cloned().collect();
        } else {
            return None;
        }
    }

    // Find the immediate post-dominator (closest one)
    // This is the node that post-dominates all our nodes but is post-dominated
    // by the fewest other nodes
    let mut best_candidate = None;
    let mut min_dominators = usize::MAX;

    for &candidate in &common_doms {
        if let Some(candidate_doms) = post_doms.post_dominators.get(&candidate) {
            let dom_count = candidate_doms.len();
            if dom_count < min_dominators {
                min_dominators = dom_count;
                best_candidate = Some(candidate);
            }
        }
    }

    best_candidate
}

/// Convert a sparse switch candidate to a switch region
pub fn sparse_candidate_to_switch_region(candidate: &SparseSwitchCandidate) -> SwitchRegion {
    // The dispatch block is the first comparison block
    let dispatch = candidate.comparison_blocks[0].block_index;

    // Convert comparison blocks to switch cases
    let mut cases = Vec::new();
    for (idx, comp_block) in candidate.comparison_blocks.iter().enumerate() {
        cases.push(SwitchCase {
            case_index: idx,
            case_head: comp_block.target_block,
        });
    }

    SwitchRegion {
        dispatch,
        cases,
        default_head: candidate.default_block,
        join_block: candidate.join_block,
        case_analyses: std::collections::HashMap::new(),
    }
}
