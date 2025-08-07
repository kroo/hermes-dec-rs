//! Sparse switch pattern detection
//!
//! This module detects sparse switch patterns in the CFG where the compiler
//! has converted a sparse switch statement into a series of equality comparisons.

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

use crate::cfg::analysis::{PostDominatorAnalysis, SwitchCase, SwitchRegion};

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
}

/// A block performing an equality comparison
#[derive(Debug, Clone)]
pub struct ComparisonBlock {
    /// The block containing the comparison
    pub block_index: NodeIndex,
    /// The value being compared against
    pub compared_value: i32,
    /// The target block if comparison succeeds
    pub target_block: NodeIndex,
    /// The next comparison block if this fails
    pub next_comparison: Option<NodeIndex>,
}

/// Find sparse switch patterns in the CFG
pub fn find_sparse_switch_patterns(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
) -> Vec<SparseSwitchCandidate> {
    let mut candidates = Vec::new();
    let mut processed = HashSet::new();

    // Look for chains of equality comparisons
    for node in graph.node_indices() {
        if processed.contains(&node) {
            continue;
        }

        if let Some(candidate) = detect_sparse_switch_chain(graph, post_doms, node, &mut processed)
        {
            candidates.push(candidate);
        }
    }

    candidates
}

/// Detect a sparse switch chain starting from a given node
fn detect_sparse_switch_chain(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    start_node: NodeIndex,
    processed: &mut HashSet<NodeIndex>,
) -> Option<SparseSwitchCandidate> {
    let block = &graph[start_node];

    // Check if this block contains a JStrictEqual comparison
    let comparison_info = extract_comparison_info(block)?;

    // This must be a conditional block with true/false branches
    let (_true_target, _false_target) = get_conditional_targets(graph, start_node)?;

    // Initialize tracking
    let mut comparison_blocks = Vec::new();
    let mut current_node = start_node;
    let compared_register = comparison_info.0;
    let mut seen_values = HashSet::new();

    // Follow the chain of comparisons
    loop {
        let block = &graph[current_node];
        let comp_info = extract_comparison_info(block)?;

        // Must be comparing the same register
        if comp_info.0 != compared_register {
            break;
        }

        // Check for duplicate values
        if !seen_values.insert(comp_info.1) {
            break;
        }

        processed.insert(current_node);

        let (true_target, false_target) = get_conditional_targets(graph, current_node)?;

        comparison_blocks.push(ComparisonBlock {
            block_index: current_node,
            compared_value: comp_info.1,
            target_block: true_target,
            next_comparison: Some(false_target),
        });

        // Check if the false target is another comparison
        let false_block = &graph[false_target];
        if let Some(next_comp) = extract_comparison_info(false_block) {
            if next_comp.0 == compared_register {
                current_node = false_target;
                continue;
            }
        }

        // We've reached the end of the comparison chain
        // The false target is the default case
        let default_block = Some(false_target);

        // Find the join point - where all cases converge
        let mut all_targets = vec![false_target];
        for comp in &comparison_blocks {
            all_targets.push(comp.target_block);
        }

        let join_block = find_common_post_dominator(post_doms, &all_targets)?;

        // We need at least 3 comparisons to consider it a switch pattern
        if comparison_blocks.len() >= 3 {
            return Some(SparseSwitchCandidate {
                compared_register,
                comparison_blocks,
                default_block,
                join_block,
            });
        }

        break;
    }

    None
}

/// Extract comparison information from a block
pub fn extract_comparison_info(block: &Block) -> Option<(u8, i32)> {
    // Look for JStrictEqual with a constant value
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
                // Check if we're comparing against a constant
                // This requires looking at the previous instruction to see if it loaded a constant
                if let Some(const_val) = find_constant_load_before(block, idx, *operand_2) {
                    return Some((*operand_1, const_val));
                }
                // Try the other operand
                if let Some(const_val) = find_constant_load_before(block, idx, *operand_1) {
                    return Some((*operand_2, const_val));
                }
            }
            _ => {}
        }
    }
    None
}

/// Find a constant load instruction before the given index
fn find_constant_load_before(block: &Block, before_index: usize, register: u8) -> Option<i32> {
    // Look backwards for a constant load into the specified register
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
                break;
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
    }
}
