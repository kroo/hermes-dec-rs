//! Fallthrough switch pattern detection
//!
//! This module detects switch patterns with fallthrough behavior where:
//! 1. Multiple comparisons at the start jump to different case labels
//! 2. Case labels can fall through to the next case
//! 3. Cases use explicit jumps to exit the switch

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet};

use super::analysis::{PostDominatorAnalysis, SwitchCase, SwitchRegion};

/// Information about a fallthrough switch pattern
#[derive(Debug, Clone)]
pub struct FallthroughSwitchCandidate {
    /// The variable being compared
    pub compared_register: u8,
    /// The initial comparison blocks and their jump targets
    pub initial_comparisons: Vec<InitialComparison>,
    /// Case blocks with their labels and whether they fall through
    pub case_blocks: HashMap<NodeIndex, CaseBlock>,
    /// The exit block where all cases converge
    pub exit_block: NodeIndex,
    /// The default case block (if any)
    pub default_block: Option<NodeIndex>,
}

/// An initial comparison that jumps to a case label
#[derive(Debug, Clone)]
pub struct InitialComparison {
    /// The block containing the comparison
    pub block_index: NodeIndex,
    /// The value being compared
    pub compared_value: i32,
    /// The target case label
    pub target_label: NodeIndex,
}

/// Information about a case block
#[derive(Debug, Clone)]
pub struct CaseBlock {
    /// The case value (if this is a direct case entry)
    pub case_value: Option<i32>,
    /// Whether this case falls through to the next
    pub falls_through: bool,
    /// The next case in fallthrough order (if any)
    pub fallthrough_target: Option<NodeIndex>,
}

/// Find fallthrough switch patterns in the CFG
pub fn find_fallthrough_switch_patterns(
    graph: &DiGraph<Block, EdgeKind>,
    _post_doms: &PostDominatorAnalysis,
    globally_processed: &mut HashSet<NodeIndex>,
) -> Vec<FallthroughSwitchCandidate> {
    let mut candidates = Vec::new();
    let mut processed = HashSet::new();

    // Look for blocks with multiple equality comparisons
    for node in graph.node_indices() {
        if processed.contains(&node) || globally_processed.contains(&node) {
            continue;
        }

        if let Some(candidate) = detect_fallthrough_switch(graph, node, &mut processed) {
            // Mark all nodes in this candidate as globally processed
            for comp in &candidate.initial_comparisons {
                globally_processed.insert(comp.block_index);
                globally_processed.insert(comp.target_label);
            }
            if let Some(default) = candidate.default_block {
                globally_processed.insert(default);
            }
            
            candidates.push(candidate);
        }
    }

    candidates
}

/// Detect a fallthrough switch pattern starting from a given node
fn detect_fallthrough_switch(
    graph: &DiGraph<Block, EdgeKind>,
    start_node: NodeIndex,
    processed: &mut HashSet<NodeIndex>,
) -> Option<FallthroughSwitchCandidate> {
    let block = &graph[start_node];

    // Check if this block contains a comparison
    let comparison_info = extract_comparison_info(block)?;
    let compared_register = comparison_info.0;

    // Collect all initial comparisons
    let mut initial_comparisons = Vec::new();
    let mut current_node = start_node;
    let mut seen_values = HashSet::new();
    let mut case_targets = HashSet::new();
    let mut default_block = None;

    // Follow the chain of comparisons
    loop {
        let block = &graph[current_node];

        if let Some((reg, value)) = extract_comparison_info(block) {
            // Must be comparing the same register
            if reg != compared_register {
                break;
            }

            // Check for duplicate values
            if !seen_values.insert(value) {
                break;
            }

            processed.insert(current_node);

            // Find the jump targets
            let mut true_target = None;
            let mut false_target = None;

            for edge in graph.edges(current_node) {
                match edge.weight() {
                    EdgeKind::True => true_target = Some(edge.target()),
                    EdgeKind::False => false_target = Some(edge.target()),
                    _ => {}
                }
            }

            if let (Some(true_target), Some(false_target)) = (true_target, false_target) {
                // The true target is the case label
                case_targets.insert(true_target);

                initial_comparisons.push(InitialComparison {
                    block_index: current_node,
                    compared_value: value,
                    target_label: true_target,
                });

                // Check if the false target is another comparison
                let false_block = &graph[false_target];
                if extract_comparison_info(false_block).is_some() {
                    current_node = false_target;
                    continue;
                } else {
                    // This is the default case or exit
                    default_block = Some(false_target);
                    break;
                }
            }
        } else {
            break;
        }
    }

    // We need at least 2 comparisons to be a switch
    if initial_comparisons.len() < 2 {
        return None;
    }
    
    // Additional check: For a fallthrough pattern, we expect to find actual fallthrough behavior
    // If none of the cases fall through, this is likely a sparse switch instead
    let mut has_fallthrough = false;

    // Analyze the case blocks to detect fallthrough patterns
    let mut case_blocks = HashMap::new();
    let mut exit_block = None;

    for comparison in &initial_comparisons {
        let case_info = analyze_case_block(graph, comparison.target_label, &case_targets)?;
        if case_info.falls_through {
            has_fallthrough = true;
        }
        case_blocks.insert(comparison.target_label, case_info);

        // Track potential exit blocks
        if let Some(exit) = find_case_exit(graph, comparison.target_label) {
            exit_block = Some(exit);
        }
    }
    
    // If no cases fall through, this is likely a sparse switch pattern, not a fallthrough pattern
    if !has_fallthrough {
        return None;
    }

    // If we don't have an exit block, try to find it from the default case
    if exit_block.is_none() {
        if let Some(default) = default_block {
            exit_block = find_case_exit(graph, default);
        }
    }

    let exit_block = exit_block?;

    Some(FallthroughSwitchCandidate {
        compared_register,
        initial_comparisons,
        case_blocks,
        exit_block,
        default_block,
    })
}

/// Extract comparison information from a block
fn extract_comparison_info(block: &Block) -> Option<(u8, i32)> {
    // Look for JStrictEqual with a constant value
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
                // Look for a preceding constant load
                for (i, prev_instr) in block.instructions().iter().enumerate() {
                    if i >= instr.instruction_index as usize {
                        break;
                    }

                    match &prev_instr.instruction {
                        UnifiedInstruction::LoadConstInt {
                            operand_0,
                            operand_1: const_value,
                        } if *operand_0 == *operand_2 => {
                            // operand_1 from JStrictEqual is the compared register (variable)
                            // const_value is the constant being compared
                            return Some((*operand_1, *const_value));
                        }
                        UnifiedInstruction::LoadConstInt {
                            operand_0,
                            operand_1: const_value,
                        } if *operand_0 == *operand_1 => {
                            // operand_2 from JStrictEqual is the compared register (variable)
                            // const_value is the constant being compared
                            return Some((*operand_2, *const_value));
                        }
                        UnifiedInstruction::LoadConstUInt8 {
                            operand_0,
                            operand_1: const_value,
                        } if *operand_0 == *operand_2 => {
                            // operand_1 from JStrictEqual is the compared register (variable)
                            // const_value is the constant being compared
                            return Some((*operand_1, *const_value as i32));
                        }
                        UnifiedInstruction::LoadConstUInt8 {
                            operand_0,
                            operand_1: const_value,
                        } if *operand_0 == *operand_1 => {
                            // operand_2 from JStrictEqual is the compared register (variable)
                            // const_value is the constant being compared
                            return Some((*operand_2, *const_value as i32));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Analyze a case block to determine if it falls through
fn analyze_case_block(
    graph: &DiGraph<Block, EdgeKind>,
    case_label: NodeIndex,
    case_targets: &HashSet<NodeIndex>,
) -> Option<CaseBlock> {
    let block = &graph[case_label];

    // Check if this block ends with an unconditional jump
    let last_instr = block.instructions().last()?;
    let has_explicit_break = matches!(
        last_instr.instruction,
        UnifiedInstruction::Jmp { .. } | UnifiedInstruction::JmpLong { .. }
    );

    // Check if the next sequential block is another case
    let mut falls_through = false;
    let mut fallthrough_target = None;

    if !has_explicit_break {
        // Look for fall edges to other case blocks
        for edge in graph.edges(case_label) {
            if matches!(edge.weight(), EdgeKind::Fall) {
                let target = edge.target();
                if case_targets.contains(&target) {
                    falls_through = true;
                    fallthrough_target = Some(target);
                    break;
                }
            }
        }
    }

    Some(CaseBlock {
        case_value: None, // Will be set based on the comparison
        falls_through,
        fallthrough_target,
    })
}

/// Find the exit block for a case
fn find_case_exit(graph: &DiGraph<Block, EdgeKind>, case_block: NodeIndex) -> Option<NodeIndex> {
    // Look for unconditional jumps
    for edge in graph.edges(case_block) {
        if matches!(edge.weight(), EdgeKind::Uncond) {
            return Some(edge.target());
        }
    }

    // For blocks with conditional logic, recursively search for exits
    // This is simplified - a full implementation would need more sophisticated analysis
    None
}

/// Convert a fallthrough switch candidate to a switch region
pub fn fallthrough_candidate_to_switch_region(
    candidate: &FallthroughSwitchCandidate,
) -> SwitchRegion {
    // The dispatch block is the first comparison block
    let dispatch = candidate.initial_comparisons[0].block_index;

    // Convert initial comparisons to switch cases
    let mut cases = Vec::new();
    for (idx, comparison) in candidate.initial_comparisons.iter().enumerate() {
        cases.push(SwitchCase {
            case_index: idx,
            case_head: comparison.target_label,
        });
    }

    SwitchRegion {
        dispatch,
        cases,
        default_head: candidate.default_block,
        join_block: candidate.exit_block,
    }
}
