//! Analysis for determining where phi variable declarations should be placed
//!
//! This module analyzes phi functions to determine if any operand dominates
//! all others (in which case that's the declaration point), or if we need
//! to insert a declaration at a common dominator.

use super::types::{PhiRegisterDeclaration, SSAAnalysis, SSAValue};
use crate::cfg::Cfg;
use crate::error::Error;
use crate::hbc::InstructionIndex;
use petgraph::algo::dominators::Dominators;
use petgraph::graph::NodeIndex;
use std::collections::HashSet;

/// Analyze phi functions and determine where variable declarations are needed
pub fn analyze_phi_declarations(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), Error> {
    let dominators = cfg.analyze_dominators().ok_or_else(|| Error::Internal {
        message: "Failed to compute dominators".to_string(),
    })?;

    // For each block with phi functions
    for (_block_id, phi_functions) in &analysis.phi_functions {
        for phi in phi_functions {
            if let Some(declaration_info) = analyze_phi_function(cfg, &dominators, phi, analysis)? {
                // Add to the list of declarations needed at the declaration block
                analysis
                    .phi_variable_declarations
                    .entry(declaration_info.declaration_block)
                    .or_insert_with(Vec::new)
                    .push(declaration_info);
            }
        }
    }

    Ok(())
}

/// Analyze a single phi function to determine if/where a declaration is needed
fn analyze_phi_function(
    cfg: &Cfg,
    dominators: &Dominators<NodeIndex>,
    phi: &super::types::PhiFunction,
    analysis: &SSAAnalysis,
) -> Result<Option<PhiRegisterDeclaration>, Error> {
    // Skip if no operands (shouldn't happen)
    if phi.operands.is_empty() {
        return Ok(None);
    }

    // Collect all definition blocks for phi operands
    let mut def_blocks: Vec<(NodeIndex, &SSAValue)> = Vec::new();
    for (_pred_block, ssa_value) in &phi.operands {
        // Find the block where this SSA value was defined
        if let Some(def) = analysis.definitions.iter().find(|d| {
            analysis
                .ssa_values
                .get(d)
                .map(|v| v == ssa_value)
                .unwrap_or(false)
        }) {
            // Get the block containing this definition
            if let Some(block_id) = find_block_for_instruction(cfg, def.instruction_idx) {
                def_blocks.push((block_id, ssa_value));
            }
        }
    }

    // If we have no definition blocks, no declaration needed
    if def_blocks.is_empty() {
        return Ok(None);
    }

    // Check if any definition dominates all others
    for (candidate_block, _candidate_value) in &def_blocks {
        let mut dominates_all = true;
        for (other_block, _) in &def_blocks {
            if candidate_block != other_block
                && !dominates(dominators, *candidate_block, *other_block)
            {
                dominates_all = false;
                break;
            }
        }

        if dominates_all {
            // This definition dominates all others, no extra declaration needed
            return Ok(None);
        }
    }

    // No single definition dominates all others, find common dominator
    let common_dominator = find_common_dominator(
        dominators,
        &def_blocks.iter().map(|(b, _)| *b).collect::<Vec<_>>(),
    )?;

    // Use the phi result SSA value - this should be coalesced with all other versions
    // The variable mapper will have already assigned a name to the representative of this equivalence class
    Ok(Some(PhiRegisterDeclaration {
        register: phi.register,
        phi_block: phi.block_id,
        declaration_block: common_dominator,
        ssa_value: phi.result.clone(),
    }))
}

/// Find the block containing a given instruction
fn find_block_for_instruction(cfg: &Cfg, instruction_idx: InstructionIndex) -> Option<NodeIndex> {
    for node_idx in cfg.graph().node_indices() {
        let block = &cfg.graph()[node_idx];
        if !block.is_exit() {
            let start = block.start_pc().value();
            let end = block.end_pc().value();
            if instruction_idx.value() >= start && instruction_idx.value() < end {
                return Some(node_idx);
            }
        }
    }
    None
}

/// Check if one block dominates another
fn dominates(dominators: &Dominators<NodeIndex>, dominator: NodeIndex, node: NodeIndex) -> bool {
    let mut current = node;
    loop {
        if current == dominator {
            return true;
        }
        if let Some(idom) = dominators.immediate_dominator(current) {
            current = idom;
        } else {
            return false;
        }
    }
}

/// Find the common dominator of a set of blocks
fn find_common_dominator(
    dominators: &Dominators<NodeIndex>,
    blocks: &[NodeIndex],
) -> Result<NodeIndex, Error> {
    if blocks.is_empty() {
        return Err(Error::Internal {
            message: "No blocks to find common dominator".to_string(),
        });
    }

    if blocks.len() == 1 {
        return Ok(blocks[0]);
    }

    // Start with the first block's dominator chain
    let mut common = blocks[0];

    // For each subsequent block, find the common dominator
    for &block in &blocks[1..] {
        common = find_common_dominator_pair(dominators, common, block)?;
    }

    Ok(common)
}

/// Find the common dominator of two blocks
fn find_common_dominator_pair(
    dominators: &Dominators<NodeIndex>,
    block1: NodeIndex,
    block2: NodeIndex,
) -> Result<NodeIndex, Error> {
    // Build dominator chain for block1
    let mut dom_chain1 = HashSet::new();
    let mut current = block1;
    loop {
        dom_chain1.insert(current);
        if let Some(idom) = dominators.immediate_dominator(current) {
            current = idom;
        } else {
            break;
        }
    }

    // Walk up block2's dominator chain until we find a common dominator
    current = block2;
    loop {
        if dom_chain1.contains(&current) {
            return Ok(current);
        }
        if let Some(idom) = dominators.immediate_dominator(current) {
            current = idom;
        } else {
            break;
        }
    }

    Err(Error::Internal {
        message: "No common dominator found".to_string(),
    })
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_dominates() {
        // Test will be implemented when we have a test CFG
    }
}
