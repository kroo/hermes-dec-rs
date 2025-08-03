use super::types::SSAAnalysis;
use super::SSAError;
use crate::cfg::Cfg;
use petgraph::algo::dominators::Dominators;
use petgraph::graph::NodeIndex;
use std::collections::HashSet;

/// Compute dominance frontiers for phi function placement
pub fn compute_dominance_frontiers(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), SSAError> {
    let dominators = cfg
        .analyze_dominators()
        .ok_or_else(|| SSAError::FrontierError("Failed to compute dominators".to_string()))?;

    // Initialize dominance frontiers
    for block_id in cfg.graph().node_indices() {
        analysis
            .dominance_frontiers
            .insert(block_id, HashSet::new());
    }

    // Compute dominance frontiers using the algorithm from the design doc
    for block_id in cfg.graph().node_indices() {
        let predecessors: Vec<_> = cfg
            .graph()
            .neighbors_directed(block_id, petgraph::Direction::Incoming)
            .collect();

        if predecessors.len() >= 2 {
            // This is a join node, compute dominance frontier
            for pred in predecessors {
                let mut runner = pred;

                // Walk up the dominator tree until we reach the immediate dominator of block_id
                if let Some(block_idom) = dominators.immediate_dominator(block_id) {
                    while !dominates(&dominators, block_idom, runner) {
                        analysis
                            .dominance_frontiers
                            .get_mut(&runner)
                            .unwrap()
                            .insert(block_id);

                        if let Some(idom) = dominators.immediate_dominator(runner) {
                            runner = idom;
                        } else {
                            break; // Reached the root
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Get the dominance frontier for a specific block
pub fn get_frontier(analysis: &SSAAnalysis, block_id: NodeIndex) -> Option<&HashSet<NodeIndex>> {
    analysis.dominance_frontiers.get(&block_id)
}

/// Check if a node dominates another node
pub fn dominates(
    dominators: &Dominators<NodeIndex>,
    dominator: NodeIndex,
    node: NodeIndex,
) -> bool {
    let mut current = node;
    while let Some(immediate_dom) = dominators.immediate_dominator(current) {
        if immediate_dom == dominator {
            return true;
        }
        current = immediate_dom;
    }
    false
}
