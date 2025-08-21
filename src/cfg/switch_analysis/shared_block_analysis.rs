//! Shared block analysis for switch statements
//!
//! This module identifies blocks that are targeted or reached by multiple switch cases,
//! which need special handling during AST generation.

use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

use super::CaseGroup;
use crate::cfg::Cfg;

/// Information about shared blocks in a switch statement
#[derive(Debug, Clone)]
pub struct SharedBlockAnalysis {
    /// Blocks that are shared by multiple cases (directly or indirectly)
    pub shared_blocks: HashSet<NodeIndex>,
    /// Count of how many paths reference each block
    pub block_references: HashMap<NodeIndex, usize>,
}

impl SharedBlockAnalysis {
    /// Analyze which blocks are shared by multiple case groups
    pub fn analyze(case_groups: &[CaseGroup], cfg: &Cfg) -> Self {
        let mut block_references: HashMap<NodeIndex, usize> = HashMap::new();

        // Count direct targets - each case group targets a block
        for group in case_groups {
            *block_references.entry(group.target_block).or_insert(0) += 1;
        }

        // Also count indirect targets (blocks reachable from case targets)
        // This helps identify convergence points where multiple cases meet
        for group in case_groups {
            use petgraph::visit::EdgeRef;
            for edge in cfg.graph().edges(group.target_block) {
                let successor = edge.target();
                // Don't count exit blocks as shared
                if !cfg.graph()[successor].is_exit() {
                    *block_references.entry(successor).or_insert(0) += 1;
                }
            }
        }

        // Identify blocks referenced by multiple paths
        let mut shared_blocks = HashSet::new();
        for (&block_id, &count) in &block_references {
            if count > 1 {
                shared_blocks.insert(block_id);
            }
        }

        Self {
            shared_blocks,
            block_references,
        }
    }

    /// Check if a block is shared by multiple cases
    pub fn is_shared(&self, block_id: NodeIndex) -> bool {
        self.shared_blocks.contains(&block_id)
    }

    /// Get the reference count for a block
    pub fn reference_count(&self, block_id: NodeIndex) -> usize {
        self.block_references.get(&block_id).copied().unwrap_or(0)
    }

    /// Check if a block is truly post-switch (not just a fallthrough target)
    ///
    /// A block is post-switch if:
    /// 1. It's shared by multiple cases, AND
    /// 2. It's not just a direct fallthrough between consecutive cases
    pub fn is_post_switch_shared(
        &self,
        block_id: NodeIndex,
        group_index: usize,
        all_groups: &[CaseGroup],
    ) -> bool {
        if !self.is_shared(block_id) {
            return false;
        }

        // Check if this is just a fallthrough target
        // A block is just a fallthrough if only consecutive cases target it
        let mut targeting_groups = Vec::new();
        for (i, group) in all_groups.iter().enumerate() {
            if group.target_block == block_id {
                targeting_groups.push(i);
            }
        }

        // If only one group directly targets it, check indirect paths
        if targeting_groups.len() <= 1 {
            // Multiple cases converge here through control flow
            return true;
        }

        // Check if all targeting groups are consecutive
        // If they are, this might be a fallthrough pattern
        for window in targeting_groups.windows(2) {
            if window[1] - window[0] != 1 {
                // Non-consecutive groups target this block
                return true;
            }
        }

        // All targeting groups are consecutive
        // Check if current group is one of them
        if targeting_groups.contains(&group_index) {
            // Current group directly targets this block
            // Check if there's a non-consecutive group that also reaches it
            let first = *targeting_groups.first().unwrap();
            let last = *targeting_groups.last().unwrap();

            // If current group is not at the boundary, it's in a fallthrough chain
            if group_index > first && group_index < last {
                return false;
            }
        }

        // Default to treating as post-switch shared
        true
    }
}
