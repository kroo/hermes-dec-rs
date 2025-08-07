//! Region analysis for control flow structures
//!
//! This module helps identify and analyze regions in the CFG that correspond
//! to control flow structures like loops, switches, and try-catch blocks.

use crate::cfg::analysis::PostDominatorAnalysis;
use crate::cfg::Cfg;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::{HashSet, VecDeque};

/// Analyzes regions in the control flow graph
pub struct RegionAnalyzer<'a> {
    cfg: &'a Cfg<'a>,
    post_doms: Option<&'a PostDominatorAnalysis>,
}

impl<'a> RegionAnalyzer<'a> {
    /// Create a new region analyzer
    pub fn new(cfg: &'a Cfg<'a>, post_doms: Option<&'a PostDominatorAnalysis>) -> Self {
        Self { cfg, post_doms }
    }

    /// Find all blocks that belong to a region between entry and exit
    pub fn find_region_blocks(&self, entry: NodeIndex, exit: Option<NodeIndex>) -> Vec<NodeIndex> {
        let mut region_blocks = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(entry);

        while let Some(current) = queue.pop_front() {
            // Stop at exit block
            if Some(current) == exit {
                continue;
            }

            if !visited.insert(current) {
                continue;
            }

            region_blocks.push(current);

            // Add successors
            for edge in self.cfg.graph().edges(current) {
                let target = edge.target();
                // Don't follow edges that leave the region
                if Some(target) != exit && !visited.contains(&target) {
                    queue.push_back(target);
                }
            }
        }

        region_blocks
    }

    /// Find the exit block for a region (where control flow converges)
    pub fn find_region_exit(&self, _entry: NodeIndex) -> Option<NodeIndex> {
        // If we have post-dominator analysis, use it
        if let Some(_post_doms) = self.post_doms {
            // TODO: The exit is the immediate post-dominator of the entry
            // Need to add immediate_dominator method to PostDominatorAnalysis
            // return post_doms.immediate_dominator(entry);
        }

        // Otherwise, try to find where paths converge
        // This is a simplified version - real implementation would be more sophisticated
        None
    }

    /// Check if a jump target is a break (exits the region)
    pub fn is_break_target(
        &self,
        from_block: NodeIndex,
        target_block: NodeIndex,
        region_blocks: &[NodeIndex],
    ) -> bool {
        region_blocks.contains(&from_block) && !region_blocks.contains(&target_block)
    }

    /// Check if a jump target is a continue (goes to loop header)
    pub fn is_continue_target(&self, target_block: NodeIndex, loop_header: NodeIndex) -> bool {
        target_block == loop_header
    }

    /// Find all break targets from blocks within the region
    pub fn find_break_targets(&self, region_blocks: &[NodeIndex]) -> HashSet<NodeIndex> {
        let mut break_targets = HashSet::new();
        let region_set: HashSet<_> = region_blocks.iter().cloned().collect();

        for &block in region_blocks {
            for edge in self.cfg.graph().edges(block) {
                let target = edge.target();
                if !region_set.contains(&target) {
                    break_targets.insert(target);
                }
            }
        }

        break_targets
    }

    /// Find all back-edges (continue targets) in a loop region
    pub fn find_continue_targets(
        &self,
        region_blocks: &[NodeIndex],
        loop_header: NodeIndex,
    ) -> HashSet<NodeIndex> {
        let mut continue_targets = HashSet::new();

        for &block in region_blocks {
            for edge in self.cfg.graph().edges(block) {
                if edge.target() == loop_header {
                    continue_targets.insert(block);
                }
            }
        }

        continue_targets
    }
}
