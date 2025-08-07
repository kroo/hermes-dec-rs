//! Block sequencing for control flow structures
//!
//! This module handles ordering blocks within control flow regions and
//! determining when explicit control flow statements (break, continue) are needed.

use crate::cfg::Cfg;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

/// Sequences blocks within control flow regions
pub struct BlockSequencer<'a> {
    cfg: &'a Cfg<'a>,
}

impl<'a> BlockSequencer<'a> {
    /// Create a new block sequencer
    pub fn new(cfg: &'a Cfg<'a>) -> Self {
        Self { cfg }
    }

    /// Order blocks in execution sequence within a region
    pub fn sequence_blocks(
        &self,
        entry: NodeIndex,
        blocks: &[NodeIndex],
        exit: Option<NodeIndex>,
    ) -> Vec<NodeIndex> {
        let mut sequenced = Vec::new();
        let mut visited = HashSet::new();
        let block_set: HashSet<_> = blocks.iter().cloned().collect();

        // Start from entry and follow execution order
        self.sequence_from(entry, &block_set, exit, &mut visited, &mut sequenced);

        // Add any remaining blocks (unreachable or part of complex control flow)
        for &block in blocks {
            if !visited.contains(&block) {
                sequenced.push(block);
            }
        }

        sequenced
    }

    /// Recursively sequence blocks following control flow
    fn sequence_from(
        &self,
        current: NodeIndex,
        region_blocks: &HashSet<NodeIndex>,
        exit: Option<NodeIndex>,
        visited: &mut HashSet<NodeIndex>,
        result: &mut Vec<NodeIndex>,
    ) {
        // Stop at exit or if already visited
        if Some(current) == exit || !region_blocks.contains(&current) {
            return;
        }

        if !visited.insert(current) {
            return;
        }

        result.push(current);

        // Get edges sorted by priority (fall-through first)
        let mut edges: Vec<_> = self
            .cfg
            .graph()
            .edges(current)
            .filter(|e| region_blocks.contains(&e.target()) || Some(e.target()) == exit)
            .collect();

        // Sort to process fall-through edges first
        edges.sort_by_key(|e| match e.weight() {
            crate::cfg::EdgeKind::Fall => 0,
            _ => 1,
        });

        // Process successors
        for edge in edges {
            self.sequence_from(edge.target(), region_blocks, exit, visited, result);
        }
    }

    /// Check if a break statement is needed between two blocks
    pub fn needs_break_statement(
        &self,
        _current: NodeIndex,
        next_expected: NodeIndex,
        actual_target: NodeIndex,
    ) -> bool {
        // Break needed if the actual target differs from expected flow
        actual_target != next_expected
    }

    /// Check if control falls through from one block to another
    pub fn has_fall_through(&self, from: NodeIndex, to: NodeIndex) -> bool {
        self.cfg
            .graph()
            .edges(from)
            .any(|e| e.target() == to && matches!(e.weight(), crate::cfg::EdgeKind::Fall))
    }

    /// Find the natural successor of a block (where it would fall through)
    pub fn find_fall_through_target(&self, block: NodeIndex) -> Option<NodeIndex> {
        self.cfg
            .graph()
            .edges(block)
            .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::Fall))
            .map(|e| e.target())
    }

    /// Determine if a block ends with an explicit jump
    pub fn has_explicit_jump(&self, block: NodeIndex) -> bool {
        let block_data = &self.cfg.graph()[block];
        // Use the block's built-in method
        block_data.ends_with_jump()
    }
}
