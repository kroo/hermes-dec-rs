//! Improved conditional chain analysis for CFG
//!
//! This module provides a correct implementation of conditional chain detection
//! that properly handles sequential, nested, and chained conditionals.

use crate::cfg::analysis::{
    find_lowest_common_post_dominator, BranchType, ChainStatistics, ChainType, ConditionalAnalysis,
    ConditionalBranch, ConditionalChain, PostDominatorAnalysis,
};
use crate::cfg::{Block, EdgeKind};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet, VecDeque};

/// Build node-to-chain mappings recursively
fn build_node_mappings_recursive(
    chains: &[ConditionalChain],
    node_to_chains: &mut HashMap<NodeIndex, Vec<usize>>,
    offset: usize,
) {
    for (idx, chain) in chains.iter().enumerate() {
        let global_idx = offset + idx;

        for branch in &chain.branches {
            // Add condition block
            node_to_chains
                .entry(branch.condition_block)
                .or_insert_with(Vec::new)
                .push(global_idx);

            // Add branch blocks
            for &block in &branch.branch_blocks {
                node_to_chains
                    .entry(block)
                    .or_insert_with(Vec::new)
                    .push(global_idx);
            }
        }

        // Recursively process nested chains
        if !chain.nested_chains.is_empty() {
            build_node_mappings_recursive(&chain.nested_chains, node_to_chains, global_idx + 1);
        }
    }
}

/// Analyze all conditional chains in a CFG
pub fn analyze_conditional_chains_improved(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
) -> ConditionalAnalysis {
    let mut detector = ConditionalDetector::new(graph, post_doms);
    let chains = detector.detect_all_chains_hierarchical();

    // Build node-to-chain mappings (flattened view)
    let mut node_to_chains = HashMap::new();
    build_node_mappings_recursive(&chains, &mut node_to_chains, 0);

    // Compute statistics
    let chain_statistics = compute_statistics(&chains);

    ConditionalAnalysis {
        chains,
        node_to_chains,
        chain_statistics,
    }
}

/// Detector for conditional chains
struct ConditionalDetector<'a> {
    graph: &'a DiGraph<Block, EdgeKind>,
    post_doms: &'a PostDominatorAnalysis,
    processed: HashSet<NodeIndex>,
    chain_id_counter: usize,
}

impl<'a> ConditionalDetector<'a> {
    fn new(graph: &'a DiGraph<Block, EdgeKind>, post_doms: &'a PostDominatorAnalysis) -> Self {
        Self {
            graph,
            post_doms,
            processed: HashSet::new(),
            chain_id_counter: 0,
        }
    }

    /// Detect all conditional chains hierarchically
    fn detect_all_chains_hierarchical(&mut self) -> Vec<ConditionalChain> {
        let entry = NodeIndex::new(0);
        let exit = self.find_exit_block();

        self.detect_chains_in_region(entry, exit, 1)
    }

    /// Find the exit block (node with no successors or is_exit flag)
    fn find_exit_block(&self) -> Option<NodeIndex> {
        self.graph
            .node_indices()
            .find(|&n| self.graph[n].is_exit() || self.graph.edges(n).count() == 0)
    }

    /// Detect chains within a specific region
    fn detect_chains_in_region(
        &mut self,
        region_entry: NodeIndex,
        region_exit: Option<NodeIndex>,
        depth: usize,
    ) -> Vec<ConditionalChain> {
        let mut chains = Vec::new();

        // Find all unprocessed conditionals in this region
        let conditionals = self.find_conditionals_in_region(region_entry, region_exit);

        for &cond_node in &conditionals {
            if self.processed.contains(&cond_node) {
                continue;
            }

            if let Some(mut chain) = self.build_chain_from_hierarchical(cond_node, depth) {
                // Recursively detect chains within each branch
                for branch in &mut chain.branches {
                    let branch_entry = branch.branch_entry;
                    let branch_exit = Some(chain.join_block);

                    // Skip if the branch immediately exits
                    if Some(branch_entry) == branch_exit {
                        continue;
                    }

                    let nested = self.detect_chains_in_region(branch_entry, branch_exit, depth + 1);

                    if !nested.is_empty() {
                        // Remove blocks that belong to nested chains from parent branch
                        let mut nested_blocks = HashSet::new();

                        // Recursively collect ALL blocks from nested chains
                        fn collect_all_nested_blocks(
                            chains: &[ConditionalChain],
                            blocks: &mut HashSet<NodeIndex>,
                        ) {
                            for chain in chains {
                                for branch in &chain.branches {
                                    blocks.insert(branch.condition_block);
                                    blocks.extend(&branch.branch_blocks);
                                }
                                blocks.insert(chain.join_block);
                                // Recursively collect from deeper nested chains
                                collect_all_nested_blocks(&chain.nested_chains, blocks);
                            }
                        }

                        collect_all_nested_blocks(&nested, &mut nested_blocks);

                        // Filter out nested blocks from parent branch
                        branch
                            .branch_blocks
                            .retain(|&block| !nested_blocks.contains(&block));

                        chain.nested_chains.extend(nested);
                    }
                }

                chains.push(chain);
            }
        }

        chains
    }

    /// Find all conditional nodes within a region
    fn find_conditionals_in_region(
        &self,
        entry: NodeIndex,
        exit: Option<NodeIndex>,
    ) -> Vec<NodeIndex> {
        let mut conditionals = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(entry);

        while let Some(node) = queue.pop_front() {
            if !visited.insert(node) {
                continue;
            }

            // Stop at exit
            if Some(node) == exit {
                continue;
            }

            // Check if this is a conditional
            if self.is_conditional_node(node) && !self.processed.contains(&node) {
                conditionals.push(node);
            }

            // Add successors
            for edge in self.graph.edges(node) {
                let target = edge.target();
                if !visited.contains(&target) && Some(target) != exit {
                    queue.push_back(target);
                }
            }
        }

        // Sort for deterministic processing
        conditionals.sort_by_key(|n| n.index());
        conditionals
    }

    /// Check if a node is a conditional (has both True and False edges)
    fn is_conditional_node(&self, node: NodeIndex) -> bool {
        let mut has_true = false;
        let mut has_false = false;

        for edge in self.graph.edges(node) {
            match edge.weight() {
                EdgeKind::True => has_true = true,
                EdgeKind::False => has_false = true,
                _ => {}
            }
        }

        has_true && has_false
    }

    /// Build a conditional chain starting from the given node (hierarchical)
    fn build_chain_from_hierarchical(
        &mut self,
        start_node: NodeIndex,
        depth: usize,
    ) -> Option<ConditionalChain> {
        let mut branches = Vec::new();
        let mut current_node = start_node;
        let mut chain_join = None;

        loop {
            // Get conditional targets
            let (true_target, false_target) = self.get_conditional_targets(current_node)?;

            // Find the join point using better heuristics for nested conditionals
            let local_join =
                self.find_proper_join_point(current_node, true_target, false_target, depth)?;

            // Track the chain join
            if chain_join.is_none() {
                chain_join = Some(local_join);
            }

            // Determine branch type
            let branch_type = if branches.is_empty() {
                BranchType::If
            } else {
                BranchType::ElseIf
            };

            // Collect true branch blocks (up to join)
            let true_blocks = self.collect_branch_blocks_bounded(true_target, local_join);
            branches.push(ConditionalBranch {
                condition_source: current_node,
                branch_type,
                condition_block: current_node,
                branch_entry: true_target,
                branch_blocks: true_blocks,
            });

            // Mark as processed
            self.processed.insert(current_node);

            // Check if false branch continues the chain
            if self.is_conditional_node(false_target) && !self.processed.contains(&false_target) {
                // Check if this is an else-if pattern
                let false_block = &self.graph[false_target];
                let is_else_if = false_block.instructions().len() <= 3;

                if is_else_if {
                    current_node = false_target;
                    continue;
                }
            }

            // Add else branch and stop
            let false_blocks = self.collect_branch_blocks_bounded(false_target, local_join);
            if !false_blocks.is_empty() || false_target != local_join {
                branches.push(ConditionalBranch {
                    condition_source: current_node,
                    branch_type: BranchType::Else,
                    condition_block: current_node,
                    branch_entry: false_target,
                    branch_blocks: false_blocks,
                });
            }
            break;
        }

        let chain_type = Self::classify_chain(&branches);
        let chain_id = self.chain_id_counter;
        self.chain_id_counter += 1;

        Some(ConditionalChain {
            chain_id,
            branches,
            join_block: chain_join?,
            chain_type,
            nesting_depth: depth,
            nested_chains: Vec::new(),
        })
    }

    /// Find the proper join point considering nesting depth
    fn find_proper_join_point(
        &self,
        cond_node: NodeIndex,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
        depth: usize,
    ) -> Option<NodeIndex> {
        // For nested conditionals, prefer joins that are not the global exit
        let exit = self.find_exit_block();

        // Method 1: Immediate convergence
        if let Some(join) = self.find_immediate_convergence(true_branch, false_branch) {
            // Always use immediate convergence if found - it's the most accurate
            return Some(join);
        }

        // Method 2: Find where both branches have returns/exits
        if let Some(join) = self.find_branch_exit_point(true_branch, false_branch) {
            if Some(join) != exit || depth == 1 {
                return Some(join);
            }
        }

        // Method 3: Use the regular join point detection
        self.find_join_point(cond_node, true_branch, false_branch)
    }

    /// Find where both branches exit (return or jump out)
    fn find_branch_exit_point(
        &self,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
    ) -> Option<NodeIndex> {
        // If both branches have no successors (returns), they join at exit
        let true_has_successors = self.graph.edges(true_branch).count() > 0;
        let false_has_successors = self.graph.edges(false_branch).count() > 0;

        if !true_has_successors && !false_has_successors {
            self.find_exit_block()
        } else {
            None
        }
    }

    /// Collect blocks in a branch up to the join point
    fn collect_branch_blocks_bounded(&self, entry: NodeIndex, join: NodeIndex) -> Vec<NodeIndex> {
        if entry == join {
            return vec![];
        }

        let mut blocks = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(entry);

        while let Some(node) = queue.pop_front() {
            if node == join || !visited.insert(node) {
                continue;
            }

            blocks.push(node);

            for edge in self.graph.edges(node) {
                let target = edge.target();
                if !visited.contains(&target) && target != join {
                    queue.push_back(target);
                }
            }
        }

        blocks
    }

    /// Get true and false targets from a conditional node
    fn get_conditional_targets(&self, node: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        let mut true_target = None;
        let mut false_target = None;

        for edge in self.graph.edges(node) {
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

    /// Find where two branches join
    fn find_join_point(
        &self,
        _cond_node: NodeIndex,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
    ) -> Option<NodeIndex> {
        // Method 1: Check for immediate convergence
        if let Some(join) = self.find_immediate_convergence(true_branch, false_branch) {
            return Some(join);
        }

        // Method 2: Find the immediate post-dominator
        // This is better than general reachability for nested conditionals
        if let Some(join) = self.find_immediate_post_dominator(true_branch, false_branch) {
            return Some(join);
        }

        // Method 3: Use reachability analysis to find convergence
        if let Some(join) = self.find_reachability_convergence(true_branch, false_branch) {
            // Verify it's not the exit block unless it's the only option
            if self.graph.edges(join).count() > 0 {
                return Some(join);
            }
        }

        // Method 4: Fall back to lowest common post-dominator
        find_lowest_common_post_dominator(self.post_doms, true_branch, false_branch)
    }

    /// Find the immediate post-dominator of two branches
    /// This is the closest block that post-dominates both branches
    fn find_immediate_post_dominator(
        &self,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
    ) -> Option<NodeIndex> {
        // First check if one branch post-dominates the other
        if self.post_doms.dominates(true_branch, false_branch) {
            return Some(true_branch);
        }
        if self.post_doms.dominates(false_branch, true_branch) {
            return Some(false_branch);
        }

        // Get immediate post-dominators and walk up the tree
        let mut current_true = self.post_doms.immediate_post_dominator(true_branch);
        let mut current_false = self.post_doms.immediate_post_dominator(false_branch);

        // Track visited nodes to find convergence
        let mut visited_from_true = HashSet::new();
        let mut visited_from_false = HashSet::new();

        visited_from_true.insert(true_branch);
        visited_from_false.insert(false_branch);

        // Walk up both post-dominator trees until we find convergence
        loop {
            // Check if we've reached a common node
            if let Some(node) = current_true {
                if visited_from_false.contains(&node) {
                    return Some(node);
                }
                visited_from_true.insert(node);
            }

            if let Some(node) = current_false {
                if visited_from_true.contains(&node) {
                    return Some(node);
                }
                visited_from_false.insert(node);
            }

            // Move up the post-dominator tree
            match (current_true, current_false) {
                (Some(t), Some(f)) => {
                    current_true = self.post_doms.immediate_post_dominator(t);
                    current_false = self.post_doms.immediate_post_dominator(f);
                }
                (Some(t), None) => {
                    current_true = self.post_doms.immediate_post_dominator(t);
                }
                (None, Some(f)) => {
                    current_false = self.post_doms.immediate_post_dominator(f);
                }
                (None, None) => break,
            }
        }

        None
    }

    /// Check if both branches immediately converge to the same block
    fn find_immediate_convergence(
        &self,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
    ) -> Option<NodeIndex> {
        // Get immediate successors
        let true_succs: HashSet<_> = self.graph.edges(true_branch).map(|e| e.target()).collect();
        let false_succs: HashSet<_> = self.graph.edges(false_branch).map(|e| e.target()).collect();

        // Find common immediate successors
        let common: Vec<_> = true_succs.intersection(&false_succs).copied().collect();

        if common.len() == 1 {
            Some(common[0])
        } else {
            None
        }
    }

    /// Use BFS to find where execution paths converge
    fn find_reachability_convergence(
        &self,
        true_branch: NodeIndex,
        false_branch: NodeIndex,
    ) -> Option<NodeIndex> {
        let mut true_visited = HashSet::new();
        let mut false_visited = HashSet::new();
        let mut true_queue = VecDeque::new();
        let mut false_queue = VecDeque::new();

        true_queue.push_back(true_branch);
        false_queue.push_back(false_branch);

        // Alternating BFS from both branches
        while !true_queue.is_empty() || !false_queue.is_empty() {
            // Process true branch
            if let Some(node) = true_queue.pop_front() {
                if false_visited.contains(&node) {
                    return Some(node);
                }
                if true_visited.insert(node) {
                    for edge in self.graph.edges(node) {
                        true_queue.push_back(edge.target());
                    }
                }
            }

            // Process false branch
            if let Some(node) = false_queue.pop_front() {
                if true_visited.contains(&node) {
                    return Some(node);
                }
                if false_visited.insert(node) {
                    for edge in self.graph.edges(node) {
                        false_queue.push_back(edge.target());
                    }
                }
            }
        }

        None
    }

    /// Classify the type of conditional chain
    fn classify_chain(branches: &[ConditionalBranch]) -> ChainType {
        match branches.len() {
            0..=1 => ChainType::SimpleIfElse,
            2 => {
                if branches[1].branch_type == BranchType::Else {
                    ChainType::SimpleIfElse
                } else {
                    ChainType::ElseIfChain
                }
            }
            _ => {
                if branches.iter().any(|b| b.branch_type == BranchType::ElseIf) {
                    ChainType::ElseIfChain
                } else {
                    ChainType::SimpleIfElse
                }
            }
        }
    }
}

/// Compute statistics for conditional chains
fn compute_statistics(chains: &[ConditionalChain]) -> ChainStatistics {
    let mut _total_chains = 0;
    let mut _simple_if_else_count = 0;
    let mut _else_if_chain_count = 0;
    let mut _max_chain_length = 0;
    let mut _max_nesting_depth = 0;

    fn count_chains_recursive(
        chains: &[ConditionalChain],
        stats: &mut (usize, usize, usize, usize, usize),
    ) {
        for chain in chains {
            stats.0 += 1; // total_chains

            match chain.chain_type {
                ChainType::SimpleIfElse => stats.1 += 1,
                ChainType::ElseIfChain => stats.2 += 1,
                _ => {}
            }

            stats.3 = stats.3.max(chain.branches.len()); // max_chain_length
            stats.4 = stats.4.max(chain.nesting_depth); // max_nesting_depth

            // Recursively count nested chains
            if !chain.nested_chains.is_empty() {
                count_chains_recursive(&chain.nested_chains, stats);
            }
        }
    }

    let mut stats = (0, 0, 0, 0, 0);
    count_chains_recursive(chains, &mut stats);

    ChainStatistics {
        total_chains: stats.0,
        simple_if_else_count: stats.1,
        else_if_chain_count: stats.2,
        max_chain_length: stats.3,
        max_nesting_depth: stats.4,
    }
}
