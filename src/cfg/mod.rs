//! Control Flow Graph (CFG) module
//!
//! This module handles building and analyzing control flow graphs from HBC instructions.

pub mod analysis;
pub mod block;
pub mod builder;
pub mod regions;
pub mod visualization;

use crate::hbc::HbcFile;
use petgraph::algo::dominators::Dominators;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

/// Edge kind in the control flow graph
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdgeKind {
    /// Unconditional jump
    Uncond,
    /// Conditional jump (true branch)
    True,
    /// Conditional jump (false branch)
    False,
    /// Switch case jump
    Switch(usize),
    /// Default case for switch
    Default,
    /// Fallthrough to next block
    Fall,
    /// Generator fallthrough (initial execution continues to next instruction)
    GeneratorFallthrough,
    /// Generator resume (resumes execution at suspension label)
    GeneratorResume,
}

/// Main CFG struct that provides high-level interface
pub struct Cfg<'a> {
    /// The underlying graph
    graph: DiGraph<block::Block, EdgeKind>,
    /// Builder for construction operations
    builder: builder::CfgBuilder<'a>,
}

impl<'a> Cfg<'a> {
    /// Create a new empty CFG
    pub fn new(hbc_file: &'a HbcFile<'a>, function_index: u32) -> Self {
        Self {
            graph: DiGraph::new(),
            builder: builder::CfgBuilder::new(hbc_file, function_index),
        }
    }

    /// Build CFG from instructions
    pub fn build(&mut self) -> &mut Self {
        self.graph = self.builder.build();
        self
    }

    /// Get the underlying graph
    pub fn graph(&self) -> &DiGraph<block::Block, EdgeKind> {
        &self.graph
    }

    /// Get the underlying graph mutably
    pub fn graph_mut(&mut self) -> &mut DiGraph<block::Block, EdgeKind> {
        &mut self.graph
    }

    /// Get the builder for advanced operations
    pub fn builder(&self) -> &builder::CfgBuilder<'a> {
        &self.builder
    }

    /// Get the builder mutably for advanced operations
    pub fn builder_mut(&mut self) -> &mut builder::CfgBuilder<'a> {
        &mut self.builder
    }

    /// Analyze dominators for the CFG
    pub fn analyze_dominators(&self) -> Option<Dominators<NodeIndex>> {
        self.builder.analyze_dominators(&self.graph)
    }

    /// Find natural loops in the CFG
    pub fn find_natural_loops(&self) -> Vec<(NodeIndex, NodeIndex)> {
        self.builder.find_natural_loops(&self.graph)
    }

    /// Find if/else join blocks
    pub fn find_if_else_joins(&self) -> Vec<NodeIndex> {
        self.builder.find_if_else_joins(&self.graph)
    }

    /// Find switch dispatch patterns
    pub fn find_switch_dispatches(&self) -> Vec<Vec<NodeIndex>> {
        self.builder.find_switch_dispatches(&self.graph)
    }

    /// Get dominator tree as a string representation
    pub fn dominator_tree_string(&self) -> String {
        self.builder.dominator_tree_string(&self.graph)
    }

    /// Export CFG to DOT format for visualization
    pub fn to_dot(&self) -> String {
        self.builder.to_dot(&self.graph)
    }

    /// Export CFG to DOT format with disassembled instructions
    pub fn to_dot_with_disassembly(&self, hbc_file: &HbcFile) -> String {
        self.builder.to_dot_with_disassembly(&self.graph, hbc_file)
    }

    /// Export CFG to DOT format as a subgraph for a specific function
    pub fn to_dot_subgraph(&self, hbc_file: &HbcFile, function_index: u32) -> String {
        self.builder
            .to_dot_subgraph(&self.graph, hbc_file, function_index)
    }

    /// Get the entry node for the function (first block)
    pub fn entry_node(&self) -> Option<NodeIndex> {
        self.graph.node_indices().next()
    }

    /// Get blocks in execution-friendly order (depth-first from entry, handling loops appropriately)
    pub fn execution_order(&self) -> Vec<NodeIndex> {
        let mut visited = std::collections::HashSet::new();
        let mut order = Vec::new();
        
        if let Some(entry) = self.entry_node() {
            self.dfs_execution_order(entry, &mut visited, &mut order);
        }
        
        // Add any remaining unvisited nodes (shouldn't happen in well-formed CFGs)
        for node in self.graph.node_indices() {
            if !visited.contains(&node) && !self.graph[node].is_exit() {
                order.push(node);
            }
        }
        
        order
    }

    /// Get blocks in a more sophisticated order that prioritizes control flow structure
    /// This method attempts to order blocks to minimize forward jumps and handle loops better
    pub fn structured_execution_order(&self) -> Vec<NodeIndex> {
        let mut visited = std::collections::HashSet::new();
        let mut order = Vec::new();
        
        // Start with entry block
        if let Some(entry) = self.entry_node() {
            self.structured_dfs(entry, &mut visited, &mut order);
        }
        
        // Add any remaining nodes (dead code or disconnected components)
        for node in self.graph.node_indices() {
            if !visited.contains(&node) && !self.graph[node].is_exit() {
                order.push(node);
            }
        }
        
        order
    }

    /// Structured depth-first search that respects control flow patterns
    fn structured_dfs(
        &self,
        node: NodeIndex,
        visited: &mut std::collections::HashSet<NodeIndex>,
        order: &mut Vec<NodeIndex>,
    ) {
        if visited.contains(&node) || self.graph[node].is_exit() {
            return;
        }
        
        visited.insert(node);
        order.push(node);
        
        // Collect outgoing edges with their types
        let mut edges: Vec<_> = self.graph
            .edges_directed(node, petgraph::Direction::Outgoing)
            .map(|edge| (edge.target(), edge.weight()))
            .collect();
        
        // Sort edges by priority:
        // 1. Fall-through edges (sequential execution)
        // 2. True branches (if conditions)
        // 3. False branches (else conditions)
        // 4. Unconditional jumps
        // 5. Everything else
        edges.sort_by(|a, b| {
            let priority_a = self.edge_priority(a.1);
            let priority_b = self.edge_priority(b.1);
            priority_a.cmp(&priority_b).then(a.0.cmp(&b.0))
        });
        
        // Visit neighbors in priority order
        for (target, _) in edges {
            self.structured_dfs(target, visited, order);
        }
    }

    /// Get priority for edge types (lower number = higher priority)
    fn edge_priority(&self, edge_kind: &EdgeKind) -> u32 {
        match edge_kind {
            EdgeKind::Fall => 0,                    // Highest priority - sequential execution
            EdgeKind::GeneratorFallthrough => 1,    // Generator sequential flow
            EdgeKind::True => 2,                     // True branch of conditionals
            EdgeKind::False => 3,                    // False branch of conditionals
            EdgeKind::Uncond => 4,                   // Unconditional jumps
            EdgeKind::Switch(_) => 5,                // Switch cases
            EdgeKind::Default => 6,                  // Switch default
            EdgeKind::GeneratorResume => 7,          // Generator resume points
        }
    }

    /// Get blocks that need labels (are targets of jumps that aren't simple fall-through)
    pub fn blocks_needing_labels(&self) -> std::collections::HashSet<NodeIndex> {
        let mut needs_labels = std::collections::HashSet::new();
        
        for edge in self.graph.edge_references() {
            match edge.weight() {
                // These edge types indicate the target might need a label
                EdgeKind::Uncond |
                EdgeKind::True |
                EdgeKind::False |
                EdgeKind::Switch(_) |
                EdgeKind::Default |
                EdgeKind::GeneratorResume => {
                    needs_labels.insert(edge.target());
                }
                // Fall-through and generator fallthrough are sequential, no label needed
                EdgeKind::Fall |
                EdgeKind::GeneratorFallthrough => {}
            }
        }
        
        // Also include blocks that have multiple incoming edges (potential merge points)
        for node in self.graph.node_indices() {
            let incoming_count = self.graph
                .neighbors_directed(node, petgraph::Direction::Incoming)
                .count();
            if incoming_count > 1 {
                needs_labels.insert(node);
            }
        }
        
        needs_labels
    }

    /// Helper for depth-first traversal that respects control flow structure
    fn dfs_execution_order(
        &self,
        node: NodeIndex,
        visited: &mut std::collections::HashSet<NodeIndex>,
        order: &mut Vec<NodeIndex>,
    ) {
        if visited.contains(&node) || self.graph[node].is_exit() {
            return;
        }
        
        visited.insert(node);
        order.push(node);
        
        // Get neighbors in a consistent order
        let mut neighbors: Vec<_> = self.graph
            .neighbors_directed(node, petgraph::Direction::Outgoing)
            .collect();
        
        // Sort neighbors to ensure deterministic ordering
        neighbors.sort();
        
        // Visit neighbors depth-first
        for neighbor in neighbors {
            self.dfs_execution_order(neighbor, visited, order);
        }
    }

    /// Get the EXIT node for the current function
    pub fn exit_node(&self) -> Option<NodeIndex> {
        self.builder.exit_node()
    }

    /// Find all EXIT nodes in the graph
    pub fn find_exit_nodes(&self) -> Vec<NodeIndex> {
        self.graph
            .node_indices()
            .filter(|&node| self.graph[node].is_exit())
            .collect()
    }

    /// Check if all terminating blocks have a path to EXIT
    pub fn all_terminators_reach_exit(&self) -> bool {
        if let Some(exit_node) = self.exit_node() {
            for node in self.graph.node_indices() {
                let block = &self.graph[node];
                if block.is_terminating() {
                    // Check if this terminating block has a direct edge to EXIT
                    let has_exit_edge = self
                        .graph
                        .neighbors_directed(node, petgraph::Direction::Outgoing)
                        .any(|neighbor| neighbor == exit_node);
                    if !has_exit_edge {
                        return false;
                    }
                }
            }
            true
        } else {
            // If no EXIT node, check if there are any terminating blocks
            !self
                .graph
                .node_indices()
                .any(|node| self.graph[node].is_terminating())
        }
    }

    /// Check if the CFG remains acyclic
    pub fn is_acyclic(&self) -> bool {
        !petgraph::algo::is_cyclic_directed(&self.graph)
    }

    /// Analyze loops in the CFG
    pub fn analyze_loops(&self) -> analysis::LoopAnalysis {
        self.builder.analyze_loops(&self.graph)
    }

    /// Export CFG to DOT format with loop analysis visualization
    pub fn to_dot_with_loops(&self) -> String {
        self.builder.to_dot_with_loops(&self.graph)
    }

    /// Analyze post-dominators for the CFG
    pub fn analyze_post_dominators(&self) -> Option<analysis::PostDominatorAnalysis> {
        self.builder.analyze_post_dominators(&self.graph)
    }

    /// Analyze conditional chains including if/else-if/else patterns
    pub fn analyze_conditional_chains(&self) -> Option<analysis::ConditionalAnalysis> {
        let post_doms = self.analyze_post_dominators()?;
        Some(analysis::analyze_conditional_chains(
            &self.graph,
            &post_doms,
        ))
    }

    /// Legacy analyze if/else regions (deprecated - use analyze_conditional_chains)
    pub fn analyze_if_else_regions(&self) -> Option<analysis::IfElseAnalysis> {
        let post_doms = self.analyze_post_dominators()?;
        Some(analysis::find_if_else_regions(&self.graph, &post_doms))
    }

    /// Analyze switch regions in the CFG
    pub fn analyze_switch_regions(&self) -> Option<analysis::SwitchAnalysis> {
        let post_doms = self.analyze_post_dominators()?;
        Some(analysis::find_switch_regions(&self.graph, &post_doms))
    }

    /// Export CFG to DOT format with comprehensive analysis visualization
    pub fn to_dot_with_analysis(&self, hbc_file: &HbcFile) -> String {
        self.builder.to_dot_with_analysis(&self.graph, hbc_file)
    }

    /// Export CFG to DOT format as a subgraph with comprehensive analysis
    pub fn to_dot_subgraph_with_analysis(&self, hbc_file: &HbcFile, function_index: u32) -> String {
        self.builder
            .to_dot_subgraph_with_analysis(&self.graph, hbc_file, function_index)
    }
}

// Re-export main types for convenience
pub use block::Block;
pub use builder::CfgBuilder;
