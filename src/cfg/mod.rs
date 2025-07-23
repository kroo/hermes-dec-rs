//! Control Flow Graph (CFG) module
//!
//! This module handles building and analyzing control flow graphs from HBC instructions.

pub mod analysis;
pub mod block;
pub mod builder;
pub mod regions;
pub mod visualization;

use petgraph::algo::dominators::Dominators;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;

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
}

/// Main CFG struct that provides high-level interface
pub struct Cfg {
    /// The underlying graph
    graph: DiGraph<block::Block, EdgeKind>,
    /// Builder for construction operations
    builder: builder::CfgBuilder,
}

impl Cfg {
    /// Create a new empty CFG
    pub fn new() -> Self {
        Self {
            graph: DiGraph::new(),
            builder: builder::CfgBuilder::new(0),
        }
    }

    /// Build CFG from instructions
    pub fn build_from_instructions(
        &mut self,
        instructions: &[crate::hbc::function_table::HbcFunctionInstruction],
        function_index: u32,
    ) -> &mut Self {
        self.builder = builder::CfgBuilder::new(function_index);
        self.graph = self.builder.build_from_instructions(instructions);
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
    pub fn builder(&self) -> &builder::CfgBuilder {
        &self.builder
    }

    /// Get the builder mutably for advanced operations
    pub fn builder_mut(&mut self) -> &mut builder::CfgBuilder {
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
                    let has_exit_edge = self.graph
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
            !self.graph.node_indices().any(|node| self.graph[node].is_terminating())
        }
    }

    /// Check if the CFG remains acyclic
    pub fn is_acyclic(&self) -> bool {
        !petgraph::algo::is_cyclic_directed(&self.graph)
    }
}

impl Default for Cfg {
    fn default() -> Self {
        Self::new()
    }
}

// Re-export main types for convenience
pub use block::Block;
pub use builder::CfgBuilder;
