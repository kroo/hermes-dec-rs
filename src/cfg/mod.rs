//! Control Flow Graph (CFG) module
//! 
//! This module handles building and analyzing control flow graphs from HBC instructions.

pub mod block;
pub mod builder;
pub mod analysis;
pub mod visualization;
pub mod regions;

use petgraph::graph::DiGraph;
use petgraph::algo::dominators::Dominators;
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
            builder: builder::CfgBuilder::new(),
        }
    }
    
    /// Build CFG from instructions
    pub fn build_from_instructions(&mut self, instructions: &[crate::hbc::function_table::HbcFunctionInstruction], function_index: u32) -> &mut Self {
        self.graph = self.builder.build_from_instructions(instructions, function_index);
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
}

impl Default for Cfg {
    fn default() -> Self {
        Self::new()
    }
}

// Re-export main types for convenience
pub use block::Block;
pub use builder::CfgBuilder; 
