//! CFG regions module
//!
//! This module contains region detection and analysis functionality.

use crate::cfg::{Block, EdgeKind};
use petgraph::graph::{DiGraph, NodeIndex};

/// Basic block variant for different types of blocks
#[derive(Debug, Clone)]
pub enum BasicBlock {
    /// Code block with instructions
    Code {
        /// Instructions in this block
        instructions: Vec<crate::hbc::function_table::HbcFunctionInstruction>,
        /// Start PC of this block
        start_pc: u32,
        /// End PC of this block
        end_pc: u32,
    },
    /// Exit node (synthetic)
    Exit,
}

/// Region detection context
pub struct RegionContext {
    /// The CFG graph
    graph: DiGraph<Block, EdgeKind>,
    /// Exit node (if any)
    exit_node: Option<NodeIndex>,
}

impl RegionContext {
    /// Create a new region context
    pub fn new(graph: DiGraph<Block, EdgeKind>) -> Self {
        Self {
            graph,
            exit_node: None,
        }
    }

    /// Set the exit node
    pub fn set_exit_node(&mut self, exit_node: NodeIndex) {
        self.exit_node = Some(exit_node);
    }

    /// Get the exit node
    pub fn exit_node(&self) -> Option<NodeIndex> {
        self.exit_node
    }

    /// Get the graph
    pub fn graph(&self) -> &DiGraph<Block, EdgeKind> {
        &self.graph
    }

    /// Get the graph mutably
    pub fn graph_mut(&mut self) -> &mut DiGraph<Block, EdgeKind> {
        &mut self.graph
    }
}

/// Region detection functions
pub fn find_conditional_sources(graph: &DiGraph<Block, EdgeKind>) -> Vec<NodeIndex> {
    let mut sources = Vec::new();

    for node in graph.node_indices() {
        let mut true_edges = 0;
        let mut false_edges = 0;

        for edge in graph.edges_directed(node, petgraph::Direction::Outgoing) {
            match edge.weight() {
                EdgeKind::True => true_edges += 1,
                EdgeKind::False => false_edges += 1,
                _ => {}
            }
        }

        // Valid if/else source has exactly one True and one False edge
        if true_edges == 1 && false_edges == 1 {
            sources.push(node);
        }
    }

    sources
}

pub fn find_switch_dispatches(graph: &DiGraph<Block, EdgeKind>) -> Vec<NodeIndex> {
    let mut dispatches = Vec::new();

    for node in graph.node_indices() {
        let mut switch_edges = 0;
        let _default_edges = 0; // Mark as unused for now

        for edge in graph.edges_directed(node, petgraph::Direction::Outgoing) {
            match edge.weight() {
                EdgeKind::Switch(_) => switch_edges += 1,
                EdgeKind::Default => {} // Count default edges if needed later
                _ => {}
            }
        }

        // Valid switch dispatch has multiple switch edges (and optionally default)
        if switch_edges >= 2 {
            dispatches.push(node);
        }
    }

    dispatches
}

pub fn find_terminating_blocks(graph: &DiGraph<Block, EdgeKind>) -> Vec<NodeIndex> {
    let mut terminating = Vec::new();

    for node in graph.node_indices() {
        if let Some(block) = graph.node_weight(node) {
            if let Some(last_instr) = block.last_instruction() {
                if matches!(last_instr.instruction.category(), "Return" | "Throw") {
                    terminating.push(node);
                }
            }
        }
    }

    terminating
}

/// Helper functions for region analysis
pub fn is_reachable(graph: &DiGraph<Block, EdgeKind>, from: NodeIndex, to: NodeIndex) -> bool {
    petgraph::algo::has_path_connecting(graph, from, to, None)
}

pub fn get_successors(graph: &DiGraph<Block, EdgeKind>, node: NodeIndex) -> Vec<NodeIndex> {
    graph
        .neighbors_directed(node, petgraph::Direction::Outgoing)
        .collect()
}

pub fn get_predecessors(graph: &DiGraph<Block, EdgeKind>, node: NodeIndex) -> Vec<NodeIndex> {
    graph
        .neighbors_directed(node, petgraph::Direction::Incoming)
        .collect()
}
