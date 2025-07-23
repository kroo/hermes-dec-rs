//! CFG analysis module
//! 
//! This module contains advanced analysis algorithms for CFGs.

use crate::cfg::{Block, EdgeKind};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::algo::dominators::Dominators;
use std::collections::{HashMap, HashSet};

/// Post-dominator analysis results
#[derive(Debug, Clone)]
pub struct PostDominatorAnalysis {
    pub post_dominators: HashMap<NodeIndex, HashSet<NodeIndex>>,
    pub immediate_post_dominators: HashMap<NodeIndex, Option<NodeIndex>>,
}

impl PostDominatorAnalysis {
    /// Check if a node post-dominates another node
    pub fn dominates(&self, post_dominator: NodeIndex, node: NodeIndex) -> bool {
        self.post_dominators
            .get(&node)
            .map(|doms| doms.contains(&post_dominator))
            .unwrap_or(false)
    }
    
    /// Get the immediate post-dominator of a node
    pub fn immediate_post_dominator(&self, node: NodeIndex) -> Option<NodeIndex> {
        self.immediate_post_dominators.get(&node).copied().flatten()
    }
}

/// Natural loop information
#[derive(Debug, Clone)]
pub struct Loop {
    pub header: NodeIndex,
    pub body_nodes: HashSet<NodeIndex>,
    pub back_edges: Vec<(NodeIndex, NodeIndex)>, // (tail, header) pairs
}

/// Loop analysis results
#[derive(Debug, Clone)]
pub struct LoopAnalysis {
    pub loops: Vec<Loop>,
    pub node_to_loops: HashMap<NodeIndex, Vec<usize>>, // node -> loop indices
}

impl LoopAnalysis {
    /// Get all loops containing a specific node
    pub fn get_loops_containing_node(&self, node: NodeIndex) -> &[usize] {
        self.node_to_loops.get(&node).map(|v| v.as_slice()).unwrap_or(&[])
    }
    
    /// Check if a node is part of any loop
    pub fn is_node_in_loop(&self, node: NodeIndex) -> bool {
        self.node_to_loops.contains_key(&node)
    }
    
    /// Get the innermost loop containing a node
    pub fn get_innermost_loop(&self, node: NodeIndex) -> Option<&Loop> {
        self.node_to_loops.get(&node)
            .and_then(|indices| indices.last())
            .map(|&idx| &self.loops[idx])
    }
}

/// If/else region information
#[derive(Debug, Clone)]
pub struct IfElseRegion {
    pub conditional_source: NodeIndex,  // S: block with conditional jump
    pub then_head: NodeIndex,          // Then branch head
    pub else_head: NodeIndex,          // Else branch head  
    pub join_block: NodeIndex,         // J: lowest common post-dominator
}

/// If/else analysis results
#[derive(Debug, Clone)]
pub struct IfElseAnalysis {
    pub regions: Vec<IfElseRegion>,
    pub node_to_regions: HashMap<NodeIndex, Vec<usize>>, // node -> region indices
}

/// Switch case information
#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub case_index: usize,  // Switch case index
    pub case_head: NodeIndex, // First block of this case
}

/// Switch region information
#[derive(Debug, Clone)]
pub struct SwitchRegion {
    pub dispatch: NodeIndex,  // Block containing switch instruction
    pub cases: Vec<SwitchCase>,
    pub default_head: Option<NodeIndex>, // Default case head (if any)
    pub join_block: NodeIndex, // Common post-dominator of all cases
}

/// Switch analysis results
#[derive(Debug, Clone)]
pub struct SwitchAnalysis {
    pub regions: Vec<SwitchRegion>,
    pub node_to_regions: HashMap<NodeIndex, Vec<usize>>, // node -> region indices
}

impl SwitchAnalysis {
    /// Get all regions containing a specific node
    pub fn get_regions_containing_node(&self, node: NodeIndex) -> &[usize] {
        self.node_to_regions.get(&node).map(|v| v.as_slice()).unwrap_or(&[])
    }
    
    /// Check if a node is part of any switch region
    pub fn is_node_in_switch(&self, node: NodeIndex) -> bool {
        self.node_to_regions.contains_key(&node)
    }
    
    /// Get the switch region for a dispatch node
    pub fn get_switch_for_dispatch(&self, dispatch: NodeIndex) -> Option<&SwitchRegion> {
        self.node_to_regions.get(&dispatch)
            .and_then(|indices| indices.first())
            .map(|&idx| &self.regions[idx])
    }
}

/// Analysis functions for CFGs
pub fn analyze_post_dominators(_graph: &DiGraph<Block, EdgeKind>) -> Option<PostDominatorAnalysis> {
    // TODO: Implement post-dominator analysis
    // This will be implemented in CFG-06
    None
}

pub fn find_natural_loops(_graph: &DiGraph<Block, EdgeKind>, _dominators: &Dominators<NodeIndex>) -> LoopAnalysis {
    // TODO: Implement natural loop detection
    // This will be implemented in CFG-05
    LoopAnalysis {
        loops: Vec::new(),
        node_to_loops: HashMap::new(),
    }
}

pub fn find_if_else_regions(_graph: &DiGraph<Block, EdgeKind>, _post_doms: &PostDominatorAnalysis) -> IfElseAnalysis {
    // TODO: Implement if/else region detection
    // This will be implemented in CFG-07
    IfElseAnalysis {
        regions: Vec::new(),
        node_to_regions: HashMap::new(),
    }
}

pub fn find_switch_regions(_graph: &DiGraph<Block, EdgeKind>, _post_doms: &PostDominatorAnalysis) -> SwitchAnalysis {
    // TODO: Implement switch region detection
    // This will be implemented in CFG-08
    SwitchAnalysis {
        regions: Vec::new(),
        node_to_regions: HashMap::new(),
    }
} 