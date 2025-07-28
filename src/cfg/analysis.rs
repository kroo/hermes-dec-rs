//! CFG analysis module
//!
//! This module contains advanced analysis algorithms for CFGs.

use crate::cfg::{Block, EdgeKind};
use petgraph::algo::dominators::Dominators;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
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

/// Loop type classification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoopType {
    /// While loop (condition at entry)
    While,
    /// For loop (initialization, condition, increment)
    For,
    /// Do-while loop (condition at exit)
    DoWhile,
}

/// Natural loop information
#[derive(Debug, Clone)]
pub struct Loop {
    pub headers: Vec<NodeIndex>, // Multiple headers for irreducible loops
    pub body_nodes: HashSet<NodeIndex>,
    pub back_edges: Vec<(NodeIndex, NodeIndex)>, // (tail, header) pairs
    pub loop_type: LoopType,
    pub exit_nodes: Vec<NodeIndex>,
    pub is_irreducible: bool, // Whether this is an irreducible loop
}

impl Loop {
    /// Get the primary header (first header for irreducible loops)
    pub fn primary_header(&self) -> NodeIndex {
        self.headers[0]
    }

    /// Check if a node is a header of this loop
    pub fn is_header(&self, node: NodeIndex) -> bool {
        self.headers.contains(&node)
    }

    /// Get all headers of this loop
    pub fn get_headers(&self) -> &[NodeIndex] {
        &self.headers
    }
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
        self.node_to_loops
            .get(&node)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Check if a node is part of any loop
    pub fn is_node_in_loop(&self, node: NodeIndex) -> bool {
        self.node_to_loops.contains_key(&node)
    }

    /// Get the innermost loop containing a node
    pub fn get_innermost_loop(&self, node: NodeIndex) -> Option<&Loop> {
        self.node_to_loops
            .get(&node)
            .and_then(|indices| indices.last())
            .map(|&idx| &self.loops[idx])
    }
}

/// If/else region information
#[derive(Debug, Clone)]
pub struct IfElseRegion {
    pub conditional_source: NodeIndex, // S: block with conditional jump
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
    pub case_index: usize,    // Switch case index
    pub case_head: NodeIndex, // First block of this case
}

/// Switch region information
#[derive(Debug, Clone)]
pub struct SwitchRegion {
    pub dispatch: NodeIndex, // Block containing switch instruction
    pub cases: Vec<SwitchCase>,
    pub default_head: Option<NodeIndex>, // Default case head (if any)
    pub join_block: NodeIndex,           // Common post-dominator of all cases
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
        self.node_to_regions
            .get(&node)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Check if a node is part of any switch region
    pub fn is_node_in_switch(&self, node: NodeIndex) -> bool {
        self.node_to_regions.contains_key(&node)
    }

    /// Get the switch region for a dispatch node
    pub fn get_switch_for_dispatch(&self, dispatch: NodeIndex) -> Option<&SwitchRegion> {
        self.node_to_regions
            .get(&dispatch)
            .and_then(|indices| indices.first())
            .map(|&idx| &self.regions[idx])
    }
}

pub fn find_natural_loops(
    _graph: &DiGraph<Block, EdgeKind>,
    _dominators: &Dominators<NodeIndex>,
) -> LoopAnalysis {
    // TODO: Implement natural loop detection
    // This will be implemented in CFG-05
    LoopAnalysis {
        loops: Vec::new(),
        node_to_loops: HashMap::new(),
    }
}

pub fn find_if_else_regions(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
) -> IfElseAnalysis {
    let mut regions = Vec::new();
    let mut node_to_regions = HashMap::new();

    // Step 1: Find all conditional source blocks
    for node in graph.node_indices() {
        if let Some(region) = detect_if_else_region(graph, post_doms, node) {
            let region_idx = regions.len();

            // Add region to list
            regions.push(region.clone());

            // Build node-to-region mapping
            add_nodes_to_region_mapping(&mut node_to_regions, &region, region_idx);
        }
    }

    IfElseAnalysis {
        regions,
        node_to_regions,
    }
}

/// Detect if/else region starting from a potential conditional source
fn detect_if_else_region(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    source: NodeIndex,
) -> Option<IfElseRegion> {
    use petgraph::Direction;

    // Check if this node has exactly True + False outgoing edges
    let outgoing_edges = graph.edges_directed(source, Direction::Outgoing);

    let mut true_target = None;
    let mut false_target = None;
    let mut other_edges = 0;

    for edge in outgoing_edges {
        match edge.weight() {
            EdgeKind::True => {
                if true_target.is_some() {
                    return None;
                } // Multiple True edges
                true_target = Some(edge.target());
            }
            EdgeKind::False => {
                if false_target.is_some() {
                    return None;
                } // Multiple False edges
                false_target = Some(edge.target());
            }
            _ => other_edges += 1,
        }
    }

    // Must have exactly True + False edges, no others
    if let (Some(then_head), Some(else_head)) = (true_target, false_target) {
        if other_edges == 0 {
            // Step 2: Find lowest common post-dominator
            if let Some(join_block) =
                find_lowest_common_post_dominator(post_doms, then_head, else_head)
            {
                return Some(IfElseRegion {
                    conditional_source: source,
                    then_head,
                    else_head,
                    join_block,
                });
            }
        }
    }

    None
}

/// Find the lowest common post-dominator of two nodes
fn find_lowest_common_post_dominator(
    post_doms: &PostDominatorAnalysis,
    node_a: NodeIndex,
    node_b: NodeIndex,
) -> Option<NodeIndex> {
    // Get all post-dominators of both nodes
    let post_doms_a = post_doms.post_dominators.get(&node_a)?;
    let post_doms_b = post_doms.post_dominators.get(&node_b)?;

    // Find intersection (common post-dominators)
    let common_post_doms: HashSet<_> = post_doms_a.intersection(post_doms_b).cloned().collect();

    if common_post_doms.is_empty() {
        return None;
    }

    // Find the "lowest" (closest to nodes) common post-dominator
    // This is the one that is post-dominated by all others
    for &candidate in &common_post_doms {
        let mut is_lowest = true;
        for &other in &common_post_doms {
            if candidate != other && post_doms.dominates(other, candidate) {
                is_lowest = false;
                break;
            }
        }
        if is_lowest {
            return Some(candidate);
        }
    }

    None
}

/// Add nodes from an if/else region to the node-to-region mapping
fn add_nodes_to_region_mapping(
    node_to_regions: &mut HashMap<NodeIndex, Vec<usize>>,
    region: &IfElseRegion,
    region_idx: usize,
) {
    // Add all nodes that are part of this if/else region
    let nodes = vec![
        region.conditional_source,
        region.then_head,
        region.else_head,
        region.join_block,
    ];

    for node in nodes {
        node_to_regions
            .entry(node)
            .or_insert_with(Vec::new)
            .push(region_idx);
    }
}

pub fn find_switch_regions(
    _graph: &DiGraph<Block, EdgeKind>,
    _post_doms: &PostDominatorAnalysis,
) -> SwitchAnalysis {
    // TODO: Implement switch region detection
    // This will be implemented in CFG-08
    SwitchAnalysis {
        regions: Vec::new(),
        node_to_regions: HashMap::new(),
    }
}
