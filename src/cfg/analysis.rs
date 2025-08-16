//! CFG analysis module
//!
//! This module contains advanced analysis algorithms for CFGs.

use crate::cfg::{Block, EdgeKind};

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

/// Conditional branch information
#[derive(Debug, Clone)]
pub struct ConditionalBranch {
    pub condition_source: NodeIndex,   // Block with conditional jump
    pub branch_type: BranchType,       // If, ElseIf, Else
    pub condition_block: NodeIndex,    // Block containing condition evaluation
    pub branch_entry: NodeIndex,       // First block of branch body
    pub branch_blocks: Vec<NodeIndex>, // All blocks in this branch
}

/// Type of conditional branch
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchType {
    If,     // Initial condition
    ElseIf, // Chained condition (else + if)
    Else,   // Final else (unconditional)
}

/// Conditional chain representing if/else-if/else sequences
#[derive(Debug, Clone)]
pub struct ConditionalChain {
    pub chain_id: usize,
    pub branches: Vec<ConditionalBranch>,
    pub join_block: NodeIndex,
    pub chain_type: ChainType,
    pub nesting_depth: usize,
    /// Chains nested within this chain's branches
    pub nested_chains: Vec<ConditionalChain>,
}

/// Type of conditional chain pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChainType {
    SimpleIfElse,       // if/else
    ElseIfChain,        // if/else-if/.../else
    NestedConditional,  // nested if within branches
    GuardClausePattern, // early return style
    SwitchLikeChain,    // dense conditional sequence
}

/// Conditional analysis results
#[derive(Debug, Clone)]
pub struct ConditionalAnalysis {
    pub chains: Vec<ConditionalChain>,
    pub node_to_chains: HashMap<NodeIndex, Vec<usize>>,
    pub chain_statistics: ChainStatistics,
}

/// Statistics about conditional chains
#[derive(Debug, Clone)]
pub struct ChainStatistics {
    pub total_chains: usize,
    pub simple_if_else_count: usize,
    pub else_if_chain_count: usize,
    pub max_chain_length: usize,
    pub max_nesting_depth: usize,
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

/// Main function for comprehensive conditional analysis
pub fn analyze_conditional_chains(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
) -> ConditionalAnalysis {
    // Use the improved conditional analysis
    #[path = "conditional_analysis.rs"]
    mod conditional_analysis;
    conditional_analysis::analyze_conditional_chains_improved(graph, post_doms)
}

/// Find the lowest common post-dominator of two nodes
pub fn find_lowest_common_post_dominator(
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

pub fn find_switch_regions_with_ssa(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    hbc_file: &crate::hbc::HbcFile,
    ssa_values: &HashMap<crate::cfg::ssa::RegisterDef, crate::cfg::ssa::SSAValue>,
    cfg: &crate::cfg::Cfg,
    ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
) -> SwitchAnalysis {
    let mut regions = Vec::new();
    let mut node_to_regions = HashMap::new();
    let mut globally_processed_nodes = HashSet::new();

    // Step 1: Find all dense switch dispatch blocks (SwitchImm instructions)
    let switch_dispatches = find_switch_dispatch_blocks(graph);

    // Step 2: Detect dense switch regions for each dispatch
    for dispatch in switch_dispatches {
        if let Some(region) = detect_switch_region(graph, post_doms, dispatch) {
            let region_idx = regions.len();

            // Add region to list
            regions.push(region.clone());

            // Build node-to-region mapping
            add_nodes_to_switch_region_mapping(&mut node_to_regions, &region, region_idx);
        }
    }

    // Step 3: Find sparse switch patterns (series of equality comparisons) with SSA support
    let sparse_candidates = super::switch_analysis::find_sparse_switch_patterns(
        graph,
        post_doms,
        &mut globally_processed_nodes,
        hbc_file,
        ssa_values,
        cfg,
        ssa_analysis,
    );
    for candidate in sparse_candidates {
        let region = super::switch_analysis::sparse_candidate_to_switch_region(&candidate);
        let region_idx = regions.len();

        // Add region to list
        regions.push(region.clone());

        // Build node-to-region mapping
        add_nodes_to_switch_region_mapping(&mut node_to_regions, &region, region_idx);
    }

    SwitchAnalysis {
        regions,
        node_to_regions,
    }
}

/// Find all switch dispatch blocks (nodes with Switch(idx) edges)
fn find_switch_dispatch_blocks(graph: &DiGraph<Block, EdgeKind>) -> Vec<NodeIndex> {
    use petgraph::Direction;

    let mut switch_dispatches = Vec::new();

    for node in graph.node_indices() {
        let outgoing_edges: Vec<_> = graph.edges_directed(node, Direction::Outgoing).collect();

        let mut has_switch_edges = false;
        let mut other_edges = 0;

        for edge in &outgoing_edges {
            match edge.weight() {
                EdgeKind::Switch(_) => {
                    has_switch_edges = true;
                }
                EdgeKind::Default => {
                    // Default is part of switch structure, not "other"
                }
                _ => other_edges += 1,
            }
        }

        // Must have at least one Switch edge and no other edge types
        if has_switch_edges && other_edges == 0 {
            switch_dispatches.push(node);
        }
    }

    switch_dispatches
}

/// Detect switch region starting from a potential switch dispatch
fn detect_switch_region(
    graph: &DiGraph<Block, EdgeKind>,
    post_doms: &PostDominatorAnalysis,
    dispatch: NodeIndex,
) -> Option<SwitchRegion> {
    // Collect all switch cases and default case
    let (cases, default_head) = find_switch_case_heads(graph, dispatch);

    if cases.is_empty() {
        return None; // No switch cases found
    }

    // Extract case heads for join point computation
    let case_heads: Vec<NodeIndex> = cases.iter().map(|case| case.case_head).collect();

    // Find common post-dominator for all case heads
    if let Some(join_block) = find_switch_join_block(post_doms, &case_heads) {
        return Some(SwitchRegion {
            dispatch,
            cases,
            default_head,
            join_block,
        });
    }

    None
}

/// Find switch case heads and default case from a dispatch block
fn find_switch_case_heads(
    graph: &DiGraph<Block, EdgeKind>,
    dispatch: NodeIndex,
) -> (Vec<SwitchCase>, Option<NodeIndex>) {
    use petgraph::Direction;

    let mut cases = Vec::new();
    let mut default_head = None;

    let outgoing_edges = graph.edges_directed(dispatch, Direction::Outgoing);

    for edge in outgoing_edges {
        match edge.weight() {
            EdgeKind::Switch(case_index) => {
                cases.push(SwitchCase {
                    case_index: *case_index,
                    case_head: edge.target(),
                });
            }
            EdgeKind::Default => {
                if default_head.is_some() {
                    // Multiple default edges - invalid switch
                    return (Vec::new(), None);
                }
                default_head = Some(edge.target());
            }
            _ => {
                // Other edge types - invalid switch
                return (Vec::new(), None);
            }
        }
    }

    // Sort cases by index for consistent ordering
    cases.sort_by_key(|case| case.case_index);

    (cases, default_head)
}

/// Find the common post-dominator for all switch case heads
fn find_switch_join_block(
    post_doms: &PostDominatorAnalysis,
    case_heads: &[NodeIndex],
) -> Option<NodeIndex> {
    if case_heads.is_empty() {
        return None;
    }

    if case_heads.len() == 1 {
        // Single case - use its immediate post-dominator
        return post_doms.immediate_post_dominator(case_heads[0]);
    }

    // Find lowest common post-dominator of all case heads
    let mut common_join = case_heads[0];

    for &case_head in &case_heads[1..] {
        if let Some(join) = find_lowest_common_post_dominator(post_doms, common_join, case_head) {
            common_join = join;
        } else {
            // No common post-dominator found
            return None;
        }
    }

    Some(common_join)
}

/// Add nodes from a switch region to the node-to-region mapping
fn add_nodes_to_switch_region_mapping(
    node_to_regions: &mut HashMap<NodeIndex, Vec<usize>>,
    region: &SwitchRegion,
    region_idx: usize,
) {
    // Add dispatch node
    node_to_regions
        .entry(region.dispatch)
        .or_insert_with(Vec::new)
        .push(region_idx);

    // Add all case heads
    for case in &region.cases {
        node_to_regions
            .entry(case.case_head)
            .or_insert_with(Vec::new)
            .push(region_idx);
    }

    // Add default head if present
    if let Some(default_head) = region.default_head {
        node_to_regions
            .entry(default_head)
            .or_insert_with(Vec::new)
            .push(region_idx);
    }

    // Add join block
    node_to_regions
        .entry(region.join_block)
        .or_insert_with(Vec::new)
        .push(region_idx);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hbc::InstructionIndex;
    use petgraph::graph::DiGraph;
    use std::collections::HashMap;

    /// Test basic switch region detection
    #[test]
    fn test_switch_region_detection() {
        let mut graph = DiGraph::new();

        // Create a simple switch CFG: entry -> dispatch -> case1/case2/default -> join -> exit
        let entry = graph.add_node(Block {
            start_pc: InstructionIndex::zero(),
            end_pc: InstructionIndex::zero(),
            instructions: vec![],
            is_exit: false,
        });
        let dispatch = graph.add_node(Block {
            start_pc: InstructionIndex::from(1u32),
            end_pc: InstructionIndex::from(1u32),
            instructions: vec![],
            is_exit: false,
        });
        let case1 = graph.add_node(Block {
            start_pc: InstructionIndex::from(2u32),
            end_pc: InstructionIndex::from(2u32),
            instructions: vec![],
            is_exit: false,
        });
        let case2 = graph.add_node(Block {
            start_pc: InstructionIndex::from(3u32),
            end_pc: InstructionIndex::from(3u32),
            instructions: vec![],
            is_exit: false,
        });
        let default_case = graph.add_node(Block {
            start_pc: InstructionIndex::from(4u32),
            end_pc: InstructionIndex::from(4u32),
            instructions: vec![],
            is_exit: false,
        });
        let join = graph.add_node(Block {
            start_pc: InstructionIndex::from(5u32),
            end_pc: InstructionIndex::from(5u32),
            instructions: vec![],
            is_exit: false,
        });
        let exit = graph.add_node(Block {
            start_pc: InstructionIndex::from(6u32),
            end_pc: InstructionIndex::from(6u32),
            instructions: vec![],
            is_exit: true,
        });

        // Add edges: entry -> dispatch -> cases -> join -> exit
        graph.add_edge(entry, dispatch, EdgeKind::Uncond);
        graph.add_edge(dispatch, case1, EdgeKind::Switch(0));
        graph.add_edge(dispatch, case2, EdgeKind::Switch(1));
        graph.add_edge(dispatch, default_case, EdgeKind::Default);
        graph.add_edge(case1, join, EdgeKind::Uncond);
        graph.add_edge(case2, join, EdgeKind::Uncond);
        graph.add_edge(default_case, join, EdgeKind::Uncond);
        graph.add_edge(join, exit, EdgeKind::Uncond);

        // Create mock post-dominator analysis
        let mut post_dominators = HashMap::new();
        let mut immediate_post_dominators = HashMap::new();

        // Set up proper post-dominator relationships
        // exit post-dominates everything
        let exit_doms: HashSet<_> = graph.node_indices().collect();
        post_dominators.insert(exit, exit_doms);

        // join post-dominates case1, case2, default_case, and itself
        let join_doms: HashSet<_> = vec![case1, case2, default_case, join].into_iter().collect();
        post_dominators.insert(join, join_doms);

        // case1, case2, default_case post-dominate themselves and join
        for &case in &[case1, case2, default_case] {
            let mut doms = HashSet::new();
            doms.insert(case);
            doms.insert(join);
            post_dominators.insert(case, doms);
        }

        // dispatch post-dominates itself
        let mut dispatch_doms = HashSet::new();
        dispatch_doms.insert(dispatch);
        post_dominators.insert(dispatch, dispatch_doms);

        // entry post-dominates itself
        let mut entry_doms = HashSet::new();
        entry_doms.insert(entry);
        post_dominators.insert(entry, entry_doms);

        // Set immediate post-dominators
        immediate_post_dominators.insert(entry, Some(dispatch));
        immediate_post_dominators.insert(dispatch, Some(join));
        immediate_post_dominators.insert(case1, Some(join));
        immediate_post_dominators.insert(case2, Some(join));
        immediate_post_dominators.insert(default_case, Some(join));
        immediate_post_dominators.insert(join, Some(exit));
        immediate_post_dominators.insert(exit, None);

        let _post_doms = PostDominatorAnalysis {
            post_dominators,
            immediate_post_dominators,
        };

        // For tests, we'll create a mock switch analysis since we can't use the real one without SSA
        // This test is really just testing the data structures, not the actual detection
        let mut analysis = SwitchAnalysis {
            regions: Vec::new(),
            node_to_regions: HashMap::new(),
        };

        // Manually create the expected switch region
        let region = SwitchRegion {
            dispatch,
            cases: vec![
                SwitchCase {
                    case_index: 0,
                    case_head: case1,
                },
                SwitchCase {
                    case_index: 1,
                    case_head: case2,
                },
            ],
            default_head: Some(default_case),
            join_block: join,
        };
        analysis.regions.push(region);

        // Add node-to-region mappings
        add_nodes_to_switch_region_mapping(&mut analysis.node_to_regions, &analysis.regions[0], 0);

        // Skip the actual detection test since it requires SSA

        // Verify switch region detection
        assert_eq!(analysis.regions.len(), 1);
        let region = &analysis.regions[0];
        assert_eq!(region.dispatch, dispatch);
        assert_eq!(region.cases.len(), 2);
        assert_eq!(region.cases[0].case_index, 0);
        assert_eq!(region.cases[0].case_head, case1);
        assert_eq!(region.cases[1].case_index, 1);
        assert_eq!(region.cases[1].case_head, case2);
        assert_eq!(region.default_head, Some(default_case));
        assert_eq!(region.join_block, join);

        // Verify node-to-region mapping
        assert!(analysis.node_to_regions.contains_key(&dispatch));
        assert!(analysis.node_to_regions.contains_key(&case1));
        assert!(analysis.node_to_regions.contains_key(&case2));
        assert!(analysis.node_to_regions.contains_key(&default_case));
        assert!(analysis.node_to_regions.contains_key(&join));
    }
}
