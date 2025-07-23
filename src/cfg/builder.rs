//! CFG builder module
//!
//! This module contains the CfgBuilder struct and related functionality.

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::tables::jump_table::JumpTable;
use petgraph::algo::dominators::{self, Dominators};
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::{HashMap, HashSet};

/// CFG builder and analyzer
pub struct CfgBuilder {
    /// Mapping from block start PC to node index
    block_starts: HashMap<u32, NodeIndex>,
    /// Mapping from every PC value within a block's range to the block node
    pc_to_block: HashMap<u32, NodeIndex>,
    /// Jump table for resolving jump targets
    jump_table: JumpTable,
}

impl CfgBuilder {
    /// Create a new CFG builder
    pub fn new() -> Self {
        CfgBuilder {
            block_starts: HashMap::new(),
            pc_to_block: HashMap::new(),
            jump_table: JumpTable::new(),
        }
    }

    /// Build CFG from instructions
    pub fn build_from_instructions(
        &mut self,
        instructions: &[HbcFunctionInstruction],
        function_index: u32,
    ) -> DiGraph<Block, EdgeKind> {
        // Clear lookup tables for a fresh build
        self.block_starts.clear();
        self.pc_to_block.clear();

        let mut graph = DiGraph::new();

        if instructions.is_empty() {
            return graph;
        }

        // Step 1: Build jump table for this function
        self.jump_table
            .build_for_function(function_index, instructions)
            .expect("Failed to build jump table");

        // Step 2: Find leaders (basic block entry points)
        let leaders = self.find_leaders(instructions);

        // Step 3: Create basic blocks
        let blocks = self.create_blocks(instructions, &leaders);

        // Step 4: Add blocks to graph and populate PC mapping
        for block in blocks {
            self.add_block(&mut graph, block);
        }

        // Step 5: Add edges between blocks
        self.add_edges(&mut graph, instructions);

        graph
    }

    /// Find basic block leaders
    fn find_leaders(&self, instructions: &[HbcFunctionInstruction]) -> HashSet<u32> {
        let mut leaders = HashSet::new();

        // Entry point is always a leader
        if !instructions.is_empty() {
            leaders.insert(0);
        }

        // Find branch targets and post-branch instructions
        for (i, instruction) in instructions.iter().enumerate() {
            let pc = i as u32;

            // Check if this is a jump instruction
            if instruction.instruction.category() == "Jump" {
                // Get jump target
                if let Some(target) = self.get_jump_target(instruction) {
                    leaders.insert(target);

                    // Post-branch instruction is also a leader (unless it's the target)
                    let post_branch = pc + 1;
                    if post_branch < instructions.len() as u32 && post_branch != target {
                        leaders.insert(post_branch);
                    }
                }
            }
        }

        leaders
    }

    /// Get jump target from instruction using jump table
    fn get_jump_target(&self, instruction: &HbcFunctionInstruction) -> Option<u32> {
        // Use the jump table to get the target instruction index
        if let Some(jump_label) = self
            .jump_table
            .get_label_by_jump_op_index(instruction.function_index, instruction.instruction_index)
        {
            // Find the label in the jump table to get the target instruction index
            if let Some(labels) = self
                .jump_table
                .get_labels_for_function(instruction.function_index)
            {
                for label in labels {
                    if &label.name == jump_label {
                        return Some(label.instruction_index);
                    }
                }
            }
        }
        None
    }

    /// Create basic blocks from instructions and leaders
    fn create_blocks(
        &self,
        instructions: &[HbcFunctionInstruction],
        leaders: &HashSet<u32>,
    ) -> Vec<Block> {
        let mut blocks = Vec::new();
        let mut sorted_leaders: Vec<u32> = leaders.iter().cloned().collect();
        sorted_leaders.sort();

        for (i, &leader_pc) in sorted_leaders.iter().enumerate() {
            let start_pc = leader_pc;
            let end_pc = if i + 1 < sorted_leaders.len() {
                sorted_leaders[i + 1]
            } else {
                instructions.len() as u32
            };

            let block_instructions: Vec<HbcFunctionInstruction> = instructions
                .iter()
                .skip(start_pc as usize)
                .take((end_pc - start_pc) as usize)
                .cloned()
                .collect();

            blocks.push(Block::new(start_pc, block_instructions));
        }

        blocks
    }

    /// Add a block to the graph and update lookup tables
    pub fn add_block(
        &mut self,
        graph: &mut DiGraph<Block, EdgeKind>,
        block: Block,
    ) -> NodeIndex {
        let start_pc = block.start_pc;
        let end_pc = block.end_pc;
        let node_index = graph.add_node(block);
        self.block_starts.insert(start_pc, node_index);
        for pc in start_pc..end_pc {
            self.pc_to_block.insert(pc, node_index);
        }
        node_index
    }

    /// Add edges between blocks
    fn add_edges(
        &mut self,
        graph: &mut DiGraph<Block, EdgeKind>,
        _instructions: &[HbcFunctionInstruction],
    ) {
        let mut block_ends: Vec<(u32, NodeIndex, Vec<HbcFunctionInstruction>)> = Vec::new();

        // Find all block end points and collect instructions
        for node_index in graph.node_indices() {
            let block = &graph[node_index];
            let end_pc = block.end_pc();
            let instructions = block.instructions().to_vec();
            block_ends.push((end_pc, node_index, instructions));
        }

        // Add edges based on control flow
        for (end_pc, from_node, instructions) in block_ends {
            if instructions.is_empty() {
                continue;
            }

            // Check if block ends with a jump
            if let Some(last_instruction) = instructions.last() {
                if last_instruction.instruction.category() == "Jump" {
                    // Add jump edge
                    if let Some(target) = self.get_jump_target(last_instruction) {
                        if let Some(&to_node) = self.block_starts.get(&target) {
                            let edge_kind = self.get_edge_kind(last_instruction);
                            graph.add_edge(from_node, to_node, edge_kind);
                        }
                    }

                    // Add fallthrough edge for conditional jumps
                    if self.is_conditional_jump(last_instruction) {
                        let fallthrough_pc = end_pc;
                        if let Some(&to_node) = self.block_starts.get(&fallthrough_pc) {
                            graph.add_edge(from_node, to_node, EdgeKind::Fall);
                        }
                    }
                } else {
                    // Fallthrough to next block
                    let fallthrough_pc = end_pc;
                    if let Some(&to_node) = self.block_starts.get(&fallthrough_pc) {
                        graph.add_edge(from_node, to_node, EdgeKind::Fall);
                    }
                }
            }
        }
    }

    /// Determine edge kind from jump instruction
    fn get_edge_kind(&self, instruction: &HbcFunctionInstruction) -> EdgeKind {
        match &instruction.instruction {
            UnifiedInstruction::Jmp { .. } | UnifiedInstruction::JmpLong { .. } => EdgeKind::Uncond,
            UnifiedInstruction::JmpTrue { .. } | UnifiedInstruction::JmpTrueLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JmpFalse { .. } | UnifiedInstruction::JmpFalseLong { .. } => {
                EdgeKind::False
            }
            UnifiedInstruction::SwitchImm { .. } => EdgeKind::Switch(0), // Default case
            _ => EdgeKind::Uncond, // Default for other conditional jumps
        }
    }

    /// Check if instruction is a conditional jump
    fn is_conditional_jump(&self, instruction: &HbcFunctionInstruction) -> bool {
        matches!(
            &instruction.instruction,
            UnifiedInstruction::JmpTrue { .. }
                | UnifiedInstruction::JmpTrueLong { .. }
                | UnifiedInstruction::JmpFalse { .. }
                | UnifiedInstruction::JmpFalseLong { .. }
                | UnifiedInstruction::JmpUndefined { .. }
                | UnifiedInstruction::JmpUndefinedLong { .. }
                | UnifiedInstruction::JEqual { .. }
                | UnifiedInstruction::JNotEqual { .. }
                | UnifiedInstruction::JLess { .. }
                | UnifiedInstruction::JGreater { .. }
                | UnifiedInstruction::JLessEqual { .. }
                | UnifiedInstruction::JGreaterEqual { .. }
                | UnifiedInstruction::JStrictEqual { .. }
                | UnifiedInstruction::JStrictNotEqual { .. }
                | UnifiedInstruction::JNotLess { .. }
                | UnifiedInstruction::JNotGreater { .. }
                | UnifiedInstruction::JNotLessEqual { .. }
                | UnifiedInstruction::JNotGreaterEqual { .. }
                | UnifiedInstruction::JLessN { .. }
                | UnifiedInstruction::JGreaterN { .. }
                | UnifiedInstruction::JLessEqualN { .. }
                | UnifiedInstruction::JGreaterEqualN { .. }
                | UnifiedInstruction::JNotLessN { .. }
                | UnifiedInstruction::JNotGreaterN { .. }
                | UnifiedInstruction::JNotLessEqualN { .. }
                | UnifiedInstruction::JNotGreaterEqualN { .. }
                | UnifiedInstruction::JEqualLong { .. }
                | UnifiedInstruction::JNotEqualLong { .. }
                | UnifiedInstruction::JLessLong { .. }
                | UnifiedInstruction::JGreaterLong { .. }
                | UnifiedInstruction::JLessEqualLong { .. }
                | UnifiedInstruction::JGreaterEqualLong { .. }
                | UnifiedInstruction::JStrictEqualLong { .. }
                | UnifiedInstruction::JStrictNotEqualLong { .. }
                | UnifiedInstruction::JNotLessLong { .. }
                | UnifiedInstruction::JNotGreaterLong { .. }
                | UnifiedInstruction::JNotLessEqualLong { .. }
                | UnifiedInstruction::JNotGreaterEqualLong { .. }
                | UnifiedInstruction::JLessNLong { .. }
                | UnifiedInstruction::JGreaterNLong { .. }
                | UnifiedInstruction::JLessEqualNLong { .. }
                | UnifiedInstruction::JGreaterEqualNLong { .. }
                | UnifiedInstruction::JNotLessNLong { .. }
                | UnifiedInstruction::JNotGreaterNLong { .. }
                | UnifiedInstruction::JNotLessEqualNLong { .. }
                | UnifiedInstruction::JNotGreaterEqualNLong { .. }
        )
    }

    /// Analyze dominators for the CFG
    pub fn analyze_dominators(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
    ) -> Option<Dominators<NodeIndex>> {
        if graph.node_count() == 0 {
            return None;
        }

        // Find the entry node (node with index 0, which should be the first block)
        let entry = graph.node_indices().next()?;

        // Compute dominators using the simple_fast algorithm
        Some(dominators::simple_fast(graph, entry))
    }

    /// Find natural loops in the CFG
    pub fn find_natural_loops(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
    ) -> Vec<(NodeIndex, NodeIndex)> {
        let mut loops = Vec::new();

        if let Some(dominators) = self.analyze_dominators(graph) {
            // Find back-edges (edges where target dominates source)
            for edge in graph.edge_indices() {
                if let Some((source, target)) = graph.edge_endpoints(edge) {
                    // Check if target dominates source (back-edge)
                    // A back-edge is where the target dominates the source
                    let mut current = source;
                    while let Some(dominator) = dominators.immediate_dominator(current) {
                        if dominator == target {
                            loops.push((target, source)); // (header, tail)
                            break;
                        }
                        current = dominator;
                    }
                }
            }
        }

        loops
    }

    /// Find if/else join blocks
    pub fn find_if_else_joins(&self, graph: &DiGraph<Block, EdgeKind>) -> Vec<NodeIndex> {
        let mut join_blocks = Vec::new();

        if let Some(_dominators) = self.analyze_dominators(graph) {
            // Look for blocks that have multiple predecessors and are post-dominated
            // by blocks that have conditional branches
            for node in graph.node_indices() {
                let predecessors: Vec<NodeIndex> = graph
                    .neighbors_directed(node, petgraph::Direction::Incoming)
                    .collect();

                if predecessors.len() >= 2 {
                    // Check if this looks like a join block
                    // (simplified heuristic: multiple predecessors)
                    join_blocks.push(node);
                }
            }
        }

        join_blocks
    }

    /// Find switch dispatch patterns
    pub fn find_switch_dispatches(&self, graph: &DiGraph<Block, EdgeKind>) -> Vec<Vec<NodeIndex>> {
        let mut switch_patterns = Vec::new();

        // Look for blocks that have multiple successors with similar structure
        for node in graph.node_indices() {
            let successors: Vec<NodeIndex> = graph
                .neighbors_directed(node, petgraph::Direction::Outgoing)
                .collect();

            if successors.len() > 2 {
                // This could be a switch dispatch
                // (simplified heuristic: multiple successors)
                switch_patterns.push(successors);
            }
        }

        switch_patterns
    }

    /// Get dominator tree as a string representation
    pub fn dominator_tree_string(&self, graph: &DiGraph<Block, EdgeKind>) -> String {
        if let Some(dominators) = self.analyze_dominators(graph) {
            let mut result = String::new();
            result.push_str("Dominator Tree:\n");

            for node in graph.node_indices() {
                if let Some(immediate_dominator) = dominators.immediate_dominator(node) {
                    result.push_str(&format!(
                        "  Block {} -> Block {}\n",
                        node.index(),
                        immediate_dominator.index()
                    ));
                } else {
                    result.push_str(&format!("  Block {} -> (root)\n", node.index()));
                }
            }

            result
        } else {
            "No dominator analysis available (empty graph)".to_string()
        }
    }

    /// Export CFG to DOT format for visualization
    pub fn to_dot(&self, graph: &DiGraph<Block, EdgeKind>) -> String {
        use petgraph::dot::{Config, Dot};
        format!("{:?}", Dot::with_config(graph, &[Config::EdgeNoLabel]))
    }

    /// Get the block containing the given PC, if any
    pub fn get_block_at_pc(&self, pc: u32) -> Option<NodeIndex> {
        self.pc_to_block.get(&pc).copied()
    }

    /// Check whether the given PC is within any block
    pub fn is_pc_in_block(&self, pc: u32) -> bool {
        self.pc_to_block.contains_key(&pc)
    }
}
