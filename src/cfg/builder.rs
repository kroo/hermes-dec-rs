//! CFG builder module
//!
//! This module contains the CfgBuilder struct and related functionality.

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::tables::jump_table::JumpTable;
use crate::hbc::HbcFile;
use petgraph::algo::dominators::{self, Dominators};
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::{HashMap, HashSet};

/// CFG builder and analyzer
pub struct CfgBuilder<'a> {
    /// Reference to the HBC file
    hbc_file: &'a HbcFile<'a>,
    /// Function this builder is associated with
    function_index: u32,
    /// Mapping from block start PC to node index
    block_starts: HashMap<u32, NodeIndex>,
    /// Mapping from every PC value within a block's range to the block node
    pc_to_block: HashMap<u32, NodeIndex>,
    /// Exit node for the current function (if any)
    exit_node: Option<NodeIndex>,
}

impl<'a> CfgBuilder<'a> {
    /// Create a new CFG builder for the given HBC file and function index
    pub fn new(hbc_file: &'a HbcFile, function_index: u32) -> Self {
        CfgBuilder {
            hbc_file,
            function_index,
            block_starts: HashMap::new(),
            pc_to_block: HashMap::new(),
            exit_node: None,
        }
    }

    /// Build CFG for the function (loads instructions from the HBC file)
    pub fn build(&mut self) -> DiGraph<Block, EdgeKind> {
        self.block_starts.clear();
        self.pc_to_block.clear();

        let instructions = match self
            .hbc_file
            .functions
            .get_instructions(self.function_index)
        {
            Ok(instrs) => instrs,
            Err(_) => return DiGraph::new(),
        };

        let mut graph = DiGraph::new();

        // Reset state for new function
        self.block_starts.clear();
        self.exit_node = None;

        if instructions.is_empty() {
            return graph;
        }

        // Use the jump table from the HBC file
        let jump_table = &self.hbc_file.jump_table;

        // Step 2: Find leaders (basic block entry points)
        let leaders = self.find_leaders(&instructions, jump_table);

        // Step 3: Create basic blocks
        let blocks = self.create_blocks(&instructions, &leaders);

        // Step 4: Add blocks to graph and populate PC mapping
        for block in blocks {
            self.add_block(&mut graph, block);
        }

        // Step 5: Add synthetic EXIT node
        let exit_block = Block::new_exit();
        let exit_node = graph.add_node(exit_block);
        self.exit_node = Some(exit_node);

        // Step 6: Add edges between blocks and connect terminators to EXIT
        self.add_edges(&mut graph, jump_table);

        // Add exception handler edges
        self.add_exception_handler_edges(&mut graph);

        graph
    }

    /// Find basic block leaders
    fn find_leaders(
        &self,
        instructions: &[HbcFunctionInstruction],
        jump_table: &JumpTable,
    ) -> HashSet<u32> {
        let mut leaders = HashSet::new();

        if !instructions.is_empty() {
            leaders.insert(0);
        }

        for (i, instruction) in instructions.iter().enumerate() {
            let pc = i as u32;
            if instruction.instruction.category() == "Jump" {
                if let Some(target) = self.get_jump_target(instruction, jump_table) {
                    leaders.insert(target);
                    let post_branch = pc + 1;
                    if post_branch < instructions.len() as u32 && post_branch != target {
                        leaders.insert(post_branch);
                    }
                }
            } else if instruction.instruction.category() == "Return" {
                // Return instructions should NOT be leaders - they belong to the preceding block
                // However, if there are instructions after this Return, they should be unreachable
                // and form a separate basic block starting at pc+1
                let post_terminator = pc + 1;
                if post_terminator < instructions.len() as u32 {
                    leaders.insert(post_terminator);
                }
            } else if instruction.instruction.category() == "Exception" {
                // Exception instructions like Throw are terminating, but Catch is not
                // Only treat actual terminating exception instructions as terminators
                if matches!(
                    instruction.instruction.name(),
                    "Throw"
                        | "ThrowIfEmpty"
                        | "ThrowIfHasRestrictedGlobalProperty"
                        | "ThrowIfUndefinedInst"
                ) {
                    let post_terminator = pc + 1;
                    if post_terminator < instructions.len() as u32 {
                        leaders.insert(post_terminator);
                    }
                }
            }
        }

        // Add exception handler targets as leaders (now included in jump table labels)
        if let Some(labels) = jump_table.get_labels_for_function(self.function_index) {
            for label in labels {
                leaders.insert(label.instruction_index);
            }
        }

        leaders
    }

    /// Get jump target from instruction using jump table
    fn get_jump_target(
        &self,
        instruction: &HbcFunctionInstruction,
        jump_table: &JumpTable,
    ) -> Option<u32> {
        if let Some(jump_label) = jump_table
            .get_label_by_jump_op_index(instruction.function_index, instruction.instruction_index)
        {
            if let Some(labels) = jump_table.get_labels_for_function(instruction.function_index) {
                for label in labels {
                    if &label.name == jump_label {
                        return Some(label.instruction_index);
                    }
                }
            }
        }
        None
    }

    /// Add exception handler edges to the CFG
    fn add_exception_handler_edges(&self, graph: &mut DiGraph<Block, EdgeKind>) {
        if let Some(parsed_header) = self
            .hbc_file
            .functions
            .get_parsed_header(self.function_index)
        {
            for handler in &parsed_header.exc_handlers {
                // Convert byte offsets to instruction indices using jump table
                if let Some(try_start_idx) = self
                    .hbc_file
                    .jump_table
                    .byte_offset_to_instruction_index(self.function_index, handler.start)
                {
                    if let Some(try_end_idx) = self
                        .hbc_file
                        .jump_table
                        .byte_offset_to_instruction_index(self.function_index, handler.end)
                    {
                        if let Some(catch_target_idx) = self
                            .hbc_file
                            .jump_table
                            .byte_offset_to_instruction_index(self.function_index, handler.target)
                        {
                            // Find the catch block
                            if let Some(&catch_node) = self.block_starts.get(&catch_target_idx) {
                                // Find all blocks that are within the try range
                                // Sort by PC to ensure deterministic order
                                let mut blocks_in_range: Vec<_> = self
                                    .block_starts
                                    .iter()
                                    .filter(|(pc, _)| **pc >= try_start_idx && **pc < try_end_idx)
                                    .collect();
                                blocks_in_range.sort_by_key(|(pc, _)| **pc);

                                for (_, &block_node) in blocks_in_range {
                                    // This block is within the try range, add exception edge to catch block
                                    graph.add_edge(block_node, catch_node, EdgeKind::Uncond);
                                }
                            }
                        }
                    }
                }
            }
        }
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
    pub fn add_block(&mut self, graph: &mut DiGraph<Block, EdgeKind>, block: Block) -> NodeIndex {
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
    fn add_edges(&mut self, graph: &mut DiGraph<Block, EdgeKind>, jump_table: &JumpTable) {
        let mut block_ends: Vec<(u32, NodeIndex, Vec<HbcFunctionInstruction>)> = Vec::new();

        // Find all block end points and collect instructions (excluding EXIT block)
        for node_index in graph.node_indices() {
            let block = &graph[node_index];
            if block.is_exit() {
                continue; // Skip EXIT block
            }
            let end_pc = block.end_pc();
            let instructions = block.instructions().to_vec();
            block_ends.push((end_pc, node_index, instructions));
        }

        // Add edges based on control flow
        for (end_pc, from_node, instructions) in block_ends {
            if instructions.is_empty() {
                continue;
            }

            // Check if block ends with a terminating instruction (Return/Throw)
            if let Some(last_instruction) = instructions.last() {
                if matches!(
                    last_instruction.instruction.category(),
                    "Return" | "Exception"
                ) {
                    // Connect terminating blocks to EXIT node
                    if let Some(exit_node) = self.exit_node {
                        graph.add_edge(from_node, exit_node, EdgeKind::Uncond);
                    }
                } else if last_instruction.instruction.category() == "Jump" {
                    // Add jump edge
                    if let Some(target) = self.get_jump_target(last_instruction, jump_table) {
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
        let mut dot = String::new();
        dot.push_str("digraph {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n\n");

        // Add nodes
        for node in graph.node_indices() {
            let block = &graph[node];
            let label = if block.is_exit() {
                "EXIT".to_string()
            } else {
                format!(
                    "Block {} (PC {}-{})",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                )
            };

            dot.push_str(&format!("  {} [ label = \"{}\" ]\n", node.index(), label));
        }

        dot.push_str("\n");

        // Add edges
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            dot.push_str(&format!("  {} -> {}\n", tail.index(), head.index()));
        }

        dot.push_str("}\n");
        dot
    }

    /// Export CFG to DOT format with disassembled instructions
    pub fn to_dot_with_disassembly(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        hbc_file: &HbcFile,
    ) -> String {
        let mut dot = String::new();
        dot.push_str("digraph {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n\n");

        // Add nodes with disassembled instructions
        for node in graph.node_indices() {
            let block = &graph[node];
            let label = if block.is_exit() {
                "EXIT".to_string()
            } else {
                let mut block_label = format!(
                    "Block {} (PC {}-{})",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                );

                // Add jump label to block title if this block is a jump target
                if let Some(jump_label) = self.get_block_jump_label(node, graph, hbc_file) {
                    block_label.push_str(&format!(" [{}]", jump_label));
                }

                block_label.push_str("\\l");

                for (i, instruction) in block.instructions().iter().enumerate() {
                    let disassembled = instruction.format_instruction(hbc_file);
                    // Escape quotes and newlines for DOT format
                    let escaped = disassembled.replace("\"", "\\\"").replace("\n", "\\l");
                    block_label.push_str(&format!("  {}: {}\\l", i, escaped));
                }

                block_label
            };

            dot.push_str(&format!("  {} [ label = \"{}\" ]\n", node.index(), label));
        }

        dot.push_str("\n");

        // Add edges (no labels)
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            dot.push_str(&format!("  {} -> {}\n", tail.index(), head.index()));
        }

        dot.push_str("}\n");
        dot
    }

    /// Export CFG to DOT format as a subgraph for a specific function
    pub fn to_dot_subgraph(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        hbc_file: &HbcFile,
        function_index: u32,
    ) -> String {
        let mut dot = String::new();
        dot.push_str(&format!(
            "  subgraph cluster_function_{} {{\n",
            function_index
        ));
        dot.push_str(&format!("    label = \"Function {}\";\n", function_index));
        dot.push_str("    style = filled;\n");
        dot.push_str("    color = lightgrey;\n\n");

        // Add nodes with disassembled instructions
        for node in graph.node_indices() {
            let block = &graph[node];
            let node_id = format!("f{}_n{}", function_index, node.index());
            let label = if block.is_exit() {
                "EXIT".to_string()
            } else {
                let mut block_label = format!(
                    "Block {} (PC {}-{})",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                );

                // Add jump label to block title if this block is a jump target
                if let Some(jump_label) = self.get_block_jump_label(node, graph, hbc_file) {
                    block_label.push_str(&format!(" [{}]", jump_label));
                }

                block_label.push_str("\\l");

                for (i, instruction) in block.instructions().iter().enumerate() {
                    let disassembled = instruction.format_instruction(hbc_file);
                    // Escape quotes and newlines for DOT format
                    let escaped = disassembled.replace("\"", "\\\"").replace("\n", "\\l");
                    block_label.push_str(&format!("  {}: {}\\l", i, escaped));
                }

                block_label
            };

            dot.push_str(&format!("    {} [ label = \"{}\" ]\n", node_id, label));
        }

        dot.push_str("\n");

        // Add edges within the subgraph (no labels)
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let tail_id = format!("f{}_n{}", function_index, tail.index());
            let head_id = format!("f{}_n{}", function_index, head.index());
            dot.push_str(&format!("    {} -> {}\n", tail_id, head_id));
        }

        dot.push_str("  }\n");
        dot
    }

    /// Get the block containing the given PC, if any
    pub fn get_block_at_pc(&self, pc: u32) -> Option<NodeIndex> {
        self.pc_to_block.get(&pc).copied()
    }

    /// Check whether the given PC is within any block
    pub fn is_pc_in_block(&self, pc: u32) -> bool {
        self.pc_to_block.contains_key(&pc)
    }

    /// Get the EXIT node for the current function
    pub fn exit_node(&self) -> Option<NodeIndex> {
        self.exit_node
    }

    /// Get jump label for a block if it's a jump target
    fn get_block_jump_label(
        &self,
        node: NodeIndex,
        graph: &DiGraph<Block, EdgeKind>,
        hbc_file: &HbcFile,
    ) -> Option<String> {
        let block = &graph[node];

        // Only check blocks that aren't exit blocks
        if block.is_exit() {
            return None;
        }

        // Check if this block's start PC is a jump target
        let start_pc = block.start_pc();

        // The jump table now includes both jump targets and exception handler targets
        hbc_file
            .jump_table
            .get_label_by_inst_index(self.function_index, start_pc)
            .cloned()
    }
}
