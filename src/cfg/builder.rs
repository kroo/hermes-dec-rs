//! CFG builder module
//!
//! This module contains the CfgBuilder struct and related functionality.

use crate::cfg::{Block, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::tables::jump_table::JumpTable;
use crate::hbc::tables::switch_table::SwitchTable;
use crate::hbc::{HbcFile, InstructionIndex};
use petgraph::algo::dominators::{self, Dominators};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet};

/// CFG builder and analyzer
pub struct CfgBuilder<'a> {
    /// Reference to the HBC file
    hbc_file: &'a HbcFile<'a>,
    /// Function this builder is associated with
    function_index: u32,
    /// Mapping from block start PC to node index
    block_starts: HashMap<InstructionIndex, NodeIndex>,
    /// Mapping from every PC value within a block's range to the block node
    pc_to_block: HashMap<InstructionIndex, NodeIndex>,
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

    /// Get the HBC file reference
    pub fn hbc_file(&self) -> &'a HbcFile<'a> {
        self.hbc_file
    }

    /// Get the function index
    pub fn function_index(&self) -> u32 {
        self.function_index
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
        log::debug!(
            "Adding exception handler edges for function {}...",
            self.function_index
        );
        self.add_exception_handler_edges(&mut graph);
        log::debug!("Exception handler edges added, CFG build complete for function {} - {} nodes, {} edges", 
                   self.function_index, graph.node_count(), graph.edge_count());

        graph
    }

    /// Find basic block leaders
    pub fn find_leaders(
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
                // Check if this is a switch instruction
                if matches!(instruction.instruction.name(), "SwitchImm") {
                    // Handle switch instructions - add all case targets as leaders
                    if let Some(switch_table) = self.get_switch_table_for_instruction(instruction) {
                        // Add default case target
                        if let Some(default_target) = switch_table.default_instruction_index {
                            leaders.insert(default_target);
                        }

                        // Add all case targets
                        for case in &switch_table.cases {
                            if let Some(target) = case.target_instruction_index {
                                leaders.insert(target);
                            }
                        }

                        // Add fallthrough (next instruction after switch)
                        let post_switch = pc + 1;
                        if post_switch < instructions.len() as u32 {
                            leaders.insert(post_switch);
                        }
                    }
                } else {
                    // Handle regular jump instructions
                    if let Some(target) = self.get_jump_target(instruction, jump_table) {
                        leaders.insert(target);
                        let post_branch = pc + 1;
                        if post_branch < instructions.len() as u32 && post_branch != target {
                            leaders.insert(post_branch);
                        }
                    }
                }
            } else if instruction.instruction.category() == "Switch" {
                // Handle switch instructions - add all case targets as leaders
                if let Some(switch_table) = self.get_switch_table_for_instruction(instruction) {
                    // Add default case target
                    if let Some(default_target) = switch_table.default_instruction_index {
                        leaders.insert(default_target);
                    }

                    // Add all case targets
                    for case in &switch_table.cases {
                        if let Some(target) = case.target_instruction_index {
                            leaders.insert(target);
                        }
                    }

                    // Add fallthrough (next instruction after switch)
                    let post_switch = pc + 1;
                    if post_switch < instructions.len() as u32 {
                        leaders.insert(post_switch);
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
            } else if matches!(
                instruction.instruction.name(),
                "SaveGenerator" | "SaveGeneratorLong"
            ) {
                // SaveGenerator instructions have two control flow paths:
                // 1. Fallthrough to next instruction (initial execution)
                // 2. Resume at target label (when generator is resumed)
                if let Some(target) = self.get_jump_target(instruction, jump_table) {
                    leaders.insert(target); // Resume target becomes leader
                }
                // Next instruction becomes leader (fallthrough path)
                let post_generator = pc + 1;
                if post_generator < instructions.len() as u32 {
                    leaders.insert(post_generator);
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

    /// Get switch table for a switch instruction
    fn get_switch_table_for_instruction(
        &self,
        instruction: &HbcFunctionInstruction,
    ) -> Option<&SwitchTable> {
        self.hbc_file.switch_tables.get_switch_table_by_instruction(
            self.function_index,
            instruction.instruction_index.into(),
        )
    }

    /// Add exception handler edges to the CFG
    fn add_exception_handler_edges(&self, graph: &mut DiGraph<Block, EdgeKind>) {
        if let Some(parsed_header) = self
            .hbc_file
            .functions
            .get_parsed_header(self.function_index)
        {
            let total_handlers = parsed_header.exc_handlers.len();
            log::debug!(
                "Processing {} exception handlers for function {}",
                total_handlers,
                self.function_index
            );

            for (handler_idx, handler) in parsed_header.exc_handlers.iter().enumerate() {
                log::debug!(
                    "Processing exception handler {}/{} for function {}",
                    handler_idx + 1,
                    total_handlers,
                    self.function_index
                );
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
                            if let Some(&catch_node) = self
                                .block_starts
                                .get(&InstructionIndex::from(catch_target_idx))
                            {
                                // Find all blocks that are within the try range
                                // Sort by PC to ensure deterministic order
                                let mut blocks_in_range: Vec<_> = self
                                    .block_starts
                                    .iter()
                                    .filter(|(pc, _)| {
                                        **pc >= InstructionIndex::from(try_start_idx)
                                            && **pc < InstructionIndex::from(try_end_idx)
                                    })
                                    .collect();
                                blocks_in_range.sort_by_key(|(pc, _)| **pc);

                                for (_, &block_node) in blocks_in_range {
                                    // This block is within the try range, add exception edge to catch block
                                    // Only add if not already connected (to avoid duplicate edges)
                                    if !graph.edges_connecting(block_node, catch_node).any(|_| true) {
                                        log::debug!("Adding exception edge from block {} to catch block {}", 
                                                   block_node.index(), catch_node.index());
                                        graph.add_edge(block_node, catch_node, EdgeKind::Exception);
                                    }
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

            blocks.push(Block::new(start_pc.into(), block_instructions));
        }

        blocks
    }

    /// Add a block to the graph and update lookup tables
    pub fn add_block(&mut self, graph: &mut DiGraph<Block, EdgeKind>, block: Block) -> NodeIndex {
        let start_pc = block.start_pc;
        let end_pc = block.end_pc;
        let node_index = graph.add_node(block);
        self.block_starts.insert(start_pc, node_index);
        // Create instruction indices for each instruction in the block
        let start_idx: usize = start_pc.into();
        let end_idx: usize = end_pc.into();
        for idx in start_idx..end_idx {
            self.pc_to_block
                .insert(InstructionIndex::from(idx as u32), node_index);
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
            block_ends.push((end_pc.into(), node_index, instructions));
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
                    // Check if this is a switch instruction
                    if matches!(last_instruction.instruction.name(), "SwitchImm") {
                        // Handle switch instructions - add edges for all cases and default
                        if let Some(switch_table) =
                            self.get_switch_table_for_instruction(last_instruction)
                        {
                            // Add edge for default case
                            if let Some(default_target) = switch_table.default_instruction_index {
                                if let Some(&to_node) = self
                                    .block_starts
                                    .get(&InstructionIndex::from(default_target))
                                {
                                    graph.add_edge(from_node, to_node, EdgeKind::Default);
                                }
                            }

                            // Add edges for all switch cases
                            for (case_index, case) in switch_table.cases.iter().enumerate() {
                                if let Some(target) = case.target_instruction_index {
                                    if let Some(&to_node) =
                                        self.block_starts.get(&InstructionIndex::from(target))
                                    {
                                        graph.add_edge(
                                            from_node,
                                            to_node,
                                            EdgeKind::Switch(case_index),
                                        );
                                    }
                                }
                            }
                        }
                    } else {
                        // Handle regular jump instructions
                        if let Some(target) = self.get_jump_target(last_instruction, jump_table) {
                            if let Some(&to_node) =
                                self.block_starts.get(&InstructionIndex::from(target))
                            {
                                let edge_kind = self.get_edge_kind(last_instruction);
                                graph.add_edge(from_node, to_node, edge_kind);
                            }
                        }

                        // Add fallthrough edge for conditional jumps
                        if self.is_conditional_jump(last_instruction) {
                            let fallthrough_pc = end_pc;
                            if let Some(&to_node) = self
                                .block_starts
                                .get(&InstructionIndex::from(fallthrough_pc))
                            {
                                // For JmpFalse, the fallthrough is the True branch
                                // For all other conditional jumps, the fallthrough is the False branch
                                let fallthrough_edge_kind = match &last_instruction.instruction {
                                    UnifiedInstruction::JmpFalse { .. }
                                    | UnifiedInstruction::JmpFalseLong { .. } => EdgeKind::True,
                                    _ => EdgeKind::False,
                                };
                                graph.add_edge(from_node, to_node, fallthrough_edge_kind);
                            }
                        }
                    }
                } else if last_instruction.instruction.category() == "Switch" {
                    // Handle switch instructions - add edges for all cases and default
                    if let Some(switch_table) =
                        self.get_switch_table_for_instruction(last_instruction)
                    {
                        // Add edge for default case
                        if let Some(default_target) = switch_table.default_instruction_index {
                            if let Some(&to_node) = self
                                .block_starts
                                .get(&InstructionIndex::from(default_target))
                            {
                                graph.add_edge(from_node, to_node, EdgeKind::Default);
                            }
                        }

                        // Add edges for all switch cases
                        for (case_index, case) in switch_table.cases.iter().enumerate() {
                            if let Some(target) = case.target_instruction_index {
                                if let Some(&to_node) =
                                    self.block_starts.get(&InstructionIndex::from(target))
                                {
                                    graph.add_edge(
                                        from_node,
                                        to_node,
                                        EdgeKind::Switch(case_index),
                                    );
                                }
                            }
                        }
                    }
                } else if matches!(
                    last_instruction.instruction.name(),
                    "SaveGenerator" | "SaveGeneratorLong"
                ) {
                    // Handle SaveGenerator instructions - create both fallthrough and resume edges
                    // 1. Generator Resume edge: to the resumption label
                    if let Some(target) = self.get_jump_target(last_instruction, jump_table) {
                        if let Some(&to_node) =
                            self.block_starts.get(&InstructionIndex::from(target))
                        {
                            graph.add_edge(from_node, to_node, EdgeKind::GeneratorResume);
                        }
                    }
                    // 2. Generator Fallthrough edge: to the next instruction
                    let fallthrough_pc = end_pc;
                    if let Some(&to_node) = self
                        .block_starts
                        .get(&InstructionIndex::from(fallthrough_pc))
                    {
                        graph.add_edge(from_node, to_node, EdgeKind::GeneratorFallthrough);
                    }
                } else {
                    // Fallthrough to next block
                    let fallthrough_pc = end_pc;
                    if let Some(&to_node) = self
                        .block_starts
                        .get(&InstructionIndex::from(fallthrough_pc))
                    {
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
            UnifiedInstruction::JEqual { .. } | UnifiedInstruction::JEqualLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JNotEqual { .. } | UnifiedInstruction::JNotEqualLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JStrictEqual { .. }
            | UnifiedInstruction::JStrictEqualLong { .. } => EdgeKind::True,
            UnifiedInstruction::JStrictNotEqual { .. }
            | UnifiedInstruction::JStrictNotEqualLong { .. } => EdgeKind::True,
            UnifiedInstruction::JLess { .. } | UnifiedInstruction::JLessLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JGreater { .. } | UnifiedInstruction::JGreaterLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JLessEqual { .. } | UnifiedInstruction::JLessEqualLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JGreaterEqual { .. }
            | UnifiedInstruction::JGreaterEqualLong { .. } => EdgeKind::True,
            UnifiedInstruction::JNotLess { .. } | UnifiedInstruction::JNotLessLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JNotGreater { .. } | UnifiedInstruction::JNotGreaterLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JNotLessEqual { .. }
            | UnifiedInstruction::JNotLessEqualLong { .. } => EdgeKind::True,
            UnifiedInstruction::JNotGreaterEqual { .. }
            | UnifiedInstruction::JNotGreaterEqualLong { .. } => EdgeKind::True,
            UnifiedInstruction::JLessN { .. } | UnifiedInstruction::JLessNLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JGreaterN { .. } | UnifiedInstruction::JGreaterNLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JLessEqualN { .. } | UnifiedInstruction::JLessEqualNLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JGreaterEqualN { .. }
            | UnifiedInstruction::JGreaterEqualNLong { .. } => EdgeKind::True,
            UnifiedInstruction::JNotLessN { .. } | UnifiedInstruction::JNotLessNLong { .. } => {
                EdgeKind::True
            }
            UnifiedInstruction::JNotGreaterN { .. }
            | UnifiedInstruction::JNotGreaterNLong { .. } => EdgeKind::True,
            UnifiedInstruction::JNotLessEqualN { .. }
            | UnifiedInstruction::JNotLessEqualNLong { .. } => EdgeKind::True,
            UnifiedInstruction::JNotGreaterEqualN { .. }
            | UnifiedInstruction::JNotGreaterEqualNLong { .. } => EdgeKind::True,
            UnifiedInstruction::JmpUndefined { .. }
            | UnifiedInstruction::JmpUndefinedLong { .. } => EdgeKind::True,
            UnifiedInstruction::SwitchImm { .. } => {
                // For switch instructions, we'll handle the edge creation separately
                // in the add_edges method to create multiple edges for each case
                EdgeKind::Default
            }
            _ => EdgeKind::Uncond, // Default for other instructions
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
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
        dot.push_str("  edge [fontname=\"Arial\"];\n\n");

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

        // Add edges with labels
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();

            let label = match edge_kind {
                EdgeKind::Uncond => to_title_case("uncond"),
                EdgeKind::True => to_title_case("true"),
                EdgeKind::False => to_title_case("false"),
                EdgeKind::Switch(case_index) => {
                    dot.push_str(&format!(
                        "  {} -> {} [label=\"Switch({})\"]\n",
                        tail.index(),
                        head.index(),
                        case_index
                    ));
                    continue;
                }
                EdgeKind::Default => to_title_case("default"),
                EdgeKind::Fall => to_title_case("fall"),
                EdgeKind::GeneratorFallthrough => "Generator Fallthrough".to_string(),
                EdgeKind::GeneratorResume => "Generator Resume".to_string(),
                EdgeKind::Exception => "Exception".to_string(),
            };

            dot.push_str(&format!(
                "  {} -> {} [label=\"{}\"]\n",
                tail.index(),
                head.index(),
                label
            ));
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
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
        dot.push_str("  edge [fontname=\"Arial\"];\n\n");

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

        // Add edges with labels
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();

            let label = match edge_kind {
                EdgeKind::Uncond => to_title_case("uncond"),
                EdgeKind::True => to_title_case("true"),
                EdgeKind::False => to_title_case("false"),
                EdgeKind::Switch(case_index) => {
                    dot.push_str(&format!(
                        "  {} -> {} [label=\"Switch({})\"]\n",
                        tail.index(),
                        head.index(),
                        case_index
                    ));
                    continue;
                }
                EdgeKind::Default => to_title_case("default"),
                EdgeKind::Fall => to_title_case("fall"),
                EdgeKind::GeneratorFallthrough => "Generator Fallthrough".to_string(),
                EdgeKind::GeneratorResume => "Generator Resume".to_string(),
                EdgeKind::Exception => "Exception".to_string(),
            };

            dot.push_str(&format!(
                "  {} -> {} [label=\"{}\"]\n",
                tail.index(),
                head.index(),
                label
            ));
        }

        dot.push_str("}\n");
        dot
    }

    /// Export CFG to DOT format with loop analysis visualization
    pub fn to_dot_with_loops(&self, graph: &DiGraph<Block, EdgeKind>) -> String {
        let mut dot = String::new();
        dot.push_str("digraph {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
        dot.push_str("  edge [fontname=\"Arial\"];\n\n");

        // Analyze loops
        let loop_analysis = self.analyze_loops(graph);

        // Add nodes with loop information
        for node in graph.node_indices() {
            let block = &graph[node];
            let mut node_attrs = Vec::new();

            // Base label
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
            node_attrs.push(format!("label=\"{}\"", label));

            // Add loop information
            if let Some(loop_indices) = loop_analysis.node_to_loops.get(&node) {
                if !loop_indices.is_empty() {
                    // Find the innermost loop (last in the list)
                    let innermost_loop_idx = loop_indices.last().unwrap();
                    let loop_info = &loop_analysis.loops[*innermost_loop_idx];

                    // Color based on loop type
                    let color = match loop_info.loop_type {
                        crate::cfg::analysis::LoopType::While => "lightblue",
                        crate::cfg::analysis::LoopType::For => "lightgreen",
                        crate::cfg::analysis::LoopType::DoWhile => "lightyellow",
                        crate::cfg::analysis::LoopType::ForIn => "lightgoldenrod",
                        crate::cfg::analysis::LoopType::ForOf => "lightpink",
                    };
                    node_attrs.push(format!("style=filled, fillcolor=\"{}\"", color));

                    // Add loop header indicator
                    if loop_info.is_header(node) {
                        node_attrs.push("penwidth=3".to_string());
                        node_attrs.push("color=red".to_string());
                    }

                    // Add loop body indicator
                    if loop_info.body_nodes.contains(&node) {
                        node_attrs.push("shape=box".to_string());
                    }
                }
            }

            // Add exit node styling
            if block.is_exit() {
                node_attrs.push("style=filled, fillcolor=lightgray".to_string());
            }

            dot.push_str(&format!("  {} [{}]\n", node.index(), node_attrs.join(", ")));
        }

        dot.push_str("\n");

        // Add edges with loop information
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();
            let mut edge_attrs = Vec::new();

            // Edge label
            let label = match edge_kind {
                EdgeKind::Uncond => to_title_case("uncond"),
                EdgeKind::True => to_title_case("true"),
                EdgeKind::False => to_title_case("false"),
                EdgeKind::Switch(case_index) => format!("Switch({})", case_index),
                EdgeKind::Default => to_title_case("default"),
                EdgeKind::Fall => to_title_case("fall"),
                EdgeKind::GeneratorFallthrough => "Generator Fallthrough".to_string(),
                EdgeKind::GeneratorResume => "Generator Resume".to_string(),
                EdgeKind::Exception => "Exception".to_string(),
            };
            edge_attrs.push(format!("label=\"{}\"", label));

            // Check if this is a back-edge
            let mut is_back_edge = false;
            for loop_info in &loop_analysis.loops {
                for (back_tail, back_head) in &loop_info.back_edges {
                    if *back_tail == tail && *back_head == head {
                        is_back_edge = true;
                        edge_attrs.push("color=red".to_string());
                        edge_attrs.push("penwidth=2".to_string());
                        edge_attrs.push("style=dashed".to_string());
                        break;
                    }
                }
                if is_back_edge {
                    break;
                }
            }

            // Check if this is a loop exit edge
            for loop_info in &loop_analysis.loops {
                if loop_info.body_nodes.contains(&tail) && !loop_info.body_nodes.contains(&head) {
                    edge_attrs.push("color=green".to_string());
                    edge_attrs.push("penwidth=2".to_string());
                    break;
                }
            }

            dot.push_str(&format!(
                "  {} -> {} [{}]\n",
                tail.index(),
                head.index(),
                edge_attrs.join(", ")
            ));
        }

        // Add loop information as subgraphs
        for (i, loop_info) in loop_analysis.loops.iter().enumerate() {
            dot.push_str(&format!("\n  subgraph cluster_loop_{} {{\n", i));
            dot.push_str(&format!(
                "    label=\"Loop {}: {:?}\";\n",
                i, loop_info.loop_type
            ));
            dot.push_str("    style=dashed;\n");
            dot.push_str("    color=blue;\n");

            for &node in &loop_info.body_nodes {
                dot.push_str(&format!("    {};\n", node.index()));
            }

            dot.push_str("  }\n");
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
        dot.push_str("    edge [fontname=\"Arial\"];\n\n");

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

        // Add edges within the subgraph with labels
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();
            let tail_id = format!("f{}_n{}", function_index, tail.index());
            let head_id = format!("f{}_n{}", function_index, head.index());

            let label = match edge_kind {
                EdgeKind::Uncond => to_title_case("uncond"),
                EdgeKind::True => to_title_case("true"),
                EdgeKind::False => to_title_case("false"),
                EdgeKind::Switch(case_index) => {
                    dot.push_str(&format!(
                        "    {} -> {} [label=\"Switch({})\"]\n",
                        tail_id, head_id, case_index
                    ));
                    continue;
                }
                EdgeKind::Default => to_title_case("default"),
                EdgeKind::Fall => to_title_case("fall"),
                EdgeKind::GeneratorFallthrough => "Generator Fallthrough".to_string(),
                EdgeKind::GeneratorResume => "Generator Resume".to_string(),
                EdgeKind::Exception => "Exception".to_string(),
            };

            dot.push_str(&format!(
                "    {} -> {} [label=\"{}\"]\n",
                tail_id, head_id, label
            ));
        }

        dot.push_str("  }\n");
        dot
    }

    /// Export CFG to DOT format with comprehensive analysis visualization
    pub fn to_dot_with_analysis(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        _hbc_file: &HbcFile,
    ) -> String {
        let mut dot = String::new();
        dot.push_str("digraph {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
        dot.push_str("  edge [fontname=\"Arial\"];\n\n");

        // Perform all analyses
        let loop_analysis = self.analyze_loops(graph);
        let conditional_analysis = self.analyze_conditional_chains(graph);
        let switch_analysis = self.analyze_switch_regions(graph);

        // Add nodes with comprehensive analysis information
        for node in graph.node_indices() {
            let block = &graph[node];
            let mut node_attrs = Vec::new();

            // Base label
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
            node_attrs.push(format!("label=\"{}\"", label));

            // Add loop information (highest priority for coloring)
            if let Some(loop_indices) = loop_analysis.node_to_loops.get(&node) {
                if !loop_indices.is_empty() {
                    let innermost_loop_idx = loop_indices.last().unwrap();
                    let loop_info = &loop_analysis.loops[*innermost_loop_idx];

                    let color = match loop_info.loop_type {
                        crate::cfg::analysis::LoopType::While => "lightblue",
                        crate::cfg::analysis::LoopType::For => "lightgreen",
                        crate::cfg::analysis::LoopType::DoWhile => "lightyellow",
                        crate::cfg::analysis::LoopType::ForIn => "lightgoldenrod",
                        crate::cfg::analysis::LoopType::ForOf => "lightpink",
                    };
                    node_attrs.push(format!("style=filled, fillcolor=\"{}\"", color));

                    if loop_info.is_header(node) {
                        node_attrs.push("penwidth=3, color=red".to_string());
                    }
                }
            } else {
                // Add conditional chain information (if not in a loop)
                if let Some(chain_indices) = conditional_analysis.node_to_chains.get(&node) {
                    if !chain_indices.is_empty() {
                        let chain_idx = chain_indices.last().unwrap();
                        let chain = &conditional_analysis.chains[*chain_idx];

                        let color = match chain.chain_type {
                            crate::cfg::analysis::ChainType::SimpleIfElse => "lightpink",
                            crate::cfg::analysis::ChainType::ElseIfChain => "lightcoral",
                            crate::cfg::analysis::ChainType::NestedConditional => "lightsteelblue",
                            crate::cfg::analysis::ChainType::GuardClausePattern => {
                                "lightgoldenrodyellow"
                            }
                            crate::cfg::analysis::ChainType::SwitchLikeChain => "lightcyan",
                        };
                        node_attrs.push(format!("style=filled, fillcolor=\"{}\"", color));
                    }
                } else {
                    // Add switch region information (if not in loop or conditional)
                    if let Some(region_indices) = switch_analysis.node_to_regions.get(&node) {
                        if !region_indices.is_empty() {
                            node_attrs.push("style=filled, fillcolor=lightseagreen".to_string());
                        }
                    }
                }
            }

            // Add exit node styling
            if block.is_exit() {
                node_attrs.push("style=filled, fillcolor=lightgray".to_string());
            }

            dot.push_str(&format!("  {} [{}]\n", node.index(), node_attrs.join(", ")));
        }

        dot.push_str("\n");

        // Add edges with comprehensive information
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();
            let mut edge_attrs = Vec::new();

            // Edge label
            let label = match edge_kind {
                EdgeKind::Uncond => to_title_case("uncond"),
                EdgeKind::True => to_title_case("true"),
                EdgeKind::False => to_title_case("false"),
                EdgeKind::Switch(case_index) => format!("Switch({})", case_index),
                EdgeKind::Default => to_title_case("default"),
                EdgeKind::Fall => to_title_case("fall"),
                EdgeKind::GeneratorFallthrough => "Generator Fallthrough".to_string(),
                EdgeKind::GeneratorResume => "Generator Resume".to_string(),
                EdgeKind::Exception => "Exception".to_string(),
            };
            edge_attrs.push(format!("label=\"{}\"", label));

            // Check for back-edges (loops)
            let mut is_back_edge = false;
            for loop_info in &loop_analysis.loops {
                for (back_tail, back_head) in &loop_info.back_edges {
                    if *back_tail == tail && *back_head == head {
                        is_back_edge = true;
                        edge_attrs.push("color=red, penwidth=2, style=dashed".to_string());
                        break;
                    }
                }
                if is_back_edge {
                    break;
                }
            }

            // Check for conditional chain edges
            if !is_back_edge {
                if let (Some(tail_chains), Some(head_chains)) = (
                    conditional_analysis.node_to_chains.get(&tail),
                    conditional_analysis.node_to_chains.get(&head),
                ) {
                    if !tail_chains.is_empty() && !head_chains.is_empty() {
                        edge_attrs.push("color=purple, penwidth=2".to_string());
                    }
                }
            }

            // Check for switch region edges
            if !is_back_edge {
                if let (Some(tail_regions), Some(head_regions)) = (
                    switch_analysis.node_to_regions.get(&tail),
                    switch_analysis.node_to_regions.get(&head),
                ) {
                    if !tail_regions.is_empty() && !head_regions.is_empty() {
                        edge_attrs.push("color=orange, penwidth=2".to_string());
                    }
                }
            }

            dot.push_str(&format!(
                "  {} -> {} [{}]\n",
                tail.index(),
                head.index(),
                edge_attrs.join(", ")
            ));
        }

        dot.push_str("}\n");
        dot
    }

    /// Export CFG to DOT format as a subgraph with comprehensive analysis
    pub fn to_dot_subgraph_with_analysis(
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
        dot.push_str("    edge [fontname=\"Arial\"];\n\n");

        // Perform all analyses
        let loop_analysis = self.analyze_loops(graph);
        let conditional_analysis = self.analyze_conditional_chains(graph);
        let switch_analysis = self.analyze_switch_regions(graph);

        // Track analysis types for legend
        let mut analysis_types = std::collections::HashSet::new();

        // Add nodes with comprehensive analysis information
        for node in graph.node_indices() {
            let block = &graph[node];
            let node_id = format!("f{}_n{}", function_index, node.index());
            let mut node_attrs = Vec::new();

            // Base label
            let mut block_label = if block.is_exit() {
                "EXIT".to_string()
            } else {
                let mut label = format!(
                    "Block {} (PC {}-{})",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                );

                // Add jump label to block title if this block is a jump target
                if let Some(jump_label) = self.get_block_jump_label(node, graph, hbc_file) {
                    label.push_str(&format!(" [{}]", jump_label));
                }

                label.push_str("\\l");

                for (i, instruction) in block.instructions().iter().enumerate() {
                    let disassembled = instruction.format_instruction(hbc_file);
                    let escaped = disassembled.replace("\"", "\\\"").replace("\n", "\\l");
                    label.push_str(&format!("  {}: {}\\l", i, escaped));
                }

                label
            };

            // Add analysis labels and colors
            if let Some(loop_indices) = loop_analysis.node_to_loops.get(&node) {
                if !loop_indices.is_empty() {
                    let innermost_loop_idx = loop_indices.last().unwrap();
                    let loop_info = &loop_analysis.loops[*innermost_loop_idx];

                    let (color, loop_type_name) = match loop_info.loop_type {
                        crate::cfg::analysis::LoopType::While => ("lightblue", "While Loop"),
                        crate::cfg::analysis::LoopType::For => ("lightgreen", "For Loop"),
                        crate::cfg::analysis::LoopType::DoWhile => ("lightyellow", "Do-While Loop"),
                        crate::cfg::analysis::LoopType::ForIn => ("lightgoldenrod", "For-In Loop"),
                        crate::cfg::analysis::LoopType::ForOf => ("lightpink", "For-Of Loop"),
                    };

                    analysis_types.insert(format!("Loop: {}", loop_type_name));

                    if loop_info.is_header(node) {
                        block_label =
                            format!("{}[LOOP HEADER: {}]\\l", block_label, loop_type_name);
                        node_attrs.push(format!(
                            "style=filled, fillcolor=\"{}\", penwidth=3, color=red",
                            color
                        ));
                    } else {
                        block_label = format!("{}[LOOP BODY: {}]\\l", block_label, loop_type_name);
                        node_attrs.push(format!("style=filled, fillcolor=\"{}\"", color));
                    }
                }
            } else {
                // Add conditional chain information (if not in a loop)
                if let Some(chain_indices) = conditional_analysis.node_to_chains.get(&node) {
                    if !chain_indices.is_empty() {
                        let chain_idx = chain_indices.last().unwrap();
                        let chain = &conditional_analysis.chains[*chain_idx];

                        let (color, chain_type_name, border_style, shape) = match chain.chain_type {
                            crate::cfg::analysis::ChainType::SimpleIfElse => {
                                ("lightpink", "IF-ELSE", "solid", "box")
                            }
                            crate::cfg::analysis::ChainType::ElseIfChain => {
                                ("lightcoral", "ELSE-IF CHAIN", "dashed", "diamond")
                            }
                            crate::cfg::analysis::ChainType::NestedConditional => {
                                ("lightsteelblue", "NESTED CONDITIONAL", "dotted", "ellipse")
                            }
                            crate::cfg::analysis::ChainType::GuardClausePattern => {
                                ("lightgoldenrodyellow", "GUARD CLAUSE", "double", "hexagon")
                            }
                            crate::cfg::analysis::ChainType::SwitchLikeChain => {
                                ("lightcyan", "SWITCH-LIKE", "bold", "octagon")
                            }
                        };

                        analysis_types.insert(format!("Conditional: {}", chain_type_name));
                        block_label =
                            format!("{}[CONDITIONAL: {}]\\l", block_label, chain_type_name);
                        node_attrs.push(format!(
                            "style=filled, fillcolor=\"{}\", shape={}, penwidth=2",
                            color, shape
                        ));

                        // Add border style based on conditional type
                        match border_style {
                            "solid" => node_attrs.push("color=black".to_string()),
                            "dashed" => node_attrs.push("color=red, style=dashed".to_string()),
                            "dotted" => node_attrs.push("color=blue, style=dotted".to_string()),
                            "double" => node_attrs.push("color=green, penwidth=3".to_string()),
                            "bold" => node_attrs.push("color=purple, penwidth=4".to_string()),
                            _ => node_attrs.push("color=black".to_string()),
                        }
                    }
                } else {
                    // Add switch region information (if not in loop or conditional)
                    if let Some(region_indices) = switch_analysis.node_to_regions.get(&node) {
                        if !region_indices.is_empty() {
                            analysis_types.insert("Switch Region".to_string());
                            block_label = format!("{}[SWITCH REGION]\\l", block_label);
                            node_attrs.push(
                                "style=filled, fillcolor=lightseagreen, shape=parallelogram"
                                    .to_string(),
                            );
                        }
                    }
                }
            }

            // Add exit node styling
            if block.is_exit() {
                node_attrs.push("style=filled, fillcolor=lightgray".to_string());
            }

            node_attrs.push(format!("label=\"{}\"", block_label));
            dot.push_str(&format!("    {} [{}]\n", node_id, node_attrs.join(", ")));
        }

        dot.push_str("\n");

        // Add edges with comprehensive information
        for edge in graph.edge_indices() {
            let (tail, head) = graph.edge_endpoints(edge).unwrap();
            let edge_kind = graph.edge_weight(edge).unwrap();
            let mut edge_attrs = Vec::new();

            let tail_id = format!("f{}_n{}", function_index, tail.index());
            let head_id = format!("f{}_n{}", function_index, head.index());

            // Edge label with descriptive text
            let (label, edge_style) = match edge_kind {
                EdgeKind::Uncond => ("Unconditional".to_string(), "black"),
                EdgeKind::True => ("True Branch".to_string(), "green"),
                EdgeKind::False => ("False Branch".to_string(), "red"),
                EdgeKind::Switch(case_index) => (format!("Switch Case {}", case_index), "blue"),
                EdgeKind::Default => ("Default Case".to_string(), "purple"),
                EdgeKind::Fall => ("Fall Through".to_string(), "orange"),
                EdgeKind::GeneratorFallthrough => {
                    ("Generator Fallthrough".to_string(), "lightblue")
                }
                EdgeKind::GeneratorResume => ("Generator Resume".to_string(), "cyan"),
                EdgeKind::Exception => ("Exception Handler".to_string(), "purple"),
            };
            edge_attrs.push(format!("label=\"{}\"", label));
            edge_attrs.push(format!("color={}", edge_style));

            // Special styling for back edges (loops)
            if let Some(loop_indices) = loop_analysis.node_to_loops.get(&tail) {
                if !loop_indices.is_empty() {
                    let innermost_loop_idx = loop_indices.last().unwrap();
                    let loop_info = &loop_analysis.loops[*innermost_loop_idx];
                    if loop_info.back_edges.contains(&(tail, head)) {
                        edge_attrs.push("color=red, penwidth=2, style=dashed".to_string());
                        edge_attrs.push("label=\"Back Edge\"".to_string());
                    }
                }
            }

            dot.push_str(&format!(
                "    {} -> {} [{}]\n",
                tail_id,
                head_id,
                edge_attrs.join(", ")
            ));
        }

        // Add legend if there are analysis types
        if !analysis_types.is_empty() {
            dot.push_str("\n    // Analysis Legend\n");
            for analysis_type in analysis_types {
                dot.push_str(&format!("    // {}\n", analysis_type));
            }
        }

        dot.push_str("  }\n");
        dot
    }

    /// Get the block containing the given PC, if any
    pub fn get_block_at_pc(&self, pc: u32) -> Option<NodeIndex> {
        self.pc_to_block.get(&InstructionIndex::from(pc)).copied()
    }

    /// Check whether the given PC is within any block
    pub fn is_pc_in_block(&self, pc: u32) -> bool {
        self.pc_to_block.contains_key(&InstructionIndex::from(pc))
    }

    /// Get the EXIT node for the current function
    pub fn exit_node(&self) -> Option<NodeIndex> {
        self.exit_node
    }

    /// Analyze loops in the CFG (including both reducible and irreducible loops)
    pub fn analyze_loops(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
    ) -> crate::cfg::analysis::LoopAnalysis {
        use crate::cfg::analysis::{Loop, LoopAnalysis};
        use std::collections::HashMap;

        let mut loops = Vec::new();
        let mut node_to_loops = HashMap::new();

        // Get dominators for loop detection
        if let Some(dominators) = self.analyze_dominators(graph) {
            // Find reducible loops (natural loops)
            let back_edges = self.find_back_edges(graph, &dominators);

            for (header, tail) in back_edges {
                let loop_body = self.compute_loop_body(graph, header, tail, &dominators);
                let loop_type = self.classify_loop_type(graph, header, tail, &loop_body);
                let exit_nodes = self.find_loop_exits(graph, header, &loop_body);

                let loop_info = Loop {
                    headers: vec![header],
                    body_nodes: loop_body,
                    back_edges: vec![(tail, header)],
                    loop_type,
                    exit_nodes,
                    is_irreducible: false,
                };

                loops.push(loop_info);
            }

            // Find irreducible loops
            let irreducible_loops = self.find_irreducible_loops(graph, &dominators);
            loops.extend(irreducible_loops);
        }

        // Build node-to-loops mapping
        for (loop_idx, loop_info) in loops.iter().enumerate() {
            for &node in &loop_info.body_nodes {
                node_to_loops
                    .entry(node)
                    .or_insert_with(Vec::new)
                    .push(loop_idx);
            }
        }

        LoopAnalysis {
            loops,
            node_to_loops,
        }
    }

    /// Analyze conditional chains in the CFG
    pub fn analyze_conditional_chains(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
    ) -> crate::cfg::analysis::ConditionalAnalysis {
        if let Some(post_doms) = self.analyze_post_dominators(graph) {
            crate::cfg::analysis::analyze_conditional_chains(graph, &post_doms)
        } else {
            crate::cfg::analysis::ConditionalAnalysis {
                chains: Vec::new(),
                node_to_chains: HashMap::new(),
                chain_statistics: crate::cfg::analysis::ChainStatistics {
                    total_chains: 0,
                    simple_if_else_count: 0,
                    else_if_chain_count: 0,
                    max_chain_length: 0,
                    max_nesting_depth: 0,
                },
            }
        }
    }

    /// Analyze switch regions in the CFG
    /// Note: This always returns an empty analysis because switch detection requires SSA analysis
    /// which is not available during CFG building. Use Cfg::analyze_switch_regions() instead.
    pub fn analyze_switch_regions(
        &self,
        _graph: &DiGraph<Block, EdgeKind>,
    ) -> crate::cfg::analysis::SwitchAnalysis {
        // Switch detection requires SSA analysis, so we return empty analysis here
        crate::cfg::analysis::SwitchAnalysis {
            regions: Vec::new(),
            node_to_regions: HashMap::new(),
        }
    }

    /// Find irreducible loops in the CFG
    fn find_irreducible_loops(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        _dominators: &Dominators<NodeIndex>,
    ) -> Vec<crate::cfg::analysis::Loop> {
        let mut irreducible_loops = Vec::new();

        // Find strongly connected components (SCCs) that contain multiple headers
        let sccs = petgraph::algo::tarjan_scc(graph);

        for scc in sccs {
            if scc.len() < 2 {
                continue; // Need at least 2 nodes for a loop
            }

            // Check if this SCC has multiple entry points (headers)
            let mut headers = Vec::new();
            for &node in &scc {
                let mut is_header = false;
                for edge in graph.edges_directed(node, petgraph::Direction::Incoming) {
                    let source = edge.source();
                    if !scc.contains(&source) {
                        is_header = true;
                        break;
                    }
                }
                if is_header {
                    headers.push(node);
                }
            }

            // If we have multiple headers, this is an irreducible loop
            if headers.len() > 1 {
                let mut loop_body = HashSet::new();
                for &node in &scc {
                    loop_body.insert(node);
                }

                // Find back-edges within this SCC
                let mut back_edges = Vec::new();
                for &node in &scc {
                    for edge in graph.edges_directed(node, petgraph::Direction::Outgoing) {
                        let target = edge.target();
                        if scc.contains(&target) {
                            back_edges.push((node, target));
                        }
                    }
                }

                // Find exit nodes
                let mut exit_nodes = Vec::new();
                for &node in &scc {
                    for edge in graph.edges_directed(node, petgraph::Direction::Outgoing) {
                        let target = edge.target();
                        if !scc.contains(&target) {
                            exit_nodes.push(target);
                        }
                    }
                }

                let loop_info = crate::cfg::analysis::Loop {
                    headers,
                    body_nodes: loop_body,
                    back_edges,
                    loop_type: crate::cfg::analysis::LoopType::While, // Default for irreducible
                    exit_nodes,
                    is_irreducible: true,
                };

                irreducible_loops.push(loop_info);
            }
        }

        irreducible_loops
    }

    /// Find all back-edges in the CFG
    fn find_back_edges(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        dominators: &Dominators<NodeIndex>,
    ) -> Vec<(NodeIndex, NodeIndex)> {
        let mut back_edges = Vec::new();

        for edge in graph.edge_indices() {
            if let Some((source, target)) = graph.edge_endpoints(edge) {
                // Check if target dominates source (back-edge condition)
                if self.dominates(dominators, target, source) {
                    back_edges.push((target, source)); // (header, tail)
                }
            }
        }

        back_edges
    }

    /// Check if a node dominates another node
    fn dominates(
        &self,
        dominators: &Dominators<NodeIndex>,
        dominator: NodeIndex,
        node: NodeIndex,
    ) -> bool {
        if dominator == node {
            return true;
        }
        let mut current = node;
        while let Some(immediate_dom) = dominators.immediate_dominator(current) {
            if immediate_dom == dominator {
                return true;
            }
            current = immediate_dom;
        }
        false
    }

    /// Compute the complete loop body for a back-edge
    fn compute_loop_body(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        header: NodeIndex,
        tail: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> HashSet<NodeIndex> {
        let mut loop_body = HashSet::new();
        let mut worklist = vec![tail];

        // Start from the tail and work backwards
        while let Some(node) = worklist.pop() {
            if loop_body.contains(&node) {
                continue;
            }

            loop_body.insert(node);

            // Add all predecessors that are dominated by the header
            for pred in graph.neighbors_directed(node, petgraph::Direction::Incoming) {
                if self.dominates(dominators, header, pred) && !loop_body.contains(&pred) {
                    worklist.push(pred);
                }
            }
        }

        // Include the header in the loop body
        loop_body.insert(header);

        loop_body
    }

    /// Classify the type of loop
    fn classify_loop_type(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        _header: NodeIndex,
        _tail: NodeIndex,
        loop_body: &HashSet<NodeIndex>,
    ) -> crate::cfg::analysis::LoopType {
        use crate::cfg::analysis::LoopType;

        // Look for distinctive iterator instructions to classify for-in/for-of loops
        let mut has_for_in = false;
        let mut has_for_of = false;
        for node in loop_body {
            let block = &graph[*node];
            for inst in &block.instructions {
                let name = inst.instruction.name();
                match name {
                    // Property name iteration used by for-in
                    "GetPNames" | "GetNextPName" => has_for_in = true,
                    // Iterator protocol used by for-of
                    "IteratorBegin" | "IteratorNext" => has_for_of = true,
                    _ => {}
                }
            }
        }

        // Helper to determine if a block ends in a conditional jump
        let is_conditional = |name: &str| name.starts_with('J') && name != "Jmp";

        let header_cond = graph[_header]
            .instructions
            .last()
            .map(|i| is_conditional(i.instruction.name()))
            .unwrap_or(false);
        let tail_cond = graph[_tail]
            .instructions
            .last()
            .map(|i| is_conditional(i.instruction.name()))
            .unwrap_or(false);

        if has_for_in {
            LoopType::ForIn
        } else if has_for_of {
            LoopType::ForOf
        } else if !header_cond && tail_cond {
            LoopType::DoWhile
        } else {
            // Default to While when we can't confidently detect other loop types.
            // The existing heuristic for distinguishing classic `for` loops from
            // simple `while` loops was misclassifying basic while loops that have
            // an unconditional back edge, so we keep those as `While` until more
            // reliable detection is implemented.
            LoopType::While
        }
    }

    /// Find loop exit nodes
    fn find_loop_exits(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
        _header: NodeIndex,
        loop_body: &HashSet<NodeIndex>,
    ) -> Vec<NodeIndex> {
        let mut exits = Vec::new();

        for &node in loop_body {
            for succ in graph.neighbors_directed(node, petgraph::Direction::Outgoing) {
                if !loop_body.contains(&succ) {
                    exits.push(succ);
                }
            }
        }

        exits
    }

    /// Analyze post-dominators for the CFG using reverse graph + simple_fast
    pub fn analyze_post_dominators(
        &self,
        graph: &DiGraph<Block, EdgeKind>,
    ) -> Option<crate::cfg::analysis::PostDominatorAnalysis> {
        if graph.node_count() == 0 {
            return None;
        }

        // Get the EXIT node as the entry point for post-dominator analysis
        let exit_node = self.exit_node?;

        // Create a reversed graph for post-dominator analysis
        let mut reversed_graph = DiGraph::new();
        let mut node_mapping = HashMap::new();

        // Add all nodes to reversed graph
        for node_idx in graph.node_indices() {
            let block = &graph[node_idx];
            let new_node = reversed_graph.add_node(block.clone());
            node_mapping.insert(node_idx, new_node);
        }

        // Add reversed edges
        for edge_idx in graph.edge_indices() {
            if let Some((source, target)) = graph.edge_endpoints(edge_idx) {
                let edge_kind = graph.edge_weight(edge_idx).unwrap();
                // Reverse the edge direction: target -> source in reversed graph
                if let (Some(&reversed_target), Some(&reversed_source)) =
                    (node_mapping.get(&target), node_mapping.get(&source))
                {
                    reversed_graph.add_edge(reversed_target, reversed_source, edge_kind.clone());
                }
            }
        }

        // Get the reversed exit node (entry point for post-dominator analysis)
        let reversed_exit = node_mapping.get(&exit_node)?;

        // Compute dominators on the reversed graph
        let dominators = dominators::simple_fast(&reversed_graph, *reversed_exit);

        // Convert dominators result to PostDominatorAnalysis
        let mut post_dominators = HashMap::new();
        let mut immediate_post_dominators = HashMap::new();

        for node_idx in graph.node_indices() {
            let reversed_node = node_mapping.get(&node_idx)?;

            // Build post-dominator set for this node
            let mut post_dom_set = HashSet::new();

            // Add all nodes that dominate this node in the reversed graph
            for other_node_idx in graph.node_indices() {
                let other_reversed_node = node_mapping.get(&other_node_idx)?;

                // Check if other_node dominates this node in reversed graph
                if self.dominates_in_graph(&dominators, *other_reversed_node, *reversed_node) {
                    post_dom_set.insert(other_node_idx);
                }
            }

            post_dominators.insert(node_idx, post_dom_set);

            // Get immediate post-dominator
            let immediate_post_dom =
                dominators
                    .immediate_dominator(*reversed_node)
                    .and_then(|dom_node| {
                        // Find original node index for this dominator
                        node_mapping
                            .iter()
                            .find(|(_, &rev_node)| rev_node == dom_node)
                            .map(|(&orig_node, _)| orig_node)
                    });

            immediate_post_dominators.insert(node_idx, immediate_post_dom);
        }

        Some(crate::cfg::analysis::PostDominatorAnalysis {
            post_dominators,
            immediate_post_dominators,
        })
    }

    /// Check if a node dominates another node using petgraph Dominators
    fn dominates_in_graph(
        &self,
        dominators: &Dominators<NodeIndex>,
        dominator: NodeIndex,
        node: NodeIndex,
    ) -> bool {
        let mut current = node;
        loop {
            if current == dominator {
                return true;
            }
            if let Some(immediate_dom) = dominators.immediate_dominator(current) {
                current = immediate_dom;
            } else {
                return false;
            }
        }
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
            .get_label_by_inst_index(self.function_index, start_pc.into())
            .cloned()
    }
}

// Helper function to convert a &str to Title Case
fn to_title_case(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap().to_uppercase();
    let rest = chars.as_str().to_lowercase();
    format!("{}{}", first, rest)
}
