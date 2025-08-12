//! Block-to-statement conversion system
//!
//! This module provides the BlockToStatementConverter that transforms CFG basic blocks
//! into sequences of JavaScript statements, building on instruction-to-expression conversion.

use crate::ast::{
    comments::AddressCommentManager,
    context::ExpressionContext,
    instructions::{InstructionResult, InstructionToStatementConverter, StatementConversionError},
};
use crate::{
    analysis::GlobalAnalysisResult,
    cfg::{block::Block, ssa::SSAAnalysis, EdgeKind},
    generated::unified_instructions::UnifiedInstruction,
    hbc::{function_table::HbcFunctionInstruction, InstructionIndex},
};
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{ast::Statement, AstBuilder as OxcAstBuilder};
use petgraph::{graph::NodeIndex, Graph};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Error types for block-to-statement conversion
#[derive(Debug, thiserror::Error)]
pub enum BlockConversionError {
    #[error("Statement conversion error: {0}")]
    StatementConversion(#[from] StatementConversionError),
    #[error("Invalid block structure: {0}")]
    InvalidBlock(String),
    #[error("Variable scoping error: {0}")]
    VariableScoping(String),
}

/// Statistics for block conversion operations
#[derive(Debug, Clone, Default)]
pub struct BlockConversionStats {
    /// Number of blocks processed
    pub blocks_processed: usize,
    /// Total instructions converted
    pub instructions_converted: usize,
    /// Total statements generated
    pub statements_generated: usize,
    /// Variables declared
    pub variables_declared: usize,
    /// Dead instructions eliminated
    pub dead_instructions_eliminated: usize,
}

/// Manages variable scoping within basic blocks
#[derive(Debug, Default)]
pub struct BlockScopeManager {
    /// Registers that have been declared as variables in current scope
    declared_variables: HashSet<u8>,
    /// Mapping from register to variable name for this block
    block_locals: HashMap<u8, String>,
    /// Registers that require declaration in this block
    requires_declaration: HashSet<u8>,
}

impl BlockScopeManager {
    /// Create a new block scope manager
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if a register has been declared in current scope
    pub fn is_declared(&self, register: u8) -> bool {
        self.declared_variables.contains(&register)
    }

    /// Mark a register as declared
    pub fn mark_declared(&mut self, register: u8) {
        self.declared_variables.insert(register);
    }

    /// Check if a register requires declaration
    pub fn requires_declaration(&self, register: u8) -> bool {
        self.requires_declaration.contains(&register)
    }

    /// Mark a register as requiring declaration
    pub fn mark_requires_declaration(&mut self, register: u8) {
        self.requires_declaration.insert(register);
    }

    /// Set variable name for a register in this block
    pub fn set_block_local(&mut self, register: u8, name: String) {
        self.block_locals.insert(register, name);
    }

    /// Get variable name for a register in this block
    pub fn get_block_local(&self, register: u8) -> Option<&String> {
        self.block_locals.get(&register)
    }

    /// Reset scope for new block
    pub fn reset(&mut self) {
        self.declared_variables.clear();
        self.block_locals.clear();
        self.requires_declaration.clear();
    }
}

/// Converts CFG basic blocks into JavaScript statement sequences
pub struct BlockToStatementConverter<'a> {
    /// New instruction-to-statement converter (1:1 mapping)
    instruction_converter: InstructionToStatementConverter<'a>,
    /// Variable scoping manager
    scope_manager: BlockScopeManager,
    /// Conversion statistics
    stats: BlockConversionStats,
    /// Whether to include instruction-level comments
    include_instruction_comments: bool,
    /// Optional SSA analysis for generating SSA comments
    ssa_analysis: Option<SSAAnalysis>,
    /// Whether to include SSA comments
    include_ssa_comments: bool,
    /// Optional global analysis for cross-function variable resolution
    _global_analysis: Option<Arc<GlobalAnalysisResult>>,
    /// Track which instructions have been rendered by their absolute offset
    rendered_instructions: HashSet<InstructionIndex>,
    /// Address-based comment manager for collecting comments
    comment_manager: Option<AddressCommentManager>,
}

impl<'a> BlockToStatementConverter<'a> {
    /// Create a new block-to-statement converter
    pub fn new(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
        include_instruction_comments: bool,
    ) -> Self {
        let instruction_converter =
            InstructionToStatementConverter::new(ast_builder, expression_context.clone());

        Self {
            instruction_converter,
            scope_manager: BlockScopeManager::new(),
            stats: BlockConversionStats::default(),
            include_instruction_comments,
            ssa_analysis: None,
            include_ssa_comments: false,
            _global_analysis: None,
            rendered_instructions: HashSet::new(),
            comment_manager: if include_instruction_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
        }
    }

    /// Create a new block-to-statement converter with SSA analysis
    pub fn with_ssa_analysis(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
        include_instruction_comments: bool,
        include_ssa_comments: bool,
        ssa_analysis: SSAAnalysis,
        cfg: &crate::cfg::Cfg,
    ) -> Self {
        let mut instruction_converter =
            InstructionToStatementConverter::new(ast_builder, expression_context.clone());

        // Generate variable mapping from SSA analysis
        let mut variable_mapper = crate::ast::variables::VariableMapper::new();
        if let Ok(variable_mapping) = variable_mapper.generate_mapping(&ssa_analysis, cfg) {
            // Set the SSA-based variable mapping in the register manager
            instruction_converter
                .register_manager_mut()
                .set_variable_mapping(variable_mapping);
        }

        Self {
            instruction_converter,
            scope_manager: BlockScopeManager::new(),
            stats: BlockConversionStats::default(),
            include_instruction_comments,
            ssa_analysis: Some(ssa_analysis),
            include_ssa_comments,
            _global_analysis: None,
            rendered_instructions: HashSet::new(),
            comment_manager: if include_instruction_comments || include_ssa_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
        }
    }

    /// Create a new block-to-statement converter with SSA and global analysis
    pub fn with_ssa_and_global_analysis(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
        include_instruction_comments: bool,
        include_ssa_comments: bool,
        ssa_analysis: SSAAnalysis,
        cfg: &crate::cfg::Cfg,
        _global_analysis: Arc<GlobalAnalysisResult>,
    ) -> Self {
        let mut instruction_converter =
            InstructionToStatementConverter::new(ast_builder, expression_context.clone());

        // Generate variable mapping from SSA analysis
        let mut variable_mapper = crate::ast::variables::VariableMapper::new();
        if let Ok(variable_mapping) = variable_mapper.generate_mapping(&ssa_analysis, cfg) {
            // Set the SSA-based variable mapping in the register manager
            instruction_converter
                .register_manager_mut()
                .set_variable_mapping(variable_mapping);
        }

        // Pass global analyzer to instruction converter for environment variable resolution
        instruction_converter.set_global_analyzer(Some(_global_analysis.clone()));

        Self {
            instruction_converter,
            scope_manager: BlockScopeManager::new(),
            stats: BlockConversionStats::default(),
            include_instruction_comments,
            ssa_analysis: Some(ssa_analysis),
            include_ssa_comments,
            _global_analysis: Some(_global_analysis),
            rendered_instructions: HashSet::new(),
            comment_manager: if include_instruction_comments || include_ssa_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
        }
    }

    /// Create a new block-to-statement converter with fallback variable mapping
    pub fn with_fallback_mapping(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
        include_instruction_comments: bool,
        variable_mapping: crate::ast::variables::VariableMapping,
    ) -> Self {
        let mut instruction_converter =
            InstructionToStatementConverter::new(ast_builder, expression_context.clone());

        // Set the fallback variable mapping in the register manager
        instruction_converter
            .register_manager_mut()
            .set_variable_mapping(variable_mapping);

        Self {
            instruction_converter,
            scope_manager: BlockScopeManager::new(),
            stats: BlockConversionStats::default(),
            include_instruction_comments,
            ssa_analysis: None,
            include_ssa_comments: false,
            _global_analysis: None,
            rendered_instructions: HashSet::new(),
            comment_manager: if include_instruction_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
        }
    }

    /// Convert multiple blocks from a CFG into a sequence of JavaScript statements
    /// This method handles proper block ordering and labeling
    pub fn convert_blocks_from_cfg(
        &mut self,
        cfg: &'a crate::cfg::Cfg<'a>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        let mut all_statements =
            ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        // Get blocks in structured execution order
        let block_order = cfg.structured_execution_order();
        let blocks_needing_labels = cfg.blocks_needing_labels();

        // Check if CFG has conditional analysis
        let conditional_analysis = cfg.analyze_conditional_chains();
        // Check if CFG has switch analysis
        let switch_analysis = cfg.analyze_switch_regions();
        let mut processed_blocks: HashSet<NodeIndex> = HashSet::new();

        for (_order_idx, block_id) in block_order.iter().enumerate() {
            // Skip if this block was already processed as part of a conditional chain
            if processed_blocks.contains(block_id) {
                continue;
            }

            let block = &cfg.graph()[*block_id];

            // Skip exit blocks
            if block.is_exit() {
                continue;
            }

            // Check if this block is the start of a switch region BEFORE checking conditionals
            // This ensures sparse switches are detected before being converted to if-else chains
            if let Some(ref analysis) = switch_analysis {
                if let Some(region) = self.find_switch_starting_at(*block_id, analysis) {
                    // Convert the entire switch region
                    // Initially mark only switch infrastructure blocks as processed
                    // to allow nested control flow detection in case bodies
                    let switch_infrastructure =
                        super::switch_converter::get_switch_infrastructure_blocks(region, cfg);
                    processed_blocks.extend(&switch_infrastructure);
                    // Create the switch converter with expression context
                    let mut switch_converter =
                        super::switch_converter::SwitchConverter::with_context(
                            self.instruction_converter.ast_builder(),
                            self.instruction_converter.get_expression_context().clone(),
                        );
                    // Convert the switch to statements
                    match switch_converter.convert_switch_region(region, cfg, self) {
                        Ok(switch_statements) => {
                            all_statements.extend(switch_statements);

                            // After conversion, mark ALL blocks that were part of the switch
                            // including case bodies to prevent duplicate processing
                            let all_switch_blocks =
                                super::switch_converter::get_all_switch_blocks_with_bodies(
                                    region, cfg,
                                );
                            processed_blocks.extend(&all_switch_blocks);

                            // After converting a switch, we need to ensure the join block is processed
                            // if it hasn't been already (it contains code after the switch)
                            if !processed_blocks.contains(&region.join_block) {
                                // The join block might be an exit block, so we need to process it manually
                                let join_block = &cfg.graph()[region.join_block];

                                // Check if join block has any instructions that need to be converted
                                if !join_block.instructions().is_empty() {
                                    let join_stmts = self.convert_block(
                                        join_block,
                                        region.join_block,
                                        cfg.graph(),
                                    )?;
                                    all_statements.extend(join_stmts);
                                    processed_blocks.insert(region.join_block);
                                }
                            }

                            // HACK: Special handling for blocks that are both case targets and contain return statements
                            // This happens when cases jump directly to the function's return block
                            // We need to ensure the return statement gets processed
                            for case in &region.cases {
                                let case_block = &cfg.graph()[case.case_head];
                                // Check if this case block contains a return statement
                                if case_block.instructions().iter().any(|instr| {
                                    matches!(&instr.instruction,
                                        crate::generated::unified_instructions::UnifiedInstruction::Ret { .. })
                                }) {
                                    // If this block hasn't been processed yet, process it now
                                    if !processed_blocks.contains(&case.case_head) {
                                        let case_stmts = self.convert_block(
                                            case_block,
                                            case.case_head,
                                            cfg.graph(),
                                        )?;
                                        all_statements.extend(case_stmts);
                                        processed_blocks.insert(case.case_head);
                                    }
                                }
                            }

                            continue;
                        }
                        Err(e) => {
                            // Error - fall back to basic block conversion
                            if self.include_instruction_comments {
                                eprintln!("Warning: Failed to convert switch region starting at block {}: {}", block_id.index(), e);
                            }
                        }
                    }
                }
            }
            // Check if this block is the start of a conditional chain
            if let Some(ref analysis) = conditional_analysis {
                if let Some(chain) = self.find_chain_starting_at(*block_id, analysis) {
                    // Convert the entire conditional chain
                    // Mark ALL blocks in the chain (including nested) as processed
                    let all_chain_blocks =
                        super::conditional_converter::ConditionalConverter::get_all_chain_blocks(
                            chain,
                        );
                    processed_blocks.extend(&all_chain_blocks);

                    // Store the join block to process it separately after the chain
                    let join_block = chain.join_block;

                    // Create the conditional converter
                    let mut conditional_converter =
                        super::conditional_converter::ConditionalConverter::new(
                            self.instruction_converter.ast_builder(),
                        );

                    // Convert the chain to statements (condition setup + if statement)
                    match conditional_converter.convert_chain(chain, cfg, self) {
                        Ok(chain_statements) => {
                            all_statements.extend(chain_statements);

                            // After converting a conditional chain, we need to ensure the join block is processed
                            // The conditional converter marks it as processed but doesn't actually convert it
                            // Remove it from the processed set so it can be converted in the normal flow
                            processed_blocks.remove(&join_block);

                            continue;
                        }
                        Err(e) => {
                            // Fall back to basic block conversion if conditional conversion fails
                            if self.include_instruction_comments {
                                eprintln!("Warning: Failed to convert conditional chain starting at block {}: {}", block_id.index(), e);
                            }
                        }
                    }
                }
            }

            // Add label if this block needs one
            if blocks_needing_labels.contains(block_id) {
                let label_stmt = self.create_block_label(*block_id)?;
                all_statements.push(label_stmt);
            }

            // Convert the block to statements
            let block_statements = self.convert_block(block, *block_id, cfg.graph())?;
            for stmt in block_statements {
                all_statements.push(stmt);
            }

            // Mark this block as processed
            processed_blocks.insert(*block_id);
        }

        // Verify all blocks and instructions were rendered
        self.verify_all_rendered(cfg, &processed_blocks)?;

        Ok(all_statements)
    }

    /// Verify that all blocks and instructions were rendered exactly once
    fn verify_all_rendered(
        &self,
        cfg: &'a crate::cfg::Cfg<'a>,
        processed_blocks: &HashSet<NodeIndex>,
    ) -> Result<(), BlockConversionError> {
        // Check all blocks (except EXIT blocks)
        for node in cfg.graph().node_indices() {
            let block = &cfg.graph()[node];

            // Skip EXIT blocks
            if block.is_exit() {
                continue;
            }

            // Check if block was processed
            if !processed_blocks.contains(&node) {
                return Err(BlockConversionError::InvalidBlock(format!(
                    "Block {} (PC {}-{}) was not processed during decompilation",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                )));
            }

            // Check all instructions in the block
            for (idx, instruction) in block.instructions().iter().enumerate() {
                if !self
                    .rendered_instructions
                    .contains(&instruction.instruction_index)
                {
                    // LoadParam instructions are handled as part of function parameter generation
                    let is_param_load = matches!(
                        &instruction.instruction,
                        UnifiedInstruction::LoadParam { .. }
                            | UnifiedInstruction::LoadParamLong { .. }
                    );

                    // Terminal jumps are often skipped as they're implicit in control flow
                    let is_terminal_jump = idx == block.instructions().len() - 1
                        && matches!(
                            &instruction.instruction,
                            UnifiedInstruction::Jmp { .. }
                                | UnifiedInstruction::JmpLong { .. }
                                | UnifiedInstruction::JmpTrue { .. }
                                | UnifiedInstruction::JmpFalse { .. }
                                | UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                                | UnifiedInstruction::JStrictNotEqual { .. }
                                | UnifiedInstruction::JStrictNotEqualLong { .. }
                        );

                    if !is_terminal_jump && !is_param_load {
                        return Err(BlockConversionError::InvalidBlock(format!(
                            "Instruction at PC {} (index {} in block {}) was not rendered: {:?}",
                            instruction.offset.value(),
                            idx,
                            node.index(),
                            instruction.instruction
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Find a conditional chain that starts at the given block
    pub fn find_chain_starting_at<'b>(
        &self,
        block_id: NodeIndex,
        analysis: &'b crate::cfg::analysis::ConditionalAnalysis,
    ) -> Option<&'b crate::cfg::analysis::ConditionalChain> {
        // Check if this block is the entry point of any conditional chain
        for chain in &analysis.chains {
            // Check if this is the first condition block in the chain
            if !chain.branches.is_empty() && chain.branches[0].condition_block == block_id {
                return Some(chain);
            }
        }
        None
    }

    /// Find a switch region that starts at the given block
    pub fn find_switch_starting_at<'b>(
        &self,
        block_id: NodeIndex,
        analysis: &'b crate::cfg::analysis::SwitchAnalysis,
    ) -> Option<&'b crate::cfg::analysis::SwitchRegion> {
        // Check if this block is the dispatch block of any switch region
        for region in &analysis.regions {
            if region.dispatch == block_id {
                return Some(region);
            }
        }
        None
    }

    /// Convert a basic block by index (public method for conditional converter)
    pub fn convert_basic_block(
        &mut self,
        _block_idx: NodeIndex,
    ) -> Result<ArenaVec<'a, Statement<'a>>, String> {
        // This is a simplified version that doesn't have access to the full CFG
        // For now, return empty statements
        // TODO: Implement proper block conversion without full CFG context
        Ok(ArenaVec::new_in(
            self.instruction_converter.ast_builder().allocator,
        ))
    }

    /// Check if an instruction has been rendered
    pub fn is_instruction_rendered(&self, instruction: &HbcFunctionInstruction) -> bool {
        self.rendered_instructions
            .contains(&instruction.instruction_index)
    }

    /// Mark an instruction as rendered
    pub fn mark_instruction_rendered(&mut self, instruction: &HbcFunctionInstruction) {
        self.rendered_instructions
            .insert(instruction.instruction_index);
    }

    /// Convert a basic block into a sequence of JavaScript statements
    pub fn convert_block(
        &mut self,
        block: &Block,
        block_id: NodeIndex,
        _cfg: &Graph<Block, EdgeKind>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        self.convert_block_with_options(
            block,
            block_id,
            _cfg,
            None::<fn(&HbcFunctionInstruction, bool) -> bool>,
            false,
        )
    }

    /// Convert a basic block into a sequence of JavaScript statements with options
    ///
    /// # Arguments
    /// * `block` - The block to convert
    /// * `block_id` - The block's node index in the CFG
    /// * `_cfg` - The control flow graph
    /// * `instruction_filter` - Optional filter to skip certain instructions (e.g., terminal jumps)
    /// * `skip_phi_declarations` - Whether to skip phi variable declarations
    pub fn convert_block_with_options<F>(
        &mut self,
        block: &Block,
        block_id: NodeIndex,
        _cfg: &Graph<Block, EdgeKind>,
        instruction_filter: Option<F>,
        skip_phi_declarations: bool,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError>
    where
        F: Fn(&HbcFunctionInstruction, bool) -> bool,
    {
        self.convert_block_with_marking_control(
            block,
            block_id,
            _cfg,
            instruction_filter,
            skip_phi_declarations,
            true, // mark_as_rendered
        )
    }

    /// Convert a block with full control over instruction marking
    pub fn convert_block_with_marking_control<F>(
        &mut self,
        block: &Block,
        block_id: NodeIndex,
        _cfg: &Graph<Block, EdgeKind>,
        instruction_filter: Option<F>,
        skip_phi_declarations: bool,
        mark_as_rendered: bool,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError>
    where
        F: Fn(&HbcFunctionInstruction, bool) -> bool,
    {
        // Reset scope for new block
        self.scope_manager.reset();

        // Pre-analyze block for variable requirements
        self.analyze_block_variables(block)?;

        // Convert instructions to statements
        let mut statements = ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        // Check if this block needs any phi variable declarations
        if !skip_phi_declarations {
            let phi_declarations = self
                .ssa_analysis
                .as_ref()
                .and_then(|ssa| ssa.phi_variable_declarations.get(&block_id))
                .cloned();

            if let Some(phi_declarations) = phi_declarations {
                for phi_decl in &phi_declarations {
                    // Create a variable declaration for this phi register
                    let var_decl = self.create_phi_variable_declaration(phi_decl)?;
                    statements.push(var_decl);
                }
            }
        }

        let instruction_count = block.instructions().len();
        for (instruction_index, instruction) in block.instructions().iter().enumerate() {
            // Skip if this instruction has already been rendered
            if self.is_instruction_rendered(instruction) {
                continue;
            }

            // Apply instruction filter if provided
            if let Some(ref filter) = instruction_filter {
                let is_last = instruction_index == instruction_count - 1;
                if !filter(instruction, is_last) {
                    continue;
                }
            }

            let instruction_offset = instruction.offset; // Use the absolute offset from the instruction
                                                         // Process instruction

            // Convert the instruction to statements first
            let instruction_statements =
                self.convert_instruction_to_statements(instruction, instruction.instruction_index)?;

            // Add comments to the first statement if we have a comment manager
            if self.comment_manager.is_some() && !instruction_statements.is_empty() {
                // Get the SSA info first (if needed) to avoid borrowing conflicts
                let ssa_info = if self.include_ssa_comments {
                    if let Some(ref ssa_analysis) = self.ssa_analysis {
                        let info = self.format_ssa_info(
                            block_id,
                            InstructionIndex::from(instruction_index),
                            ssa_analysis,
                        );
                        info
                    } else {
                        None
                    }
                } else {
                    None
                };

                // Now borrow the comment manager mutably and attach comments to the first statement
                if let Some(ref mut comment_manager) = self.comment_manager {
                    if let Some(first_stmt) = instruction_statements.first() {
                        // Add instruction comment if enabled
                        if self.include_instruction_comments {
                            let instruction_info = if let Some(hbc_file) = self
                                .instruction_converter
                                .get_expression_context()
                                .hbc_file()
                            {
                                format!(
                                    "PC {}: {}",
                                    instruction_offset,
                                    instruction.format_instruction(hbc_file)
                                )
                            } else {
                                format!("PC {}: {:?}", instruction_offset, instruction.instruction)
                            };
                            comment_manager.add_comment(
                                first_stmt,
                                instruction_info,
                                crate::ast::comments::CommentKind::Line,
                                crate::ast::comments::CommentPosition::Leading,
                            );
                        }

                        // Add SSA comment if we have one
                        if let Some(ssa_info) = ssa_info {
                            comment_manager.add_comment(
                                first_stmt,
                                ssa_info,
                                crate::ast::comments::CommentKind::Line,
                                crate::ast::comments::CommentPosition::Leading,
                            );
                        }
                    }
                }
            }

            for stmt in instruction_statements {
                statements.push(stmt);
            }

            // Mark this instruction as rendered (if requested)
            if mark_as_rendered {
                self.mark_instruction_rendered(instruction);
            }
        }

        // Post-process for optimizations
        self.optimize_statement_sequence(&mut statements)?;

        // Update statistics
        self.stats.blocks_processed += 1;
        self.stats.instructions_converted += block.instructions().len();
        self.stats.statements_generated += statements.len();

        Ok(statements)
    }

    /// Convert a single instruction into one or more statements
    fn convert_instruction_to_statements(
        &mut self,
        instruction: &HbcFunctionInstruction,
        pc: InstructionIndex,
    ) -> Result<Vec<Statement<'a>>, BlockConversionError> {
        let mut statements = Vec::new();

        // Set current PC in the instruction converter
        self.instruction_converter.set_current_pc(pc.0 as u32);

        // Use the new InstructionToStatementConverter for direct 1:1 instruction mapping
        match self
            .instruction_converter
            .convert_instruction(&instruction.instruction)
            .map_err(BlockConversionError::StatementConversion)?
        {
            InstructionResult::Statement(stmt) => {
                // Track variable declarations for statistics
                if matches!(stmt, Statement::VariableDeclaration(_)) {
                    self.stats.variables_declared += 1;
                }
                statements.push(stmt);
            }
            InstructionResult::JumpCondition(_jump_info) => {
                // Jump instructions don't generate statements directly
                // They provide condition info for the block converter to handle control flow
            }
            InstructionResult::None => {
                // Instruction like nops that don't produce output
            }
        }

        // Handle only the special instructions that need block-level context
        // Most instructions are now handled by the InstructionToStatementConverter
        match &instruction.instruction {
            // Instructions that don't need to generate visible statements
            UnifiedInstruction::CreateEnvironment { operand_0 } => {
                // CreateEnvironment is an internal VM operation that sets up lexical scopes
                // Track the environment register but don't generate visible JavaScript
                self.instruction_converter
                    .register_manager_mut()
                    .track_usage(*operand_0, pc.into());
                // Mark this register as representing an environment
                let env_var_name = format!("__env_{}", operand_0);
                self.instruction_converter
                    .register_manager_mut()
                    .set_variable_name(*operand_0, env_var_name);
            }

            // All other instructions are handled by InstructionToStatementConverter
            _ => {
                // Already handled above by the InstructionToStatementConverter
                // This branch is kept for future special cases
            }
        }

        Ok(statements)
    }

    /// Analyze block to determine variable declaration requirements
    fn analyze_block_variables(&mut self, block: &Block) -> Result<(), BlockConversionError> {
        for instruction in block.instructions() {
            if let Some(target_register) = self.get_target_register(&instruction.instruction) {
                // For now, mark all target registers as requiring declaration
                // TODO: Implement more sophisticated analysis based on register lifetimes
                self.scope_manager
                    .mark_requires_declaration(target_register);
            }
        }
        Ok(())
    }

    /// Extract the target register from an instruction (if any)
    fn get_target_register(&self, instruction: &UnifiedInstruction) -> Option<u8> {
        match instruction {
            // Instructions with destination registers (Reg8 as first parameter)
            UnifiedInstruction::Add { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Add32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::AddEmptyString { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::AddN { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::BitAnd { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::BitNot { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::BitOr { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::BitXor { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call1 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call2 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call3 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call4 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CallBuiltin { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CallBuiltinLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CallDirect { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CallDirectLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CallLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Catch { operand_0 } => Some(*operand_0),
            UnifiedInstruction::CoerceThisNS { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Construct { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ConstructLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateAsyncClosure { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateAsyncClosureLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateClosure { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateClosureLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateGenerator { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateGeneratorClosure { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateGeneratorClosureLongIndex { operand_0, .. } => {
                Some(*operand_0)
            }
            UnifiedInstruction::CreateGeneratorLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateInnerEnvironment { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateRegExp { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::CreateThis { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Dec { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::DelById { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::DelByIdLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::DelByVal { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::DirectEval { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Div { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::DivN { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Divi32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Divu32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Eq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetArgumentsLength { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetArgumentsPropByVal { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetBuiltinClosure { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetById { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetByIdLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetByIdShort { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetByVal { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetEnvironment { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetGlobalObject { operand_0 } => Some(*operand_0),
            UnifiedInstruction::GetNewTarget { operand_0 } => Some(*operand_0),
            UnifiedInstruction::GetNextPName { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetPNameList { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Greater { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GreaterEq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Inc { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::InstanceOf { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::IsIn { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::IteratorBegin { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::IteratorClose { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::IteratorNext { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LShift { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Less { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LessEq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstBigInt { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstBigIntLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstDouble { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstEmpty { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstFalse { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstInt { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstNull { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstString { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstStringLongIndex { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstTrue { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstUInt8 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstUndefined { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstZero { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadFromEnvironment { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadFromEnvironmentL { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadParam { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadParamLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadThisNS { operand_0 } => Some(*operand_0),
            UnifiedInstruction::Loadi16 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Loadi32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Loadi8 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Loadu16 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Loadu32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Loadu8 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mod { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mov { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mul { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mul32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::MulN { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Negate { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Neq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewArray { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewArrayWithBuffer { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewArrayWithBufferLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewObject { operand_0 } => Some(*operand_0),
            UnifiedInstruction::NewObjectWithBuffer { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewObjectWithBufferLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::NewObjectWithParent { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Not { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::RShift { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ReifyArguments { operand_0 } => Some(*operand_0),
            UnifiedInstruction::ResumeGenerator { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::SelectObject { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::StrictEq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::StrictNeq { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Sub { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Sub32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::SubN { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ThrowIfEmpty { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ToInt32 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ToNumber { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::ToNumeric { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::TryGetById { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::TryGetByIdLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::TryPutById { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::TryPutByIdLong { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::TypeOf { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::URshift { operand_0, .. } => Some(*operand_0),

            // Instructions without target registers
            UnifiedInstruction::AsyncBreakCheck { .. } => None,
            UnifiedInstruction::MovLong { .. } => None,
            UnifiedInstruction::CompleteGenerator { .. } => None,
            UnifiedInstruction::CreateEnvironment { .. } => None,
            UnifiedInstruction::Debugger { .. } => None,
            UnifiedInstruction::DebuggerCheckBreak { .. } => None,
            UnifiedInstruction::DeclareGlobalVar { .. } => None,
            UnifiedInstruction::Jmp { .. } => None,
            UnifiedInstruction::JmpFalse { .. } => None,
            UnifiedInstruction::JmpFalseLong { .. } => None,
            UnifiedInstruction::JmpLong { .. } => None,
            UnifiedInstruction::JmpTrue { .. } => None,
            UnifiedInstruction::JmpTrueLong { .. } => None,
            UnifiedInstruction::JmpUndefined { .. } => None,
            UnifiedInstruction::JmpUndefinedLong { .. } => None,
            UnifiedInstruction::JEqual { .. } => None,
            UnifiedInstruction::JEqualLong { .. } => None,
            UnifiedInstruction::JNotEqual { .. } => None,
            UnifiedInstruction::JNotEqualLong { .. } => None,
            UnifiedInstruction::JStrictEqual { .. } => None,
            UnifiedInstruction::JStrictEqualLong { .. } => None,
            UnifiedInstruction::JStrictNotEqual { .. } => None,
            UnifiedInstruction::JStrictNotEqualLong { .. } => None,
            UnifiedInstruction::JLess { .. } => None,
            UnifiedInstruction::JLessLong { .. } => None,
            UnifiedInstruction::JLessEqual { .. } => None,
            UnifiedInstruction::JLessEqualLong { .. } => None,
            UnifiedInstruction::JLessN { .. } => None,
            UnifiedInstruction::JLessNLong { .. } => None,
            UnifiedInstruction::JLessEqualN { .. } => None,
            UnifiedInstruction::JLessEqualNLong { .. } => None,
            UnifiedInstruction::JNotLess { .. } => None,
            UnifiedInstruction::JNotLessLong { .. } => None,
            UnifiedInstruction::JNotLessEqual { .. } => None,
            UnifiedInstruction::JNotLessEqualLong { .. } => None,
            UnifiedInstruction::JNotLessN { .. } => None,
            UnifiedInstruction::JNotLessNLong { .. } => None,
            UnifiedInstruction::JNotLessEqualN { .. } => None,
            UnifiedInstruction::JNotLessEqualNLong { .. } => None,
            UnifiedInstruction::JGreater { .. } => None,
            UnifiedInstruction::JGreaterLong { .. } => None,
            UnifiedInstruction::JGreaterEqual { .. } => None,
            UnifiedInstruction::JGreaterEqualLong { .. } => None,
            UnifiedInstruction::JGreaterN { .. } => None,
            UnifiedInstruction::JGreaterNLong { .. } => None,
            UnifiedInstruction::JGreaterEqualN { .. } => None,
            UnifiedInstruction::JGreaterEqualNLong { .. } => None,
            UnifiedInstruction::JNotGreater { .. } => None,
            UnifiedInstruction::JNotGreaterLong { .. } => None,
            UnifiedInstruction::JNotGreaterEqual { .. } => None,
            UnifiedInstruction::JNotGreaterEqualLong { .. } => None,
            UnifiedInstruction::JNotGreaterN { .. } => None,
            UnifiedInstruction::JNotGreaterNLong { .. } => None,
            UnifiedInstruction::JNotGreaterEqualN { .. } => None,
            UnifiedInstruction::JNotGreaterEqualNLong { .. } => None,
            UnifiedInstruction::ProfilePoint { .. } => None,
            UnifiedInstruction::PutById { .. } => None,
            UnifiedInstruction::PutByIdLong { .. } => None,
            UnifiedInstruction::PutByVal { .. } => None,
            UnifiedInstruction::PutNewOwnById { .. } => None,
            UnifiedInstruction::PutNewOwnByIdLong { .. } => None,
            UnifiedInstruction::PutNewOwnByIdShort { .. } => None,
            UnifiedInstruction::PutNewOwnNEById { .. } => None,
            UnifiedInstruction::PutNewOwnNEByIdLong { .. } => None,
            UnifiedInstruction::PutOwnByIndex { .. } => None,
            UnifiedInstruction::PutOwnByIndexL { .. } => None,
            UnifiedInstruction::PutOwnByVal { .. } => None,
            UnifiedInstruction::PutOwnGetterSetterByVal { .. } => None,
            UnifiedInstruction::Ret { .. } => None,
            UnifiedInstruction::SaveGenerator { .. } => None,
            UnifiedInstruction::SaveGeneratorLong { .. } => None,
            UnifiedInstruction::StartGenerator { .. } => None,
            UnifiedInstruction::Store16 { .. } => None,
            UnifiedInstruction::Store32 { .. } => None,
            UnifiedInstruction::Store8 { .. } => None,
            UnifiedInstruction::StoreNPToEnvironment { .. } => None,
            UnifiedInstruction::StoreNPToEnvironmentL { .. } => None,
            UnifiedInstruction::StoreToEnvironment { .. } => None,
            UnifiedInstruction::StoreToEnvironmentL { .. } => None,
            UnifiedInstruction::SwitchImm { .. } => None,
            UnifiedInstruction::Throw { .. } => None,
            UnifiedInstruction::ThrowIfHasRestrictedGlobalProperty { .. } => None,
            UnifiedInstruction::ThrowIfUndefinedInst { .. } => None,
            UnifiedInstruction::Unreachable { .. } => None,
        }
    }

    /// Optimize the generated statement sequence
    fn optimize_statement_sequence(
        &mut self,
        _statements: &mut ArenaVec<'a, Statement<'a>>,
    ) -> Result<(), BlockConversionError> {
        // TODO: Implement statement-level optimizations:
        // - Merge consecutive variable declarations
        // - Eliminate dead assignments
        // - Combine assignment chains
        Ok(())
    }

    /// Get conversion statistics
    pub fn get_stats(&self) -> &BlockConversionStats {
        &self.stats
    }

    /// Reset conversion statistics
    pub fn reset_stats(&mut self) {
        self.stats = BlockConversionStats::default();
    }

    /// Get variable name for a register at a specific PC (for conditional expressions)
    pub fn get_variable_name_for_condition(
        &mut self,
        register: u8,
        pc: InstructionIndex,
    ) -> String {
        self.instruction_converter.set_current_pc(pc.0 as u32);
        self.instruction_converter
            .register_manager_mut()
            .get_source_variable_name(register)
    }

    /// Check if instruction comments are enabled
    pub fn include_instruction_comments(&self) -> bool {
        self.include_instruction_comments
    }

    /// Check if SSA comments are enabled
    pub fn include_ssa_comments(&self) -> bool {
        self.include_ssa_comments
    }

    /// Get a reference to the SSA analysis
    pub fn ssa_analysis(&self) -> Option<&SSAAnalysis> {
        self.ssa_analysis.as_ref()
    }

    /// Get access to the underlying instruction converter
    pub fn instruction_converter(&self) -> &InstructionToStatementConverter<'a> {
        &self.instruction_converter
    }

    /// Get mutable access to the underlying instruction converter
    pub fn instruction_converter_mut(&mut self) -> &mut InstructionToStatementConverter<'a> {
        &mut self.instruction_converter
    }

    /// Get access to the scope manager
    pub fn scope_manager(&self) -> &BlockScopeManager {
        &self.scope_manager
    }

    /// Set the comment manager (useful when building from external context)
    pub fn set_comment_manager(&mut self, comment_manager: AddressCommentManager) {
        self.comment_manager = Some(comment_manager);
    }

    /// Take the comment manager (transfers ownership)
    pub fn take_comment_manager(&mut self) -> Option<AddressCommentManager> {
        self.comment_manager.take()
    }

    /// Get mutable access to the comment manager
    pub fn comment_manager_mut(&mut self) -> Option<&mut AddressCommentManager> {
        self.comment_manager.as_mut()
    }

    /// Check if we have a comment manager (for future address-based comments)
    pub fn has_comment_manager(&self) -> bool {
        self.comment_manager.is_some()
    }

    /// Format SSA information for a comment
    pub fn format_ssa_info(
        &self,
        block_id: NodeIndex,
        instruction_index: InstructionIndex,
        ssa_analysis: &SSAAnalysis,
    ) -> Option<String> {
        let mut comment_parts = Vec::new();

        // Find SSA definitions and uses at this instruction index
        for def in &ssa_analysis.definitions {
            if def.instruction_idx == instruction_index && def.block_id == block_id {
                if let Some(ssa_value) = ssa_analysis.ssa_values.get(def) {
                    comment_parts.push(format!(
                        "DEF: r{} → r{}_{}",
                        def.register, ssa_value.register, ssa_value.version
                    ));
                }
            }
        }

        for use_site in &ssa_analysis.uses {
            if use_site.instruction_idx == instruction_index && use_site.block_id == block_id {
                if let Some(def_site) = ssa_analysis.use_def_chains.get(use_site) {
                    if let Some(ssa_value) = ssa_analysis.ssa_values.get(def_site) {
                        comment_parts.push(format!(
                            "USE: r{} → r{}_{}",
                            use_site.register, ssa_value.register, ssa_value.version
                        ));
                    }
                }
            }
        }

        if comment_parts.is_empty() {
            None
        } else {
            Some(comment_parts.join(", "))
        }
    }

    // ===== Private Helper Methods =====

    /// Create a label statement for a block
    fn create_block_label(
        &self,
        block_id: NodeIndex,
    ) -> Result<Statement<'a>, BlockConversionError> {
        let span = oxc_span::Span::default();
        let label_name = format!("L{}", block_id.index());
        let label_atom = self
            .instruction_converter
            .ast_builder()
            .allocator
            .alloc_str(&label_name);

        // Create an empty statement as the labeled statement body
        let empty_stmt = self
            .instruction_converter
            .ast_builder()
            .statement_empty(span);

        // Create the label identifier
        let label_id = self
            .instruction_converter
            .ast_builder()
            .label_identifier(span, label_atom);

        // Create the label statement
        Ok(self
            .instruction_converter
            .ast_builder()
            .statement_labeled(span, label_id, empty_stmt))
    }

    /// Create a variable declaration for a phi function
    fn create_phi_variable_declaration(
        &mut self,
        phi_decl: &crate::cfg::ssa::PhiRegisterDeclaration,
    ) -> Result<Statement<'a>, BlockConversionError> {
        // Get the variable name from the SSA value through the variable mapper
        // The phi_decl.ssa_value should already be mapped to the correct coalesced variable name
        let var_name = if let Some(mapping) = self
            .instruction_converter
            .register_manager_mut()
            .variable_mapping()
        {
            // Look up the variable name for this SSA value
            // This should give us the coalesced name that all versions of this variable use
            mapping
                .ssa_to_var
                .get(&phi_decl.ssa_value)
                .cloned()
                .unwrap_or_else(|| format!("var{}", phi_decl.register))
        } else {
            // No variable mapping available, use default naming
            format!("var{}", phi_decl.register)
        };

        // Create a declaration without initialization
        // We use 'let' because phi variables are typically reassigned
        self.instruction_converter
            .create_variable_declaration(
                &var_name,
                None,
                oxc_ast::ast::VariableDeclarationKind::Let,
            )
            .map_err(BlockConversionError::StatementConversion)
    }
}
