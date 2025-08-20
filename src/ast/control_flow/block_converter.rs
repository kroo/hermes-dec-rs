//! Block-to-statement conversion system
//!
//! This module provides the BlockToStatementConverter that transforms CFG basic blocks
//! into sequences of JavaScript statements, building on instruction-to-expression conversion.

use crate::ast::comments::{CommentKind, CommentPosition};
use crate::ast::{
    comments::AddressCommentManager,
    context::ExpressionContext,
    instructions::{InstructionResult, InstructionToStatementConverter, StatementConversionError},
    optimization::SSAUsageTracker,
};
use crate::{
    analysis::GlobalAnalysisResult,
    cfg::ssa::SSAValue,
    cfg::{block::Block, ssa::SSAAnalysis, EdgeKind},
    generated::unified_instructions::UnifiedInstruction,
    hbc::{function_table::HbcFunctionInstruction, InstructionIndex},
    HbcFile,
};
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{BindingPatternKind, Statement, VariableDeclarationKind},
    AstBuilder as OxcAstBuilder,
};
use oxc_span::Span;
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
    /// Whether to include SSA comments
    include_ssa_comments: bool,
    /// Track which instructions have been rendered by their absolute offset
    rendered_instructions: HashSet<InstructionIndex>,
    /// Address-based comment manager for collecting comments
    comment_manager: Option<AddressCommentManager>,
    /// SSA usage tracker for smart variable elimination and tracking
    ssa_usage_tracker: SSAUsageTracker<'a>,
    /// Function analysis containing all analysis data
    function_analysis: &'a crate::analysis::FunctionAnalysis<'a>,
    /// Track variables that were used but not declared
    undeclared_variables: HashSet<String>,
}

impl<'a> BlockToStatementConverter<'a> {
    /// Create a new block-to-statement converter with function analysis
    pub fn new_with_analysis(
        ast_builder: &'a OxcAstBuilder<'a>,
        function_analysis: &'a crate::analysis::FunctionAnalysis<'a>,
        hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
        include_instruction_comments: bool,
        include_ssa_comments: bool,
    ) -> Self {
        // Create expression context from function analysis
        let expression_context = ExpressionContext {
            current_block: None,
            current_pc: InstructionIndex(0),
            hbc_file: Some(function_analysis.hbc_file),
            function_index: Some(function_analysis.function_index),
        };

        let mut instruction_converter = InstructionToStatementConverter::new_with_analysis(
            ast_builder,
            expression_context,
            hbc_analysis,
        );

        // Generate variable mapping from SSA analysis
        let mut variable_mapper = crate::ast::variables::VariableMapper::new();
        if let Ok(variable_mapping) =
            variable_mapper.generate_mapping(&function_analysis.ssa, &function_analysis.cfg)
        {
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
            include_ssa_comments,
            rendered_instructions: HashSet::new(),
            comment_manager: if include_instruction_comments || include_ssa_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
            ssa_usage_tracker: SSAUsageTracker::new(function_analysis),
            function_analysis,
            undeclared_variables: HashSet::new(),
        }
    }

    /// Create a new block-to-statement converter (legacy - DEPRECATED)
    #[deprecated(note = "Use new_with_analysis instead - this will panic")]
    pub fn new(
        _ast_builder: &'a OxcAstBuilder<'a>,
        _expression_context: ExpressionContext<'a>,
        _include_instruction_comments: bool,
    ) -> Self {
        panic!("Legacy constructor is deprecated. Use new_with_analysis with FunctionAnalysis instead.");
    }

    /// Create a new block-to-statement converter with SSA analysis (legacy - DEPRECATED)
    #[deprecated(note = "Use new_with_analysis instead - this will panic")]
    pub fn with_ssa_analysis(
        _ast_builder: &'a OxcAstBuilder<'a>,
        _expression_context: ExpressionContext<'a>,
        _include_instruction_comments: bool,
        _include_ssa_comments: bool,
        _ssa_analysis: SSAAnalysis,
        _cfg: &crate::cfg::Cfg,
    ) -> Self {
        panic!("Legacy constructor is deprecated. Use new_with_analysis with FunctionAnalysis instead.");
    }

    /// Create a new block-to-statement converter with SSA and global analysis (legacy - DEPRECATED)
    #[deprecated(note = "Use new_with_analysis instead - this will panic")]
    pub fn with_ssa_and_global_analysis(
        _ast_builder: &'a OxcAstBuilder<'a>,
        _expression_context: ExpressionContext<'a>,
        _include_instruction_comments: bool,
        _include_ssa_comments: bool,
        _ssa_analysis: SSAAnalysis,
        _cfg: &crate::cfg::Cfg,
        _global_analysis: Arc<GlobalAnalysisResult>,
    ) -> Self {
        panic!("Legacy constructor is deprecated. Use new_with_analysis with FunctionAnalysis instead.");
    }

    /// Create a new block-to-statement converter with fallback variable mapping (legacy - DEPRECATED)
    #[deprecated(note = "Use new_with_analysis instead - this will panic")]
    pub fn with_fallback_mapping(
        _ast_builder: &'a OxcAstBuilder<'a>,
        _expression_context: ExpressionContext<'a>,
        _include_instruction_comments: bool,
        _variable_mapping: crate::ast::variables::VariableMapping,
    ) -> Self {
        panic!("Legacy constructor is deprecated. Use new_with_analysis with FunctionAnalysis instead.");
    }

    /// Convert multiple blocks from a CFG into a sequence of JavaScript statements
    /// This method handles proper block ordering and labeling
    pub fn convert_blocks_from_cfg(
        &mut self,
        cfg: &'a crate::cfg::Cfg<'a>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        self.convert_blocks_from_cfg_with_options(cfg, false)
    }

    /// Convert multiple blocks from a CFG into a sequence of JavaScript statements
    /// This method handles proper block ordering and labeling
    pub fn convert_blocks_from_cfg_with_options(
        &mut self,
        cfg: &'a crate::cfg::Cfg<'a>,
        skip_validation: bool,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        // TODO: Fix lifetime issue - can't store reference to locally created CFG
        // For now, nested control flow detection won't work optimally
        // self.set_full_cfg(cfg);

        let mut all_statements =
            ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        // Get blocks in structured execution order
        let block_order = cfg.structured_execution_order();
        let blocks_needing_labels = cfg.blocks_needing_labels();

        // Check if CFG has conditional analysis
        let conditional_analysis = cfg.analyze_conditional_chains();
        // Check if CFG has switch analysis
        let switch_analysis = cfg.analyze_switch_regions(&self.function_analysis.ssa);

        // Identify which switches are nested inside other switches
        let mut nested_switches = HashSet::new();
        if let Some(ref analysis) = switch_analysis {
            // Check which switches are nested
            for (i, outer_region) in analysis.regions.iter().enumerate() {
                for (j, inner_region) in analysis.regions.iter().enumerate() {
                    if i != j {
                        // Check if inner_region's dispatch is within any case body of outer_region
                        for (_, case_analysis) in &outer_region.case_analyses {
                            if case_analysis.blocks.contains(&inner_region.dispatch) {
                                nested_switches.insert(j);
                                break;
                            }
                        }
                    }
                }
            }
        }

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
                if let Some((region_idx, region)) =
                    self.find_switch_starting_at_indexed(*block_id, analysis)
                {
                    // Skip if this is a nested switch - it will be converted by its parent
                    if nested_switches.contains(&region_idx) {
                        // Check if this dispatch block has any unrendered instructions before the switch
                        let dispatch_block = &cfg.graph()[region.dispatch];
                        let dispatch_instructions = dispatch_block.instructions();

                        // Find the first comparison instruction
                        let first_comparison_idx = dispatch_instructions.iter().position(|instr| {
                            matches!(
                                &instr.instruction,
                                UnifiedInstruction::JStrictEqual { .. }
                                    | UnifiedInstruction::JStrictEqualLong { .. }
                                    | UnifiedInstruction::JStrictNotEqual { .. }
                                    | UnifiedInstruction::JStrictNotEqualLong { .. }
                            )
                        });

                        // Check if there are unrendered instructions before the comparison
                        let has_unrendered_before_comparison =
                            if let Some(idx) = first_comparison_idx {
                                dispatch_instructions
                                    .iter()
                                    .take(idx)
                                    .any(|instr| !self.is_instruction_rendered(instr))
                            } else {
                                false
                            };

                        if has_unrendered_before_comparison {
                            // Don't skip this block - let it be processed normally
                            // The switch converter will handle converting only the case body part
                        } else {
                            // Mark the infrastructure blocks as processed since they'll be handled by the parent switch
                            let nested_infrastructure =
                                super::switch_converter::get_switch_infrastructure_blocks(&[
                                    region.clone(),
                                ]);
                            processed_blocks.extend(&nested_infrastructure);
                            continue;
                        }
                    }
                    // Convert the entire switch region
                    // Initially mark only switch infrastructure blocks as processed
                    // to allow nested control flow detection in case bodies
                    let switch_infrastructure =
                        super::switch_converter::get_switch_infrastructure_blocks(
                            &[region.clone()],
                        );
                    processed_blocks.extend(&switch_infrastructure);
                    // Create the switch converter with expression context
                    let mut switch_converter = super::switch_converter::SwitchConverter::new(
                        self.instruction_converter.ast_builder(),
                    );
                    // Convert the switch to statements
                    match switch_converter.convert_switch_region(region, cfg, self) {
                        Ok(switch_statements) => {
                            all_statements.extend(switch_statements);

                            // Mark only the switch infrastructure blocks as processed
                            // Don't mark case bodies yet - they might contain nested switches
                            processed_blocks.extend(&switch_infrastructure);

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
                    // IMPORTANT: Check if this conditional chain is actually part of a switch
                    // This prevents switch comparison chains from being converted as if-else chains
                    let mut is_switch_comparison_chain = false;
                    if let Some(ref switch_analysis) = switch_analysis {
                        // Check if any of the chain's condition blocks are switch dispatch blocks
                        for switch_region in &switch_analysis.regions {
                            // Check if the chain starts at a switch dispatch block
                            if switch_region.dispatch == *block_id {
                                is_switch_comparison_chain = true;
                                break;
                            }
                            // Also check if any blocks in the chain are part of switch comparison logic
                            for branch in &chain.branches {
                                if switch_region.dispatch == branch.condition_block {
                                    is_switch_comparison_chain = true;
                                    break;
                                }
                            }
                            if is_switch_comparison_chain {
                                break;
                            }
                        }
                    }

                    if is_switch_comparison_chain {
                        log::debug!("Skipping conditional chain at block {:?} - it contains switch dispatch blocks", block_id);
                        // Skip conditional chain conversion - this will be handled as a switch
                        continue;
                    } else {
                        log::debug!("Processing conditional chain at block {:?} - no switch dispatch blocks found", block_id);
                    }

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

            // Check if all instructions in this block have been rendered
            let all_rendered = block
                .instructions()
                .iter()
                .all(|instr| self.is_instruction_rendered(instr));

            // Skip blocks where all instructions have been rendered
            // This avoids empty labels and stray variable declarations
            if all_rendered {
                processed_blocks.insert(*block_id);
                continue;
            }

            // Convert the block to statements first
            let block_statements = self.convert_block(block, *block_id, cfg.graph())?;

            // Only add label if this block needs one AND has statements to label
            // This avoids empty labels for blocks that were already processed
            if blocks_needing_labels.contains(block_id) && !block_statements.is_empty() {
                let label_stmt = self.create_block_label(*block_id)?;
                all_statements.push(label_stmt);
            }

            // Add the block statements
            for stmt in block_statements {
                all_statements.push(stmt);
            }

            // Mark this block as processed
            processed_blocks.insert(*block_id);
        }

        // Verify all blocks and instructions were rendered
        if !skip_validation {
            self.verify_all_rendered(cfg, &processed_blocks)?;
        } else {
            // Still run verification but only warn instead of failing
            if let Err(e) = self.verify_all_rendered(cfg, &processed_blocks) {
                eprintln!("WARNING: Block conversion validation failed (ignored due to --skip-validation): {}", e);
            }
        }

        // Copy undeclared variables from instruction converter
        for var_name in self
            .instruction_converter
            .get_undeclared_variables()
            .clone()
        {
            self.undeclared_variables.insert(var_name);
        }

        // Generate function-scoped variable declarations AFTER processing blocks
        // This allows us to filter out eliminated SSA values
        let function_decls = self.generate_function_declarations();
        if let Some(decl) = function_decls {
            // Insert at the beginning
            all_statements.insert(0, decl);
        }

        Ok(all_statements)
    }

    /// Verify that all blocks and instructions were rendered exactly once
    fn verify_all_rendered(
        &self,
        cfg: &'a crate::cfg::Cfg<'a>,
        processed_blocks: &HashSet<NodeIndex>,
    ) -> Result<(), BlockConversionError> {
        let mut unprocessed_blocks = Vec::new();

        // Check all blocks (except EXIT blocks)
        for node in cfg.graph().node_indices() {
            let block = &cfg.graph()[node];

            // Skip EXIT blocks
            if block.is_exit() {
                continue;
            }

            // Check if block was processed
            if !processed_blocks.contains(&node) {
                unprocessed_blocks.push(format!(
                    "Block {} (PC {}-{})",
                    node.index(),
                    block.start_pc(),
                    block.end_pc()
                ));
            }
        }

        if !unprocessed_blocks.is_empty() {
            return Err(BlockConversionError::InvalidBlock(format!(
                "{} blocks were not processed during decompilation: {}",
                unprocessed_blocks.len(),
                unprocessed_blocks.join(", ")
            )));
        }

        // Check all instructions in all processed blocks
        for node in processed_blocks {
            let block = &cfg.graph()[*node];
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

    /// Convert an analyzed case body region with nested control flow
    pub fn convert_analyzed_region(
        &mut self,
        analysis: &crate::cfg::analysis::CaseBodyAnalysis,
        cfg: &'a crate::cfg::Cfg<'a>,
        entry_block: NodeIndex,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        let mut statements = ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);
        let mut processed_blocks = HashSet::new();

        // Process nested switches first
        for nested_switch in &analysis.nested_switches {
            log::debug!(
                "Converting nested switch with dispatch block {}",
                nested_switch.dispatch.index()
            );
            let mut switch_converter = super::switch_converter::SwitchConverter::new(
                self.instruction_converter.ast_builder(),
            );
            match switch_converter.convert_switch_region(nested_switch, cfg, self) {
                Ok(switch_statements) => {
                    statements.extend(switch_statements);
                    // Mark the switch region as rendered to prevent double conversion
                    log::debug!("Marking nested switch region as rendered");
                    switch_converter.mark_switch_region_as_rendered(nested_switch, self, cfg);
                    // Mark all blocks in the switch as processed
                    processed_blocks.insert(nested_switch.dispatch);
                    for case in &nested_switch.cases {
                        processed_blocks.insert(case.case_head);
                    }
                    if let Some(default) = nested_switch.default_head {
                        processed_blocks.insert(default);
                    }
                }
                Err(e) => {
                    log::warn!("Failed to convert nested switch: {}", e);
                }
            }
        }

        // Process loops
        for loop_info in &analysis.loops.loops {
            if !processed_blocks.contains(&loop_info.primary_header()) {
                // TODO: Implement loop conversion
                // For now, just mark as processed
                for &block in &loop_info.body_nodes {
                    processed_blocks.insert(block);
                }
            }
        }

        // Process conditional chains
        for chain in &analysis.conditionals.chains {
            let entry = chain
                .branches
                .first()
                .map(|b| b.condition_block)
                .unwrap_or(entry_block);

            if !processed_blocks.contains(&entry) {
                // Check if any of the chain blocks are already rendered
                let any_rendered = chain.branches.iter().any(|branch| {
                    let block = &cfg.graph()[branch.condition_block];
                    block
                        .instructions()
                        .iter()
                        .all(|instr| self.is_instruction_rendered(instr))
                });

                if any_rendered {
                    log::debug!(
                        "Skipping conditional chain starting at {} - already rendered",
                        entry.index()
                    );
                    continue;
                }

                let mut conditional_converter =
                    super::conditional_converter::ConditionalConverter::new(
                        self.instruction_converter.ast_builder(),
                    );
                match conditional_converter.convert_chain(chain, cfg, self) {
                    Ok(cond_statements) => {
                        statements.extend(cond_statements);
                        // Mark all blocks in the chain as processed
                        for branch in &chain.branches {
                            processed_blocks.insert(branch.condition_block);
                            for &block in &branch.branch_blocks {
                                processed_blocks.insert(block);
                            }
                        }
                    }
                    Err(e) => {
                        log::warn!("Failed to convert conditional chain: {}", e);
                    }
                }
            }
        }

        // Process remaining blocks in order
        let mut remaining_blocks: Vec<_> = analysis
            .blocks
            .iter()
            .filter(|&&b| !processed_blocks.contains(&b))
            .copied()
            .collect();
        remaining_blocks.sort_by_key(|&b| b.index());

        for &block_id in &remaining_blocks {
            if let Some(block) = cfg.graph().node_weight(block_id) {
                match self.convert_block(block, block_id, cfg.graph()) {
                    Ok(block_stmts) => statements.extend(block_stmts),
                    Err(e) => {
                        log::warn!("Failed to convert block {}: {}", block_id.index(), e);
                    }
                }
            }
        }

        Ok(statements)
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

    /// Find a switch region that starts at the given block, returning its index
    fn find_switch_starting_at_indexed<'b>(
        &self,
        block_id: NodeIndex,
        analysis: &'b crate::cfg::analysis::SwitchAnalysis,
    ) -> Option<(usize, &'b crate::cfg::analysis::SwitchRegion)> {
        // Check if this block is the dispatch block of any switch region
        for (idx, region) in analysis.regions.iter().enumerate() {
            if region.dispatch == block_id {
                return Some((idx, region));
            }
        }
        None
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

    /// Convert a block with nested control flow detection (switches before conditionals)
    pub fn convert_block_with_nested_control_flow(
        &mut self,
        block: &Block,
        block_id: NodeIndex,
        cfg: &'a crate::cfg::Cfg<'a>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        let mut statements = ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        // First check if this block is the start of a switch region
        let ssa = &self.function_analysis.ssa;
        if let Some(switch_analysis) = cfg.analyze_switch_regions(ssa) {
            for region in &switch_analysis.regions {
                if region.dispatch == block_id {
                    // This is a switch dispatch block - convert it as a switch
                    let mut switch_converter = super::switch_converter::SwitchConverter::new(
                        self.instruction_converter.ast_builder(),
                    );
                    match switch_converter.convert_switch_region(region, cfg, self) {
                        Ok(switch_statements) => {
                            statements.extend(switch_statements);
                            return Ok(statements);
                        }
                        Err(e) => {
                            log::warn!(
                                "Failed to convert switch at block {}: {}",
                                block_id.index(),
                                e
                            );
                            // Fall through to regular conversion
                        }
                    }
                }
            }
        }

        // Then check for conditional chains
        if let Some(conditional_analysis) = cfg.analyze_conditional_chains() {
            if let Some(chain) = self.find_chain_starting_at(block_id, &conditional_analysis) {
                // Convert as conditional chain
                let mut conditional_converter =
                    super::conditional_converter::ConditionalConverter::new(
                        self.instruction_converter.ast_builder(),
                    );
                match conditional_converter.convert_chain(chain, cfg, self) {
                    Ok(chain_statements) => {
                        statements.extend(chain_statements);
                        return Ok(statements);
                    }
                    Err(e) => {
                        return Err(BlockConversionError::InvalidBlock(format!(
                            "Failed to convert conditional chain: {:?}",
                            e
                        )));
                    }
                }
            }
        }

        // Otherwise, just convert the block normally
        self.convert_block(block, block_id, cfg.graph())
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
                .function_analysis
                .ssa
                .phi_variable_declarations
                .get(&block_id)
                .cloned();

            if let Some(phi_declarations) = phi_declarations {
                for phi_decl in &phi_declarations {
                    // Create a variable declaration for this phi register
                    if let Some(var_decl) = self.create_phi_variable_declaration(phi_decl)? {
                        statements.push(var_decl);
                    }
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
                    let info = self.format_ssa_info(
                        block_id,
                        instruction.instruction_index,
                        &self.function_analysis.ssa,
                    );
                    info
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
    pub fn ssa_analysis(&self) -> &SSAAnalysis {
        &self.function_analysis.ssa
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

    /// Set whether to decompile nested function bodies
    pub fn set_decompile_nested(&mut self, decompile_nested: bool) {
        self.instruction_converter
            .set_decompile_nested(decompile_nested);
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
    ) -> Result<Option<Statement<'a>>, BlockConversionError> {
        // Get the variable name from the SSA value through the variable mapper
        // The phi_decl.ssa_value should already be mapped to the correct coalesced variable name
        let var_name = if let Some(mapping) = self
            .instruction_converter
            .register_manager_mut()
            .variable_mapping()
        {
            // Look up the variable name for this SSA value
            // This should give us the coalesced name that all versions of this variable use
            let name = mapping
                .ssa_to_var
                .get(&phi_decl.ssa_value)
                .cloned()
                .unwrap_or_else(|| format!("var{}", phi_decl.register));

            // Check if this variable is already declared at function scope
            if mapping.function_scope_vars.contains(&name) {
                // Skip PHI declaration for function-scoped variables
                return Ok(None);
            }

            name
        } else {
            // No variable mapping available, use default naming
            format!("var{}", phi_decl.register)
        };

        // Create a declaration without initialization
        // We use 'let' because phi variables are typically reassigned
        let mut stmt = self
            .instruction_converter
            .create_variable_declaration(
                &var_name,
                None,
                oxc_ast::ast::VariableDeclarationKind::Let,
            )
            .map_err(BlockConversionError::StatementConversion)?;

        // Add a comment explaining why this PHI declaration is needed
        if let Some(ref mut comment_manager) = self.comment_manager {
            let comment = format!(
                "PHI declaration for register {} ({}): declared at block {:?}",
                phi_decl.register, var_name, phi_decl.declaration_block
            );
            comment_manager.add_comment(
                &mut stmt,
                comment,
                CommentKind::Line,
                CommentPosition::Leading,
            );
        }

        Ok(Some(stmt))
    }

    /// Report that specific uses of an SSA value have been consumed
    pub fn mark_ssa_uses_consumed(
        &mut self,
        ssa_value: &SSAValue,
        uses: &[crate::cfg::ssa::types::RegisterUse],
    ) {
        self.ssa_usage_tracker.mark_uses_consumed(ssa_value, uses);
    }

    /// Check if an SSA value is fully eliminated (all uses consumed)
    pub fn is_ssa_value_eliminated(&self, ssa_value: &SSAValue) -> bool {
        self.ssa_usage_tracker.is_fully_eliminated(ssa_value)
    }

    /// Get a reference to the SSA usage tracker
    pub fn ssa_usage_tracker(&self) -> &SSAUsageTracker<'a> {
        &self.ssa_usage_tracker
    }

    /// Get a mutable reference to the SSA usage tracker
    pub fn ssa_usage_tracker_mut(&mut self) -> &mut SSAUsageTracker<'a> {
        &mut self.ssa_usage_tracker
    }

    /// Get the full CFG if available
    pub fn get_full_cfg(&self) -> Option<&'a crate::cfg::Cfg<'a>> {
        Some(&self.function_analysis.cfg)
    }

    /// Get the HBC file reference if available
    pub fn hbc_file(&self) -> Option<&'a HbcFile<'a>> {
        Some(self.function_analysis.hbc_file)
    }

    /// Get the function analysis
    pub fn function_analysis(&self) -> &'a crate::analysis::FunctionAnalysis<'a> {
        self.function_analysis
    }

    /// Track an undeclared variable
    pub fn track_undeclared_variable(&mut self, var_name: String) {
        self.undeclared_variables.insert(var_name);
    }

    /// Generate function-scoped variable declarations
    fn generate_function_declarations(&self) -> Option<Statement<'a>> {
        let mut all_vars = HashSet::new();

        // Add undeclared variables first
        for var_name in &self.undeclared_variables {
            all_vars.insert(var_name.clone());
        }

        // Then add function scope variables from mapping
        if let Some(mapping) = self.instruction_converter.get_variable_mapping() {
            let function_vars: Vec<_> = mapping
                .function_scope_vars
                .iter()
                .filter(|var_name| {
                    // Check if all SSA values for this variable have been eliminated
                    let var_ssa_values: Vec<_> = mapping
                        .ssa_to_var
                        .iter()
                        .filter(|(_, var)| var == var_name)
                        .map(|(ssa, _)| ssa)
                        .collect();

                    let all_eliminated = !var_ssa_values.is_empty()
                        && var_ssa_values
                            .iter()
                            .all(|ssa| self.is_ssa_value_eliminated(ssa));

                    if all_eliminated {
                        log::debug!(
                            "Variable {} has all SSA values eliminated: {:?}",
                            var_name,
                            var_ssa_values
                        );
                    } else {
                        log::debug!(
                            "Variable {} has non-eliminated SSA values: {:?} (eliminated: {:?})",
                            var_name,
                            var_ssa_values
                                .iter()
                                .filter(|ssa| !self.is_ssa_value_eliminated(ssa))
                                .collect::<Vec<_>>(),
                            var_ssa_values
                                .iter()
                                .filter(|ssa| self.is_ssa_value_eliminated(ssa))
                                .collect::<Vec<_>>()
                        );
                    }

                    // Keep the variable if:
                    // 1. At least one SSA value is not eliminated
                    // 2. OR if there are no SSA values for this variable (shouldn't happen)
                    // 3. OR if it's in the undeclared variables set (was used but not declared)
                    // 3. OR if it's in the undeclared variables set (used but not declared)
                    !all_eliminated || self.undeclared_variables.contains(*var_name)
                })
                .collect();

            // Add filtered function vars to all_vars
            for var in function_vars {
                all_vars.insert(var.clone());
            }
        }

        // Convert to sorted vector for deterministic output
        let mut all_vars_vec: Vec<_> = all_vars.into_iter().collect();
        all_vars_vec.sort();

        if !all_vars_vec.is_empty() {
            // Create a single let declaration with all function-scoped variables
            let mut declarators =
                ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

            for var_name in all_vars_vec {
                let id = self.instruction_converter.ast_builder().binding_identifier(
                    Span::default(),
                    self.instruction_converter.ast_builder().atom(&var_name),
                );
                let pattern = self.instruction_converter.ast_builder().binding_pattern(
                    BindingPatternKind::BindingIdentifier(
                        self.instruction_converter.ast_builder().alloc(id),
                    ),
                    None::<oxc_ast::ast::TSTypeAnnotation>,
                    false,
                );
                let declarator = self
                    .instruction_converter
                    .ast_builder()
                    .variable_declarator(
                        Span::default(),
                        VariableDeclarationKind::Let,
                        pattern,
                        None, // No initializer
                        false,
                    );
                declarators.push(declarator);
            }

            let var_decl = self
                .instruction_converter
                .ast_builder()
                .variable_declaration(
                    Span::default(),
                    VariableDeclarationKind::Let,
                    declarators,
                    false,
                );

            return Some(Statement::VariableDeclaration(
                self.instruction_converter.ast_builder().alloc(var_decl),
            ));
        }
        None
    }
}
