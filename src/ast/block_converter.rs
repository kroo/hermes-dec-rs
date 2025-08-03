//! Block-to-statement conversion system
//!
//! This module provides the BlockToStatementConverter that transforms CFG basic blocks
//! into sequences of JavaScript statements, building on instruction-to-expression conversion.

use super::{
    expression_context::ExpressionContext,
    instruction_to_statement_converter::{
        InstructionResult, InstructionToStatementConverter, JumpCondition, StatementConversionError,
    },
};
use crate::{
    cfg::{block::Block, EdgeKind},
    generated::unified_instructions::UnifiedInstruction,
    hbc::function_table::HbcFunctionInstruction,
};
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{ast::Statement, AstBuilder as OxcAstBuilder};
use petgraph::{graph::NodeIndex, visit::EdgeRef, Graph};
use std::collections::{HashMap, HashSet};

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
        }
    }

    /// Convert multiple blocks from a CFG into a sequence of JavaScript statements
    /// This method handles proper block ordering and labeling
    pub fn convert_blocks_from_cfg(
        &mut self,
        cfg: &crate::cfg::Cfg,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        let mut all_statements =
            ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        // Get blocks in structured execution order
        let block_order = cfg.structured_execution_order();
        let blocks_needing_labels = cfg.blocks_needing_labels();

        for (order_idx, block_id) in block_order.iter().enumerate() {
            let block = &cfg.graph()[*block_id];

            // Skip exit blocks
            if block.is_exit() {
                continue;
            }

            // Add block information comment
            let block_comment = self.create_block_info_comment(*block_id, block, cfg)?;
            all_statements.push(block_comment);

            // Add incoming edge information comments
            if order_idx > 0 {
                let edge_comments = self.create_incoming_edge_comments(*block_id, cfg)?;
                for comment in edge_comments {
                    all_statements.push(comment);
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
        }

        Ok(all_statements)
    }

    /// Convert a basic block into a sequence of JavaScript statements
    pub fn convert_block(
        &mut self,
        block: &Block,
        _block_id: NodeIndex,
        _cfg: &Graph<Block, EdgeKind>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        // Reset scope for new block
        self.scope_manager.reset();

        // Pre-analyze block for variable requirements
        self.analyze_block_variables(block)?;

        // Convert instructions to statements
        let mut statements = ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);

        for (pc_offset, instruction) in block.instructions().iter().enumerate() {
            let pc = block.start_pc() + pc_offset as u32;
            // Process instruction

            // Add instruction comment if enabled
            if self.include_instruction_comments {
                let instruction_comment = self.create_instruction_comment(pc, instruction)?;
                statements.push(instruction_comment);
            }

            let instruction_statements = self.convert_instruction_to_statements(instruction, pc)?;
            for stmt in instruction_statements {
                statements.push(stmt);
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
        pc: u32,
    ) -> Result<Vec<Statement<'a>>, BlockConversionError> {
        let mut statements = Vec::new();

        // Set current PC in the instruction converter
        self.instruction_converter.set_current_pc(pc);

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
            InstructionResult::JumpCondition(jump_info) => {
                // Jump instructions don't generate statements directly
                // They provide condition info for the block converter to handle control flow
                // For now, we'll create a comment about the jump
                let jump_comment = self.create_jump_comment(&jump_info)?;
                statements.push(jump_comment);
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
                    .track_usage(*operand_0, pc);
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

    // ===== Private Helper Methods =====

    /// Get property name from the string table using property index
    // Dead methods removed: get_property_name, create_property_assignment_statement,
    // create_computed_property_assignment_statement, create_identifier_expression
    // These are now handled by InstructionToStatementConverter

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

    /// Create a comment statement with block information
    fn create_block_info_comment(
        &self,
        block_id: NodeIndex,
        block: &Block,
        _cfg: &crate::cfg::Cfg,
    ) -> Result<Statement<'a>, BlockConversionError> {
        let span = oxc_span::Span::default();

        // Gather block information
        let block_info = format!(
            "/* Block {}: PC {}..{}, {} instructions */",
            block_id.index(),
            block.start_pc(),
            block.end_pc(),
            block.instructions().len()
        );

        // Create as a string literal expression statement that will be rendered as a comment
        let comment_atom = self
            .instruction_converter
            .ast_builder()
            .allocator
            .alloc_str(&block_info);
        let comment_expr = self
            .instruction_converter
            .ast_builder()
            .expression_string_literal(span, comment_atom, None);

        // Mark this as a comment by wrapping it in a special way
        // We'll use void to indicate this should be rendered as a comment
        let void_expr = self.instruction_converter.ast_builder().expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Void,
            comment_expr,
        );

        Ok(self
            .instruction_converter
            .ast_builder()
            .statement_expression(span, void_expr))
    }

    /// Create comment statements for incoming edges to a block
    fn create_incoming_edge_comments(
        &self,
        block_id: NodeIndex,
        cfg: &crate::cfg::Cfg,
    ) -> Result<Vec<Statement<'a>>, BlockConversionError> {
        let mut comments = Vec::new();
        let span = oxc_span::Span::default();

        // Get incoming edges
        let incoming_edges: Vec<_> = cfg
            .graph()
            .edges_directed(block_id, petgraph::Direction::Incoming)
            .collect();

        if !incoming_edges.is_empty() {
            for edge in incoming_edges {
                let edge_info = format!(
                    "/* Edge from Block {} -> Block {} ({:?}) */",
                    edge.source().index(),
                    edge.target().index(),
                    edge.weight()
                );

                let comment_atom = self
                    .instruction_converter
                    .ast_builder()
                    .allocator
                    .alloc_str(&edge_info);
                let comment_expr = self
                    .instruction_converter
                    .ast_builder()
                    .expression_string_literal(span, comment_atom, None);

                let void_expr = self.instruction_converter.ast_builder().expression_unary(
                    span,
                    oxc_ast::ast::UnaryOperator::Void,
                    comment_expr,
                );

                comments.push(
                    self.instruction_converter
                        .ast_builder()
                        .statement_expression(span, void_expr),
                );
            }
        }

        Ok(comments)
    }

    /// Create a comment statement for an individual instruction
    fn create_instruction_comment(
        &self,
        pc: u32,
        instruction: &HbcFunctionInstruction,
    ) -> Result<Statement<'a>, BlockConversionError> {
        let span = oxc_span::Span::default();

        // Format the instruction information
        let instruction_info = if let Some(hbc_file) = self
            .instruction_converter
            .get_expression_context()
            .hbc_file()
        {
            // Use the formatted instruction display
            format!(
                "/* PC {}: {} */",
                pc,
                instruction.format_instruction(hbc_file)
            )
        } else {
            // Fallback to debug formatting if no HBC file access
            format!("/* PC {}: {:?} */", pc, instruction.instruction)
        };

        let comment_atom = self
            .instruction_converter
            .ast_builder()
            .allocator
            .alloc_str(&instruction_info);
        let comment_expr = self
            .instruction_converter
            .ast_builder()
            .expression_string_literal(span, comment_atom, None);

        let void_expr = self.instruction_converter.ast_builder().expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Void,
            comment_expr,
        );

        Ok(self
            .instruction_converter
            .ast_builder()
            .statement_expression(span, void_expr))
    }

    /// Create a comment statement for jump conditions
    fn create_jump_comment(
        &self,
        jump_info: &JumpCondition,
    ) -> Result<Statement<'a>, BlockConversionError> {
        let span = oxc_span::Span::default();

        // Format the jump information
        let jump_info_str = if let Some(_condition_expr) = &jump_info.condition_expression {
            format!(
                "/* Jump: {:?} condition with expression to offset {:?} */",
                jump_info.jump_type, jump_info.target_offset
            )
        } else {
            format!(
                "/* Jump: {:?} to offset {:?} */",
                jump_info.jump_type, jump_info.target_offset
            )
        };

        let comment_atom = self
            .instruction_converter
            .ast_builder()
            .allocator
            .alloc_str(&jump_info_str);
        let comment_expr = self
            .instruction_converter
            .ast_builder()
            .expression_string_literal(span, comment_atom, None);

        let void_expr = self.instruction_converter.ast_builder().expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Void,
            comment_expr,
        );

        Ok(self
            .instruction_converter
            .ast_builder()
            .statement_expression(span, void_expr))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::ExpressionContext, cfg::Block, generated::unified_instructions::UnifiedInstruction,
        hbc::function_table::HbcFunctionInstruction,
    };
    use oxc_allocator::Allocator;
    use petgraph::Graph;

    fn create_test_instruction(instruction: UnifiedInstruction) -> HbcFunctionInstruction {
        HbcFunctionInstruction {
            instruction,
            offset: 0,
            function_index: 0,
            instruction_index: 0,
        }
    }

    #[test]
    fn test_block_converter_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();

        let converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Verify initial state
        let stats = converter.get_stats();
        assert_eq!(stats.blocks_processed, 0);
        assert_eq!(stats.instructions_converted, 0);
        assert_eq!(stats.statements_generated, 0);
    }

    #[test]
    fn test_target_register_extraction() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Test various instruction types
        assert_eq!(
            converter.get_target_register(&UnifiedInstruction::Add {
                operand_0: 5,
                operand_1: 1,
                operand_2: 2
            }),
            Some(5)
        );

        assert_eq!(
            converter.get_target_register(&UnifiedInstruction::LoadConstTrue { operand_0: 3 }),
            Some(3)
        );

        assert_eq!(
            converter.get_target_register(&UnifiedInstruction::Mov {
                operand_0: 10,
                operand_1: 5
            }),
            Some(10)
        );

        // Instructions without target registers
        assert_eq!(
            converter.get_target_register(&UnifiedInstruction::Ret { operand_0: 1 }),
            None
        );

        assert_eq!(
            converter.get_target_register(&UnifiedInstruction::PutByVal {
                operand_0: 1,
                operand_1: 2,
                operand_2: 3
            }),
            None
        );
    }

    #[test]
    fn test_convert_simple_block() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a simple block with load constant instruction
        let instructions = vec![create_test_instruction(UnifiedInstruction::LoadConstTrue {
            operand_0: 1,
        })];

        let block = Block::new(0, instructions);

        // Create empty graph for the test
        let mut cfg: Graph<Block, EdgeKind> = Graph::new();
        let block_id = cfg.add_node(block.clone());

        // Convert the block
        let result = converter.convert_block(&block, block_id, &cfg);
        assert!(result.is_ok(), "Block conversion should succeed");

        let statements = result.unwrap();
        assert_eq!(statements.len(), 1, "Should generate one statement");

        // Verify statistics
        let stats = converter.get_stats();
        assert_eq!(stats.blocks_processed, 1);
        assert_eq!(stats.instructions_converted, 1);
        assert_eq!(stats.statements_generated, 1);
    }

    #[test]
    fn test_convert_arithmetic_block() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a block with arithmetic instructions
        let instructions = vec![
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            }),
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 2,
                operand_1: 10,
            }),
            create_test_instruction(UnifiedInstruction::Add {
                operand_0: 3,
                operand_1: 1,
                operand_2: 2,
            }),
        ];

        let block = Block::new(0, instructions);

        // Create empty graph for the test
        let mut cfg: Graph<Block, EdgeKind> = Graph::new();
        let block_id = cfg.add_node(block.clone());

        // Convert the block
        let result = converter.convert_block(&block, block_id, &cfg);
        assert!(result.is_ok(), "Block conversion should succeed");

        let statements = result.unwrap();
        assert_eq!(statements.len(), 3, "Should generate three statements");

        // Verify statistics
        let stats = converter.get_stats();
        assert_eq!(stats.blocks_processed, 1);
        assert_eq!(stats.instructions_converted, 3);
        assert_eq!(stats.statements_generated, 3);
    }

    #[test]
    fn test_convert_return_block() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a block with return instruction
        let instructions = vec![
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            }),
            create_test_instruction(UnifiedInstruction::Ret { operand_0: 1 }),
        ];

        let block = Block::new(0, instructions);

        // Create empty graph for the test
        let mut cfg: Graph<Block, EdgeKind> = Graph::new();
        let block_id = cfg.add_node(block.clone());

        // Convert the block
        let result = converter.convert_block(&block, block_id, &cfg);
        if let Err(e) = &result {
            println!("Error: {:?}", e);
        }
        assert!(
            result.is_ok(),
            "Block conversion should succeed: {:?}",
            result.as_ref().err()
        );

        let statements = result.unwrap();
        assert_eq!(statements.len(), 2, "Should generate two statements");

        // Check that the last statement is a return statement
        if let Some(last_stmt) = statements.last() {
            assert!(
                matches!(last_stmt, oxc_ast::ast::Statement::ReturnStatement(_)),
                "Last statement should be a return statement"
            );
        }
    }

    #[test]
    fn test_convert_assignment_sequence() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a block with move operations (assignments)
        let instructions = vec![
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 5,
            }),
            create_test_instruction(UnifiedInstruction::Mov {
                operand_0: 2,
                operand_1: 1,
            }),
            create_test_instruction(UnifiedInstruction::Mov {
                operand_0: 3,
                operand_1: 2,
            }),
        ];

        let block = Block::new(0, instructions);

        // Create empty graph for the test
        let mut cfg: Graph<Block, EdgeKind> = Graph::new();
        let block_id = cfg.add_node(block.clone());

        // Convert the block
        let result = converter.convert_block(&block, block_id, &cfg);
        assert!(result.is_ok(), "Block conversion should succeed");

        let statements = result.unwrap();
        assert_eq!(statements.len(), 3, "Should generate three statements");

        // First should be a variable declaration, rest should be assignments
        // (Note: exact behavior depends on scope manager logic)
        let stats = converter.get_stats();
        assert!(
            stats.variables_declared > 0,
            "Should have declared some variables"
        );
    }

    #[test]
    fn test_side_effect_instructions() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a block with call instruction (has side effects)
        let instructions = vec![create_test_instruction(UnifiedInstruction::Call {
            operand_0: 3, // result register
            operand_1: 1, // function register
            operand_2: 0, // arg count
        })];

        let block = Block::new(0, instructions);

        // Create empty graph for the test
        let mut cfg: Graph<Block, EdgeKind> = Graph::new();
        let block_id = cfg.add_node(block.clone());

        // Convert the block
        let result = converter.convert_block(&block, block_id, &cfg);
        assert!(result.is_ok(), "Block conversion should succeed");

        let statements = result.unwrap();
        assert_eq!(statements.len(), 1, "Should generate one statement");

        // Should generate a statement for the call
        let stats = converter.get_stats();
        assert_eq!(stats.statements_generated, 1);
    }

    #[test]
    fn test_scope_manager_basic_functionality() {
        let mut scope_manager = BlockScopeManager::new();

        // Test declaration tracking
        assert!(!scope_manager.is_declared(5));
        scope_manager.mark_declared(5);
        assert!(scope_manager.is_declared(5));

        // Test declaration requirements
        assert!(!scope_manager.requires_declaration(10));
        scope_manager.mark_requires_declaration(10);
        assert!(scope_manager.requires_declaration(10));

        // Test block locals
        scope_manager.set_block_local(1, "var1".to_string());
        assert_eq!(scope_manager.get_block_local(1), Some(&"var1".to_string()));
        assert_eq!(scope_manager.get_block_local(2), None);
    }

    #[test]
    fn test_scope_manager_reset() {
        let mut scope_manager = BlockScopeManager::new();

        scope_manager.mark_declared(5);
        scope_manager.mark_requires_declaration(10);
        scope_manager.set_block_local(1, "var1".to_string());

        scope_manager.reset();

        assert!(!scope_manager.is_declared(5));
        assert!(!scope_manager.requires_declaration(10));
        assert_eq!(scope_manager.get_block_local(1), None);
    }

    #[test]
    fn test_conversion_stats_tracking() {
        let mut stats = BlockConversionStats::default();

        assert_eq!(stats.blocks_processed, 0);
        assert_eq!(stats.instructions_converted, 0);
        assert_eq!(stats.statements_generated, 0);

        // Stats should be updateable
        stats.blocks_processed += 1;
        stats.instructions_converted += 5;
        stats.statements_generated += 5;

        assert_eq!(stats.blocks_processed, 1);
        assert_eq!(stats.instructions_converted, 5);
        assert_eq!(stats.statements_generated, 5);
    }

    #[test]
    fn test_block_variable_analysis() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();
        let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

        // Create a block with various target registers
        let instructions = vec![
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            }),
            create_test_instruction(UnifiedInstruction::Add {
                operand_0: 2,
                operand_1: 1,
                operand_2: 1,
            }),
            create_test_instruction(UnifiedInstruction::Mov {
                operand_0: 3,
                operand_1: 2,
            }),
        ];

        let block = Block::new(0, instructions);

        // Test variable analysis
        let result = converter.analyze_block_variables(&block);
        assert!(result.is_ok(), "Variable analysis should succeed");

        // Check that target registers are marked for declaration
        assert!(converter.scope_manager.requires_declaration(1));
        assert!(converter.scope_manager.requires_declaration(2));
        assert!(converter.scope_manager.requires_declaration(3));
    }
}
