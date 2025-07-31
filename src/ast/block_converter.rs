//! Block-to-statement conversion system
//!
//! This module provides the BlockToStatementConverter that transforms CFG basic blocks
//! into sequences of JavaScript statements, building on instruction-to-expression conversion.

use super::{
    expression_context::ExpressionContext,
    instruction_converter::{ConversionError, InstructionToExpressionConverter},
    statement_builder::{StatementBuilder, StatementBuilderError},
};
use crate::{
    cfg::{block::Block, EdgeKind},
    generated::unified_instructions::UnifiedInstruction,
    hbc::function_table::HbcFunctionInstruction,
};
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{Statement, VariableDeclarationKind},
    AstBuilder as OxcAstBuilder,
};
use petgraph::{graph::NodeIndex, Graph};
use std::collections::{HashMap, HashSet};

/// Error types for block-to-statement conversion
#[derive(Debug, thiserror::Error)]
pub enum BlockConversionError {
    #[error("Instruction conversion error: {0}")]
    InstructionConversion(#[from] ConversionError),
    #[error("Statement builder error: {0}")]
    StatementBuilder(#[from] StatementBuilderError),
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
    /// Instruction-to-expression converter
    expression_converter: InstructionToExpressionConverter<'a>,
    /// Statement builder for creating OXC statements
    statement_builder: StatementBuilder<'a>,
    /// Variable scoping manager
    scope_manager: BlockScopeManager,
    /// Conversion statistics
    stats: BlockConversionStats,
}

impl<'a> BlockToStatementConverter<'a> {
    /// Create a new block-to-statement converter
    pub fn new(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: ExpressionContext<'a>,
    ) -> Self {
        let expression_converter =
            InstructionToExpressionConverter::new(ast_builder, expression_context);
        let statement_builder = StatementBuilder::new(ast_builder);

        Self {
            expression_converter,
            statement_builder,
            scope_manager: BlockScopeManager::new(),
            stats: BlockConversionStats::default(),
        }
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
        let mut statements = ArenaVec::new_in(self.statement_builder.ast_builder.allocator);

        for (pc_offset, instruction) in block.instructions().iter().enumerate() {
            let pc = block.start_pc() + pc_offset as u32;
            self.expression_converter.set_current_pc(pc);

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
        _pc: u32,
    ) -> Result<Vec<Statement<'a>>, BlockConversionError> {
        let mut statements = Vec::new();

        // Convert instruction to expression first
        let expression = self
            .expression_converter
            .convert_instruction(&instruction.instruction)?;

        // Determine how to convert the expression to statement(s)
        match &instruction.instruction {
            // Return instructions become return statements
            UnifiedInstruction::Ret { operand_0 } => {
                let return_value = if *operand_0 != 0 {
                    Some(expression)
                } else {
                    None
                };
                statements.push(self.statement_builder.create_return_statement(return_value));
            }

            // Throw instructions become throw statements
            UnifiedInstruction::Throw { .. } => {
                statements.push(self.statement_builder.create_throw_statement(expression)?);
            }

            // Most instructions become either variable declarations or assignments
            _ => {
                if let Some(target_register) = self.get_target_register(&instruction.instruction) {
                    if self.scope_manager.requires_declaration(target_register)
                        && !self.scope_manager.is_declared(target_register)
                    {
                        // Create variable declaration
                        let var_name = self
                            .expression_converter
                            .register_manager_mut()
                            .get_variable_name(target_register);
                        statements.push(self.statement_builder.create_variable_declaration(
                            &var_name,
                            Some(expression),
                            VariableDeclarationKind::Let,
                        )?);
                        self.scope_manager.mark_declared(target_register);
                        self.stats.variables_declared += 1;
                    } else {
                        // Create assignment statement
                        let var_name = self
                            .expression_converter
                            .register_manager_mut()
                            .get_variable_name(target_register);
                        statements.push(
                            self.statement_builder
                                .create_assignment_statement(&var_name, expression)?,
                        );
                    }
                } else if self.statement_builder.has_side_effects(&expression) {
                    // Create expression statement for side effects
                    statements.push(self.statement_builder.create_expression_statement(expression));
                }
                // If no target register and no side effects, this is likely dead code
                // TODO: Track dead code elimination statistics
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
                self.scope_manager.mark_requires_declaration(target_register);
            }
        }
        Ok(())
    }

    /// Extract the target register from an instruction (if any)
    fn get_target_register(&self, instruction: &UnifiedInstruction) -> Option<u8> {
        match instruction {
            // Most instructions use operand_0 as destination
            UnifiedInstruction::Add { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Sub { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mul { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Div { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mod { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstTrue { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstFalse { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstNull { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstUndefined { operand_0 } => Some(*operand_0),
            UnifiedInstruction::LoadConstUInt8 { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstInt { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstString { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::LoadConstBigInt { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Mov { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetByVal { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::GetById { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Call { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Negate { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Not { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Inc { operand_0, .. } => Some(*operand_0),
            UnifiedInstruction::Dec { operand_0, .. } => Some(*operand_0),

            // Instructions without target registers
            UnifiedInstruction::Ret { .. } => None,
            UnifiedInstruction::Throw { .. } => None,
            UnifiedInstruction::PutByVal { .. } => None,
            UnifiedInstruction::PutById { .. } => None,

            // For any unhandled instructions, assume no target register
            _ => None,
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

    /// Get access to the underlying expression converter
    pub fn expression_converter(&self) -> &InstructionToExpressionConverter<'a> {
        &self.expression_converter
    }

    /// Get mutable access to the underlying expression converter
    pub fn expression_converter_mut(&mut self) -> &mut InstructionToExpressionConverter<'a> {
        &mut self.expression_converter
    }

    /// Get access to the statement builder
    pub fn statement_builder(&self) -> &StatementBuilder<'a> {
        &self.statement_builder
    }

    /// Get access to the scope manager
    pub fn scope_manager(&self) -> &BlockScopeManager {
        &self.scope_manager
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ExpressionContext;
    use oxc_allocator::Allocator;

    #[test]
    fn test_block_converter_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let context = ExpressionContext::new();

        let converter = BlockToStatementConverter::new(&ast_builder, context);

        // Verify initial state
        let stats = converter.get_stats();
        assert_eq!(stats.blocks_processed, 0);
        assert_eq!(stats.instructions_converted, 0);
        assert_eq!(stats.statements_generated, 0);
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
}