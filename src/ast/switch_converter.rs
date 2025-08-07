//! Switch statement conversion from CFG switch regions
//!
//! This module converts switch regions detected in the CFG into JavaScript switch statements.

use crate::cfg::{analysis::SwitchRegion, Cfg};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::tables::switch_table::SwitchTable;
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{Expression, Statement, SwitchCase},
    AstBuilder as OxcAstBuilder,
};
use oxc_span::Span;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

/// Error types for switch conversion
#[derive(Debug, thiserror::Error)]
pub enum SwitchConversionError {
    #[error("Invalid switch region: {0}")]
    InvalidRegion(String),
    #[error("Missing switch instruction in dispatch block")]
    MissingSwitchInstruction,
    #[error("Failed to build switch expression: {0}")]
    ExpressionBuildError(String),
    #[error("Failed to convert case blocks: {0}")]
    CaseConversionError(String),
}

/// Information about a switch case
#[derive(Debug, Clone)]
pub struct SwitchCaseInfo {
    /// The case value (for now, only integer values)
    pub value: i32,
    /// The blocks that make up this case
    pub blocks: Vec<NodeIndex>,
    /// Whether this case has an explicit break
    pub has_break: bool,
    /// Whether this case falls through to the next
    pub falls_through: bool,
}

/// Converts switch regions to JavaScript switch statements
pub struct SwitchConverter<'a> {
    ast_builder: &'a OxcAstBuilder<'a>,
}

/// Type of switch region
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchType {
    /// Dense switch with SwitchImm instruction and switch table
    Dense,
    /// Sparse switch detected from equality comparison chain
    Sparse,
}

impl<'a> SwitchConverter<'a> {
    /// Create a new switch converter
    pub fn new(ast_builder: &'a OxcAstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    /// Convert a switch region to a switch statement
    pub fn convert_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut statements = Vec::new();

        // Determine the type of switch
        let switch_type = self.detect_switch_type(region, cfg)?;

        // First, process any setup instructions in the dispatch block
        // (e.g., LoadParam instructions that come before the switch)
        let dispatch_block = &cfg.graph()[region.dispatch];

        // Find where the switch/comparison starts
        let switch_start_idx = match switch_type {
            SwitchType::Dense => {
                // Find the SwitchImm instruction
                dispatch_block
                    .instructions()
                    .iter()
                    .position(|instr| {
                        matches!(instr.instruction, UnifiedInstruction::SwitchImm { .. })
                    })
                    .ok_or(SwitchConversionError::MissingSwitchInstruction)?
            }
            SwitchType::Sparse => {
                // Find the first comparison instruction
                dispatch_block
                    .instructions()
                    .iter()
                    .position(|instr| {
                        matches!(
                            instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                        )
                    })
                    .ok_or(SwitchConversionError::MissingSwitchInstruction)?
            }
        };

        // Convert all instructions before the switch/comparison
        if switch_start_idx > 0 {
            let setup_instructions = &dispatch_block.instructions()[..switch_start_idx];
            for (i, instr) in setup_instructions.iter().enumerate() {
                // For sparse switches, skip constant loads that are immediately followed by comparisons
                if matches!(switch_type, SwitchType::Sparse) {
                    // Check if this is a constant load
                    let is_const_load = matches!(
                        instr.instruction,
                        UnifiedInstruction::LoadConstInt { .. }
                            | UnifiedInstruction::LoadConstUInt8 { .. }
                            | UnifiedInstruction::LoadConstZero { .. }
                    );

                    // Check if the next instruction is the comparison
                    let next_is_comparison = i + 1 == switch_start_idx;

                    if is_const_load && next_is_comparison {
                        // Skip this constant load as it's part of the comparison
                        continue;
                    }
                }

                // Set the current PC for this instruction
                let pc = dispatch_block.start_pc() + instr.instruction_index as u32;
                block_converter
                    .instruction_converter_mut()
                    .set_current_pc(pc);

                // Convert the instruction
                if let Ok(result) = block_converter
                    .instruction_converter_mut()
                    .convert_instruction(&instr.instruction)
                {
                    match result {
                        crate::ast::InstructionResult::Statement(stmt) => {
                            statements.push(stmt);
                        }
                        crate::ast::InstructionResult::JumpCondition(_) => {
                            // Skip jump instructions in setup
                        }
                        crate::ast::InstructionResult::None => {
                            // No statement generated
                        }
                    }
                }
            }
        }

        // Build the switch expression
        let discriminant = match switch_type {
            SwitchType::Dense => {
                self.build_dense_switch_expression(region, cfg, block_converter)?
            }
            SwitchType::Sparse => {
                self.build_sparse_switch_expression(region, cfg, block_converter)?
            }
        };

        // Convert case blocks
        let cases = match switch_type {
            SwitchType::Dense => self.convert_dense_cases(region, cfg, block_converter)?,
            SwitchType::Sparse => self.convert_sparse_cases(region, cfg, block_converter)?,
        };

        // Create the switch statement
        let span = Span::default();
        let switch_stmt = self.ast_builder.switch_statement(span, discriminant, cases);

        statements.push(Statement::SwitchStatement(
            self.ast_builder.alloc(switch_stmt),
        ));

        Ok(statements)
    }

    /// Detect the type of switch (dense or sparse)
    fn detect_switch_type(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
    ) -> Result<SwitchType, SwitchConversionError> {
        let dispatch_block = &cfg.graph()[region.dispatch];

        // Check if this is a dense switch (has SwitchImm instruction)
        let has_switch_imm = dispatch_block
            .instructions()
            .iter()
            .any(|instr| matches!(instr.instruction, UnifiedInstruction::SwitchImm { .. }));

        if has_switch_imm {
            Ok(SwitchType::Dense)
        } else {
            // This is a sparse switch detected from comparison chain
            Ok(SwitchType::Sparse)
        }
    }

    /// Build the switch expression from the dispatch block (dense switch)
    fn build_dense_switch_expression(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Expression<'a>, SwitchConversionError> {
        let dispatch_block = &cfg.graph()[region.dispatch];

        // Find the SwitchImm instruction
        let switch_instr = dispatch_block
            .instructions()
            .iter()
            .find(|instr| matches!(instr.instruction, UnifiedInstruction::SwitchImm { .. }))
            .ok_or(SwitchConversionError::MissingSwitchInstruction)?;

        // Extract the operand register
        let operand_register = match &switch_instr.instruction {
            UnifiedInstruction::SwitchImm { operand_0, .. } => *operand_0,
            _ => unreachable!(),
        };

        // Get the variable name using SSA analysis
        // We need the PC of the switch instruction itself
        let pc = dispatch_block.start_pc() + switch_instr.instruction_index as u32;
        // But we need to get the variable name from before the instruction
        let var_name = block_converter.get_variable_name_for_condition(operand_register, pc);

        // Build identifier expression
        let span = Span::default();
        // Convert to Atom to handle lifetime
        let atom = self.ast_builder.atom(&var_name);
        let ident = self.ast_builder.identifier_reference(span, atom);
        Ok(Expression::Identifier(self.ast_builder.alloc(ident)))
    }

    /// Build the switch expression from the dispatch block (sparse switch)
    fn build_sparse_switch_expression(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Expression<'a>, SwitchConversionError> {
        // For sparse switches, we need to get the variable being compared from the sparse switch analysis
        // The sparse switch detection has already determined which register is the variable

        // Re-run the sparse switch detection to get the compared register
        // This is a bit inefficient but ensures we have the correct register
        let dispatch_block = &cfg.graph()[region.dispatch];
        if let Some((compared_register, _)) =
            crate::cfg::sparse_switch_analysis::extract_comparison_info(dispatch_block)
        {
            // Get the PC of the first instruction (where the comparison happens)
            let pc = dispatch_block.start_pc();
            let var_name = block_converter.get_variable_name_for_condition(compared_register, pc);

            // Build identifier expression
            let span = Span::default();
            let atom = self.ast_builder.atom(&var_name);
            let ident = self.ast_builder.identifier_reference(span, atom);
            Ok(Expression::Identifier(self.ast_builder.alloc(ident)))
        } else {
            Err(SwitchConversionError::MissingSwitchInstruction)
        }
    }

    /// Convert switch cases (dense)
    fn convert_dense_cases(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<ArenaVec<'a, SwitchCase<'a>>, SwitchConversionError> {
        let mut cases = ArenaVec::new_in(self.ast_builder.allocator);

        // Get switch table information
        let min_value = {
            let switch_table = self.get_switch_table(region, cfg)?;
            switch_table.min_value
        };

        // Process each case
        for case_info in &region.cases {
            // For dense switches, the case value is calculated from min_value + case_index
            let case_value = min_value as i32 + case_info.case_index as i32;

            // Build case test expression
            let test = self.build_case_test(case_value)?;

            // Convert case body
            let consequent = self.convert_case_body(
                case_info.case_head,
                region.join_block,
                cfg,
                block_converter,
            )?;

            let span = Span::default();
            let switch_case = self.ast_builder.switch_case(span, Some(test), consequent);
            cases.push(switch_case);
        }

        // Add default case if present
        if let Some(default_head) = region.default_head {
            let consequent =
                self.convert_case_body(default_head, region.join_block, cfg, block_converter)?;

            let span = Span::default();
            let default_case = self.ast_builder.switch_case(span, None, consequent);
            cases.push(default_case);
        }

        Ok(cases)
    }

    /// Convert switch cases (sparse)
    fn convert_sparse_cases(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<ArenaVec<'a, SwitchCase<'a>>, SwitchConversionError> {
        let mut cases = ArenaVec::new_in(self.ast_builder.allocator);

        // For sparse switches, we need to extract the case values from the comparison blocks
        for (idx, case_info) in region.cases.iter().enumerate() {
            // The dispatch block for case i is the block that compares against value i
            let comparison_block_idx = if idx == 0 {
                region.dispatch
            } else {
                // For subsequent cases, we need to find the comparison block
                // This is typically the false target of the previous comparison
                self.find_comparison_block_for_case(region, cfg, idx)?
            };

            let comparison_block = &cfg.graph()[comparison_block_idx];

            // Extract the case value from the comparison
            let case_value = self.extract_case_value_from_comparison(comparison_block)?;

            // Build case test expression
            let test = self.build_case_test(case_value)?;

            // Convert case body
            let consequent = self.convert_case_body(
                case_info.case_head,
                region.join_block,
                cfg,
                block_converter,
            )?;

            let span = Span::default();
            let switch_case = self.ast_builder.switch_case(span, Some(test), consequent);
            cases.push(switch_case);
        }

        // Add default case if present
        if let Some(default_head) = region.default_head {
            let consequent =
                self.convert_case_body(default_head, region.join_block, cfg, block_converter)?;

            let span = Span::default();
            let default_case = self.ast_builder.switch_case(span, None, consequent);
            cases.push(default_case);
        }

        Ok(cases)
    }

    /// Find the comparison block for a specific case in a sparse switch
    fn find_comparison_block_for_case(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        case_index: usize,
    ) -> Result<NodeIndex, SwitchConversionError> {
        // Start from dispatch and follow false edges
        let mut current = region.dispatch;

        for i in 0..case_index {
            // Find the false edge
            let false_target = cfg
                .graph()
                .edges(current)
                .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::False))
                .map(|e| e.target())
                .ok_or_else(|| {
                    SwitchConversionError::InvalidRegion(format!(
                        "No false edge from comparison block {}",
                        i
                    ))
                })?;
            current = false_target;
        }

        Ok(current)
    }

    /// Extract case value from a comparison block
    fn extract_case_value_from_comparison(
        &self,
        block: &crate::cfg::Block,
    ) -> Result<i32, SwitchConversionError> {
        // Look for constant load followed by comparison
        for (i, instr) in block.instructions().iter().enumerate() {
            match &instr.instruction {
                UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                    // Check if next instruction uses this value in a comparison
                    if i + 1 < block.instructions().len() {
                        let next_instr = &block.instructions()[i + 1];
                        if matches!(
                            next_instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                        ) {
                            return Ok(*operand_1);
                        }
                    }
                }
                UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                    // Check if next instruction uses this value in a comparison
                    if i + 1 < block.instructions().len() {
                        let next_instr = &block.instructions()[i + 1];
                        if matches!(
                            next_instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                        ) {
                            return Ok(*operand_1 as i32);
                        }
                    }
                }
                UnifiedInstruction::LoadConstZero { .. } => {
                    // Check if next instruction uses this in a comparison
                    if i + 1 < block.instructions().len() {
                        let next_instr = &block.instructions()[i + 1];
                        if matches!(
                            next_instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                        ) {
                            return Ok(0);
                        }
                    }
                }
                _ => {}
            }
        }

        Err(SwitchConversionError::InvalidRegion(
            "Could not extract case value from comparison block".to_string(),
        ))
    }

    /// Build case test expression (the value to match)
    fn build_case_test(&mut self, value: i32) -> Result<Expression<'a>, SwitchConversionError> {
        let span = Span::default();
        let numeric_literal = self.ast_builder.numeric_literal(
            span,
            value as f64,
            None, // raw value
            oxc_syntax::number::NumberBase::Decimal,
        );
        Ok(Expression::NumericLiteral(
            self.ast_builder.alloc(numeric_literal),
        ))
    }

    /// Convert case body blocks to statements
    fn convert_case_body(
        &mut self,
        case_head: NodeIndex,
        join_block: NodeIndex,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, SwitchConversionError> {
        let mut statements = ArenaVec::new_in(self.ast_builder.allocator);
        let mut visited = HashSet::new();
        let mut to_visit = vec![case_head];

        // Collect all blocks that belong to this case
        while let Some(block_idx) = to_visit.pop() {
            if visited.contains(&block_idx) || block_idx == join_block {
                continue;
            }
            visited.insert(block_idx);

            // Convert the block
            let block = &cfg.graph()[block_idx];

            // Skip if this is an exit block
            if block.is_exit() {
                continue;
            }

            // Convert block statements
            let block_stmts = block_converter
                .convert_block(block, block_idx, cfg.graph())
                .map_err(|e| SwitchConversionError::CaseConversionError(e.to_string()))?;

            statements.extend(block_stmts);

            // Check if we need a break statement
            let needs_break = self.check_needs_break(block_idx, join_block, cfg, &statements);
            if needs_break {
                let span = Span::default();
                let break_stmt = self.ast_builder.break_statement(span, None);
                statements.push(Statement::BreakStatement(
                    self.ast_builder.alloc(break_stmt),
                ));
            }

            // Add successor blocks within the case
            for edge in cfg.graph().edges(block_idx) {
                let target = edge.target();
                if target != join_block && !visited.contains(&target) {
                    to_visit.push(target);
                }
            }
        }

        Ok(statements)
    }

    /// Check if a block needs a break statement
    fn check_needs_break(
        &self,
        block_idx: NodeIndex,
        join_block: NodeIndex,
        cfg: &Cfg<'a>,
        statements: &[Statement<'a>],
    ) -> bool {
        // Don't add break if the last statement is already a return or break
        if let Some(last_stmt) = statements.last() {
            match last_stmt {
                Statement::ReturnStatement(_) => return false,
                Statement::BreakStatement(_) => return false,
                Statement::ThrowStatement(_) => return false,
                _ => {}
            }
        }

        // If the block has a direct edge to the join block, it needs a break
        cfg.graph()
            .edges(block_idx)
            .any(|edge| edge.target() == join_block)
    }

    /// Get the switch table for the dispatch block
    fn get_switch_table(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
    ) -> Result<&SwitchTable, SwitchConversionError> {
        let dispatch_block = &cfg.graph()[region.dispatch];

        // Find the SwitchImm instruction
        let switch_instr = dispatch_block
            .instructions()
            .iter()
            .find(|instr| matches!(instr.instruction, UnifiedInstruction::SwitchImm { .. }))
            .ok_or(SwitchConversionError::MissingSwitchInstruction)?;

        // Get the actual instruction index
        let switch_instr_idx = switch_instr.instruction_index;

        // Get the switch table from HBC file
        cfg.hbc_file()
            .switch_tables
            .get_switch_table_by_instruction(cfg.function_index(), switch_instr_idx)
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion(format!(
                    "No switch table found for instruction {} in function {}",
                    switch_instr_idx,
                    cfg.function_index()
                ))
            })
    }
}

/// Get all blocks involved in a switch region (for marking as processed)
/// This is called from block_converter to mark all switch blocks as processed
pub fn get_all_switch_blocks(region: &SwitchRegion, cfg: &crate::cfg::Cfg) -> HashSet<NodeIndex> {
    let mut blocks = HashSet::new();

    // Add dispatch block
    blocks.insert(region.dispatch);

    // For sparse switches, we need to add all comparison blocks
    // These are the blocks between dispatch and case heads
    let mut current = region.dispatch;
    for case in &region.cases {
        // Follow false edges to find comparison blocks
        while current != case.case_head {
            blocks.insert(current);
            // Find the false edge
            let false_target = cfg
                .graph()
                .edges(current)
                .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::False))
                .map(|e| e.target());

            if let Some(next) = false_target {
                current = next;
            } else {
                break;
            }
        }
    }

    // Add all case heads
    for case in &region.cases {
        blocks.insert(case.case_head);
    }

    // Add default head if present
    if let Some(default_head) = region.default_head {
        blocks.insert(default_head);
    }

    // Note: We don't add the join block as it might be shared with other control flow

    blocks
}
