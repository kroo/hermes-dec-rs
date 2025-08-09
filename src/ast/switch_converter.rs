//! Switch statement conversion from CFG switch regions
//!
//! This module converts switch regions detected in the CFG into JavaScript switch statements.

use crate::cfg::{analysis::SwitchRegion, Cfg, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::tables::switch_table::SwitchTable;
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{Expression, Statement, SwitchCase},
    AstBuilder as OxcAstBuilder,
};
use oxc_span::Span;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet};

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
    
    /// Convert an instruction with optional comment annotations
    fn convert_instruction_with_comments(
        &self,
        instr: &HbcFunctionInstruction,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
        statements: &mut ArenaVec<'a, Statement<'a>>,
    ) -> Result<(), SwitchConversionError> {
        // Skip if already rendered
        if block_converter.is_instruction_rendered(instr) {
            return Ok(());
        }
        
        let pc = instr.offset;
        block_converter.instruction_converter_mut().set_current_pc(pc);
        
        // Check if we should add instruction comments
        if block_converter.include_instruction_comments() {
            // Create instruction comment
            let comment = self.create_instruction_comment(pc, instr)?;
            statements.push(comment);
        }
        
        // Check if we should add SSA comments
        if block_converter.include_ssa_comments() {
            if let Some(ssa_comment) = self.create_ssa_comment(pc, block_converter)? {
                statements.push(ssa_comment);
            }
        }
        
        // Convert the instruction
        if let Ok(result) = block_converter
            .instruction_converter_mut()
            .convert_instruction(&instr.instruction)
        {
            if let crate::ast::InstructionResult::Statement(stmt) = result {
                statements.push(stmt);
            }
        }
        
        // Mark as rendered
        block_converter.mark_instruction_rendered(instr);
        
        Ok(())
    }
    
    /// Create an instruction comment
    fn create_instruction_comment(
        &self,
        pc: u32,
        instr: &HbcFunctionInstruction,
    ) -> Result<Statement<'a>, SwitchConversionError> {
        let span = Span::default();
        let comment_text = format!("/* PC {}: {:?} */", pc, instr.instruction);
        
        // Create a void expression with the comment
        let comment_expr = self.ast_builder.expression_string_literal(
            span, 
            self.ast_builder.atom(&comment_text),
            None
        );
        let void_expr = self.ast_builder.expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Void,
            comment_expr,
        );
        
        Ok(self.ast_builder.statement_expression(span, void_expr))
    }
    
    /// Create an SSA comment if available
    fn create_ssa_comment(
        &self,
        _pc: u32,
        _block_converter: &crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Option<Statement<'a>>, SwitchConversionError> {
        // For now, return None - we'd need access to SSA analysis and block info
        // This would require passing more context through
        Ok(None)
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

        // For sparse switches, handle initial setup instructions
        if matches!(switch_type, SwitchType::Sparse) {
            // For sparse switches, find truly global setup instructions
            // These are instructions that initialize variables used across multiple cases
            // Walk through the comparison chain to find the first LoadConstString
            let mut current = region.dispatch;
            let mut visited_for_setup = HashSet::new();
            
            while !visited_for_setup.contains(&current) {
                visited_for_setup.insert(current);
                let block = &cfg.graph()[current];
                
                // Look for LoadConstString that appears before any comparison
                // and is not immediately followed by a comparison using the same register
                for (i, instr) in block.instructions().iter().enumerate() {
                    if let UnifiedInstruction::LoadConstString { operand_0, operand_1 } = &instr.instruction {
                        // Check if this is followed by a comparison that uses this register
                        let used_in_comparison = block.instructions().iter().skip(i + 1).any(|later| {
                            match &later.instruction {
                                UnifiedInstruction::JStrictEqual { operand_1: op1, operand_2: op2, .. } |
                                UnifiedInstruction::JStrictEqualLong { operand_1: op1, operand_2: op2, .. } => {
                                    op1 == operand_0 || op2 == operand_0
                                }
                                _ => false
                            }
                        });
                        
                        // If not used in comparison and is an empty string, it's likely initial setup
                        if !used_in_comparison && *operand_1 == 0 {  // String index 0 is empty string
                            // Convert to ArenaVec for compatibility
                            let mut arena_statements = ArenaVec::new_in(self.ast_builder.allocator);
                            self.convert_instruction_with_comments(instr, block_converter, &mut arena_statements)?;
                            // Move statements to our Vec
                            for stmt in arena_statements {
                                statements.push(stmt);
                            }
                            break; // Only process the first such instruction
                        }
                    }
                }
                
                // Move to next block in chain
                let mut next = None;
                for edge in cfg.graph().edges_directed(current, petgraph::Direction::Outgoing) {
                    if matches!(edge.weight(), EdgeKind::False) {
                        next = Some(edge.target());
                        break;
                    }
                }
                
                match next {
                    Some(n) => current = n,
                    None => break,
                }
            }
        } else {
            // For dense switches, convert instructions before the SwitchImm
            if switch_start_idx > 0 {
                let setup_instructions = &dispatch_block.instructions()[..switch_start_idx];
                for instr in setup_instructions.iter() {
                    // Convert with comments
                    let mut arena_statements = ArenaVec::new_in(self.ast_builder.allocator);
                    self.convert_instruction_with_comments(instr, block_converter, &mut arena_statements)?;
                    // Move statements to our Vec
                    for stmt in arena_statements {
                        statements.push(stmt);
                    }
                }
            }
        }

        // For sparse switches, ensure LoadParam is processed before building expression
        if matches!(switch_type, SwitchType::Sparse) {
            if let Some(entry) = cfg.entry_node() {
                let entry_block = &cfg.graph()[entry];
                for instr in entry_block.instructions().iter() {
                    if matches!(instr.instruction, UnifiedInstruction::LoadParam { .. }) {
                        if !block_converter.is_instruction_rendered(instr) {
                            // Convert with comments
                            let mut arena_statements = ArenaVec::new_in(self.ast_builder.allocator);
                            self.convert_instruction_with_comments(instr, block_converter, &mut arena_statements)?;
                            // Move statements to our Vec
                            for stmt in arena_statements {
                                statements.push(stmt);
                            }
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
        
        // Check if we need to add a return statement after the switch
        // This happens when all cases jump to a shared return block
        if let Some(shared_return_block) = self.detect_shared_return_block(region, cfg) {
            
            // Get the return instruction from the shared block
            let return_block = &cfg.graph()[shared_return_block];
            if let Some(ret_instr) = return_block.instructions().first() {
                if let UnifiedInstruction::Ret { operand_0 } = &ret_instr.instruction {
                    // Mark the return instruction as rendered
                    block_converter.mark_instruction_rendered(ret_instr);
                    
                    // Get the variable name for the return value
                    let pc = ret_instr.offset;
                    block_converter.instruction_converter_mut().set_current_pc(pc);
                    let var_name = block_converter.get_variable_name_for_condition(*operand_0, pc);
                    
                    // Create return statement
                    let return_expr = self.ast_builder.expression_identifier(span, self.ast_builder.atom(&var_name));
                    let return_stmt = self.ast_builder.statement_return(span, Some(return_expr));
                    statements.push(return_stmt);
                }
            }
        }
        
        // Mark all infrastructure block instructions as rendered
        self.mark_infrastructure_instructions_rendered(region, cfg, block_converter);

        Ok(statements)
    }
    
    /// Mark all instructions in switch infrastructure blocks as rendered
    fn mark_infrastructure_instructions_rendered(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) {
        let infrastructure_blocks = get_switch_infrastructure_blocks(region, cfg);
        for &block_idx in &infrastructure_blocks {
            let block = &cfg.graph()[block_idx];
            for instr in block.instructions().iter() {
                block_converter.mark_instruction_rendered(instr);
            }
        }
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
        if let Some((compared_register, _, _)) =
            crate::cfg::sparse_switch_analysis::extract_comparison_info(dispatch_block, &cfg.graph(), region.dispatch)
        {
            // Find the PC of the comparison instruction
            let comparison_pc = dispatch_block
                .instructions()
                .iter()
                .enumerate()
                .find(|(_, instr)| matches!(
                    instr.instruction,
                    UnifiedInstruction::JStrictEqual { .. } | UnifiedInstruction::JStrictEqualLong { .. }
                ))
                .map(|(idx, _)| dispatch_block.start_pc() + idx as u32)
                .unwrap_or(dispatch_block.start_pc());
            
            let var_name = block_converter.get_variable_name_for_condition(compared_register, comparison_pc);

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

        // Collect all case heads for fallthrough detection
        let case_heads: HashSet<NodeIndex> = region.cases.iter().map(|c| c.case_head).collect();

        // Process each case
        for (idx, case_info) in region.cases.iter().enumerate() {
            // For dense switches, the case value is calculated from min_value + case_index
            let case_value = min_value as i32 + case_info.case_index as i32;

            // Build case test expression
            let test = self.build_case_test(case_value)?;

            // Determine the end block for this case (next case or join block)
            let next_case_head = if idx + 1 < region.cases.len() {
                Some(region.cases[idx + 1].case_head)
            } else {
                None
            };

            // Convert case body with fallthrough detection
            let (consequent, _falls_through) = self.convert_case_body_with_fallthrough(
                case_info.case_head,
                region.join_block,
                next_case_head,
                &case_heads,
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

    /// Collect setup instructions for sparse switch cases
    /// Returns a map from case index to the instructions that should be prepended to that case
    fn collect_sparse_case_setup_instructions(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
    ) -> HashMap<usize, Vec<(NodeIndex, usize)>> {
        let mut case_setup_instructions = HashMap::new();
        
        // Walk through the comparison chain
        let mut current_block = region.dispatch;
        let mut visited = HashSet::new();
        let mut case_idx = 0;
        
        while !visited.contains(&current_block) && case_idx < region.cases.len() {
            visited.insert(current_block);
            let block = &cfg.graph()[current_block];
            
            // Find the comparison instruction in this block
            let mut comparison_idx = None;
            for (i, instr) in block.instructions().iter().enumerate() {
                if matches!(instr.instruction, 
                    UnifiedInstruction::JStrictEqual { .. } | 
                    UnifiedInstruction::JStrictEqualLong { .. }
                ) {
                    comparison_idx = Some(i);
                    break;
                }
            }
            
            if let Some(comp_idx) = comparison_idx {
                // Collect setup instructions that appear between comparisons
                // These are instructions that execute if the previous comparison failed
                let mut setup_instrs = Vec::new();
                
                // Only collect instructions that are truly setup for the next case
                // Skip instructions that are part of the comparison mechanism
                for i in 0..comp_idx {
                    let instr = &block.instructions()[i];
                    
                    // Skip LoadConstString that initializes variables at the start
                    if case_idx == 0 && i == 0 && matches!(instr.instruction,
                        UnifiedInstruction::LoadConstString { operand_1: 0, .. } // empty string
                    ) {
                        continue; // This is handled as pre-switch setup
                    }
                    
                    // Skip constant loads that are part of the comparison
                    if i + 1 == comp_idx && matches!(instr.instruction,
                        UnifiedInstruction::LoadConstZero { .. } |
                        UnifiedInstruction::LoadConstUInt8 { .. } |
                        UnifiedInstruction::LoadConstInt { .. }
                    ) {
                        continue;
                    }
                    
                    // Skip parameter loads and jumps
                    if matches!(instr.instruction,
                        UnifiedInstruction::LoadParam { .. } |
                        UnifiedInstruction::Jmp { .. }
                    ) {
                        continue;
                    }
                    
                    // Include LoadConstString and Mov instructions that set up case-specific values
                    if matches!(instr.instruction,
                        UnifiedInstruction::LoadConstString { .. } |
                        UnifiedInstruction::Mov { .. }
                    ) {
                        setup_instrs.push((current_block, i));
                    }
                }
                
                // These instructions belong to the cases that follow this comparison
                // For sparse switches, the pattern is:
                // - Instructions before case 0 comparison -> belong to case 0 setup
                // - Instructions between case N and case N+1 comparisons -> belong to case N+1
                if !setup_instrs.is_empty() {
                    // Special case: if this is the first block and we have a LoadConstString 
                    // at the beginning, it's likely an initial value that should go before the switch
                    if case_idx == 0 && current_block == region.dispatch {
                        // Check if first instruction is LoadConstString
                        if let Some((_, 0)) = setup_instrs.first() {
                            let block = &cfg.graph()[current_block];
                            if matches!(block.instructions()[0].instruction, 
                                UnifiedInstruction::LoadConstString { .. }) {
                                // Skip this - it will be handled as a pre-switch setup
                                continue;
                            }
                        }
                    }
                    
                    // These instructions execute between comparisons, so they belong
                    // to the case that will be checked next
                    let target_case = case_idx + 1;
                    if target_case < region.cases.len() {
                        case_setup_instructions.entry(target_case)
                            .or_insert_with(Vec::new)
                            .extend(setup_instrs);
                    }
                }
                
                case_idx += 1;
            }
            
            // Move to the next block in the chain (false branch)
            let mut next_block = None;
            for edge in cfg.graph().edges_directed(current_block, petgraph::Direction::Outgoing) {
                if matches!(edge.weight(), EdgeKind::False) {
                    next_block = Some(edge.target());
                    break;
                }
            }
            
            match next_block {
                Some(next) => current_block = next,
                None => break,
            }
        }
        
        case_setup_instructions
    }

    /// Convert switch cases (sparse)
    fn convert_sparse_cases(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<ArenaVec<'a, SwitchCase<'a>>, SwitchConversionError> {
        let mut cases = ArenaVec::new_in(self.ast_builder.allocator);

        // Collect all case heads for fallthrough detection
        let case_heads: HashSet<NodeIndex> = region.cases.iter().map(|c| c.case_head).collect();
        
        // Collect setup instructions for each case
        let case_setup_instructions = self.collect_sparse_case_setup_instructions(region, cfg);
        
        
        // Group cases by their target block and fallthrough relationships
        let mut case_groups: Vec<Vec<usize>> = Vec::new();
        let mut processed = vec![false; region.cases.len()];
        
        // First, create initial groups based on same target blocks
        let mut initial_groups: Vec<Vec<usize>> = Vec::new();
        for i in 0..region.cases.len() {
            if processed[i] {
                continue;
            }
            
            let mut group = vec![i];
            processed[i] = true;
            
            // Find all other cases with the same target
            for j in (i + 1)..region.cases.len() {
                if !processed[j] && region.cases[j].case_head == region.cases[i].case_head {
                    group.push(j);
                    processed[j] = true;
                }
            }
            
            initial_groups.push(group);
        }
        
        // Use the initial groups as-is for now
        case_groups = initial_groups;

        // Build a map of which cases should have bodies
        let mut case_should_have_body = vec![false; region.cases.len()];
        
        // Mark the last case in each group as needing a body
        for group in &case_groups {
            if let Some(&last_idx) = group.last() {
                case_should_have_body[last_idx] = true;
            }
        }
        
        // Process cases in original order
        for (idx, case_info) in region.cases.iter().enumerate() {
            // Extract case value
            let comparison_block_idx = if idx == 0 {
                region.dispatch
            } else {
                self.find_comparison_block_for_case(region, cfg, idx)?
            };

            let comparison_block = &cfg.graph()[comparison_block_idx];
            let case_value = self.extract_case_value_from_comparison(comparison_block)?;

            // Build case test expression
            let test = self.build_case_test(case_value)?;
            
            let consequent = if case_should_have_body[idx] {
                // This case gets the actual body
                let mut case_statements = ArenaVec::new_in(self.ast_builder.allocator);
                
                // First, add any setup instructions for this case
                if let Some(setup_instrs) = case_setup_instructions.get(&idx) {
                    for &(block_idx, instr_idx) in setup_instrs {
                        let block = &cfg.graph()[block_idx];
                        let instr = &block.instructions()[instr_idx];
                        self.convert_instruction_with_comments(instr, block_converter, &mut case_statements)?;
                    }
                }
                
                // Find which group this case belongs to
                let next_case_head = if idx + 1 < region.cases.len() {
                    Some(region.cases[idx + 1].case_head)
                } else {
                    None
                };
                
                // Check if this is one of the special cases where assignment happens before jump
                let case_block = &cfg.graph()[case_info.case_head];
                if case_block.instructions().is_empty() || 
                   (case_block.instructions().len() == 1 && 
                    matches!(&case_block.instructions()[0].instruction, UnifiedInstruction::Ret { .. })) {
                    // This case has an empty body or just a return
                    // Add break if needed
                    if !self.has_terminal_statement(&case_statements) {
                        let span = Span::default();
                        let break_stmt = self.ast_builder.break_statement(span, None);
                        case_statements.push(Statement::BreakStatement(
                            self.ast_builder.alloc(break_stmt),
                        ));
                    }
                    
                    case_statements
                } else {
                    // Regular case body conversion
                    let (mut body, mut falls_through) = self.convert_case_body_with_fallthrough(
                        case_info.case_head,
                        region.join_block,
                        next_case_head,
                        &case_heads,
                        cfg,
                        block_converter,
                    )?;
                    
                    // Prepend setup instructions to the body
                    case_statements.extend(body);
                    
                    // Check if this case falls through to another case's target block
                    // This happens when the case body ends by jumping to a block that is another case's head
                    if !falls_through && !self.has_terminal_statement(&case_statements) {
                        // Check where this case's blocks lead
                        let case_blocks = self.identify_case_blocks(case_info.case_head, region.join_block, next_case_head, &case_heads, cfg);
                        
                        // Find the exit edges from this case
                        for block_idx in &case_blocks {
                            for edge in cfg.graph().edges(*block_idx) {
                                let target = edge.target();
                                // Check if this target is another case's head (not the next case for regular fallthrough)
                                if case_heads.contains(&target) && Some(target) != next_case_head && target != region.join_block {
                                    // This case falls through to another case
                                    // We should not add a break statement
                                    falls_through = true;
                                    break;
                                }
                            }
                            if falls_through {
                                break;
                            }
                        }
                    }
                    
                    case_statements
                }
            } else {
                // This case is empty (fallthrough)
                ArenaVec::new_in(self.ast_builder.allocator)
            };

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
        // First, find the comparison instruction
        let mut comparison_idx = None;
        let mut comparison_operands = None;

        for (i, instr) in block.instructions().iter().enumerate() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual {
                    operand_1,
                    operand_2,
                    ..
                }
                | UnifiedInstruction::JStrictEqualLong {
                    operand_1,
                    operand_2,
                    ..
                } => {
                    comparison_idx = Some(i);
                    comparison_operands = Some((*operand_1, *operand_2));
                    break;
                }
                _ => {}
            }
        }

        if let (Some(idx), Some((op1, op2))) = (comparison_idx, comparison_operands) {
            // Now look backwards for constant loads into either operand
            for i in (0..idx).rev() {
                match &block.instructions()[i].instruction {
                    UnifiedInstruction::LoadConstInt {
                        operand_0,
                        operand_1,
                    } => {
                        if *operand_0 == op1 || *operand_0 == op2 {
                            return Ok(*operand_1);
                        }
                    }
                    UnifiedInstruction::LoadConstUInt8 {
                        operand_0,
                        operand_1,
                    } => {
                        if *operand_0 == op1 || *operand_0 == op2 {
                            return Ok(*operand_1 as i32);
                        }
                    }
                    UnifiedInstruction::LoadConstZero { operand_0 } => {
                        if *operand_0 == op1 || *operand_0 == op2 {
                            return Ok(0);
                        }
                    }
                    _ => {}
                }
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

    /// Convert case body blocks to statements with fallthrough detection
    fn convert_case_body_with_fallthrough(
        &mut self,
        case_head: NodeIndex,
        join_block: NodeIndex,
        next_case_head: Option<NodeIndex>,
        case_heads: &HashSet<NodeIndex>,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<(ArenaVec<'a, Statement<'a>>, bool), SwitchConversionError> {
        
        let mut statements = ArenaVec::new_in(self.ast_builder.allocator);
        let mut falls_through = false;

        // Special case: if the case head is the join block, this is an empty case
        // that jumps directly to the end of the switch
        if case_head == join_block {
            // Return empty statements - the switch will add a break if needed
            return Ok((statements, false));
        }
        
        // Check if this block contains only a return statement and is shared by other cases
        let case_block = &cfg.graph()[case_head];
        if case_block.instructions().len() == 1 {
            if let UnifiedInstruction::Ret { .. } = &case_block.instructions()[0].instruction {
                // Check how many blocks jump to this return block
                let mut jumps_to_this_block = 0;
                for node in cfg.graph().node_indices() {
                    for edge in cfg.graph().edges(node) {
                        if edge.target() == case_head {
                            jumps_to_this_block += 1;
                        }
                    }
                }
                
                // If this is a shared return block (multiple cases jump here), 
                // we should return empty statements for this case (but it will get a break)
                if jumps_to_this_block > 1 {
                    // Add a break statement since this case is empty
                    let span = Span::default();
                    let break_stmt = self.ast_builder.break_statement(span, None);
                    statements.push(Statement::BreakStatement(
                        self.ast_builder.alloc(break_stmt),
                    ));
                    return Ok((statements, false));
                }
            }
        }

        // First, identify all blocks that belong to this case
        let case_blocks =
            self.identify_case_blocks(case_head, join_block, next_case_head, case_heads, cfg);

        // Check for fallthrough by seeing if any block in this case jumps to the next case head
        if let Some(next_case) = next_case_head {
            for block_idx in &case_blocks {
                for edge in cfg.graph().edges(*block_idx) {
                    if edge.target() == next_case {
                        falls_through = true;
                        break;
                    }
                }
                if falls_through {
                    break;
                }
            }
        }

        // Now convert the case body starting from the head, preserving control flow
        self.convert_case_control_flow(
            case_head,
            &case_blocks,
            join_block,
            cfg,
            block_converter,
            &mut statements,
            &mut HashSet::new(),
        )?;

        // Add break if needed
        if !falls_through && !self.has_terminal_statement(&statements) {
            let span = Span::default();
            let break_stmt = self.ast_builder.break_statement(span, None);
            statements.push(Statement::BreakStatement(
                self.ast_builder.alloc(break_stmt),
            ));
        }

        Ok((statements, falls_through))
    }

    /// Detect if all cases jump to a shared return block
    fn detect_shared_return_block(
        &self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
    ) -> Option<NodeIndex> {
        // Check all case heads to see if they jump to a common return block
        let mut return_blocks = HashSet::new();
        
        // Check each case
        for case in &region.cases {
            // Find what block this case jumps to
            for edge in cfg.graph().edges(case.case_head) {
                let target = edge.target();
                let target_block = &cfg.graph()[target];
                
                // Check if it's a return block
                if target_block.instructions().len() == 1 {
                    if let UnifiedInstruction::Ret { .. } = &target_block.instructions()[0].instruction {
                        return_blocks.insert(target);
                    }
                }
            }
        }
        
        // Check default case too
        if let Some(default_head) = region.default_head {
            for edge in cfg.graph().edges(default_head) {
                let target = edge.target();
                let target_block = &cfg.graph()[target];
                
                // Check if it's a return block
                if target_block.instructions().len() == 1 {
                    if let UnifiedInstruction::Ret { .. } = &target_block.instructions()[0].instruction {
                        return_blocks.insert(target);
                    }
                }
            }
        }
        
        // If there's exactly one shared return block, return it
        if return_blocks.len() == 1 {
            return return_blocks.into_iter().next();
        }
        
        None
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
        
        // Special case: if the case head is the join block, this is an empty case
        // that jumps directly to the end of the switch
        if case_head == join_block {
            // Return empty statements - the switch will add a break if needed
            return Ok(statements);
        }
        
        // Check if this block contains only a return statement and is shared by other cases
        let case_block = &cfg.graph()[case_head];
        if case_block.instructions().len() == 1 {
            if let UnifiedInstruction::Ret { .. } = &case_block.instructions()[0].instruction {
                // Check how many blocks jump to this return block
                let mut jumps_to_this_block = 0;
                for node in cfg.graph().node_indices() {
                    for edge in cfg.graph().edges(node) {
                        if edge.target() == case_head {
                            jumps_to_this_block += 1;
                        }
                    }
                }
                
                // If this is a shared return block, return empty statements
                if jumps_to_this_block > 1 {
                    return Ok(statements);
                }
            }
        }
        
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

            // Convert block statements, filtering out jump instructions
            let block_stmts =
                self.convert_case_block_statements(block, block_idx, cfg, block_converter)?;

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
                    // Also skip if this is a shared return block
                    let target_block = &cfg.graph()[target];
                    if target_block.instructions().len() == 1 {
                        if let UnifiedInstruction::Ret { .. } = &target_block.instructions()[0].instruction {
                            // Check if this is shared by multiple blocks
                            let mut incoming_count = 0;
                            for node in cfg.graph().node_indices() {
                                for edge in cfg.graph().edges(node) {
                                    if edge.target() == target {
                                        incoming_count += 1;
                                    }
                                }
                            }
                            if incoming_count > 1 {
                                continue;
                            }
                        }
                    }
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

    /// Identify all blocks that belong to a case
    fn identify_case_blocks(
        &self,
        case_head: NodeIndex,
        join_block: NodeIndex,
        next_case_head: Option<NodeIndex>,
        case_heads: &HashSet<NodeIndex>,
        cfg: &Cfg<'a>,
    ) -> HashSet<NodeIndex> {
        let mut case_blocks = HashSet::new();
        let mut to_visit = vec![case_head];

        while let Some(block_idx) = to_visit.pop() {
            if case_blocks.contains(&block_idx) || block_idx == join_block {
                continue;
            }

            // Stop if we hit another case
            if block_idx != case_head && case_heads.contains(&block_idx) {
                // Don't include other case heads in our case blocks
                continue;
            }

            case_blocks.insert(block_idx);

            // Add successors
            for edge in cfg.graph().edges(block_idx) {
                let target = edge.target();
                if !case_blocks.contains(&target) {
                    // Don't follow edges to blocks that are case heads of other cases
                    // unless it's the expected next case (fallthrough)
                    if case_heads.contains(&target) && Some(target) != next_case_head {
                        continue;
                    }
                    to_visit.push(target);
                }
            }
        }

        case_blocks
    }

    /// Convert control flow within a case, preserving structure
    fn convert_case_control_flow(
        &mut self,
        current_block: NodeIndex,
        case_blocks: &HashSet<NodeIndex>,
        join_block: NodeIndex,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
        statements: &mut ArenaVec<'a, Statement<'a>>,
        visited: &mut HashSet<NodeIndex>,
    ) -> Result<(), SwitchConversionError> {
        if visited.contains(&current_block) || !case_blocks.contains(&current_block) {
            return Ok(());
        }
        
        // Before processing this block, check if it's the start of any nested control flow structure
        
        // 1. Check for nested switch
        if let Some(switch_analysis) = cfg.analyze_switch_regions() {
            if let Some(nested_region) = block_converter.find_switch_starting_at(current_block, &switch_analysis) {
                // This is a nested switch! Convert it as a whole
                let mut nested_converter = SwitchConverter::new(self.ast_builder);
                if let Ok(nested_statements) = nested_converter.convert_switch_region(nested_region, cfg, block_converter) {
                    statements.extend(nested_statements);
                    
                    // Mark all blocks in the nested switch as visited
                    let nested_blocks = get_switch_infrastructure_blocks(nested_region, cfg);
                    for block in &nested_blocks {
                        visited.insert(*block);
                    }
                    // Also mark case heads as visited
                    for case in &nested_region.cases {
                        visited.insert(case.case_head);
                    }
                    if let Some(default) = nested_region.default_head {
                        visited.insert(default);
                    }
                    
                    // Continue from the join block of the nested switch
                    if case_blocks.contains(&nested_region.join_block) && nested_region.join_block != join_block {
                        return self.convert_case_control_flow(
                            nested_region.join_block,
                            case_blocks,
                            join_block,
                            cfg,
                            block_converter,
                            statements,
                            visited,
                        );
                    }
                    return Ok(());
                }
            }
        }
        
        // 2. Check for nested if/else chains
        if let Some(conditional_analysis) = cfg.analyze_conditional_chains() {
            if let Some(nested_chain) = block_converter.find_chain_starting_at(current_block, &conditional_analysis) {
                // This is a nested conditional! Convert it as a whole
                let mut conditional_converter = crate::ast::conditional_converter::ConditionalConverter::new(self.ast_builder);
                if let Ok(nested_statements) = conditional_converter.convert_chain(nested_chain, cfg, block_converter) {
                    statements.extend(nested_statements);
                    
                    // Mark all blocks in the conditional chain as visited
                    let nested_blocks = crate::ast::conditional_converter::ConditionalConverter::get_all_chain_blocks(nested_chain);
                    for block in &nested_blocks {
                        visited.insert(*block);
                    }
                    
                    // Continue from the join block of the conditional
                    if case_blocks.contains(&nested_chain.join_block) && nested_chain.join_block != join_block {
                        return self.convert_case_control_flow(
                            nested_chain.join_block,
                            case_blocks,
                            join_block,
                            cfg,
                            block_converter,
                            statements,
                            visited,
                        );
                    }
                    return Ok(());
                }
            }
        }
        
        // 3. TODO: Check for nested loops (when loop analysis is implemented)
        // if let Some(loop_analysis) = cfg.analyze_loops() {
        //     if let Some(nested_loop) = block_converter.find_loop_starting_at(current_block, &loop_analysis) {
        //         // Convert nested loop...
        //     }
        // }

        visited.insert(current_block);

        let block = &cfg.graph()[current_block];
        if block.is_exit() {
            return Ok(());
        }

        // Convert the statements in this block
        let block_stmts =
            self.convert_case_block_statements(block, current_block, cfg, block_converter)?;
        statements.extend(block_stmts);

        // Check the outgoing edges to determine control flow
        let edges: Vec<_> = cfg.graph().edges(current_block).collect();

        match edges.len() {
            0 => {} // No successors
            1 => {
                // Unconditional flow to next block
                let target = edges[0].target();
                if case_blocks.contains(&target) && target != join_block {
                    self.convert_case_control_flow(
                        target,
                        case_blocks,
                        join_block,
                        cfg,
                        block_converter,
                        statements,
                        visited,
                    )?;
                }
            }
            2 => {
                // Conditional branch - need to create if statement
                self.convert_conditional_in_case(
                    current_block,
                    case_blocks,
                    join_block,
                    cfg,
                    block_converter,
                    statements,
                    visited,
                )?;
            }
            _ => {
                // Multiple branches (switch?) - handle specially
                // For now, just convert the first successor
                if let Some(edge) = edges.first() {
                    let target = edge.target();
                    if case_blocks.contains(&target) && target != join_block {
                        self.convert_case_control_flow(
                            target,
                            case_blocks,
                            join_block,
                            cfg,
                            block_converter,
                            statements,
                            visited,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Convert a conditional branch within a case
    fn convert_conditional_in_case(
        &mut self,
        cond_block: NodeIndex,
        case_blocks: &HashSet<NodeIndex>,
        join_block: NodeIndex,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
        statements: &mut ArenaVec<'a, Statement<'a>>,
        visited: &mut HashSet<NodeIndex>,
    ) -> Result<(), SwitchConversionError> {
        // Find the true and false branches
        let mut true_branch = None;
        let mut false_branch = None;

        for edge in cfg.graph().edges(cond_block) {
            match edge.weight() {
                crate::cfg::EdgeKind::True => true_branch = Some(edge.target()),
                crate::cfg::EdgeKind::False => false_branch = Some(edge.target()),
                _ => {}
            }
        }

        if let (Some(true_target), Some(false_target)) = (true_branch, false_branch) {
            // Get the condition from the block
            let block = &cfg.graph()[cond_block];
            let condition = self.extract_condition_from_block(block, block_converter)?;

            // Convert true branch
            let mut consequent = ArenaVec::new_in(self.ast_builder.allocator);
            if case_blocks.contains(&true_target) && true_target != join_block {
                self.convert_case_control_flow(
                    true_target,
                    case_blocks,
                    join_block,
                    cfg,
                    block_converter,
                    &mut consequent,
                    visited,
                )?;
            }

            // Convert false branch
            let mut alternate = None;
            if case_blocks.contains(&false_target) && false_target != join_block {
                let mut alt_stmts = ArenaVec::new_in(self.ast_builder.allocator);
                self.convert_case_control_flow(
                    false_target,
                    case_blocks,
                    join_block,
                    cfg,
                    block_converter,
                    &mut alt_stmts,
                    visited,
                )?;
                if !alt_stmts.is_empty() {
                    alternate = Some(Statement::BlockStatement(
                        self.ast_builder
                            .alloc(self.ast_builder.block_statement(Span::default(), alt_stmts)),
                    ));
                }
            }

            // Create if statement
            let span = Span::default();
            let if_stmt = self.ast_builder.if_statement(
                span,
                condition,
                Statement::BlockStatement(
                    self.ast_builder
                        .alloc(self.ast_builder.block_statement(span, consequent)),
                ),
                alternate,
            );
            statements.push(Statement::IfStatement(self.ast_builder.alloc(if_stmt)));
        }

        Ok(())
    }

    /// Extract condition expression from a block
    fn extract_condition_from_block(
        &mut self,
        block: &crate::cfg::Block,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Expression<'a>, SwitchConversionError> {
        // Look for a comparison instruction followed by a conditional jump
        let instructions = block.instructions();

        // Find the conditional jump at the end
        if let Some(last_instr) = instructions.last() {
            match &last_instr.instruction {
                // Direct conditional jumps with built-in comparisons
                UnifiedInstruction::JGreater {
                    operand_0: _,
                    operand_1,
                    operand_2,
                }
                | UnifiedInstruction::JGreaterLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let pc = block.start_pc() + (instructions.len() - 1) as u32;
                    block_converter
                        .instruction_converter_mut()
                        .set_current_pc(pc);
                    let left = self.build_operand_expression(*operand_1 as u8, block_converter);
                    let right = self.build_operand_expression(*operand_2 as u8, block_converter);
                    let span = Span::default();
                    return Ok(Expression::BinaryExpression(self.ast_builder.alloc(
                        self.ast_builder.binary_expression(
                            span,
                            left,
                            oxc_ast::ast::BinaryOperator::GreaterThan,
                            right,
                        ),
                    )));
                }
                UnifiedInstruction::JLess {
                    operand_0: _,
                    operand_1,
                    operand_2,
                }
                | UnifiedInstruction::JLessLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let pc = block.start_pc() + (instructions.len() - 1) as u32;
                    block_converter
                        .instruction_converter_mut()
                        .set_current_pc(pc);
                    let left = self.build_operand_expression(*operand_1 as u8, block_converter);
                    let right = self.build_operand_expression(*operand_2 as u8, block_converter);
                    let span = Span::default();
                    return Ok(Expression::BinaryExpression(self.ast_builder.alloc(
                        self.ast_builder.binary_expression(
                            span,
                            left,
                            oxc_ast::ast::BinaryOperator::LessThan,
                            right,
                        ),
                    )));
                }
                UnifiedInstruction::JStrictEqual {
                    operand_0: _,
                    operand_1,
                    operand_2,
                }
                | UnifiedInstruction::JStrictEqualLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let pc = block.start_pc() + (instructions.len() - 1) as u32;
                    block_converter
                        .instruction_converter_mut()
                        .set_current_pc(pc);
                    let left = self.build_operand_expression(*operand_1 as u8, block_converter);
                    let right = self.build_operand_expression(*operand_2 as u8, block_converter);
                    let span = Span::default();
                    return Ok(Expression::BinaryExpression(self.ast_builder.alloc(
                        self.ast_builder.binary_expression(
                            span,
                            left,
                            oxc_ast::ast::BinaryOperator::StrictEquality,
                            right,
                        ),
                    )));
                }
                UnifiedInstruction::JmpTrue { .. } | UnifiedInstruction::JmpFalse { .. } => {
                    // Look backwards for the condition-setting instruction
                    for i in (0..instructions.len() - 1).rev() {
                        let instr = &instructions[i];
                        match &instr.instruction {
                            // Comparison instructions that set a boolean result
                            UnifiedInstruction::Less {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::LessEq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::Greater {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::GreaterEq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::Eq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::Neq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::StrictEq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            }
                            | UnifiedInstruction::StrictNeq {
                                operand_0: _,
                                operand_1,
                                operand_2,
                            } => {
                                // Convert this comparison to an expression
                                let pc = block.start_pc() + i as u32;
                                block_converter
                                    .instruction_converter_mut()
                                    .set_current_pc(pc);

                                // Build the comparison expression directly
                                let left = self
                                    .build_operand_expression(*operand_1 as u8, block_converter);
                                let right = self
                                    .build_operand_expression(*operand_2 as u8, block_converter);

                                let span = Span::default();
                                let op = match &instr.instruction {
                                    UnifiedInstruction::Less { .. } => {
                                        oxc_ast::ast::BinaryOperator::LessThan
                                    }
                                    UnifiedInstruction::LessEq { .. } => {
                                        oxc_ast::ast::BinaryOperator::LessEqualThan
                                    }
                                    UnifiedInstruction::Greater { .. } => {
                                        oxc_ast::ast::BinaryOperator::GreaterThan
                                    }
                                    UnifiedInstruction::GreaterEq { .. } => {
                                        oxc_ast::ast::BinaryOperator::GreaterEqualThan
                                    }
                                    UnifiedInstruction::Eq { .. } => {
                                        oxc_ast::ast::BinaryOperator::Equality
                                    }
                                    UnifiedInstruction::Neq { .. } => {
                                        oxc_ast::ast::BinaryOperator::Inequality
                                    }
                                    UnifiedInstruction::StrictEq { .. } => {
                                        oxc_ast::ast::BinaryOperator::StrictEquality
                                    }
                                    UnifiedInstruction::StrictNeq { .. } => {
                                        oxc_ast::ast::BinaryOperator::StrictInequality
                                    }
                                    _ => unreachable!(),
                                };

                                return Ok(Expression::BinaryExpression(self.ast_builder.alloc(
                                    self.ast_builder.binary_expression(span, left, op, right),
                                )));
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }

        // Fallback to placeholder
        let span = Span::default();
        Ok(Expression::BooleanLiteral(
            self.ast_builder
                .alloc(self.ast_builder.boolean_literal(span, true)),
        ))
    }

    /// Build an expression for an operand (register)
    fn build_operand_expression(
        &mut self,
        register: u8,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Expression<'a> {
        // Note: The PC should already be set by the caller
        // Use get_source_variable_name to get the correct SSA name for reading
        let var_name = block_converter
            .instruction_converter_mut()
            .register_manager_mut()
            .get_source_variable_name(register);

        let span = Span::default();
        let atom = self.ast_builder.atom(&var_name);
        let ident = self.ast_builder.identifier_reference(span, atom);
        Expression::Identifier(self.ast_builder.alloc(ident))
    }

    /// Check if the statements list has a terminal statement
    fn has_terminal_statement(&self, statements: &[Statement<'a>]) -> bool {
        if let Some(last) = statements.last() {
            matches!(
                last,
                Statement::ReturnStatement(_)
                    | Statement::BreakStatement(_)
                    | Statement::ThrowStatement(_)
            )
        } else {
            false
        }
    }

    /// Convert statements in a case block, filtering out structural jumps
    fn convert_case_block_statements(
        &mut self,
        block: &crate::cfg::Block,
        _block_idx: NodeIndex,
        _cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<ArenaVec<'a, Statement<'a>>, SwitchConversionError> {
        let mut statements = ArenaVec::new_in(self.ast_builder.allocator);

        for (pc_offset, instruction) in block.instructions().iter().enumerate() {
            // Skip if already rendered
            if block_converter.is_instruction_rendered(instruction) {
                continue;
            }
            
            let pc = instruction.offset; // Use absolute offset

            // Skip ONLY terminal jump instructions (at the end of the block)
            let is_last = pc_offset == block.instructions().len() - 1;
            if is_last {
                match &instruction.instruction {
                    UnifiedInstruction::Jmp { .. }
                    | UnifiedInstruction::JmpLong { .. }
                    | UnifiedInstruction::JmpTrue { .. }
                    | UnifiedInstruction::JmpFalse { .. }
                    | UnifiedInstruction::JStrictEqual { .. }
                    | UnifiedInstruction::JStrictEqualLong { .. } => {
                        // Terminal jump - skip it
                        continue;
                    }
                    _ => {}
                }
            }

            // Convert the instruction with comments
            self.convert_instruction_with_comments(instruction, block_converter, &mut statements)?;
        }

        Ok(statements)
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

/// Get all blocks that are part of a switch region for the purpose of tracking what was processed
/// This includes infrastructure blocks AND all blocks within case bodies
pub fn get_all_switch_blocks_with_bodies(region: &SwitchRegion, cfg: &crate::cfg::Cfg) -> HashSet<NodeIndex> {
    let mut blocks = HashSet::new();

    // Add dispatch block
    blocks.insert(region.dispatch);

    // For sparse switches, add all comparison blocks
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
    let mut case_heads = HashSet::new();
    for case in &region.cases {
        blocks.insert(case.case_head);
        case_heads.insert(case.case_head);
    }

    // Add default head if present
    if let Some(default_head) = region.default_head {
        blocks.insert(default_head);
        case_heads.insert(default_head);
    }

    // Now find all blocks that belong to each case body
    // This is important to prevent them from being processed again
    for case in &region.cases {
        let mut to_visit = vec![case.case_head];
        let mut case_blocks = HashSet::new();
        
        while let Some(block_idx) = to_visit.pop() {
            if case_blocks.contains(&block_idx) || block_idx == region.join_block {
                continue;
            }
            
            case_blocks.insert(block_idx);
            blocks.insert(block_idx);
            
            // Add successors that aren't other case heads
            for edge in cfg.graph().edges(block_idx) {
                let target = edge.target();
                if !case_blocks.contains(&target) && !case_heads.contains(&target) {
                    to_visit.push(target);
                }
            }
        }
    }
    
    // Do the same for default case
    if let Some(default_head) = region.default_head {
        let mut to_visit = vec![default_head];
        let mut case_blocks = HashSet::new();
        
        while let Some(block_idx) = to_visit.pop() {
            if case_blocks.contains(&block_idx) || block_idx == region.join_block {
                continue;
            }
            
            case_blocks.insert(block_idx);
            blocks.insert(block_idx);
            
            // Add successors that aren't other case heads
            for edge in cfg.graph().edges(block_idx) {
                let target = edge.target();
                if !case_blocks.contains(&target) && !case_heads.contains(&target) {
                    to_visit.push(target);
                }
            }
        }
    }

    // Note: We don't add the join block as it might be shared with other control flow

    blocks
}

/// Get only the switch infrastructure blocks (dispatch and comparison blocks)
/// This is called from block_converter to mark switch infrastructure as processed
/// Case blocks are NOT included so they can be analyzed for nested control flow
pub fn get_switch_infrastructure_blocks(region: &SwitchRegion, cfg: &crate::cfg::Cfg) -> HashSet<NodeIndex> {
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

    // IMPORTANT: Don't add case heads or default head
    // These blocks need to be analyzed for nested control flow structures

    // Note: We don't add the join block as it might be shared with other control flow

    blocks
}
