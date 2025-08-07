//! Conditional statement converter
//!
//! This module converts CFG if/else region analysis into JavaScript conditional statements.
//! It leverages the existing ConditionalAnalysis from CFG to generate clean AST nodes.

use crate::cfg::analysis::{BranchType, ChainType, ConditionalBranch, ConditionalChain};
use crate::cfg::{Cfg, EdgeKind};
use crate::generated::unified_instructions::UnifiedInstruction;
use oxc_allocator::Vec as AllocVec;
use oxc_ast::{ast::*, AstBuilder};
use oxc_span::Span;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashSet;

/// Converts conditional regions from CFG analysis into AST if statements
pub struct ConditionalConverter<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> ConditionalConverter<'a> {
    /// Create a new conditional converter
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    /// Convert a conditional chain into a list of statements (condition setup + if statement)
    pub fn convert_chain(
        &mut self,
        chain: &ConditionalChain,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<AllocVec<'a, Statement<'a>>, String> {
        let mut statements = AllocVec::new_in(self.ast_builder.allocator);

        // First, process all condition blocks (they contain setup instructions)
        // But only if they're not part of any branch blocks (to avoid duplication)
        let mut processed_condition_blocks = std::collections::HashSet::new();
        let mut all_branch_blocks = std::collections::HashSet::new();

        // Collect all branch blocks to avoid processing them as condition setup
        for branch in &chain.branches {
            all_branch_blocks.extend(&branch.branch_blocks);
            all_branch_blocks.insert(branch.branch_entry);
        }

        for branch in &chain.branches {
            // Only process condition blocks that are not part of any branch
            if !processed_condition_blocks.contains(&branch.condition_block)
                && !all_branch_blocks.contains(&branch.condition_block)
            {
                processed_condition_blocks.insert(branch.condition_block);

                let condition_block = &cfg.graph()[branch.condition_block];
                let condition_statements = block_converter
                    .convert_block(condition_block, branch.condition_block, cfg.graph())
                    .map_err(|e| {
                        format!(
                            "Failed to convert condition block {}: {:?}",
                            branch.condition_block.index(),
                            e
                        )
                    })?;

                // Add all statements except the last one (which should be the conditional jump)
                let stmt_count = condition_statements.len();
                for (i, stmt) in condition_statements.into_iter().enumerate() {
                    if i < stmt_count - 1 {
                        statements.push(stmt);
                    }
                }
            }
        }

        // Then convert the conditional structure
        let if_stmt = match &chain.chain_type {
            ChainType::SimpleIfElse => self.convert_simple_if_else(chain, cfg, block_converter)?,
            ChainType::ElseIfChain => self.convert_else_if_chain(chain, cfg, block_converter)?,
            ChainType::GuardClausePattern => {
                self.convert_guard_clauses(chain, cfg, block_converter)?
            }
            ChainType::NestedConditional => {
                // For now, treat nested conditionals as simple if-else
                // TODO: Implement proper nested conditional handling
                self.convert_simple_if_else(chain, cfg, block_converter)?
            }
            ChainType::SwitchLikeChain => {
                // TODO: This will be handled in AST-06
                self.convert_simple_if_else(chain, cfg, block_converter)?
            }
        };

        statements.push(if_stmt);
        Ok(statements)
    }

    /// Convert a simple if or if-else statement
    fn convert_simple_if_else(
        &mut self,
        chain: &ConditionalChain,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Statement<'a>, String> {
        if chain.branches.is_empty() {
            return Err("No branches in conditional chain".to_string());
        }

        // For simple if-else, we should have 1 or 2 branches
        let first_branch = &chain.branches[0];

        // Build the condition expression
        let test = self.build_condition_expression(&first_branch, cfg)?;

        // Get blocks to convert, excluding those that belong to nested chains
        let branch_idx = 0; // First branch is always index 0
        let mut blocks_to_convert = first_branch.branch_blocks.clone();

        // Find nested chains for this branch
        let nested_chains_opt = self.find_nested_chains_for_branch(chain, branch_idx);

        // Exclude blocks that belong to nested chains
        if let Some(ref nested_chains) = nested_chains_opt {
            let mut nested_blocks = std::collections::HashSet::new();
            for nested_chain in nested_chains {
                // Add all blocks from nested chain
                for branch in &nested_chain.branches {
                    nested_blocks.insert(branch.condition_block);
                    nested_blocks.extend(&branch.branch_blocks);
                }
            }
            blocks_to_convert.retain(|&block| !nested_blocks.contains(&block));
        }

        // Convert the consequent blocks
        let mut consequent_statements =
            self.convert_branch_blocks(&blocks_to_convert, cfg, block_converter)?;

        // Handle nested chains in this branch
        if let Some(nested_chains) = nested_chains_opt {
            for nested_chain in nested_chains {
                let nested_stmts = self.convert_chain(nested_chain, cfg, block_converter)?;
                consequent_statements.extend(nested_stmts);
            }
        }

        let consequent = self.create_block_statement(consequent_statements);

        // Check for else branch
        let alternate = if chain.branches.len() > 1 {
            let else_branch = &chain.branches[1];
            if else_branch.branch_type == BranchType::Else {
                // Get blocks to convert, excluding those that belong to nested chains
                let branch_idx = 1;
                let mut blocks_to_convert = else_branch.branch_blocks.clone();

                // Find nested chains for this branch
                let nested_chains_opt = self.find_nested_chains_for_branch(chain, branch_idx);

                // Exclude blocks that belong to nested chains
                if let Some(ref nested_chains) = nested_chains_opt {
                    let mut nested_blocks = std::collections::HashSet::new();
                    for nested_chain in nested_chains {
                        // Add all blocks from nested chain
                        for branch in &nested_chain.branches {
                            nested_blocks.insert(branch.condition_block);
                            nested_blocks.extend(&branch.branch_blocks);
                        }
                    }
                    blocks_to_convert.retain(|&block| !nested_blocks.contains(&block));
                }

                let mut alternate_statements =
                    self.convert_branch_blocks(&blocks_to_convert, cfg, block_converter)?;

                // Handle nested chains in else branch
                if let Some(nested_chains) = nested_chains_opt {
                    for nested_chain in nested_chains {
                        let nested_stmts =
                            self.convert_chain(nested_chain, cfg, block_converter)?;
                        alternate_statements.extend(nested_stmts);
                    }
                }

                Some(self.create_block_statement(alternate_statements))
            } else {
                None
            }
        } else {
            None
        };

        let if_stmt = self
            .ast_builder
            .statement_if(Span::default(), test, consequent, alternate);

        Ok(if_stmt)
    }

    /// Convert an if-else-if chain
    fn convert_else_if_chain(
        &mut self,
        chain: &ConditionalChain,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Statement<'a>, String> {
        if chain.branches.is_empty() {
            return Err("No branches in else-if chain".to_string());
        }

        // Build from the end backwards to create nested if-else statements
        let mut current_stmt: Option<Statement<'a>> = None;

        // Check if there's a final else branch
        let has_else = chain
            .branches
            .last()
            .map(|b| b.branch_type == BranchType::Else)
            .unwrap_or(false);

        let conditional_branches = if has_else {
            &chain.branches[..chain.branches.len() - 1]
        } else {
            &chain.branches[..]
        };

        // Process branches in reverse order
        for (i, branch) in conditional_branches.iter().enumerate().rev() {
            // Build the condition expression
            let test = self.build_condition_expression(branch, cfg)?;

            // Get the actual branch index in the original chain
            let actual_branch_idx = conditional_branches.len() - 1 - i;

            // Get blocks to convert, excluding those that belong to nested chains
            let mut blocks_to_convert = branch.branch_blocks.clone();

            // Find nested chains for this branch
            let nested_chains_opt = self.find_nested_chains_for_branch(chain, actual_branch_idx);

            // Exclude blocks that belong to nested chains
            if let Some(ref nested_chains) = nested_chains_opt {
                let mut nested_blocks = std::collections::HashSet::new();
                for nested_chain in nested_chains {
                    // Add all blocks from nested chain
                    for branch in &nested_chain.branches {
                        nested_blocks.insert(branch.condition_block);
                        nested_blocks.extend(&branch.branch_blocks);
                    }
                }
                blocks_to_convert.retain(|&block| !nested_blocks.contains(&block));
            }

            // Convert the consequent blocks
            let mut consequent_statements =
                self.convert_branch_blocks(&blocks_to_convert, cfg, block_converter)?;

            // Handle nested chains in this branch
            if let Some(nested_chains) = nested_chains_opt {
                for nested_chain in nested_chains {
                    let nested_stmts = self.convert_chain(nested_chain, cfg, block_converter)?;
                    consequent_statements.extend(nested_stmts);
                }
            }

            let consequent = self.create_block_statement(consequent_statements);

            // Determine the alternate
            let alternate = if i == conditional_branches.len() - 1 && has_else {
                // This is the last conditional branch and we have an else
                let else_branch = &chain.branches[chain.branches.len() - 1];
                let else_branch_idx = chain.branches.len() - 1;

                // Get blocks to convert, excluding those that belong to nested chains
                let mut blocks_to_convert = else_branch.branch_blocks.clone();

                // Find nested chains for the else branch
                let nested_chains_opt = self.find_nested_chains_for_branch(chain, else_branch_idx);

                // Exclude blocks that belong to nested chains
                if let Some(ref nested_chains) = nested_chains_opt {
                    let mut nested_blocks = std::collections::HashSet::new();
                    for nested_chain in nested_chains {
                        // Add all blocks from nested chain
                        for branch in &nested_chain.branches {
                            nested_blocks.insert(branch.condition_block);
                            nested_blocks.extend(&branch.branch_blocks);
                        }
                    }
                    blocks_to_convert.retain(|&block| !nested_blocks.contains(&block));
                }

                let mut alternate_statements =
                    self.convert_branch_blocks(&blocks_to_convert, cfg, block_converter)?;

                // Handle nested chains in else branch
                if let Some(nested_chains) = nested_chains_opt {
                    for nested_chain in nested_chains {
                        let nested_stmts =
                            self.convert_chain(nested_chain, cfg, block_converter)?;
                        alternate_statements.extend(nested_stmts);
                    }
                }

                Some(self.create_block_statement(alternate_statements))
            } else {
                // Use the previously built statement as the else clause
                current_stmt.take()
            };

            current_stmt =
                Some(
                    self.ast_builder
                        .statement_if(Span::default(), test, consequent, alternate),
                );
        }

        current_stmt.ok_or_else(|| "Failed to build else-if chain".to_string())
    }

    /// Convert guard clauses (early returns)
    fn convert_guard_clauses(
        &mut self,
        chain: &ConditionalChain,
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<Statement<'a>, String> {
        // Guard clauses are typically just simple if statements with early returns
        // For now, treat them as simple if statements
        // TODO: Detect and preserve early return patterns
        self.convert_simple_if_else(chain, cfg, block_converter)
    }

    /// Build a condition expression from a conditional branch
    fn build_condition_expression(
        &mut self,
        branch: &ConditionalBranch,
        cfg: &Cfg<'a>,
    ) -> Result<Expression<'a>, String> {
        let condition_block = &cfg.graph()[branch.condition_block];

        // Get the last instruction (should be a conditional jump)
        let last_instr = condition_block
            .instructions()
            .last()
            .ok_or("Condition block has no instructions")?;

        // Get the edge from condition block to branch entry to determine polarity
        let edges: Vec<_> = cfg
            .graph()
            .edges(branch.condition_block)
            .filter(|e| e.target() == branch.branch_entry)
            .collect();

        if edges.is_empty() {
            return Err("No edge from condition block to branch entry".to_string());
        }

        let edge_kind = edges[0].weight();
        let needs_negation = match edge_kind {
            EdgeKind::True => false,
            EdgeKind::False => true,
            _ => return Err("Expected conditional edge".to_string()),
        };

        // Build the base condition expression
        let base_expr = self.build_condition_from_instruction(&last_instr.instruction)?;

        // Apply negation if needed
        if needs_negation {
            Ok(self.ast_builder.expression_unary(
                Span::default(),
                UnaryOperator::LogicalNot,
                base_expr,
            ))
        } else {
            Ok(base_expr)
        }
    }

    /// Build condition expression from a jump instruction
    fn build_condition_from_instruction(
        &mut self,
        instruction: &UnifiedInstruction,
    ) -> Result<Expression<'a>, String> {
        match instruction {
            // Simple boolean jumps
            UnifiedInstruction::JmpTrue { operand_0, .. } => {
                self.create_register_expression(*operand_0 as u8)
            }
            UnifiedInstruction::JmpTrueLong { operand_0, .. } => {
                self.create_register_expression(*operand_0 as u8)
            }

            UnifiedInstruction::JmpFalse { operand_0, .. } => {
                let expr = self.create_register_expression(*operand_0 as u8)?;
                Ok(self.ast_builder.expression_unary(
                    Span::default(),
                    UnaryOperator::LogicalNot,
                    expr,
                ))
            }
            UnifiedInstruction::JmpFalseLong { operand_0, .. } => {
                let expr = self.create_register_expression(*operand_0 as u8)?;
                Ok(self.ast_builder.expression_unary(
                    Span::default(),
                    UnaryOperator::LogicalNot,
                    expr,
                ))
            }

            // Comparison jumps
            UnifiedInstruction::JEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::Equality),

            UnifiedInstruction::JNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::Inequality),

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
                self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::StrictEquality)
            }

            UnifiedInstruction::JStrictNotEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JStrictNotEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(
                *operand_1,
                *operand_2,
                BinaryOperator::StrictInequality,
            ),

            UnifiedInstruction::JLess {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::LessThan),

            UnifiedInstruction::JLessEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JLessEqualLong {
                operand_1,
                operand_2,
                ..
            } => {
                self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::LessEqualThan)
            }

            UnifiedInstruction::JGreater {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::GreaterThan),

            UnifiedInstruction::JGreaterEqual {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JGreaterEqualLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(
                *operand_1,
                *operand_2,
                BinaryOperator::GreaterEqualThan,
            ),

            // NotGreater is equivalent to LessEqual
            UnifiedInstruction::JNotGreater {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotGreaterLong {
                operand_1,
                operand_2,
                ..
            } => {
                self.build_binary_comparison(*operand_1, *operand_2, BinaryOperator::LessEqualThan)
            }

            // NotLess is equivalent to GreaterEqual
            UnifiedInstruction::JNotLess {
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::JNotLessLong {
                operand_1,
                operand_2,
                ..
            } => self.build_binary_comparison(
                *operand_1,
                *operand_2,
                BinaryOperator::GreaterEqualThan,
            ),

            // Undefined/null checks
            UnifiedInstruction::JmpUndefined { operand_1, .. }
            | UnifiedInstruction::JmpUndefinedLong { operand_1, .. } => {
                let expr = self.create_register_expression(*operand_1)?;
                let undefined = self
                    .ast_builder
                    .expression_identifier(Span::default(), "undefined");
                Ok(self.ast_builder.expression_binary(
                    Span::default(),
                    expr,
                    BinaryOperator::StrictEquality,
                    undefined,
                ))
            }

            _ => Err(format!(
                "Unsupported jump instruction for condition: {:?}",
                instruction
            )),
        }
    }

    /// Build a binary comparison expression
    fn build_binary_comparison(
        &mut self,
        left_reg: u8,
        right_reg: u8,
        operator: BinaryOperator,
    ) -> Result<Expression<'a>, String> {
        let left = self.create_register_expression(left_reg)?;
        let right = self.create_register_expression(right_reg)?;

        Ok(self
            .ast_builder
            .expression_binary(Span::default(), left, operator, right))
    }

    /// Convert multiple branch blocks into statements
    fn convert_branch_blocks(
        &mut self,
        block_indices: &[NodeIndex],
        cfg: &Cfg<'a>,
        block_converter: &mut crate::ast::BlockToStatementConverter<'a>,
    ) -> Result<AllocVec<'a, Statement<'a>>, String> {
        let mut statements = AllocVec::new_in(self.ast_builder.allocator);

        for &block_idx in block_indices {
            let block = &cfg.graph()[block_idx];
            let block_statements = block_converter
                .convert_block(block, block_idx, cfg.graph())
                .map_err(|e| format!("Failed to convert block {}: {:?}", block_idx.index(), e))?;
            statements.extend(block_statements);
        }

        Ok(statements)
    }

    /// Create a block statement from a vector of statements
    fn create_block_statement(&self, statements: AllocVec<'a, Statement<'a>>) -> Statement<'a> {
        self.ast_builder
            .statement_block(Span::default(), statements)
    }

    /// Get all blocks that are part of this conditional chain INCLUDING nested chains
    /// This is used to mark all blocks as processed
    pub fn get_all_chain_blocks(chain: &ConditionalChain) -> HashSet<NodeIndex> {
        let mut blocks = HashSet::new();

        // Recursive function to collect all blocks
        fn collect_all_blocks(chain: &ConditionalChain, blocks: &mut HashSet<NodeIndex>) {
            // Add all blocks from all branches
            for branch in &chain.branches {
                blocks.insert(branch.condition_source);
                blocks.insert(branch.condition_block);
                blocks.insert(branch.branch_entry);
                blocks.extend(&branch.branch_blocks);
            }

            // Add join block
            blocks.insert(chain.join_block);

            // Recursively collect from nested chains
            for nested_chain in &chain.nested_chains {
                collect_all_blocks(nested_chain, blocks);
            }
        }

        collect_all_blocks(chain, &mut blocks);
        blocks
    }

    /// Get all blocks that are part of this conditional chain (excluding nested chains)
    pub fn get_chain_blocks(chain: &ConditionalChain) -> HashSet<NodeIndex> {
        let mut blocks = HashSet::new();

        // First, collect all blocks that belong to nested chains
        let mut nested_blocks = HashSet::new();
        fn collect_nested_blocks_recursive(
            chain: &ConditionalChain,
            nested_blocks: &mut HashSet<NodeIndex>,
        ) {
            for nested_chain in &chain.nested_chains {
                // Add blocks from nested chain
                for branch in &nested_chain.branches {
                    nested_blocks.insert(branch.condition_source);
                    nested_blocks.insert(branch.condition_block);
                    nested_blocks.insert(branch.branch_entry);
                    nested_blocks.extend(&branch.branch_blocks);
                }
                nested_blocks.insert(nested_chain.join_block);

                // Recursively collect from deeper nested chains
                collect_nested_blocks_recursive(nested_chain, nested_blocks);
            }
        }
        collect_nested_blocks_recursive(chain, &mut nested_blocks);

        // Add all blocks from all branches, excluding those in nested chains
        for branch in &chain.branches {
            blocks.insert(branch.condition_source);
            blocks.insert(branch.condition_block);
            if !nested_blocks.contains(&branch.branch_entry) {
                blocks.insert(branch.branch_entry);
            }
            // Only include branch blocks that aren't part of nested chains
            for &block in &branch.branch_blocks {
                if !nested_blocks.contains(&block) {
                    blocks.insert(block);
                }
            }
        }

        // Add join block if it's not part of a nested chain
        if !nested_blocks.contains(&chain.join_block) {
            blocks.insert(chain.join_block);
        }

        blocks
    }

    /// Find nested chains for a specific branch
    fn find_nested_chains_for_branch<'b>(
        &self,
        chain: &'b ConditionalChain,
        branch_idx: usize,
    ) -> Option<Vec<&'b ConditionalChain>> {
        if branch_idx >= chain.branches.len() || chain.nested_chains.is_empty() {
            return None;
        }

        let branch = &chain.branches[branch_idx];
        let branch_entry = branch.branch_entry;

        // Find nested chains that start within this branch's blocks
        let mut branch_nested_chains = Vec::new();
        for nested in &chain.nested_chains {
            let nested_start = nested.branches.first().map(|b| b.condition_block);
            // Check if the nested chain starts at this branch's entry or within its blocks
            if nested_start == Some(branch_entry)
                || (nested_start.is_some() && branch.branch_blocks.contains(&nested_start.unwrap()))
            {
                branch_nested_chains.push(nested);
            }
        }

        if branch_nested_chains.is_empty() {
            None
        } else {
            Some(branch_nested_chains)
        }
    }

    /// Create an expression for a register using a simple naming scheme
    /// This is called after condition setup blocks have been processed,
    /// so we use the same naming convention as the block converter
    fn create_register_expression(&mut self, register: u8) -> Result<Expression<'a>, String> {
        // For now, use a simple var{N} naming scheme that matches what the block converter uses
        // TODO: Integrate with proper variable mapping system
        let var_name = if register == 0 {
            "var0".to_string()
        } else {
            format!("var{}", register)
        };

        Ok(self
            .ast_builder
            .expression_identifier(Span::default(), self.ast_builder.atom(&var_name)))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_conditional_converter_creation() {
        // TODO: Add unit tests for conditional converter
    }
}
