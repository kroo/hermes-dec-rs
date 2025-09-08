//! Jump instruction handlers
//!
//! This module handles all jump-related instructions that control program flow.
//! Jump instructions don't generate statements but provide condition information
//! for the block converter to use in control flow analysis.

use super::{
    InstructionResult, InstructionToStatementConverter, JumpCondition, JumpType,
    StatementConversionError,
};
use oxc_span::Span;

/// Trait providing jump condition builder methods
pub trait JumpHelpers<'a> {
    /// Build a comparison-based jump condition (e.g., JGreater, JLess, etc.)
    fn build_comparison_jump(
        &mut self,
        left_reg: u8,
        right_reg: u8,
        comparison_op: &str,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Build a truthiness-based jump condition (JmpTrue, JmpFalse)
    fn build_truthiness_jump(
        &mut self,
        condition_reg: u8,
        jump_type: JumpType,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Build an unconditional jump
    fn build_unconditional_jump(
        &mut self,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Build an undefined check jump
    fn build_undefined_jump(
        &mut self,
        condition_reg: u8,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> JumpHelpers<'a> for InstructionToStatementConverter<'a> {
    fn build_comparison_jump(
        &mut self,
        left_reg: u8,
        right_reg: u8,
        comparison_op: &str,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Use register_to_expression to handle inlining properly
        let left_expr = self.register_to_expression(left_reg)?;
        let right_expr = self.register_to_expression(right_reg)?;

        // Map comparison operation to binary operator
        let binary_op = match comparison_op {
            "Greater" => oxc_ast::ast::BinaryOperator::GreaterThan,
            "GreaterEqual" => oxc_ast::ast::BinaryOperator::GreaterEqualThan,
            "Less" => oxc_ast::ast::BinaryOperator::LessThan,
            "LessEqual" => oxc_ast::ast::BinaryOperator::LessEqualThan,
            "Equal" => oxc_ast::ast::BinaryOperator::Equality,
            "NotEqual" => oxc_ast::ast::BinaryOperator::Inequality,
            "StrictEqual" => oxc_ast::ast::BinaryOperator::StrictEquality,
            "StrictNotEqual" => oxc_ast::ast::BinaryOperator::StrictInequality,
            // NotGreater is equivalent to LessEqual (!(a > b) === a <= b)
            "NotGreater" => oxc_ast::ast::BinaryOperator::LessEqualThan,
            // NotLess is equivalent to GreaterEqual (!(a < b) === a >= b)
            "NotLess" => oxc_ast::ast::BinaryOperator::GreaterEqualThan,
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(format!(
                    "Unknown comparison operator: {}",
                    comparison_op
                )))
            }
        };

        // Create binary expression
        let condition_expr = self
            .ast_builder
            .expression_binary(span, left_expr, binary_op, right_expr);

        Ok(InstructionResult::JumpCondition(JumpCondition {
            condition_expression: Some(condition_expr),
            jump_type: JumpType::Conditional,
            target_offset: Some(target_offset),
        }))
    }

    fn build_truthiness_jump(
        &mut self,
        condition_reg: u8,
        jump_type: JumpType,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Use register_to_expression to handle inlining properly
        let condition_expr = self.register_to_expression(condition_reg)?;

        Ok(InstructionResult::JumpCondition(JumpCondition {
            condition_expression: Some(condition_expr),
            jump_type,
            target_offset: Some(target_offset),
        }))
    }

    fn build_unconditional_jump(
        &mut self,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        Ok(InstructionResult::JumpCondition(JumpCondition {
            condition_expression: None, // No condition for unconditional jumps
            jump_type: JumpType::Unconditional,
            target_offset: Some(target_offset),
        }))
    }

    fn build_undefined_jump(
        &mut self,
        condition_reg: u8,
        target_offset: i32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Use register_to_expression to handle inlining properly
        let condition_expr = self.register_to_expression(condition_reg)?;

        // Create undefined expression
        let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
        let undefined_expr = self.ast_builder.expression_identifier(span, undefined_atom);

        // Create comparison: condition === undefined
        let comparison_expr = self.ast_builder.expression_binary(
            span,
            condition_expr,
            oxc_ast::ast::BinaryOperator::StrictEquality,
            undefined_expr,
        );

        Ok(InstructionResult::JumpCondition(JumpCondition {
            condition_expression: Some(comparison_expr),
            jump_type: JumpType::Undefined,
            target_offset: Some(target_offset),
        }))
    }
}
