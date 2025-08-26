//! Arithmetic and bitwise operation helper methods
//!
//! This module provides helper methods for creating arithmetic and bitwise
//! operation statements. These are used by the main instruction converter.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_span::Span;

/// Trait providing arithmetic operation helper methods
pub trait ArithmeticHelpers<'a> {
    /// Create a binary operation statement: `let var0_1 = left op right;`
    fn create_binary_operation(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a unary operation: `let var0_1 = !var1_0;`
    fn create_unary_operation(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_update_operation(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_comparison(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> ArithmeticHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_binary_operation(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Check if we should inline the operands based on use strategies
        let current_pc = self.register_manager.current_pc()
            .ok_or(StatementConversionError::NoCurrentPC)?;
        let current_block = self.register_manager.current_block()
            .ok_or(StatementConversionError::NoCurrentBlock)?;
        
        // Get source operand names using the "before" lookup to avoid self-references
        let left_var = self.register_manager.get_source_variable_name(left_reg);
        let right_var = self.register_manager.get_source_variable_name(right_reg);

        // Now create the new variable for the destination
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create operand expressions, checking for inline values
        let left_expr = if let Some(ssa_value) = self.register_manager.get_current_ssa_for_register(left_reg) {
            let use_site = crate::cfg::ssa::RegisterUse {
                register: left_reg,
                block_id: current_block,
                instruction_idx: current_pc,
            };
            let dup_value = crate::cfg::ssa::DuplicatedSSAValue::original(ssa_value.clone());
            
            if let Some(strategy) = self.control_flow_plan.use_strategies.get(&(dup_value, use_site)) {
                match strategy {
                    crate::analysis::ssa_usage_tracker::UseStrategy::InlineValue(constant) => {
                        // Create a constant expression
                        self.create_constant_expression(constant)?
                    }
                    crate::analysis::ssa_usage_tracker::UseStrategy::UseVariable => {
                        let left_atom = self.ast_builder.allocator.alloc_str(&left_var);
                        self.ast_builder.expression_identifier(span, left_atom)
                    }
                }
            } else {
                let left_atom = self.ast_builder.allocator.alloc_str(&left_var);
                self.ast_builder.expression_identifier(span, left_atom)
            }
        } else {
            let left_atom = self.ast_builder.allocator.alloc_str(&left_var);
            self.ast_builder.expression_identifier(span, left_atom)
        };

        let right_expr = if let Some(ssa_value) = self.register_manager.get_current_ssa_for_register(right_reg) {
            let use_site = crate::cfg::ssa::RegisterUse {
                register: right_reg,
                block_id: current_block,
                instruction_idx: current_pc,
            };
            let dup_value = crate::cfg::ssa::DuplicatedSSAValue::original(ssa_value.clone());
            
            if let Some(strategy) = self.control_flow_plan.use_strategies.get(&(dup_value, use_site)) {
                match strategy {
                    crate::analysis::ssa_usage_tracker::UseStrategy::InlineValue(constant) => {
                        // Create a constant expression
                        self.create_constant_expression(constant)?
                    }
                    crate::analysis::ssa_usage_tracker::UseStrategy::UseVariable => {
                        let right_atom = self.ast_builder.allocator.alloc_str(&right_var);
                        self.ast_builder.expression_identifier(span, right_atom)
                    }
                }
            } else {
                let right_atom = self.ast_builder.allocator.alloc_str(&right_var);
                self.ast_builder.expression_identifier(span, right_atom)
            }
        } else {
            let right_atom = self.ast_builder.allocator.alloc_str(&right_var);
            self.ast_builder.expression_identifier(span, right_atom)
        };

        // Create binary expression
        let binary_op = match op_name {
            "Add" => oxc_ast::ast::BinaryOperator::Addition,
            "Sub" => oxc_ast::ast::BinaryOperator::Subtraction,
            "Mul" => oxc_ast::ast::BinaryOperator::Multiplication,
            "Div" => oxc_ast::ast::BinaryOperator::Division,
            "Mod" => oxc_ast::ast::BinaryOperator::Remainder,
            "BitOr" => oxc_ast::ast::BinaryOperator::BitwiseOR,
            "BitAnd" => oxc_ast::ast::BinaryOperator::BitwiseAnd,
            "BitXor" => oxc_ast::ast::BinaryOperator::BitwiseXOR,
            "LShift" => oxc_ast::ast::BinaryOperator::ShiftLeft,
            "RShift" => oxc_ast::ast::BinaryOperator::ShiftRight,
            "URShift" => oxc_ast::ast::BinaryOperator::ShiftRightZeroFill,
            // Specialized operations that we'll implement as regular binary ops for now
            "Add32" | "AddN" => oxc_ast::ast::BinaryOperator::Addition,
            "DivN" | "Divi32" | "Divu32" => oxc_ast::ast::BinaryOperator::Division,
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(format!(
                    "Binary operation: {}",
                    op_name
                )))
            }
        };

        let binary_expr = self
            .ast_builder
            .expression_binary(span, left_expr, binary_op, right_expr);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(binary_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create a unary operation: `let var0_1 = !var1_0;`
    fn create_unary_operation(
        &mut self,
        dest_reg: u8,
        operand_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let operand_var = self.register_manager.get_source_variable_name(operand_reg);

        let span = Span::default();

        // Create operand expression
        let operand_atom = self.ast_builder.allocator.alloc_str(&operand_var);
        let operand_expr = self.ast_builder.expression_identifier(span, operand_atom);

        // Create unary expression
        let unary_op = match op_name {
            "Not" => oxc_ast::ast::UnaryOperator::LogicalNot,
            "BitNot" => oxc_ast::ast::UnaryOperator::BitwiseNot,
            "Negate" => oxc_ast::ast::UnaryOperator::UnaryNegation,
            "TypeOf" => oxc_ast::ast::UnaryOperator::Typeof,
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(format!(
                    "Unary operation: {}",
                    op_name
                )))
            }
        };

        let unary_expr = self
            .ast_builder
            .expression_unary(span, unary_op, operand_expr);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(unary_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create an update operation: `let var0_1 = ++var1_0;`
    fn create_update_operation(
        &mut self,
        dest_reg: u8,
        operand_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let operand_var = self.register_manager.get_source_variable_name(operand_reg);

        let span = Span::default();

        // Create operand expression
        let operand_atom = self.ast_builder.allocator.alloc_str(&operand_var);
        let operand_expr = self.ast_builder.expression_identifier(span, operand_atom);

        // Create update expression
        let update_op = match op_name {
            "Inc" => oxc_ast::ast::UpdateOperator::Increment,
            "Dec" => oxc_ast::ast::UpdateOperator::Decrement,
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(format!(
                    "Update operation: {}",
                    op_name
                )))
            }
        };

        // Create simple assignment target from identifier
        let assignment_target = match operand_expr {
            oxc_ast::ast::Expression::Identifier(id_ref) => {
                oxc_ast::ast::SimpleAssignmentTarget::AssignmentTargetIdentifier(id_ref)
            }
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(
                    "Update operations only support identifiers".to_string(),
                ))
            }
        };

        let update_expr = self.ast_builder.expression_update(
            span,
            update_op,
            true, // prefix
            assignment_target,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(update_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create comparison operation: `let var0_1 = left < right;`
    fn create_comparison(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
        op_name: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let left_var = self.register_manager.get_source_variable_name(left_reg);
        let right_var = self.register_manager.get_source_variable_name(right_reg);

        let span = Span::default();

        // Create operand expressions
        let left_atom = self.ast_builder.allocator.alloc_str(&left_var);
        let left_expr = self.ast_builder.expression_identifier(span, left_atom);

        let right_atom = self.ast_builder.allocator.alloc_str(&right_var);
        let right_expr = self.ast_builder.expression_identifier(span, right_atom);

        // Create comparison expression
        let comparison_op = match op_name {
            "Less" => oxc_ast::ast::BinaryOperator::LessThan,
            "Greater" => oxc_ast::ast::BinaryOperator::GreaterThan,
            "Eq" => oxc_ast::ast::BinaryOperator::Equality,
            "StrictEq" => oxc_ast::ast::BinaryOperator::StrictEquality,
            _ => {
                return Err(StatementConversionError::UnsupportedInstruction(format!(
                    "Comparison operation: {}",
                    op_name
                )))
            }
        };

        let comparison_expr =
            self.ast_builder
                .expression_binary(span, left_expr, comparison_op, right_expr);

        let stmt =
            self.create_variable_declaration_or_assignment(&dest_var, Some(comparison_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }
}
