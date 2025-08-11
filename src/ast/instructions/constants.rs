//! Constant loading helper methods
//!
//! This module provides helper methods for creating constant loading statements.
//! These handle literal values (numbers, strings, booleans, null, undefined, etc.).

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_ast::ast::VariableDeclarationKind;

use oxc_span::Span;

/// Trait providing constant loading helper methods
pub trait ConstantHelpers<'a> {
    /// Create a numeric constant assignment: `let var0_1 = 42;`
    fn create_constant_assignment(
        &mut self,
        dest_reg: u8,
        value: f64,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a boolean assignment: `let var0_1 = true;`
    fn create_boolean_assignment(
        &mut self,
        dest_reg: u8,
        value: bool,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a null assignment: `let var0_1 = null;`
    fn create_null_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create an undefined assignment: `let var0_1 = undefined;`
    fn create_undefined_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a string assignment: `let var0_1 = "hello";`
    fn create_string_assignment(
        &mut self,
        dest_reg: u8,
        string_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a BigInt assignment: `let var0_1 = 123n;`
    fn create_bigint_assignment(
        &mut self,
        dest_reg: u8,
        bigint_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create empty value assignment: `let var0_1 = /* empty */;`
    fn create_empty_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create 8-bit integer load: `let var0_1 = __uasm.loadi8(array, addr);`
    fn create_loadi8(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create 32-bit integer load: `let var0_1 = __uasm.loadi32(array, addr);`
    fn create_loadi32(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create unsigned 8-bit integer load: `let var0_1 = __uasm.loadu8(array, addr);`
    fn create_loadu8(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create unsigned 16-bit integer load: `let var0_1 = __uasm.loadu16(array, addr);`
    fn create_loadu16(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create 8-bit integer store: `__uasm.store8(array, addr, value);`
    fn create_store8(
        &mut self,
        array_reg: u8,
        addr_reg: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create 16-bit integer store: `__uasm.store16(array, addr, value);`
    fn create_store16(
        &mut self,
        array_reg: u8,
        addr_reg: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> ConstantHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_constant_assignment(
        &mut self,
        dest_reg: u8,
        value: f64,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let value_expr = self.ast_builder.expression_numeric_literal(
            span,
            value,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let stmt = self.create_variable_declaration(
            &dest_var,
            Some(value_expr),
            VariableDeclarationKind::Let,
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_boolean_assignment(
        &mut self,
        dest_reg: u8,
        value: bool,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let value_expr = self.ast_builder.expression_boolean_literal(span, value);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(value_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_null_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let value_expr = self.ast_builder.expression_null_literal(span);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(value_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_undefined_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
        let undefined_expr = self.ast_builder.expression_identifier(span, undefined_atom);

        let stmt =
            self.create_variable_declaration_or_assignment(&dest_var, Some(undefined_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_string_assignment(
        &mut self,
        dest_reg: u8,
        string_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        // Look up string from the string table
        let string_value = self.expression_context.lookup_string(string_id)?;

        let span = Span::default();
        let string_atom = self.ast_builder.allocator.alloc_str(&string_value);
        let string_expr = self
            .ast_builder
            .expression_string_literal(span, string_atom, None);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(string_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_bigint_assignment(
        &mut self,
        dest_reg: u8,
        bigint_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        // Look up BigInt from the BigInt table
        let bigint_value = self.expression_context.lookup_bigint(bigint_id)?;

        let span = Span::default();
        let bigint_atom = self.ast_builder.allocator.alloc_str(&bigint_value);
        let bigint_expr = self.ast_builder.expression_big_int_literal(
            span,
            bigint_atom,
            None,
            oxc_syntax::number::BigintBase::Decimal,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(bigint_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create empty value assignment: `let var0_1 = /* empty */;`
    fn create_empty_assignment(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create a comment expression representing the empty value
        let empty_comment = "/* empty */";
        let comment_atom = self.ast_builder.allocator.alloc_str(empty_comment);
        let empty_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(empty_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_loadi8(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);

        let span = Span::default();

        // Create __uasm.loadi8(array, addr) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("loadi8");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_loadi32(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);

        let span = Span::default();

        // Create __uasm.loadi32(array, addr) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("loadi32");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_loadu8(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);

        let span = Span::default();

        // Create __uasm.loadu8(array, addr) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("loadu8");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_loadu16(
        &mut self,
        dest_reg: u8,
        array_reg: u8,
        addr_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);

        let span = Span::default();

        // Create __uasm.loadu16(array, addr) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("loadu16");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_store8(
        &mut self,
        array_reg: u8,
        addr_reg: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create __uasm.store8(array, addr, value) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("store8");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));
        arguments.push(oxc_ast::ast::Argument::from(value_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.ast_builder.statement_expression(span, call_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_store16(
        &mut self,
        array_reg: u8,
        addr_reg: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let array_var = self.register_manager.get_variable_name(array_reg);
        let addr_var = self.register_manager.get_variable_name(addr_reg);
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create __uasm.store16(array, addr, value) call
        let uasm_atom = self.ast_builder.allocator.alloc_str("__uasm");
        let uasm_expr = self.ast_builder.expression_identifier(span, uasm_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("store16");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, uasm_expr, method_name, false);

        let array_atom = self.ast_builder.allocator.alloc_str(&array_var);
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let addr_atom = self.ast_builder.allocator.alloc_str(&addr_var);
        let addr_expr = self.ast_builder.expression_identifier(span, addr_atom);

        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(array_expr));
        arguments.push(oxc_ast::ast::Argument::from(addr_expr));
        arguments.push(oxc_ast::ast::Argument::from(value_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.ast_builder.statement_expression(span, call_expr);
        Ok(InstructionResult::Statement(stmt))
    }
}
