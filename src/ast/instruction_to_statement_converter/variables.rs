//! Variable and register operation helper methods
//!
//! This module provides helper methods for creating variable-related statements.
//! These handle register movements, variable assignments, and heap operations.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_ast::ast::VariableDeclarationKind;
use oxc_span::Span;

/// Trait providing variable operation helper methods
pub trait VariableHelpers<'a> {
    /// Create register assignment: `let var0_1 = var1_0;`
    fn create_register_assignment(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create heap load operation: `let var0_1 = heap[offset];`
    fn create_heap_load(
        &mut self,
        dest_reg: u8,
        offset: u32,
        type_hint: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create heap store operation: `heap[offset] = value;`
    fn create_heap_store(
        &mut self,
        value_reg: u8,
        offset: u32,
        _type_hint: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create type conversion: `let var0_1 = Number(value);`
    fn create_type_conversion(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
        conversion_type: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;


    /// Create global object access: `let var0_1 = globalThis;`
    fn create_get_global_object(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Declare global variable: `globalThis.varName = undefined;`
    fn create_declare_global_var(
        &mut self,
        name_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create environment (placeholder comment)
    fn create_environment(
        &mut self,
        env_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Store to environment (placeholder comment)
    fn create_store_to_environment(
        &mut self,
        env_reg: u8,
        index: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create inner environment: `let var0_1 = createInnerEnv(env, size);` 
    fn create_inner_environment(
        &mut self,
        dest_reg: u8,
        parent_env_reg: u8,
        size: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Get environment: `let var0_1 = getCurrentEnv(level);`
    fn create_get_environment(
        &mut self,
        dest_reg: u8,
        level: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Load from environment: `let var0_1 = env.getVar(index);`
    fn create_load_from_environment(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Load from environment with long index: `let var0_1 = env.getVar(longIndex);`
    fn create_load_from_environment_long(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Store to environment with long index: `env.setVar(longIndex, value);`
    fn create_store_to_environment_long(
        &mut self,
        env_reg: u8,
        index: u32,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Store non-pointer to environment: `env.setVarNP(index, value);`
    fn create_store_np_to_environment(
        &mut self,
        env_reg: u8,
        index: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Store non-pointer to environment with long index: `env.setVarNP(longIndex, value);`
    fn create_store_np_to_environment_long(
        &mut self,
        env_reg: u8,
        index: u32,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> VariableHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_register_assignment(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let src_var = self.register_manager.get_variable_name(src_reg);

        let span = Span::default();
        let src_atom = self.ast_builder.allocator.alloc_str(&src_var);
        let src_expr = self.ast_builder.expression_identifier(span, src_atom);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(src_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_heap_load(
        &mut self,
        dest_reg: u8,
        offset: u32,
        type_hint: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create heap access expression: heap[offset]
        let heap_atom = self.ast_builder.allocator.alloc_str("heap");
        let heap_expr = self.ast_builder.expression_identifier(span, heap_atom);

        let offset_expr = self.ast_builder.expression_numeric_literal(
            span,
            offset as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let member_expr =
            self.ast_builder
                .alloc_computed_member_expression(span, heap_expr, offset_expr, false);

        // Add type hint as comment if needed
        let comment_text = format!("/* {} */", type_hint);
        let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
        let _comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self
            .create_variable_declaration(
                &dest_var,
                Some(oxc_ast::ast::Expression::ComputedMemberExpression(
                    member_expr,
                )),
                VariableDeclarationKind::Let,
            )
?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_heap_store(
        &mut self,
        value_reg: u8,
        offset: u32,
        _type_hint: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create heap access expression: heap[offset]
        let heap_atom = self.ast_builder.allocator.alloc_str("heap");
        let heap_expr = self.ast_builder.expression_identifier(span, heap_atom);

        let offset_expr = self.ast_builder.expression_numeric_literal(
            span,
            offset as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let member_expr =
            self.ast_builder
                .alloc_computed_member_expression(span, heap_expr, offset_expr, false);

        // Create value expression
        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        // Create assignment statement
        let assignment_expr = self.ast_builder.expression_assignment(
            span,
            oxc_ast::ast::AssignmentOperator::Assign,
            oxc_ast::ast::AssignmentTarget::ComputedMemberExpression(member_expr),
            value_expr,
        );

        let stmt = self.ast_builder.statement_expression(span, assignment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_type_conversion(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
        conversion_type: &str,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let src_var = self.register_manager.get_variable_name(src_reg);

        let span = Span::default();

        // Create conversion function call
        let func_atom = self.ast_builder.allocator.alloc_str(conversion_type);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        let src_atom = self.ast_builder.allocator.alloc_str(&src_var);
        let src_expr = self.ast_builder.expression_identifier(span, src_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(src_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            func_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }


    /// Create global object access: `let var0_1 = globalThis;`
    fn create_get_global_object(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create globalThis identifier
        let global_atom = self.ast_builder.allocator.alloc_str("globalThis");
        let global_expr = self.ast_builder.expression_identifier(span, global_atom);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(global_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Declare global variable: `globalThis.varName = undefined;`
    fn create_declare_global_var(
        &mut self,
        name_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Look up variable name from string table
        let var_name = self.expression_context.lookup_string(name_id)?;

        let span = Span::default();

        // Create globalThis identifier
        let global_atom = self.ast_builder.allocator.alloc_str("globalThis");
        let global_expr = self.ast_builder.expression_identifier(span, global_atom);

        // Create property access for assignment target
        let prop_atom = self.ast_builder.allocator.alloc_str(&var_name);
        let property_name = self.ast_builder.identifier_name(span, prop_atom);
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span,
            global_expr,
            property_name,
            false,
        );

        // Create undefined expression
        let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
        let undefined_expr = self.ast_builder.expression_identifier(span, undefined_atom);

        // Create assignment statement
        let assignment_expr = self.ast_builder.expression_assignment(
            span,
            oxc_ast::ast::AssignmentOperator::Assign,
            oxc_ast::ast::AssignmentTarget::StaticMemberExpression(member_expr),
            undefined_expr,
        );

        let stmt = self.ast_builder.statement_expression(span, assignment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    /// Create environment (placeholder comment)
    fn create_environment(
        &mut self,
        _env_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Environment creation is a runtime concept that doesn't directly translate to JS
        // We'll return a comment for now
        let span = Span::default();
        let comment_text = "/* CREATE_ENVIRONMENT */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    /// Store to environment (placeholder comment)
    fn create_store_to_environment(
        &mut self,
        _env_reg: u8,
        _index: u8,
        _value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Environment storage is a runtime concept that doesn't directly translate to JS
        // We'll return a comment for now
        let span = Span::default();
        let comment_text = "/* STORE_TO_ENVIRONMENT */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    /// Create inner environment: `let var0_1 = createInnerEnv(env, size);` 
    fn create_inner_environment(
        &mut self,
        dest_reg: u8,
        parent_env_reg: u8,
        size: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let parent_env_var = self.register_manager.get_variable_name(parent_env_reg);

        let span = Span::default();

        // Create createInnerEnv function call
        let func_atom = self.ast_builder.allocator.alloc_str("createInnerEnv");
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Create arguments: parent environment and size
        let parent_atom = self.ast_builder.allocator.alloc_str(&parent_env_var);
        let parent_expr = self.ast_builder.expression_identifier(span, parent_atom);

        let size_expr = self.ast_builder.expression_numeric_literal(
            span,
            size as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(parent_expr));
        arguments.push(oxc_ast::ast::Argument::from(size_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            func_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Get environment: `let var0_1 = getCurrentEnv(level);`
    fn create_get_environment(
        &mut self,
        dest_reg: u8,
        level: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create getCurrentEnv function call
        let func_atom = self.ast_builder.allocator.alloc_str("getCurrentEnv");
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Create level argument
        let level_expr = self.ast_builder.expression_numeric_literal(
            span,
            level as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(level_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            func_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Load from environment: `let var0_1 = env.getVar(index);`
    fn create_load_from_environment(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let env_var = self.register_manager.get_variable_name(env_reg);

        let span = Span::default();

        // Create env.getVar(index) call
        let env_atom = self.ast_builder.allocator.alloc_str(&env_var);
        let env_expr = self.ast_builder.expression_identifier(span, env_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("getVar");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span, env_expr, method_name, false
        );

        // Create index argument
        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(index_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Load from environment with long index: `let var0_1 = env.getVar(longIndex);`
    fn create_load_from_environment_long(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let env_var = self.register_manager.get_variable_name(env_reg);

        let span = Span::default();

        // Create env.getVar(index) call
        let env_atom = self.ast_builder.allocator.alloc_str(&env_var);
        let env_expr = self.ast_builder.expression_identifier(span, env_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("getVar");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span, env_expr, method_name, false
        );

        // Create index argument
        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(index_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Store to environment with long index: `env.setVar(longIndex, value);`
    fn create_store_to_environment_long(
        &mut self,
        env_reg: u8,
        index: u32,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let env_var = self.register_manager.get_variable_name(env_reg);
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create env.setVar(index, value) call
        let env_atom = self.ast_builder.allocator.alloc_str(&env_var);
        let env_expr = self.ast_builder.expression_identifier(span, env_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("setVar");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span, env_expr, method_name, false
        );

        // Create arguments: index and value
        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(index_expr));
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

    /// Store non-pointer to environment: `env.setVarNP(index, value);`
    fn create_store_np_to_environment(
        &mut self,
        env_reg: u8,
        index: u8,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let env_var = self.register_manager.get_variable_name(env_reg);
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create env.setVarNP(index, value) call
        let env_atom = self.ast_builder.allocator.alloc_str(&env_var);
        let env_expr = self.ast_builder.expression_identifier(span, env_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("setVarNP");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span, env_expr, method_name, false
        );

        // Create arguments: index and value
        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(index_expr));
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

    /// Store non-pointer to environment with long index: `env.setVarNP(longIndex, value);`
    fn create_store_np_to_environment_long(
        &mut self,
        env_reg: u8,
        index: u32,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let env_var = self.register_manager.get_variable_name(env_reg);
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create env.setVarNP(index, value) call
        let env_atom = self.ast_builder.allocator.alloc_str(&env_var);
        let env_expr = self.ast_builder.expression_identifier(span, env_atom);

        let method_atom = self.ast_builder.allocator.alloc_str("setVarNP");
        let method_name = self.ast_builder.identifier_name(span, method_atom);
        
        let member_expr = self.ast_builder.alloc_static_member_expression(
            span, env_expr, method_name, false
        );

        // Create arguments: index and value
        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(index_expr));
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
