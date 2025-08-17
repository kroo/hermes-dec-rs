//! Function-related helper methods
//!
//! This module provides helper methods for creating function-related statements.
//! These handle function calls, returns, and parameter loading operations.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use crate::ast::context::ExpressionContext;
use crate::hbc::InstructionIndex;
use oxc_ast::ast::Statement;
use oxc_span::Span;
use std::sync::Arc;

/// Trait providing function operation helper methods
pub trait FunctionHelpers<'a> {
    /// Create a function call: `let var0_1 = func(arg1, arg2, ...);`
    fn create_function_call(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a function call with 1 argument: `let var0_1 = func(arg1);`
    fn create_function_call_1(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a function call with 2 arguments: `let var0_1 = func(arg1, arg2);`
    fn create_function_call_2(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a function call with 3 arguments: `let var0_1 = func(arg1, arg2, arg3);`
    fn create_function_call_3(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
        arg3_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a function call with 4 arguments: `let var0_1 = func(arg1, arg2, arg3, arg4);`
    fn create_function_call_4(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
        arg3_reg: u8,
        arg4_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a return statement: `return value;`
    fn create_return_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a throw statement: `throw value;`
    fn create_throw_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create parameter load: `let var0_1 = arguments[index];`
    fn create_parameter_load(
        &mut self,
        dest_reg: u8,
        param_index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create closure: `let var0_1 = function_1;` (placeholder)
    fn create_closure(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create async closure: `let var0_1 = async_function_6;` (placeholder)
    fn create_async_closure(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create builtin function call: `let var0_1 = console.log(arg1, arg2);`
    fn create_builtin_call(
        &mut self,
        dest_reg: u8,
        builtin_id: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create constructor call: `let var0_1 = new Constructor(arg1, arg2);`
    fn create_constructor_call(
        &mut self,
        dest_reg: u8,
        constructor_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create direct eval call: `let var0_1 = eval(code);`
    fn create_direct_eval(
        &mut self,
        dest_reg: u8,
        code_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create closure with long index: `let var0_1 = function largeFunctionName() { ... };`
    fn create_closure_long_index(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create async closure with long index: `let var0_1 = async function largeFunctionName() { ... };`
    fn create_async_closure_long_index(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create generator closure: `let var0_1 = function* generatorName() { ... };`
    fn create_generator_closure(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create generator closure with long index: `let var0_1 = function* generatorName() { ... };`
    fn create_generator_closure_long_index(
        &mut self,
        dest_reg: u8,
        env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Get arguments length: `let var0_1 = arguments.length;`
    fn create_get_arguments_length(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Get arguments property by value: `let var0_1 = arguments[prop];`
    fn create_get_arguments_prop_by_val(
        &mut self,
        dest_reg: u8,
        prop_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Reify arguments: `let var0_1 = Array.from(arguments);`
    fn create_reify_arguments(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create builtin closure reference: `let var0_1 = /* builtin closure */;`
    fn create_get_builtin_closure(
        &mut self,
        dest_reg: u8,
        builtin_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> FunctionHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_function_call(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Read all variable names BEFORE creating destination variable to avoid conflicts
        let func_var = self.register_manager.get_source_variable_name(func_reg);

        // Collect argument variable names
        let mut arg_vars = Vec::new();
        for i in 0..arg_count {
            let arg_reg = func_reg + 1 + i;
            let arg_var = self.register_manager.get_source_variable_name(arg_reg);
            arg_vars.push(arg_var);
        }

        // Now create the destination variable (this updates register mappings)
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create function expression
        let func_atom = self.ast_builder.allocator.alloc_str(&func_var);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Handle Hermes calling convention: always use func.call(thisArg, ...args)
        let call_expr = if arg_vars.is_empty() {
            // No arguments, regular function call
            self.ast_builder.expression_call(
                span,
                func_expr,
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                self.ast_builder.vec(),
                false,
            )
        } else {
            // Use .call() syntax: func.call(thisArg, arg1, arg2, ...)
            let call_atom = self.ast_builder.allocator.alloc_str("call");
            let call_name = self.ast_builder.identifier_name(span, call_atom);
            let member_expr = self
                .ast_builder
                .alloc_static_member_expression(span, func_expr, call_name, false);

            let mut arguments = self.ast_builder.vec();
            // Add all arguments (first is 'this', rest are actual arguments)
            for arg_var in arg_vars {
                let arg_atom = self.ast_builder.allocator.alloc_str(&arg_var);
                let arg_expr = self.ast_builder.expression_identifier(span, arg_atom);
                arguments.push(oxc_ast::ast::Argument::from(arg_expr));
            }

            self.ast_builder.expression_call(
                span,
                oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                arguments,
                false,
            )
        };

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_function_call_1(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Read all variable names BEFORE creating destination variable
        let func_var = self.register_manager.get_source_variable_name(func_reg);
        let arg1_var = self.register_manager.get_source_variable_name(arg1_reg);

        // Now create the destination variable
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create function expression
        let func_atom = self.ast_builder.allocator.alloc_str(&func_var);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Use .call() syntax: func.call(thisArg)
        let call_atom = self.ast_builder.allocator.alloc_str("call");
        let call_name = self.ast_builder.identifier_name(span, call_atom);
        let member_expr = self
            .ast_builder
            .alloc_static_member_expression(span, func_expr, call_name, false);

        let mut arguments = self.ast_builder.vec();
        // arg1 is the 'this' argument in Hermes calling convention
        let arg1_atom = self.ast_builder.allocator.alloc_str(&arg1_var);
        let arg1_expr = self.ast_builder.expression_identifier(span, arg1_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg1_expr));

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

    fn create_function_call_2(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Read all variable names BEFORE creating destination variable
        let func_var = self.register_manager.get_source_variable_name(func_reg);
        let arg1_var = self.register_manager.get_source_variable_name(arg1_reg);
        let arg2_var = self.register_manager.get_source_variable_name(arg2_reg);

        // Now create the destination variable
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create function expression
        let func_atom = self.ast_builder.allocator.alloc_str(&func_var);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Use .call() syntax: func.call(thisArg, arg1)
        let call_atom = self.ast_builder.allocator.alloc_str("call");
        let call_name = self.ast_builder.identifier_name(span, call_atom);
        let member_expr = self
            .ast_builder
            .alloc_static_member_expression(span, func_expr, call_name, false);

        let mut arguments = self.ast_builder.vec();
        // arg1 is 'this', arg2 is the first actual argument
        let arg1_atom = self.ast_builder.allocator.alloc_str(&arg1_var);
        let arg1_expr = self.ast_builder.expression_identifier(span, arg1_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg1_expr));

        let arg2_atom = self.ast_builder.allocator.alloc_str(&arg2_var);
        let arg2_expr = self.ast_builder.expression_identifier(span, arg2_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg2_expr));

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

    fn create_function_call_3(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
        arg3_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Read all variable names BEFORE creating destination variable
        let func_var = self.register_manager.get_source_variable_name(func_reg);
        let arg1_var = self.register_manager.get_source_variable_name(arg1_reg);
        let arg2_var = self.register_manager.get_source_variable_name(arg2_reg);
        let arg3_var = self.register_manager.get_source_variable_name(arg3_reg);

        // Now create the destination variable
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create function expression
        let func_atom = self.ast_builder.allocator.alloc_str(&func_var);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Use .call() syntax: func.call(thisArg, arg1, arg2)
        let call_atom = self.ast_builder.allocator.alloc_str("call");
        let call_name = self.ast_builder.identifier_name(span, call_atom);
        let member_expr = self
            .ast_builder
            .alloc_static_member_expression(span, func_expr, call_name, false);

        let mut arguments = self.ast_builder.vec();
        // arg1 is 'this', arg2 and arg3 are actual arguments
        let arg1_atom = self.ast_builder.allocator.alloc_str(&arg1_var);
        let arg1_expr = self.ast_builder.expression_identifier(span, arg1_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg1_expr));

        let arg2_atom = self.ast_builder.allocator.alloc_str(&arg2_var);
        let arg2_expr = self.ast_builder.expression_identifier(span, arg2_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg2_expr));

        let arg3_atom = self.ast_builder.allocator.alloc_str(&arg3_var);
        let arg3_expr = self.ast_builder.expression_identifier(span, arg3_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg3_expr));

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

    fn create_function_call_4(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
        arg3_reg: u8,
        arg4_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Read all variable names BEFORE creating destination variable
        let func_var = self.register_manager.get_source_variable_name(func_reg);
        let arg1_var = self.register_manager.get_source_variable_name(arg1_reg);
        let arg2_var = self.register_manager.get_source_variable_name(arg2_reg);
        let arg3_var = self.register_manager.get_source_variable_name(arg3_reg);
        let arg4_var = self.register_manager.get_source_variable_name(arg4_reg);

        // Now create the destination variable
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create function expression
        let func_atom = self.ast_builder.allocator.alloc_str(&func_var);
        let func_expr = self.ast_builder.expression_identifier(span, func_atom);

        // Use .call() syntax: func.call(thisArg, arg1, arg2, arg3)
        let call_atom = self.ast_builder.allocator.alloc_str("call");
        let call_name = self.ast_builder.identifier_name(span, call_atom);
        let member_expr = self
            .ast_builder
            .alloc_static_member_expression(span, func_expr, call_name, false);

        let mut arguments = self.ast_builder.vec();
        // arg1 is 'this', arg2, arg3, arg4 are actual arguments
        let arg1_atom = self.ast_builder.allocator.alloc_str(&arg1_var);
        let arg1_expr = self.ast_builder.expression_identifier(span, arg1_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg1_expr));

        let arg2_atom = self.ast_builder.allocator.alloc_str(&arg2_var);
        let arg2_expr = self.ast_builder.expression_identifier(span, arg2_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg2_expr));

        let arg3_atom = self.ast_builder.allocator.alloc_str(&arg3_var);
        let arg3_expr = self.ast_builder.expression_identifier(span, arg3_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg3_expr));

        let arg4_atom = self.ast_builder.allocator.alloc_str(&arg4_var);
        let arg4_expr = self.ast_builder.expression_identifier(span, arg4_atom);
        arguments.push(oxc_ast::ast::Argument::from(arg4_expr));

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

    fn create_return_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Use get_source_variable_name since we're reading the value
        let value_var = self.register_manager.get_source_variable_name(value_reg);

        let span = Span::default();
        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let return_stmt = self.ast_builder.statement_return(span, Some(value_expr));

        Ok(InstructionResult::Statement(return_stmt))
    }

    fn create_throw_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();
        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let throw_stmt = self.ast_builder.statement_throw(span, value_expr);

        Ok(InstructionResult::Statement(throw_stmt))
    }

    fn create_parameter_load(
        &mut self,
        dest_reg: u8,
        param_index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create parameter name based on index
        // Account for implicit 'this' parameter at index 0
        let param_name = if param_index > 0 {
            format!("arg{}", param_index - 1)
        } else {
            // This is the implicit 'this' parameter
            "this".to_string()
        };
        let param_atom = self.ast_builder.allocator.alloc_str(&param_name);
        let param_expr = self.ast_builder.expression_identifier(span, param_atom);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(param_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create closure: `let var0_1 = function ifElseTest() { ... };`
    fn create_closure(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx as u32)
            .unwrap_or_else(|_| format!("function_{}", func_idx));

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx as u32)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx as u32);

        // Create function body
        let body = if self.decompile_nested {
            // Decompile the nested function
            match self.decompile_nested_function_body(func_idx as u32) {
                Ok(body_stmts) => {
                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
                Err(e) => {
                    // On error, create a comment with the error message
                    let body_comment = format!("/* Nested function {} - decompilation failed: {} */", func_idx, e);
                    let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
                    let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
                    let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

                    let mut body_stmts = self.ast_builder.vec();
                    body_stmts.push(comment_stmt);

                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
            }
        } else {
            // Create placeholder body when not decompiling nested functions
            let body_comment = "/* Nested function - body not yet decompiled */";
            let comment_atom = self.ast_builder.allocator.alloc_str(body_comment);
            let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
            let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

            let mut body_stmts = self.ast_builder.vec();
            body_stmts.push(comment_stmt);

            self.ast_builder
                .function_body(span, self.ast_builder.vec(), body_stmts)
        };

        // Create function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            false,                                            // is_async
            false,                                            // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create async closure: `let var0_1 = async function asyncAwaitTest() { ... };`
    fn create_async_closure(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx as u32)
            .unwrap_or_else(|_| format!("async_function_{}", func_idx));

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx as u32)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx as u32);

        // Create function body with placeholder comment
        // Subtract 1 from param_count to account for implicit 'this' parameter
        let actual_param_count = if param_count > 0 { param_count - 1 } else { 0 };
        let _param_info = if actual_param_count > 0 {
            format!(" (expects {} args)", actual_param_count)
        } else {
            String::new()
        };

        let body = if self.decompile_nested {
            // Decompile the nested async function
            match self.decompile_nested_function_body(func_idx as u32) {
                Ok(body_stmts) => {
                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
                Err(e) => {
                    // On error, create a comment with the error message
                    let body_comment = format!("/* Nested async function {} - decompilation failed: {} */", func_idx, e);
                    let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
                    let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
                    let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

                    let mut body_stmts = self.ast_builder.vec();
                    body_stmts.push(comment_stmt);

                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
            }
        } else {
            let body_comment = "/* Nested async function - body not yet decompiled */";
            let comment_atom = self.ast_builder.allocator.alloc_str(body_comment);
            let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
            let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

            let mut body_stmts = self.ast_builder.vec();
            body_stmts.push(comment_stmt);

            self.ast_builder
                .function_body(span, self.ast_builder.vec(), body_stmts)
        };

        // Create async function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            true,                                             // is_async
            false,                                            // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create builtin function call: `let var0_1 = console.log(arg1, arg2);`
    fn create_builtin_call(
        &mut self,
        dest_reg: u8,
        builtin_id: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Map builtin ID to known builtin function names
        // This is a simplified mapping - in reality, builtin IDs would be looked up from the HBC file
        let builtin_name = match builtin_id {
            0 => "console.log",
            1 => "Math.abs",
            2 => "Math.floor",
            3 => "Math.ceil",
            4 => "Math.max",
            5 => "Math.min",
            6 => "Array.isArray",
            7 => "Object.keys",
            8 => "Object.values",
            9 => "JSON.stringify",
            10 => "JSON.parse",
            _ => "builtin_function", // Generic fallback
        };

        // Create builtin function expression (e.g., console.log, Math.abs)
        let builtin_parts: Vec<&str> = builtin_name.split('.').collect();
        let func_expr = if builtin_parts.len() == 2 {
            // Object method like console.log or Math.abs
            let obj_atom = self.ast_builder.allocator.alloc_str(builtin_parts[0]);
            let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

            let method_atom = self.ast_builder.allocator.alloc_str(builtin_parts[1]);
            let method_name = self.ast_builder.identifier_name(span, method_atom);

            let member_expr =
                self.ast_builder
                    .alloc_static_member_expression(span, obj_expr, method_name, false);
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr)
        } else {
            // Simple function name
            let func_atom = self.ast_builder.allocator.alloc_str(builtin_name);
            self.ast_builder.expression_identifier(span, func_atom)
        };

        // Create arguments (simplified - using placeholder args)
        let mut arguments = self.ast_builder.vec();
        for i in 0..arg_count {
            let arg_name = format!("arg{}", i);
            let arg_atom = self.ast_builder.allocator.alloc_str(&arg_name);
            let arg_expr = self.ast_builder.expression_identifier(span, arg_atom);
            arguments.push(oxc_ast::ast::Argument::from(arg_expr));
        }

        let call_expr = self.ast_builder.expression_call(
            span,
            func_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create constructor call: `let var0_1 = new Constructor(arg1, arg2);`
    fn create_constructor_call(
        &mut self,
        dest_reg: u8,
        constructor_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let constructor_var = self.register_manager.get_variable_name(constructor_reg);

        let span = Span::default();

        // Create constructor expression
        let constructor_atom = self.ast_builder.allocator.alloc_str(&constructor_var);
        let constructor_expr = self
            .ast_builder
            .expression_identifier(span, constructor_atom);

        // Create arguments (simplified - using placeholder args)
        let mut arguments = self.ast_builder.vec();
        for i in 0..arg_count {
            let arg_name = format!("arg{}", i);
            let arg_atom = self.ast_builder.allocator.alloc_str(&arg_name);
            let arg_expr = self.ast_builder.expression_identifier(span, arg_atom);
            arguments.push(oxc_ast::ast::Argument::from(arg_expr));
        }

        let new_expr = self.ast_builder.expression_new(
            span,
            constructor_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(new_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create direct eval call: `let var0_1 = eval(code);`
    fn create_direct_eval(
        &mut self,
        dest_reg: u8,
        code_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let code_var = self.register_manager.get_variable_name(code_reg);

        let span = Span::default();

        // Create eval function expression
        let eval_atom = self.ast_builder.allocator.alloc_str("eval");
        let eval_expr = self.ast_builder.expression_identifier(span, eval_atom);

        // Create code argument
        let code_atom = self.ast_builder.allocator.alloc_str(&code_var);
        let code_expr = self.ast_builder.expression_identifier(span, code_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(code_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            eval_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create closure with long index: `let var0_1 = function largeFunctionName() { ... };`
    fn create_closure_long_index(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx)
            .unwrap_or_else(|_| format!("function_{}", func_idx));

        // Create function parameters (empty for now)
        let params = self.ast_builder.formal_parameters(
            span,
            oxc_ast::ast::FormalParameterKind::FormalParameter,
            self.ast_builder.vec(),
            None::<oxc_ast::ast::BindingRestElement>,
        );

        let body_comment = "/* Nested function - body not yet decompiled */";
        let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
        let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

        let mut body_stmts = self.ast_builder.vec();
        body_stmts.push(comment_stmt);

        let body = self
            .ast_builder
            .function_body(span, self.ast_builder.vec(), body_stmts);

        // Create function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            false,                                            // is_async
            false,                                            // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create async closure with long index: `let var0_1 = async function largeFunctionName() { ... };`
    fn create_async_closure_long_index(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx)
            .unwrap_or_else(|_| format!("async_function_{}", func_idx));

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx as u32)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx as u32);

        // Create function body with placeholder comment
        // Subtract 1 from param_count to account for implicit 'this' parameter
        let actual_param_count = if param_count > 0 { param_count - 1 } else { 0 };
        let _param_info = if actual_param_count > 0 {
            format!(" (expects {} args)", actual_param_count)
        } else {
            String::new()
        };

        let body = if self.decompile_nested {
            // Decompile the nested async function
            match self.decompile_nested_function_body(func_idx as u32) {
                Ok(body_stmts) => {
                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
                Err(e) => {
                    // On error, create a comment with the error message
                    let body_comment = format!("/* Nested async function {} - decompilation failed: {} */", func_idx, e);
                    let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
                    let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
                    let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

                    let mut body_stmts = self.ast_builder.vec();
                    body_stmts.push(comment_stmt);

                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
            }
        } else {
            let body_comment = "/* Nested async function - body not yet decompiled */";
            let comment_atom = self.ast_builder.allocator.alloc_str(body_comment);
            let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
            let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

            let mut body_stmts = self.ast_builder.vec();
            body_stmts.push(comment_stmt);

            self.ast_builder
                .function_body(span, self.ast_builder.vec(), body_stmts)
        };

        // Create async function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            true,                                             // is_async
            false,                                            // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create generator closure: `let var0_1 = function* generatorName() { ... };`
    fn create_generator_closure(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx as u32)
            .unwrap_or_else(|_| format!("generator_{}", func_idx));

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx as u32)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx as u32);

        // Create function body with placeholder comment
        // Subtract 1 from param_count to account for implicit 'this' parameter
        let actual_param_count = if param_count > 0 { param_count - 1 } else { 0 };
        let _param_info = if actual_param_count > 0 {
            format!(" (expects {} args)", actual_param_count)
        } else {
            String::new()
        };

        let body_comment = "/* Nested generator function - body not yet decompiled */";
        let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
        let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

        let mut body_stmts = self.ast_builder.vec();
        body_stmts.push(comment_stmt);

        let body = self
            .ast_builder
            .function_body(span, self.ast_builder.vec(), body_stmts);

        // Create generator function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            false,                                            // is_async
            true,                                             // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create generator closure with long index: `let var0_1 = function* generatorName() { ... };`
    fn create_generator_closure_long_index(
        &mut self,
        dest_reg: u8,
        _env_reg: u8,
        func_idx: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Look up the actual function name from the HBC file
        let func_name = self
            .expression_context
            .lookup_function_name(func_idx)
            .unwrap_or_else(|_| format!("generator_{}", func_idx));

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx as u32)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx as u32);

        // Create function body with placeholder comment
        // Subtract 1 from param_count to account for implicit 'this' parameter
        let actual_param_count = if param_count > 0 { param_count - 1 } else { 0 };
        let _param_info = if actual_param_count > 0 {
            format!(" (expects {} args)", actual_param_count)
        } else {
            String::new()
        };

        let body_comment = "/* Nested generator function - body not yet decompiled */";
        let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
        let comment_stmt = self.ast_builder.statement_expression(span, comment_expr);

        let mut body_stmts = self.ast_builder.vec();
        body_stmts.push(comment_stmt);

        let body = self
            .ast_builder
            .function_body(span, self.ast_builder.vec(), body_stmts);

        // Create generator function expression
        let func_name_atom = self.ast_builder.allocator.alloc_str(&func_name);
        let func_id = self.ast_builder.binding_identifier(span, func_name_atom);

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_id),
            false,                                            // is_async
            true,                                             // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(func_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Get arguments length: `let var0_1 = arguments.length;`
    fn create_get_arguments_length(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create arguments.length expression
        let args_atom = self.ast_builder.allocator.alloc_str("arguments");
        let args_expr = self.ast_builder.expression_identifier(span, args_atom);

        let length_atom = self.ast_builder.allocator.alloc_str("length");
        let length_name = self.ast_builder.identifier_name(span, length_atom);

        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, args_expr, length_name, false);

        let stmt = self.create_variable_declaration_or_assignment(
            &dest_var,
            Some(oxc_ast::ast::Expression::StaticMemberExpression(
                member_expr,
            )),
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Get arguments property by value: `let var0_1 = arguments[prop];`
    fn create_get_arguments_prop_by_val(
        &mut self,
        dest_reg: u8,
        prop_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let prop_var = self.register_manager.get_variable_name(prop_reg);

        let span = Span::default();

        // Create arguments[prop] expression
        let args_atom = self.ast_builder.allocator.alloc_str("arguments");
        let args_expr = self.ast_builder.expression_identifier(span, args_atom);

        let prop_atom = self.ast_builder.allocator.alloc_str(&prop_var);
        let prop_expr = self.ast_builder.expression_identifier(span, prop_atom);

        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, args_expr, prop_expr, false);

        let stmt = self.create_variable_declaration_or_assignment(
            &dest_var,
            Some(oxc_ast::ast::Expression::ComputedMemberExpression(
                member_expr,
            )),
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Reify arguments: `let var0_1 = Array.from(arguments);`
    fn create_reify_arguments(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create Array.from(arguments) call
        let array_atom = self.ast_builder.allocator.alloc_str("Array");
        let array_expr = self.ast_builder.expression_identifier(span, array_atom);

        let from_atom = self.ast_builder.allocator.alloc_str("from");
        let from_name = self.ast_builder.identifier_name(span, from_atom);

        let method_expr = self
            .ast_builder
            .alloc_static_member_expression(span, array_expr, from_name, false);

        // Create arguments argument
        let args_atom = self.ast_builder.allocator.alloc_str("arguments");
        let args_expr = self.ast_builder.expression_identifier(span, args_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(args_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(method_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_get_builtin_closure(
        &mut self,
        dest_reg: u8,
        builtin_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create comment indicating builtin closure - these are internal VM functions
        let comment_text = format!("/* builtin closure {} */", builtin_id);
        let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(comment_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }
}

impl<'a> InstructionToStatementConverter<'a> {
    /// Decompile a nested function body
    fn decompile_nested_function_body(
        &self,
        function_index: u32,
    ) -> Result<oxc_allocator::Vec<'a, Statement<'a>>, StatementConversionError> {
        // Get HBC file reference from expression context
        let hbc_file = self.expression_context.hbc_file
            .ok_or_else(|| StatementConversionError::UnsupportedInstruction(
                "No HBC file available for nested function decompilation".to_string()
            ))?;

        // Build CFG for the nested function
        let mut cfg = crate::cfg::Cfg::new(hbc_file, function_index);
        cfg.build();

        // Create a new expression context for the nested function
        let nested_context = ExpressionContext::with_context(
            hbc_file,
            function_index,
            InstructionIndex::zero(),
        );

        // Create a new instruction converter for the nested function
        let mut nested_converter = InstructionToStatementConverter::new(
            self.ast_builder,
            nested_context.clone(),
        );

        // Keep the same decompile_nested setting for nested functions
        // This allows deeply nested functions to be decompiled
        nested_converter.set_decompile_nested(self.decompile_nested);

        // If we have a global analyzer, share it
        if let Some(ref analyzer) = self.global_analyzer {
            nested_converter.set_global_analyzer(Some(analyzer.clone()));
        }

        // Get SSA analysis for the nested function if available
        let ssa_analysis = if let Some(ref analyzer) = self.global_analyzer {
            analyzer.analyzer().get_function_analysis(function_index).cloned()
        } else {
            // Fallback: run local SSA analysis
            crate::cfg::ssa::construct_ssa(&cfg, function_index).ok()
        };

        // Create block converter for the nested function
        let mut block_converter = if let Some(ssa) = ssa_analysis {
            let mut converter = crate::ast::control_flow::BlockToStatementConverter::with_ssa_and_global_analysis(
                self.ast_builder,
                nested_context,
                false, // no instruction comments in nested functions
                false, // no SSA comments in nested functions
                ssa,
                &cfg,
                self.global_analyzer.clone().unwrap_or_else(|| {
                    // Create a dummy global analyzer if none exists
                    match crate::analysis::GlobalSSAAnalyzer::analyze(hbc_file) {
                        Ok(result) => Arc::new(result),
                        Err(_) => panic!("Failed to create global analyzer for nested function"),
                    }
                }),
            );
            converter.set_decompile_nested(self.decompile_nested);
            converter
        } else {
            let mut converter = crate::ast::control_flow::BlockToStatementConverter::new(
                self.ast_builder,
                nested_context,
                false, // no instruction comments in nested functions
            );
            converter.set_decompile_nested(self.decompile_nested);
            converter
        };

        // Convert the nested function's blocks to statements
        block_converter.convert_blocks_from_cfg(&cfg)
            .map_err(|e| StatementConversionError::UnsupportedInstruction(
                format!("Failed to convert nested function blocks: {}", e)
            ))
    }
}
