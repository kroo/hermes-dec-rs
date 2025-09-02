//! Function-related helper methods
//!
//! This module provides helper methods for creating function-related statements.
//! These handle function calls, returns, and parameter loading operations.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_ast::ast::Statement;
use oxc_span::Span;

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

impl<'a> InstructionToStatementConverter<'a> {
    /// Helper to create a call expression, potentially simplified based on the use strategy
    ///
    /// This method checks if the 'this' argument should trigger call simplification:
    /// - If 'this' is undefined and simplify_calls is enabled: outputs `func(args)`
    /// - If 'this' matches the object in a member expression: outputs `obj.method(args)`
    /// - Otherwise: outputs `func.call(this, args)`
    fn create_call_expression(
        &mut self,
        func_expr: oxc_ast::ast::Expression<'a>,
        this_reg: Option<u8>,
        arg_regs: &[u8],
    ) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
        let span = Span::default();

        // Check if we should simplify this call
        let should_simplify = if let Some(this_reg) = this_reg {
            // Check if the 'this' argument has SimplifyCall strategy
            if let Some(current_block) = self.register_manager.current_block() {
                let current_pc = self.expression_context.current_pc;

                // Create the RegisterUse for lookup
                let use_site = crate::cfg::ssa::types::RegisterUse {
                    register: this_reg,
                    block_id: current_block,
                    instruction_idx: current_pc,
                };

                // Check all use strategies to find one matching this use site
                // We need to iterate because we don't know the exact SSA value
                self.control_flow_plan
                    .use_strategies
                    .iter()
                    .any(|((_, reg_use), strategy)| {
                        reg_use == &use_site
                            && matches!(
                                strategy,
                                crate::analysis::ssa_usage_tracker::UseStrategy::SimplifyCall { .. }
                            )
                    })
            } else {
                false
            }
        } else {
            false
        };

        if should_simplify
            && (self.inline_config.simplify_calls || self.inline_config.unsafe_simplify_calls)
        {
            // Create simplified call: func(args) without .call(this, ...)
            let mut arguments = self.ast_builder.vec();

            // Skip the 'this' argument and use the rest
            for &arg_reg in arg_regs {
                let arg_expr = self.register_to_expression(arg_reg)?;
                arguments.push(oxc_ast::ast::Argument::from(arg_expr));
            }

            Ok(self.ast_builder.expression_call(
                span,
                func_expr,
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                arguments,
                false,
            ))
        } else if this_reg.is_some() || !arg_regs.is_empty() {
            // Use .call() syntax: func.call(this, args...)
            let call_atom = self.ast_builder.allocator.alloc_str("call");
            let call_name = self.ast_builder.identifier_name(span, call_atom);
            let member_expr = self
                .ast_builder
                .alloc_static_member_expression(span, func_expr, call_name, false);

            let mut arguments = self.ast_builder.vec();

            // Add 'this' argument if present
            if let Some(this_reg) = this_reg {
                let this_expr = self.register_to_expression(this_reg)?;
                arguments.push(oxc_ast::ast::Argument::from(this_expr));
            }

            // Add rest of arguments
            for &arg_reg in arg_regs {
                let arg_expr = self.register_to_expression(arg_reg)?;
                arguments.push(oxc_ast::ast::Argument::from(arg_expr));
            }

            Ok(self.ast_builder.expression_call(
                span,
                oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                arguments,
                false,
            ))
        } else {
            // No arguments, regular function call
            Ok(self.ast_builder.expression_call(
                span,
                func_expr,
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                self.ast_builder.vec(),
                false,
            ))
        }
    }
}

impl<'a> FunctionHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_function_call(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Check if this should be executed for side effects only
        let is_side_effect_only = self.should_be_side_effect_only(dest_reg);

        // Create the destination variable only if we need it
        let dest_var = if !is_side_effect_only {
            self.register_manager
                .create_new_variable_for_register(dest_reg)
        } else {
            // Still need to register the variable even if we don't use it
            self.register_manager
                .create_new_variable_for_register(dest_reg);
            String::new() // Won't be used
        };

        // Collect argument registers - use call site analysis if available
        let mut arg_regs = Vec::new();

        // Try to get argument registers from call site analysis
        if let Some(current_block) = self.register_manager.current_block() {
            let current_pc = self.expression_context.current_pc;
            let call_site_key = (current_block, current_pc);

            if let Some(call_site_info) = self
                .control_flow_plan
                .call_site_analysis
                .call_sites
                .get(&call_site_key)
            {
                // Use the actual argument registers from the analysis
                arg_regs = call_site_info.argument_registers.clone();
            } else {
                // Fallback to heuristic: sequential registers after func_reg
                for i in 0..arg_count {
                    let arg_reg = func_reg + 1 + i;
                    arg_regs.push(arg_reg);
                }
            }
        } else {
            // Fallback to heuristic: sequential registers after func_reg
            for i in 0..arg_count {
                let arg_reg = func_reg + 1 + i;
                arg_regs.push(arg_reg);
            }
        }

        // Create function expression using register_to_expression
        let func_expr = self.register_to_expression(func_reg)?;

        // For Call/CallLong, first arg is 'this', rest are actual arguments
        let (this_reg, actual_args) = if !arg_regs.is_empty() {
            (Some(arg_regs[0]), &arg_regs[1..])
        } else {
            (None, &arg_regs[..])
        };

        let call_expr = self.create_call_expression(func_expr, this_reg, actual_args)?;

        let stmt = if is_side_effect_only {
            self.ast_builder
                .statement_expression(Span::default(), call_expr)
        } else {
            self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?
        };

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_function_call_1(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Check if this should be executed for side effects only
        let is_side_effect_only = self.should_be_side_effect_only(dest_reg);

        // Create the destination variable only if we need it
        let dest_var = if !is_side_effect_only {
            self.register_manager
                .create_new_variable_for_register(dest_reg)
        } else {
            // Still need to register the variable even if we don't use it
            self.register_manager
                .create_new_variable_for_register(dest_reg);
            String::new() // Won't be used
        };

        // Create function expression using register_to_expression
        let func_expr = self.register_to_expression(func_reg)?;

        // Call1 has arg1 as 'this' and no other arguments
        let call_expr = self.create_call_expression(func_expr, Some(arg1_reg), &[])?;

        let stmt = if is_side_effect_only {
            self.ast_builder
                .statement_expression(Span::default(), call_expr)
        } else {
            self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?
        };

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_function_call_2(
        &mut self,
        dest_reg: u8,
        func_reg: u8,
        arg1_reg: u8,
        arg2_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Check if this should be executed for side effects only
        let is_side_effect_only = self.should_be_side_effect_only(dest_reg);

        // Create the destination variable only if we need it
        let dest_var = if !is_side_effect_only {
            self.register_manager
                .create_new_variable_for_register(dest_reg)
        } else {
            // Still need to register the variable even if we don't use it
            self.register_manager
                .create_new_variable_for_register(dest_reg);
            String::new() // Won't be used
        };

        // Create function expression using register_to_expression
        let func_expr = self.register_to_expression(func_reg)?;

        // Call2 has arg1 as 'this' and arg2 as the first actual argument
        let call_expr = self.create_call_expression(func_expr, Some(arg1_reg), &[arg2_reg])?;

        let stmt = if is_side_effect_only {
            self.ast_builder
                .statement_expression(Span::default(), call_expr)
        } else {
            self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?
        };

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
        // Check if this should be executed for side effects only
        let is_side_effect_only = self.should_be_side_effect_only(dest_reg);

        // Create the destination variable only if we need it
        let dest_var = if !is_side_effect_only {
            self.register_manager
                .create_new_variable_for_register(dest_reg)
        } else {
            // Still need to register the variable even if we don't use it
            self.register_manager
                .create_new_variable_for_register(dest_reg);
            String::new() // Won't be used
        };

        // Create function expression using register_to_expression
        let func_expr = self.register_to_expression(func_reg)?;

        // Call3 has arg1 as 'this' and arg2, arg3 as actual arguments
        let call_expr =
            self.create_call_expression(func_expr, Some(arg1_reg), &[arg2_reg, arg3_reg])?;

        let stmt = if is_side_effect_only {
            self.ast_builder
                .statement_expression(Span::default(), call_expr)
        } else {
            self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?
        };

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
        // Check if this should be executed for side effects only
        let is_side_effect_only = self.should_be_side_effect_only(dest_reg);

        // Create the destination variable only if we need it
        let dest_var = if !is_side_effect_only {
            self.register_manager
                .create_new_variable_for_register(dest_reg)
        } else {
            // Still need to register the variable even if we don't use it
            self.register_manager
                .create_new_variable_for_register(dest_reg);
            String::new() // Won't be used
        };

        // Create function expression using register_to_expression
        let func_expr = self.register_to_expression(func_reg)?;

        // Call4 has arg1 as 'this' and arg2, arg3, arg4 as actual arguments
        let call_expr = self.create_call_expression(
            func_expr,
            Some(arg1_reg),
            &[arg2_reg, arg3_reg, arg4_reg],
        )?;

        let stmt = if is_side_effect_only {
            self.ast_builder
                .statement_expression(Span::default(), call_expr)
        } else {
            self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?
        };

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_return_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Use register_to_expression to handle inline values properly
        let value_expr = self.register_to_expression(value_reg)?;

        let span = Span::default();
        let return_stmt = self.ast_builder.statement_return(span, Some(value_expr));

        Ok(InstructionResult::Statement(return_stmt))
    }

    fn create_throw_statement(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Use register_to_expression to handle inlining properly
        let value_expr = self.register_to_expression(value_reg)?;

        let span = Span::default();
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
                    let body_comment = format!(
                        "/* Nested function {} - decompilation failed: {} */",
                        func_idx, e
                    );
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
                    let body_comment = format!(
                        "/* Nested async function {} - decompilation failed: {} */",
                        func_idx, e
                    );
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

    /// Create constructor call: `let var0_1 = Constructor.call(thisObj, arg1, arg2);`
    /// The Construct bytecode instruction manually implements the 'new' operator:
    /// it expects a 'this' object to be created by CreateThis and passed as the first argument
    fn create_constructor_call(
        &mut self,
        dest_reg: u8,
        constructor_reg: u8,
        arg_count: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create constructor expression using register_to_expression to handle inlining
        let constructor_expr = self.register_to_expression(constructor_reg)?;

        // Create the .call member expression
        let call_atom = self.ast_builder.allocator.alloc_str("call");
        let call_name = self.ast_builder.identifier_name(span, call_atom);
        let constructor_call = self.ast_builder.alloc_static_member_expression(
            span,
            constructor_expr,
            call_name,
            false,
        );

        // Look up the actual argument registers from call site analysis
        let mut arguments = self.ast_builder.vec();

        // Get the current block ID and instruction index from register manager
        // We need to find this constructor call in the call site analysis
        if let Some(current_block) = self.register_manager.current_block() {
            let current_pc = self.expression_context.current_pc;
            let call_site_key = (current_block, current_pc);

            if let Some(call_site_info) = self
                .control_flow_plan
                .call_site_analysis
                .call_sites
                .get(&call_site_key)
            {
                // Use the actual argument registers from the analysis
                // The first argument is 'this', followed by the constructor's actual arguments
                let arg_registers = call_site_info.argument_registers.clone();
                for arg_reg in arg_registers {
                    // Use register_to_expression to handle inlining properly
                    let arg_expr = self.register_to_expression(arg_reg)?;
                    arguments.push(oxc_ast::ast::Argument::from(arg_expr));
                }
            } else {
                // Fallback: create placeholder arguments if call site analysis is missing
                log::warn!(
                    "Call site analysis not found for constructor at block {:?}, PC {:?}",
                    current_block,
                    current_pc.0
                );
                // First argument is 'this', rest are constructor arguments
                for i in 0..arg_count {
                    let arg_name = if i == 0 {
                        "this".to_string()
                    } else {
                        format!("arg{}", i - 1)
                    };
                    let arg_atom = self.ast_builder.allocator.alloc_str(&arg_name);
                    let arg_expr = self.ast_builder.expression_identifier(span, arg_atom);
                    arguments.push(oxc_ast::ast::Argument::from(arg_expr));
                }
            }
        } else {
            // No current block context - use placeholder arguments
            for i in 0..arg_count {
                let arg_name = if i == 0 {
                    "this".to_string()
                } else {
                    format!("arg{}", i - 1)
                };
                let arg_atom = self.ast_builder.allocator.alloc_str(&arg_name);
                let arg_expr = self.ast_builder.expression_identifier(span, arg_atom);
                arguments.push(oxc_ast::ast::Argument::from(arg_expr));
            }
        }

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(constructor_call),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            arguments,
            false,
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(call_expr))?;

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
        let span = Span::default();

        // Create eval function expression
        let eval_atom = self.ast_builder.allocator.alloc_str("eval");
        let eval_expr = self.ast_builder.expression_identifier(span, eval_atom);

        // Use register_to_expression for code argument to handle inlining
        let code_expr = self.register_to_expression(code_reg)?;

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

        // Get parameter count and create parameters
        let param_count = self
            .expression_context
            .lookup_function_param_count(func_idx)
            .unwrap_or(0);

        let params = self.create_function_parameters(param_count, func_idx);

        // Create function body
        let body = if self.decompile_nested {
            // Decompile the nested function
            match self.decompile_nested_function_body(func_idx) {
                Ok(body_stmts) => {
                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), body_stmts)
                }
                Err(e) => {
                    // If decompilation fails, show error in placeholder
                    let error_msg = format!("/* Nested function - decompilation error: {} */", e);
                    let error_atom = self.ast_builder.allocator.alloc_str(&error_msg);
                    let error_expr = self.ast_builder.expression_identifier(span, error_atom);
                    let error_stmt = self.ast_builder.statement_expression(span, error_expr);

                    let mut error_stmts = self.ast_builder.vec();
                    error_stmts.push(error_stmt);

                    self.ast_builder
                        .function_body(span, self.ast_builder.vec(), error_stmts)
                }
            }
        } else {
            // Use placeholder
            let body_comment = "/* Nested function - body not yet decompiled */";
            let comment_atom = self.ast_builder.allocator.alloc_str(&body_comment);
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
                    let body_comment = format!(
                        "/* Nested async function {} - decompilation failed: {} */",
                        func_idx, e
                    );
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

        let span = Span::default();

        // Create arguments[prop] expression
        let args_atom = self.ast_builder.allocator.alloc_str("arguments");
        let args_expr = self.ast_builder.expression_identifier(span, args_atom);

        // Use register_to_expression for prop to handle inlining
        let prop_expr = self.register_to_expression(prop_reg)?;

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
        let hbc_file = self.expression_context.hbc_file.ok_or_else(|| {
            StatementConversionError::UnsupportedInstruction(
                "No HBC file available for nested function decompilation".to_string(),
            )
        })?;

        // Build function analysis for the nested function
        // Get the function from HBC file
        let function = hbc_file
            .functions
            .get(function_index, hbc_file)
            .map_err(|e| {
                StatementConversionError::UnsupportedInstruction(format!(
                    "Function {} not found: {:?}",
                    function_index, e
                ))
            })?;

        // Build CFG for the nested function
        let mut cfg = crate::cfg::Cfg::new(hbc_file, function_index);
        cfg.build();

        // Build SSA for the nested function
        let ssa = crate::cfg::ssa::construct_ssa(&cfg, function_index).map_err(|e| {
            StatementConversionError::UnsupportedInstruction(format!(
                "Failed to build SSA for nested function {}: {:?}",
                function_index, e
            ))
        })?;

        // Create function analysis for the nested function
        let nested_function_analysis =
            crate::analysis::FunctionAnalysis::new(function, cfg, ssa, hbc_file, function_index);

        // Build and analyze the control flow plan for the nested function
        let plan_builder = crate::analysis::control_flow_plan_builder::ControlFlowPlanBuilder::new(
            &nested_function_analysis.cfg,
            &nested_function_analysis,
        );
        let mut plan = plan_builder.build();

        log::debug!(
            "Nested function {} plan has {} structures",
            function_index,
            plan.structures.len()
        );

        // Analyze the plan to determine declaration and use strategies
        // Use the same inline configuration as the parent function
        let analyzer = crate::analysis::control_flow_plan_analyzer::ControlFlowPlanAnalyzer::with_inline_config(
            &mut plan,
            &nested_function_analysis,
            &self.inline_config,
        );
        analyzer.analyze();

        // Now convert the control flow plan to AST statements
        let mut converter = crate::ast::control_flow_plan_converter::ControlFlowPlanConverter::new(
            self.ast_builder,
            hbc_file,
            self.hbc_analysis,
            function_index,
            &nested_function_analysis,
            plan,
            false, // no SSA comments in nested functions
            false, // no instruction comments in nested functions
            &self.inline_config,
        );

        // Keep the same decompile_nested setting for deeply nested functions
        converter.set_decompile_nested(self.decompile_nested);

        // Convert the control flow plan to statements
        Ok(converter.convert_to_ast())
    }
}
