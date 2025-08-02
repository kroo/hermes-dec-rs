//! Miscellaneous helper methods
//!
//! This module provides helper methods for creating various specialized statements.
//! These handle debug operations, async operations, and other miscellaneous instructions.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_ast::ast::VariableDeclarationKind;
use oxc_span::Span;

/// Trait providing miscellaneous operation helper methods
pub trait MiscHelpers<'a> {
    /// Create a debugger statement: `debugger;`
    fn create_debugger_statement(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create a profile point statement: `createProfilePoint(Arg1);`
    fn create_profile_point(
        &mut self,
        point_id: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create an async break check statement
    fn create_async_break_check(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;


    /// Create an "Add empty string" statement
    fn create_add_empty_string(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create coerce this: `let var0_1 = this;`
    fn create_coerce_this(
        &mut self,
        dest_reg: u8,
        _src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create this object: `let var0_1 = Object.create(constructor.prototype);`
    fn create_this_statement(
        &mut self,
        dest_reg: u8,
        constructor_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new.target access: `let var0_1 = new.target;`
    fn create_new_target_statement(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create this binding load: `let var0_1 = this;`
    fn create_load_this_statement(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create select object: `let Arg1 = Arg3 instanceof Object ? Arg3 : Arg2`
    fn create_select_object(
        &mut self,
        dest_reg: u8,
        test_reg: u8,
        alt_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create regular expression: `let var0_1 = /pattern/flags;`
    fn create_regexp(
        &mut self,
        dest_reg: u8,
        pattern_id: u32,
        flags: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create instanceof check: `let var0_1 = left instanceof right;`
    fn create_instanceof(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create 'in' operator check: `let var0_1 = prop in obj;`
    fn create_is_in(
        &mut self,
        dest_reg: u8,
        prop_reg: u8,
        obj_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create for-in loop property enumeration: `let var0_1 = Object.keys(var1_0)[var2_0];`
    fn create_get_next_pname(
        &mut self,
        dest_reg: u8,
        iterator_reg: u8,
        obj_reg: u8,
        index_reg: u8,
        size_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create throw if undefined: `if (var === undefined) throw new ReferenceError();`
    fn create_throw_if_undefined(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create throw if empty: `if (var === empty) throw new ReferenceError();`
    fn create_throw_if_empty(
        &mut self,
        value_reg: u8,
        msg_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create throw if has restricted global property: `if (global.hasOwnProperty(prop)) throw new TypeError();`
    fn create_throw_if_has_restricted_global_property(
        &mut self,
        prop_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create unreachable statement: `throw new Error("Unreachable code");`
    fn create_unreachable(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create start generator: `/* Start generator */;`
    fn create_start_generator(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create complete generator: `/* Complete generator */;`
    fn create_complete_generator(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create catch statement: `/* Catch exception */;`
    fn create_catch(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create debugger check break: `/* Debugger check break */;`
    fn create_debugger_check_break(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create switch statement with immediate: `switch (discriminant) { ... }`
    fn create_switch_imm(
        &mut self,
        discriminant_reg: u8,
        min_value: u32,
        size: u16,
        default_offset: u32,
        table_offset: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> MiscHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_debugger_statement(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();
        let debugger_stmt = self.ast_builder.statement_debugger(span);
        Ok(InstructionResult::Statement(debugger_stmt))
    }

    /// Create profile point (no-op comment)
    fn create_profile_point(
        &mut self,
        _point_id: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // ProfilePoint is a profiling hint, create empty statement
        let span = Span::default();
        let empty_stmt = self.ast_builder.statement_empty(span);

        Ok(InstructionResult::Statement(empty_stmt))
    }

    fn create_async_break_check(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // Create a comment indicating this is an async break check
        let span = Span::default();
        let comment_text = "/* ASYNC_BREAK_CHECK */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }


    /// Create add empty string: `let var0_1 = "" + var1_0;`
    fn create_add_empty_string(
        &mut self,
        dest_reg: u8,
        src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let src_var = self.register_manager.get_variable_name(src_reg);

        let span = Span::default();

        // Create empty string literal
        let empty_str_atom = self.ast_builder.allocator.alloc_str("");
        let empty_str_expr = self
            .ast_builder
            .expression_string_literal(span, empty_str_atom, None);

        // Create source expression
        let src_atom = self.ast_builder.allocator.alloc_str(&src_var);
        let src_expr = self.ast_builder.expression_identifier(span, src_atom);

        // Create binary expression
        let binary_expr = self.ast_builder.expression_binary(
            span,
            empty_str_expr,
            oxc_ast::ast::BinaryOperator::Addition,
            src_expr,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(binary_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create coerce this: `let var0_1 = this;`
    /// Reg8, Reg8 (total size 2)
    /// Coerce a value assumed to contain 'this' to an object using non-strict
    /// mode rules. Primitives are boxed, \c null or \c undefed produce the global
    /// object.
    /// Arg1 = coerce_to_object(Arg2)
    fn create_coerce_this(
        &mut self,
        dest_reg: u8,
        _src_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let this_expr = self.ast_builder.expression_this(span);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(this_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create this object: `let var0_1 = Object.create(constructor.prototype);`
    fn create_this_statement(
        &mut self,
        dest_reg: u8,
        constructor_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let constructor_var = self.register_manager.get_variable_name(constructor_reg);

        let span = Span::default();

        // Create Object.create call with constructor.prototype
        let object_atom = self.ast_builder.allocator.alloc_str("Object");
        let object_expr = self.ast_builder.expression_identifier(span, object_atom);

        let create_atom = self.ast_builder.allocator.alloc_str("create");
        let create_name = self.ast_builder.identifier_name(span, create_atom);
        let object_create =
            self.ast_builder
                .alloc_static_member_expression(span, object_expr, create_name, false);

        // Create constructor.prototype
        let constructor_atom = self.ast_builder.allocator.alloc_str(&constructor_var);
        let constructor_expr = self
            .ast_builder
            .expression_identifier(span, constructor_atom);

        let prototype_atom = self.ast_builder.allocator.alloc_str("prototype");
        let prototype_name = self.ast_builder.identifier_name(span, prototype_atom);
        let prototype_expr = self.ast_builder.alloc_static_member_expression(
            span,
            constructor_expr,
            prototype_name,
            false,
        );

        // Create call
        let mut args = self.ast_builder.vec();
        args.push(oxc_ast::ast::Argument::from(
            oxc_ast::ast::Expression::StaticMemberExpression(prototype_expr),
        ));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(object_create),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            args,
            false,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(call_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create new.target access: `let var0_1 = new.target;`
    fn create_new_target_statement(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create new.target meta property
        let new_atom = self.ast_builder.allocator.alloc_str("new");
        let target_atom = self.ast_builder.allocator.alloc_str("target");
        let new_name = self.ast_builder.identifier_name(span, new_atom);
        let target_name = self.ast_builder.identifier_name(span, target_atom);
        let meta_prop = self
            .ast_builder
            .expression_meta_property(span, new_name, target_name);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(meta_prop), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create this binding load: `let var0_1 = this;`
    fn create_load_this_statement(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let this_expr = self.ast_builder.expression_this(span);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(this_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create select object: `let Arg1 = Arg3 instanceof Object ? Arg3 : Arg2`
    fn create_select_object(
        &mut self,
        dest_reg: u8,
        test_reg: u8,
        alt_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let test_var = self.register_manager.get_variable_name(test_reg);
        let alt_var = self.register_manager.get_variable_name(alt_reg);

        let span = Span::default();

        // Create test expression
        let test_atom = self.ast_builder.allocator.alloc_str(&test_var);
        let test_expr = self.ast_builder.expression_identifier(span, test_atom);

        // Create null literal
        let null_expr = self.ast_builder.expression_null_literal(span);

        // Create second test expression for conditional
        let test_atom2 = self.ast_builder.allocator.alloc_str(&test_var);
        let test_expr2 = self.ast_builder.expression_identifier(span, test_atom2);

        // Create test != null
        let condition = self.ast_builder.expression_binary(
            span,
            test_expr,
            oxc_ast::ast::BinaryOperator::Inequality,
            null_expr,
        );

        // Create alternative expression
        let alt_atom = self.ast_builder.allocator.alloc_str(&alt_var);
        let alt_expr = self.ast_builder.expression_identifier(span, alt_atom);

        // Create conditional expression
        let conditional_expr = self
            .ast_builder
            .expression_conditional(span, condition, test_expr2, alt_expr);

        let stmt = self
            .create_variable_declaration(
                &dest_var,
                Some(conditional_expr),
                VariableDeclarationKind::Let,
            )
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create regular expression: `let var0_1 = /pattern/flags;`
    fn create_regexp(
        &mut self,
        dest_reg: u8,
        pattern_id: u32,
        flags: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Try to look up the actual pattern and flags from the regexp table
        match self.expression_context.lookup_regexp(pattern_id) {
            Ok((pattern, pattern_flags)) => {
                // Create new RegExp() expression
                let regexp_atom = self.ast_builder.allocator.alloc_str("RegExp");
                let regexp_ident = self.ast_builder.expression_identifier(span, regexp_atom);
                
                // Create arguments: pattern and flags
                let mut args = self.ast_builder.vec();
                
                // Add pattern as string literal
                let pattern_atom = self.ast_builder.allocator.alloc_str(&pattern);
                let pattern_expr = self.ast_builder.expression_string_literal(span, pattern_atom, None);
                args.push(oxc_ast::ast::Argument::from(pattern_expr));
                
                // Combine flags from the table with the flags parameter
                // The flags parameter from the instruction might override or add to pattern flags
                let combined_flags = if flags > 0 {
                    // Convert numeric flags to string representation
                    // TODO: Decode the numeric flags format if needed
                    pattern_flags
                } else {
                    pattern_flags
                };
                
                // Add flags if present
                if !combined_flags.is_empty() {
                    let flags_atom = self.ast_builder.allocator.alloc_str(&combined_flags);
                    let flags_expr = self.ast_builder.expression_string_literal(span, flags_atom, None);
                    args.push(oxc_ast::ast::Argument::from(flags_expr));
                }
                
                let regexp_expr = self.ast_builder.expression_new(
                    span,
                    regexp_ident,
                    None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                    args,
                );

                let stmt = self
                    .create_variable_declaration(&dest_var, Some(regexp_expr), VariableDeclarationKind::Let)?;

                Ok(InstructionResult::Statement(stmt))
            }
            Err(_) => {
                // Fallback: create a comment indicating a RegExp would be created
                let comment_text = format!("new RegExp(/* pattern {} */, /* flags {} */)", pattern_id, flags);
                let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
                let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

                let stmt = self
                    .create_variable_declaration(&dest_var, Some(comment_expr), VariableDeclarationKind::Let)?;

                Ok(InstructionResult::Statement(stmt))
            }
        }
    }

    /// Create instanceof check: `let var0_1 = left instanceof right;`
    fn create_instanceof(
        &mut self,
        dest_reg: u8,
        left_reg: u8,
        right_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let left_var = self.register_manager.get_variable_name(left_reg);
        let right_var = self.register_manager.get_variable_name(right_reg);

        let span = Span::default();

        // Create left expression
        let left_atom = self.ast_builder.allocator.alloc_str(&left_var);
        let left_expr = self.ast_builder.expression_identifier(span, left_atom);

        // Create right expression
        let right_atom = self.ast_builder.allocator.alloc_str(&right_var);
        let right_expr = self.ast_builder.expression_identifier(span, right_atom);

        // Create instanceof binary expression
        let instanceof_expr = self.ast_builder.expression_binary(
            span,
            left_expr,
            oxc_ast::ast::BinaryOperator::Instanceof,
            right_expr,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(instanceof_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create 'in' operator check: `let var0_1 = prop in obj;`
    fn create_is_in(
        &mut self,
        dest_reg: u8,
        prop_reg: u8,
        obj_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let prop_var = self.register_manager.get_variable_name(prop_reg);
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        let span = Span::default();

        // Create property expression
        let prop_atom = self.ast_builder.allocator.alloc_str(&prop_var);
        let prop_expr = self.ast_builder.expression_identifier(span, prop_atom);

        // Create object expression
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        // Create 'in' binary expression
        let in_expr = self.ast_builder.expression_binary(
            span,
            prop_expr,
            oxc_ast::ast::BinaryOperator::In,
            obj_expr,
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(in_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create for-in loop property enumeration: `let var0_1 = Object.keys(var1_0)[var2_0];`
    fn create_get_next_pname(
        &mut self,
        dest_reg: u8,
        iterator_reg: u8,
        obj_reg: u8,
        index_reg: u8,
        _size_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let iterator_var = self.register_manager.get_variable_name(iterator_reg);
        let _obj_var = self.register_manager.get_variable_name(obj_reg);
        let index_var = self.register_manager.get_variable_name(index_reg);

        let span = Span::default();

        // Create Object.keys(obj)[index] expression
        // For simplicity, we'll use iterator[index] assuming iterator contains the keys
        let iterator_atom = self.ast_builder.allocator.alloc_str(&iterator_var);
        let iterator_expr = self.ast_builder.expression_identifier(span, iterator_atom);

        let index_atom = self.ast_builder.allocator.alloc_str(&index_var);
        let index_expr = self.ast_builder.expression_identifier(span, index_atom);

        // Create member expression for accessing array element
        let member_expr = oxc_ast::ast::Expression::ComputedMemberExpression(
            self.ast_builder.alloc_computed_member_expression(
                span,
                iterator_expr,
                index_expr,
                false,
            )
        );

        let stmt = self
            .create_variable_declaration(&dest_var, Some(member_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_throw_if_undefined(
        &mut self,
        value_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create if (value === undefined) throw new ReferenceError()
        let value_atom = self.ast_builder.allocator.alloc_str(&value_var);
        let value_expr = self.ast_builder.expression_identifier(span, value_atom);

        let undefined_expr = self.ast_builder.expression_identifier(span, self.ast_builder.allocator.alloc_str("undefined"));

        // Create strict equality check
        let condition = self.ast_builder.expression_binary(
            span,
            value_expr,
            oxc_ast::ast::BinaryOperator::StrictEquality,
            undefined_expr,
        );

        // Create throw new ReferenceError()
        let ref_error_atom = self.ast_builder.allocator.alloc_str("ReferenceError");
        let ref_error_expr = self.ast_builder.expression_identifier(span, ref_error_atom);

        let new_error_expr = self.ast_builder.expression_new(
            span,
            ref_error_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            self.ast_builder.vec(),
        );

        let throw_stmt = self.ast_builder.statement_throw(span, new_error_expr);

        // Create if statement
        let if_stmt = self.ast_builder.statement_if(span, condition, throw_stmt, None);

        Ok(InstructionResult::Statement(if_stmt))
    }

    fn create_throw_if_empty(
        &mut self,
        value_reg: u8,
        _msg_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let value_var = self.register_manager.get_variable_name(value_reg);

        let span = Span::default();

        // Create comment for now - empty slot checking is complex
        let comment_text = format!("/* Check {} is not empty slot */", value_var);
        let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_throw_if_has_restricted_global_property(
        &mut self,
        prop_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let prop_var = self.register_manager.get_variable_name(prop_reg);

        let span = Span::default();

        // Create comment for now - restricted global property checking is complex
        let comment_text = format!("/* Check {} is not restricted global property */", prop_var);
        let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_unreachable(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Create throw new Error("Unreachable code")
        let error_atom = self.ast_builder.allocator.alloc_str("Error");
        let error_expr = self.ast_builder.expression_identifier(span, error_atom);

        let message_atom = self.ast_builder.allocator.alloc_str("Unreachable code");
        let message_expr = self.ast_builder.expression_string_literal(span, message_atom, None);

        let mut args = self.ast_builder.vec();
        args.push(oxc_ast::ast::Argument::from(message_expr));

        let new_error_expr = self.ast_builder.expression_new(
            span,
            error_expr,
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            args,
        );

        let throw_stmt = self.ast_builder.statement_throw(span, new_error_expr);

        Ok(InstructionResult::Statement(throw_stmt))
    }

    fn create_start_generator(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Create comment indicating generator start
        let comment_text = "/* Start generator */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_complete_generator(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Create comment indicating generator completion
        let comment_text = "/* Complete generator */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_catch(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create variable to catch exception: let var0_1 = /* caught exception */;
        let comment_text = "/* caught exception */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self
            .create_variable_declaration(&dest_var, Some(comment_expr), VariableDeclarationKind::Let)
?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_debugger_check_break(
        &mut self,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let span = Span::default();

        // Create comment indicating debugger check
        let comment_text = "/* Debugger check break */";
        let comment_atom = self.ast_builder.allocator.alloc_str(comment_text);
        let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);

        let stmt = self.ast_builder.statement_expression(span, comment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_switch_imm(
        &mut self,
        discriminant_reg: u8,
        min_value: u32,
        size: u16,
        _default_offset: u32,
        _table_offset: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let discriminant_var = self.register_manager.get_variable_name(discriminant_reg);

        let span = Span::default();

        // Create switch statement discriminant
        let discriminant_atom = self.ast_builder.allocator.alloc_str(&discriminant_var);
        let discriminant_expr = self.ast_builder.expression_identifier(span, discriminant_atom);

        // For now, create a simple switch with placeholder cases
        // TODO: In a full implementation, we'd read the switch table and create proper cases
        let mut cases = self.ast_builder.vec();
        
        // Create a few example cases based on min_value and size
        for i in 0..std::cmp::min(size, 3) as u32 {
            let case_value = min_value + i;
            let case_expr = self.ast_builder.expression_numeric_literal(
                span,
                case_value as f64,
                None,
                oxc_syntax::number::NumberBase::Decimal,
            );

            // Create empty case body for now
            let case_body = self.ast_builder.vec();
            
            let switch_case = self.ast_builder.switch_case(span, Some(case_expr), case_body);
            cases.push(switch_case);
        }

        // Create default case
        let default_body = self.ast_builder.vec();
        let default_case = self.ast_builder.switch_case(span, None, default_body);
        cases.push(default_case);

        let switch_stmt = self.ast_builder.statement_switch(span, discriminant_expr, cases);

        Ok(InstructionResult::Statement(switch_stmt))
    }
}
