//! Object property and object creation helper methods
//!
//! This module provides helper methods for creating object-related statements.
//! These handle property access, property assignment, object creation, and deletion operations.

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use crate::ast::context::ExpressionContext;
use crate::hbc::serialized_literal_parser::SLPValue;
use oxc_span::Span;

/// Convert a key-value pair to an object property
fn create_object_property<'a>(
    key: &SLPValue,
    value: &SLPValue,
    ast_builder: &'a oxc_ast::AstBuilder<'a>,
    expression_context: &ExpressionContext,
) -> Result<oxc_allocator::Box<'a, oxc_ast::ast::ObjectProperty<'a>>, StatementConversionError> {
    let span = Span::default();

    // Convert the value to an expression
    let value_expr = slp_value_to_expression(value, ast_builder, expression_context)?;

    // Handle the key - try to create an identifier if it's a string, otherwise use computed property
    let property_key = match key {
        SLPValue::LongString(string_id) => {
            // Try to get the string value and create an identifier property key
            match expression_context.lookup_string(*string_id) {
                Ok(string_value) => {
                    // Check if the string is a valid identifier
                    if is_valid_identifier(&string_value) {
                        let key_atom = ast_builder.allocator.alloc_str(&string_value);
                        let identifier = ast_builder.identifier_name(span, key_atom);
                        oxc_ast::ast::PropertyKey::StaticIdentifier(ast_builder.alloc(identifier))
                    } else {
                        // Use computed property for non-identifier strings
                        let key_expr =
                            slp_value_to_expression(key, ast_builder, expression_context)?;
                        oxc_ast::ast::PropertyKey::from(key_expr)
                    }
                }
                Err(_) => {
                    // Fallback to computed property
                    let key_expr = slp_value_to_expression(key, ast_builder, expression_context)?;
                    oxc_ast::ast::PropertyKey::from(key_expr)
                }
            }
        }
        SLPValue::ShortString(string_id) => {
            // Similar logic for short strings
            match expression_context.lookup_string(*string_id as u32) {
                Ok(string_value) => {
                    if is_valid_identifier(&string_value) {
                        let key_atom = ast_builder.allocator.alloc_str(&string_value);
                        let identifier = ast_builder.identifier_name(span, key_atom);
                        oxc_ast::ast::PropertyKey::StaticIdentifier(ast_builder.alloc(identifier))
                    } else {
                        let key_expr =
                            slp_value_to_expression(key, ast_builder, expression_context)?;
                        oxc_ast::ast::PropertyKey::from(key_expr)
                    }
                }
                Err(_) => {
                    let key_expr = slp_value_to_expression(key, ast_builder, expression_context)?;
                    oxc_ast::ast::PropertyKey::from(key_expr)
                }
            }
        }
        SLPValue::ByteString(string_id) => {
            // Similar logic for byte strings
            match expression_context.lookup_string(*string_id as u32) {
                Ok(string_value) => {
                    if is_valid_identifier(&string_value) {
                        let key_atom = ast_builder.allocator.alloc_str(&string_value);
                        let identifier = ast_builder.identifier_name(span, key_atom);
                        oxc_ast::ast::PropertyKey::StaticIdentifier(ast_builder.alloc(identifier))
                    } else {
                        let key_expr =
                            slp_value_to_expression(key, ast_builder, expression_context)?;
                        oxc_ast::ast::PropertyKey::from(key_expr)
                    }
                }
                Err(_) => {
                    let key_expr = slp_value_to_expression(key, ast_builder, expression_context)?;
                    oxc_ast::ast::PropertyKey::from(key_expr)
                }
            }
        }
        _ => {
            // For non-string keys (numbers, booleans, etc.), always use computed properties
            let key_expr = slp_value_to_expression(key, ast_builder, expression_context)?;
            oxc_ast::ast::PropertyKey::from(key_expr)
        }
    };

    // Create the object property
    let property = ast_builder.object_property(
        span,
        oxc_ast::ast::PropertyKind::Init,
        property_key,
        value_expr,
        false, // computed: depends on key type, but PropertyKey already handles this
        false, // shorthand
        false, // method
    );

    Ok(ast_builder.alloc(property))
}

/// Check if a string is a valid JavaScript identifier
fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    // Check first character
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !first.is_ascii_alphabetic() && first != '_' && first != '$' {
        return false;
    }

    // Check remaining characters
    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' && c != '$' {
            return false;
        }
    }

    // Check if it's a reserved keyword (simplified check)
    matches!(
        s,
        "break"
            | "case"
            | "catch"
            | "class"
            | "const"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "export"
            | "extends"
            | "false"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "import"
            | "in"
            | "instanceof"
            | "new"
            | "null"
            | "return"
            | "super"
            | "switch"
            | "this"
            | "throw"
            | "true"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
    )
    .then(|| false)
    .unwrap_or(true)
}

/// Convert an SLPValue to an AST expression
pub fn slp_value_to_expression<'a>(
    value: &SLPValue,
    ast_builder: &'a oxc_ast::AstBuilder<'a>,
    expression_context: &ExpressionContext,
) -> Result<oxc_ast::ast::Expression<'a>, StatementConversionError> {
    let span = Span::default();

    match value {
        SLPValue::Null => {
            let null_atom = ast_builder.allocator.alloc_str("null");
            Ok(ast_builder.expression_identifier(span, null_atom))
        }
        SLPValue::True => Ok(ast_builder.expression_boolean_literal(span, true)),
        SLPValue::False => Ok(ast_builder.expression_boolean_literal(span, false)),
        SLPValue::Number(n) => Ok(ast_builder.expression_numeric_literal(
            span,
            *n,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        )),
        SLPValue::Integer(i) => Ok(ast_builder.expression_numeric_literal(
            span,
            *i as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        )),
        SLPValue::LongString(string_id) => {
            // Look up the string from the string table
            match expression_context.lookup_string(*string_id) {
                Ok(string_value) => {
                    let string_atom = ast_builder.allocator.alloc_str(&string_value);
                    Ok(ast_builder.expression_string_literal(span, string_atom, None))
                }
                Err(_) => {
                    // Fallback to placeholder
                    let placeholder = format!("string_{}", string_id);
                    let placeholder_atom = ast_builder.allocator.alloc_str(&placeholder);
                    Ok(ast_builder.expression_string_literal(span, placeholder_atom, None))
                }
            }
        }
        SLPValue::ShortString(string_id) => {
            // Look up the string from the string table
            match expression_context.lookup_string(*string_id as u32) {
                Ok(string_value) => {
                    let string_atom = ast_builder.allocator.alloc_str(&string_value);
                    Ok(ast_builder.expression_string_literal(span, string_atom, None))
                }
                Err(_) => {
                    // Fallback to placeholder
                    let placeholder = format!("string_{}", string_id);
                    let placeholder_atom = ast_builder.allocator.alloc_str(&placeholder);
                    Ok(ast_builder.expression_string_literal(span, placeholder_atom, None))
                }
            }
        }
        SLPValue::ByteString(string_id) => {
            // Look up the string from the string table
            match expression_context.lookup_string(*string_id as u32) {
                Ok(string_value) => {
                    let string_atom = ast_builder.allocator.alloc_str(&string_value);
                    Ok(ast_builder.expression_string_literal(span, string_atom, None))
                }
                Err(_) => {
                    // Fallback to placeholder
                    let placeholder = format!("string_{}", string_id);
                    let placeholder_atom = ast_builder.allocator.alloc_str(&placeholder);
                    Ok(ast_builder.expression_string_literal(span, placeholder_atom, None))
                }
            }
        }
    }
}

/// Trait providing object operation helper methods
pub trait ObjectHelpers<'a> {
    /// Create property assignment by ID: `obj.prop = value;`
    fn create_property_assignment_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        _cache_id: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create property assignment by value: `obj[key] = value;`
    fn create_property_assignment_by_value(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create property access by ID: `let var0_1 = obj.prop;`
    fn create_property_access_by_id(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        _cache_id: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create property access by value: `let var0_1 = obj[key];`
    fn create_property_access_by_value(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new object: `let var0_1 = {};`
    fn create_new_object(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new array: `let var0_1 = new Array(size);`
    fn create_new_array(
        &mut self,
        dest_reg: u8,
        size: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create GetPNameList statement for for-in loop enumeration: `let var0_1 = Object.keys(var1_0);`
    fn create_get_pname_list_statement(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        _flags_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create property deletion by ID: `let var0_1 = delete obj.prop;`
    fn create_property_deletion_by_id(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create property deletion by value: `let var0_1 = delete obj[key];`
    fn create_property_deletion_by_value(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create array with buffer: `let var0_1 = [1, 2, 3];`
    fn create_new_array_with_buffer(
        &mut self,
        dest_reg: u8,
        size_hint: u16,
        num_literals: u16,
        buffer_start_index: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create array with buffer (long): `let var0_1 = [1, 2, 3];`
    fn create_new_array_with_buffer_long(
        &mut self,
        dest_reg: u8,
        size_hint: u16,
        num_literals: u16,
        buffer_start_index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create object with buffer: `let var0_1 = {a: 1, b: 2};`
    fn create_new_object_with_buffer(
        &mut self,
        dest_reg: u8,
        size_hint: u16,
        num_literals: u16,
        key_buffer_start_index: u16,
        value_buffer_start_index: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create object with buffer (long): `let var0_1 = {a: 1, b: 2};`
    fn create_new_object_with_buffer_long(
        &mut self,
        dest_reg: u8,
        size_hint: u16,
        num_literals: u16,
        key_buffer_start_index: u32,
        value_buffer_start_index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create object with parent: `let var0_1 = Object.create(parent);`
    fn create_new_object_with_parent(
        &mut self,
        dest_reg: u8,
        parent_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create try property access by ID with long cache: `let var0_1 = obj.prop;`
    fn create_try_property_access_by_id_long(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        prop_id: u32,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create try property assignment by ID: `obj.prop = value;`
    fn create_try_property_assignment_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u8,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create try property assignment by ID with long cache: `obj.prop = value;`
    fn create_try_property_assignment_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new own property assignment by ID: `obj.prop = value;` (non-extensible)
    fn create_put_new_own_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new own property assignment by ID short: `obj.prop = value;` (non-extensible)
    fn create_put_new_own_by_id_short(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new own non-enumerable property by ID: `obj.prop = value;`
    fn create_put_new_own_ne_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create new own non-enumerable property by ID long: `obj.prop = value;`
    fn create_put_new_own_ne_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create own property assignment by index: `obj[index] = value;`
    fn create_put_own_by_index(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create own property assignment by index long: `obj[index] = value;`
    fn create_put_own_by_index_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    /// Create getter/setter property definition: `Object.defineProperty(obj, key, {get: getter, set: setter});`
    fn create_put_own_getter_setter_by_val(
        &mut self,
        obj_reg: u8,
        key_reg: u8,
        getter_reg: u8,
        setter_reg: u8,
        enumerable: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> ObjectHelpers<'a> for InstructionToStatementConverter<'a> {
    fn create_property_assignment_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        _cache_id: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        // Look up property name from string table
        // Note: cache_id is used by the VM for optimization but not needed for decompilation
        let prop_name = self.expression_context.lookup_string(prop_id)?;

        let span = Span::default();

        // Create object identifier
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        // Create value expression (may be inlined)
        let value_expr = self.register_to_expression(value_reg)?;

        // Create property access for assignment target
        let prop_atom = self.ast_builder.allocator.alloc_str(&prop_name);
        let property_name = self.ast_builder.identifier_name(span, prop_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, obj_expr, property_name, false);

        // Create assignment statement
        let assignment_expr = self.ast_builder.expression_assignment(
            span,
            oxc_ast::ast::AssignmentOperator::Assign,
            oxc_ast::ast::AssignmentTarget::StaticMemberExpression(member_expr),
            value_expr,
        );

        let stmt = self.ast_builder.statement_expression(span, assignment_expr);
        Ok(InstructionResult::Statement(stmt))
    }

    fn create_property_assignment_by_value(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        let span = Span::default();

        // Create expressions
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        // Key and value expressions may be inlined
        let key_expr = self.register_to_expression(key_reg)?;
        let value_expr = self.register_to_expression(value_reg)?;

        // Create computed member expression for assignment target
        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, obj_expr, key_expr, false);

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

    fn create_property_access_by_id(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        _cache_id: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        // Look up property name from string table
        // Note: cache_id is used by the VM for optimization but not needed for decompilation
        let prop_name = self.expression_context.lookup_string(prop_id)?;

        let span = Span::default();

        // Use register_to_expression to handle inlining
        let obj_expr = self.register_to_expression(obj_reg)?;

        // Check if we're accessing a standard global property on globalThis
        let final_expr = if let oxc_ast::ast::Expression::Identifier(ident) = &obj_expr {
            if ident.name.as_str() == "globalThis"
                && crate::ast::instructions::is_standard_global(&prop_name)
            {
                // Simplify globalThis.console to just console
                let prop_atom = self.ast_builder.allocator.alloc_str(&prop_name);
                self.ast_builder.expression_identifier(span, prop_atom)
            } else {
                // Create normal property access
                let prop_atom = self.ast_builder.allocator.alloc_str(&prop_name);
                let property_name = self.ast_builder.identifier_name(span, prop_atom);
                let member_expr = self.ast_builder.alloc_static_member_expression(
                    span,
                    obj_expr,
                    property_name,
                    false,
                );
                oxc_ast::ast::Expression::StaticMemberExpression(member_expr)
            }
        } else {
            // Create normal property access
            let prop_atom = self.ast_builder.allocator.alloc_str(&prop_name);
            let property_name = self.ast_builder.identifier_name(span, prop_atom);
            let member_expr = self.ast_builder.alloc_static_member_expression(
                span,
                obj_expr,
                property_name,
                false,
            );
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr)
        };

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(final_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_property_access_by_value(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let obj_var = self.register_manager.get_variable_name(obj_reg);
        let key_var = self.register_manager.get_variable_name(key_reg);

        let span = Span::default();

        // Create expressions
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        let key_atom = self.ast_builder.allocator.alloc_str(&key_var);
        let key_expr = self.ast_builder.expression_identifier(span, key_atom);

        // Create computed member expression
        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, obj_expr, key_expr, false);

        let stmt = self.create_variable_declaration_or_assignment(
            &dest_var,
            Some(oxc_ast::ast::Expression::ComputedMemberExpression(
                member_expr,
            )),
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_new_object(
        &mut self,
        dest_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();
        let obj_expr = self
            .ast_builder
            .expression_object(span, self.ast_builder.vec());

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(obj_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    fn create_new_array(
        &mut self,
        dest_reg: u8,
        size: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        let expr = if size == 0 {
            // For size 0, create an empty array literal []
            let elements = self.ast_builder.vec();
            oxc_ast::ast::Expression::ArrayExpression(
                self.ast_builder.alloc_array_expression(span, elements),
            )
        } else {
            // For non-zero size, create new Array(size)
            let array_atom = self.ast_builder.allocator.alloc_str("Array");
            let array_expr = self.ast_builder.expression_identifier(span, array_atom);

            // Create size argument
            let size_expr = self.ast_builder.expression_numeric_literal(
                span,
                size as f64,
                None,
                oxc_syntax::number::NumberBase::Decimal,
            );

            let mut arguments = self.ast_builder.vec();
            arguments.push(oxc_ast::ast::Argument::from(size_expr));

            // Create new expression
            self.ast_builder.expression_new(
                span,
                array_expr,
                None::<oxc_ast::ast::TSTypeParameterInstantiation>,
                arguments,
            )
        };

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create GetPNameList statement for for-in loop enumeration: `let var0_1 = Object.keys(var1_0);`
    fn create_get_pname_list_statement(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        _flags_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        let span = Span::default();

        // Create Object.keys(obj) call expression
        let object_atom = self.ast_builder.allocator.alloc_str("Object");
        let object_expr = self.ast_builder.expression_identifier(span, object_atom);

        let keys_atom = self.ast_builder.allocator.alloc_str("keys");
        let keys_name = self.ast_builder.identifier_name(span, keys_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, object_expr, keys_name, false);

        // Create argument
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);
        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(obj_expr));

        // Create call expression
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

    /// Create property deletion by ID: `let var0_1 = delete obj.prop;`
    fn create_property_deletion_by_id(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        // Look up property name from string table
        let prop_name = self.expression_context.lookup_string(prop_id)?;

        let span = Span::default();

        // Create object identifier
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        // Create property access
        let prop_atom = self.ast_builder.allocator.alloc_str(&prop_name);
        let property_name = self.ast_builder.identifier_name(span, prop_atom);
        let member_expr =
            self.ast_builder
                .alloc_static_member_expression(span, obj_expr, property_name, false);

        // Create delete expression
        let delete_expr = self.ast_builder.expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Delete,
            oxc_ast::ast::Expression::StaticMemberExpression(member_expr),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(delete_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create property deletion by value: `let var0_1 = delete obj[key];`
    fn create_property_deletion_by_value(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        key_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let obj_var = self.register_manager.get_variable_name(obj_reg);
        let key_var = self.register_manager.get_variable_name(key_reg);

        let span = Span::default();

        // Create expressions
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        let key_atom = self.ast_builder.allocator.alloc_str(&key_var);
        let key_expr = self.ast_builder.expression_identifier(span, key_atom);

        // Create computed member expression
        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, obj_expr, key_expr, false);

        // Create delete expression
        let delete_expr = self.ast_builder.expression_unary(
            span,
            oxc_ast::ast::UnaryOperator::Delete,
            oxc_ast::ast::Expression::ComputedMemberExpression(member_expr),
        );

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(delete_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create array with buffer: `let var0_1 = [1, 2, 3];`
    fn create_new_array_with_buffer(
        &mut self,
        dest_reg: u8,
        _size_hint: u16,
        num_literals: u16,
        buffer_start_index: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Try to look up actual array data from HBC file
        let mut elements = self.ast_builder.vec();

        match self
            .expression_context
            .lookup_array_literal_range(buffer_start_index as u32, num_literals as u32)
        {
            Ok(slp_values) => {
                // Convert each SLPValue to an AST expression
                for slp_value in slp_values {
                    match slp_value_to_expression(
                        &slp_value,
                        &self.ast_builder,
                        &self.expression_context,
                    ) {
                        Ok(expr) => {
                            elements.push(oxc_ast::ast::ArrayExpressionElement::from(expr));
                        }
                        Err(_) => {
                            // Fallback to placeholder if conversion fails
                            let placeholder_atom = self
                                .ast_builder
                                .allocator
                                .alloc_str("/* conversion error */");
                            let placeholder_expr = self
                                .ast_builder
                                .expression_identifier(span, placeholder_atom);
                            elements
                                .push(oxc_ast::ast::ArrayExpressionElement::from(placeholder_expr));
                        }
                    }
                }
            }
            Err(_) => {
                // Fallback: create comment indicating the buffer range (short array)
                let comment_text = format!(
                    "/* Array buffer range {}-{} */",
                    buffer_start_index as u32,
                    buffer_start_index as u32 + num_literals as u32
                );
                let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
                let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
                elements.push(oxc_ast::ast::ArrayExpressionElement::from(comment_expr));
            }
        }

        let array_expr = self.ast_builder.expression_array(span, elements);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(array_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create array with buffer (long): `let var0_1 = [1, 2, 3];`
    fn create_new_array_with_buffer_long(
        &mut self,
        dest_reg: u8,
        _size_hint: u16,
        num_literals: u16,
        buffer_start_index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Try to look up actual array data from HBC file
        let mut elements = self.ast_builder.vec();

        match self
            .expression_context
            .lookup_array_literal_range(buffer_start_index, num_literals as u32)
        {
            Ok(slp_values) => {
                // Convert each SLPValue to an AST expression
                for slp_value in slp_values {
                    match slp_value_to_expression(
                        &slp_value,
                        &self.ast_builder,
                        &self.expression_context,
                    ) {
                        Ok(expr) => {
                            elements.push(oxc_ast::ast::ArrayExpressionElement::from(expr));
                        }
                        Err(_) => {
                            // Fallback to placeholder if conversion fails
                            let placeholder_atom = self
                                .ast_builder
                                .allocator
                                .alloc_str("/* conversion error */");
                            let placeholder_expr = self
                                .ast_builder
                                .expression_identifier(span, placeholder_atom);
                            elements
                                .push(oxc_ast::ast::ArrayExpressionElement::from(placeholder_expr));
                        }
                    }
                }
            }
            Err(_) => {
                // Fallback: create comment indicating the buffer ID
                let comment_text = format!(
                    "/* Array buffer range {}-{} */",
                    buffer_start_index,
                    buffer_start_index + num_literals as u32
                );
                let comment_atom = self.ast_builder.allocator.alloc_str(&comment_text);
                let comment_expr = self.ast_builder.expression_identifier(span, comment_atom);
                elements.push(oxc_ast::ast::ArrayExpressionElement::from(comment_expr));
            }
        }

        let array_expr = self.ast_builder.expression_array(span, elements);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(array_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create object with buffer: `let var0_1 = {a: 1, b: 2};`
    fn create_new_object_with_buffer(
        &mut self,
        dest_reg: u8,
        _size_hint: u16,
        num_literals: u16,
        key_buffer_start_index: u16,
        value_buffer_start_index: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Try to look up actual object data from HBC file
        let mut properties = self.ast_builder.vec();

        // Try to look up object data from HBC file
        match self.expression_context.lookup_object_literal_range(
            key_buffer_start_index as u32,
            value_buffer_start_index as u32,
            num_literals as u32,
        ) {
            Ok((keys, values)) => {
                // Successfully looked up data - convert key-value pairs to object properties
                for (key, value) in keys.iter().zip(values.iter()) {
                    match create_object_property(
                        key,
                        value,
                        &self.ast_builder,
                        &self.expression_context,
                    ) {
                        Ok(property) => {
                            properties
                                .push(oxc_ast::ast::ObjectPropertyKind::ObjectProperty(property));
                        }
                        Err(_) => {
                            // Fallback: skip this property if conversion fails
                            // Could log the error in the future
                        }
                    }
                }
            }
            Err(_) => {
                // Lookup failed - create empty object (no properties to add)
            }
        }

        let object_expr = self.ast_builder.expression_object(span, properties);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(object_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create object with buffer (long): `let var0_1 = {a: 1, b: 2};` (placeholder)
    fn create_new_object_with_buffer_long(
        &mut self,
        dest_reg: u8,
        _size_hint: u16,
        num_literals: u16,
        key_buffer_start_index: u32,
        value_buffer_start_index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Use same logic as short version, just call the lookup with the buffer_id directly
        // Try to look up actual object data from HBC file
        let mut properties = self.ast_builder.vec();

        match self.expression_context.lookup_object_literal_range(
            key_buffer_start_index,
            value_buffer_start_index,
            num_literals as u32,
        ) {
            Ok((keys, values)) => {
                // Successfully looked up data - convert key-value pairs to object properties
                for (key, value) in keys.iter().zip(values.iter()) {
                    match create_object_property(
                        key,
                        value,
                        &self.ast_builder,
                        &self.expression_context,
                    ) {
                        Ok(property) => {
                            properties
                                .push(oxc_ast::ast::ObjectPropertyKind::ObjectProperty(property));
                        }
                        Err(_) => {
                            // Fallback: skip this property if conversion fails
                            // Could log the error in the future
                        }
                    }
                }
            }
            Err(_) => {
                // Lookup failed - create empty object (no properties to add)
            }
        }

        let object_expr = self.ast_builder.expression_object(span, properties);

        let stmt = self.create_variable_declaration_or_assignment(&dest_var, Some(object_expr))?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create object with parent: `let var0_1 = Object.create(parent);`
    fn create_new_object_with_parent(
        &mut self,
        dest_reg: u8,
        parent_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let parent_var = self.register_manager.get_variable_name(parent_reg);

        let span = Span::default();

        // Create Object.create(parent) call
        let object_atom = self.ast_builder.allocator.alloc_str("Object");
        let object_expr = self.ast_builder.expression_identifier(span, object_atom);

        let create_atom = self.ast_builder.allocator.alloc_str("create");
        let create_name = self.ast_builder.identifier_name(span, create_atom);

        let method_expr =
            self.ast_builder
                .alloc_static_member_expression(span, object_expr, create_name, false);

        // Create parent argument
        let parent_atom = self.ast_builder.allocator.alloc_str(&parent_var);
        let parent_expr = self.ast_builder.expression_identifier(span, parent_atom);

        let mut arguments = self.ast_builder.vec();
        arguments.push(oxc_ast::ast::Argument::from(parent_expr));

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

    fn create_try_property_access_by_id_long(
        &mut self,
        dest_reg: u8,
        obj_reg: u8,
        prop_id: u32,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property access (ignore cache)
        self.create_property_access_by_id(dest_reg, obj_reg, 0, prop_id as u32)
    }

    fn create_try_property_assignment_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u8,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment (ignore cache)
        self.create_property_assignment_by_id(obj_reg, value_reg, 0, prop_id as u32)
    }

    fn create_try_property_assignment_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
        _cache_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment (ignore cache)
        self.create_property_assignment_by_id(obj_reg, value_reg, _cache_id, prop_id)
    }

    fn create_put_new_own_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment
        self.create_property_assignment_by_id(obj_reg, value_reg, 0, prop_id as u32)
    }

    fn create_put_new_own_by_id_short(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u16,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment
        self.create_property_assignment_by_id(obj_reg, value_reg, 0, prop_id as u32)
    }

    fn create_put_new_own_ne_by_id(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment
        self.create_property_assignment_by_id(obj_reg, value_reg, 0, prop_id as u32)
    }

    fn create_put_new_own_ne_by_id_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        prop_id: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // For decompilation, treat this as regular property assignment
        self.create_property_assignment_by_id(obj_reg, value_reg, 0, prop_id as u32)
    }

    fn create_put_own_by_index(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        index: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        let span = Span::default();

        // Create obj[index] = value
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        // Value expression may be inlined
        let value_expr = self.register_to_expression(value_reg)?;

        // Create computed member expression for assignment target
        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, obj_expr, index_expr, false);

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

    fn create_put_own_by_index_long(
        &mut self,
        obj_reg: u8,
        value_reg: u8,
        index: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let obj_var = self.register_manager.get_variable_name(obj_reg);

        let span = Span::default();

        // Create obj[index] = value
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        let index_expr = self.ast_builder.expression_numeric_literal(
            span,
            index as f64,
            None,
            oxc_syntax::number::NumberBase::Decimal,
        );

        // Value expression may be inlined
        let value_expr = self.register_to_expression(value_reg)?;

        // Create computed member expression for assignment target
        let member_expr = self
            .ast_builder
            .alloc_computed_member_expression(span, obj_expr, index_expr, false);

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

    fn create_put_own_getter_setter_by_val(
        &mut self,
        obj_reg: u8,
        key_reg: u8,
        getter_reg: u8,
        setter_reg: u8,
        _enumerable: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let obj_var = self.register_manager.get_variable_name(obj_reg);
        let key_var = self.register_manager.get_variable_name(key_reg);
        let getter_var = self.register_manager.get_variable_name(getter_reg);
        let setter_var = self.register_manager.get_variable_name(setter_reg);

        let span = Span::default();

        // Create Object.defineProperty(obj, key, {get: getter, set: setter})
        let object_atom = self.ast_builder.allocator.alloc_str("Object");
        let object_expr = self.ast_builder.expression_identifier(span, object_atom);

        let define_prop_atom = self.ast_builder.allocator.alloc_str("defineProperty");
        let define_prop_name = self.ast_builder.identifier_name(span, define_prop_atom);
        let define_prop_expr = self.ast_builder.alloc_static_member_expression(
            span,
            object_expr,
            define_prop_name,
            false,
        );

        // Create arguments
        let obj_atom = self.ast_builder.allocator.alloc_str(&obj_var);
        let obj_expr = self.ast_builder.expression_identifier(span, obj_atom);

        let key_atom = self.ast_builder.allocator.alloc_str(&key_var);
        let key_expr = self.ast_builder.expression_identifier(span, key_atom);

        // Create descriptor object {get: getter, set: setter}
        let mut properties = self.ast_builder.vec();

        // Add get property
        let get_atom = self.ast_builder.allocator.alloc_str("get");
        let get_key = oxc_ast::ast::PropertyKey::StaticIdentifier(
            self.ast_builder
                .alloc(self.ast_builder.identifier_name(span, get_atom)),
        );
        let getter_atom = self.ast_builder.allocator.alloc_str(&getter_var);
        let getter_expr = self.ast_builder.expression_identifier(span, getter_atom);
        let get_prop = self.ast_builder.alloc_object_property(
            span,
            oxc_ast::ast::PropertyKind::Init,
            get_key,
            getter_expr,
            false,
            false,
            false,
        );
        properties.push(oxc_ast::ast::ObjectPropertyKind::ObjectProperty(get_prop));

        // Add set property
        let set_atom = self.ast_builder.allocator.alloc_str("set");
        let set_key = oxc_ast::ast::PropertyKey::StaticIdentifier(
            self.ast_builder
                .alloc(self.ast_builder.identifier_name(span, set_atom)),
        );
        let setter_atom = self.ast_builder.allocator.alloc_str(&setter_var);
        let setter_expr = self.ast_builder.expression_identifier(span, setter_atom);
        let set_prop = self.ast_builder.alloc_object_property(
            span,
            oxc_ast::ast::PropertyKind::Init,
            set_key,
            setter_expr,
            false,
            false,
            false,
        );
        properties.push(oxc_ast::ast::ObjectPropertyKind::ObjectProperty(set_prop));

        let descriptor_expr = self.ast_builder.expression_object(span, properties);

        // Create call arguments
        let mut args = self.ast_builder.vec();
        args.push(oxc_ast::ast::Argument::from(obj_expr));
        args.push(oxc_ast::ast::Argument::from(key_expr));
        args.push(oxc_ast::ast::Argument::from(descriptor_expr));

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(define_prop_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            args,
            false,
        );

        let stmt = self.ast_builder.statement_expression(span, call_expr);
        Ok(InstructionResult::Statement(stmt))
    }
}
