//! Iterator helper methods
//!
//! This module provides helper methods for generating statements for

use super::{InstructionResult, InstructionToStatementConverter, StatementConversionError};
use oxc_ast::ast::VariableDeclarationKind;
use oxc_span::Span;

/// Trait providing miscellaneous operation helper methods
pub trait IteratorHelpers<'a> {
    fn create_iterator_begin(
        &mut self,
        dest_reg: u8,
        iterable_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_iterator_next(
        &mut self,
        dest_reg: u8,
        iterator_reg: u8,
        _source_or_next_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_iterator_close(
        &mut self,
        iterator_reg: u8,
        _method_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_generator(
        &mut self,
        dest_reg: u8,
        iterable_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_save_generator(
        &mut self,
        dest_reg: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;

    fn create_resume_generator(
        &mut self,
        dest_reg: u8,
        iterable_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError>;
}

impl<'a> IteratorHelpers<'a> for InstructionToStatementConverter<'a> {
    /// Create iterator begin: `let var0_1 = iterable[Symbol.iterator]();`
    fn create_iterator_begin(
        &mut self,
        dest_reg: u8,
        iterable_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let iterable_var = self.register_manager.get_variable_name(iterable_reg);

        let span = Span::default();

        // Create iterable[Symbol.iterator]()
        let iterable_atom = self.ast_builder.allocator.alloc_str(&iterable_var);
        let iterable_expr = self.ast_builder.expression_identifier(span, iterable_atom);

        // Create Symbol.iterator
        let symbol_atom = self.ast_builder.allocator.alloc_str("Symbol");
        let symbol_expr = self.ast_builder.expression_identifier(span, symbol_atom);

        let iterator_atom = self.ast_builder.allocator.alloc_str("iterator");
        let iterator_name = self.ast_builder.identifier_name(span, iterator_atom);
        let symbol_iterator = self.ast_builder.alloc_static_member_expression(
            span,
            symbol_expr,
            iterator_name,
            false,
        );

        // Create iterable[Symbol.iterator]
        let member_expr = self.ast_builder.alloc_computed_member_expression(
            span,
            iterable_expr,
            oxc_ast::ast::Expression::StaticMemberExpression(symbol_iterator),
            false,
        );

        // Create call expression
        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::ComputedMemberExpression(member_expr),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            self.ast_builder.vec(),
            false,
        );

        let stmt = self.create_variable_declaration(
            &dest_var,
            Some(call_expr),
            VariableDeclarationKind::Let,
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create iterator next: `let var0_1 = iterator.next();`
    /// Reg8, Reg8, Reg8 (total size 3)
    ///
    /// Arg1 (%0) [out] is the result, or undefined if done.
    /// Arg2 (%iterator) [in/out] is the iterator or index.
    /// Arg3 (%sourceOrNext) [in] is the source or the next method.
    ///
    /// if (typeof %iterator === 'undefined') {
    ///   return undefined;
    /// }
    /// if (typeof %iterator === 'number') {
    ///   if (%iterator >= %sourceOrNext.length) {
    ///     %iterator = undefined;
    ///     return undefined;
    ///   }
    ///   return %sourceOrNext[%iterator];
    /// }
    /// var iterResult = %sourceOrNext();
    /// if (iterResult.done) {
    ///   %iterator = undefined;
    ///   return undefined;
    /// }
    /// return iterResult.value;
    fn create_iterator_next(
        &mut self,
        dest_reg: u8,
        iterator_reg: u8,
        _source_or_next_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let iterator_var = self.register_manager.get_variable_name(iterator_reg);

        let span = Span::default();

        // Create iterator.next()
        let iterator_atom = self.ast_builder.allocator.alloc_str(&iterator_var);
        let iterator_expr = self.ast_builder.expression_identifier(span, iterator_atom);

        let next_atom = self.ast_builder.allocator.alloc_str("next");
        let next_name = self.ast_builder.identifier_name(span, next_atom);
        let next_member =
            self.ast_builder
                .alloc_static_member_expression(span, iterator_expr, next_name, false);

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(next_member),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            self.ast_builder.vec(),
            false,
        );

        let stmt = self.create_variable_declaration(
            &dest_var,
            Some(call_expr),
            VariableDeclarationKind::Let,
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create iterator close: `iterator.return && iterator.return();`
    fn create_iterator_close(
        &mut self,
        iterator_reg: u8,
        _method_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let iterator_var = self.register_manager.get_variable_name(iterator_reg);

        let span = Span::default();

        // Create iterator.return
        let iterator_atom = self.ast_builder.allocator.alloc_str(&iterator_var);
        let iterator_expr1 = self.ast_builder.expression_identifier(span, iterator_atom);
        let iterator_expr2 = self.ast_builder.expression_identifier(span, iterator_atom);

        let return_atom = self.ast_builder.allocator.alloc_str("return");
        let return_name1 = self.ast_builder.identifier_name(span, return_atom);
        let return_name2 = self.ast_builder.identifier_name(span, return_atom);

        let return_member1 = self.ast_builder.alloc_static_member_expression(
            span,
            iterator_expr1,
            return_name1,
            false,
        );

        let return_member2 = self.ast_builder.alloc_static_member_expression(
            span,
            iterator_expr2,
            return_name2,
            false,
        );

        // Create call expression iterator.return()
        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(return_member2),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            self.ast_builder.vec(),
            false,
        );

        // Create logical AND: iterator.return && iterator.return()
        let logical_expr = self.ast_builder.expression_logical(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(return_member1),
            oxc_ast::ast::LogicalOperator::And,
            call_expr,
        );

        let stmt = self.ast_builder.statement_expression(span, logical_expr);

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create generator: `let var0_1 = function* generator() {};`
    fn create_generator(
        &mut self,
        dest_reg: u8,
        _func_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);

        let span = Span::default();

        // Create a placeholder generator function
        let func_name_atom = self.ast_builder.allocator.alloc_str("generator");
        let func_name = self.ast_builder.binding_identifier(span, func_name_atom);

        let params = self.ast_builder.formal_parameters(
            span,
            oxc_ast::ast::FormalParameterKind::FormalParameter,
            self.ast_builder.vec(),
            None::<oxc_ast::ast::BindingRestElement>,
        );

        let body =
            self.ast_builder
                .function_body(span, self.ast_builder.vec(), self.ast_builder.vec());

        let func_expr = self.ast_builder.expression_function(
            span,
            oxc_ast::ast::FunctionType::FunctionExpression,
            Some(func_name),
            false,                                            // is_async
            true,                                             // is_generator
            false,                                            // is_type_script_syntax
            None::<oxc_ast::ast::TSTypeParameterDeclaration>, // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,            // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>, // return_type
            Some(body),
        );

        let stmt = self.create_variable_declaration(
            &dest_var,
            Some(func_expr),
            VariableDeclarationKind::Let,
        )?;

        Ok(InstructionResult::Statement(stmt))
    }

    /// Create save generator state (no-op)
    fn create_save_generator(
        &mut self,
        _generator_reg: u32,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        // SaveGenerator is a VM internal operation
        let span = Span::default();
        let empty_stmt = self.ast_builder.statement_empty(span);

        Ok(InstructionResult::Statement(empty_stmt))
    }

    /// Create resume generator: `let var0_1 = generator.next(value);`
    fn create_resume_generator(
        &mut self,
        dest_reg: u8,
        generator_reg: u8,
    ) -> Result<InstructionResult<'a>, StatementConversionError> {
        let dest_var = self
            .register_manager
            .create_new_variable_for_register(dest_reg);
        let generator_var = self.register_manager.get_variable_name(generator_reg);

        let span = Span::default();

        // Create generator.next() call
        let generator_atom = self.ast_builder.allocator.alloc_str(&generator_var);
        let generator_expr = self.ast_builder.expression_identifier(span, generator_atom);

        let next_atom = self.ast_builder.allocator.alloc_str("next");
        let next_name = self.ast_builder.identifier_name(span, next_atom);
        let next_member =
            self.ast_builder
                .alloc_static_member_expression(span, generator_expr, next_name, false);

        let call_expr = self.ast_builder.expression_call(
            span,
            oxc_ast::ast::Expression::StaticMemberExpression(next_member),
            None::<oxc_ast::ast::TSTypeParameterInstantiation>,
            self.ast_builder.vec(),
            false,
        );

        let stmt = self.create_variable_declaration(
            &dest_var,
            Some(call_expr),
            VariableDeclarationKind::Let,
        )?;

        Ok(InstructionResult::Statement(stmt))
    }
}
