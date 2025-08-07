//! AST builder utilities for control flow structures
//!
//! This module provides helper methods for constructing JavaScript control flow
//! AST nodes (if/else, loops, switch, try/catch, break/continue).

use oxc_allocator::Vec as AllocVec;
use oxc_ast::ast::{Expression, ForStatementInit, Statement};
use oxc_ast::AstBuilder;
use oxc_span::SPAN;

/// Utility struct for building control flow AST nodes
pub struct ControlFlowBuilder<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> ControlFlowBuilder<'a> {
    /// Create a new control flow builder
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    /// Build an if statement
    pub fn build_if_statement(
        &self,
        test: Expression<'a>,
        consequent: Statement<'a>,
        alternate: Option<Statement<'a>>,
    ) -> Statement<'a> {
        self.ast_builder
            .statement_if(SPAN, test, consequent, alternate)
    }

    /// Build a block statement from a list of statements
    pub fn build_block_statement(&self, statements: AllocVec<'a, Statement<'a>>) -> Statement<'a> {
        self.ast_builder.statement_block(SPAN, statements)
    }

    /// Build a while statement
    pub fn build_while_statement(
        &self,
        test: Expression<'a>,
        body: Statement<'a>,
    ) -> Statement<'a> {
        self.ast_builder.statement_while(SPAN, test, body)
    }

    /// Build a do-while statement
    pub fn build_do_while_statement(
        &self,
        body: Statement<'a>,
        test: Expression<'a>,
    ) -> Statement<'a> {
        self.ast_builder.statement_do_while(SPAN, body, test)
    }

    /// Build a for statement
    pub fn build_for_statement(
        &self,
        init: Option<ForStatementInit<'a>>,
        test: Option<Expression<'a>>,
        update: Option<Expression<'a>>,
        body: Statement<'a>,
    ) -> Statement<'a> {
        self.ast_builder
            .statement_for(SPAN, init, test, update, body)
    }

    /// Build a break statement
    pub fn build_break_statement(&self, label: Option<&str>) -> Statement<'a> {
        let label = label.map(|name| {
            self.ast_builder
                .label_identifier(SPAN, self.ast_builder.atom(name))
        });
        self.ast_builder.statement_break(SPAN, label)
    }

    /// Build a continue statement
    pub fn build_continue_statement(&self, label: Option<&str>) -> Statement<'a> {
        let label = label.map(|name| {
            self.ast_builder
                .label_identifier(SPAN, self.ast_builder.atom(name))
        });
        self.ast_builder.statement_continue(SPAN, label)
    }

    /// Build a return statement
    pub fn build_return_statement(&self, argument: Option<Expression<'a>>) -> Statement<'a> {
        self.ast_builder.statement_return(SPAN, argument)
    }

    /// Build a throw statement
    pub fn build_throw_statement(&self, argument: Expression<'a>) -> Statement<'a> {
        self.ast_builder.statement_throw(SPAN, argument)
    }

    /// Build a labeled statement
    pub fn build_labeled_statement(&self, label: &str, body: Statement<'a>) -> Statement<'a> {
        let label_ident = self
            .ast_builder
            .label_identifier(SPAN, self.ast_builder.atom(label));
        self.ast_builder.statement_labeled(SPAN, label_ident, body)
    }

    /// Build an empty statement
    pub fn build_empty_statement(&self) -> Statement<'a> {
        self.ast_builder.statement_empty(SPAN)
    }
}
