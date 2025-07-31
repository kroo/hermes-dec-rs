//! Statement builder for creating OXC AST statement nodes
//!
//! This module provides utilities for creating JavaScript statements from expressions
//! and managing the conversion from instruction-level to statement-level constructs.

use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{
        AssignmentExpression, AssignmentOperator, AssignmentTarget, BindingIdentifier,
        BindingPattern, BindingPatternKind, Expression, Statement, ThrowStatement,
        VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
    },
    AstBuilder as OxcAstBuilder,
};
use oxc_span::{Atom, Span};
use std::cell::Cell;

/// Error types for statement building operations
#[derive(Debug, thiserror::Error)]
pub enum StatementBuilderError {
    #[error("Invalid expression for statement context: {0}")]
    InvalidExpression(String),
    #[error("Invalid assignment target: {0}")]
    InvalidAssignmentTarget(String),
    #[error("Missing required expression for statement: {0}")]
    MissingExpression(String),
}

/// Builder for creating OXC AST statement nodes from expressions
pub struct StatementBuilder<'a> {
    pub ast_builder: &'a OxcAstBuilder<'a>,
}

impl<'a> StatementBuilder<'a> {
    /// Create a new StatementBuilder with the given AST builder
    pub fn new(ast_builder: &'a OxcAstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    /// Create an expression statement from an expression
    /// Used for expressions with side effects that should be executed as statements
    pub fn create_expression_statement(&self, expression: Expression<'a>) -> Statement<'a> {
        self.ast_builder
            .statement_expression(Span::default(), expression)
    }

    /// Create a variable declaration statement
    /// Example: let var1 = expression;
    pub fn create_variable_declaration(
        &self,
        variable_name: &str,
        init_expression: Option<Expression<'a>>,
        kind: VariableDeclarationKind,
    ) -> Result<Statement<'a>, StatementBuilderError> {
        let span = Span::default();

        // Create the binding identifier
        let name_atom = self.ast_builder.allocator.alloc_str(variable_name);
        let binding_identifier = BindingIdentifier {
            span,
            name: Atom::from(name_atom),
            symbol_id: Cell::new(None),
        };

        // Create the binding pattern
        let binding_pattern = BindingPattern {
            kind: BindingPatternKind::BindingIdentifier(self.ast_builder.alloc(binding_identifier)),
            type_annotation: None,
            optional: false,
        };

        // Create the variable declarator
        let declarator = VariableDeclarator {
            span,
            kind: kind.clone(),
            id: binding_pattern,
            init: init_expression,
            definite: false,
        };

        // Create the declarations vector
        let mut declarations = ArenaVec::new_in(self.ast_builder.allocator);
        declarations.push(declarator);

        // Create the variable declaration
        let var_decl = VariableDeclaration {
            span,
            kind,
            declarations,
            declare: false,
        };

        Ok(Statement::VariableDeclaration(
            self.ast_builder.alloc(var_decl),
        ))
    }

    /// Create an assignment statement
    /// Example: var1 = expression;
    pub fn create_assignment_statement(
        &self,
        target_name: &str,
        value_expression: Expression<'a>,
    ) -> Result<Statement<'a>, StatementBuilderError> {
        let span = Span::default();

        // Create identifier for the assignment target
        let target_atom = self.ast_builder.allocator.alloc_str(target_name);
        let target_identifier = self.ast_builder.expression_identifier(span, target_atom);

        // Create assignment target
        let assignment_target = match target_identifier {
            Expression::Identifier(id_ref) => AssignmentTarget::AssignmentTargetIdentifier(id_ref),
            _ => {
                return Err(StatementBuilderError::InvalidAssignmentTarget(
                    "Expected identifier for assignment target".to_string(),
                ))
            }
        };

        // Create assignment expression
        let assignment_expr = AssignmentExpression {
            span,
            operator: AssignmentOperator::Assign,
            left: assignment_target,
            right: value_expression,
        };

        let assignment_expr_node =
            Expression::AssignmentExpression(self.ast_builder.alloc(assignment_expr));

        Ok(self.create_expression_statement(assignment_expr_node))
    }

    /// Create a return statement
    /// Example: return expression; or return;
    pub fn create_return_statement(&self, return_value: Option<Expression<'a>>) -> Statement<'a> {
        self.ast_builder
            .statement_return(Span::default(), return_value)
    }

    /// Create a throw statement  
    /// Example: throw expression;
    pub fn create_throw_statement(
        &self,
        throw_expression: Expression<'a>,
    ) -> Result<Statement<'a>, StatementBuilderError> {
        let throw_stmt = ThrowStatement {
            span: Span::default(),
            argument: throw_expression,
        };

        Ok(Statement::ThrowStatement(
            self.ast_builder.alloc(throw_stmt),
        ))
    }

    /// Create a block statement from a vector of statements
    /// Used for grouping multiple statements together
    pub fn create_block_statement(&self, statements: ArenaVec<'a, Statement<'a>>) -> Statement<'a> {
        self.ast_builder
            .statement_block(Span::default(), statements)
    }

    /// Check if an expression has side effects and should be converted to a statement
    pub fn has_side_effects(&self, expression: &Expression<'a>) -> bool {
        match expression {
            // Call expressions always have potential side effects
            Expression::CallExpression(_) => true,
            Expression::NewExpression(_) => true,

            // Assignment expressions have side effects
            Expression::AssignmentExpression(_) => true,
            Expression::UpdateExpression(_) => true,

            // Member expressions might have side effects (getters/setters)
            Expression::ComputedMemberExpression(_) => true,
            Expression::StaticMemberExpression(_) => true,
            Expression::PrivateFieldExpression(_) => true,

            // These typically don't have side effects
            Expression::Identifier(_) => false,
            Expression::NumericLiteral(_) => false,
            Expression::StringLiteral(_) => false,
            Expression::BooleanLiteral(_) => false,
            Expression::NullLiteral(_) => false,
            Expression::BigIntLiteral(_) => false,

            // Binary/unary expressions depend on their operands
            Expression::BinaryExpression(bin_expr) => {
                self.has_side_effects(&bin_expr.left) || self.has_side_effects(&bin_expr.right)
            }
            Expression::UnaryExpression(unary_expr) => self.has_side_effects(&unary_expr.argument),

            // For other expression types, assume they might have side effects
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxc_allocator::Allocator;

    #[test]
    fn test_statement_builder_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let builder = StatementBuilder::new(&ast_builder);

        // Test creation succeeds
        assert!(std::ptr::eq(builder.ast_builder, &ast_builder));
    }

    #[test]
    fn test_expression_statement_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let builder = StatementBuilder::new(&ast_builder);

        // Create a simple identifier expression
        let expr = ast_builder.expression_identifier(Span::default(), "test");
        let stmt = builder.create_expression_statement(expr);

        assert!(matches!(stmt, Statement::ExpressionStatement(_)));
    }

    #[test]
    fn test_variable_declaration_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let builder = StatementBuilder::new(&ast_builder);

        // Create variable declaration without initializer
        let stmt = builder
            .create_variable_declaration("myVar", None, VariableDeclarationKind::Let)
            .unwrap();

        assert!(matches!(stmt, Statement::VariableDeclaration(_)));

        // Create variable declaration with initializer
        let init_expr = ast_builder.expression_numeric_literal(
            Span::default(),
            42.0,
            Some(Atom::from("42")),
            oxc_syntax::number::NumberBase::Decimal,
        );
        let stmt_with_init = builder
            .create_variable_declaration("myVar2", Some(init_expr), VariableDeclarationKind::Const)
            .unwrap();

        assert!(matches!(stmt_with_init, Statement::VariableDeclaration(_)));
    }

    #[test]
    fn test_return_statement_creation() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let builder = StatementBuilder::new(&ast_builder);

        // Test return without value
        let return_stmt = builder.create_return_statement(None);
        assert!(matches!(return_stmt, Statement::ReturnStatement(_)));

        // Test return with value
        let return_expr = ast_builder.expression_identifier(Span::default(), "result");
        let return_stmt_with_value = builder.create_return_statement(Some(return_expr));
        assert!(matches!(
            return_stmt_with_value,
            Statement::ReturnStatement(_)
        ));
    }

    #[test]
    fn test_side_effects_detection() {
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        let builder = StatementBuilder::new(&ast_builder);

        // Test expressions without side effects
        let literal = ast_builder.expression_numeric_literal(
            Span::default(),
            42.0,
            Some(Atom::from("42")),
            oxc_syntax::number::NumberBase::Decimal,
        );
        assert!(!builder.has_side_effects(&literal));

        let identifier = ast_builder.expression_identifier(Span::default(), "var1");
        assert!(!builder.has_side_effects(&identifier));
    }
}
