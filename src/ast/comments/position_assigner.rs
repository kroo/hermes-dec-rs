//! Position assignment for address-based comment system
//!
//! This module provides AST traversal to assign final positions to nodes
//! for the address-based comment system.

use oxc_allocator::{Address, GetAddress};
use oxc_ast::ast::*;
use oxc_ast_visit::{walk_mut::*, VisitMut};
use std::collections::HashMap;

/// Visitor that assigns final positions to AST nodes using their addresses
pub struct PositionAssigner {
    current_pos: u32,
    address_to_position: HashMap<Address, u32>,
}

impl PositionAssigner {
    pub fn new() -> Self {
        Self {
            current_pos: 0,
            address_to_position: HashMap::new(),
        }
    }

    /// Assign positions by traversing the AST
    pub fn assign_positions<'a>(&mut self, statements: &mut [Statement<'a>]) {
        for stmt in statements {
            self.visit_statement(stmt);
        }
    }

    /// Get the final position mapping
    pub fn get_address_positions(&self) -> &HashMap<Address, u32> {
        &self.address_to_position
    }

    /// Helper to get boxed statement if available
    fn get_boxed_statement<'a>(stmt: &'a Statement<'a>) -> Option<&'a dyn GetAddress> {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => Some(expr_stmt as &dyn GetAddress),
            Statement::BlockStatement(block) => Some(block as &dyn GetAddress),
            Statement::IfStatement(if_stmt) => Some(if_stmt as &dyn GetAddress),
            Statement::SwitchStatement(switch) => Some(switch as &dyn GetAddress),
            Statement::ReturnStatement(ret) => Some(ret as &dyn GetAddress),
            Statement::VariableDeclaration(var_decl) => Some(var_decl as &dyn GetAddress),
            Statement::FunctionDeclaration(func_decl) => Some(func_decl as &dyn GetAddress),
            Statement::ForStatement(for_stmt) => Some(for_stmt as &dyn GetAddress),
            Statement::WhileStatement(while_stmt) => Some(while_stmt as &dyn GetAddress),
            Statement::DoWhileStatement(do_while) => Some(do_while as &dyn GetAddress),
            Statement::BreakStatement(break_stmt) => Some(break_stmt as &dyn GetAddress),
            Statement::ContinueStatement(continue_stmt) => Some(continue_stmt as &dyn GetAddress),
            Statement::ThrowStatement(throw_stmt) => Some(throw_stmt as &dyn GetAddress),
            Statement::TryStatement(try_stmt) => Some(try_stmt as &dyn GetAddress),
            Statement::LabeledStatement(labeled) => Some(labeled as &dyn GetAddress),
            Statement::WithStatement(with_stmt) => Some(with_stmt as &dyn GetAddress),
            _ => None,
        }
    }
}

impl<'a> VisitMut<'a> for PositionAssigner {
    fn visit_program(&mut self, program: &mut Program<'a>) {
        walk_program(self, program);
    }

    fn visit_statement(&mut self, stmt: &mut Statement<'a>) {
        // For statements that can have an address, assign position
        if let Some(addressable) = Self::get_boxed_statement(stmt) {
            let addr = addressable.address();
            self.address_to_position.insert(addr, self.current_pos);
            
            // Update the statement's span to the final position if possible
            match stmt {
                Statement::ExpressionStatement(expr_stmt) => {
                    expr_stmt.span.start = self.current_pos;
                    expr_stmt.span.end = self.current_pos + 1;
                }
                Statement::BlockStatement(block) => {
                    block.span.start = self.current_pos;
                    block.span.end = self.current_pos + 1;
                }
                Statement::IfStatement(if_stmt) => {
                    if_stmt.span.start = self.current_pos;
                    if_stmt.span.end = self.current_pos + 1;
                }
                Statement::SwitchStatement(switch) => {
                    switch.span.start = self.current_pos;
                    switch.span.end = self.current_pos + 1;
                }
                Statement::ReturnStatement(ret) => {
                    ret.span.start = self.current_pos;
                    ret.span.end = self.current_pos + 1;
                }
                Statement::VariableDeclaration(var_decl) => {
                    var_decl.span.start = self.current_pos;
                    var_decl.span.end = self.current_pos + 1;
                }
                Statement::FunctionDeclaration(func_decl) => {
                    func_decl.span.start = self.current_pos;
                    func_decl.span.end = self.current_pos + 1;
                }
                _ => {
                    // For other statement types, we can't directly update spans
                    // but the address mapping is still recorded
                }
            }
            
            self.current_pos += 100; // Leave space for comments
        }
        
        walk_statement(self, stmt);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        // We could also handle expressions if needed
        walk_expression(self, expr);
    }
}

impl Default for PositionAssigner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxc_allocator::Allocator;
    use oxc_ast::AstBuilder;
    use oxc_span::SPAN;

    #[test]
    fn test_position_assigner() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);
        
        // Create some test statements
        let return_stmt = ast_builder.statement_return(SPAN, None);
        let addr = return_stmt.address();
        
        let mut statements = vec![return_stmt];
        let mut assigner = PositionAssigner::new();
        
        assigner.assign_positions(&mut statements);
        
        let positions = assigner.get_address_positions();
        assert!(positions.contains_key(&addr));
        assert_eq!(positions[&addr], 0);
    }
}