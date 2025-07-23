#![allow(dead_code)]
//! Abstract Syntax Tree (AST) module
//! 
//! This module handles building OXC AST nodes from CFG and instructions.

use crate::cfg::Cfg;
use oxc_allocator::Allocator;
use oxc_ast::{AstBuilder as OxcAstBuilder, ast::Program};

/// AST builder for converting CFG to OXC AST
pub struct AstBuilder<'a> {
    /// Program counter for generating unique identifiers
    pub counter: u32,

    /// Allocator for AST nodes
    allocator: &'a Allocator,

    /// OXC AST builder
    builder: OxcAstBuilder<'a>,
}

impl<'a> AstBuilder<'a> {
    /// Create a new AST builder
    pub fn new(allocator: &'a Allocator) -> Self {
        AstBuilder {
            counter: 0,
            allocator,
            builder: OxcAstBuilder::new(allocator),
        }
    }
    
    /// Build AST from CFG
    pub fn build_from_cfg(&mut self, _cfg: &Cfg) -> Program {
        // TODO: Implement AST building from CFG
        // 1. Traverse CFG in topological order
        // 2. Convert blocks to statements
        // 3. Handle control flow structures
        // 4. Generate expressions from instructions
        
        
        let span = oxc_span::Span::default();
        let source_type = oxc_span::SourceType::default();
        let comments = oxc_allocator::Vec::new_in(self.allocator);
        let directives = oxc_allocator::Vec::new_in(self.allocator);
        let body = oxc_allocator::Vec::new_in(self.allocator);

        let program = self.builder.program(
            span,
            source_type,
            "",
            comments,
            None,
            directives,
            body,
        );

        program
    }
    
    /// Create a simple expression statement
    pub fn create_expression_statement(&mut self, expr: &str) -> String {
        format!("{};", expr)
    }
    
    /// Create a binary expression
    pub fn create_binary_expression(
        &mut self,
        left: &str,
        operator: &str,
        right: &str,
    ) -> String {
        format!("({} {} {})", left, operator, right)
    }
    
    /// Create a numeric literal
    pub fn create_numeric_literal(&mut self, value: f64) -> String {
        value.to_string()
    }
    
    /// Create a string literal
    pub fn create_string_literal(&mut self, value: &str) -> String {
        format!("\"{}\"", value.escape_default())
    }
    
    /// Create an identifier
    pub fn create_identifier(&mut self, name: &str) -> String {
        name.to_string()
    }
    
    /// Create a variable declaration
    pub fn create_variable_declaration(
        &mut self,
        name: &str,
        init: Option<&str>,
    ) -> String {
        match init {
            Some(value) => format!("var {} = {};", name, value),
            None => format!("var {};", name),
        }
    }
    
    /// Generate a unique identifier
    pub fn generate_id(&mut self) -> String {
        self.counter += 1;
        format!("_var{}", self.counter)
    }
} 