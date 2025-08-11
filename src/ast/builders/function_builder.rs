//! Function AST builder with address-based comment support
//!
//! This module provides utilities for building complete function ASTs with proper
//! comment attachment using the Address-Based Comment Anchors pattern.

use crate::analysis::FunctionType;
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{
        BindingPatternKind, Declaration, Program, Statement,
    },
    AstBuilder as OxcAstBuilder,
};
use oxc_codegen::{Codegen, CodegenOptions, CommentOptions};
use oxc_span::{Span, SourceType};
use std::collections::HashMap;

use crate::ast::comments::{AddressCommentManager, PositionAssigner};


/// Builder for creating functions with address-based comment support
pub struct FunctionBuilder<'a> {
    ast_builder: &'a OxcAstBuilder<'a>,
    comment_manager: AddressCommentManager,
}

impl<'a> FunctionBuilder<'a> {
    /// Create a new function builder
    pub fn new(ast_builder: &'a OxcAstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            comment_manager: AddressCommentManager::new(),
        }
    }

    /// Build a function declaration with comments using address-based system
    pub fn build_function_with_comments(
        mut self,
        name: &str,
        params: Vec<String>,
        _default_params: &HashMap<u32, String>,
        statements: ArenaVec<'a, Statement<'a>>,
        function_type: &FunctionType,
    ) -> (Program<'a>, String) {
        let allocator = self.ast_builder.allocator;
        
        // Build formal parameters
        let mut formal_params = ArenaVec::new_in(allocator);
        for (_i, param_name) in params.iter().enumerate() {
            let param_span = Span::default();
            
            // Create binding identifier
            let binding_name = self.ast_builder.binding_identifier(
                param_span,
                self.ast_builder.atom(param_name),
            );
            
            // Create binding pattern
            let pattern = self.ast_builder.binding_pattern(
                BindingPatternKind::BindingIdentifier(self.ast_builder.alloc(binding_name)),
                None::<oxc_ast::ast::TSTypeAnnotation>,
                false,
            );
            
            // Create formal parameter
            let formal_param = self.ast_builder.formal_parameter(
                param_span,
                ArenaVec::new_in(allocator), // decorators
                pattern,
                None, // accessibility
                false, // readonly
                false, // r#override
            );
            
            formal_params.push(formal_param);
        }
        
        let params = self.ast_builder.formal_parameters(
            Span::default(),
            oxc_ast::ast::FormalParameterKind::FormalParameter,
            formal_params,
            None::<oxc_ast::ast::BindingRestElement>, // rest
        );
        
        // Create function body
        let body = self.ast_builder.function_body(
            Span::default(),
            ArenaVec::new_in(allocator), // directives
            statements,
        );
        
        // Create function
        let id = self.ast_builder.binding_identifier(
            Span::default(),
            self.ast_builder.atom(name),
        );
        
        let function = self.ast_builder.alloc(self.ast_builder.function(
            Span::default(),
            oxc_ast::ast::FunctionType::FunctionDeclaration,
            Some(id),
            false, // generator
            false, // async
            false, // declare
            None::<oxc_ast::ast::TSTypeParameterDeclaration>,  // type_parameters
            None::<oxc_ast::ast::TSThisParameter>,  // this_param
            params,
            None::<oxc_ast::ast::TSTypeAnnotation>,  // return_type
            Some(body),
        ));
        
        // Create program body with the function
        let mut program_statements = ArenaVec::new_in(allocator);
        let function_decl = Declaration::FunctionDeclaration(function);
        let function_stmt = Statement::from(function_decl);
        program_statements.push(function_stmt);
        
        // Add function type comment to the statement if needed
        if let Some(stmt) = program_statements.first() {
            match function_type {
                FunctionType::Constructor { property_count, has_methods } => {
                    self.comment_manager.add_comment(
                        stmt,
                        format!(
                            "Constructor: {} properties{}",
                            property_count,
                            if *has_methods { ", includes methods" } else { "" }
                        ),
                        crate::ast::comments::CommentKind::Block,
                        crate::ast::comments::CommentPosition::Leading,
                    );
                }
                FunctionType::Method { is_prototype_method } => {
                    self.comment_manager.add_comment(
                        stmt,
                        if *is_prototype_method {
                            "Prototype method"
                        } else {
                            "Method"
                        },
                        crate::ast::comments::CommentKind::Block,
                        crate::ast::comments::CommentPosition::Leading,
                    );
                }
                FunctionType::Anonymous { context } => {
                    use crate::analysis::function_classifier::AnonymousContext;
                    match context {
                        AnonymousContext::IIFE => {
                            self.comment_manager.add_comment(
                                stmt,
                                "IIFE (Immediately Invoked Function Expression)",
                                crate::ast::comments::CommentKind::Block,
                                crate::ast::comments::CommentPosition::Leading,
                            );
                        }
                        AnonymousContext::Callback => {
                            self.comment_manager.add_comment(
                                stmt,
                                "Callback function",
                                crate::ast::comments::CommentKind::Block,
                                crate::ast::comments::CommentPosition::Leading,
                            );
                        }
                        AnonymousContext::Unknown => {}
                    }
                }
                FunctionType::Standalone => {}
            }
        }
        
        // Assign positions to all statements
        let mut position_assigner = PositionAssigner::new();
        position_assigner.assign_positions(&mut program_statements);
        
        // Finalize comments with positions and get synthetic source
        let (comments, synthetic_source) = self.comment_manager.finalize_comments(
            position_assigner.get_address_positions()
        );
        
        // Convert comments to arena vec
        let mut comments_vec = ArenaVec::new_in(allocator);
        for comment in comments {
            comments_vec.push(comment);
        }
        
        // Allocate synthetic source in the arena
        let synthetic_source_str = allocator.alloc_str(&synthetic_source);
        
        // Create the final program with comments
        let program = self.ast_builder.program(
            Span::new(0, synthetic_source.len() as u32),
            SourceType::mjs(),
            synthetic_source_str,  // CRITICAL: Provide synthetic source for span lookup
            comments_vec,
            None, // hashbang
            ArenaVec::new_in(allocator), // directives
            program_statements, // body
        );
        
        (program, synthetic_source)
    }
    
    /// Get mutable access to the comment manager
    pub fn comment_manager(&mut self) -> &mut AddressCommentManager {
        &mut self.comment_manager
    }
}

/// Build a complete program with a function and comments using the address-based system
pub fn build_function_program<'a>(
    ast_builder: &'a OxcAstBuilder<'a>,
    name: &str,
    params: Vec<String>,
    default_params: &HashMap<u32, String>,
    statements: ArenaVec<'a, Statement<'a>>,
    function_type: &FunctionType,
    comment_manager: Option<AddressCommentManager>,
) -> (Program<'a>, String) {
    // Use provided comment manager or create a new one
    let mut builder = FunctionBuilder::new(ast_builder);
    if let Some(cm) = comment_manager {
        builder.comment_manager = cm;
    }
    
    builder.build_function_with_comments(
        name,
        params,
        default_params,
        statements,
        function_type,
    )
}

/// Generate final JavaScript code with comments using the address-based system
pub fn generate_code_with_comments<'a>(
    program: &Program<'a>,
    synthetic_source: &str,
) -> String {
    // Generate code with comments
    let result = Codegen::new()
        .with_source_text(synthetic_source)
        .with_options(CodegenOptions {
            comments: CommentOptions::default(),
            ..Default::default()
        })
        .build(program);

    result.code
}