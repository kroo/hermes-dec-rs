#![allow(dead_code)]
//! Main decompiler module
//!
//! This module orchestrates the entire decompilation process from HBC to JavaScript.

use crate::ast::{BlockToStatementConverter, ExpressionContext};
use crate::cfg::Cfg;
use crate::hbc::HbcFile;
use crate::{DecompilerError, DecompilerResult};
use oxc_allocator::Allocator;
use oxc_ast::AstBuilder as OxcAstBuilder;
use oxc_codegen::Codegen;

/// Main decompiler struct
pub struct Decompiler {}

impl Decompiler {
    /// Create a new decompiler
    pub fn new() -> DecompilerResult<Self> {
        Ok(Decompiler {})
    }

    /// Decompile an HBC file to JavaScript
    pub fn decompile(&mut self, hbc_file: &HbcFile) -> DecompilerResult<String> {
        // Process functions sequentially (for now)
        let mut modules = Vec::new();
        for i in 0..hbc_file.functions.count() {
            modules.push(self.decompile_function(hbc_file, i)?);
        }
        // Combine modules (for now, just take the first one)
        let module = modules
            .into_iter()
            .next()
            .ok_or_else(|| DecompilerError::Internal {
                message: "No functions found in HBC file".to_string(),
            })?;

        // Generate JavaScript code
        self.generate_code(&module)
    }

    /// Decompile a single function
    pub fn decompile_function(
        &self,
        hbc_file: &HbcFile,
        function_index: u32,
    ) -> DecompilerResult<String> {
        self.decompile_function_with_options(hbc_file, function_index, "none")
    }

    /// Decompile a single function with options
    pub fn decompile_function_with_options(
        &self,
        hbc_file: &HbcFile,
        function_index: u32,
        comments: &str,
    ) -> DecompilerResult<String> {
        // Build CFG from instructions
        let mut cfg = Cfg::new(hbc_file, function_index);
        cfg.build();

        // Create allocator and AST builder
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);
        
        // Create expression context with HBC file access
        let expression_context = ExpressionContext::with_context(hbc_file, function_index, 0);
        
        // Determine if instruction comments should be included
        let include_instruction_comments = comments == "instructions";
        
        // Create block-to-statement converter
        let mut converter = BlockToStatementConverter::new(&ast_builder, expression_context, include_instruction_comments);
        
        // Process all blocks in the CFG with proper ordering and labeling
        let all_statements = match converter.convert_blocks_from_cfg(&cfg) {
            Ok(statements) => statements,
            Err(e) => {
                return Err(DecompilerError::Internal {
                    message: format!("Failed to convert blocks: {}", e),
                });
            }
        };
        
        // For now, just generate code for the statements directly
        // We'll wrap them in a function declaration as text
        let function_name = format!("function_{}", function_index);
        
        // Create a temporary program with just the statements
        let program = ast_builder.program(
            oxc_span::Span::default(),
            oxc_ast::ast::SourceType::default(),
            "", // source text  
            oxc_allocator::Vec::new_in(&allocator), // directives
            None, // hashbang
            oxc_allocator::Vec::new_in(&allocator), // body
            all_statements, // statements
        );
        
        // Generate JavaScript code using oxc_codegen
        let codegen = Codegen::new();
        let result = codegen.build(&program);
        
        // Post-process the generated code to convert void expressions to comments
        let processed_code = self.convert_void_expressions_to_comments(&result.code);
        
        // Wrap the processed code in a function declaration
        let code = format!("function {}() {{\n{}\n}}", function_name, processed_code.trim());
        
        Ok(code)
    }

    /// Generate JavaScript code from AST
    fn generate_code(&self, code: &str) -> DecompilerResult<String> {
        // For now, just return the code as-is
        Ok(code.to_string())
    }

    /// Convert void expressions in generated code to JavaScript comments
    /// Looks for patterns like `void "/* comment */";` and converts them to `/* comment */`
    fn convert_void_expressions_to_comments(&self, code: &str) -> String {
        use regex::Regex;
        
        // Pattern to match void expressions with string literals containing comment markers
        let void_comment_regex = Regex::new(r#"void\s+"(/\*[^"]*\*/)"\s*;"#).unwrap();
        
        // Replace void expressions with actual comments
        let processed = void_comment_regex.replace_all(code, |caps: &regex::Captures| {
            // Extract the comment content from the captured group
            let comment = &caps[1];
            format!("{}", comment)
        });
        
        processed.to_string()
    }

    /// Decompile HBC file with options
    pub fn decompile_with_options(
        &mut self,
        _hbc_file: &HbcFile,
        _minify: bool,
        _include_comments: bool,
    ) -> DecompilerResult<String> {
        // TODO: Implement decompilation with options
        // 1. Parse HBC file
        // 2. Build CFG for each function
        // 3. Structure control flow
        // 4. Generate AST
        // 5. Apply minification if requested
        // 6. Add comments if requested
        // 7. Generate code

        Ok("// TODO: Implement decompilation".to_string())
    }
}
