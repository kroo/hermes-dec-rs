#![allow(dead_code)]
//! Main decompiler module
//!
//! This module orchestrates the entire decompilation process from HBC to JavaScript.

use crate::analysis::{
    DefaultParameterAnalyzer, FunctionClassifier, FunctionType, GlobalAnalysisResult,
    GlobalSSAAnalyzer,
};
use crate::ast::{BlockToStatementConverter, ExpressionContext};
use crate::cfg::Cfg;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::HbcFile;
use crate::{DecompilerError, DecompilerResult};
use oxc_allocator::Allocator;
use oxc_ast::AstBuilder as OxcAstBuilder;
use oxc_codegen::Codegen;
use std::sync::Arc;

/// Main decompiler struct
pub struct Decompiler {
    /// Cached global analysis result
    global_analysis: Option<Arc<GlobalAnalysisResult>>,
}

impl Decompiler {
    /// Create a new decompiler
    pub fn new() -> DecompilerResult<Self> {
        Ok(Decompiler {
            global_analysis: None,
        })
    }

    /// Decompile an HBC file to JavaScript
    pub fn decompile(&mut self, hbc_file: &HbcFile) -> DecompilerResult<String> {
        // Run global analysis if not already cached
        if self.global_analysis.is_none() {
            let global_result = match GlobalSSAAnalyzer::analyze(hbc_file) {
                Ok(result) => result,
                Err(e) => {
                    return Err(DecompilerError::Internal {
                        message: format!("Global analysis failed: {}", e),
                    });
                }
            };
            self.global_analysis = Some(Arc::new(global_result));
        }

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
        &mut self,
        hbc_file: &HbcFile,
        function_index: u32,
    ) -> DecompilerResult<String> {
        self.decompile_function_with_options(hbc_file, function_index, "none")
    }

    /// Decompile a single function with options
    pub fn decompile_function_with_options(
        &mut self,
        hbc_file: &HbcFile,
        function_index: u32,
        comments: &str,
    ) -> DecompilerResult<String> {
        // Ensure global analysis is run
        if self.global_analysis.is_none() {
            let global_result = match GlobalSSAAnalyzer::analyze(hbc_file) {
                Ok(result) => result,
                Err(e) => {
                    return Err(DecompilerError::Internal {
                        message: format!("Global analysis failed: {}", e),
                    });
                }
            };
            self.global_analysis = Some(Arc::new(global_result));
        }

        // Get the global analysis
        let global_analysis = self.global_analysis.as_ref().unwrap();

        // Build CFG from instructions
        let mut cfg = Cfg::new(hbc_file, function_index);
        cfg.build();

        // Create allocator and AST builder
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);

        // Create expression context with HBC file access
        let expression_context = ExpressionContext::with_context(hbc_file, function_index, 0);

        // Get the actual function name from the HBC file before moving expression_context
        let function_name = expression_context
            .lookup_function_name(function_index)
            .unwrap_or_else(|_| format!("function_{}", function_index));

        // Get parameter count before moving expression_context
        let param_count = expression_context
            .lookup_function_param_count(function_index)
            .unwrap_or(0);

        // Determine if instruction comments should be included
        let include_instruction_comments = comments == "instructions";

        // Get the SSA analysis from global analyzer
        let ssa_analysis = match global_analysis
            .analyzer()
            .get_function_analysis(function_index)
        {
            Some(analysis) => analysis.clone(),
            None => {
                // Fallback: run local SSA if not found in global analysis
                match crate::cfg::ssa::construct_ssa(&cfg, function_index) {
                    Ok(analysis) => analysis,
                    Err(e) => {
                        return Err(DecompilerError::Internal {
                            message: format!("SSA analysis failed: {}", e),
                        });
                    }
                }
            }
        };

        // Determine if SSA comments should be included
        let include_ssa_comments = comments == "ssa";

        // Create block-to-statement converter with SSA analysis and global analyzer
        let mut converter = BlockToStatementConverter::with_ssa_and_global_analysis(
            &ast_builder,
            expression_context,
            include_instruction_comments,
            include_ssa_comments,
            ssa_analysis,
            &cfg,
            global_analysis.clone(),
        );

        // Process all blocks in the CFG with proper ordering and labeling
        let all_statements = match converter.convert_blocks_from_cfg(&cfg) {
            Ok(statements) => statements,
            Err(e) => {
                return Err(DecompilerError::Internal {
                    message: format!("Failed to convert blocks: {}", e),
                });
            }
        };

        // Analyze the function for patterns
        let (default_params, function_type) =
            match hbc_file.functions.get(function_index, hbc_file) {
                Ok(func) => {
                    // Extract UnifiedInstruction from HbcFunctionInstruction
                    let unified_instructions: Vec<UnifiedInstruction> = func
                        .instructions
                        .iter()
                        .map(|instr| instr.instruction.clone())
                        .collect();

                    let defaults = DefaultParameterAnalyzer::analyze(&unified_instructions);

                    // Get global analysis - it should always be available at this point
                    let global_analysis = self.global_analysis.as_ref().expect(
                        "Global analysis should be available during function classification",
                    );

                    let func_type = FunctionClassifier::classify(
                        function_index,
                        &unified_instructions,
                        hbc_file,
                        global_analysis,
                    );
                    (defaults, func_type)
                }
                Err(_) => {
                    // No function found, return empty results
                    (std::collections::HashMap::new(), FunctionType::Standalone)
                }
            };

        // Subtract 1 to account for implicit 'this' parameter
        let metadata_param_count = if param_count > 0 { param_count - 1 } else { 0 };

        // For the global function (function_index 0), never show parameters
        let is_global_function = function_index == 0;

        // When functions have default parameters, we need to check the actual parameter
        // indices used in LoadParam instructions to ensure we show all parameters.
        let max_param_idx = default_params.keys().max().copied().unwrap_or(0);

        // Use the maximum of the metadata count and the highest parameter index found
        let actual_param_count = if is_global_function {
            0 // Global function should never show parameters
        } else {
            metadata_param_count.max(max_param_idx + 1) as usize
        };

        // Generate parameter names with default values
        let params = if actual_param_count > 0 {
            let param_names: Vec<String> = (0..actual_param_count)
                .map(|i| {
                    let param_name = format!("arg{}", i);

                    // Check if this parameter has a default value
                    if let Some(default_info) = default_params.get(&(i as u32)) {
                        // Extract the default value from the instruction
                        let default_value = match &default_info.default_value_instruction {
                            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                                // Look up the actual string from the string table
                                hbc_file
                                    .strings
                                    .get(*operand_1 as u32)
                                    .map(|s| format!("\"{}\"", s.replace('"', "\\\"")))
                                    .unwrap_or_else(|_| "\"default\"".to_string())
                            }
                            UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => {
                                // Look up the actual string from the string table (long index version)
                                hbc_file
                                    .strings
                                    .get(*operand_1)
                                    .map(|s| format!("\"{}\"", s.replace('"', "\\\"")))
                                    .unwrap_or_else(|_| "\"default\"".to_string())
                            }
                            UnifiedInstruction::LoadConstZero { .. } => "0".to_string(),
                            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                                operand_1.to_string()
                            }
                            UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                                operand_1.to_string()
                            }
                            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                                // operand_1 is the actual double value
                                format!("{}", operand_1)
                            }
                            UnifiedInstruction::LoadConstTrue { .. } => "true".to_string(),
                            UnifiedInstruction::LoadConstFalse { .. } => "false".to_string(),
                            UnifiedInstruction::LoadConstNull { .. } => "null".to_string(),
                            UnifiedInstruction::LoadConstUndefined { .. } => {
                                "undefined".to_string()
                            }
                            UnifiedInstruction::LoadConstEmpty { .. } => "/* empty */".to_string(),
                            UnifiedInstruction::LoadConstBigInt { operand_1, .. } => {
                                // Look up the actual bigint value from the bigint table
                                hbc_file
                                    .bigints
                                    .get(*operand_1 as u32)
                                    .map(|bi| format!("{}n", bi))
                                    .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1))
                            }
                            UnifiedInstruction::LoadConstBigIntLongIndex { operand_1, .. } => {
                                hbc_file
                                    .bigints
                                    .get(*operand_1)
                                    .map(|bi| format!("{}n", bi))
                                    .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1))
                            }
                            UnifiedInstruction::NewObject { .. } => "{}".to_string(),
                            UnifiedInstruction::NewArray { .. } => "[]".to_string(),
                            UnifiedInstruction::NewArrayWithBuffer { .. } => "[]".to_string(),
                            UnifiedInstruction::NewArrayWithBufferLong { .. } => "[]".to_string(),
                            UnifiedInstruction::GetGlobalObject { .. } => "globalThis".to_string(),
                            _ => format!(
                                "/* unsupported default: {:?} */",
                                std::mem::discriminant(&default_info.default_value_instruction)
                            ),
                        };
                        format!("{} = {}", param_name, default_value)
                    } else {
                        param_name
                    }
                })
                .collect();
            format!("({})", param_names.join(", "))
        } else {
            "()".to_string()
        };

        // Create a temporary program with just the statements
        let program = ast_builder.program(
            oxc_span::Span::default(),
            oxc_ast::ast::SourceType::default(),
            "",                                     // source text
            oxc_allocator::Vec::new_in(&allocator), // directives
            None,                                   // hashbang
            oxc_allocator::Vec::new_in(&allocator), // body
            all_statements,                         // statements
        );

        // Generate JavaScript code using oxc_codegen
        let codegen = Codegen::new();
        let result = codegen.build(&program);

        // Post-process the generated code to convert void expressions to comments
        let processed_code = self.convert_void_expressions_to_comments(&result.code);

        // Add function type comment
        let function_comment = match &function_type {
            FunctionType::Constructor {
                property_count,
                has_methods,
            } => {
                format!(
                    "/* Constructor: {} properties{} */\n",
                    property_count,
                    if *has_methods {
                        ", includes methods"
                    } else {
                        ""
                    }
                )
            }
            FunctionType::Method {
                is_prototype_method,
            } => {
                if *is_prototype_method {
                    "/* Prototype method */\n".to_string()
                } else {
                    "/* Method */\n".to_string()
                }
            }
            FunctionType::Anonymous { context } => match context {
                crate::analysis::function_classifier::AnonymousContext::IIFE => {
                    "/* IIFE (Immediately Invoked Function Expression) */\n".to_string()
                }
                crate::analysis::function_classifier::AnonymousContext::Callback => {
                    "/* Callback function */\n".to_string()
                }
                crate::analysis::function_classifier::AnonymousContext::Unknown => String::new(),
            },
            FunctionType::Standalone => String::new(),
        };

        // Wrap the processed code in a function declaration with parameters
        let code = format!(
            "{}function {}{} {{\n{}\n}}",
            function_comment,
            function_name,
            params,
            processed_code.trim()
        );

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
