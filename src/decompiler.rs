#![allow(dead_code)]
//! Main decompiler module
//!
//! This module orchestrates the entire decompilation process from HBC to JavaScript.

use crate::analysis::{
    DefaultParameterAnalyzer, FunctionClassifier, FunctionType, GlobalAnalysisResult,
    GlobalSSAAnalyzer,
};
use crate::ast::{
    build_function_program, generate_code_with_comments, AddressCommentManager, ExpressionContext,
    InstructionIndex,
};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::HbcFile;
use crate::{DecompilerError, DecompilerResult};
use oxc_allocator::Allocator;
use oxc_ast::AstBuilder as OxcAstBuilder;
use std::collections::HashMap;
use std::sync::Arc;

/// Main decompiler struct
pub struct Decompiler {
    /// Cached global analysis result
    global_analysis: Option<Arc<GlobalAnalysisResult>>,
    /// Cache of decompiled functions
    function_cache: HashMap<u32, String>,
}

/// Result of function decompilation - AST node and metadata
pub struct FunctionDecompilationResult<'a> {
    /// The function body statements
    pub body_statements: oxc_allocator::Vec<'a, oxc_ast::ast::Statement<'a>>,
    /// Function name
    pub function_name: String,
    /// Function type (standalone, method, etc.)
    pub function_type: FunctionType,
    /// Default parameter information
    pub default_params: std::collections::HashMap<u32, crate::analysis::DefaultParameterInfo>,
    /// Parameter names (including default values)
    pub param_names: Vec<String>,
    /// Comment manager for the function
    pub comment_manager: Option<AddressCommentManager>,
    /// Nested function indices found during decompilation
    pub nested_functions: Vec<u32>,
}

/// Decompiler for a single function
pub struct FunctionDecompiler<'a> {
    /// Reference to the HBC file
    hbc_file: &'a HbcFile<'a>,
    /// Function index to decompile
    function_index: u32,
    /// Global analysis result (shared)
    global_analysis: Arc<GlobalAnalysisResult>,
    /// Whether to include instruction comments
    include_instruction_comments: bool,
    /// Whether to include SSA comments
    include_ssa_comments: bool,
    /// Whether to skip validation
    skip_validation: bool,
    /// Whether to decompile nested functions
    decompile_nested: bool,
}

impl Decompiler {
    /// Create a new decompiler
    pub fn new() -> DecompilerResult<Self> {
        Ok(Decompiler {
            global_analysis: None,
            function_cache: HashMap::new(),
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

        // For now, just return the first module
        Ok(module)
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
        self.decompile_function_with_full_options(hbc_file, function_index, comments, false)
    }

    /// Decompile a single function with full options
    pub fn decompile_function_with_full_options(
        &mut self,
        hbc_file: &HbcFile,
        function_index: u32,
        comments: &str,
        skip_validation: bool,
    ) -> DecompilerResult<String> {
        self.decompile_function_with_full_options_and_nested(
            hbc_file,
            function_index,
            comments,
            skip_validation,
            false, // default: don't decompile nested functions
        )
    }

    /// Decompile a single function with full options and nested function support
    pub fn decompile_function_with_full_options_and_nested(
        &mut self,
        hbc_file: &HbcFile,
        function_index: u32,
        comments: &str,
        skip_validation: bool,
        decompile_nested: bool,
    ) -> DecompilerResult<String> {
        // Check cache first
        if !decompile_nested && self.function_cache.contains_key(&function_index) {
            return Ok(self.function_cache[&function_index].clone());
        }

        // Ensure global analysis is run (lazy - functions analyzed on demand)
        if self.global_analysis.is_none() {
            log::debug!("Running global analysis for decompiler");
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

        let global_analysis = self.global_analysis.as_ref().unwrap().clone();

        // Parse comment types
        let comment_types: Vec<&str> = comments.split(',').map(|s| s.trim()).collect();
        let include_instruction_comments = comment_types.contains(&"instructions");
        let include_ssa_comments = comment_types.contains(&"ssa");

        // Create allocator and AST builder for this decompilation
        let allocator = Allocator::default();
        let ast_builder = OxcAstBuilder::new(&allocator);

        // Create a function decompiler
        let function_decompiler = FunctionDecompiler {
            hbc_file,
            function_index,
            global_analysis,
            include_instruction_comments,
            include_ssa_comments,
            skip_validation,
            decompile_nested,
        };

        // Create HBC analysis for this file
        let mut hbc_analysis = crate::analysis::HbcAnalysis::analyze(hbc_file).map_err(|e| {
            DecompilerError::Internal {
                message: format!("Failed to analyze HBC file: {}", e),
            }
        })?;

        // Decompile the function to AST
        let decompilation_result =
            function_decompiler.decompile(&allocator, &ast_builder, &mut hbc_analysis)?;

        // Convert AST to string
        let result = self.ast_to_string(
            &allocator,
            &ast_builder,
            decompilation_result,
            hbc_file,
            function_index,
        )?;

        // Cache the result if not decompiling nested functions
        if !decompile_nested {
            self.function_cache.insert(function_index, result.clone());
        }

        Ok(result)
    }

    /// Decompile a function from cache or create new decompilation
    pub fn decompile_function_cached(&self, function_index: u32) -> Option<&String> {
        self.function_cache.get(&function_index)
    }

    /// Convert AST to string
    fn ast_to_string<'a>(
        &self,
        _allocator: &'a Allocator,
        ast_builder: &'a OxcAstBuilder<'a>,
        result: FunctionDecompilationResult<'a>,
        hbc_file: &HbcFile,
        _function_index: u32,
    ) -> DecompilerResult<String> {
        // Convert default_params to the format expected by build_function_program
        let default_params_strings: HashMap<u32, String> = result
            .default_params
            .into_iter()
            .map(|(k, v)| {
                // Extract the default value from the instruction
                let default_value = match &v.default_value_instruction {
                    UnifiedInstruction::LoadConstString { operand_1, .. } => {
                        // Look up the actual string from the string table
                        hbc_file
                            .strings
                            .get(*operand_1 as u32)
                            .map(|s| format!("\"{}\"", s.replace('"', "\\\"")))
                            .unwrap_or_else(|_| "\"default\"".to_string())
                    }
                    UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => hbc_file
                        .strings
                        .get(*operand_1)
                        .map(|s| format!("\"{}\"", s.replace('"', "\\\"")))
                        .unwrap_or_else(|_| "\"default\"".to_string()),
                    UnifiedInstruction::LoadConstZero { .. } => "0".to_string(),
                    UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => operand_1.to_string(),
                    UnifiedInstruction::LoadConstInt { operand_1, .. } => operand_1.to_string(),
                    UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                        format!("{}", operand_1)
                    }
                    UnifiedInstruction::LoadConstTrue { .. } => "true".to_string(),
                    UnifiedInstruction::LoadConstFalse { .. } => "false".to_string(),
                    UnifiedInstruction::LoadConstNull { .. } => "null".to_string(),
                    UnifiedInstruction::LoadConstUndefined { .. } => "undefined".to_string(),
                    UnifiedInstruction::LoadConstEmpty { .. } => "/* empty */".to_string(),
                    UnifiedInstruction::LoadConstBigInt { operand_1, .. } => hbc_file
                        .bigints
                        .get(*operand_1 as u32)
                        .map(|bi| format!("{}n", bi))
                        .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1)),
                    UnifiedInstruction::LoadConstBigIntLongIndex { operand_1, .. } => hbc_file
                        .bigints
                        .get(*operand_1)
                        .map(|bi| format!("{}n", bi))
                        .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1)),
                    UnifiedInstruction::NewObject { .. } => "{}".to_string(),
                    UnifiedInstruction::NewArray { .. } => "[]".to_string(),
                    UnifiedInstruction::NewArrayWithBuffer { .. } => "[]".to_string(),
                    UnifiedInstruction::NewArrayWithBufferLong { .. } => "[]".to_string(),
                    UnifiedInstruction::GetGlobalObject { .. } => "globalThis".to_string(),
                    _ => format!(
                        "/* unsupported default: {:?} */",
                        std::mem::discriminant(&v.default_value_instruction)
                    ),
                };
                (k, default_value)
            })
            .collect();

        // Build the full function program
        let (program, synthetic_source) = build_function_program(
            ast_builder,
            &result.function_name,
            result.param_names,
            &default_params_strings,
            result.body_statements,
            &result.function_type,
            result.comment_manager,
        );

        // Generate the final code
        let output = generate_code_with_comments(&program, &synthetic_source);

        Ok(output)
    }
}

impl<'a> FunctionDecompiler<'a> {
    /// Decompile the function to AST
    pub fn decompile(
        &self,
        _allocator: &'a Allocator,
        ast_builder: &'a OxcAstBuilder<'a>,
        hbc_analysis: &'a mut crate::analysis::HbcAnalysis<'a>,
    ) -> DecompilerResult<FunctionDecompilationResult<'a>> {
        // First ensure the function analysis exists
        hbc_analysis
            .get_function_analysis(self.function_index)
            .map_err(|e| DecompilerError::Internal {
                message: format!("Failed to get function analysis: {}", e),
            })?;

        // Create expression context with HBC file access
        let expression_context = ExpressionContext::with_context(
            self.hbc_file,
            self.function_index,
            InstructionIndex::zero(),
        );

        // Get the actual function name from the HBC file before moving expression_context
        let function_name = expression_context
            .lookup_function_name(self.function_index)
            .unwrap_or_else(|_| format!("function_{}", self.function_index));

        // Get parameter count before moving expression_context
        let param_count = expression_context
            .lookup_function_param_count(self.function_index)
            .unwrap_or(0);

        // Now get the function analysis reference (guaranteed to exist)
        let function_analysis = hbc_analysis
            .get_function_analysis_ref(self.function_index)
            .ok_or_else(|| DecompilerError::Internal {
                message: format!("Function analysis not found after creation"),
            })?;

        // Build and analyze the control flow plan
        let plan_builder = crate::analysis::control_flow_plan_builder::ControlFlowPlanBuilder::new(
            &function_analysis.cfg,
            function_analysis,
        );
        let mut plan = plan_builder.build();

        // Analyze the plan to determine declaration and use strategies
        let analyzer = crate::analysis::control_flow_plan_analyzer::ControlFlowPlanAnalyzer::new(
            &mut plan,
            function_analysis,
        );
        analyzer.analyze();

        // Convert the plan to AST
        let mut converter = crate::ast::ControlFlowPlanConverter::new(
            ast_builder,
            self.hbc_file,
            hbc_analysis,
            self.function_index,
            function_analysis,
            plan,
            self.include_ssa_comments,
            self.include_instruction_comments,
        );
        let all_statements = converter.convert_to_ast();

        // Take the comment manager back from the converter
        let comment_manager = converter.take_comment_manager();

        // Analyze the function for patterns
        let (default_params, function_type) = match self
            .hbc_file
            .functions
            .get(self.function_index, self.hbc_file)
        {
            Ok(func) => {
                // Extract UnifiedInstruction from HbcFunctionInstruction
                let unified_instructions: Vec<UnifiedInstruction> = func
                    .instructions
                    .iter()
                    .map(|instr| instr.instruction.clone())
                    .collect();

                let defaults = DefaultParameterAnalyzer::analyze(&unified_instructions);

                let func_type = FunctionClassifier::classify(
                    self.function_index,
                    &unified_instructions,
                    self.hbc_file,
                    &self.global_analysis,
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
        let is_global_function = self.function_index == 0;

        // When functions have default parameters, we need to check the actual parameter
        // indices used in LoadParam instructions to ensure we show all parameters.
        let max_param_idx = default_params.keys().max().copied().unwrap_or(0);

        // Also check all LoadParam instructions to find the maximum parameter index
        let max_load_param_idx = if let Ok(func) = self
            .hbc_file
            .functions
            .get(self.function_index, self.hbc_file)
        {
            func.instructions
                .iter()
                .filter_map(|instr| match &instr.instruction {
                    UnifiedInstruction::LoadParam { operand_1, .. } => Some(*operand_1 as u32),
                    _ => None,
                })
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        // Use the maximum of the metadata count and the highest parameter index found
        let actual_param_count = if is_global_function {
            0 // Global function should never show parameters
        } else {
            metadata_param_count
                .max(max_param_idx + 1)
                .max(max_load_param_idx + 1) as usize
        };

        // Convert param_names vector to vector of strings
        let param_names: Vec<String> = if actual_param_count > 0 {
            (0..actual_param_count)
                .map(|i| {
                    let param_name = format!("arg{}", i);
                    // Check if this parameter has a default value
                    if let Some(default_info) = default_params.get(&(i as u32)) {
                        // Extract the default value from the instruction
                        let default_value = match &default_info.default_value_instruction {
                            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                                // Look up the actual string from the string table
                                self.hbc_file
                                    .strings
                                    .get(*operand_1 as u32)
                                    .map(|s| format!("\"{}\"", s.replace('"', "\\\"")))
                                    .unwrap_or_else(|_| "\"default\"".to_string())
                            }
                            UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => {
                                // Look up the actual string from the string table (long index version)
                                self.hbc_file
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
                                self.hbc_file
                                    .bigints
                                    .get(*operand_1 as u32)
                                    .map(|bi| format!("{}n", bi))
                                    .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1))
                            }
                            UnifiedInstruction::LoadConstBigIntLongIndex { operand_1, .. } => self
                                .hbc_file
                                .bigints
                                .get(*operand_1)
                                .map(|bi| format!("{}n", bi))
                                .unwrap_or_else(|_| format!("/* bigint[{}] */", operand_1)),
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
                .collect()
        } else {
            vec![]
        };

        Ok(FunctionDecompilationResult {
            body_statements: all_statements,
            function_name,
            function_type,
            default_params,
            param_names,
            comment_manager,
            nested_functions: Vec::new(), // TODO: Collect from converter
        })
    }
}
