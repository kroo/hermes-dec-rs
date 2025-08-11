//! Integration tests for complex nested conditionals with SSA
//!
//! This test ensures that the AST-05 conditional conversion correctly handles
//! deeply nested conditionals and that SSA variable naming is properly applied
//! to both conditional expressions and statements within branches.

use hermes_dec_rs::{
    ast::{BlockToStatementConverter, ExpressionContext},
    cfg::{ssa, Cfg},
    hbc::HbcFile,
};
use oxc_allocator::Allocator;
use oxc_ast::AstBuilder as OxcAstBuilder;
use oxc_codegen::{Codegen, CodegenOptions};
use oxc_span::{SourceType, Span};
use std::path::Path;

#[test]
fn test_complex_nested_conditionals_with_ssa() {
    // Load the complex control flow HBC file
    let hbc_file_path = Path::new("data/complex_control_flow.hbc");
    assert!(hbc_file_path.exists(), "Test file not found");

    let file_data = std::fs::read(hbc_file_path).expect("Failed to read test file");
    let hbc_file = HbcFile::parse(&file_data).expect("Failed to parse HBC file");

    // Find the complexNestedConditionals function (function 2)
    let function_index = 2;

    // Create and build CFG
    let mut cfg = Cfg::new(&hbc_file, function_index as u32);
    cfg.build();

    // Run SSA analysis
    let ssa_analysis =
        ssa::construct_ssa(&cfg, function_index as u32).expect("Failed to run SSA analysis");

    // Convert to AST with SSA
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::with_ssa_analysis(
        &ast_builder,
        context,
        false,
        false,
        ssa_analysis,
        &cfg,
    );

    // Convert all blocks to statements
    let statements = converter
        .convert_blocks_from_cfg(&cfg)
        .expect("Failed to convert CFG to AST");

    // Generate JavaScript code
    let _codegen_options = CodegenOptions::default();
    let codegen = Codegen::new();

    // Create a minimal program to hold our statements
    let source_type = SourceType::default();
    let directives = ast_builder.vec();
    let hashbang = None;
    let span = Span::default();
    let source_text = "";
    let comments = ast_builder.vec();
    let body = ast_builder.vec_from_iter(statements);

    let program = ast_builder.program(
        span,
        source_type,
        source_text,
        comments,
        hashbang,
        directives,
        body,
    );

    let output = codegen.build(&program).code;

    // Key assertions for the complex nested conditionals:

    // 1. Should use SSA variable names with parameters (arg0, arg1, etc.)
    assert!(
        output.contains("arg3 + arg0") && output.contains("const var"),
        "Should have arithmetic operations with parameters (arg3 + arg0). Got output:\n{}",
        output
    );
    
    // 2. Should NOT have self-referencing SSA variables in arithmetic  
    let has_self_reference = output.lines().any(|line| {
        if let Some(var_name) = line.trim_start().strip_prefix("const ").and_then(|s| s.split(' ').next()) {
            line.contains(&format!(" = {} +", var_name)) || 
            line.contains(&format!(" = {} -", var_name)) || 
            line.contains(&format!("+ {} ", var_name)) ||
            line.contains(&format!("- {} ", var_name))
        } else {
            false
        }
    });
    assert!(
        !has_self_reference,
        "Should NOT have self-referencing SSA variables. Got output:\n{}",
        output
    );

    // 3. Should use proper parameter names (arg0, arg1, arg2, arg3)
    assert!(
        output.contains("let arg3 = arg0;") && 
        output.contains("let arg0 = arg1;") && 
        output.contains("let arg2 = arg2;") &&
        output.contains("let arg1 = arg3;"),
        "Should have proper parameter setup. Got output:\n{}",
        output
    );

    // 4. Should have proper nested structure with parameters
    assert!(
        output.contains("if (arg3 >") 
            && output.contains("if (arg0 >") 
            && output.contains("if (arg2 >"),
        "Should have nested if statements with parameters. Got output:\n{}",
        output
    );

    // 5. Should have proper else-if chains with parameters
    assert!(
        output.contains("} else if (arg2 <") || output.contains("} else if (arg3 <"),
        "Should have else-if chains with parameters. Got output:\n{}",
        output
    );

    // 6. Should have return statements in branches
    assert!(
        output.matches("return ").count() >= 3,
        "Should have multiple return statements in different branches. Got output:\n{}",
        output
    );
}

#[test]
fn test_ssa_variable_progression() {
    // This test specifically checks that SSA versions progress correctly
    let hbc_file_path = Path::new("data/complex_control_flow.hbc");
    assert!(hbc_file_path.exists(), "Test file not found");

    let file_data = std::fs::read(hbc_file_path).expect("Failed to read test file");
    let hbc_file = HbcFile::parse(&file_data).expect("Failed to parse HBC file");

    // Find the complexNestedConditionals function (function 2)
    let function_index = 2;

    // Create and build CFG
    let mut cfg = Cfg::new(&hbc_file, function_index as u32);
    cfg.build();

    // Run SSA analysis
    let ssa_analysis =
        ssa::construct_ssa(&cfg, function_index as u32).expect("Failed to run SSA analysis");

    // Check that SSA has created proper versions for register 5
    let r5_versions: Vec<_> = ssa_analysis
        .ssa_values
        .values()
        .filter(|v| v.register == 5)
        .map(|v| v.version)
        .collect();

    assert!(
        r5_versions.len() >= 3,
        "Register 5 should have multiple SSA versions (found: {:?})",
        r5_versions
    );

    // Check that SSA has created proper versions for register 0
    let r0_versions: Vec<_> = ssa_analysis
        .ssa_values
        .values()
        .filter(|v| v.register == 0)
        .map(|v| v.version)
        .collect();

    assert!(
        r0_versions.len() >= 2,
        "Register 0 should have multiple SSA versions for the Add operations"
    );
}
