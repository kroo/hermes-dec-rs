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

    // 1. Should use SSA variable names in arithmetic operations (no self-references)
    assert!(
        output.contains("let var0_a = var3 + var0;"),
        "Should have correct SSA naming for var0_a = var3 + var0. Got output:\n{}",
        output
    );
    assert!(
        !output.contains("let var0_a = var3 + var0_a;"),
        "Should NOT have self-referencing SSA variables"
    );

    // 2. Should use SSA variable names in conditional expressions
    assert!(
        output.contains("} else if (var5_m === var4) {"),
        "Should use var5_m (from modulo operation) in conditional. Got output:\n{}",
        output
    );
    assert!(
        !output.contains("} else if (var5 === var4) {"),
        "Should NOT use stale var5 in conditional"
    );

    // 3. Should have proper nested structure
    assert!(
        output.contains("if (var3 > var4) {")
            && output.contains("if (var0 > var4) {")
            && output.contains("if (var2 > var4) {"),
        "Should have nested if statements"
    );

    // 4. Should compute var5_m from modulo operation
    assert!(
        output.contains("let var5_m = var1 % var5_l;"),
        "Should compute var5_m from modulo operation"
    );

    // 5. Should have proper else-if chains
    assert!(
        output.contains("} else if (var2 < var5_k) {"),
        "Should have else-if for var2 < var5_k"
    );

    // 6. Should use SSA values consistently in nested blocks
    assert!(
        output.contains("let var0_b = var0_a + var2;"),
        "Should build on var0_a in nested block"
    );
    assert!(
        output.contains("let var0_c = var0_b + var1;"),
        "Should build on var0_b in nested block"
    );

    // Print the output for debugging if test fails
    if output.contains("var0_a = var3 + var0_a") || output.contains("} else if (var5 === var4) {") {
        eprintln!("Generated code with SSA issues:\n{}", output);
    }
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
