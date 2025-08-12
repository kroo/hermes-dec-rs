//! Integration tests for AST conversion functionality
//!
//! These tests validate end-to-end functionality of converting CFG blocks
//! to JavaScript statements, ensuring the AST-01 and AST-02 components
//! work together correctly.

use hermes_dec_rs::{
    ast::{BlockToStatementConverter, ExpressionContext},
    cfg::{Block, EdgeKind},
    generated::unified_instructions::UnifiedInstruction,
    hbc::function_table::HbcFunctionInstruction,
    hbc::InstructionIndex,
    hbc::InstructionOffset,
};
use oxc_allocator::Allocator;
use oxc_ast::AstBuilder as OxcAstBuilder;
use petgraph::Graph;

fn create_test_instruction_with_index(
    instruction: UnifiedInstruction,
    index: usize,
) -> HbcFunctionInstruction {
    HbcFunctionInstruction {
        instruction,
        offset: InstructionOffset::new(index as u32),
        function_index: 0,
        instruction_index: InstructionIndex::new(index),
    }
}

// Keep the old function for backward compatibility with a default index counter
static mut INSTRUCTION_COUNTER: usize = 0;

fn create_test_instruction(instruction: UnifiedInstruction) -> HbcFunctionInstruction {
    unsafe {
        let idx = INSTRUCTION_COUNTER;
        INSTRUCTION_COUNTER += 1;
        create_test_instruction_with_index(instruction, idx)
    }
}

fn reset_instruction_counter() {
    unsafe {
        INSTRUCTION_COUNTER = 0;
    }
}

#[test]
fn test_end_to_end_simple_function() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create a simple function: let x = 42; return x;
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1, // register 1 = 42
            operand_1: 42,
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 1, // return register 1
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(result.is_ok(), "Integration test should succeed");

    let statements = result.unwrap();
    assert_eq!(
        statements.len(),
        2,
        "Should generate two statements: var decl + return"
    );

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 2);
    assert_eq!(stats.statements_generated, 2);
    assert_eq!(stats.variables_declared, 1);
}

#[test]
fn test_end_to_end_arithmetic_chain() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create arithmetic chain: let a = 10; let b = 20; let c = a + b; return c;
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1, // r1 = 10
            operand_1: 10,
        }),
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2, // r2 = 20
            operand_1: 20,
        }),
        create_test_instruction(UnifiedInstruction::Add {
            operand_0: 3, // r3 = r1 + r2
            operand_1: 1,
            operand_2: 2,
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 3, // return r3
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(result.is_ok(), "Arithmetic chain conversion should succeed");

    let statements = result.unwrap();
    assert_eq!(statements.len(), 4, "Should generate four statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 4);
    assert_eq!(stats.statements_generated, 4);
    assert_eq!(stats.variables_declared, 3); // r1, r2, r3 declared
}

#[test]
fn test_end_to_end_function_call() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create function call: let fn = true; let result = fn(); return result;
    // Using LoadConstTrue instead of LoadConstString to avoid string table dependency
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstTrue {
            operand_0: 1, // r1 = true (function reference placeholder)
        }),
        create_test_instruction(UnifiedInstruction::Call {
            operand_0: 2, // r2 = call result
            operand_1: 1, // function in r1
            operand_2: 0, // 0 arguments
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 2, // return call result
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    if let Err(e) = &result {
        println!("Function call conversion error: {:?}", e);
    }
    assert!(
        result.is_ok(),
        "Function call conversion should succeed: {:?}",
        result.as_ref().err()
    );

    let statements = result.unwrap();
    assert_eq!(statements.len(), 3, "Should generate three statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 3);
    assert_eq!(stats.statements_generated, 3);
    assert_eq!(stats.variables_declared, 2); // r1, r2 declared
}

#[test]
fn test_end_to_end_member_access() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create member access: let obj = 42; let prop = obj[1]; return prop;
    // Using numeric constants to avoid string table dependency
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1, // r1 = 42 (object reference placeholder)
            operand_1: 42,
        }),
        create_test_instruction(UnifiedInstruction::GetByVal {
            operand_0: 2, // r2 = obj[idx]
            operand_1: 1, // object in r1
            operand_2: 1, // index in r1 (reusing)
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 2, // return property value
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(result.is_ok(), "Member access conversion should succeed");

    let statements = result.unwrap();
    assert_eq!(statements.len(), 3, "Should generate three statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 3);
    assert_eq!(stats.statements_generated, 3);
    assert_eq!(stats.variables_declared, 2); // r1, r2 declared
}

#[test]
fn test_end_to_end_variable_assignment_sequence() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create assignment sequence: let x = 5; let y = x; let z = y; return z;
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1, // r1 = 5
            operand_1: 5,
        }),
        create_test_instruction(UnifiedInstruction::Mov {
            operand_0: 2, // r2 = r1
            operand_1: 1,
        }),
        create_test_instruction(UnifiedInstruction::Mov {
            operand_0: 3, // r3 = r2
            operand_1: 2,
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 3, // return r3
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(
        result.is_ok(),
        "Assignment sequence conversion should succeed"
    );

    let statements = result.unwrap();
    assert_eq!(statements.len(), 4, "Should generate four statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 4);
    assert_eq!(stats.statements_generated, 4);
    assert_eq!(stats.variables_declared, 3); // r1, r2, r3 declared
}

#[test]
fn test_end_to_end_side_effect_statements() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create side effect operations: call function (discard result), load const, return const
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1, // r1 = 42 (function reference placeholder)
            operand_1: 42,
        }),
        create_test_instruction(UnifiedInstruction::Call {
            operand_0: 2, // r2 = call result (but may be ignored if side effects only)
            operand_1: 1, // function in r1
            operand_2: 0, // 0 arguments
        }),
        create_test_instruction(UnifiedInstruction::LoadConstTrue {
            operand_0: 3, // r3 = true
        }),
        create_test_instruction(UnifiedInstruction::Ret {
            operand_0: 3, // return true
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(result.is_ok(), "Side effect conversion should succeed");

    let statements = result.unwrap();
    assert_eq!(statements.len(), 4, "Should generate four statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 4);
    assert_eq!(stats.statements_generated, 4);
}

#[test]
fn test_end_to_end_throw_statement() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create throw statement: let error = 404; throw error;
    let instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,  // r1 = 42 (error placeholder)
            operand_1: 42, // Simple error code
        }),
        create_test_instruction(UnifiedInstruction::Throw {
            operand_0: 1, // throw r1
        }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Convert the block
    let result = converter.convert_block(&block, block_id, &cfg);
    assert!(result.is_ok(), "Throw statement conversion should succeed");

    let statements = result.unwrap();
    assert_eq!(
        statements.len(),
        2,
        "Should generate two statements: var decl + throw"
    );

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 2);
    assert_eq!(stats.statements_generated, 2);
    assert_eq!(stats.variables_declared, 1); // r1 declared
}

#[test]
fn test_multiple_blocks_conversion() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Test converting multiple blocks independently
    let block1_instructions = vec![create_test_instruction(
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 10,
        },
    )];

    let block2_instructions = vec![
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 20,
        }),
        create_test_instruction(UnifiedInstruction::Ret { operand_0: 2 }),
    ];

    let block1 = Block::new(InstructionIndex::new(0), block1_instructions);
    let block2 = Block::new(InstructionIndex::new(1), block2_instructions);

    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block1_id = cfg.add_node(block1.clone());
    let block2_id = cfg.add_node(block2.clone());

    // Convert first block
    let result1 = converter.convert_block(&block1, block1_id, &cfg);
    assert!(result1.is_ok(), "First block conversion should succeed");
    let statements1 = result1.unwrap();
    assert_eq!(
        statements1.len(),
        1,
        "First block should generate one statement"
    );

    // Convert second block
    let result2 = converter.convert_block(&block2, block2_id, &cfg);
    assert!(result2.is_ok(), "Second block conversion should succeed");
    let statements2 = result2.unwrap();
    assert_eq!(
        statements2.len(),
        2,
        "Second block should generate two statements"
    );

    // Verify overall statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 2);
    assert_eq!(stats.instructions_converted, 3); // 1 + 2 instructions
    assert_eq!(stats.statements_generated, 3); // 1 + 2 statements
}

#[test]
fn test_converter_reset_and_reuse() {
    reset_instruction_counter();
    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Convert first block
    let instructions1 = vec![create_test_instruction(UnifiedInstruction::LoadConstTrue {
        operand_0: 1,
    })];
    let block1 = Block::new(InstructionIndex::new(0), instructions1);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block1_id = cfg.add_node(block1.clone());

    let result1 = converter.convert_block(&block1, block1_id, &cfg);
    assert!(result1.is_ok());

    // Check stats before reset
    let stats_before = converter.get_stats();
    assert_eq!(stats_before.blocks_processed, 1);

    // Reset stats
    converter.reset_stats();
    let stats_after_reset = converter.get_stats();
    assert_eq!(stats_after_reset.blocks_processed, 0);

    // Convert another block
    let instructions2 = vec![create_test_instruction(
        UnifiedInstruction::LoadConstFalse { operand_0: 2 },
    )];
    let block2 = Block::new(InstructionIndex::new(1), instructions2);
    let block2_id = cfg.add_node(block2.clone());

    let result2 = converter.convert_block(&block2, block2_id, &cfg);
    assert!(result2.is_ok());

    // Verify stats are reset and counting from zero
    let final_stats = converter.get_stats();
    assert_eq!(final_stats.blocks_processed, 1); // Only counting since reset
}

#[test]
fn test_performance_targets() {
    reset_instruction_counter();
    use std::time::Instant;

    let allocator = Allocator::default();
    let ast_builder = OxcAstBuilder::new(&allocator);
    let context = ExpressionContext::new();
    let mut converter = BlockToStatementConverter::new(&ast_builder, context, false);

    // Create a moderately complex block with 20 instructions
    let instructions = vec![
        // Load constants
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 10,
        }),
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 20,
        }),
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 30,
        }),
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 4,
            operand_1: 40,
        }),
        create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
            operand_0: 5,
            operand_1: 50,
        }),
        // Arithmetic operations
        create_test_instruction(UnifiedInstruction::Add {
            operand_0: 6,
            operand_1: 1,
            operand_2: 2,
        }),
        create_test_instruction(UnifiedInstruction::Sub {
            operand_0: 7,
            operand_1: 3,
            operand_2: 4,
        }),
        create_test_instruction(UnifiedInstruction::Mul {
            operand_0: 8,
            operand_1: 5,
            operand_2: 6,
        }),
        create_test_instruction(UnifiedInstruction::Div {
            operand_0: 9,
            operand_1: 7,
            operand_2: 8,
        }),
        // Variable operations
        create_test_instruction(UnifiedInstruction::Mov {
            operand_0: 10,
            operand_1: 9,
        }),
        create_test_instruction(UnifiedInstruction::Mov {
            operand_0: 11,
            operand_1: 10,
        }),
        create_test_instruction(UnifiedInstruction::Mov {
            operand_0: 12,
            operand_1: 11,
        }),
        // Function call
        create_test_instruction(UnifiedInstruction::Call {
            operand_0: 13,
            operand_1: 12,
            operand_2: 0,
        }),
        // More arithmetic
        create_test_instruction(UnifiedInstruction::Add {
            operand_0: 14,
            operand_1: 13,
            operand_2: 1,
        }),
        create_test_instruction(UnifiedInstruction::Sub {
            operand_0: 15,
            operand_1: 14,
            operand_2: 2,
        }),
        // Unary operations
        create_test_instruction(UnifiedInstruction::Negate {
            operand_0: 16,
            operand_1: 15,
        }),
        create_test_instruction(UnifiedInstruction::Not {
            operand_0: 17,
            operand_1: 16,
        }),
        // Member access
        create_test_instruction(UnifiedInstruction::GetByVal {
            operand_0: 18,
            operand_1: 17,
            operand_2: 1,
        }),
        // Final operations
        create_test_instruction(UnifiedInstruction::Inc {
            operand_0: 19,
            operand_1: 18,
        }),
        create_test_instruction(UnifiedInstruction::Ret { operand_0: 19 }),
    ];

    let block = Block::new(InstructionIndex::new(0), instructions);
    let mut cfg: Graph<Block, EdgeKind> = Graph::new();
    let block_id = cfg.add_node(block.clone());

    // Measure conversion time
    let start = Instant::now();
    let result = converter.convert_block(&block, block_id, &cfg);
    let duration = start.elapsed();

    // Verify conversion succeeded
    assert!(
        result.is_ok(),
        "Performance test block conversion should succeed"
    );

    // Verify performance target: < 10ms per block
    assert!(
        duration.as_millis() < 10,
        "Block conversion took {}ms, exceeds 10ms target",
        duration.as_millis()
    );

    // Verify we generated the expected number of statements
    let statements = result.unwrap();
    assert_eq!(statements.len(), 20, "Should generate 20 statements");

    // Verify statistics
    let stats = converter.get_stats();
    assert_eq!(stats.blocks_processed, 1);
    assert_eq!(stats.instructions_converted, 20);
    assert_eq!(stats.statements_generated, 20);

    println!(
        "Performance test passed: {}μs for 20-instruction block",
        duration.as_micros()
    );
}
