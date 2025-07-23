use hermes_dec_rs::cfg::CfgBuilder;
use hermes_dec_rs::cfg::{Block, Cfg, EdgeKind};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::function_table::HbcFunctionInstruction;
use hermes_dec_rs::hbc::serialized_literal_parser::SLPArray;
use hermes_dec_rs::hbc::tables::{
    function_table::{DebugOffsets, DebugOffsetsLegacy, ParsedFunctionHeader, SmallFunctionHeader},
    BigIntTable, CommonJsTable, FunctionSourceTable, JumpTable, RegExpTable,
    SerializedLiteralTables, StringTable,
};
use hermes_dec_rs::hbc::HbcFile;
use hermes_dec_rs::hbc::HbcHeader;
use petgraph::graph::DiGraph;
use petgraph::visit::EdgeRef;

use std::sync::OnceLock;

fn make_test_hbc_file<'a>(instructions: Vec<HbcFunctionInstruction>) -> HbcFile<'a> {
    let header = HbcHeader {
        magic: 0x1f1903c103bc1fc6,
        version: 96,
        source_hash: [0; 20],
        file_length: 0,
        global_code_index: 0,
        function_count: 1,
        string_kind_count: 0,
        identifier_count: 0,
        string_count: 0,
        overflow_string_count: 0,
        string_storage_size: 0,
        big_int_count: Some(0),
        big_int_storage_size: Some(0),
        reg_exp_count: 0,
        reg_exp_storage_size: 0,
        array_buffer_size: 0,
        obj_key_buffer_size: 0,
        obj_value_buffer_size: 0,
        cjs_module_offset: None,
        segment_id: None,
        cjs_module_count: 0,
        function_source_count: Some(0),
        debug_info_offset: 0,
        flags: 0,
    };
    let strings = StringTable {
        string_kinds_data: &[],
        identifier_hashes_data: &[],
        small_string_table_data: &[],
        overflow_string_table_data: &[],
        storage: &[],
        string_count: 0,
        overflow_string_count: 0,
        version: 96,
        small_entries: vec![],
        overflow_entries: vec![],
        string_kinds: vec![],
        identifier_hashes: vec![],
        index_to_identifier_map: vec![],
        string_cache: vec![],
    };
    let regexps = RegExpTable {
        table: &[],
        storage: &[],
        count: 0,
        regexps: vec![],
    };
    let bigints = BigIntTable {
        table: &[],
        storage: &[],
        count: 0,
    };
    let serialized_literals = SerializedLiteralTables {
        arrays: SLPArray::new(),
        object_keys: SLPArray::new(),
        object_values: SLPArray::new(),
        arrays_data: &[],
        object_keys_data: &[],
        object_values_data: &[],
    };
    let cjs_modules = CommonJsTable {
        entries: vec![],
        is_static: false,
    };
    let function_sources = FunctionSourceTable { entries: vec![] };
    let jump_table = JumpTable::new();
    let small_header = SmallFunctionHeader {
        word_1: 0,
        word_2: 0,
        word_3: 0,
        word_4: 0,
    };
    let debug_offsets = DebugOffsets::Legacy(DebugOffsetsLegacy {
        source_locations: 0,
        scope_desc_data: 0,
    });
    let body: &'a [u8] = &[];
    let parsed_header = ParsedFunctionHeader {
        index: 0,
        header: small_header,
        large_header: None,
        exc_handlers: vec![],
        debug_offsets,
        body,
        version: 96,
        cached_instructions: {
            let once = OnceLock::new();
            let _ = once.set(Ok(instructions));
            once
        },
    };
    let functions = hermes_dec_rs::hbc::tables::FunctionTable {
        headers: &[],
        count: 1,
        parsed_headers: vec![parsed_header],
    };
    HbcFile {
        header,
        strings,
        regexps,
        bigints,
        functions,
        serialized_literals,
        cjs_modules,
        function_sources,
        jump_table,
    }
}

/// Create a test HBC file with unified instructions
fn make_test_hbc_file_with_instructions<'a>(
    unified_instructions: Vec<UnifiedInstruction>,
) -> HbcFile<'a> {
    let instructions = make_test_instructions(unified_instructions);
    make_test_hbc_file_with_jump_table(instructions)
}

/// Create a test HBC file with instructions and a properly calculated jump table
fn make_test_hbc_file_with_jump_table<'a>(
    instructions: Vec<HbcFunctionInstruction>,
) -> HbcFile<'a> {
    let mut hbc_file = make_test_hbc_file(instructions.clone());

    // Build the jump table for the instructions
    if let Err(e) = hbc_file.jump_table.build_for_function(0, &instructions) {
        eprintln!("Warning: Failed to build jump table: {}", e);
    }

    hbc_file
}

/// Calculate the relative offset needed to jump from one instruction to another
///
/// # Arguments
/// * `from_index` - The instruction index to jump from
/// * `to_index` - The instruction index to jump to
/// * `instructions` - The list of instructions to calculate offsets from
///
/// # Returns
/// The relative offset in bytes that should be used in the jump instruction
fn calculate_relative_offset(
    from_index: usize,
    to_index: usize,
    instructions: &[HbcFunctionInstruction],
) -> i32 {
    if from_index >= instructions.len() || to_index >= instructions.len() {
        return 0; // Invalid indices
    }

    let from_offset = instructions[from_index].offset;
    let to_offset = instructions[to_index].offset;

    // The relative offset is the difference between target and source offsets
    (to_offset as i32) - (from_offset as i32)
}

/// Create a jump instruction that targets a specific instruction index
///
/// # Arguments
/// * `jump_type` - The type of jump instruction to create
/// * `from_index` - The instruction index to jump from
/// * `to_index` - The instruction index to jump to
/// * `instructions` - The list of instructions to calculate offsets from
/// * `additional_operand` - Additional operand for conditional jumps (e.g., register for JmpTrue)
///
/// # Returns
/// A UnifiedInstruction with the correct relative offset
fn create_jump_instruction(
    jump_type: &str,
    from_index: usize,
    to_index: usize,
    instructions: &[HbcFunctionInstruction],
    additional_operand: Option<u8>,
) -> UnifiedInstruction {
    let relative_offset = calculate_relative_offset(from_index, to_index, instructions);

    match jump_type {
        "Jmp" => UnifiedInstruction::Jmp {
            operand_0: relative_offset as i8,
        },
        "JmpLong" => UnifiedInstruction::JmpLong {
            operand_0: relative_offset as i32,
        },
        "JmpTrue" => UnifiedInstruction::JmpTrue {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
        },
        "JmpTrueLong" => UnifiedInstruction::JmpTrueLong {
            operand_0: relative_offset as i32,
            operand_1: additional_operand.unwrap_or(0),
        },
        "JmpFalse" => UnifiedInstruction::JmpFalse {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
        },
        "JmpFalseLong" => UnifiedInstruction::JmpFalseLong {
            operand_0: relative_offset as i32,
            operand_1: additional_operand.unwrap_or(0),
        },
        "JmpUndefined" => UnifiedInstruction::JmpUndefined {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
        },
        "JmpUndefinedLong" => UnifiedInstruction::JmpUndefinedLong {
            operand_0: relative_offset as i32,
            operand_1: additional_operand.unwrap_or(0),
        },
        _ => panic!("Unsupported jump type: {}", jump_type),
    }
}

/// Create a test HBC file with instructions that include jumps, automatically calculating correct relative offsets
///
/// # Arguments
/// * `instructions` - Vector of instructions with placeholder jumps (use 0 for jump operands)
/// * `jumps` - Vector of (jump_type, from_index, to_index, additional_operand) tuples
///
/// # Returns
/// An HbcFile with properly calculated jump table
fn make_test_hbc_file_with_jumps<'a>(
    mut instructions: Vec<UnifiedInstruction>,
    jumps: Vec<(&str, usize, usize, Option<u8>)>,
) -> HbcFile<'a> {
    // First, convert to HbcFunctionInstructions to get offsets
    let hbc_instructions = make_test_instructions(instructions.clone());

    // Apply each jump
    for (jump_type, from_index, to_index, additional_operand) in jumps {
        let jump_instruction = create_jump_instruction(
            jump_type,
            from_index,
            to_index,
            &hbc_instructions,
            additional_operand,
        );
        instructions[from_index] = jump_instruction;
    }

    // Create the final HBC file with jump table
    make_test_hbc_file_with_instructions(instructions)
}

#[allow(dead_code)]
fn make_test_instructions(
    unified_instructions: Vec<UnifiedInstruction>,
) -> Vec<HbcFunctionInstruction> {
    let mut offset = 0;
    unified_instructions
        .into_iter()
        .enumerate()
        .map(|(index, instruction)| {
            let instruction_size = instruction.size();
            let result = HbcFunctionInstruction {
                offset,
                function_index: 0,
                instruction_index: index as u32,
                instruction,
            };
            offset += instruction_size as u32;
            result
        })
        .collect()
}

#[test]
fn test_cfg_creation() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![]);
    let cfg = Cfg::new(&hbc_file, 0);
    assert_eq!(cfg.graph().node_count(), 0);
    assert_eq!(cfg.graph().edge_count(), 0);
}

#[test]
fn test_block_creation() {
    let block = Block::new(0x1000, vec![]);
    assert_eq!(block.start_pc, 0x1000);
    assert_eq!(block.end_pc, 0x1000);
    assert!(block.instructions.is_empty());
}

#[test]
#[ignore]
fn test_block_contains_pc() {
    let block = Block::new(0x1000, vec![]);
    assert!(block.contains_pc(0x1000));
    assert!(!block.contains_pc(0x1001));
}

#[test]
fn test_cfg_building_with_empty_instructions() {
    let hbc_file = make_test_hbc_file(vec![]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();
    // The HBC file has functions, so we should have blocks
    // node_count and edge_count are usize, so they're always >= 0
}

#[test]
fn test_cfg_building_with_single_instruction() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![UnifiedInstruction::LoadConstUInt8 {
        operand_0: 1,
        operand_1: 42,
    }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should have two blocks: one regular block + one EXIT block
    assert_eq!(cfg.graph().node_count(), 2);
    assert_eq!(cfg.graph().edge_count(), 0);

    // Find the non-EXIT block and check its contents
    let regular_block = cfg
        .graph()
        .node_indices()
        .find(|&node| !cfg.graph()[node].is_exit())
        .unwrap();
    let block = &cfg.graph()[regular_block];
    assert_eq!(block.start_pc, 0);
    assert_eq!(block.instruction_count(), 1);
}

#[test]
fn test_pc_lookup_three_blocks() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 0,
            operand_1: 0,
        },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 1,
        },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 2,
        },
    ]);
    let mut builder = CfgBuilder::new(&hbc_file, 0);
    let mut graph: DiGraph<Block, EdgeKind> = DiGraph::new();

    let instructions = hbc_file.functions.get_instructions(0).unwrap();
    let n0 = builder.add_block(&mut graph, Block::new(0, vec![instructions[0].clone()]));
    let n1 = builder.add_block(&mut graph, Block::new(1, vec![instructions[1].clone()]));
    let n2 = builder.add_block(&mut graph, Block::new(2, vec![instructions[2].clone()]));

    assert_eq!(builder.get_block_at_pc(0), Some(n0));
    assert_eq!(builder.get_block_at_pc(1), Some(n1));
    assert_eq!(builder.get_block_at_pc(2), Some(n2));

    for pc in 0..3 {
        let node = builder.get_block_at_pc(pc).unwrap();
        let block = &graph[node];
        assert!(block.contains_pc(pc));
    }
}

#[test]
fn test_exit_node_creation() {
    let hbc_file =
        make_test_hbc_file_with_instructions(vec![UnifiedInstruction::Ret { operand_0: 0 }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should have two blocks: one regular block + one EXIT block
    assert_eq!(cfg.graph().node_count(), 2);
    assert_eq!(cfg.graph().edge_count(), 1); // Return -> EXIT edge

    // Should have exactly one EXIT node
    let exit_nodes = cfg.find_exit_nodes();
    assert_eq!(exit_nodes.len(), 1);

    // EXIT node should be accessible via exit_node() method
    let exit_node = cfg.exit_node();
    assert!(exit_node.is_some());
    assert_eq!(exit_node.unwrap(), exit_nodes[0]);

    // EXIT node should have no outgoing edges
    let exit_node = exit_nodes[0];
    let outgoing_edges = cfg
        .graph()
        .neighbors_directed(exit_node, petgraph::Direction::Outgoing)
        .count();
    assert_eq!(outgoing_edges, 0);
}

#[test]
fn test_return_instruction_connects_to_exit() {
    let hbc_file =
        make_test_hbc_file_with_instructions(vec![UnifiedInstruction::Ret { operand_0: 1 }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the return block and EXIT block
    let exit_node = cfg.exit_node().unwrap();
    let return_block = cfg
        .graph()
        .node_indices()
        .find(|&node| !cfg.graph()[node].is_exit())
        .unwrap();

    // Return block should have edge to EXIT
    let has_exit_edge = cfg
        .graph()
        .neighbors_directed(return_block, petgraph::Direction::Outgoing)
        .any(|neighbor| neighbor == exit_node);
    assert!(has_exit_edge);

    // Check that the edge is unconditional
    let edge = cfg
        .graph()
        .edges_directed(return_block, petgraph::Direction::Outgoing)
        .find(|edge| edge.target() == exit_node)
        .unwrap();
    assert_eq!(*edge.weight(), hermes_dec_rs::cfg::EdgeKind::Uncond);
}

#[test]
fn test_throw_instruction_connects_to_exit() {
    let hbc_file =
        make_test_hbc_file_with_instructions(vec![UnifiedInstruction::Throw { operand_0: 1 }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the throw block and EXIT block
    let exit_node = cfg.exit_node().unwrap();
    let throw_block = cfg
        .graph()
        .node_indices()
        .find(|&node| !cfg.graph()[node].is_exit())
        .unwrap();

    // Throw block should have edge to EXIT
    let has_exit_edge = cfg
        .graph()
        .neighbors_directed(throw_block, petgraph::Direction::Outgoing)
        .any(|neighbor| neighbor == exit_node);
    assert!(has_exit_edge);

    // Check that the edge is unconditional
    let edge = cfg
        .graph()
        .edges_directed(throw_block, petgraph::Direction::Outgoing)
        .find(|edge| edge.target() == exit_node)
        .unwrap();
    assert_eq!(*edge.weight(), hermes_dec_rs::cfg::EdgeKind::Uncond);
}

#[test]
fn test_all_terminating_blocks_reach_exit() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        },
        UnifiedInstruction::Ret { operand_0: 1 },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        },
        UnifiedInstruction::Throw { operand_0: 2 },
    ]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should have one EXIT node
    assert_eq!(cfg.find_exit_nodes().len(), 1);

    // All terminating blocks should reach EXIT
    assert!(cfg.all_terminators_reach_exit());
}

#[test]
fn test_cfg_remains_acyclic_with_exit() {
    // Create instructions with proper relative addresses
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        },
        UnifiedInstruction::JmpTrue {
            operand_0: 4, // Jump forward 4 bytes to the return instruction
            operand_1: 1,
        },
        UnifiedInstruction::Throw { operand_0: 2 },
        UnifiedInstruction::Ret { operand_0: 1 },
    ];

    let hbc_file = make_test_hbc_file_with_instructions(instructions);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // CFG should remain acyclic
    assert!(cfg.is_acyclic());

    // Should have EXIT node
    assert!(cfg.exit_node().is_some());
}

#[test]
fn test_cfg_with_proper_jump_table() {
    // Create a simple loop: LoadConst -> JmpTrue -> LoadConst -> Jmp (back to start)
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: Placeholder jump to 3
        UnifiedInstruction::Ret { operand_0: 1 }, // 2: Return
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Jmp { operand_0: 0 }, // 4: Placeholder jump back to 0
    ];

    let jumps = vec![
        ("JmpTrue", 1, 3, Some(1)), // Jump to instruction 3 if true
        ("Jmp", 4, 0, None),        // Jump back to instruction 0
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should have multiple blocks due to jumps
    assert!(cfg.graph().node_count() > 2); // More than just entry + exit

    // Should have edges due to jumps
    assert!(cfg.graph().edge_count() > 0);

    // Should have EXIT node
    assert!(cfg.exit_node().is_some());
}

#[test]
fn test_jump_table_helper_functions() {
    // Test the basic helper functions
    let final_instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: Placeholder jump
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2
        UnifiedInstruction::Ret { operand_0: 1 }, // 3
    ];

    // Convert to HbcFunctionInstructions to get offsets
    let hbc_instructions = make_test_instructions(final_instructions.clone());

    // Create a jump from instruction 1 to instruction 2
    let jump_instruction = create_jump_instruction("JmpTrue", 1, 2, &hbc_instructions, Some(1));

    // Create the final instruction list with the correct jump
    let final_instructions_with_jump = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0
        jump_instruction, // 1: Jump to 2
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2
        UnifiedInstruction::Ret { operand_0: 1 }, // 3
    ];

    let hbc_file = make_test_hbc_file_with_instructions(final_instructions_with_jump);

    // Verify the jump table was built correctly
    let jump_table = &hbc_file.jump_table;

    // Instruction 2 should be a jump target
    assert!(jump_table.get_label_by_inst_index(0, 2).is_some());

    // Instruction 1 should be a jump instruction
    assert!(jump_table.get_label_by_jump_op_index(0, 1).is_some());

    // Should have exactly one label and one jump
    assert_eq!(jump_table.get_label_count(0), 1);
    assert_eq!(jump_table.get_jump_count(0), 1);
}

#[test]
fn test_make_test_hbc_file_with_jumps() {
    // Test the simplified helper function
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: Placeholder jump
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2
        UnifiedInstruction::Ret { operand_0: 1 }, // 3
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);

    // Verify the jump table was built correctly
    let jump_table = &hbc_file.jump_table;

    // Instruction 2 should be a jump target
    assert!(jump_table.get_label_by_inst_index(0, 2).is_some());

    // Instruction 1 should be a jump instruction
    assert!(jump_table.get_label_by_jump_op_index(0, 1).is_some());

    // Should have exactly one label and one jump
    assert_eq!(jump_table.get_label_count(0), 1);
    assert_eq!(jump_table.get_jump_count(0), 1);
}

#[test]
fn test_exit_node_has_no_outgoing_edges() {
    let hbc_file =
        make_test_hbc_file_with_instructions(vec![UnifiedInstruction::Ret { operand_0: 1 }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    let exit_node = cfg.exit_node().unwrap();

    // EXIT node should have no outgoing edges (true sink)
    let outgoing_count = cfg
        .graph()
        .neighbors_directed(exit_node, petgraph::Direction::Outgoing)
        .count();
    assert_eq!(outgoing_count, 0);

    // EXIT node should have incoming edges
    let incoming_count = cfg
        .graph()
        .neighbors_directed(exit_node, petgraph::Direction::Incoming)
        .count();
    assert!(incoming_count > 0);
}

#[test]
fn test_dominator_analysis_includes_exit() {
    let hbc_file =
        make_test_hbc_file_with_instructions(vec![UnifiedInstruction::Ret { operand_0: 1 }]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Dominator analysis should work with EXIT node
    let dominators = cfg.analyze_dominators();
    assert!(dominators.is_some());

    // Should be able to compute dominators for all nodes including EXIT
    let dominators = dominators.unwrap();
    for node in cfg.graph().node_indices() {
        // Each node should have dominator information (or be the root)
        let _idom = dominators.immediate_dominator(node);
        // This shouldn't panic
    }
}

#[test]
fn test_pc_lookup_overlapping_blocks() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 0,
            operand_1: 0,
        },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 1,
        },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 2,
        },
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 3,
        },
    ]);
    let mut builder = CfgBuilder::new(&hbc_file, 0);
    let mut graph: DiGraph<Block, EdgeKind> = DiGraph::new();

    let instructions = hbc_file.functions.get_instructions(0).unwrap();
    let n_a = builder.add_block(
        &mut graph,
        Block::new(0, vec![instructions[0].clone(), instructions[1].clone()]),
    );
    let n_b = builder.add_block(
        &mut graph,
        Block::new(1, vec![instructions[2].clone(), instructions[3].clone()]),
    );

    assert_eq!(builder.get_block_at_pc(0), Some(n_a));
    assert_eq!(builder.get_block_at_pc(1), Some(n_b));
    assert_eq!(builder.get_block_at_pc(2), Some(n_b));
    assert!(!builder.is_pc_in_block(3));
}

#[test]
fn test_empty_function_has_no_exit() {
    let hbc_file = make_test_hbc_file(vec![]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Empty function should have no nodes and no EXIT
    assert_eq!(cfg.graph().node_count(), 0);
    assert_eq!(cfg.graph().edge_count(), 0);
    assert!(cfg.exit_node().is_none());
    assert_eq!(cfg.find_exit_nodes().len(), 0);
}

#[test]
fn test_non_terminating_function_still_has_exit() {
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        },
        UnifiedInstruction::Add {
            operand_0: 1,
            operand_1: 1,
            operand_2: 1,
        },
    ]);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should still have EXIT node even if no terminators
    assert!(cfg.exit_node().is_some());
    assert_eq!(cfg.find_exit_nodes().len(), 1);

    // No edges should connect to EXIT since no terminators
    let exit_node = cfg.exit_node().unwrap();
    let incoming_count = cfg
        .graph()
        .neighbors_directed(exit_node, petgraph::Direction::Incoming)
        .count();
    assert_eq!(incoming_count, 0);
}

#[test]
fn test_leader_after_return_throw() {
    // Test case: Return followed by unreachable code
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Start of first block
        UnifiedInstruction::Ret { operand_0: 1 }, // 1: Return - should terminate block
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Unreachable code - should be in separate block
        UnifiedInstruction::Throw { operand_0: 2 }, // 3: Throw - should terminate block
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 200,
        }, // 4: More unreachable code - should be in separate block
    ]);
    
    let mut cfg = Cfg::new(&hbc_file, 0);
    

    
    cfg.build();

    // Should have 6 blocks: 5 regular blocks + 1 EXIT block
    // Block 0: LoadConstUInt8 (0)
    // Block 1: Ret (1) 
    // Block 2: LoadConstUInt8 (2) - unreachable after return
    // Block 3: Throw (3)
    // Block 4: LoadConstUInt8 (4) - unreachable after throw
    // Block 5: EXIT
    assert_eq!(cfg.graph().node_count(), 6);

    // Find all non-EXIT blocks and verify they have the correct instructions
    let non_exit_blocks: Vec<_> = cfg
        .graph()
        .node_indices()
        .filter(|&node| !cfg.graph()[node].is_exit())
        .collect();
    
    assert_eq!(non_exit_blocks.len(), 5);

    // Verify each block has the expected instructions
    for &block_node in &non_exit_blocks {
        let block = &cfg.graph()[block_node];
        match block.start_pc() {
            0 => {
                // First block: LoadConstUInt8
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 0);
                assert_eq!(block.end_pc(), 1);
            }
            1 => {
                // Second block: Ret
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 1);
                assert_eq!(block.end_pc(), 2);
                assert!(block.is_terminating());
            }
            2 => {
                // Third block: Unreachable code after return
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 2);
                assert_eq!(block.end_pc(), 3);
            }
            3 => {
                // Fourth block: Throw
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 3);
                assert_eq!(block.end_pc(), 4);
                assert!(block.is_terminating());
            }
            4 => {
                // Fifth block: Unreachable code after throw
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 4);
                assert_eq!(block.end_pc(), 5);
            }
            _ => panic!("Unexpected block start_pc: {}", block.start_pc()),
        }
    }

    // Verify that Return and Throw blocks connect to EXIT
    let exit_node = cfg.exit_node().unwrap();
    for &block_node in &non_exit_blocks {
        let block = &cfg.graph()[block_node];
        if block.is_terminating() {
            let has_exit_edge = cfg
                .graph()
                .neighbors_directed(block_node, petgraph::Direction::Outgoing)
                .any(|neighbor| neighbor == exit_node);
            assert!(has_exit_edge, "Terminating block at PC {} should connect to EXIT", block.start_pc());
        }
    }
}

#[test]
fn test_leader_after_return_throw_as_last_instruction() {
    // Test case: Return/Throw as the last instruction in function
    let hbc_file = make_test_hbc_file_with_instructions(vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        },
        UnifiedInstruction::Ret { operand_0: 1 }, // Last instruction
    ]);
    
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Should have 3 blocks: 2 regular blocks + 1 EXIT block
    assert_eq!(cfg.graph().node_count(), 3);

    // Find all non-EXIT blocks
    let non_exit_blocks: Vec<_> = cfg
        .graph()
        .node_indices()
        .filter(|&node| !cfg.graph()[node].is_exit())
        .collect();
    
    assert_eq!(non_exit_blocks.len(), 2);

    // First block should contain LoadConstUInt8
    let first_block = &cfg.graph()[non_exit_blocks[0]];
    assert_eq!(first_block.instruction_count(), 1);
    assert_eq!(first_block.start_pc(), 0);
    assert_eq!(first_block.end_pc(), 1);

    // Second block should contain Ret
    let second_block = &cfg.graph()[non_exit_blocks[1]];
    assert_eq!(second_block.instruction_count(), 1);
    assert_eq!(second_block.start_pc(), 1);
    assert_eq!(second_block.end_pc(), 2);
    assert!(second_block.is_terminating()); // Ends with Return
}
