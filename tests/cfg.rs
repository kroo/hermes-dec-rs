use hermes_dec_rs::cfg::analysis::LoopType;
use hermes_dec_rs::cfg::builder::CfgBuilder;
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
use std::fs;
use std::path::Path;
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
        switch_tables: hermes_dec_rs::hbc::tables::switch_table::SwitchTableCollection::new(),
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
    if let Err(e) = hbc_file
        .jump_table
        .build_for_function(0, &instructions, &[])
    {
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
        "JEqual" => UnifiedInstruction::JEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JNotEqual" => UnifiedInstruction::JNotEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JStrictEqual" => UnifiedInstruction::JStrictEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JStrictNotEqual" => UnifiedInstruction::JStrictNotEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JLess" => UnifiedInstruction::JLess {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JGreater" => UnifiedInstruction::JGreater {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JLessEqual" => UnifiedInstruction::JLessEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
        },
        "JGreaterEqual" => UnifiedInstruction::JGreaterEqual {
            operand_0: relative_offset as i8,
            operand_1: additional_operand.unwrap_or(0),
            operand_2: (additional_operand.unwrap_or(0) + 1) % 255,
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

    // Should have 4 blocks: 3 regular blocks + 1 EXIT block
    // Block 0: LoadConstUInt8 + Ret (0-1) - Return included in preceding block
    // Block 1: LoadConstUInt8 + Throw (2-3) - Throw included in preceding block
    // Block 2: LoadConstUInt8 (4) - Unreachable code after throw
    // Block 3: EXIT
    assert_eq!(cfg.graph().node_count(), 4);

    // Find all non-EXIT blocks and verify they have the correct instructions
    let non_exit_blocks: Vec<_> = cfg
        .graph()
        .node_indices()
        .filter(|&node| !cfg.graph()[node].is_exit())
        .collect();

    assert_eq!(non_exit_blocks.len(), 3);

    // Verify each block has the expected instructions
    for &block_node in &non_exit_blocks {
        let block = &cfg.graph()[block_node];
        match block.start_pc() {
            0 => {
                // First block: LoadConstUInt8 + Ret (Return included in preceding block)
                assert_eq!(block.instruction_count(), 2);
                assert_eq!(block.start_pc(), 0);
                assert_eq!(block.end_pc(), 2);
                assert!(block.is_terminating());
            }
            2 => {
                // Second block: LoadConstUInt8 + Throw (Throw included in preceding block)
                assert_eq!(block.instruction_count(), 2);
                assert_eq!(block.start_pc(), 2);
                assert_eq!(block.end_pc(), 4);
                assert!(block.is_terminating());
            }
            4 => {
                // Third block: Unreachable code after throw
                assert_eq!(block.instruction_count(), 1);
                assert_eq!(block.start_pc(), 4);
                assert_eq!(block.end_pc(), 5);
                assert!(!block.is_terminating());
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
            assert!(
                has_exit_edge,
                "Terminating block at PC {} should connect to EXIT",
                block.start_pc()
            );
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

    // Should have 2 blocks: 1 regular block + 1 EXIT block
    // Block 0: LoadConstUInt8 + Ret (Return included in preceding block)
    // Block 1: EXIT
    assert_eq!(cfg.graph().node_count(), 2);

    // Find all non-EXIT blocks
    let non_exit_blocks: Vec<_> = cfg
        .graph()
        .node_indices()
        .filter(|&node| !cfg.graph()[node].is_exit())
        .collect();

    assert_eq!(non_exit_blocks.len(), 1);

    // First block should contain LoadConstUInt8 + Ret
    let first_block = &cfg.graph()[non_exit_blocks[0]];
    assert_eq!(first_block.instruction_count(), 2);
    assert_eq!(first_block.start_pc(), 0);
    assert_eq!(first_block.end_pc(), 2);
    assert!(first_block.is_terminating()); // Ends with Return
}

#[test]
fn test_cfg_with_switch_table() {
    use hermes_dec_rs::hbc::tables::switch_table::SwitchTable;

    // Create a simple switch table for testing
    let mut switch_table = SwitchTable::new(0, 2, 100, 150, 1000, 1, 0);
    switch_table.add_case(0, 10);
    switch_table.add_case(1, 20);
    switch_table.add_case(2, 30);

    // Set instruction indices for the targets
    switch_table.default_instruction_index = Some(5);
    for (i, case) in switch_table.cases.iter_mut().enumerate() {
        case.target_instruction_index = Some(10 + i as u32);
    }

    // Create test instructions with a switch instruction
    let instructions = vec![
        HbcFunctionInstruction {
            offset: 0,
            function_index: 0,
            instruction_index: 0,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 0,
                operand_1: 0,
            },
        },
        HbcFunctionInstruction {
            offset: 4,
            function_index: 0,
            instruction_index: 1,
            instruction: UnifiedInstruction::SwitchImm {
                operand_0: 0,
                operand_1: 0,
                operand_2: 0,
                operand_3: 0,
                operand_4: 0,
            },
        },
        HbcFunctionInstruction {
            offset: 8,
            function_index: 0,
            instruction_index: 2,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 1,
                operand_1: 1,
            },
        },
        HbcFunctionInstruction {
            offset: 12,
            function_index: 0,
            instruction_index: 3,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 2,
                operand_1: 2,
            },
        },
        HbcFunctionInstruction {
            offset: 16,
            function_index: 0,
            instruction_index: 4,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 3,
                operand_1: 3,
            },
        },
        HbcFunctionInstruction {
            offset: 20,
            function_index: 0,
            instruction_index: 5,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 4,
                operand_1: 4,
            },
        }, // Default case
        HbcFunctionInstruction {
            offset: 24,
            function_index: 0,
            instruction_index: 6,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 5,
                operand_1: 5,
            },
        },
        HbcFunctionInstruction {
            offset: 28,
            function_index: 0,
            instruction_index: 7,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 6,
                operand_1: 6,
            },
        },
        HbcFunctionInstruction {
            offset: 32,
            function_index: 0,
            instruction_index: 8,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 7,
                operand_1: 7,
            },
        },
        HbcFunctionInstruction {
            offset: 36,
            function_index: 0,
            instruction_index: 9,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 8,
                operand_1: 8,
            },
        },
        HbcFunctionInstruction {
            offset: 40,
            function_index: 0,
            instruction_index: 10,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 9,
                operand_1: 9,
            },
        }, // Case 0
        HbcFunctionInstruction {
            offset: 44,
            function_index: 0,
            instruction_index: 11,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 10,
                operand_1: 10,
            },
        }, // Case 1
        HbcFunctionInstruction {
            offset: 48,
            function_index: 0,
            instruction_index: 12,
            instruction: UnifiedInstruction::LoadConstString {
                operand_0: 11,
                operand_1: 11,
            },
        }, // Case 2
        HbcFunctionInstruction {
            offset: 52,
            function_index: 0,
            instruction_index: 13,
            instruction: UnifiedInstruction::Ret { operand_0: 0 },
        },
    ];

    // Create HBC file with switch table
    let mut hbc_file = make_test_hbc_file(instructions);
    hbc_file.switch_tables.add_switch_table(switch_table);

    // Build CFG
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    let graph = cfg.graph();

    // Verify that leaders were created for switch targets
    let cfg_builder = CfgBuilder::new(&hbc_file, 0);
    let leaders = cfg_builder.find_leaders(
        &hbc_file.functions.get_instructions(0).unwrap(),
        &hbc_file.jump_table,
    );

    // Should have leaders at: 0 (start), 2 (fallthrough), 5 (default), 10 (case 0), 11 (case 1), 12 (case 2)
    assert!(leaders.contains(&0));
    assert!(leaders.contains(&2)); // Fallthrough after switch
    assert!(leaders.contains(&5)); // Default case
    assert!(leaders.contains(&10)); // Case 0
    assert!(leaders.contains(&11)); // Case 1
    assert!(leaders.contains(&12)); // Case 2

    // Verify that edges were created for switch cases
    let mut switch_edges = 0;
    let mut default_edges = 0;

    for edge in graph.edge_indices() {
        let (source, _target) = graph.edge_endpoints(edge).unwrap();
        let edge_weight = graph.edge_weight(edge).unwrap();

        // Check if this edge comes from the switch instruction block
        let source_block = &graph[source];
        if source_block.start_pc() == 0 && source_block.end_pc() == 2 {
            match edge_weight {
                EdgeKind::Switch(_) => switch_edges += 1,
                EdgeKind::Default => default_edges += 1,
                _ => {}
            }
        }
    }

    // Should have 3 switch edges (one for each case) and 1 default edge
    assert_eq!(switch_edges, 3);
    assert_eq!(default_edges, 1);
}

/// Test CFG analysis for all .hbc files in the data directory
#[test]
fn test_cfg_integration_with_hbc_files() {
    let data_dir = Path::new("data");
    let mut hbc_files = Vec::new();

    // Find all .hbc files in the data directory
    if let Ok(entries) = fs::read_dir(data_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if let Some(extension) = path.extension() {
                    if extension == "hbc" {
                        // Skip hermes_dec_sample as requested
                        if !path.to_string_lossy().contains("hermes_dec_sample") {
                            hbc_files.push(path);
                        }
                    }
                }
            }
        }
    }

    hbc_files.sort(); // Sort for consistent test ordering

    for hbc_file_path in hbc_files {
        let test_name = hbc_file_path.file_stem().unwrap().to_string_lossy();
        println!("Testing CFG analysis for: {}", test_name);

        // Load and parse the HBC file
        let data = fs::read(&hbc_file_path)
            .unwrap_or_else(|_| panic!("Failed to read HBC file: {}", hbc_file_path.display()));

        let hbc_file = HbcFile::parse(&data)
            .unwrap_or_else(|e| panic!("Failed to parse HBC file {}: {}", test_name, e));

        // Collect DOT output for all functions
        let mut all_dot_output = String::new();
        all_dot_output.push_str(&format!("// CFG analysis for {}\n", test_name));
        all_dot_output.push_str(&format!(
            "// Generated from {}\n\n",
            hbc_file_path.display()
        ));
        all_dot_output.push_str("digraph {\n");
        all_dot_output.push_str("  rankdir=TB;\n");
        all_dot_output.push_str("  node [shape=box, fontname=\"monospace\"];\n\n");

        // Test CFG analysis for each function
        for function_index in 0..hbc_file.functions.count() {
            println!("  Analyzing function {}", function_index);

            // Create CFG for this function
            let mut cfg = Cfg::new(&hbc_file, function_index);
            cfg.build();

            // Generate DOT output for this function as a subgraph
            let dot_output = cfg.to_dot_subgraph(&hbc_file, function_index);

            // Add subgraph to combined output
            all_dot_output.push_str(&dot_output);
            all_dot_output.push_str("\n");

            // Basic CFG validation
            validate_cfg_structure(&cfg, &test_name, function_index);
        }

        // Close the digraph
        all_dot_output.push_str("}\n");

        // Create expected DOT file path for the entire file
        let expected_dot_path = data_dir.join(format!("{}.dot", test_name));

        // Check if expected file exists
        if expected_dot_path.exists() {
            // Read expected content
            let expected_content = fs::read_to_string(&expected_dot_path).unwrap_or_else(|_| {
                panic!(
                    "Failed to read expected DOT file: {}",
                    expected_dot_path.display()
                )
            });

            // Compare actual vs expected
            if all_dot_output != expected_content {
                // Generate a detailed diff for debugging
                let diff = generate_dot_diff(&expected_content, &all_dot_output);
                panic!("CFG DOT output mismatch for {}:\n\n{}", test_name, diff);
            }

            println!("    ✓ DOT output matches expected");
        } else {
            // Create expected file for first run
            fs::write(&expected_dot_path, &all_dot_output).unwrap_or_else(|_| {
                panic!(
                    "Failed to write expected DOT file: {}",
                    expected_dot_path.display()
                )
            });
            println!(
                "    ✓ Created expected DOT file: {}",
                expected_dot_path.display()
            );
        }

        println!("✓ {} CFG analysis completed", test_name);
    }
}

/// Validate basic CFG structure properties
fn validate_cfg_structure(cfg: &Cfg, test_name: &str, function_index: u32) {
    let graph = cfg.graph();

    // CFG should not be empty (unless function has no instructions)
    if graph.node_count() == 0 {
        println!("    Warning: Function {} has empty CFG", function_index);
        return;
    }

    // Should have exactly one EXIT node
    let exit_nodes = cfg.find_exit_nodes();
    assert_eq!(
        exit_nodes.len(),
        1,
        "Function {} in {} should have exactly one EXIT node, found {}",
        function_index,
        test_name,
        exit_nodes.len()
    );

    // EXIT node should have no outgoing edges
    let exit_node = exit_nodes[0];
    let outgoing_edges = graph
        .neighbors_directed(exit_node, petgraph::Direction::Outgoing)
        .count();
    assert_eq!(
        outgoing_edges, 0,
        "EXIT node in function {} of {} should have no outgoing edges, found {}",
        function_index, test_name, outgoing_edges
    );

    // All terminating blocks should reach EXIT
    assert!(
        cfg.all_terminators_reach_exit(),
        "All terminating blocks in function {} of {} should reach EXIT",
        function_index,
        test_name
    );

    // CFG should remain acyclic (but loops are allowed)
    // Note: Real code often contains loops, so we'll be more lenient here
    // and only warn about potential issues rather than failing
    if !cfg.is_acyclic() {
        println!(
            "    Warning: Function {} in {} contains cycles (loops)",
            function_index, test_name
        );
        // For now, we'll allow cycles as they can be legitimate loops
        // In the future, we could add more sophisticated cycle detection
    }

    // Basic block count validation
    let non_exit_blocks = graph
        .node_indices()
        .filter(|&node| !graph[node].is_exit())
        .count();

    assert!(
        non_exit_blocks > 0,
        "Function {} in {} should have at least one non-EXIT block",
        function_index,
        test_name
    );

    println!("    ✓ Function {} CFG structure validated", function_index);
}

/// Generate a simple diff between expected and actual DOT content
fn generate_dot_diff(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();

    let mut diff = String::new();
    diff.push_str("Expected vs Actual DOT output:\n");
    diff.push_str("================================\n\n");

    let max_lines = expected_lines.len().max(actual_lines.len());

    for i in 0..max_lines {
        let expected_line = expected_lines.get(i).unwrap_or(&"<missing>");
        let actual_line = actual_lines.get(i).unwrap_or(&"<missing>");

        if expected_line != actual_line {
            diff.push_str(&format!("Line {}:\n", i + 1));
            diff.push_str(&format!("  Expected: {}\n", expected_line));
            diff.push_str(&format!("  Actual:   {}\n", actual_line));
            diff.push_str("\n");
        }
    }

    diff
}

/// Test CFG analysis with specific focus on complex control flow
#[test]
fn test_cfg_complex_control_flow() {
    // Test files that are known to have complex control flow
    let test_files = vec!["data/flow_control.hbc", "data/regex_test.hbc"];

    for hbc_file_path in test_files {
        let path = Path::new(hbc_file_path);
        if !path.exists() {
            continue;
        }

        let test_name = path.file_stem().unwrap().to_string_lossy();
        println!("Testing complex control flow for: {}", test_name);

        // Load and parse the HBC file
        let data = fs::read(path)
            .unwrap_or_else(|_| panic!("Failed to read HBC file: {}", path.display()));

        let hbc_file = HbcFile::parse(&data)
            .unwrap_or_else(|e| panic!("Failed to parse HBC file {}: {}", test_name, e));

        // Test CFG analysis for each function
        for function_index in 0..hbc_file.functions.count() {
            println!(
                "  Analyzing function {} for complex control flow",
                function_index
            );

            let mut cfg = Cfg::new(&hbc_file, function_index);
            cfg.build();

            let graph = cfg.graph();

            // For complex control flow, we expect multiple blocks and edges
            let non_exit_blocks = graph
                .node_indices()
                .filter(|&node| !graph[node].is_exit())
                .count();

            let total_edges = graph.edge_count();

            println!(
                "    Function {}: {} blocks, {} edges",
                function_index, non_exit_blocks, total_edges
            );

            // Complex functions should have multiple blocks
            if non_exit_blocks > 1 {
                // Test dominator analysis
                if let Some(_dominators) = cfg.analyze_dominators() {
                    println!("    ✓ Dominator analysis successful");

                    // Test natural loop detection
                    let loops = cfg.find_natural_loops();
                    if !loops.is_empty() {
                        println!("    ✓ Found {} natural loops", loops.len());
                    }

                    // Test if/else join detection
                    let if_else_joins = cfg.find_if_else_joins();
                    if !if_else_joins.is_empty() {
                        println!("    ✓ Found {} if/else join blocks", if_else_joins.len());
                    }

                    // Test switch dispatch detection
                    let switch_dispatches = cfg.find_switch_dispatches();
                    if !switch_dispatches.is_empty() {
                        println!(
                            "    ✓ Found {} switch dispatch patterns",
                            switch_dispatches.len()
                        );
                    }
                }
            }

            // Validate CFG structure
            validate_cfg_structure(&cfg, &test_name, function_index);
        }

        println!("✓ {} complex control flow analysis completed", test_name);
    }
}

/// Test precise conditional edge kinds for JmpTrue instruction
#[test]
fn test_precise_conditional_edges_jmp_true() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpTrue {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
        }, // 1: Jump if true
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JmpTrue instruction
    let jmp_true_block = cfg.builder().get_block_at_pc(1).unwrap();
    let edges: Vec<_> = cfg.graph().edges(jmp_true_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JmpTrue should create a True edge");
    assert!(found_false, "JmpTrue should create a False edge");
}

/// Test precise conditional edge kinds for JmpFalse instruction
#[test]
fn test_precise_conditional_edges_jmp_false() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpFalse {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
        }, // 1: Jump if false
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpFalse", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JmpFalse instruction
    let jmp_false_block = cfg.builder().get_block_at_pc(1).unwrap();
    let edges: Vec<_> = cfg.graph().edges(jmp_false_block).collect();

    // Should have exactly 2 edges: True (fallthrough) and False (jump)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(
        found_true,
        "JmpFalse should create a True edge (fallthrough)"
    );
    assert!(found_false, "JmpFalse should create a False edge (jump)");
}

/// Test precise conditional edge kinds for JEqual instruction
#[test]
fn test_precise_conditional_edges_j_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load first value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 42,
        }, // 1: Load second value
        UnifiedInstruction::JEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JEqual instruction
    let j_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JEqual should create a True edge");
    assert!(found_false, "JEqual should create a False edge");
}

/// Test precise conditional edge kinds for JNotEqual instruction
#[test]
fn test_precise_conditional_edges_j_not_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load first value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 1: Load different value
        UnifiedInstruction::JNotEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if not equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 200,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JNotEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JNotEqual instruction
    let j_not_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_not_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JNotEqual should create a True edge");
    assert!(found_false, "JNotEqual should create a False edge");
}

/// Test precise conditional edge kinds for JStrictEqual instruction
#[test]
fn test_precise_conditional_edges_j_strict_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load first value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 42,
        }, // 1: Load second value
        UnifiedInstruction::JStrictEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if strict equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JStrictEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JStrictEqual instruction
    let j_strict_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_strict_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JStrictEqual should create a True edge");
    assert!(found_false, "JStrictEqual should create a False edge");
}

/// Test precise conditional edge kinds for JStrictNotEqual instruction
#[test]
fn test_precise_conditional_edges_j_strict_not_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load first value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 1: Load different value
        UnifiedInstruction::JStrictNotEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if strict not equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 200,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JStrictNotEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JStrictNotEqual instruction
    let j_strict_not_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_strict_not_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JStrictNotEqual should create a True edge");
    assert!(found_false, "JStrictNotEqual should create a False edge");
}

/// Test precise conditional edge kinds for JLess instruction
#[test]
fn test_precise_conditional_edges_j_less() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 10,
        }, // 0: Load smaller value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 20,
        }, // 1: Load larger value
        UnifiedInstruction::JLess {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if less
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JLess", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JLess instruction
    let j_less_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_less_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JLess should create a True edge");
    assert!(found_false, "JLess should create a False edge");
}

/// Test precise conditional edge kinds for JGreater instruction
#[test]
fn test_precise_conditional_edges_j_greater() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 20,
        }, // 0: Load larger value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 10,
        }, // 1: Load smaller value
        UnifiedInstruction::JGreater {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if greater
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JGreater", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JGreater instruction
    let j_greater_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_greater_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JGreater should create a True edge");
    assert!(found_false, "JGreater should create a False edge");
}

/// Test precise conditional edge kinds for JLessEqual instruction
#[test]
fn test_precise_conditional_edges_j_less_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 10,
        }, // 0: Load smaller value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 20,
        }, // 1: Load larger value
        UnifiedInstruction::JLessEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if less or equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JLessEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JLessEqual instruction
    let j_less_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_less_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JLessEqual should create a True edge");
    assert!(found_false, "JLessEqual should create a False edge");
}

/// Test precise conditional edge kinds for JGreaterEqual instruction
#[test]
fn test_precise_conditional_edges_j_greater_equal() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 20,
        }, // 0: Load larger value
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 10,
        }, // 1: Load smaller value
        UnifiedInstruction::JGreaterEqual {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
            operand_2: 2, // Register 2
        }, // 2: Jump if greater or equal
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 100,
        }, // 3: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JGreaterEqual", 2, 3, None), // Jump from instruction 2 to instruction 3
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JGreaterEqual instruction
    let j_greater_equal_block = cfg.builder().get_block_at_pc(2).unwrap();
    let edges: Vec<_> = cfg.graph().edges(j_greater_equal_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JGreaterEqual should create a True edge");
    assert!(found_false, "JGreaterEqual should create a False edge");
}

/// Test precise conditional edge kinds for Long variants
#[test]
fn test_precise_conditional_edges_long_variants() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpTrueLong {
            operand_0: 0, // Placeholder jump offset (32-bit)
            operand_1: 1, // Register 1
        }, // 1: Jump if true (long)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpTrueLong", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JmpTrueLong instruction
    let jmp_true_long_block = cfg.builder().get_block_at_pc(1).unwrap();
    let edges: Vec<_> = cfg.graph().edges(jmp_true_long_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JmpTrueLong should create a True edge");
    assert!(found_false, "JmpTrueLong should create a False edge");
}

/// Test precise conditional edge kinds for JmpUndefined instructions
#[test]
fn test_precise_conditional_edges_jmp_undefined() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpUndefined {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
        }, // 1: Jump if undefined
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpUndefined", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JmpUndefined instruction
    let jmp_undefined_block = cfg.builder().get_block_at_pc(1).unwrap();
    let edges: Vec<_> = cfg.graph().edges(jmp_undefined_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JmpUndefined should create a True edge");
    assert!(found_false, "JmpUndefined should create a False edge");
}

/// Test precise conditional edge kinds for JmpUndefinedLong instructions
#[test]
fn test_precise_conditional_edges_jmp_undefined_long() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpUndefinedLong {
            operand_0: 0, // Placeholder jump offset (32-bit)
            operand_1: 1, // Register 1
        }, // 1: Jump if undefined (long)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpUndefinedLong", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Find the edge from the JmpUndefinedLong instruction
    let jmp_undefined_long_block = cfg.builder().get_block_at_pc(1).unwrap();
    let edges: Vec<_> = cfg.graph().edges(jmp_undefined_long_block).collect();

    // Should have exactly 2 edges: True (jump) and False (fallthrough)
    assert_eq!(edges.len(), 2);

    let mut found_true = false;
    let mut found_false = false;

    for edge in edges {
        match edge.weight() {
            EdgeKind::True => found_true = true,
            EdgeKind::False => found_false = true,
            _ => panic!("Expected True/False edges, got {:?}", edge.weight()),
        }
    }

    assert!(found_true, "JmpUndefinedLong should create a True edge");
    assert!(found_false, "JmpUndefinedLong should create a False edge");
}

/// Test that edge labels are correctly generated in DOT output
#[test]
fn test_edge_labels_in_dot_output() {
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: Load value
        UnifiedInstruction::JmpTrue {
            operand_0: 0, // Placeholder jump offset
            operand_1: 1, // Register 1
        }, // 1: Jump if true
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: Load another value
        UnifiedInstruction::Ret { operand_0: 1 }, // 3: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from instruction 1 to instruction 2
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // Generate DOT output
    let dot_output = cfg.builder().to_dot(cfg.graph());

    // Verify that edge labels are present
    assert!(
        dot_output.contains("label="),
        "DOT output should contain edge labels"
    );
    assert!(
        dot_output.contains("True"),
        "DOT output should contain 'True' edge label"
    );
    assert!(
        dot_output.contains("False"),
        "DOT output should contain 'False' edge label"
    );

    // Verify the structure includes edge labels
    let lines: Vec<&str> = dot_output.lines().collect();
    let edge_lines: Vec<&str> = lines
        .iter()
        .filter(|line| line.contains("->") && line.contains("label="))
        .map(|&line| line)
        .collect();

    assert!(
        !edge_lines.is_empty(),
        "Should have at least one edge with a label"
    );

    // Verify specific edge label formats
    for line in edge_lines {
        assert!(
            line.contains("[label="),
            "Edge line should have label attribute"
        );
        assert!(line.contains("]"), "Edge line should have closing bracket");
    }
}

/// Test that loop body computation works correctly
#[test]
fn test_compute_loop_body() {
    // Create a simple loop: A -> B -> C -> B (loop)
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: A
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: B (conditional)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: C
        UnifiedInstruction::Jmp { operand_0: 0 }, // 3: Jump back to B
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from B to C
        ("Jmp", 3, 1, None),        // Jump from C back to B
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    // This should fail initially - we haven't implemented loop analysis yet
    let loop_analysis = cfg.analyze_loops();

    // Should find one loop
    assert_eq!(loop_analysis.loops.len(), 1, "Should find exactly one loop");

    let loop_info = &loop_analysis.loops[0];

    // Loop should contain nodes B and C
    assert!(loop_info
        .body_nodes
        .contains(&cfg.builder().get_block_at_pc(1).unwrap()));
    assert!(loop_info
        .body_nodes
        .contains(&cfg.builder().get_block_at_pc(2).unwrap()));

    // Loop header should be B (the conditional jump)
    assert_eq!(loop_info.header, cfg.builder().get_block_at_pc(1).unwrap());

    // Should have one back-edge from C to B
    assert_eq!(loop_info.back_edges.len(), 1);
    assert_eq!(
        loop_info.back_edges[0],
        (
            cfg.builder().get_block_at_pc(2).unwrap(),
            cfg.builder().get_block_at_pc(1).unwrap()
        )
    );
}

/// Test that loop type classification works correctly
#[test]
fn test_classify_loop_types() {
    // Create a while loop: A -> B -> C -> B (while condition)
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: A
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: B (while condition)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: C (loop body)
        UnifiedInstruction::Jmp { operand_0: 0 }, // 3: Jump back to B
        UnifiedInstruction::Ret { operand_0: 1 }, // 4: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from B to C
        ("Jmp", 3, 1, None),        // Jump from C back to B
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    let loop_analysis = cfg.analyze_loops();

    // Should find one loop
    assert_eq!(loop_analysis.loops.len(), 1);

    let loop_info = &loop_analysis.loops[0];

    // Should classify as While loop
    assert_eq!(
        loop_info.loop_type,
        LoopType::While,
        "Should classify as While loop"
    );
}

/// Test that loop exit detection works correctly
#[test]
fn test_loop_exit_detection() {
    // Create a loop with exit: A -> B -> C -> B, B -> D (exit when condition is false)
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: A
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: B (condition)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: C (body)
        UnifiedInstruction::Jmp { operand_0: 0 }, // 3: Jump back to B
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 200,
        }, // 4: D (exit)
        UnifiedInstruction::Ret { operand_0: 1 }, // 5: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from B to C (condition true)
        ("Jmp", 3, 1, None),        // Jump from C back to B
        ("JmpTrue", 1, 4, Some(0)), // Jump from B to D (condition false)
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    let loop_analysis = cfg.analyze_loops();

    // Should find one loop
    assert_eq!(loop_analysis.loops.len(), 1);

    let loop_info = &loop_analysis.loops[0];

    // Should have exit nodes
    assert!(!loop_info.exit_nodes.is_empty(), "Should have exit nodes");

    // Exit should be node D (the block after the loop)
    let exit_node = cfg.builder().get_block_at_pc(4).unwrap();
    assert!(
        loop_info.exit_nodes.contains(&exit_node),
        "Should have D as exit node"
    );
}

/// Test that multiple loops are handled correctly
#[test]
fn test_multiple_loops() {
    // Create nested loops: A -> B -> C -> B (outer), C -> D -> C (inner)
    let instructions = vec![
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 1,
            operand_1: 42,
        }, // 0: A
        UnifiedInstruction::JmpTrue {
            operand_0: 0,
            operand_1: 1,
        }, // 1: B (outer loop)
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 2,
            operand_1: 100,
        }, // 2: C (inner loop)
        UnifiedInstruction::Jmp { operand_0: 0 }, // 3: Jump back to B
        UnifiedInstruction::LoadConstUInt8 {
            operand_0: 3,
            operand_1: 200,
        }, // 4: D
        UnifiedInstruction::Jmp { operand_0: 0 }, // 5: Jump back to C
        UnifiedInstruction::Ret { operand_0: 1 }, // 6: Return
    ];

    let jumps = vec![
        ("JmpTrue", 1, 2, Some(1)), // Jump from B to C (outer loop)
        ("Jmp", 3, 1, None),        // Jump from C back to B (outer loop)
        ("JmpTrue", 2, 4, Some(1)), // Jump from C to D (inner loop)
        ("Jmp", 5, 2, None),        // Jump from D back to C (inner loop)
    ];

    let hbc_file = make_test_hbc_file_with_jumps(instructions, jumps);
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();

    let loop_analysis = cfg.analyze_loops();

    // Should find two loops
    assert_eq!(
        loop_analysis.loops.len(),
        2,
        "Should find exactly two loops"
    );

    // Check that nodes are properly mapped to loops
    let node_b = cfg.builder().get_block_at_pc(1).unwrap();
    let node_c = cfg.builder().get_block_at_pc(2).unwrap();

    let loops_containing_b = loop_analysis.get_loops_containing_node(node_b);
    let loops_containing_c = loop_analysis.get_loops_containing_node(node_c);

    assert_eq!(loops_containing_b.len(), 1, "Node B should be in one loop");
    assert_eq!(
        loops_containing_c.len(),
        2,
        "Node C should be in two loops (nested)"
    );
}
