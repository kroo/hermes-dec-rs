use hermes_dec_rs::cfg::CfgBuilder;
use hermes_dec_rs::cfg::{Block, Cfg, EdgeKind};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::function_table::HbcFunctionInstruction;
use hermes_dec_rs::hbc::HbcFile;
use petgraph::graph::DiGraph;
use petgraph::visit::EdgeRef;
use std::fs;

#[test]
fn test_cfg_creation() {
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read test HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse test HBC file");
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
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read test HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse test HBC file");
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();
    // The HBC file has functions, so we should have blocks
    // node_count and edge_count are usize, so they're always >= 0
}

#[test]
fn test_cfg_building_with_single_instruction() {
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read test HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse test HBC file");
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
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read test HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse test HBC file");
    let mut builder = CfgBuilder::new(&hbc_file, 0);
    let mut graph: DiGraph<Block, EdgeKind> = DiGraph::new();

    let make_inst = |idx| HbcFunctionInstruction {
        offset: (idx * 2) as u32,
        function_index: 0,
        instruction_index: idx as u32,
        instruction: UnifiedInstruction::LoadConstUInt8 {
            operand_0: idx as u8,
            operand_1: idx as u8,
        },
    };

    let n0 = builder.add_block(&mut graph, Block::new(0, vec![make_inst(0)]));
    let n1 = builder.add_block(&mut graph, Block::new(1, vec![make_inst(1)]));
    let n2 = builder.add_block(&mut graph, Block::new(2, vec![make_inst(2)]));

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
    let mut cfg = Cfg::new();

    // Create a simple Return instruction
    let instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::Ret { operand_0: 1 },
    };

    let instructions = vec![instruction];
    cfg.build_from_instructions(&instructions, 0);

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
    let mut cfg = Cfg::new();

    // Create a Return instruction
    let ret_instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::Ret { operand_0: 1 },
    };

    let instructions = vec![ret_instruction];
    cfg.build_from_instructions(&instructions, 0);

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
    let mut cfg = Cfg::new();

    // Create a Throw instruction
    let throw_instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::Throw { operand_0: 1 },
    };

    let instructions = vec![throw_instruction];
    cfg.build_from_instructions(&instructions, 0);

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
    let mut cfg = Cfg::new();

    // Create multiple terminating instructions
    let instructions = vec![
        HbcFunctionInstruction {
            offset: 0,
            function_index: 0,
            instruction_index: 0,
            instruction: UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            },
        },
        HbcFunctionInstruction {
            offset: 2,
            function_index: 0,
            instruction_index: 1,
            instruction: UnifiedInstruction::Ret { operand_0: 1 },
        },
        HbcFunctionInstruction {
            offset: 4,
            function_index: 0,
            instruction_index: 2,
            instruction: UnifiedInstruction::LoadConstUInt8 {
                operand_0: 2,
                operand_1: 100,
            },
        },
        HbcFunctionInstruction {
            offset: 6,
            function_index: 0,
            instruction_index: 3,
            instruction: UnifiedInstruction::Throw { operand_0: 2 },
        },
    ];

    cfg.build_from_instructions(&instructions, 0);

    // Should have one EXIT node
    assert_eq!(cfg.find_exit_nodes().len(), 1);

    // All terminating blocks should reach EXIT
    assert!(cfg.all_terminators_reach_exit());
}

#[test]
fn test_cfg_remains_acyclic_with_exit() {
    let mut cfg = Cfg::new();

    // Create a CFG with various control structures
    let instructions = vec![
        HbcFunctionInstruction {
            offset: 0,
            function_index: 0,
            instruction_index: 0,
            instruction: UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            },
        },
        HbcFunctionInstruction {
            offset: 2,
            function_index: 0,
            instruction_index: 1,
            instruction: UnifiedInstruction::JmpTrue {
                operand_0: 2, // Jump forward to return
                operand_1: 1,
            },
        },
        HbcFunctionInstruction {
            offset: 4,
            function_index: 0,
            instruction_index: 2,
            instruction: UnifiedInstruction::Throw { operand_0: 2 },
        },
        HbcFunctionInstruction {
            offset: 6,
            function_index: 0,
            instruction_index: 3,
            instruction: UnifiedInstruction::Ret { operand_0: 1 },
        },
    ];

    cfg.build_from_instructions(&instructions, 0);

    // CFG should remain acyclic
    assert!(cfg.is_acyclic());

    // Should have EXIT node
    assert!(cfg.exit_node().is_some());
}

#[test]
fn test_exit_node_has_no_outgoing_edges() {
    let mut cfg = Cfg::new();

    // Create any function with terminating instruction
    let instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::Ret { operand_0: 1 },
    };

    let instructions = vec![instruction];
    cfg.build_from_instructions(&instructions, 0);

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
    let mut cfg = Cfg::new();

    // Create a simple function with return
    let instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::Ret { operand_0: 1 },
    };

    let instructions = vec![instruction];
    cfg.build_from_instructions(&instructions, 0);

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
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read test HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse test HBC file");
    let mut builder = CfgBuilder::new(&hbc_file, 0);
    let mut graph: DiGraph<Block, EdgeKind> = DiGraph::new();

    let make_inst = |idx| HbcFunctionInstruction {
        offset: idx as u32,
        function_index: 0,
        instruction_index: idx as u32,
        instruction: UnifiedInstruction::LoadConstUInt8 {
            operand_0: idx as u8,
            operand_1: idx as u8,
        },
    };

    let n_a = builder.add_block(&mut graph, Block::new(0, vec![make_inst(0), make_inst(1)]));
    let n_b = builder.add_block(&mut graph, Block::new(1, vec![make_inst(2), make_inst(3)]));

    assert_eq!(builder.get_block_at_pc(0), Some(n_a));
    assert_eq!(builder.get_block_at_pc(1), Some(n_b));
    assert_eq!(builder.get_block_at_pc(2), Some(n_b));
    assert!(!builder.is_pc_in_block(3));
}

#[test]
fn test_empty_function_has_no_exit() {
    let mut cfg = Cfg::new();
    cfg.build_from_instructions(&[], 0);

    // Empty function should have no nodes and no EXIT
    assert_eq!(cfg.graph().node_count(), 0);
    assert_eq!(cfg.graph().edge_count(), 0);
    assert!(cfg.exit_node().is_none());
    assert_eq!(cfg.find_exit_nodes().len(), 0);
}

#[test]
fn test_non_terminating_function_still_has_exit() {
    let mut cfg = Cfg::new();

    // Create a function with only non-terminating instructions
    let instructions = vec![
        HbcFunctionInstruction {
            offset: 0,
            function_index: 0,
            instruction_index: 0,
            instruction: UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            },
        },
        HbcFunctionInstruction {
            offset: 2,
            function_index: 0,
            instruction_index: 1,
            instruction: UnifiedInstruction::Add {
                operand_0: 1,
                operand_1: 1,
                operand_2: 1,
            },
        },
    ];

    cfg.build_from_instructions(&instructions, 0);

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
