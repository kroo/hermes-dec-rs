use hermes_dec_rs::cfg::CfgBuilder;
use hermes_dec_rs::cfg::{Block, Cfg, EdgeKind};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::function_table::HbcFunctionInstruction;
use petgraph::graph::DiGraph;

#[test]
fn test_cfg_creation() {
    let cfg = Cfg::new();
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
    let mut cfg = Cfg::new();
    cfg.build_from_instructions(&[], 0);
    assert_eq!(cfg.graph().node_count(), 0);
    assert_eq!(cfg.graph().edge_count(), 0);
}

#[test]
fn test_cfg_building_with_single_instruction() {
    let mut cfg = Cfg::new();

    // Create a simple instruction (LoadConstUInt8)
    let instruction = HbcFunctionInstruction {
        offset: 0,
        function_index: 0,
        instruction_index: 0,
        instruction: UnifiedInstruction::LoadConstUInt8 {
            operand_0: 0,
            operand_1: 42,
        },
    };

    let instructions = vec![instruction];
    cfg.build_from_instructions(&instructions, 0);

    // Should have one block
    assert_eq!(cfg.graph().node_count(), 1);
    assert_eq!(cfg.graph().edge_count(), 0);

    // Check the block contents
    let block = &cfg.graph()[cfg.graph().node_indices().next().unwrap()];
    assert_eq!(block.start_pc, 0);
    assert_eq!(block.instruction_count(), 1);
}

#[test]
fn test_pc_lookup_three_blocks() {
    let mut builder = CfgBuilder::new(0);
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
fn test_pc_lookup_overlapping_blocks() {
    let mut builder = CfgBuilder::new(0);
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
