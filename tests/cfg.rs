use hermes_dec_rs::cfg::{Cfg, Block};
use hermes_dec_rs::hbc::function_table::HbcFunctionInstruction;
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;

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
        instruction: UnifiedInstruction::LoadConstUInt8 { operand_0: 0, operand_1: 42 },
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