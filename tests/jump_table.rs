use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::tables::function_table::HbcFunctionInstruction;
use hermes_dec_rs::hbc::tables::JumpTable;

#[test]
fn test_jump_table_basic_functionality() {
    let mut jump_table = JumpTable::new();

    // Create a simple set of instructions with jumps
    let instructions = vec![
        HbcFunctionInstruction {
            offset: 0,
            function_index: 0,
            instruction_index: 0,
            instruction: UnifiedInstruction::Jmp { operand_0: 4 }, // Jump to instruction index 2 (offset 4)
        },
        HbcFunctionInstruction {
            offset: 2,
            function_index: 0,
            instruction_index: 1,
            instruction: UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            },
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
            instruction: UnifiedInstruction::JmpFalse {
                operand_0: -4,
                operand_1: 4,
            }, // Jump to instruction index 1
        },
        HbcFunctionInstruction {
            offset: 8,
            function_index: 0,
            instruction_index: 4,
            instruction: UnifiedInstruction::Ret { operand_0: 4 },
        },
    ];

    // Build the jump table
    let result = jump_table.build_for_function(0, &instructions, &[]);
    assert!(result.is_ok());

    // Check that labels were created for jump targets
    assert!(jump_table.get_label_by_inst_index(0, 2).is_some()); // Instruction 2 should have a label
    assert!(jump_table.get_label_by_inst_index(0, 1).is_some()); // Instruction 1 should have a label

    // Check that labels were NOT created for non-jump targets
    assert!(jump_table.get_label_by_inst_index(0, 0).is_none()); // Instruction 0 is not a jump target
    assert!(jump_table.get_label_by_inst_index(0, 3).is_none()); // Instruction 3 is not a jump target
    assert!(jump_table.get_label_by_inst_index(0, 4).is_none()); // Instruction 4 is not a jump target

    // Check that we can get labels for the function
    let labels = jump_table.get_labels_for_function(0);
    assert!(labels.is_some());
    assert_eq!(labels.unwrap().len(), 2); // Should have 2 labels (L1 and L2)

    // Check that we can get jumps for the function
    let jumps = jump_table.get_jumps_for_function(0);
    assert!(jumps.is_some());
    assert_eq!(jumps.unwrap().len(), 2); // Should have 2 jump instructions (Jmp and JmpFalse)
}
