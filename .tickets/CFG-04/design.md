# CFG-04 Design: Precise conditional-edge kinds

## Overview
Extend the conditional jump edge classification to properly handle all Hermes conditional jump variants with precise True/False labeling.

## Implementation Plan

### 1. Extend Conditional Jump Detection
```rust
impl CfgBuilder {
    fn is_conditional_jump(&self, opcode: Opcode) -> bool {
        matches!(opcode,
            Opcode::JEqual | Opcode::JNotEqual |
            Opcode::JLess | Opcode::JLessEqual |
            Opcode::JGreater | Opcode::JGreaterEqual |
            Opcode::JLessUnsigned | Opcode::JLessEqualUnsigned |
            Opcode::JGreaterUnsigned | Opcode::JGreaterEqualUnsigned |
            Opcode::JStrictEqual | Opcode::JStrictNotEqual |
            Opcode::JTrue | Opcode::JFalse
        )
    }
}
```

### 2. Enhanced Edge Kind Mapping
```rust
fn get_edge_kind(&self, instruction: &Instruction, is_taken: bool) -> EdgeKind {
    match instruction.opcode {
        // Existing cases
        Opcode::JTrue => if is_taken { EdgeKind::True } else { EdgeKind::False },
        Opcode::JFalse => if is_taken { EdgeKind::False } else { EdgeKind::True },
        
        // New conditional cases - all follow same pattern
        Opcode::JEqual | Opcode::JLess | Opcode::JLessEqual |
        Opcode::JGreater | Opcode::JGreaterEqual | Opcode::JNotEqual |
        Opcode::JLessUnsigned | Opcode::JLessEqualUnsigned |
        Opcode::JGreaterUnsigned | Opcode::JGreaterEqualUnsigned |
        Opcode::JStrictEqual | Opcode::JStrictNotEqual => {
            if is_taken { EdgeKind::True } else { EdgeKind::False }
        }
        
        // Non-conditional jumps
        Opcode::Jmp | Opcode::JmpLong => EdgeKind::Uncond,
        Opcode::SwitchImm | Opcode::SwitchLong => EdgeKind::Switch(0), // Will be refined in CFG-03
        
        // Default case
        _ => EdgeKind::Uncond,
    }
}
```

### 3. Edge Creation Logic
```rust
fn add_conditional_edges(&mut self, from: NodeIndex, instruction: &Instruction) {
    let target_pc = self.get_jump_target(instruction);
    
    if let Some(target_node) = self.block_starts.get(&target_pc) {
        // Taken edge (jump to target)
        let taken_kind = self.get_edge_kind(instruction, true);
        self.graph.add_edge(from, *target_node, taken_kind);
    }
    
    // Fall-through edge (don't jump)
    let fall_through_pc = instruction.pc + instruction.size as u32;
    if let Some(fall_node) = self.block_starts.get(&fall_through_pc) {
        let fall_kind = self.get_edge_kind(instruction, false);
        self.graph.add_edge(from, *fall_node, fall_kind);
    }
}
```

### 4. Helper Method
```rust
impl CfgBuilder {
    fn get_jump_target(&self, instruction: &Instruction) -> u32 {
        match instruction.opcode {
            Opcode::Jmp | Opcode::JTrue | Opcode::JFalse |
            Opcode::JEqual | Opcode::JNotEqual |
            Opcode::JLess | Opcode::JLessEqual |
            Opcode::JGreater | Opcode::JGreaterEqual |
            Opcode::JLessUnsigned | Opcode::JLessEqualUnsigned |
            Opcode::JGreaterUnsigned | Opcode::JGreaterEqualUnsigned |
            Opcode::JStrictEqual | Opcode::JStrictNotEqual => {
                instruction.pc + instruction.operands[0] as u32
            }
            _ => instruction.pc
        }
    }
}
```

## Test Cases
- Each conditional jump variant with both taken and fall-through paths
- Mixed conditional and unconditional jumps in same function
- Edge cases: conditional jumps to same block as fall-through
- Complex nested conditional structures

## Considerations
- All conditional jumps follow the same True/False pattern
- Signed vs unsigned variants are semantically equivalent for edge classification
- Strict equality variants (JStrictEqual/JStrictNotEqual) follow same pattern as regular equality
- Maintains existing behavior for JTrue/JFalse instructions 