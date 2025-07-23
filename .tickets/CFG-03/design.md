# CFG-03 Design: Multi-target Switch support

## Overview
Extend switch instruction handling to create proper edges for each case target and default case, enabling accurate switch region detection.

## Implementation Plan

### 1. Extend EdgeKind
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum EdgeKind {
    Uncond,
    True,
    False,
    Switch(usize), // Case index (0-based)
    Default,       // Default case for switches
    Fall,          // Fall-through
}
```

### 2. Switch Table Parsing
```rust
impl CfgBuilder {
    fn parse_switch_table(&self, instruction: &Instruction) -> Vec<(u32, u32)> {
        match instruction.opcode {
            Opcode::SwitchImm => {
                // Parse immediate switch table
                let table_offset = instruction.operands[1] as usize;
                // Access jump table from instruction context
                self.get_jump_table(table_offset)
            }
            Opcode::SwitchLong => {
                // Parse long switch table
                let table_offset = instruction.operands[1] as usize;
                self.get_jump_table_long(table_offset)
            }
            _ => vec![]
        }
    }
}
```

### 3. Multi-Edge Creation
```rust
fn add_switch_edges(&mut self, from: NodeIndex, instruction: &Instruction) {
    let switch_table = self.parse_switch_table(instruction);
    
    // Add edge for each case
    for (case_idx, target_pc) in switch_table.iter().enumerate() {
        if let Some(target_node) = self.block_starts.get(target_pc) {
            self.graph.add_edge(from, *target_node, EdgeKind::Switch(case_idx));
        }
    }
    
    // Add default edge (fall-through)
    let default_pc = instruction.pc + instruction.size as u32;
    if let Some(default_node) = self.block_starts.get(&default_pc) {
        self.graph.add_edge(from, *default_node, EdgeKind::Default);
    }
}
```

### 4. Jump Table Integration
```rust
impl CfgBuilder {
    fn get_jump_table(&self, offset: usize) -> Vec<(u32, u32)> {
        // Access the jump table from the function's jump table data
        // Return Vec<(case_value, target_pc)>
    }
    
    fn get_jump_table_long(&self, offset: usize) -> Vec<(u32, u32)> {
        // Similar to get_jump_table but for long format
    }
}
```

## Test Cases
- Switch with 3 cases + default
- Sparse switch (gaps in case values)
- Switch with only default case
- Empty switch table
- Both SwitchImm and SwitchLong variants

## Considerations
- Need access to jump table data from function context
- Handle edge cases where target PCs don't correspond to block starts
- Ensure switch edges are properly labeled for visualization
- Consider performance impact of parsing switch tables 