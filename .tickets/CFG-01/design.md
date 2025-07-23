# CFG-01 Design: Populate pc_to_block map

## Overview
Extend the CFG builder to maintain a complete mapping from program counter values to their containing basic block nodes.

## Implementation Plan

### 1. Modify CfgBuilder::add_block
```rust
pub fn add_block(&mut self, start_pc: u32, end_pc: u32, instructions: Vec<Instruction>) -> NodeIndex {
    let node = self.graph.add_node(BasicBlock { /* ... */ });
    
    // Populate pc_to_block for all PCs in this block's range
    for pc in start_pc..end_pc {
        self.pc_to_block.insert(pc, node);
    }
    
    node
}
```

### 2. Add Helper Methods
```rust
impl CfgBuilder {
    /// Get the basic block containing the given PC
    pub fn get_block_at_pc(&self, pc: u32) -> Option<NodeIndex> {
        self.pc_to_block.get(&pc).copied()
    }
    
    /// Check if a PC is within any basic block
    pub fn is_pc_in_block(&self, pc: u32) -> bool {
        self.pc_to_block.contains_key(&pc)
    }
}
```

### 3. Test Cases
- Three-block function with gaps between blocks
- Single-instruction blocks (start_pc == end_pc - 1)
- Adjacent blocks (no gaps)
- Edge case: PC outside any block range

## Considerations
- Memory usage: Each PC value requires a HashMap entry
- Performance: HashMap lookup is O(1) average case
- Alternative: Could use sorted Vec + binary search for memory efficiency
- Validation: Ensure no overlapping block ranges during construction 