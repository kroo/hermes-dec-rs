# CFG-02 Design: Add explicit Exit node & edges

## Overview
Introduce a synthetic EXIT node to represent function termination and ensure all control flow paths have a clear destination.

## Implementation Plan

### 1. Extend BasicBlock enum
```rust
#[derive(Debug, Clone)]
pub enum BasicBlock {
    Code { /* existing fields */ },
    Exit, // New variant for synthetic exit node
}
```

### 2. Modify CfgBuilder
```rust
impl CfgBuilder {
    pub fn new() -> Self {
        let mut builder = CfgBuilder { /* existing fields */ };
        
        // Add EXIT node immediately
        let exit_node = builder.graph.add_node(BasicBlock::Exit);
        builder.exit_node = Some(exit_node);
        
        builder
    }
    
    pub fn add_exit_edge(&mut self, from: NodeIndex) {
        if let Some(exit_node) = self.exit_node {
            self.graph.add_edge(from, exit_node, EdgeKind::Uncond);
        }
    }
}
```

### 3. Update Edge Creation Logic
```rust
// In add_edges method, after processing all instructions:
if self.is_terminating_block(block_node) {
    self.add_exit_edge(block_node);
}
```

### 4. Helper Methods
```rust
impl CfgBuilder {
    fn is_terminating_block(&self, node: NodeIndex) -> bool {
        if let Some(block) = self.graph.node_weight(node) {
            if let BasicBlock::Code { instructions, .. } = block {
                if let Some(last_instr) = instructions.last() {
                    return matches!(last_instr.opcode, Opcode::Return | Opcode::Throw);
                }
            }
        }
        false
    }
}
```

## Considerations
- EXIT node should be added before any other nodes to ensure consistent NodeIndex
- All analysis algorithms must handle the EXIT node appropriately
- DOT visualization should clearly distinguish EXIT from code blocks
- Post-dominator analysis will use EXIT as the root node 