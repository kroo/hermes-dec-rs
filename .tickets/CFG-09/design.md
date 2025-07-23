# CFG-09 Design: Leader after Return/Throw

## Overview
Extend leader detection to insert a leader after Return/Throw instructions to properly separate unreachable code into its own basic block.

## Implementation Plan

### 1. Update Leader Detection Logic
```rust
impl CfgBuilder {
    fn find_leaders(&self, instructions: &[Instruction]) -> HashSet<u32> {
        let mut leaders = HashSet::new();
        
        // Entry point is always a leader
        if !instructions.is_empty() {
            leaders.insert(instructions[0].pc);
        }
        
        // Find leaders after jumps and branches
        for (i, instruction) in instructions.iter().enumerate() {
            // Existing logic for jump targets
            if self.is_jump_instruction(instruction) {
                let target_pc = self.get_jump_target(instruction);
                leaders.insert(target_pc);
                
                // Add leader after jump (fall-through)
                if i + 1 < instructions.len() {
                    let next_pc = instructions[i + 1].pc;
                    leaders.insert(next_pc);
                }
            }
            
            // NEW: Add leader after Return/Throw terminators
            if self.is_terminating_instruction(instruction) {
                let next_pc = instruction.pc + instruction.size as u32;
                leaders.insert(next_pc);
            }
        }
        
        leaders
    }
}
```

### 2. Terminating Instruction Detection
```rust
impl CfgBuilder {
    fn is_terminating_instruction(&self, instruction: &Instruction) -> bool {
        matches!(instruction.opcode,
            Opcode::Return | Opcode::Throw
        )
    }
}
```

### 3. Handle Edge Cases
```rust
impl CfgBuilder {
    fn find_leaders(&self, instructions: &[Instruction]) -> HashSet<u32> {
        let mut leaders = HashSet::new();
        
        // Entry point is always a leader
        if !instructions.is_empty() {
            leaders.insert(instructions[0].pc);
        }
        
        for (i, instruction) in instructions.iter().enumerate() {
            // Existing jump logic...
            
            // Add leader after Return/Throw terminators
            if self.is_terminating_instruction(instruction) {
                let next_pc = instruction.pc + instruction.size as u32;
                
                // Only add if next_pc is within function bounds
                if next_pc < self.function_end_pc {
                    leaders.insert(next_pc);
                }
            }
        }
        
        leaders
    }
}
```

### 4. Update Basic Block Creation
```rust
impl CfgBuilder {
    fn create_basic_blocks(&mut self, instructions: &[Instruction]) -> Vec<(u32, u32, Vec<Instruction>)> {
        let leaders = self.find_leaders(instructions);
        let mut leader_pcs: Vec<u32> = leaders.into_iter().collect();
        leader_pcs.sort();
        
        let mut blocks = Vec::new();
        
        for i in 0..leader_pcs.len() {
            let start_pc = leader_pcs[i];
            let end_pc = if i + 1 < leader_pcs.len() {
                leader_pcs[i + 1]
            } else {
                self.function_end_pc
            };
            
            // Collect instructions for this block
            let block_instructions: Vec<Instruction> = instructions
                .iter()
                .filter(|instr| instr.pc >= start_pc && instr.pc < end_pc)
                .cloned()
                .collect();
            
            // Only create block if it has instructions or is a valid leader
            if !block_instructions.is_empty() || self.is_valid_leader(start_pc) {
                blocks.push((start_pc, end_pc, block_instructions));
            }
        }
        
        blocks
    }
    
    fn is_valid_leader(&self, pc: u32) -> bool {
        // A PC is a valid leader if it's within function bounds
        pc < self.function_end_pc
    }
}
```

### 5. Test Cases
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_leader_after_return() {
        let instructions = vec![
            Instruction { pc: 0, opcode: Opcode::LoadConst, operands: vec![1], size: 2 },
            Instruction { pc: 2, opcode: Opcode::Return, operands: vec![], size: 1 },
            Instruction { pc: 3, opcode: Opcode::LoadConst, operands: [2], size: 2 }, // Unreachable
        ];
        
        let mut builder = CfgBuilder::new();
        let leaders = builder.find_leaders(&instructions);
        
        assert!(leaders.contains(&0));  // Entry
        assert!(leaders.contains(&3));  // After return
        assert_eq!(leaders.len(), 2);
    }
    
    #[test]
    fn test_leader_after_throw() {
        let instructions = vec![
            Instruction { pc: 0, opcode: Opcode::LoadConst, operands: vec![1], size: 2 },
            Instruction { pc: 2, opcode: Opcode::Throw, operands: vec![], size: 1 },
            Instruction { pc: 3, opcode: Opcode::LoadConst, operands: [2], size: 2 }, // Unreachable
        ];
        
        let mut builder = CfgBuilder::new();
        let leaders = builder.find_leaders(&instructions);
        
        assert!(leaders.contains(&0));  // Entry
        assert!(leaders.contains(&3));  // After throw
        assert_eq!(leaders.len(), 2);
    }
    
    #[test]
    fn test_return_at_end() {
        let instructions = vec![
            Instruction { pc: 0, opcode: Opcode::LoadConst, operands: vec![1], size: 2 },
            Instruction { pc: 2, opcode: Opcode::Return, operands: vec![], size: 1 },
        ];
        
        let mut builder = CfgBuilder::new();
        builder.function_end_pc = 3;
        let leaders = builder.find_leaders(&instructions);
        
        assert!(leaders.contains(&0));  // Entry
        assert!(leaders.contains(&3));  // After return (function end)
        assert_eq!(leaders.len(), 2);
    }
}
```

## Considerations
- Only add leader if next_pc is within function bounds
- Handle case where Return/Throw is the last instruction
- Maintain existing leader detection performance
- Ensure unreachable blocks are properly separated
- Consider impact on CFG analysis algorithms
- May need to update edge creation logic for terminating blocks 