# PHI Deconstruction Strategy for Block Duplication

## Overview

When duplicating blocks for switch fallthrough optimization, PHI functions in those blocks need special handling. This document describes the strategy for PHI deconstruction during block duplication.

## Problem Statement

### The Duplication Scenario
When we have switch fallthrough (e.g., cases 0,1,2 falling through to cases 3,4), we duplicate the target block to maintain correct semantics. Consider:

```
Block 9 (cases 0,1,2):
  r2 = "low "  // Creates r2_v3
  
Block 10 (cases 3,4):
  PHI: ɸ(r2_v1, r2_v2, r2_v3) → r2_v4
  r1 = "mid "
  r0 = r2_v4 + r1
```

When duplicating Block 10 for the fallthrough:
- **Original Block 10**: Receives control from blocks 3, 4, or 9
- **Duplicated Block 10**: Only receives control from block 9

### The PHI Problem
1. **Duplicated blocks**: PHI functions become meaningless (single predecessor)
2. **Original blocks**: PHI functions need updating (lost predecessors) 
3. **Variable references**: Must use correct SSA values in each context

## Solution Architecture

### 1. Data Structures

```rust
/// PHI deconstruction information for a block
pub struct PhiDeconstructionInfo {
    /// PHI replacements for duplicated blocks
    /// Maps: PHI result → concrete predecessor value
    pub replacements: HashMap<SSAValue, SSAValue>,
    
    /// Updated PHI functions for original blocks
    /// (with removed predecessors)
    pub updated_phis: Vec<UpdatedPhiFunction>,
}

pub struct UpdatedPhiFunction {
    pub result: SSAValue,
    /// Remaining predecessors and their values
    pub operands: Vec<(NodeIndex, SSAValue)>,
}

/// Extension to ControlFlowPlan
pub struct ControlFlowPlan {
    // ... existing fields ...
    
    /// PHI deconstruction info per block and duplication context
    /// Key: (block_id, Option<DuplicationContext>)
    /// - None = original block (may have updated PHIs)
    /// - Some(ctx) = duplicated block (has PHI replacements)
    pub phi_deconstructions: HashMap<(NodeIndex, Option<DuplicationContext>), PhiDeconstructionInfo>,
}
```

### 2. Analysis Phase (ControlFlowPlanAnalyzer)

#### Step 1: Identify Blocks with PHIs to Deconstruct
```rust
fn collect_phi_affected_blocks(&self) -> HashMap<NodeIndex, PhiAffectedInfo> {
    let mut affected = HashMap::new();
    
    // For each block that will be duplicated:
    for (block_id, contexts) in &self.duplicated_blocks {
        if let Some(phi_functions) = self.ssa.phi_functions.get(&block_id) {
            let info = PhiAffectedInfo {
                phis: phi_functions.clone(),
                duplication_contexts: contexts.clone(),
                predecessors: self.cfg.predecessors(block_id).collect(),
            };
            affected.insert(block_id, info);
        }
    }
    
    affected
}
```

#### Step 2: Compute PHI Replacements for Duplicated Blocks
```rust
fn compute_phi_replacements(&mut self, 
    block: NodeIndex, 
    context: &DuplicationContext,
    phi_functions: &[PhiFunction]
) -> PhiDeconstructionInfo {
    let mut replacements = HashMap::new();
    
    // Determine the predecessor for this duplication context
    let predecessor = match context {
        DuplicationContext::SwitchFallthrough { from_case_index, .. } => {
            // Find the block that implements from_case_index
            self.find_case_block(*from_case_index)
        }
        DuplicationContext::SwitchBlockDuplication { case_group_keys } => {
            // Find the common predecessor for these cases
            self.find_common_predecessor(case_group_keys)
        }
    };
    
    // For each PHI in the block:
    for phi in phi_functions {
        // Find the operand from this predecessor
        if let Some(operand) = phi.operands.get(&predecessor) {
            // Create replacement: phi_result → concrete_operand
            replacements.insert(phi.result.clone(), operand.clone());
            
            // Mark PHI result as Skip in duplicated context
            let dup_phi_result = DuplicatedSSAValue {
                original: phi.result.clone(),
                duplication_context: Some(context.clone()),
            };
            self.plan.set_declaration_strategy(dup_phi_result, DeclarationStrategy::Skip);
            
            // Ensure concrete operand has appropriate strategy
            let dup_operand = DuplicatedSSAValue {
                original: operand.clone(),
                duplication_context: Some(context.clone()),
            };
            // The operand keeps its original strategy (likely AssignOnly if it's a PHI operand)
            let operand_strategy = self.usage_tracker.get_declaration_strategy(&dup_operand);
            self.plan.set_declaration_strategy(dup_operand, operand_strategy);
        }
    }
    
    PhiDeconstructionInfo {
        replacements,
        updated_phis: vec![], // No updated PHIs for duplicated blocks
    }
}
```

#### Step 3: Update PHIs in Original Blocks
```rust
fn update_original_phis(&mut self, 
    block: NodeIndex,
    phi_functions: &[PhiFunction],
    removed_predecessors: &[NodeIndex]
) -> PhiDeconstructionInfo {
    let mut updated_phis = Vec::new();
    
    for phi in phi_functions {
        // Filter out removed predecessors
        let remaining_operands: Vec<(NodeIndex, SSAValue)> = phi.operands
            .iter()
            .filter(|(pred, _)| !removed_predecessors.contains(pred))
            .map(|(p, v)| (*p, v.clone()))
            .collect();
        
        if remaining_operands.len() > 1 {
            // Still need a PHI
            updated_phis.push(UpdatedPhiFunction {
                result: phi.result.clone(),
                operands: remaining_operands,
            });
        } else if remaining_operands.len() == 1 {
            // PHI degenerates to simple assignment
            // Mark the PHI result to use AssignOnly strategy
            let dup_result = DuplicatedSSAValue::original(phi.result.clone());
            self.plan.set_declaration_strategy(dup_result, DeclarationStrategy::AssignOnly);
        }
        // If no operands remain, this is an error condition
    }
    
    PhiDeconstructionInfo {
        replacements: HashMap::new(), // No replacements for original blocks
        updated_phis,
    }
}
```

### 3. SSA Usage Tracking Integration

#### Declaration Strategy Adjustments
```rust
impl SSAUsageTracker {
    fn get_declaration_strategy_with_phi(&self, 
        dup_value: &DuplicatedSSAValue
    ) -> DeclarationStrategy {
        // 1. Check if this is a PHI result in a duplicated block
        if let Some(replacement) = self.get_phi_replacement(dup_value) {
            // PHI result doesn't exist in duplicated context
            return DeclarationStrategy::Skip;
        }
        
        // 2. Check if this is a concrete value replacing a PHI
        if self.is_phi_replacement_value(dup_value) {
            // May need different strategy than original
            return self.compute_replacement_strategy(dup_value);
        }
        
        // 3. Normal strategy computation
        self.get_declaration_strategy(dup_value)
    }
}
```

#### Use Strategy Adjustments
```rust
impl SSAUsageTracker {
    fn get_use_strategy_with_phi(&self,
        value: &SSAValue,
        use_site: &RegisterUse,
        context: Option<&DuplicationContext>
    ) -> UseStrategy {
        // 1. Get effective value after PHI deconstruction
        let effective_value = self.resolve_phi_replacement(value, use_site.block_id, context);
        
        // 2. Determine if effective value can be inlined
        if self.can_inline_value(&effective_value, use_site) {
            return UseStrategy::InlineValue(self.get_constant(&effective_value));
        }
        
        // 3. Use variable reference
        UseStrategy::UseVariable
    }
}
```

### 4. Code Generation Phase

#### Block Generation with PHI Handling
```rust
fn generate_block(&mut self, block_id: NodeIndex, context: Option<&DuplicationContext>) {
    // 1. Get PHI deconstruction info
    let phi_info = self.plan.get_phi_info(block_id, context);
    
    if let Some(context) = context {
        // Duplicated block:
        // - Skip PHI generation entirely
        // - Set up value mappings for replacements
        for (phi_result, concrete_value) in &phi_info.replacements {
            self.value_map.insert(phi_result, concrete_value);
        }
    } else if !phi_info.updated_phis.is_empty() {
        // Original block with updated PHIs:
        // - Generate PHIs with reduced operands
        for updated_phi in &phi_info.updated_phis {
            if updated_phi.operands.len() > 1 {
                self.generate_phi(updated_phi);
            } else {
                // Single operand - convert to simple assignment
                self.generate_assignment(updated_phi.result, updated_phi.operands[0].1);
            }
        }
    }
    
    // Generate remaining instructions with value mappings applied
    self.generate_instructions(block_id, context);
}
```

## Example Walkthrough

### Input
```
Block 9: r2_v3 = "low "
Block 10: 
  PHI: ɸ(from_3: r2_v1, from_4: r2_v2, from_9: r2_v3) → r2_v4
  r1_v5 = "mid "
  r0_v10 = r2_v4 + r1_v5
```

### After Duplication and PHI Deconstruction

#### Original Block 10 (cases 3,4)
```
PHI: ɸ(from_3: r2_v1, from_4: r2_v2) → r2_v4  // Updated: removed from_9
r1_v5 = "mid "
r0_v10 = r2_v4 + r1_v5
```

#### Duplicated Block 10 (fallthrough from 0,1,2)
```
// No PHI - replaced with concrete value
// Replacements: r2_v4 → r2_v3
r1_v5_dup = "mid "
r0_v10_dup = r2_v3 + r1_v5_dup  // Uses r2_v3 directly
```

### Generated JavaScript
```javascript
let var0_e, var2;

// Cases with fallthrough
case 0:
case 1:
case 2:
  var2 = "low ";  // r2_v3
  // Duplicated block 10 (no PHI)
  const var1_d_dup = "mid ";
  var0_e = var2 + var1_d_dup;  // Uses r2_v3 via var2
  break;

// Original cases
case 3:
  var2 = "";  // r2_v1
  // Fall into block 10
case 4:
  if (/* came from case 4 */) var2 = "";  // r2_v2
  // Original block 10 (updated PHI)
  const var1_d = "mid ";
  var0_e = var2 + var1_d;  // Uses r2_v4 (result of updated PHI)
  break;
```

## Critical Implementation Notes

### Predecessor Mapping
The key challenge is correctly mapping duplication contexts to their predecessors:

1. **SwitchFallthrough**: 
   - The predecessor is the last block of the "from" case group
   - For `SwitchFallthrough { from_case_index: 0, to_case_index: 1 }`
   - Find the case group at index 0, get its body blocks
   - The predecessor is the last block that would execute before fallthrough

2. **SwitchBlockDuplication**:
   - Used when multiple cases share a body
   - For `SwitchBlockDuplication { case_group_keys: [0, 1, 2] }`
   - The predecessor is the dispatch block or comparison chain
   - Need to trace control flow from dispatch to the duplicated block

### Determining the Concrete Predecessor
```rust
fn determine_predecessor_for_context(
    &self,
    duplicated_block: NodeIndex,
    context: &DuplicationContext,
) -> NodeIndex {
    match context {
        DuplicationContext::SwitchFallthrough { from_case_index, .. } => {
            // Get the case group that's falling through
            let from_group = &self.plan.get_case_group(from_case_index);
            // Find the last block in that group's body
            self.find_last_block_in_structure(from_group.body)
        }
        DuplicationContext::SwitchBlockDuplication { case_group_keys } => {
            // Find which block leads to this duplication
            // This is typically the block just before the duplicated block
            // in the original control flow
            self.cfg.predecessors(duplicated_block)
                .find(|pred| self.is_predecessor_for_cases(pred, case_group_keys))
                .unwrap_or_else(|| panic!("No predecessor found for duplication"))
        }
    }
}
```

### SSA Value References
When replacing PHI results with concrete values, we must update ALL references:
- Direct uses in instructions
- Uses as operands to other PHIs
- Uses in condition expressions

### Declaration Strategy Coordination
The strategy for replaced values must be coordinated:
- PHI result in duplicated context: Always Skip
- Concrete replacement value: Depends on its original context
- May need to create new variable if used across scopes

## Implementation Checklist

### Phase 1: Data Structure Setup
- [ ] Add `PhiDeconstructionInfo` struct to control_flow_plan.rs
- [ ] Add `UpdatedPhiFunction` struct to control_flow_plan.rs
- [ ] Extend `ControlFlowPlan` with `phi_deconstructions` field
- [ ] Add `get_phi_info()` method to ControlFlowPlan
- [ ] Add `set_phi_info()` method to ControlFlowPlan

### Phase 2: Analysis Implementation
- [ ] Add `PhiAffectedInfo` helper struct
- [ ] Implement `collect_phi_affected_blocks()`
- [ ] Implement `find_case_block()` helper
- [ ] Implement `find_common_predecessor()` helper
- [ ] Implement `compute_phi_replacements()`
- [ ] Implement `update_original_phis()`
- [ ] Add `analyze_phi_deconstruction()` main method
- [ ] Call from `ControlFlowPlanAnalyzer::analyze()`

### Phase 3: SSA Tracker Integration
- [ ] Add `phi_replacements` cache to SSAUsageTracker
- [ ] Add `get_phi_replacement()` method
- [ ] Update `get_declaration_strategy()` to check PHI replacements
- [ ] Update `get_use_strategy()` to resolve replacements
- [ ] Add `is_phi_replacement()` helper
- [ ] Update `is_duplicated_fully_eliminated()` for replaced values

### Phase 4: Code Generation Updates
- [ ] Create `PhiDeconstructionContext` for code gen
- [ ] Update instruction visitor to use replacement mappings
- [ ] Skip PHI generation for duplicated blocks
- [ ] Generate updated PHIs for original blocks
- [ ] Handle degenerate PHIs (single operand)
- [ ] Add value resolution through replacement chain

### Phase 5: Testing and Validation
- [ ] Create test case: simple fallthrough with PHI
- [ ] Create test case: multiple fallthroughs to same block
- [ ] Create test case: nested switches with PHI
- [ ] Create test case: PHI with all constant operands
- [ ] Create test case: PHI with mixed constant/variable operands
- [ ] Verify no undefined variables in output
- [ ] Verify correct value propagation
- [ ] Verify optimal variable allocation

## Success Criteria

1. **Correctness**: Generated code produces same results as original
2. **No Undefined Variables**: All PHI replacements resolve to defined values
3. **Optimal Variable Usage**: No unnecessary intermediate variables
4. **Proper Scoping**: Variables declared at appropriate scope levels
5. **Clean Output**: Generated JavaScript is readable and idiomatic

## Edge Cases to Handle

1. **Nested PHIs**: PHI results used as operands to other PHIs
2. **Multiple Duplications**: Same block duplicated for different contexts
3. **Chained Fallthroughs**: Cases falling through multiple times
4. **PHI-only Blocks**: Blocks containing only PHI functions
5. **Recursive PHIs**: PHI functions with self-references (loops)

## Future Enhancements

1. **PHI Coalescing**: Merge PHI results that always have same value
2. **Dead PHI Elimination**: Remove PHIs whose results are never used
3. **PHI Minimization**: Reduce number of PHI operands through analysis
4. **Smart Inlining**: Inline PHI operands when beneficial