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
    // For each block that will be duplicated:
    // 1. Check if it contains PHI functions
    // 2. Identify which predecessors lead to duplication
    // 3. Determine concrete values for each duplication context
}
```

#### Step 2: Compute PHI Replacements for Duplicated Blocks
```rust
fn compute_phi_replacements(&mut self, block: NodeIndex, context: &DuplicationContext) {
    // For each PHI in the block:
    // 1. Identify the single predecessor for this duplication context
    // 2. Find the PHI operand from that predecessor
    // 3. Create replacement: phi_result → concrete_operand
    // 4. Mark PHI result as Skip in duplicated context
    // 5. Update strategy for concrete operand in duplicated context
}
```

#### Step 3: Update PHIs in Original Blocks
```rust
fn update_original_phis(&mut self, block: NodeIndex) {
    // For each PHI in the original block:
    // 1. Remove operands from predecessors that now go to duplicated blocks
    // 2. Create UpdatedPhiFunction with remaining operands
    // 3. If only one operand remains, mark for complete elimination
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

## Implementation Checklist

### Phase 1: Data Structure Setup
- [ ] Add `PhiDeconstructionInfo` struct
- [ ] Add `UpdatedPhiFunction` struct  
- [ ] Extend `ControlFlowPlan` with `phi_deconstructions` map
- [ ] Add methods to query/store PHI info

### Phase 2: Analysis Implementation
- [ ] Implement `collect_phi_affected_blocks()`
- [ ] Implement `compute_phi_replacements()`
- [ ] Implement `update_original_phis()`
- [ ] Integrate into `ControlFlowPlanAnalyzer::analyze()`

### Phase 3: SSA Tracker Integration
- [ ] Add `get_phi_replacement()` helper
- [ ] Update `get_declaration_strategy()` to handle PHI replacements
- [ ] Update `get_use_strategy()` to handle PHI replacements
- [ ] Add `resolve_phi_replacement()` for value resolution

### Phase 4: Code Generation Updates
- [ ] Update block generator to check for PHI info
- [ ] Implement PHI skipping for duplicated blocks
- [ ] Implement PHI updating for original blocks
- [ ] Add value mapping for replacements

### Phase 5: Testing and Validation
- [ ] Test simple fallthrough with PHI
- [ ] Test multiple fallthroughs
- [ ] Test nested switches with PHI
- [ ] Verify generated JavaScript correctness

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