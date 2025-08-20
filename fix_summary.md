# Fix Summary: Break-only Cases in Switch Statements

## Problem Fixed
Functions 3 and 6 had switch cases that would immediately break without any statements, causing missing variable assignments that should contribute to PHI nodes.

## Root Cause
Setup instructions that contribute to PHI nodes were being eliminated too aggressively. When a case had no body (just a break), the setup instruction that should assign a value was skipped entirely.

## Solution
Implemented a `force_phi_contributions` parameter that prevents skipping eliminated SSA values when they contribute to PHI nodes.

### Key Changes Made

1. **Added new method**: `generate_setup_instructions_with_phi_check()` with a `force_phi_contributions` parameter

2. **Modified elimination checks**: Multiple places that check for eliminated SSA values now respect the `force_phi_contributions` flag:
   - Initial elimination check (line ~1117)
   - Smart skip check for LoadConst instructions (line ~1152)  
   - Final elimination check before statement generation (line ~1342)

3. **Enhanced PHI detection**: Check for PHI nodes even when there's no formal "shared tail":
   ```rust
   let force_phi = if let Some(shared_tail) = &switch_info.shared_tail {
       group.target_block == shared_tail.block_id
   } else {
       // Even without a shared tail, check if target has PHI nodes
       block_converter.ssa_analysis()
           .phi_functions
           .get(&group.target_block)
           .map(|phis| !phis.is_empty())
           .unwrap_or(false)
   };
   ```

## Results

### Function 3 (switchWithBreak) - ✅ Fixed
```javascript
// Before: case 1 had just `break;`
// After: case 1 now has `var1 = "one ";` before break
case 1:
    var1 = "one ";
    break;
```

### Function 6 (mixedBreakReturn) - ✅ Fixed  
```javascript
// Before: case 1 had just `break;`
// After: case 1 now has `var1 = "one";` before break
case 1:
    var1 = "one";
    break;
```

## Technical Details

The fix handles two scenarios:
1. **Formal shared tail**: When all cases converge to the same block (like function 3)
2. **Partial convergence**: When only some cases converge to a block with PHI nodes (like function 6)

Function 6 was particularly challenging because block 8 has a PHI node but isn't a "shared tail" in the formal sense (not all cases go there). The enhanced check for PHI nodes regardless of shared tail status resolved this issue.