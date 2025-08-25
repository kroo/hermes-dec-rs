# Switch Statement Issues - Root Cause Analysis

## Overview
After implementing SSAUsageTracker to fix the partial elimination issue, we've identified four remaining issues in switch statement handling. All stem from how we handle **eliminated setup instructions** and **PHI node contributions**.

## Issue 1: Break-only Cases (Functions 3, 6) ✅ FIXED
**Status**: Both Functions Fixed

### Problem
When a switch case immediately breaks without any statements, setup instructions are eliminated but their values don't appear in the case body.

### Example (Function 3)
```javascript
// Original source (expected):
case 1: 
    result += "one ";
    break;

// Current decompiled output (broken):
case 1: 
    break;  // Missing: var1 = "one ";
```

### CFG Analysis
```
Case 0:
  Setup instructions: LoadConstString r1, "one "
  PHI contribution: <no contribution found>  // ← This is the problem
  
Shared tail PHI node: r1_5
  Expected contributions:
    Case 0: "one "     // Missing!
    Case 1: "two "     // Works
    Case 2: "three "   // Works
    Default: "other "  // Works
```

### Root Cause
1. Setup instruction `LoadConstString r1, "one "` is executed in the comparison block
2. The instruction is marked as eliminated by SSAUsageTracker
3. Case 0 jumps directly to shared tail without a case body
4. No PHI contribution is generated because there's no case body
5. The value "one " is lost

### Solution Implemented
1. Added `force_phi_contributions` parameter to prevent skipping eliminated SSA values when they contribute to PHI nodes
2. Check both for shared tail blocks AND blocks with PHI functions
3. Force generation of setup instructions for break-only cases that contribute to PHI nodes

The fix handles two scenarios:
- Cases going to a formal "shared tail" (all cases converge)
- Cases going to blocks with PHI nodes (partial convergence, like function 6)

### Code Location
- `src/ast/control_flow/switch_converter.rs`: `generate_case_body()` and `generate_setup_instructions()`
- Need to check if case contributes to PHI and has no other statements

---

## Issue 2: Fallthrough Variable Tracking (Function 4) ✅ FIXED
**Status**: Fixed

### Problem
Variables defined in early fallthrough cases aren't available in later cases.

### Example
```javascript
// Cases 0-2 define var2:
case 0:
case 1:
case 2:
    var2 = "low ";
    var0_e = var2 + var1_d;
    break;

// Cases 3-4 use undefined var2:
case 3:
case 4:
    var0_e = var2 + var1_d;  // ERROR: var2 undefined
    break;
```

### Solution Implemented
1. Force PHI contributions for cases with PHI nodes at their target block
2. Detect Mov instructions with empty string values that represent fallthroughs
3. Skip generating the incorrect empty string assignment
4. Let the variable reference from the previous case flow through

The fix detects when a Mov instruction is:
- Setting up a PHI contribution (target block has PHI for that register)
- Has a suspicious empty string value
- Skips the incorrect assignment, allowing the value from the previous case to be used

### Root Cause
- Fallthrough values aren't properly tracked through the control flow
- `Mov` instructions in bytecode use empty strings as placeholders
- The CFG analysis stores the literal empty string instead of tracking the actual fallthrough value

---

## Issue 3: Nested Switch Variable Scoping (Function 5)
**Status**: Not Started

### Problem
Variables used in nested switches aren't declared at function level.

### Example
```javascript
case 0:
    var0 = 0;  // ERROR: var0 not declared at top
    switch (param1) { ... }
```

### Root Cause
- Nested switch setup instructions create variables in local scope
- These variables aren't hoisted to function-level declarations
- Need better communication between nested and parent switch converters

---

## Issue 4: Duplicate Variable Declarations (Function 7)
**Status**: Not Started

### Problem
Variables declared multiple times.

### Example
```javascript
let var2;        // First declaration
let var2 = 0;    // Duplicate declaration
```

### Root Cause
- Variable added to declarations list
- Same variable also generated as a statement
- Missing check for already-declared variables

---

## Test Commands

### Test Function 3 (break-only case):
```bash
cargo run --bin hermes-dec-rs -- decompile data/dense_switch_test.hbc --function 3
```

Expected: `case 1:` should have `var1 = "one ";` before `break;`

### Test Function 4 (fallthrough):
```bash
cargo run --bin hermes-dec-rs -- decompile data/dense_switch_test.hbc --function 4
```

Expected: Cases 3-4 should define `var2` or reference it correctly

### Test Function 5 (nested switch):
```bash
cargo run --bin hermes-dec-rs -- decompile data/dense_switch_test.hbc --function 5
```

Expected: `var0` should be declared at function level

### Test Function 7 (duplicates):
```bash
cargo run --bin hermes-dec-rs -- decompile data/dense_switch_test.hbc --function 7
```

Expected: Only one declaration of `var2`