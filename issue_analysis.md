# Common Issue Analysis for Dense Switch Test

## Issue 1: Break-only Cases (Functions 3, 6)

### Pattern:
When a switch case immediately breaks without any statements, the setup instructions from the comparison block are being eliminated but not properly handled.

### Example from Function 3:
```javascript
// Original:
case 1: 
    result += "one ";
    break;

// Decompiled:
case 1: 
    break;  // Missing: var1 = "one ";
```

### Root Cause:
- The CFG shows setup instruction `LoadConstString r1, "one "` for case 0
- This instruction is marked as eliminated (see debug output)
- But no PHI contribution is found for this case (`<no contribution found>`)
- The setup instruction is being eliminated without being properly converted to a case statement

### Code Location:
In `switch_converter.rs`, when generating setup instructions for a case that immediately jumps to the shared tail, the setup values are being marked as eliminated but not actually used in the case body.

## Issue 2: Fallthrough Variable Tracking (Function 4)

### Pattern:
Variables defined in early fallthrough cases are not available in later cases that fall through.

### Example from Function 4:
```javascript
// Cases 3-4 use var2 which is only defined in cases 0-2
case 3:
case 4:
    var0_e = var2 + var1_d;  // var2 is undefined here
```

### Root Cause:
- Setup instruction for cases 3-4 shows `Mov r2, r1` with r2_1 = String("")
- The value should be the actual string "low " from the previous fallthrough
- The Mov instruction is being eliminated without propagating the correct value

## Issue 3: Nested Switch Variable Scoping (Function 5)

### Pattern:
Variables used in nested switch statements are not properly declared in the parent scope.

### Example:
```javascript
case 0:
    var0 = 0;  // var0 not declared at function level
```

### Root Cause:
- Setup instruction `LoadConstZero r0` is processed for the nested switch
- But var0 is not being hoisted to the function-level declarations
- The nested switch converter doesn't communicate variable needs to parent scope

## Issue 4: Duplicate Variable Declarations (Function 7)

### Pattern:
Variables are declared multiple times - both at the top and in the function body.

### Example:
```javascript
let var2;        // First declaration
let var2 = 0;    // Duplicate declaration
```

### Root Cause:
- r2_1 is defined with `LoadConstZero r2`
- The variable is added to declarations but also generated as a statement
- Missing check for already-declared variables when generating statements

## Common Theme:

All these issues relate to **setup instruction elimination and PHI node handling**:

1. **Eliminated setup instructions** are not being properly converted to case statements when needed
2. **PHI node values** are not being correctly propagated through fallthrough cases
3. **Variable declarations** are not being properly coordinated between nested scopes
4. **Declaration tracking** is not preventing duplicate declarations

## Suggested Fix Approach:

1. **For break-only cases**: Don't eliminate setup instructions that contribute to PHI nodes - instead, generate them as case statements
2. **For fallthroughs**: Track the actual values flowing through fallthrough paths, not just empty strings
3. **For nested switches**: Hoist variable declarations from nested switches to parent scope
4. **For duplicates**: Check if variable is already declared before generating declaration statements