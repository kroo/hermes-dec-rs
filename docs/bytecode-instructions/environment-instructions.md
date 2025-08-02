# Environment Instructions

This document describes the environment-related bytecode instructions in Hermes VM.

## CreateEnvironment vs CreateInnerEnvironment

### CreateEnvironment

**Purpose:** Creates a new function top-level environment.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(CreateEnvironment, Reg8)
```

**Operands:**
- `Reg8`: Destination register

**Semantics:**
- Creates the top-level environment for a function scope
- Automatically uses the current function's closure environment as parent
- Environment size determined by the function's compile-time analysis (`curCodeBlock->getEnvironmentSize()`)
- Used for function-level variable scoping

**Usage Example:**
```javascript
function test() {
    let a = 42;  // Variables go in this environment
    let b = 10;
}
```

### CreateInnerEnvironment

**Purpose:** Creates a new environment with a specified parent environment.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(CreateInnerEnvironment, Reg8, Reg8, UInt32)
```

**Operands:**
- `Arg1` (Reg8): Destination register
- `Arg2` (Reg8): Parent environment register
- `Arg3` (UInt32): Number of slots in the environment

**Semantics:**
- Creates nested inner environments (block scopes, nested functions)
- Explicitly specifies the parent environment and slot count
- Calls `Environment::create()` with explicit parent and slot count
- Used for lexical scoping within functions

**Usage Example:**
```javascript
function test() {
    let x = 1;
    {
        let y = 2;  // This block creates an inner environment
        let z = 3;
    }
}
```

## Key Differences

| Aspect | CreateEnvironment | CreateInnerEnvironment |
|--------|------------------|----------------------|
| **Scope Level** | Function top-level | Nested scopes (blocks, inner functions) |
| **Parent** | Automatic (function closure) | Explicit parent environment |
| **Slot Count** | Predetermined by function analysis | Explicit parameter |
| **Usage** | Function entry | Block scopes, nested functions |

## Implementation Details

Both instructions are implemented in the VM interpreter:

```cpp
// CreateEnvironment implementation
CASE(CreateEnvironment) {
  // Uses function's predetermined environment size
  // Parent is automatically the function's closure environment
}

// CreateInnerEnvironment implementation  
CASE(CreateInnerEnvironment) {
  // Takes explicit parent and slot count
  // More flexible for nested scoping
}
```

## Related Instructions

- `StoreToEnvironment` / `StoreNPToEnvironment` - Store values to environment slots
- `LoadFromEnvironment` - Load values from environment slots
- `GetEnvironment` - Get the current environment