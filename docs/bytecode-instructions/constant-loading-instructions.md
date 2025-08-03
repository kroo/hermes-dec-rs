# Constant Loading Instructions

This document describes all LoadConst* instructions in Hermes bytecode and how they should be decompiled to JavaScript.

## Overview

LoadConst* instructions load constant values into registers. Hermes optimizes constant loading by using specific instructions for different value types and ranges.

## Integer Constants

### LoadConstUInt8
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstUInt8, Reg8, UInt8)
```

**Operands:**
- `Reg8`: Destination register
- `UInt8`: 8-bit unsigned integer value (0-255)

**Decompiles to:**
```javascript
// LoadConstUInt8 r0, 42
let x = 42;

// LoadConstUInt8 r1, 255  
let y = 255;
```

### LoadConstInt
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstInt, Reg8, Imm32)
```

**Operands:**
- `Reg8`: Destination register
- `Imm32`: 32-bit signed integer value

**Decompiles to:**
```javascript
// LoadConstInt r0, 12345
let x = 12345;

// LoadConstInt r1, -42
let y = -42;
```

### LoadConstZero
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstZero, Reg8)
```

**Operands:**
- `Reg8`: Destination register

**Purpose:** Optimized instruction for loading zero (very common value).

**Decompiles to:**
```javascript
// LoadConstZero r0
let x = 0;
```

## Floating Point Constants

### LoadConstDouble
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstDouble, Reg8, Double)
```

**Operands:**
- `Reg8`: Destination register
- `Double`: 64-bit double-precision floating-point value

**Decompiles to:**
```javascript
// LoadConstDouble r0, 3.14159
let pi = 3.14159;

// LoadConstDouble r1, -0.00056
let small = -0.00056;
```

## BigInt Constants

### LoadConstBigInt
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstBigInt, Reg8, UInt16)
```

**Operands:**
- `Reg8`: Destination register
- `UInt16`: Index into the BigInt table

**Decompiles to:**
```javascript
// LoadConstBigInt r0, 1  (table[1] = "1234567890123456789")
let big = 1234567890123456789n;
```

### LoadConstBigIntLongIndex
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstBigIntLongIndex, Reg8, UInt32)
```

**Operands:**
- `Reg8`: Destination register
- `UInt32`: Index into the BigInt table (for large tables)

**Purpose:** Used when BigInt table has more than 65535 entries.

**Decompiles to:**
```javascript
// LoadConstBigIntLongIndex r0, 70000
let big = 9999999999999999999999n;
```

## String Constants

### LoadConstString
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstString, Reg8, UInt16)
```

**Operands:**
- `Reg8`: Destination register
- `UInt16`: Index into the string table

**Decompiles to:**
```javascript
// LoadConstString r0, 5  (table[5] = "hello")
let greeting = "hello";

// LoadConstString r1, 12 (table[12] = "_modified")
let prop = "_modified";
```

### LoadConstStringLongIndex
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(LoadConstStringLongIndex, Reg8, UInt32)
```

**Operands:**
- `Reg8`: Destination register
- `UInt32`: Index into the string table (for large tables)

**Purpose:** Used when string table has more than 65535 entries.

**Decompiles to:**
```javascript
// LoadConstStringLongIndex r0, 100000
let str = "some string from large table";
```

## Special Value Constants

### LoadConstUndefined
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstUndefined, Reg8)
```

**Decompiles to:**
```javascript
// LoadConstUndefined r0
let x = undefined;

// Often implicit in variable declarations
let y;  // implicitly undefined
```

### LoadConstNull
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstNull, Reg8)
```

**Decompiles to:**
```javascript
// LoadConstNull r0
let x = null;
```

### LoadConstTrue
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstTrue, Reg8)
```

**Decompiles to:**
```javascript
// LoadConstTrue r0
let flag = true;
```

### LoadConstFalse
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstFalse, Reg8)
```

**Decompiles to:**
```javascript
// LoadConstFalse r0
let flag = false;
```

### LoadConstEmpty
**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(LoadConstEmpty, Reg8)
```

**Purpose:** Loads internal "empty" value used for uninitialized slots.

**Decompilation Note:** This is an internal VM value not directly representable in JavaScript. Usually indicates uninitialized variables or deleted array elements.

## Instruction Selection Hierarchy

Hermes chooses LoadConst instructions based on value type and range:

1. **+0.0** → `LoadConstZero`
2. **0-255** → `LoadConstUInt8`
3. **-2³¹ to 2³¹-1** → `LoadConstInt`
4. **Other numbers** → `LoadConstDouble`
5. **Strings** → `LoadConstString` (or LongIndex variant)
6. **BigInts** → `LoadConstBigInt` (or LongIndex variant)
7. **Special values** → Dedicated instructions

## Decompilation Guidelines

### Number Literals
```javascript
// Choose appropriate JavaScript representation
LoadConstUInt8 r0, 42      → let x = 42;
LoadConstInt r0, -1000     → let x = -1000;
LoadConstDouble r0, 3.14   → let x = 3.14;
LoadConstZero r0           → let x = 0;
```

### String Literals
```javascript
// Require table lookup and proper escaping
LoadConstString r0, idx    → let x = "resolved_string_value";
```

### BigInt Literals
```javascript
// Require table lookup and 'n' suffix
LoadConstBigInt r0, idx    → let x = 1234567890123456789n;
```

### Boolean and Special Values
```javascript
LoadConstTrue r0           → let x = true;
LoadConstFalse r0          → let x = false;
LoadConstNull r0           → let x = null;
LoadConstUndefined r0      → let x = undefined;  // or let x;
```

## Implementation Notes

- All LoadConst instructions use the `LOAD_CONST` macro pattern in the interpreter
- String and BigInt constants require table lookups to resolve the actual values
- LongIndex variants are automatically used when tables exceed 16-bit indexing
- The destination register receives the loaded constant value
- These are single-cycle operations that advance the instruction pointer

## For Decompiler Implementation

1. **Table Resolution**: String and BigInt instructions need table lookups
2. **Type Preservation**: Maintain JavaScript type semantics (number vs BigInt)
3. **Literal Formatting**: Apply proper JavaScript literal syntax
4. **Optimization Recognition**: Understand why certain instructions were chosen
5. **Context Awareness**: Consider how constants are used in surrounding code