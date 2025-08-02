# Memory Access Instructions

This document describes the unsafe memory access instructions in Hermes VM, used for WebAssembly and asm.js compatibility.

## Overview

These instructions provide direct memory access to typed arrays, bypassing JavaScript's normal memory safety mechanisms. They are only available when compiled with WebAssembly support (`HERMES_RUN_WASM`) and require the `-funsafe-intrinsics` compiler flag.

## Load Instructions

### Loadu32

**Purpose:** Load unsigned 32-bit integer from typed array memory.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(Loadu32, Reg8, Reg8, Reg8)
```

**Operands:**
- `Arg1` (Reg8): Destination register
- `Arg2` (Reg8): Source typed array register (must be JSTypedArrayBase)
- `Arg3` (Reg8): Address/offset register (treated as uint32_t)

**Implementation:**
```cpp
auto *mem = vmcast<JSTypedArrayBase>(O2REG(Loadu32));
uint32_t *basePtr = reinterpret_cast<uint32_t *>(mem->begin(runtime));
const uint32_t addr = (uint32_t)(int32_t)(O3REG(Loadu32).getNumber());
O1REG(Loadu32) = HermesValue::encodeUntrustedNumberValue(basePtr[addr >> 2]);
```

**Key Points:**
- Address is divided by 4 (`addr >> 2`) for 32-bit alignment
- Returns unsigned 32-bit value
- Result is untrusted number value

### Loadi16

**Purpose:** Load signed 16-bit integer from typed array memory.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(Loadi16, Reg8, Reg8, Reg8)
```

**Operands:**
- `Arg1` (Reg8): Destination register
- `Arg2` (Reg8): Source typed array register (must be JSTypedArrayBase)
- `Arg3` (Reg8): Address/offset register (treated as uint32_t)

**Implementation:**
```cpp
auto *mem = vmcast<JSTypedArrayBase>(O2REG(Loadi16));
int16_t *basePtr = reinterpret_cast<int16_t *>(mem->begin(runtime));
const uint32_t addr = (uint32_t)(int32_t)(O3REG(Loadi16).getNumber());
O1REG(Loadi16) = HermesValue::encodeUntrustedNumberValue(basePtr[addr >> 1]);
```

**Key Points:**
- Address is divided by 2 (`addr >> 1`) for 16-bit alignment
- Returns signed 16-bit value, sign-extended to 32-bit
- Result is untrusted number value

## Store Instructions

### Store8

**Purpose:** Store value as 8-bit signed integer.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(Store8, Reg8, Reg8, Reg8)
```

**Operands:**
- `Arg1` (Reg8): Typed array object (JSTypedArrayBase)
- `Arg2` (Reg8): Address/offset in the buffer
- `Arg3` (Reg8): Value to store

**Implementation:**
```cpp
auto *mem = vmcast<JSTypedArrayBase>(O1REG(Store8));
int8_t *basePtr = reinterpret_cast<int8_t *>(mem->begin(runtime));
const uint32_t addr = (uint32_t)(int32_t)(O2REG(Store8).getNumber());
basePtr[addr] = (int8_t)(int32_t)(O3REG(Store8).getNumber());
```

**Key Points:**
- Direct storage at `basePtr[addr]`
- No address alignment needed
- Value truncated to 8-bit signed integer

### Store16

**Purpose:** Store value as 16-bit signed integer.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(Store16, Reg8, Reg8, Reg8)
```

**Operands:**
- `Arg1` (Reg8): Typed array object (JSTypedArrayBase)
- `Arg2` (Reg8): Address/offset in the buffer
- `Arg3` (Reg8): Value to store

**Implementation:**
```cpp
auto *mem = vmcast<JSTypedArrayBase>(O1REG(Store16));
int16_t *basePtr = reinterpret_cast<int16_t *>(mem->begin(runtime));
const uint32_t addr = (uint32_t)(int32_t)(O2REG(Store16).getNumber());
basePtr[addr >> 1] = (int16_t)(int32_t)(O3REG(Store16).getNumber());
```

**Key Points:**
- Address divided by 2 (`addr >> 1`) for 16-bit alignment
- Value truncated to 16-bit signed integer

### Store32

**Purpose:** Store value as 32-bit signed integer.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(Store32, Reg8, Reg8, Reg8)
```

**Operands:**
- `Arg1` (Reg8): Typed array object (JSTypedArrayBase)
- `Arg2` (Reg8): Address/offset in the buffer
- `Arg3` (Reg8): Value to store

**Implementation:**
```cpp
auto *mem = vmcast<JSTypedArrayBase>(O1REG(Store32));
int32_t *basePtr = reinterpret_cast<int32_t *>(mem->begin(runtime));
const uint32_t addr = (uint32_t)(int32_t)(O2REG(Store32).getNumber());
basePtr[addr >> 2] = (int32_t)(O3REG(Store32).getNumber());
```

**Key Points:**
- Address divided by 4 (`addr >> 2`) for 32-bit alignment
- Value truncated to 32-bit signed integer

## JavaScript Usage

These instructions are generated from `__uasm` intrinsic functions:

```javascript
// Setup typed arrays as memory buffers
var buffer = new ArrayBuffer(131072);
var HEAP8 = new Int8Array(buffer);
var HEAP16 = new Int16Array(buffer);
var HEAP32 = new Int32Array(buffer);

// Load operations
var value32 = __uasm.loadu32(HEAP32, addr);  // → Loadu32 instruction
var value16 = __uasm.loadi16(HEAP16, addr);  // → Loadi16 instruction

// Store operations  
__uasm.store8(HEAP8, addr, value);   // → Store8 instruction
__uasm.store16(HEAP16, addr, value); // → Store16 instruction
__uasm.store32(HEAP32, addr, value); // → Store32 instruction
```

## Alignment Summary

| Instruction | Data Type | Alignment | Address Calculation |
|-------------|-----------|-----------|-------------------|
| **Store8**  | 8-bit     | None      | `basePtr[addr]` |
| **Store16** | 16-bit    | 2-byte    | `basePtr[addr >> 1]` |
| **Store32** | 32-bit    | 4-byte    | `basePtr[addr >> 2]` |
| **Loadi16** | 16-bit    | 2-byte    | `basePtr[addr >> 1]` |
| **Loadu32** | 32-bit    | 4-byte    | `basePtr[addr >> 2]` |

## Compilation Requirements

- **Flag:** `-funsafe-intrinsics` must be enabled
- **Build:** `HERMES_RUN_WASM` must be defined
- **Safety:** These instructions bypass JavaScript memory safety
- **Purpose:** WebAssembly and asm.js compatibility

## For Decompilers

When decompiling these instructions back to JavaScript:

- **Store8** `r1, r2, r3` → `__uasm.store8(r1, r2, r3)`
- **Store16** `r1, r2, r3` → `__uasm.store16(r1, r2, r3)`
- **Store32** `r1, r2, r3` → `__uasm.store32(r1, r2, r3)`
- **Loadi16** `r1, r2, r3` → `r1 = __uasm.loadi16(r2, r3)`
- **Loadu32** `r1, r2, r3` → `r1 = __uasm.loadu32(r2, r3)`

Where:
- `r1`/first operand: typed array object (for stores) or destination (for loads)
- `r2`/second operand: address/offset (for stores) or typed array (for loads)
- `r3`/third operand: value to store (for stores) or address/offset (for loads)