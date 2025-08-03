# Object and Array Creation Instructions

This document describes the NewObject* and NewArray* instructions in Hermes bytecode and how they should be decompiled to JavaScript.

## Overview

Hermes optimizes object and array creation by using different instructions for various scenarios:
- Empty objects/arrays
- Objects with specific prototypes
- Objects/arrays with static literal data (using buffers)

## NewObject* Instructions

### NewObject

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_1(NewObject, Reg8)
```

**Operands:**
- `Reg8`: Destination register

**Purpose:** Creates a new, empty object using the built-in constructor.

**Implementation:** `JSObject::create(runtime).getHermesValue()`

**Decompiles to:**
```javascript
// NewObject r1
let obj = {};
```

### NewObjectWithParent

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(NewObjectWithParent, Reg8, Reg8)
```

**Operands:**
- `Reg8`: Destination register
- `Reg8`: Parent prototype register

**Purpose:** Creates a new empty object with a specified parent prototype.

**Implementation:** Uses the parent if it's an object, null if parent is null, or Object.prototype otherwise.

**Decompiles to:**
```javascript
// NewObjectWithParent r1, r2
let obj = Object.create(parent);

// Or in some cases:
let obj = {};
obj.__proto__ = parent;
```

### NewObjectWithBuffer

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_5(NewObjectWithBuffer, Reg8, UInt16, UInt16, UInt16, UInt16)
```

**Operands:**
- `Reg8`: Destination register
- `UInt16`: Size hint (preallocation hint)
- `UInt16`: Number of literal properties
- `UInt16`: Key buffer index
- `UInt16`: Value buffer index

**Purpose:** Creates an object from static property data stored in buffers.

**Decompiles to:**
```javascript
// NewObjectWithBuffer r1, 7, 7, 0, 0
// (reads 7 properties from key/value buffers starting at index 0)
let obj = {
    a: "hello",
    b: 1,
    c: true,
    d: null,
    e: false,
    f: "world",
    g: 2
};
```

### NewObjectWithBufferLong

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_5(NewObjectWithBufferLong, Reg8, UInt16, UInt16, UInt32, UInt32)
```

**Operands:** Same as NewObjectWithBuffer but with 32-bit buffer indices.

**Purpose:** Used when buffer indices exceed 16-bit range (>65535).

**Decompiles to:** Same as NewObjectWithBuffer.

## NewArray* Instructions

### NewArray

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_2(NewArray, Reg8, UInt16)
```

**Operands:**
- `Reg8`: Destination register
- `UInt16`: Size hint

**Purpose:** Creates a new array with a given size hint.

**Implementation:** `JSArray::create(runtime, size, size)`

**Decompiles to:**
```javascript
// NewArray r1, 0
let arr = [];

// NewArray r1, 5
let arr = new Array(5);

// NewArray r1, 10
let arr = new Array(10);
```

### NewArrayWithBuffer

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_4(NewArrayWithBuffer, Reg8, UInt16, UInt16, UInt16)
```

**Operands:**
- `Reg8`: Destination register
- `UInt16`: Number of elements (size hint)
- `UInt16`: Number of literal elements
- `UInt16`: Array buffer index

**Purpose:** Creates an array from static element data stored in buffers.

**Decompiles to:**
```javascript
// NewArrayWithBuffer r1, 4, 4, 0
// (reads 4 elements from array buffer starting at index 0)
let arr = [1, "hello", true, null];
```

### NewArrayWithBufferLong

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_4(NewArrayWithBufferLong, Reg8, UInt16, UInt16, UInt32)
```

**Operands:** Same as NewArrayWithBuffer but with 32-bit buffer index.

**Purpose:** Used when buffer indices exceed 16-bit range.

**Decompiles to:** Same as NewArrayWithBuffer.

## Buffer System

### Object Buffers

The buffer system efficiently stores static literal data:

**Key Buffer:** Stores property names (strings, symbols)
**Value Buffer:** Stores property values (numbers, strings, booleans, null)

Example:
```javascript
// Original JavaScript
{a: 1, b: "hello", c: true}

// Buffer contents:
// Key buffer:   ["a", "b", "c"]
// Value buffer: [1, "hello", true]
```

### Array Buffers

**Array Buffer:** Stores array element values directly.

Example:
```javascript
// Original JavaScript
[42, "test", false, null]

// Buffer contents:
// Array buffer: [42, "test", false, null]
```

### Buffer Limitations

Buffers can only store **serializable literal values:**
- Numbers, strings, booleans, null
- **Cannot store:** functions, undefined (in some contexts), complex objects

For non-serializable values, Hermes uses **null placeholders** in buffers and sets them separately with other instructions.

## Decompilation Process

### Step 1: Identify Instruction Type

```javascript
NewObject                    → {}
NewObjectWithParent         → Object.create(parent)
NewObjectWithBuffer         → {key1: val1, key2: val2, ...}
NewArray                    → [] or new Array(size)
NewArrayWithBuffer          → [val1, val2, val3, ...]
```

### Step 2: For Buffer Instructions

1. **Read buffer indices** from instruction operands
2. **Look up values** in the corresponding buffers (string, value, array)
3. **Reconstruct literal** by pairing keys with values
4. **Handle placeholders** - null values may indicate non-serializable data set by subsequent instructions

### Step 3: Handle Subsequent Instructions

Often followed by instructions that set non-serializable properties:

```javascript
// Bytecode sequence:
NewObjectWithBuffer r1, 3, 3, 0, 0  // Creates {a: 1, b: "hello", c: null}
CreateClosure r2, r0, Function1     // Creates a function
PutById r1, r2, 5, "c"              // Sets obj.c = function

// Decompiles to:
let obj = {
    a: 1,
    b: "hello", 
    c: function() { /* ... */ }  // Function set separately
};
```

## Optimization Recognition

### When Hermes Uses Each Instruction

1. **NewObject**: Empty object literals `{}`
2. **NewObjectWithParent**: `Object.create()` calls, `__proto__` assignments
3. **NewObjectWithBuffer**: Object literals with static properties
4. **NewArray**: Empty arrays `[]` or `new Array(size)`
5. **NewArrayWithBuffer**: Array literals with static elements

### Buffer Size Limits

- **Regular variants**: Use 16-bit indices (up to 65535 buffer entries)
- **Long variants**: Use 32-bit indices (for larger buffers)

## Implementation Notes

1. **Hidden Classes**: `NewObjectWithBuffer` uses cached hidden classes for performance
2. **Preallocation**: Size hints optimize memory allocation
3. **Buffer Iteration**: Uses iterators to efficiently read buffer data
4. **Null Handling**: Null placeholders in buffers indicate values set by other instructions

## Common Decompilation Patterns

### Simple Cases
```javascript
NewObject r1              → let obj = {};
NewArray r1, 0           → let arr = [];
```

### Buffer-Based Literals
```javascript
NewObjectWithBuffer r1, 2, 2, 0, 0    → let obj = {key1: val1, key2: val2};
NewArrayWithBuffer r1, 3, 3, 0        → let arr = [val1, val2, val3];
```

### With Subsequent Modifications
```javascript
NewObjectWithBuffer r1, 1, 1, 0, 0    → let obj = {a: 1,
PutById r1, r2, 8, "b"                →          b: someValue};
```

### Prototype-Based Creation
```javascript
NewObjectWithParent r1, r2            → let obj = Object.create(parent);
```

## Error Handling

When decompiling buffer-based instructions:
1. **Validate buffer indices** are within bounds
2. **Handle missing buffer data** gracefully
3. **Detect buffer corruption** or inconsistencies
4. **Fallback to generic representations** if buffer data is unavailable