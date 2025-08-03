# Property Assignment Instructions (Put*)

This document describes all Put* instructions in Hermes bytecode and how they should be decompiled to JavaScript.

## Overview

Put* instructions handle property assignment and object construction in various forms. Hermes distinguishes between:
- **General property assignment** (following prototype chain)
- **Own property creation** (object/array literal construction)
- **Accessor property definition** (getters/setters)

## General Property Assignment

### PutById / PutByIdLong

**Purpose:** Set an object property by string identifier (normal property assignment).

**Bytecode Definitions:**
```cpp
DEFINE_OPCODE_4(PutById, Reg8, Reg8, UInt8, UInt16)
DEFINE_OPCODE_4(PutByIdLong, Reg8, Reg8, UInt8, UInt32)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Value register
- `UInt8`: Property cache index (for optimization)
- `UInt16/UInt32`: String table index for property name

**Implementation:** Uses `JSObject::putNamed_RJS()` with prototype chain lookup.

**Decompiles to:**
```javascript
// PutById r1, r2, 5, 42  (string table[42] = "name")
obj.name = value;

// Alternative syntax
obj["name"] = value;
```

### TryPutById / TryPutByIdLong

**Purpose:** Set an object property by string identifier, throw if undeclared (used for global variables).

**Bytecode Definitions:**
```cpp
DEFINE_OPCODE_4(TryPutById, Reg8, Reg8, UInt8, UInt16)
DEFINE_OPCODE_4(TryPutByIdLong, Reg8, Reg8, UInt8, UInt32)
```

**Operands:** Same as PutById variants.

**Implementation:** Similar to PutById but throws ReferenceError if property doesn't exist.

**Decompiles to:**
```javascript
// TryPutById global_obj, r1, 0, 15  (string table[15] = "x")
// In strict mode context:
"use strict";
x = value;  // Throws if x is undeclared

// In non-strict mode:
x = value;  // Creates global property
```

### PutByVal

**Purpose:** Set a property by computed value (dynamic property name).

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_3(PutByVal, Reg8, Reg8, Reg8)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Property name register (computed at runtime)
- `Reg8`: Value register

**Implementation:** Uses `JSObject::putComputed_RJS()`.

**Decompiles to:**
```javascript
// PutByVal r1, r2, r3
obj[computed_property] = value;

// Examples:
obj[key] = value;
obj[expr + "_suffix"] = value;
obj[symbolKey] = value;
```

## Own Property Creation

### PutNewOwnByIdShort / PutNewOwnById / PutNewOwnByIdLong

**Purpose:** Create a new enumerable own property on an object (must not already exist).

**Bytecode Definitions:**
```cpp
DEFINE_OPCODE_3(PutNewOwnByIdShort, Reg8, Reg8, UInt8)
DEFINE_OPCODE_3(PutNewOwnById, Reg8, Reg8, UInt16)
DEFINE_OPCODE_3(PutNewOwnByIdLong, Reg8, Reg8, UInt32)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Value register
- `UInt8/UInt16/UInt32`: String table index for property name

**Implementation:** Uses `JSObject::defineNewOwnProperty()` with enumerable flags.

**Decompiles to:**
```javascript
// PutNewOwnById r1, r2, 25  (string table[25] = "prop")
// Usually appears in object literal construction:
let obj = {
    prop: value,  // This becomes PutNewOwnById
    // ...
};

// Or equivalently:
Object.defineProperty(obj, "prop", {
    value: value,
    enumerable: true,
    configurable: true,
    writable: true
});
```

### PutNewOwnNEById / PutNewOwnNEByIdLong

**Purpose:** Create a new non-enumerable own property on an object.

**Bytecode Definitions:**
```cpp
DEFINE_OPCODE_3(PutNewOwnNEById, Reg8, Reg8, UInt16)
DEFINE_OPCODE_3(PutNewOwnNEByIdLong, Reg8, Reg8, UInt32)
```

**Operands:** Same as PutNewOwnById variants.

**Implementation:** Uses `JSObject::defineNewOwnProperty()` with non-enumerable flags.

**Decompiles to:**
```javascript
// PutNewOwnNEById r1, r2, 30  (string table[30] = "method")
// Usually from class method definitions:
class MyClass {
    method() {  // Non-enumerable method
        // ...
    }
}

// Or explicit non-enumerable property:
Object.defineProperty(obj, "method", {
    value: function() { /* ... */ },
    enumerable: false,
    configurable: true,
    writable: true
});
```

### PutOwnByIndex / PutOwnByIndexL

**Purpose:** Set an own property by numeric index (array-like assignment).

**Bytecode Definitions:**
```cpp
DEFINE_OPCODE_3(PutOwnByIndex, Reg8, Reg8, UInt8)
DEFINE_OPCODE_3(PutOwnByIndexL, Reg8, Reg8, UInt32)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Value register
- `UInt8/UInt32`: Numeric index

**Implementation:** Uses `JSObject::defineOwnComputedPrimitive()` with numeric index.

**Decompiles to:**
```javascript
// PutOwnByIndex r1, r2, 0
// PutOwnByIndex r1, r3, 1
// PutOwnByIndex r1, r4, 2
// Usually from array literal construction:
let arr = [value0, value1, value2];

// Or object with numeric properties:
let obj = {
    0: value0,
    1: value1,
    2: value2
};
```

### PutOwnByVal

**Purpose:** Set an own property by computed value with enumeration control.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_4(PutOwnByVal, Reg8, Reg8, Reg8, UInt8)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Value register
- `Reg8`: Property name register (computed)
- `UInt8`: Enumerable flag (0 = non-enumerable, 1 = enumerable)

**Implementation:** Uses `JSObject::defineOwnComputed()` with configurable flags.

**Decompiles to:**
```javascript
// PutOwnByVal r1, r2, r3, 1  (enumerable)
// From computed property in object literal:
let obj = {
    [computed_key]: value,  // Enumerable computed property
    // ...
};

// PutOwnByVal r1, r2, r3, 0  (non-enumerable)
Object.defineProperty(obj, computed_key, {
    value: value,
    enumerable: false,
    configurable: true,
    writable: true
});
```

## Accessor Properties

### PutOwnGetterSetterByVal

**Purpose:** Define getter/setter properties on an object.

**Bytecode Definition:**
```cpp
DEFINE_OPCODE_5(PutOwnGetterSetterByVal, Reg8, Reg8, Reg8, Reg8, UInt8)
```

**Operands:**
- `Reg8`: Object register
- `Reg8`: Property name register
- `Reg8`: Getter function register (or undefined)
- `Reg8`: Setter function register (or undefined)
- `UInt8`: Enumerable flag

**Implementation:** Creates accessor property descriptor with getter/setter functions.

**Decompiles to:**
```javascript
// PutOwnGetterSetterByVal r1, r2, r3, r4, 1
// From object literal with accessor syntax:
let obj = {
    get [computed_prop]() {
        // getter implementation
    },
    set [computed_prop](value) {
        // setter implementation  
    }
};

// Or explicit descriptor:
Object.defineProperty(obj, computed_prop, {
    get: getter_function,
    set: setter_function,
    enumerable: true,
    configurable: true
});
```

## Decompilation Strategy

### Context Recognition

1. **Object Literal Construction**
   - `PutNewOwnById*` → `{property: value}`
   - `PutOwnByIndex*` → `[element0, element1, ...]` or `{0: val, 1: val}`
   - `PutOwnByVal` → `{[computed]: value}`

2. **Property Assignment**
   - `PutById*` → `obj.property = value`
   - `PutByVal` → `obj[computed] = value`
   - `TryPutById*` → Global variable assignment

3. **Special Properties**
   - `PutNewOwnNEById*` → Class methods, non-enumerable properties
   - `PutOwnGetterSetterByVal` → Accessor properties

### Sequential Pattern Recognition

```javascript
// Typical object literal construction pattern:
NewObject r1                        // let obj = {
PutNewOwnById r1, r2, "a"          //     a: value1,
PutNewOwnById r1, r3, "b"          //     b: value2,
PutOwnByVal r1, r4, r5, 1          //     [computed]: value3
                                   // };

// Array literal construction pattern:
NewArray r1, 3                     // let arr = [
PutOwnByIndex r1, r2, 0           //     element0,
PutOwnByIndex r1, r3, 1           //     element1,
PutOwnByIndex r1, r4, 2           //     element2
                                  // ];
```

### String Table Resolution

Most Put* instructions reference string table indices for property names:
- Look up string ID in the bytecode's string table
- Apply proper JavaScript identifier rules vs quoted properties
- Handle escape sequences in string literals

### Property Descriptor Mapping

| Instruction | Enumerable | Configurable | Writable | Get/Set |
|-------------|------------|--------------|----------|---------|
| `PutById*` | N/A (follows prototype) | N/A | N/A | N/A |
| `PutNewOwnById*` | ✓ | ✓ | ✓ | N/A |
| `PutNewOwnNEById*` | ✗ | ✓ | ✓ | N/A |
| `PutOwnByVal` | Flag | ✓ | ✓ | N/A |
| `PutOwnGetterSetterByVal` | Flag | ✓ | N/A | ✓ |

## Error Handling

When decompiling Put* instructions:
1. **Validate string table indices** are within bounds
2. **Handle missing property names** gracefully
3. **Detect invalid register references**
4. **Recognize incomplete object construction** patterns
5. **Preserve enumerable/configurable semantics** where relevant

## Optimization Notes

- **Cache indices** in PutById can be ignored during decompilation
- **Short variants** are optimizations for small string table indices
- **Property order** in object literals may not match original source due to optimization
- **Hidden class optimization** may affect instruction selection but not semantics