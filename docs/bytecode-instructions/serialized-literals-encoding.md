# Serialized Literals Encoding Format

This document describes how serialized literals are encoded in Hermes bytecode, with special attention to edge cases and issues that affect large real-world files.

## Overview

Hermes uses a compact variable-length encoding for array and object literal data stored in buffers. This system allows efficient serialization of static literal values but has several limitations that become problematic in large files with thousands of functions.

## Buffer Structure

### Tag Format

Serialized literals use a tag-based encoding with variable-length sequences:

**Short Sequences (≤15 elements):**
```
Byte 0: 0tttllll
- Bit 7: 0 (indicates short format)
- Bits 6-4: ttt (3-bit type tag)
- Bits 3-0: llll (4-bit length)
```

**Long Sequences (≤4095 elements):**
```
Byte 0: 1tttllll
Byte 1: llllllll
- Bit 7: 1 (indicates long format)
- Bits 6-4: ttt (3-bit type tag)
- Bits 3-0 + Byte 1: 12-bit length (4096 max)
```

### Data Type Tags

From `SerializedLiteralGenerator.h`:

| Tag | Value | Name | Data Size | Purpose |
|-----|-------|------|-----------|---------|
| 0 | `0x00` | NullTag | 0 bytes | null values |
| 1 | `0x10` | TrueTag | 0 bytes | boolean true |
| 2 | `0x20` | FalseTag | 0 bytes | boolean false |
| 3 | `0x30` | NumberTag | 8 bytes | double-precision float |
| 4 | `0x40` | IntegerTag | 4 bytes | 32-bit signed integer |
| 5 | `0x50` | ByteStringTag | 1 byte | string ID ≤ 255 |
| 6 | `0x60` | ShortStringTag | 2 bytes | string ID ≤ 65535 |
| 7 | `0x70` | LongStringTag | 4 bytes | string ID > 65535 |

### Critical Constraint: Maximum Sequence Length

**Hard limit:** 4095 elements per sequence (`SequenceMax = (1 << 12) - 1`)

This means any array literal with more than 4095 elements must be split into multiple sequences, causing:
- Increased encoding overhead
- More complex parsing logic
- Larger buffer sizes in aggregate

## Buffer Access Instructions

### Index Size Variants

**Regular Instructions:**
- `NewArrayWithBuffer`: Uses `UInt16` buffer index (max 65,535)
- `NewObjectWithBuffer`: Uses `UInt16` key/value buffer indices

**Long Instructions:**
- `NewArrayWithBufferLong`: Uses `UInt32` buffer index (max 4,294,967,295)
- `NewObjectWithBufferLong`: Uses `UInt32` key/value buffer indices

### Buffer Index Overflow Issue

**Problem:** Large files easily exceed 65,535 byte buffer sizes, but many tools assume the regular `UInt16` instructions.

**When it happens:**
- 10,000+ functions with many literals
- Cumulative buffer size > 65KB
- Complex object literals with many properties

## Parsing Implementation Details

### Buffer Slicing

```cpp
// From CodeBlock.cpp
SerializedLiteralParser CodeBlock::getArrayBufferIter(uint32_t idx, unsigned int numLiterals) const {
  return SLP{
      runtimeModule_->getBytecode()->getArrayBuffer().slice(idx),
      numLiterals,
      runtimeModule_};
}
```

**Critical Issue:** The `slice(idx)` operation performs NO bounds checking in release builds.

### Tag Parsing Vulnerability

```cpp
// From SerializedLiteralParserBase.cpp
void SerializedLiteralParserBase::parseTagAndSeqLength() {
  auto tag = buffer_[currIdx_];           // No bounds check!
  if (tag & 0x80) {
    leftInSeq_ = ((tag & 0x0f) << 8) | (buffer_[currIdx_ + 1]);  // No bounds check!
    currIdx_ += 2;
  } else {
    leftInSeq_ = tag & 0x0f;
    currIdx_ += 1;
  }
  lastTag_ = tag & SerializedLiteralGenerator::TagMask;
}
```

**Vulnerabilities:**
1. No validation that `currIdx_` is within buffer bounds
2. No check that `currIdx_ + 1` is valid for long format
3. No validation that `leftInSeq_` elements actually exist in buffer

### Data Reading Without Bounds Checking

```cpp
// Data access pattern in parser
const char* dataPtr = buffer_.data() + currIdx_;
// Reads 1, 2, 4, or 8 bytes based on tag type
// NO validation that enough bytes remain!
```

## Large File Failure Scenarios

### 1. Buffer Index Overflow

**Scenario:** File with 50,000 functions, each with small array literals.

**Problem:** Cumulative buffer size exceeds 65,535 bytes, but decompiler assumes `UInt16` indices.

**Symptoms:**
- Wrong buffer offsets calculated
- Reading from incorrect buffer positions
- Corrupted literal values in decompiled output

**Solution:** Always check instruction variant (`NewArrayWithBuffer` vs `NewArrayWithBufferLong`) and use appropriate index size.

### 2. String ID Overflow

**Scenario:** Large application with thousands of unique property names and string literals.

**Problem:** String table exceeds 65,535 entries, forcing `LongStringTag` usage.

**Symptoms:**
- Wrong string IDs read from buffers
- Property names become garbage
- String literals show incorrect values

**String ID Ranges:**
- `≤ 255`: ByteStringTag (1 byte)
- `256-65535`: ShortStringTag (2 bytes)  
- `> 65535`: LongStringTag (4 bytes)

### 3. Sequence Fragmentation

**Scenario:** Array literal with 10,000 elements.

**Problem:** Must be split into multiple 4095-element sequences.

**Encoding:**
```
Tag: Long format, length=4095  // First 4095 elements
[4095 elements of data]
Tag: Long format, length=4095  // Next 4095 elements  
[4095 elements of data]
Tag: Short format, length=10   // Remaining 1810 elements
[1810 elements of data]
```

**Parsing Complexity:**
- Must read multiple sequences for single array
- Track element count across sequence boundaries
- Handle sequence boundaries correctly

### 4. Buffer Corruption Detection

**Common Corruption Patterns:**
- Invalid tag values (bits 6-4 > 7)
- Sequence length extends past buffer end
- Multi-byte values with insufficient remaining bytes
- Malformed long-format tags (missing second byte)

### 5. Endianness Issues

**Problem:** Multi-byte values stored in little-endian format.

**Affected Data:**
- NumberTag: 8-byte doubles
- IntegerTag: 4-byte integers
- ShortStringTag: 2-byte string IDs
- LongStringTag: 4-byte string IDs
- Long format lengths: 2-byte total length

**Special Case:** NaN double values use specific bit pattern `0xfff8000000000000`.

## Robust Decompiler Implementation

### 1. Instruction Variant Detection

```rust
// Always check for Long variants first
match opcode {
    Opcode::NewArrayWithBufferLong => {
        let buffer_idx = read_u32(); // Use 32-bit index
    }
    Opcode::NewArrayWithBuffer => {
        let buffer_idx = read_u16() as u32; // Extend to 32-bit
    }
}
```

### 2. Buffer Bounds Validation

```rust
fn validate_buffer_access(buffer: &[u8], offset: usize, size: usize) -> Result<(), Error> {
    if offset + size > buffer.len() {
        return Err(Error::BufferOverrun { 
            offset, 
            size, 
            buffer_len: buffer.len() 
        });
    }
    Ok(())
}
```

### 3. Safe Tag Parsing

```rust
fn parse_tag_and_length(buffer: &[u8], idx: &mut usize) -> Result<(u8, u16), Error> {
    if *idx >= buffer.len() {
        return Err(Error::UnexpectedEndOfBuffer);
    }
    
    let tag = buffer[*idx];
    let tag_type = (tag & 0x70) >> 4;
    
    if tag_type > 7 {
        return Err(Error::InvalidTagType(tag_type));
    }
    
    let length = if tag & 0x80 != 0 {
        // Long format - need second byte
        if *idx + 1 >= buffer.len() {
            return Err(Error::IncompleteTag);
        }
        let length = ((tag & 0x0f) as u16) << 8 | buffer[*idx + 1] as u16;
        *idx += 2;
        length
    } else {
        // Short format
        let length = (tag & 0x0f) as u16;
        *idx += 1;
        length
    };
    
    Ok((tag_type, length))
}
```

### 4. Sequence Reconstruction

```rust
fn read_array_literals(buffer: &[u8], num_literals: usize) -> Result<Vec<Value>, Error> {
    let mut values = Vec::with_capacity(num_literals);
    let mut idx = 0;
    
    while values.len() < num_literals {
        let (tag_type, seq_length) = parse_tag_and_length(buffer, &mut idx)?;
        
        // Validate sequence doesn't exceed remaining needed elements
        let remaining_needed = num_literals - values.len();
        if seq_length as usize > remaining_needed {
            return Err(Error::SequenceTooLong { 
                seq_length: seq_length as usize, 
                remaining_needed 
            });
        }
        
        // Read sequence data based on tag type
        for _ in 0..seq_length {
            let value = read_value_by_tag(buffer, &mut idx, tag_type)?;
            values.push(value);
        }
    }
    
    Ok(values)
}
```

### 5. Error Recovery Strategies

For corrupted or malformed buffers:

1. **Graceful Degradation:** Return placeholder values instead of crashing
2. **Skip Corrupted Sequences:** Continue parsing after errors when possible  
3. **Buffer Truncation Detection:** Detect when buffers end unexpectedly
4. **Validation Checksums:** If available, verify buffer integrity

### 6. Performance Considerations

**For Large Files:**
- Pre-validate entire buffer structure before parsing
- Cache string table for repeated lookups
- Use memory-mapped files for very large bytecode
- Implement incremental parsing for memory efficiency

## Testing Edge Cases

### Required Test Cases

1. **Buffer index at UInt16 boundary** (exactly 65,535)
2. **String table with 65,536 entries** (forcing LongStringTag)
3. **Array with exactly 4,095 elements** (sequence boundary)
4. **Array with 4,096 elements** (requires multiple sequences)
5. **Truncated buffers** at various points
6. **Invalid tag types** (values 8-15)
7. **Corrupted length fields** (extending past buffer)
8. **Mixed endianness data** for cross-platform compatibility

### Real-World Test Files

- Large React Native applications
- Webpack-bundled JavaScript with thousands of modules
- Minified code with extensive use of object/array literals
- Files with internationalization strings (large string tables)

The key insight is that Hermes optimizes for performance in the common case (well-formed bytecode) but provides minimal error checking, making robust decompiler implementation critical for handling real-world edge cases.