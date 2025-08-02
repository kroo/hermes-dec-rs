# Hermes Bytecode Instructions Reference

This directory contains detailed documentation for Hermes VM bytecode instructions, organized by category.

## Documentation Files

### [Environment Instructions](./environment-instructions.md)
- `CreateEnvironment` - Function-level environment creation
- `CreateInnerEnvironment` - Nested scope environment creation
- Environment management and scoping concepts

### [Memory Instructions](./memory-instructions.md)
- `Store8`, `Store16`, `Store32` - Memory store operations
- `Loadi16`, `Loadu32` - Memory load operations  
- WebAssembly/asm.js unsafe memory access
- Typed array manipulation

## Quick Reference

### Environment Creation
| Instruction | Purpose | Operands |
|-------------|---------|----------|
| `CreateEnvironment` | Function top-level environment | `dest_reg` |
| `CreateInnerEnvironment` | Nested environment | `dest_reg, parent_reg, slot_count` |

### Memory Access (Unsafe Intrinsics)
| Instruction | Purpose | Operands | Alignment |
|-------------|---------|----------|-----------|
| `Store8` | Store 8-bit value | `array_reg, addr_reg, value_reg` | None |
| `Store16` | Store 16-bit value | `array_reg, addr_reg, value_reg` | 2-byte |
| `Store32` | Store 32-bit value | `array_reg, addr_reg, value_reg` | 4-byte |
| `Loadi16` | Load signed 16-bit | `dest_reg, array_reg, addr_reg` | 2-byte |
| `Loadu32` | Load unsigned 32-bit | `dest_reg, array_reg, addr_reg` | 4-byte |

## Compilation Notes

- **Memory instructions** require `-funsafe-intrinsics` flag and `HERMES_RUN_WASM` build option
- **Environment instructions** are always available in standard Hermes builds
- All instructions use register-based operands (Reg8 format)

## For Decompiler Developers

These documentation files provide:
- Exact bytecode definitions from Hermes source
- Implementation details from VM interpreter
- JavaScript usage patterns that generate these instructions  
- Operand formats and semantics
- Alignment and safety considerations

When implementing decompilation for these instructions, refer to the specific files for detailed operand meanings and reconstruction patterns.