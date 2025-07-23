# HBC Instructions Auto-Generation Plan

## Overview

This document outlines a robust, concise, and clean system for automatically generating Rust instruction types for all Hermes bytecode versions from v51 to the latest (currently v96+). The system will be based on insights from three existing implementations:

1. **hermes-dec** - Python-based parser that fetches source files from Hermes Git repo
2. **hermes_rs** - JavaScript-based macro generator with manual .def files
3. **hermes-dec-rust** - Rust build script with regex-based parsing

## Key Compatibility Requirements

### Scroll Integration
- **Zero-copy parsing**: Generated instructions must use scroll's `gread` methods
- **Interface compatibility**: Maintain existing `Instruction` struct interface
- **Function table integration**: Work seamlessly with `FunctionTable::parse`
- **Error handling**: Integrate with existing `DecompilerError` system
- **Version-specific parsing**: Handle different operand formats across versions

### Architecture Integration
- **HBC library compatibility**: Generated code must work with existing `HbcFile::parse`
- **CLI command support**: All existing commands (`inspect`, `disasm`, `decompile`) must work
- **Performance**: Maintain zero-copy parsing performance characteristics
- **Memory efficiency**: Minimize allocations during parsing

## Key Insights from Investigation

### hermes-dec Approach
- **Source**: Fetches `BytecodeList.def` and `BytecodeVersion.h` files directly from Hermes Git tags
- **Mapping**: Maintains a comprehensive mapping of Git tags to HBC versions
- **Process**: Downloads source files → parses with Python → generates tables
- **Strengths**: Always up-to-date, comprehensive version coverage
- **Weaknesses**: Complex Python parsing, external dependencies

### hermes_rs Approach
- **Source**: Manual .def files stored in repository
- **Mapping**: JavaScript script generates Rust macros from .def files
- **Process**: Parse .def → generate macro invocations → expand at compile time
- **Strengths**: Clean macro-based approach, type-safe
- **Weaknesses**: Manual file maintenance, limited version coverage

### hermes-dec-rust Approach
- **Source**: Build script with regex parsing of .def files
- **Mapping**: Hardcoded version list with path resolution
- **Process**: Parse .def → generate HashMap → runtime lookup
- **Strengths**: Rust-native, build-time generation
- **Weaknesses**: Fragile regex parsing, limited error handling

## Proposed System Design

### 1. Architecture Overview

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Git Fetcher   │───▶│  Parser/Generator │───▶│  Rust Output    │
│                 │    │                  │    │                 │
│ - Fetch .def    │    │ - Parse macros   │    │ - Enum variants │
│ - Fetch .h      │    │ - Extract types  │    │ - From<u8> impl │
│ - Version map   │    │ - Generate Rust  │    │ - Documentation │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

### 2. Core Components

#### A. Version Management (`src/hbc/instructions/versions.rs`)
```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HbcVersion {
    pub version: u32,
    pub git_tag: String,
    pub commit_hash: Option<String>,
    pub release_date: Option<String>,
}

pub struct VersionRegistry {
    versions: HashMap<u32, HbcVersion>,
    min_version: u32,
    max_version: u32,
}
```

#### B. Source Fetcher (`src/hbc/instructions/fetcher.rs`)
```rust
pub struct SourceFetcher {
    cache_dir: PathBuf,
    hermes_repo: String,
}

impl SourceFetcher {
    pub async fn fetch_version(&self, version: &HbcVersion) -> Result<SourceFiles, Error>;
    pub async fn fetch_all_versions(&self, registry: &VersionRegistry) -> Result<(), Error>;
}
```

#### C. Parser (`src/hbc/instructions/parser.rs`)
```rust
#[derive(Debug, Clone)]
pub struct InstructionDef {
    pub name: String,
    pub opcode: u8,
    pub operands: Vec<OperandType>,
    pub has_ret_target: bool,
    pub documentation: String,
}

#[derive(Debug, Clone)]
pub enum OperandType {
    Reg8,
    Reg32,
    UInt8,
    UInt16,
    UInt32,
    Addr8,
    Addr32,
    Imm32,
    Double,
    StringId,
    BigIntId,
    FunctionId,
}

pub struct DefParser {
    // Parse BytecodeList.def files
    pub fn parse_def_file(&self, content: &str) -> Result<Vec<InstructionDef>, Error>;
}
```

#### D. Code Generator (`src/hbc/instructions/generator.rs`)
```rust
pub struct RustGenerator {
    template_engine: TemplateEngine,
}

impl RustGenerator {
    pub fn generate_enum(&self, instructions: &[InstructionDef]) -> String;
    pub fn generate_from_impl(&self, instructions: &[InstructionDef]) -> String;
    pub fn generate_documentation(&self, instructions: &[InstructionDef]) -> String;
}
```

### 3. Build System Integration

#### A. Build Script (`src/hbc/instructions/build.rs`)
```rust
fn main() {
    // 1. Load version registry
    let registry = VersionRegistry::load()?;
    
    // 2. Fetch source files (with caching)
    let fetcher = SourceFetcher::new()?;
    fetcher.fetch_all_versions(&registry).await?;
    
    // 3. Parse all versions
    let parser = DefParser::new();
    let mut all_instructions = HashMap::new();
    
    for version in registry.versions() {
        let def_content = fs::read_to_string(version.def_path())?;
        let instructions = parser.parse_def_file(&def_content)?;
        all_instructions.insert(version.version, instructions);
    }
    
    // 4. Generate Rust code
    let generator = RustGenerator::new();
    let output = generator.generate_all_versions(&all_instructions)?;
    
    // 5. Write to OUT_DIR
    let out_dir = env::var("OUT_DIR")?;
    fs::write(out_dir.join("instructions.rs"), output)?;
    
    // 6. Register for rebuilds
    for version in registry.versions() {
        println!("cargo:rerun-if-changed={}", version.def_path().display());
    }
}
```

#### B. Generated Code Structure
```rust
// Auto-generated file: src/hbc/instructions/generated.rs
#![allow(dead_code)]

use serde::{Deserialize, Serialize};

/// Hermes Bytecode Instructions
/// 
/// Auto-generated from Hermes source code for versions 51-96+
/// Generated on: 2024-01-XX
/// Source: https://github.com/facebook/hermes
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Opcode {
    // Version 51 instructions
    #[cfg(feature = "hbc51")]
    Unreachable = 0x00,
    #[cfg(feature = "hbc51")]
    LoadConstString = 0x01,
    // ... more instructions
    
    // Version 95+ instructions  
    #[cfg(feature = "hbc95")]
    NewObjectWithBuffer = 0x01,
    #[cfg(feature = "hbc95")]
    NewObjectWithBufferLong = 0x02,
    // ... more instructions
    
    // Unknown opcode for forward compatibility
    Unknown(u8),
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0x00 => Opcode::Unreachable,
            0x01 => {
                #[cfg(feature = "hbc95")]
                return Opcode::NewObjectWithBuffer;
                #[cfg(not(feature = "hbc95"))]
                return Opcode::LoadConstString;
            }
            // ... more mappings
            _ => Opcode::Unknown(value),
        }
    }
}

/// Instruction metadata for each version
pub struct InstructionInfo {
    pub name: &'static str,
    pub operands: &'static [OperandType],
    pub has_ret_target: bool,
    pub documentation: &'static str,
}

pub static INSTRUCTION_INFO: &[InstructionInfo] = &[
    InstructionInfo {
        name: "Unreachable",
        operands: &[],
        has_ret_target: false,
        documentation: "Unreachable opcode for stubs and similar",
    },
    // ... more instructions
];
```

### 4. Version Registry Management

#### A. Registry File (`src/hbc/instructions/versions.json`)
```json
{
  "min_version": 51,
  "max_version": 96,
  "versions": [
    {
      "version": 51,
      "git_tag": "v0.0.1",
      "commit_hash": null,
      "release_date": "2019-07-17"
    },
    {
      "version": 95,
      "git_tag": "f6b56d3",
      "commit_hash": "f6b56d3",
      "release_date": "2023-03-29"
    }
  ]
}
```

#### B. Update Script (`scripts/update_versions.sh`)
```bash
#!/bin/bash
# Script to update version registry from Hermes releases

# Fetch latest tags from Hermes repo
git -C /tmp/hermes fetch --tags

# Parse version mapping from hermes-dec git_tags.sh
# Generate updated versions.json
# Commit changes if any
```

### 5. Implementation Phases

#### Phase 1: Core Infrastructure
1. Create version registry system
2. Implement source fetcher with caching
3. Build basic parser for .def files
4. Create simple code generator

#### Phase 2: Full Parser
1. Implement comprehensive .def file parsing
2. Handle all operand types and annotations
3. Parse documentation comments
4. Add error handling and validation

#### Phase 3: Advanced Features
1. Implement conditional compilation with features
2. Add version compatibility checking
3. Create instruction metadata system
4. Add comprehensive tests

#### Phase 4: Tooling
1. Create CLI tool for manual updates
2. Add CI/CD integration
3. Create documentation generator
4. Add performance benchmarks

### 6. Benefits of This Approach

#### A. Robustness
- **Version Coverage**: Supports all Hermes versions from 51 to latest
- **Error Handling**: Comprehensive error reporting and recovery
- **Validation**: Type safety and consistency checks
- **Caching**: Efficient source file management

#### B. Maintainability
- **Auto-updating**: Can automatically fetch new versions
- **Clean Separation**: Clear separation of concerns
- **Documentation**: Auto-generated documentation
- **Testing**: Comprehensive test coverage

#### C. Performance
- **Compile-time**: All generation happens at build time
- **Feature Flags**: Conditional compilation for version-specific code
- **Zero-cost**: No runtime overhead for instruction lookup
- **Caching**: Efficient caching of downloaded sources

### 7. Migration Strategy

#### A. Immediate (Current State)
- Keep existing manual `Opcode` enum
- Add build script infrastructure
- Start with a few key versions (80, 90, 95)

#### B. Short-term (Next Sprint)
- Implement basic fetcher and parser
- Generate instructions for versions 80-95
- Add feature flags for version selection

#### C. Medium-term (Next Month)
- Full version coverage (51-96+)
- Advanced parsing features
- Comprehensive testing

## Scroll Integration & Zero-Copy Parsing Compatibility

### 8. Critical Design Requirements

The generated instruction types must be fully compatible with the existing scroll-based parsing architecture:

#### A. Instruction Structure Compatibility
```rust
// Current structure in src/hbc/instructions.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instruction {
    pub pc: u32,
    pub opcode: Opcode,
    pub args: Vec<u32>,
}

// Generated structure must maintain this interface
// But with enhanced opcode enum and metadata
```

#### B. Scroll Pread Integration
The generated code must support zero-copy parsing through scroll's `Pread` trait:

```rust
// Generated instruction parsing functions
impl Instruction {
    /// Parse instruction from byte slice using scroll
    pub fn parse_from_bytes(data: &[u8], offset: &mut usize, version: u32) -> Result<Self, scroll::Error> {
        // Parse opcode
        let opcode_byte: u8 = data.gread(offset)?;
        let opcode = Opcode::from(opcode_byte);
        
        // Parse operands based on instruction metadata
        let operand_count = get_operand_count(opcode, version);
        let mut args = Vec::with_capacity(operand_count);
        
        for _ in 0..operand_count {
            let arg: u32 = data.gread(offset)?;
            args.push(arg);
        }
        
        Ok(Instruction {
            pc: 0, // Will be set by caller
            opcode,
            args,
        })
    }
    
    /// Get instruction size for scroll parsing
    pub fn size(opcode: Opcode, version: u32) -> usize {
        let operand_count = get_operand_count(opcode, version);
        1 + operand_count * 4 // 1 byte opcode + 4 bytes per operand
    }
}
```

#### C. Function Table Integration
The generated instructions must work seamlessly with the existing `FunctionTable::parse` method:

```rust
// In src/hbc/tables.rs - FunctionTable implementation
impl FunctionTable {
    pub fn parse(data: &[u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        // Parse function count
        let function_count: u32 = data.gread(offset)
            .map_err(|e| format!("Failed to parse function count: {}", e))?;
        
        let mut functions = Vec::with_capacity(function_count as usize);
        
        for _ in 0..function_count {
            // Parse function header
            let name_id: u32 = data.gread(offset)?;
            let flags: u32 = data.gread(offset)?;
            let param_count: u32 = data.gread(offset)?;
            let env_size: u32 = data.gread(offset)?;
            let instruction_count: u32 = data.gread(offset)?;
            
            // Parse instructions using generated parsing logic
            let mut instructions = Vec::with_capacity(instruction_count as usize);
            let mut pc = 0;
            
            for _ in 0..instruction_count {
                let instruction = Instruction::parse_from_bytes(data, offset, header.version)
                    .map_err(|e| format!("Failed to parse instruction at PC {}: {}", pc, e))?;
                
                instructions.push(Instruction {
                    pc,
                    opcode: instruction.opcode,
                    args: instruction.args,
                });
                
                pc += Instruction::size(instruction.opcode, header.version) as u32;
            }
            
            functions.push(HbcFunction {
                name: format!("function_{}", name_id), // Will be resolved from string table
                flags,
                param_count,
                env_size,
                instruction_count,
                instructions,
            });
        }
        
        Ok(FunctionTable { functions })
    }
}
```

#### D. Version-Specific Parsing
The generated code must handle version-specific instruction formats:

```rust
// Generated version-specific parsing logic
pub fn get_operand_count(opcode: Opcode, version: u32) -> usize {
    match (opcode, version) {
        // Version-specific operand counts
        (Opcode::LoadConstString, 51..=94) => 1,
        (Opcode::LoadConstString, 95..) => 2, // New format in v95+
        (Opcode::NewObjectWithBuffer, 95..) => 3,
        // ... more version-specific mappings
        _ => {
            // Fallback to metadata lookup
            INSTRUCTION_INFO.iter()
                .find(|info| info.name == opcode_name(opcode))
                .map(|info| info.operands.len())
                .unwrap_or(0)
        }
    }
}

pub fn opcode_name(opcode: Opcode) -> &'static str {
    match opcode {
        Opcode::Unreachable => "Unreachable",
        Opcode::LoadConstString => "LoadConstString",
        // ... generated mappings
        Opcode::Unknown(_) => "Unknown",
    }
}
```

#### E. Error Handling Integration
Generated parsing must integrate with the existing error handling system:

```rust
// Generated error types compatible with DecompilerError
#[derive(Debug, thiserror::Error)]
pub enum InstructionParseError {
    #[error("Unknown opcode {opcode} at PC {pc}")]
    UnknownOpcode { opcode: u8, pc: u32 },
    
    #[error("Invalid operand count for {opcode} at PC {pc}: expected {expected}, got {got}")]
    InvalidOperandCount { opcode: Opcode, pc: u32, expected: usize, got: usize },
    
    #[error("Unsupported instruction {opcode} for HBC version {version}")]
    UnsupportedInstruction { opcode: Opcode, version: u32 },
}

// Integration with existing error system
impl From<InstructionParseError> for crate::DecompilerError {
    fn from(err: InstructionParseError) -> Self {
        match err {
            InstructionParseError::UnknownOpcode { opcode, pc } => {
                crate::DecompilerError::UnknownOpcode { opcode, pc }
            }
            InstructionParseError::InvalidOperandCount { opcode, pc, expected, got } => {
                crate::DecompilerError::InvalidArgs {
                    message: format!("Invalid operand count for {:?}: expected {}, got {}", opcode, expected, got)
                }
            }
            InstructionParseError::UnsupportedInstruction { opcode, version } => {
                crate::DecompilerError::UnsupportedVersion { version }
            }
        }
    }
}
```

### 9. Performance Considerations

#### A. Zero-Copy Parsing
- Generated instruction parsing must use scroll's `gread` methods for zero-copy access
- Avoid unnecessary allocations during parsing
- Use `&[u8]` slices directly where possible

#### B. Memory Efficiency
- Instruction metadata should be static and shared across threads
- Use `&'static str` for instruction names and documentation
- Minimize heap allocations during parsing

#### C. Compile-Time Optimization
- Generated code should be optimized by the Rust compiler
- Use const generics where possible for version-specific logic
- Leverage LLVM's optimization passes

### 10. Testing Strategy

#### A. Unit Tests
- Test generated instruction parsing against known bytecode
- Verify version-specific instruction formats
- Test error handling for malformed bytecode

#### B. Integration Tests
- Test full HBC file parsing with generated instructions
- Verify compatibility with existing CLI commands
- Test performance with real-world bytecode files

#### C. Regression Tests
- Maintain test corpus of bytecode files from different Hermes versions
- Ensure parsing accuracy across version boundaries
- Test forward compatibility with unknown opcodes

### 11. Migration Path

#### A. Phase 1: Infrastructure (Current)
- Set up build script and generation infrastructure
- Keep existing manual `Opcode` enum as fallback
- Add generated instructions alongside existing ones

#### B. Phase 2: Integration (Next Sprint)
- Replace manual opcode parsing with generated versions
- Update `FunctionTable::parse` to use generated parsing
- Add version-specific instruction handling

#### C. Phase 3: Optimization (Next Month)
- Optimize generated code for performance
- Add comprehensive error handling
- Implement advanced version compatibility features

This design ensures that the generated instruction types are fully compatible with the existing scroll-based parsing architecture while providing the flexibility to handle all Hermes bytecode versions efficiently and accurately. 