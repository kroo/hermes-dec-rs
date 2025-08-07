# Control Flow AST Generation Implementation Plan

## Phase 1: Shared Infrastructure (1-2 days)

### 1.1 Create Base Control Flow Module
```rust
// src/ast/control_flow/mod.rs
pub mod region_analyzer;
pub mod block_sequencer;
pub mod control_flow_converter;

pub use region_analyzer::RegionAnalyzer;
pub use block_sequencer::BlockSequencer;
pub use control_flow_converter::{ControlFlowConverter, ControlFlowContext};
```

### 1.2 Region Analyzer Implementation
Key responsibilities:
- Find all blocks within a control structure region
- Identify entry, body, and exit blocks
- Detect break/continue targets
- Handle nested regions

### 1.3 Block Sequencer Implementation
Key responsibilities:
- Order blocks in execution sequence
- Handle fall-through logic
- Manage jump targets within region
- Support partial block processing

### 1.4 Enhanced Expression Builder
Additions to existing InstructionConverter:
- Compound condition building (AND/OR chains)
- Loop condition extraction
- Switch discriminant extraction
- Increment/decrement pattern detection

## Phase 2: Control Structure Converters (3-4 days)

### 2.1 Switch Statement Converter (AST-06)
```rust
// src/ast/switch_converter.rs
pub struct SwitchConverter<'a> {
    ast_builder: &'a AstBuilder<'a>,
    region_analyzer: RegionAnalyzer<'a>,
    block_sequencer: BlockSequencer<'a>,
}

impl<'a> SwitchConverter<'a> {
    pub fn convert_switch_region(
        &mut self,
        dispatch_block: NodeIndex,
        switch_info: &SwitchInfo,
    ) -> Result<Statement<'a>, String> {
        // 1. Extract discriminant expression
        // 2. Build case statements
        // 3. Handle fall-through logic
        // 4. Generate switch statement
    }
}
```

### 2.2 Loop Statement Converter (AST-07)
```rust
// src/ast/loop_converter.rs
pub struct LoopConverter<'a> {
    ast_builder: &'a AstBuilder<'a>,
    region_analyzer: RegionAnalyzer<'a>,
    block_sequencer: BlockSequencer<'a>,
}

impl<'a> LoopConverter<'a> {
    pub fn convert_loop(
        &mut self,
        loop_info: &NaturalLoop,
    ) -> Result<Statement<'a>, String> {
        // 1. Classify loop type (while/for/do-while)
        // 2. Extract loop condition
        // 3. Process loop body
        // 4. Handle break/continue
    }
}
```

### 2.3 Try-Catch Converter (AST-08)
```rust
// src/ast/try_catch_converter.rs
pub struct TryCatchConverter<'a> {
    ast_builder: &'a AstBuilder<'a>,
    region_analyzer: RegionAnalyzer<'a>,
    block_sequencer: BlockSequencer<'a>,
}

impl<'a> TryCatchConverter<'a> {
    pub fn convert_try_catch(
        &mut self,
        try_block: NodeIndex,
        catch_info: &CatchInfo,
    ) -> Result<Statement<'a>, String> {
        // 1. Process try block
        // 2. Build catch clause
        // 3. Handle finally block if present
        // 4. Generate try statement
    }
}
```

## Phase 3: Integration (1 day)

### 3.1 Update BlockToStatementConverter
```rust
impl<'a> BlockToStatementConverter<'a> {
    pub fn convert_block(&mut self, block_idx: NodeIndex) -> Result<AllocVec<'a, Statement<'a>>, String> {
        // Check for control structures in order:
        // 1. Try-catch regions (highest priority)
        // 2. Switch regions
        // 3. Loop regions
        // 4. Conditional chains (existing)
        // 5. Basic blocks (fallback)
    }
}
```

### 3.2 Pattern Matching Priority
1. **Try-Catch**: Must be detected first (exception handlers override other patterns)
2. **Switches**: Check for switch dispatch patterns
3. **Loops**: Detect natural loops
4. **Conditionals**: Existing if-else chains
5. **Sequential**: Default block processing

## Testing Strategy

### Shared Infrastructure Tests
- Test RegionAnalyzer with various CFG patterns
- Test BlockSequencer with complex jump patterns
- Test expression building with compound conditions

### Individual Converter Tests
- **Switch**: Test with dense/sparse switches, fall-through
- **Loops**: Test while/for/do-while, nested loops, break/continue
- **Try-Catch**: Test simple try-catch, nested handlers, finally blocks

### Integration Tests
- Test with complex_control_flow.hbc
- Test nested control structures
- Test all combinations of control structures

## Benefits of This Approach

1. **Incremental Development**: Each phase builds on the previous
2. **Parallel Work**: Once Phase 1 is done, converters can be built in parallel
3. **Testable Components**: Each piece can be tested independently
4. **Clear Interfaces**: Well-defined responsibilities for each component
5. **Reusable Patterns**: Shared logic reduces bugs and maintenance

## Estimated Timeline

- **Phase 1**: 1-2 days (shared infrastructure)
- **Phase 2**: 3-4 days (1-2 days per converter, can parallelize)
- **Phase 3**: 1 day (integration and testing)
- **Total**: 5-7 days for all three control structures

This is much faster than implementing each separately (which would take ~15 days).