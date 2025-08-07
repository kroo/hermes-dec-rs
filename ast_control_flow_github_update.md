# Control Flow AST Generation - Shared Infrastructure Approach

## Summary

After analyzing AST-06 (switches), AST-07 (loops), and AST-08 (try-catch), I've identified significant shared patterns and created a unified infrastructure approach that will:

1. **Reduce implementation time** from ~15 days to ~5-7 days
2. **Ensure consistency** across all control flow structures
3. **Enable parallel development** after the shared infrastructure is complete
4. **Simplify testing and maintenance**

## Shared Infrastructure Components

### 1. `RegionAnalyzer` (src/ast/control_flow/region_analyzer.rs)
- Identifies blocks belonging to control flow regions
- Detects break/continue targets
- Finds region entry/exit points
- Handles nested structures

### 2. `BlockSequencer` (src/ast/control_flow/block_sequencer.rs)
- Orders blocks in execution sequence
- Determines when break/continue statements are needed
- Handles fall-through logic
- Manages partial block processing

### 3. `ControlFlowConverter` trait (src/ast/control_flow/control_flow_converter.rs)
- Common interface for all control flow converters
- Shared methods for:
  - Block-to-statement conversion
  - Condition expression building
  - Break/continue statement generation
  - Control flow analysis

### 4. Enhanced `InstructionConverter`
- Compound condition building (for complex loop/switch conditions)
- Increment expression extraction (for loops)
- Switch discriminant extraction
- Pattern recognition for control flow idioms

## Implementation Plan

### Phase 1: Shared Infrastructure (1-2 days) ✅ STARTED
- [x] Create control_flow module structure
- [x] Implement base traits and types
- [ ] Complete RegionAnalyzer implementation
- [ ] Complete BlockSequencer implementation
- [ ] Enhance InstructionConverter with control flow methods

### Phase 2: Individual Converters (3-4 days)
Can be done in parallel once Phase 1 is complete:

#### AST-06: SwitchConverter
- Use RegionAnalyzer to identify switch regions
- Use BlockSequencer to order cases
- Handle fall-through with break statement insertion
- Support both dense and sparse switches

#### AST-07: LoopConverter  
- Use RegionAnalyzer to identify loop boundaries
- Classify loop types (while/for/do-while)
- Use BlockSequencer for loop body ordering
- Handle break/continue with proper labeling

#### AST-08: TryCatchConverter
- Use RegionAnalyzer for try/catch/finally regions
- Handle exception variable binding
- Support nested try-catch blocks
- Manage control flow through finally blocks

### Phase 3: Integration (1 day)
- Update BlockToStatementConverter to check for all control structures
- Establish precedence order (try-catch > switch > loop > conditional)
- Add comprehensive tests

## Benefits

1. **Code Reuse**: ~60% of logic is shared between control structures
2. **Consistency**: All structures follow the same conversion patterns
3. **Maintainability**: Bug fixes and improvements benefit all structures
4. **Extensibility**: Easy to add new control structures (e.g., for-of, for-in)
5. **Testing**: Shared components can be unit tested independently

## Next Steps

1. Complete the shared infrastructure implementation
2. Update AST-06, AST-07, and AST-08 issues with specific implementation details
3. Begin parallel implementation of individual converters
4. Create comprehensive test suite for each control structure

## Code Location

All shared infrastructure is in: `src/ast/control_flow/`

Individual converters will be:
- `src/ast/switch_converter.rs` (AST-06)
- `src/ast/loop_converter.rs` (AST-07)  
- `src/ast/try_catch_converter.rs` (AST-08)