# Shared Infrastructure Analysis for AST-06, 07, 08

## Common Patterns Across Control Flow Structures

### 1. CFG Region Detection Pattern
All three control structures share a common pattern:
- **Region Detection**: Identify the start, body blocks, and exit points
- **Entry/Exit Analysis**: Determine how control enters and exits the structure
- **Nested Structure Handling**: Manage structures within structures

### 2. Block Collection and Ordering
- **Body Block Collection**: Gather all blocks that belong to the structure
- **Block Ordering**: Determine the correct sequence of statements
- **Jump Target Resolution**: Handle internal jumps within the structure

### 3. Expression Conversion
- **Condition Expression Building**: Convert bytecode comparisons to AST expressions
- **Variable/Register Mapping**: Map SSA registers to JavaScript variables
- **Complex Expression Reconstruction**: Handle compound conditions

### 4. Statement Generation Pattern
- **Statement List Building**: Convert block sequences to statement arrays
- **Break/Continue Insertion**: Identify and insert control flow statements
- **Scope Management**: Handle variable scoping within structures

## Proposed Shared Infrastructure

### 1. `ControlFlowConverter` Base Module
```rust
// src/ast/control_flow_converter.rs
pub trait ControlFlowConverter<'a> {
    // Common methods for all control flow converters
    fn collect_region_blocks(&self, entry: NodeIndex, exit: NodeIndex) -> Vec<NodeIndex>;
    fn convert_blocks_to_statements(&mut self, blocks: &[NodeIndex]) -> AllocVec<'a, Statement<'a>>;
    fn build_condition_expression(&mut self, condition_block: NodeIndex) -> Expression<'a>;
    fn find_break_targets(&self, blocks: &[NodeIndex]) -> HashSet<NodeIndex>;
    fn find_continue_targets(&self, blocks: &[NodeIndex]) -> HashSet<NodeIndex>;
}
```

### 2. `RegionAnalyzer` Helper
```rust
// src/ast/region_analyzer.rs
pub struct RegionAnalyzer<'a> {
    cfg: &'a Cfg<'a>,
    post_doms: &'a PostDominatorAnalysis,
}

impl<'a> RegionAnalyzer<'a> {
    // Find all blocks dominated by entry and post-dominated by exit
    pub fn find_region_blocks(&self, entry: NodeIndex, exit: NodeIndex) -> Vec<NodeIndex>;
    
    // Determine if a block is a break target (jumps outside region)
    pub fn is_break_target(&self, block: NodeIndex, region_exit: NodeIndex) -> bool;
    
    // Find the immediate exit point for a region
    pub fn find_region_exit(&self, entry: NodeIndex) -> Option<NodeIndex>;
}
```

### 3. `ExpressionBuilder` Enhancement
```rust
// Enhance existing instruction_converter.rs
impl<'a> InstructionConverter<'a> {
    // Build complex conditions from multiple blocks
    pub fn build_compound_condition(&mut self, blocks: &[NodeIndex]) -> Expression<'a>;
    
    // Extract loop increment expressions
    pub fn extract_increment_expression(&mut self, block: NodeIndex) -> Option<Expression<'a>>;
    
    // Build switch discriminant expression
    pub fn build_switch_discriminant(&mut self, dispatch_block: NodeIndex) -> Expression<'a>;
}
```

### 4. `BlockSequencer` Helper
```rust
// src/ast/block_sequencer.rs
pub struct BlockSequencer<'a> {
    cfg: &'a Cfg<'a>,
    processed: HashSet<NodeIndex>,
}

impl<'a> BlockSequencer<'a> {
    // Order blocks in execution sequence
    pub fn sequence_blocks(&mut self, blocks: &[NodeIndex]) -> Vec<NodeIndex>;
    
    // Handle fall-through vs explicit jumps
    pub fn needs_break_statement(&self, current: NodeIndex, next: NodeIndex) -> bool;
}
```

## Specific Converters Using Shared Infrastructure

### AST-06: Switch Statement Converter
```rust
pub struct SwitchConverter<'a> {
    base: ControlFlowConverterBase<'a>,
    switch_tables: &'a SwitchTables,
}
```

### AST-07: Loop Statement Converter
```rust
pub struct LoopConverter<'a> {
    base: ControlFlowConverterBase<'a>,
    loop_analysis: &'a LoopAnalysis,
}
```

### AST-08: Try-Catch Converter
```rust
pub struct TryCatchConverter<'a> {
    base: ControlFlowConverterBase<'a>,
    exception_handlers: &'a ExceptionHandlers,
}
```

## Implementation Order

1. **First**: Build shared infrastructure modules
   - ControlFlowConverter trait
   - RegionAnalyzer
   - BlockSequencer
   - Enhanced ExpressionBuilder

2. **Then**: Implement specific converters in parallel
   - Each can use the shared infrastructure
   - Reduces code duplication
   - Consistent patterns across all control structures

## Benefits of This Approach

1. **Code Reuse**: Common logic is shared, reducing duplication
2. **Consistency**: All control structures follow similar patterns
3. **Maintainability**: Fixes/improvements benefit all structures
4. **Testability**: Shared components can be tested independently
5. **Extensibility**: Easy to add new control structures later