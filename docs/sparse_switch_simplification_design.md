# Sparse Switch Handling Simplification Design

## Acceptance Criteria

This implementation must meet the following acceptance criteria before rollout:

### **Functional Requirements**
- **✅ Zero semantic regressions**: All switch conversions must produce semantically identical JavaScript on finite test domains
- **✅ Structural improvements**: Generated switches should be cleaner than current if/else chains while preserving correctness
- **✅ Safety-first approach**: Must bail out to if/else chains when pattern is unsafe rather than generate incorrect code
- **✅ Deterministic output**: Same input bytecode must always produce identical AST structure (stable for version control)

### **Performance Requirements** 
- **✅ Compilation speed**: Switch detection and conversion should not significantly slow compilation (< 10% overhead)
- **✅ Memory efficiency**: New data structures should not cause memory regressions on large functions
- **✅ Scalability**: Must handle real-world functions with 100+ blocks and complex control flow without quadratic blowup

### **Correctness Requirements**
- **✅ Complete type safety**: All InstructionIndex arithmetic and register operations must use proper types
- **✅ SSA consistency**: Must respect existing SSA analysis without requiring reconstruction or mutations
- **✅ Control flow preservation**: Generated switch statements must maintain exact control flow semantics including -0, Infinity, and NaN handling

### **Quality Requirements**
- **✅ Comprehensive bail-out**: Must handle all edge cases (irreducible loops, exception handlers, non-constant compares) with clear logging
- **✅ Maintainable code**: Simplified implementation should be significantly easier to understand and modify than current 2200+ line version
- **✅ Testability**: Must pass all existing tests plus new corpus validation on mixed type switches, PHI scenarios, and shared tail patterns

### **Integration Requirements**
- **✅ Inline replacement**: Must replace existing SwitchConverter logic incrementally without maintaining parallel implementations
- **✅ Interface compatibility**: Must work with existing BlockToStatementConverter and AST builder patterns
- **✅ Rollback capability**: Each phase must be independently revertible via git if issues are discovered

### **Validation Strategy**
1. **Phase 1 gate**: Auto-diff classification shows zero semantic differences on representative corpus
2. **Performance gate**: Compilation time regression < 10% on large functions
3. **Quality gate**: Generated JavaScript is human-readable and matches or exceeds current output quality
4. **Completeness gate**: All identified sparse switch patterns from corpus are handled or explicitly logged as bail-outs

**Success metric**: 80%+ of identified sparse switch patterns convert cleanly with remaining 20% safely bailing to if/else chains with clear reasoning.

### **Implementation Checklist (Go/No-Go Criteria)**

#### **Functional Requirements → Actions**
- **Zero semantic regressions**: Wire Phase-1 tracer to auto-run finite domain validation on every converted switch: `{all case keys} ∪ {one non-member}` against old/new emitters. Tag diffs as `SemanticallyEqual`/`Different`
- **Structural improvements**: Ensure stable case/key ordering for reproducible "cleaner" output 
- **Safety-first**: Enforce strict bail check order: `constants → default → shared tail → PHI/modelability → control-flow`; abort early with reason
- **Deterministic output**: Sort groups by `(min(start_pc of compare blocks), target_block.index())`; sort keys by `(type, numeric value incl. ±∞, string value)`

#### **Performance Requirements → Actions**  
- **< 10% overhead**: Add counters/timers: `switch_detect_ms`, `reachability_cache_hits/misses`, `cases_detected`, `bails`. Gate on CI perf job
- **Memory efficiency**: Use `SmallVec<[SetupInstruction; 4]>` for `CaseInfo.setup`; intern strings once
- **Scalability**: Hard cap BFS nodes per `is_reachable_without_passing_through`; bail with "reachability budget" if exceeded

#### **Correctness Requirements → Actions**
- **Type safety**: Single helper `fn pc_at(block: NodeIndex, idx: usize) -> InstructionIndex` replacing all manual adds
- **SSA consistency**: When `get_reaching_def_value` returns `None` for tail-consumed register → bail (don't invent undefined)
- **Control flow preservation**: Emit `Infinity`/`-Infinity` correctly; keep `-0` via unary minus

#### **Quality Requirements → Actions**
- **Comprehensive bail-out**: Log reason + compact "shape hash" (`discriminator reg`, `case count`, `targets`) for corpus stats grouping
- **Maintainable code**: Split into files: `switch_info.rs`, `switch_safety.rs`, `switch_emit.rs` 
- **Testability**: Add fixtures for mixed types, `-0` grouping, `±∞` labels, PHI with missing predecessor, immediate tail redef, unsafe setup

#### **Integration Requirements → Actions**
- **Inline replacement**: Behind feature flag `sparse_switch_v2` for quick rollback
- **Interface compatibility**: Use existing AST builder patterns
- **Rollback capability**: Each phase = separate commit with green tests

## Overview

This document outlines a simplified approach to handling sparse switch statements in the Hermes decompiler. The key insight is that we can achieve clean JavaScript output without complex IR transformations by handling everything during AST generation.

## Current Problems

The existing `SwitchConverter` in `src/ast/control_flow/switch_converter.rs` is overly complex (2200+ lines) with several issues:

1. **Complex Multi-Pass Processing**: Pattern detection, setup instruction analysis, and fallthrough detection happen in multiple passes
2. **Fragile Fallthrough Logic**: The current system has complex rules for when to use fallthrough vs explicit breaks
3. **Duplicate Setup Handling**: Setup instructions are collected and processed in multiple places with different rules
4. **Over-engineered Case Grouping**: Complex heuristics that don't leverage Hermes's predictable patterns

## Proposed Solution

### Core Principle: Single-Pass AST Generation

Instead of the current complex multi-pass approach:
```
CFG → Switch Pattern Detection → Setup Collection → Case Grouping → Fallthrough Analysis → JavaScript
```

We simplify to:
```
CFG → Single-Pass Switch Generation (with safety predicates) → JavaScript
```

### Key Insights

1. **Hermes Patterns Are Predictable**: Hermes generates sparse switches with a consistent pattern:
   - Setup instructions immediately before comparisons
   - Sequential comparison blocks
   - Shared jump targets

2. **No IR Mutation Needed**: We can emit clean switch statements by tracking setup during AST generation

3. **Existing SSA Is Sufficient**: The current `SSAAnalysis` in `src/cfg/ssa/types.rs` already provides comprehensive use-def chains, liveness analysis, and all information we need

## Current Codebase Mapping

Before implementing, it's crucial to understand how this design maps to the existing codebase structure:

### Key Files and Integration Points

#### **Primary Target: `src/ast/control_flow/switch_converter.rs` (2200+ lines)**
- **Current**: Complex multi-pass `SwitchConverter` implementation
- **Replace with**: Single-pass pattern detection and AST generation as described
- **Keep**: Same public interface (`convert_switch_pattern()` entry point)
- **Entry point**: Called from `BlockToStatementConverter::convert_blocks_from_cfg()`

#### **Core Infrastructure (Already Available)**
- **`src/cfg/ssa/types.rs`**: `SSAAnalysis`, `RegisterDef`, `RegisterUse` - use as-is
- **`src/cfg/analysis.rs`**: `PostDominatorAnalysis` with `dominates()` method - use as-is  
- **`src/ast/block_converter.rs`**: `BlockToStatementConverter` - extend with new methods
- **`src/cfg/ssa/mod.rs`**: `construct_ssa()` provides complete SSA analysis - use as-is

#### **Instruction Analysis (Existing)**
- **`src/generated/instruction_analysis.rs`**: 
  - `analyze_register_usage()` → use for purity checks and use/def detection
  - Returns `RegisterUsage { target: Option<u8>, sources: Vec<u8>, operands: Vec<u8> }`
- **`src/generated/unified_instructions.rs`**: 
  - `UnifiedInstruction` enum → pattern match for setup instruction types
  - `UnifiedOpcode` → use for purity checking in `is_pure_setup()`

#### **AST Builder (Existing)**
- **Location**: Part of `BlockToStatementConverter` as `self.ast_builder` 
- **Methods to use**:
  - `switch_statement(discriminator: Expression, cases: Vec<SwitchCase>)`
  - `switch_case(test: Option<Expression>, body: Vec<Statement>)`
  - `assignment_statement(left: Pattern, right: Expression)`
  - `break_statement()`
  - `block_statement(statements: Vec<Statement>)`

#### **CFG Structure (Existing)**
- **`src/cfg/mod.rs`**: `Cfg` type with `graph()` method returning `DiGraph<Block, EdgeKind>`
- **`src/cfg/block.rs`**: `Block` with `instructions()`, `start_pc()`, `end_pc()`
- **Edge types**: `EdgeKind::Switch(usize)`, `EdgeKind::Default`, `EdgeKind::Uncond`
- **Traversal**: Use `petgraph` methods like `edges()`, `node_indices()`, `node_weights()`

### Implementation Strategy

#### **Phase 1: Add New Methods to `BlockToStatementConverter`**
Add these methods to the existing converter in `src/ast/block_converter.rs`:

```rust
impl<'a> BlockToStatementConverter<'a> {
    // New methods from design document:
    fn detect_switch_pattern(&self, start_block: NodeIndex) -> Option<SwitchInfo>
    fn extract_case_from_block(&self, block: &Block, discriminator: u8, block_id: NodeIndex) -> Option<CaseInfo>
    fn get_constant_value_at(&self, block_id: NodeIndex, instruction_idx: usize, register: u8) -> Option<ConstantValue>
    fn convert_switch_pattern(&mut self, info: &SwitchInfo) -> Statement
    
    // Existing method to modify:
    fn convert_blocks_from_cfg(&mut self, cfg: &'a Cfg) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError>
    // ↑ Add switch pattern detection before normal block conversion
}
```

#### **Phase 2: Replace Switch Converter Logic**
In `src/ast/control_flow/switch_converter.rs`, replace the existing complex implementation while keeping the same interface:

```rust
// Keep existing public interface:
impl SwitchConverter {
    pub fn convert_switch_pattern(...) -> ... {
        // Replace with simplified single-pass logic from design
    }
}
```

#### **Phase 3: Data Structure Definitions**
Add new data structures (from design) to either:
- **Option A**: New file `src/ast/switch_info.rs`
- **Option B**: Extend `src/ast/control_flow/switch_converter.rs` with new structs
- **Structures**: `SwitchInfo`, `CaseInfo`, `CaseKey`, `ConstantValue`, `SetupInstruction`, etc.

### Critical Integration Points

#### **SSA Integration**
```rust
// From existing SSA analysis:
let ssa_analysis = &self.ssa; // Already available in BlockToStatementConverter
let use_def_chains = &ssa_analysis.use_def_chains; // HashMap<RegisterUse, RegisterDef>
let register_uses = ssa_analysis.get_register_uses(register); // Vec<&RegisterUse>
```

#### **Post-Dominator Integration**  
```rust
// From existing analysis:
let postdom = &self.postdom; // Already available
let dominates = postdom.dominates(dominator_node, target_node); // bool
```

#### **Current Block Context**
```rust
// BlockToStatementConverter already tracks:
self.current_block // NodeIndex - current block being processed
// Need to add:
self.current_instruction_idx // usize - for context-aware constant lookup
```

#### **Arena Allocator Integration**
```rust
// AST nodes must use arena allocation:
let allocator = self.ast_builder.allocator; // &'a Bump
let statements = ArenaVec::new_in(allocator); // For Vec<Statement<'a>>
```

### Existing Patterns to Follow

#### **Error Handling Pattern**
```rust
// Follow existing pattern in block_converter.rs:
fn convert_blocks_from_cfg(&mut self, cfg: &'a Cfg) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
    // Use same error type and patterns
}
```

#### **AST Builder Pattern**  
```rust
// Follow existing usage:
let assignment = self.ast_builder.assignment_statement(
    self.register_to_pattern(register), 
    self.constant_to_expression(value)
);
```

#### **Logging Pattern**
```rust
// Follow existing debug patterns:
log::debug!("Switch pattern detection bailed out at block {:?}: {}", block, reason);
```

### Dependencies and Imports

#### **Required Imports for New Code**
```rust
use crate::cfg::{Cfg, Block, EdgeKind};
use crate::cfg::ssa::{SSAAnalysis, RegisterUse, RegisterDef};  
use crate::cfg::analysis::PostDominatorAnalysis;
use crate::generated::unified_instructions::{UnifiedInstruction, UnifiedOpcode};
use crate::generated::instruction_analysis;
use petgraph::graph::{NodeIndex, DiGraph};
use std::collections::{HashMap, HashSet, BTreeMap};
use std::cell::RefCell; // For memoization cache
use ordered_float::OrderedFloat; // For f64 keys in HashMap
```

#### **External Dependencies**
- **`petgraph`**: Already used for CFG traversal
- **`ordered_float`**: Already available, needed for `OrderedFloat<f64>` in case keys
- **`log`**: Already used for debug logging

### Testing Integration

#### **Existing Test Structure**
- **Location**: `tests/ast_integration.rs` - add new switch test cases here
- **Pattern**: Follow existing integration test patterns for AST generation
- **Golden files**: Likely in `tests/fixtures/` directory structure

#### **Validation Strategy**
```rust
// Add to existing test framework:
#[test]
fn test_sparse_switch_conversion() {
    // Use existing test harness patterns
    let cfg = create_test_cfg_with_sparse_switch();
    let converter = BlockToStatementConverter::new(...);
    let ast = converter.convert_blocks_from_cfg(&cfg);
    // Validate switch structure
}
```

### Important Implementation Notes

#### **Critical Method Signatures from Existing Codebase**
Based on actual codebase inspection:

```rust
// SSAAnalysis methods (from src/cfg/ssa/types.rs):
impl SSAAnalysis {
    pub fn get_register_uses(&self, register: u8) -> Vec<&RegisterUse>
    pub fn get_register_definitions(&self, register: u8) -> Vec<&RegisterDef>  
    pub fn get_stats(&self) -> SSAStats
    // Note: use_def_chains is HashMap<RegisterUse, RegisterDef>
}

// RegisterDef/RegisterUse constructors:
RegisterDef::new(register: u8, block_id: NodeIndex, instruction_idx: InstructionIndex)
RegisterUse::new(register: u8, block_id: NodeIndex, instruction_idx: InstructionIndex)

// Block methods (from src/cfg/block.rs):
impl Block {
    pub fn instructions(&self) -> &[HbcFunctionInstruction]
    pub fn start_pc(&self) -> InstructionIndex  // Note: returns InstructionIndex, not u32
    pub fn end_pc(&self) -> InstructionIndex
}

// PostDominatorAnalysis methods (from src/cfg/analysis.rs):
impl PostDominatorAnalysis {
    pub fn dominates(&self, post_dominator: NodeIndex, node: NodeIndex) -> bool
    pub fn immediate_post_dominator(&self, node: NodeIndex) -> Option<NodeIndex>
}
```

#### **Instruction Index Handling**
- **Type**: `InstructionIndex` (not `u32` or `usize`)
- **Conversion**: `block.start_pc() + idx as u32` where `idx` is instruction position
- **Usage**: `InstructionIndex::zero()`, `InstructionIndex::from(value)`, `instruction_idx.as_usize()`

#### **Arena Vector Types**
- **Correct type**: `ArenaVec<'a, Statement<'a>>` (not `Vec<Statement>`)
- **Creation**: `ArenaVec::new_in(self.ast_builder.allocator)`
- **Import**: Already available in `BlockToStatementConverter`

#### **Edge Traversal Patterns**  
```rust
// Existing patterns for CFG traversal:
for edge in self.cfg.graph().edges(node) {
    let target = edge.target();
    match edge.weight() {
        EdgeKind::Switch(case_idx) => { /* handle switch edge */ }
        EdgeKind::Default => { /* handle default edge */ }
        EdgeKind::Uncond => { /* handle unconditional edge */ }
    }
}
```

#### **Register to Expression Methods**
```rust
// These methods already exist in BlockToStatementConverter:
self.register_to_expression(register: u8) -> Expression<'a>
self.register_to_pattern(register: u8) -> Pattern<'a>  
self.constant_to_expression(value: &ConstantValue) -> Expression<'a>
```

#### **String/Constant Access Methods**
```rust
// These methods exist for constant table access:
self.get_string_constant(string_id: u32) -> String
self.get_double_constant(double_id: u32) -> f64
// Use these in get_constant_value_at() implementation
```

### Directory Structure for New Files
If creating new files, follow existing patterns:
- **Switch logic**: `src/ast/switch_info.rs` (data structures)
- **Safety checks**: `src/ast/switch_safety.rs` (safety predicates)  
- **Tests**: `tests/switch_conversion.rs` (integration tests)

### Performance Considerations from Codebase
- **Arena allocation**: All AST nodes must use `&'a` lifetime and arena allocator
- **CFG size**: Real functions can have 100+ blocks, so O(n²) algorithms need memoization
- **Memory usage**: Keep data structures lean, avoid unnecessary cloning

### **Critical Implementation Fixes (Must Address Before Coding)**

#### **1. Robust Successor Identification (Don't Rely on Edge Order!)**
```rust
/// Returns (true_target, false_target) for a strict-eq conditional
/// Reads the terminator instruction and matches edges by target PC
/// Bails if block's terminator isn't supported conditional or fanout != 2
fn successors_for_strict_eq(&self, block_id: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
    let block = &self.cfg.graph()[block_id];
    
    // 1) Read last instruction; ensure JStrictEqual{,Long}
    let last_instr = block.instructions().last()?;
    let (taken_pc, not_taken_pc) = match &last_instr.instruction {
        UnifiedInstruction::JStrictEqual { target_pc, .. } => {
            (target_pc, &(last_instr.pc + 1)) // not-taken = fall through
        }
        UnifiedInstruction::JStrictEqualLong { target_pc, .. } => {
            (target_pc, &(last_instr.pc + instruction_size))
        }
        _ => return None, // Not a supported conditional
    };
    
    // 2) Match PCs to outgoing edges
    let outgoing: Vec<_> = self.cfg.graph().edges(block_id).collect();
    if outgoing.len() != 2 {
        return None; // Must have exactly 2 successors
    }
    
    // 3) Map target PCs to nodes
    let mut true_target = None;
    let mut false_target = None;
    
    for edge in outgoing {
        let target_block = &self.cfg.graph()[edge.target()];
        if target_block.start_pc() == *taken_pc {
            true_target = Some(edge.target());
        } else if target_block.start_pc() == *not_taken_pc {
            false_target = Some(edge.target());
        }
    }
    
    Some((true_target?, false_target?))
}
```

#### **2. Conservative Block Termination Check**
```rust
/// True if every path from start reaches Ret/Throw before hitting a join
/// Conservative: if uncertain, return false (safer to not use fallthrough)
fn block_always_terminates(&self, start: NodeIndex) -> bool {
    let mut visited = FxHashSet::default();
    let mut work = smallvec![start];
    
    while let Some(current) = work.pop() {
        if !visited.insert(current) { 
            continue; // Already processed
        }
        
        let block = &self.cfg.graph()[current];
        
        // Check if this block terminates
        if let Some(last_instr) = block.instructions().last() {
            match &last_instr.instruction {
                UnifiedInstruction::Ret { .. } | UnifiedInstruction::Throw { .. } => {
                    continue; // This path terminates
                }
                _ => {}
            }
        }
        
        // Check successors
        let successors: SmallVec<[NodeIndex; 2]> = self.cfg.graph()
            .edges(current)
            .map(|e| e.target())
            .collect();
            
        if successors.is_empty() {
            continue; // Dead end → terminates
        } else if successors.len() > 1 {
            return false; // Join-ish → conservative bail
        } else {
            work.push(successors[0]);
        }
        
        // Budget cap to prevent infinite analysis
        if visited.len() > 256 {
            return false; // Conservative bail on complexity
        }
    }
    
    true
}
```

#### **3. Deterministic Ordering Implementation**
```rust
/// Sort groups for deterministic output
fn sort_groups_deterministically(&self, mut groups: Vec<CaseGroup>) -> Vec<CaseGroup> {
    groups.sort_by(|a, b| {
        // Primary: min start_pc of contributing compare blocks
        let min_pc_a = a.cases.iter()
            .map(|c| c.source_compare_pc)
            .min()
            .unwrap_or(InstructionIndex::zero());
        let min_pc_b = b.cases.iter()
            .map(|c| c.source_compare_pc)
            .min()
            .unwrap_or(InstructionIndex::zero());
            
        min_pc_a.cmp(&min_pc_b)
            .then_with(|| a.target.index().cmp(&b.target.index())) // Tie-break by target
    });
    
    // Also sort case keys within each group
    for group in &mut groups {
        group.keys.sort_by(|a, b| self.compare_case_keys(a, b));
    }
    
    groups
}

/// Deterministic case key ordering: Numbers before strings, special numeric handling
fn compare_case_keys(&self, a: &CaseKey, b: &CaseKey) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    
    match (a, b) {
        (CaseKey::Num(a), CaseKey::Num(b)) => {
            // Handle special numeric values first
            let OrderedFloat(a_val) = a;
            let OrderedFloat(b_val) = b;
            
            match (a_val.is_infinite(), b_val.is_infinite()) {
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less, 
                (true, true) => a_val.partial_cmp(b_val).unwrap_or(Ordering::Equal),
                (false, false) => {
                    // Handle -0 vs +0
                    if *a_val == 0.0 && *b_val == 0.0 {
                        a_val.is_sign_negative().cmp(&b_val.is_sign_negative())
                    } else {
                        a_val.partial_cmp(b_val).unwrap_or(Ordering::Equal)
                    }
                }
            }
        }
        (CaseKey::Str(a), CaseKey::Str(b)) => a.cmp(b), // String ordering by code points
        (CaseKey::Num(_), CaseKey::Str(_)) => Ordering::Less,  // Numbers before strings
        (CaseKey::Str(_), CaseKey::Num(_)) => Ordering::Greater,
    }
}
```

#### **4. Type-Safe PC Helper** 
```rust
/// Single source of truth for instruction PC calculation
fn pc_at(&self, block_id: NodeIndex, instruction_idx: usize) -> InstructionIndex {
    self.cfg.graph()[block_id].start_pc().offset(instruction_idx as u32)
}
```

#### **5. Validation Plan Integration**
```rust
/// Validation gates for each phase
struct ValidationGates {
    phase_1_tracer: PhaseOneTracer,  // Auto-diff classification
    perf_profiler: PerformanceProfiler, // Timing + counters
    quality_checker: QualityChecker,    // AST golden comparison
    completeness_tracker: CompletenessTracker, // Conversion rate metrics
}

impl ValidationGates {
    /// Phase 1 gate: Zero semantic differences on representative corpus
    fn validate_phase_1(&self, results: &TracerResults) -> Result<(), String> {
        let semantic_diffs = results.classifications.iter()
            .filter(|c| matches!(c, DiffClassification::Different))
            .count();
            
        if semantic_diffs > 0 {
            return Err(format!("Phase 1 FAILED: {} semantic differences detected", semantic_diffs));
        }
        
        Ok(())
    }
    
    /// Performance gate: < 10% compilation time regression
    fn validate_performance(&self, baseline_ms: u64, current_ms: u64) -> Result<(), String> {
        let regression_pct = ((current_ms as f64 / baseline_ms as f64) - 1.0) * 100.0;
        if regression_pct > 10.0 {
            return Err(format!("Performance FAILED: {:.1}% regression", regression_pct));
        }
        
        Ok(())
    }
}
```

This mapping provides all the context and concrete implementation details needed to implement the design without the current conversation window.

## Detailed Design

### 1. Simplified Data Structures

```rust
/// Information about a switch pattern detected in the CFG
struct SwitchInfo {
    /// The register being switched on (u8 in actual codebase)
    discriminator: u8,
    /// Information about each case
    cases: Vec<CaseInfo>,
    /// Default case target and setup (if any)
    default_case: Option<DefaultCase>,
    /// Blocks that are part of this switch pattern
    involved_blocks: HashSet<NodeIndex>,
    /// Shared tail block that multiple cases jump to (if any)
    shared_tail: Option<SharedTailInfo>,
}

/// Information about a single case or group of cases
struct CaseInfo {
    /// The case keys (numeric or string)
    keys: Vec<CaseKey>,
    /// Setup instructions needed for this case (SmallVec for memory efficiency)
    setup: SmallVec<[SetupInstruction; 4]>,
    /// The target block for this case
    target_block: NodeIndex,
    /// Whether this case's body always terminates (return/throw)
    guaranteed_termination: bool,
    /// Source compare block PC for deterministic ordering
    source_compare_pc: InstructionIndex,
}

/// Default case information
struct DefaultCase {
    /// Target block for default
    target_block: NodeIndex,
    /// Setup instructions for default (often after last compare)
    setup: Vec<SetupInstruction>,
}

/// Information about a shared tail block
struct SharedTailInfo {
    /// The block that multiple cases converge to
    block_id: NodeIndex,
    /// PHI nodes needed (register -> case-specific values)
    phi_nodes: HashMap<u8, PhiNode>,
}

/// PHI node information for join locals
struct PhiNode {
    /// The register being phi'd
    register: u8,
    /// Values per predecessor block
    values: HashMap<NodeIndex, ConstantValue>,
}

/// A setup instruction that needs to be emitted in a case
struct SetupInstruction {
    /// The register being set up
    register: u8,
    /// The value to assign
    value: ConstantValue,
    /// Original instruction (for comments if needed)
    source_instruction: InstructionIndex,
    /// The opcode of the original instruction (for grouping)
    opcode: UnifiedOpcode,
}

/// Case key types for switch statements
#[derive(Clone, PartialEq, Eq, Hash)]
enum CaseKey {
    /// Numeric case key (supports Infinity/-Infinity)
    Num(OrderedFloat<f64>),
    /// String case key (for string switches)
    /// Note: Rc<str> implements Eq by value comparison (string contents), not pointer identity
    Str(std::rc::Rc<str>),
}

impl CaseKey {
    /// Normalize for grouping (treat +0 and -0 as same key)
    fn normalize_for_grouping(&self) -> Self {
        match self {
            CaseKey::Num(OrderedFloat(n)) if *n == 0.0 => CaseKey::Num(OrderedFloat(0.0)),
            _ => self.clone(),
        }
    }
}

/// Constant values that can be set up
#[derive(Clone, PartialEq, Eq, Hash)]
enum ConstantValue {
    String(String),
    Number(OrderedFloat<f64>),  // Use OrderedFloat for Hash
    Boolean(bool),
    Null,
    Undefined,
    // Future: BigInt(BigInt),
}

/// Setup signature for grouping - maps registers to their final values
#[derive(Clone, PartialEq, Eq, Hash)]
struct SetupSignature {
    /// Map of register to its value (last writer wins)
    register_values: BTreeMap<u8, ConstantValue>,
    /// Target block for this setup
    target: NodeIndex,
}

/// A group of cases with the same setup signature and target
struct CaseGroup {
    /// Cases in this group
    cases: Vec<CaseInfo>,
    /// Deduplicated case keys for emission
    keys: Vec<CaseKey>,
    /// Shared setup for all cases in this group (SmallVec for memory efficiency)
    shared_setup: SmallVec<[SetupInstruction; 4]>,
    /// Common target block
    target: NodeIndex,
    /// Whether all cases in this group guarantee termination
    guaranteed_termination: bool,
}
```

### 2. Single-Pass Detection and Conversion

```rust
impl BlockToStatementConverter {
    pub fn convert_blocks_from_cfg(
        &mut self, 
        cfg: &'a crate::cfg::Cfg<'a>
    ) -> Result<ArenaVec<'a, Statement<'a>>, BlockConversionError> {
        let mut statements = ArenaVec::new_in(self.instruction_converter.ast_builder().allocator);
        let mut processed = HashSet::new();
        
        // Get blocks in structured execution order (existing method)
        let block_order = cfg.structured_execution_order();
        
        for (_order_idx, block_id) in block_order.iter().enumerate() {
            if processed.contains(&block_id) {
                continue;
            }
            
            // Try to convert as a switch pattern
            if let Some(switch_info) = self.detect_switch_pattern(block_id) {
                // Convert to switch statement
                let switch_stmt = self.convert_switch_pattern(&switch_info);
                statements.push(switch_stmt);
                
                // Mark all involved blocks as processed
                processed.extend(&switch_info.involved_blocks);
            } else {
                // Normal block conversion
                let block_stmts = self.convert_block(block_id);
                statements.extend(block_stmts);
                processed.insert(block_id);
            }
        }
        
        Ok(statements)
    }
}
```

### 3. Safety Predicates

```rust
/// Determines if a setup instruction is safe to sink into a case
struct SetupSafetyChecker<'a> {
    cfg: &'a crate::cfg::Cfg<'a>,
    ssa: &'a crate::cfg::ssa::SSAAnalysis,
    postdom: &'a crate::cfg::analysis::PostDominatorAnalysis,
    // Memoization for reachability checks: maps (start, barrier) to reachable node set
    reachability_cache: RefCell<HashMap<(NodeIndex, NodeIndex), HashSet<NodeIndex>>>,
}

/// Compare context for anchored safety checks
struct CompareContext {
    compare_block: NodeIndex,
    true_successor: NodeIndex,
    false_successor: NodeIndex,
    discriminator: u8,
}

impl<'a> SetupSafetyChecker<'a> {
    /// Check if a setup instruction is case-localizable
    fn is_case_localizable(
        &self,
        setup: &SetupInstruction,
        compare_ctx: &CompareContext,
        involved_blocks: &HashSet<NodeIndex>,
    ) -> bool {
        // 1. Purity check - only allow safe opcodes
        if !self.is_pure_setup(&setup.opcode) {
            return false;
        }
        
        // 2. Taken-only use check - anchored at compare site
        if !self.all_uses_dominated_by(setup.register, compare_ctx, involved_blocks) {
            return false;
        }
        
        // 3. False-path kill check - semantic bounds
        if self.is_live_on_false_edge(setup.register, compare_ctx, involved_blocks) {
            return false;
        }
        
        true
    }
    
    /// Check if opcode is pure and safe to defer
    fn is_pure_setup(&self, opcode: &UnifiedOpcode) -> bool {
        matches!(opcode,
            UnifiedOpcode::LoadConstString |
            UnifiedOpcode::LoadConstZero |
            UnifiedOpcode::LoadConstUInt8 |
            UnifiedOpcode::LoadConstInt |
            UnifiedOpcode::LoadConstDouble |
            UnifiedOpcode::LoadConstNull |
            UnifiedOpcode::LoadConstUndefined |
            UnifiedOpcode::LoadConstTrue |
            UnifiedOpcode::LoadConstFalse |
            UnifiedOpcode::Mov  // Only if source is constant
        )
    }
    
    /// Check if all uses of a register are dominated by the true successor (anchored at compare)
    fn all_uses_dominated_by(&self, register: u8, compare_ctx: &CompareContext, involved_blocks: &HashSet<NodeIndex>) -> bool {
        // For each use site, test: "can a path from false_succ(compare) reach use_site without passing through true_succ(compare)?"
        // If yes → not safe to sink
        self.ssa.get_register_uses(register)
            .iter()
            .filter(|use_site| involved_blocks.contains(&use_site.block_id)) // Only consider uses in switch region
            .all(|use_site| {
                // Anchored check: start from the false successor of THIS compare
                !self.is_reachable_without_passing_through(
                    compare_ctx.false_successor,
                    use_site.block_id,
                    compare_ctx.true_successor,
                    involved_blocks,
                    compare_ctx.discriminator,
                )
            })
    }
    
    /// Check if target is reachable from start without passing through barrier (constrained to switch region)
    fn is_reachable_without_passing_through(
        &self, 
        start: NodeIndex, 
        target: NodeIndex, 
        barrier: NodeIndex,
        involved_blocks: &HashSet<NodeIndex>,
        discriminator: u8,
    ) -> bool {
        if start == barrier || target == barrier {
            return false;
        }
        
        // Check memoization cache for reachable set
        let cache_key = (start, barrier);
        let reachable_nodes = if let Some(cached_set) = self.reachability_cache.borrow().get(&cache_key) {
            cached_set.clone()
        } else {
            // Compute reachable set once and cache it
            let reachable = self.compute_reachable_set(start, barrier, involved_blocks, discriminator);
            self.reachability_cache.borrow_mut().insert(cache_key, reachable.clone());
            reachable
        };
        
        // Answer membership query from cached set
        reachable_nodes.contains(&target)
    }
    
    /// Compute all nodes reachable from start without passing through barrier
    fn compute_reachable_set(
        &self,
        start: NodeIndex, 
        barrier: NodeIndex,
        involved_blocks: &HashSet<NodeIndex>,
        discriminator: u8,
    ) -> HashSet<NodeIndex> {
        let mut reachable = HashSet::new();
        let mut visited = HashSet::new();
        let mut to_visit = vec![start];
        
        while let Some(current) = to_visit.pop() {
            if current == barrier || !visited.insert(current) {
                continue;
            }
            
            reachable.insert(current);
            
            // Only traverse within the switch region and stop at multi-succ nodes outside the chain
            if involved_blocks.contains(&current) {
                let successors: Vec<_> = self.cfg.graph().edges(current).collect();
                
                // Stop at multi-successor nodes that aren't part of the switch chain
                if successors.len() > 1 && !self.is_switch_chain_node(current, discriminator) {
                    continue;
                }
                
                for edge in successors {
                    let next = edge.target();
                    if involved_blocks.contains(&next) && !visited.contains(&next) {
                        to_visit.push(next);
                    }
                }
            }
        }
        
        reachable
    }
    
    /// Check if a node is part of the switch comparison chain
    fn is_switch_chain_node(&self, node: NodeIndex, discriminator: u8) -> bool {
        let block = &self.cfg.graph()[node];
        
        // A switch chain node contains ≤1 compare on the same discriminator OR is default/tail
        let mut compare_count = 0;
        let mut valid_discriminator = true;
        
        for (idx, instr) in block.instructions().iter().enumerate() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual { operand_0, operand_1, .. } |
                UnifiedInstruction::JStrictEqualLong { operand_0, operand_1, .. } => {
                    compare_count += 1;
                    
                    // Check if this comparison uses the same discriminator
                    let op0_is_const = self.get_constant_value_at(node, idx, *operand_0).is_some();
                    let op1_is_const = self.get_constant_value_at(node, idx, *operand_1).is_some();
                    
                    let found_discriminator = match (op0_is_const, op1_is_const) {
                        (false, true) => Some(*operand_0),  // reg === const
                        (true, false) => Some(*operand_1),  // const === reg
                        _ => None, // Both const or both reg - not discriminator compare
                    };
                    
                    if found_discriminator != Some(discriminator) {
                        valid_discriminator = false;
                    }
                }
                _ => {}
            }
        }
        
        // Valid chain node: exactly one comparison on correct discriminator or no comparisons (default/tail)
        compare_count <= 1 && valid_discriminator
    }
    
    /// Check if register is live on the false edge (semantic bounds)
    fn is_live_on_false_edge(
        &self,
        register: u8,
        compare_ctx: &CompareContext,
        involved_blocks: &HashSet<NodeIndex>,
    ) -> bool {
        // Walk only along the single-successor "ladder" on the false edge
        let mut current = compare_ctx.false_successor;
        let mut visited = HashSet::new();
        
        // Semantic bounds: stop at next compare/default block, blocks outside involved_blocks, or multi-succ blocks
        while visited.insert(current) {
            let block = &self.cfg.graph()[current];
            
            // Check each instruction in the block
            for instr in block.instructions() {
                // If we see a use of the register, it's live
                if self.instruction_uses_register(&instr.instruction, register) {
                    return true;
                }
                // If we see a def of the register, it's killed (not live)
                if self.instruction_defines_register(&instr.instruction, register) {
                    return false;
                }
            }
            
            // Stop conditions:
            // (a) Block not in involved_blocks
            if !involved_blocks.contains(&current) {
                break;
            }
            
            // (b) Multi-successor block (next compare or join point)
            let successors: Vec<_> = self.cfg.graph().edges(current).collect();
            if successors.len() != 1 {
                break;
            }
            
            // Continue to next block in the ladder
            current = successors[0].target();
        }
        
        // Default to not live if we reach the end without finding use or def
        false
    }
    
    /// Check if an instruction uses a register
    fn instruction_uses_register(&self, instr: &crate::generated::unified_instructions::UnifiedInstruction, register: u8) -> bool {
        // Use existing register analysis to determine if register is used
        let usage = crate::generated::instruction_analysis::analyze_register_usage(instr);
        usage.operands.contains(&register)
    }
    
    /// Check if an instruction defines a register
    fn instruction_defines_register(&self, instr: &crate::generated::unified_instructions::UnifiedInstruction, register: u8) -> bool {
        // Use existing register analysis to determine if register is defined
        let usage = crate::generated::instruction_analysis::analyze_register_usage(instr);
        usage.target == Some(register)
    }
}
```

### 4. Pattern Detection Algorithm

```rust
impl BlockToStatementConverter {
    /// Detect if a block starts a switch pattern
    fn detect_switch_pattern(&self, start_block: NodeIndex) -> Option<SwitchInfo> {
        // Quick check: does this block load a parameter or have a comparison?
        let first_block = &self.cfg.graph()[start_block];
        let discriminator = self.find_discriminator(first_block, start_block)?;
        
        // Safety checker for setup instructions
        let safety_checker = SetupSafetyChecker {
            cfg: &self.cfg,
            ssa: &self.ssa,
            postdom: &self.postdom,
            reachability_cache: RefCell::new(HashMap::new()),
        };
        
        // Follow the comparison chain
        let mut current_block = start_block;
        let mut cases = Vec::new();
        let mut involved_blocks = HashSet::new();
        let mut visited = HashSet::new();
        let mut last_compare_block = None;
        
        // Track which registers are live across false edges
        let mut live_across_false = HashSet::new();
        
        loop {
            if !visited.insert(current_block) {
                break; // Avoid infinite loops
            }
            
            involved_blocks.insert(current_block);
            let block = &self.cfg.graph()[current_block];
            
            // Look for comparison pattern
            if let Some(mut case_info) = self.extract_case_from_block(block, discriminator, current_block) {
                // Create compare context for anchored safety checks
                let true_successor = case_info.target_block;
                let false_successor = self.get_false_successor(current_block)?;
                let compare_ctx = CompareContext {
                    compare_block: current_block,
                    true_successor,
                    false_successor,
                    discriminator,
                };
                
                case_info.setup.retain(|setup| {
                    let is_safe = safety_checker.is_case_localizable(
                        setup,
                        &compare_ctx,
                        &involved_blocks,
                    );
                    
                    if !is_safe {
                        live_across_false.insert(setup.register);
                    }
                    
                    is_safe
                });
                
                // Check if this case's body always terminates
                case_info.guaranteed_termination = self.block_always_terminates(true_successor);
                
                cases.push(case_info);
                last_compare_block = Some(current_block);
                
                // Follow the false branch to next comparison
                current_block = false_successor;
            } else {
                // This might be the default block
                break;
            }
        }
        
        // Need at least 2 cases to be worth converting
        if cases.len() < 2 {
            return None;
        }
        
        // Bail out if too many cases (configurable threshold)
        if cases.len() > 32 {
            self.bail_out("Too many cases (>32)", start_block);
            return None;
        }
        
        // Bail out if too many unique targets (switch won't be cleaner)
        let unique_targets: HashSet<_> = cases.iter()
            .map(|c| c.target_block)
            .collect();
        if unique_targets.len() > 8 {  // Configurable threshold
            self.bail_out("Too many unique target blocks", start_block);
            return None;
        }
        
        // Explicit bail check order to avoid wasted work:
        // 1. Check constants first (cheapest)
        if self.should_bail_out_for_constants(&cases) {
            return None;
        }
        
        // 2. Extract default case
        let default_case = if let Some(last_compare) = last_compare_block {
            self.extract_default_setup(current_block, last_compare, &live_across_false)
                .map(|setup| DefaultCase {
                    target_block: current_block,
                    setup,
                })
        } else {
            None
        };
        
        // 3. Detect shared tail block
        let shared_tail = self.detect_shared_tail(&cases, &default_case);
        
        // 4. Check PHI scenarios and control flow (more expensive)
        if self.should_bail_out_for_phi_scenarios(&shared_tail) {
            return None;
        }
        
        if self.should_bail_out_for_control_flow(&involved_blocks) {
            return None;
        }
        
        Some(SwitchInfo {
            discriminator,
            cases,
            default_case,
            involved_blocks,
            shared_tail,
        })
    }
    
    /// Find what register is being used as discriminator
    fn find_discriminator(&self, block: &Block, block_id: NodeIndex) -> Option<u8> {
        // Look for LoadParam as first instruction
        if let Some(first) = block.instructions().first() {
            match &first.instruction {
                UnifiedInstruction::LoadParam { operand_0, .. } => {
                    return Some(*operand_0);
                }
                _ => {}
            }
        }
        
        // Look for a comparison to find discriminator
        for (idx, inst) in block.instructions().iter().enumerate() {
            match &inst.instruction {
                UnifiedInstruction::JStrictEqual { operand_0, operand_1, .. } |
                UnifiedInstruction::JStrictEqualLong { operand_0, operand_1, .. } => {
                    // Hermes can compare (const, reg) or (reg, const)
                    // Find which operand is the register using context-aware lookup
                    let op0_is_const = self.get_constant_value_at(block_id, idx, *operand_0).is_some();
                    let op1_is_const = self.get_constant_value_at(block_id, idx, *operand_1).is_some();
                    
                    match (op0_is_const, op1_is_const) {
                        (false, true) => return Some(*operand_0),  // reg === const
                        (true, false) => return Some(*operand_1),  // const === reg
                        _ => return None,  // Both const or both reg - bail out
                    }
                }
                _ => {}
            }
        }
        
        None
    }
    
    /// Extract case information from a comparison block
    fn extract_case_from_block(
        &self, 
        block: &Block, 
        discriminator: u8,
        block_id: NodeIndex,
    ) -> Option<CaseInfo> {
        let mut setup = Vec::new();
        let mut case_value = None;
        let mut target = None;
        let mut found_non_constant_compare = false;
        
        // Scan instructions
        for (idx, inst) in block.instructions().iter().enumerate() {
            let opcode = inst.instruction.opcode();
            
            match &inst.instruction {
                // Setup instructions
                UnifiedInstruction::LoadConstString { operand_0, operand_1 } => {
                    setup.push(SetupInstruction {
                        register: *operand_0,
                        value: ConstantValue::String(
                            self.get_string_constant(*operand_1)
                        ),
                        source_instruction: block.start_pc() + idx,
                        opcode,
                    });
                }
                UnifiedInstruction::LoadConstZero { operand_0 } => {
                    setup.push(SetupInstruction {
                        register: *operand_0,
                        value: ConstantValue::Number(OrderedFloat(0.0)),
                        source_instruction: block.start_pc() + idx,
                        opcode,
                    });
                }
                UnifiedInstruction::Mov { operand_0, operand_1 } => {
                    // Track moves of constants
                    if let Some(const_val) = self.get_constant_value_at(block_id, idx, *operand_1) {
                        setup.push(SetupInstruction {
                            register: *operand_0,
                            value: const_val,
                            source_instruction: block.start_pc() + idx,
                            opcode,
                        });
                    }
                }
                
                // Comparison
                UnifiedInstruction::JStrictEqual { operand_0, operand_1, .. } |
                UnifiedInstruction::JStrictEqualLong { operand_0, operand_1, .. } => {
                    // Handle both (reg, const) and (const, reg) patterns
                    let (reg_operand, const_operand) = 
                        if *operand_0 == discriminator {
                            (*operand_0, *operand_1)
                        } else if *operand_1 == discriminator {
                            (*operand_1, *operand_0)
                        } else {
                            // Discriminator changed - bail out
                            return None;
                        };
                    
                    // Extract constant value using context-aware lookup
                    case_value = self.get_constant_value_at(block_id, idx, const_operand)
                        .and_then(|v| match v {
                            ConstantValue::Number(OrderedFloat(n)) => {
                                // Check for NaN - these comparisons are always false
                                if n.is_nan() {
                                    return None;  // Bail on NaN comparisons
                                }
                                // Support Infinity/-Infinity as valid case labels
                                Some(CaseKey::Num(OrderedFloat(n)))
                            },
                            ConstantValue::String(ref s) => {
                                Some(CaseKey::Str(std::rc::Rc::from(s.as_str())))
                            },
                            _ => None,
                        });
                    
                    // Bail if comparing to non-constant
                    if case_value.is_none() {
                        found_non_constant_compare = true;
                    }
                    
                    target = self.get_true_successor(block);
                }
                
                _ => {} // Ignore other instructions
            }
        }
        
        // Bail if we found non-constant comparison
        if found_non_constant_compare {
            return None;
        }
        
        // Build case info if we found a comparison
        case_value.and_then(|key| {
            target.map(|target_block| {
                CaseInfo {
                    keys: vec![key],
                    setup,
                    target_block,
                    guaranteed_termination: false, // Will be filled later
                }
            })
        })
    }
    
    /// Extract default case setup from the tail block
    fn extract_default_setup(
        &self,
        default_block: NodeIndex,
        last_compare_block: NodeIndex,
        live_across_false: &HashSet<u8>,
    ) -> Option<Vec<SetupInstruction>> {
        let block = &self.cfg.graph()[default_block];
        let mut setup = Vec::new();
        
        // Look for setup instructions before any jump
        for (idx, inst) in block.instructions().iter().enumerate() {
            let opcode = inst.instruction.opcode();
            
            // Stop at first jump/control flow
            if matches!(opcode, 
                UnifiedOpcode::Jmp | 
                UnifiedOpcode::JmpTrue | 
                UnifiedOpcode::JmpFalse |
                UnifiedOpcode::Ret |
                UnifiedOpcode::Throw
            ) {
                break;
            }
            
            // Extract constant loads
            match &inst.instruction {
                UnifiedInstruction::LoadConstString { operand_0, operand_1 } => {
                    if !live_across_false.contains(operand_0) {
                        setup.push(SetupInstruction {
                            register: *operand_0,
                            value: ConstantValue::String(
                                self.get_string_constant(*operand_1)
                            ),
                            source_instruction: block.start_pc() + idx,
                            opcode,
                        });
                    }
                }
                // ... similar for other const loads ...
                _ => {}
            }
        }
        
        if setup.is_empty() {
            None
        } else {
            Some(setup)
        }
    }
}
```

### 5. Shared Tail Detection and PHI Analysis

```rust
impl BlockToStatementConverter {
    /// Detect if multiple cases converge to a shared tail block
    fn detect_shared_tail(
        &self,
        cases: &[CaseInfo],
        default_case: &Option<DefaultCase>,
    ) -> Option<SharedTailInfo> {
        // Find blocks that are jumped to by multiple cases
        let mut target_counts: HashMap<NodeIndex, usize> = HashMap::new();
        
        for case in cases {
            *target_counts.entry(case.target_block).or_insert(0) += 1;
        }
        
        if let Some(default) = default_case {
            *target_counts.entry(default.target_block).or_insert(0) += 1;
        }
        
        // Find postdominator of all targets
        let all_targets: Vec<NodeIndex> = target_counts.keys().copied().collect();
        let shared_tail = self.postdom.find_common_postdominator(&all_targets)?;
        
        // Check if this is a meaningful shared tail (not just exit block)
        if !self.is_meaningful_shared_tail(shared_tail) {
            return None;
        }
        
        // Analyze PHI requirements
        let phi_nodes = self.analyze_phi_requirements(shared_tail, cases, default_case);
        
        Some(SharedTailInfo {
            block_id: shared_tail,
            phi_nodes,
        })
    }
    
    /// Analyze which registers need PHI nodes at the shared tail
    fn analyze_phi_requirements(
        &self,
        tail_block: NodeIndex,
        cases: &[CaseInfo],
        default_case: &Option<DefaultCase>,
    ) -> HashMap<u8, PhiNode> {
        let mut phi_nodes = HashMap::new();
        let tail = &self.cfg.graph()[tail_block];
        
        // Find all registers consumed in the tail (read before def)
        let consumed_registers = self.find_consumed_registers_read_before_def(tail);
        
        for register in consumed_registers {
            let mut values = HashMap::new();
            let mut needs_phi = false;
            let mut first_value = None;
            
            // Check each predecessor's definition
            for case in cases {
                if let Some(value) = self.get_reaching_def_value(register, case.target_block) {
                    values.insert(case.target_block, value.clone());
                    
                    if let Some(ref first) = first_value {
                        if first != &value {
                            needs_phi = true;
                        }
                    } else {
                        first_value = Some(value);
                    }
                }
            }
            
            // Check default case
            if let Some(default) = default_case {
                if let Some(value) = self.get_reaching_def_value(register, default.target_block) {
                    values.insert(default.target_block, value.clone());
                    
                    if let Some(ref first) = first_value {
                        if first != &value {
                            needs_phi = true;
                        }
                    }
                }
            }
            
            // Only create PHI if values differ
            if needs_phi {
                phi_nodes.insert(register, PhiNode {
                    register,
                    values,
                });
            }
        }
        
        phi_nodes
    }
    
    /// Find registers consumed (read before def) in the tail block
    fn find_consumed_registers_read_before_def(&self, tail: &Block) -> HashSet<u8> {
        let mut consumed = HashSet::new();
        let mut redefined = HashSet::new();
        
        // Scan instructions in order
        for instr in tail.instructions() {
            let usage = crate::generated::instruction_analysis::analyze_register_usage(&instr.instruction);
            
            // If we see a use of a register that hasn't been redefined yet, it's consumed
            for &src_reg in &usage.sources {
                if !redefined.contains(&src_reg) {
                    consumed.insert(src_reg);
                }
            }
            
            // If we see a def, mark as redefined (no longer needs PHI)
            if let Some(target_reg) = usage.target {
                redefined.insert(target_reg);
                consumed.remove(&target_reg); // Remove if it was added earlier
            }
        }
        
        consumed
    }
}
```

### 6. Switch Statement Generation

```rust
impl BlockToStatementConverter {
    /// Convert switch pattern to AST
    fn convert_switch_pattern(&mut self, info: &SwitchInfo) -> Statement {
        let discriminator_expr = self.register_to_expression(info.discriminator);
        
        // For shared tail: always use join locals + single tail emission
        let (join_locals, requires_tail_emission) = if let Some(ref shared_tail) = info.shared_tail {
            // Always emit shared tail once after the switch - never inline in any case
            (self.create_join_locals(&shared_tail.phi_nodes), true)
        } else {
            (HashMap::new(), false)
        };
        
        // Group cases by normalized setup and target
        let grouped_cases = self.group_cases_by_normalized_setup(&info.cases);
        
        let mut switch_cases = Vec::new();
        let mut all_statements = Vec::new();
        
        // Emit join local declarations if needed
        for (register, local_name) in &join_locals {
            let decl = self.ast_builder.variable_declaration(
                local_name.clone(),
                None, // undefined initially
            );
            all_statements.push(decl);
        }
        
        // Generate switch cases
        for group in grouped_cases {
            let case_keys = &group.keys; // Use deduplicated keys from grouping
            
            // Determine if we should use fallthrough
            let use_fallthrough = self.should_use_fallthrough(&group);
            
            if use_fallthrough {
                // Emit empty cases for all but last
                for key in &case_keys[..case_keys.len() - 1] {
                    let test = Some(self.case_key_to_expression(key));
                    let case = self.ast_builder.switch_case(test, vec![]);
                    switch_cases.push(case);
                }
                
                // Last case has the body
                let last_key = case_keys.last().unwrap();
                let test = Some(self.case_key_to_expression(last_key));
                let body = self.generate_case_body(&group, &join_locals);
                let case = self.ast_builder.switch_case(test, body);
                switch_cases.push(case);
            } else {
                // Emit full body for each case key
                for key in &case_keys {
                    let test = Some(self.case_key_to_expression(key));
                    let body = self.generate_case_body(&group, &join_locals);
                    let case = self.ast_builder.switch_case(test, body);
                    switch_cases.push(case);
                }
            }
        }
        
        // Add default case
        if let Some(ref default_case) = info.default_case {
            let mut default_body = Vec::new();
            
            // Emit default setup (only non-phi registers)
            for setup in &default_case.setup {
                if info.shared_tail.as_ref()
                    .map(|tail| !tail.phi_nodes.contains_key(&setup.register))
                    .unwrap_or(true) {
                    default_body.push(self.create_setup_statement(setup));
                }
            }
            
            // For shared tail: assign join locals and break, never inline the tail
            if let Some(ref shared_tail) = info.shared_tail {
                // Emit join local assignments
                for (register, phi_node) in &shared_tail.phi_nodes {
                    if let Some(local_name) = join_locals.get(register) {
                        if let Some(value) = phi_node.values.get(&default_case.target_block) {
                            let assignment = self.ast_builder.assignment_statement(
                                self.ast_builder.identifier(local_name.clone()),
                                self.constant_to_expression_preserving_negative_zero(value),
                            );
                            default_body.push(assignment);
                        }
                    }
                }
                // Always break for shared tail
                default_body.push(self.ast_builder.break_statement());
            } else {
                // No shared tail - convert default target inline
                let target_stmts = self.convert_block_with_env(
                    default_case.target_block,
                    &join_locals,
                );
                default_body.extend(target_stmts);
            }
            
            let default = self.ast_builder.switch_case(None, default_body);
            switch_cases.push(default);
        }
        
        let switch_stmt = self.ast_builder.switch_statement(discriminator_expr, switch_cases);
        all_statements.push(switch_stmt);
        
        // Emit shared tail if needed (standardized approach)
        if requires_tail_emission {
            if let Some(ref shared_tail) = info.shared_tail {
                let tail_stmts = self.convert_block_with_env(
                    shared_tail.block_id,
                    &join_locals,
                );
                all_statements.extend(tail_stmts);
            }
        }
        
        // Wrap in block if multiple statements
        if all_statements.len() == 1 {
            all_statements.into_iter().next().unwrap()
        } else {
            self.ast_builder.block_statement(all_statements)
        }
    }
    
    /// Generate the body for a case group
    fn generate_case_body(
        &mut self,
        group: &CaseGroup,
        join_locals: &HashMap<u8, String>,
    ) -> Vec<Statement> {
        let mut body = Vec::new();
        
        // Emit setup (suppress redundant assignments for shared tail PHI registers)
        for setup in &group.shared_setup {
            if self.current_switch_info.shared_tail.as_ref()
                .map(|tail| !tail.phi_nodes.contains_key(&setup.register))
                .unwrap_or(true) {
                body.push(self.create_setup_statement(setup));
            }
        }
        
        // Emit join local assignments if needed
        if let Some(ref tail_info) = self.current_switch_info.shared_tail {
            for (register, phi_node) in &tail_info.phi_nodes {
                if let Some(local_name) = join_locals.get(register) {
                    if let Some(value) = phi_node.values.get(&group.target) {
                        let assignment = self.ast_builder.assignment_statement(
                            self.ast_builder.identifier(local_name.clone()),
                            self.constant_to_expression_preserving_negative_zero(value),
                        );
                        body.push(assignment);
                    }
                }
            }
        }
        
        // For shared tail cases: don't inline the tail, just break
        if self.current_switch_info.shared_tail.is_some() {
            // Always emit break for shared tail cases
            body.push(self.ast_builder.break_statement());
        } else {
            // No shared tail - convert target block inline
            let target_stmts = self.convert_block_with_env(group.target, join_locals);
            body.extend(target_stmts);
            
            // Add break unless guaranteed termination
            if !group.guaranteed_termination {
                body.push(self.ast_builder.break_statement());
            }
        }
        
        body
    }
    
    /// Determine if we should use fallthrough for this group
    fn should_use_fallthrough(&self, group: &CaseGroup) -> bool {
        // Prefer explicit breaks for safety and clarity
        // Only use fallthrough when body is guaranteed to terminate immediately
        // AND there are no join locals involved
        group.cases.len() > 1 && 
        group.guaranteed_termination && 
        self.current_switch_info.shared_tail.is_none()
    }
    
    /// Group cases by normalized setup and target (deterministic ordering)
    fn group_cases_by_normalized_setup(&self, cases: &[CaseInfo]) -> Vec<CaseGroup> {
        let mut groups: HashMap<SetupSignature, (Vec<&CaseInfo>, usize)> = HashMap::new();
        
        // Group cases and preserve discovery order
        for (discovery_order, case) in cases.iter().enumerate() {
            let signature = self.normalize_setup(&case.setup, case.target_block);
            groups.entry(signature).or_insert_with(|| (Vec::new(), discovery_order)).0.push(case);
        }
        
        // Convert to stable-ordered groups
        let mut group_list: Vec<_> = groups.into_iter()
            .map(|(setup_sig, (cases, discovery_order))| {
                // Build deduplicated setup from the signature
                let shared_setup = self.signature_to_setup(&setup_sig);
                
                // Deduplicate case keys using normalize_for_grouping, preserve first-seen order
                let mut seen_normalized = HashSet::new();
                let mut deduplicated_keys = Vec::new();
                
                for case in &cases {
                    for key in &case.keys {
                        let normalized = key.normalize_for_grouping();
                        if seen_normalized.insert(normalized.clone()) {
                            deduplicated_keys.push(key.clone()); // Keep original for proper emission
                        }
                    }
                }
                
                (CaseGroup {
                    cases: cases.into_iter().cloned().collect(),
                    keys: deduplicated_keys, // Use deduplicated keys
                    shared_setup,
                    target: setup_sig.target,
                    guaranteed_termination: cases[0].guaranteed_termination,
                }, discovery_order)
            })
            .collect();
        
        // Sort by discovery order to maintain deterministic output
        group_list.sort_by_key(|(_, order)| *order);
        group_list.into_iter().map(|(group, _)| group).collect()
    }
    
    /// Normalize setup to a signature (register -> value map, last writer wins)
    fn normalize_setup(&self, setup: &[SetupInstruction], target: NodeIndex) -> SetupSignature {
        let mut register_values = BTreeMap::new();
        
        // Process in order, last writer wins
        for inst in setup {
            register_values.insert(inst.register, inst.value.clone());
        }
        
        SetupSignature {
            register_values,
            target,
        }
    }
    
    /// Convert signature back to setup instructions
    fn signature_to_setup(&self, sig: &SetupSignature) -> Vec<SetupInstruction> {
        sig.register_values.iter()
            .map(|(register, value)| {
                SetupInstruction {
                    register: *register,
                    value: value.clone(),
                    source_instruction: InstructionIndex::zero(),  // Will be updated if needed
                    opcode: self.value_to_opcode(value),
                }
            })
            .collect()
    }
    
    /// Convert constant value to corresponding opcode (total coverage)
    fn value_to_opcode(&self, value: &ConstantValue) -> UnifiedOpcode {
        match value {
            ConstantValue::String(_) => UnifiedOpcode::LoadConstString,
            ConstantValue::Number(OrderedFloat(n)) if *n == 0.0 => UnifiedOpcode::LoadConstZero,
            ConstantValue::Number(_) => UnifiedOpcode::LoadConstDouble, // Default to double for other numbers
            ConstantValue::Boolean(true) => UnifiedOpcode::LoadConstTrue,
            ConstantValue::Boolean(false) => UnifiedOpcode::LoadConstFalse,
            ConstantValue::Null => UnifiedOpcode::LoadConstNull,
            ConstantValue::Undefined => UnifiedOpcode::LoadConstUndefined,
        }
    }
    
    /// Check if a block always terminates (conservative check)
    fn block_always_terminates(&self, start_block: NodeIndex) -> bool {
        // Conservative check: walk from target_block to next join/postdom
        // If every path hits Ret or Throw before a join, it terminates
        let mut visited = HashSet::new();
        let mut to_visit = vec![start_block];
        
        while let Some(current) = to_visit.pop() {
            if !visited.insert(current) {
                continue; // Already processed
            }
            
            let block = &self.cfg.graph()[current];
            
            // Check if this block terminates
            if let Some(last_instr) = block.instructions().last() {
                match &last_instr.instruction {
                    UnifiedInstruction::Ret { .. } |
                    UnifiedInstruction::Throw { .. } => {
                        continue; // This path terminates
                    }
                    _ => {}
                }
            }
            
            // Check successors
            let successors: Vec<_> = self.cfg.graph().edges(current).collect();
            if successors.is_empty() {
                // Dead end without terminator - assume terminates
                continue;
            } else if successors.len() > 1 {
                // Multiple successors suggest a join point - be conservative
                return false;
            } else {
                // Single successor - continue checking
                to_visit.push(successors[0].target());
            }
            
            // Limit search depth to avoid infinite loops
            if visited.len() > 10 {
                return false; // Conservative bail
            }
        }
        
        true
    }
    
    /// Normalize zero values (-0 -> 0 for grouping)
    fn normalize_zero(&self, n: f64) -> f64 {
        if n == 0.0 { 0.0 } else { n }
    }
    
    /// Create assignment statement for setup
    fn create_setup_statement(&mut self, setup: &SetupInstruction) -> Statement {
        let left = self.register_to_pattern(setup.register);
        let right = self.constant_to_expression_preserving_negative_zero(&setup.value);
        
        self.ast_builder.assignment_statement(left, right)
    }
    
    /// Convert case key to expression
    fn case_key_to_expression(&mut self, key: &CaseKey) -> Expression {
        match key {
            CaseKey::Num(OrderedFloat(n)) => self.num_case_expr(*n),
            CaseKey::Str(ref s) => {
                self.ast_builder.literal_string(s.as_ref())
            }
        }
    }
    
    /// Convert numeric case key to expression (handles special values)
    fn num_case_expr(&mut self, n: f64) -> Expression {
        if n.is_nan() { 
            unreachable!("NaN should have been filtered out during detection"); 
        }
        
        if n.is_infinite() {
            let infinity_id = self.ast_builder.identifier("Infinity");
            return if n.is_sign_negative() {
                self.ast_builder.unary_expression(UnaryOperator::Minus, infinity_id)
            } else { 
                infinity_id 
            };
        }
        
        if n.is_sign_negative() && n == 0.0 {
            // Preserve -0 with unary minus
            return self.ast_builder.unary_expression(
                UnaryOperator::Minus,
                self.ast_builder.literal_number(0.0),
            );
        }
        
        self.ast_builder.literal_number(n)
    }
    
    /// Convert constant to expression, preserving -0
    fn constant_to_expression_preserving_negative_zero(&mut self, value: &ConstantValue) -> Expression {
        match value {
            ConstantValue::Number(OrderedFloat(n)) if n.is_sign_negative() && *n == 0.0 => {
                // Preserve -0 with unary minus
                self.ast_builder.unary_expression(
                    UnaryOperator::Minus,
                    self.ast_builder.literal_number(0.0),
                )
            }
            _ => self.constant_to_expression(value),
        }
    }
}
```

### 7. Integration with Existing Infrastructure

```rust
impl BlockToStatementConverter {
    /// Get the constant value that a register holds at a specific location
    fn get_constant_value_at(&self, block_id: NodeIndex, instruction_idx: usize, register: u8) -> Option<ConstantValue> {
        // Use existing SSA use-def chains from the actual SSA analysis
        let instruction_pc = self.cfg.graph()[block_id].start_pc().offset(instruction_idx as u32);
        let use_site = crate::cfg::ssa::RegisterUse::new(
            register, 
            block_id, 
            instruction_pc
        );
        
        if let Some(def_site) = self.ssa.use_def_chains.get(&use_site) {
            // Look at the defining instruction
            let def_block = &self.cfg.graph()[def_site.block_id];
            if let Some(def_inst) = def_block.instructions().get(def_site.instruction_idx.as_usize()) {
                match &def_inst.instruction {
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstString { operand_1, .. } => {
                        Some(ConstantValue::String(self.get_string_constant(*operand_1)))
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstZero { .. } => {
                        Some(ConstantValue::Number(OrderedFloat(0.0)))
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                        Some(ConstantValue::Number(OrderedFloat(*operand_1 as f64)))
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                        Some(ConstantValue::Number(OrderedFloat(self.get_double_constant(*operand_1))))
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstNull { .. } => {
                        Some(ConstantValue::Null)
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstUndefined { .. } => {
                        Some(ConstantValue::Undefined)
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstTrue { .. } => {
                        Some(ConstantValue::Boolean(true))
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstFalse { .. } => {
                        Some(ConstantValue::Boolean(false))
                    }
                    // ... other constant loads ...
                    _ => None
                }
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// Legacy method for compatibility (delegates to context-aware version)
    fn get_constant_value(&self, register: u8) -> Option<ConstantValue> {
        self.get_constant_value_at(self.current_block, self.current_instruction_idx, register)
    }
    
    /// Get the true successor (taken branch) for a comparison block
    fn get_true_successor(&self, block_id: NodeIndex) -> Option<NodeIndex> {
        let block = &self.cfg.graph()[block_id];
        
        // Look for JStrictEqual{,Long} as last instruction
        if let Some(last_instr) = block.instructions().last() {
            match &last_instr.instruction {
                UnifiedInstruction::JStrictEqual { .. } |
                UnifiedInstruction::JStrictEqualLong { .. } => {
                    // Find the taken edge (should be first successor for conditional jumps)
                    let successors: Vec<_> = self.cfg.graph().edges(block_id).collect();
                    if successors.len() == 2 {
                        // Convention: true edge is first, false edge is second
                        return Some(successors[0].target());
                    }
                }
                _ => {}
            }
        }
        
        None
    }
    
    /// Get the false successor (not-taken branch) for a comparison block  
    fn get_false_successor(&self, block_id: NodeIndex) -> Option<NodeIndex> {
        let block = &self.cfg.graph()[block_id];
        
        // Look for JStrictEqual{,Long} as last instruction
        if let Some(last_instr) = block.instructions().last() {
            match &last_instr.instruction {
                UnifiedInstruction::JStrictEqual { .. } |
                UnifiedInstruction::JStrictEqualLong { .. } => {
                    // Find the not-taken edge (should be second successor for conditional jumps)
                    let successors: Vec<_> = self.cfg.graph().edges(block_id).collect();
                    if successors.len() == 2 {
                        // Convention: true edge is first, false edge is second
                        return Some(successors[1].target());
                    } else {
                        // Non-binary successors - bail out
                        self.bail_out("Non-binary successor count for comparison", block_id);
                        return None;
                    }
                }
                _ => {}
            }
        }
        
        None
    }
    
    /// Bail out with logging for debugging
    fn bail_out(&self, reason: &str, block: NodeIndex) {
        log::debug!("Switch pattern detection bailed out at block {:?}: {}", block, reason);
    }
}
```

## Benefits of This Approach

1. **Simplicity**: No CFG mutations, no SSA reconstruction
2. **Correctness**: Fewer moving parts means fewer bugs
3. **Performance**: Single pass through the CFG
4. **Maintainability**: All logic in one place (AST generation)
5. **Debuggability**: Can easily trace what's happening

## Bail-Out Conditions

The pattern detection should bail out and fall back to if/else chains when:

1. **Non-constant comparisons**: Any comparison uses a non-constant value
2. **Changing discriminators**: The register being compared changes between comparisons
3. **Unsafe setup operations**: Setup includes operations that can throw, allocate, or have side effects
4. **Cross-edge liveness**: Setup values are used on the false path before redefinition
5. **Excessive cases**: More than 32 distinct case values (configurable threshold)
6. **Complex control flow**: The comparison chain has unexpected branches or loops
7. **NaN comparisons**: Any case compares against NaN (always false)
8. **Too many unique targets**: More than 8 distinct target blocks
9. **Non-finite constants**: Only NaN should cause bail-out; Infinity/-Infinity are valid
10. **Conflicting duplicates**: Same constant with different targets/setup (merge non-conflicting duplicates)
11. **Non-constant PHI scenarios**: When PHI inputs are expressions/non-constants we can't model
12. **Irreducible control flow**: When involved blocks form irreducible loops
13. **Exception handling**: When any involved blocks contain exception-throwing instructions

### Additional Safety Checks

```rust
fn should_bail_out_for_constants(&self, cases: &[CaseInfo]) -> bool {
    let mut constant_to_info: HashMap<CaseKey, (NodeIndex, SetupSignature)> = HashMap::new();
    
    // Check for conflicting duplicates and non-finite values
    for case in cases {
        for key in &case.keys {
            // Only bail on NaN, allow Infinity/-Infinity
            if let CaseKey::Num(OrderedFloat(n)) = key {
                if n.is_nan() {
                    self.bail_out("NaN case constant detected", case.target_block);
                    return true;
                }
            }
            
            let normalized_key = key.normalize_for_grouping();
            let setup_signature = self.normalize_setup(&case.setup, case.target_block);
            
            if let Some((existing_target, existing_signature)) = constant_to_info.get(&normalized_key) {
                // Check for conflicting targets or normalized setup
                if *existing_target != case.target_block || existing_signature != &setup_signature {
                    self.bail_out("Conflicting duplicate case constants", case.target_block);
                    return true;
                }
                // Non-conflicting duplicate - will be merged later
            } else {
                constant_to_info.insert(normalized_key, (case.target_block, setup_signature));
            }
        }
    }
    
    false
}

fn should_bail_out_for_phi_scenarios(&self, shared_tail: &Option<SharedTailInfo>) -> bool {
    if let Some(ref tail) = shared_tail {
        // Only bail if PHI inputs are non-constants/expressions we can't model
        for phi_node in tail.phi_nodes.values() {
            for value in phi_node.values.values() {
                if !self.is_modelable_constant(value) {
                    self.bail_out("Non-constant PHI input detected", tail.block_id);
                    return true;
                }
            }
        }
    }
    
    false
}

fn should_bail_out_for_control_flow(&self, involved_blocks: &HashSet<NodeIndex>) -> bool {
    // Check for exception-throwing instructions
    for &block_id in involved_blocks {
        let block = &self.cfg.graph()[block_id];
        for instr in block.instructions() {
            if self.instruction_can_throw(&instr.instruction) {
                self.bail_out("Exception-throwing instruction in switch region", block_id);
                return true;
            }
        }
    }
    
    // Check for irreducible loops (simplified check)
    if self.has_irreducible_control_flow(involved_blocks) {
        self.bail_out("Irreducible control flow detected", self.current_block);
        return true;
    }
    
    false
}

fn is_modelable_constant(&self, value: &ConstantValue) -> bool {
    // We can model simple constants but not complex expressions
    matches!(value, 
        ConstantValue::String(_) |
        ConstantValue::Number(_) |
        ConstantValue::Boolean(_) |
        ConstantValue::Null |
        ConstantValue::Undefined
    )
}
```

When bailing out, log the reason for debugging:

```rust
fn bail_out(&self, reason: &str, block: NodeIndex) {
    debug!("Switch pattern detection bailed out at block {:?}: {}", block, reason);
}
```

## Migration Path: Inline Replacement Strategy

Instead of maintaining parallel implementations, we'll replace the existing `SwitchConverter` inline:

1. **Phase 1**: Add tracer logging with auto-classification to compare outputs:
   ```rust
   enum DiffClassification {
       StructurallyEqual,    // AST-wise identical semantics
       SemanticallyEqual,    // Different structure, same behavior on finite domain
       Different,            // True semantic difference - needs investigation
   }
   
   fn classify_switch_diff(&self, old_ast: &Statement, new_ast: &Statement) -> DiffClassification {
       // Auto-classify structural vs semantic differences
   }
   ```

2. **Phase 2**: Replace pattern detection in existing `SwitchConverter`:
   - Keep same entry points and interfaces
   - Replace complex multi-pass detection with simplified single-pass
   - Gate rollout on zero semantic differences

3. **Phase 3**: Replace setup handling and grouping:
   - Swap out complex setup analysis with safety predicates
   - Replace case grouping with normalized setup signatures
   - Maintain existing test compatibility

4. **Phase 4**: Replace AST generation:
   - Swap out complex switch generation with simplified version
   - Always emit shared tail after switch (never inline in cases)
   - Use join locals for PHI node handling

5. **Phase 5**: Performance optimization and cleanup:
   - Add memoization for reachability checks
   - Remove dead code from old implementation
   - Tune thresholds based on corpus analysis

The key insight: Always have the current code in git history for easy revert, but don't maintain parallel codepaths that can diverge.

#### **Git Strategy for Inline Replacement**
```bash
# Recommended branch structure:
git checkout -b sparse-switch-simplification

# Make incremental commits for each phase:
git commit -m "Add switch pattern detection structs and safety predicates"
git commit -m "Replace pattern detection in SwitchConverter with single-pass"  
git commit -m "Replace setup analysis with safety-based filtering"
git commit -m "Replace AST generation with simplified switch emission"
git commit -m "Add performance optimizations and cleanup dead code"

# Each commit should maintain working tests - easier bisection if issues arise
```

## Example Outputs

### Example 1: Simple fallthrough with guaranteed termination

```javascript
// Before
function switchWithFallthrough(arg0) {
    let r2;
    if (arg0 === 0) { r2 = "low "; goto L1; }
    if (arg0 === 1) { r2 = "low "; goto L1; }
    if (arg0 === 2) { r2 = "low "; goto L1; }
    if (arg0 === 3) { r2 = ""; goto L1; }
    if (arg0 === 4) { r2 = ""; goto L1; }
    if (arg0 === 5) return "high";
    if (arg0 === 6) return "high";
    if (arg0 === 7) return "high";
    return "unknown";
L1:
    return r2 + "mid ";
}

// After (shared-tail + join locals approach)
function switchWithFallthrough(x) {
  let _r2;
  switch (x) {
    case 0:
    case 1:
    case 2:
      _r2 = "low ";
      break;
    case 3:
    case 4:
      _r2 = "";
      break;
    case 5:
    case 6:
    case 7:
      return "high";
    default:
      return "unknown";
  }
  return _r2 + "mid ";
}

// After (with constant-folding pass)
function switchWithFallthrough(x) {
  switch (x) {
    case 0:
    case 1:
    case 2:
      return "low mid ";
    case 3:
    case 4:
      return "mid ";
    case 5:
    case 6:
    case 7:
      return "high";
    default:
      return "unknown";
  }
}
```

### Example 2: Shared tail with join locals

```javascript
// Before
function processValue(x) {
    let r2, r3;
    if (x === 0) { r2 = "zero"; r3 = 0; goto L1; }
    if (x === 1) { r2 = "one"; r3 = 1; goto L1; }
    if (x === 2) { r2 = "two"; r3 = 2; goto L1; }
    r2 = "other"; r3 = -1;
L1:
    console.log(r2);
    return r3 * 100;
}

// After (with join locals)
function processValue(x) {
    let _r2, _r3;
    switch (x) {
        case 0:
            _r2 = "zero";
            _r3 = 0;
            break;
        case 1:
            _r2 = "one";
            _r3 = 1;
            break;
        case 2:
            _r2 = "two";
            _r3 = 2;
            break;
        default:
            _r2 = "other";
            _r3 = -1;
            break;
    }
    console.log(_r2);
    return _r3 * 100;
}
```

### Example 3: Unsafe setup that must bail out

```javascript
// This pattern should NOT be converted to switch
// because getString() might throw
function unsafeSwitch(x) {
    let s;
    if (x === 0) { s = getString(); return process(s); }
    if (x === 1) { s = getString(); return process(s); }
    return "default";
}
// Output: Keeps if/else chain due to non-pure setup
```

## Testing Strategy

1. **Golden Tests**: Create test cases for:
   - Simple fallthrough patterns
   - Shared tail with PHI requirements
   - Default case with setup
   - Large sparse sets (test grouping)
   - Unsafe setup that should bail out
   - Setup reused by later comparisons
   - Default case equals shared tail scenario
   - Multiple registers converging to PHIs at join point

2. **Corpus Test Cases**:

### Test Case: Default == Shared Tail
```javascript
// This tests the scenario where default case is the same as shared tail
function defaultEqualsSharedTail(x) {
    let result;
    if (x === 1) { result = "one"; goto shared; }
    if (x === 2) { result = "two"; goto shared; }
    // Default case falls through to shared tail
    result = "default";
shared:
    console.log(result);
    return result.toUpperCase();
}
```

### Test Case: Multiple Registers to PHIs
```javascript
// This tests setup on multiple registers converging to PHI functions
function multipleRegistersPhi(x) {
    let a, b;
    if (x === 1) { a = "one"; b = 1; goto join; }
    if (x === 2) { a = "two"; b = 2; goto join; }
    if (x === 3) { a = "three"; b = 3; goto join; }
    a = "default"; b = 0;
join:
    return a + ": " + b;
}
```

### Test Case: Mixed Type Keys
```javascript
// This tests mixed string and numeric case labels
function mixedTypeSwitch(x) {
    let result;
    if (x === "a") { result = "string_a"; goto end; }
    if (x === "b") { result = "string_b"; goto end; }
    if (x === 0) { result = "number_0"; goto end; }
    if (x === 1) { result = "number_1"; goto end; }
    result = "default";
end:
    return result;
}
```

### Test Case: Immediate Tail Redefinition
```javascript
// This tests PHI suppression when tail immediately redefines register
// No join local for r2 should be emitted since tail overwrites it immediately
function immediateTailRedef(x) {
    let r2;
    if (x === 0) { r2 = "a"; goto tail; }
    if (x === 1) { r2 = "b"; goto tail; }
    r2 = "default_value";
tail:
    r2 = "overwritten"; // Immediate redefinition - no PHI needed
    return r2;
}
```

3. **Symbolic Validation**: For each test function:
   - Execute original bytecode on finite domain: {all case constants} ∪ {one "other" value}
   - Execute generated JavaScript on same inputs
   - Compare outputs to ensure semantic preservation

4. **Performance Tests**:
   - Measure compilation time with/without optimization
   - Ensure reasonable thresholds for case counts
   - Verify memory usage for large switches
   - Include at least one "other" value that exercises default path

5. **Edge Liveness Optimization**:
   - Precompute per-block forward liveness once
   - Use helper `live_on_false_edge(reg, cmp_block)` for efficiency

6. **Performance Hotspots**:
   - Precompute reachability within involved_blocks (bitset or dominance summary)
   - Memoize reachability checks per (start, barrier) to avoid N×M BFS explosions
   - Constrain all walks to switch region boundaries to avoid false negatives through unrelated code

## Conclusion

This refined design addresses all the key safety concerns while dramatically simplifying the existing 2200+ line switch converter. By adding:

- Strict safety predicates for setup localization
- PHI analysis with join locals for shared tails  
- Proper grouping by normalized setup (register→value maps)
- Clear bail-out conditions with comprehensive logging
- Defensive use of fallthrough only when guaranteed safe
- Leveraging existing SSA analysis infrastructure

We achieve clean, correct JavaScript output while reducing complexity by ~80%. The design builds on proven existing infrastructure (`SSAAnalysis`, `BlockToStatementConverter`) and is ready for implementation as a drop-in replacement for the current `SwitchConverter`.