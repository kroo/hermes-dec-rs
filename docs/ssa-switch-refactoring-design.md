# SSA Usage Tracker and Switch Analysis Refactoring Design

## Overview

This document outlines the refactoring of SSA usage tracking and switch analysis to properly handle variable declarations, block duplication, and value elimination. The core insight is that these decisions must be made during analysis phases BEFORE AST generation, not during it.

## Current Problems

1. **Late Decision Making**: Critical decisions (what to duplicate, what to inline, where to declare) are made during AST generation
2. **Mixed Concerns**: Switch converter does both analysis and AST generation
3. **Incorrect Duplication**: All variables in duplicated blocks get duplicated names, even when they shouldn't
4. **Declaration Issues**: Variables declared multiple times or in wrong scopes

## Proposed Architecture

### Phase 1: Analysis (Pre-AST)
All analysis happens before any AST generation:
- CFG analysis
- SSA analysis
- Switch pattern detection
- Conversion planning

### Phase 2: AST Generation
Pure transformation from analysis results to AST:
- No analysis decisions
- Just executes pre-computed plans

## Component Design

### 1. SSAUsageTracker

The SSAUsageTracker determines declaration strategies and use strategies for SSA values.

#### Declaration Strategy (at definition sites)

```rust
pub enum DeclarationStrategy {
    /// Skip - all uses have been inlined/eliminated
    Skip,
    
    /// Declare at dominator point (for PHI nodes with no single dominating def)
    DeclareAtDominator {
        dominator_block: NodeIndex,
        kind: VariableKind, // let/const
    },
    
    /// Declare and initialize at this definition site
    DeclareAndInitialize {
        kind: VariableKind, // let/const  
    },
    
    /// Just assign - variable already declared elsewhere
    AssignOnly,
}

pub enum VariableKind {
    Let,
    Const,
}
```

#### Use Strategy (at use sites)

```rust
pub enum UseStrategy {
    /// Reference the variable by name
    UseVariable,
    
    /// Inline the constant value directly
    InlineValue(ConstantValue),
}
```

#### Key Methods

```rust
impl SSAUsageTracker {
    /// Determine strategy for a (potentially duplicated) SSA value at its definition site
    pub fn get_declaration_strategy(&self, dup_ssa_value: &DuplicatedSSAValue) -> DeclarationStrategy {
        let ssa_value = dup_ssa_value.original_ssa_value();
        
        // Special handling for PHI result values
        // PHI results themselves don't generate code - the actual assignments do
        if self.is_phi_result(ssa_value) {
            return DeclarationStrategy::Skip;
        }
        
        // 1. Check if fully eliminated (considering duplication context)
        if self.is_fully_eliminated_duplicated(dup_ssa_value) {
            return DeclarationStrategy::Skip;
        }
        
        // 2. Check if part of PHI group (coalesced values)
        if let Some(coalesced_rep) = self.var_analysis.coalesced_values.get(ssa_value) {
            // Check if this is the declaration point
            if self.is_declaration_point(ssa_value, coalesced_rep) {
                // Check if needs dominator declaration
                if self.needs_dominator_declaration(coalesced_rep) {
                    return DeclarationStrategy::DeclareAtDominator { ... };
                } else {
                    return DeclarationStrategy::DeclareAndInitialize { ... };
                }
            } else {
                return DeclarationStrategy::AssignOnly;
            }
        }
        
        // 3. Single SSA value - declare at definition
        return DeclarationStrategy::DeclareAndInitialize { ... };
    }
    
    /// Determine strategy for using a (potentially duplicated) SSA value
    pub fn get_use_strategy(
        &self, 
        dup_ssa_value: &DuplicatedSSAValue,
        use_site: &RegisterUse,
    ) -> UseStrategy {
        // Check if this use has been marked as consumed/inlined
        if self.is_use_consumed_duplicated(dup_ssa_value, use_site) {
            if let Some(const_val) = self.get_constant_value(dup_ssa_value.original_ssa_value()) {
                return UseStrategy::InlineValue(const_val);
            }
        }
        
        return UseStrategy::UseVariable;
    }
}
```

### 2. ControlFlowPlan

The ControlFlowPlan captures all control flow patterns and analysis decisions before AST generation. This essentially becomes a high-level IR between CFG analysis and AST generation.

```rust
pub struct ControlFlowPlan {
    /// How each block should be converted (its control flow pattern)
    pub block_patterns: HashMap<NodeIndex, BlockPattern>,
    
    /// Which blocks will be duplicated and in what contexts
    pub block_duplications: HashMap<NodeIndex, Vec<DuplicationContext>>,
    
    /// Variables that need declaration at block entry points
    pub block_declarations: HashMap<NodeIndex, Vec<DuplicatedSSAValue>>,
    
    /// Declaration strategy for each (potentially duplicated) SSA value
    pub declaration_strategies: HashMap<DuplicatedSSAValue, DeclarationStrategy>,
    
    /// Which uses have been consumed (will be inlined)
    pub consumed_uses: HashMap<DuplicatedSSAValue, HashSet<RegisterUse>>,
}

pub enum BlockPattern {
    /// Part of a switch statement
    SwitchCase {
        dispatch_block: NodeIndex,
        case_info: CaseInfo,
        fallthrough: Option<FallthroughInfo>,
    },
    
    /// Part of a conditional chain (if-else)
    ConditionalBranch {
        chain_head: NodeIndex,
        branch_info: BranchInfo,
    },
    
    /// Loop header or body
    Loop {
        loop_type: LoopType,
        loop_info: LoopInfo,
    },
    
    /// Try/catch/finally blocks
    TryCatch {
        try_block: NodeIndex,
        catch_info: Option<CatchInfo>,
        finally_block: Option<NodeIndex>,
    },
    
    /// Simple sequential block (no special control flow)
    Sequential,
}
```

### 3. Switch Analysis Enhancements

Move analysis from switch_converter to cfg/switch_analysis:

```rust
pub struct SwitchConversionPlan {
    /// Basic switch information
    pub switch_info: SwitchInfo,
    
    /// Fallthrough duplications needed
    pub fallthrough_duplications: Vec<FallthroughDuplication>,
    
    /// Blocks shared between multiple case groups
    pub shared_blocks: HashSet<NodeIndex>,
    
    /// Nested switches within case bodies
    pub nested_switches: Vec<NestedSwitchInfo>,
    
    /// PHI contributions from each case
    pub phi_contributions: HashMap<CaseGroup, HashMap<u8, ConstantValue>>,
    
    /// Which setup instructions will be inlined
    pub inlined_setup_instructions: Vec<(SSAValue, RegisterUse)>,
}

pub struct FallthroughDuplication {
    pub from_case_group: CaseGroup,
    pub to_case_group: CaseGroup,
    pub blocks_to_duplicate: Vec<NodeIndex>,
    pub duplication_context: DuplicationContext,
}

pub struct NestedSwitchInfo {
    pub parent_case: CaseGroup,
    pub nested_dispatch_block: NodeIndex,
    pub eliminated_values: HashSet<SSAValue>,
}
```

### 4. Analysis Pipeline

```rust
pub fn analyze_function(func: &Function) -> Result<ConversionPlan> {
    // 1. CFG construction
    let cfg = build_cfg(func)?;
    
    // 2. SSA analysis
    let ssa = analyze_ssa(&cfg)?;
    
    // 3. Variable analysis (coalescing, scopes)
    let var_analysis = analyze_variables(&ssa, &cfg)?;
    
    // 4. Switch analysis (including duplications)
    let switch_analysis = analyze_switches(&cfg, &ssa)?;
    
    // 5. Build conversion plan
    let mut plan = ConversionPlan::new();
    
    // Add switch plans
    for region in switch_analysis.regions {
        let switch_plan = create_switch_conversion_plan(&region, &cfg, &ssa)?;
        plan.switch_plans.insert(region.dispatch, switch_plan);
        
        // Record block duplications
        for dup in &switch_plan.fallthrough_duplications {
            for block in &dup.blocks_to_duplicate {
                plan.block_duplications.entry(*block)
                    .or_default()
                    .push(BlockDuplication { ... });
            }
        }
    }
    
    // 6. Compute declaration strategies
    compute_declaration_strategies(&mut plan, &ssa, &var_analysis)?;
    
    // 7. Determine consumed uses
    compute_consumed_uses(&mut plan, &ssa)?;
    
    Ok(plan)
}
```

### 5. Simplified Switch Converter

With all analysis pre-computed, switch_converter becomes much simpler:

```rust
impl SwitchConverter {
    pub fn convert_switch(
        &mut self,
        dispatch_block: NodeIndex,
        plan: &SwitchConversionPlan,
        block_converter: &mut BlockConverter,
    ) -> Result<Statement> {
        // No analysis here - just use the plan
        
        let discriminator = self.create_discriminator_expr(&plan.switch_info)?;
        let mut cases = vec![];
        
        for case_group in &plan.switch_info.case_groups {
            // Check if this case needs fallthrough duplication
            if let Some(dup) = plan.find_fallthrough_for_case(case_group) {
                // Set duplication context
                block_converter.set_duplication_context(&dup.duplication_context);
                
                // Convert duplicated blocks
                let stmts = block_converter.convert_blocks(&dup.blocks_to_duplicate)?;
                
                // Clear duplication context
                block_converter.clear_duplication_context();
                
                cases.push(self.create_case(case_group, stmts)?);
            } else {
                // Normal case conversion
                let stmts = block_converter.convert_block(case_group.target_block)?;
                cases.push(self.create_case(case_group, stmts)?);
            }
        }
        
        Ok(self.create_switch_statement(discriminator, cases))
    }
}
```

## Implementation Steps

1. **Extract analysis functions** from switch_converter to cfg/switch_analysis
2. **Implement ConversionPlan** generation in FunctionAnalysis
3. **Update SSAUsageTracker** to use pre-computed plans
4. **Refactor switch_converter** to use plans instead of doing analysis
5. **Update analyze-cfg** command to show the analysis results
6. **Fix duplication context** to only apply to variables defined in duplicated blocks

## Benefits

1. **Correctness**: Decisions made with full context before generation
2. **Clarity**: Clear separation between analysis and generation
3. **Testability**: Can test analysis independently from AST generation
4. **Performance**: Analysis can be cached/reused
5. **Debuggability**: Can inspect analysis results before generation

## Migration Strategy

We can implement this incrementally:
1. Start by extracting analysis functions without changing behavior
2. Build ConversionPlan alongside existing code
3. Gradually migrate converters to use the plan
4. Remove old analysis code from converters

This allows us to maintain working code while refactoring.