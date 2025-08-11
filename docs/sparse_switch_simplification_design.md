# Sparse Switch Handling Simplification Design

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
    /// Setup instructions needed for this case
    setup: Vec<SetupInstruction>,
    /// The target block for this case
    target_block: NodeIndex,
    /// Whether this case's body always terminates (return/throw)
    guaranteed_termination: bool,
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
    /// Numeric case key
    Num(OrderedFloat<f64>),
    /// String case key (for string switches)
    Str(std::rc::Rc<str>),
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
    // Note: Edge liveness would need to be implemented or approximated
}

impl<'a> SetupSafetyChecker<'a> {
    /// Check if a setup instruction is case-localizable
    fn is_case_localizable(
        &self,
        setup: &SetupInstruction,
        true_successor: NodeIndex,
        false_successor: NodeIndex,
        compare_block: NodeIndex,
        involved_blocks: &HashSet<NodeIndex>,
    ) -> bool {
        // 1. Purity check - only allow safe opcodes
        if !self.is_pure_setup(&setup.opcode) {
            return false;
        }
        
        // 2. Taken-only use check - all uses must be dominated by true successor
        if !self.all_uses_dominated_by(setup.register, true_successor, involved_blocks) {
            return false;
        }
        
        // 3. False-path kill check
        if self.is_live_on_false_edge(setup.register, compare_block, false_successor) {
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
    
    /// Check if all uses of a register are dominated by a block
    fn all_uses_dominated_by(&self, register: u8, true_successor: NodeIndex, involved_blocks: &HashSet<NodeIndex>) -> bool {
        // Every use must be dominated by the true successor
        // i.e., there is no path from the compare to a use that avoids the true edge
        self.ssa.get_register_uses(register)
            .iter()
            .filter(|use_site| involved_blocks.contains(&use_site.block_id)) // Only consider uses in switch region
            .all(|use_site| {
                // Check if use_site.block is reachable from the false successor without going through true_successor
                !self.is_reachable_without_passing_through(
                    self.get_false_successor_from_compare(use_site.block_id),
                    use_site.block_id,
                    true_successor
                )
            })
    }
    
    /// Check if target is reachable from start without passing through barrier
    fn is_reachable_without_passing_through(&self, start: NodeIndex, target: NodeIndex, barrier: NodeIndex) -> bool {
        if start == barrier || target == barrier {
            return false;
        }
        
        let mut visited = HashSet::new();
        let mut to_visit = vec![start];
        
        while let Some(current) = to_visit.pop() {
            if current == target {
                return true;
            }
            
            if current == barrier || !visited.insert(current) {
                continue;
            }
            
            // Add successors
            for edge in self.cfg.graph().edges(current) {
                to_visit.push(edge.target());
            }
        }
        
        false
    }
    
    /// Check if register is live on the false edge
    fn is_live_on_false_edge(
        &self,
        register: u8,
        from: NodeIndex, 
        to: NodeIndex,
    ) -> bool {
        // Walk the false path and check if register is used before being redefined
        let mut current = to;
        let mut visited = HashSet::new();
        
        // Bounded walk along false path to next compare/default block
        while visited.len() < 10 && visited.insert(current) { // Limit to prevent infinite loops
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
            
            // Move to next block in false chain, stop at comparison or multi-successor blocks
            let successors: Vec<_> = self.cfg.graph().edges(current).collect();
            if successors.len() != 1 {
                break; // Reached comparison or join point
            }
            current = successors[0].target();
        }
        
        // Default to not live if we can't determine
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
        let discriminator = self.find_discriminator(first_block)?;
        
        // Safety checker for setup instructions
        let safety_checker = SetupSafetyChecker {
            cfg: &self.cfg,
            ssa: &self.ssa,
            postdom: &self.postdom,
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
            if let Some(mut case_info) = self.extract_case_from_block(block, discriminator) {
                // Filter setup instructions for safety
                let true_successor = case_info.target_block;
                let false_successor = self.get_false_successor(current_block)?;
                
                case_info.setup.retain(|setup| {
                    let is_safe = safety_checker.is_case_localizable(
                        setup,
                        true_successor,
                        false_successor,
                        current_block,
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
        
        // Extract default case setup if present
        let default_case = if let Some(last_compare) = last_compare_block {
            self.extract_default_setup(current_block, last_compare, &live_across_false)
                .map(|setup| DefaultCase {
                    target_block: current_block,
                    setup,
                })
        } else {
            None
        };
        
        // Detect shared tail block
        let shared_tail = self.detect_shared_tail(&cases, &default_case);
        
        Some(SwitchInfo {
            discriminator,
            cases,
            default_case,
            involved_blocks,
            shared_tail,
        })
    }
    
    /// Find what register is being used as discriminator
    fn find_discriminator(&self, block: &Block) -> Option<u8> {
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
        for inst in block.instructions() {
            match &inst.instruction {
                UnifiedInstruction::JStrictEqual { operand_0, operand_1, .. } |
                UnifiedInstruction::JStrictEqualLong { operand_0, operand_1, .. } => {
                    // Hermes can compare (const, reg) or (reg, const)
                    // Find which operand is the register
                    let op0_is_const = self.get_constant_value(*operand_0).is_some();
                    let op1_is_const = self.get_constant_value(*operand_1).is_some();
                    
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
        discriminator: u8
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
                    if let Some(const_val) = self.get_constant_value(*operand_1) {
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
                    
                    // Extract constant value
                    case_value = self.get_constant_value(const_operand)
                        .and_then(|v| match v {
                            ConstantValue::Number(OrderedFloat(n)) => {
                                // Check for NaN - these comparisons are always false
                                if n.is_nan() {
                                    return None;  // Bail on NaN comparisons
                                }
                                Some(CaseKey::Num(OrderedFloat(self.normalize_zero(n))))
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
        
        // Find all registers consumed in the tail
        let consumed_registers = self.find_consumed_registers(tail);
        
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
            let case_keys = group.cases.iter()
                .flat_map(|c| &c.keys)
                .cloned()
                .collect::<Vec<_>>();
            
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
    
    /// Group cases by normalized setup and target
    fn group_cases_by_normalized_setup(&self, cases: &[CaseInfo]) -> Vec<CaseGroup> {
        let mut groups: HashMap<SetupSignature, Vec<&CaseInfo>> = HashMap::new();
        
        for case in cases {
            let signature = self.normalize_setup(&case.setup, case.target_block);
            groups.entry(signature).or_insert_with(Vec::new).push(case);
        }
        
        groups.into_iter()
            .map(|(setup_sig, cases)| {
                // Build deduplicated setup from the signature
                let shared_setup = self.signature_to_setup(&setup_sig);
                
                CaseGroup {
                    cases: cases.into_iter().cloned().collect(),
                    shared_setup,
                    target: setup_sig.target,
                    guaranteed_termination: cases[0].guaranteed_termination,
                }
            })
            .collect()
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
                    source_instruction: 0,  // Will be updated if needed
                    opcode: self.value_to_opcode(value),
                }
            })
            .collect()
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
            CaseKey::Num(OrderedFloat(n)) => {
                if n.is_sign_negative() && *n == 0.0 {
                    // Preserve -0 with unary minus
                    self.ast_builder.unary_expression(
                        UnaryOperator::Minus,
                        self.ast_builder.literal_number(0.0),
                    )
                } else {
                    self.ast_builder.literal_number(*n)
                }
            }
            CaseKey::Str(ref s) => {
                self.ast_builder.literal_string(s.as_ref())
            }
        }
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
    /// Get the constant value that a register holds (if any)
    fn get_constant_value(&self, register: u8) -> Option<ConstantValue> {
        // Use existing SSA use-def chains from the actual SSA analysis
        let use_site = crate::cfg::ssa::RegisterUse::new(
            register, 
            self.current_block, 
            self.current_instruction
        );
        
        if let Some(def_site) = self.ssa.use_def_chains.get(&use_site) {
            // Look at the defining instruction
            let def_block = &self.cfg.graph()[def_site.block_id];
            let def_inst = &def_block.instructions()[def_site.instruction_idx];
            
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
                // ... other constant loads ...
                _ => None
            }
        } else {
            None
        }
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
9. **Huge constant labels**: When case constants exceed 2³¹ or are non-integer types
10. **Duplicate case constants**: When the same constant appears in multiple case blocks
11. **Complex phi scenarios**: When multiple registers being tested converge to phi functions at the join block
12. **Irreducible control flow**: When involved blocks form irreducible loops
13. **Exception handling**: When any involved blocks contain exception-throwing instructions

### Additional Safety Checks

```rust
fn should_bail_out_for_constants(&self, case_constants: &[i64]) -> bool {
    // Check for huge constants (exceeding safe 32-bit range)
    if case_constants.iter().any(|&c| c > i32::MAX as i64 || c < i32::MIN as i64) {
        self.bail_out("Huge constant labels detected", self.current_block);
        return true;
    }
    
    // Check for duplicate constants
    let mut seen = HashSet::new();
    for &constant in case_constants {
        if !seen.insert(constant) {
            self.bail_out("Duplicate case constants detected", self.current_block);
            return true; // Duplicate found
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
```

When bailing out, log the reason for debugging:

```rust
fn bail_out(&self, reason: &str, block: NodeIndex) {
    debug!("Switch pattern detection bailed out at block {:?}: {}", block, reason);
}
```

## Migration Path

1. **Phase 0.5**: Add tracer logging behind a flag to compare current vs new switch output
2. **Phase 1**: Implement safety predicates and pattern detection to replace existing complex heuristics
3. **Phase 2**: Add PHI analysis and join local generation for shared tail handling
4. **Phase 3**: Implement simplified switch generation with proper grouping
5. **Phase 4**: Add comprehensive test coverage and validation against existing tests
6. **Phase 5**: Replace existing `SwitchConverter` implementation with simplified version
7. **Phase 6**: Enable by default once validated on real corpora
8. **Phase 7**: Performance optimization and threshold tuning

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

// After (with this design)
function switchWithFallthrough(arg0) {
    switch (arg0) {
        case 0:
        case 1:
        case 2:
            return "low " + "mid ";
        case 3:
        case 4:
            return "" + "mid ";
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

## Conclusion

This refined design addresses all the key safety concerns while dramatically simplifying the existing 2200+ line switch converter. By adding:

- Strict safety predicates for setup localization
- PHI analysis with join locals for shared tails  
- Proper grouping by normalized setup (register→value maps)
- Clear bail-out conditions with comprehensive logging
- Defensive use of fallthrough only when guaranteed safe
- Leveraging existing SSA analysis infrastructure

We achieve clean, correct JavaScript output while reducing complexity by ~80%. The design builds on proven existing infrastructure (`SSAAnalysis`, `BlockToStatementConverter`) and is ready for implementation as a drop-in replacement for the current `SwitchConverter`.