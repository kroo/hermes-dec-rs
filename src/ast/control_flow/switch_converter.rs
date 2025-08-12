//! Switch statement conversion from CFG switch regions (Simplified Design)
//!
//! This module converts switch regions detected in the CFG into JavaScript switch statements
//! using a simplified single-pass approach as described in the design document.

use crate::cfg::analysis::PostDominatorAnalysis;
use crate::cfg::ssa::SSAAnalysis;
use crate::cfg::{analysis::SwitchRegion, Cfg};
use crate::generated::instruction_analysis::analyze_register_usage;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::function_table::HbcFunctionInstruction;
use crate::hbc::InstructionIndex;
use ordered_float::OrderedFloat;
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::{
    ast::{
        AssignmentOperator, AssignmentTarget, Expression, SimpleAssignmentTarget, Statement,
        SwitchCase,
    },
    AstBuilder as OxcAstBuilder,
};
use oxc_span::Span;
use oxc_syntax::number::NumberBase;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Error types for switch conversion
#[derive(Debug, thiserror::Error)]
pub enum SwitchConversionError {
    #[error("Invalid switch region: {0}")]
    InvalidRegion(String),
    #[error("Missing switch instruction")]
    MissingSwitchInstruction,
    #[error("Failed to convert case: {0}")]
    CaseConversionError(String),
    #[error("Switch too complex: {0}")]
    TooComplex(String),
}

/// Information about a switch pattern detected in the CFG (simplified approach)
#[derive(Debug, Clone)]
pub struct SwitchInfo {
    /// The register being switched on
    pub discriminator: u8,
    /// Information about each case
    pub cases: Vec<CaseInfo>,
    /// Default case target and setup (if any)
    pub default_case: Option<DefaultCase>,
    /// Blocks that are part of this switch pattern
    pub involved_blocks: HashSet<NodeIndex>,
    /// Shared tail block that multiple cases jump to (if any)
    pub shared_tail: Option<SharedTailInfo>,
}

/// Information about a single case or group of cases
#[derive(Debug, Clone)]
pub struct CaseInfo {
    /// The case keys (numeric or string)
    pub keys: Vec<CaseKey>,
    /// Setup instructions needed for this case (SmallVec for memory efficiency)
    pub setup: SmallVec<[SetupInstruction; 4]>,
    /// The target block for this case
    pub target_block: NodeIndex,
    /// Whether this case's body always terminates (return/throw)
    pub always_terminates: bool,
    /// Source compare block PC for deterministic ordering
    pub source_pc: InstructionIndex,
    /// Execution order index (order in which comparisons are evaluated)
    pub execution_order: usize,
    /// The comparison block for this case (for PHI tracking)
    pub comparison_block: NodeIndex,
}

/// Default case information
#[derive(Debug, Clone)]
pub struct DefaultCase {
    /// Target block for default
    pub target_block: NodeIndex,
    /// Setup instructions for default (often after last compare)
    pub setup: SmallVec<[SetupInstruction; 4]>,
}

/// Information about a shared tail block
#[derive(Debug, Clone)]
pub struct SharedTailInfo {
    /// The block that multiple cases converge to
    pub block_id: NodeIndex,
    /// PHI nodes needed (register -> case-specific values)
    pub phi_nodes: HashMap<u8, PhiNode>,
}

/// PHI node information for join locals
#[derive(Debug, Clone)]
pub struct PhiNode {
    /// The register being phi'd
    pub register: u8,
    /// Values per predecessor block
    pub values: HashMap<NodeIndex, ConstantValue>,
    /// SSA value that represents this PHI (if available)
    pub ssa_phi_value: Option<crate::cfg::ssa::SSAValue>,
}

/// A group of consecutive cases that share the same behavior
#[derive(Debug, Clone)]
pub struct CaseGroup {
    /// The case keys in this group (in execution order)
    pub keys: Vec<CaseKey>,
    /// The shared target block
    pub target_block: NodeIndex,
    /// The shared setup instructions
    pub setup: SmallVec<[SetupInstruction; 4]>,
    /// Whether this group's body always terminates
    pub always_terminates: bool,
    /// The execution order of the first case in the group
    pub first_execution_order: usize,
    /// The comparison blocks for each case in this group (parallel to keys)
    pub comparison_blocks: Vec<NodeIndex>,
}

/// A setup instruction that needs to be emitted in a case
#[derive(Debug, Clone)]
pub struct SetupInstruction {
    /// The SSA value being defined
    pub ssa_value: crate::cfg::ssa::SSAValue,
    /// The value to assign
    pub value: ConstantValue,
    /// Original instruction (for comments if needed)
    pub instruction: HbcFunctionInstruction,
}

/// Case key types for switch statements
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CaseKey {
    /// Numeric case key (supports Infinity/-Infinity)
    Num(OrderedFloat<f64>),
    /// String case key (for string switches)
    /// Note: Rc<str> implements Eq by value comparison (string contents), not pointer identity
    Str(Rc<str>),
}

impl CaseKey {
    /// Normalize for grouping (treat +0 and -0 as same key)
    pub fn normalize_for_grouping(&self) -> Self {
        match self {
            CaseKey::Num(n) if n.0 == 0.0 => CaseKey::Num(OrderedFloat(0.0)), // Normalize -0 to +0
            _ => self.clone(),
        }
    }
}

/// Constant values that can be set up
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

/// Context for comparison instruction analysis
#[derive(Debug, Clone)]
pub struct CompareContext {
    pub compare_block: NodeIndex,
    pub true_successor: NodeIndex,
    pub false_successor: NodeIndex,
    pub discriminator: u8,
}

/// Determines if a setup instruction is safe to sink into a case
pub struct SetupSafetyChecker<'a> {
    #[allow(dead_code)]
    cfg: &'a Cfg<'a>,
    #[allow(dead_code)]
    ssa: &'a SSAAnalysis,
    #[allow(dead_code)]
    postdom: &'a PostDominatorAnalysis,
    #[allow(dead_code)]
    reachability_cache: RefCell<HashMap<(NodeIndex, NodeIndex), HashSet<NodeIndex>>>,
}

impl<'a> SetupSafetyChecker<'a> {
    pub fn new(cfg: &'a Cfg<'a>, ssa: &'a SSAAnalysis, postdom: &'a PostDominatorAnalysis) -> Self {
        Self {
            cfg,
            ssa,
            postdom,
            reachability_cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn is_case_localizable(&self, setup_instr: &SetupInstruction) -> bool {
        // Simplified safety check - for now, allow all constant loads and moves
        // In a full implementation, this would check:
        // 1. No observable effects between setup and use
        // 2. Setup instruction is anchored to the comparison
        // 3. No interference with other register definitions
        matches!(
            &setup_instr.instruction.instruction,
            UnifiedInstruction::LoadConstString { .. }
                | UnifiedInstruction::LoadConstStringLongIndex { .. }
                | UnifiedInstruction::LoadConstUInt8 { .. }
                | UnifiedInstruction::LoadConstInt { .. }
                | UnifiedInstruction::LoadConstDouble { .. }
                | UnifiedInstruction::LoadConstZero { .. }
                | UnifiedInstruction::LoadConstNull { .. }
                | UnifiedInstruction::LoadConstUndefined { .. }
                | UnifiedInstruction::LoadConstTrue { .. }
                | UnifiedInstruction::LoadConstFalse { .. }
                | UnifiedInstruction::Mov { .. }
        )
    }

    /// Check if an instruction uses a register
    #[allow(dead_code)]
    fn instruction_uses_register(&self, instr: &UnifiedInstruction, register: u8) -> bool {
        // Use existing register analysis to determine register usage
        let usage = analyze_register_usage(instr);
        usage.sources.contains(&register)
    }

    /// Check if an instruction defines a register
    #[allow(dead_code)]
    fn instruction_defines_register(&self, instr: &UnifiedInstruction, register: u8) -> bool {
        // Use existing register analysis to determine if register is defined
        let usage = analyze_register_usage(instr);
        usage.target == Some(register)
    }
}

/// Switch statement converter using the simplified design
pub struct SwitchConverter<'a> {
    ast_builder: &'a OxcAstBuilder<'a>,
    expression_context: Option<crate::ast::context::ExpressionContext<'a>>,
}

impl<'a> SwitchConverter<'a> {
    /// Create a new switch converter
    pub fn new(ast_builder: &'a OxcAstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            expression_context: None,
        }
    }

    /// Create a new switch converter with expression context
    pub fn with_context(
        ast_builder: &'a OxcAstBuilder<'a>,
        expression_context: crate::ast::context::ExpressionContext<'a>,
    ) -> Self {
        Self {
            ast_builder,
            expression_context: Some(expression_context),
        }
    }

    /// Convert a switch region to statements (compatibility method for block converter)
    pub fn convert_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        // Check if this is a dense switch (SwitchImm instruction)
        let dispatch_block = &cfg.graph()[region.dispatch];
        let has_switch_imm = dispatch_block
            .instructions()
            .iter()
            .any(|instr| matches!(&instr.instruction, UnifiedInstruction::SwitchImm { .. }));

        if has_switch_imm {
            // This is a dense switch - convert it to AST
            return self.convert_dense_switch_region(region, cfg, block_converter);
        }

        // For sparse switch regions, we need to find all comparison blocks
        // Start from dispatch block and follow the chain
        let mut comparison_blocks = HashSet::new();
        let mut current = region.dispatch;
        let mut visited = HashSet::new();

        // Collect all blocks that are part of the comparison chain
        loop {
            if !visited.insert(current) {
                break;
            }

            comparison_blocks.insert(current);
            let block = &cfg.graph()[current];

            // Check if this block has a comparison
            let has_comparison = block.instructions().iter().any(|instr| {
                matches!(
                    &instr.instruction,
                    UnifiedInstruction::JStrictEqual { .. }
                        | UnifiedInstruction::JStrictEqualLong { .. }
                        | UnifiedInstruction::JStrictNotEqual { .. }
                        | UnifiedInstruction::JStrictNotEqualLong { .. }
                )
            });

            if !has_comparison {
                break;
            }

            // Follow false edge to next comparison
            if let Some(next) = self.get_false_successor(current, cfg) {
                // Check if next block is still part of comparison chain
                if next != region.join_block && !region.cases.iter().any(|c| c.case_head == next) {
                    current = next;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Mark all comparison block instructions as rendered
        for block_id in &comparison_blocks {
            let block = &cfg.graph()[*block_id];
            for instruction in block.instructions() {
                block_converter.mark_instruction_rendered(instruction);
            }
        }

        // Create SSA and postdominator analysis for pattern detection using existing infrastructure
        let ssa_analysis = crate::cfg::ssa::construct_ssa(cfg, 0).map_err(|e| {
            SwitchConversionError::TooComplex(format!("SSA construction failed: {}", e))
        })?;

        // Use existing postdominator analysis from cfg builder
        let postdom_analysis = cfg
            .builder()
            .analyze_post_dominators(cfg.graph())
            .ok_or_else(|| {
                SwitchConversionError::TooComplex(
                    "Failed to compute postdominator analysis".to_string(),
                )
            })?;

        // Try to detect a switch pattern in the switch region
        if let Some(switch_info) =
            self.detect_switch_pattern(region.dispatch, cfg, &ssa_analysis, &postdom_analysis)
        {
            // Convert the detected switch pattern to AST
            match self.convert_switch_pattern(&switch_info, cfg, block_converter) {
                Ok(statements) => {
                    // Mark all instructions in involved blocks as rendered
                    // BUT exclude the shared tail block - it needs to be processed after the switch
                    for block_id in &switch_info.involved_blocks {
                        // Skip the shared tail block if it exists
                        if let Some(ref shared_tail) = switch_info.shared_tail {
                            if *block_id == shared_tail.block_id {
                                continue; // Don't mark shared tail instructions as rendered
                            }
                        }

                        let block = &cfg.graph()[*block_id];
                        for instruction in block.instructions() {
                            block_converter.mark_instruction_rendered(instruction);
                        }
                    }
                    return Ok(statements);
                }
                Err(e) => {
                    // Log the error and fall back to block-by-block conversion
                    eprintln!("Warning: Switch pattern conversion failed: {}", e);
                }
            }
        }

        // Fall back to traditional block-by-block conversion
        // This is a simplified fallback - in practice, block_converter would
        // handle the individual blocks in the switch region
        Ok(Vec::new())
    }

    /// Detect if a block starts a sparse switch pattern (simplified approach)
    pub fn detect_switch_pattern(
        &self,
        start_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
        postdom: &PostDominatorAnalysis,
    ) -> Option<SwitchInfo> {
        // Quick check: does this block load a parameter or have a comparison?
        let first_block = &cfg.graph()[start_block];
        let discriminator = self.find_discriminator(first_block)?;

        // Safety checker for setup instructions
        let safety_checker = SetupSafetyChecker::new(cfg, ssa, postdom);

        // Follow the comparison chain
        let mut current_block = start_block;
        let mut cases = Vec::new();
        let mut involved_blocks = HashSet::new();
        let mut visited = HashSet::new();
        let mut execution_order = 0;

        // Track which registers are live across false edges
        let _live_across_false: HashSet<u8> = HashSet::new();

        loop {
            if !visited.insert(current_block) {
                break; // Avoid infinite loops
            }

            involved_blocks.insert(current_block);
            let block = &cfg.graph()[current_block];

            // Look for comparison pattern
            if let Some(mut case_info) =
                self.extract_case_from_block(block, discriminator, current_block, cfg, ssa)
            {
                // Create compare context for anchored safety checks
                let true_successor = case_info.target_block;
                let false_successor = self.get_false_successor(current_block, cfg)?;
                let _compare_ctx = CompareContext {
                    compare_block: current_block,
                    true_successor,
                    false_successor,
                    discriminator,
                };

                // Filter setup instructions by safety
                case_info
                    .setup
                    .retain(|setup_instr| safety_checker.is_case_localizable(setup_instr));

                // Set execution order
                case_info.execution_order = execution_order;
                execution_order += 1;

                cases.push(case_info);

                // Move to false successor to continue the chain
                current_block = false_successor;
            } else {
                // No more comparisons found - this might be the default case
                break;
            }
        }

        // We need at least 2 cases to form a switch
        if cases.len() < 2 {
            return None;
        }

        // Determine default case
        let default_case = if current_block != start_block {
            // If we ended up at a different block, it might be the default
            let setup = self
                .extract_default_setup(current_block, cfg, ssa)
                .unwrap_or_default();

            Some(DefaultCase {
                target_block: current_block,
                setup,
            })
        } else {
            None
        };

        // 3. Detect shared tail block
        let shared_tail = self.detect_shared_tail(&cases, &default_case, postdom, cfg, ssa);

        // 4. Check PHI scenarios and control flow (more expensive)
        if self.should_bail_out_for_phi_scenarios() {
            return None;
        }

        if self.should_bail_out_for_control_flow() {
            return None;
        }

        if self.should_bail_out_for_constants(&cases) {
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

    /// Convert a switch pattern to switch statement AST nodes
    pub fn convert_switch_pattern(
        &mut self,
        switch_info: &SwitchInfo,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut statements = Vec::new();

        // Create discriminator expression based on the register used in switch
        let discriminator_expr = self.create_discriminator_expression(switch_info.discriminator);

        // Get SSA analysis from block converter (required for proper switch handling)
        let ssa = block_converter
            .ssa_analysis()
            .expect("SSA analysis required for switch conversion");

        // Group consecutive cases that share the same target and setup
        let case_groups = self.group_consecutive_cases(&switch_info.cases);

        // Collect all PHI nodes that need declarations:
        // 1. PHI nodes from shared tail
        // 2. PHI nodes referenced in setup instructions
        let mut all_phi_nodes = HashMap::new();

        // Add shared tail PHI nodes
        if let Some(shared_tail) = &switch_info.shared_tail {
            all_phi_nodes.extend(shared_tail.phi_nodes.clone());
        }

        // Find PHI nodes referenced in setup instructions
        self.collect_phi_nodes_from_setup(&switch_info, &mut all_phi_nodes, cfg, ssa);

        // Create declarations for all collected PHI nodes
        if !all_phi_nodes.is_empty() {
            let phi_declarations =
                self.create_join_locals_for_phi_nodes(&all_phi_nodes, block_converter)?;
            statements.extend(phi_declarations);
        }

        // Generate switch cases
        let mut switch_cases = ArenaVec::new_in(self.ast_builder.allocator);

        for (i, group) in case_groups.iter().enumerate() {
            // For groups with multiple keys, create multiple cases
            if group.keys.len() > 1 {
                // Create empty cases for all but the last key
                for key in &group.keys[..group.keys.len() - 1] {
                    let test_expr = Some(self.create_constant_expression(key));
                    let empty_statements = ArenaVec::new_in(self.ast_builder.allocator);
                    let span = Span::default();
                    let empty_case =
                        self.ast_builder
                            .switch_case(span, test_expr, empty_statements);
                    switch_cases.push(empty_case);
                }
            }

            // Create the actual case with the body for the last key
            let switch_case =
                self.convert_case_group(group, cfg, block_converter, switch_info, i, &case_groups)?;
            switch_cases.push(switch_case);
        }

        // Handle default case
        if let Some(default_case) = &switch_info.default_case {
            let default_switch_case =
                self.convert_default_case(default_case, cfg, block_converter, switch_info)?;
            switch_cases.push(default_switch_case);
        }

        // Create the switch statement
        let span = Span::default();
        let switch_stmt = self
            .ast_builder
            .switch_statement(span, discriminator_expr, switch_cases);

        statements.push(Statement::SwitchStatement(
            self.ast_builder.alloc(switch_stmt),
        ));

        Ok(statements)
    }

    /// Group consecutive cases that share the same target and setup
    /// But don't group cases that would have different PHI contributions to the target block
    fn group_consecutive_cases(&self, cases: &[CaseInfo]) -> Vec<CaseGroup> {
        let mut groups = Vec::new();
        let mut i = 0;

        while i < cases.len() {
            let mut group_keys = vec![cases[i].keys[0].clone()];
            let mut comparison_blocks = vec![cases[i].comparison_block];
            let target = cases[i].target_block;
            let setup = cases[i].setup.clone();
            // eprintln!("Case at index {} has {} setup instructions", i, setup.len());
            let first_order = cases[i].execution_order;
            let mut j = i + 1;

            // Group consecutive cases with same target and setup
            while j < cases.len() {
                // Check if next case is consecutive in execution order
                if cases[j].execution_order != cases[j - 1].execution_order + 1 {
                    break;
                }

                // Check if target matches
                if cases[j].target_block == target {
                    // Allow grouping if:
                    // 1. Setup instructions are equal, OR
                    // 2. The next case has no setup (relies on previous case's setup)
                    if self.setup_instructions_equal(&cases[j].setup, &setup)
                        || cases[j].setup.is_empty()
                    {
                        group_keys.push(cases[j].keys[0].clone());
                        comparison_blocks.push(cases[j].comparison_block);
                        j += 1;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // eprintln!("Creating group with {} keys, setup={:?}", group_keys.len(), setup.len());
            groups.push(CaseGroup {
                keys: group_keys,
                target_block: target,
                setup,
                always_terminates: cases[i].always_terminates,
                first_execution_order: first_order,
                comparison_blocks,
            });

            i = j;
        }

        groups
    }

    /// Check if two setup instruction lists are equal
    fn setup_instructions_equal(
        &self,
        a: &SmallVec<[SetupInstruction; 4]>,
        b: &SmallVec<[SetupInstruction; 4]>,
    ) -> bool {
        if a.len() != b.len() {
            return false;
        }

        for (instr_a, instr_b) in a.iter().zip(b.iter()) {
            if instr_a.ssa_value.register != instr_b.ssa_value.register
                || !self.constant_values_equal(&instr_a.value, &instr_b.value)
            {
                return false;
            }
        }

        true
    }

    /// Check if two constant values are equal
    fn constant_values_equal(&self, a: &ConstantValue, b: &ConstantValue) -> bool {
        match (a, b) {
            (ConstantValue::Number(n1), ConstantValue::Number(n2)) => n1 == n2,
            (ConstantValue::String(s1), ConstantValue::String(s2)) => s1 == s2,
            (ConstantValue::Boolean(b1), ConstantValue::Boolean(b2)) => b1 == b2,
            (ConstantValue::Null, ConstantValue::Null) => true,
            (ConstantValue::Undefined, ConstantValue::Undefined) => true,
            _ => false,
        }
    }

    /// Create discriminator expression from register
    fn create_discriminator_expression(&self, register: u8) -> Expression<'a> {
        let span = Span::default();
        // Check if this register is loaded from a parameter
        // In the future, this should be done through proper SSA analysis
        let name = match register {
            0 | 1 | 2 | 3 => {
                // Common registers used for parameters
                // For simplicity, assume first parameter is always arg0
                "arg0".to_string()
            }
            _ => format!("r{}", register),
        };
        let name_atom = self.ast_builder.allocator.alloc_str(&name);
        let identifier = self.ast_builder.identifier_reference(span, name_atom);
        Expression::Identifier(self.ast_builder.alloc(identifier))
    }

    /// Convert a case group to a switch case AST node
    fn convert_case_group(
        &mut self,
        group: &CaseGroup,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        group_index: usize,
        all_groups: &[CaseGroup],
    ) -> Result<SwitchCase<'a>, SwitchConversionError> {
        if group.keys.is_empty() {
            return Err(SwitchConversionError::InvalidRegion(
                "Empty case group".to_string(),
            ));
        }

        // Use the last key in the group for the case with body
        let test_expr = Some(self.create_constant_expression(&group.keys[group.keys.len() - 1]));

        // Convert the case body
        let mut case_statements = ArenaVec::new_in(self.ast_builder.allocator);

        // Check if this case goes directly to the shared tail
        if let Some(shared_tail) = &switch_info.shared_tail {
            if group.target_block == shared_tail.block_id {
                // This case goes directly to the shared tail
                // Generate code based on PHI node contributions
                self.generate_case_body_for_shared_tail(
                    &mut case_statements,
                    group,
                    block_converter,
                )?;
            } else {
                // Check if the target block has PHI nodes
                let has_phi = block_converter
                    .ssa_analysis()
                    .and_then(|ssa| ssa.phi_functions.get(&group.target_block))
                    .map(|phis| !phis.is_empty())
                    .unwrap_or(false);

                if has_phi {
                    // Target block has PHI nodes - generate PHI assignments
                    self.generate_case_body_with_phi_assignments(
                        &mut case_statements,
                        group,
                        group.target_block,
                        cfg,
                        block_converter,
                    )?;
                } else {
                    // No PHI nodes - convert normally
                    self.convert_target_block_normally(
                        &mut case_statements,
                        group,
                        cfg,
                        block_converter,
                    )?;
                }
            }
        } else {
            // No shared tail - still need to check for PHI nodes in target
            let has_phi = block_converter
                .ssa_analysis()
                .and_then(|ssa| ssa.phi_functions.get(&group.target_block))
                .map(|phis| !phis.is_empty())
                .unwrap_or(false);

            if has_phi {
                // Target block has PHI nodes - generate PHI assignments
                self.generate_case_body_with_phi_assignments(
                    &mut case_statements,
                    group,
                    group.target_block,
                    cfg,
                    block_converter,
                )?;
            } else {
                // No PHI nodes - convert normally
                self.convert_target_block_normally(
                    &mut case_statements,
                    group,
                    cfg,
                    block_converter,
                )?;
            }
        }

        // Check if we need to include fallthrough blocks that have PHI nodes
        // This happens when the current block flows to the next group's target block
        // and that target has PHI nodes. We need to duplicate the block to ensure
        // correct execution when falling through from this case.
        if group_index + 1 < all_groups.len() {
            let next_group = &all_groups[group_index + 1];

            // Check if current block falls through to next group's target
            let blocks_are_consecutive = {
                let current_idx = group.target_block.index();
                let next_idx = next_group.target_block.index();
                next_idx == current_idx + 1
            };

            if blocks_are_consecutive
                || self.block_flows_to(group.target_block, next_group.target_block, cfg)
            {
                // Don't duplicate the shared tail
                let is_shared_tail = if let Some(shared_tail) = &switch_info.shared_tail {
                    if next_group.target_block == shared_tail.block_id {
                        // Skip duplicating the shared tail - it will be generated after the switch
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                // Check if the next block has PHI nodes
                if let Some(ssa) = block_converter.ssa_analysis() {
                    if let Some(phi_functions) = ssa.phi_functions.get(&next_group.target_block) {
                        if !phi_functions.is_empty() && !is_shared_tail {
                            // We need to duplicate the next block(s) here
                            // This ensures correct PHI values for the fallthrough path
                            let target_block = &cfg.graph()[next_group.target_block];
                            match block_converter.convert_block_with_marking_control(
                                target_block,
                                next_group.target_block,
                                cfg.graph(),
                                None::<
                                    fn(
                                        &crate::hbc::function_table::HbcFunctionInstruction,
                                        bool,
                                    ) -> bool,
                                >,
                                false,
                                false, // don't mark as rendered
                            ) {
                                Ok(statements) => {
                                    case_statements.extend(statements);
                                }
                                Err(e) => {
                                    return Err(SwitchConversionError::CaseConversionError(
                                        format!("Failed to convert fallthrough block: {}", e),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }

        // Add break if this case doesn't fall through
        if self.should_add_break_for_group(
            group,
            switch_info,
            cfg,
            group_index,
            all_groups,
            block_converter,
        ) {
            let span = Span::default();
            let break_stmt = self.ast_builder.break_statement(span, None);
            case_statements.push(Statement::BreakStatement(
                self.ast_builder.alloc(break_stmt),
            ));
        }

        let span = Span::default();
        Ok(self
            .ast_builder
            .switch_case(span, test_expr, case_statements))
    }

    /// Determine if a case group needs a break statement
    fn should_add_break_for_group(
        &self,
        group: &CaseGroup,
        switch_info: &SwitchInfo,
        cfg: &Cfg<'a>,
        group_index: usize,
        all_groups: &[CaseGroup],
        block_converter: &super::BlockToStatementConverter<'a>,
    ) -> bool {
        // A case needs a break if it doesn't fall through to the next case or shared tail

        // First check if the target block has a terminator (return/throw)
        let target_block = &cfg.graph()[group.target_block];
        for instr in target_block.instructions() {
            if matches!(
                instr.instruction,
                UnifiedInstruction::Ret { .. } | UnifiedInstruction::Throw { .. }
            ) {
                return false; // No break needed, already terminates
            }
        }

        // FIRST: Check if the next case group would have PHI conflicts if we fall through
        // This takes precedence over shared tail checks
        if group_index + 1 < all_groups.len() {
            let next_group = &all_groups[group_index + 1];

            // Check if blocks are consecutive (fallthrough)
            let blocks_are_consecutive = {
                let current_idx = group.target_block.index();
                let next_idx = next_group.target_block.index();
                next_idx == current_idx + 1
            };

            // Check if the current group flows to the next group's target
            let flows_to_next =
                self.block_flows_to(group.target_block, next_group.target_block, cfg);

            if flows_to_next || blocks_are_consecutive {
                // Check if the next group's target has PHI nodes
                if let Some(ssa) = block_converter.ssa_analysis() {
                    if let Some(phi_functions) = ssa.phi_functions.get(&next_group.target_block) {
                        if !phi_functions.is_empty() {
                            // The next case's target has PHI nodes
                            // We MUST add a break to prevent incorrect PHI values from fallthrough
                            return true;
                        }
                    }
                }
            }
        }

        // THEN: Check if this block flows to the shared tail
        if let Some(shared_tail) = &switch_info.shared_tail {
            if self.block_flows_to(group.target_block, shared_tail.block_id, cfg) {
                // If it flows to shared tail, check if shared tail has a return
                let tail_block = &cfg.graph()[shared_tail.block_id];
                for instr in tail_block.instructions() {
                    if matches!(instr.instruction, UnifiedInstruction::Ret { .. }) {
                        return false; // No break needed, flows to return
                    }
                }
            }
        }

        // Otherwise, add a break to prevent fall-through
        true
    }

    /// Generate case body with PHI assignments for direct entry
    fn generate_case_body_with_phi_assignments(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        target_block: NodeIndex,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // For each PHI function in the target block, generate appropriate assignments
        // based on which comparison blocks are entering this case

        // First, generate setup instructions if any
        self.generate_setup_instructions(case_statements, group, block_converter)?;

        // Get PHI functions for the target block
        let phi_functions = block_converter
            .ssa_analysis()
            .and_then(|ssa| ssa.phi_functions.get(&target_block))
            .map(|phis| phis.as_slice())
            .unwrap_or(&[]);

        // Then, generate PHI assignments for direct entry into this case
        for phi_function in phi_functions {
            // Find the PHI contribution from one of this group's comparison blocks
            let mut phi_value = None;
            for comp_block in &group.comparison_blocks {
                if let Some(value) = phi_function.operands.get(comp_block) {
                    phi_value = Some(value);
                    break;
                }
            }

            if let Some(ssa_value) = phi_value {
                // Get the variable name for the PHI result
                let phi_var_name = if let Some(mapping) = block_converter
                    .instruction_converter()
                    .register_manager()
                    .variable_mapping()
                {
                    mapping
                        .ssa_to_var
                        .get(&phi_function.result)
                        .cloned()
                        .unwrap_or_else(|| format!("var{}", phi_function.register))
                } else {
                    format!("var{}", phi_function.register)
                };

                // Get the variable name for the source SSA value
                let source_var_name = if let Some(mapping) = block_converter
                    .instruction_converter()
                    .register_manager()
                    .variable_mapping()
                {
                    mapping
                        .ssa_to_var
                        .get(ssa_value)
                        .cloned()
                        .unwrap_or_else(|| format!("var{}", ssa_value.register))
                } else {
                    format!("var{}", ssa_value.register)
                };

                // Only generate assignment if source and destination are different
                if phi_var_name != source_var_name {
                    // Generate assignment: phi_var = source_var
                    let span = Span::default();

                    // Create identifier reference for the PHI variable
                    let phi_var_atom = self.ast_builder.allocator.alloc_str(&phi_var_name);
                    let phi_ident_ref = self
                        .ast_builder
                        .alloc_identifier_reference(span, phi_var_atom);

                    // Create identifier expression for the source variable
                    let source_var_atom = self.ast_builder.allocator.alloc_str(&source_var_name);
                    let source_ident_ref = self
                        .ast_builder
                        .expression_identifier(span, source_var_atom);

                    // Create assignment expression: phi_var = source_var
                    let simple_target =
                        SimpleAssignmentTarget::AssignmentTargetIdentifier(phi_ident_ref);
                    let assignment_target = AssignmentTarget::from(simple_target);
                    let assignment = self.ast_builder.expression_assignment(
                        span,
                        AssignmentOperator::Assign,
                        assignment_target,
                        source_ident_ref,
                    );

                    // Wrap in expression statement
                    let expr_stmt = self.ast_builder.expression_statement(span, assignment);
                    case_statements.push(Statement::ExpressionStatement(
                        self.ast_builder.alloc(expr_stmt),
                    ));
                }
            }
        }

        // Then convert the target block normally
        self.convert_target_block_normally(case_statements, group, cfg, block_converter)
    }

    /// Generate statements for setup instructions
    fn generate_setup_instructions(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // First, identify which SSA values are just temporaries (defined and only used in Mov)
        let mut temp_ssa_values = HashSet::new();
        for setup_instr in &group.setup {
            if let UnifiedInstruction::Mov { .. } = &setup_instr.instruction.instruction {
                // This is a Mov, skip temp detection for it
                continue;
            }
            // Check if this SSA value is only used by a Mov
            let is_temp = group.setup.iter().any(|other| {
                if let UnifiedInstruction::Mov { operand_1, .. } = &other.instruction.instruction {
                    *operand_1 as u8 == setup_instr.ssa_value.register
                } else {
                    false
                }
            });
            if is_temp {
                temp_ssa_values.insert(&setup_instr.ssa_value);
            }
        }

        for setup_instr in &group.setup {
            // Skip LoadConst instructions that load into temporary SSA values
            // But always process Mov instructions as they assign to the actual variables
            if temp_ssa_values.contains(&setup_instr.ssa_value) {
                if let UnifiedInstruction::Mov { .. } = &setup_instr.instruction.instruction {
                    // Process Mov instructions even if source is temporary
                } else {
                    continue;
                }
            }

            // Get the variable name for this SSA value
            let var_name = if let Some(mapping) = block_converter
                .instruction_converter()
                .register_manager()
                .variable_mapping()
            {
                let ssa = block_converter
                    .ssa_analysis()
                    .expect("SSA analysis required for switch conversion");

                // Check if there's a PHI function at the target block for this register
                if let Some(phi_functions) = ssa.phi_functions.get(&group.target_block) {
                    if let Some(phi_func) = phi_functions
                        .iter()
                        .find(|phi| phi.register == setup_instr.ssa_value.register)
                    {
                        // Use the PHI result variable
                        mapping
                            .ssa_to_var
                            .get(&phi_func.result)
                            .cloned()
                            .unwrap_or_else(|| format!("var{}", setup_instr.ssa_value.register))
                    } else {
                        // No PHI function for this register, use the SSA value directly
                        mapping
                            .ssa_to_var
                            .get(&setup_instr.ssa_value)
                            .cloned()
                            .unwrap_or_else(|| format!("var{}", setup_instr.ssa_value.register))
                    }
                } else {
                    // No PHI functions at target block, use the SSA value directly
                    mapping
                        .ssa_to_var
                        .get(&setup_instr.ssa_value)
                        .cloned()
                        .unwrap_or_else(|| format!("var{}", setup_instr.ssa_value.register))
                }
            } else {
                format!("var{}", setup_instr.ssa_value.register)
            };

            // Create the constant expression from the setup value
            let value_expr = self.create_constant_expression_from_value(&setup_instr.value);

            // Use the instruction converter's method to create either declaration or assignment
            let stmt = block_converter
                .instruction_converter_mut()
                .create_variable_declaration_or_assignment(&var_name, Some(value_expr))
                .map_err(|e| {
                    SwitchConversionError::CaseConversionError(format!(
                        "Failed to create variable declaration/assignment: {}",
                        e
                    ))
                })?;

            // Mark this instruction as rendered to prevent double processing
            block_converter.mark_instruction_rendered(&setup_instr.instruction);

            // Add instruction comment if enabled
            if block_converter.include_instruction_comments() {
                if let Some(comment_manager) = block_converter.comment_manager_mut() {
                    if let Some(expr_ctx) = &self.expression_context {
                        if let Some(hbc_file) = expr_ctx.hbc_file() {
                            let instruction_info = format!(
                                "PC {}: {}",
                                setup_instr.instruction.instruction_index.value(),
                                setup_instr.instruction.format_instruction(hbc_file)
                            );
                            comment_manager.add_comment(
                                &stmt,
                                instruction_info,
                                crate::ast::comments::CommentKind::Line,
                                crate::ast::comments::CommentPosition::Leading,
                            );
                        }
                    }
                }
            }

            case_statements.push(stmt);
        }
        Ok(())
    }

    /// Convert target block normally without PHI handling
    fn convert_target_block_normally(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        let target_block = &cfg.graph()[group.target_block];
        match block_converter.convert_block_with_options(
            target_block,
            group.target_block,
            cfg.graph(),
            None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
            false,
        ) {
            Ok(statements) => {
                case_statements.extend(statements);
                Ok(())
            }
            Err(e) => Err(SwitchConversionError::CaseConversionError(format!(
                "Failed to convert case body: {}",
                e
            ))),
        }
    }

    /// Generate case body for cases that go directly to shared tail
    fn generate_case_body_for_shared_tail(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // For cases that jump directly to the shared tail, we need to:
        // 1. Generate setup instructions
        // 2. Generate any PHI value assignments
        // 3. Add a break statement

        // Generate setup instructions - these should handle all necessary assignments
        self.generate_setup_instructions(case_statements, group, block_converter)?;

        // Add break statement
        let span = Span::default();
        let break_stmt = self.ast_builder.break_statement(span, None);
        case_statements.push(Statement::BreakStatement(
            self.ast_builder.alloc(break_stmt),
        ));

        Ok(())
    }

    /// Find what value a case contributes to a PHI node
    pub fn find_phi_contribution_for_case(
        &self,
        group: &CaseGroup,
        phi_node: &PhiNode,
    ) -> Option<ConstantValue> {
        // The PHI node has values from specific predecessor blocks.
        // We need to trace the control flow path from this case to the PHI block
        // and find which predecessor block on that path contributes to the PHI.

        // If we don't have SSA PHI info, we can't properly trace contributions
        if phi_node.ssa_phi_value.is_none() {
            // Without SSA info, we can't properly trace
            return None;
        }

        // For each case, we need to find the path to the PHI block
        // The contribution comes from whichever predecessor block is on that path

        // Start from the case's target block
        let start_block = group.target_block;

        // If the target block is directly in the PHI's predecessors, use its value
        if let Some(value) = phi_node.values.get(&start_block) {
            return Some(value.clone());
        }

        // Otherwise, check comparison blocks (for cases that jump directly to PHI)
        for comparison_block in &group.comparison_blocks {
            if let Some(value) = phi_node.values.get(comparison_block) {
                return Some(value.clone());
            }
        }

        // If neither the target nor comparison blocks contribute to the PHI,
        // then this case must flow through an intermediate block that computes a value.
        // We can't represent computed values as constants.
        None
    }

    /// Create constant expression from a ConstantValue
    fn create_constant_expression_from_value(&self, value: &ConstantValue) -> Expression<'a> {
        let span = Span::default();
        match value {
            ConstantValue::Number(n) => {
                self.ast_builder.expression_numeric_literal(
                    span,
                    n.0,
                    None, // raw
                    NumberBase::Decimal,
                )
            }
            ConstantValue::String(s) => {
                let string_atom = self.ast_builder.allocator.alloc_str(s);
                self.ast_builder
                    .expression_string_literal(span, string_atom, None)
            }
            ConstantValue::Boolean(b) => self.ast_builder.expression_boolean_literal(span, *b),
            ConstantValue::Null => self.ast_builder.expression_null_literal(span),
            ConstantValue::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder.expression_identifier(span, undefined_atom)
            }
        }
    }

    /// Check if one block flows to another (follows unconditional jumps)
    fn block_flows_to(&self, from: NodeIndex, to: NodeIndex, cfg: &Cfg<'a>) -> bool {
        use petgraph::Direction;

        let mut current = from;
        let mut visited = HashSet::new();

        while visited.insert(current) {
            if current == to {
                return true;
            }

            // Get outgoing edges
            let edges: Vec<_> = cfg
                .graph()
                .edges_directed(current, Direction::Outgoing)
                .collect();

            // If only one unconditional edge, follow it
            if edges.len() == 1
                && matches!(
                    edges[0].weight(),
                    crate::cfg::EdgeKind::Uncond | crate::cfg::EdgeKind::Fall
                )
            {
                current = edges[0].target();
            } else {
                break;
            }
        }

        false
    }

    /// Convert default case to switch case AST node
    fn convert_default_case(
        &mut self,
        default_case: &DefaultCase,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
    ) -> Result<SwitchCase<'a>, SwitchConversionError> {
        let mut case_statements = ArenaVec::new_in(self.ast_builder.allocator);

        // Check if default case contributes to shared tail
        if let Some(shared_tail) = &switch_info.shared_tail {
            // Check if default case flows to shared tail (either directly or through a simple block)
            let flows_to_shared_tail = default_case.target_block == shared_tail.block_id
                || self.block_flows_to(default_case.target_block, shared_tail.block_id, cfg);

            if flows_to_shared_tail {
                // Generate setup instructions for default case
                if !default_case.setup.is_empty() {
                    // Create a temporary CaseGroup to reuse generate_setup_instructions
                    let temp_group = CaseGroup {
                        keys: vec![], // Default case has no keys
                        target_block: default_case.target_block,
                        setup: default_case.setup.clone(),
                        always_terminates: false,
                        first_execution_order: usize::MAX, // Default is last
                        comparison_blocks: vec![],
                    };

                    self.generate_setup_instructions(
                        &mut case_statements,
                        &temp_group,
                        block_converter,
                    )?;
                }

                // If the target block is not the shared tail itself, convert it
                if default_case.target_block != shared_tail.block_id {
                    let target_block = &cfg.graph()[default_case.target_block];
                    match block_converter.convert_block_with_options(
                        target_block,
                        default_case.target_block,
                        cfg.graph(),
                        None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
                        false,
                    ) {
                        Ok(statements) => {
                            case_statements.extend(statements);
                        }
                        Err(e) => {
                            return Err(SwitchConversionError::CaseConversionError(format!(
                                "Failed to convert default case body: {}",
                                e
                            )));
                        }
                    }
                }
            } else {
                // Convert the default case body normally
                let target_block = &cfg.graph()[default_case.target_block];
                match block_converter.convert_block_with_options(
                    target_block,
                    default_case.target_block,
                    cfg.graph(),
                    None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
                    false,
                ) {
                    Ok(statements) => {
                        case_statements.extend(statements);
                    }
                    Err(e) => {
                        return Err(SwitchConversionError::CaseConversionError(format!(
                            "Failed to convert default case body: {}",
                            e
                        )));
                    }
                }
            }
        } else {
            // No shared tail - but still generate setup instructions if any
            if !default_case.setup.is_empty() {
                let temp_group = CaseGroup {
                    keys: vec![], // Default case has no keys
                    target_block: default_case.target_block,
                    setup: default_case.setup.clone(),
                    always_terminates: false,
                    first_execution_order: usize::MAX, // Default is last
                    comparison_blocks: vec![],
                };

                self.generate_setup_instructions(
                    &mut case_statements,
                    &temp_group,
                    block_converter,
                )?;
            }

            // Then convert the target block normally
            let target_block = &cfg.graph()[default_case.target_block];
            match block_converter.convert_block_with_options(
                target_block,
                default_case.target_block,
                cfg.graph(),
                None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
                false,
            ) {
                Ok(statements) => {
                    case_statements.extend(statements);
                }
                Err(e) => {
                    return Err(SwitchConversionError::CaseConversionError(format!(
                        "Failed to convert default case body: {}",
                        e
                    )));
                }
            }
        }

        // Add break statement if needed
        if !self.default_case_falls_through(default_case, cfg) {
            let span = Span::default();
            let break_stmt = self.ast_builder.break_statement(span, None);
            case_statements.push(Statement::BreakStatement(
                self.ast_builder.alloc(break_stmt),
            ));
        }

        let span = Span::default();
        Ok(self.ast_builder.switch_case(span, None, case_statements)) // None = default case
    }

    /// Create constant expression from case key
    fn create_constant_expression(&self, key: &CaseKey) -> Expression<'a> {
        let span = Span::default();
        match key {
            CaseKey::Str(s) => {
                // Allocate the string in the arena to manage its lifetime
                let s_str = self.ast_builder.allocator.alloc_str(s.as_ref());
                self.ast_builder
                    .expression_string_literal(span, s_str, None)
            }
            CaseKey::Num(n) => {
                self.ast_builder
                    .expression_numeric_literal(span, n.0, None, NumberBase::Decimal)
            }
        }
    }

    /// Get the consistent variable name for a PHI node
    fn get_phi_variable_name(
        &self,
        phi_node: &PhiNode,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<String, SwitchConversionError> {
        // PHI nodes must have SSA information
        let ssa_phi_value = phi_node.ssa_phi_value.as_ref().ok_or_else(|| {
            SwitchConversionError::InvalidRegion(
                "PHI node missing SSA value information".to_string(),
            )
        })?;

        // Variable mapping must be available
        let var_mapping = block_converter
            .instruction_converter_mut()
            .register_manager_mut()
            .variable_mapping()
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion("Variable mapping not available".to_string())
            })?;

        // The SSA value must have a variable name assigned
        var_mapping
            .ssa_to_var
            .get(ssa_phi_value)
            .cloned()
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion(format!(
                    "No variable name found for PHI SSA value {}",
                    ssa_phi_value.name()
                ))
            })
    }

    /// Collect PHI nodes referenced in setup instructions
    fn collect_phi_nodes_from_setup(
        &self,
        switch_info: &SwitchInfo,
        all_phi_nodes: &mut HashMap<u8, PhiNode>,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) {
        // Check each case's setup instructions
        for case in &switch_info.cases {
            for setup_instr in &case.setup {
                // Check if this register has a PHI function at its target block
                if let Some(phi_functions) = ssa.phi_functions.get(&case.target_block) {
                    if let Some(phi_func) = phi_functions
                        .iter()
                        .find(|phi| phi.register == setup_instr.ssa_value.register)
                    {
                        // Convert PhiFunction to PhiNode
                        let phi_node = self.convert_phi_function_to_node(phi_func);
                        all_phi_nodes.insert(phi_func.register, phi_node);
                    }
                }

                // Also check for PHI functions at any block in the switch region
                // This handles cases where variables are defined by PHI functions in intermediate blocks
                for block_id in cfg.graph().node_indices() {
                    if let Some(phi_functions) = ssa.phi_functions.get(&block_id) {
                        if let Some(phi_func) = phi_functions
                            .iter()
                            .find(|phi| phi.result == setup_instr.ssa_value)
                        {
                            // Convert PhiFunction to PhiNode
                            let phi_node = self.convert_phi_function_to_node(phi_func);
                            all_phi_nodes.insert(phi_func.register, phi_node);
                        }
                    }
                }
            }
        }

        // Also check default case setup instructions
        if let Some(default_case) = &switch_info.default_case {
            for setup_instr in &default_case.setup {
                // Check if this register has a PHI function at its target block
                if let Some(phi_functions) = ssa.phi_functions.get(&default_case.target_block) {
                    if let Some(phi_func) = phi_functions
                        .iter()
                        .find(|phi| phi.register == setup_instr.ssa_value.register)
                    {
                        // Convert PhiFunction to PhiNode
                        let phi_node = self.convert_phi_function_to_node(phi_func);
                        all_phi_nodes.insert(phi_func.register, phi_node);
                    }
                }

                // Also check for PHI functions at any block in the switch region
                for block_id in cfg.graph().node_indices() {
                    if let Some(phi_functions) = ssa.phi_functions.get(&block_id) {
                        if let Some(phi_func) = phi_functions
                            .iter()
                            .find(|phi| phi.result == setup_instr.ssa_value)
                        {
                            // Convert PhiFunction to PhiNode
                            let phi_node = self.convert_phi_function_to_node(phi_func);
                            all_phi_nodes.insert(phi_func.register, phi_node);
                        }
                    }
                }
            }
        }
    }

    /// Convert a PhiFunction to a PhiNode
    fn convert_phi_function_to_node(&self, phi_func: &crate::cfg::ssa::PhiFunction) -> PhiNode {
        // For now, create a PhiNode without constant values
        // In a real implementation, we would analyze the operands to extract constant values
        PhiNode {
            register: phi_func.register,
            values: HashMap::new(), // Empty for now
            ssa_phi_value: Some(phi_func.result.clone()),
        }
    }

    /// Create join locals for PHI nodes in shared tail
    fn create_join_locals_for_phi_nodes(
        &mut self,
        phi_nodes: &HashMap<u8, PhiNode>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut declarations = Vec::new();

        // For each PHI node in the shared tail, we need to create a variable declaration
        // before the switch statement to ensure the variable is in scope for all cases

        for phi_node in phi_nodes.values() {
            // Get the variable name for this PHI node
            let var_name = self.get_phi_variable_name(phi_node, block_converter)?;

            // Create a variable declaration: let var_name;
            let span = Span::default();
            let var_atom = self.ast_builder.allocator.alloc_str(&var_name);

            // Create binding identifier
            let binding_identifier = oxc_ast::ast::BindingIdentifier {
                span,
                name: oxc_span::Atom::from(var_atom),
                symbol_id: std::cell::Cell::new(None),
            };

            // Create binding pattern
            let binding_pattern = oxc_ast::ast::BindingPattern {
                kind: oxc_ast::ast::BindingPatternKind::BindingIdentifier(
                    self.ast_builder.alloc(binding_identifier),
                ),
                type_annotation: None,
                optional: false,
            };

            // Create variable declarator without initializer
            let var_declarator = self.ast_builder.variable_declarator(
                span,
                oxc_ast::ast::VariableDeclarationKind::Let,
                binding_pattern,
                None,  // No initializer
                false, // definite
            );

            // Create variable declaration statement
            let mut declarators = self.ast_builder.vec();
            declarators.push(var_declarator);

            let var_decl = self.ast_builder.variable_declaration(
                span,
                oxc_ast::ast::VariableDeclarationKind::Let,
                declarators,
                false, // declare
            );

            declarations.push(Statement::VariableDeclaration(
                self.ast_builder.alloc(var_decl),
            ));
        }

        Ok(declarations)
    }

    /// Check if default case falls through
    fn default_case_falls_through(&self, default_case: &DefaultCase, cfg: &Cfg<'a>) -> bool {
        let block = &cfg.graph()[default_case.target_block];

        // Check if the last instruction is a jump or return
        if let Some(last_instr) = block.instructions().last() {
            match &last_instr.instruction {
                UnifiedInstruction::Jmp { .. }
                | UnifiedInstruction::JmpLong { .. }
                | UnifiedInstruction::Ret { .. } => false, // Explicit control flow, no fallthrough
                _ => true, // Implicit fallthrough
            }
        } else {
            true // Empty block falls through
        }
    }

    // Helper methods for pattern detection

    /// Find what register is being used as discriminator
    fn find_discriminator(&self, block: &crate::cfg::Block) -> Option<u8> {
        // Look for LoadParam as first instruction
        if let Some(first) = block.instructions().first() {
            if let UnifiedInstruction::LoadParam { operand_0, .. } = &first.instruction {
                return Some(*operand_0);
            }
        }

        // Look for JStrictEqual instruction and extract discriminator register
        // The discriminator is the register that's NOT loaded with a constant in this block
        for instr in block.instructions() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual {
                    operand_1,
                    operand_2,
                    ..
                }
                | UnifiedInstruction::JStrictEqualLong {
                    operand_1,
                    operand_2,
                    ..
                } => {
                    // Check which operand was loaded with a constant in this block
                    let op1_is_const = block.instructions().iter().any(|i| match &i.instruction {
                        UnifiedInstruction::LoadConstZero { operand_0 } => {
                            *operand_0 == *operand_1 as u8
                        }
                        UnifiedInstruction::LoadConstUInt8 { operand_0, .. } => {
                            *operand_0 == *operand_1 as u8
                        }
                        UnifiedInstruction::LoadConstInt { operand_0, .. } => {
                            *operand_0 == *operand_1 as u8
                        }
                        _ => false,
                    });

                    let op2_is_const = block.instructions().iter().any(|i| match &i.instruction {
                        UnifiedInstruction::LoadConstZero { operand_0 } => {
                            *operand_0 == *operand_2 as u8
                        }
                        UnifiedInstruction::LoadConstUInt8 { operand_0, .. } => {
                            *operand_0 == *operand_2 as u8
                        }
                        UnifiedInstruction::LoadConstInt { operand_0, .. } => {
                            *operand_0 == *operand_2 as u8
                        }
                        _ => false,
                    });

                    // The discriminator is the non-constant operand
                    if op1_is_const && !op2_is_const {
                        return Some(*operand_2 as u8);
                    } else if !op1_is_const && op2_is_const {
                        return Some(*operand_1 as u8);
                    }
                    // If neither or both are constants in this block, default to operand_2
                    return Some(*operand_2 as u8);
                }
                _ => continue,
            }
        }

        None
    }

    /// Extract case information from a comparison block
    fn extract_case_from_block(
        &self,
        block: &crate::cfg::Block,
        discriminator: u8,
        block_id: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> Option<CaseInfo> {
        let mut setup = SmallVec::new();
        let mut keys = Vec::new();
        let mut comparison_index = None;
        let mut comparison_const_reg = None;
        let mut target_block = None;
        let mut source_pc = InstructionIndex(0);

        // First pass: find the JStrictEqual comparison
        for (i, instr) in block.instructions().iter().enumerate() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual {
                    operand_0: _,
                    operand_1,
                    operand_2,
                }
                | UnifiedInstruction::JStrictEqualLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    // Check if this is comparing our discriminator
                    let (const_reg, is_discriminator_match) = if *operand_1 as u8 == discriminator {
                        (*operand_2 as u8, true)
                    } else if *operand_2 as u8 == discriminator {
                        (*operand_1 as u8, true)
                    } else {
                        (0, false)
                    };

                    if is_discriminator_match {
                        comparison_index = Some(i);
                        comparison_const_reg = Some(const_reg);
                        source_pc = instr.instruction_index;

                        // Extract the constant value for the case key
                        if let Some(constant_value) =
                            self.get_constant_value_at(block_id, i, const_reg, cfg)
                        {
                            let case_key = match constant_value {
                                ConstantValue::Number(n) => CaseKey::Num(n),
                                ConstantValue::String(s) => CaseKey::Str(s.into()),
                                _ => return None,
                            };
                            keys.push(case_key);
                        }

                        // Find the target block
                        target_block = self.get_true_successor(block_id, cfg);
                        break;
                    }
                }
                _ => {}
            }
        }

        // Bail if we didn't find a comparison
        let comparison_idx = comparison_index?;
        let const_reg = comparison_const_reg?;
        let target = target_block?;

        // Second pass: collect all setup instructions
        // These are all instructions except:
        // 1. The LoadConst that loads the comparison value
        // 2. The JStrictEqual comparison itself
        for (i, instr) in block.instructions().iter().enumerate() {
            // Skip the comparison instruction
            if i == comparison_idx {
                continue;
            }

            // Check if this instruction defines a register
            let usage =
                crate::generated::instruction_analysis::analyze_register_usage(&instr.instruction);
            if let Some(target_reg) = usage.target {
                // Skip if this is loading the constant for comparison
                let loads_comparison_const = target_reg == const_reg
                    && i < comparison_idx
                    && match &instr.instruction {
                        UnifiedInstruction::LoadConstZero { .. }
                        | UnifiedInstruction::LoadConstUInt8 { .. }
                        | UnifiedInstruction::LoadConstInt { .. }
                        | UnifiedInstruction::LoadConstDouble { .. }
                        | UnifiedInstruction::LoadConstString { .. }
                        | UnifiedInstruction::LoadConstStringLongIndex { .. }
                        | UnifiedInstruction::LoadConstTrue { .. }
                        | UnifiedInstruction::LoadConstFalse { .. }
                        | UnifiedInstruction::LoadConstNull { .. }
                        | UnifiedInstruction::LoadConstUndefined { .. } => true,
                        _ => false,
                    };

                if loads_comparison_const {
                    continue;
                }

                // Find the SSA value for this definition
                let def_site = crate::cfg::ssa::RegisterDef {
                    register: target_reg,
                    block_id,
                    instruction_idx: instr.instruction_index,
                };

                if let Some(ssa_value) = ssa.ssa_values.get(&def_site) {
                    // Extract the value for this instruction
                    if let Some(value) =
                        self.extract_constant_value_from_instruction(&instr.instruction)
                    {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value,
                            instruction: instr.clone(),
                        });
                    } else if let UnifiedInstruction::Mov {
                        operand_0: _,
                        operand_1,
                    } = &instr.instruction
                    {
                        // For Mov, find the source value from our setup instructions
                        if let Some(source_setup) = setup
                            .iter()
                            .find(|s: &&SetupInstruction| s.ssa_value.register == *operand_1 as u8)
                        {
                            setup.push(SetupInstruction {
                                ssa_value: ssa_value.clone(),
                                value: source_setup.value.clone(),
                                instruction: instr.clone(),
                            });
                        }
                    }
                }
            }
        }

        // eprintln!("Case with {} keys has {} setup instructions:", keys.len(), setup.len());
        // for (i, s) in setup.iter().enumerate() {
        //     eprintln!("  Setup[{}]: ssa={:?}, value={:?}", i, s.ssa_value, s.value);
        // }

        Some(CaseInfo {
            keys,
            setup,
            target_block: target,
            always_terminates: false,
            source_pc,
            execution_order: 0,
            comparison_block: block_id,
        })
    }

    /// Get the constant value that a register holds at a specific location
    fn get_constant_value_at(
        &self,
        block_id: NodeIndex,
        instruction_idx: usize,
        register: u8,
        cfg: &Cfg<'a>,
    ) -> Option<ConstantValue> {
        let block = &cfg.graph()[block_id];

        // Look backwards from the instruction for the most recent definition of this register
        for instr in block.instructions().iter().take(instruction_idx).rev() {
            let usage = analyze_register_usage(&instr.instruction);

            if let Some(target_reg) = usage.target {
                if target_reg == register {
                    // Found a definition - try to extract the constant value
                    return self.extract_constant_value_from_instruction(&instr.instruction);
                }
            }
        }

        None
    }

    /// Extract constant value from an instruction that defines a register
    fn extract_constant_value_from_instruction(
        &self,
        instr: &UnifiedInstruction,
    ) -> Option<ConstantValue> {
        match instr {
            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                // Get the actual string from the HBC file
                if let Some(ref ctx) = self.expression_context {
                    if let Some(hbc_file) = ctx.hbc_file() {
                        if let Ok(string_value) = hbc_file.strings.get(*operand_1 as u32) {
                            return Some(ConstantValue::String(string_value));
                        } else {
                            panic!("String not found in HBC file");
                        }
                    }
                }
                // Fallback if we can't get the string
                panic!("String not found in HBC file")
            }
            UnifiedInstruction::LoadConstZero { .. } => {
                Some(ConstantValue::Number(OrderedFloat(0.0)))
            }
            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                Some(ConstantValue::Number(OrderedFloat(*operand_1 as f64)))
            }
            UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                Some(ConstantValue::Number(OrderedFloat(*operand_1 as f64)))
            }
            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                Some(ConstantValue::Number(OrderedFloat(*operand_1 as f64)))
            }
            UnifiedInstruction::LoadConstNull { .. } => Some(ConstantValue::Null),
            UnifiedInstruction::LoadConstUndefined { .. } => Some(ConstantValue::Undefined),
            UnifiedInstruction::LoadConstTrue { .. } => Some(ConstantValue::Boolean(true)),
            UnifiedInstruction::LoadConstFalse { .. } => Some(ConstantValue::Boolean(false)),
            UnifiedInstruction::Mov { .. } => {
                // For Mov instructions, we need to trace the source register
                // This will be handled by the caller to look up the value of operand_1
                None
            }
            _ => None, // Non-constant instruction
        }
    }

    /// Get the true successor (taken branch) for a comparison block
    fn get_true_successor(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> Option<NodeIndex> {
        let edges: Vec<_> = cfg.graph().edges(block_id).collect();
        if edges.len() == 2 {
            // Find the true edge
            edges
                .iter()
                .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::True))
                .map(|e| e.target())
        } else {
            None
        }
    }

    /// Get the false successor (not-taken branch) for a comparison block
    fn get_false_successor(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> Option<NodeIndex> {
        let edges: Vec<_> = cfg.graph().edges(block_id).collect();
        if edges.len() == 2 {
            // Find the false edge
            edges
                .iter()
                .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::False))
                .map(|e| e.target())
        } else {
            None
        }
    }

    /// Extract default case setup instructions
    fn extract_default_setup(
        &self,
        default_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa_analysis: &SSAAnalysis,
    ) -> Option<SmallVec<[SetupInstruction; 4]>> {
        // Use the same logic as extract_case_from_block but without the comparison handling
        let block = &cfg.graph()[default_block];
        let mut setup = SmallVec::new();

        // For default block, extract all instructions except the final jump
        let instructions = block.instructions();
        if instructions.is_empty() {
            return Some(setup);
        }

        // Get HBC file from cfg
        let hbc_file = cfg.hbc_file();

        // Process all instructions except the last (which should be a jump)
        for (i, instr) in instructions.iter().enumerate() {
            // Skip the last instruction if it's a jump
            if i == instructions.len() - 1 {
                if matches!(
                    instr.instruction,
                    UnifiedInstruction::Jmp { .. }
                        | UnifiedInstruction::JmpLong { .. }
                        | UnifiedInstruction::JmpTrue { .. }
                        | UnifiedInstruction::JmpFalse { .. }
                        | UnifiedInstruction::JmpUndefined { .. }
                        | UnifiedInstruction::Ret { .. }
                ) {
                    continue;
                }
            }

            // Find SSA value defined by this instruction
            let ssa_value = ssa_analysis
                .definitions
                .iter()
                .find(|def| def.instruction_idx == instr.instruction_index)
                .and_then(|def| ssa_analysis.ssa_values.get(def));

            if let Some(ssa_value) = ssa_value {
                // Extract constant value from LoadConst instructions
                match &instr.instruction {
                    UnifiedInstruction::LoadConstString {
                        operand_0: _,
                        operand_1,
                    } => {
                        if let Ok(string_value) = hbc_file.strings.get(*operand_1 as u32) {
                            setup.push(SetupInstruction {
                                ssa_value: ssa_value.clone(),
                                value: ConstantValue::String(string_value),
                                instruction: instr.clone(),
                            });
                        }
                    }
                    UnifiedInstruction::LoadConstInt {
                        operand_0: _,
                        operand_1,
                    } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Number(OrderedFloat(*operand_1 as f64)),
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstDouble {
                        operand_0: _,
                        operand_1,
                    } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Number(OrderedFloat(*operand_1)),
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstZero { operand_0: _ } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Number(OrderedFloat(0.0)),
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstNull { operand_0: _ } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Null,
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstUndefined { operand_0: _ } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Undefined,
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstTrue { operand_0: _ } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Boolean(true),
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstFalse { operand_0: _ } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Boolean(false),
                            instruction: instr.clone(),
                        });
                    }
                    UnifiedInstruction::LoadConstUInt8 {
                        operand_0: _,
                        operand_1,
                    } => {
                        setup.push(SetupInstruction {
                            ssa_value: ssa_value.clone(),
                            value: ConstantValue::Number(OrderedFloat(*operand_1 as f64)),
                            instruction: instr.clone(),
                        });
                    }
                    _ => {
                        // Other instructions are not considered setup for default case
                    }
                }
            }
        }

        Some(setup)
    }

    /// Detect if multiple cases converge to a shared tail block
    fn detect_shared_tail(
        &self,
        cases: &[CaseInfo],
        default_case: &Option<DefaultCase>,
        postdom: &PostDominatorAnalysis,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
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
        let shared_tail = self.find_common_postdominator(&all_targets, postdom)?;

        // Check if this is a meaningful shared tail (not just exit block)
        if !self.is_meaningful_shared_tail(shared_tail, cfg) {
            return None;
        }

        // Analyze PHI requirements
        let phi_nodes = self.analyze_phi_requirements(shared_tail, cfg, ssa);

        Some(SharedTailInfo {
            block_id: shared_tail,
            phi_nodes,
        })
    }

    /// Find common postdominator
    fn find_common_postdominator(
        &self,
        targets: &[NodeIndex],
        postdom: &PostDominatorAnalysis,
    ) -> Option<NodeIndex> {
        if targets.is_empty() {
            return None;
        }

        if targets.len() == 1 {
            return Some(targets[0]);
        }

        // Find a node that postdominates all targets
        let mut best_candidate = None;

        for &candidate in targets {
            let dominates_all = targets
                .iter()
                .all(|&target| postdom.dominates(candidate, target));

            if dominates_all {
                // Choose the one that's closest (has the highest node index, indicating it's deeper)
                if best_candidate.map_or(true, |best: NodeIndex| candidate.index() > best.index()) {
                    best_candidate = Some(candidate);
                }
            }
        }

        best_candidate
    }

    /// Check if this is a meaningful shared tail (not just exit block)
    fn is_meaningful_shared_tail(&self, tail_block: NodeIndex, cfg: &Cfg<'a>) -> bool {
        // A meaningful shared tail should have:
        // 1. More than just a return/throw instruction
        // 2. Some actual computation or multiple instructions
        // 3. Not be the CFG exit node

        // Simple heuristic: if the block has more than 1 instruction, it's meaningful
        // or if it has exactly 1 instruction that's not just Ret/Throw
        let block = &cfg.graph()[tail_block];
        let instructions = block.instructions();

        if instructions.len() > 1 {
            return true;
        }

        if instructions.len() == 1 {
            match &instructions[0].instruction {
                UnifiedInstruction::Ret { .. } => {
                    // A return is meaningful if it has PHI nodes (multiple paths converge)
                    // For now, consider all return blocks as meaningful for switch handling
                    true
                }
                UnifiedInstruction::Throw { .. } => false,
                _ => true, // Other single instructions are meaningful
            }
        } else {
            false // Empty blocks are not meaningful
        }
    }

    /// Analyze which registers need PHI nodes at the shared tail
    fn analyze_phi_requirements(
        &self,
        tail_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> HashMap<u8, PhiNode> {
        let mut phi_nodes = HashMap::new();

        // Get existing PHI functions at the tail block
        let existing_phis = ssa
            .phi_functions
            .get(&tail_block)
            .map(|phis| phis.as_slice())
            .unwrap_or(&[]);

        // For each PHI function at the tail block, extract the values
        for phi_func in existing_phis {
            let mut values = HashMap::new();

            // Extract constant values from each PHI operand
            for (pred_block, ssa_value) in &phi_func.operands {
                // Find which instruction defined this SSA value and extract its constant
                if let Some(value) = self.extract_constant_from_ssa_value(ssa_value, cfg, ssa) {
                    values.insert(*pred_block, value);
                }
            }

            // Only create our PhiNode if we have values to track
            if !values.is_empty() {
                phi_nodes.insert(
                    phi_func.register,
                    PhiNode {
                        register: phi_func.register,
                        values,
                        ssa_phi_value: Some(phi_func.result.clone()),
                    },
                );
            }
        }

        phi_nodes
    }

    /// Extract constant value from an SSA value by looking at its definition
    fn extract_constant_from_ssa_value(
        &self,
        ssa_value: &crate::cfg::ssa::types::SSAValue,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> Option<ConstantValue> {
        // The SSA value has a definition site
        let def_site = &ssa_value.def_site;
        let block = &cfg.graph()[def_site.block_id];

        // Find the instruction at the definition site
        for instr in block.instructions() {
            if instr.instruction_index != def_site.instruction_idx {
                continue;
            }

            // Check if this instruction defines our register and extract the value
            let usage = analyze_register_usage(&instr.instruction);
            if let Some(target) = usage.target {
                if target == ssa_value.register {
                    // First try to extract a constant value
                    if let Some(value) =
                        self.extract_constant_value_from_instruction(&instr.instruction)
                    {
                        return Some(value);
                    }

                    // If it's a Mov instruction, trace the source
                    if let UnifiedInstruction::Mov { operand_1, .. } = &instr.instruction {
                        // Find the SSA value for the source register at this point
                        for (def, ssa_val) in &ssa.ssa_values {
                            if def.register == *operand_1
                                && def.instruction_idx < def_site.instruction_idx
                            {
                                // Recursively extract from the source
                                return self.extract_constant_from_ssa_value(ssa_val, cfg, ssa);
                            }
                        }
                    }

                    // If it's an Add instruction, try to fold if both operands are constants
                    if let UnifiedInstruction::Add {
                        operand_1,
                        operand_2,
                        ..
                    } = &instr.instruction
                    {
                        // Find the SSA values for both operands that are live at this instruction
                        let left_ssa =
                            self.find_ssa_value_for_use(*operand_1, def_site.instruction_idx, ssa);
                        let right_ssa =
                            self.find_ssa_value_for_use(*operand_2, def_site.instruction_idx, ssa);

                        if let (Some(left_ssa_val), Some(right_ssa_val)) = (left_ssa, right_ssa) {
                            // Try to extract constants from both SSA values
                            let left_const =
                                self.extract_constant_from_ssa_value(left_ssa_val, cfg, ssa);
                            let right_const =
                                self.extract_constant_from_ssa_value(right_ssa_val, cfg, ssa);

                            if let (Some(left), Some(right)) = (left_const, right_const) {
                                // Try to fold the Add operation
                                return self.fold_add_operation(&left, &right);
                            }
                        }
                    }

                    return None;
                }
            }
        }

        None
    }

    /// Find the SSA value for a register use at a specific instruction
    fn find_ssa_value_for_use<'b>(
        &self,
        register: u8,
        at_instruction: InstructionIndex,
        ssa: &'b SSAAnalysis,
    ) -> Option<&'b crate::cfg::ssa::types::SSAValue> {
        // Find the use at this instruction
        let use_site = ssa
            .uses
            .iter()
            .find(|u| u.register == register && u.instruction_idx == at_instruction)?;

        // Find the definition that reaches this use
        let def = ssa.use_def_chains.get(use_site)?;

        // Get the SSA value for this definition
        ssa.ssa_values.get(def)
    }

    /// Fold an Add operation on two constant values
    fn fold_add_operation(
        &self,
        left: &ConstantValue,
        right: &ConstantValue,
    ) -> Option<ConstantValue> {
        match (left, right) {
            // String concatenation
            (ConstantValue::String(s1), ConstantValue::String(s2)) => {
                Some(ConstantValue::String(format!("{}{}", s1, s2)))
            }
            // Number addition
            (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                Some(ConstantValue::Number(OrderedFloat(n1.0 + n2.0)))
            }
            // String + Number (JavaScript coercion)
            (ConstantValue::String(s), ConstantValue::Number(n)) => {
                Some(ConstantValue::String(format!("{}{}", s, n.0)))
            }
            (ConstantValue::Number(n), ConstantValue::String(s)) => {
                Some(ConstantValue::String(format!("{}{}", n.0, s)))
            }
            // Other combinations don't fold to constants
            _ => None,
        }
    }

    /// Check if we should bail out for PHI scenarios
    fn should_bail_out_for_phi_scenarios(&self) -> bool {
        // For now, don't bail out for PHI scenarios
        // In a full implementation, this would check for complex PHI node requirements
        false
    }

    /// Convert a dense switch region (SwitchImm) to AST
    fn convert_dense_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        // Find the SwitchImm instruction
        let dispatch_block = &cfg.graph()[region.dispatch];
        let switch_imm_instr = dispatch_block
            .instructions()
            .iter()
            .find(|instr| matches!(&instr.instruction, UnifiedInstruction::SwitchImm { .. }))
            .ok_or_else(|| {
                SwitchConversionError::TooComplex("No SwitchImm instruction found".to_string())
            })?;

        // Mark dispatch block instructions as rendered
        for instruction in dispatch_block.instructions() {
            block_converter.mark_instruction_rendered(instruction);
        }

        // Extract discriminator register from SwitchImm
        let discriminator_reg = match &switch_imm_instr.instruction {
            UnifiedInstruction::SwitchImm { operand_0, .. } => *operand_0,
            _ => unreachable!(),
        };

        // Create discriminator expression
        let discriminator_expr = self.create_discriminator_expression(discriminator_reg);

        // Get switch table from HBC file
        let hbc_file = self
            .expression_context
            .as_ref()
            .and_then(|ctx| ctx.hbc_file())
            .ok_or_else(|| SwitchConversionError::TooComplex("No HBC file context".to_string()))?;

        // Get function index from block converter
        let function_index = cfg.function_index();

        let switch_table = hbc_file
            .switch_tables
            .get_switch_table_by_instruction(
                function_index,
                switch_imm_instr.instruction_index.into(),
            )
            .ok_or_else(|| {
                SwitchConversionError::TooComplex("No switch table found".to_string())
            })?;

        // Build case statements
        let mut switch_cases = ArenaVec::new_in(self.ast_builder.allocator);

        // Create cases for each value in the switch table
        for (i, switch_case) in switch_table.cases.iter().enumerate() {
            // Find the corresponding case in the region
            let region_case = region.cases.get(i).ok_or_else(|| {
                SwitchConversionError::TooComplex(format!("Case {} not found in region", i))
            })?;

            // Create case test expression
            let case_value = switch_case.value as f64;
            let test_expr =
                Some(self.create_constant_expression(&CaseKey::Num(OrderedFloat(case_value))));

            // Convert case block to statements
            let case_block = &cfg.graph()[region_case.case_head];

            // Convert the entire block
            let case_statements = block_converter
                .convert_block_with_options(
                    case_block,
                    region_case.case_head,
                    cfg.graph(),
                    None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
                    false,
                )
                .map_err(|e| {
                    SwitchConversionError::CaseConversionError(format!(
                        "Failed to convert case body: {}",
                        e
                    ))
                })?;

            // Create the switch case
            let span = Span::default();
            let switch_case_node = self
                .ast_builder
                .switch_case(span, test_expr, case_statements);

            switch_cases.push(switch_case_node);
        }

        // Handle default case if present
        if let Some(default_head) = region.default_head {
            let default_block = &cfg.graph()[default_head];

            // Convert the default block
            let default_statements = block_converter
                .convert_block_with_options(
                    default_block,
                    default_head,
                    cfg.graph(),
                    None::<fn(&crate::hbc::function_table::HbcFunctionInstruction, bool) -> bool>,
                    false,
                )
                .map_err(|e| {
                    SwitchConversionError::CaseConversionError(format!(
                        "Failed to convert default case body: {}",
                        e
                    ))
                })?;

            // Create the default case
            let span = Span::default();
            let default_case = self.ast_builder.switch_case(
                span,
                None, // No test expression for default
                default_statements,
            );

            switch_cases.push(default_case);
        }

        // Create the switch statement
        let span = Span::default();
        let switch_stmt = self
            .ast_builder
            .switch_statement(span, discriminator_expr, switch_cases);

        Ok(vec![Statement::SwitchStatement(
            self.ast_builder.alloc(switch_stmt),
        )])
    }

    /// Check if we should bail out for control flow issues
    fn should_bail_out_for_control_flow(&self) -> bool {
        // For now, don't bail out for control flow issues
        // In a full implementation, this would check for:
        // - Exception-throwing instructions
        // - Complex nested control flow
        // - Irreducible control flow patterns
        false
    }

    /// Check if we should bail out for constant-related issues
    fn should_bail_out_for_constants(&self, cases: &[CaseInfo]) -> bool {
        let mut constant_set = HashSet::new();

        // Check for duplicate constants
        for case in cases {
            for key in &case.keys {
                if !constant_set.insert(key.normalize_for_grouping()) {
                    return true; // Duplicate constant found
                }
            }
        }

        false
    }
}

/// Get all switch blocks including case bodies and infrastructure
/// This includes infrastructure blocks AND all blocks within case bodies
pub fn get_all_switch_blocks_with_bodies(
    region: &SwitchRegion,
    cfg: &crate::cfg::Cfg,
) -> HashSet<NodeIndex> {
    let mut blocks = HashSet::new();

    // Add infrastructure blocks (dispatch and comparison blocks)
    blocks.extend(get_switch_infrastructure_blocks(region, cfg));

    // Add all case heads and their reachable blocks
    // BUT: Don't add case heads that are the join block (they need to be processed after the switch)
    // AND: Don't add case heads that are simple return blocks (shared tail)
    for case in &region.cases {
        // Skip case heads that are the join block - they'll be processed after the switch
        if case.case_head == region.join_block {
            continue;
        }

        // Also skip if this block contains a return statement (likely shared tail)
        // This includes blocks with PHI nodes + return
        let case_block = &cfg.graph()[case.case_head];
        let contains_return = case_block.instructions().iter().any(|instr| {
            matches!(
                &instr.instruction,
                crate::generated::unified_instructions::UnifiedInstruction::Ret { .. }
            )
        });

        if !contains_return {
            blocks.insert(case.case_head);
        }

        // For now, just add the immediate case head
        // In a full implementation, we'd traverse all reachable blocks within the case
        // until we hit a join block or return
    }

    // Add default case head if it exists
    if let Some(default_head) = region.default_head {
        blocks.insert(default_head);
    }

    blocks
}

/// Get only the switch infrastructure blocks (dispatch and comparison blocks)
/// These are blocks that should not be processed as regular statements
/// since they will be consumed during switch generation
pub fn get_switch_infrastructure_blocks(
    region: &SwitchRegion,
    cfg: &crate::cfg::Cfg,
) -> HashSet<NodeIndex> {
    let mut blocks = HashSet::new();

    // Add dispatch block
    blocks.insert(region.dispatch);

    // For sparse switches, we need to add all comparison blocks
    // These are the blocks between dispatch and case heads
    let mut current = region.dispatch;
    for case in &region.cases {
        // Follow false edges to find comparison blocks
        while current != case.case_head {
            blocks.insert(current);
            // Find the false edge
            let false_target = cfg
                .graph()
                .edges(current)
                .find(|e| matches!(e.weight(), crate::cfg::EdgeKind::False))
                .map(|e| e.target());

            if let Some(next) = false_target {
                current = next;
            } else {
                break;
            }
        }
    }

    // IMPORTANT: Don't add case heads or default head
    // These blocks need to be analyzed for nested control flow structures

    // Note: We don't add the join block as it might be shared with other control flow

    blocks
}
