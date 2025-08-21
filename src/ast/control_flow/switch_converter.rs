//! Switch statement AST generation from analyzed switch patterns
//!
//! This module converts analyzed switch patterns into JavaScript switch statement AST nodes

use crate::analysis::value_tracker::ConstantValue;
use crate::cfg::switch_analysis::{
    CaseGroup, CaseInfo, CaseKey, DefaultCase, PhiNode, SharedBlockAnalysis, SharedTailInfo,
    SwitchInfo,
};
use crate::cfg::{
    analysis::SwitchRegion,
    ssa::{DuplicatedSSAValue, SSAValue},
    Cfg,
};
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
use std::collections::{HashMap, HashSet};

/// Error types for switch conversion
#[derive(Debug, thiserror::Error)]
pub enum SwitchConversionError {
    #[error("Invalid switch region: {0}")]
    InvalidRegion(String),
    #[error("Unsupported switch pattern: {0}")]
    UnsupportedPattern(String),
    #[error("Block conversion error: {0}")]
    BlockConversionError(String),
    #[error("Case conversion error: {0}")]
    CaseConversionError(String),
    #[error("Instruction conversion error: {0}")]
    InstructionConversionError(String),
}

/// Switch statement AST converter
pub struct SwitchConverter<'a> {
    ast_builder: &'a OxcAstBuilder<'a>,
}

impl<'a> SwitchConverter<'a> {
    /// Create a new switch converter
    pub fn new(ast_builder: &'a OxcAstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    /// Convert a switch region to statements (compatibility method for block converter)
    pub fn convert_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &'a Cfg<'a>,
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

        // For sparse switch regions, we need to analyze the comparison chain
        self.convert_sparse_switch_region(region, cfg, block_converter)
    }

    /// Convert an analyzed switch pattern to AST
    pub fn convert_switch_pattern(
        &mut self,
        switch_info: &SwitchInfo,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        self.convert_switch_pattern_with_region(switch_info, cfg, block_converter, None)
    }

    /// Convert an analyzed switch pattern to AST with optional region for case body analysis
    pub fn convert_switch_pattern_with_region(
        &mut self,
        switch_info: &SwitchInfo,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        region: Option<&SwitchRegion>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        log::debug!(
            "Converting switch pattern with {} cases, default: {:?}",
            switch_info.cases.len(),
            switch_info
                .default_case
                .as_ref()
                .map(|d| d.target_block.index())
        );
        let mut statements = Vec::new();

        // Group consecutive cases that share the same target and setup
        let case_groups_vec = self.group_consecutive_cases(&switch_info.cases);
        // Allocate in arena to extend lifetime
        let case_groups: Vec<CaseGroup> = case_groups_vec;

        // Identify blocks that are targeted by multiple case groups
        let shared_block_analysis = SharedBlockAnalysis::analyze(&case_groups, cfg);
        let shared_blocks = shared_block_analysis.shared_blocks.clone();

        // Pre-analyze all case groups for nested switches to mark eliminated values
        // This must be done before generating any setup instructions
        for group in case_groups.iter() {
            if !group.keys.is_empty() {
                self.pre_analyze_nested_switches(group, cfg, cfg, block_converter)?;
            }
        }

        // Report which uses have been consumed by this switch statement
        // This must be done BEFORE generating case bodies so that setup instructions
        // can check if values are eliminated
        if let Some(region) = region {
            self.report_switch_consumed_uses(region, block_converter, cfg);
        }

        // Generate switch cases
        let mut switch_cases = ArenaVec::new_in(self.ast_builder.allocator);

        for (i, group) in case_groups.iter().enumerate() {
            // Skip groups with no keys (nested switches comparing against parameters)
            if group.keys.is_empty() {
                continue;
            }

            // For groups with multiple keys, create empty cases for all but the last
            if group.keys.len() > 1 {
                for j in 0..(group.keys.len() - 1) {
                    let test = self.create_constant_expression(&group.keys[j]);
                    let mut empty_case_body = ArenaVec::new_in(self.ast_builder.allocator);

                    // Check if this empty case needs a break statement
                    // This handles the situation where case 0 falls through to case 1
                    // but they shouldn't be grouped because they contribute different values to PHI
                    if j == 0
                        && group.keys.len() > 1
                        && self.should_add_break_for_group(group, i, &case_groups, switch_info, cfg)
                    {
                        // Check if the first key in this group should have its own break
                        // This happens when consecutive cases have different PHI contributions
                        if let Some(shared_tail) = &switch_info.shared_tail {
                            if group.target_block == shared_tail.block_id {
                                // Before generating separate code for the first key, check if all keys
                                // in the group would generate the same code after optimization
                                let all_keys_same_code =
                                    self.check_if_all_keys_generate_same_code(group, cfg);

                                if !all_keys_same_code {
                                    // Generate setup for just this key
                                    let single_key_group = CaseGroup {
                                        keys: vec![group.keys[j].clone()],
                                        target_block: group.target_block,
                                        setup: group.setup.clone(),
                                        always_terminates: group.always_terminates,
                                        first_execution_order: group.first_execution_order,
                                        comparison_blocks: vec![group.comparison_blocks[j]],
                                    };
                                    // For empty cases that go to shared tail, force PHI contributions
                                    let force_phi =
                                        if let Some(shared_tail) = &switch_info.shared_tail {
                                            single_key_group.target_block == shared_tail.block_id
                                        } else {
                                            false
                                        };

                                    self.generate_setup_instructions_with_phi_check(
                                        &mut empty_case_body,
                                        &single_key_group,
                                        block_converter,
                                        cfg,
                                        force_phi,
                                    )?;

                                    let break_stmt =
                                        self.ast_builder.break_statement(Span::default(), None);
                                    empty_case_body.push(Statement::BreakStatement(
                                        self.ast_builder.alloc(break_stmt),
                                    ));
                                }
                            }
                        }
                    }

                    let empty_case =
                        self.ast_builder
                            .switch_case(Span::default(), Some(test), empty_case_body);
                    switch_cases.push(empty_case);
                }
            }

            // Create the actual case with the body for the last key
            let switch_case = self.convert_case_group(
                group,
                cfg,
                block_converter,
                switch_info,
                i,
                &case_groups,
                &shared_block_analysis,
                region,
            )?;
            switch_cases.push(switch_case);
        }

        // Handle default case
        if let Some(default_case) = &switch_info.default_case {
            // Create empty PHI nodes map since we're not tracking them here
            let empty_phi_nodes = HashMap::new();
            let default_switch_case = self.convert_default_case(
                default_case,
                cfg,
                cfg, // Pass the same cfg as full_cfg
                block_converter,
                switch_info.shared_tail.as_ref(),
                &empty_phi_nodes,
                region,
            )?;
            switch_cases.push(default_switch_case);
        }

        // Create discriminator expression
        let discriminant = self.create_discriminator_expression(
            switch_info.discriminator,
            switch_info.discriminator_instruction_index,
            block_converter,
        );

        // Create switch statement
        let switch_stmt =
            self.ast_builder
                .switch_statement(Span::default(), discriminant, switch_cases);

        statements.push(Statement::SwitchStatement(
            self.ast_builder.alloc(switch_stmt),
        ));

        // Convert the shared tail block if it exists
        if let Some(shared_tail) = &switch_info.shared_tail {
            // The shared tail should be converted after the switch statement
            let tail_block = &cfg.graph()[shared_tail.block_id];
            match block_converter.convert_block(tail_block, shared_tail.block_id, cfg.graph()) {
                Ok(tail_stmts) => {
                    statements.extend(tail_stmts);
                    // Mark the shared tail instructions as rendered
                    for instruction in tail_block.instructions() {
                        block_converter.mark_instruction_rendered(instruction);
                    }
                }
                Err(e) => {
                    return Err(SwitchConversionError::BlockConversionError(format!(
                        "Failed to convert shared tail: {}",
                        e
                    )));
                }
            }
        }

        // Convert any other shared blocks that are true post-switch code
        for block_id in &shared_blocks {
            // Skip if this is the main shared tail (already processed)
            if let Some(shared_tail) = &switch_info.shared_tail {
                if *block_id == shared_tail.block_id {
                    continue;
                }
            }

            // Only process blocks that have PHI nodes (indicating convergence of values)
            let has_phi = block_converter
                .ssa_analysis()
                .phi_functions
                .get(block_id)
                .map(|phis| !phis.is_empty())
                .unwrap_or(false);

            if !has_phi {
                continue; // Skip blocks without PHI nodes
            }

            let block = &cfg.graph()[*block_id];

            // Check if this block was already rendered
            if block
                .instructions()
                .iter()
                .all(|instr| block_converter.is_instruction_rendered(instr))
            {
                continue;
            }

            // Convert this shared block
            match block_converter.convert_block(block, *block_id, cfg.graph()) {
                Ok(block_stmts) => {
                    statements.extend(block_stmts);
                    // Mark the block instructions as rendered
                    for instruction in block.instructions() {
                        block_converter.mark_instruction_rendered(instruction);
                    }
                }
                Err(e) => {
                    return Err(SwitchConversionError::BlockConversionError(format!(
                        "Failed to convert shared block {}: {}",
                        block_id.index(),
                        e
                    )));
                }
            }
        }

        Ok(statements)
    }

    /// Group consecutive cases that share the same target and setup
    /// But don't group cases that would have different PHI contributions to the target block
    fn group_consecutive_cases(&self, cases: &[CaseInfo]) -> Vec<CaseGroup> {
        let mut groups = Vec::new();
        let mut i = 0;

        while i < cases.len() {
            // Handle cases with empty keys (e.g., nested switches comparing against parameters)
            let mut group_keys = if cases[i].keys.is_empty() {
                Vec::new()
            } else {
                vec![cases[i].keys[0].clone()]
            };
            let mut comparison_blocks = vec![cases[i].comparison_block];
            let target = cases[i].target_block;
            let setup = cases[i].setup.clone();
            let first_order = cases[i].execution_order;
            let mut j = i + 1;

            // Group consecutive cases that:
            // 1. Have the same target block
            // 2. Are sequential in execution order
            // 3. Either have the same setup or later cases reuse constants from earlier ones
            while j < cases.len()
                && cases[j].target_block == target
                && cases[j].execution_order == cases[j - 1].execution_order + 1
            {
                // Check if this case can be grouped with the current group
                // This happens when:
                // 1. Setup instructions are identical, OR
                // 2. The case has fewer setup instructions but they match the tail of the group's setup
                let can_group = if cases[j].setup.len() == setup.len() {
                    // Same number of setup instructions - check if they match
                    setup.iter().zip(&cases[j].setup).all(|(a, b)| {
                        a.instruction.instruction.name() == b.instruction.instruction.name()
                            && a.ssa_value.register == b.ssa_value.register
                    })
                } else if cases[j].setup.len() < setup.len() {
                    // Fewer setup instructions - check if they match the tail of the group's setup
                    // This handles the pattern where later cases reuse constants loaded by earlier ones
                    let offset = setup.len() - cases[j].setup.len();
                    setup[offset..].iter().zip(&cases[j].setup).all(|(a, b)| {
                        // Simply check if instructions match by name and register
                        a.instruction.instruction.name() == b.instruction.instruction.name()
                            && a.ssa_value.register == b.ssa_value.register
                    })
                } else {
                    false
                };

                if !can_group {
                    break;
                }

                if !cases[j].keys.is_empty() {
                    group_keys.push(cases[j].keys[0].clone());
                }
                comparison_blocks.push(cases[j].comparison_block);
                j += 1;
            }

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

    /// Create discriminator expression from register
    fn create_discriminator_expression(
        &self,
        register: u8,
        instruction_index: InstructionIndex,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Expression<'a> {
        let span = Span::default();

        // Get the variable name from register manager using the correct PC context
        // We need to look up the variable at the instruction that uses it
        let name = if let Some(mapping) = block_converter
            .instruction_converter()
            .register_manager()
            .variable_mapping()
        {
            // Use get_source_variable_name with the instruction's PC to get the correct variable name
            mapping
                .get_source_variable_name(register, instruction_index)
                .cloned()
                .expect("No variable mapping found")
        } else {
            panic!("No variable mapping found")
        };

        let name_atom = self.ast_builder.allocator.alloc_str(&name);
        let identifier = self.ast_builder.identifier_reference(span, name_atom);
        Expression::Identifier(self.ast_builder.alloc(identifier))
    }

    /// Convert a case group to a switch case AST node
    fn convert_case_group(
        &mut self,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        group_index: usize,
        all_groups: &[CaseGroup],
        shared_block_analysis: &SharedBlockAnalysis,
        region: Option<&SwitchRegion>,
    ) -> Result<SwitchCase<'a>, SwitchConversionError> {
        if group.keys.is_empty() {
            return Err(SwitchConversionError::InvalidRegion(
                "Empty case group".to_string(),
            ));
        }

        // Use the last key for the test expression
        let test_expr = self.create_constant_expression(&group.keys[group.keys.len() - 1]);

        // Generate case body
        let mut case_statements = ArenaVec::new_in(self.ast_builder.allocator);

        // Check if the target block is post-switch code
        // A block is post-switch if multiple cases converge to it (even if one targets it directly)
        // We need to handle the case where case 1 directly targets block 9, but case 4 also reaches it
        let is_post_switch_shared = shared_block_analysis.is_post_switch_shared(
            group.target_block,
            group_index,
            all_groups,
        );

        if is_post_switch_shared {
            // This case jumps to a post-switch shared block - generate setup and break
            // Check if this is also the shared tail (PHI node location)
            // OR if the target block has PHI nodes that need contributions
            let force_phi = if let Some(shared_tail) = &switch_info.shared_tail {
                group.target_block == shared_tail.block_id
            } else {
                // Even without a shared tail, check if target has PHI nodes
                block_converter
                    .ssa_analysis()
                    .phi_functions
                    .get(&group.target_block)
                    .map(|phis| !phis.is_empty())
                    .unwrap_or(false)
            };

            self.generate_setup_instructions_with_phi_check(
                &mut case_statements,
                group,
                block_converter,
                cfg,
                force_phi,
            )?;
            // The shared block will be processed after the switch
            // so we don't inline it here
            // The break will be added by the logic at the end of this method
        } else {
            // Handle normal cases that aren't post-switch shared
            if let Some(shared_tail) = &switch_info.shared_tail {
                if group.target_block == shared_tail.block_id {
                    // This case goes directly to the shared tail
                    // Generate code based on PHI node contributions
                    self.generate_case_body_for_shared_tail(
                        &mut case_statements,
                        group,
                        block_converter,
                        cfg,
                    )?;
                } else {
                    // Check if the target block has PHI nodes
                    let has_phi = block_converter
                        .ssa_analysis()
                        .phi_functions
                        .get(&group.target_block)
                        .map(|phis| !phis.is_empty())
                        .unwrap_or(false);

                    if has_phi {
                        log::debug!(
                            "Case group {:?} has PHI nodes, using PHI assignment path",
                            group.keys
                        );
                        // Generate PHI assignments before converting the target block
                        self.generate_case_body_with_phi_assignments(
                            &mut case_statements,
                            group,
                            cfg,
                            block_converter,
                        )?;
                    } else {
                        log::debug!(
                            "Case group {:?} has no PHI nodes, using normal path",
                            group.keys
                        );
                        // First, check for nested switches and mark setup instructions for elimination
                        // BEFORE generating any setup instructions
                        self.pre_analyze_nested_switches(group, cfg, cfg, block_converter)?;

                        // Normal case: generate setup instructions then convert target block
                        self.generate_setup_instructions(
                            &mut case_statements,
                            group,
                            block_converter,
                            cfg,
                        )?;

                        // Check if this case contains nested control flow and convert it
                        let converted_nested = self.convert_nested_control_flow(
                            &mut case_statements,
                            group,
                            cfg,
                            cfg, // Pass the same cfg as full_cfg
                            block_converter,
                            switch_info,
                        )?;

                        if !converted_nested {
                            // Convert the target block normally (no duplication context needed here)
                            self.convert_target_block_normally(
                                &mut case_statements,
                                group,
                                cfg,
                                block_converter,
                                switch_info,
                                region,
                                group_index,
                            )?;
                        }
                    }
                }
            } else {
                // No shared tail - first check for nested switches
                self.pre_analyze_nested_switches(group, cfg, cfg, block_converter)?;

                // Generate setup instructions
                self.generate_setup_instructions(
                    &mut case_statements,
                    group,
                    block_converter,
                    cfg,
                )?;

                // Check if this case contains nested control flow and convert it
                let converted_nested = self.convert_nested_control_flow(
                    &mut case_statements,
                    group,
                    cfg,
                    cfg, // Pass the same cfg as full_cfg
                    block_converter,
                    switch_info,
                )?;

                if !converted_nested {
                    self.convert_target_block_normally(
                        &mut case_statements,
                        group,
                        cfg,
                        block_converter,
                        switch_info,
                        region,
                        group_index,
                    )?;
                }
            }
        }

        // Check if we need to duplicate the next case's code due to fallthrough
        let mut has_duplicated_terminator = false;
        if let Some(next_group) =
            self.needs_fallthrough_duplication(group, group_index, all_groups, cfg, switch_info)
        {
            // Clone the next group to avoid borrow checker issues
            let next_group_clone = next_group.clone();
            // Check if the next group's target block contains a terminator
            let next_target_block = &cfg.graph()[next_group_clone.target_block];
            has_duplicated_terminator = next_target_block.instructions().iter().any(|instr| {
                matches!(
                    &instr.instruction,
                    UnifiedInstruction::Ret { .. }
                        | UnifiedInstruction::Throw { .. }
                        | UnifiedInstruction::ThrowIfUndefinedInst { .. }
                )
            });

            // Duplicate the next case's target block code instead of falling through
            // This avoids conflicts with setup instructions
            // Set duplication context for this fallthrough duplication
            log::debug!(
                "Setting duplication context for fallthrough from case group {:?} to block {}",
                group.keys,
                next_group_clone.target_block.index()
            );
            block_converter.set_duplication_context_for_case_group(group);

            // Don't mark instructions as rendered since they'll be converted again for the actual case
            self.convert_target_block_with_marking(
                &mut case_statements,
                &next_group_clone,
                cfg,
                block_converter,
                switch_info,
                false, // don't mark_as_rendered
                region,
                group_index + 1, // Use next group's index
            )?;

            // Clear duplication context after duplicating the fallthrough block
            log::debug!("Clearing duplication context after fallthrough duplication");
            block_converter.clear_duplication_context();
        }

        // Check if we need to add a break statement
        // Always add break for post-switch shared blocks
        // Don't add break if we duplicated code that contains a terminator
        if !has_duplicated_terminator
            && (is_post_switch_shared
                || self.should_add_break_for_group(
                    group,
                    group_index,
                    all_groups,
                    switch_info,
                    cfg,
                ))
        {
            let break_stmt = self.ast_builder.break_statement(Span::default(), None);
            case_statements.push(Statement::BreakStatement(
                self.ast_builder.alloc(break_stmt),
            ));
        }

        let switch_case =
            self.ast_builder
                .switch_case(Span::default(), Some(test_expr), case_statements);

        Ok(switch_case)
    }

    /// Check if all keys in a group would generate the same code after optimization
    fn check_if_all_keys_generate_same_code(&self, group: &CaseGroup, cfg: &Cfg<'a>) -> bool {
        // For now, we'll check if the group goes to the shared tail and has simple setup
        // In the future, this could do more sophisticated analysis

        // If the target block has actual code, all keys will execute the same code
        if let Some(target_block) = cfg.graph().node_weight(group.target_block) {
            if !target_block.instructions().is_empty() {
                // Target block has instructions, so all cases execute the same code
                return true;
            }
        }

        // If target block is empty (like the shared tail), check if setup generates the same output
        // For cases 5,6,7, they all generate var0_e = "high" after optimization
        // This is a simplified check - in practice we'd need to simulate the optimization

        // Simplify: Trust SSAUsageTracker to handle value elimination properly
        // Don't need instruction-specific checks here

        false
    }

    /// Determine if a break statement should be added for a case group
    fn should_add_break_for_group(
        &self,
        group: &CaseGroup,
        group_index: usize,
        all_groups: &[CaseGroup],
        switch_info: &SwitchInfo,
        _cfg: &Cfg<'a>,
    ) -> bool {
        // Don't add break if the case always terminates
        if group.always_terminates {
            return false;
        }

        // For all other cases, we need to check if a break is needed
        // Even if a case physically falls through to the shared tail in the bytecode,
        // we still need a break in the switch to prevent fallthrough to the next case

        // Check if this is the last group
        let is_last_group = group_index == all_groups.len() - 1;

        // Check if there's a default case
        let has_default = switch_info.default_case.is_some();

        // Don't add break if this is the last case and there's no default
        if is_last_group && !has_default {
            return false;
        }

        // For all other cases, add a break
        // This includes cases that jump to shared tail
        true
    }

    /// Check if this case group needs to duplicate the next case's code due to fallthrough
    fn needs_fallthrough_duplication<'b>(
        &self,
        group: &CaseGroup,
        group_index: usize,
        all_groups: &'b [CaseGroup],
        cfg: &'a Cfg<'a>,
        switch_info: &SwitchInfo,
    ) -> Option<&'b CaseGroup> {
        // Check if this group's target block has no terminating instruction
        let current_target = &cfg.graph()[group.target_block];
        let has_terminator = current_target.instructions().iter().any(|instr| {
            matches!(
                &instr.instruction,
                UnifiedInstruction::Ret { .. }
                    | UnifiedInstruction::Throw { .. }
                    | UnifiedInstruction::Jmp { .. }
                    | UnifiedInstruction::JmpLong { .. }
            )
        });

        if has_terminator {
            return None;
        }

        // Find which case group's target block this one falls through to
        for (i, other_group) in all_groups.iter().enumerate() {
            if i == group_index {
                continue;
            }

            // Skip if the other group targets the shared tail
            // (falling through to shared tail is normal and doesn't need duplication)
            if let Some(shared_tail) = &switch_info.shared_tail {
                if other_group.target_block == shared_tail.block_id {
                    continue;
                }
            }

            // Check if control flow goes from our target to this group's target
            let falls_through = cfg
                .graph()
                .edges(group.target_block)
                .any(|e| e.target() == other_group.target_block);

            if falls_through {
                // Check if the target group has setup instructions that would conflict
                if !other_group.setup.is_empty() {
                    return Some(other_group);
                }
            }
        }

        None
    }

    /// Generate case body with PHI assignments
    fn generate_case_body_with_phi_assignments(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // First, generate setup instructions with PHI check since we know there are PHI nodes
        self.generate_setup_instructions_with_phi_check(
            case_statements,
            group,
            block_converter,
            cfg,
            true, // force_phi_contributions = true because we have PHI nodes at target
        )?;

        // Get PHI functions at the target block
        let ssa = block_converter.ssa_analysis();

        if let Some(phi_functions) = ssa.phi_functions.get(&group.target_block) {
            // For each PHI function, generate an assignment from the appropriate operand
            for phi_function in phi_functions {
                // Find which operand corresponds to our source block
                // We need to use the last comparison block in the group as the predecessor
                let source_block = group
                    .comparison_blocks
                    .last()
                    .copied()
                    .unwrap_or(group.comparison_blocks[0]);

                if let Some(ssa_value) = phi_function.operands.get(&source_block) {
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

                    // Only generate assignment if the names are different
                    if phi_var_name != source_var_name {
                        // Generate assignment: phi_var = source_var
                        let span = Span::default();

                        // Create identifier reference for the PHI variable
                        let phi_var_atom = self.ast_builder.allocator.alloc_str(&phi_var_name);
                        let phi_ident_ref = self
                            .ast_builder
                            .alloc_identifier_reference(span, phi_var_atom);

                        // Create identifier expression for the source variable
                        let source_var_atom =
                            self.ast_builder.allocator.alloc_str(&source_var_name);
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
        }

        // Then convert the target block (no duplication context for PHI path)
        let target_block = &cfg.graph()[group.target_block];
        let result = block_converter.convert_block_with_options(
            target_block,
            group.target_block,
            cfg.graph(),
            None::<fn(&HbcFunctionInstruction, bool) -> bool>,
            false,
        );

        match result {
            Ok(stmts) => case_statements.extend(stmts),
            Err(e) => {
                return Err(SwitchConversionError::BlockConversionError(format!(
                    "Failed to convert target block: {}",
                    e
                )))
            }
        }

        Ok(())
    }

    /// Generate setup instructions with optional PHI contribution forcing
    fn generate_setup_instructions_with_phi_check(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
        force_phi_contributions: bool,
    ) -> Result<(), SwitchConversionError> {
        self.generate_setup_instructions_impl(
            case_statements,
            group,
            block_converter,
            cfg,
            force_phi_contributions,
        )
    }

    /// Generate setup instructions for a case
    fn generate_setup_instructions(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
    ) -> Result<(), SwitchConversionError> {
        self.generate_setup_instructions_impl(
            case_statements,
            group,
            block_converter,
            cfg,
            false, // force_phi_contributions = false by default
        )
    }

    /// Internal implementation of generate_setup_instructions
    fn generate_setup_instructions_impl(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
        force_phi_contributions: bool,
    ) -> Result<(), SwitchConversionError> {
        // Check if the target block is just a return statement
        let _target_is_simple_return =
            if let Some(target_block) = cfg.graph().node_weight(group.target_block) {
                target_block.instructions().len() == 1
                    && matches!(
                        &target_block.instructions()[0].instruction,
                        UnifiedInstruction::Ret { .. }
                    )
            } else {
                false
            };

        // Remove instruction-specific handling - let SSAUsageTracker handle elimination

        log::debug!(
            "generate_setup_instructions for case {:?} with {} setup instructions",
            group.keys,
            group.setup.len()
        );
        for setup_instr in &group.setup {
            // Check if this SSA value has already been fully consumed
            // BUT: If force_phi_contributions is true, we need to generate the assignment
            // even if the SSA value is eliminated, because it contributes to a PHI node
            let is_eliminated = block_converter.is_ssa_value_eliminated(&setup_instr.ssa_value);

            if is_eliminated && !force_phi_contributions {
                log::debug!(
                    "Skipping setup instruction for eliminated SSA value: {:?} in case {:?}",
                    setup_instr.ssa_value,
                    group.keys
                );
                continue;
            }

            // Use normal variable naming for setup instructions (no duplication)
            let var_name = if let Some(mapping) = block_converter
                .instruction_converter()
                .register_manager()
                .variable_mapping()
            {
                // Setup instructions don't need duplicated names - they're unique per case
                mapping
                    .ssa_to_var
                    .get(&setup_instr.ssa_value)
                    .cloned()
                    .unwrap_or_else(|| format!("var{}", setup_instr.ssa_value.register))
            } else {
                panic!("Missing variable mapping!");
            };

            // Allocate the string in the arena to extend its lifetime
            let var_name = self.ast_builder.allocator.alloc_str(&var_name);

            // Create a variable declaration with the constant value
            // Use the stored constant value for all instruction types
            let value_expr = if let Some(const_value) = &setup_instr.value {
                // Convert the constant value to an expression
                self.create_constant_expression_from_value(const_value)
            } else {
                // For other non-constant setup instructions, create an undefined literal
                Expression::Identifier(
                    self.ast_builder.alloc(
                        self.ast_builder
                            .identifier_reference(Span::default(), "undefined"),
                    ),
                )
            };

            // Use SSAUsageTracker to determine declaration strategy
            let ssa_tracker = block_converter.ssa_usage_tracker();
            // For setup instructions, get the declaration strategy for the original SSA value
            let dup_ssa_value = DuplicatedSSAValue::original(setup_instr.ssa_value.clone());
            let declaration_strategy = ssa_tracker.get_declaration_strategy(&dup_ssa_value);

            // Determine if we should declare based on the strategy
            let should_declare = matches!(
                declaration_strategy,
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::DeclareAndInitialize { .. }
            );

            log::debug!(
                "Setup instruction for {} in case {:?}: should_declare={}",
                var_name,
                group.keys,
                should_declare
            );

            // If this SSA value is eliminated AND we don't need to declare it, skip it
            // BUT: Don't skip if we're forcing PHI contributions
            if is_eliminated && !force_phi_contributions && !should_declare {
                log::debug!(
                    "Skipping eliminated setup instruction for SSA value: {:?}",
                    setup_instr.ssa_value
                );
                continue;
            }

            // Note: Variable name tracking is handled separately from SSA tracking

            let stmt = if should_declare {
                // Create variable declaration
                let binding_pattern = self.ast_builder.binding_pattern(
                    self.ast_builder
                        .binding_pattern_kind_binding_identifier(Span::default(), var_name),
                    None::<oxc_ast::ast::TSTypeAnnotation>,
                    false,
                );

                let var_declarator = self.ast_builder.variable_declarator(
                    Span::default(),
                    oxc_ast::ast::VariableDeclarationKind::Let,
                    binding_pattern,
                    Some(value_expr),
                    false,
                );

                let mut declarators = ArenaVec::new_in(self.ast_builder.allocator);
                declarators.push(var_declarator);

                let var_declaration = self.ast_builder.variable_declaration(
                    Span::default(),
                    oxc_ast::ast::VariableDeclarationKind::Let,
                    declarators,
                    false,
                );

                Statement::VariableDeclaration(self.ast_builder.alloc(var_declaration))
            } else {
                // Create assignment expression
                let ident_ref = self
                    .ast_builder
                    .alloc_identifier_reference(Span::default(), var_name);
                let simple_target =
                    oxc_ast::ast::SimpleAssignmentTarget::AssignmentTargetIdentifier(ident_ref);
                let assignment_target = oxc_ast::ast::AssignmentTarget::from(simple_target);

                let assignment_expr = self.ast_builder.expression_assignment(
                    Span::default(),
                    oxc_ast::ast::AssignmentOperator::Assign,
                    assignment_target,
                    value_expr,
                );

                self.ast_builder
                    .statement_expression(Span::default(), assignment_expr)
            };

            // Mark this instruction as rendered to prevent double processing
            block_converter.mark_instruction_rendered(&setup_instr.instruction);

            // Add instruction comment if comment manager is available
            let has_comment_manager = block_converter.has_comment_manager();
            if has_comment_manager {
                let instruction_info = if let Some(hbc_file) = block_converter
                    .instruction_converter()
                    .get_expression_context()
                    .hbc_file()
                {
                    format!(
                        "PC {}: {} (setup instr for case)",
                        setup_instr.instruction.offset,
                        setup_instr.instruction.format_instruction(hbc_file)
                    )
                } else {
                    panic!("HBC file not found");
                };

                if let Some(comment_manager) = block_converter.comment_manager_mut() {
                    comment_manager.add_comment(
                        &stmt,
                        instruction_info,
                        crate::ast::comments::CommentKind::Line,
                        crate::ast::comments::CommentPosition::Leading,
                    );
                }
            }

            // Add SSA comment if enabled
            if block_converter.include_ssa_comments() {
                // Format SSA info for this setup instruction
                let ssa_info = format!(
                    " SSA: {} = r{} (version {})",
                    var_name, setup_instr.ssa_value.register, setup_instr.ssa_value.version
                );

                if let Some(comment_manager) = block_converter.comment_manager_mut() {
                    comment_manager.add_comment(
                        &stmt,
                        ssa_info,
                        crate::ast::comments::CommentKind::Line,
                        crate::ast::comments::CommentPosition::Leading,
                    );
                }
            }

            case_statements.push(stmt);
        }
        Ok(())
    }

    /// Convert target block normally
    fn convert_target_block_normally(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        region: Option<&SwitchRegion>,
        group_index: usize,
    ) -> Result<(), SwitchConversionError> {
        self.convert_target_block_with_marking(
            case_statements,
            group,
            cfg,
            block_converter,
            switch_info,
            true, // mark_as_rendered
            region,
            group_index,
        )
    }

    /// Convert target block with control over instruction marking
    fn convert_target_block_with_marking(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        mark_as_rendered: bool,
        region: Option<&SwitchRegion>,
        _group_index: usize,
    ) -> Result<(), SwitchConversionError> {
        // Don't convert the shared tail block here - it will be converted after the switch
        if let Some(shared_tail) = &switch_info.shared_tail {
            if group.target_block == shared_tail.block_id {
                return Ok(());
            }
        }

        let target_block = &cfg.graph()[group.target_block];

        // Check if this target block is also a switch dispatch block
        // by looking for switch comparison instructions at the start
        let target_instructions = target_block.instructions();
        let has_switch_comparison = target_instructions.iter().any(|instr| {
            matches!(
                &instr.instruction,
                UnifiedInstruction::JStrictEqual { .. }
                    | UnifiedInstruction::JStrictEqualLong { .. }
                    | UnifiedInstruction::JStrictNotEqual { .. }
                    | UnifiedInstruction::JStrictNotEqualLong { .. }
            )
        });

        // Also check if there are any non-comparison instructions before the first comparison
        let first_comparison_idx = if has_switch_comparison {
            target_instructions.iter().position(|instr| {
                matches!(
                    &instr.instruction,
                    UnifiedInstruction::JStrictEqual { .. }
                        | UnifiedInstruction::JStrictEqualLong { .. }
                        | UnifiedInstruction::JStrictNotEqual { .. }
                        | UnifiedInstruction::JStrictNotEqualLong { .. }
                )
            })
        } else {
            None
        };

        let is_also_switch_dispatch =
            has_switch_comparison && first_comparison_idx.map(|idx| idx > 0).unwrap_or(false);

        if is_also_switch_dispatch {
            // This block is both a case target and a switch dispatch
            // We need to convert only the case body instructions (before the switch dispatch)

            if let Some(comparison_idx) = first_comparison_idx {
                // Convert only instructions before the switch comparison
                let mut case_body_instructions = Vec::new();
                for i in 0..comparison_idx {
                    let instr = &target_instructions[i];
                    // Skip if already marked as rendered
                    if !block_converter.is_instruction_rendered(instr) {
                        case_body_instructions.push(instr.clone());
                    }
                }

                if !case_body_instructions.is_empty() {
                    // Create a temporary block with just these instructions
                    let temp_block =
                        crate::cfg::Block::new(target_block.start_pc(), case_body_instructions);

                    // For partial blocks, we can't use convert_block_with_nested_control_flow
                    // since it expects a complete block. Use the basic conversion.
                    match block_converter.convert_block_with_marking_control(
                        &temp_block,
                        group.target_block,
                        cfg.graph(),
                        None::<fn(&HbcFunctionInstruction, bool) -> bool>,
                        false,
                        mark_as_rendered,
                    ) {
                        Ok(stmts) => case_statements.extend(stmts),
                        Err(e) => {
                            return Err(SwitchConversionError::BlockConversionError(format!(
                                "Failed to convert case body in dispatch block: {}",
                                e
                            )))
                        }
                    }
                }
                // The switch dispatch part will be handled when the nested switch is processed
                return Ok(());
            }
        }

        // Check if we have case body analysis available
        let case_analysis = region.and_then(|r| {
            // Find the case index for this group's target block
            r.cases
                .iter()
                .position(|case| case.case_head == group.target_block)
                .and_then(|idx| r.case_analyses.get(&idx))
        });

        // Normal case: convert the entire block
        // Use convert_block_with_nested_control_flow to handle nested switches properly
        if mark_as_rendered {
            if let Some(analysis) = case_analysis {
                // We have case body analysis - use it
                log::debug!(
                    "Using case body analysis for block {} with {} blocks",
                    group.target_block.index(),
                    analysis.blocks.len()
                );
                match block_converter.convert_analyzed_region(analysis, cfg, group.target_block) {
                    Ok(stmts) => {
                        case_statements.extend(stmts);
                        // Mark all blocks in the analysis as rendered
                        for &block_id in &analysis.blocks {
                            if let Some(block) = cfg.graph().node_weight(block_id) {
                                for instruction in block.instructions() {
                                    block_converter.mark_instruction_rendered(instruction);
                                }
                            }
                        }
                    }
                    Err(e) => {
                        return Err(SwitchConversionError::BlockConversionError(format!(
                            "Failed to convert analyzed region: {}",
                            e
                        )))
                    }
                }
            } else {
                match block_converter.convert_block_with_nested_control_flow(
                    target_block,
                    group.target_block,
                    cfg,
                ) {
                    Ok(stmts) => {
                        case_statements.extend(stmts);
                        // Mark instructions as rendered after successful conversion
                        for instruction in target_block.instructions() {
                            block_converter.mark_instruction_rendered(instruction);
                        }
                    }
                    Err(e) => {
                        return Err(SwitchConversionError::BlockConversionError(format!(
                            "Failed to convert target block: {}",
                            e
                        )))
                    }
                }
            }
        } else {
            // When not marking as rendered (for fallthrough duplication)
            match block_converter.convert_block_with_marking_control(
                target_block,
                group.target_block,
                cfg.graph(),
                None::<fn(&HbcFunctionInstruction, bool) -> bool>,
                false,
                mark_as_rendered,
            ) {
                Ok(stmts) => case_statements.extend(stmts),
                Err(e) => {
                    return Err(SwitchConversionError::BlockConversionError(format!(
                        "Failed to convert target block: {}",
                        e
                    )))
                }
            }
        }
        Ok(())
    }

    /// Generate case body for cases that go directly to shared tail
    fn generate_case_body_for_shared_tail(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
    ) -> Result<(), SwitchConversionError> {
        // For cases that jump directly to the shared tail, we need to:
        // 1. Generate setup instructions (including PHI contributions)
        // 2. Add a break statement

        // Special handling: for break-only cases, we need to generate PHI contributions
        // even if the SSA values are marked as eliminated
        self.generate_setup_instructions_with_phi_check(
            case_statements,
            group,
            block_converter,
            cfg,
            true, // force_phi_contributions = true for shared tail cases
        )?;

        // Break is added by the caller based on should_add_break_for_group
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

        // Otherwise, we might need to trace through the CFG
        // For now, return None for complex cases
        None
    }

    /// Create a constant expression from a constant value
    fn create_constant_expression_from_value(&self, value: &ConstantValue) -> Expression<'a> {
        let span = Span::default();
        match value {
            ConstantValue::Number(n) => {
                let literal = self
                    .ast_builder
                    .numeric_literal(span, *n, None, NumberBase::Decimal);
                Expression::NumericLiteral(self.ast_builder.alloc(literal))
            }
            ConstantValue::String(s) => {
                // Allocate string in arena to extend lifetime
                let s_alloc = self.ast_builder.allocator.alloc_str(s);
                let literal = self.ast_builder.string_literal(span, s_alloc, None);
                Expression::StringLiteral(self.ast_builder.alloc(literal))
            }
            ConstantValue::Boolean(b) => {
                let literal = self.ast_builder.boolean_literal(span, *b);
                Expression::BooleanLiteral(self.ast_builder.alloc(literal))
            }
            ConstantValue::Null => {
                let literal = self.ast_builder.null_literal(span);
                Expression::NullLiteral(self.ast_builder.alloc(literal))
            }
            ConstantValue::Undefined => {
                let ident = self
                    .ast_builder
                    .identifier_reference(span, self.ast_builder.allocator.alloc_str("undefined"));
                Expression::Identifier(self.ast_builder.alloc(ident))
            }
        }
    }

    /// Convert default case to switch case AST node
    fn convert_default_case(
        &mut self,
        default_case: &DefaultCase,
        cfg: &'a Cfg<'a>,
        full_cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        shared_tail: Option<&SharedTailInfo>,
        _all_phi_nodes: &HashMap<u8, PhiNode>,
        region: Option<&SwitchRegion>,
    ) -> Result<SwitchCase<'a>, SwitchConversionError> {
        log::debug!(
            "Converting default case with target block {}",
            default_case.target_block.index()
        );

        // Check if the target block's instructions are already rendered
        let target_block = &cfg.graph()[default_case.target_block];
        let already_rendered = target_block
            .instructions()
            .iter()
            .all(|instr| block_converter.is_instruction_rendered(instr));
        if already_rendered {
            log::warn!(
                "Default case target block {} already has all instructions rendered!",
                default_case.target_block.index()
            );
        } else {
            log::debug!(
                "Default case target block {} has {} instructions, not yet rendered",
                default_case.target_block.index(),
                target_block.instructions().len()
            );
        }

        let mut case_statements = ArenaVec::new_in(self.ast_builder.allocator);

        // Handle different default case patterns
        if let Some(shared_tail) = shared_tail {
            // Check if default goes directly to shared tail
            if default_case.target_block == shared_tail.block_id {
                // Generate setup instructions for the default case
                if !default_case.setup.is_empty() {
                    let temp_group = CaseGroup {
                        keys: vec![],
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
                        cfg,
                    )?;
                }

                // If the target block is not the shared tail itself, convert it
                if default_case.target_block != shared_tail.block_id {
                    // Check if the default target is a nested switch
                    let mut converted_nested = false;

                    let ssa = block_converter.ssa_analysis();
                    if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                        for region in &switch_analysis.regions {
                            if region.dispatch == default_case.target_block {
                                // DO NOT mark blocks as rendered before conversion
                                // The nested switch converter needs to process all blocks

                                // Convert the nested switch
                                let mut nested_converter = SwitchConverter::new(self.ast_builder);
                                match nested_converter.convert_switch_region(
                                    region,
                                    full_cfg,
                                    block_converter,
                                ) {
                                    Ok(nested_stmts) => {
                                        case_statements.extend(nested_stmts);
                                        converted_nested = true;

                                        // NOW mark the switch region as rendered after successful conversion
                                        self.mark_switch_region_as_rendered(
                                            region,
                                            block_converter,
                                            cfg,
                                        );
                                        break;
                                    }
                                    Err(e) => {
                                        log::error!(
                                            "Failed to convert nested switch in default case: {:?}",
                                            e
                                        );
                                    }
                                }
                            }
                        }
                    }

                    if !converted_nested {
                        let target_block = &cfg.graph()[default_case.target_block];
                        log::debug!(
                            "Converting default case target block {}",
                            default_case.target_block.index()
                        );

                        match block_converter.convert_block_with_nested_control_flow(
                            target_block,
                            default_case.target_block,
                            full_cfg,
                        ) {
                            Ok(stmts) => {
                                log::debug!(
                                    "Default case target block {} converted with {} statements",
                                    default_case.target_block.index(),
                                    stmts.len()
                                );
                                case_statements.extend(stmts);
                                // Mark the default case instructions as rendered
                                for instruction in target_block.instructions() {
                                    block_converter.mark_instruction_rendered(instruction);
                                }
                            }
                            Err(e) => {
                                return Err(SwitchConversionError::BlockConversionError(format!(
                                    "Failed to convert default target block: {}",
                                    e
                                )))
                            }
                        }
                    }
                }
                // Add break for shared tail (the shared tail itself will be converted after the switch)
                let break_stmt = self.ast_builder.break_statement(Span::default(), None);
                case_statements.push(Statement::BreakStatement(
                    self.ast_builder.alloc(break_stmt),
                ));
            } else {
                // Default case has its own target
                // First generate setup instructions if any
                if !default_case.setup.is_empty() {
                    let temp_group = CaseGroup {
                        keys: vec![],
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
                        cfg,
                    )?;
                }

                // Then convert the target block
                // Check if the default target is a nested switch
                let mut converted_nested = false;

                let ssa = block_converter.ssa_analysis();
                if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                    for region in &switch_analysis.regions {
                        if region.dispatch == default_case.target_block {
                            // Don't mark the dispatch block as rendered yet - let the nested
                            // switch converter handle it, as it needs to process setup instructions

                            // Convert the nested switch
                            let mut nested_converter = SwitchConverter::new(self.ast_builder);
                            match nested_converter.convert_switch_region(
                                region,
                                full_cfg,
                                block_converter,
                            ) {
                                Ok(nested_stmts) => {
                                    log::debug!(
                                        "Nested switch region {} converted successfully",
                                        region.dispatch.index()
                                    );
                                    log::debug!(
                                        "Nested switch has default: {:?}",
                                        region.default_head
                                    );
                                    case_statements.extend(nested_stmts);
                                    converted_nested = true;
                                    break;
                                }
                                Err(e) => {
                                    log::error!(
                                        "Failed to convert nested switch in default case: {:?}",
                                        e
                                    );
                                }
                            }
                        }
                    }
                }

                if !converted_nested {
                    let target_block = &cfg.graph()[default_case.target_block];
                    match block_converter.convert_block_with_nested_control_flow(
                        target_block,
                        default_case.target_block,
                        full_cfg,
                    ) {
                        Ok(stmts) => {
                            case_statements.extend(stmts);
                            // Mark the default case instructions as rendered
                            for instruction in target_block.instructions() {
                                block_converter.mark_instruction_rendered(instruction);
                            }
                        }
                        Err(e) => {
                            return Err(SwitchConversionError::BlockConversionError(format!(
                                "Failed to convert default target block: {}",
                                e
                            )))
                        }
                    }
                }
            }
        } else {
            // No shared tail - just generate setup and convert target normally
            // First generate setup instructions if any
            if !default_case.setup.is_empty() {
                let temp_group = CaseGroup {
                    keys: vec![],
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
                    cfg,
                )?;
            }

            // Then convert the target block normally
            // Check if the default target is a nested switch
            let mut converted_nested = false;

            let ssa = block_converter.ssa_analysis();
            if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                for region in &switch_analysis.regions {
                    if region.dispatch == default_case.target_block {
                        // DO NOT mark blocks as rendered before conversion
                        // The nested switch converter needs to process all blocks

                        // Convert the nested switch
                        let mut nested_converter = SwitchConverter::new(self.ast_builder);
                        match nested_converter.convert_switch_region(
                            region,
                            full_cfg,
                            block_converter,
                        ) {
                            Ok(nested_stmts) => {
                                case_statements.extend(nested_stmts);
                                converted_nested = true;

                                // NOW mark the switch region as rendered after successful conversion
                                self.mark_switch_region_as_rendered(region, block_converter, cfg);
                                break;
                            }
                            Err(e) => {
                                log::error!(
                                    "Failed to convert nested switch in default case: {:?}",
                                    e
                                );
                            }
                        }
                    }
                }
            }

            if !converted_nested {
                // Check if we have case body analysis for the default case
                let default_case_analysis = region.and_then(|r| r.case_analyses.get(&usize::MAX));

                if let Some(analysis) = default_case_analysis {
                    // We have case body analysis for default - use it
                    log::debug!(
                        "Using case body analysis for default block {} with {} blocks",
                        default_case.target_block.index(),
                        analysis.blocks.len()
                    );
                    match block_converter.convert_analyzed_region(
                        analysis,
                        full_cfg,
                        default_case.target_block,
                    ) {
                        Ok(stmts) => {
                            case_statements.extend(stmts);
                            // Mark all blocks in the analysis as rendered
                            for &block_id in &analysis.blocks {
                                let block = &full_cfg.graph()[block_id];
                                for instruction in block.instructions() {
                                    block_converter.mark_instruction_rendered(instruction);
                                }
                            }
                        }
                        Err(e) => {
                            return Err(SwitchConversionError::BlockConversionError(format!(
                                "Failed to convert default case with analysis: {}",
                                e
                            )))
                        }
                    }
                } else {
                    // Fallback to regular conversion
                    let target_block = &cfg.graph()[default_case.target_block];
                    log::debug!(
                        "Converting default case target block {} with {} instructions",
                        default_case.target_block.index(),
                        target_block.instructions().len()
                    );

                    // Check if the target block ends with a conditional jump
                    let has_conditional = target_block.instructions().iter().any(|instr| {
                        matches!(
                            &instr.instruction,
                            UnifiedInstruction::JStrictEqual { .. }
                                | UnifiedInstruction::JStrictEqualLong { .. }
                                | UnifiedInstruction::JEqual { .. }
                                | UnifiedInstruction::JEqualLong { .. }
                                | UnifiedInstruction::JLess { .. }
                                | UnifiedInstruction::JLessLong { .. }
                                | UnifiedInstruction::JLessEqual { .. }
                                | UnifiedInstruction::JLessEqualLong { .. }
                                | UnifiedInstruction::JGreater { .. }
                                | UnifiedInstruction::JGreaterLong { .. }
                                | UnifiedInstruction::JGreaterEqual { .. }
                                | UnifiedInstruction::JGreaterEqualLong { .. }
                        )
                    });

                    if has_conditional {
                        log::debug!("Default case target block {} has conditional jump, converting with nested control flow",
                            default_case.target_block.index());
                    }

                    // Use convert_block_with_nested_control_flow to handle conditionals
                    match block_converter.convert_block_with_nested_control_flow(
                        target_block,
                        default_case.target_block,
                        full_cfg,
                    ) {
                        Ok(stmts) => {
                            log::debug!(
                                "Default case conversion produced {} statements",
                                stmts.len()
                            );
                            case_statements.extend(stmts);
                            // Mark the default case instructions as rendered so they won't be processed again
                            for instruction in target_block.instructions() {
                                block_converter.mark_instruction_rendered(instruction);
                            }
                        }
                        Err(e) => {
                            return Err(SwitchConversionError::BlockConversionError(format!(
                                "Failed to convert default target block: {}",
                                e
                            )))
                        }
                    }
                }
            }
        }

        // Create default case (test expression is None)
        let default_case = self
            .ast_builder
            .switch_case(Span::default(), None, case_statements);

        Ok(default_case)
    }

    /// Create constant expression from case key
    fn create_constant_expression(&self, key: &CaseKey) -> Expression<'a> {
        let span = Span::default();
        match key {
            CaseKey::Number(n) => {
                let literal =
                    self.ast_builder
                        .numeric_literal(span, n.0, None, NumberBase::Decimal);
                Expression::NumericLiteral(self.ast_builder.alloc(literal))
            }
            CaseKey::String(s) => {
                // Allocate string in arena to extend lifetime
                let s_alloc = self.ast_builder.allocator.alloc_str(s);
                let literal = self.ast_builder.string_literal(span, s_alloc, None);
                Expression::StringLiteral(self.ast_builder.alloc(literal))
            }
            CaseKey::Boolean(b) => {
                let literal = self.ast_builder.boolean_literal(span, *b);
                Expression::BooleanLiteral(self.ast_builder.alloc(literal))
            }
            CaseKey::Null => {
                let literal = self.ast_builder.null_literal(span);
                Expression::NullLiteral(self.ast_builder.alloc(literal))
            }
            CaseKey::Undefined => {
                let ident = self
                    .ast_builder
                    .identifier_reference(span, self.ast_builder.allocator.alloc_str("undefined"));
                Expression::Identifier(self.ast_builder.alloc(ident))
            }
        }
    }

    /// Convert a dense switch region (SwitchImm) to AST
    fn convert_dense_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut all_statements = Vec::new();

        // Find the SwitchImm instruction to get the discriminator
        let dispatch_block = &cfg.graph()[region.dispatch];
        let switch_imm_instr = dispatch_block
            .instructions()
            .iter()
            .find(|instr| matches!(&instr.instruction, UnifiedInstruction::SwitchImm { .. }))
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion("No SwitchImm instruction found".to_string())
            })?;

        // Extract discriminator register from SwitchImm
        let discriminator_reg = match &switch_imm_instr.instruction {
            UnifiedInstruction::SwitchImm { operand_0, .. } => *operand_0,
            _ => unreachable!(),
        };

        // Mark only the SwitchImm instruction as rendered
        block_converter.mark_instruction_rendered(switch_imm_instr);

        // Convert all other instructions in the dispatch block
        // This will process LoadParam and any other setup instructions
        match block_converter.convert_block(dispatch_block, region.dispatch, cfg.graph()) {
            Ok(stmts) => all_statements.extend(stmts),
            Err(e) => {
                return Err(SwitchConversionError::BlockConversionError(format!(
                    "Failed to convert dispatch block: {}",
                    e
                )));
            }
        }

        // Get switch table from HBC file
        let hbc_file = block_converter
            .instruction_converter()
            .get_expression_context()
            .hbc_file()
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion("No HBC file context".to_string())
            })?;

        let function_index = cfg.function_index();

        let switch_table = hbc_file
            .switch_tables
            .get_switch_table_by_instruction(
                function_index,
                switch_imm_instr.instruction_index.into(),
            )
            .ok_or_else(|| {
                SwitchConversionError::InvalidRegion("No switch table found".to_string())
            })?;

        // Get SSA analysis
        let ssa = block_converter.ssa_analysis();

        // Dense switches don't have setup instructions - all setup is in the dispatch block
        // which has already been processed
        let setup_instructions = SmallVec::new();

        // Build cases from the switch table and region data
        let mut cases = Vec::new();

        // Process each case in the switch table
        for (i, switch_case) in switch_table.cases.iter().enumerate() {
            // Find the corresponding region case
            if let Some(region_case) = region.cases.get(i) {
                // Check if the target block always terminates
                let target_block = &cfg.graph()[region_case.case_head];
                let always_terminates = target_block.instructions().iter().any(|instr| {
                    matches!(
                        &instr.instruction,
                        UnifiedInstruction::Ret { .. }
                            | UnifiedInstruction::Throw { .. }
                            | UnifiedInstruction::ThrowIfUndefinedInst { .. }
                    )
                });

                cases.push(CaseInfo {
                    keys: vec![CaseKey::Number(OrderedFloat(switch_case.value as f64))],
                    comparison_block: region.dispatch,
                    target_block: region_case.case_head,
                    setup: setup_instructions.clone(),
                    always_terminates,
                    execution_order: i,
                });
            }
        }

        // Handle default case
        // Default case doesn't have setup instructions - it's just a regular block
        let default_case = region.default_head.map(|default_head| DefaultCase {
            target_block: default_head,
            setup: SmallVec::new(), // No setup for default case
        });

        // Detect shared tail using post-dominator analysis
        let _postdom = cfg.analyze_post_dominators().ok_or_else(|| {
            SwitchConversionError::InvalidRegion("Failed to compute post-dominators".to_string())
        })?;

        let shared_tail = if region.join_block != region.dispatch {
            // Check if the join block has PHI nodes
            let phi_nodes = if let Some(phis) = ssa.phi_functions.get(&region.join_block) {
                let mut nodes = HashMap::new();
                for phi in phis {
                    nodes.insert(
                        phi.register,
                        PhiNode {
                            register: phi.register,
                            values: HashMap::new(),
                            ssa_phi_value: Some(phi.result.clone()),
                        },
                    );
                }
                nodes
            } else {
                HashMap::new()
            };

            Some(SharedTailInfo {
                block_id: region.join_block,
                phi_nodes,
            })
        } else {
            None
        };

        // Create the SwitchInfo structure
        let switch_info = SwitchInfo {
            discriminator: discriminator_reg,
            discriminator_instruction_index: switch_imm_instr.instruction_index,
            cases,
            default_case,
            shared_tail,
        };

        let switch_statements = self.convert_switch_pattern_with_region(
            &switch_info,
            cfg,
            block_converter,
            Some(region),
        )?;
        all_statements.extend(switch_statements);

        Ok(all_statements)
    }

    /// Convert a sparse switch region to AST
    fn convert_sparse_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut all_statements = Vec::new();

        // Get SSA analysis from block converter
        let ssa = block_converter.ssa_analysis();

        // We need post-dominator analysis for switch detection
        let postdom = cfg.analyze_post_dominators().ok_or_else(|| {
            SwitchConversionError::InvalidRegion("Failed to compute post-dominators".to_string())
        })?;

        // Try to detect sparse switch pattern
        // Get HBC file from block converter for string constant extraction
        let hbc_file = block_converter
            .instruction_converter()
            .get_expression_context()
            .hbc_file();

        let analyzer = if let Some(hbc) = hbc_file {
            crate::cfg::switch_analysis::SparseSwitchAnalyzer::with_hbc_file(hbc)
        } else {
            crate::cfg::switch_analysis::SparseSwitchAnalyzer::new()
        };

        // Detect the switch pattern
        let mut switch_info = analyzer
            .detect_switch_pattern(region.dispatch, cfg, ssa, &postdom)
            .ok_or_else(|| {
                SwitchConversionError::UnsupportedPattern(
                    "Could not detect switch pattern".to_string(),
                )
            })?;

        // Override the default case with the one from the region analysis if needed
        if switch_info.default_case.is_none() && region.default_head.is_some() {
            log::debug!(
                "Sparse switch analyzer missed default case, using region default_head: {:?}",
                region.default_head
            );
            switch_info.default_case = region.default_head.map(|block_id| DefaultCase {
                target_block: block_id,
                setup: SmallVec::new(),
            });
        }

        // Process the dispatch block
        let dispatch_block = &cfg.graph()[region.dispatch];
        let dispatch_instructions = dispatch_block.instructions();

        // Find the comparison instruction and its dependencies
        let mut comparison_idx = None;
        let mut const_load_idx = None;
        let mut const_reg = None;

        // Find the comparison instruction
        for (i, instr) in dispatch_instructions.iter().enumerate() {
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
                    comparison_idx = Some(i);
                    // Figure out which operand is the discriminator and which is the constant
                    if *operand_1 == switch_info.discriminator {
                        const_reg = Some(*operand_2);
                    } else if *operand_2 == switch_info.discriminator {
                        const_reg = Some(*operand_1);
                    }
                    break;
                }
                _ => {}
            }
        }

        let comparison_idx = comparison_idx.ok_or_else(|| {
            SwitchConversionError::InvalidRegion(
                "No comparison found in dispatch block".to_string(),
            )
        })?;

        let const_reg = const_reg.ok_or_else(|| {
            SwitchConversionError::InvalidRegion(
                "Could not determine constant register".to_string(),
            )
        })?;

        // Find the instruction that loads the constant
        for (i, instr) in dispatch_instructions
            .iter()
            .take(comparison_idx)
            .enumerate()
        {
            let usage =
                crate::generated::instruction_analysis::analyze_register_usage(&instr.instruction);
            if usage.target == Some(const_reg) {
                const_load_idx = Some(i);
                break;
            }
        }

        // If we didn't find a constant load in this block, check if the register
        // contains a parameter or was defined in a parent scope
        if const_load_idx.is_none() {
            // Check if this is a parameter by looking at SSA analysis
            if let Some(mapping) = block_converter
                .instruction_converter()
                .register_manager()
                .variable_mapping()
            {
                // Get the SSA value for this register at this point
                let pc = dispatch_instructions[comparison_idx].offset;
                let instruction_idx = crate::hbc::InstructionIndex::from(pc.value());
                if let Some(ssa_value) = mapping
                    .register_before_pc
                    .get(&(const_reg, instruction_idx))
                {
                    // Check if this is a parameter
                    if let Some(var_name) = mapping.ssa_to_var.get(ssa_value) {
                        if var_name.starts_with("param") || var_name.starts_with("arg") {
                            // This is a parameter, so we don't need to find a const load
                            // We'll handle this case differently below
                        }
                    }
                }
            }
        }

        // Only fail if we need a const load and didn't find one
        let needs_const_load = const_load_idx.is_none();
        if needs_const_load {
            // For nested switches, we might not have a const load in the dispatch block
            // Skip marking const load as rendered if we don't have one
        }

        // Mark the comparison as rendered
        block_converter.mark_instruction_rendered(&dispatch_instructions[comparison_idx]);

        // DON'T mark the const load as rendered - it might be used elsewhere
        // In particular, if the constant register is used in other places (like the default case),
        // we need to ensure the variable declaration is still emitted.
        // The switch statement will still work correctly because it will reference the variable
        // that was declared by the const load instruction.

        // Also mark any dispatch block instructions that are setup instructions for case 0
        if let Some(first_case) = switch_info.cases.first() {
            if first_case.comparison_block == region.dispatch {
                // Case 0's comparison is in the dispatch block
                for setup_instr in &first_case.setup {
                    // Find this instruction in the dispatch block and mark it as rendered
                    for (i, instr) in dispatch_instructions.iter().enumerate() {
                        if instr.offset == setup_instr.instruction.offset {
                            block_converter.mark_instruction_rendered(&dispatch_instructions[i]);

                            // Check if this SSA value is used outside case 0
                            let _case0_group = CaseGroup {
                                keys: first_case.keys.clone(),
                                target_block: first_case.target_block,
                                setup: first_case.setup.clone(),
                                always_terminates: first_case.always_terminates,
                                first_execution_order: first_case.execution_order,
                                comparison_blocks: vec![first_case.comparison_block],
                            };

                            // TODO: Implement cross-case analysis to check if this setup value
                            // is used in other cases. If not, mark its uses in case 0 as consumed.
                            // This requires:
                            // 1. Check all other cases to see if they use this SSA value
                            // 2. If only case 0 uses it, mark those uses as consumed
                            // 3. This would allow elimination of setup instructions that are
                            //    only used by a single case
                            break;
                        }
                    }
                }
            }
        }

        // Convert all other instructions in the dispatch block
        // convert_block will skip the instructions we marked as rendered
        match block_converter.convert_block(dispatch_block, region.dispatch, cfg.graph()) {
            Ok(stmts) => all_statements.extend(stmts),
            Err(e) => {
                return Err(SwitchConversionError::BlockConversionError(format!(
                    "Failed to convert dispatch block: {}",
                    e
                )));
            }
        }

        // Mark all other comparison blocks as fully rendered
        let mut comparison_blocks = HashSet::new();
        let mut current = region.dispatch;
        let mut visited = HashSet::new();

        loop {
            if !visited.insert(current) {
                break;
            }

            comparison_blocks.insert(current);

            // Skip the dispatch block - we already handled it
            // Also skip the default block - it will be handled by convert_default_case
            if current != region.dispatch && Some(current) != region.default_head {
                // IMPORTANT: Only mark comparison blocks, not case target blocks
                // Check if this is actually a comparison block (contains JStrictEqual)
                let block = &cfg.graph()[current];
                let is_comparison_block = block.instructions().iter().any(|instr| {
                    matches!(
                        &instr.instruction,
                        UnifiedInstruction::JStrictEqual { .. }
                            | UnifiedInstruction::JStrictEqualLong { .. }
                    )
                });

                if is_comparison_block {
                    for instruction in block.instructions() {
                        block_converter.mark_instruction_rendered(instruction);

                        // The uses of constants in comparison blocks will be marked as consumed
                        // by report_switch_consumed_uses() after the switch is generated
                    }
                }
            }

            // Follow false edge to next comparison
            if let Some(next) = self.get_false_successor(current, cfg) {
                if next != region.join_block && !region.cases.iter().any(|c| c.case_head == next) {
                    current = next;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Now generate the switch statement
        let switch_statements = self.convert_switch_pattern_with_region(
            &switch_info,
            cfg,
            block_converter,
            Some(region),
        )?;
        all_statements.extend(switch_statements);

        Ok(all_statements)
    }

    /// Get the false successor of a conditional jump
    fn get_false_successor(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> Option<NodeIndex> {
        for edge in cfg.graph().edges(block_id) {
            if matches!(edge.weight(), crate::cfg::EdgeKind::False) {
                return Some(edge.target());
            }
        }
        None
    }
    /// Check if a switch region is contained within a case body
    fn is_switch_in_case_body(
        &self,
        case_target: NodeIndex,
        switch_region: &SwitchRegion,
        cfg: &'a Cfg<'a>,
    ) -> bool {
        use crate::cfg::switch_analysis::nested_switch_detection::NestedSwitchAnalysis;
        NestedSwitchAnalysis::is_switch_in_case_body(case_target, switch_region, cfg)
    }

    /// Convert nested control flow within a case body
    /// This method detects and converts any type of nested control structure:
    /// - Nested switches
    /// - Conditional chains (if/else-if/else)
    /// - Loops (for/while/do-while)
    /// - Any combination of the above
    fn convert_nested_control_flow(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        full_cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        _switch_info: &SwitchInfo,
    ) -> Result<bool, SwitchConversionError> {
        // 1. Check for nested switches first (highest priority)
        let ssa = block_converter.ssa_analysis();
        if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
            for region in &switch_analysis.regions {
                if self.is_switch_in_case_body(group.target_block, region, cfg) {
                    if group.target_block == region.dispatch {
                        // Case jumps directly to a nested switch
                        // Don't mark the dispatch block as rendered yet - let the nested
                        // switch converter handle it, as it needs to process setup instructions

                        // The nested switch will report its own consumed uses when it's converted

                        let mut nested_converter = SwitchConverter::new(self.ast_builder);
                        match nested_converter.convert_switch_region(
                            region,
                            full_cfg,
                            block_converter,
                        ) {
                            Ok(nested_stmts) => {
                                case_statements.extend(nested_stmts);
                                return Ok(true);
                            }
                            Err(e) => {
                                log::error!("Failed to convert nested switch: {:?}", e);
                                // Continue to try other types
                            }
                        }
                    }
                }
            }
        }

        // 2. Check for conditional chains (if/else-if/else)
        if let Some(conditional_analysis) = full_cfg.analyze_conditional_chains() {
            log::debug!(
                "Checking for conditional chains starting at block {:?}",
                group.target_block
            );
            log::debug!("Available chains: {:?}", conditional_analysis.chains.len());
            for (i, chain) in conditional_analysis.chains.iter().enumerate() {
                log::debug!("Chain {}: {:?} branches", i, chain.branches.len());
                for (j, branch) in chain.branches.iter().enumerate() {
                    log::debug!(
                        "  Branch {}: condition_block={:?}, branch_entry={:?}",
                        j,
                        branch.condition_block,
                        branch.branch_entry
                    );
                }
            }

            if let Some(chain) =
                self.find_chain_starting_at_recursive(group.target_block, &conditional_analysis)
            {
                log::debug!(
                    "Found conditional chain starting at block {:?} with {} branches",
                    group.target_block,
                    chain.branches.len()
                );

                // IMPORTANT: Check if this conditional chain contains any switch dispatch blocks
                let ssa = block_converter.ssa_analysis();
                if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                    log::debug!(
                        "Checking if chain contains any of {} switch dispatch blocks",
                        switch_analysis.regions.len()
                    );
                    if self.chain_contains_switch_dispatch(chain, &switch_analysis.regions) {
                        log::debug!("Skipping conditional chain conversion - it contains switch dispatch blocks");
                        // Don't convert as conditional chain - it will be handled as switches
                        return Ok(false);
                    }
                }

                log::debug!(
                    "Found conditional chain starting at block {:?}: {:?}",
                    group.target_block,
                    chain.chain_type
                );
                return self.convert_conditional_chain_in_case(
                    case_statements,
                    chain,
                    cfg,
                    block_converter,
                );
            } else {
                log::debug!(
                    "No conditional chain found starting at block {:?}",
                    group.target_block
                );
            }
        }

        // 3. Check for loops (for/while/do-while)
        // TODO: Implement loop conversion when LoopConverter is available
        let loop_analysis = full_cfg.analyze_loops();
        if let Some(_loop_info) = self.find_loop_starting_at(group.target_block, &loop_analysis) {
            log::debug!("Found loop in case body starting at block {:?}, but loop conversion not yet implemented", group.target_block);
            // For now, fall back to normal block conversion
            // return self.convert_loop_in_case(case_statements, loop_info, cfg, block_converter);
        }

        // 4. Check for other complex control flow patterns
        // (This can be extended in the future for specific patterns)

        Ok(false) // No nested control flow found
    }

    /// Convert a conditional chain within a case
    fn convert_conditional_chain_in_case(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        chain: &crate::cfg::analysis::ConditionalChain,
        cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<bool, SwitchConversionError> {
        log::debug!(
            "convert_conditional_chain_in_case: Converting chain with {} branches",
            chain.branches.len()
        );
        for (i, branch) in chain.branches.iter().enumerate() {
            log::debug!("  Branch {}: block {:?}", i, branch.branch_entry);
            if branch.branch_entry.index() == 5 {
                log::warn!("  This chain will convert block 5!");
            }
        }
        let mut conditional_converter =
            super::conditional_converter::ConditionalConverter::new(self.ast_builder);

        // Convert the conditional chain FIRST, before marking blocks as rendered
        match conditional_converter.convert_chain(chain, cfg, block_converter) {
            Ok(chain_statements) => {
                case_statements.extend(chain_statements);

                // AFTER successful conversion, mark all blocks in the conditional chain as rendered
                let all_chain_blocks =
                    super::conditional_converter::ConditionalConverter::get_all_chain_blocks(chain);

                for &block_id in &all_chain_blocks {
                    let block = &cfg.graph()[block_id];
                    for instruction in block.instructions() {
                        block_converter.mark_instruction_rendered(instruction);
                    }
                }

                Ok(true)
            }
            Err(e) => {
                log::error!("Failed to convert conditional chain in case: {:?}", e);
                Ok(false) // Fall back to normal conversion
            }
        }
    }

    /// Find a conditional chain starting at the given block, searching recursively through nested chains
    fn find_chain_starting_at_recursive<'b>(
        &self,
        start_block: NodeIndex,
        analysis: &'b crate::cfg::analysis::ConditionalAnalysis,
    ) -> Option<&'b crate::cfg::analysis::ConditionalChain> {
        // Search through all chains (top-level and nested)
        self.search_chain_recursive(start_block, &analysis.chains)
    }

    /// Recursive helper to search through chains and their nested chains
    fn search_chain_recursive<'b>(
        &self,
        start_block: NodeIndex,
        chains: &'b [crate::cfg::analysis::ConditionalChain],
    ) -> Option<&'b crate::cfg::analysis::ConditionalChain> {
        for chain in chains {
            // Check if this chain starts at the target block
            if !chain.branches.is_empty() && chain.branches[0].condition_block == start_block {
                return Some(chain);
            }

            // Search nested chains
            if let Some(nested_chain) =
                self.search_chain_recursive(start_block, &chain.nested_chains)
            {
                return Some(nested_chain);
            }
        }
        None
    }

    /// Check if a conditional chain contains any switch dispatch blocks
    fn chain_contains_switch_dispatch(
        &self,
        chain: &crate::cfg::analysis::ConditionalChain,
        switch_regions: &[crate::cfg::analysis::SwitchRegion],
    ) -> bool {
        // Check if any branch condition block is a switch dispatch
        for branch in &chain.branches {
            for region in switch_regions {
                if region.dispatch == branch.condition_block {
                    return true;
                }
            }
        }

        // Also check nested chains
        for nested_chain in &chain.nested_chains {
            if self.chain_contains_switch_dispatch(nested_chain, switch_regions) {
                return true;
            }
        }

        false
    }

    /// Find a loop starting at the given block
    fn find_loop_starting_at<'b>(
        &self,
        start_block: NodeIndex,
        loop_analysis: &'b crate::cfg::analysis::LoopAnalysis,
    ) -> Option<&'b crate::cfg::analysis::Loop> {
        // Find a loop where start_block is a header
        loop_analysis
            .loops
            .iter()
            .find(move |loop_info| loop_info.is_header(start_block))
    }

    /// Pre-analyze nested switches to mark setup instructions for elimination
    /// This is called BEFORE generating setup instructions
    fn pre_analyze_nested_switches(
        &self,
        group: &CaseGroup,
        cfg: &'a Cfg<'a>,
        full_cfg: &'a Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // Check if the target block is a switch dispatch
        let ssa = block_converter.ssa_analysis();
        if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
            for region in &switch_analysis.regions {
                if self.is_switch_in_case_body(group.target_block, region, cfg) {
                    if group.target_block == region.dispatch {
                        // Case jumps directly to a nested switch
                        log::debug!("Case {:?} jumps directly to nested switch", group.keys);
                        // The nested switch will report its own consumed uses when it's converted
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    /// Report which SSA value uses have been consumed by switch statement generation
    /// This should be called AFTER we've decided to generate a switch statement
    fn report_switch_consumed_uses(
        &self,
        region: &SwitchRegion,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
    ) {
        log::debug!(
            "Reporting consumed uses for switch region starting at block {}",
            region.dispatch.index()
        );

        // Collect all uses to mark as consumed
        let mut uses_to_consume: Vec<(SSAValue, Vec<crate::cfg::ssa::types::RegisterUse>)> =
            Vec::new();

        {
            // Scope for immutable borrows
            let ssa = block_converter.ssa_analysis();

            // For sparse switches, find comparison blocks between dispatch and case heads
            let mut comparison_blocks = std::collections::HashSet::new();
            comparison_blocks.insert(region.dispatch);

            // Find all blocks between dispatch and case heads that contain comparisons
            let mut to_visit = vec![region.dispatch];
            let mut visited = std::collections::HashSet::new();

            while let Some(current) = to_visit.pop() {
                if !visited.insert(current) {
                    continue;
                }

                let block = &cfg.graph()[current];

                // Check if this block contains comparison instructions
                let has_comparison = block.instructions().iter().any(|instr| {
                    matches!(
                        &instr.instruction,
                        UnifiedInstruction::JStrictEqual { .. }
                            | UnifiedInstruction::JStrictEqualLong { .. }
                            | UnifiedInstruction::JStrictNotEqual { .. }
                            | UnifiedInstruction::JStrictNotEqualLong { .. }
                    )
                });

                if has_comparison {
                    comparison_blocks.insert(current);

                    // Continue searching for more comparison blocks
                    for edge in cfg.graph().edges(current) {
                        let successor = edge.target();
                        // Don't go past case heads
                        if !region.cases.iter().any(|c| c.case_head == successor) {
                            to_visit.push(successor);
                        }
                    }
                }
            }

            // Now collect uses of constants in comparison instructions
            for &block_id in &comparison_blocks {
                let block = &cfg.graph()[block_id];
                for instr in block.instructions() {
                    match &instr.instruction {
                        UnifiedInstruction::JStrictEqual {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            // Check both operands to see if they're constants
                            for &reg in &[*operand_1, *operand_2] {
                                if let Some(ssa_value) =
                                    ssa.get_value_before_instruction(reg, instr.instruction_index)
                                {
                                    // Check if this is a constant value
                                    let value_tracker =
                                        block_converter.function_analysis().value_tracker();
                                    if let crate::analysis::TrackedValue::Constant(_) =
                                        value_tracker.get_value(ssa_value)
                                    {
                                        // This constant's use in the comparison is consumed
                                        let uses_in_comparison = ssa
                                            .get_ssa_value_uses(ssa_value)
                                            .into_iter()
                                            .filter(|u| {
                                                u.block_id == block_id
                                                    && u.instruction_idx == instr.instruction_index
                                            })
                                            .cloned()
                                            .collect::<Vec<_>>();

                                        if !uses_in_comparison.is_empty() {
                                            log::debug!("Will mark {} uses as consumed for constant in comparison at block {}", 
                                                     uses_in_comparison.len(), block_id.index());
                                            uses_to_consume
                                                .push((ssa_value.clone(), uses_in_comparison));
                                        }
                                    }
                                }
                            }
                        }
                        UnifiedInstruction::JStrictEqualLong {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            // For Long variant, operand_0 is i32 (the offset), so we only check the register operands
                            // Check both register operands
                            for &reg in &[*operand_1, *operand_2] {
                                if let Some(ssa_value) =
                                    ssa.get_value_before_instruction(reg, instr.instruction_index)
                                {
                                    // Check if this is a constant value
                                    let value_tracker =
                                        block_converter.function_analysis().value_tracker();
                                    if let crate::analysis::TrackedValue::Constant(_) =
                                        value_tracker.get_value(ssa_value)
                                    {
                                        // This constant's use in the comparison is consumed
                                        let uses_in_comparison = ssa
                                            .get_ssa_value_uses(ssa_value)
                                            .into_iter()
                                            .filter(|u| {
                                                u.block_id == block_id
                                                    && u.instruction_idx == instr.instruction_index
                                            })
                                            .cloned()
                                            .collect::<Vec<_>>();

                                        if !uses_in_comparison.is_empty() {
                                            log::debug!("Will mark {} uses as consumed for constant in comparison at block {}", 
                                                     uses_in_comparison.len(), block_id.index());
                                            uses_to_consume
                                                .push((ssa_value.clone(), uses_in_comparison));
                                        }
                                    }
                                }
                            }
                        }
                        // Remove Mov-specific handling - cascading elimination will handle it
                        _ => {}
                    }
                }
            }
        }

        // Report the consumed values to the SSA usage tracker
        if !uses_to_consume.is_empty() {
            log::debug!(
                "Reporting {} SSA values as consumed by switch comparisons",
                uses_to_consume.len()
            );
            block_converter
                .ssa_usage_tracker_mut()
                .report_switch_inlined_uses(uses_to_consume);
        }

        // Also mark the comparison instructions themselves as rendered
        // since they're being replaced by the switch statement
        self.mark_switch_region_as_rendered(region, block_converter, cfg);
    }

    /// Mark all blocks in a switch region as rendered to prevent double conversion
    pub fn mark_switch_region_as_rendered(
        &self,
        region: &SwitchRegion,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &'a Cfg<'a>,
    ) {
        log::debug!(
            "Marking switch region as rendered, dispatch block: {}",
            region.dispatch.index()
        );

        // For the dispatch block, we need to be careful:
        // If the dispatch block is also a case target, it may contain setup instructions
        // that should be processed before the switch. We should only mark the switch
        // comparison instructions as rendered.

        let dispatch_block = &cfg.graph()[region.dispatch];

        // Only mark the actual comparison instructions, not any setup instructions
        for instr in dispatch_block.instructions() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual { .. }
                | UnifiedInstruction::JStrictEqualLong { .. }
                | UnifiedInstruction::JStrictNotEqual { .. }
                | UnifiedInstruction::JStrictNotEqualLong { .. } => {
                    // Mark only the comparison instruction itself
                    block_converter.mark_instruction_rendered(instr);
                }
                _ => {
                    // Don't mark non-comparison instructions - they may be needed
                    // for variable declarations or other setup
                }
            }
        }

        // For sparse switches, we need to mark all comparison blocks
        // These are blocks between the dispatch and the case heads that perform comparisons

        // We need to identify all comparison blocks
        // For sparse switches, we have a chain of JStrictEqual comparisons
        // We need to mark ALL of them, not just the ones on one path

        // First, collect all blocks that are part of the switch structure
        let mut switch_blocks = HashSet::new();
        switch_blocks.insert(region.dispatch);

        // Add all case heads
        for case in &region.cases {
            switch_blocks.insert(case.case_head);
        }

        if let Some(default) = region.default_head {
            switch_blocks.insert(default);
        }

        // Now find all blocks between dispatch and case heads that contain comparisons
        let mut comparison_blocks = HashSet::new();
        let mut to_visit = vec![region.dispatch];
        let mut visited = HashSet::new();

        while let Some(current) = to_visit.pop() {
            if !visited.insert(current) {
                continue;
            }

            let block = &cfg.graph()[current];

            // Check if this block contains comparison instructions
            let has_comparison = block.instructions().iter().any(|instr| {
                matches!(
                    &instr.instruction,
                    UnifiedInstruction::JStrictEqual { .. }
                        | UnifiedInstruction::JStrictEqualLong { .. }
                        | UnifiedInstruction::JStrictNotEqual { .. }
                        | UnifiedInstruction::JStrictNotEqualLong { .. }
                )
            });

            if has_comparison {
                comparison_blocks.insert(current);

                // Mark all instructions in this block as rendered
                log::debug!("Marking comparison block {} as rendered", current.index());
                for instr in block.instructions() {
                    block_converter.mark_instruction_rendered(instr);
                }
            }

            // Continue exploring until we reach case heads or the default
            for edge in cfg.graph().edges(current) {
                let target = edge.target();

                // Don't go past case heads or default block
                if !switch_blocks.contains(&target) || target == region.dispatch {
                    to_visit.push(target);
                }
            }
        }

        // NOTE: We do NOT mark case target blocks as rendered here!
        // The case bodies need to be converted when we actually process the nested switch.
    }
}

/// Get all blocks that are part of switch bodies (for exclusion from normal conversion)
pub fn get_all_switch_blocks_with_bodies(
    switch_regions: &[SwitchRegion],
    cfg: &Cfg,
) -> HashSet<NodeIndex> {
    let mut switch_blocks = HashSet::new();

    for region in switch_regions {
        // Add dispatch block
        switch_blocks.insert(region.dispatch);

        // For each case, add the path from case head to join block
        for case in &region.cases {
            let mut current = case.case_head;
            let mut visited = HashSet::new();

            // Follow the path until we reach the join block or a cycle
            while current != region.join_block && visited.insert(current) {
                switch_blocks.insert(current);

                // Get successors
                let successors: Vec<_> = cfg.graph().edges(current).map(|e| e.target()).collect();

                // If there's only one successor, follow it
                // If there are multiple, we've reached a branch point
                if successors.len() == 1 {
                    current = successors[0];
                } else {
                    break;
                }
            }
        }

        // Don't add default case blocks here - they should be processed normally
        // when convert_default_case is called
    }

    switch_blocks
}

/// Get switch infrastructure blocks (dispatch, comparison blocks, and join blocks)
pub fn get_switch_infrastructure_blocks(switch_regions: &[SwitchRegion]) -> HashSet<NodeIndex> {
    let mut infrastructure = HashSet::new();

    for region in switch_regions {
        // Add dispatch block
        infrastructure.insert(region.dispatch);

        // For sparse switches, we need to also include comparison blocks
        // These are blocks that perform the JStrictEqual comparisons
        // They can be identified by being between the dispatch and case heads
        // For now, we'll rely on the sparse switch detector having marked them

        // Add join block
        infrastructure.insert(region.join_block);
    }

    infrastructure
}
