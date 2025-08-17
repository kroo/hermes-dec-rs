//! Switch statement AST generation from analyzed switch patterns
//!
//! This module converts analyzed switch patterns into JavaScript switch statement AST nodes

use crate::analysis::value_tracker::ConstantValue;
use crate::cfg::switch_analysis::{
    CaseGroup, CaseInfo, CaseKey, DefaultCase, PhiNode, SharedTailInfo, SwitchInfo,
};
use crate::cfg::{analysis::SwitchRegion, Cfg};
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

        // For sparse switch regions, we need to analyze the comparison chain
        self.convert_sparse_switch_region(region, cfg, block_converter)
    }

    /// Convert an analyzed switch pattern to AST
    pub fn convert_switch_pattern(
        &mut self,
        switch_info: &SwitchInfo,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut statements = Vec::new();

        // Group consecutive cases that share the same target and setup
        let case_groups_vec = self.group_consecutive_cases(&switch_info.cases);
        // Allocate in arena to extend lifetime
        let case_groups: Vec<CaseGroup> = case_groups_vec;

        // Identify blocks that are targeted by multiple case groups
        let shared_blocks = self.identify_shared_target_blocks(&case_groups, cfg);

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
                                    self.generate_setup_instructions(
                                        &mut empty_case_body,
                                        &single_key_group,
                                        block_converter,
                                        cfg,
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
                &shared_blocks,
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
                .and_then(|ssa| ssa.phi_functions.get(block_id))
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
                        // For Mov instructions, check if they reference the same source register
                        if let (
                            UnifiedInstruction::Mov {
                                operand_1: src1, ..
                            },
                            UnifiedInstruction::Mov {
                                operand_1: src2, ..
                            },
                        ) = (&a.instruction.instruction, &b.instruction.instruction)
                        {
                            *src1 == *src2 && a.ssa_value.register == b.ssa_value.register
                        } else {
                            a.instruction.instruction.name() == b.instruction.instruction.name()
                                && a.ssa_value.register == b.ssa_value.register
                        }
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
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        group_index: usize,
        all_groups: &[CaseGroup],
        shared_blocks: &HashSet<NodeIndex>,
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
        let is_post_switch_shared = if !shared_blocks.contains(&group.target_block) {
            false
        } else {
            // This block is in shared_blocks, but we need to check if it's truly post-switch
            // or just a fallthrough target (like block 8 for cases 2 and 3)

            // Check if this is part of a consecutive case pattern (fallthrough)
            // If the current group and the previous group are consecutive cases that share the target,
            // then this is a fallthrough pattern, not post-switch code
            let is_fallthrough_pattern = if group_index > 0 {
                let prev_group = &all_groups[group_index - 1];
                // Check if previous case can reach current target through fallthrough
                // This happens when the previous case's target can reach the current target
                let prev_can_reach_current = cfg
                    .graph()
                    .edges(prev_group.target_block)
                    .any(|edge| edge.target() == group.target_block);

                // Also check if cases are consecutive in the original switch
                let cases_consecutive = if let (Some(prev_key), Some(curr_key)) =
                    (prev_group.keys.last(), group.keys.first())
                {
                    match (prev_key, curr_key) {
                        (CaseKey::Number(OrderedFloat(n1)), CaseKey::Number(OrderedFloat(n2))) => {
                            (*n2 - *n1).abs() == 1.0
                        }
                        _ => false,
                    }
                } else {
                    false
                };

                prev_can_reach_current && cases_consecutive
            } else {
                false
            };

            if is_fallthrough_pattern {
                false // This is part of a fallthrough pattern, not post-switch code
            } else {
                // Check for PHI nodes as additional evidence
                let has_phi = block_converter
                    .ssa_analysis()
                    .and_then(|ssa| ssa.phi_functions.get(&group.target_block))
                    .map(|phis| !phis.is_empty())
                    .unwrap_or(false);

                has_phi
            }
        };

        if is_post_switch_shared {
            // This case jumps to a post-switch shared block - generate setup and break
            self.generate_setup_instructions(&mut case_statements, group, block_converter, cfg)?;
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
                        .and_then(|ssa| ssa.phi_functions.get(&group.target_block))
                        .map(|phis| !phis.is_empty())
                        .unwrap_or(false);

                    if has_phi {
                        // Generate PHI assignments before converting the target block
                        self.generate_case_body_with_phi_assignments(
                            &mut case_statements,
                            group,
                            cfg,
                            block_converter,
                        )?;
                    } else {
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
                            self.convert_target_block_normally(
                                &mut case_statements,
                                group,
                                cfg,
                                block_converter,
                                switch_info,
                            )?;
                        }
                    }
                }
            } else {
                // No shared tail - generate setup and convert target block normally
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
            // Don't mark instructions as rendered since they'll be converted again for the actual case
            self.convert_target_block_with_marking(
                &mut case_statements,
                &next_group_clone,
                cfg,
                block_converter,
                switch_info,
                false, // don't mark_as_rendered
            )?;
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

    /// Identify blocks that are shared by multiple cases
    fn identify_shared_target_blocks(
        &self,
        case_groups: &[CaseGroup],
        cfg: &Cfg<'a>,
    ) -> HashSet<NodeIndex> {
        let mut shared_blocks = HashSet::new();
        let mut block_references: HashMap<NodeIndex, usize> = HashMap::new();

        // Count direct targets
        for group in case_groups {
            *block_references.entry(group.target_block).or_insert(0) += 1;
        }

        // Also check indirect targets (blocks reachable from case targets)
        for group in case_groups {
            for edge in cfg.graph().edges(group.target_block) {
                let successor = edge.target();
                if !cfg.graph()[successor].is_exit() {
                    *block_references.entry(successor).or_insert(0) += 1;
                }
            }
        }

        // Mark blocks referenced by multiple paths as shared
        for (block_id, count) in block_references {
            if count > 1 {
                shared_blocks.insert(block_id);
            }
        }

        shared_blocks
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

        // Check if all setup instructions are Mov instructions with the same source
        let all_mov_same_source = group.setup.iter().all(|instr| {
            matches!(
                &instr.instruction.instruction,
                UnifiedInstruction::Mov { .. }
            )
        });

        if all_mov_same_source && group.setup.len() > 1 {
            // Get the source registers from all Mov instructions
            let sources: Vec<u8> = group
                .setup
                .iter()
                .filter_map(|instr| {
                    if let UnifiedInstruction::Mov { operand_1, .. } =
                        &instr.instruction.instruction
                    {
                        Some(*operand_1)
                    } else {
                        None
                    }
                })
                .collect();

            // Check if all Mov instructions use the same source
            if !sources.is_empty() && sources.iter().all(|&s| s == sources[0]) {
                return true;
            }
        }

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
        cfg: &Cfg<'a>,
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
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<(), SwitchConversionError> {
        // First, generate setup instructions
        self.generate_setup_instructions(case_statements, group, block_converter, cfg)?;

        // Get PHI functions at the target block
        let ssa = block_converter
            .ssa_analysis()
            .expect("SSA analysis required");

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

        // Then convert the target block
        let target_block = &cfg.graph()[group.target_block];
        match block_converter.convert_block_with_options(
            target_block,
            group.target_block,
            cfg.graph(),
            None::<fn(&HbcFunctionInstruction, bool) -> bool>,
            false,
        ) {
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

    /// Generate setup instructions for a case
    fn generate_setup_instructions(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &Cfg<'a>,
    ) -> Result<(), SwitchConversionError> {
        // Check if the target block is just a return statement
        let target_is_simple_return =
            if let Some(target_block) = cfg.graph().node_weight(group.target_block) {
                target_block.instructions().len() == 1
                    && matches!(
                        &target_block.instructions()[0].instruction,
                        UnifiedInstruction::Ret { .. }
                    )
            } else {
                false
            };

        // Identify registers that are only used as sources for Mov instructions
        let mov_only_sources: std::collections::HashSet<u8> = {
            let mut sources = std::collections::HashSet::new();

            // First, find all Mov instructions and their source registers
            for setup_instr in &group.setup {
                if let UnifiedInstruction::Mov { operand_1, .. } =
                    &setup_instr.instruction.instruction
                {
                    // Check if this source is defined in the setup
                    let source_defined_in_setup = group.setup.iter().any(|other| {
                        if let Some(target_reg) =
                            crate::generated::instruction_analysis::analyze_register_usage(
                                &other.instruction.instruction,
                            )
                            .target
                        {
                            target_reg == *operand_1
                        } else {
                            false
                        }
                    });

                    if source_defined_in_setup {
                        sources.insert(*operand_1);
                    }
                }
            }

            // Now check if these sources are used by anything other than Mov
            sources.retain(|&reg| {
                group.setup.iter().all(|instr| {
                    let usage = crate::generated::instruction_analysis::analyze_register_usage(
                        &instr.instruction.instruction,
                    );

                    // If this instruction uses the register as a source
                    if usage.sources.contains(&reg) {
                        // It's only OK if this is a Mov instruction
                        matches!(
                            &instr.instruction.instruction,
                            UnifiedInstruction::Mov { .. }
                        )
                    } else {
                        true // Not used as source, so OK
                    }
                })
            });

            sources
        };

        for setup_instr in &group.setup {
            // Smart temporary instruction skipping
            // Skip LoadConst instructions if:
            // 1. They are immediately followed by a Ret instruction that uses the loaded value
            // 2. There are no other instructions between the LoadConst and Ret
            let should_skip =
                if group.setup.len() == 1 && group.always_terminates && target_is_simple_return {
                    // Single setup instruction in a terminating case with just a return - likely a LoadConst + Ret pattern
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
                    )
                } else {
                    false
                };

            if should_skip {
                continue;
            }

            // Skip instructions that define registers only used as Mov sources
            if let Some(target_reg) =
                crate::generated::instruction_analysis::analyze_register_usage(
                    &setup_instr.instruction.instruction,
                )
                .target
            {
                if mov_only_sources.contains(&target_reg) {
                    // Mark this SSA value as eliminated
                    block_converter.mark_ssa_value_eliminated(setup_instr.ssa_value.clone());
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
                            .expect("PHI result variable not found")
                    } else {
                        // No PHI function for this register, use the SSA value directly
                        mapping
                            .ssa_to_var
                            .get(&setup_instr.ssa_value)
                            .cloned()
                            .expect("SSA value not found")
                    }
                } else {
                    // No PHI functions at target block, use the SSA value directly
                    mapping
                        .ssa_to_var
                        .get(&setup_instr.ssa_value)
                        .cloned()
                        .expect("SSA value not found")
                }
            } else {
                panic!("Missing variable mapping!");
            };

            // Allocate the string in the arena to extend its lifetime
            let var_name = self.ast_builder.allocator.alloc_str(&var_name);

            // Create a variable declaration with the constant value
            let value_expr = if let Some(const_value) = &setup_instr.value {
                // Convert the constant value to an expression
                // Convert the constant value directly using the same method
                self.create_constant_expression_from_value(const_value)
            } else if let UnifiedInstruction::Mov { operand_1, .. } =
                &setup_instr.instruction.instruction
            {
                // For Mov instructions, find the value being moved
                // Look for the source value in other setup instructions
                let source_value = group
                    .setup
                    .iter()
                    .find(|s| {
                        if let Some(target_reg) =
                            crate::generated::instruction_analysis::analyze_register_usage(
                                &s.instruction.instruction,
                            )
                            .target
                        {
                            target_reg == *operand_1 && s.value.is_some()
                        } else {
                            false
                        }
                    })
                    .and_then(|s| s.value.as_ref());

                if let Some(const_value) = source_value {
                    self.create_constant_expression_from_value(const_value)
                } else {
                    // If we can't find the source value, use the source register variable
                    let source_var_name = if let Some(mapping) = block_converter
                        .instruction_converter()
                        .register_manager()
                        .variable_mapping()
                    {
                        mapping
                            .get_source_variable_name(
                                *operand_1,
                                setup_instr.instruction.instruction_index,
                            )
                            .cloned()
                            .unwrap_or_else(|| format!("var{}", operand_1))
                    } else {
                        format!("var{}", operand_1)
                    };

                    let source_var_name = self.ast_builder.allocator.alloc_str(&source_var_name);
                    Expression::Identifier(
                        self.ast_builder.alloc(
                            self.ast_builder
                                .identifier_reference(Span::default(), source_var_name),
                        ),
                    )
                }
            } else {
                // For other non-constant setup instructions, create an undefined literal
                Expression::Identifier(
                    self.ast_builder.alloc(
                        self.ast_builder
                            .identifier_reference(Span::default(), "undefined"),
                    ),
                )
            };

            // Check if this variable has already been declared at function scope
            // If it's a function-scope variable, we should only assign, not declare
            let is_function_scoped = if let Some(mapping) = block_converter
                .instruction_converter()
                .register_manager()
                .variable_mapping()
            {
                // Check if this variable is marked as function-scoped
                mapping.function_scope_vars.contains(&var_name.to_string())
            } else {
                false // Default to not function-scoped if we can't check
            };

            // We should declare if it's NOT function-scoped (i.e., it's a local variable)
            let should_declare = !is_function_scoped;

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
                if let Some(_ssa_analysis) = block_converter.ssa_analysis() {
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
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
    ) -> Result<(), SwitchConversionError> {
        self.convert_target_block_with_marking(
            case_statements,
            group,
            cfg,
            block_converter,
            switch_info,
            true, // mark_as_rendered
        )
    }

    /// Convert target block with control over instruction marking
    fn convert_target_block_with_marking(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        switch_info: &SwitchInfo,
        mark_as_rendered: bool,
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

        // Normal case: convert the entire block
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
        Ok(())
    }

    /// Generate case body for cases that go directly to shared tail
    fn generate_case_body_for_shared_tail(
        &mut self,
        case_statements: &mut ArenaVec<'a, Statement<'a>>,
        group: &CaseGroup,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &Cfg<'a>,
    ) -> Result<(), SwitchConversionError> {
        // For cases that jump directly to the shared tail, we need to:
        // 1. Generate setup instructions
        // 2. Generate any PHI value assignments
        // 3. Add a break statement

        // Generate setup instructions - these should handle all necessary assignments
        self.generate_setup_instructions(case_statements, group, block_converter, cfg)?;

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
        cfg: &Cfg<'a>,
        full_cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        shared_tail: Option<&SharedTailInfo>,
        _all_phi_nodes: &HashMap<u8, PhiNode>,
    ) -> Result<SwitchCase<'a>, SwitchConversionError> {
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

                    if let Some(ssa) = block_converter.ssa_analysis() {
                        if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                            for region in &switch_analysis.regions {
                                if region.dispatch == default_case.target_block {
                                    // Mark all blocks in the nested switch region as rendered
                                    self.mark_switch_region_as_rendered(
                                        region,
                                        block_converter,
                                        cfg,
                                    );

                                    // Convert the nested switch
                                    let mut nested_converter =
                                        SwitchConverter::new(self.ast_builder);
                                    match nested_converter.convert_switch_region(
                                        region,
                                        full_cfg,
                                        block_converter,
                                    ) {
                                        Ok(nested_stmts) => {
                                            case_statements.extend(nested_stmts);
                                            converted_nested = true;
                                            break;
                                        }
                                        Err(e) => {
                                            log::error!("Failed to convert nested switch in default case: {:?}", e);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !converted_nested {
                        let target_block = &cfg.graph()[default_case.target_block];
                        match block_converter.convert_block(
                            target_block,
                            default_case.target_block,
                            cfg.graph(),
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

                if let Some(ssa) = block_converter.ssa_analysis() {
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
                }

                if !converted_nested {
                    let target_block = &cfg.graph()[default_case.target_block];
                    match block_converter.convert_block(
                        target_block,
                        default_case.target_block,
                        cfg.graph(),
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

            if let Some(ssa) = block_converter.ssa_analysis() {
                if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                    for region in &switch_analysis.regions {
                        if region.dispatch == default_case.target_block {
                            // Mark all blocks in the nested switch region as rendered
                            self.mark_switch_region_as_rendered(region, block_converter, cfg);

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
            }

            if !converted_nested {
                let target_block = &cfg.graph()[default_case.target_block];

                // Just use regular convert_block - the block should not be marked as processed
                match block_converter.convert_block(
                    target_block,
                    default_case.target_block,
                    cfg.graph(),
                ) {
                    Ok(stmts) => {
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
        cfg: &Cfg<'a>,
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
        let ssa = block_converter
            .ssa_analysis()
            .expect("SSA analysis required for switch conversion");

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

        let switch_statements = self.convert_switch_pattern(&switch_info, cfg, block_converter)?;
        all_statements.extend(switch_statements);

        Ok(all_statements)
    }

    /// Convert a sparse switch region to AST
    fn convert_sparse_switch_region(
        &mut self,
        region: &SwitchRegion,
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<Vec<Statement<'a>>, SwitchConversionError> {
        let mut all_statements = Vec::new();

        // Get SSA analysis from block converter
        let ssa = block_converter
            .ssa_analysis()
            .expect("SSA analysis required for switch conversion");

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
        let switch_info = analyzer
            .detect_switch_pattern(region.dispatch, cfg, ssa, &postdom)
            .ok_or_else(|| {
                SwitchConversionError::UnsupportedPattern(
                    "Could not detect switch pattern".to_string(),
                )
            })?;

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

        // Mark the const load as rendered if we found one
        if let Some(idx) = const_load_idx {
            block_converter.mark_instruction_rendered(&dispatch_instructions[idx]);

            // Also mark its SSA value as eliminated
            if let Some(ssa_analysis) = block_converter.ssa_analysis() {
                let instr = &dispatch_instructions[idx];
                let reg_def = crate::cfg::ssa::RegisterDef {
                    register: const_reg,
                    block_id: region.dispatch,
                    instruction_idx: instr.instruction_index,
                };
                if let Some(ssa_value) = ssa_analysis.ssa_values.get(&reg_def) {
                    block_converter.mark_ssa_value_eliminated(ssa_value.clone());
                }
            }
        }

        // Also mark any dispatch block instructions that are setup instructions for case 0
        if let Some(first_case) = switch_info.cases.first() {
            if first_case.comparison_block == region.dispatch {
                // Case 0's comparison is in the dispatch block
                for setup_instr in &first_case.setup {
                    // Find this instruction in the dispatch block and mark it as rendered
                    for (i, instr) in dispatch_instructions.iter().enumerate() {
                        if instr.offset == setup_instr.instruction.offset {
                            block_converter.mark_instruction_rendered(&dispatch_instructions[i]);

                            // Also mark its SSA value as eliminated
                            block_converter
                                .mark_ssa_value_eliminated(setup_instr.ssa_value.clone());
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
                let block = &cfg.graph()[current];
                for instruction in block.instructions() {
                    block_converter.mark_instruction_rendered(instruction);

                    // Also mark SSA values from const loads as eliminated
                    if let Some(ssa_analysis) = block_converter.ssa_analysis() {
                        let usage = crate::generated::instruction_analysis::analyze_register_usage(
                            &instruction.instruction,
                        );
                        if let Some(target_reg) = usage.target {
                            // Check if this is a const load instruction
                            if matches!(instruction.instruction,
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstZero { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstUInt8 { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstInt { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstDouble { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstString { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstStringLongIndex { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstTrue { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstFalse { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstNull { .. } |
                                crate::generated::unified_instructions::UnifiedInstruction::LoadConstUndefined { .. }
                            ) {
                                let reg_def = crate::cfg::ssa::RegisterDef {
                                    register: target_reg,
                                    block_id: current,
                                    instruction_idx: instruction.instruction_index,
                                };
                                if let Some(ssa_value) = ssa_analysis.ssa_values.get(&reg_def) {
                                    block_converter.mark_ssa_value_eliminated(ssa_value.clone());
                                }
                            }
                        }
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
        let switch_statements = self.convert_switch_pattern(&switch_info, cfg, block_converter)?;
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
        cfg: &Cfg<'a>,
    ) -> bool {
        // A switch is in the case body if:
        // 1. The switch dispatch block IS the case target block (nested switch as first statement)
        // 2. OR the switch dispatch block is reachable from the case target block

        // Common pattern: the case target block IS the dispatch block of the nested switch
        if switch_region.dispatch == case_target {
            return true; // This case immediately starts with a nested switch
        }

        // Check if case target has a direct edge to the switch dispatch
        for edge in cfg.graph().edges(case_target) {
            if edge.target() == switch_region.dispatch {
                return true;
            }
        }

        // Also check if the switch dispatch is the case target + 1 (common pattern)
        if switch_region.dispatch.index() == case_target.index() + 1 {
            return true;
        }

        false
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
        cfg: &Cfg<'a>,
        full_cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        _switch_info: &SwitchInfo,
    ) -> Result<bool, SwitchConversionError> {
        // 1. Check for nested switches first (highest priority)
        if let Some(ssa) = block_converter.ssa_analysis() {
            if let Some(switch_analysis) = full_cfg.analyze_switch_regions(ssa) {
                for region in &switch_analysis.regions {
                    if self.is_switch_in_case_body(group.target_block, region, cfg) {
                        if group.target_block == region.dispatch {
                            // Case jumps directly to a nested switch
                            // Don't mark the dispatch block as rendered yet - let the nested
                            // switch converter handle it, as it needs to process setup instructions
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
        cfg: &Cfg<'a>,
        block_converter: &mut super::BlockToStatementConverter<'a>,
    ) -> Result<bool, SwitchConversionError> {
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

    /// Mark all blocks in a switch region as rendered to prevent double conversion
    fn mark_switch_region_as_rendered(
        &self,
        region: &SwitchRegion,
        block_converter: &mut super::BlockToStatementConverter<'a>,
        cfg: &Cfg<'a>,
    ) {
        // For the dispatch block, we need to be careful:
        // If the dispatch block is also a case target, it may contain setup instructions
        // that should be processed before the switch. We should only mark the switch
        // comparison instructions as rendered.

        let dispatch_block = &cfg.graph()[region.dispatch];

        // Find the first comparison instruction (JStrictEqual or similar)
        let mut found_comparison = false;
        for instr in dispatch_block.instructions() {
            match &instr.instruction {
                UnifiedInstruction::JStrictEqual { .. }
                | UnifiedInstruction::JStrictEqualLong { .. }
                | UnifiedInstruction::JStrictNotEqual { .. }
                | UnifiedInstruction::JStrictNotEqualLong { .. } => {
                    found_comparison = true;
                }
                _ => {}
            }

            // Only mark instructions from the first comparison onward
            if found_comparison {
                block_converter.mark_instruction_rendered(instr);
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

/// Get switch infrastructure blocks (dispatch and join blocks only)
pub fn get_switch_infrastructure_blocks(switch_regions: &[SwitchRegion]) -> HashSet<NodeIndex> {
    let mut infrastructure = HashSet::new();

    for region in switch_regions {
        // Only add dispatch and join blocks
        infrastructure.insert(region.dispatch);
        infrastructure.insert(region.join_block);
    }

    infrastructure
}
