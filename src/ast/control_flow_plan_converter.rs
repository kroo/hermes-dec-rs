//! Converts a ControlFlowPlan into JavaScript AST
//!
//! This module replaces the old block_converter/switch_converter/conditional_converter
//! with a unified approach that works directly from the analyzed ControlFlowPlan.

use crate::analysis::control_flow_plan::{
    CaseGroupStructure, CatchClause, ControlFlowKind, ControlFlowPlan, LoopType, SequentialElement,
    StructureId,
};
use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, UseStrategy, VariableKind};
use crate::analysis::value_tracker::ConstantValue;
use crate::ast::comments::{AddressCommentManager, CommentKind, CommentPosition};
use crate::ast::{ExpressionContext, InstructionToStatementConverter};
use crate::cfg::ssa::{DuplicatedSSAValue, DuplicationContext, RegisterUse, SSAValue};
use crate::cfg::switch_analysis::switch_info::{CaseKey, SwitchInfo};
use crate::hbc::HbcFile;
use oxc_allocator::Vec as OxcVec;
use oxc_ast::ast::*;
use oxc_ast::AstBuilder;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Converts a ControlFlowPlan into JavaScript AST
pub struct ControlFlowPlanConverter<'a> {
    /// AST builder for creating nodes
    ast_builder: &'a AstBuilder<'a>,
    /// HBC analysis for function information
    hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
    /// Function index we're converting
    function_index: u32,
    /// Instruction-to-statement converter for basic blocks
    instruction_converter: InstructionToStatementConverter<'a>,
    /// Map from SSA values to variable names
    variable_names: HashMap<DuplicatedSSAValue, String>,
    /// Whether to include SSA debug comments
    include_ssa_comments: bool,
    /// Whether to include instruction debug comments
    include_instruction_comments: bool,
    /// Comment manager for attaching comments to statements
    comment_manager: Option<AddressCommentManager>,
}

impl<'a> ControlFlowPlanConverter<'a> {
    /// Create a new converter
    pub fn new(
        ast_builder: &'a AstBuilder<'a>,
        hbc: &'a HbcFile<'a>,
        hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
        function_index: u32,
        function_analysis: &crate::analysis::FunctionAnalysis<'a>,
        include_ssa_comments: bool,
        include_instruction_comments: bool,
    ) -> Self {
        let expression_context = ExpressionContext::with_context(
            hbc,
            function_index,
            crate::hbc::InstructionIndex::zero(),
        );

        let mut instruction_converter =
            InstructionToStatementConverter::new(ast_builder, expression_context, hbc_analysis);

        // Generate variable mapping from SSA analysis
        let mut variable_mapper = crate::ast::variables::VariableMapper::new();
        if let Ok(variable_mapping) =
            variable_mapper.generate_mapping(&function_analysis.ssa, &function_analysis.cfg)
        {
            // Set the SSA-based variable mapping in the register manager
            instruction_converter
                .register_manager_mut()
                .set_variable_mapping(variable_mapping);
        }

        Self {
            ast_builder,
            hbc_analysis,
            function_index,
            instruction_converter,
            variable_names: HashMap::new(),
            include_ssa_comments,
            include_instruction_comments,
            comment_manager: if include_ssa_comments || include_instruction_comments {
                Some(AddressCommentManager::new())
            } else {
                None
            },
        }
    }

    /// Take the comment manager (transfers ownership)
    pub fn take_comment_manager(&mut self) -> Option<AddressCommentManager> {
        self.comment_manager.take()
    }

    /// Convert the entire plan to statements
    pub fn convert_to_ast(&mut self, plan: &ControlFlowPlan) -> OxcVec<'a, Statement<'a>> {
        // Extract the set of duplicated SSA values that actually exist (i.e., are declared, not skipped)
        let existing_duplicated_ssas: std::collections::HashSet<DuplicatedSSAValue> = plan
            .declaration_strategies
            .iter()
            .filter_map(|(dup_val, strategy)| {
                // Only include duplicated SSA values that are actually declared (not Skip)
                if dup_val.duplication_context.is_some()
                    && !matches!(strategy, DeclarationStrategy::Skip)
                {
                    Some(dup_val.clone())
                } else {
                    None
                }
            })
            .collect();

        // Pass this information to the register manager
        self.instruction_converter
            .register_manager_mut()
            .set_existing_duplicated_ssas(existing_duplicated_ssas);

        let mut statements = self.ast_builder.vec();

        // Convert the root structure
        self.convert_structure_id(plan, plan.root, &mut statements, None);

        statements
    }

    /// Convert a structure by its ID
    fn convert_structure_id(
        &mut self,
        plan: &ControlFlowPlan,
        structure_id: StructureId,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        if let Some(structure) = plan.structures.get(&structure_id) {
            // Check if this is a duplicated structure and update context
            let ctx = if let Some(dup_info) = &structure.duplication_info {
                Some(&dup_info.context)
            } else {
                context
            };

            self.convert_structure_kind(plan, &structure.kind, statements, ctx);
        }
    }

    /// Convert a control flow kind to statements
    fn convert_structure_kind(
        &mut self,
        plan: &ControlFlowPlan,
        kind: &ControlFlowKind,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        match kind {
            ControlFlowKind::Sequential { elements } => {
                self.convert_sequential(plan, elements, statements, context);
            }
            ControlFlowKind::Switch {
                dispatch_block,
                info,
                discriminator_value,
                discriminator_use,
                case_groups,
                default_case,
            } => {
                self.convert_switch(
                    plan,
                    dispatch_block,
                    info,
                    discriminator_value.as_ref(),
                    discriminator_use.as_ref(),
                    case_groups,
                    default_case.as_ref(),
                    statements,
                    context,
                );
            }
            ControlFlowKind::Conditional {
                condition_block,
                condition_expr,
                condition_use,
                true_branch,
                false_branch,
            } => {
                self.convert_conditional(
                    plan,
                    *condition_block,
                    condition_expr.as_ref(),
                    condition_use.as_ref(),
                    *true_branch,
                    false_branch.as_ref(),
                    statements,
                    context,
                );
            }
            ControlFlowKind::Loop {
                loop_type,
                header_block,
                condition,
                condition_use,
                body,
                update,
                break_target,
                continue_target,
            } => {
                self.convert_loop(
                    plan,
                    loop_type,
                    *header_block,
                    condition.as_ref(),
                    condition_use.as_ref(),
                    *body,
                    update.as_ref(),
                    break_target.as_ref(),
                    continue_target.as_ref(),
                    statements,
                    context,
                );
            }
            ControlFlowKind::TryCatch {
                try_body,
                catch_clause,
                finally_body,
            } => {
                self.convert_try_catch(
                    plan,
                    *try_body,
                    catch_clause.as_ref(),
                    finally_body.as_ref(),
                    statements,
                    context,
                );
            }
            ControlFlowKind::BasicBlock {
                block,
                instruction_count,
                is_synthetic,
            } => {
                self.convert_basic_block(
                    plan,
                    *block,
                    *instruction_count,
                    *is_synthetic,
                    statements,
                    context,
                );
            }
            ControlFlowKind::Empty => {
                // Empty structure - no statements to generate
            }
        }
    }

    /// Convert a sequential structure
    fn convert_sequential(
        &mut self,
        plan: &ControlFlowPlan,
        elements: &[SequentialElement],
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        for element in elements {
            match element {
                SequentialElement::Block(block_id) => {
                    self.convert_basic_block(plan, *block_id, 0, false, statements, context);
                }
                SequentialElement::Structure(structure_id) => {
                    self.convert_structure_id(plan, *structure_id, statements, context);
                }
            }
        }
    }

    /// Convert a single block (not the whole structure)
    fn convert_single_block(
        &mut self,
        plan: &ControlFlowPlan,
        block_idx: NodeIndex,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the basic block from the function analysis
        if let Some(function_analysis) = self
            .hbc_analysis
            .get_function_analysis_ref(self.function_index)
        {
            if let Some(block) = function_analysis.cfg.graph().node_weight(block_idx) {
                // Convert each instruction in the block
                for hbc_instruction in &block.instructions {
                    if self.include_instruction_comments {
                        // Add instruction comment
                        let comment_text = format!(
                            "// {} @{}: {:?}",
                            hbc_instruction.instruction_index.0,
                            hbc_instruction.offset,
                            hbc_instruction.instruction
                        );
                        // TODO: Add proper comment support when needed
                        // For now, just skip the comment
                        _ = comment_text;
                    }

                    // Update current PC for source maps
                    self.instruction_converter
                        .set_current_pc(hbc_instruction.instruction_index.0 as u32);

                    // Set duplication context if present
                    if let Some(ctx) = context {
                        self.instruction_converter
                            .set_duplication_context(Some(ctx.clone()));
                    }

                    // Convert instruction to statement(s) - apply PHI replacements if we have a context
                    // This is where the duplication context matters - we need to replace PHI values
                    match self
                        .instruction_converter
                        .convert_instruction(&hbc_instruction.instruction)
                    {
                        Ok(result) => {
                            // Handle the result based on its type
                            match result {
                                crate::ast::InstructionResult::Statement(stmt) => {
                                    // Prepare comments before borrowing comment_manager
                                    let instruction_comment = if self.include_instruction_comments {
                                        Some(format!(
                                            "PC {} (duplicated): {}",
                                            hbc_instruction.instruction_index.0,
                                            hbc_instruction
                                                .format_instruction(self.hbc_analysis.hbc_file)
                                        ))
                                    } else {
                                        None
                                    };

                                    let ssa_comment = if self.include_ssa_comments {
                                        // Use the duplicated version if we have a duplication context
                                        if let Some(ctx) = context {
                                            self.format_ssa_info_duplicated(
                                                plan,
                                                block_idx,
                                                hbc_instruction.instruction_index,
                                                &function_analysis.ssa,
                                                ctx,
                                            )
                                        } else {
                                            self.format_ssa_info(
                                                plan,
                                                block_idx,
                                                hbc_instruction.instruction_index,
                                                &function_analysis.ssa,
                                            )
                                        }
                                    } else {
                                        None
                                    };

                                    // Now add comments if we have a comment manager
                                    if let Some(ref mut comment_manager) = self.comment_manager {
                                        if let Some(instr_info) = instruction_comment {
                                            comment_manager.add_comment(
                                                &stmt,
                                                instr_info,
                                                CommentKind::Line,
                                                CommentPosition::Leading,
                                            );
                                        }

                                        if let Some(ssa_info) = ssa_comment {
                                            comment_manager.add_comment(
                                                &stmt,
                                                ssa_info,
                                                CommentKind::Line,
                                                CommentPosition::Leading,
                                            );
                                        }
                                    }

                                    statements.push(stmt);
                                }
                                crate::ast::InstructionResult::None => {
                                    // No statement generated
                                }
                                _ => {
                                    // Other result types not expected for basic block instructions
                                }
                            }
                        }
                        Err(_) => {
                            // Skip instructions that fail to convert
                        }
                    }

                    // Clear duplication context
                    if context.is_some() {
                        self.instruction_converter.set_duplication_context(None);
                    }
                }
            }
        }
    }

    /// Convert a switch structure
    fn convert_switch(
        &mut self,
        plan: &ControlFlowPlan,
        dispatch_block: &NodeIndex,
        info: &SwitchInfo,
        discriminator_value: Option<&SSAValue>,
        discriminator_use: Option<&RegisterUse>,
        case_groups: &[CaseGroupStructure],
        default_case: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // First, handle any variable declarations for the dispatch block
        // These are typically PHI nodes that need to be declared before the switch
        if let Some(declarations) = plan.block_declarations.get(dispatch_block) {
            for dup_value in declarations {
                // Check if this declaration applies to our context
                if dup_value.duplication_context == context.cloned() {
                    // Check the declaration strategy
                    if let Some(strategy) = plan.declaration_strategies.get(dup_value) {
                        match strategy {
                            DeclarationStrategy::DeclareAtDominator { kind, .. } => {
                                // Create a declaration statement
                                let var_name = self.get_variable_name(dup_value);

                                let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
                                let binding_id = self
                                    .ast_builder
                                    .binding_identifier(oxc_span::SPAN, var_atom);
                                let binding = self.ast_builder.binding_pattern(
                                    oxc_ast::ast::BindingPatternKind::BindingIdentifier(
                                        self.ast_builder.alloc(binding_id),
                                    ),
                                    None::<oxc_ast::ast::TSTypeAnnotation>,
                                    false,
                                );
                                let decl_kind = match kind {
                                    VariableKind::Let => oxc_ast::ast::VariableDeclarationKind::Let,
                                    VariableKind::Const => {
                                        oxc_ast::ast::VariableDeclarationKind::Const
                                    }
                                };
                                let declarator = self.ast_builder.variable_declarator(
                                    oxc_span::SPAN,
                                    decl_kind,
                                    binding,
                                    None, // No initializer for dominator declarations
                                    false,
                                );
                                let var_decl = self.ast_builder.declaration_variable(
                                    oxc_span::SPAN,
                                    decl_kind,
                                    self.ast_builder.vec1(declarator),
                                    false,
                                );
                                let stmt = match var_decl {
                                    oxc_ast::ast::Declaration::VariableDeclaration(v) => {
                                        Statement::VariableDeclaration(v)
                                    }
                                    _ => unreachable!("declaration_variable should always return VariableDeclaration"),
                                };

                                // Add comment for declaration
                                if let Some(ref mut comment_manager) = self.comment_manager {
                                    if self.include_ssa_comments {
                                        let comment = format!(
                                            "Variable declaration: {} [r{}_{}]",
                                            var_name,
                                            dup_value.original.register,
                                            dup_value.original.version
                                        );
                                        comment_manager.add_comment(
                                            &stmt,
                                            comment,
                                            CommentKind::Line,
                                            CommentPosition::Leading,
                                        );
                                    }
                                }

                                statements.push(stmt);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        // Collect all setup instruction SSA values so we can skip them
        let mut setup_ssa_values = std::collections::HashSet::new();
        for group in case_groups {
            for case in &group.cases {
                for setup_instr in &case.setup_instructions {
                    setup_ssa_values.insert(setup_instr.ssa_value.clone());
                }
            }
        }

        // Convert any instructions in the dispatch block that come before the switch
        // (e.g., LoadParam instructions that initialize parameters)
        // But skip any instructions that are setup instructions for cases
        if let Some(function_analysis) = self
            .hbc_analysis
            .get_function_analysis_ref(self.function_index)
        {
            if let Some(block) = function_analysis.cfg.graph().node_weight(*dispatch_block) {
                let switch_idx =
                    info.discriminator_instruction_index.value() - block.start_pc().value();
                if switch_idx > 0 {
                    // Set the duplication context for the instruction converter
                    self.instruction_converter
                        .set_duplication_context(context.cloned());

                    // Convert instructions one by one, skipping setup instructions
                    let instructions = block.instructions();
                    for (i, hbc_instruction) in instructions.iter().enumerate() {
                        if i >= switch_idx {
                            break; // Stop before the switch comparison
                        }

                        // Check if this instruction produces a setup SSA value
                        let is_setup = if let Some(target_reg) =
                            crate::generated::instruction_analysis::analyze_register_usage(
                                &hbc_instruction.instruction,
                            )
                            .target
                        {
                            // Find the SSA value defined at this instruction
                            function_analysis
                                .ssa
                                .ssa_values
                                .iter()
                                .any(|(def, ssa_value)| {
                                    def.block_id == *dispatch_block
                                        && def.instruction_idx == hbc_instruction.instruction_index
                                        && def.register == target_reg
                                        && setup_ssa_values.contains(ssa_value)
                                })
                        } else {
                            false
                        };

                        if !is_setup {
                            // Convert this single instruction using the same logic as convert_basic_block
                            // Check if this instruction should be skipped due to declaration strategy
                            let should_skip = if let Some(target_reg) =
                                crate::generated::instruction_analysis::analyze_register_usage(
                                    &hbc_instruction.instruction,
                                )
                                .target
                            {
                                // Find the SSA value defined at this instruction
                                let mut skip = false;
                                for (def, ssa_value) in &function_analysis.ssa.ssa_values {
                                    if def.block_id == *dispatch_block
                                        && def.instruction_idx == hbc_instruction.instruction_index
                                        && def.register == target_reg
                                    {
                                        let dup_value = DuplicatedSSAValue {
                                            original: ssa_value.clone(),
                                            duplication_context: context.cloned(),
                                        };

                                        if let Some(strategy) =
                                            plan.declaration_strategies.get(&dup_value)
                                        {
                                            if matches!(strategy, DeclarationStrategy::Skip) {
                                                skip = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                                skip
                            } else {
                                false
                            };

                            if !should_skip {
                                // Set the current PC for context
                                self.instruction_converter
                                    .set_current_pc(hbc_instruction.instruction_index.0 as u32);

                                // Convert instruction to statement(s)
                                match self
                                    .instruction_converter
                                    .convert_instruction(&hbc_instruction.instruction)
                                {
                                    Ok(result) => {
                                        // Handle the result based on its type
                                        match result {
                                            crate::ast::InstructionResult::Statement(stmt) => {
                                                // Add comments for this instruction
                                                // Prepare comments first to avoid borrow checker issues
                                                let instruction_comment =
                                                    if self.include_instruction_comments {
                                                        Some(format!(
                                                            "PC {}: {}",
                                                            hbc_instruction.instruction_index.0,
                                                            hbc_instruction.format_instruction(
                                                                self.hbc_analysis.hbc_file
                                                            )
                                                        ))
                                                    } else {
                                                        None
                                                    };

                                                let ssa_comment = if self.include_ssa_comments {
                                                    self.format_ssa_info(
                                                        plan,
                                                        *dispatch_block,
                                                        hbc_instruction.instruction_index,
                                                        &function_analysis.ssa,
                                                    )
                                                } else {
                                                    None
                                                };

                                                // Now add the comments
                                                if let Some(ref mut comment_manager) =
                                                    self.comment_manager
                                                {
                                                    if let Some(comment) = instruction_comment {
                                                        comment_manager.add_comment(
                                                            &stmt,
                                                            comment,
                                                            CommentKind::Line,
                                                            CommentPosition::Leading,
                                                        );
                                                    }

                                                    if let Some(ssa_info) = ssa_comment {
                                                        comment_manager.add_comment(
                                                            &stmt,
                                                            ssa_info,
                                                            CommentKind::Line,
                                                            CommentPosition::Leading,
                                                        );
                                                    }
                                                }

                                                statements.push(stmt);
                                            }
                                            crate::ast::InstructionResult::None => {
                                                // No statement generated
                                            }
                                            _ => {
                                                // Other result types not expected for dispatch block instructions
                                            }
                                        }
                                    }
                                    Err(_) => {
                                        // Skip instructions that fail to convert
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Get the discriminant expression using the SSA value and use strategy
        let discriminant =
            if let (Some(ssa_value), Some(use_site)) = (discriminator_value, discriminator_use) {
                // Use the same approach as conditionals - create a use expression
                self.create_use_expression(ssa_value, context, plan, Some(use_site.clone()))
            } else if let Some(ssa_value) = discriminator_value {
                // Fallback if we don't have the use site
                self.create_use_expression(ssa_value, context, plan, None)
            } else {
                // This shouldn't happen if the analysis is correct
                panic!("Switch discriminator should have an SSA value");
            };

        // Create switch cases
        let mut cases = self.ast_builder.vec();

        for group in case_groups {
            // Create switch case for each key in the group
            let mut first_case = true;
            for case in &group.cases {
                for key in &case.keys {
                    let test = self.create_case_key_expression(key);

                    // Only generate body for the first case in the group
                    // Others will fall through to it
                    let body = if first_case {
                        let mut case_statements = self.ast_builder.vec();

                        // First, convert any setup instructions for this case group
                        // These are instructions that were hoisted from the comparison blocks
                        // We need to deduplicate them since multiple cases might have the same setup
                        let mut processed_setup = std::collections::HashSet::new();

                        for case in &group.cases {
                            for setup_instr in &case.setup_instructions {
                                // Create a key for deduplication based on the SSA value
                                let setup_key = format!("{:?}", setup_instr.ssa_value);
                                if processed_setup.contains(&setup_key) {
                                    // Already processed this setup instruction
                                    continue;
                                }
                                processed_setup.insert(setup_key.clone());

                                // Create an assignment statement for the setup instruction
                                // The SSA value tells us what variable is being assigned
                                let var_name = self.get_variable_name(
                                    &DuplicatedSSAValue::original(setup_instr.ssa_value.clone()),
                                );

                                // Create the value expression based on the setup instruction
                                let value_expr = if let Some(ref const_value) = setup_instr.value {
                                    // It's a constant value
                                    self.create_constant_expression(const_value)
                                } else {
                                    // For non-constant setup instructions, we need to convert the instruction
                                    // This is more complex and might need the actual instruction
                                    continue; // Skip for now if we can't handle it
                                };

                                // Create the assignment: var_name = value_expr
                                let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
                                let left =
                                    oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(
                                        self.ast_builder.alloc(
                                            self.ast_builder
                                                .identifier_reference(oxc_span::SPAN, var_atom),
                                        ),
                                    );
                                let assign = self.ast_builder.expression_assignment(
                                    oxc_span::SPAN,
                                    oxc_ast::ast::AssignmentOperator::Assign,
                                    left,
                                    value_expr,
                                );
                                let stmt = self
                                    .ast_builder
                                    .statement_expression(oxc_span::SPAN, assign);

                                // Add comment for setup instruction
                                if let Some(ref mut comment_manager) = self.comment_manager {
                                    // Add instruction comment
                                    if self.include_instruction_comments {
                                        let instr_comment = format!(
                                            "PC {}: {}",
                                            setup_instr.instruction.instruction_index.0,
                                            setup_instr
                                                .instruction
                                                .format_instruction(self.hbc_analysis.hbc_file)
                                        );
                                        comment_manager.add_comment(
                                            &stmt,
                                            instr_comment,
                                            CommentKind::Line,
                                            CommentPosition::Leading,
                                        );
                                    }

                                    // Add SSA comment
                                    if self.include_ssa_comments {
                                        let ssa_comment = format!(
                                            "Setup: {} [r{}_{}]",
                                            var_name,
                                            setup_instr.ssa_value.register,
                                            setup_instr.ssa_value.version
                                        );
                                        comment_manager.add_comment(
                                            &stmt,
                                            ssa_comment,
                                            CommentKind::Line,
                                            CommentPosition::Leading,
                                        );
                                    }
                                }

                                case_statements.push(stmt);
                            }
                        }

                        self.convert_structure_id(plan, group.body, &mut case_statements, context);

                        // Handle fallthrough by duplicating blocks from the next group
                        if let Some(ref fallthrough_info) = group.fallthrough {
                            if !fallthrough_info.blocks_to_duplicate.is_empty() {
                                // Convert the duplicated blocks with the duplication context
                                // This will apply PHI replacements as needed
                                for &block_idx in &fallthrough_info.blocks_to_duplicate {
                                    // Convert just the specific block, not the whole structure
                                    self.convert_single_block(
                                        plan,
                                        block_idx,
                                        &mut case_statements,
                                        Some(&fallthrough_info.duplication_context),
                                    );
                                }
                            }
                        }

                        // Add break based on the needs_break field
                        match &group.needs_break {
                            crate::analysis::control_flow_plan::BreakRequirement::Required => {
                                let break_stmt = self.ast_builder.statement_break(
                                    oxc_span::SPAN,
                                    None,
                                );
                                case_statements.push(break_stmt);
                            }
                            crate::analysis::control_flow_plan::BreakRequirement::NotNeeded { .. } => {
                                // No break needed - case terminates on its own
                            }
                            crate::analysis::control_flow_plan::BreakRequirement::FallthroughIntended => {
                                // Since we've duplicated the next group's blocks, we need a break
                                // to prevent actual fallthrough
                                if group.fallthrough.is_some() {
                                    let break_stmt = self.ast_builder.statement_break(
                                        oxc_span::SPAN,
                                        None,
                                    );
                                    case_statements.push(break_stmt);
                                }
                            }
                        }
                        first_case = false;
                        case_statements
                    } else {
                        // Empty body - falls through to the first case
                        self.ast_builder.vec()
                    };

                    let switch_case =
                        self.ast_builder
                            .switch_case(oxc_span::SPAN, Some(test), body);
                    cases.push(switch_case);
                }
            }
        }

        // Add default case if present and non-empty
        if let Some(default_id) = default_case {
            // Check if default case is empty
            let is_empty = if let Some(structure) = plan.get_structure(*default_id) {
                matches!(
                    &structure.kind,
                    crate::analysis::control_flow_plan::ControlFlowKind::Empty
                )
            } else {
                false
            };

            // Only add default case if it's not empty
            if !is_empty {
                let mut default_statements = self.ast_builder.vec();
                self.convert_structure_id(plan, *default_id, &mut default_statements, context);

                let default_case =
                    self.ast_builder
                        .switch_case(oxc_span::SPAN, None, default_statements);
                cases.push(default_case);
            }
        }

        // Create the switch statement
        let switch_stmt = self
            .ast_builder
            .statement_switch(oxc_span::SPAN, discriminant, cases);

        // Add comment for the switch statement itself
        if let Some(ref mut comment_manager) = self.comment_manager {
            if self.include_instruction_comments {
                let comment = format!("PC {}: SwitchImm", info.discriminator_instruction_index.0);
                comment_manager.add_comment(
                    &switch_stmt,
                    comment,
                    CommentKind::Line,
                    CommentPosition::Leading,
                );
            }

            // Could add SSA comment about discriminant usage here if needed
        }

        statements.push(switch_stmt);
    }

    /// Convert a conditional structure
    fn convert_conditional(
        &mut self,
        plan: &ControlFlowPlan,
        _condition_block: NodeIndex,
        condition_expr: Option<&SSAValue>,
        condition_use: Option<&RegisterUse>,
        true_branch: StructureId,
        false_branch: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the test expression
        let test = if let Some(cond) = condition_expr {
            self.create_use_expression(cond, context, plan, condition_use.cloned())
        } else {
            // Fallback condition
            self.ast_builder
                .expression_boolean_literal(oxc_span::SPAN, true)
        };

        // Convert the consequent
        let mut consequent_stmts = self.ast_builder.vec();
        self.convert_structure_id(plan, true_branch, &mut consequent_stmts, context);
        let consequent = self
            .ast_builder
            .statement_block(oxc_span::SPAN, consequent_stmts);

        // Convert the alternate if present
        let alternate = if let Some(false_id) = false_branch {
            let mut alternate_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, *false_id, &mut alternate_stmts, context);
            Some(
                self.ast_builder
                    .statement_block(oxc_span::SPAN, alternate_stmts),
            )
        } else {
            None
        };

        // Create the if statement
        let if_stmt = self
            .ast_builder
            .statement_if(oxc_span::SPAN, test, consequent, alternate);
        statements.push(if_stmt);
    }

    /// Convert a loop structure
    fn convert_loop(
        &mut self,
        plan: &ControlFlowPlan,
        loop_type: &LoopType,
        _header_block: NodeIndex,
        condition: Option<&SSAValue>,
        condition_use: Option<&RegisterUse>,
        body: StructureId,
        update: Option<&StructureId>,
        _break_target: Option<&StructureId>,
        _continue_target: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the test expression
        let test = if let Some(cond) = condition {
            self.create_use_expression(cond, context, plan, condition_use.cloned())
        } else {
            // Infinite loop: while (true)
            self.ast_builder
                .expression_boolean_literal(oxc_span::SPAN, true)
        };

        // Convert the loop body
        let mut body_stmts = self.ast_builder.vec();
        self.convert_structure_id(plan, body, &mut body_stmts, context);

        // Add update if present (for for-loops)
        if let Some(update_id) = update {
            self.convert_structure_id(plan, *update_id, &mut body_stmts, context);
        }

        let body_block = self.ast_builder.statement_block(oxc_span::SPAN, body_stmts);

        // Create appropriate loop based on type
        match loop_type {
            LoopType::While => {
                let while_stmt = self
                    .ast_builder
                    .statement_while(oxc_span::SPAN, test, body_block);
                statements.push(while_stmt);
            }
            LoopType::DoWhile => {
                let do_while_stmt =
                    self.ast_builder
                        .statement_do_while(oxc_span::SPAN, body_block, test);
                statements.push(do_while_stmt);
            }
            LoopType::For => {
                // TODO: Properly handle for loops with init/update
                let while_stmt = self
                    .ast_builder
                    .statement_while(oxc_span::SPAN, test, body_block);
                statements.push(while_stmt);
            }
            LoopType::ForIn | LoopType::ForOf => {
                // TODO: Handle for-in/for-of loops
                let while_stmt = self
                    .ast_builder
                    .statement_while(oxc_span::SPAN, test, body_block);
                statements.push(while_stmt);
            }
        }
    }

    /// Convert a try-catch structure
    fn convert_try_catch(
        &mut self,
        plan: &ControlFlowPlan,
        try_body: StructureId,
        catch_clause: Option<&CatchClause>,
        finally_body: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Convert try block
        let mut try_stmts = self.ast_builder.vec();
        self.convert_structure_id(plan, try_body, &mut try_stmts, context);
        let try_block = self.ast_builder.block_statement(oxc_span::SPAN, try_stmts);

        // Convert catch clause if present
        let handler = if let Some(catch) = catch_clause {
            let mut catch_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, catch.body, &mut catch_stmts, context);

            // Create catch parameter from error register
            let param = {
                let error_name = format!("r{}", catch.error_register);
                let error_atom = self.ast_builder.allocator.alloc_str(&error_name);
                let binding_id = self
                    .ast_builder
                    .binding_identifier(oxc_span::SPAN, error_atom);
                let pattern = self.ast_builder.binding_pattern(
                    oxc_ast::ast::BindingPatternKind::BindingIdentifier(
                        self.ast_builder.alloc(binding_id),
                    ),
                    None::<oxc_ast::ast::TSTypeAnnotation>,
                    false,
                );
                let param = self.ast_builder.catch_parameter(oxc_span::SPAN, pattern);
                Some(param)
            };

            let catch_block = self
                .ast_builder
                .block_statement(oxc_span::SPAN, catch_stmts);

            Some(
                self.ast_builder
                    .catch_clause(oxc_span::SPAN, param, catch_block),
            )
        } else {
            None
        };

        // Convert finally block if present
        let finalizer = if let Some(finally_id) = finally_body {
            let mut finally_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, *finally_id, &mut finally_stmts, context);
            Some(
                self.ast_builder
                    .block_statement(oxc_span::SPAN, finally_stmts),
            )
        } else {
            None
        };

        let try_stmt =
            self.ast_builder
                .statement_try(oxc_span::SPAN, try_block, handler, finalizer);
        statements.push(try_stmt);
    }

    /// Convert a basic block
    fn convert_basic_block(
        &mut self,
        plan: &ControlFlowPlan,
        block_id: NodeIndex,
        instruction_count: usize,
        _is_synthetic: bool,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Handle any variable declarations for this block
        if let Some(declarations) = plan.block_declarations.get(&block_id) {
            for dup_value in declarations {
                // Check if this declaration applies to our context
                if dup_value.duplication_context == context.cloned() {
                    // Check the declaration strategy
                    if let Some(strategy) = plan.declaration_strategies.get(dup_value) {
                        match strategy {
                            DeclarationStrategy::DeclareAtDominator { kind, .. } => {
                                // Create a declaration statement
                                let var_name = self.get_variable_name(dup_value);

                                let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
                                let binding_id = self
                                    .ast_builder
                                    .binding_identifier(oxc_span::SPAN, var_atom);
                                let binding = self.ast_builder.binding_pattern(
                                    oxc_ast::ast::BindingPatternKind::BindingIdentifier(
                                        self.ast_builder.alloc(binding_id),
                                    ),
                                    None::<oxc_ast::ast::TSTypeAnnotation>,
                                    false,
                                );
                                let decl_kind = match kind {
                                    VariableKind::Let => oxc_ast::ast::VariableDeclarationKind::Let,
                                    VariableKind::Const => {
                                        oxc_ast::ast::VariableDeclarationKind::Const
                                    }
                                };
                                let declarator = self.ast_builder.variable_declarator(
                                    oxc_span::SPAN,
                                    decl_kind,
                                    binding,
                                    None, // No initializer for dominator declarations
                                    false,
                                );
                                let var_decl = self.ast_builder.declaration_variable(
                                    oxc_span::SPAN,
                                    decl_kind,
                                    self.ast_builder.vec1(declarator),
                                    false,
                                );
                                let stmt = match var_decl {
                                    oxc_ast::ast::Declaration::VariableDeclaration(v) => {
                                        Statement::VariableDeclaration(v)
                                    }
                                    _ => unreachable!("declaration_variable should always return VariableDeclaration"),
                                };

                                // Add comment for declaration
                                if let Some(ref mut comment_manager) = self.comment_manager {
                                    if self.include_ssa_comments {
                                        let comment = format!(
                                            "Variable declaration: {} [r{}_{}]",
                                            var_name,
                                            dup_value.original.register,
                                            dup_value.original.version
                                        );
                                        comment_manager.add_comment(
                                            &stmt,
                                            comment,
                                            CommentKind::Line,
                                            CommentPosition::Leading,
                                        );
                                    }
                                }

                                statements.push(stmt);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        // Handle any PHI deconstructions for this block
        let phi_key = (block_id, context.cloned());
        if let Some(phi_info) = plan.phi_deconstructions.get(&phi_key) {
            // Apply PHI replacements
            for (original, replacement) in &phi_info.replacements {
                // Create assignment: phi_var = replacement_value
                let var_name = self.get_variable_name_for_ssa(original);
                let var_atom = self.ast_builder.allocator.alloc_str(&var_name);
                let lhs = oxc_ast::ast::AssignmentTarget::AssignmentTargetIdentifier(
                    self.ast_builder.alloc(
                        self.ast_builder
                            .identifier_reference(oxc_span::SPAN, var_atom),
                    ),
                );
                // TODO: Get proper RegisterUse for PHI replacement
                let rhs = self.create_use_expression(replacement, context, plan, None);
                let assign = self.ast_builder.expression_assignment(
                    oxc_span::SPAN,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    lhs,
                    rhs,
                );
                let stmt = self
                    .ast_builder
                    .statement_expression(oxc_span::SPAN, assign);
                statements.push(stmt);
            }
        }

        // Get the function analysis to access the CFG
        if let Some(function_analysis) = self
            .hbc_analysis
            .get_function_analysis_ref(self.function_index)
        {
            // Get the block from the CFG
            if let Some(block) = function_analysis.cfg.graph().node_weight(block_id) {
                // Set the duplication context for the instruction converter
                self.instruction_converter
                    .set_duplication_context(context.cloned());

                // Convert each instruction in the block
                // If instruction_count is non-zero, only convert that many instructions
                let instructions_to_convert = if instruction_count > 0 {
                    &block.instructions[..instruction_count.min(block.instructions.len())]
                } else {
                    &block.instructions[..]
                };

                for hbc_instruction in instructions_to_convert {
                    // Check if this instruction defines a value that should be skipped
                    let should_skip = if let Some(target_reg) =
                        crate::generated::instruction_analysis::analyze_register_usage(
                            &hbc_instruction.instruction,
                        )
                        .target
                    {
                        // Find the SSA value defined at this instruction
                        let mut skip = false;
                        for (def, ssa_value) in &function_analysis.ssa.ssa_values {
                            if def.block_id == block_id
                                && def.instruction_idx == hbc_instruction.instruction_index
                                && def.register == target_reg
                            {
                                let dup_value = DuplicatedSSAValue {
                                    original: ssa_value.clone(),
                                    duplication_context: context.cloned(),
                                };

                                if let Some(strategy) = plan.declaration_strategies.get(&dup_value)
                                {
                                    if matches!(strategy, DeclarationStrategy::Skip) {
                                        skip = true;
                                        break;
                                    }
                                }
                            }
                        }
                        skip
                    } else {
                        false
                    };

                    if should_skip {
                        // Skip this instruction - it's been eliminated
                        continue;
                    }

                    // Set the current PC for context
                    self.instruction_converter
                        .set_current_pc(hbc_instruction.instruction_index.0 as u32);

                    // Convert instruction to statement(s)
                    match self
                        .instruction_converter
                        .convert_instruction(&hbc_instruction.instruction)
                    {
                        Ok(result) => {
                            // Handle the result based on its type
                            match result {
                                crate::ast::InstructionResult::Statement(stmt) => {
                                    // Prepare comments before borrowing comment_manager
                                    let instruction_comment = if self.include_instruction_comments {
                                        let comment = format!(
                                            "PC {}: {}",
                                            hbc_instruction.instruction_index.0,
                                            hbc_instruction
                                                .format_instruction(self.hbc_analysis.hbc_file)
                                        );
                                        Some(comment)
                                    } else {
                                        None
                                    };

                                    let ssa_comment = if self.include_ssa_comments {
                                        self.format_ssa_info(
                                            plan,
                                            block_id,
                                            hbc_instruction.instruction_index,
                                            &function_analysis.ssa,
                                        )
                                    } else {
                                        None
                                    };

                                    // Now add comments if we have a comment manager
                                    if let Some(ref mut comment_manager) = self.comment_manager {
                                        if let Some(instr_info) = instruction_comment {
                                            comment_manager.add_comment(
                                                &stmt,
                                                instr_info,
                                                CommentKind::Line,
                                                CommentPosition::Leading,
                                            );
                                        }

                                        if let Some(ssa_info) = ssa_comment {
                                            comment_manager.add_comment(
                                                &stmt,
                                                ssa_info,
                                                CommentKind::Line,
                                                CommentPosition::Leading,
                                            );
                                        }
                                    }

                                    statements.push(stmt);
                                }
                                crate::ast::InstructionResult::JumpCondition(_) => {
                                    // Jumps are handled by the control flow structure
                                    // We don't need to generate them here
                                }
                                crate::ast::InstructionResult::None => {
                                    // No statement generated
                                }
                            }
                        }
                        Err(e) => {
                            // Add error comment
                            let error_comment = format!(
                                "// Error converting instruction at {}: {}",
                                hbc_instruction.instruction_index.0, e
                            );
                            statements.push(self.create_comment_statement(&error_comment));
                        }
                    }
                }
            }
        }
    }

    /// Create an expression for using an SSA value
    fn create_use_expression(
        &mut self,
        value: &SSAValue,
        context: Option<&DuplicationContext>,
        plan: &ControlFlowPlan,
        use_location: Option<crate::cfg::ssa::RegisterUse>,
    ) -> Expression<'a> {
        // Create duplicated SSA value for lookups
        let dup_value = if let Some(ctx) = context {
            DuplicatedSSAValue {
                original: value.clone(),
                duplication_context: Some(ctx.clone()),
            }
        } else {
            DuplicatedSSAValue::original(value.clone())
        };

        // Check use strategy if we have a use location
        if let Some(use_loc) = use_location {
            if let Some(strategy) = plan.use_strategies.get(&(dup_value.clone(), use_loc)) {
                match strategy {
                    UseStrategy::InlineValue(constant) => {
                        // Inline the constant value directly
                        return self.create_constant_expression(constant);
                    }
                    UseStrategy::UseVariable => {
                        // Fall through to use variable name
                    }
                }
            }
        }

        // Default: use the variable name
        let name = self.get_variable_name(&dup_value);
        self.create_identifier_expression(&name)
    }

    /// Create an expression for a case key
    fn create_case_key_expression(&self, key: &CaseKey) -> Expression<'a> {
        match key {
            CaseKey::Number(n) => self.ast_builder.expression_numeric_literal(
                oxc_span::SPAN,
                n.into_inner(),
                None,
                oxc_ast::ast::NumberBase::Decimal,
            ),
            CaseKey::String(s) => {
                let string_atom = self.ast_builder.allocator.alloc_str(s);
                self.ast_builder
                    .expression_string_literal(oxc_span::SPAN, string_atom, None)
            }
            CaseKey::Boolean(b) => self
                .ast_builder
                .expression_boolean_literal(oxc_span::SPAN, *b),
            CaseKey::Null => self.ast_builder.expression_null_literal(oxc_span::SPAN),
            CaseKey::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder
                    .expression_identifier(oxc_span::SPAN, undefined_atom)
            }
        }
    }

    /// Create a constant expression
    fn create_constant_expression(&self, value: &ConstantValue) -> Expression<'a> {
        match value {
            ConstantValue::Number(n) => self.ast_builder.expression_numeric_literal(
                oxc_span::SPAN,
                *n,
                None,
                oxc_ast::ast::NumberBase::Decimal,
            ),
            ConstantValue::String(s) => {
                let string_atom = self.ast_builder.allocator.alloc_str(s);
                self.ast_builder
                    .expression_string_literal(oxc_span::SPAN, string_atom, None)
            }
            ConstantValue::Boolean(b) => self
                .ast_builder
                .expression_boolean_literal(oxc_span::SPAN, *b),
            ConstantValue::Null => self.ast_builder.expression_null_literal(oxc_span::SPAN),
            ConstantValue::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder
                    .expression_identifier(oxc_span::SPAN, undefined_atom)
            }
        }
    }

    /// Create an identifier expression
    fn create_identifier_expression(&self, name: &str) -> Expression<'a> {
        let name_atom = self.ast_builder.allocator.alloc_str(name);
        self.ast_builder
            .expression_identifier(oxc_span::SPAN, name_atom)
    }

    /// Get the variable name for a duplicated SSA value
    fn get_variable_name(&mut self, value: &DuplicatedSSAValue) -> String {
        if let Some(name) = self.variable_names.get(value) {
            return name.clone();
        }

        // Get the proper variable name from the register manager's mapping
        let name = self
            .instruction_converter
            .register_manager()
            .get_variable_name_for_duplicated(value);

        self.variable_names.insert(value.clone(), name.clone());
        name
    }

    /// Get the variable name for a plain SSA value
    fn get_variable_name_for_ssa(&mut self, value: &SSAValue) -> String {
        let dup_value = DuplicatedSSAValue::original(value.clone());
        self.get_variable_name(&dup_value)
    }

    /// Create a comment statement
    fn create_comment_statement(&self, _text: &str) -> Statement<'a> {
        // For now, create an empty statement
        // TODO: Properly attach comments
        self.ast_builder.statement_empty(oxc_span::SPAN)
    }

    /// Format SSA information for a comment with enhanced details
    fn format_ssa_info(
        &mut self,
        plan: &ControlFlowPlan,
        block_id: NodeIndex,
        instruction_index: crate::hbc::InstructionIndex,
        ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
    ) -> Option<String> {
        let mut comment_parts = Vec::new();

        // Find SSA definitions and uses at this instruction index
        for def in &ssa_analysis.definitions {
            if def.instruction_idx == instruction_index && def.block_id == block_id {
                if let Some(ssa_value) = ssa_analysis.ssa_values.get(def) {
                    let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
                    let var_name = self.get_variable_name(&dup_value);

                    // Get the declaration strategy from the plan
                    let strategy_str = if let Some(strategy) =
                        plan.declaration_strategies.get(&dup_value)
                    {
                        match strategy {
                            DeclarationStrategy::DeclareAndInitialize { kind } => match kind {
                                VariableKind::Const => "const",
                                VariableKind::Let => "let",
                            },
                            DeclarationStrategy::DeclareAtDominator { kind, .. } => match kind {
                                VariableKind::Const => "const@dom",
                                VariableKind::Let => "let@dom",
                            },
                            DeclarationStrategy::AssignOnly => "assign",
                            DeclarationStrategy::Skip => "skip",
                        }
                    } else {
                        ""
                    };

                    comment_parts.push(format!(
                        "DEF[{}]: r{} → r{}_{} [{}]",
                        strategy_str, def.register, ssa_value.register, ssa_value.version, var_name
                    ));
                }
            }
        }

        for use_site in &ssa_analysis.uses {
            if use_site.instruction_idx == instruction_index && use_site.block_id == block_id {
                if let Some(def_site) = ssa_analysis.use_def_chains.get(use_site) {
                    if let Some(ssa_value) = ssa_analysis.ssa_values.get(def_site) {
                        let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
                        let var_name = self.get_variable_name(&dup_value);

                        // Get the use strategy from the plan
                        let use_key = (dup_value.clone(), use_site.clone());
                        let strategy_str = if let Some(strategy) = plan.use_strategies.get(&use_key)
                        {
                            match strategy {
                                UseStrategy::UseVariable => "var",
                                UseStrategy::InlineValue(_) => "inline",
                            }
                        } else {
                            ""
                        };

                        comment_parts.push(format!(
                            "USE[{}]: r{} → r{}_{} [{}]",
                            strategy_str,
                            use_site.register,
                            ssa_value.register,
                            ssa_value.version,
                            var_name
                        ));
                    }
                }
            }
        }

        if comment_parts.is_empty() {
            None
        } else {
            Some(comment_parts.join(", "))
        }
    }

    /// Format SSA information for duplicated instructions with context
    fn format_ssa_info_duplicated(
        &mut self,
        plan: &ControlFlowPlan,
        block_id: NodeIndex,
        instruction_index: crate::hbc::InstructionIndex,
        ssa_analysis: &crate::cfg::ssa::SSAAnalysis,
        duplication_context: &DuplicationContext,
    ) -> Option<String> {
        let mut comment_parts = Vec::new();

        // Find SSA definitions and uses at this instruction index
        for def in &ssa_analysis.definitions {
            if def.instruction_idx == instruction_index && def.block_id == block_id {
                if let Some(ssa_value) = ssa_analysis.ssa_values.get(def) {
                    let dup_value = DuplicatedSSAValue {
                        original: ssa_value.clone(),
                        duplication_context: Some(duplication_context.clone()),
                    };
                    let var_name = self.get_variable_name(&dup_value);

                    // Get the declaration strategy from the plan
                    let strategy_str = if let Some(strategy) =
                        plan.declaration_strategies.get(&dup_value)
                    {
                        match strategy {
                            DeclarationStrategy::DeclareAndInitialize { kind } => match kind {
                                VariableKind::Const => "const/dup",
                                VariableKind::Let => "let/dup",
                            },
                            DeclarationStrategy::DeclareAtDominator { kind, .. } => match kind {
                                VariableKind::Const => "const@dom/dup",
                                VariableKind::Let => "let@dom/dup",
                            },
                            DeclarationStrategy::AssignOnly => "assign/dup",
                            DeclarationStrategy::Skip => "skip/dup",
                        }
                    } else {
                        "dup"
                    };

                    comment_parts.push(format!(
                        "DEF[{}]: r{} → r{}_{} [{}]",
                        strategy_str, def.register, ssa_value.register, ssa_value.version, var_name
                    ));
                }
            }
        }

        for use_site in &ssa_analysis.uses {
            if use_site.instruction_idx == instruction_index && use_site.block_id == block_id {
                if let Some(def_site) = ssa_analysis.use_def_chains.get(use_site) {
                    if let Some(ssa_value) = ssa_analysis.ssa_values.get(def_site) {
                        // For duplicated blocks, try to use the duplicated value
                        let dup_value = DuplicatedSSAValue {
                            original: ssa_value.clone(),
                            duplication_context: Some(duplication_context.clone()),
                        };

                        // Try to get the duplicated variable name first, fall back to original if not found
                        let var_name = if self.variable_names.contains_key(&dup_value) {
                            self.get_variable_name(&dup_value)
                        } else {
                            let original_dup = DuplicatedSSAValue::original(ssa_value.clone());
                            self.get_variable_name(&original_dup)
                        };

                        // Get the use strategy from the plan
                        let use_key = (dup_value.clone(), use_site.clone());
                        let strategy_str = if let Some(strategy) = plan.use_strategies.get(&use_key)
                        {
                            match strategy {
                                UseStrategy::UseVariable => "var/dup",
                                UseStrategy::InlineValue(_) => "inline/dup",
                            }
                        } else {
                            "dup"
                        };

                        comment_parts.push(format!(
                            "USE[{}]: r{} → r{}_{} [{}]",
                            strategy_str,
                            use_site.register,
                            ssa_value.register,
                            ssa_value.version,
                            var_name
                        ));
                    }
                }
            }
        }

        if comment_parts.is_empty() {
            None
        } else {
            Some(comment_parts.join(", "))
        }
    }
}
