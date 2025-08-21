//! Converts a ControlFlowPlan into JavaScript AST
//!
//! This module replaces the old block_converter/switch_converter/conditional_converter
//! with a unified approach that works directly from the analyzed ControlFlowPlan.

use crate::analysis::control_flow_plan::{
    CaseGroupStructure, CatchClause, ControlFlowKind, ControlFlowPlan,
    ControlFlowStructure, LoopType, SequentialElement, StructureId,
};
use crate::cfg::switch_analysis::switch_info::{CaseKey, SwitchInfo};
use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, UseStrategy};
use crate::analysis::value_tracker::ConstantValue;
use crate::ast::{ExpressionContext, InstructionToStatementConverter};
use crate::cfg::ssa::{DuplicatedSSAValue, DuplicationContext, RegisterUse, SSAValue};
use crate::hbc::HbcFile;
use oxc_allocator::Vec as OxcVec;
use oxc_ast::ast::*;
use oxc_ast::AstBuilder;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Converts a ControlFlowPlan into JavaScript AST
pub struct ControlFlowPlanConverter<'a> {
    /// AST builder for creating nodes
    ast_builder: &'a AstBuilder<'a>,
    /// HBC file for looking up constants and metadata
    hbc: &'a HbcFile<'a>,
    /// HBC analysis for function information
    hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
    /// Function index we're converting
    function_index: u32,
    /// Instruction-to-statement converter for basic blocks
    instruction_converter: InstructionToStatementConverter<'a>,
    /// Track which variables have been declared
    declared_variables: HashSet<String>,
    /// Map from SSA values to variable names
    variable_names: HashMap<DuplicatedSSAValue, String>,
}

impl<'a> ControlFlowPlanConverter<'a> {
    /// Create a new converter
    pub fn new(
        ast_builder: &'a AstBuilder<'a>,
        hbc: &'a HbcFile<'a>,
        hbc_analysis: &'a crate::analysis::HbcAnalysis<'a>,
        function_index: u32,
    ) -> Self {
        let expression_context = ExpressionContext::with_context(
            hbc,
            function_index,
            crate::hbc::InstructionIndex::zero(),
        );
        
        let instruction_converter = InstructionToStatementConverter::new(
            ast_builder,
            expression_context,
            hbc_analysis,
        );

        Self {
            ast_builder,
            hbc,
            hbc_analysis,
            function_index,
            instruction_converter,
            declared_variables: HashSet::new(),
            variable_names: HashMap::new(),
        }
    }

    /// Convert the entire plan to statements
    pub fn convert_to_ast(&mut self, plan: &ControlFlowPlan) -> OxcVec<'a, Statement<'a>> {
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
                case_groups,
                default_case,
            } => {
                self.convert_switch(plan, dispatch_block, info, case_groups, default_case.as_ref(), statements, context);
            }
            ControlFlowKind::Conditional {
                condition_block,
                condition_expr,
                true_branch,
                false_branch,
            } => {
                self.convert_conditional(plan, 
                    *condition_block,
                    condition_expr.as_ref(),
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
                body,
                update,
                break_target,
                continue_target,
            } => {
                self.convert_loop(plan, 
                    loop_type,
                    *header_block,
                    condition.as_ref(),
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
                self.convert_try_catch(plan, *try_body, catch_clause.as_ref(), finally_body.as_ref(), statements, context);
            }
            ControlFlowKind::BasicBlock {
                block,
                instruction_count,
                is_synthetic,
            } => {
                self.convert_basic_block(plan, *block, *instruction_count, *is_synthetic, statements, context);
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

    /// Convert a switch structure
    fn convert_switch(
        &mut self,
        plan: &ControlFlowPlan,
        dispatch_block: &NodeIndex,
        info: &SwitchInfo,
        case_groups: &[CaseGroupStructure],
        default_case: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the discriminant expression
        // TODO: Convert discriminator register to SSA value
        let discriminant_name = format!("r{}", info.discriminator);
        let discriminant_atom = self.ast_builder.allocator.alloc_str(&discriminant_name);
        let discriminant = self.ast_builder.expression_identifier(
            oxc_span::SPAN,
            discriminant_atom,
        );
        
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
                        self.convert_structure_id(plan, group.body, &mut case_statements, context);
                        
                        // Add break if no fallthrough
                        if group.fallthrough.is_none() {
                            let break_stmt = self.ast_builder.statement_break(
                                oxc_span::SPAN,
                                None,
                            );
                            case_statements.push(break_stmt);
                        }
                        first_case = false;
                        case_statements
                    } else {
                        // Empty body - falls through to the first case
                        self.ast_builder.vec()
                    };
                    
                    let switch_case = self.ast_builder.switch_case(
                        oxc_span::SPAN,
                        Some(test),
                        body,
                    );
                    cases.push(switch_case);
                }
            }
        }
        
        // Add default case if present
        if let Some(default_id) = default_case {
            let mut default_statements = self.ast_builder.vec();
            self.convert_structure_id(plan, *default_id, &mut default_statements, context);
            
            let default_case = self.ast_builder.switch_case(
                oxc_span::SPAN,
                None,
                default_statements,
            );
            cases.push(default_case);
        }
        
        // Create the switch statement
        let switch_stmt = self.ast_builder.statement_switch(
            oxc_span::SPAN,
            discriminant,
            cases,
        );
        statements.push(switch_stmt);
    }

    /// Convert a conditional structure
    fn convert_conditional(
        &mut self,
        plan: &ControlFlowPlan,
        condition_block: NodeIndex,
        condition_expr: Option<&SSAValue>,
        true_branch: StructureId,
        false_branch: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the test expression
        let test = if let Some(cond) = condition_expr {
            self.create_use_expression(cond, context)
        } else {
            // Fallback condition
            self.ast_builder.expression_boolean_literal(oxc_span::SPAN, true)
        };
        
        // Convert the consequent
        let mut consequent_stmts = self.ast_builder.vec();
        self.convert_structure_id(plan, true_branch, &mut consequent_stmts, context);
        let consequent = self.ast_builder.statement_block(
            oxc_span::SPAN,
            consequent_stmts,
        );
        
        // Convert the alternate if present
        let alternate = if let Some(false_id) = false_branch {
            let mut alternate_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, *false_id, &mut alternate_stmts, context);
            Some(self.ast_builder.statement_block(
                oxc_span::SPAN,
                alternate_stmts,
            ))
        } else {
            None
        };
        
        // Create the if statement
        let if_stmt = self.ast_builder.statement_if(
            oxc_span::SPAN,
            test,
            consequent,
            alternate,
        );
        statements.push(if_stmt);
    }

    /// Convert a loop structure
    fn convert_loop(
        &mut self,
        plan: &ControlFlowPlan,
        loop_type: &LoopType,
        header_block: NodeIndex,
        condition: Option<&SSAValue>,
        body: StructureId,
        update: Option<&StructureId>,
        break_target: Option<&StructureId>,
        continue_target: Option<&StructureId>,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
        // Get the test expression
        let test = if let Some(cond) = condition {
            self.create_use_expression(cond, context)
        } else {
            // Infinite loop: while (true)
            self.ast_builder.expression_boolean_literal(oxc_span::SPAN, true)
        };
        
        // Convert the loop body
        let mut body_stmts = self.ast_builder.vec();
        self.convert_structure_id(plan, body, &mut body_stmts, context);
        
        // Add update if present (for for-loops)
        if let Some(update_id) = update {
            self.convert_structure_id(plan, *update_id, &mut body_stmts, context);
        }
        
        let body_block = self.ast_builder.statement_block(
            oxc_span::SPAN,
            body_stmts,
        );
        
        // Create appropriate loop based on type
        match loop_type {
            LoopType::While => {
                let while_stmt = self.ast_builder.statement_while(
                    oxc_span::SPAN,
                    test,
                    body_block,
                );
                statements.push(while_stmt);
            }
            LoopType::DoWhile => {
                let do_while_stmt = self.ast_builder.statement_do_while(
                    oxc_span::SPAN,
                    body_block,
                    test,
                );
                statements.push(do_while_stmt);
            }
            LoopType::For => {
                // TODO: Properly handle for loops with init/update
                let while_stmt = self.ast_builder.statement_while(
                    oxc_span::SPAN,
                    test,
                    body_block,
                );
                statements.push(while_stmt);
            }
            LoopType::ForIn | LoopType::ForOf => {
                // TODO: Handle for-in/for-of loops
                let while_stmt = self.ast_builder.statement_while(
                    oxc_span::SPAN,
                    test,
                    body_block,
                );
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
        let try_block = self.ast_builder.block_statement(
            oxc_span::SPAN,
            try_stmts,
        );
        
        // Convert catch clause if present
        let handler = if let Some(catch) = catch_clause {
            let mut catch_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, catch.body, &mut catch_stmts, context);
            
            // Create catch parameter from error register
            let param = {
                let error_name = format!("r{}", catch.error_register);
                let error_atom = self.ast_builder.allocator.alloc_str(&error_name);
                let binding_id = self.ast_builder.binding_identifier(oxc_span::SPAN, error_atom);
                let pattern = self.ast_builder.binding_pattern(
                    oxc_ast::ast::BindingPatternKind::BindingIdentifier(self.ast_builder.alloc(binding_id)),
                    None::<oxc_ast::ast::TSTypeAnnotation>,
                    false,
                );
                let param = self.ast_builder.catch_parameter(
                    oxc_span::SPAN,
                    pattern,
                );
                Some(param)
            };
            
            let catch_block = self.ast_builder.block_statement(
                oxc_span::SPAN,
                catch_stmts,
            );
            
            Some(self.ast_builder.catch_clause(
                oxc_span::SPAN,
                param,
                catch_block,
            ))
        } else {
            None
        };
        
        // Convert finally block if present
        let finalizer = if let Some(finally_id) = finally_body {
            let mut finally_stmts = self.ast_builder.vec();
            self.convert_structure_id(plan, *finally_id, &mut finally_stmts, context);
            Some(self.ast_builder.block_statement(
                oxc_span::SPAN,
                finally_stmts,
            ))
        } else {
            None
        };
        
        let try_stmt = self.ast_builder.statement_try(
            oxc_span::SPAN,
            try_block,
            handler,
            finalizer,
        );
        statements.push(try_stmt);
    }

    /// Convert a basic block
    fn convert_basic_block(
        &mut self,
        plan: &ControlFlowPlan,
        block_id: NodeIndex,
        instruction_count: usize,
        is_synthetic: bool,
        statements: &mut OxcVec<'a, Statement<'a>>,
        context: Option<&DuplicationContext>,
    ) {
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
                        self.ast_builder.identifier_reference(oxc_span::SPAN, var_atom)
                    )
                );
                let rhs = self.create_use_expression(replacement, context);
                let assign = self.ast_builder.expression_assignment(
                    oxc_span::SPAN,
                    oxc_ast::ast::AssignmentOperator::Assign,
                    lhs,
                    rhs,
                );
                let stmt = self.ast_builder.statement_expression(
                    oxc_span::SPAN,
                    assign,
                );
                statements.push(stmt);
            }
        }
        
        // Get the function analysis to access the CFG
        if let Some(function_analysis) = self.hbc_analysis.get_function_analysis_ref(self.function_index) {
            // Get the block from the CFG
            if let Some(block) = function_analysis.cfg.graph().node_weight(block_id) {
                // Set the duplication context for the instruction converter
                self.instruction_converter.set_duplication_context(context.cloned());
                
                // Convert each instruction in the block
                for hbc_instruction in &block.instructions {
                    // Set the current PC for context
                    self.instruction_converter.set_current_pc(hbc_instruction.instruction_index.0 as u32);
                    
                    // Convert instruction to statement(s)
                    match self.instruction_converter.convert_instruction(&hbc_instruction.instruction) {
                        Ok(result) => {
                            // Handle the result based on its type
                            match result {
                                crate::ast::InstructionResult::Statement(stmt) => {
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
                            let error_comment = format!("// Error converting instruction at {}: {}", hbc_instruction.instruction_index.0, e);
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
        
        // For now, just use the variable name
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
                self.ast_builder.expression_string_literal(
                    oxc_span::SPAN,
                    string_atom,
                    None,
                )
            }
            CaseKey::Boolean(b) => self.ast_builder.expression_boolean_literal(
                oxc_span::SPAN,
                *b,
            ),
            CaseKey::Null => self.ast_builder.expression_null_literal(oxc_span::SPAN),
            CaseKey::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder.expression_identifier(
                    oxc_span::SPAN,
                    undefined_atom,
                )
            }
        }
    }

    /// Create a constant expression
    fn create_constant_expression(&self, value: &ConstantValue) -> Expression<'a> {
        match value {
            ConstantValue::Number(n) => {
                self.ast_builder.expression_numeric_literal(
                    oxc_span::SPAN,
                    *n,
                    None,
                    oxc_ast::ast::NumberBase::Decimal,
                )
            }
            ConstantValue::String(s) => {
                let string_atom = self.ast_builder.allocator.alloc_str(s);
                self.ast_builder.expression_string_literal(
                    oxc_span::SPAN,
                    string_atom,
                    None,
                )
            }
            ConstantValue::Boolean(b) => {
                self.ast_builder.expression_boolean_literal(
                    oxc_span::SPAN,
                    *b,
                )
            }
            ConstantValue::Null => {
                self.ast_builder.expression_null_literal(oxc_span::SPAN)
            }
            ConstantValue::Undefined => {
                let undefined_atom = self.ast_builder.allocator.alloc_str("undefined");
                self.ast_builder.expression_identifier(
                    oxc_span::SPAN,
                    undefined_atom,
                )
            }
        }
    }

    /// Create an identifier expression
    fn create_identifier_expression(&self, name: &str) -> Expression<'a> {
        let name_atom = self.ast_builder.allocator.alloc_str(name);
        self.ast_builder.expression_identifier(
            oxc_span::SPAN,
            name_atom,
        )
    }

    /// Get the variable name for a duplicated SSA value
    fn get_variable_name(&mut self, value: &DuplicatedSSAValue) -> String {
        if let Some(name) = self.variable_names.get(value) {
            return name.clone();
        }
        
        // Generate a name based on the SSA value
        let name = value.to_string();
        self.variable_names.insert(value.clone(), name.clone());
        name
    }

    /// Get the variable name for a plain SSA value
    fn get_variable_name_for_ssa(&mut self, value: &SSAValue) -> String {
        let dup_value = DuplicatedSSAValue::original(value.clone());
        self.get_variable_name(&dup_value)
    }

    /// Create a comment statement
    fn create_comment_statement(&self, text: &str) -> Statement<'a> {
        // For now, create an empty statement
        // TODO: Properly attach comments
        self.ast_builder.statement_empty(oxc_span::SPAN)
    }
}