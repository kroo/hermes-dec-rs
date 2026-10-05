//! Instruction dispatch for exception regions that cannot yet be structured.
//!
//! Keeping the original registers and protected ranges avoids SSA joins across
//! exceptional edges and preserves the compiler's duplicated finally paths.

use crate::analysis::{control_flow_plan::ControlFlowPlan, FunctionAnalysis, HbcAnalysis};
use crate::ast::{ExpressionContext, InstructionResult, InstructionToStatementConverter, JumpType};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::{HbcFile, InstructionIndex};
use crate::{DecompilerError, DecompilerResult};
use oxc_allocator::Vec as OxcVec;
use oxc_ast::{ast::*, AstBuilder};
use oxc_span::SPAN;
use oxc_syntax::number::NumberBase;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::Internal {
        message: message.into(),
    }
}

struct DispatchBuilder<'a> {
    ast: &'a AstBuilder<'a>,
}

impl<'a> DispatchBuilder<'a> {
    fn identifier(&self, name: &str) -> Expression<'a> {
        self.ast
            .expression_identifier(SPAN, self.ast.allocator.alloc_str(name))
    }

    fn number(&self, value: u32) -> Expression<'a> {
        self.ast
            .expression_numeric_literal(SPAN, value as f64, None, NumberBase::Decimal)
    }

    fn assign(&self, name: &str, value: Expression<'a>) -> Statement<'a> {
        let target = AssignmentTarget::AssignmentTargetIdentifier(
            self.ast.alloc(
                self.ast
                    .identifier_reference(SPAN, self.ast.allocator.alloc_str(name)),
            ),
        );
        let assignment =
            self.ast
                .expression_assignment(SPAN, AssignmentOperator::Assign, target, value);
        self.ast.statement_expression(SPAN, assignment)
    }

    fn transition(&self, target: u32) -> OxcVec<'a, Statement<'a>> {
        let mut statements = self.ast.vec();
        statements.push(self.assign("__hbc_pc", self.number(target)));
        statements.push(self.ast.statement_continue(SPAN, None));
        statements
    }
}

pub(crate) fn convert_exception_fallback<'a>(
    ast: &'a AstBuilder<'a>,
    hbc: &'a HbcFile<'a>,
    analysis: &'a HbcAnalysis<'a>,
    function: &FunctionAnalysis<'a>,
    function_index: u32,
) -> DecompilerResult<OxcVec<'a, Statement<'a>>> {
    let header = hbc
        .functions
        .get_parsed_header(function_index)
        .ok_or_else(|| error("Missing exception fallback function header"))?;
    let instructions = hbc.functions.get_instructions(function_index)?;
    let mut plan = ControlFlowPlan::new();
    plan.call_site_analysis =
        crate::analysis::call_site_analysis::CallSiteAnalysis::analyze(&function.cfg);
    let context = ExpressionContext::with_context(hbc, function_index, InstructionIndex::zero());
    let mut converter = InstructionToStatementConverter::new(ast, context, analysis, plan);
    converter.register_manager_mut().use_physical_registers();
    let dispatch = DispatchBuilder { ast };
    let mut statements = ast.vec();
    statements.push(
        converter
            .create_variable_declaration(
                "__hbc_pc",
                Some(dispatch.number(0)),
                VariableDeclarationKind::Let,
            )
            .map_err(|e| error(e.to_string()))?,
    );
    statements.push(
        converter
            .create_variable_declaration("__hbc_error", None, VariableDeclarationKind::Let)
            .map_err(|e| error(e.to_string()))?,
    );
    let frame_size = header
        .large_header
        .as_ref()
        .map_or(header.header.frame_size(), |large| large.frame_size);
    for register in 0..frame_size {
        statements.push(
            converter
                .create_variable_declaration(
                    &format!("var{register}"),
                    None,
                    VariableDeclarationKind::Let,
                )
                .map_err(|e| error(e.to_string()))?,
        );
    }

    let mut cases = ast.vec();
    for instruction in &instructions {
        let pc = instruction.instruction_index.value() as u32;
        converter.set_current_pc(pc);
        let block = function
            .cfg
            .builder()
            .get_block_at_pc(pc)
            .or_else(|| {
                function.cfg.graph().node_indices().find(|&node| {
                    let block = &function.cfg.graph()[node];
                    block.start_pc() <= instruction.instruction_index
                        && instruction.instruction_index < block.end_pc()
                })
            })
            .ok_or_else(|| error(format!("No block for fallback instruction {pc}")))?;
        converter.register_manager_mut().set_current_block(block);
        let mut body = ast.vec();
        match &instruction.instruction {
            UnifiedInstruction::Catch { operand_0 } => {
                body.push(dispatch.assign(
                    &format!("var{operand_0}"),
                    dispatch.identifier("__hbc_error"),
                ));
            }
            UnifiedInstruction::Construct {
                operand_0,
                operand_1,
                ..
            }
            | UnifiedInstruction::ConstructLong {
                operand_0,
                operand_1,
                ..
            } => {
                let call_site = converter
                    .control_flow_plan
                    .call_site_analysis
                    .call_sites
                    .get(&(block, instruction.instruction_index))
                    .ok_or_else(|| error(format!("Missing constructor arguments at {pc}")))?;
                // Native constructors (including Error and classes) require construction,
                // not a .call() on the provisional CreateThis object.
                let mut arguments = ast.vec();
                for register in call_site.argument_registers.iter().skip(1) {
                    arguments.push(Argument::from(
                        dispatch.identifier(&format!("var{register}")),
                    ));
                }
                let expression = ast.expression_new(
                    SPAN,
                    dispatch.identifier(&format!("var{operand_1}")),
                    None::<TSTypeParameterInstantiation>,
                    arguments,
                );
                body.push(dispatch.assign(&format!("var{operand_0}"), expression));
            }
            UnifiedInstruction::SwitchImm { operand_0, .. } => {
                let table = hbc
                    .switch_tables
                    .get_switch_table_by_instruction(function_index, pc)
                    .ok_or_else(|| error(format!("Missing fallback switch table at {pc}")))?;
                let mut targets = ast.vec();
                for case in &table.cases {
                    let target = case
                        .target_instruction_index
                        .ok_or_else(|| error("Unresolved switch target"))?;
                    targets.push(ast.switch_case(
                        SPAN,
                        Some(dispatch.number(case.value)),
                        dispatch.transition(target),
                    ));
                }
                let target = table
                    .default_instruction_index
                    .ok_or_else(|| error("Unresolved default target"))?;
                targets.push(ast.switch_case(SPAN, None, dispatch.transition(target)));
                body.push(ast.statement_switch(
                    SPAN,
                    dispatch.identifier(&format!("var{operand_0}")),
                    targets,
                ));
            }
            _ => match converter
                .convert_instruction(&instruction.instruction)
                .map_err(|e| error(format!("Fallback instruction {pc}: {e}")))?
            {
                InstructionResult::Statement(statement) => body.push(statement),
                InstructionResult::None => {}
                InstructionResult::JumpCondition(jump) => {
                    let offset = jump
                        .target_offset
                        .ok_or_else(|| error("Missing jump offset"))?;
                    let address = (instruction.offset.value() as i64 + offset as i64) as u32;
                    let target = hbc
                        .jump_table
                        .byte_offset_to_instruction_index(function_index, address)
                        .ok_or_else(|| error(format!("Unresolved fallback jump at {pc}")))?;
                    let next = if let Some(mut condition) = jump.condition_expression {
                        if matches!(jump.jump_type, JumpType::False) {
                            condition =
                                ast.expression_unary(SPAN, UnaryOperator::LogicalNot, condition);
                        }
                        // Negated relational jumps must retain NaN behavior.
                        let name = instruction.instruction.name();
                        if name.starts_with("JNot") {
                            let usage =
                                crate::generated::instruction_analysis::analyze_register_usage(
                                    &instruction.instruction,
                                );
                            let operator = if name.starts_with("JNotLessEqual") {
                                BinaryOperator::LessEqualThan
                            } else if name.starts_with("JNotGreaterEqual") {
                                BinaryOperator::GreaterEqualThan
                            } else if name.starts_with("JNotLess") {
                                BinaryOperator::LessThan
                            } else {
                                BinaryOperator::GreaterThan
                            };
                            let comparison = ast.expression_binary(
                                SPAN,
                                dispatch.identifier(&format!("var{}", usage.sources[0])),
                                operator,
                                dispatch.identifier(&format!("var{}", usage.sources[1])),
                            );
                            condition =
                                ast.expression_unary(SPAN, UnaryOperator::LogicalNot, comparison);
                        }
                        ast.expression_conditional(
                            SPAN,
                            condition,
                            dispatch.number(target),
                            dispatch.number(pc + 1),
                        )
                    } else {
                        dispatch.number(target)
                    };
                    body.push(dispatch.assign("__hbc_pc", next));
                    body.push(ast.statement_continue(SPAN, None));
                }
            },
        }
        if !matches!(
            instruction.instruction,
            UnifiedInstruction::Ret { .. }
                | UnifiedInstruction::Throw { .. }
                | UnifiedInstruction::SwitchImm { .. }
        ) && !matches!(body.last(), Some(Statement::ContinueStatement(_)))
        {
            body.extend(dispatch.transition(pc + 1));
        }
        cases.push(ast.switch_case(SPAN, Some(dispatch.number(pc)), body));
    }
    let invalid_pc = ast.statement_throw(SPAN, dispatch.identifier("__hbc_error"));
    cases.push(ast.switch_case(SPAN, None, ast.vec1(invalid_pc)));
    let switch = ast.statement_switch(SPAN, dispatch.identifier("__hbc_pc"), cases);
    let try_body = ast.block_statement(SPAN, ast.vec1(switch));

    let mut catch_body = ast.vec();
    // Hermes selects the first matching protected range in bytecode table order.
    for handler in &header.exc_handlers {
        let start = hbc
            .jump_table
            .byte_offset_to_instruction_index(function_index, handler.start)
            .ok_or_else(|| error("Unresolved exception range start"))?;
        let end = hbc
            .jump_table
            .byte_offset_to_instruction_index(function_index, handler.end)
            .ok_or_else(|| error("Unresolved exception range end"))?;
        let target = hbc
            .jump_table
            .byte_offset_to_instruction_index(function_index, handler.target)
            .ok_or_else(|| error("Unresolved exception handler target"))?;
        let lower = ast.expression_binary(
            SPAN,
            dispatch.identifier("__hbc_pc"),
            BinaryOperator::GreaterEqualThan,
            dispatch.number(start),
        );
        let upper = ast.expression_binary(
            SPAN,
            dispatch.identifier("__hbc_pc"),
            BinaryOperator::LessThan,
            dispatch.number(end),
        );
        let condition = ast.expression_logical(SPAN, lower, LogicalOperator::And, upper);
        let mut transition = ast.vec();
        transition.push(dispatch.assign("__hbc_error", dispatch.identifier("__hbc_caught")));
        transition.extend(dispatch.transition(target));
        catch_body.push(ast.statement_if(
            SPAN,
            condition,
            ast.statement_block(SPAN, transition),
            None,
        ));
    }
    catch_body.push(ast.statement_throw(SPAN, dispatch.identifier("__hbc_caught")));
    let binding = ast.binding_pattern(
        BindingPatternKind::BindingIdentifier(
            ast.alloc(ast.binding_identifier(SPAN, "__hbc_caught")),
        ),
        None::<TSTypeAnnotation>,
        false,
    );
    let handler = ast.catch_clause(
        SPAN,
        Some(ast.catch_parameter(SPAN, binding)),
        ast.block_statement(SPAN, catch_body),
    );
    let try_statement = ast.statement_try(SPAN, try_body, Some(handler), None::<BlockStatement>);
    statements.push(ast.statement_while(
        SPAN,
        ast.expression_boolean_literal(SPAN, true),
        ast.statement_block(SPAN, ast.vec1(try_statement)),
    ));
    Ok(statements)
}
