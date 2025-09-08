//! Constructor call inlining optimization pass
//!
//! This pass identifies and simplifies the CreateThis/Construct/SelectObject pattern
//! into a single `new Constructor(...)` expression.

use super::{OptimizationContext, OptimizationPass};
use crate::analysis::control_flow_plan::{ConstructorPattern, ControlFlowPlan};
use crate::analysis::FunctionAnalysis;
use crate::cfg::ssa::types::{DuplicatedSSAValue, DuplicationContext, RegisterDef, SSAValue};
use crate::decompiler::InlineConfig;
use crate::generated::unified_instructions::UnifiedInstruction;
use log::{debug, trace};
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Constructor call inlining optimization pass
pub struct ConstructorCallInliningPass<'a> {
    function_analysis: &'a FunctionAnalysis<'a>,
    inline_config: &'a InlineConfig,
    duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
}

impl<'a> ConstructorCallInliningPass<'a> {
    /// Create a new constructor call inlining pass
    pub fn new(
        function_analysis: &'a FunctionAnalysis<'a>,
        inline_config: &'a InlineConfig,
        duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
    ) -> Self {
        Self {
            function_analysis,
            inline_config,
            duplicated_blocks,
        }
    }
}

impl<'a> OptimizationPass for ConstructorCallInliningPass<'a> {
    fn name(&self) -> &'static str {
        "ConstructorCallInlining"
    }

    fn should_run(&self) -> bool {
        self.inline_config.inline_constructor_calls
    }

    fn run(&mut self, plan: &mut ControlFlowPlan) {
        if !self.inline_config.inline_constructor_calls {
            return;
        }

        perform_constructor_call_inlining(
            plan,
            self.function_analysis,
            self.inline_config,
            self.duplicated_blocks,
        );
    }
}

/// Perform constructor call inlining optimization
pub fn perform_constructor_call_inlining(
    plan: &mut ControlFlowPlan,
    function_analysis: &FunctionAnalysis,
    inline_config: &InlineConfig,
    duplicated_blocks: &HashMap<NodeIndex, Vec<DuplicationContext>>,
) {
    if !inline_config.inline_constructor_calls {
        return;
    }

    let mut ctx =
        OptimizationContext::new(plan, function_analysis, inline_config, duplicated_blocks);

    // Find all constructor patterns
    let patterns = find_constructor_patterns(&ctx);

    debug!("Found {} constructor patterns", patterns.len());

    // Process each pattern
    for (select_result, pattern) in patterns {
        process_constructor_pattern(&mut ctx, select_result, pattern);
    }
}

/// Information about instructions we're looking for
#[derive(Debug)]
struct InstructionInfo {
    pc: u32,
    instruction: UnifiedInstruction,
    block_id: NodeIndex,
}

/// Find all constructor patterns in the function
fn find_constructor_patterns(ctx: &OptimizationContext) -> Vec<(SSAValue, ConstructorPattern)> {
    let mut patterns = Vec::new();
    let cfg = &ctx.function_analysis.cfg;

    // Collect all instructions with their PCs and block IDs
    let mut all_instructions: Vec<InstructionInfo> = Vec::new();

    for block_index in cfg.graph().node_indices() {
        let block = &cfg.graph()[block_index];

        for instr in &block.instructions {
            // Get the PC from the instruction
            let pc = instr.instruction_index.value() as u32;

            // The instruction is already a UnifiedInstruction
            all_instructions.push(InstructionInfo {
                pc,
                instruction: instr.instruction.clone(),
                block_id: block_index,
            });
        }
    }

    // Look for SelectObject instructions as they're the final step
    debug!(
        "Scanning {} instructions for SelectObject",
        all_instructions.len()
    );
    for select_info in &all_instructions {
        if let UnifiedInstruction::SelectObject {
            operand_0: select_dst,
            operand_1: select_this,
            operand_2: select_return,
        } = &select_info.instruction
        {
            debug!(
                "Found SelectObject at PC {}: dst={}, this={}, return={}",
                select_info.pc, select_dst, select_this, select_return
            );
            // Try to find the corresponding CreateThis and Construct instructions
            if let Some(pattern) = find_pattern_for_select(
                ctx,
                &all_instructions,
                select_info,
                *select_dst,
                *select_this,
                *select_return,
            ) {
                patterns.push(pattern);
            }
        }
    }

    patterns
}

/// Try to find a constructor pattern for a SelectObject instruction
fn find_pattern_for_select(
    ctx: &OptimizationContext,
    all_instructions: &[InstructionInfo],
    select_info: &InstructionInfo,
    select_dst: u8,
    select_this: u8,
    select_return: u8,
) -> Option<(SSAValue, ConstructorPattern)> {
    let ssa = &ctx.function_analysis.ssa;

    // Find the SSA value that's defined by this SelectObject
    let select_def = RegisterDef {
        register: select_dst,
        block_id: select_info.block_id,
        instruction_idx: crate::hbc::InstructionIndex::new(select_info.pc as usize),
    };

    let select_result = ssa.ssa_values.get(&select_def)?.clone();

    trace!(
        "Checking SelectObject at PC {} defining SSA value {}",
        select_info.pc,
        select_result
    );

    // Now find the CreateThis and Construct instructions that define the operands
    // IMPORTANT: Only look within the same block to ensure the pattern is sequential
    let mut create_this_info = None;
    let mut construct_info = None;

    // Find the SSA values that SelectObject is actually using
    let select_this_def = find_definition_for_use_at(ctx, select_info.pc, select_this)?;
    let select_return_def = find_definition_for_use_at(ctx, select_info.pc, select_return)?;
    
    trace!(
        "SelectObject at PC {} uses this from PC {} and return from PC {}",
        select_info.pc,
        select_this_def.instruction_idx.value(),
        select_return_def.instruction_idx.value()
    );

    // Look for CreateThis that defines select_this and Construct that defines select_return
    // Only consider instructions in the same block as the SelectObject
    for instr_info in all_instructions.iter().filter(|i| i.block_id == select_info.block_id) {
        match &instr_info.instruction {
            UnifiedInstruction::CreateThis {
                operand_0: create_dst,
                operand_1: fn_reg,
                operand_2: _new_target,
            } => {
                trace!(
                    "CreateThis at PC {}: dst={}, fn_reg={}, looking for PC={}",
                    instr_info.pc,
                    create_dst,
                    fn_reg,
                    select_this_def.instruction_idx.value()
                );
                // Check if this is the specific CreateThis that defines the SSA value used by SelectObject
                if *create_dst == select_this && instr_info.pc as usize == select_this_def.instruction_idx.value() {
                    // This CreateThis defines the 'this' operand of SelectObject
                    create_this_info = Some((instr_info, *fn_reg));
                    debug!("Found CreateThis at PC {} for SelectObject", instr_info.pc);
                }
            }
            UnifiedInstruction::Construct {
                operand_0: construct_dst,
                operand_1: fn_reg,
                operand_2: arg_count,
            } => {
                trace!(
                    "Construct at PC {}: dst={}, fn_reg={}, looking for PC={}",
                    instr_info.pc,
                    construct_dst,
                    fn_reg,
                    select_return_def.instruction_idx.value()
                );
                // Check if this is the specific Construct that defines the SSA value used by SelectObject
                if *construct_dst == select_return && instr_info.pc as usize == select_return_def.instruction_idx.value() {
                    // This Construct defines the 'return' operand of SelectObject
                    construct_info = Some((instr_info, *fn_reg, *arg_count));
                    debug!("Found Construct at PC {} for SelectObject", instr_info.pc);
                }
            }
            _ => {}
        }
    }

    // If we found both instructions, we have a pattern
    // Note: We don't require the constructor registers to match between CreateThis and Construct
    if let (
        Some((create_info, create_prototype_reg)),
        Some((construct_info, construct_fn_reg, arg_count)),
    ) = (create_this_info, construct_info)
    {
        debug!(
            "Found constructor pattern: CreateThis@{} -> Construct@{} -> SelectObject@{}",
            create_info.pc, construct_info.pc, select_info.pc
        );

        // Get the SSA values for the intermediate results
        let create_this_def = RegisterDef {
            register: select_this,
            block_id: create_info.block_id,
            instruction_idx: crate::hbc::InstructionIndex::new(create_info.pc as usize),
        };
        let create_this_result = ssa
            .ssa_values
            .get(&create_this_def)
            .cloned()
            .unwrap_or_else(|| {
                // Fallback: create a dummy SSA value
                SSAValue::new(select_this, 0, create_this_def.clone())
            });

        let construct_def = RegisterDef {
            register: select_return,
            block_id: construct_info.block_id,
            instruction_idx: crate::hbc::InstructionIndex::new(construct_info.pc as usize),
        };
        let construct_result = ssa
            .ssa_values
            .get(&construct_def)
            .cloned()
            .unwrap_or_else(|| {
                // Fallback: create a dummy SSA value
                SSAValue::new(select_return, 0, construct_def.clone())
            });

        // Get the SSA value for the constructor function
        // We need to find what SSA value is used at the Construct instruction
        let constructor_ssa = find_ssa_value_at_use(ctx, construct_info.pc, construct_fn_reg)
            .unwrap_or_else(|| {
                // Fallback: create a dummy SSA value
                let dummy_def = RegisterDef {
                    register: construct_fn_reg,
                    block_id: construct_info.block_id,
                    instruction_idx: crate::hbc::InstructionIndex::zero(),
                };
                SSAValue::new(construct_fn_reg, 0, dummy_def)
            });

        // Get the SSA values for the arguments from call site analysis
        let mut arguments = Vec::new();

        // Look up the call site info for this Construct instruction
        let call_site_key = (
            construct_info.block_id,
            crate::hbc::InstructionIndex::new(construct_info.pc as usize),
        );
        
        let mut construct_this = None;
        
        if let Some(call_site_info) = ctx.plan.call_site_analysis.call_sites.get(&call_site_key) {
            debug!(
                "Found call site info for Construct at PC {}: arg_registers={:?}, arg_count={}",
                construct_info.pc, call_site_info.argument_registers, call_site_info.arg_count
            );
            // The first register in argument_registers is 'this' for Construct
            // The rest are the actual arguments
            for (i, &arg_reg) in call_site_info.argument_registers.iter().enumerate() {
                if i == 0 {
                    // This is the 'this' argument - track it so we can mark it as consumed
                    if let Some(this_ssa) = find_ssa_value_at_use(ctx, construct_info.pc, arg_reg) {
                        debug!("Found 'this' SSA value for Construct: {}", this_ssa);
                        construct_this = Some(this_ssa);
                    }
                    continue;
                }
                debug!("Looking for argument {} in register {}", i, arg_reg);
                if let Some(arg_ssa) = find_ssa_value_at_use(ctx, construct_info.pc, arg_reg) {
                    debug!("Found argument SSA value: {}", arg_ssa);
                    arguments.push(arg_ssa);
                } else {
                    debug!(
                        "Could not find SSA value for register {} at PC {}",
                        arg_reg, construct_info.pc
                    );
                }
            }
        } else {
            panic!("Missing call site analysis")
        }

        // Get the SSA value for the prototype used by CreateThis
        let create_this_prototype = find_ssa_value_at_use(ctx, create_info.pc, create_prototype_reg);
        
        if let Some(ref proto) = create_this_prototype {
            debug!("Found prototype SSA value for CreateThis: {}", proto);
        }

        let pattern = ConstructorPattern {
            constructor: constructor_ssa,
            arguments,
            construct_this,
            create_this_prototype,
            create_this_result,
            construct_result,
            select_object_pc: select_info.pc,
            construct_pc: construct_info.pc,
            constructor_reg: construct_fn_reg,
            arg_count,
        };

        return Some((select_result, pattern));
    }

    None
}

/// Find the definition that reaches a use at a specific PC for a register
fn find_definition_for_use_at(ctx: &OptimizationContext, pc: u32, register: u8) -> Option<RegisterDef> {
    let ssa = &ctx.function_analysis.ssa;
    let instruction_idx = crate::hbc::InstructionIndex::new(pc as usize);

    // Look through all uses to find one at this PC for this register
    for use_site in &ssa.uses {
        if use_site.register == register && use_site.instruction_idx == instruction_idx {
            // Find the definition that reaches this use
            return ssa.use_def_chains.get(use_site).cloned();
        }
    }

    // Fallback: look for the most recent definition of this register before this PC
    ssa.definitions
        .iter()
        .filter(|def| def.register == register && def.instruction_idx.value() < pc as usize)
        .max_by_key(|def| def.instruction_idx.value())
        .cloned()
}

/// Find the SSA value that's being used at a specific PC for a register
fn find_ssa_value_at_use(ctx: &OptimizationContext, pc: u32, register: u8) -> Option<SSAValue> {
    let ssa = &ctx.function_analysis.ssa;

    // Find the use at this PC
    let instruction_idx = crate::hbc::InstructionIndex::new(pc as usize);

    // Look through all uses to find one at this PC for this register
    for use_site in &ssa.uses {
        if use_site.register == register && use_site.instruction_idx == instruction_idx {
            // Find the definition that reaches this use
            // This is a simplified version - a full implementation would use proper reaching definitions
            return ssa
                .use_def_chains
                .get(use_site)
                .and_then(|def| ssa.ssa_values.get(def))
                .cloned();
        }
    }

    // Fallback: look for the most recent definition of this register before this PC
    ssa.definitions
        .iter()
        .filter(|def| def.register == register && def.instruction_idx.value() < pc as usize)
        .max_by_key(|def| def.instruction_idx.value())
        .and_then(|def| ssa.ssa_values.get(def))
        .cloned()
}

/// Process a constructor pattern and mark uses for inlining
fn process_constructor_pattern(
    ctx: &mut OptimizationContext,
    select_result: SSAValue,
    pattern: ConstructorPattern,
) {
    debug!(
        "Processing constructor pattern for SSA value {} at PC {}",
        select_result, pattern.select_object_pc
    );

    // Add the pattern to the control flow plan
    ctx.plan
        .constructor_patterns
        .insert(select_result.clone(), pattern.clone());

    // Mark the CreateThis and Construct instructions themselves as consumed
    // so they won't generate any statements
    let create_this_pc = pattern.create_this_result.def_site.instruction_idx;
    let construct_pc = crate::hbc::InstructionIndex::new(pattern.construct_pc as usize);
    
    ctx.plan.consumed_instructions.insert((pattern.create_this_result.def_site.block_id, create_this_pc));
    ctx.plan.consumed_instructions.insert((pattern.construct_result.def_site.block_id, construct_pc));
    
    debug!("Marked CreateThis at PC {:?} and Construct at PC {} as consumed instructions", 
           create_this_pc, pattern.construct_pc);

    // Mark all uses of the intermediate values as consumed
    // These intermediate values won't need to be declared or assigned

    // Mark CreateThis result uses as consumed
    mark_all_uses_consumed(ctx, &pattern.create_this_result);

    // Mark Construct result uses as consumed
    mark_all_uses_consumed(ctx, &pattern.construct_result);
    
    // Mark the 'this' value passed to Construct as consumed
    // This is often a copy of create_this_result (like var6_b = var1_a)
    if let Some(ref construct_this) = pattern.construct_this {
        mark_all_uses_consumed(ctx, construct_this);
        
        // Also mark the Mov instruction that creates this copy as consumed
        let mov_pc = construct_this.def_site.instruction_idx;
        let mov_block = construct_this.def_site.block_id;
        ctx.plan.consumed_instructions.insert((mov_block, mov_pc));
        debug!("Marked Mov instruction at PC {:?} as consumed (construct 'this' copy)", mov_pc);
    }
    
    // Mark the prototype value used by CreateThis as consumed
    // This is often something like var1_m = var9.prototype
    if let Some(ref create_this_prototype) = pattern.create_this_prototype {
        mark_all_uses_consumed(ctx, create_this_prototype);
    }

    // The SelectObject result itself keeps its normal declaration strategy
    // The AST generator will check if it's in constructor_patterns and generate `new` instead
}

/// Mark all uses of an SSA value as consumed
fn mark_all_uses_consumed(ctx: &mut OptimizationContext, ssa_value: &SSAValue) {
    // Get all uses of this SSA value
    let uses = ctx.function_analysis.ssa.get_ssa_value_uses(ssa_value);

    // Mark each use as consumed in the original (non-duplicated) context
    for use_site in uses {
        let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
        ctx.mark_use_consumed(dup_value, use_site.clone());
    }

    // Also handle uses in duplicated blocks
    for (_block_index, contexts) in ctx.duplicated_blocks {
        for context in contexts {
            // For duplicated blocks, we need to check if this SSA value appears there
            // This is a simplified approach - a full implementation would track remapped SSA values
            let dup_value = DuplicatedSSAValue {
                original: ssa_value.clone(),
                duplication_context: Some(context.clone()),
            };

            // Mark uses in this duplication context as consumed
            for use_site in ctx.function_analysis.ssa.get_ssa_value_uses(ssa_value) {
                ctx.mark_use_consumed(dup_value.clone(), use_site.clone());
            }
        }
    }
}
