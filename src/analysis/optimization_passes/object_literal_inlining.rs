//! Object literal inlining optimization pass
//!
//! This pass identifies objects that can be safely inlined as object literals
//! and marks them for inlining during code generation.

use super::{OptimizationContext, OptimizationPass};
use crate::analysis::control_flow_plan::ControlFlowPlan;
use crate::analysis::ssa_usage_tracker::{EmitPosition, UseStrategy};
use crate::analysis::value_tracker::ValueTracker;
use crate::analysis::value_tracking::{
    EscapeAnalyzer, LiteralReconstructor, PatternDetector, ReconstructionResult,
    TrackedValue,
};
use crate::analysis::FunctionAnalysis;
use crate::cfg::ssa::types::{DuplicatedSSAValue, DuplicationContext};
use crate::decompiler::InlineConfig;
use crate::hbc::InstructionIndex;
use log::debug;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNodeReferences;
use std::collections::HashMap;

/// Object literal inlining optimization pass
pub struct ObjectLiteralInliningPass<'a> {
    function_analysis: &'a FunctionAnalysis<'a>,
    inline_config: &'a InlineConfig,
    duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
}

impl<'a> ObjectLiteralInliningPass<'a> {
    /// Create a new object literal inlining pass
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

impl<'a> OptimizationPass for ObjectLiteralInliningPass<'a> {
    fn name(&self) -> &'static str {
        "ObjectLiteralInlining"
    }

    fn should_run(&self) -> bool {
        self.inline_config.inline_object_literals
    }

    fn run(&mut self, plan: &mut ControlFlowPlan) {
        // Create optimization context
        let mut ctx = OptimizationContext::new(
            plan,
            self.function_analysis,
            self.inline_config,
            self.duplicated_blocks,
        );

        perform_object_literal_inlining(&mut ctx);
    }
}

/// Main implementation of object literal inlining
pub fn perform_object_literal_inlining(ctx: &mut OptimizationContext) {
    if !ctx.inline_config.inline_object_literals {
        return;
    }

    debug!("Running object literal inlining optimization");

    // Create value tracker
    let value_tracker = ValueTracker::with_phi_deconstructions(
        &ctx.function_analysis.cfg,
        &ctx.function_analysis.ssa,
        ctx.function_analysis.hbc_file,
        &ctx.plan.phi_deconstructions,
    );

    // Create escape analyzer
    let escape_analyzer = EscapeAnalyzer::new(&ctx.function_analysis.ssa, &ctx.function_analysis.cfg);

    // Collect all objects that can be inlined
    let mut objects_to_inline = Vec::new();

    // Iterate through all SSA values
    for (def_site, ssa_value) in &ctx.function_analysis.ssa.ssa_values {
        // Get the tracked value
        let tracked_value = value_tracker.get_value(&ssa_value);

        // Check if it's a mutable object
        if let TrackedValue::MutableObject {
            creation_pc,
            base_type,
            mutations,
            ..
        } = &tracked_value
        {
            // Skip if no mutations (empty object)
            if mutations.is_empty() {
                continue;
            }

            // Check if the object is safe to inline
            let escape_result = escape_analyzer.analyze_object_escape(def_site);
            if !escape_result.safe_to_inline {
                debug!(
                    "Object {} at PC {} is not safe to inline (escapes: {}, reasons: {:?})",
                    ssa_value, creation_pc, escape_result.escapes, escape_result.reasons
                );
                continue;
            }
            
            if escape_result.escapes {
                debug!(
                    "Object {} at PC {} escapes but is safe to inline (reasons: {:?})",
                    ssa_value, creation_pc, escape_result.reasons
                );
            }

            // Check if the pattern is safe to inline
            let pattern = PatternDetector::detect_pattern(base_type, mutations, *creation_pc as u32);
            if !PatternDetector::is_safe_to_inline(&pattern) {
                debug!("Object {} at PC {} has unsafe pattern", ssa_value, creation_pc);
                continue;
            }

            // Try to reconstruct the object
            let reconstructor = LiteralReconstructor::new(&ctx.function_analysis.ssa, &ctx.function_analysis.cfg);
            let reconstruction = reconstructor.reconstruct(&tracked_value, ssa_value);

            match reconstruction {
                ReconstructionResult::Literal(js_code) => {
                    debug!("Object {} at PC {} can be inlined as literal: {}", ssa_value, creation_pc, js_code);
                    
                    // Determine the safe inlining position
                    let emit_position = determine_emit_position(
                        ctx,
                        ssa_value,
                        mutations,
                        *creation_pc,
                    );
                    
                    objects_to_inline.push((ssa_value.clone(), tracked_value.clone(), emit_position));
                }
                ReconstructionResult::Partial { .. } => {
                    debug!("Object {} at PC {} has partial reconstruction", ssa_value, creation_pc);
                    // For now, skip partial reconstructions
                }
                ReconstructionResult::CannotReconstruct(reason) => {
                    debug!("Object {} at PC {} cannot be reconstructed: {}", ssa_value, creation_pc, reason);
                }
            }
        }
    }

    // Apply the inlining strategies
    for (ssa_value, tracked_value, emit_position) in objects_to_inline {
        apply_object_literal_inlining(ctx, &ssa_value, tracked_value, emit_position);
    }
}

/// Determine where to emit the object literal declaration
fn determine_emit_position(
    ctx: &OptimizationContext,
    ssa_value: &crate::cfg::ssa::types::SSAValue,
    mutations: &[crate::analysis::value_tracking::ObjectMutation],
    creation_pc: usize,
) -> EmitPosition {
    use crate::analysis::value_tracking::MutationKind;
    // Find the last mutation before the first non-mutation use
    let uses = ctx.function_analysis.ssa.get_ssa_value_uses(ssa_value);
    
    // Find mutation instruction indices
    let mutation_pcs: Vec<usize> = mutations.iter().map(|m| m.pc).collect();
    
    // Find the first use that's not a mutation
    let mut first_non_mutation_use = None;
    for use_site in &uses {
        let use_pc = use_site.instruction_idx.value();
        if !mutation_pcs.contains(&use_pc) {
            if first_non_mutation_use.is_none() || use_pc < first_non_mutation_use.unwrap() {
                first_non_mutation_use = Some(use_pc);
            }
        }
    }
    
    // Find the last mutation before the first non-mutation use
    let last_safe_mutation = if let Some(first_use) = first_non_mutation_use {
        mutations.iter()
            .filter(|m| m.pc < first_use)
            .max_by_key(|m| m.pc)
    } else {
        // No non-mutation uses, can use the last mutation
        mutations.iter().max_by_key(|m| m.pc)
    };
    
    // Check if all mutations use values defined before the creation point
    let all_mutations_safe_at_creation = mutations.iter().all(|mutation| {
        // Check if the mutation value (if any) is defined before creation
        match &mutation.kind {
            MutationKind::PropertySet { value, .. } |
            MutationKind::PropertyDefine { value, .. } |
            MutationKind::ArraySet { value, .. } => {
                // Check if the value depends on SSA values defined after creation
                is_value_safe_at_creation(value.as_ref(), creation_pc, ctx)
            }
            _ => true, // Other mutation types don't have values to check
        }
    });
    
    if all_mutations_safe_at_creation {
        // All mutations use values available at creation time
        EmitPosition::AtCreation
    } else if let Some(last_mutation) = last_safe_mutation {
        // Emit at the last safe mutation point
        // Find the block and instruction index for this mutation
        if let Some(block_id) = find_block_for_pc(ctx, last_mutation.pc) {
            EmitPosition::AtMutation {
                block_id,
                instruction_idx: InstructionIndex(last_mutation.pc),
            }
        } else {
            // Fallback to creation if we can't find the block
            EmitPosition::AtCreation
        }
    } else {
        // No safe mutation point found, emit at creation
        EmitPosition::AtCreation
    }
}

/// Check if a tracked value is safe to use at a given creation PC
fn is_value_safe_at_creation(
    value: &TrackedValue,
    creation_pc: usize,
    ctx: &OptimizationContext,
) -> bool {
    use crate::analysis::value_tracking::TrackedValue;
    
    match value {
        // Constants are always safe
        TrackedValue::Constant(_) => true,
        // Global object is always safe
        TrackedValue::GlobalObject => true,
        // Parameters depend on SSA values
        TrackedValue::Parameter { ssa_value, .. } => {
            ssa_value.def_site.instruction_idx.value() < creation_pc
        }
        // Phi nodes depend on their SSA value
        TrackedValue::Phi { ssa_value } => {
            ssa_value.def_site.instruction_idx.value() < creation_pc
        }
        // Property access depends on the object
        TrackedValue::PropertyAccess { object, .. } => {
            is_value_safe_at_creation(object.as_ref(), creation_pc, ctx)
        }
        // Mutable objects depend on their creation PC and mutations
        TrackedValue::MutableObject { creation_pc: obj_pc, mutations, .. } => {
            // Object must be created before
            if *obj_pc >= creation_pc {
                return false;
            }
            // All mutations must also be safe
            mutations.iter().all(|m| {
                use crate::analysis::value_tracking::MutationKind;
                match &m.kind {
                    MutationKind::PropertySet { value, .. } |
                    MutationKind::PropertyDefine { value, .. } |
                    MutationKind::ArraySet { value, .. } => {
                        is_value_safe_at_creation(value.as_ref(), creation_pc, ctx)
                    }
                    _ => true,
                }
            })
        }
        // Unknown values are not safe
        TrackedValue::Unknown => false,
        // Merged objects are not safe (too complex to analyze)
        TrackedValue::MergedObject { .. } => false,
    }
}

/// Find the block containing a given PC
fn find_block_for_pc(ctx: &OptimizationContext, pc: usize) -> Option<NodeIndex> {
    for (block_id, block) in ctx.function_analysis.cfg.graph().node_references() {
        for instr in block.instructions() {
            if instr.instruction_index.value() == pc {
                return Some(block_id);
            }
        }
    }
    None
}

/// Apply object literal inlining for a specific SSA value
fn apply_object_literal_inlining(
    ctx: &mut OptimizationContext,
    ssa_value: &crate::cfg::ssa::types::SSAValue,
    tracked_value: TrackedValue,
    emit_position: EmitPosition,
) {
    debug!(
        "Applying object literal inlining for {} at position {:?}",
        ssa_value, emit_position
    );
    
    // Mark the creation instruction with the DeclareObjectLiteral strategy
    let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
    
    // Find the creation use site
    let creation_use = crate::cfg::ssa::types::RegisterUse {
        block_id: ssa_value.def_site.block_id,
        instruction_idx: ssa_value.def_site.instruction_idx,
        register: ssa_value.def_site.register,
    };
    
    // Set the declaration strategy for the object creation
    ctx.plan.use_strategies.insert(
        (dup_value.clone(), creation_use.clone()),
        UseStrategy::DeclareObjectLiteral {
            tracked_value: tracked_value.clone(),
            emit_position: emit_position.clone(),
        },
    );
    
    // Mark all mutation instructions as consumed
    if let TrackedValue::MutableObject { mutations, .. } = &tracked_value {
        for mutation in mutations {
            // Find the block containing this mutation
            if let Some(block_id) = find_block_for_pc(ctx, mutation.pc) {
                let _mutation_use = crate::cfg::ssa::types::RegisterUse {
                    block_id,
                    instruction_idx: InstructionIndex(mutation.pc),
                    register: 0, // Mutations don't have a specific register
                };
                
                // Mark this instruction as consumed (skip during generation)
                ctx.plan.consumed_instructions.insert((block_id, InstructionIndex(mutation.pc)));
                
                debug!("Marked mutation at PC {} as consumed", mutation.pc);
            }
        }
    }
    
    // If emitting at creation, mark the creation as consumed
    if matches!(emit_position, EmitPosition::AtCreation) {
        ctx.mark_use_consumed(dup_value.clone(), creation_use);
    }
    
    // Mark all non-mutation uses of the object as using the variable
    let uses = ctx.function_analysis.ssa.get_ssa_value_uses(ssa_value);
    for use_site in uses {
        // Skip if this is a mutation site (already handled)
        if let TrackedValue::MutableObject { mutations, .. } = &tracked_value {
            if mutations.iter().any(|m| m.pc == use_site.instruction_idx.value()) {
                continue;
            }
        }
        
        // This is a regular use - it should reference the variable
        let key = (dup_value.clone(), use_site.clone());
        if !ctx.plan.use_strategies.contains_key(&key) {
            ctx.plan.use_strategies.insert(
                key,
                UseStrategy::UseVariable,
            );
        }
    }
}