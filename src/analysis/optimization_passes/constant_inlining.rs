//! Constant inlining optimization pass
//!
//! This pass inlines constant values based on the configured inlining strategy.

use super::OptimizationContext;
use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, UseStrategy};
use crate::analysis::value_tracker::{TrackedValue, ValueTracker};
use crate::cfg::ssa::types::DuplicatedSSAValue;
use std::collections::HashMap;

/// Perform aggressive constant inlining with PHI updates
pub fn perform_constant_inlining(ctx: &mut OptimizationContext) {
    // Skip if constant inlining is disabled
    if !ctx.inline_config.inline_constants && !ctx.inline_config.inline_all_constants {
        return;
    }

    log::debug!("Performing aggressive constant inlining...");

    // Collect updates to apply later (to avoid borrowing conflicts)
    let mut updates_to_apply = Vec::new();

    {
        // Create value tracker
        let value_tracker = ValueTracker::with_phi_deconstructions(
            &ctx.function_analysis.cfg,
            &ctx.function_analysis.ssa,
            ctx.function_analysis.hbc_file,
            &ctx.plan.phi_deconstructions,
        );

        // Determine which SSA values to check for inlining
        let ssa_values_to_check: Vec<_> = if ctx.plan.mandatory_inline.is_empty() {
            // Normal case: check all SSA values
            ctx.function_analysis
                .ssa
                .ssa_values
                .values()
                .cloned()
                .collect()
        } else {
            // Have mandatory inline values - only check those
            ctx.plan.mandatory_inline.iter().cloned().collect()
        };

        // Check each SSA value
        log::debug!(
            "Checking {} SSA values for constant inlining",
            ssa_values_to_check.len()
        );

        for ssa_value in ssa_values_to_check {
            // First check in the original context (no duplication)
            let tracked_value = value_tracker.get_value(&ssa_value);

            // Also check in all duplication contexts where this value might be used
            let mut context_values = HashMap::new();
            context_values.insert(None, tracked_value.clone());

            // Check all contexts where this value or its uses might appear
            for (_block_id, contexts) in ctx.duplicated_blocks {
                for context in contexts {
                    // Create a context-aware value tracker
                    let context_tracker = ValueTracker::with_phi_deconstructions(
                        &ctx.function_analysis.cfg,
                        &ctx.function_analysis.ssa,
                        ctx.function_analysis.hbc_file,
                        &ctx.plan.phi_deconstructions,
                    )
                    .with_context(Some(context.clone()));
                    let context_value = context_tracker.get_value(&ssa_value);
                    context_values.insert(Some(context.clone()), context_value);
                }
            }

            // Now process each context where the value is a constant
            for (context, tracked_value) in context_values {
                if let TrackedValue::Constant(ref const_val) = tracked_value {
                    log::debug!(
                        "SSA value {} is constant {:?} in context {:?}",
                        ssa_value,
                        const_val,
                        context
                    );

                    // Get uses for this SSA value
                    let uses = ctx.function_analysis.ssa.get_ssa_value_uses(&ssa_value);
                    log::debug!("SSA value {} has {} uses", ssa_value, uses.len());

                    // Count non-consumed uses in this specific context
                    let non_consumed_uses = uses
                        .iter()
                        .filter(|use_site| {
                            let use_in_context = if let Some(ref ctx_dup) = context {
                                ctx.duplicated_blocks
                                    .get(&use_site.block_id)
                                    .map(|contexts| contexts.contains(ctx_dup))
                                    .unwrap_or(false)
                            } else {
                                true
                            };

                            if !use_in_context {
                                return false;
                            }

                            let dup_value = if let Some(ref ctx_dup) = context {
                                DuplicatedSSAValue {
                                    original: ssa_value.clone(),
                                    duplication_context: Some(ctx_dup.clone()),
                                }
                            } else {
                                DuplicatedSSAValue::original(ssa_value.clone())
                            };
                            !ctx.plan.consumed_uses.contains_key(&dup_value)
                        })
                        .count();

                    log::debug!(
                        "SSA value {} has {} non-consumed uses in context {:?}",
                        ssa_value,
                        non_consumed_uses,
                        context
                    );

                    // Decide whether to inline
                    let should_inline = if ctx.inline_config.inline_all_constants {
                        true
                    } else if ctx.inline_config.inline_constants {
                        non_consumed_uses == 1
                    } else {
                        false
                    };

                    if should_inline {
                        log::debug!(
                            "Marking SSA value {} for constant inlining in context {:?} ({} non-consumed uses)",
                            ssa_value, context, non_consumed_uses
                        );

                        // Collect uses to mark for inlining
                        for use_site in &uses {
                            let use_in_context = if let Some(ref ctx_dup) = context {
                                ctx.duplicated_blocks
                                    .get(&use_site.block_id)
                                    .map(|contexts| contexts.contains(ctx_dup))
                                    .unwrap_or(false)
                            } else {
                                true
                            };

                            if use_in_context {
                                let dup_value = if let Some(ref ctx_dup) = context {
                                    DuplicatedSSAValue {
                                        original: ssa_value.clone(),
                                        duplication_context: Some(ctx_dup.clone()),
                                    }
                                } else {
                                    DuplicatedSSAValue::original(ssa_value.clone())
                                };

                                log::debug!(
                                    "Setting InlineValue strategy for {} at {:?} (context: {})",
                                    ssa_value,
                                    use_site,
                                    dup_value.context_description()
                                );

                                // Collect update to apply later
                                updates_to_apply.push((
                                    dup_value,
                                    (*use_site).clone(),
                                    const_val.clone(),
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    // Apply all the updates
    for (dup_value, use_site, const_val) in updates_to_apply {
        ctx.plan.use_strategies.insert(
            (dup_value.clone(), use_site.clone()),
            UseStrategy::InlineValue(const_val),
        );

        // Mark as consumed
        ctx.mark_use_consumed(dup_value, use_site);
    }

    // After inlining constants, update PHI result declaration strategies
    update_phi_result_declaration_strategies(ctx);
}

/// Update PHI result declaration strategies when the PHI result itself is fully inlined
fn update_phi_result_declaration_strategies(ctx: &mut OptimizationContext) {
    // Check each PHI function
    for phi_functions in ctx.function_analysis.ssa.phi_functions.values() {
        for phi in phi_functions {
            // Check if the PHI result itself has all its uses inlined
            let phi_uses = ctx.function_analysis.ssa.get_ssa_value_uses(&phi.result);
            let mut all_uses_inlined = true;

            for use_site in &phi_uses {
                let dup_value = DuplicatedSSAValue::original(phi.result.clone());
                let key = (dup_value, (*use_site).clone());

                match ctx.plan.use_strategies.get(&key) {
                    Some(UseStrategy::InlineValue(_)) => {
                        // This use is being inlined
                    }
                    _ => {
                        // This use is not being inlined
                        all_uses_inlined = false;
                        break;
                    }
                }
            }

            // If all uses of the PHI result are inlined, update its declaration strategy
            if all_uses_inlined && !phi_uses.is_empty() {
                let dup_phi_result = DuplicatedSSAValue::original(phi.result.clone());

                // Check if this PHI result has a declaration strategy
                if let Some(current_strategy) = ctx.plan.declaration_strategies.get(&dup_phi_result)
                {
                    if !matches!(current_strategy, DeclarationStrategy::Skip) {
                        log::debug!(
                            "Updating PHI result {} declaration to Skip (all uses inlined)",
                            phi.result
                        );
                        ctx.plan
                            .declaration_strategies
                            .insert(dup_phi_result.clone(), DeclarationStrategy::Skip);

                        // Also mark PHI operands as Skip since they're no longer needed
                        for (_, operand) in &phi.operands {
                            let dup_operand = DuplicatedSSAValue::original(operand.clone());
                            if let Some(op_strategy) =
                                ctx.plan.declaration_strategies.get(&dup_operand)
                            {
                                if matches!(op_strategy, DeclarationStrategy::AssignOnly) {
                                    log::debug!(
                                        "Updating PHI operand {} to Skip (PHI result eliminated)",
                                        operand
                                    );
                                    ctx.plan
                                        .declaration_strategies
                                        .insert(dup_operand, DeclarationStrategy::Skip);
                                }
                            }
                        }

                        // Also check PHI replacements in duplicated contexts
                        for ((block_id, _context), phi_decon) in &ctx.plan.phi_deconstructions {
                            if *block_id == phi.result.def_site.block_id {
                                // Found a PHI deconstruction for this PHI
                                for (original, replacement) in &phi_decon.replacements {
                                    if *original == phi.result {
                                        // This replacement is for our eliminated PHI
                                        let dup_replacement =
                                            DuplicatedSSAValue::original(replacement.clone());
                                        if let Some(rep_strategy) =
                                            ctx.plan.declaration_strategies.get(&dup_replacement)
                                        {
                                            if matches!(
                                                rep_strategy,
                                                DeclarationStrategy::AssignOnly
                                            ) {
                                                log::debug!(
                                                    "Updating PHI replacement {} to Skip (PHI result eliminated)",
                                                    replacement
                                                );
                                                ctx.plan.declaration_strategies.insert(
                                                    dup_replacement,
                                                    DeclarationStrategy::Skip,
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
