//! Property access inlining optimization pass
//!
//! This pass inlines property access chains based on the configured strategy.

use super::OptimizationContext;
use crate::analysis::ssa_usage_tracker::UseStrategy;
use crate::analysis::value_tracker::{TrackedValue, ValueTracker};
use crate::cfg::ssa::types::DuplicatedSSAValue;
use std::collections::HashMap;

/// Perform property access inlining based on options
pub fn perform_property_access_inlining(ctx: &mut OptimizationContext) {
    // Skip if property access inlining is disabled
    if !ctx.inline_config.inline_property_access && !ctx.inline_config.inline_all_property_access {
        return;
    }
    
    log::debug!("Performing property access inlining...");
    
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
        
        // Check each SSA value
        for (_def_site, ssa_value) in &ctx.function_analysis.ssa.ssa_values {
            // Get the tracked value for different contexts
            let mut context_values = HashMap::new();
            
            // First get the value in the original context
            let tracked_value = value_tracker.get_value(&ssa_value);
            
            // Check if this is a property access
            if matches!(tracked_value, TrackedValue::PropertyAccess { .. }) {
                context_values.insert(None, tracked_value.clone());
                
                // Check all contexts where this value might appear
                for (_block_id, contexts) in ctx.duplicated_blocks {
                    for context in contexts {
                        let context_tracker = ValueTracker::with_phi_deconstructions(
                            &ctx.function_analysis.cfg,
                            &ctx.function_analysis.ssa,
                            ctx.function_analysis.hbc_file,
                            &ctx.plan.phi_deconstructions,
                        )
                        .with_context(Some(context.clone()));
                        let context_value = context_tracker.get_value(&ssa_value);
                        if matches!(context_value, TrackedValue::PropertyAccess { .. }) {
                            context_values.insert(Some(context.clone()), context_value);
                        }
                    }
                }
                
                // Process each context where the value is a property access
                for (context, tracked_value) in context_values {
                    if let TrackedValue::PropertyAccess { .. } = tracked_value {
                        log::debug!(
                            "SSA value {} is property access in context {:?}",
                            ssa_value, context
                        );
                        
                        // Get uses for this SSA value
                        let uses = ctx.function_analysis.ssa.get_ssa_value_uses(&ssa_value);
                        log::debug!("SSA value {} has {} uses", ssa_value, uses.len());
                        
                        // Count non-consumed uses in this specific context
                        let non_consumed_uses = uses.iter()
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
                            ssa_value, non_consumed_uses, context
                        );
                        
                        // Decide whether to inline
                        let should_inline = if ctx.inline_config.inline_all_property_access {
                            true
                        } else if ctx.inline_config.inline_property_access {
                            non_consumed_uses == 1
                        } else {
                            false
                        };
                        
                        if should_inline {
                            log::debug!(
                                "Marking property access SSA value {} for inlining in context {:?} ({} non-consumed uses)",
                                ssa_value, context, non_consumed_uses
                            );
                            
                            // Collect uses to mark for inlining
                            for use_site in uses {
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
                                    
                                    if !ctx.plan.consumed_uses.contains_key(&dup_value) {
                                        updates_to_apply.push((
                                            dup_value,
                                            use_site.clone(),
                                            tracked_value.clone(),
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    
    // Apply all the updates
    for (dup_value, use_site, tracked_value) in updates_to_apply {
        ctx.plan.use_strategies.insert(
            (dup_value.clone(), use_site.clone()),
            UseStrategy::InlinePropertyAccess(tracked_value),
        );
        
        // Mark as consumed
        ctx.mark_use_consumed(dup_value, use_site.clone());
    }
    
}