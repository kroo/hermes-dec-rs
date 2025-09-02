//! GlobalThis inlining optimization pass
//!
//! This pass inlines all direct uses of GetGlobalObject.

use super::OptimizationContext;
use crate::analysis::ssa_usage_tracker::UseStrategy;
use crate::analysis::value_tracker::{TrackedValue, ValueTracker};
use crate::cfg::ssa::types::DuplicatedSSAValue;

/// Perform globalThis inlining - inline all direct uses of GetGlobalObject
pub fn perform_global_this_inlining(ctx: &mut OptimizationContext) {
    // Skip if globalThis inlining is disabled
    if !ctx.inline_config.inline_global_this {
        return;
    }
    
    log::debug!("Performing globalThis inlining...");
    
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
            // Get the tracked value
            let tracked_value = value_tracker.get_value(&ssa_value);
            
            // Check if this is just the global object
            if matches!(tracked_value, TrackedValue::GlobalObject) {
                log::debug!("SSA value {} is GlobalObject", ssa_value);
                
                // Get all uses for this SSA value
                let uses = ctx.function_analysis.ssa.get_ssa_value_uses(&ssa_value);
                
                // Mark all uses for inlining
                for use_site in uses {
                    let dup_value = DuplicatedSSAValue::original(ssa_value.clone());
                    
                    if !ctx.plan.consumed_uses.contains_key(&dup_value) {
                        updates_to_apply.push((
                            dup_value,
                            use_site.clone(),
                            TrackedValue::GlobalObject,
                        ));
                    }
                }
            }
        }
    }
    
    // Check if we have updates before consuming the vector
    let has_updates = !updates_to_apply.is_empty();
    
    // Apply all the updates
    for (dup_value, use_site, _tracked_value) in updates_to_apply {
        log::debug!("Marking globalThis for inlining at {:?}", use_site);
        ctx.plan.use_strategies.insert(
            (dup_value.clone(), use_site.clone()),
            UseStrategy::InlineGlobalThis,
        );
        
        // Mark as consumed
        ctx.mark_use_consumed(dup_value, use_site.clone());
    }
    
    if has_updates {
        log::debug!("GlobalThis inlining complete");
    }
}