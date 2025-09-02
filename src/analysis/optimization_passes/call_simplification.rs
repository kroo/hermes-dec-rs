//! Call simplification optimization pass
//!
//! This pass simplifies call patterns like fn.call(undefined, ...) to fn(...).

use super::OptimizationContext;
use crate::analysis::ssa_usage_tracker::UseStrategy;
use crate::analysis::value_tracker::{ConstantValue, TrackedValue, ValueTracker};
use crate::cfg::ssa::types::{DuplicatedSSAValue, RegisterUse};

/// Analyze call sites and mark 'this' arguments for simplification when appropriate
pub fn analyze_call_simplification(ctx: &mut OptimizationContext) {
    // Skip if call simplification is disabled
    if !ctx.inline_config.simplify_calls && !ctx.inline_config.unsafe_simplify_calls {
        return;
    }

    log::debug!(
        "Analyzing {} call sites for simplification",
        ctx.plan.call_site_analysis.call_sites.len()
    );

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

        // Iterate through all call sites
        for ((block_id, pc), call_site_info) in &ctx.plan.call_site_analysis.call_sites {
            // Skip constructor calls - they have different semantics
            if call_site_info.is_constructor {
                continue;
            }

            // Get the 'this' argument register (first argument for calls)
            if let Some(&this_reg) = call_site_info.argument_registers.first() {
                // Find the SSA value for the 'this' register at this use site
                let use_site = RegisterUse {
                    register: this_reg,
                    block_id: *block_id,
                    instruction_idx: *pc,
                };

                // Look up the SSA value from the use-def chain
                if let Some(def_site) = ctx.function_analysis.ssa.use_def_chains.get(&use_site) {
                    // Find the SSA value with this def site
                    if let Some(ssa_value) = ctx
                        .function_analysis
                        .ssa
                        .ssa_values
                        .values()
                        .find(|v| &v.def_site == def_site)
                    {
                        // Check if the 'this' value is undefined (safe simplification)
                        let tracked_value = value_tracker.get_value(ssa_value);
                        let is_undefined = matches!(
                            tracked_value,
                            TrackedValue::Constant(ConstantValue::Undefined)
                        );

                        let should_simplify = if is_undefined && ctx.inline_config.simplify_calls {
                            // Safe simplification: fn.call(undefined, ...) -> fn(...)
                            log::debug!(
                                "Marking call at block {}, PC {} for safe simplification (undefined 'this')",
                                block_id.index(),
                                pc.0
                            );
                            true
                        } else if ctx.inline_config.unsafe_simplify_calls {
                            // Unsafe simplification: obj.fn.call(obj, ...) -> obj.fn(...)
                            // TODO: Implement proper safety checks as outlined in the safety checklist:
                            // 1. Verify the callee is an ordinary ECMAScript function (not a Proxy, exotic callable, etc.)
                            // 2. Ensure Function.prototype.call hasn't been modified
                            // 3. Ensure the function object doesn't have its own "call" property
                            // 4. Verify no accessor/reactivity concerns on the object or function property
                            // 5. Check this behavior matches (strict mode, bound functions, etc.)
                            // 6. Ensure it's a normal method call site (no optional chaining)
                            //
                            // For now, we unsafely simplify all non-undefined 'this' values
                            // This is NOT semantics-preserving in all cases!
                            if !is_undefined {
                                log::debug!(
                                    "Marking call at block {}, PC {} for UNSAFE simplification (non-undefined 'this')",
                                    block_id.index(),
                                    pc.0
                                );
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if should_simplify {
                            // Check if this use is in a duplicated block
                            if let Some(contexts) = ctx.duplicated_blocks.get(block_id) {
                                // This use is in a duplicated block
                                for context in contexts {
                                    let dup_value = DuplicatedSSAValue {
                                        original: ssa_value.clone(),
                                        duplication_context: Some(context.clone()),
                                    };

                                    // Collect update to apply later
                                    // is_method_call is true if 'this' is not undefined
                                    updates_to_apply.push((
                                        dup_value,
                                        use_site.clone(),
                                        UseStrategy::SimplifyCall {
                                            is_method_call: !is_undefined,
                                        },
                                    ));
                                }
                            } else {
                                // Normal use, not in a duplicated block
                                let dup_value = DuplicatedSSAValue::original(ssa_value.clone());

                                // Collect update to apply later
                                updates_to_apply.push((
                                    dup_value,
                                    use_site.clone(),
                                    UseStrategy::SimplifyCall {
                                        is_method_call: !is_undefined,
                                    },
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    // Apply all updates
    for (dup_value, use_site, strategy) in updates_to_apply {
        ctx.plan.set_use_strategy(dup_value, use_site, strategy);
    }
}
