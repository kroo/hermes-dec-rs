//! GlobalThis inlining optimization pass
//!
//! This pass inlines all direct uses of GetGlobalObject.

use super::{OptimizationContext, OptimizationPass};
use crate::analysis::control_flow_plan::ControlFlowPlan;
use crate::analysis::ssa_usage_tracker::UseStrategy;
use crate::analysis::value_tracker::{TrackedValue, ValueTracker};
use crate::analysis::FunctionAnalysis;
use crate::cfg::ssa::types::{DuplicatedSSAValue, DuplicationContext};
use crate::decompiler::InlineConfig;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// GlobalThis inlining optimization pass
pub struct GlobalThisInliningPass<'a> {
    function_analysis: &'a FunctionAnalysis<'a>,
    inline_config: &'a InlineConfig,
    duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
}

impl<'a> GlobalThisInliningPass<'a> {
    /// Create a new globalThis inlining pass
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

impl<'a> OptimizationPass for GlobalThisInliningPass<'a> {
    fn name(&self) -> &'static str {
        "GlobalThisInlining"
    }

    fn should_run(&self) -> bool {
        self.inline_config.inline_global_this
    }

    fn run(&mut self, plan: &mut ControlFlowPlan) {
        // Skip if globalThis inlining is disabled
        if !self.inline_config.inline_global_this {
            return;
        }

        // Create optimization context
        let mut ctx = OptimizationContext::new(
            plan,
            self.function_analysis,
            self.inline_config,
            self.duplicated_blocks,
        );

        perform_global_this_inlining(&mut ctx);
    }
}

/// Perform globalThis inlining - inline all direct uses of GetGlobalObject
pub fn perform_global_this_inlining(ctx: &mut OptimizationContext) {
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
