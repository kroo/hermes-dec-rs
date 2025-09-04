//! Parameter Inlining Optimization Pass
//!
//! This optimization pass replaces references to parameter variables with
//! direct references to the original parameter names (this, arg0, arg1, etc.)
//! when the tracked value is a Parameter.

use crate::analysis::control_flow_plan::ControlFlowPlan;
use crate::analysis::optimization_passes::{OptimizationContext, OptimizationPass};
use crate::analysis::ssa_usage_tracker::UseStrategy;
use crate::analysis::FunctionAnalysis;
use crate::analysis::TrackedValue;
use crate::cfg::ssa::types::DuplicationContext;
use crate::decompiler::InlineConfig;
use log::debug;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Parameter inlining optimization pass
pub struct ParameterInliningPass<'a> {
    function_analysis: &'a FunctionAnalysis<'a>,
    inline_config: &'a InlineConfig,
    duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
}

impl<'a> ParameterInliningPass<'a> {
    /// Create a new parameter inlining pass
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

impl<'a> OptimizationPass for ParameterInliningPass<'a> {
    fn name(&self) -> &'static str {
        "ParameterInlining"
    }

    fn should_run(&self) -> bool {
        self.inline_config.inline_parameters && self.function_analysis.function.param_count > 0
    }

    fn run(&mut self, plan: &mut ControlFlowPlan) {
        // Skip if parameter inlining is disabled
        if !self.inline_config.inline_parameters {
            return;
        }

        // Skip if function has no parameters
        if self.function_analysis.function.param_count == 0 {
            return;
        }

        // Create optimization context
        let mut ctx = OptimizationContext::new(
            plan,
            self.function_analysis,
            self.inline_config,
            self.duplicated_blocks,
        );

        perform_parameter_inlining(&mut ctx);
    }
}

/// Perform parameter inlining optimization
pub fn perform_parameter_inlining(ctx: &mut OptimizationContext) {
    debug!("Performing parameter inlining...");

    let mut updates = Vec::new();
    let mut inlined_count = 0;

    // Check each use strategy
    let strategies: Vec<_> = ctx.plan.use_strategies.clone().into_iter().collect();
    for ((dup_ssa, use_site), strategy) in strategies {
        // Only process UseVariable strategies
        if !matches!(strategy, UseStrategy::UseVariable) {
            continue;
        }

        // Get the tracked value for this SSA value
        let tracked_value = ctx
            .function_analysis
            .value_tracker()
            .get_value(&dup_ssa.original);

        debug!(
            "Checking SSA {} at {:?}: tracked value = {:?}",
            dup_ssa.original, use_site, tracked_value
        );

        // Check if it's a parameter
        if let TrackedValue::Parameter {
            index,
            ssa_value: _,
        } = tracked_value
        {
            debug!(
                "Inlining parameter {} at use {:?} -> param_index {}",
                dup_ssa.original, use_site, index
            );

            updates.push((
                dup_ssa.clone(),
                use_site.clone(),
                UseStrategy::InlineParameter {
                    param_index: index as u8,
                },
            ));
            inlined_count += 1;
        }
    }

    // Apply updates and mark uses as consumed
    for (dup_ssa, use_site, new_strategy) in updates {
        ctx.plan
            .use_strategies
            .insert((dup_ssa.clone(), use_site.clone()), new_strategy);

        // Mark this use as consumed since we're inlining it
        ctx.mark_use_consumed(dup_ssa, use_site);
    }

    if inlined_count > 0 {
        debug!(
            "Parameter inlining pass inlined {} parameter references",
            inlined_count
        );
    }
}
