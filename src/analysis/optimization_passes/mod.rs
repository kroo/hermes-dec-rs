//! Optimization passes for the decompiler
//!
//! This module contains various optimization passes that can be applied
//! during decompilation to improve the quality of the output code.

mod call_simplification;
mod constant_inlining;
mod constructor_call_inlining;
mod global_this_inlining;
mod parameter_inlining;
mod property_access_inlining;

// Export the pass implementations
pub use call_simplification::{analyze_call_simplification, CallSimplificationPass};
pub use constant_inlining::{perform_constant_inlining, ConstantInliningPass};
pub use constructor_call_inlining::{
    perform_constructor_call_inlining, ConstructorCallInliningPass,
};
pub use global_this_inlining::{perform_global_this_inlining, GlobalThisInliningPass};
pub use parameter_inlining::{perform_parameter_inlining, ParameterInliningPass};
pub use property_access_inlining::{perform_property_access_inlining, PropertyAccessInliningPass};

use crate::analysis::control_flow_plan::ControlFlowPlan;
use crate::analysis::FunctionAnalysis;
use crate::cfg::ssa::types::{DuplicatedSSAValue, DuplicationContext, RegisterUse};
use crate::decompiler::InlineConfig;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Trait for optimization passes
pub trait OptimizationPass {
    /// Get the name of this optimization pass
    fn name(&self) -> &'static str;

    /// Run the optimization pass on the control flow plan
    fn run(&mut self, plan: &mut ControlFlowPlan);

    /// Check if this pass should run
    fn should_run(&self) -> bool {
        true
    }
}

/// Context for optimization passes
pub struct OptimizationContext<'a> {
    pub plan: &'a mut ControlFlowPlan,
    pub function_analysis: &'a FunctionAnalysis<'a>,
    pub inline_config: &'a InlineConfig,
    pub duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
    /// Track uses marked as consumed during this optimization pass
    pub consumed_uses: HashMap<DuplicatedSSAValue, HashSet<RegisterUse>>,
}

impl<'a> OptimizationContext<'a> {
    /// Create a new optimization context
    pub fn new(
        plan: &'a mut ControlFlowPlan,
        function_analysis: &'a FunctionAnalysis<'a>,
        inline_config: &'a InlineConfig,
        duplicated_blocks: &'a HashMap<NodeIndex, Vec<DuplicationContext>>,
    ) -> Self {
        Self {
            plan,
            function_analysis,
            inline_config,
            duplicated_blocks,
            consumed_uses: HashMap::new(),
        }
    }

    /// Mark a use as consumed
    pub fn mark_use_consumed(&mut self, dup_value: DuplicatedSSAValue, use_site: RegisterUse) {
        // Mark in the plan
        self.plan
            .mark_use_consumed(dup_value.clone(), use_site.clone());

        // Also track locally for cascading elimination
        self.consumed_uses
            .entry(dup_value)
            .or_insert_with(HashSet::new)
            .insert(use_site);
    }
}
