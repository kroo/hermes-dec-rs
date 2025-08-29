//! SSA Usage Tracker
//!
//! Tracks which uses of SSA values have been consumed (inlined) during AST generation,
//! enabling smart decisions about variable declaration and constant folding.

use crate::analysis::{ConstantValue, FunctionAnalysis, TrackedValue};
use crate::cfg::ssa::types::{
    DuplicatedSSAValue, DuplicationContext, RegisterUse, SSAAnalysis, SSAValue,
};
use log::{debug, trace};
use petgraph::algo::dominators::Dominators;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Tracks the usage status of SSA values during AST generation
pub struct SSAUsageTracker<'a> {
    /// The function analysis containing SSA and CFG information
    function_analysis: &'a FunctionAnalysis<'a>,

    /// Track which specific uses of duplicated SSA values have been consumed (inlined)
    consumed_uses: HashMap<DuplicatedSSAValue, HashSet<RegisterUse>>,

    /// Cache of constant values for duplicated SSA values
    /// This is populated when we first analyze an SSA value
    constant_cache: HashMap<DuplicatedSSAValue, ConstantValue>,
    
    /// Cache of SSA values that are implicit arguments (pre-computed for performance)
    implicit_arguments_cache: Option<HashSet<SSAValue>>,
}

/// The usage status of an SSA value
#[derive(Debug, Clone, PartialEq)]
pub enum UsageStatus {
    /// All uses have been consumed/inlined
    FullyConsumed,

    /// Some uses consumed, others remain
    PartiallyConsumed {
        consumed_count: usize,
        remaining_count: usize,
    },

    /// No uses have been consumed yet
    Unconsumed { total_uses: usize },
}

/// Strategy for handling an SSA value at its definition site
#[derive(Debug, Clone)]
pub enum DeclarationStrategy {
    /// Skip - all uses have been inlined/eliminated
    Skip,

    /// Execute for side effects only - no assignment or declaration
    /// Used for instructions with side effects whose result is not used
    SideEffectOnly,

    /// Declare at dominator point (for PHI nodes with no single dominating def)
    DeclareAtDominator {
        dominator_block: petgraph::graph::NodeIndex,
        kind: VariableKind,
    },

    /// Declare and initialize at this definition site
    DeclareAndInitialize { kind: VariableKind },

    /// Just assign - variable already declared elsewhere
    AssignOnly,
}

/// Variable declaration kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableKind {
    Let,
    Const,
}

/// Strategy for handling an SSA value at a use site
#[derive(Debug, Clone)]
pub enum UseStrategy {
    /// Reference the variable by name
    UseVariable,

    /// Inline the constant value directly
    InlineValue(ConstantValue),
}

impl<'a> SSAUsageTracker<'a> {
    /// Create a new SSA usage tracker with function analysis
    pub fn new(function_analysis: &'a FunctionAnalysis<'a>) -> Self {
        Self {
            function_analysis,
            consumed_uses: HashMap::new(),
            constant_cache: HashMap::new(),
            implicit_arguments_cache: None,
        }
    }

    /// Get the SSA analysis
    fn ssa(&self) -> &SSAAnalysis {
        &self.function_analysis.ssa
    }

    /// Pre-compute all SSA values that are implicit arguments
    /// This significantly improves performance for functions with many SSA values
    pub fn precompute_implicit_arguments(
        &mut self,
        call_site_analysis: &crate::analysis::call_site_analysis::CallSiteAnalysis,
    ) {
        let mut implicit_args = HashSet::new();
        
        // For each call site, find which SSA values are implicit arguments
        for ((_block_id, instr_idx), call_info) in &call_site_analysis.call_sites {
            // For each argument register at this call site
            for &arg_register in &call_info.argument_registers {
                // Find the SSA value that's active at this register at this call
                if let Some(ssa_value) = self
                    .ssa()
                    .get_value_before_instruction(arg_register, *instr_idx)
                {
                    implicit_args.insert(ssa_value.clone());
                }
            }
        }
        
        log::debug!("Pre-computed {} implicit argument SSA values", implicit_args.len());
        self.implicit_arguments_cache = Some(implicit_args);
    }

    /// Check if an SSA value is used as an implicit argument in a Call/Construct instruction
    fn is_implicit_argument(
        &self,
        ssa_value: &SSAValue,
        call_site_analysis: Option<&crate::analysis::call_site_analysis::CallSiteAnalysis>,
    ) -> bool {
        // If we have a pre-computed cache, use it
        if let Some(ref cache) = self.implicit_arguments_cache {
            return cache.contains(ssa_value);
        }
        
        // Otherwise, fall back to the old implementation (for backwards compatibility)
        // If we don't have call site analysis, assume not an implicit argument
        let Some(call_site_analysis) = call_site_analysis else {
            return false;
        };

        // Get the definition site of this SSA value to find its register
        let register = ssa_value.def_site.register;

        // Check all call sites to see if this register is used as an implicit argument
        for ((block_id, instr_idx), call_info) in &call_site_analysis.call_sites {
            // Check if this SSA value is defined before this call site and still active
            if let Some(value_at_call) = self
                .ssa()
                .get_value_before_instruction(register, *instr_idx)
            {
                if *value_at_call == *ssa_value {
                    // This SSA value is active at this call site
                    // Check if its register is in the argument list
                    if call_info.argument_registers.contains(&register) {
                        trace!(
                            "SSA value {} (r{}) is used as implicit argument at block {:?} instruction {:?}",
                            ssa_value.name(), register, block_id, instr_idx.0
                        );
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Get all uses of an SSA value
    fn get_all_uses(&self, ssa_value: &SSAValue) -> Vec<RegisterUse> {
        self.ssa()
            .get_ssa_value_uses(ssa_value)
            .into_iter()
            .cloned()
            .collect()
    }

    /// Get all uses of a duplicated SSA value (uses the original SSA value)
    fn get_all_uses_for_duplicated(&self, dup_ssa_value: &DuplicatedSSAValue) -> Vec<RegisterUse> {
        self.get_all_uses(dup_ssa_value.original_ssa_value())
    }

    /// Mark a specific use of an SSA value as consumed (inlined)
    /// Note: This method assumes the original (non-duplicated) context.
    /// Use mark_use_consumed_with_context for duplicated contexts.
    pub fn mark_use_consumed(&mut self, ssa_value: &SSAValue, use_site: &RegisterUse) {
        self.mark_duplicated_use_consumed(
            &DuplicatedSSAValue::original(ssa_value.clone()),
            use_site,
        );
    }

    /// Mark a specific use of an SSA value as consumed with an optional duplication context
    pub fn mark_use_consumed_with_context(
        &mut self,
        ssa_value: &SSAValue,
        use_site: &RegisterUse,
        context: Option<&DuplicationContext>,
    ) {
        let dup_value = if let Some(ctx) = context {
            DuplicatedSSAValue {
                original: ssa_value.clone(),
                duplication_context: Some(ctx.clone()),
            }
        } else {
            DuplicatedSSAValue::original(ssa_value.clone())
        };
        self.mark_duplicated_use_consumed(&dup_value, use_site);
    }

    /// Mark all uses in a list as consumed
    pub fn mark_uses_consumed(&mut self, ssa_value: &SSAValue, use_sites: &[RegisterUse]) {
        for use_site in use_sites {
            self.mark_use_consumed(ssa_value, use_site);
        }
    }

    /// Get all consumed uses
    pub fn get_consumed_uses(&self) -> &HashMap<DuplicatedSSAValue, HashSet<RegisterUse>> {
        &self.consumed_uses
    }

    /// Mark a specific use of a duplicated SSA value as consumed (inlined)
    pub fn mark_duplicated_use_consumed(
        &mut self,
        dup_ssa_value: &DuplicatedSSAValue,
        use_site: &RegisterUse,
    ) {
        log::debug!(
            "Marking duplicated use consumed: SSA {} ({}) at block {} instruction {}",
            dup_ssa_value.original_ssa_value().name(),
            dup_ssa_value.context_description(),
            use_site.block_id.index(),
            use_site.instruction_idx.value()
        );

        let was_new = self
            .consumed_uses
            .entry(dup_ssa_value.clone())
            .or_default()
            .insert(use_site.clone());

        let name = dup_ssa_value.original_ssa_value().name();
        if name.contains("r1") && name.contains("5") {
            log::debug!(
                "Marking {} use as consumed (was_new={}) (context: {}) at block {} instruction {}",
                name,
                was_new,
                dup_ssa_value.context_description(),
                use_site.block_id.index(),
                use_site.instruction_idx.value()
            );
            if was_new {
                log::debug!("Stack trace: {:?}", std::backtrace::Backtrace::capture());
            }
        }
    }

    /// Mark all uses in a list as consumed for a duplicated SSA value
    pub fn mark_duplicated_uses_consumed(
        &mut self,
        dup_ssa_value: &DuplicatedSSAValue,
        use_sites: &[RegisterUse],
    ) {
        for use_site in use_sites {
            self.mark_duplicated_use_consumed(dup_ssa_value, use_site);
        }
    }

    /// Check if a specific use has been consumed
    pub fn is_use_consumed(&self, ssa_value: &SSAValue, use_site: &RegisterUse) -> bool {
        self.consumed_uses
            .get(&DuplicatedSSAValue::original(ssa_value.clone()))
            .map(|uses| uses.contains(use_site))
            .unwrap_or(false)
    }

    /// Check if a specific use has been consumed for a duplicated SSA value
    pub fn is_duplicated_use_consumed(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
        use_site: &RegisterUse,
    ) -> bool {
        self.consumed_uses
            .get(dup_ssa_value)
            .map(|uses| uses.contains(use_site))
            .unwrap_or(false)
    }

    /// Get the usage status of an SSA value
    pub fn get_usage_status(&self, ssa_value: &SSAValue) -> UsageStatus {
        self.get_duplicated_usage_status(&DuplicatedSSAValue::original(ssa_value.clone()))
    }

    /// Get the usage status of a duplicated SSA value
    pub fn get_duplicated_usage_status(&self, dup_ssa_value: &DuplicatedSSAValue) -> UsageStatus {
        let all_uses = self.get_all_uses_for_duplicated(dup_ssa_value);
        let consumed_count = self
            .consumed_uses
            .get(dup_ssa_value)
            .map(|uses| uses.len())
            .unwrap_or(0);

        let total_uses = all_uses.len();

        if consumed_count == 0 {
            UsageStatus::Unconsumed { total_uses }
        } else if consumed_count == total_uses {
            UsageStatus::FullyConsumed
        } else {
            UsageStatus::PartiallyConsumed {
                consumed_count,
                remaining_count: total_uses - consumed_count,
            }
        }
    }

    /// Get remaining (non-consumed) uses of an SSA value
    pub fn get_remaining_uses(&self, ssa_value: &SSAValue) -> Vec<RegisterUse> {
        self.get_duplicated_remaining_uses(&DuplicatedSSAValue::original(ssa_value.clone()))
    }

    /// Get remaining (non-consumed) uses of a duplicated SSA value
    pub fn get_duplicated_remaining_uses(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
    ) -> Vec<RegisterUse> {
        let all_uses = self.get_all_uses_for_duplicated(dup_ssa_value);
        let consumed = self.consumed_uses.get(dup_ssa_value);

        all_uses
            .into_iter()
            .filter(|use_site| consumed.map(|set| !set.contains(use_site)).unwrap_or(true))
            .collect()
    }

    /// Cache a constant value for an SSA value
    pub fn cache_constant_value(&mut self, ssa_value: &SSAValue, value: ConstantValue) {
        self.cache_duplicated_constant_value(
            &DuplicatedSSAValue::original(ssa_value.clone()),
            value,
        );
    }

    /// Cache a constant value for a duplicated SSA value
    pub fn cache_duplicated_constant_value(
        &mut self,
        dup_ssa_value: &DuplicatedSSAValue,
        value: ConstantValue,
    ) {
        self.constant_cache.insert(dup_ssa_value.clone(), value);
    }

    /// Get cached constant value
    pub fn get_constant_value(&self, ssa_value: &SSAValue) -> Option<&ConstantValue> {
        self.get_duplicated_constant_value(&DuplicatedSSAValue::original(ssa_value.clone()))
    }

    /// Get cached constant value for a duplicated SSA value
    pub fn get_duplicated_constant_value(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
    ) -> Option<&ConstantValue> {
        self.constant_cache.get(dup_ssa_value)
    }

    /// Check if an SSA value has been fully eliminated (all uses consumed)
    /// This is a derived property based on consumed uses
    pub fn is_fully_eliminated(&self, ssa_value: &SSAValue) -> bool {
        self.is_duplicated_fully_eliminated(&DuplicatedSSAValue::original(ssa_value.clone()))
    }

    /// Check if a duplicated SSA value has been fully eliminated (all uses consumed)
    pub fn is_duplicated_fully_eliminated(&self, dup_ssa_value: &DuplicatedSSAValue) -> bool {
        self.is_duplicated_fully_eliminated_with_context(dup_ssa_value, None)
    }

    /// Check if a duplicated SSA value has been fully eliminated (all uses consumed)
    /// with optional call site analysis for detecting implicit arguments
    pub fn is_duplicated_fully_eliminated_with_context(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
        call_site_analysis: Option<&crate::analysis::call_site_analysis::CallSiteAnalysis>,
    ) -> bool {
        // First check if this value is used as an implicit argument in a Call/Construct
        if self.is_implicit_argument(dup_ssa_value.original_ssa_value(), call_site_analysis) {
            trace!(
                "{} is used as an implicit argument, not eliminated",
                dup_ssa_value.original_ssa_value()
            );
            return false;
        }

        let all_uses = self.get_all_uses_for_duplicated(dup_ssa_value);

        if all_uses.is_empty() {
            trace!(
                "{} has no uses, eliminated",
                dup_ssa_value.original_ssa_value()
            );
            // No uses means it's effectively eliminated
            return true;
        }

        // Check if all uses have been consumed
        if let Some(consumed) = self.consumed_uses.get(dup_ssa_value) {
            let is_eliminated = all_uses.iter().all(|use_site| consumed.contains(use_site));
            trace!(
                "{} has {} uses, {} consumed, eliminated: {}",
                dup_ssa_value.original_ssa_value(),
                all_uses.len(),
                consumed.len(),
                is_eliminated
            );
            is_eliminated
        } else {
            trace!(
                "{} has {} uses, 0 consumed, not eliminated",
                dup_ssa_value.original_ssa_value(),
                all_uses.len()
            );
            // No consumed uses tracked, so not eliminated
            false
        }
    }

    /// Determine the declaration strategy for a (potentially duplicated) SSA value at its definition site
    ///
    /// This is called when we encounter an SSA value definition and must decide whether to:
    /// - Skip generating any code (all uses eliminated)
    /// - Declare at a dominator point
    /// - Declare and initialize at the definition site
    /// - Just assign (variable already declared)
    pub fn get_declaration_strategy(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
    ) -> DeclarationStrategy {
        self.get_declaration_strategy_with_context(dup_ssa_value, None)
    }

    pub fn get_declaration_strategy_with_context(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
        call_site_analysis: Option<&crate::analysis::call_site_analysis::CallSiteAnalysis>,
    ) -> DeclarationStrategy {
        let ssa_value = dup_ssa_value.original_ssa_value();

        // Check for side effects first - even if a value has no uses,
        // we need to execute the instruction if it has side effects
        if let Some(block) = self
            .function_analysis
            .cfg
            .graph()
            .node_weight(ssa_value.def_site.block_id)
        {
            for instr in &block.instructions {
                if instr.instruction_index == ssa_value.def_site.instruction_idx {
                    if has_side_effects(&instr.instruction) {
                        // Check if this value has any uses
                        let all_uses = self.get_all_uses_for_duplicated(dup_ssa_value);
                        if all_uses.is_empty()
                            || self.is_duplicated_fully_eliminated_with_context(
                                dup_ssa_value,
                                call_site_analysis,
                            )
                        {
                            trace!(
                                "SSA value {} has side effects but no uses, using SideEffectOnly",
                                ssa_value
                            );
                            return DeclarationStrategy::SideEffectOnly;
                        }
                    }
                    break;
                }
            }
        }

        // Special handling for PHI result values
        // PHI results need special handling - they represent the merged value
        // but don't generate code themselves. However, if the PHI result is used
        // (e.g., in a return statement), we need to keep it.
        if self.is_phi_result(ssa_value) {
            // Check if this PHI result is actually used
            let uses = self.ssa().get_ssa_value_uses(ssa_value);
            if uses.is_empty() {
                // Unused PHI result - skip it
                return DeclarationStrategy::Skip;
            }
            // PHI result is used - it needs to exist as a variable
            // Fall through to normal handling
        }

        // 1. Check if part of PHI group (coalesced values) first
        // This needs to be checked before elimination because PHI operands
        // might have no direct uses but still need to generate assignments
        if let Some(var_analysis) = &self.function_analysis.ssa.variable_analysis {
            if let Some(coalesced_rep) = var_analysis.coalesced_values.get(ssa_value) {
                trace!(
                    "{} is part of PHI group with representative {}",
                    ssa_value, coalesced_rep
                );
                // This is part of a PHI group (coalesced values)
                if self.is_declaration_point(ssa_value, coalesced_rep) {
                    // Check if fully eliminated even for declaration points
                    if self.is_duplicated_fully_eliminated_with_context(
                        dup_ssa_value,
                        call_site_analysis,
                    ) {
                        return DeclarationStrategy::Skip;
                    }

                    // This is the declaration point for the coalesced group
                    if self.needs_dominator_declaration(coalesced_rep) {
                        let dominator_block = self.find_dominator_for_coalesced(coalesced_rep);
                        let kind = self.determine_variable_kind(coalesced_rep);
                        return DeclarationStrategy::DeclareAtDominator {
                            dominator_block,
                            kind,
                        };
                    } else {
                        let kind = self.determine_variable_kind(coalesced_rep);
                        return DeclarationStrategy::DeclareAndInitialize { kind };
                    }
                } else {
                    // Not the declaration point - this is a PHI operand that needs assignment
                    return DeclarationStrategy::AssignOnly;
                }
            }
        }

        // 2. Check if fully eliminated (considering duplication context)
        // Only check this AFTER checking for PHI operands
        // Side effects were already checked above
        if self.is_duplicated_fully_eliminated_with_context(dup_ssa_value, call_site_analysis) {
            debug!(
                "SSA value {} (context: {}) is fully eliminated with no side effects, using Skip",
                ssa_value,
                dup_ssa_value.context_description()
            );
            return DeclarationStrategy::Skip;
        }

        // 3. Single SSA value - declare at definition
        let kind = self.determine_variable_kind(ssa_value);
        DeclarationStrategy::DeclareAndInitialize { kind }
    }

    /// Determine the use strategy for a (potentially duplicated) SSA value at a use site
    ///
    /// This is called when we need to use an SSA value and must decide whether to:
    /// - Reference the variable by name
    /// - Inline the constant value directly
    pub fn get_use_strategy(
        &self,
        dup_ssa_value: &DuplicatedSSAValue,
        use_site: &RegisterUse,
    ) -> UseStrategy {
        // Check if this use has been marked as consumed/inlined
        if self.is_duplicated_use_consumed(dup_ssa_value, use_site) {
            // This use was already inlined - get the constant value
            let value_tracker = self.function_analysis.value_tracker();
            let tracked_value = value_tracker.get_value(dup_ssa_value.original_ssa_value());
            if let TrackedValue::Constant(c) = tracked_value {
                return UseStrategy::InlineValue(c);
            }
        }

        // Default: use the variable
        UseStrategy::UseVariable
    }

    /// Update tracking after a switch converter has processed cases
    ///
    /// The switch converter should call this to report which SSA values
    /// had their comparison uses inlined into the switch cases
    pub fn report_switch_inlined_uses(
        &mut self,
        inlined_comparisons: Vec<(SSAValue, Vec<RegisterUse>)>,
    ) {
        for (ssa_value, uses) in inlined_comparisons {
            self.mark_uses_consumed(&ssa_value, &uses);
        }

        // After marking uses as consumed, perform cascading elimination
        // Note: This method doesn't have access to duplicated blocks info,
        // so we pass an empty map. This is safe since the method is currently unused.
        let empty_duplicated_blocks = HashMap::new();
        self.perform_cascading_elimination(&empty_duplicated_blocks);
    }

    /// Check if an SSA value is a PHI result
    fn is_phi_result(&self, ssa_value: &SSAValue) -> bool {
        // Check if this SSA value is the result of a PHI function
        for phi_list in self.ssa().phi_functions.values() {
            for phi in phi_list {
                if &phi.result == ssa_value {
                    return true;
                }
            }
        }
        false
    }

    /// Check if this SSA value is the declaration point for its coalesced group
    fn is_declaration_point(&self, ssa_value: &SSAValue, coalesced_rep: &SSAValue) -> bool {
        // Use the representative as the declaration point
        // This ensures a single declaration for all coalesced values
        ssa_value == coalesced_rep
    }

    /// Check if a coalesced value needs declaration at a dominator
    fn needs_dominator_declaration(&self, coalesced_rep: &SSAValue) -> bool {
        // A coalesced value needs dominator declaration if no single definition
        // dominates all uses (e.g., PHI nodes where the result is used before some definitions)

        // Check if this is involved in PHI functions - if so, it likely needs
        // declaration at a dominator point
        let Some(var_analysis) = &self.function_analysis.ssa.variable_analysis else {
            return false;
        };

        // Check if this representative appears in PHI functions
        for phi_list in self.ssa().phi_functions.values() {
            for phi in phi_list {
                // Check if the PHI result maps to this representative
                if var_analysis.coalesced_values.get(&phi.result) == Some(coalesced_rep) {
                    return true;
                }
                // Check if any operand maps to this representative
                for operand in phi.operands.values() {
                    if var_analysis.coalesced_values.get(operand) == Some(coalesced_rep) {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Find the dominator block for a coalesced value
    fn find_dominator_for_coalesced(&self, coalesced_rep: &SSAValue) -> petgraph::graph::NodeIndex {
        // Find the common dominator of all definition sites in the coalesced group
        let Some(var_analysis) = &self.function_analysis.ssa.variable_analysis else {
            return petgraph::graph::NodeIndex::new(0);
        };

        // Collect all blocks where values in this coalesced group are defined
        let mut def_blocks = Vec::new();
        for (value, rep) in &var_analysis.coalesced_values {
            if rep == coalesced_rep {
                def_blocks.push(value.def_site.block_id);
            }
        }

        // Also check PHI results
        for phi_list in self.ssa().phi_functions.values() {
            for phi in phi_list {
                if var_analysis.coalesced_values.get(&phi.result) == Some(coalesced_rep) {
                    def_blocks.push(phi.result.def_site.block_id);
                }
            }
        }

        if def_blocks.is_empty() {
            // No definitions found, use entry block
            return petgraph::graph::NodeIndex::new(0);
        }

        // Use the CFG's dominator analysis to find common dominator
        if let Some(dominators) = self.function_analysis.cfg.analyze_dominators() {
            // Find the lowest common dominator of all definition blocks
            let mut common_dominator = def_blocks[0];
            for &block in &def_blocks[1..] {
                // Find the common dominator of current and next block
                common_dominator = self.find_common_dominator(&dominators, common_dominator, block);
            }
            common_dominator
        } else {
            // Dominator analysis failed, use entry block as fallback
            petgraph::graph::NodeIndex::new(0)
        }
    }

    /// Find the common dominator of two blocks
    fn find_common_dominator(
        &self,
        dominators: &Dominators<petgraph::graph::NodeIndex>,
        a: petgraph::graph::NodeIndex,
        b: petgraph::graph::NodeIndex,
    ) -> petgraph::graph::NodeIndex {
        // Find the lowest common ancestor in the dominator tree
        let mut path_a = Vec::new();
        let mut current = a;
        loop {
            path_a.push(current);
            if let Some(idom) = dominators.immediate_dominator(current) {
                if idom == current {
                    break;
                }
                current = idom;
            } else {
                break;
            }
        }

        // Walk up from b until we find a node in path_a
        let mut current = b;
        loop {
            if path_a.contains(&current) {
                return current;
            }
            if let Some(idom) = dominators.immediate_dominator(current) {
                if idom == current {
                    break;
                }
                current = idom;
            } else {
                break;
            }
        }

        // Fallback to entry block
        petgraph::graph::NodeIndex::new(0)
    }

    /// Determine if a variable should be const or let
    fn determine_variable_kind(&self, ssa_value: &SSAValue) -> VariableKind {
        let Some(var_analysis) = &self.function_analysis.ssa.variable_analysis else {
            return VariableKind::Let;
        };

        // Get the representative for this value
        let representative = var_analysis
            .coalesced_values
            .get(ssa_value)
            .unwrap_or(ssa_value);

        // Check the usage pattern
        if let Some(usage) = var_analysis.variable_usage.get(representative) {
            if usage.should_be_const {
                VariableKind::Const
            } else {
                VariableKind::Let
            }
        } else {
            // Default to let if we don't have usage info
            VariableKind::Let
        }
    }

    /// Perform cascading elimination optimization
    ///
    /// When an SSA value becomes fully consumed, check if it was the only use of another value.
    /// If so, that value can also be eliminated. This process cascades transitively.
    ///
    /// The duplicated_blocks parameter provides information about which blocks are duplicated
    /// and in what contexts, allowing us to mark uses as consumed in the correct context.
    pub fn perform_cascading_elimination(
        &mut self,
        duplicated_blocks: &HashMap<NodeIndex, Vec<DuplicationContext>>,
    ) {
        let start_time = std::time::Instant::now();
        let total_ssa_values = self.ssa().all_ssa_values().count();
        log::debug!("Starting cascading elimination for {} SSA values", total_ssa_values);
        
        let mut changed = true;
        let max_iterations = 10; // Prevent infinite loops
        let mut iterations = 0;

        while changed && iterations < max_iterations {
            let iteration_start = std::time::Instant::now();
            changed = false;
            iterations += 1;

            // Collect SSA values that are now fully eliminated
            let mut newly_eliminated = Vec::new();
            let mut values_checked = 0;

            // Check all SSA values to see if they've become fully eliminated
            for ssa_value in self.ssa().all_ssa_values() {
                values_checked += 1;
                if values_checked % 1000 == 0 {
                    log::trace!("  Checked {}/{} values in iteration {}", 
                               values_checked, total_ssa_values, iterations);
                }
                if !self.is_fully_eliminated(ssa_value) {
                    continue;
                }

                // This value is fully eliminated - check what it was defined by
                if let Some(def_instruction) = self.ssa().get_defining_instruction(ssa_value) {
                    let block_id = def_instruction.block_id;
                    let instr_idx = def_instruction.instruction_idx;

                    // Check if this value is an input to any PHI function
                    // If so, don't cascade eliminate its sources
                    let is_phi_input = self
                        .ssa()
                        .phi_functions
                        .values()
                        .flatten()
                        .any(|phi| phi.operands.values().any(|operand| operand == ssa_value));

                    if is_phi_input {
                        log::debug!(
                            "Not cascading elimination for sources of {} - it's a PHI function input",
                            ssa_value.name()
                        );
                        continue;
                    }

                    // Get the instruction
                    if let Some(block) = self.function_analysis.cfg.graph().node_weight(block_id) {
                        // Find the instruction with the matching index
                        let instr = block
                            .instructions()
                            .iter()
                            .find(|i| i.instruction_index == instr_idx);
                        if let Some(instr) = instr {
                            // Check if this instruction has side effects and cannot be eliminated
                            if has_side_effects(&instr.instruction) {
                                // This instruction has side effects, cannot eliminate its uses
                                continue;
                            }

                            // Check what SSA values this instruction uses
                            let usage =
                                crate::generated::instruction_analysis::analyze_register_usage(
                                    &instr.instruction,
                                );

                            // For each source register used by this instruction
                            for &source_reg in &usage.sources {
                                if let Some(source_ssa) = self
                                    .ssa()
                                    .get_value_before_instruction(source_reg, instr_idx)
                                {
                                    // Check if this was the only remaining use of the source SSA value
                                    let source_uses = self.get_remaining_uses(source_ssa);
                                    if source_uses.len() == 1
                                        && source_uses[0].block_id == block_id
                                        && source_uses[0].instruction_idx == instr_idx
                                    {
                                        // This was the only use - mark it as consumed
                                        newly_eliminated.push((source_ssa.clone(), source_uses));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Mark newly eliminated values as consumed with appropriate context
            let num_eliminated = newly_eliminated.len();
            for (ssa_value, uses) in newly_eliminated {
                // Check if ANY of the uses are in duplicated blocks
                let has_duplicated_use = uses
                    .iter()
                    .any(|use_site| duplicated_blocks.contains_key(&use_site.block_id));

                if has_duplicated_use {
                    log::debug!(
                        "Skipping cascading elimination for {} - has uses in duplicated blocks",
                        ssa_value.name()
                    );
                    // Don't mark as changed since we didn't actually eliminate anything
                    continue;
                }

                log::debug!(
                    "Cascading elimination: marking SSA {} as fully consumed (was only used by eliminated values)",
                    ssa_value.name()
                );

                // All uses are in non-duplicated blocks, safe to mark as consumed
                for use_site in &uses {
                    self.mark_use_consumed(&ssa_value, use_site);
                }
                changed = true;
            }
            
            let iteration_time = iteration_start.elapsed();
            log::debug!("  Iteration {} completed in {:?}, {} values eliminated", 
                       iterations, iteration_time, num_eliminated);
        }

        let total_time = start_time.elapsed();
        log::info!("Cascading elimination completed in {:?} ({} iterations, {} SSA values)", 
                  total_time, iterations, total_ssa_values);
        
        if iterations >= max_iterations {
            log::warn!("Cascading elimination reached maximum iterations - possible cycle");
        }
    }
}

#[cfg(test)]
mod tests {
    // Note: These tests are temporarily disabled due to complex setup requirements
    // The core functionality they test is verified through integration tests

    /*
    #[test]
    fn test_duplicated_value_declaration_tracking() {
        // TODO: Re-enable with proper mocking infrastructure
    }

    #[test]
    fn test_duplicated_value_independence() {
        // TODO: Re-enable with proper mocking infrastructure
    */
}

/// Check if an instruction has side effects that prevent it from being eliminated
///
/// Returns true if the instruction has observable side effects beyond just writing
/// to its target register. These instructions cannot be eliminated even if their
/// result is unused.
///
/// Instructions with side effects include:
/// - Function calls (can throw, modify global state)
/// - Object/Array creation (allocates memory, can throw)
/// - Closure/Generator creation (captures environment)
/// - Property access (can call getters/setters, can throw)
/// - Iterator operations (follow protocol, can throw)
/// - Exception handling (Catch, Throw)
/// - Environment operations (modify scope)
/// - Direct eval (can do anything)
///
/// Pure instructions that are safe to eliminate when unused include:
/// - Load constants (LoadConstNull, LoadConstTrue, etc.)
/// - Register moves (Mov, MovLong)
/// - Arithmetic operations (Add, Sub, Mul, Div, etc.)
/// - Bitwise operations (BitAnd, BitOr, etc.)
/// - Comparisons (Eq, StrictEq, Less, etc.)
/// - Type conversions (ToNumber, ToString, etc.)
/// - Memory loads (Loadi8, Loadu16, etc.)
/// - Load from environment (pure reads)
/// - PHI functions (SSA construction)
fn has_side_effects(
    instruction: &crate::generated::unified_instructions::UnifiedInstruction,
) -> bool {
    use crate::generated::unified_instructions::UnifiedInstruction;

    match instruction {
        // Function calls can throw, have side effects, modify state
        UnifiedInstruction::Call { .. }
        | UnifiedInstruction::CallLong { .. }
        | UnifiedInstruction::Call1 { .. }
        | UnifiedInstruction::Call2 { .. }
        | UnifiedInstruction::Call3 { .. }
        | UnifiedInstruction::Call4 { .. }
        | UnifiedInstruction::CallDirect { .. }
        | UnifiedInstruction::CallDirectLongIndex { .. }
        | UnifiedInstruction::CallBuiltin { .. }
        | UnifiedInstruction::CallBuiltinLong { .. }
        | UnifiedInstruction::Construct { .. }
        | UnifiedInstruction::ConstructLong { .. }
        // Object/Array creation allocates memory, can throw
        | UnifiedInstruction::NewObject { .. }
        | UnifiedInstruction::NewObjectWithParent { .. }
        | UnifiedInstruction::NewObjectWithBuffer { .. }
        | UnifiedInstruction::NewObjectWithBufferLong { .. }
        | UnifiedInstruction::NewArray { .. }
        | UnifiedInstruction::NewArrayWithBuffer { .. }
        | UnifiedInstruction::NewArrayWithBufferLong { .. }
        // Closure/Generator creation allocates memory, captures environment
        | UnifiedInstruction::CreateClosure { .. }
        | UnifiedInstruction::CreateClosureLongIndex { .. }
        | UnifiedInstruction::CreateAsyncClosure { .. }
        | UnifiedInstruction::CreateAsyncClosureLongIndex { .. }
        | UnifiedInstruction::CreateGenerator { .. }
        | UnifiedInstruction::CreateGeneratorLongIndex { .. }
        | UnifiedInstruction::CreateGeneratorClosure { .. }
        | UnifiedInstruction::CreateGeneratorClosureLongIndex { .. }
        // Environment operations modify program state
        | UnifiedInstruction::CreateEnvironment { .. }
        | UnifiedInstruction::CreateInnerEnvironment { .. }
        | UnifiedInstruction::CreateThis { .. }
        // Iterator operations follow iterator protocol, can throw
        | UnifiedInstruction::IteratorBegin { .. }
        | UnifiedInstruction::IteratorNext { .. }
        // Property access can call getters, can throw
        | UnifiedInstruction::GetById { .. }
        | UnifiedInstruction::GetByIdShort { .. }
        | UnifiedInstruction::GetByIdLong { .. }
        | UnifiedInstruction::TryGetById { .. }
        | UnifiedInstruction::TryGetByIdLong { .. }
        | UnifiedInstruction::GetByVal { .. }
        | UnifiedInstruction::GetPNameList { .. }
        // Exception handling must not be eliminated
        | UnifiedInstruction::Catch { .. }
        // Meta properties and special values might be needed
        | UnifiedInstruction::GetNewTarget { .. }
        | UnifiedInstruction::GetGlobalObject { .. }
        | UnifiedInstruction::GetArgumentsLength { .. }
        | UnifiedInstruction::GetArgumentsPropByVal { .. }
        | UnifiedInstruction::ReifyArguments { .. }
        // Regex creation allocates memory, can throw
        | UnifiedInstruction::CreateRegExp { .. }
        // TypeOf can be observed through toString/valueOf
        | UnifiedInstruction::TypeOf { .. }
        // These instructions throw or have other observable effects
        | UnifiedInstruction::ThrowIfUndefinedInst { .. }
        | UnifiedInstruction::ThrowIfEmpty { .. }
        // Direct eval can do anything
        | UnifiedInstruction::DirectEval { .. } => true,
        // All other instructions are pure or only write to their target
        _ => false,
    }
}
