//! Variable analysis for SSA values
//!
//! This module handles:
//! - Phi coalescing using Union-Find
//! - Scope determination based on live ranges
//! - Usage pattern analysis (const vs let, reassignments)
//! - Building lookup tables for variable resolution

use super::types::{SSAAnalysis, SSAValue};
use crate::cfg::Cfg;
use crate::hbc::InstructionIndex;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Variable scope information
#[derive(Debug, Clone)]
pub enum VariableScope {
    /// Variable needs function-level scope
    Function,
    /// Variable can be block-scoped
    Block(NodeIndex),
}

/// Variable usage pattern
#[derive(Debug, Clone)]
pub struct VariableUsage {
    /// Number of assignments to this variable
    pub assignment_count: usize,
    /// Whether the variable is reassigned after initial declaration
    pub is_reassigned: bool,
    /// Whether this variable should be const (never reassigned)
    pub should_be_const: bool,
    /// PCs where this variable is defined/assigned
    pub definition_pcs: Vec<InstructionIndex>,
    /// Whether this is a function parameter
    pub is_parameter: bool,
}

/// Variable analysis results
#[derive(Debug, Clone)]
pub struct VariableAnalysis {
    /// Maps each SSA value to its equivalence class representative
    pub coalesced_values: HashMap<SSAValue, SSAValue>,
    /// Variable scopes for each equivalence class
    pub variable_scopes: HashMap<SSAValue, VariableScope>,
    /// Variable usage patterns for each equivalence class
    pub variable_usage: HashMap<SSAValue, VariableUsage>,
    /// Maps (register, pc) to the equivalence class representative
    pub register_at_pc: HashMap<(u8, InstructionIndex), SSAValue>,
    /// Maps (register, pc) to SSA value BEFORE the instruction (for source lookups)
    pub register_before_pc: HashMap<(u8, InstructionIndex), SSAValue>,
}

/// Union-Find data structure for phi coalescing
#[derive(Debug)]
struct UnionFind {
    parent: HashMap<SSAValue, SSAValue>,
    rank: HashMap<SSAValue, usize>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parent: HashMap::new(),
            rank: HashMap::new(),
        }
    }

    fn make_set(&mut self, value: SSAValue) {
        self.parent.insert(value.clone(), value.clone());
        self.rank.insert(value, 0);
    }

    fn find(&mut self, value: &SSAValue) -> SSAValue {
        if let Some(parent) = self.parent.get(value).cloned() {
            if parent != *value {
                let root = self.find(&parent);
                self.parent.insert(value.clone(), root.clone());
                root
            } else {
                value.clone()
            }
        } else {
            value.clone()
        }
    }

    fn union(&mut self, a: &SSAValue, b: &SSAValue) {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return;
        }

        let rank_a = self.rank.get(&root_a).copied().unwrap_or(0);
        let rank_b = self.rank.get(&root_b).copied().unwrap_or(0);

        if rank_a < rank_b {
            self.parent.insert(root_a, root_b);
        } else if rank_a > rank_b {
            self.parent.insert(root_b, root_a);
        } else {
            self.parent.insert(root_b, root_a.clone());
            self.rank.insert(root_a, rank_a + 1);
        }
    }
}

/// Analyze variables for SSA values
pub fn analyze_variables(
    ssa: &SSAAnalysis,
    cfg: &Cfg,
) -> Result<VariableAnalysis, crate::error::Error> {
    let mut analysis = VariableAnalysis {
        coalesced_values: HashMap::new(),
        variable_scopes: HashMap::new(),
        variable_usage: HashMap::new(),
        register_at_pc: HashMap::new(),
        register_before_pc: HashMap::new(),
    };

    // Step 1: Perform phi coalescing
    analysis.coalesced_values = perform_phi_coalescing(ssa);

    // Step 2: Determine variable scopes for equivalence classes
    determine_scopes(ssa, cfg, &mut analysis);

    // Step 3: Analyze variable usage patterns
    analyze_usage_patterns(ssa, cfg, &mut analysis)?;

    // Step 4: Build register lookup tables
    build_lookup_tables(ssa, cfg, &mut analysis);

    Ok(analysis)
}

/// Perform phi coalescing to group SSA values that should share names
fn perform_phi_coalescing(ssa: &SSAAnalysis) -> HashMap<SSAValue, SSAValue> {
    let mut union_find = UnionFind::new();

    // Initialize all SSA values
    for ssa_value in ssa.ssa_values.values() {
        union_find.make_set(ssa_value.clone());
    }

    // Initialize phi results
    for phi_list in ssa.phi_functions.values() {
        for phi in phi_list {
            union_find.make_set(phi.result.clone());
        }
    }

    // Union phi results with their operands
    for phi_list in ssa.phi_functions.values() {
        for phi in phi_list {
            for operand in phi.operands.values() {
                union_find.union(&phi.result, operand);
            }
        }
    }

    // Build mapping to representatives
    let mut coalesced = HashMap::new();

    // Map all SSA values
    for ssa_value in ssa.ssa_values.values() {
        let root = union_find.find(ssa_value);
        coalesced.insert(ssa_value.clone(), root);
    }

    // Map phi results
    for phi_list in ssa.phi_functions.values() {
        for phi in phi_list {
            let root = union_find.find(&phi.result);
            coalesced.insert(phi.result.clone(), root);
        }
    }

    coalesced
}

/// Determine variable scopes based on usage patterns
fn determine_scopes(ssa: &SSAAnalysis, cfg: &Cfg, analysis: &mut VariableAnalysis) {
    // Get unique representatives
    let mut representatives = HashSet::new();
    for representative in analysis.coalesced_values.values() {
        representatives.insert(representative.clone());
    }

    // Determine scope for each equivalence class
    for representative in representatives {
        let scope = determine_variable_scope(&representative, ssa, cfg, &analysis.coalesced_values);
        analysis.variable_scopes.insert(representative, scope);
    }
}

/// Determine scope for a single equivalence class
fn determine_variable_scope(
    representative: &SSAValue,
    ssa: &SSAAnalysis,
    cfg: &Cfg,
    coalesced: &HashMap<SSAValue, SSAValue>,
) -> VariableScope {
    let mut use_blocks = HashSet::new();

    // Find all values in this equivalence class
    let mut class_values = Vec::new();
    for (value, rep) in coalesced {
        if rep == representative {
            class_values.push(value);
            // Add definition blocks
            use_blocks.insert(value.def_site.block_id);
        }
    }

    // Find all blocks where values in this class are used
    for value in &class_values {
        for (use_site, def_site) in &ssa.use_def_chains {
            if def_site == &value.def_site {
                use_blocks.insert(use_site.block_id);
            }
        }
    }

    // Check if used in phi functions
    for (block_id, phi_list) in &ssa.phi_functions {
        for phi in phi_list {
            // Check if phi result is in this class
            if coalesced.get(&phi.result) == Some(representative) {
                use_blocks.insert(*block_id);
            }
            // Check if any operand is in this class
            for operand in phi.operands.values() {
                if coalesced.get(operand) == Some(representative) {
                    use_blocks.insert(*block_id);
                }
            }
        }
    }

    // If used across multiple blocks or crosses loop boundary, needs function scope
    if use_blocks.len() > 1 || crosses_loop_boundary_class(&class_values, cfg) {
        VariableScope::Function
    } else if let Some(first_value) = class_values.first() {
        VariableScope::Block(first_value.def_site.block_id)
    } else {
        VariableScope::Function // Conservative default
    }
}

/// Check if any value in the class crosses a loop boundary
fn crosses_loop_boundary_class(values: &[&SSAValue], cfg: &Cfg) -> bool {
    let loops = cfg.find_natural_loops();
    for (_, back_edge_to) in loops {
        for value in values {
            if value.def_site.block_id == back_edge_to {
                return true;
            }
        }
    }
    false
}

/// Analyze variable usage patterns
fn analyze_usage_patterns(
    ssa: &SSAAnalysis,
    cfg: &Cfg,
    analysis: &mut VariableAnalysis,
) -> Result<(), crate::error::Error> {
    // Get unique representatives
    let mut representatives = HashSet::new();
    for representative in analysis.coalesced_values.values() {
        representatives.insert(representative.clone());
    }

    // Analyze each equivalence class
    for representative in representatives {
        let mut usage = VariableUsage {
            assignment_count: 0,
            is_reassigned: false,
            should_be_const: false,
            definition_pcs: Vec::new(),
            is_parameter: false,
        };

        // Find all values in this equivalence class
        let mut class_values = Vec::new();
        for (value, rep) in &analysis.coalesced_values {
            if rep == &representative {
                class_values.push(value);
            }
        }

        // Sort class_values by instruction index for deterministic ordering
        class_values.sort_by_key(|v| v.def_site.instruction_idx);

        // Count assignments and track PCs
        for value in &class_values {
            usage.assignment_count += 1;
            usage.definition_pcs.push(value.def_site.instruction_idx);

            let instructions = cfg
                .builder()
                .hbc_file()
                .functions
                .get_instructions(cfg.function_index())?;

            if let Some(instruction) = instructions.get(value.def_site.instruction_idx.value()) {
                // Check if this is a LoadParam instruction
                if matches!(
                    instruction.instruction,
                    crate::generated::unified_instructions::UnifiedInstruction::LoadParam { .. }
                ) {
                    usage.is_parameter = true;
                }
            }
        }

        // Check if it includes phi functions (which are like assignments)
        for (_, phi_list) in &ssa.phi_functions {
            for phi in phi_list {
                if analysis.coalesced_values.get(&phi.result) == Some(&representative) {
                    usage.assignment_count += 1;
                    // Phi functions happen at block start
                    if let Some(block) = cfg.graph().node_weight(phi.result.def_site.block_id) {
                        usage.definition_pcs.push(block.start_pc());
                    }
                }
            }
        }

        // Sort definition PCs for deterministic ordering
        usage.definition_pcs.sort();

        // Determine if reassigned
        usage.is_reassigned = usage.assignment_count > 1;

        // Variables can be const if they're never reassigned
        // Parameters can't be const in JavaScript
        usage.should_be_const = !usage.is_reassigned && !usage.is_parameter;

        analysis.variable_usage.insert(representative, usage);
    }

    Ok(())
}

/// Build register lookup tables
fn build_lookup_tables(ssa: &SSAAnalysis, _cfg: &Cfg, analysis: &mut VariableAnalysis) {
    // Build register_at_pc mapping
    for def in &ssa.definitions {
        if let Some(ssa_value) = ssa.ssa_values.values().find(|v| v.def_site == *def) {
            // Store the actual SSA value, not the representative
            // The coalescing is already tracked in coalesced_values
            analysis
                .register_at_pc
                .insert((def.register, def.instruction_idx), ssa_value.clone());
        }
    }

    // Map phi functions - they're available at their definition site
    for (_block_id, phi_list) in &ssa.phi_functions {
        for phi in phi_list {
            // Use the PHI's actual definition site, which is set to block_start_pc - 1
            // to avoid collisions with instructions at the block start
            analysis.register_at_pc.insert(
                (phi.register, phi.result.def_site.instruction_idx),
                phi.result.clone(),
            );
        }
    }

    // Build register_before_pc mapping
    // For each use, map it to the SSA value that was live before the instruction
    for (use_site, def_site) in &ssa.use_def_chains {
        if let Some(ssa_value) = ssa.ssa_values.values().find(|v| v.def_site == *def_site) {
            // Store the actual SSA value, not the representative
            analysis.register_before_pc.insert(
                (use_site.register, use_site.instruction_idx),
                ssa_value.clone(),
            );
        }
    }
}
