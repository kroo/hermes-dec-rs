use crate::cfg::ssa::{SSAAnalysis, SSAValue};
use crate::cfg::Cfg;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

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

#[derive(Debug, Error)]
pub enum VariableMapError {
    #[error("Variable name generation failed: {0}")]
    NameGeneration(String),
    #[error("Scope analysis failed: {0}")]
    ScopeAnalysis(String),
}

/// Maps SSA values to JavaScript variables
#[derive(Debug)]
pub struct VariableMapping {
    pub ssa_to_var: HashMap<SSAValue, String>,
    pub register_at_pc: HashMap<(u8, u32), SSAValue>,
    /// Maps (register, pc) to SSA value BEFORE the instruction at that PC
    /// Used for source operand lookups to avoid self-reference issues
    pub register_before_pc: HashMap<(u8, u32), SSAValue>,
    pub function_scope_vars: HashSet<String>,
    pub block_scope_vars: HashMap<NodeIndex, HashSet<String>>,
    /// Fallback variable names for registers not covered by SSA
    pub fallback_register_names: HashMap<u8, String>,
    /// Global variable names by string table index
    pub global_variables: HashMap<usize, String>,
    /// Temporary variable counter for generating unique names
    pub temp_counter: u32,
}

impl VariableMapping {
    pub fn new() -> Self {
        Self {
            ssa_to_var: HashMap::new(),
            register_at_pc: HashMap::new(),
            register_before_pc: HashMap::new(),
            function_scope_vars: HashSet::new(),
            block_scope_vars: HashMap::new(),
            fallback_register_names: HashMap::new(),
            global_variables: HashMap::new(),
            temp_counter: 0,
        }
    }

    /// Check if a variable needs function-level declaration
    pub fn needs_function_scope(&self, var_name: &str) -> bool {
        self.function_scope_vars.contains(var_name)
    }

    /// Get all variables that need to be declared in a specific block
    pub fn get_block_scope_vars(&self, block_id: NodeIndex) -> Option<&HashSet<String>> {
        self.block_scope_vars.get(&block_id)
    }

    /// Get variable name with fallback to non-SSA naming
    pub fn get_variable_name_with_fallback(&self, register: u8, pc: u32) -> Option<&String> {
        // First try SSA-based lookup
        if let Some(ssa_value) = self.register_at_pc.get(&(register, pc)) {
            // Look up the coalesced variable name for this SSA value
            if let Some(var_name) = self.ssa_to_var.get(ssa_value) {
                return Some(var_name);
            }
        }

        // Fallback to simple register naming
        self.fallback_register_names.get(&register)
    }

    /// Get variable name for a source operand (before the instruction executes)
    /// This prevents self-reference issues like `let x = y - x` when it should be `let x2 = y - x1`
    pub fn get_source_variable_name(&self, register: u8, pc: u32) -> Option<&String> {
        // Use the "before" lookup for source operands
        if let Some(ssa_value) = self.register_before_pc.get(&(register, pc)) {
            // Look up the coalesced variable name for this SSA value
            if let Some(var_name) = self.ssa_to_var.get(ssa_value) {
                return Some(var_name);
            }
        }

        // IMPORTANT: For source operands, we should NOT use register_at_pc for the current PC
        // because that might contain the AFTER value if this instruction defines the register.
        // Instead, we need to find the value from a previous PC.

        // Try to find the most recent definition before this PC
        for check_pc in (0..pc).rev() {
            if let Some(ssa_value) = self.register_at_pc.get(&(register, check_pc)) {
                if let Some(var_name) = self.ssa_to_var.get(ssa_value) {
                    return Some(var_name);
                }
            }
        }

        // Last resort: check if there's a fallback name
        self.fallback_register_names.get(&register)
    }

    /// Generate a unique temporary variable name
    pub fn generate_temp_name(&mut self) -> String {
        self.temp_counter += 1;
        format!("_temp{}", self.temp_counter)
    }

    /// Register a global variable by string table index and name
    pub fn register_global_variable(&mut self, string_index: usize, var_name: String) {
        self.global_variables.insert(string_index, var_name);
    }

    /// Get a global variable name by string table index
    pub fn get_global_variable(&self, string_index: usize) -> Option<&String> {
        self.global_variables.get(&string_index)
    }

    /// Check if a string table index represents a known global variable
    pub fn is_global_variable(&self, string_index: usize) -> bool {
        self.global_variables.contains_key(&string_index)
    }

    /// Set a fallback variable name for a register (for special cases)
    pub fn set_fallback_variable_name(&mut self, register: u8, name: String) {
        self.fallback_register_names.insert(register, name);
    }
}

pub struct VariableMapper {
    reserved_names: HashSet<String>,
}

impl VariableMapper {
    pub fn new() -> Self {
        let mut reserved_names = HashSet::new();

        // JavaScript reserved words
        for word in &[
            "break",
            "case",
            "catch",
            "class",
            "const",
            "continue",
            "debugger",
            "default",
            "delete",
            "do",
            "else",
            "export",
            "extends",
            "finally",
            "for",
            "function",
            "if",
            "import",
            "in",
            "instanceof",
            "new",
            "return",
            "super",
            "switch",
            "this",
            "throw",
            "try",
            "typeof",
            "var",
            "void",
            "while",
            "with",
            "yield",
            "let",
            "static",
            "enum",
            "implements",
            "package",
            "protected",
            "interface",
            "private",
            "public",
        ] {
            reserved_names.insert(word.to_string());
        }

        Self { reserved_names }
    }

    /// Generate comprehensive variable mapping from SSA analysis
    pub fn generate_mapping(
        &mut self,
        ssa: &SSAAnalysis,
        cfg: &Cfg,
    ) -> Result<VariableMapping, VariableMapError> {
        let mut mapping = VariableMapping::new();

        // Step 1: Perform phi coalescing to create equivalence classes
        let coalesced_mapping = self.perform_phi_coalescing(ssa)?;

        // Step 2: Generate names for coalesced equivalence classes
        self.generate_coalesced_names(ssa, &coalesced_mapping, &mut mapping)?;

        // Step 3: Determine optimal scopes
        self.determine_scopes(ssa, cfg, &mut mapping)?;

        // Step 4: Build register-to-SSA lookup table
        self.build_lookup_table(ssa, cfg, &mut mapping)?;

        // Step 5: Generate fallback names for any registers not covered by SSA
        self.generate_fallback_names(ssa, cfg, &mut mapping)?;

        Ok(mapping)
    }

    /// Generate a complete variable mapping with no SSA analysis (fallback mode)
    pub fn generate_fallback_mapping(
        &mut self,
        cfg: &Cfg,
    ) -> Result<VariableMapping, VariableMapError> {
        let mut mapping = VariableMapping::new();

        // Track register assignments to generate unique variable names
        let mut register_assignment_count: HashMap<u8, u32> = HashMap::new();

        // Process all blocks to create unique variable names for each register assignment
        for block in cfg.graph().node_weights() {
            for (inst_idx, instruction) in block.instructions().iter().enumerate() {
                let pc = block.start_pc() + inst_idx as u32;
                let usage = crate::generated::instruction_analysis::analyze_register_usage(
                    &instruction.instruction,
                );

                // For each target register (assignment), create a unique variable name
                if let Some(target_reg) = usage.target {
                    let assignment_num = register_assignment_count.entry(target_reg).or_insert(0);
                    *assignment_num += 1;

                    let var_name = if *assignment_num == 1 {
                        // First assignment uses simple name
                        if target_reg == 0 && inst_idx == 0 && pc == 0 {
                            // Likely a parameter
                            format!("arg{}", target_reg)
                        } else {
                            format!("var{}", target_reg)
                        }
                    } else {
                        // Subsequent assignments use letter suffixes
                        let suffix = self.version_to_letters(*assignment_num - 2);
                        format!("var{}_{}", target_reg, suffix)
                    };

                    // Map this specific PC to this variable name
                    mapping.register_at_pc.insert(
                        (target_reg, pc),
                        crate::cfg::ssa::SSAValue {
                            register: target_reg,
                            version: *assignment_num,
                            def_site: crate::cfg::ssa::RegisterDef {
                                register: target_reg,
                                block_id: petgraph::graph::NodeIndex::new(0), // dummy
                                instruction_idx: inst_idx,
                                pc,
                            },
                        },
                    );

                    // Also create the variable name mapping
                    let ssa_value = crate::cfg::ssa::SSAValue {
                        register: target_reg,
                        version: *assignment_num,
                        def_site: crate::cfg::ssa::RegisterDef {
                            register: target_reg,
                            block_id: petgraph::graph::NodeIndex::new(0), // dummy
                            instruction_idx: inst_idx,
                            pc,
                        },
                    };
                    mapping.ssa_to_var.insert(ssa_value, var_name);
                }

                // For source registers, map them to their current variable names
                for source_reg in usage.sources {
                    if let Some(&current_count) = register_assignment_count.get(&source_reg) {
                        if current_count > 0 {
                            // Use the most recent variable name for this register
                            let _var_name = if current_count == 1 {
                                if source_reg == 0 && inst_idx == 0 && pc == 0 {
                                    format!("arg{}", source_reg)
                                } else {
                                    format!("var{}", source_reg)
                                }
                            } else {
                                let suffix = self.version_to_letters(current_count - 2);
                                format!("var{}_{}", source_reg, suffix)
                            };

                            // Create a dummy SSA value for the current version
                            let ssa_value = crate::cfg::ssa::SSAValue {
                                register: source_reg,
                                version: current_count,
                                def_site: crate::cfg::ssa::RegisterDef {
                                    register: source_reg,
                                    block_id: petgraph::graph::NodeIndex::new(0), // dummy
                                    instruction_idx: 0,                           // dummy
                                    pc: 0,                                        // dummy
                                },
                            };

                            // Map this PC to the current variable
                            mapping
                                .register_at_pc
                                .insert((source_reg, pc), ssa_value.clone());
                        }
                    }
                }
            }
        }

        Ok(mapping)
    }

    /// Generate a meaningful variable name for an SSA value
    pub fn generate_name(&self, value: &SSAValue, _ssa: &SSAAnalysis) -> String {
        // Try various heuristics for meaningful names:

        // 1. Function parameters (registers defined at instruction 0)
        // TODO: This heuristic is incorrect - we should check if the instruction
        // at def_site is a LoadParam, not just if it's at PC 0
        // For now, disable this to avoid confusion where r1 is named "arg1"
        // when it's not actually a parameter
        /*
        if value.def_site.instruction_idx == 0 && value.def_site.pc == 0 {
            let param_name = format!("arg{}", value.register);
            if !self.reserved_names.contains(&param_name) {
                return param_name;
            }
        }
        */

        // 2. For now, use descriptive register-based names
        // TODO: Add heuristics for:
        // - String assignments from literal values
        // - Property access patterns
        // - Common variable patterns like counters, iterators

        let base_name = if value.version == 1 {
            // First version can use shorter name
            format!("var{}", value.register)
        } else {
            // Later versions need disambiguation
            format!("var{}_{}", value.register, value.version)
        };

        // Ensure we don't conflict with reserved words
        if self.reserved_names.contains(&base_name) {
            format!("_{}", base_name)
        } else {
            base_name
        }
    }

    /// Determine optimal scope for a variable
    pub fn determine_scope(
        &self,
        _var_name: &str,
        value: &SSAValue,
        ssa: &SSAAnalysis,
        cfg: &Cfg,
    ) -> VariableScope {
        let mut use_blocks = HashSet::new();

        // Find all blocks where this SSA value is used
        for (use_site, def_site) in &ssa.use_def_chains {
            if def_site == &value.def_site {
                use_blocks.insert(use_site.block_id);
            }
        }

        // Check if used in phi functions
        for (block_id, phis) in &ssa.phi_functions {
            for phi in phis {
                if phi.operands.values().any(|v| v == value) {
                    use_blocks.insert(*block_id);
                }
            }
        }

        // Determine scope based on usage pattern
        if use_blocks.len() > 1 || self.crosses_loop_boundary(value, cfg) {
            VariableScope::Function
        } else if use_blocks.len() == 1 {
            VariableScope::Block(value.def_site.block_id)
        } else {
            VariableScope::Function // Conservative default
        }
    }

    /// Perform phi coalescing to group SSA values that should share variable names
    /// Only coalesces SSA values that are actually connected through phi functions
    fn perform_phi_coalescing(
        &mut self,
        ssa: &SSAAnalysis,
    ) -> Result<HashMap<SSAValue, SSAValue>, VariableMapError> {
        let mut union_find = UnionFind::new();

        // Initialize union-find with all SSA values (each starts as its own equivalence class)
        for ssa_value in ssa.ssa_values.values() {
            union_find.make_set(ssa_value.clone());
        }

        // Add phi results to union-find
        for (_, phis) in &ssa.phi_functions {
            for phi in phis {
                union_find.make_set(phi.result.clone());
            }
        }

        // ONLY union SSA values that are connected through phi functions
        // This is the key fix: we only coalesce when there's an actual phi connection
        for (_, phis) in &ssa.phi_functions {
            for phi in phis {
                // Union the phi result with all its operands
                // This creates an equivalence class of values that must share a variable name
                for operand in phi.operands.values() {
                    union_find.union(&phi.result, operand);
                }
            }
        }

        // Build mapping from SSA value to its representative (root)
        // Values that weren't connected by phi functions remain in their own equivalence classes
        let mut coalesced_mapping = HashMap::new();
        for ssa_value in ssa.ssa_values.values() {
            let root = union_find.find(ssa_value);
            coalesced_mapping.insert(ssa_value.clone(), root);
        }

        // Add phi results to the mapping
        for (_, phis) in &ssa.phi_functions {
            for phi in phis {
                let root = union_find.find(&phi.result);
                coalesced_mapping.insert(phi.result.clone(), root);
            }
        }

        Ok(coalesced_mapping)
    }

    /// Generate names for coalesced equivalence classes
    fn generate_coalesced_names(
        &mut self,
        ssa: &SSAAnalysis,
        coalesced_mapping: &HashMap<SSAValue, SSAValue>,
        mapping: &mut VariableMapping,
    ) -> Result<(), VariableMapError> {
        let mut representatives_to_names = HashMap::new();

        // Generate names for each equivalence class representative
        for (ssa_value, representative) in coalesced_mapping {
            if !representatives_to_names.contains_key(representative) {
                // Generate a name for this equivalence class
                let var_name = self.generate_coalesced_name(representative, ssa);
                representatives_to_names.insert(representative.clone(), var_name);
            }

            // All SSA values in the same equivalence class get the same name
            let var_name = representatives_to_names[representative].clone();
            mapping.ssa_to_var.insert(ssa_value.clone(), var_name);
        }

        Ok(())
    }

    /// Generate a meaningful name for a coalesced equivalence class
    fn generate_coalesced_name(&self, representative: &SSAValue, _ssa: &SSAAnalysis) -> String {
        // Use the representative as the basis for naming
        let base_name =
            // TODO: Check if the instruction is LoadParam instead of assuming PC 0
            if false && representative.def_site.instruction_idx == 0 && representative.def_site.pc == 0 {
                // Function parameter
                format!("arg{}", representative.register)
            } else {
                // For variables, use letters to differentiate from SSA intermediate names
                // This ensures that r0_1, r0_2, r0_3 get different names (var0_a, var0_b, var0_c)
                // unless they're actually connected by phi functions
                if representative.version == 1 {
                    format!("var{}", representative.register)
                } else {
                    // Convert version number to letters: 2->a, 3->b, ..., 27->z, 28->aa, 29->ab, etc.
                    let suffix = self.version_to_letters(representative.version - 2);
                    format!("var{}_{}", representative.register, suffix)
                }
            };

        // Ensure we don't conflict with reserved words
        if self.reserved_names.contains(&base_name) {
            format!("_{}", base_name)
        } else {
            base_name
        }
    }

    /// Determine optimal scopes for all variables
    fn determine_scopes(
        &mut self,
        ssa: &SSAAnalysis,
        cfg: &Cfg,
        mapping: &mut VariableMapping,
    ) -> Result<(), VariableMapError> {
        // Analyze where each variable is used
        for (ssa_value, var_name) in &mapping.ssa_to_var {
            match self.determine_scope(var_name, ssa_value, ssa, cfg) {
                VariableScope::Function => {
                    mapping.function_scope_vars.insert(var_name.clone());
                }
                VariableScope::Block(block_id) => {
                    mapping
                        .block_scope_vars
                        .entry(block_id)
                        .or_default()
                        .insert(var_name.clone());
                }
            }
        }

        Ok(())
    }

    /// Build lookup table from (register, pc) to current SSA value
    fn build_lookup_table(
        &mut self,
        ssa: &SSAAnalysis,
        cfg: &Cfg,
        mapping: &mut VariableMapping,
    ) -> Result<(), VariableMapError> {
        // Process each block in execution order
        for block_id in cfg.execution_order() {
            let block = &cfg.graph()[block_id];
            let mut current_versions: HashMap<u8, SSAValue> = HashMap::new();

            // Initialize with phi functions
            if let Some(phis) = ssa.phi_functions.get(&block_id) {
                for phi in phis {
                    current_versions.insert(phi.register, phi.result.clone());
                }
            }

            // Process instructions
            for (inst_idx, _instruction) in block.instructions().iter().enumerate() {
                let pc = block.start_pc() + inst_idx as u32;

                // First, record the BEFORE state for this PC
                // This is used for source operand lookups
                for (register, ssa_value) in &current_versions {
                    mapping
                        .register_before_pc
                        .insert((*register, pc), ssa_value.clone());
                }

                // Update for definitions at this instruction
                for def in &ssa.definitions {
                    if def.block_id == block_id && def.instruction_idx == inst_idx {
                        if let Some(ssa_value) = ssa.ssa_values.get(def) {
                            current_versions.insert(def.register, ssa_value.clone());

                            // IMPORTANT: Record the AFTER state immediately for this register
                            // at this PC. This ensures that when we look up the destination
                            // register, we get the new SSA value.
                            mapping
                                .register_at_pc
                                .insert((def.register, pc), ssa_value.clone());
                        }
                    }
                }

                // Record current versions for OTHER registers at this PC (AFTER state)
                // Skip registers that were just defined - they're already recorded above
                for (register, ssa_value) in &current_versions {
                    if !mapping.register_at_pc.contains_key(&(*register, pc)) {
                        mapping
                            .register_at_pc
                            .insert((*register, pc), ssa_value.clone());
                    }
                }
            }
        }

        Ok(())
    }

    /// Check if an SSA value crosses loop boundaries
    fn crosses_loop_boundary(&self, _ssa_value: &SSAValue, _cfg: &Cfg) -> bool {
        // For now, conservatively assume variables might cross loop boundaries
        // TODO: Implement proper loop analysis
        // This would involve checking if the definition and uses are in different loop nests
        false
    }

    /// Generate fallback names for registers not covered by SSA analysis
    fn generate_fallback_names(
        &mut self,
        ssa: &SSAAnalysis,
        cfg: &Cfg,
        mapping: &mut VariableMapping,
    ) -> Result<(), VariableMapError> {
        // Collect all registers used in the CFG
        let mut all_registers = HashSet::new();
        for block in cfg.graph().node_weights() {
            for instruction in block.instructions() {
                let usage = crate::generated::instruction_analysis::analyze_register_usage(
                    &instruction.instruction,
                );
                if let Some(target) = usage.target {
                    all_registers.insert(target);
                }
                for source in usage.sources {
                    all_registers.insert(source);
                }
            }
        }

        // Find registers not covered by SSA
        let mut ssa_registers = HashSet::new();
        for def in &ssa.definitions {
            ssa_registers.insert(def.register);
        }

        // Generate fallback names for uncovered registers
        for register in all_registers {
            if !ssa_registers.contains(&register) {
                let name = self.generate_simple_register_name(register);
                mapping.fallback_register_names.insert(register, name);
            }
        }

        Ok(())
    }

    /// Generate a simple register name (fallback mode)
    fn generate_simple_register_name(&self, register: u8) -> String {
        let base_name = format!("var{}", register);
        if self.reserved_names.contains(&base_name) {
            format!("_{}", base_name)
        } else {
            base_name
        }
    }

    /// Convert a version number to a letter suffix (a, b, ..., z, aa, ab, ..., zz, aaa, ...)
    fn version_to_letters(&self, version: u32) -> String {
        let mut result = String::new();
        let mut n = version as i32;

        // We use a base-26 system where 0=a, 1=b, ..., 25=z
        // But we want: 0=a, 25=z, 26=aa, 51=az, 52=ba, etc.
        // This is similar to Excel column naming

        loop {
            result.push(((n % 26) as u8 + b'a') as char);
            n = n / 26;
            if n == 0 {
                break;
            }
            n -= 1; // Adjust for 1-based indexing in multi-character sequences
        }

        result.chars().rev().collect()
    }
}

#[derive(Debug)]
pub enum VariableScope {
    Function,
    Block(NodeIndex),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_to_letters() {
        let mapper = VariableMapper::new();

        // Single letters
        assert_eq!(mapper.version_to_letters(0), "a");
        assert_eq!(mapper.version_to_letters(1), "b");
        assert_eq!(mapper.version_to_letters(25), "z");

        // Double letters
        assert_eq!(mapper.version_to_letters(26), "aa");
        assert_eq!(mapper.version_to_letters(27), "ab");
        assert_eq!(mapper.version_to_letters(51), "az");
        assert_eq!(mapper.version_to_letters(52), "ba");
        assert_eq!(mapper.version_to_letters(701), "zz");

        // Triple letters
        assert_eq!(mapper.version_to_letters(702), "aaa");
        assert_eq!(mapper.version_to_letters(703), "aab");
    }
}
