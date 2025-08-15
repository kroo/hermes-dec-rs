use crate::cfg::ssa::variable_analysis::{VariableScope, VariableUsage};
use crate::cfg::ssa::{SSAAnalysis, SSAValue};
use crate::cfg::Cfg;
use crate::hbc::InstructionIndex;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

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
    pub register_at_pc: HashMap<(u8, InstructionIndex), SSAValue>,
    /// Maps (register, pc) to SSA value BEFORE the instruction at that PC
    /// Used for source operand lookups to avoid self-reference issues
    pub register_before_pc: HashMap<(u8, InstructionIndex), SSAValue>,
    pub function_scope_vars: HashSet<String>,
    pub block_scope_vars: HashMap<NodeIndex, HashSet<String>>,
    /// Fallback variable names for registers not covered by SSA
    pub fallback_register_names: HashMap<u8, String>,
    /// Global variable names by string table index
    pub global_variables: HashMap<usize, String>,
    /// Temporary variable counter for generating unique names
    pub temp_counter: u32,
    /// Variable usage information (const vs let, reassignments, etc.)
    pub variable_usage: HashMap<String, VariableUsage>,
    /// Track which PC locations are the first definition of a variable
    pub first_definitions: HashMap<String, InstructionIndex>,
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
            variable_usage: HashMap::new(),
            first_definitions: HashMap::new(),
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
    pub fn get_variable_name_with_fallback(
        &self,
        register: u8,
        pc: InstructionIndex,
    ) -> Option<&String> {
        // First try SSA-based lookup
        if let Some(ssa_value) = self.register_at_pc.get(&(register, pc)) {
            // Look up the coalesced variable name for this SSA value
            if let Some(var_name) = self.ssa_to_var.get(ssa_value) {
                return Some(var_name);
            }
        }

        // Used to be fallback to simple register naming, now panic.
        panic!(
            "Couldn't find mapping for register {} at PC {}",
            register,
            pc.value()
        )
    }

    /// Get variable name for a source operand (before the instruction executes)
    /// This prevents self-reference issues like `let x = y - x` when it should be `let x2 = y - x1`
    pub fn get_source_variable_name(&self, register: u8, pc: InstructionIndex) -> Option<&String> {
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
        // Performance note: This is currently O(n) where n is the number of PC values to check.
        // In practice, this is bounded by the function size and only searches backwards until
        // a definition is found. For most cases, the register_before_pc lookup above will
        // handle it, and this is only a fallback for edge cases.
        //
        // Future optimization: Use BTreeMap for register_at_pc to enable efficient range queries.
        for check_pc in (0..pc.value()).rev() {
            if let Some(ssa_value) = self
                .register_at_pc
                .get(&(register, InstructionIndex::new(check_pc)))
            {
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

    /// Check if a variable should be declared as const
    pub fn should_be_const(&self, var_name: &str) -> bool {
        self.variable_usage
            .get(var_name)
            .map(|usage| usage.should_be_const)
            .unwrap_or(false)
    }

    /// Check if this is the first definition of a variable at this PC
    pub fn is_first_definition(&self, var_name: &str, pc: u32) -> bool {
        self.first_definitions
            .get(var_name)
            .map(|first_pc| first_pc.value() as u32 == pc)
            .unwrap_or(true) // If we don't have info, assume it's the first
    }

    /// Get variable usage information
    pub fn get_variable_usage(&self, var_name: &str) -> Option<&VariableUsage> {
        self.variable_usage.get(var_name)
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
        _cfg: &Cfg,
    ) -> Result<VariableMapping, VariableMapError> {
        // Check if SSA has variable analysis
        let var_analysis = ssa.variable_analysis.as_ref().ok_or_else(|| {
            VariableMapError::NameGeneration("SSA analysis missing variable analysis".to_string())
        })?;

        let mut mapping = VariableMapping::new();

        // Generate names for each equivalence class
        let mut name_counter: HashMap<u8, u32> = HashMap::new();
        let mut class_to_name: HashMap<SSAValue, String> = HashMap::new();

        // Get unique representatives
        let mut representatives = HashSet::new();
        for representative in var_analysis.coalesced_values.values() {
            representatives.insert(representative.clone());
        }

        // Sort representatives for deterministic processing
        let mut sorted_representatives: Vec<SSAValue> = representatives.into_iter().collect();
        sorted_representatives.sort_by_key(|v| (v.register, v.version, v.def_site.instruction_idx));

        // Generate a name for each equivalence class
        for representative in &sorted_representatives {
            let usage = var_analysis.variable_usage.get(representative);
            let name = self.generate_variable_name(representative, usage, &mut name_counter);
            class_to_name.insert(representative.clone(), name.clone());

            // Add to appropriate scope (but not if it's a parameter)
            let is_parameter = usage.map(|u| u.is_parameter).unwrap_or(false);
            if !is_parameter {
                if let Some(scope) = var_analysis.variable_scopes.get(representative) {
                    match scope {
                        VariableScope::Function => {
                            mapping.function_scope_vars.insert(name.clone());
                        }
                        VariableScope::Block(block_id) => {
                            mapping
                                .block_scope_vars
                                .entry(*block_id)
                                .or_default()
                                .insert(name.clone());
                        }
                    }
                }
            }

            // Store variable usage information
            if let Some(usage) = usage {
                mapping.variable_usage.insert(name.clone(), usage.clone());

                // Track the first definition PC
                // Check if this variable has a phi declaration that should be the first definition
                let first_def_pc = if let Some(_phi_decl_block) =
                    find_phi_declaration_block(ssa, representative)
                {
                    // If there's a phi declaration, use the start of that block as first definition
                    InstructionIndex(0) // This will be handled specially in block converter
                } else if let Some(first_pc) = usage.definition_pcs.first() {
                    *first_pc
                } else {
                    continue;
                };
                mapping.first_definitions.insert(name.clone(), first_def_pc);
            }
        }

        // Map all SSA values to their variable names
        for (ssa_value, representative) in &var_analysis.coalesced_values {
            if let Some(name) = class_to_name.get(representative) {
                mapping.ssa_to_var.insert(ssa_value.clone(), name.clone());
            }
        }

        // Build register lookup tables
        for ((reg, pc), ssa_value) in &var_analysis.register_at_pc {
            mapping
                .register_at_pc
                .insert((*reg, *pc), ssa_value.clone());
        }

        for ((reg, pc), ssa_value) in &var_analysis.register_before_pc {
            mapping
                .register_before_pc
                .insert((*reg, *pc), ssa_value.clone());
        }

        Ok(mapping)
    }

    /// Generate a meaningful variable name for an equivalence class
    fn generate_variable_name(
        &self,
        representative: &SSAValue,
        usage: Option<&VariableUsage>,
        name_counter: &mut HashMap<u8, u32>,
    ) -> String {
        // Check if this is a parameter
        if let Some(usage) = usage {
            if usage.is_parameter {
                let param_name = format!("param{}", representative.register);
                if !self.reserved_names.contains(&param_name) {
                    return param_name;
                }
            }
        }

        // Generate register-based name
        let count = name_counter.entry(representative.register).or_insert(0);
        *count += 1;

        let base_name = if *count == 1 {
            format!("var{}", representative.register)
        } else {
            // Use letter suffixes for subsequent variables
            let suffix = self.version_to_letters(*count - 2);
            format!("var{}_{}", representative.register, suffix)
        };

        // Avoid reserved words
        if self.reserved_names.contains(&base_name) {
            format!("_{}", base_name)
        } else {
            base_name
        }
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
            for (inst_idx_in_block, instruction) in block.instructions().iter().enumerate() {
                let instr_idx = block.start_pc() + (inst_idx_in_block as u32);
                let usage = crate::generated::instruction_analysis::analyze_register_usage(
                    &instruction.instruction,
                );

                // For each target register (assignment), create a unique variable name
                if let Some(target_reg) = usage.target {
                    let assignment_num = register_assignment_count.entry(target_reg).or_insert(0);
                    *assignment_num += 1;

                    let var_name = if *assignment_num == 1 {
                        // First assignment uses simple name
                        if target_reg == 0
                            && inst_idx_in_block == 0
                            && instr_idx == InstructionIndex::zero()
                        {
                            // Likely a parameter
                            format!("param{}", target_reg)
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
                        (target_reg, instr_idx.into()),
                        crate::cfg::ssa::SSAValue {
                            register: target_reg,
                            version: *assignment_num,
                            def_site: crate::cfg::ssa::RegisterDef {
                                register: target_reg,
                                block_id: petgraph::graph::NodeIndex::new(0), // dummy
                                instruction_idx: instr_idx,
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
                            instruction_idx: instr_idx,
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
                                if source_reg == 0
                                    && inst_idx_in_block == 0
                                    && instr_idx == InstructionIndex::zero()
                                {
                                    format!("param{}", source_reg)
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
                                    instruction_idx: crate::hbc::InstructionIndex::zero(), // dummy
                                },
                            };

                            // Map this PC to the current variable
                            mapping
                                .register_at_pc
                                .insert((source_reg, instr_idx.into()), ssa_value.clone());
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

/// Find if an SSA value has a phi declaration block
fn find_phi_declaration_block(ssa: &SSAAnalysis, ssa_value: &SSAValue) -> Option<NodeIndex> {
    // Check all phi declarations to see if any match this SSA value's register
    for (_block_id, declarations) in &ssa.phi_variable_declarations {
        for decl in declarations {
            if decl.register == ssa_value.register {
                // This is a simplified check - in practice we might need to verify
                // this is the right SSA value for this phi
                return Some(decl.declaration_block);
            }
        }
    }
    None
}
