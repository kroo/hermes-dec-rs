use crate::hbc::InstructionIndex;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// A definition site for a register
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegisterDef {
    pub register: u8,
    pub block_id: NodeIndex,
    pub instruction_idx: InstructionIndex,
}

impl RegisterDef {
    pub fn new(register: u8, block_id: NodeIndex, instruction_idx: InstructionIndex) -> Self {
        Self {
            register,
            block_id,
            instruction_idx,
        }
    }
}

/// A use site for a register
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegisterUse {
    pub register: u8,
    pub block_id: NodeIndex,
    pub instruction_idx: InstructionIndex,
}

impl RegisterUse {
    pub fn new(register: u8, block_id: NodeIndex, instruction_idx: InstructionIndex) -> Self {
        Self {
            register,
            block_id,
            instruction_idx,
        }
    }
}

/// An SSA value (versioned register)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SSAValue {
    pub register: u8,
    pub version: u32,
    pub def_site: RegisterDef,
}

impl SSAValue {
    pub fn new(register: u8, version: u32, def_site: RegisterDef) -> Self {
        Self {
            register,
            version,
            def_site,
        }
    }

    /// Get a name for this SSA value (e.g., "r1_2")
    pub fn name(&self) -> String {
        format!("r{}_{}", self.register, self.version)
    }
}

/// A phi function at a merge point
#[derive(Debug, Clone)]
pub struct PhiFunction {
    pub register: u8,
    pub block_id: NodeIndex,
    pub operands: HashMap<NodeIndex, SSAValue>,
    pub result: SSAValue,
}

impl PhiFunction {
    pub fn new(register: u8, block_id: NodeIndex, result: SSAValue) -> Self {
        Self {
            register,
            block_id,
            operands: HashMap::new(),
            result,
        }
    }

    /// Add an operand from a predecessor block
    pub fn add_operand(&mut self, predecessor: NodeIndex, value: SSAValue) {
        self.operands.insert(predecessor, value);
    }

    /// Get the number of operands in this phi function
    pub fn operand_count(&self) -> usize {
        self.operands.len()
    }

    /// Format the phi function as a string (e.g. "ɸ @ BlockId#2(r0_1, r0_2)")
    pub fn format_phi_function(&self) -> String {
        let mut operands = Vec::new();
        for value in self.operands.values() {
            operands.push(value.name());
        }
        format!(
            "ɸ @ BlockId#{:?}({}) -> {}",
            self.block_id,
            operands.join(", "),
            self.result.name()
        )
    }
}

/// Type of environment register
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnvironmentType {
    /// Function-level environment created by CreateEnvironment
    FunctionScope,
    /// Block-level environment created by CreateInnerEnvironment
    BlockScope,
    /// Environment obtained via GetEnvironment
    ParentScope(u8), // level
}

/// Information about an environment variable
#[derive(Debug, Clone)]
pub struct EnvironmentVariable {
    pub slot: u8,
    pub first_store: Option<RegisterDef>,
    pub stores: Vec<RegisterUse>,
    pub loads: Vec<RegisterUse>,
    pub suggested_name: Option<String>,
}

impl EnvironmentVariable {
    pub fn new(slot: u8) -> Self {
        Self {
            slot,
            first_store: None,
            stores: Vec::new(),
            loads: Vec::new(),
            suggested_name: None,
        }
    }
}

/// Tracks environment operations within a function
#[derive(Debug, Default, Clone)]
pub struct EnvironmentInfo {
    /// Map environment register to its type
    pub environment_registers: HashMap<u8, EnvironmentType>,

    /// The main function environment register (from CreateEnvironment)
    pub function_env_register: Option<u8>,

    /// Map (env_reg, slot) to variable info
    pub environment_variables: HashMap<(u8, u8), EnvironmentVariable>,

    /// Track which registers hold closure references
    pub closure_registers: HashMap<u8, u32>, // reg -> function_index

    /// Pending environment resolutions from GetEnvironment
    pub pending_env_resolution: HashMap<u8, EnvironmentResolution>,
}

/// Resolution info for GetEnvironment results
#[derive(Debug, Clone)]
pub struct EnvironmentResolution {
    pub source_function: u32,
    pub original_env_register: u8,
    pub access_level: u8,
}

/// Information about a captured variable access
#[derive(Debug, Clone)]
pub struct CapturedVarAccess {
    pub local_register: u8,
    pub source_function: u32,
    pub source_slot: u8,
    pub variable_name: String,
    pub access_type: AccessType,
}

/// Type of variable access
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessType {
    Read,
    Write,
}

/// Information about a closure variable declaration
#[derive(Debug, Clone)]
pub struct ClosureVarDecl {
    pub env_register: u8,
    pub slot: u8,
    pub name: String,
    pub first_assignment: Option<u8>, // register containing initial value
}

/// Complete SSA analysis results
#[derive(Debug, Clone)]
pub struct SSAAnalysis {
    pub function_id: u32,
    pub definitions: Vec<RegisterDef>,
    pub uses: Vec<RegisterUse>,
    pub def_use_chains: HashMap<RegisterDef, Vec<RegisterUse>>,
    pub use_def_chains: HashMap<RegisterUse, RegisterDef>,
    pub live_in: HashMap<NodeIndex, HashSet<u8>>,
    pub live_out: HashMap<NodeIndex, HashSet<u8>>,
    pub dominance_frontiers: HashMap<NodeIndex, HashSet<NodeIndex>>,
    pub phi_functions: HashMap<NodeIndex, Vec<PhiFunction>>,
    pub ssa_values: HashMap<RegisterDef, SSAValue>,

    pub environment_info: EnvironmentInfo,
    pub captured_variable_accesses: Vec<CapturedVarAccess>,
    pub required_closure_variables: HashSet<String>,
    pub closure_variable_declarations: Vec<ClosureVarDecl>,
    pub variable_analysis: Option<super::variable_analysis::VariableAnalysis>,
    pub phi_variable_declarations: HashMap<NodeIndex, Vec<PhiRegisterDeclaration>>,
}

/// Information about a register that needs declaration for a phi function
#[derive(Debug, Clone)]
pub struct PhiRegisterDeclaration {
    pub register: u8,
    pub phi_block: NodeIndex,
    pub declaration_block: NodeIndex,
    /// The SSA value that represents this declaration (for variable mapping)
    pub ssa_value: SSAValue,
}

impl SSAAnalysis {
    pub fn new(function_id: u32) -> Self {
        Self {
            function_id,
            definitions: Vec::new(),
            uses: Vec::new(),
            def_use_chains: HashMap::new(),
            use_def_chains: HashMap::new(),
            live_in: HashMap::new(),
            live_out: HashMap::new(),
            dominance_frontiers: HashMap::new(),
            phi_functions: HashMap::new(),
            ssa_values: HashMap::new(),
            environment_info: EnvironmentInfo::default(),
            captured_variable_accesses: Vec::new(),
            required_closure_variables: HashSet::new(),
            closure_variable_declarations: Vec::new(),
            variable_analysis: None,
            phi_variable_declarations: HashMap::new(),
        }
    }

    /// Get the SSA value for a register that is available before an instruction executes
    /// This is used when looking up operand values
    pub fn get_value_before_instruction(
        &self,
        register: u8,
        instruction_idx: InstructionIndex,
    ) -> Option<&SSAValue> {
        // First check if there's a PHI function at this PC
        // PHI functions conceptually execute before any regular instruction at the same PC
        if let Some(phi_value) = self
            .definitions
            .iter()
            .filter(|def| def.register == register && def.instruction_idx == instruction_idx)
            .find_map(|def| {
                // Check if this definition is from a PHI function
                self.ssa_values.get(def).filter(|ssa_val| {
                    // Check if any block has a PHI function with this result
                    self.phi_functions.values().any(|phis| {
                        phis.iter()
                            .any(|phi| phi.result.def_site == ssa_val.def_site)
                    })
                })
            })
        {
            return Some(phi_value);
        }

        // Otherwise find the most recent definition BEFORE this PC
        self.definitions
            .iter()
            .filter(|def| def.register == register && def.instruction_idx < instruction_idx)
            .max_by_key(|def| def.instruction_idx)
            .and_then(|def| self.ssa_values.get(def))
    }

    /// Get the SSA value for a register after an instruction executes
    /// This includes the definition from the instruction itself
    pub fn get_value_after_instruction(
        &self,
        register: u8,
        instruction_idx: InstructionIndex,
    ) -> Option<&SSAValue> {
        // Find the definition that reaches this program point
        // We want the most recent definition at or before this PC
        self.definitions
            .iter()
            .filter(|def| def.register == register && def.instruction_idx <= instruction_idx)
            .max_by_key(|def| def.instruction_idx)
            .and_then(|def| self.ssa_values.get(def))
    }

    /// Get all phi functions for a block
    pub fn get_phi_functions(&self, block_id: NodeIndex) -> &[PhiFunction] {
        self.phi_functions
            .get(&block_id)
            .map_or(&[], |v| v.as_slice())
    }

    /// Get all definitions of a specific register
    pub fn get_register_definitions(&self, register: u8) -> Vec<&RegisterDef> {
        self.definitions
            .iter()
            .filter(|def| def.register == register)
            .collect()
    }

    /// Get all uses of a specific register
    pub fn get_register_uses(&self, register: u8) -> Vec<&RegisterUse> {
        self.uses
            .iter()
            .filter(|use_site| use_site.register == register)
            .collect()
    }

    /// Get all registers that are defined in the function
    pub fn get_defined_registers(&self) -> HashSet<u8> {
        self.definitions.iter().map(|def| def.register).collect()
    }

    /// Get all registers that are used in the function
    pub fn get_used_registers(&self) -> HashSet<u8> {
        self.uses.iter().map(|use_site| use_site.register).collect()
    }

    /// Check if a register has multiple definitions (needs phi functions)
    pub fn has_multiple_definitions(&self, register: u8) -> bool {
        self.get_register_definitions(register).len() > 1
    }

    /// Get all uses of a specific SSA value
    pub fn get_ssa_value_uses(&self, ssa_value: &SSAValue) -> Vec<&RegisterUse> {
        self.def_use_chains
            .get(&ssa_value.def_site)
            .map(|uses| uses.iter().collect())
            .unwrap_or_default()
    }

    /// Get statistics about the SSA analysis
    pub fn get_stats(&self) -> SSAStats {
        SSAStats {
            total_definitions: self.definitions.len(),
            total_uses: self.uses.len(),
            unique_registers: self
                .get_defined_registers()
                .union(&self.get_used_registers())
                .count(),
            phi_functions: self.phi_functions.values().map(|v| v.len()).sum(),
            blocks_with_phis: self.phi_functions.len(),
        }
    }

    /// Get all SSA values defined in the function
    pub fn all_ssa_values(&self) -> impl Iterator<Item = &SSAValue> {
        self.ssa_values.values()
    }

    /// Get the defining instruction for an SSA value
    pub fn get_defining_instruction(&self, ssa_value: &SSAValue) -> Option<RegisterDef> {
        // The def_site in SSAValue is the RegisterDef that created it
        if self.ssa_values.values().any(|v| v == ssa_value) {
            Some(ssa_value.def_site.clone())
        } else {
            None
        }
    }
}

/// Statistics about SSA analysis
#[derive(Debug, Clone)]
pub struct SSAStats {
    pub total_definitions: usize,
    pub total_uses: usize,
    pub unique_registers: usize,
    pub phi_functions: usize,
    pub blocks_with_phis: usize,
}

impl SSAStats {
    /// Calculate the average number of uses per definition
    pub fn avg_uses_per_def(&self) -> f64 {
        if self.total_definitions == 0 {
            0.0
        } else {
            self.total_uses as f64 / self.total_definitions as f64
        }
    }

    /// Calculate the average number of phi functions per block that has them
    pub fn avg_phis_per_block(&self) -> f64 {
        if self.blocks_with_phis == 0 {
            0.0
        } else {
            self.phi_functions as f64 / self.blocks_with_phis as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::graph::NodeIndex;

    #[test]
    fn test_register_def_creation() {
        let def = RegisterDef::new(5, NodeIndex::new(0), InstructionIndex::new(2));
        assert_eq!(def.register, 5);
        assert_eq!(def.block_id, NodeIndex::new(0));
        assert_eq!(def.instruction_idx, InstructionIndex::new(2));
    }

    #[test]
    fn test_register_use_creation() {
        let use_site = RegisterUse::new(3, NodeIndex::new(1), InstructionIndex::new(1));
        assert_eq!(use_site.register, 3);
        assert_eq!(use_site.block_id, NodeIndex::new(1));
        assert_eq!(use_site.instruction_idx, InstructionIndex::new(1));
    }

    #[test]
    fn test_ssa_value_creation() {
        let def = RegisterDef::new(2, NodeIndex::new(0), InstructionIndex::new(2));
        let value = SSAValue::new(2, 1, def);
        assert_eq!(value.register, 2);
        assert_eq!(value.version, 1);
        assert_eq!(value.name(), "r2_1");
    }

    #[test]
    fn test_phi_function_creation() {
        let def = RegisterDef::new(1, NodeIndex::new(2), InstructionIndex::new(0));
        let result = SSAValue::new(1, 3, def);
        let mut phi = PhiFunction::new(1, NodeIndex::new(2), result);

        assert_eq!(phi.register, 1);
        assert_eq!(phi.block_id, NodeIndex::new(2));
        assert_eq!(phi.operand_count(), 0);

        let def1 = RegisterDef::new(1, NodeIndex::new(0), InstructionIndex::new(1));
        let value1 = SSAValue::new(1, 1, def1);
        phi.add_operand(NodeIndex::new(0), value1);
        assert_eq!(phi.operand_count(), 1);

        let def2 = RegisterDef::new(1, NodeIndex::new(1), InstructionIndex::new(2));
        let value2 = SSAValue::new(1, 2, def2);
        phi.add_operand(NodeIndex::new(1), value2);
        assert_eq!(phi.operand_count(), 2);
    }

    #[test]
    fn test_ssa_analysis_creation() {
        let analysis = SSAAnalysis::new(42);
        assert_eq!(analysis.function_id, 42);
        assert!(analysis.definitions.is_empty());
        assert!(analysis.uses.is_empty());
        assert!(analysis.def_use_chains.is_empty());
    }

    #[test]
    fn test_ssa_analysis_register_queries() {
        let mut analysis = SSAAnalysis::new(0);

        // Add some definitions
        analysis.definitions.push(RegisterDef::new(
            1,
            NodeIndex::new(0),
            InstructionIndex::new(0),
        ));
        analysis.definitions.push(RegisterDef::new(
            1,
            NodeIndex::new(1),
            InstructionIndex::new(1),
        ));
        analysis.definitions.push(RegisterDef::new(
            2,
            NodeIndex::new(0),
            InstructionIndex::new(2),
        ));

        // Add some uses
        analysis.uses.push(RegisterUse::new(
            1,
            NodeIndex::new(1),
            InstructionIndex::new(0),
        ));
        analysis.uses.push(RegisterUse::new(
            2,
            NodeIndex::new(1),
            InstructionIndex::new(1),
        ));

        // Test queries
        assert_eq!(analysis.get_register_definitions(1).len(), 2);
        assert_eq!(analysis.get_register_definitions(2).len(), 1);
        assert_eq!(analysis.get_register_definitions(3).len(), 0);

        assert_eq!(analysis.get_register_uses(1).len(), 1);
        assert_eq!(analysis.get_register_uses(2).len(), 1);
        assert_eq!(analysis.get_register_uses(3).len(), 0);

        assert!(analysis.has_multiple_definitions(1));
        assert!(!analysis.has_multiple_definitions(2));
        assert!(!analysis.has_multiple_definitions(3));

        let defined_regs = analysis.get_defined_registers();
        assert!(defined_regs.contains(&1));
        assert!(defined_regs.contains(&2));
        assert!(!defined_regs.contains(&3));

        let used_regs = analysis.get_used_registers();
        assert!(used_regs.contains(&1));
        assert!(used_regs.contains(&2));
        assert!(!used_regs.contains(&3));
    }

    #[test]
    fn test_ssa_stats() {
        let mut analysis = SSAAnalysis::new(0);

        // Add some data
        analysis.definitions.push(RegisterDef::new(
            1,
            NodeIndex::new(0),
            InstructionIndex::new(0),
        ));
        analysis.definitions.push(RegisterDef::new(
            2,
            NodeIndex::new(0),
            InstructionIndex::new(1),
        ));
        analysis.uses.push(RegisterUse::new(
            1,
            NodeIndex::new(1),
            InstructionIndex::new(0),
        ));
        analysis.uses.push(RegisterUse::new(
            1,
            NodeIndex::new(1),
            InstructionIndex::new(1),
        ));
        analysis.uses.push(RegisterUse::new(
            2,
            NodeIndex::new(1),
            InstructionIndex::new(2),
        ));

        let stats = analysis.get_stats();
        assert_eq!(stats.total_definitions, 2);
        assert_eq!(stats.total_uses, 3);
        assert_eq!(stats.unique_registers, 2);
        assert_eq!(stats.avg_uses_per_def(), 1.5);
    }
}
