//! Register management and variable naming system
//!
//! This module handles the mapping of Hermes virtual registers to JavaScript variable names,
//! including lifetime analysis and optimization for register usage patterns.

use std::collections::HashMap;

/// Register lifetime information for optimization
#[derive(Debug, Clone)]
pub struct RegisterLifetime {
    pub first_use: u32,
    pub last_use: u32,
    pub is_parameter: bool,
    pub is_local: bool,
}

/// Manages virtual register mapping to JavaScript variables
///
/// TODO (AST-03): Current SSA-like implementation has limitations with control flow
///
/// CURRENT LIMITATIONS:
/// The current implementation uses naive SSA-like naming (var0_1, var0_2, etc.) that works
/// well for straight-line code but has issues with complex control flow:
///
/// 1. **Phi Functions Missing**: When control flow merges (e.g., after if/else), we need
///    phi functions to handle cases where different versions of a register are available
///    from different predecessors:
///    ```js
///    if (condition) {
///        let var0_1 = "branch1";
///    } else {
///        let var0_2 = "branch2";
///    }
///    // Here we need: let var0_3 = φ(var0_1, var0_2)
///    console.log(var0_3); // Which version to use?
///    ```
///
/// 2. **Cross-block Variable Resolution**: Variables defined in one block may be used in
///    multiple successor blocks, but our current per-block processing doesn't track this.
///
/// 3. **Loop Variable Handling**: Loop-carried dependencies need special handling:
///    ```js
///    let var0_1 = initial;
///    while (condition) {
///        let var0_2 = φ(var0_1, var0_3); // Loop entry phi
///        let var0_3 = transform(var0_2);  // Loop body update
///    }
///    ```
///
/// PROPER SOLUTION (AST-03):
/// To fix this correctly, we need:
///
/// 1. **Dominance Analysis**: Identify which blocks dominate others to determine where
///    variables are live and visible.
///
/// 2. **Live Variable Analysis**: Track which registers are live at each program point
///    and across block boundaries.
///
/// 3. **Phi Function Insertion**: Insert phi functions at merge points where multiple
///    definitions of the same register are available from different predecessors.
///
/// 4. **Variable Renaming with CFG Context**: Instead of naive sequential naming,
///    use CFG-aware renaming that ensures variables are accessible where needed.
///
/// 5. **Structured Control Flow Recovery**: Convert low-level CFG with phi functions
///    back to high-level constructs (if/else, loops) where possible to avoid explicit
///    phi functions in the final output.
///
/// IMPLEMENTATION STRATEGY:
/// - Phase 1: Build complete CFG with dominance information
/// - Phase 2: Insert phi functions at join points
/// - Phase 3: Rename variables with SSA construction algorithm
/// - Phase 4: Recover structured control flow (if/while/for)
/// - Phase 5: Emit JavaScript with proper variable scoping
///
/// For now, the current implementation works well for function-level code with simple
/// control flow, but will need this upgrade for complex control structures.
pub struct RegisterManager {
    /// Map Hermes register numbers to JavaScript variable names
    register_to_var: HashMap<u8, String>,
    /// Track register lifetime for optimization
    register_lifetimes: HashMap<u8, RegisterLifetime>,
    /// Generate unique temporary variable names
    temp_counter: u32,
    /// Track global variable names by string table index
    global_variables: HashMap<usize, String>,
    /// Track current version of each register for SSA-like naming
    /// TODO (AST-03): Replace with proper SSA construction with phi functions
    register_versions: HashMap<u8, u32>,
    /// Track the current variable name for each register at current PC
    /// TODO (AST-03): Replace with CFG-aware variable resolution
    current_register_vars: HashMap<u8, String>,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            register_to_var: HashMap::new(),
            register_lifetimes: HashMap::new(),
            temp_counter: 0,
            global_variables: HashMap::new(),
            register_versions: HashMap::new(),
            current_register_vars: HashMap::new(),
        }
    }

    /// Get the current variable name for a register (for reading)
    pub fn get_variable_name(&mut self, register: u8) -> String {
        if let Some(name) = self.current_register_vars.get(&register) {
            name.clone()
        } else {
            // First use of this register, create initial version (version 0)
            let name = self.generate_variable_name(register, 0);
            self.current_register_vars.insert(register, name.clone());
            // Initialize the version counter to 0 for this register
            self.register_versions.insert(register, 0);
            name
        }
    }

    /// Get the current variable name for a register if it exists (for reading)
    /// Returns None if the register has never been assigned to
    pub fn try_get_variable_name(&self, register: u8) -> Option<String> {
        self.current_register_vars.get(&register).cloned()
    }

    /// Get a variable name for reading, with fallback if not defined
    pub fn get_variable_name_for_read(&mut self, register: u8) -> String {
        if let Some(name) = self.current_register_vars.get(&register) {
            name.clone()
        } else {
            // For undefined registers used in expressions, create a placeholder that indicates this is an error
            // This helps identify when we're using undefined registers in function arguments
            format!("/* undefined var{} */", register)
        }
    }

    /// Create a new variable name when a register is written to (for definitions)
    pub fn create_new_variable_for_register(&mut self, register: u8) -> String {
        // Increment the version for this register
        let version = *self.register_versions.entry(register).or_insert(0) + 1;
        self.register_versions.insert(register, version);

        // Generate new variable name with version
        let name = self.generate_variable_name(register, version);

        // Update current variable for this register
        self.current_register_vars.insert(register, name.clone());

        name
    }

    /// Generate a new variable name for a register with version
    fn generate_variable_name(&mut self, register: u8, version: u32) -> String {
        // Check if this is a parameter register (typically 0-n)
        if let Some(lifetime) = self.register_lifetimes.get(&register) {
            if lifetime.is_parameter {
                if version == 0 {
                    return format!("param{}", register);
                } else {
                    return format!("param{}_{}", register, version);
                }
            }
        }

        // Use SSA-like naming: var0_0, var0_1, var0_2, etc.
        if version == 0 {
            format!("var{}", register)
        } else {
            format!("var{}_{}", register, version)
        }
    }

    /// Generate a unique temporary variable name
    pub fn generate_temp_var(&mut self) -> String {
        self.temp_counter += 1;
        format!("_temp{}", self.temp_counter)
    }

    /// Set register as parameter
    pub fn mark_as_parameter(&mut self, register: u8, first_use: u32) {
        self.register_lifetimes.insert(
            register,
            RegisterLifetime {
                first_use,
                last_use: first_use,
                is_parameter: true,
                is_local: false,
            },
        );
    }

    /// Track register usage at a specific PC for lifetime analysis
    pub fn track_usage(&mut self, register: u8, pc: u32) {
        let lifetime = self
            .register_lifetimes
            .entry(register)
            .or_insert(RegisterLifetime {
                first_use: pc,
                last_use: pc,
                is_parameter: false,
                is_local: false,
            });

        if pc < lifetime.first_use {
            lifetime.first_use = pc;
        }
        if pc > lifetime.last_use {
            lifetime.last_use = pc;
        }
    }

    /// Mark a register as a local variable (not parameter, not temporary)
    pub fn mark_as_local(&mut self, register: u8, first_use: u32) {
        let lifetime = self
            .register_lifetimes
            .entry(register)
            .or_insert(RegisterLifetime {
                first_use,
                last_use: first_use,
                is_parameter: false,
                is_local: true,
            });

        if !lifetime.is_parameter {
            lifetime.is_local = true;
        }
    }

    /// Get register lifetime information if available
    pub fn get_lifetime(&self, register: u8) -> Option<&RegisterLifetime> {
        self.register_lifetimes.get(&register)
    }

    /// Check if a register is single-use (optimization opportunity)
    pub fn is_single_use(&self, register: u8) -> bool {
        if let Some(lifetime) = self.register_lifetimes.get(&register) {
            lifetime.first_use == lifetime.last_use
        } else {
            false
        }
    }

    /// Get all tracked registers
    pub fn tracked_registers(&self) -> impl Iterator<Item = u8> + '_ {
        self.register_lifetimes.keys().copied()
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

    /// Get all global variables
    pub fn get_all_globals(&self) -> &HashMap<usize, String> {
        &self.global_variables
    }

    /// Set a custom variable name for a register (for special cases like environments)
    pub fn set_variable_name(&mut self, register: u8, name: String) {
        self.register_to_var.insert(register, name);
    }

    /// Clear all register mappings (useful for function boundaries)
    /// Note: Global variables are preserved across function boundaries
    pub fn reset(&mut self) {
        self.register_to_var.clear();
        self.register_lifetimes.clear();
        self.temp_counter = 0;
        // Note: We keep global_variables as they persist across functions
    }

    /// Reset everything including global variables (for complete cleanup)
    pub fn reset_all(&mut self) {
        self.register_to_var.clear();
        self.register_lifetimes.clear();
        self.temp_counter = 0;
        self.global_variables.clear();
    }

    /// Get statistics about register usage
    pub fn get_stats(&self) -> RegisterStats {
        let total_registers = self.register_lifetimes.len();
        let parameter_count = self
            .register_lifetimes
            .values()
            .filter(|l| l.is_parameter)
            .count();
        let local_count = self
            .register_lifetimes
            .values()
            .filter(|l| l.is_local && !l.is_parameter)
            .count();
        let single_use_count = self
            .register_lifetimes
            .iter()
            .filter(|(_, lifetime)| lifetime.first_use == lifetime.last_use)
            .count();

        RegisterStats {
            total_registers,
            parameter_count,
            local_count,
            temporary_count: total_registers - parameter_count - local_count,
            single_use_count,
            temp_var_count: self.temp_counter as usize,
            global_var_count: self.global_variables.len(),
        }
    }
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about register usage patterns
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegisterStats {
    pub total_registers: usize,
    pub parameter_count: usize,
    pub local_count: usize,
    pub temporary_count: usize,
    pub single_use_count: usize,
    pub temp_var_count: usize,
    pub global_var_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_manager_basic_functionality() {
        let mut rm = RegisterManager::new();

        // Test variable name generation
        let var1 = rm.get_variable_name(0);
        let var2 = rm.get_variable_name(1);
        assert_eq!(var1, "var0");
        assert_eq!(var2, "var1");

        // Test consistent naming
        let var1_again = rm.get_variable_name(0);
        assert_eq!(var1, var1_again);
    }

    #[test]
    fn test_parameter_naming() {
        let mut rm = RegisterManager::new();

        // Mark register as parameter
        rm.mark_as_parameter(0, 0);
        let param_name = rm.get_variable_name(0);
        assert_eq!(param_name, "param0");
    }

    #[test]
    fn test_temp_variable_generation() {
        let mut rm = RegisterManager::new();

        let temp1 = rm.generate_temp_var();
        let temp2 = rm.generate_temp_var();
        assert_eq!(temp1, "_temp1");
        assert_eq!(temp2, "_temp2");
    }

    #[test]
    fn test_lifetime_tracking() {
        let mut rm = RegisterManager::new();

        // Track usage
        rm.track_usage(5, 10);
        rm.track_usage(5, 20);
        rm.track_usage(5, 5); // Earlier use

        let lifetime = rm.get_lifetime(5).unwrap();
        assert_eq!(lifetime.first_use, 5);
        assert_eq!(lifetime.last_use, 20);
        assert!(!lifetime.is_parameter);
        assert!(!lifetime.is_local);
    }

    #[test]
    fn test_single_use_detection() {
        let mut rm = RegisterManager::new();

        // Single use register
        rm.track_usage(10, 15);
        assert!(rm.is_single_use(10));

        // Multi-use register
        rm.track_usage(11, 15);
        rm.track_usage(11, 20);
        assert!(!rm.is_single_use(11));
    }

    #[test]
    fn test_register_stats() {
        let mut rm = RegisterManager::new();

        rm.mark_as_parameter(0, 0);
        rm.mark_as_parameter(1, 0);
        rm.mark_as_local(2, 5);
        rm.track_usage(3, 10); // temporary
        rm.track_usage(4, 15); // single use
        let _temp = rm.generate_temp_var();

        let stats = rm.get_stats();
        assert_eq!(stats.parameter_count, 2);
        assert_eq!(stats.local_count, 1);
        assert_eq!(stats.temporary_count, 2); // registers 3 and 4
        assert_eq!(stats.single_use_count, 5); // registers 0, 1, 2, 3, and 4 (all have first_use == last_use)
        assert_eq!(stats.temp_var_count, 1);
        assert_eq!(stats.global_var_count, 0);
    }

    #[test]
    fn test_reset() {
        let mut rm = RegisterManager::new();

        rm.mark_as_parameter(0, 0);
        rm.generate_temp_var();
        rm.track_usage(1, 5);

        assert!(!rm.tracked_registers().collect::<Vec<_>>().is_empty());

        rm.reset();

        assert!(rm.tracked_registers().collect::<Vec<_>>().is_empty());
        let new_temp = rm.generate_temp_var();
        assert_eq!(new_temp, "_temp1"); // Counter reset
    }

    #[test]
    fn test_global_variable_tracking() {
        let mut rm = RegisterManager::new();

        // Register global variables
        rm.register_global_variable(5, "testVar".to_string());
        rm.register_global_variable(10, "anotherGlobal".to_string());

        // Test retrieval
        assert_eq!(rm.get_global_variable(5), Some(&"testVar".to_string()));
        assert_eq!(
            rm.get_global_variable(10),
            Some(&"anotherGlobal".to_string())
        );
        assert_eq!(rm.get_global_variable(15), None);

        // Test is_global_variable
        assert!(rm.is_global_variable(5));
        assert!(rm.is_global_variable(10));
        assert!(!rm.is_global_variable(15));

        // Test stats include globals
        let stats = rm.get_stats();
        assert_eq!(stats.global_var_count, 2);

        // Test that reset preserves globals but reset_all clears them
        rm.reset();
        assert_eq!(rm.global_variables.len(), 2);

        rm.reset_all();
        assert_eq!(rm.global_variables.len(), 0);
    }
}
