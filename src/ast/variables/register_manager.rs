//! Register management and variable naming system
//!
//! This module provides a pure lookup service for variable names during AST generation.
//! All naming logic has been moved to the VariableMapper for clean separation of concerns.

use super::variable_mapper::VariableMapping;
use crate::cfg::ssa::{DuplicatedSSAValue, DuplicationContext};
use crate::hbc::InstructionIndex;
use std::collections::{HashMap, HashSet};

/// Register lifetime information for optimization
#[derive(Debug, Clone)]
pub struct RegisterLifetime {
    pub first_use: u32,
    pub last_use: u32,
    pub is_parameter: bool,
    pub is_local: bool,
}

/// Pure lookup service for variable names during AST generation
///
/// This is now a clean, simple lookup service that uses pre-computed variable mappings
/// from the VariableMapper. It has no naming logic of its own.
pub struct RegisterManager {
    /// Pre-computed variable mapping from VariableMapper
    variable_mapping: Option<VariableMapping>,
    /// Current program counter for lookup
    current_pc: Option<InstructionIndex>,
    /// Track register lifetime for optimization/statistics
    register_lifetimes: HashMap<u8, RegisterLifetime>,
    /// Current duplication context for switch case processing
    current_duplication_context: Option<DuplicationContext>,
    /// Set of duplicated SSA values that actually exist (have definitions in duplicated blocks)
    existing_duplicated_ssas: HashSet<DuplicatedSSAValue>,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            variable_mapping: None,
            current_pc: None,
            register_lifetimes: HashMap::new(),
            current_duplication_context: None,
            existing_duplicated_ssas: HashSet::new(),
        }
    }

    /// Set the pre-computed variable mapping (from VariableMapper)
    pub fn set_variable_mapping(&mut self, mapping: VariableMapping) {
        self.variable_mapping = Some(mapping);
    }

    /// Set the existing duplicated SSAs from the control flow plan
    pub fn set_existing_duplicated_ssas(&mut self, ssas: HashSet<DuplicatedSSAValue>) {
        self.existing_duplicated_ssas = ssas;
    }

    /// Update the current program counter for variable lookup
    pub fn set_current_pc(&mut self, pc: InstructionIndex) {
        self.current_pc = Some(pc);
    }

    /// Get reference to the variable mapping
    pub fn variable_mapping(&self) -> Option<&VariableMapping> {
        self.variable_mapping.as_ref()
    }

    /// Get the current PC
    pub fn current_pc(&self) -> Option<InstructionIndex> {
        self.current_pc
    }

    /// Set the current duplication context
    pub fn set_duplication_context(&mut self, context: Option<DuplicationContext>) {
        self.current_duplication_context = context;
    }

    /// Get the current variable name for a register (for reading)
    /// Pure lookup - no naming logic
    pub fn get_variable_name(&mut self, register: u8) -> String {
        // First check if we're in a duplication context
        if let Some(ref context) = self.current_duplication_context {
            if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
                // Look for the SSA value at this register/PC
                let ssa_before = mapping.register_before_pc.get(&(register, pc));
                let ssa_at = mapping.register_at_pc.get(&(register, pc));
                let ssa_value = ssa_before.or(ssa_at);

                log::debug!(
                    "Looking up register {} at PC {} in duplication context. SSA before: {:?}, SSA at: {:?}, Using: {:?}",
                    register, pc.value(), ssa_before, ssa_at, ssa_value
                );

                if let Some(ssa_value) = ssa_value {
                    // Create a duplicated SSA value
                    let dup_ssa = DuplicatedSSAValue {
                        original: ssa_value.clone(),
                        duplication_context: Some(context.clone()),
                    };
                    // Get the duplicated variable name
                    let dup_name = mapping.get_variable_name_for_duplicated(&dup_ssa);
                    log::debug!(
                        "Returning duplicated name: {} for SSA {:?}",
                        dup_name,
                        ssa_value
                    );
                    return dup_name;
                } else {
                    // If we don't find an SSA value at this exact PC, try to find the most recent one
                    log::debug!(
                        "No SSA value found at PC {}, searching for most recent definition",
                        pc.value()
                    );

                    // Search backwards for the most recent definition of this register
                    for check_pc in (0..pc.value()).rev() {
                        let check_idx = InstructionIndex::new(check_pc);
                        if let Some(ssa_value) = mapping.register_at_pc.get(&(register, check_idx))
                        {
                            log::debug!("Found SSA value at PC {}: {:?}", check_pc, ssa_value);
                            let dup_ssa = DuplicatedSSAValue {
                                original: ssa_value.clone(),
                                duplication_context: Some(context.clone()),
                            };
                            let dup_name = mapping.get_variable_name_for_duplicated(&dup_ssa);
                            log::debug!(
                                "Returning duplicated name: {} for SSA {:?}",
                                dup_name,
                                ssa_value
                            );
                            return dup_name;
                        }
                    }
                }
            }
        }

        // Fall back to normal lookup if not in duplication context
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(var_name) = mapping.get_source_variable_name(register, pc) {
                return var_name.clone();
            }
        }

        // if we can't find the variable in the variable mapping, panic.
        panic!(
            "Couldn't find mapping for register {} at PC {}",
            register,
            self.current_pc.unwrap()
        )
    }

    /// Get variable name for a source operand (before current instruction)
    /// This prevents self-reference issues in operations like `r1 = r2 - r1`
    pub fn get_source_variable_name(&mut self, register: u8) -> String {
        // First check if we're in a duplication context
        if let Some(ref context) = self.current_duplication_context {
            if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
                // Look for the SSA value at this register/PC (before the instruction)
                if let Some(ssa_value) =
                    mapping.register_before_pc.get(&(register, pc)).or_else(|| {
                        // Try to find the most recent definition before this PC
                        for check_pc in (0..pc.value()).rev() {
                            if let Some(ssa_val) = mapping
                                .register_at_pc
                                .get(&(register, InstructionIndex::new(check_pc)))
                            {
                                return Some(ssa_val);
                            }
                        }
                        None
                    })
                {
                    // Check if a duplicated version of this SSA value actually exists
                    let dup_ssa_with_context = DuplicatedSSAValue {
                        original: ssa_value.clone(),
                        duplication_context: Some(context.clone()),
                    };

                    // Only use the duplicated name if this SSA value is actually defined in a duplicated block
                    let dup_ssa = if self
                        .existing_duplicated_ssas
                        .contains(&dup_ssa_with_context)
                    {
                        // This SSA value is defined in a duplicated block, use the duplicated name
                        dup_ssa_with_context
                    } else {
                        // This SSA value is NOT defined in a duplicated block, use the original name
                        DuplicatedSSAValue::original(ssa_value.clone())
                    };

                    // Get the variable name
                    return mapping.get_variable_name_for_duplicated(&dup_ssa);
                }
            }
        }

        // Fall back to normal lookup if not in duplication context
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(var_name) = mapping.get_source_variable_name(register, pc) {
                return var_name.clone();
            }
        }
        // Fallback to simple naming for backward compatibility
        format!("var{}", register)
    }

    /// Get the current variable name for a register if it exists (for reading)
    /// Returns None if the register has never been assigned to
    pub fn try_get_variable_name(&self, register: u8) -> Option<String> {
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(var_name) = mapping.get_variable_name_with_fallback(register, pc) {
                return Some(var_name.clone());
            }
        }
        None
    }

    /// Get a variable name for reading, with fallback if not defined
    pub fn get_variable_name_for_read(&mut self, register: u8) -> String {
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(var_name) = mapping.get_variable_name_with_fallback(register, pc) {
                return var_name.clone();
            }
        }
        // For undefined registers used in expressions, create a placeholder
        format!("/* undefined var{} */", register)
    }

    /// Create a new variable name when a register is written to (for definitions)
    /// Pure lookup - the name is already pre-computed by VariableMapper
    pub fn create_new_variable_for_register(&mut self, register: u8) -> String {
        // First check if we're in a duplication context
        if let Some(ref context) = self.current_duplication_context {
            if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
                // Look for the SSA value that will be created at this register/PC
                if let Some(ssa_value) = mapping.register_at_pc.get(&(register, pc)) {
                    // Create a duplicated SSA value
                    let dup_ssa = DuplicatedSSAValue {
                        original: ssa_value.clone(),
                        duplication_context: Some(context.clone()),
                    };
                    // Get the duplicated variable name
                    let dup_name = mapping.get_variable_name_for_duplicated(&dup_ssa);
                    log::debug!(
                        "Creating new duplicated variable: {} for register {} at PC {}",
                        dup_name,
                        register,
                        pc.value()
                    );
                    return dup_name;
                }
            }
        }

        // Fall back to normal lookup if not in duplication context
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(var_name) = mapping.get_variable_name_with_fallback(register, pc) {
                return var_name.clone();
            }
        }
        // Fallback to simple naming for backward compatibility
        format!("var{}", register)
    }

    /// Generate a unique temporary variable name
    pub fn generate_temp_var(&mut self) -> String {
        if let Some(mapping) = &mut self.variable_mapping {
            mapping.generate_temp_name()
        } else {
            // Fallback if no mapping available
            "_temp".to_string()
        }
    }

    /// Set register as parameter (for lifetime tracking)
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

    /// Check if the current PC is the first definition of a variable
    pub fn is_first_definition(&self, variable_name: &str) -> bool {
        if let (Some(mapping), Some(pc)) = (&self.variable_mapping, self.current_pc) {
            if let Some(first_def_pc) = mapping.first_definitions.get(variable_name) {
                return *first_def_pc == pc;
            }
        }
        // If we don't have mapping info, assume it's the first definition
        // to ensure we generate valid JavaScript
        true
    }

    /// Check if a variable should be declared as const
    pub fn should_be_const(&self, variable_name: &str) -> bool {
        if let Some(mapping) = &self.variable_mapping {
            return mapping.should_be_const(variable_name);
        }
        false
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
        if let Some(mapping) = &mut self.variable_mapping {
            mapping.register_global_variable(string_index, var_name);
        }
    }

    /// Get a global variable name by string table index
    pub fn get_global_variable(&self, string_index: usize) -> Option<String> {
        if let Some(mapping) = &self.variable_mapping {
            mapping.get_global_variable(string_index).cloned()
        } else {
            None
        }
    }

    /// Check if a string table index represents a known global variable
    pub fn is_global_variable(&self, string_index: usize) -> bool {
        if let Some(mapping) = &self.variable_mapping {
            mapping.is_global_variable(string_index)
        } else {
            false
        }
    }

    /// Get all global variables
    pub fn get_all_globals(&self) -> HashMap<usize, String> {
        if let Some(mapping) = &self.variable_mapping {
            mapping.global_variables.clone()
        } else {
            HashMap::new()
        }
    }

    /// Set a custom variable name for a register (for special cases like environments)
    pub fn set_variable_name(&mut self, register: u8, name: String) {
        if let Some(mapping) = &mut self.variable_mapping {
            mapping.set_fallback_variable_name(register, name);
        }
    }

    /// Get variable name for a duplicated SSA value
    pub fn get_variable_name_for_duplicated(&self, dup_ssa: &DuplicatedSSAValue) -> String {
        if let Some(mapping) = &self.variable_mapping {
            mapping.get_variable_name_for_duplicated(dup_ssa)
        } else {
            // Fallback for cases without mapping
            format!("var{}", dup_ssa.original.register)
        }
    }

    /// Clear all register mappings (useful for function boundaries)
    /// Note: Global variables are preserved across function boundaries
    pub fn reset(&mut self) {
        self.register_lifetimes.clear();
        // Note: We keep variable_mapping as it's function-scoped
    }

    /// Reset everything including variable mapping (for complete cleanup)
    pub fn reset_all(&mut self) {
        self.register_lifetimes.clear();
        self.variable_mapping = None;
        self.current_pc = None;
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
            temp_var_count: if let Some(mapping) = &self.variable_mapping {
                mapping.temp_counter as usize
            } else {
                0
            },
            global_var_count: if let Some(mapping) = &self.variable_mapping {
                mapping.global_variables.len()
            } else {
                0
            },
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
        let rm = RegisterManager::new();

        // Without variable mapping, should return None
        assert_eq!(rm.try_get_variable_name(0), None);
    }

    #[test]
    fn test_parameter_marking() {
        let mut rm = RegisterManager::new();

        // Mark register as parameter
        rm.mark_as_parameter(0, 0);
        let lifetime = rm.get_lifetime(0).unwrap();
        assert!(lifetime.is_parameter);
        assert_eq!(lifetime.first_use, 0);
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

        let stats = rm.get_stats();
        assert_eq!(stats.parameter_count, 2);
        assert_eq!(stats.local_count, 1);
        assert_eq!(stats.temporary_count, 2); // registers 3 and 4
        assert_eq!(stats.single_use_count, 5); // all have first_use == last_use
        assert_eq!(stats.temp_var_count, 0); // no variable mapping set
        assert_eq!(stats.global_var_count, 0);
    }

    #[test]
    fn test_reset() {
        let mut rm = RegisterManager::new();

        rm.mark_as_parameter(0, 0);
        rm.track_usage(1, 5);

        assert!(!rm.tracked_registers().collect::<Vec<_>>().is_empty());

        rm.reset();

        assert!(rm.tracked_registers().collect::<Vec<_>>().is_empty());
    }
}
