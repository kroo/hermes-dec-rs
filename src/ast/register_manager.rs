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
pub struct RegisterManager {
    /// Map Hermes register numbers to JavaScript variable names
    register_to_var: HashMap<u8, String>,
    /// Track register lifetime for optimization
    register_lifetimes: HashMap<u8, RegisterLifetime>,
    /// Generate unique temporary variable names
    temp_counter: u32,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            register_to_var: HashMap::new(),
            register_lifetimes: HashMap::new(),
            temp_counter: 0,
        }
    }

    /// Get or create a variable name for a register
    pub fn get_variable_name(&mut self, register: u8) -> String {
        if let Some(name) = self.register_to_var.get(&register) {
            name.clone()
        } else {
            let name = self.generate_variable_name(register);
            self.register_to_var.insert(register, name.clone());
            name
        }
    }

    /// Generate a new variable name for a register
    fn generate_variable_name(&mut self, register: u8) -> String {
        // Check if this is a parameter register (typically 0-n)
        if let Some(lifetime) = self.register_lifetimes.get(&register) {
            if lifetime.is_parameter {
                return format!("param{}", register);
            }
        }
        
        // For now, use simple naming scheme
        format!("var{}", register)
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
        let lifetime = self.register_lifetimes.entry(register).or_insert(RegisterLifetime {
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
        let lifetime = self.register_lifetimes.entry(register).or_insert(RegisterLifetime {
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

    /// Clear all register mappings (useful for function boundaries)
    pub fn reset(&mut self) {
        self.register_to_var.clear();
        self.register_lifetimes.clear();
        self.temp_counter = 0;
    }

    /// Get statistics about register usage
    pub fn get_stats(&self) -> RegisterStats {
        let total_registers = self.register_lifetimes.len();
        let parameter_count = self.register_lifetimes.values()
            .filter(|l| l.is_parameter)
            .count();
        let local_count = self.register_lifetimes.values()
            .filter(|l| l.is_local && !l.is_parameter)
            .count();
        let single_use_count = self.register_lifetimes.iter()
            .filter(|(_, lifetime)| lifetime.first_use == lifetime.last_use)
            .count();

        RegisterStats {
            total_registers,
            parameter_count,
            local_count,
            temporary_count: total_registers - parameter_count - local_count,
            single_use_count,
            temp_var_count: self.temp_counter as usize,
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
}