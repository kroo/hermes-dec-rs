//! Global SSA analysis for cross-function variable resolution
//!
//! This module coordinates SSA analyses across multiple functions to:
//! - Resolve closure variable references
//! - Build function relationship trees
//! - Track variable names across scope boundaries

use crate::{
    cfg::{ssa::*, Cfg},
    hbc::HbcFile,
};
use std::collections::HashMap;

/// Function relationship information
#[derive(Debug, Clone)]
pub struct FunctionRelationship {
    pub parent_function: u32,
    pub child_function: u32,
    pub captured_env_register: u8,
    pub closure_type: ClosureType,
}

/// Type of closure
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureType {
    Regular,
    Async,
    Generator,
}

/// Information about a captured environment
#[derive(Debug, Clone)]
pub struct CapturedEnvironment {
    pub parent_function: u32,
    pub env_register: u8,
    pub capture_point: RegisterDef,
}

/// Global SSA analyzer that coordinates across functions
pub struct GlobalSSAAnalyzer {
    /// Per-function SSA analyses
    function_analyses: HashMap<u32, SSAAnalysis>,

    /// Function parent-child relationships
    function_relationships: Vec<FunctionRelationship>,

    /// Map function index to its parent
    function_parents: HashMap<u32, u32>,

    /// Map function index to the environment it captures
    function_captured_environments: HashMap<u32, CapturedEnvironment>,

    /// Map function index to its environment register
    function_environments: HashMap<u32, u8>,

    /// Global variable name resolver
    variable_names: HashMap<(u32, u8, u8), String>, // (function, env_reg, slot) -> name
}

/// Result of global analysis
pub struct GlobalAnalysisResult {
    pub analyzer: GlobalSSAAnalyzer,
}

impl GlobalSSAAnalyzer {
    /// Create a new global analyzer
    pub fn new() -> Self {
        Self {
            function_analyses: HashMap::new(),
            function_relationships: Vec::new(),
            function_parents: HashMap::new(),
            function_captured_environments: HashMap::new(),
            function_environments: HashMap::new(),
            variable_names: HashMap::new(),
        }
    }

    /// Analyze all functions in an HBC file
    pub fn analyze(hbc_file: &HbcFile) -> Result<GlobalAnalysisResult, SSAError> {
        let mut analyzer = Self::new();

        // Phase 1: First pass - scan all functions for closure creation
        // This identifies parent-child relationships without needing SSA
        analyzer.scan_closure_relationships(hbc_file)?;

        // Phase 2: Build function parent mapping
        analyzer.build_parent_mapping();

        // Phase 3: Run SSA on each function with parent context
        for func_idx in 0..hbc_file.functions.count() {
            let mut cfg = Cfg::new(hbc_file, func_idx);
            cfg.build();

            let mut ssa = construct_ssa(&cfg, func_idx)?;

            // Extract function environment register
            if let Some(env_reg) = ssa.environment_info.function_env_register {
                analyzer.function_environments.insert(func_idx, env_reg);
            }

            // Update pending environment resolutions with actual source functions
            analyzer.update_environment_resolutions(func_idx, &mut ssa);

            analyzer.function_analyses.insert(func_idx, ssa);
        }

        // Phase 4: Resolve cross-function variable references
        analyzer.resolve_closure_variables()?;

        Ok(GlobalAnalysisResult { analyzer })
    }

    /// Scan all functions for closure creation instructions
    fn scan_closure_relationships(&mut self, hbc_file: &HbcFile) -> Result<(), SSAError> {
        use crate::generated::unified_instructions::UnifiedInstruction;

        for parent_idx in 0..hbc_file.functions.count() {
            if let Ok(function) = hbc_file.functions.get(parent_idx, hbc_file) {
                for instruction in &function.instructions {
                    match &instruction.instruction {
                        UnifiedInstruction::CreateClosure {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2 as u32,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Regular,
                            });
                        }
                        UnifiedInstruction::CreateClosureLongIndex {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Regular,
                            });
                        }
                        UnifiedInstruction::CreateAsyncClosure {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2 as u32,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Async,
                            });
                        }
                        UnifiedInstruction::CreateAsyncClosureLongIndex {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Async,
                            });
                        }
                        UnifiedInstruction::CreateGeneratorClosure {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2 as u32,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Generator,
                            });
                        }
                        UnifiedInstruction::CreateGeneratorClosureLongIndex {
                            operand_1,
                            operand_2,
                            ..
                        } => {
                            self.function_relationships.push(FunctionRelationship {
                                parent_function: parent_idx,
                                child_function: *operand_2,
                                captured_env_register: *operand_1,
                                closure_type: ClosureType::Generator,
                            });
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }

    /// Update environment resolutions with actual source functions
    fn update_environment_resolutions(&self, func_idx: u32, ssa: &mut SSAAnalysis) {
        let resolutions: Vec<_> = ssa
            .environment_info
            .pending_env_resolution
            .iter()
            .map(|(reg, res)| (*reg, res.clone()))
            .collect();

        for (reg, mut resolution) in resolutions {
            if let Some(source_func) =
                self.resolve_function_at_level(func_idx, resolution.access_level)
            {
                resolution.source_function = source_func;
                ssa.environment_info
                    .pending_env_resolution
                    .insert(reg, resolution);
            }
        }
    }

    /// Extract closure relationships from a function's SSA analysis
    fn extract_closure_relationships(&mut self, parent_func: u32, ssa: &SSAAnalysis) {
        for (&dest_reg, &child_func) in &ssa.environment_info.closure_registers {
            // Find the environment register that was passed to this closure
            // This requires looking at the instruction that created the closure
            // For now, we'll assume it's the function's main environment
            let env_reg = ssa.environment_info.function_env_register.unwrap_or(0);

            self.function_relationships.push(FunctionRelationship {
                parent_function: parent_func,
                child_function: child_func,
                captured_env_register: env_reg,
                closure_type: ClosureType::Regular, // TODO: Detect actual type
            });

            self.function_captured_environments.insert(
                child_func,
                CapturedEnvironment {
                    parent_function: parent_func,
                    env_register: env_reg,
                    capture_point: RegisterDef::new(dest_reg, Default::default(), 0, 0), // TODO: Get actual location
                },
            );
        }
    }

    /// Build parent function mapping from relationships
    fn build_parent_mapping(&mut self) {
        for rel in &self.function_relationships {
            self.function_parents
                .insert(rel.child_function, rel.parent_function);
        }
    }

    /// Resolve closure variables across function boundaries
    fn resolve_closure_variables(&mut self) -> Result<(), SSAError> {
        // Iterate through all functions
        for (&func_idx, ssa) in &self.function_analyses {
            // Process captured variable accesses
            for access in &ssa.captured_variable_accesses {
                if let Some(source_func) =
                    self.resolve_function_at_level(func_idx, access.source_slot)
                {
                    // Look up the variable in the source function
                    if let Some(source_ssa) = self.function_analyses.get(&source_func) {
                        // Find the environment register in source function
                        if let Some(source_env) = source_ssa.environment_info.function_env_register
                        {
                            let key = (source_env, access.source_slot);
                            if let Some(var_info) =
                                source_ssa.environment_info.environment_variables.get(&key)
                            {
                                // Found the variable! Store its name globally
                                let name = var_info.suggested_name.clone().unwrap_or_else(|| {
                                    format!("closure_var{}", access.source_slot)
                                });

                                self.variable_names
                                    .insert((source_func, source_env, access.source_slot), name);
                            }
                        }
                    }
                }
            }

            // Process environment variables defined in this function
            for ((env_reg, slot), var_info) in &ssa.environment_info.environment_variables {
                if let Some(name) = &var_info.suggested_name {
                    self.variable_names
                        .insert((func_idx, *env_reg, *slot), name.clone());
                }
            }
        }

        Ok(())
    }

    /// Resolve which function is at a given environment level from current function
    pub fn resolve_function_at_level(&self, current: u32, level: u8) -> Option<u32> {
        let mut func = current;
        let mut remaining = level;

        while remaining > 0 {
            if let Some(&parent) = self.function_parents.get(&func) {
                func = parent;
                remaining -= 1;
            } else {
                return None;
            }
        }

        Some(func)
    }

    /// Get the SSA analysis for a specific function
    pub fn get_function_analysis(&self, func_idx: u32) -> Option<&SSAAnalysis> {
        self.function_analyses.get(&func_idx)
    }

    /// Resolve a variable name from environment access
    pub fn resolve_variable_name(&self, function: u32, env_reg: u8, slot: u8) -> String {
        // First check if we have a direct mapping
        if let Some(name) = self.variable_names.get(&(function, env_reg, slot)) {
            return name.clone();
        }

        // Check if this is accessing a parent environment
        if let Some(ssa) = self.function_analyses.get(&function) {
            if let Some(resolution) = ssa.environment_info.pending_env_resolution.get(&env_reg) {
                // This register came from GetEnvironment
                if let Some(source_func) =
                    self.resolve_function_at_level(function, resolution.access_level)
                {
                    if let Some(source_env) = self.function_environments.get(&source_func) {
                        if let Some(name) =
                            self.variable_names.get(&(source_func, *source_env, slot))
                        {
                            return name.clone();
                        }
                    }
                }
            }
        }

        // Fallback name
        format!("local{}", slot)
    }

    /// Get all functions that are children of a given function
    pub fn get_child_functions(&self, parent: u32) -> Vec<u32> {
        self.function_relationships
            .iter()
            .filter(|rel| rel.parent_function == parent)
            .map(|rel| rel.child_function)
            .collect()
    }

    /// Check if a function is nested within another
    pub fn is_nested_function(&self, func_idx: u32) -> bool {
        self.function_parents.contains_key(&func_idx)
    }

    /// Get the environment variables defined in a function
    pub fn get_function_environment_variables(&self, func_idx: u32) -> Vec<(u8, String)> {
        let mut vars = Vec::new();

        if let Some(ssa) = self.function_analyses.get(&func_idx) {
            if let Some(env_reg) = ssa.environment_info.function_env_register {
                for (&(e_reg, slot), _var_info) in &ssa.environment_info.environment_variables {
                    if e_reg == env_reg {
                        let name = self.resolve_variable_name(func_idx, e_reg, slot);
                        vars.push((slot, name));
                    }
                }
            }
        }

        vars.sort_by_key(|(slot, _)| *slot);
        vars
    }
}

impl GlobalAnalysisResult {
    /// Get the analyzer
    pub fn analyzer(&self) -> &GlobalSSAAnalyzer {
        &self.analyzer
    }

    /// Get mutable reference to the analyzer
    pub fn analyzer_mut(&mut self) -> &mut GlobalSSAAnalyzer {
        &mut self.analyzer
    }
}
