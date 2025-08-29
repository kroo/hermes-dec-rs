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
use std::fmt;
use std::sync::Mutex;

/// Errors that can occur during global SSA analysis
#[derive(Debug)]
pub enum GlobalSSAError {
    /// Function not found when expected
    FunctionNotFound { func_idx: u32, context: String },
    /// Parent function not found when resolving environment
    ParentNotFound { func_idx: u32, parent_level: u8 },
    /// SSA analysis missing for a function that should have been analyzed
    MissingSSAAnalysis { func_idx: u32, context: String },
    /// Environment register missing when expected
    MissingEnvironmentRegister { func_idx: u32 },
    /// Unexpected state in analysis
    UnexpectedState(String),
    /// Wrapped SSA error
    SSAError(SSAError),
}

impl fmt::Display for GlobalSSAError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalSSAError::FunctionNotFound { func_idx, context } => {
                write!(f, "Function {} not found: {}", func_idx, context)
            }
            GlobalSSAError::ParentNotFound {
                func_idx,
                parent_level,
            } => {
                write!(
                    f,
                    "Parent function not found for function {} at level {}",
                    func_idx, parent_level
                )
            }
            GlobalSSAError::MissingSSAAnalysis { func_idx, context } => {
                write!(
                    f,
                    "SSA analysis missing for function {}: {}",
                    func_idx, context
                )
            }
            GlobalSSAError::MissingEnvironmentRegister { func_idx } => {
                write!(f, "Environment register missing for function {}", func_idx)
            }
            GlobalSSAError::UnexpectedState(msg) => {
                write!(f, "Unexpected state: {}", msg)
            }
            GlobalSSAError::SSAError(e) => {
                write!(f, "SSA error: {:?}", e)
            }
        }
    }
}

impl From<SSAError> for GlobalSSAError {
    fn from(e: SSAError) -> Self {
        GlobalSSAError::SSAError(e)
    }
}

impl std::error::Error for GlobalSSAError {}

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

/// Global SSA analyzer that coordinates across functions
pub struct GlobalSSAAnalyzer {
    /// Per-function SSA analyses (using Mutex for lazy analysis)
    pub function_analyses: Mutex<HashMap<u32, SSAAnalysis>>,

    /// Function parent-child relationships
    pub function_relationships: Vec<FunctionRelationship>,

    /// Map function index to its parent
    pub function_parents: HashMap<u32, u32>,

    /// Map function index to its environment register (using Mutex for lazy analysis)
    pub function_environments: Mutex<HashMap<u32, u8>>,

    /// Global variable name resolver
    pub variable_names: HashMap<(u32, u8, u8), String>, // (function, env_reg, slot) -> name

    /// Cached HBC file reference for lazy analysis (stored as raw pointer)
    hbc_file_ptr: Option<*const HbcFile<'static>>,
}

/// Result of global analysis
pub struct GlobalAnalysisResult {
    pub analyzer: GlobalSSAAnalyzer,
    /// If true, this result was created with lazy analysis
    pub is_lazy: bool,
}

impl GlobalSSAAnalyzer {
    /// Create a new global analyzer
    pub fn new() -> Self {
        Self {
            function_analyses: Mutex::new(HashMap::new()),
            function_relationships: Vec::new(),
            function_parents: HashMap::new(),
            function_environments: Mutex::new(HashMap::new()),
            variable_names: HashMap::new(),
            hbc_file_ptr: None,
        }
    }

    /// Analyze all functions in an HBC file (lazy analysis for better performance)
    pub fn analyze(hbc_file: &HbcFile) -> Result<GlobalAnalysisResult, GlobalSSAError> {
        log::debug!("GlobalSSAAnalyzer: Using lazy analysis for better performance");

        // Create a minimal analyzer with just the closure relationships scanned (which is fast)
        let mut analyzer = Self::new();

        // Store HBC file reference for lazy analysis
        // SAFETY: We're storing a raw pointer but the HBC file lifetime is managed by the caller
        // The decompiler ensures the HBC file outlives the GlobalSSAAnalyzer
        analyzer.hbc_file_ptr = Some(hbc_file as *const _ as *const HbcFile<'static>);

        // Phase 1: Scan closure relationships (fast)
        log::debug!("GlobalSSAAnalyzer Phase 1: Scanning closure relationships...");
        analyzer.scan_closure_relationships(hbc_file)?;

        // Phase 2: Build parent mapping (fast)
        log::debug!("GlobalSSAAnalyzer Phase 2: Building parent mapping...");
        analyzer.build_parent_mapping();

        // Phase 3: Functions will be analyzed on-demand
        log::debug!("GlobalSSAAnalyzer Phase 3: Functions will be analyzed on-demand");

        Ok(GlobalAnalysisResult {
            analyzer,
            is_lazy: true,
        })
    }

    /// Ensure a specific function and its parent chain are analyzed (for lazy mode with const self)
    pub fn ensure_function_analyzed_lazy(&self, func_idx: u32) {
        self.ensure_function_analyzed_lazy_impl(func_idx, &mut std::collections::HashSet::new());
    }

    fn ensure_function_analyzed_lazy_impl(
        &self,
        func_idx: u32,
        visited: &mut std::collections::HashSet<u32>,
    ) {
        // Prevent infinite recursion
        if !visited.insert(func_idx) {
            log::warn!(
                "GlobalSSA: Circular dependency detected for function {}",
                func_idx
            );
            return;
        }

        // Check if already analyzed
        {
            let analyses = self.function_analyses.lock().unwrap();
            if analyses.contains_key(&func_idx) {
                return;
            }
        }

        // Get HBC file reference
        let hbc_file = match self.hbc_file_ptr {
            Some(ptr) => unsafe { &*ptr },
            None => {
                panic!("GlobalSSA: INTERNAL ERROR - HBC file pointer not set for lazy analysis");
            }
        };

        // First, analyze parent functions if this is a nested function
        if let Some(parent) = self.function_parents.get(&func_idx).copied() {
            // Recursively ensure parent is analyzed
            self.ensure_function_analyzed_lazy_impl(parent, visited);
        }

        log::info!("GlobalSSA: Lazy analyzing function {} on-demand", func_idx);
        let mut cfg = Cfg::new(hbc_file, func_idx);
        cfg.build();

        let mut ssa = match construct_ssa(&cfg, func_idx) {
            Ok(ssa) => ssa,
            Err(e) => {
                panic!(
                    "GlobalSSA: Failed to construct SSA for function {}: {:?}",
                    func_idx, e
                );
            }
        };

        // Extract function environment register
        if let Some(env_reg) = ssa.environment_info.function_env_register {
            self.function_environments
                .lock()
                .unwrap()
                .insert(func_idx, env_reg);
        }

        // Update pending environment resolutions
        self.update_environment_resolutions(func_idx, &mut ssa);

        // Insert the analyzed function
        self.function_analyses.lock().unwrap().insert(func_idx, ssa);
    }

    /// Scan all functions for closure creation instructions
    fn scan_closure_relationships(&mut self, hbc_file: &HbcFile) -> Result<(), GlobalSSAError> {
        use crate::generated::unified_instructions::UnifiedInstruction;

        for parent_idx in 0..hbc_file.functions.count() {
            match hbc_file.functions.get(parent_idx, hbc_file) {
                Ok(function) => {
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
                Err(e) => {
                    log::warn!(
                        "GlobalSSA: Failed to get function {} for closure scanning: {:?}",
                        parent_idx,
                        e
                    );
                    // Continue scanning other functions
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
            } else {
                log::debug!(
                    "GlobalSSA: Failed to resolve environment for function {} register {} at level {}",
                    func_idx, reg, resolution.access_level
                );
            }
        }
    }

    /// Build parent function mapping from relationships
    fn build_parent_mapping(&mut self) {
        for rel in &self.function_relationships {
            self.function_parents
                .insert(rel.child_function, rel.parent_function);
        }
    }

    /// Resolve which function is at a given environment level from current function
    /// Returns None only if level is 0 (current function) or if this is expected (e.g., top-level function)
    pub fn resolve_function_at_level(&self, current: u32, level: u8) -> Option<u32> {
        if level == 0 {
            return Some(current);
        }

        let mut func = current;
        let mut remaining = level;

        while remaining > 0 {
            if let Some(&parent) = self.function_parents.get(&func) {
                func = parent;
                remaining -= 1;
            } else {
                // This is only expected for top-level functions (function 0)
                // For any other function, missing parent is an error condition
                if func != 0 && level > 0 {
                    // This indicates a bug in our analysis - we're trying to access a parent
                    // environment that doesn't exist
                    panic!(
                        "GlobalSSA: Unexpected missing parent - tried to resolve {} levels up from function {}, \
                        but function {} has no parent. This indicates a bug in environment level tracking.",
                        level, current, func
                    );
                }
                // For function 0 or when we genuinely don't have a parent, return None
                log::trace!(
                    "GlobalSSA: Function {} has no parent (expected for top-level function)",
                    func
                );
                return None;
            }
        }

        Some(func)
    }

    /// Get the SSA analysis for a specific function
    pub fn get_function_analysis(&self, func_idx: u32) -> Option<SSAAnalysis> {
        self.function_analyses
            .lock()
            .unwrap()
            .get(&func_idx)
            .cloned()
    }

    /// Resolve a variable name from environment access
    /// This should always succeed for valid bytecode - analyzes functions on-demand if needed
    pub fn resolve_variable_name(&self, function: u32, env_reg: u8, slot: u8) -> String {
        log::trace!(
            "GlobalSSA: Resolving variable name for function {}, env_reg {}, slot {}",
            function,
            env_reg,
            slot
        );

        // First check if we have a direct mapping
        if let Some(name) = self.variable_names.get(&(function, env_reg, slot)) {
            log::trace!("GlobalSSA: Found direct mapping: {}", name);
            return name.clone();
        }

        // Ensure the function is analyzed (lazy analysis)
        self.ensure_function_analyzed_lazy(function);

        // Check if this is accessing a parent environment
        // We need to extract the info we need and drop the lock before calling ensure_function_analyzed_lazy
        let (source_func, resolution_level) = {
            let function_analyses = self.function_analyses.lock().unwrap();
            if let Some(ssa) = function_analyses.get(&function) {
                if let Some(resolution) = ssa.environment_info.pending_env_resolution.get(&env_reg)
                {
                    // This register came from GetEnvironment - we MUST be able to resolve it
                    let source = self.resolve_function_at_level(function, resolution.access_level);
                    (source, resolution.access_level)
                } else {
                    (None, 0)
                }
            } else {
                // Missing SSA analysis when trying to resolve a variable name is unexpected
                panic!(
                    "GlobalSSA: INTERNAL ERROR - Trying to resolve variable name for function {} but no SSA analysis exists. \
                    This function should have been analyzed before variable resolution.",
                    function
                );
            }
        }; // Lock released here

        if let Some(source_func) = source_func {
            // If source function is the same as current function (level 0),
            // we're accessing our own environment, use the env_reg directly
            if source_func == function {
                // Check if we have a name for this variable using the env_reg we have
                if let Some(name) = self.variable_names.get(&(source_func, env_reg, slot)) {
                    log::trace!("GlobalSSA: Resolved from own environment: {}", name);
                    return name.clone();
                } else {
                    log::trace!(
                        "GlobalSSA: No specific name in own environment for env {} slot {} - using generic name",
                        env_reg, slot
                    );
                }
            } else {
                // Ensure source function is analyzed (no lock held)
                self.ensure_function_analyzed_lazy(source_func);

                // If we found the source function, we MUST have its environment info
                let function_environments = self.function_environments.lock().unwrap();
                let source_env = function_environments.get(&source_func)
                    .expect(&format!(
                        "GlobalSSA: INTERNAL ERROR - Function {} should have environment register but doesn't. \
                        This indicates incomplete SSA analysis.",
                        source_func
                    ));

                // Check if we have a name for this variable
                if let Some(name) = self.variable_names.get(&(source_func, *source_env, slot)) {
                    log::trace!(
                        "GlobalSSA: Resolved from parent function {}: {}",
                        source_func,
                        name
                    );
                    return name.clone();
                } else {
                    // No specific name found - use generic name
                    // This is expected since Hermes doesn't preserve variable names
                    log::trace!(
                        "GlobalSSA: No specific name in parent function {} for env {} slot {} - using generic name",
                        source_func, source_env, slot
                    );
                }
            }
        } else if resolution_level > 0 {
            // Could not resolve parent function at level - this is only expected for top-level
            log::trace!(
                "GlobalSSA: Could not resolve parent function at level {} (expected for top-level)",
                resolution_level
            );
        }

        // Use generic fallback name (this is normal - Hermes doesn't preserve variable names)
        let fallback = format!("local{}", slot);
        log::trace!("GlobalSSA: Using generic name: {}", fallback);
        fallback
    }

    pub fn get_variable_name_for_load(&self, function: u32, env_reg: u8, slot: u8) -> String {
        self.resolve_variable_name(function, env_reg, slot)
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

    /// Get the parent function of a given function
    pub fn get_parent_function(&self, func_idx: u32) -> Option<u32> {
        self.function_parents.get(&func_idx).copied()
    }

    /// Get the environment variables defined in a function
    pub fn get_function_environment_variables(&self, func_idx: u32) -> Vec<(u8, String)> {
        // Ensure function is analyzed
        self.ensure_function_analyzed_lazy(func_idx);

        // Collect slots first
        let mut slots_to_resolve = Vec::new();
        {
            let function_analyses = self.function_analyses.lock().unwrap();

            if let Some(ssa) = function_analyses.get(&func_idx) {
                if let Some(env_reg) = ssa.environment_info.function_env_register {
                    for (&(e_reg, slot), _var_info) in &ssa.environment_info.environment_variables {
                        if e_reg == env_reg {
                            slots_to_resolve.push((e_reg, slot));
                        }
                    }
                } else {
                    log::debug!(
                        "GlobalSSA: Function {} has no environment register",
                        func_idx
                    );
                }
            } else {
                log::debug!("GlobalSSA: No SSA analysis available for function {} when getting environment variables", func_idx);
            }
        }

        // Now resolve names without holding the lock
        let mut vars = Vec::new();
        for (e_reg, slot) in slots_to_resolve {
            let name = self.resolve_variable_name(func_idx, e_reg, slot);
            vars.push((slot, name));
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
