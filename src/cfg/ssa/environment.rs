//! Environment analysis for SSA
//!
//! This module analyzes environment-related instructions to track:
//! - Which registers hold environments
//! - Variable definitions in environments (StoreToEnvironment)
//! - Variable accesses from environments (LoadFromEnvironment)
//! - Function closures and their captured environments

use super::types::*;
use crate::{
    cfg::{Block, Cfg},
    generated::unified_instructions::UnifiedInstruction,
};
use petgraph::graph::NodeIndex;

/// Analyze environment operations in the CFG
pub fn analyze_environments(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), super::SSAError> {
    // Iterate through all blocks
    for (block_id, block) in cfg.graph().node_indices().zip(cfg.graph().node_weights()) {
        analyze_block_environments(block_id, block, analysis)?;
    }

    // Post-process to infer variable names
    infer_variable_names(analysis);

    Ok(())
}

/// Analyze environment operations in a single block
fn analyze_block_environments(
    block_id: NodeIndex,
    block: &Block,
    analysis: &mut SSAAnalysis,
) -> Result<(), super::SSAError> {
    for (inst_idx, hbc_instruction) in block.instructions().iter().enumerate() {
        let pc = block.start_pc() + inst_idx as u32;

        match &hbc_instruction.instruction {
            UnifiedInstruction::CreateEnvironment { operand_0 } => {
                handle_create_environment(*operand_0, analysis);
            }

            UnifiedInstruction::CreateInnerEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_inner_environment(
                    *operand_0,
                    *operand_1,
                    *operand_2 as u32,
                    analysis,
                );
            }

            UnifiedInstruction::GetEnvironment {
                operand_0,
                operand_1,
                ..
            } => {
                handle_get_environment(*operand_0, *operand_1, block_id, inst_idx, analysis);
            }

            UnifiedInstruction::StoreToEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::StoreNPToEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_store_to_environment(
                    *operand_0, *operand_1, *operand_2, block_id, inst_idx, pc, analysis,
                );
            }

            UnifiedInstruction::StoreToEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            }
            | UnifiedInstruction::StoreNPToEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_store_to_environment(
                    *operand_0,
                    *operand_1 as u8,
                    *operand_2,
                    block_id,
                    inst_idx,
                    pc,
                    analysis,
                );
            }

            UnifiedInstruction::LoadFromEnvironment {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_load_from_environment(
                    *operand_0, *operand_1, *operand_2, block_id, inst_idx, pc, analysis,
                );
            }

            UnifiedInstruction::LoadFromEnvironmentL {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_load_from_environment(
                    *operand_0,
                    *operand_1,
                    *operand_2 as u8,
                    block_id,
                    inst_idx,
                    pc,
                    analysis,
                );
            }

            UnifiedInstruction::CreateClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2 as u32, analysis);
            }
            UnifiedInstruction::CreateClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2, analysis);
            }

            UnifiedInstruction::CreateAsyncClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2 as u32, analysis);
            }
            UnifiedInstruction::CreateAsyncClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2, analysis);
            }

            UnifiedInstruction::CreateGeneratorClosure {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2 as u32, analysis);
            }
            UnifiedInstruction::CreateGeneratorClosureLongIndex {
                operand_0,
                operand_1,
                operand_2,
                ..
            } => {
                handle_create_closure(*operand_0, *operand_1, *operand_2, analysis);
            }

            _ => {}
        }
    }

    Ok(())
}

/// Handle CreateEnvironment instruction
fn handle_create_environment(env_reg: u8, analysis: &mut SSAAnalysis) {
    analysis
        .environment_info
        .environment_registers
        .insert(env_reg, EnvironmentType::FunctionScope);
    analysis.environment_info.function_env_register = Some(env_reg);
}

/// Handle CreateInnerEnvironment instruction
fn handle_create_inner_environment(
    dest_reg: u8,
    _parent_reg: u8,
    _size: u32,
    analysis: &mut SSAAnalysis,
) {
    analysis
        .environment_info
        .environment_registers
        .insert(dest_reg, EnvironmentType::BlockScope);
    // TODO: Track parent relationship for inner environments
}

/// Handle GetEnvironment instruction
fn handle_get_environment(
    dest_reg: u8,
    level: u8,
    _block_id: NodeIndex,
    _inst_idx: usize,
    analysis: &mut SSAAnalysis,
) {
    // Mark destination register as holding a parent environment
    analysis
        .environment_info
        .environment_registers
        .insert(dest_reg, EnvironmentType::ParentScope(level));

    // Create pending resolution for subsequent loads
    // Note: We'll need global analysis to resolve the actual source function
    analysis.environment_info.pending_env_resolution.insert(
        dest_reg,
        EnvironmentResolution {
            source_function: analysis.function_id, // Will be updated by global analysis
            original_env_register: dest_reg,
            access_level: level,
        },
    );
}

/// Handle StoreToEnvironment instruction
fn handle_store_to_environment(
    env_reg: u8,
    slot: u8,
    value_reg: u8,
    block_id: NodeIndex,
    inst_idx: usize,
    pc: u32,
    analysis: &mut SSAAnalysis,
) {
    let key = (env_reg, slot);

    // Get or create variable info
    let var_info = analysis
        .environment_info
        .environment_variables
        .entry(key)
        .or_insert_with(|| EnvironmentVariable::new(slot));

    // If this is the first store, record it as a definition
    if var_info.first_store.is_none() {
        var_info.first_store = Some(RegisterDef::new(value_reg, block_id, inst_idx, pc));

        // Add to closure variable declarations
        analysis.closure_variable_declarations.push(ClosureVarDecl {
            env_register: env_reg,
            slot,
            name: format!("local{}", slot), // Placeholder name
            first_assignment: Some(value_reg),
        });
    }

    // Record this store
    var_info
        .stores
        .push(RegisterUse::new(value_reg, block_id, inst_idx, pc));
}

/// Handle LoadFromEnvironment instruction
fn handle_load_from_environment(
    dest_reg: u8,
    env_reg: u8,
    slot: u8,
    block_id: NodeIndex,
    inst_idx: usize,
    pc: u32,
    analysis: &mut SSAAnalysis,
) {
    let key = (env_reg, slot);

    // Get or create variable info
    let var_info = analysis
        .environment_info
        .environment_variables
        .entry(key)
        .or_insert_with(|| EnvironmentVariable::new(slot));

    // Record this load
    var_info
        .loads
        .push(RegisterUse::new(dest_reg, block_id, inst_idx, pc));

    // Check if this is accessing a parent environment
    if let Some(resolution) = analysis
        .environment_info
        .pending_env_resolution
        .get(&env_reg)
    {
        // This is accessing a variable from a parent scope
        // We'll need global analysis to resolve the actual variable name
        analysis.captured_variable_accesses.push(CapturedVarAccess {
            local_register: dest_reg,
            source_function: resolution.source_function,
            source_slot: slot,
            variable_name: format!("outer{}", slot), // Placeholder
            access_type: AccessType::Read,
        });
    }
}

/// Handle CreateClosure instructions
fn handle_create_closure(dest_reg: u8, _env_reg: u8, func_idx: u32, analysis: &mut SSAAnalysis) {
    // Track that this register holds a closure
    analysis
        .environment_info
        .closure_registers
        .insert(dest_reg, func_idx);

    // TODO: Implement logic to handle closure creation
    // The env_reg tells us which environment this closure captures
    // This will be used by global analysis to build function relationships
}

/// Infer variable names from their usage patterns
fn infer_variable_names(analysis: &mut SSAAnalysis) {
    // Collect inference data first to avoid borrow conflicts
    let mut names_to_set = Vec::new();

    for ((env_reg, slot), var_info) in &analysis.environment_info.environment_variables {
        if var_info.suggested_name.is_none() {
            // Try to infer name from first store
            if let Some(first_store) = &var_info.first_store {
                // Check if the value comes from a parameter load
                if let Some(name) = try_infer_from_param(first_store.register, analysis) {
                    names_to_set.push(((*env_reg, *slot), name));
                    continue;
                }

                // Check if the value is a constant
                if let Some(name) = try_infer_from_constant(first_store.register, analysis) {
                    names_to_set.push(((*env_reg, *slot), name));
                    continue;
                }
            }

            // Default naming based on usage count
            if var_info.loads.len() > 5 {
                names_to_set.push(((*env_reg, *slot), format!("local{}", slot)));
            } else if var_info.loads.len() == 1 && var_info.stores.len() == 1 {
                names_to_set.push(((*env_reg, *slot), format!("tmp{}", slot)));
            } else {
                names_to_set.push(((*env_reg, *slot), format!("local{}", slot)));
            }
        }
    }

    // Apply the inferred names
    for ((env_reg, slot), name) in names_to_set {
        if let Some(var_info) = analysis
            .environment_info
            .environment_variables
            .get_mut(&(env_reg, slot))
        {
            var_info.suggested_name = Some(name);
        }
    }

    // Update closure variable declarations with inferred names
    for decl in &mut analysis.closure_variable_declarations {
        let key = (decl.env_register, decl.slot);
        if let Some(var_info) = analysis.environment_info.environment_variables.get(&key) {
            if let Some(name) = &var_info.suggested_name {
                decl.name = name.clone();
            }
        }
    }
}

/// Try to infer variable name from parameter load
fn try_infer_from_param(_register: u8, _analysis: &SSAAnalysis) -> Option<String> {
    // TODO: Trace register back to LoadParam instruction
    // For now, return None
    None
}

/// Try to infer variable name from constant value
fn try_infer_from_constant(_register: u8, _analysis: &SSAAnalysis) -> Option<String> {
    // TODO: Trace register back to LoadConst instruction
    // For now, return None
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hbc::function_table::HbcFunctionInstruction;

    fn _create_test_instruction(instruction: UnifiedInstruction) -> HbcFunctionInstruction {
        HbcFunctionInstruction {
            instruction,
            offset: 0,
            function_index: 0,
            instruction_index: 0,
        }
    }

    #[test]
    fn test_create_environment_tracking() {
        let mut analysis = SSAAnalysis::new(0);
        handle_create_environment(1, &mut analysis);

        assert_eq!(
            analysis.environment_info.environment_registers.get(&1),
            Some(&EnvironmentType::FunctionScope)
        );
        assert_eq!(analysis.environment_info.function_env_register, Some(1));
    }

    #[test]
    fn test_store_to_environment_tracking() {
        let mut analysis = SSAAnalysis::new(0);
        let block_id = NodeIndex::new(0);

        // First store creates a variable
        handle_store_to_environment(1, 0, 5, block_id, 0, 10, &mut analysis);

        let key = (1u8, 0u8);
        assert!(analysis
            .environment_info
            .environment_variables
            .contains_key(&key));

        let var_info = &analysis.environment_info.environment_variables[&key];
        assert!(var_info.first_store.is_some());
        assert_eq!(var_info.stores.len(), 1);
        assert_eq!(analysis.closure_variable_declarations.len(), 1);
    }
}
