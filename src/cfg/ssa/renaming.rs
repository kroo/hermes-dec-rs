use super::types::{RegisterDef, RegisterUse, SSAAnalysis, SSAValue};
use super::SSAError;
use crate::{
    analysis::call_site_analysis::CallSiteAnalysis, cfg::Cfg, generated::instruction_analysis,
};
use petgraph::algo::dominators::Dominators;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Rename registers to SSA form
pub fn rename_to_ssa(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), SSAError> {
    let dominators = cfg
        .analyze_dominators()
        .ok_or_else(|| SSAError::RenamingError("Failed to compute dominators".to_string()))?;

    // Build call site analysis to properly track argument registers
    let call_site_analysis = CallSiteAnalysis::analyze(cfg);

    // Version counters for each register
    let mut version_counters: HashMap<u8, u32> = HashMap::new();

    // Stack of SSA values for each register (for scoped renaming)
    let mut value_stacks: HashMap<u8, Vec<SSAValue>> = HashMap::new();

    // Find entry block (first block in graph)
    let entry_block = cfg
        .graph()
        .node_indices()
        .next()
        .ok_or_else(|| SSAError::RenamingError("Empty CFG".to_string()))?;

    // Start recursive renaming from entry block
    rename_block(
        entry_block,
        cfg,
        analysis,
        &dominators,
        &mut version_counters,
        &mut value_stacks,
        &call_site_analysis,
    )?;

    Ok(())
}

/// Get the current SSA value for a register in a block
pub fn get_current_value(
    analysis: &SSAAnalysis,
    register: u8,
    _block_id: NodeIndex,
) -> Option<&SSAValue> {
    // Find the most recent definition of this register that reaches this block
    // This is a simplified implementation - the full version would use the value stacks
    analysis
        .definitions
        .iter()
        .filter(|def| def.register == register)
        .filter_map(|def| analysis.ssa_values.get(def))
        .last() // Simplified - should use proper dominance
}

/// Recursively rename registers in a block and its dominated children
fn rename_block(
    block_id: NodeIndex,
    cfg: &Cfg,
    analysis: &mut SSAAnalysis,
    dominators: &Dominators<NodeIndex>,
    version_counters: &mut HashMap<u8, u32>,
    value_stacks: &mut HashMap<u8, Vec<SSAValue>>,
    call_site_analysis: &CallSiteAnalysis,
) -> Result<(), SSAError> {
    let mut new_values = Vec::new();

    // Process phi functions at block start
    if let Some(phis) = analysis.phi_functions.get_mut(&block_id) {
        for phi in phis {
            let version = version_counters.entry(phi.register).or_insert(0);
            *version += 1;

            phi.result.version = *version;
            // PHI functions logically execute before any instruction in the block
            // Use a special marker to distinguish them from regular instructions
            // We subtract 1 from the block start PC to ensure PHI definitions
            // don't collide with instruction definitions at the same PC
            phi.result.def_site.instruction_idx =
                cfg.graph()[block_id].start_pc().saturating_sub(1);

            let ssa_value = phi.result.clone();
            analysis
                .ssa_values
                .insert(phi.result.def_site.clone(), ssa_value.clone());

            value_stacks
                .entry(phi.register)
                .or_default()
                .push(ssa_value.clone());
            new_values.push((phi.register, ssa_value));
        }
    }

    // Process instructions in block
    let block = &cfg.graph()[block_id];
    for (inst_idx, hbc_instruction) in block.instructions().iter().enumerate() {
        let pc = block.start_pc() + inst_idx as u32;
        let mut usage = instruction_analysis::analyze_register_usage(&hbc_instruction.instruction);

        // Check if this instruction has call site info (for proper argument tracking)
        if let Some(call_info) = call_site_analysis
            .call_sites
            .get(&(block_id, hbc_instruction.instruction_index))
        {
            // Add all argument registers from the call site analysis
            for &arg_reg in &call_info.argument_registers {
                if !usage.sources.contains(&arg_reg) {
                    usage.sources.push(arg_reg);
                }
            }
        }

        // Rename uses (build use-def chains)
        for source_reg in usage.sources {
            if let Some(stack) = value_stacks.get(&source_reg) {
                if let Some(ssa_value) = stack.last() {
                    let use_site = RegisterUse {
                        register: source_reg,
                        block_id,
                        instruction_idx: pc,
                    };
                    analysis
                        .use_def_chains
                        .insert(use_site.clone(), ssa_value.def_site.clone());

                    // Build def-use chains
                    analysis
                        .def_use_chains
                        .entry(ssa_value.def_site.clone())
                        .or_default()
                        .push(use_site);
                }
            }
        }

        // Rename definition
        if let Some(target_reg) = usage.target {
            let version = version_counters.entry(target_reg).or_insert(0);
            *version += 1;

            let def_site = RegisterDef {
                register: target_reg,
                block_id,
                instruction_idx: pc,
            };

            let ssa_value = SSAValue {
                register: target_reg,
                version: *version,
                def_site: def_site.clone(),
            };

            analysis.ssa_values.insert(def_site, ssa_value.clone());
            value_stacks
                .entry(target_reg)
                .or_default()
                .push(ssa_value.clone());
            new_values.push((target_reg, ssa_value));
        }
    }

    // Fill in phi operands for successors
    for succ in cfg.graph().neighbors(block_id) {
        if let Some(phis) = analysis.phi_functions.get_mut(&succ) {
            for phi in phis {
                if let Some(stack) = value_stacks.get(&phi.register) {
                    if let Some(ssa_value) = stack.last() {
                        phi.add_operand(block_id, ssa_value.clone());
                    }
                }
            }
        }
    }

    // Process dominated children
    for child in cfg.graph().node_indices() {
        if dominators.immediate_dominator(child) == Some(block_id) {
            rename_block(
                child,
                cfg,
                analysis,
                dominators,
                version_counters,
                value_stacks,
                call_site_analysis,
            )?;
        }
    }

    // Pop values added in this block (restore scope)
    for (register, _) in new_values.iter().rev() {
        if let Some(stack) = value_stacks.get_mut(register) {
            stack.pop();
        }
    }

    Ok(())
}
