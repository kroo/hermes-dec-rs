use super::types::SSAAnalysis;
use super::SSAError;
use crate::{cfg::Cfg, generated::instruction_analysis};
use petgraph::graph::NodeIndex;
use std::collections::HashSet;

/// Compute live variable analysis for the CFG
pub fn compute_liveness(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), SSAError> {
    // Initialize live sets for all blocks
    for block_id in cfg.graph().node_indices() {
        analysis.live_in.insert(block_id, HashSet::new());
        analysis.live_out.insert(block_id, HashSet::new());
    }

    let mut changed = true;
    let mut iteration = 0;
    const MAX_ITERATIONS: usize = 1000; // Prevent infinite loops

    while changed && iteration < MAX_ITERATIONS {
        changed = false;
        iteration += 1;

        // Process blocks in reverse post-order for faster convergence
        let mut block_order: Vec<_> = cfg.graph().node_indices().collect();
        block_order.reverse(); // Simple reverse order - could be improved with proper post-order

        for block_id in block_order {
            let block = &cfg.graph()[block_id];

            // Compute live-out as union of successor live-ins
            let mut new_live_out = HashSet::new();
            for succ in cfg.graph().neighbors(block_id) {
                if let Some(succ_live_in) = analysis.live_in.get(&succ) {
                    new_live_out.extend(succ_live_in);
                }
            }

            // Check if live-out changed
            if new_live_out != analysis.live_out[&block_id] {
                analysis.live_out.insert(block_id, new_live_out.clone());
                changed = true;
            }

            // Compute live-in: live_in = (live_out - defs) ∪ uses
            let mut live = new_live_out;

            // Process instructions in reverse order
            for (_inst_idx, hbc_instruction) in block.instructions().iter().enumerate().rev() {
                let usage =
                    instruction_analysis::analyze_register_usage(&hbc_instruction.instruction);

                // Remove definitions from live set
                if let Some(target_reg) = usage.target {
                    live.remove(&target_reg);
                }

                // Add uses to live set
                for source_reg in usage.sources {
                    live.insert(source_reg);
                }
            }

            // Check if live-in changed
            if live != analysis.live_in[&block_id] {
                analysis.live_in.insert(block_id, live);
                changed = true;
            }
        }
    }

    if iteration >= MAX_ITERATIONS {
        return Err(SSAError::LivenessError(
            "Liveness analysis did not converge".to_string(),
        ));
    }

    Ok(())
}

/// Get registers defined in a block
pub fn get_block_defs(analysis: &SSAAnalysis, block_id: NodeIndex) -> HashSet<u8> {
    analysis
        .definitions
        .iter()
        .filter(|def| def.block_id == block_id)
        .map(|def| def.register)
        .collect()
}

/// Get registers used in a block
pub fn get_block_uses(analysis: &SSAAnalysis, block_id: NodeIndex) -> HashSet<u8> {
    analysis
        .uses
        .iter()
        .filter(|use_site| use_site.block_id == block_id)
        .map(|use_site| use_site.register)
        .collect()
}
