use super::types::{PhiFunction, RegisterDef, SSAAnalysis, SSAValue};
use super::SSAError;
use crate::cfg::Cfg;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Place phi functions at appropriate merge points
pub fn place_phi_functions(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), SSAError> {
    // Group definitions by register
    let mut reg_def_blocks: HashMap<u8, HashSet<NodeIndex>> = HashMap::new();
    for def in &analysis.definitions {
        reg_def_blocks
            .entry(def.register)
            .or_default()
            .insert(def.block_id);
    }

    // For each register that's defined in multiple blocks
    for (register, def_blocks) in reg_def_blocks {
        if def_blocks.len() <= 1 {
            continue; // No phi needed for single definition
        }

        // Iteratively add phi functions using worklist algorithm
        let mut worklist: Vec<NodeIndex> = def_blocks.iter().copied().collect();
        let mut has_phi = HashSet::new();

        while let Some(block) = worklist.pop() {
            if let Some(df_blocks) = analysis.dominance_frontiers.get(&block) {
                for &df_block in df_blocks {
                    // Only place phi if register is live at block entry and we haven't placed one yet
                    if !has_phi.contains(&df_block) {
                        if let Some(live_in) = analysis.live_in.get(&df_block) {
                            if live_in.contains(&register) {
                                has_phi.insert(df_block);

                                // Create phi function
                                let def_site = RegisterDef {
                                    register,
                                    block_id: df_block,
                                    instruction_idx: 0, // Phi at block start
                                    pc: cfg.graph()[df_block].start_pc(),
                                };

                                let result = SSAValue {
                                    register,
                                    version: 0, // Will be assigned during renaming
                                    def_site: def_site.clone(),
                                };

                                let phi = PhiFunction::new(register, df_block, result);

                                analysis
                                    .phi_functions
                                    .entry(df_block)
                                    .or_default()
                                    .push(phi);

                                // Phi function is also a definition
                                if !def_blocks.contains(&df_block) {
                                    worklist.push(df_block);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Determine if a phi function is needed for a register at a block
///
/// This implements the standard SSA construction algorithm for phi placement.
/// A phi function is needed for a register R at block B if:
/// 1. The register is live-in at block B (will be used in B or later)
/// 2. There are multiple definitions of R in the CFG
/// 3. Block B is in the dominance frontier of some block that defines R
/// 4. Block B doesn't already have a phi function for R
///
/// The algorithm also handles transitive dominance frontiers for iterative phi placement.
pub fn needs_phi(analysis: &SSAAnalysis, register: u8, block_id: NodeIndex) -> bool {
    // Check if register is live-in at this block
    if let Some(live_in) = analysis.live_in.get(&block_id) {
        if !live_in.contains(&register) {
            return false;
        }
    } else {
        // No liveness info means we can't determine - be conservative
        return false;
    }

    // Get all blocks that define this register
    let def_blocks: std::collections::HashSet<NodeIndex> = analysis
        .definitions
        .iter()
        .filter(|def| def.register == register)
        .map(|def| def.block_id)
        .collect();

    // If there's only one definition block, no phi needed
    if def_blocks.len() <= 1 {
        return false;
    }

    // Check if this block already has a phi function for this register
    if let Some(existing_phis) = analysis.phi_functions.get(&block_id) {
        for phi in existing_phis {
            if phi.register == register {
                // Already has a phi function for this register
                return false;
            }
        }
    }

    // Check if this block is in the dominance frontier of any definition block
    for def_block in &def_blocks {
        if let Some(frontier) = analysis.dominance_frontiers.get(def_block) {
            if frontier.contains(&block_id) {
                return true;
            }
        }
    }

    // Check for transitive dominance frontiers
    // If any block with a phi function for this register has this block in its dominance frontier
    for (phi_block, phis) in &analysis.phi_functions {
        for phi in phis {
            if phi.register == register {
                if let Some(phi_frontier) = analysis.dominance_frontiers.get(phi_block) {
                    if phi_frontier.contains(&block_id) {
                        return true;
                    }
                }
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::ssa::types::RegisterDef;
    use petgraph::graph::NodeIndex;
    use std::collections::HashSet;

    #[test]
    fn test_needs_phi_single_definition() {
        let mut analysis = SSAAnalysis::new(0);
        let block0 = NodeIndex::new(0);
        let block1 = NodeIndex::new(1);

        // Add single definition for register 1
        analysis.definitions.push(RegisterDef::new(1, block0, 0, 0));

        // Add liveness info - register 1 is live-in at block1
        let mut live_in = HashSet::new();
        live_in.insert(1);
        analysis.live_in.insert(block1, live_in);

        // With single definition, no phi needed
        assert!(!needs_phi(&analysis, 1, block1));
    }

    #[test]
    fn test_needs_phi_multiple_definitions_in_frontier() {
        let mut analysis = SSAAnalysis::new(0);
        let block0 = NodeIndex::new(0);
        let block1 = NodeIndex::new(1);
        let block2 = NodeIndex::new(2); // merge block

        // Add multiple definitions for register 1
        analysis.definitions.push(RegisterDef::new(1, block0, 0, 0));
        analysis.definitions.push(RegisterDef::new(1, block1, 0, 1));

        // Add liveness info - register 1 is live-in at merge block
        let mut live_in = HashSet::new();
        live_in.insert(1);
        analysis.live_in.insert(block2, live_in);

        // Set up dominance frontiers - block2 is in frontier of block0 and block1
        let mut frontier0 = HashSet::new();
        frontier0.insert(block2);
        analysis.dominance_frontiers.insert(block0, frontier0);

        let mut frontier1 = HashSet::new();
        frontier1.insert(block2);
        analysis.dominance_frontiers.insert(block1, frontier1);

        // Should need phi at the merge block
        assert!(needs_phi(&analysis, 1, block2));
    }

    #[test]
    fn test_needs_phi_not_live_in() {
        let mut analysis = SSAAnalysis::new(0);
        let block0 = NodeIndex::new(0);
        let block1 = NodeIndex::new(1);
        let block2 = NodeIndex::new(2);

        // Add multiple definitions for register 1
        analysis.definitions.push(RegisterDef::new(1, block0, 0, 0));
        analysis.definitions.push(RegisterDef::new(1, block1, 0, 1));

        // Register 1 is NOT live-in at block2
        analysis.live_in.insert(block2, HashSet::new());

        // Set up dominance frontiers
        let mut frontier0 = HashSet::new();
        frontier0.insert(block2);
        analysis.dominance_frontiers.insert(block0, frontier0);

        // Should NOT need phi because register is not live-in
        assert!(!needs_phi(&analysis, 1, block2));
    }

    #[test]
    fn test_needs_phi_already_has_phi() {
        let mut analysis = SSAAnalysis::new(0);
        let block0 = NodeIndex::new(0);
        let block1 = NodeIndex::new(1);
        let block2 = NodeIndex::new(2);

        // Add multiple definitions for register 1
        analysis.definitions.push(RegisterDef::new(1, block0, 0, 0));
        analysis.definitions.push(RegisterDef::new(1, block1, 0, 1));

        // Add liveness info
        let mut live_in = HashSet::new();
        live_in.insert(1);
        analysis.live_in.insert(block2, live_in);

        // Set up dominance frontiers
        let mut frontier0 = HashSet::new();
        frontier0.insert(block2);
        analysis.dominance_frontiers.insert(block0, frontier0);

        // Add existing phi function for register 1 at block2
        let def_site = RegisterDef::new(1, block2, 0, 2);
        let result = SSAValue::new(1, 1, def_site.clone());
        let phi = PhiFunction::new(1, block2, result);
        analysis.phi_functions.entry(block2).or_default().push(phi);

        // Should NOT need phi because it already has one
        assert!(!needs_phi(&analysis, 1, block2));
    }
}
