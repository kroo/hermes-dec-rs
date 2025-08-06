//! Static Single Assignment (SSA) form construction for the CFG

pub mod environment;
pub mod frontiers;
pub mod liveness;
pub mod phi_placement;
pub mod renaming;
pub mod types;

pub use types::*;

use crate::{cfg::Cfg, generated::instruction_analysis};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SSAError {
    #[error("Liveness analysis failed: {0}")]
    LivenessError(String),
    #[error("Dominance frontier computation failed: {0}")]
    FrontierError(String),
    #[error("Phi placement failed: {0}")]
    PhiPlacementError(String),
    #[error("SSA renaming failed: {0}")]
    RenamingError(String),
    #[error("Register analysis failed: {0}")]
    RegisterAnalysisError(String),
}

/// Main entry point for SSA construction
pub fn construct_ssa(cfg: &Cfg, function_id: u32) -> Result<SSAAnalysis, SSAError> {
    let mut analysis = SSAAnalysis::new(function_id);

    // Step 1: Collect definitions and uses
    collect_defs_and_uses(cfg, &mut analysis)?;

    // Step 2: Compute liveness
    liveness::compute_liveness(cfg, &mut analysis)?;

    // Step 3: Compute dominance frontiers
    frontiers::compute_dominance_frontiers(cfg, &mut analysis)?;

    // Step 4: Place phi functions
    phi_placement::place_phi_functions(cfg, &mut analysis)?;

    // Step 5: Rename to SSA
    renaming::rename_to_ssa(cfg, &mut analysis)?;

    // Step 6: Analyze environment operations
    environment::analyze_environments(cfg, &mut analysis)?;

    Ok(analysis)
}

/// Collect all register definitions and uses from the CFG
fn collect_defs_and_uses(cfg: &Cfg, analysis: &mut SSAAnalysis) -> Result<(), SSAError> {
    // Iterate through all blocks in the CFG
    for (block_id, block) in cfg.graph().node_indices().zip(cfg.graph().node_weights()) {
        // Process each instruction in the block
        for (inst_idx, hbc_instruction) in block.instructions().iter().enumerate() {
            let pc = block.start_pc() + inst_idx as u32;

            // Analyze register usage for this instruction
            let usage = instruction_analysis::analyze_register_usage(&hbc_instruction.instruction);

            // Record the definition (target register)
            if let Some(target_reg) = usage.target {
                let def = RegisterDef {
                    register: target_reg,
                    block_id,
                    instruction_idx: inst_idx,
                    pc,
                };
                analysis.definitions.push(def);
            }

            // Record the uses (source registers)
            for source_reg in usage.sources {
                let use_site = RegisterUse {
                    register: source_reg,
                    block_id,
                    instruction_idx: inst_idx,
                    pc,
                };
                analysis.uses.push(use_site);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::Block;
    use crate::generated::unified_instructions::UnifiedInstruction;
    use crate::hbc::function_table::HbcFunctionInstruction;

    fn create_test_instruction(instruction: UnifiedInstruction) -> HbcFunctionInstruction {
        HbcFunctionInstruction {
            instruction,
            offset: 0,
            function_index: 0,
            instruction_index: 0,
        }
    }

    #[test]
    fn test_collect_defs_and_uses_simple() {
        // Test just the def/use collection logic without full CFG
        let instructions = vec![
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 1,
                operand_1: 42,
            }),
            create_test_instruction(UnifiedInstruction::LoadConstUInt8 {
                operand_0: 2,
                operand_1: 10,
            }),
            create_test_instruction(UnifiedInstruction::Add {
                operand_0: 3,
                operand_1: 1,
                operand_2: 2,
            }),
        ];

        let block = Block::new(0, instructions);
        let mut analysis = SSAAnalysis::new(0);
        let block_id = petgraph::graph::NodeIndex::new(0);

        // Test each instruction individually
        for (inst_idx, hbc_instruction) in block.instructions().iter().enumerate() {
            let pc = block.start_pc() + inst_idx as u32;

            let usage = crate::generated::instruction_analysis::analyze_register_usage(
                &hbc_instruction.instruction,
            );

            if let Some(target_reg) = usage.target {
                analysis.definitions.push(RegisterDef {
                    register: target_reg,
                    block_id,
                    instruction_idx: inst_idx,
                    pc,
                });
            }

            for source_reg in usage.sources {
                analysis.uses.push(RegisterUse {
                    register: source_reg,
                    block_id,
                    instruction_idx: inst_idx,
                    pc,
                });
            }
        }

        // Should have 3 definitions: r1, r2, r3
        assert_eq!(analysis.definitions.len(), 3);
        assert_eq!(analysis.definitions[0].register, 1);
        assert_eq!(analysis.definitions[1].register, 2);
        assert_eq!(analysis.definitions[2].register, 3);

        // Should have 2 uses: r1, r2 (in the Add instruction)
        assert_eq!(analysis.uses.len(), 2);
        assert_eq!(analysis.uses[0].register, 1);
        assert_eq!(analysis.uses[1].register, 2);
    }

    #[test]
    fn test_ssa_analysis_creation() {
        let analysis = SSAAnalysis::new(42);
        assert_eq!(analysis.function_id, 42);
        assert!(analysis.definitions.is_empty());
        assert!(analysis.uses.is_empty());
    }

    #[test]
    fn test_ssa_analysis_structure() {
        use petgraph::graph::NodeIndex;

        let mut analysis = SSAAnalysis::new(0);
        let block0_id = NodeIndex::new(0);
        let block1_id = NodeIndex::new(1);

        // Manually add some definitions and uses to test the structure
        analysis
            .definitions
            .push(RegisterDef::new(1, block0_id, 0, 0));
        analysis
            .definitions
            .push(RegisterDef::new(2, block0_id, 1, 1));
        analysis
            .definitions
            .push(RegisterDef::new(3, block1_id, 0, 10));

        analysis.uses.push(RegisterUse::new(1, block1_id, 0, 10));
        analysis.uses.push(RegisterUse::new(2, block1_id, 0, 10));

        // Test that we can create the analysis structure
        assert_eq!(analysis.definitions.len(), 3);
        assert_eq!(analysis.uses.len(), 2);
        assert!(!analysis.has_multiple_definitions(1));
        assert!(!analysis.has_multiple_definitions(2));
        assert!(!analysis.has_multiple_definitions(3));

        // Test register queries
        assert_eq!(analysis.get_register_definitions(1).len(), 1);
        assert_eq!(analysis.get_register_uses(1).len(), 1);

        let stats = analysis.get_stats();
        assert_eq!(stats.total_definitions, 3);
        assert_eq!(stats.total_uses, 2);
    }

    #[test]
    fn test_dominates_function() {
        // We can't easily test the dominates function without a real dominator tree,
        // but we can at least verify the function signature compiles
        use petgraph::algo::dominators;
        use petgraph::graph::DiGraph;

        let mut graph: DiGraph<(), ()> = DiGraph::new();
        let n0 = graph.add_node(());
        let n1 = graph.add_node(());
        graph.add_edge(n0, n1, ());

        let dominators = dominators::simple_fast(&graph, n0);
        // Test that our dominates function can be called
        let result = frontiers::dominates(&dominators, n0, n1);
        // n0 should dominate n1 in this simple case
        assert!(result);
    }
}
