//! Fallthrough analysis for switch statements
//!
//! This module analyzes which case groups fall through to other case groups,
//! helping determine where code duplication is needed during AST generation.

use super::{CaseGroup, SwitchInfo};
use crate::cfg::Cfg;
use log::debug;

/// Information about fallthrough between case groups
#[derive(Debug, Clone)]
pub struct FallthroughAnalysis {
    /// For each case group index, the index of the group it falls through to (if any)
    pub fallthrough_targets: Vec<Option<usize>>,
}

impl FallthroughAnalysis {
    /// Analyze fallthrough patterns in a switch
    pub fn analyze(case_groups: &[CaseGroup], switch_info: &SwitchInfo, cfg: &Cfg) -> Self {
        let mut fallthrough_targets = Vec::new();

        for (group_index, group) in case_groups.iter().enumerate() {
            fallthrough_targets.push(Self::needs_fallthrough_duplication(
                group,
                group_index,
                case_groups,
                cfg,
                switch_info,
            ));
        }

        Self {
            fallthrough_targets,
        }
    }

    /// Check if a case group needs fallthrough duplication
    /// Returns the index of the group it falls through to, if any
    ///
    /// This is extracted from switch_converter::needs_fallthrough_duplication
    fn needs_fallthrough_duplication(
        group: &CaseGroup,
        group_index: usize,
        all_groups: &[CaseGroup],
        cfg: &Cfg,
        switch_info: &SwitchInfo,
    ) -> Option<usize> {
        // Check if this group's target block has a terminating instruction
        // Note: Jmp/JmpLong are NOT terminators - they redirect control flow (e.g., to shared tail)
        // Only Ret and Throw actually terminate execution
        let current_target = &cfg.graph()[group.target_block];
        let has_terminator = current_target.instructions().iter().any(|instr| {
            matches!(
                &instr.instruction,
                crate::generated::unified_instructions::UnifiedInstruction::Ret { .. }
                    | crate::generated::unified_instructions::UnifiedInstruction::Throw { .. }
            )
        });

        if has_terminator {
            return None;
        }

        debug!(
            "Checking fallthrough for group {} (block {:?})",
            group_index, group.target_block
        );
        if let Some(ref shared_tail) = switch_info.shared_tail {
            debug!("  Shared tail is block {:?}", shared_tail.block_id);
        } else {
            debug!("  No shared tail detected");
        }

        // Find which case group's target block this one falls through to
        for (i, other_group) in all_groups.iter().enumerate() {
            if i == group_index {
                continue;
            }

            // Check if control flow goes from our target to this group's target
            use petgraph::visit::EdgeRef;
            let falls_through = cfg
                .graph()
                .edges(group.target_block)
                .any(|edge| edge.target() == other_group.target_block);

            if falls_through {
                debug!(
                    "  Group {} flows to group {}'s target block {:?}",
                    group_index, i, other_group.target_block
                );
                // Check if the target block is also the shared tail
                // If it is, this is a break to shared exit, not a fallthrough
                if let Some(shared_tail) = &switch_info.shared_tail {
                    if other_group.target_block == shared_tail.block_id {
                        debug!(
                            "    -> But target is shared tail, treating as break not fallthrough"
                        );
                        // This case jumps to a block that is both a case target
                        // and the shared tail - treat as break, not fallthrough
                        continue;
                    }
                }
                debug!("    -> This is a true fallthrough");
                return Some(i);
            }
        }

        None
    }
}
