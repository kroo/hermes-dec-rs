//! SwitchImm (dense switch) pattern analysis
//!
//! This module detects and analyzes SwitchImm-based switch patterns in the CFG

use super::switch_info::*;
use crate::cfg::analysis::PostDominatorAnalysis;
use crate::cfg::ssa::SSAAnalysis;
use crate::cfg::Cfg;
use crate::generated::instruction_analysis::analyze_register_usage;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::{HbcFile, InstructionIndex};
use ordered_float::OrderedFloat;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::rc::Rc;

/// SwitchImm pattern analyzer
pub struct SwitchImmAnalyzer<'a> {
    hbc_file: &'a HbcFile<'a>,
    function_index: u32,
}

impl<'a> SwitchImmAnalyzer<'a> {
    pub fn new(hbc_file: &'a HbcFile<'a>, function_index: u32) -> Self {
        Self {
            hbc_file,
            function_index,
        }
    }

    /// Detect a SwitchImm pattern starting from a given block
    pub fn detect_switch_pattern(
        &self,
        start_block: NodeIndex,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
        postdom: &PostDominatorAnalysis,
    ) -> Option<SwitchInfo> {
        let block = &cfg.graph()[start_block];
        
        // Find SwitchImm instruction
        let (switch_imm_idx, switch_imm_instr) = block
            .instructions()
            .iter()
            .enumerate()
            .find(|(_, instr)| matches!(&instr.instruction, UnifiedInstruction::SwitchImm { .. }))?;

        // Extract discriminator and other info from SwitchImm
        let (discriminator, _min_value, _default_offset, num_targets, _jump_table_offset) = match &switch_imm_instr.instruction {
            UnifiedInstruction::SwitchImm {
                operand_0,
                operand_1,
                operand_2,
                operand_3,
                operand_4,
            } => (*operand_0, *operand_1, *operand_2, *operand_3, *operand_4),
            _ => unreachable!(),
        };

        // Get switch table from HBC file
        let switch_table = self
            .hbc_file
            .switch_tables
            .get_switch_table_by_instruction(
                self.function_index,
                switch_imm_instr.instruction_index.into(),
            )?;

        // Collect setup instructions (everything before SwitchImm)
        let mut setup_instructions = SmallVec::new();
        for (i, instr) in block.instructions().iter().enumerate() {
            if i >= switch_imm_idx {
                break;
            }

            let usage = analyze_register_usage(&instr.instruction);
            if let Some(target_reg) = usage.target {
                let instr_idx = InstructionIndex::new(block.start_pc().value() + i);
                if let Some(ssa_value) = ssa.get_value_at(target_reg, instr_idx) {
                    let const_value = self.extract_constant_value_from_instruction(&instr.instruction);
                    
                    setup_instructions.push(SetupInstruction {
                        instruction: Rc::new(instr.clone()),
                        ssa_value: ssa_value.clone(),
                        value: const_value,
                    });
                }
            }
        }

        // Get all successor blocks
        let successors: Vec<_> = cfg
            .graph()
            .edges(start_block)
            .map(|e| e.target())
            .collect();

        if successors.len() != num_targets as usize {
            return None; // Mismatch in expected targets
        }

        // Build cases from switch table and successors
        let mut cases = Vec::new();
        let mut default_case = None;

        // The last successor is typically the default case
        if successors.len() > 0 {
            let default_target = successors[successors.len() - 1];
            default_case = Some(DefaultCase {
                target_block: default_target,
                setup: SmallVec::new(), // Default case doesn't need setup - it's just a regular block
            });
        }

        // Process each case in the switch table
        for (i, switch_case) in switch_table.cases.iter().enumerate() {
            let case_value = switch_case.value;
            
            // Calculate which successor this case maps to
            // The switch table stores offsets, we need to map them to successor indices
            // For now, we'll use the index directly if available
            let target_index = if let Some(target_idx) = switch_case.target_instruction_index {
                // Find which successor corresponds to this instruction index
                // This is a simplification - in a real implementation we'd need to map
                // instruction indices to block indices
                (target_idx as usize).min(successors.len().saturating_sub(2))
            } else {
                // Fallback: use the case index
                i.min(successors.len().saturating_sub(2))
            };
            
            if target_index >= successors.len() - 1 {
                continue; // Skip if this would point to default
            }

            let target_block = successors[target_index];
            
            cases.push(CaseInfo {
                keys: vec![CaseKey::Number(OrderedFloat(case_value as f64))],
                comparison_block: start_block,
                target_block,
                setup: setup_instructions.clone(),
                always_terminates: false, // Will be determined later
                execution_order: i,
            });
        }

        // Detect shared tail
        let shared_tail = self.detect_shared_tail(&cases, &default_case, postdom, cfg, ssa);

        // Check termination for each case
        for case in &mut cases {
            case.always_terminates = self.check_always_terminates(case.target_block, cfg);
        }

        Some(SwitchInfo {
            discriminator,
            discriminator_instruction_index: switch_imm_instr.instruction_index,
            cases,
            default_case,
            shared_tail,
        })
    }

    /// Extract constant value from an instruction
    fn extract_constant_value_from_instruction(
        &self,
        instr: &UnifiedInstruction,
    ) -> Option<ConstantValue> {
        match instr {
            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    return Some(ConstantValue::String(string.clone()));
                }
                None
            }
            UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    return Some(ConstantValue::String(string.clone()));
                }
                None
            }
            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                Some(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                Some(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                Some(ConstantValue::Number(*operand_1))
            }
            UnifiedInstruction::LoadConstZero { .. } => Some(ConstantValue::Number(0.0)),
            UnifiedInstruction::LoadConstTrue { .. } => Some(ConstantValue::Boolean(true)),
            UnifiedInstruction::LoadConstFalse { .. } => Some(ConstantValue::Boolean(false)),
            UnifiedInstruction::LoadConstNull { .. } => Some(ConstantValue::Null),
            UnifiedInstruction::LoadConstUndefined { .. } => Some(ConstantValue::Undefined),
            _ => None,
        }
    }

    /// Check if a block always terminates
    fn check_always_terminates(&self, block_id: NodeIndex, cfg: &Cfg<'a>) -> bool {
        let block = &cfg.graph()[block_id];
        block.instructions().iter().any(|instr| {
            matches!(
                &instr.instruction,
                UnifiedInstruction::Ret { .. }
                    | UnifiedInstruction::Throw { .. }
                    | UnifiedInstruction::ThrowIfUndefinedInst { .. }
            )
        })
    }

    /// Detect shared tail block
    fn detect_shared_tail(
        &self,
        cases: &[CaseInfo],
        default_case: &Option<DefaultCase>,
        postdom: &PostDominatorAnalysis,
        cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> Option<SharedTailInfo> {
        // Skip if any case always terminates
        if cases.iter().any(|c| c.always_terminates) {
            return None;
        }

        // Collect all target blocks
        let mut target_blocks = Vec::new();
        for case in cases {
            target_blocks.push(case.target_block);
        }
        if let Some(default) = default_case {
            target_blocks.push(default.target_block);
        }

        // Find common post-dominator
        let shared_tail = self.find_common_postdominator(&target_blocks, postdom)?;

        // Check if it's meaningful
        if !self.is_meaningful_shared_tail(shared_tail, cfg) {
            return None;
        }

        // Analyze PHI requirements
        let phi_nodes = self.analyze_phi_requirements(shared_tail, cfg, ssa);

        Some(SharedTailInfo {
            block_id: shared_tail,
            phi_nodes,
        })
    }

    /// Find common post-dominator
    fn find_common_postdominator(
        &self,
        blocks: &[NodeIndex],
        postdom: &PostDominatorAnalysis,
    ) -> Option<NodeIndex> {
        if blocks.is_empty() {
            return None;
        }

        let mut common = blocks[0];

        for &block in &blocks[1..] {
            // Find lowest common ancestor in post-dominator tree
            let mut a = common;
            let mut b = block;

            // Get paths to root for both nodes
            let mut path_a = vec![a];
            while let Some(parent) = postdom.immediate_post_dominator(a) {
                if parent == a {
                    break;
                }
                path_a.push(parent);
                a = parent;
            }

            let mut path_b = vec![b];
            while let Some(parent) = postdom.immediate_post_dominator(b) {
                if parent == b {
                    break;
                }
                path_b.push(parent);
                b = parent;
            }

            // Find common ancestor
            let mut found = None;
            for node_a in &path_a {
                if path_b.contains(node_a) {
                    found = Some(*node_a);
                    break;
                }
            }

            common = found?;
        }

        Some(common)
    }

    /// Check if a block is meaningful as shared tail
    fn is_meaningful_shared_tail(&self, tail_block: NodeIndex, cfg: &Cfg<'a>) -> bool {
        let block = &cfg.graph()[tail_block];
        !block.instructions().is_empty()
    }

    /// Analyze PHI requirements at shared tail
    fn analyze_phi_requirements(
        &self,
        tail_block: NodeIndex,
        _cfg: &Cfg<'a>,
        ssa: &SSAAnalysis,
    ) -> HashMap<u8, PhiNode> {
        let mut phi_nodes = HashMap::new();

        // Get existing PHI functions at the tail block
        if let Some(existing_phis) = ssa.phi_functions.get(&tail_block) {
            for phi_func in existing_phis {
                phi_nodes.insert(
                    phi_func.register,
                    PhiNode {
                        register: phi_func.register,
                        values: HashMap::new(), // Could be populated with constant analysis
                        ssa_phi_value: Some(phi_func.result.clone()),
                    },
                );
            }
        }

        phi_nodes
    }
}