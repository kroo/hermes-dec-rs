use super::function_table::HbcFunctionInstruction;
use crate::generated::generated_traits::is_jump_instruction;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::DecompilerError;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Represents a label in the disassembly
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Label {
    /// The label name (e.g., "L1", "L2")
    pub name: String,
    /// The instruction index where this label appears
    pub instruction_index: u32,
    /// The function index this label belongs to
    pub function_index: u32,
}

/// Represents a jump instruction with its target
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JumpInstruction {
    /// The instruction index where this jump occurs
    pub instruction_index: u32,
    /// The target address from the instruction
    pub target_address: u32,
    /// The resolved label name (if available)
    pub resolved_label: Option<String>,
    /// The function index this jump belongs to
    pub function_index: u32,
}

/// Jump table that organizes labels and jump instructions by function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JumpTable {
    /// Maps function index to labels in that function
    labels_by_function: HashMap<u32, Vec<Label>>,
    /// Maps function index to jump instructions in that function
    jumps_by_function: HashMap<u32, Vec<JumpInstruction>>,
    /// Maps function index to {instruction index -> label name}
    /// This is used to get the label name for a given instruction index
    label_map: HashMap<u32, HashMap<u32, String>>,
    /// Maps function index to {instruction index -> label name}
    /// This is used to get the label name for a jump instruction's first operand
    jump_map: HashMap<u32, HashMap<u32, String>>,
}

impl JumpTable {
    /// Create a new empty jump table
    pub fn new() -> Self {
        Self {
            labels_by_function: HashMap::new(),
            jumps_by_function: HashMap::new(),
            label_map: HashMap::new(),
            jump_map: HashMap::new(),
        }
    }

    /// Build jump table data for a single function without modifying the main jump table
    /// This is used for parallel processing
    pub fn build_for_function_parallel(
        function_index: u32,
        instructions: &[HbcFunctionInstruction],
    ) -> Result<
        (
            u32,
            Vec<Label>,
            Vec<JumpInstruction>,
            HashMap<u32, String>,
            HashMap<u32, String>,
        ),
        DecompilerError,
    > {
        let mut labels = Vec::new();
        let mut jumps = Vec::new();
        let mut label_map = HashMap::new();
        let mut jump_map = HashMap::new();
        let mut label_counter = 0;

        // Build address-to-instruction-index mapping
        let mut address_to_index = HashMap::new();
        for instruction in instructions.iter() {
            // Record the current offset for this instruction
            address_to_index.insert(instruction.offset, instruction.instruction_index);
        }

        // First pass: identify all jump targets by converting addresses to instruction indices
        let mut jump_targets = HashSet::new();

        for (_i, instruction) in instructions.iter().enumerate() {
            if is_jump_instruction(instruction.name()) {
                if let Some(relative_offset) =
                    Self::extract_jump_target_relative_offset_static(&instruction.instruction)
                {
                    // Convert address to instruction index
                    let target_address = instruction.offset as i32 + relative_offset;
                    if let Some(&target_index) = address_to_index.get(&(target_address as u32)) {
                        if !jump_targets.contains(&target_index) {
                            // update jump targets set
                            jump_targets.insert(target_index);

                            // create label
                            label_counter += 1;
                            let label_name = format!("L{}", label_counter);
                            labels.push(Label {
                                name: label_name.clone(),
                                instruction_index: target_index,
                                function_index: instruction.function_index,
                            });

                            // create jump instruction
                            jumps.push(JumpInstruction {
                                instruction_index: instruction.instruction_index,
                                target_address: target_address as u32,
                                resolved_label: Some(label_name.clone()),
                                function_index: instruction.function_index,
                            });

                            // update label map
                            label_map.insert(target_index, label_name.clone());

                            // update jump map
                            jump_map.insert(instruction.instruction_index, label_name.clone());
                        } else {
                            let label_name = label_map.get(&target_index).unwrap();

                            // create jump instruction
                            jumps.push(JumpInstruction {
                                instruction_index: instruction.instruction_index,
                                target_address: target_address as u32,
                                resolved_label: Some(label_name.clone()),
                                function_index: instruction.function_index,
                            });

                            // update jump map
                            jump_map.insert(instruction.instruction_index, label_name.clone());
                        }
                    }
                }
            }
        }

        Ok((function_index, labels, jumps, label_map, jump_map))
    }

    /// Merge jump table data from parallel processing
    pub fn merge_function_data(
        &mut self,
        function_index: u32,
        labels: Vec<Label>,
        jumps: Vec<JumpInstruction>,
        label_map: HashMap<u32, String>,
        jump_map: HashMap<u32, String>,
    ) {
        self.labels_by_function.insert(function_index, labels);
        self.jumps_by_function.insert(function_index, jumps);
        self.label_map.insert(function_index, label_map);
        self.jump_map.insert(function_index, jump_map);
    }

    /// Build the jump table from a function's instructions
    /// This method now takes the function body bytes to build proper address-to-index mapping
    pub fn build_for_function(
        &mut self,
        function_index: u32,
        instructions: &[HbcFunctionInstruction],
    ) -> Result<(), DecompilerError> {
        let (_, labels, jumps, label_map, jump_map) =
            Self::build_for_function_parallel(function_index, instructions)?;
        self.merge_function_data(function_index, labels, jumps, label_map, jump_map);
        Ok(())
    }

    /// Extract the jump target address from a jump instruction (static version for parallel processing)
    fn extract_jump_target_relative_offset_static(instruction: &UnifiedInstruction) -> Option<i32> {
        match instruction {
            UnifiedInstruction::Jmp { operand_0 } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpLong { operand_0 } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpFalse { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpFalseLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpTrue { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpTrueLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpUndefined { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JmpUndefinedLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JStrictEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JStrictNotEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JStrictEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JStrictNotEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLess { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreater { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessEqualN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JLessEqualNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterEqualN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JGreaterEqualNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLess { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreater { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterEqual { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterEqualLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessEqualN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotLessEqualNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterEqualN { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::JNotGreaterEqualNLong { operand_0, .. } => Some(*operand_0 as i32),
            UnifiedInstruction::SaveGenerator { operand_0 } => Some(*operand_0 as i32),
            UnifiedInstruction::SaveGeneratorLong { operand_0 } => Some(*operand_0 as i32),
            _ => None,
        }
    }

    /// Extract the jump target address from a jump instruction
    fn extract_jump_target_relative_offset(&self, instruction: &UnifiedInstruction) -> Option<i32> {
        Self::extract_jump_target_relative_offset_static(instruction)
    }

    /// Get the label name by index for a given function
    pub fn get_label(&self, function_index: u32, index: u32) -> Option<&Label> {
        self.labels_by_function
            .get(&function_index)
            .and_then(|labels| labels.get(index as usize))
    }

    /// Get the number of labels for a given function
    pub fn get_label_count(&self, function_index: u32) -> usize {
        self.labels_by_function
            .get(&function_index)
            .map(|labels| labels.len())
            .unwrap_or(0)
    }

    /// Get the jump instruction by index for a given function
    pub fn get_jump(&self, function_index: u32, index: u32) -> Option<&JumpInstruction> {
        self.jumps_by_function
            .get(&function_index)
            .and_then(|jumps| jumps.get(index as usize))
    }

    /// Get the number of jump instructions for a given function
    pub fn get_jump_count(&self, function_index: u32) -> usize {
        self.jumps_by_function
            .get(&function_index)
            .map(|jumps| jumps.len())
            .unwrap_or(0)
    }

    /// Get the label name for a given instruction index in a function
    pub fn get_label_by_inst_index(
        &self,
        function_index: u32,
        instruction_index: u32,
    ) -> Option<&String> {
        self.label_map
            .get(&function_index)
            .and_then(|labels| labels.get(&instruction_index))
    }

    /// Get the label name for a given jump instruction
    pub fn get_label_by_jump_op_index(
        &self,
        function_index: u32,
        instruction_index: u32,
    ) -> Option<&String> {
        self.jump_map
            .get(&function_index)
            .and_then(|jumps| jumps.get(&instruction_index))
    }

    /// Get all labels for a function
    pub fn get_labels_for_function(&self, function_index: u32) -> Option<&Vec<Label>> {
        self.labels_by_function.get(&function_index)
    }

    /// Get all jump instructions for a function
    pub fn get_jumps_for_function(&self, function_index: u32) -> Option<&Vec<JumpInstruction>> {
        self.jumps_by_function.get(&function_index)
    }
}
