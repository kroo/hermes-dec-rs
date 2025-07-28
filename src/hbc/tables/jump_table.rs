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
    /// Maps function index to {byte offset -> instruction index}
    /// This is used to convert byte offsets to instruction indices
    address_to_index_map: HashMap<u32, HashMap<u32, u32>>,
}

impl JumpTable {
    /// Create a new empty jump table
    pub fn new() -> Self {
        Self {
            labels_by_function: HashMap::new(),
            jumps_by_function: HashMap::new(),
            label_map: HashMap::new(),
            jump_map: HashMap::new(),
            address_to_index_map: HashMap::new(),
        }
    }

    /// Build jump table data for a single function without modifying the main jump table
    /// This is used for parallel processing
    pub fn build_for_function_parallel(
        function_index: u32,
        instructions: &[HbcFunctionInstruction],
        exc_handlers: &[super::function_table::ExceptionHandlerInfo],
    ) -> Result<
        (
            u32,
            Vec<Label>,
            Vec<JumpInstruction>,
            HashMap<u32, String>,
            HashMap<u32, String>,
            HashMap<u32, u32>,
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

        // Second pass: add exception handler labels
        for (_handler_idx, handler) in exc_handlers.iter().enumerate() {
            // Copy packed struct fields to local variables to avoid unaligned access
            let start_offset = handler.start;
            let end_offset = handler.end;
            let target_offset = handler.target;

            // Convert byte offsets to instruction indices
            if let Some(start_idx) = address_to_index.get(&start_offset) {
                if !jump_targets.contains(start_idx) {
                    // create label for exception handler start
                    label_counter += 1;
                    let label_name = format!("L{}", label_counter);
                    labels.push(Label {
                        name: label_name.clone(),
                        instruction_index: *start_idx,
                        function_index,
                    });
                    label_map.insert(*start_idx, label_name);
                    jump_targets.insert(*start_idx);
                }
            }

            if let Some(end_idx) = address_to_index.get(&end_offset) {
                if !jump_targets.contains(end_idx) {
                    // create label for exception handler end
                    label_counter += 1;
                    let label_name = format!("L{}", label_counter);
                    labels.push(Label {
                        name: label_name.clone(),
                        instruction_index: *end_idx,
                        function_index,
                    });
                    label_map.insert(*end_idx, label_name);
                    jump_targets.insert(*end_idx);
                }
            }

            if let Some(target_idx) = address_to_index.get(&target_offset) {
                if !jump_targets.contains(target_idx) {
                    // create label for exception handler target (catch block)
                    label_counter += 1;
                    let label_name = format!("L{}", label_counter);
                    labels.push(Label {
                        name: label_name.clone(),
                        instruction_index: *target_idx,
                        function_index,
                    });
                    label_map.insert(*target_idx, label_name);
                    jump_targets.insert(*target_idx);
                }
            }
        }

        Ok((
            function_index,
            labels,
            jumps,
            label_map,
            jump_map,
            address_to_index,
        ))
    }

    /// Parse switch tables for a function
    pub fn parse_switch_tables_for_function(
        function_index: u32,
        instructions: &[HbcFunctionInstruction],
        hbc_file_data: &[u8], // Full HBC file data instead of just function bytecode
        jump_table_cache: &mut super::switch_table::JumpTableCache,
        function_body_offset: u32, // Absolute offset of the function body in the HBC file
    ) -> Result<Vec<super::switch_table::SwitchTable>, DecompilerError> {
        let mut switch_tables = Vec::new();

        // Build address-to-instruction-index mapping
        let mut address_to_index = HashMap::new();
        for instruction in instructions.iter() {
            // Use absolute addresses (function_body_offset + instruction.offset)
            let absolute_address = function_body_offset + instruction.offset;
            address_to_index.insert(absolute_address, instruction.instruction_index);
        }

        for (_i, instruction) in instructions.iter().enumerate() {
            if let UnifiedInstruction::SwitchImm {
                operand_0: _register,
                operand_1: jump_table_offset,
                operand_2: default_offset,
                operand_3: min_value,
                operand_4: max_value,
            } = &instruction.instruction
            {
                // Calculate the absolute offset of the switch instruction in the HBC file
                let switch_instruction_absolute_offset = function_body_offset + instruction.offset;
                
                // Calculate jump table address with 4-byte alignment
                // jump_table_addr = (ip + op2 + 3) & ~3
                let jump_table_addr = (switch_instruction_absolute_offset + jump_table_offset + 3) & !3;

                // Get or create jump table from cache
                let jump_table = jump_table_cache.get_or_create_jump_table(
                    jump_table_addr,
                    *min_value,
                    *max_value,
                );

                // Only parse the jump table if it hasn't been parsed yet
                if jump_table.entries.is_empty() {
                    // Parse each entry in the jump table
                    for case_value in *min_value..=*max_value {
                        let table_index = (case_value - min_value) as usize;
                        let entry_offset = jump_table_addr as usize + (table_index * 4);
                        
                        // Ensure we have enough bytes to read from the full HBC file data
                        if entry_offset + 4 <= hbc_file_data.len() {
                            // Read 32-bit signed offset (little-endian)
                            let offset_bytes = [
                                hbc_file_data[entry_offset],
                                hbc_file_data[entry_offset + 1],
                                hbc_file_data[entry_offset + 2],
                                hbc_file_data[entry_offset + 3],
                            ];
                            let target_offset = i32::from_le_bytes(offset_bytes);
                            
                            // Add entry to jump table
                            jump_table.add_entry(case_value, target_offset);
                        } else {
                            // If we can't read the bytecode, use a placeholder
                            let target_offset = (case_value * 4) as i32; // Placeholder
                            jump_table.add_entry(case_value, target_offset);
                        }
                    }
                }

                // Increment reference count for this jump table
                jump_table.increment_reference_count();

                // Create switch table
                let mut switch_table = super::switch_table::SwitchTable::new(
                    *min_value,
                    *max_value,
                    *default_offset,
                    *jump_table_offset,
                    jump_table_addr,
                    instruction.instruction_index,
                    function_index,
                );

                // Add cases from the cached jump table
                for case_value in *min_value..=*max_value {
                    if let Some(&target_offset) = jump_table.get_entry(case_value) {
                        switch_table.add_case(case_value, target_offset);
                        
                        // Set the target instruction index if we can find it
                        let jump_dest = switch_instruction_absolute_offset as i32 + target_offset;
                        
                        if let Some(&target_instruction_index) = address_to_index.get(&(jump_dest as u32)) {
                            // Update the case with the target instruction index
                            if let Some(case) = switch_table.cases.last_mut() {
                                case.target_instruction_index = Some(target_instruction_index);
                            }
                        }
                    }
                }

                // Add default case
                let default_target_offset = *default_offset;
                let default_jump_dest = switch_instruction_absolute_offset as i32 + default_target_offset;
                switch_table.default_instruction_index = address_to_index
                    .get(&(default_jump_dest as u32))
                    .copied();

                switch_tables.push(switch_table);
            }
        }

        Ok(switch_tables)
    }

    /// Merge jump table data from parallel processing
    pub fn merge_function_data(
        &mut self,
        function_index: u32,
        labels: Vec<Label>,
        jumps: Vec<JumpInstruction>,
        label_map: HashMap<u32, String>,
        jump_map: HashMap<u32, String>,
        address_to_index: HashMap<u32, u32>,
    ) {
        self.labels_by_function.insert(function_index, labels);
        self.jumps_by_function.insert(function_index, jumps);
        self.label_map.insert(function_index, label_map);
        self.jump_map.insert(function_index, jump_map);
        self.address_to_index_map
            .insert(function_index, address_to_index);
    }

    /// Build the jump table from a function's instructions
    /// This method now takes the function body bytes to build proper address-to-index mapping
    pub fn build_for_function(
        &mut self,
        function_index: u32,
        instructions: &[HbcFunctionInstruction],
        exc_handlers: &[super::function_table::ExceptionHandlerInfo],
    ) -> Result<(), DecompilerError> {
        let (_, labels, jumps, label_map, jump_map, address_to_index) =
            Self::build_for_function_parallel(function_index, instructions, exc_handlers)?;
        self.merge_function_data(
            function_index,
            labels,
            jumps,
            label_map,
            jump_map,
            address_to_index,
        );
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

    /// Convert byte offset to instruction index for a specific function
    pub fn byte_offset_to_instruction_index(
        &self,
        function_index: u32,
        byte_offset: u32,
    ) -> Option<u32> {
        self.address_to_index_map
            .get(&function_index)
            .and_then(|map| map.get(&byte_offset))
            .copied()
    }
}
