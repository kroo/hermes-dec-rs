//! Basic block module
//! 
//! This module contains the Block struct and related functionality.

use crate::hbc::function_table::HbcFunctionInstruction;
use serde::{Deserialize, Serialize};

/// Basic block containing a sequence of instructions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    /// Instructions in this block
    pub instructions: Vec<HbcFunctionInstruction>,
    /// Start PC of this block
    pub start_pc: u32,
    /// End PC of this block
    pub end_pc: u32,
}

impl Block {
    /// Create a new basic block
    pub fn new(start_pc: u32, instructions: Vec<HbcFunctionInstruction>) -> Self {
        let end_pc = if instructions.is_empty() {
            start_pc
        } else {
            start_pc + instructions.len() as u32
        };
        Self {
            instructions,
            start_pc,
            end_pc,
        }
    }
    
    /// Get instruction at a specific index
    pub fn get_instruction(&self, index: usize) -> Option<&HbcFunctionInstruction> {
        self.instructions.get(index)
    }
    
    /// Get all instructions
    pub fn instructions(&self) -> &[HbcFunctionInstruction] {
        &self.instructions
    }
    
    /// Get the number of instructions in this block
    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }
    
    /// Get the start PC of this block
    pub fn start_pc(&self) -> u32 {
        self.start_pc
    }
    
    /// Get the end PC of this block
    pub fn end_pc(&self) -> u32 {
        self.end_pc
    }

    pub fn contains_pc(&self, pc: u32) -> bool {
        pc >= self.start_pc && pc < self.end_pc
    }
    
    /// Get the last instruction in this block
    pub fn last_instruction(&self) -> Option<&HbcFunctionInstruction> {
        self.instructions.last()
    }
    
    /// Check if this block ends with a jump instruction
    pub fn ends_with_jump(&self) -> bool {
        if let Some(last) = self.last_instruction() {
            matches!(last.instruction.category(), "Jump")
        } else {
            false
        }
    }
} 