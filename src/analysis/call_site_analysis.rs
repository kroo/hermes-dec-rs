//! Call site analysis for tracking argument registers in Call/Construct instructions
//!
//! This module analyzes Call and Construct instructions to determine which registers
//! contain their arguments. In Hermes bytecode, arguments are placed in the highest
//! registers of the current frame in sequential order.

use crate::cfg::{Block, Cfg};
use crate::generated::instruction_analysis::analyze_register_usage;
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::InstructionIndex;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Analysis of call sites and their argument registers
#[derive(Debug, Clone)]
pub struct CallSiteAnalysis {
    /// Maps each call instruction to its argument registers
    /// Key: (block_id, instruction_index)
    /// Value: CallSiteInfo with argument details
    pub call_sites: HashMap<(NodeIndex, InstructionIndex), CallSiteInfo>,
}

/// Information about a specific call site
#[derive(Debug, Clone)]
pub struct CallSiteInfo {
    /// Registers containing arguments in order
    /// For constructors: [this, arg0, arg1, ...]
    /// For regular calls: [this, arg0, arg1, ...]
    pub argument_registers: Vec<u8>,

    /// The constructor/function register
    pub callee_register: u8,

    /// Number of arguments (from instruction)
    pub arg_count: u8,

    /// Whether this is a constructor call
    pub is_constructor: bool,
}

impl CallSiteAnalysis {
    /// Create an empty analysis
    pub fn new() -> Self {
        Self {
            call_sites: HashMap::new(),
        }
    }

    /// Analyze a function's CFG to find all call sites and their arguments
    pub fn analyze(cfg: &Cfg) -> Self {
        let mut analysis = CallSiteAnalysis::new();

        for node in cfg.graph().node_indices() {
            let block = &cfg.graph()[node];
            analysis.analyze_block(node, block);
        }

        analysis
    }

    fn analyze_block(&mut self, block_id: NodeIndex, block: &Block) {
        // Track which registers have been written to in this block
        // We maintain them in order of discovery
        let mut written_registers = Vec::new();

        for hbc_instr in block.instructions() {
            let pc = hbc_instr.instruction_index;

            // Track register writes before processing the instruction
            let usage = analyze_register_usage(&hbc_instr.instruction);
            if let Some(target) = usage.target {
                // Update our tracking - remove if exists and add to end (most recent)
                written_registers.retain(|&r| r != target);
                written_registers.push(target);
            }

            // Check if this is a Call or Construct
            match &hbc_instr.instruction {
                UnifiedInstruction::Call {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let arg_registers =
                        self.find_argument_registers(&written_registers, *operand_2);

                    log::debug!(
                        "Call at block {:?}, PC {}: {} args in registers {:?}",
                        block_id,
                        pc.0,
                        *operand_2,
                        arg_registers
                    );

                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: arg_registers,
                            callee_register: *operand_1,
                            arg_count: *operand_2,
                            is_constructor: false,
                        },
                    );
                }

                UnifiedInstruction::CallLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let arg_registers =
                        self.find_argument_registers(&written_registers, *operand_2 as u8);

                    log::debug!(
                        "CallLong at block {:?}, PC {}: {} args in registers {:?}",
                        block_id,
                        pc.0,
                        *operand_2,
                        arg_registers
                    );

                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: arg_registers,
                            callee_register: *operand_1,
                            arg_count: *operand_2 as u8,
                            is_constructor: false,
                        },
                    );
                }

                UnifiedInstruction::Construct {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let arg_registers =
                        self.find_argument_registers(&written_registers, *operand_2);

                    log::debug!(
                        "Construct at block {:?}, PC {}: {} args in registers {:?}",
                        block_id,
                        pc.0,
                        *operand_2,
                        arg_registers
                    );

                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: arg_registers,
                            callee_register: *operand_1,
                            arg_count: *operand_2,
                            is_constructor: true,
                        },
                    );
                }

                UnifiedInstruction::ConstructLong {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    let arg_registers =
                        self.find_argument_registers(&written_registers, *operand_2 as u8);

                    log::debug!(
                        "ConstructLong at block {:?}, PC {}: {} args in registers {:?}",
                        block_id,
                        pc.0,
                        *operand_2,
                        arg_registers
                    );

                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: arg_registers,
                            callee_register: *operand_1,
                            arg_count: *operand_2 as u8,
                            is_constructor: true,
                        },
                    );
                }

                // Also handle Call1, Call2, Call3, Call4 which have explicit argument registers
                UnifiedInstruction::Call1 {
                    operand_0: _,
                    operand_1,
                    operand_2,
                } => {
                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: vec![*operand_2], // Single argument
                            callee_register: *operand_1,
                            arg_count: 1,
                            is_constructor: false,
                        },
                    );
                }

                UnifiedInstruction::Call2 {
                    operand_0: _,
                    operand_1,
                    operand_2,
                    operand_3,
                } => {
                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: vec![*operand_2, *operand_3], // Two arguments
                            callee_register: *operand_1,
                            arg_count: 2,
                            is_constructor: false,
                        },
                    );
                }

                UnifiedInstruction::Call3 {
                    operand_0: _,
                    operand_1,
                    operand_2,
                    operand_3,
                    operand_4,
                } => {
                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: vec![*operand_2, *operand_3, *operand_4],
                            callee_register: *operand_1,
                            arg_count: 3,
                            is_constructor: false,
                        },
                    );
                }

                UnifiedInstruction::Call4 {
                    operand_0: _,
                    operand_1,
                    operand_2,
                    operand_3,
                    operand_4,
                    operand_5,
                } => {
                    self.call_sites.insert(
                        (block_id, pc),
                        CallSiteInfo {
                            argument_registers: vec![
                                *operand_2, *operand_3, *operand_4, *operand_5,
                            ],
                            callee_register: *operand_1,
                            arg_count: 4,
                            is_constructor: false,
                        },
                    );
                }

                _ => {}
            }
        }
    }

    fn find_argument_registers(&self, written_registers: &[u8], arg_count: u8) -> Vec<u8> {
        // Arguments are placed in the highest registers
        // According to Hermes calling convention:
        // - Arguments are at the top of the register frame
        // - They're in REVERSE order from the end of the current frame
        // - For Construct: the last register is 'this', then actual args in reverse

        // Get unique registers and sort them
        let mut unique_regs: Vec<u8> = written_registers.to_vec();
        unique_regs.sort_unstable();
        unique_regs.dedup();

        // Take the highest arg_count registers
        let start_idx = unique_regs.len().saturating_sub(arg_count as usize);
        let mut arg_registers = unique_regs[start_idx..].to_vec();

        // Reverse the order since Hermes stores them in reverse
        // After reversal: first element is 'this', rest are actual arguments
        arg_registers.reverse();

        log::trace!(
            "Finding {} arguments from written registers {:?} -> {:?} (reversed for calling convention)",
            arg_count, unique_regs, arg_registers
        );

        arg_registers
    }
}
