//! Value tracking and analysis
//!
//! This module provides functionality to track and analyze values through SSA form,
//! including constant folding, parameter detection, and static value analysis.

use crate::cfg::{
    ssa::{types::SSAValue, SSAAnalysis},
    Cfg,
};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::{HbcFile, InstructionIndex};
use petgraph::graph::NodeIndex;

/// Constant value types that can be tracked
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl ConstantValue {
    /// Try to convert the constant value to an i32
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            ConstantValue::Number(n) => {
                let int_val = *n as i32;
                // Check if the conversion is lossless
                if int_val as f64 == *n {
                    Some(int_val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Types of values that can be tracked
#[derive(Debug, Clone, PartialEq)]
pub enum TrackedValue {
    /// A compile-time constant
    Constant(ConstantValue),
    /// A function parameter
    Parameter {
        /// Parameter index (1-based, 0 is 'this')
        index: u32,
        /// The SSA value containing the parameter
        ssa_value: SSAValue,
    },
    /// A phi node result
    Phi {
        /// The SSA value produced by the phi
        ssa_value: SSAValue,
    },
    /// Unknown or dynamic value
    Unknown,
}

/// Value tracker for analyzing SSA values
pub struct ValueTracker<'a> {
    cfg: &'a Cfg<'a>,
    ssa: &'a SSAAnalysis,
    hbc_file: &'a HbcFile<'a>,
}

impl<'a> ValueTracker<'a> {
    /// Create a new value tracker
    pub fn new(cfg: &'a Cfg<'a>, ssa: &'a SSAAnalysis, hbc_file: &'a HbcFile<'a>) -> Self {
        Self { cfg, ssa, hbc_file }
    }

    /// Get the tracked value of an SSA value
    pub fn get_value(&self, ssa_value: &SSAValue) -> TrackedValue {
        // Check if this SSA value is produced by a phi function
        if let Some(phi_functions) = self.ssa.phi_functions.get(&ssa_value.def_site.block_id) {
            for phi in phi_functions {
                if phi.result == *ssa_value {
                    return self.analyze_phi_function(phi);
                }
            }
        }

        // Look up the instruction that defined this SSA value
        let block = &self.cfg.graph()[ssa_value.def_site.block_id];
        let instructions = block.instructions();

        // Find the instruction at the definition site
        let instruction_offset =
            ssa_value.def_site.instruction_idx.value() - block.start_pc().value();
        if let Some(instr) = instructions.get(instruction_offset) {
            return self.analyze_instruction(&instr.instruction, ssa_value);
        }

        TrackedValue::Unknown
    }

    /// Analyze a phi function to see if all inputs resolve to the same constant
    fn analyze_phi_function(&self, phi: &crate::cfg::ssa::types::PhiFunction) -> TrackedValue {
        let mut constant_value = None;

        // Check all operands of the phi function
        for (_pred_block, operand_ssa) in &phi.operands {
            match self.get_value(operand_ssa) {
                TrackedValue::Constant(c) => {
                    if let Some(ref existing) = constant_value {
                        if existing != &c {
                            // Different constants - this phi doesn't resolve to a single constant
                            return TrackedValue::Phi {
                                ssa_value: phi.result.clone(),
                            };
                        }
                    } else {
                        constant_value = Some(c);
                    }
                }
                _ => {
                    // Non-constant operand - this phi doesn't resolve to a constant
                    return TrackedValue::Phi {
                        ssa_value: phi.result.clone(),
                    };
                }
            }
        }

        // All operands are the same constant
        if let Some(c) = constant_value {
            TrackedValue::Constant(c)
        } else {
            TrackedValue::Phi {
                ssa_value: phi.result.clone(),
            }
        }
    }

    /// Analyze an instruction, potentially walking the def tree for complex cases
    fn analyze_instruction(
        &self,
        instruction: &UnifiedInstruction,
        ssa_value: &SSAValue,
    ) -> TrackedValue {
        match instruction {
            // Direct constant loads
            UnifiedInstruction::LoadConstUInt8 { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstInt { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1 as f64))
            }
            UnifiedInstruction::LoadConstDouble { operand_1, .. } => {
                TrackedValue::Constant(ConstantValue::Number(*operand_1))
            }
            UnifiedInstruction::LoadConstString { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    TrackedValue::Constant(ConstantValue::String(string.clone()))
                } else {
                    TrackedValue::Unknown
                }
            }
            UnifiedInstruction::LoadConstStringLongIndex { operand_1, .. } => {
                if let Ok(string) = self.hbc_file.strings.get((*operand_1).into()) {
                    TrackedValue::Constant(ConstantValue::String(string.clone()))
                } else {
                    TrackedValue::Unknown
                }
            }
            UnifiedInstruction::LoadConstNull { .. } => TrackedValue::Constant(ConstantValue::Null),
            UnifiedInstruction::LoadConstUndefined { .. } => {
                TrackedValue::Constant(ConstantValue::Undefined)
            }
            UnifiedInstruction::LoadConstTrue { .. } => {
                TrackedValue::Constant(ConstantValue::Boolean(true))
            }
            UnifiedInstruction::LoadConstFalse { .. } => {
                TrackedValue::Constant(ConstantValue::Boolean(false))
            }
            UnifiedInstruction::LoadConstZero { .. } => {
                TrackedValue::Constant(ConstantValue::Number(0.0))
            }

            // Parameter loads
            UnifiedInstruction::LoadParam { operand_1, .. } => TrackedValue::Parameter {
                index: *operand_1 as u32,
                ssa_value: ssa_value.clone(),
            },

            // Mov instruction - follow the source
            UnifiedInstruction::Mov { operand_1, .. } => {
                // Find the SSA value for the source register
                if let Some(source_ssa) = self
                    .ssa
                    .get_value_before_instruction(*operand_1, ssa_value.def_site.instruction_idx)
                {
                    self.get_value(source_ssa)
                } else {
                    TrackedValue::Unknown
                }
            }

            // Arithmetic operations - perform constant folding
            UnifiedInstruction::Add {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 + n2))
                    }
                    (ConstantValue::String(s1), ConstantValue::String(s2)) => {
                        Some(ConstantValue::String(s1.clone() + s2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Sub {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 - n2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Mul {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) => {
                        Some(ConstantValue::Number(n1 * n2))
                    }
                    _ => None,
                },
            ),

            UnifiedInstruction::Div {
                operand_1,
                operand_2,
                ..
            } => self.fold_binary_operation(
                *operand_1,
                *operand_2,
                ssa_value.def_site.instruction_idx,
                |a, b| match (a, b) {
                    (ConstantValue::Number(n1), ConstantValue::Number(n2)) if *n2 != 0.0 => {
                        Some(ConstantValue::Number(n1 / n2))
                    }
                    _ => None,
                },
            ),

            // TODO: Handle more operations like:
            // - Negate, Not, BitNot
            // - Mod, BitAnd, BitOr, BitXor, LShift, RShift, URshift
            // - StrictEq, StrictNotEq, Eq, NotEq, Less, LessEq, Greater, GreaterEq
            // - TypeOf, InstanceOf, IsIn
            // - ToNumber, ToString
            _ => TrackedValue::Unknown,
        }
    }

    /// Helper to fold binary operations
    fn fold_binary_operation<F>(
        &self,
        operand1: u8,
        operand2: u8,
        pc: InstructionIndex,
        folder: F,
    ) -> TrackedValue
    where
        F: Fn(&ConstantValue, &ConstantValue) -> Option<ConstantValue>,
    {
        // Get SSA values for both operands
        let val1 = if let Some(ssa1) = self.ssa.get_value_before_instruction(operand1, pc) {
            self.get_value(ssa1)
        } else {
            TrackedValue::Unknown
        };

        let val2 = if let Some(ssa2) = self.ssa.get_value_before_instruction(operand2, pc) {
            self.get_value(ssa2)
        } else {
            TrackedValue::Unknown
        };

        // Try to fold if both are constants
        match (val1, val2) {
            (TrackedValue::Constant(c1), TrackedValue::Constant(c2)) => {
                if let Some(result) = folder(&c1, &c2) {
                    TrackedValue::Constant(result)
                } else {
                    TrackedValue::Unknown
                }
            }
            _ => TrackedValue::Unknown,
        }
    }

    /// Check if a tracked value is a constant
    pub fn is_constant(value: &TrackedValue) -> bool {
        matches!(value, TrackedValue::Constant(_))
    }

    /// Check if a tracked value is a parameter
    pub fn is_parameter(value: &TrackedValue) -> bool {
        matches!(value, TrackedValue::Parameter { .. })
    }

    /// Extract the constant value if this is a constant
    pub fn as_constant(value: &TrackedValue) -> Option<&ConstantValue> {
        match value {
            TrackedValue::Constant(c) => Some(c),
            _ => None,
        }
    }

    /// Convert a register lookup at a specific point to an SSA value lookup
    pub fn get_value_at_point(
        &self,
        register: u8,
        _block_id: NodeIndex,
        instruction_idx: InstructionIndex,
    ) -> TrackedValue {
        // Find the SSA value for this register at this point
        if let Some(ssa_value) = self
            .ssa
            .get_value_before_instruction(register, instruction_idx)
        {
            self.get_value(ssa_value)
        } else {
            TrackedValue::Unknown
        }
    }
}
