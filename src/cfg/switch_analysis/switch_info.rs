//! Shared types for switch analysis
//!
//! This module contains types used by both sparse and dense switch analysis

use crate::cfg::ssa::SSAValue;
use crate::hbc::function_table::HbcFunctionInstruction;
use ordered_float::OrderedFloat;
use petgraph::graph::NodeIndex;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::rc::Rc;

/// Information about a detected switch pattern
#[derive(Debug, Clone)]
pub struct SwitchInfo {
    /// The register being switched on
    pub discriminator: u8,
    /// The instruction index where the discriminator is used (for proper PC context)
    pub discriminator_instruction_index: crate::hbc::InstructionIndex,
    /// The comparison blocks and their case values
    pub cases: Vec<CaseInfo>,
    /// Default case information (if present)
    pub default_case: Option<DefaultCase>,
    /// Shared tail block information (if present)
    pub shared_tail: Option<SharedTailInfo>,
}

/// Information about a single case in a switch
#[derive(Debug, Clone)]
pub struct CaseInfo {
    /// The constant values for this case (multiple for fallthrough)
    pub keys: Vec<CaseKey>,
    /// The block where comparison happens
    pub comparison_block: NodeIndex,
    /// The target block if comparison succeeds
    pub target_block: NodeIndex,
    /// Setup instructions that should be executed before the case body
    pub setup: SmallVec<[SetupInstruction; 4]>,
    /// Whether this case always terminates (return, throw, etc)
    pub always_terminates: bool,
    /// Order of execution in the original switch
    pub execution_order: usize,
}

/// Information about the default case
#[derive(Debug, Clone)]
pub struct DefaultCase {
    /// The target block for the default case
    pub target_block: NodeIndex,
    /// Setup instructions for the default case
    pub setup: SmallVec<[SetupInstruction; 4]>,
}

/// Information about a shared tail block (common exit point)
#[derive(Debug, Clone)]
pub struct SharedTailInfo {
    /// The shared tail block
    pub block_id: NodeIndex,
    /// PHI nodes at the shared tail
    pub phi_nodes: HashMap<u8, PhiNode>,
}

/// Information about a PHI node at a join point
#[derive(Debug, Clone)]
pub struct PhiNode {
    /// The register this PHI node writes to
    pub register: u8,
    /// Values from different predecessor blocks
    pub values: HashMap<NodeIndex, ConstantValue>,
    /// SSA PHI value if available
    pub ssa_phi_value: Option<SSAValue>,
}

/// Grouped consecutive cases that share the same target
#[derive(Debug, Clone)]
pub struct CaseGroup {
    /// All keys in this group
    pub keys: Vec<CaseKey>,
    /// The target block for all cases in this group
    pub target_block: NodeIndex,
    /// Setup instructions (shared by all cases in the group)
    pub setup: SmallVec<[SetupInstruction; 4]>,
    /// Whether this group always terminates
    pub always_terminates: bool,
    /// The execution order of the first case in this group
    pub first_execution_order: usize,
    /// All comparison blocks in this group
    pub comparison_blocks: Vec<NodeIndex>,
}

/// A setup instruction that should be executed before entering a case
/// 
/// Setup instructions are additional instructions (other than the comparison and its
/// associated constant load) that appear in comparison blocks and need to be executed
/// when taking that case path. This typically happens when multiple cases are grouped
/// together and share some common setup code.
/// 
/// For example, in a pattern like:
/// ```
/// case 1:
/// case 2:
///     x = 5;  // This would be a setup instruction
///     // fall through to shared code
/// ```
/// 
/// Note: Instructions in the dispatch block that execute before ANY case comparison
/// are NOT setup instructions - they're part of the normal control flow before the switch.
#[derive(Debug, Clone)]
pub struct SetupInstruction {
    /// The instruction to execute
    pub instruction: Rc<HbcFunctionInstruction>,
    /// The SSA value this instruction produces
    pub ssa_value: SSAValue,
    /// The constant value if this is a constant load
    pub value: Option<ConstantValue>,
}

/// Key for a case in a switch statement
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CaseKey {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl CaseKey {
    /// Normalize for grouping (e.g., convert -0.0 to 0.0)
    pub fn normalize_for_grouping(&self) -> Self {
        match self {
            CaseKey::Number(n) if n.0 == 0.0 => CaseKey::Number(OrderedFloat(0.0)),
            _ => self.clone(),
        }
    }
}

/// Constant value that can be used in switch cases
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

/// Context for comparison instruction analysis
#[derive(Debug, Clone)]
pub struct CompareContext {
    pub compare_block: NodeIndex,
    pub true_successor: NodeIndex,
    pub false_successor: NodeIndex,
    pub discriminator: u8,
}