//! Control Flow Plan - High-level IR between CFG analysis and AST generation
//!
//! This module defines the ControlFlowPlan structure which captures all control flow
//! patterns and analysis decisions before AST generation. This allows us to make
//! all important decisions (declaration points, duplication contexts, value elimination)
//! during the analysis phase rather than during AST generation.

use crate::analysis::call_site_analysis::CallSiteAnalysis;
use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, UseStrategy};
use crate::cfg::ssa::types::DuplicationContext;
use crate::cfg::ssa::{DuplicatedSSAValue, RegisterUse, SSAValue};
use crate::cfg::switch_analysis::switch_info::{CaseGroup, CaseKey, SetupInstruction, SwitchInfo};
use crate::generated::generated_traits::BinaryOperator;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Information about a constructor pattern (CreateThis/Construct/SelectObject)
#[derive(Debug, Clone)]
pub struct ConstructorPattern {
    /// The constructor function SSA value
    pub constructor: SSAValue,
    /// The arguments to the constructor
    pub arguments: Vec<SSAValue>,
    /// The 'this' value passed to Construct (often a copy of create_this_result)
    pub construct_this: Option<SSAValue>,
    /// The CreateThis result (intermediate, will be consumed)
    pub create_this_result: SSAValue,
    /// The Construct result (intermediate, will be consumed)
    pub construct_result: SSAValue,
    /// PC of the SelectObject instruction (for AST generation context)
    pub select_object_pc: u32,
    /// PC of the Construct instruction (to get register info during AST generation)
    pub construct_pc: u32,
    /// Register containing the constructor function at Construct
    pub constructor_reg: u8,
    /// Number of arguments
    pub arg_count: u8,
}

/// Unique identifier for a control flow structure
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructureId(pub usize);

/// PHI deconstruction information for a block
#[derive(Debug, Clone)]
pub struct PhiDeconstructionInfo {
    /// PHI replacements for duplicated blocks
    /// Maps: PHI result → concrete predecessor value
    pub replacements: HashMap<SSAValue, SSAValue>,

    /// Updated PHI functions for original blocks
    /// (with removed predecessors)
    pub updated_phis: Vec<UpdatedPhiFunction>,
}

/// An updated PHI function with removed predecessors
#[derive(Debug, Clone)]
pub struct UpdatedPhiFunction {
    /// The PHI result value
    pub result: SSAValue,
    /// Remaining predecessors and their values
    pub operands: Vec<(NodeIndex, SSAValue)>,
}

/// The complete plan for converting a function from CFG to AST
#[derive(Debug, Clone)]
pub struct ControlFlowPlan {
    /// The root control flow structure ID
    pub root: StructureId,

    /// Registry of all control flow structures
    pub structures: HashMap<StructureId, ControlFlowStructure>,

    /// Variables that need declaration at specific block entry points
    pub block_declarations: HashMap<NodeIndex, Vec<DuplicatedSSAValue>>,

    /// Declaration strategy for each (potentially duplicated) SSA value at its definition site
    pub declaration_strategies: HashMap<DuplicatedSSAValue, DeclarationStrategy>,

    /// Use strategy for each (potentially duplicated) SSA value at each use site
    pub use_strategies: HashMap<(DuplicatedSSAValue, RegisterUse), UseStrategy>,

    /// Which uses have been consumed (will be inlined)
    pub consumed_uses: HashMap<DuplicatedSSAValue, HashSet<RegisterUse>>,

    /// Variable scope boundaries
    pub scope_boundaries: HashMap<StructureId, ScopeInfo>,

    /// PHI deconstruction info per block and duplication context
    /// Key: (block_id, Option<DuplicationContext>)
    /// - None = original block (may have updated PHIs)
    /// - Some(ctx) = duplicated block (has PHI replacements)
    pub phi_deconstructions:
        HashMap<(NodeIndex, Option<DuplicationContext>), PhiDeconstructionInfo>,

    /// Set of SSA values that are PHI results
    /// These should not use duplicated names even when in duplicated blocks
    pub phi_results: HashSet<SSAValue>,

    /// Call site analysis with argument registers for each Call/Construct
    pub call_site_analysis: CallSiteAnalysis,

    /// SSA values that must be inlined (e.g., source operands of setup instructions)
    /// These values' defining instructions won't be in the generated code path
    pub mandatory_inline: HashSet<SSAValue>,

    /// Constructor patterns detected in the function
    /// Maps SelectObject result SSA value to the constructor pattern info
    pub constructor_patterns: HashMap<SSAValue, ConstructorPattern>,

    /// Next available structure ID
    next_structure_id: usize,
}

/// Common fields for all control flow structures
#[derive(Debug, Clone)]
pub struct ControlFlowStructure {
    /// Unique identifier for this structure
    pub id: StructureId,

    /// If this is a duplicate of another structure
    pub duplication_info: Option<DuplicationInfo>,

    /// Variables that should be scoped to this structure
    pub scoped_variables: Vec<DuplicatedSSAValue>,

    /// Entry blocks for this structure (blocks that can enter this structure)
    pub entry_blocks: Vec<NodeIndex>,

    /// Exit blocks for this structure (blocks that can exit this structure)
    pub exit_blocks: Vec<NodeIndex>,

    /// The actual control flow pattern
    pub kind: ControlFlowKind,
}

/// Information about structure duplication
#[derive(Debug, Clone)]
pub struct DuplicationInfo {
    /// The original structure being duplicated
    pub original: StructureId,
    /// The context in which this duplication occurs
    pub context: DuplicationContext,
}

/// Represents a comparison expression from jump instructions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonExpression {
    /// Simple register condition (JmpTrue, JmpFalse, etc.)
    SimpleCondition {
        operand: SSAValue,
        operand_use: RegisterUse,
    },
    /// Binary comparison (JEqual, JLess, etc.)
    BinaryComparison {
        operator: BinaryOperator,
        left: SSAValue,
        left_use: RegisterUse,
        right: SSAValue,
        right_use: RegisterUse,
    },
}

/// Format a comparison expression for debug output
fn format_comparison_expression(expr: &ComparisonExpression) -> String {
    match expr {
        ComparisonExpression::SimpleCondition { operand, .. } => {
            format!("r{}_v{}", operand.register, operand.version)
        }
        ComparisonExpression::BinaryComparison {
            operator,
            left,
            right,
            ..
        } => {
            let op_str = match operator {
                BinaryOperator::Equality => "==",
                BinaryOperator::Inequality => "!=",
                BinaryOperator::StrictEquality => "===",
                BinaryOperator::StrictInequality => "!==",
                BinaryOperator::LessThan => "<",
                BinaryOperator::LessEqualThan => "<=",
                BinaryOperator::GreaterThan => ">",
                BinaryOperator::GreaterEqualThan => ">=",
                _ => "?", // Other operators not typically used in conditionals
            };
            format!(
                "r{}_v{} {} r{}_v{}",
                left.register, left.version, op_str, right.register, right.version
            )
        }
    }
}

impl ComparisonExpression {
    /// Extract SSAValue and RegisterUse for backward compatibility with AST converter
    /// This is temporary until the AST converter is updated to handle full comparison expressions
    pub fn extract_legacy_format(&self) -> (Option<SSAValue>, Option<RegisterUse>) {
        match self {
            ComparisonExpression::SimpleCondition {
                operand,
                operand_use,
            } => (Some(operand.clone()), Some(operand_use.clone())),
            ComparisonExpression::BinaryComparison { left, left_use, .. } => {
                // For backward compatibility, just return the left operand
                (Some(left.clone()), Some(left_use.clone()))
            }
        }
    }

    /// Create an inverted version of this comparison expression
    /// This is used for conditional inversion when empty if-branches should be negated
    pub fn invert(&self) -> ComparisonExpression {
        match self {
            ComparisonExpression::SimpleCondition {
                operand,
                operand_use,
            } => {
                // Simple conditions become negated - we'll represent this the same way
                // The AST converter will need to add the logical NOT
                ComparisonExpression::SimpleCondition {
                    operand: operand.clone(),
                    operand_use: operand_use.clone(),
                }
            }
            ComparisonExpression::BinaryComparison {
                operator,
                left,
                left_use,
                right,
                right_use,
            } => {
                let inverted_operator = match operator {
                    BinaryOperator::Equality => BinaryOperator::Inequality,
                    BinaryOperator::Inequality => BinaryOperator::Equality,
                    BinaryOperator::StrictEquality => BinaryOperator::StrictInequality,
                    BinaryOperator::StrictInequality => BinaryOperator::StrictEquality,
                    BinaryOperator::LessThan => BinaryOperator::GreaterEqualThan,
                    BinaryOperator::LessEqualThan => BinaryOperator::GreaterThan,
                    BinaryOperator::GreaterThan => BinaryOperator::LessEqualThan,
                    BinaryOperator::GreaterEqualThan => BinaryOperator::LessThan,
                    // For other operators, we don't have a direct inversion
                    _ => *operator,
                };

                ComparisonExpression::BinaryComparison {
                    operator: inverted_operator,
                    left: left.clone(),
                    left_use: left_use.clone(),
                    right: right.clone(),
                    right_use: right_use.clone(),
                }
            }
        }
    }
}

/// Specific control flow patterns
#[derive(Debug, Clone)]
pub enum ControlFlowKind {
    /// Sequential execution of blocks and nested structures
    Sequential { elements: Vec<SequentialElement> },

    /// Switch statement structure
    Switch {
        dispatch_block: NodeIndex,
        info: SwitchInfo,
        discriminator_value: Option<SSAValue>, // The SSA value being switched on
        discriminator_use: Option<RegisterUse>, // The specific use of the discriminator
        case_groups: Vec<CaseGroupStructure>,
        default_case: Option<StructureId>,
    },

    /// Conditional (if-else) structure
    Conditional {
        condition_block: NodeIndex,
        condition_expr: Option<ComparisonExpression>,
        true_branch: StructureId,
        false_branch: Option<StructureId>,
    },

    /// Loop structure
    Loop {
        loop_type: LoopType,
        header_block: NodeIndex,
        condition: Option<SSAValue>,
        condition_use: Option<RegisterUse>, // The specific use of the condition value
        body: StructureId,
        update: Option<StructureId>,
        break_target: Option<StructureId>,
        continue_target: Option<StructureId>,
    },

    /// Try-catch-finally structure
    TryCatch {
        try_body: StructureId,
        catch_clause: Option<CatchClause>,
        finally_clause: Option<FinallyClause>,
    },

    /// A single basic block
    BasicBlock {
        block: NodeIndex,
        instruction_count: usize,
        is_synthetic: bool, // Created by analysis vs original CFG block
    },

    /// Empty structure (for missing else branches, etc.)
    Empty,
}

/// An element in a sequential structure
#[derive(Debug, Clone)]
pub enum SequentialElement {
    /// A basic block
    Block(NodeIndex),
    /// A nested control flow structure
    Structure(StructureId),
}

/// A group of cases that share the same body (e.g., case 0: case 1: case 2:)
#[derive(Debug, Clone)]
pub struct CaseGroupStructure {
    /// The individual cases in this group
    pub cases: Vec<SwitchCase>,
    /// The shared body for all cases in this group
    pub body: StructureId,
    /// Whether this group falls through to the next
    pub fallthrough: Option<FallthroughInfo>,
    /// Whether this case needs a break statement
    pub needs_break: BreakRequirement,
}

/// Describes whether and why a switch case needs a break statement
#[derive(Debug, Clone, PartialEq)]
pub enum BreakRequirement {
    /// No break needed - case body always terminates (return, throw, etc.)
    NotNeeded { reason: TerminationReason },
    /// Break is required to prevent fallthrough
    Required,
    /// No break because this case intentionally falls through
    FallthroughIntended,
}

/// Reason why a case doesn't need a break statement
#[derive(Debug, Clone, PartialEq)]
pub enum TerminationReason {
    /// Case ends with return statement
    Return,
    /// Case ends with throw statement
    Throw,
    /// Case ends with an explicit break
    ExplicitBreak,
    /// Case ends with continue (in a loop context)
    Continue,
    /// Case body is empty
    EmptyBody,
}

/// A single case in a switch statement
#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub keys: Vec<CaseKey>,
    /// Comparison block for this specific case (in sparse switches)
    pub comparison_block: Option<NodeIndex>,
    /// Setup instructions executed when entering this case
    pub setup_instructions: Vec<SetupInstruction>,
    /// Execution order (which case is evaluated first)
    pub execution_order: usize,
}

/// Information about fallthrough behavior
#[derive(Debug, Clone)]
pub struct FallthroughInfo {
    pub to_case_index: usize, // Index in the cases vector
    pub blocks_to_duplicate: Vec<NodeIndex>,
    pub duplication_context: DuplicationContext,
}

/// A catch clause in a try-catch
#[derive(Debug, Clone)]
pub struct CatchClause {
    pub catch_block: NodeIndex,
    pub error_register: u8,
    pub error_ssa_value: SSAValue, // The SSA value assigned by the Catch instruction (first instruction)
    pub body: StructureId,
}

/// A finally clause in a try-catch-finally
#[derive(Debug, Clone)]
pub struct FinallyClause {
    pub finally_block: NodeIndex,
    pub body: StructureId,
    /// Instructions to skip at start (e.g., Catch in exception handler version)
    pub skip_start: usize,
    /// Instructions to skip at end (e.g., Throw in exception handler version)
    pub skip_end: usize,
}

/// Type of loop
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoopType {
    While,
    DoWhile,
    For,
    ForIn,
    ForOf,
}

/// Variable scope information
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    /// Variables declared in this scope
    pub variables: Vec<DuplicatedSSAValue>,
    /// Parent scope if any
    pub parent_scope: Option<StructureId>,
    /// Whether this is a function scope
    pub is_function_scope: bool,
}

/// Detailed plan for converting a switch statement
/// This is computed during analysis and attached to Switch structures
#[derive(Debug, Clone)]
pub struct SwitchConversionPlan {
    /// Fallthrough duplications needed
    pub fallthrough_duplications: Vec<FallthroughDuplication>,

    /// Blocks shared between multiple case groups
    pub shared_blocks: HashSet<NodeIndex>,

    /// Nested switches within case bodies
    pub nested_switches: Vec<NestedSwitchInfo>,

    /// PHI contributions from each case
    pub phi_contributions: HashMap<CaseGroup, HashMap<u8, SSAValue>>,

    /// Which setup instruction uses will be inlined
    pub inlined_setup_uses: Vec<(SSAValue, RegisterUse)>,
}

/// Information about fallthrough duplication
#[derive(Debug, Clone)]
pub struct FallthroughDuplication {
    pub from_case_group: CaseGroup,
    pub to_case_group: CaseGroup,
    pub blocks_to_duplicate: Vec<NodeIndex>,
    pub duplication_context: DuplicationContext,
}

/// Information about a nested switch
#[derive(Debug, Clone)]
pub struct NestedSwitchInfo {
    pub parent_case: CaseGroup,
    pub nested_dispatch_block: NodeIndex,
    pub eliminated_values: HashSet<SSAValue>,
}

impl ControlFlowPlan {
    /// Create a new control flow plan
    pub fn new() -> Self {
        // Create an empty root structure
        let root_id = StructureId(0);
        let root = ControlFlowStructure {
            id: root_id,
            duplication_info: None,
            scoped_variables: Vec::new(),
            entry_blocks: Vec::new(),
            exit_blocks: Vec::new(),
            kind: ControlFlowKind::Empty,
        };

        let mut structures = HashMap::new();
        structures.insert(root_id, root);

        Self {
            root: root_id,
            structures,
            block_declarations: HashMap::new(),
            declaration_strategies: HashMap::new(),
            use_strategies: HashMap::new(),
            consumed_uses: HashMap::new(),
            scope_boundaries: HashMap::new(),
            phi_deconstructions: HashMap::new(),
            phi_results: HashSet::new(),
            call_site_analysis: CallSiteAnalysis::new(),
            mandatory_inline: HashSet::new(),
            constructor_patterns: HashMap::new(),
            next_structure_id: 1,
        }
    }

    /// Create a new structure ID
    pub fn new_structure_id(&mut self) -> StructureId {
        let id = StructureId(self.next_structure_id);
        self.next_structure_id += 1;
        id
    }

    /// Add a control flow structure to the plan
    pub fn add_structure(&mut self, structure: ControlFlowStructure) -> StructureId {
        let id = structure.id;
        self.structures.insert(id, structure);
        id
    }

    /// Create and add a new control flow structure
    pub fn create_structure(&mut self, kind: ControlFlowKind) -> StructureId {
        let id = self.new_structure_id();
        let structure = ControlFlowStructure {
            id,
            duplication_info: None,
            scoped_variables: Vec::new(),
            entry_blocks: Vec::new(),
            exit_blocks: Vec::new(),
            kind,
        };
        self.add_structure(structure)
    }

    /// Get a structure by ID
    pub fn get_structure(&self, id: StructureId) -> Option<&ControlFlowStructure> {
        self.structures.get(&id)
    }

    /// Get a mutable structure by ID
    pub fn get_structure_mut(&mut self, id: StructureId) -> Option<&mut ControlFlowStructure> {
        self.structures.get_mut(&id)
    }

    /// Set the root structure
    pub fn set_root(&mut self, id: StructureId) {
        self.root = id;
    }

    /// Add a declaration point for a variable
    pub fn add_block_declaration(&mut self, block: NodeIndex, ssa_value: DuplicatedSSAValue) {
        let declarations = self.block_declarations.entry(block).or_default();
        // Only add if not already present (avoid duplicates)
        if !declarations.contains(&ssa_value) {
            declarations.push(ssa_value);
        }
    }

    /// Set the declaration strategy for an SSA value
    pub fn set_declaration_strategy(
        &mut self,
        ssa_value: DuplicatedSSAValue,
        strategy: DeclarationStrategy,
    ) {
        self.declaration_strategies.insert(ssa_value, strategy);
    }

    /// Get PHI deconstruction info for a block in a specific duplication context
    pub fn get_phi_info(
        &self,
        block_id: NodeIndex,
        context: Option<&DuplicationContext>,
    ) -> Option<&PhiDeconstructionInfo> {
        let key = (block_id, context.cloned());
        self.phi_deconstructions.get(&key)
    }

    /// Check if a structure always terminates (returns, throws, etc.)
    pub fn structure_always_terminates(&self, id: StructureId) -> Option<TerminationReason> {
        let structure = self.get_structure(id)?;

        match &structure.kind {
            ControlFlowKind::BasicBlock { block: _, .. } => {
                // Check if the block ends with a terminal instruction
                // This would need access to the CFG to check the actual instructions
                // For now, we'll return None and handle this in the builder
                None
            }
            ControlFlowKind::Sequential { elements } => {
                // Check if the last element terminates
                if let Some(last) = elements.last() {
                    match last {
                        SequentialElement::Block(_) => None, // Would need CFG access
                        SequentialElement::Structure(sid) => self.structure_always_terminates(*sid),
                    }
                } else {
                    Some(TerminationReason::EmptyBody)
                }
            }
            ControlFlowKind::Conditional {
                true_branch,
                false_branch,
                ..
            } => {
                // Both branches must terminate for the conditional to always terminate
                let true_terminates = self.structure_always_terminates(*true_branch);
                let false_terminates =
                    false_branch.and_then(|fb| self.structure_always_terminates(fb));

                // Only if both branches terminate with the same reason
                if true_terminates.is_some() && true_terminates == false_terminates {
                    true_terminates
                } else {
                    None
                }
            }
            ControlFlowKind::Switch {
                case_groups,
                default_case,
                ..
            } => {
                // All cases must terminate for the switch to always terminate
                let all_cases_terminate = case_groups
                    .iter()
                    .all(|group| self.structure_always_terminates(group.body).is_some());

                let default_terminates = default_case
                    .map(|dc| self.structure_always_terminates(dc).is_some())
                    .unwrap_or(false);

                if all_cases_terminate && default_terminates {
                    // Return the most common termination reason
                    Some(TerminationReason::Return)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Set PHI deconstruction info for a block in a specific duplication context
    pub fn set_phi_info(
        &mut self,
        block_id: NodeIndex,
        context: Option<DuplicationContext>,
        info: PhiDeconstructionInfo,
    ) {
        self.phi_deconstructions.insert((block_id, context), info);
    }

    /// Set the use strategy for an SSA value at a specific use site
    pub fn set_use_strategy(
        &mut self,
        ssa_value: DuplicatedSSAValue,
        use_site: RegisterUse,
        strategy: UseStrategy,
    ) {
        self.use_strategies.insert((ssa_value, use_site), strategy);
    }

    /// Mark a use as consumed (will be inlined)
    pub fn mark_use_consumed(&mut self, ssa_value: DuplicatedSSAValue, use_site: RegisterUse) {
        log::debug!(
            "ControlFlowPlan::mark_use_consumed: {} (context: {}) at block {} instruction {}",
            ssa_value.original_ssa_value().name(),
            ssa_value.context_description(),
            use_site.block_id.index(),
            use_site.instruction_idx.value()
        );
        self.consumed_uses
            .entry(ssa_value)
            .or_default()
            .insert(use_site);
    }

    /// Add scope information for a structure
    pub fn add_scope_info(&mut self, structure_id: StructureId, scope_info: ScopeInfo) {
        self.scope_boundaries.insert(structure_id, scope_info);
    }

    /// Get the declaration strategy for an SSA value
    pub fn get_declaration_strategy(
        &self,
        ssa_value: &DuplicatedSSAValue,
    ) -> Option<&DeclarationStrategy> {
        self.declaration_strategies.get(ssa_value)
    }

    /// Check if a duplicated SSA value actually exists (has a declaration)
    /// This is used to determine if we should use a duplicated variable name
    pub fn duplicated_ssa_exists(&self, ssa_value: &DuplicatedSSAValue) -> bool {
        // A duplicated SSA value exists if it has a declaration strategy
        self.declaration_strategies.contains_key(ssa_value)
    }

    /// Determine whether a variable reference should use a duplicated name
    ///
    /// This is the key logic that determines whether we use var0_ft_0_1 or var0
    /// based on where the SSA value is defined vs where it's being used.
    ///
    /// The rule: If an SSA value is defined inside a duplication context,
    /// and we're using it in the same duplication context, use the duplicated name.
    /// Otherwise, use the original name.
    pub fn should_use_duplicated_name(
        &self,
        ssa_value: &SSAValue,
        current_duplication_context: Option<&DuplicationContext>,
    ) -> Option<DuplicatedSSAValue> {
        // If we're not in a duplication context, use the original
        let context = current_duplication_context?;

        // The key insight: we should only use duplicated names for SSA values
        // that are DEFINED WITHIN the blocks being duplicated BY ACTUAL INSTRUCTIONS.
        // SSA values defined outside the duplicated blocks should use their original names.
        // PHI results, even if defined in duplicated blocks, should use original names.

        // Check which blocks are being duplicated in this context
        match context {
            DuplicationContext::SwitchFallthrough {
                from_case_index,
                to_case_index,
            } => {
                // Find which blocks are duplicated for this fallthrough
                // We need to look through the switch structure to find the fallthrough info
                for structure in self.structures.values() {
                    if let ControlFlowKind::Switch { case_groups, .. } = &structure.kind {
                        for group in case_groups {
                            if let Some(ref fallthrough) = group.fallthrough {
                                if let DuplicationContext::SwitchFallthrough {
                                    from_case_index: from,
                                    to_case_index: to,
                                } = &fallthrough.duplication_context
                                {
                                    if from == from_case_index && to == to_case_index {
                                        // Found the matching fallthrough
                                        // Check if this SSA value is defined in a duplicated block

                                        if !fallthrough
                                            .blocks_to_duplicate
                                            .contains(&ssa_value.def_site.block_id)
                                        {
                                            // SSA value is defined outside the duplicated blocks
                                            return None;
                                        }

                                        // SSA value is defined in a duplicated block
                                        // But we need to check if it's a PHI result or an actual instruction

                                        // PHI results should not use duplicated names even when in duplicated blocks
                                        // because they represent merges from predecessor blocks
                                        if self.phi_results.contains(ssa_value) {
                                            // This is a PHI result - use original name
                                            return None;
                                        }

                                        // It's an actual instruction - use duplicated name
                                        return Some(DuplicatedSSAValue {
                                            original: ssa_value.clone(),
                                            duplication_context: Some(context.clone()),
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
            DuplicationContext::SwitchBlockDuplication { .. } => {
                // For switch block duplication, check if the SSA is defined in the duplicated blocks
                // This would need similar logic to find which blocks are duplicated
            }
        }

        // Fallback: check if this duplicated SSA value is in block_declarations
        let dup_ssa = DuplicatedSSAValue {
            original: ssa_value.clone(),
            duplication_context: Some(context.clone()),
        };

        for declarations in self.block_declarations.values() {
            if declarations.contains(&dup_ssa) {
                return Some(dup_ssa);
            }
        }

        None
    }

    /// Get the use strategy for an SSA value at a use site
    pub fn get_use_strategy(
        &self,
        ssa_value: &DuplicatedSSAValue,
        use_site: &RegisterUse,
    ) -> Option<&UseStrategy> {
        self.use_strategies
            .get(&(ssa_value.clone(), use_site.clone()))
    }

    /// Check if a use has been marked as consumed
    pub fn is_use_consumed(&self, ssa_value: &DuplicatedSSAValue, use_site: &RegisterUse) -> bool {
        self.consumed_uses
            .get(ssa_value)
            .map(|uses| uses.contains(use_site))
            .unwrap_or(false)
    }

    /// Resolve PHI replacements for a value in a specific context
    /// Returns the replacement value if the original is a PHI result in a duplicated block
    pub fn resolve_phi_replacement(
        &self,
        original_value: &SSAValue,
        block_id: NodeIndex,
        context: Option<&DuplicationContext>,
    ) -> Option<SSAValue> {
        // Check if this block has PHI deconstruction info for the given context
        if let Some(phi_info) = self.get_phi_info(block_id, context) {
            // Look for a replacement for the original value
            phi_info.replacements.get(original_value).cloned()
        } else {
            None
        }
    }

    /// Check if a PHI result should be excluded from block declarations in a duplicated context
    pub fn is_phi_result_replaced(
        &self,
        ssa_value: &SSAValue,
        block_id: NodeIndex,
        context: Option<&DuplicationContext>,
    ) -> bool {
        if let Some(phi_info) = self.get_phi_info(block_id, context) {
            phi_info.replacements.contains_key(ssa_value)
        } else {
            false
        }
    }
}

impl ControlFlowStructure {
    /// Get all blocks contained within this structure (recursively)
    pub fn get_all_blocks(&self) -> Vec<NodeIndex> {
        let mut blocks = Vec::new();
        self.collect_blocks(&mut blocks);
        blocks
    }

    fn collect_blocks(&self, blocks: &mut Vec<NodeIndex>) {
        // Add entry and exit blocks
        blocks.extend(&self.entry_blocks);
        blocks.extend(&self.exit_blocks);

        // Collect blocks based on kind
        match &self.kind {
            ControlFlowKind::Sequential { elements } => {
                for element in elements {
                    if let SequentialElement::Block(block) = element {
                        blocks.push(*block);
                    }
                    // Note: Nested structures are handled separately via structure registry
                }
            }
            ControlFlowKind::Switch { dispatch_block, .. } => {
                blocks.push(*dispatch_block);
            }
            ControlFlowKind::Conditional {
                condition_block, ..
            } => {
                blocks.push(*condition_block);
            }
            ControlFlowKind::Loop {
                header_block,
                condition_use: _,
                ..
            } => {
                blocks.push(*header_block);
            }
            ControlFlowKind::TryCatch { .. } => {
                // Try-catch blocks are in nested structures
            }
            ControlFlowKind::BasicBlock { block, .. } => {
                blocks.push(*block);
            }
            ControlFlowKind::Empty => {}
        }
    }

    /// Check if this structure is a duplicate
    pub fn is_duplicate(&self) -> bool {
        self.duplication_info.is_some()
    }

    /// Get the original structure if this is a duplicate
    pub fn original_structure(&self) -> Option<StructureId> {
        self.duplication_info.as_ref().map(|info| info.original)
    }
}

impl Default for ControlFlowPlan {
    fn default() -> Self {
        Self::new()
    }
}

// Display implementations for debugging
use std::fmt;

impl fmt::Display for ControlFlowPlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ControlFlowPlan {{")?;
        writeln!(f, "  Root: {:?}", self.root)?;
        writeln!(f, "  Structures: {} total", self.structures.len())?;

        // Display the root structure tree
        if let Some(root_structure) = self.get_structure(self.root) {
            writeln!(f, "\n  Structure Tree:")?;
            self.fmt_structure(f, root_structure, 2)?;
        }

        // Declaration strategies are displayed separately by the CLI with better formatting
        // Commenting out to avoid duplication
        // if !self.declaration_strategies.is_empty() {
        //     writeln!(f, "\n  Declaration Strategies:")?;
        //     for (ssa_value, strategy) in &self.declaration_strategies {
        //         writeln!(f, "    {} -> {:?}", ssa_value, strategy)?;
        //     }
        // }

        // Display block declarations
        if !self.block_declarations.is_empty() {
            writeln!(f, "\n  Block Declarations:")?;
            for (block, values) in &self.block_declarations {
                let values_str: Vec<String> = values.iter().map(|v| v.to_string()).collect();
                writeln!(
                    f,
                    "    Block {}: [{}]",
                    block.index(),
                    values_str.join(", ")
                )?;
            }
        }

        // Display PHI deconstructions
        if !self.phi_deconstructions.is_empty() {
            writeln!(f, "\n  PHI Deconstructions:")?;
            for ((block_id, context), info) in &self.phi_deconstructions {
                write!(f, "    Block {}", block_id.index())?;
                if let Some(ctx) = context {
                    write!(f, " (duplicated: {:?})", ctx)?;
                } else {
                    write!(f, " (original)")?;
                }
                writeln!(f, ":")?;

                if !info.replacements.is_empty() {
                    writeln!(f, "      Replacements:")?;
                    for (phi_result, concrete_value) in &info.replacements {
                        writeln!(f, "        {} → {}", phi_result, concrete_value)?;
                    }
                }

                if !info.updated_phis.is_empty() {
                    writeln!(f, "      Updated PHIs:")?;
                    for updated_phi in &info.updated_phis {
                        write!(f, "        {} = ɸ(", updated_phi.result)?;
                        let operands_str: Vec<String> = updated_phi
                            .operands
                            .iter()
                            .map(|(pred, val)| format!("{}:{}", pred.index(), val))
                            .collect();
                        writeln!(f, "{})", operands_str.join(", "))?;
                    }
                }
            }
        }

        writeln!(f, "}}")
    }
}

impl ControlFlowPlan {
    /// Format a structure recursively for display
    fn fmt_structure(
        &self,
        f: &mut fmt::Formatter<'_>,
        structure: &ControlFlowStructure,
        indent: usize,
    ) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        write!(f, "{}Structure {} ", indent_str, structure.id.0)?;

        if structure.is_duplicate() {
            write!(f, "[DUPLICATE of {:?}] ", structure.original_structure())?;
        }

        match &structure.kind {
            ControlFlowKind::Sequential { elements } => {
                writeln!(f, "Sequential ({} elements)", elements.len())?;
                for element in elements {
                    match element {
                        SequentialElement::Block(block) => {
                            writeln!(f, "{}  Block {}", indent_str, block.index())?;
                        }
                        SequentialElement::Structure(id) => {
                            if let Some(nested) = self.get_structure(*id) {
                                self.fmt_structure(f, nested, indent + 1)?;
                            } else {
                                writeln!(f, "{}  Structure {:?} (not found)", indent_str, id)?;
                            }
                        }
                    }
                }
            }
            ControlFlowKind::Switch {
                dispatch_block,
                case_groups,
                default_case,
                ..
            } => {
                writeln!(f, "Switch (dispatch: {})", dispatch_block.index())?;
                for (i, group) in case_groups.iter().enumerate() {
                    let keys: Vec<_> = group.cases.iter().flat_map(|c| c.keys.iter()).collect();
                    writeln!(f, "{}  CaseGroup {}: keys={:?}", indent_str, i, keys)?;

                    // Show setup instructions if any (deduplicated by SSA value)
                    let mut unique_setup = std::collections::HashMap::new();
                    for case in &group.cases {
                        for instr in &case.setup_instructions {
                            unique_setup
                                .entry(instr.ssa_value.clone())
                                .or_insert(instr.clone());
                        }
                    }
                    let setup_instructions: Vec<_> = unique_setup.values().cloned().collect();
                    if !setup_instructions.is_empty() {
                        writeln!(
                            f,
                            "{}    Setup: {} instructions",
                            indent_str,
                            setup_instructions.len()
                        )?;
                        for setup in setup_instructions.iter().take(3) {
                            write!(f, "{}      - {} ← ", indent_str, setup.ssa_value)?;
                            if let Some(ref value) = setup.value {
                                writeln!(f, "{:?}", value)?;
                            } else {
                                writeln!(f, "<instruction>")?;
                            }
                        }
                        if setup_instructions.len() > 3 {
                            writeln!(
                                f,
                                "{}      ... and {} more",
                                indent_str,
                                setup_instructions.len() - 3
                            )?;
                        }
                    }

                    if let Some(ref fallthrough) = group.fallthrough {
                        if !fallthrough.blocks_to_duplicate.is_empty() {
                            writeln!(
                                f,
                                "{}    (falls through to group {} with duplication)",
                                indent_str, fallthrough.to_case_index
                            )?;
                        } else {
                            writeln!(
                                f,
                                "{}    (falls through to group {})",
                                indent_str, fallthrough.to_case_index
                            )?;
                        }
                    }
                    if let Some(body) = self.get_structure(group.body) {
                        self.fmt_structure(f, body, indent + 2)?;
                    }
                    // Show blocks to duplicate after the body
                    if let Some(ref fallthrough) = group.fallthrough {
                        if !fallthrough.blocks_to_duplicate.is_empty() {
                            let block_indices: Vec<_> = fallthrough
                                .blocks_to_duplicate
                                .iter()
                                .map(|b| format!("Block {}", b.index()))
                                .collect();
                            writeln!(
                                f,
                                "{}    Blocks to duplicate from group {}: [{}]",
                                indent_str,
                                fallthrough.to_case_index,
                                block_indices.join(", ")
                            )?;
                        }
                    }
                }
                if let Some(default) = default_case {
                    writeln!(f, "{}  Default:", indent_str)?;
                    if let Some(body) = self.get_structure(*default) {
                        self.fmt_structure(f, body, indent + 2)?;
                    }
                }
            }
            ControlFlowKind::Conditional {
                condition_block,
                condition_expr,
                true_branch,
                false_branch,
            } => {
                writeln!(f, "Conditional")?;
                writeln!(f, "{}  Condition block: {} (contains condition evaluation + any other instructions)",
                    indent_str, condition_block.index())?;
                if let Some(expr) = condition_expr {
                    writeln!(
                        f,
                        "{}  Expression: {}",
                        indent_str,
                        format_comparison_expression(expr)
                    )?;
                }
                writeln!(f, "{}  True branch:", indent_str)?;
                if let Some(branch) = self.get_structure(*true_branch) {
                    self.fmt_structure(f, branch, indent + 2)?;
                }
                if let Some(false_id) = false_branch {
                    writeln!(f, "{}  False branch:", indent_str)?;
                    if let Some(branch) = self.get_structure(*false_id) {
                        self.fmt_structure(f, branch, indent + 2)?;
                    }
                }
            }
            ControlFlowKind::Loop {
                loop_type,
                header_block,
                body,
                ..
            } => {
                writeln!(
                    f,
                    "Loop ({:?}, header: {})",
                    loop_type,
                    header_block.index()
                )?;
                if let Some(body_structure) = self.get_structure(*body) {
                    self.fmt_structure(f, body_structure, indent + 1)?;
                }
            }
            ControlFlowKind::TryCatch {
                try_body,
                catch_clause,
                finally_clause,
            } => {
                writeln!(f, "TryCatch")?;
                writeln!(f, "{}  Try:", indent_str)?;
                if let Some(try_structure) = self.get_structure(*try_body) {
                    self.fmt_structure(f, try_structure, indent + 2)?;
                }
                if let Some(catch) = catch_clause {
                    writeln!(f, "{}  Catch (r{}):", indent_str, catch.error_register)?;
                    if let Some(catch_structure) = self.get_structure(catch.body) {
                        self.fmt_structure(f, catch_structure, indent + 2)?;
                    }
                }
                if let Some(finally) = finally_clause {
                    writeln!(f, "{}  Finally:", indent_str)?;
                    if let Some(finally_structure) = self.get_structure(finally.body) {
                        self.fmt_structure(f, finally_structure, indent + 2)?;
                    }
                }
            }
            ControlFlowKind::BasicBlock {
                block,
                instruction_count,
                is_synthetic,
            } => {
                write!(f, "BasicBlock {}", block.index())?;
                if *is_synthetic {
                    write!(f, " [synthetic]")?;
                }
                if *instruction_count == 0 {
                    writeln!(f, " (empty)")?;
                } else if *instruction_count == 1 {
                    writeln!(f, " (1 instruction)")?;
                } else {
                    writeln!(f, " ({} instructions)", instruction_count)?;
                }
            }
            ControlFlowKind::Empty => {
                writeln!(f, "Empty")?;
            }
        }

        Ok(())
    }
}
