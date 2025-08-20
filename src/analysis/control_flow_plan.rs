//! Control Flow Plan - High-level IR between CFG analysis and AST generation
//!
//! This module defines the ControlFlowPlan structure which captures all control flow
//! patterns and analysis decisions before AST generation. This allows us to make
//! all important decisions (declaration points, duplication contexts, value elimination)
//! during the analysis phase rather than during AST generation.

use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, UseStrategy};
use crate::cfg::ssa::types::DuplicationContext;
use crate::cfg::ssa::{DuplicatedSSAValue, RegisterUse, SSAValue};
use crate::cfg::switch_analysis::switch_info::{CaseGroup, CaseKey, SetupInstruction, SwitchInfo};
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

/// Unique identifier for a control flow structure
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructureId(pub usize);

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

/// Specific control flow patterns
#[derive(Debug, Clone)]
pub enum ControlFlowKind {
    /// Sequential execution of blocks and nested structures
    Sequential {
        elements: Vec<SequentialElement>,
    },
    
    /// Switch statement structure
    Switch {
        dispatch_block: NodeIndex,
        info: SwitchInfo,
        case_groups: Vec<CaseGroupStructure>,
        default_case: Option<StructureId>,
    },
    
    /// Conditional (if-else) structure
    Conditional {
        condition_block: NodeIndex,
        condition_expr: Option<SSAValue>,
        true_branch: StructureId,
        false_branch: Option<StructureId>,
    },
    
    /// Loop structure
    Loop {
        loop_type: LoopType,
        header_block: NodeIndex,
        condition: Option<SSAValue>,
        body: StructureId,
        update: Option<StructureId>,
        break_target: Option<StructureId>,
        continue_target: Option<StructureId>,
    },
    
    /// Try-catch-finally structure
    TryCatch {
        try_body: StructureId,
        catch_clause: Option<CatchClause>,
        finally_body: Option<StructureId>,
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
    pub body: StructureId,
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
        self.block_declarations
            .entry(block)
            .or_default()
            .push(ssa_value);
    }
    
    /// Set the declaration strategy for an SSA value
    pub fn set_declaration_strategy(&mut self, ssa_value: DuplicatedSSAValue, strategy: DeclarationStrategy) {
        self.declaration_strategies.insert(ssa_value, strategy);
    }
    
    /// Set the use strategy for an SSA value at a specific use site
    pub fn set_use_strategy(&mut self, ssa_value: DuplicatedSSAValue, use_site: RegisterUse, strategy: UseStrategy) {
        self.use_strategies.insert((ssa_value, use_site), strategy);
    }
    
    /// Mark a use as consumed (will be inlined)
    pub fn mark_use_consumed(&mut self, ssa_value: DuplicatedSSAValue, use_site: RegisterUse) {
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
    pub fn get_declaration_strategy(&self, ssa_value: &DuplicatedSSAValue) -> Option<&DeclarationStrategy> {
        self.declaration_strategies.get(ssa_value)
    }
    
    /// Get the use strategy for an SSA value at a use site
    pub fn get_use_strategy(&self, ssa_value: &DuplicatedSSAValue, use_site: &RegisterUse) -> Option<&UseStrategy> {
        self.use_strategies.get(&(ssa_value.clone(), use_site.clone()))
    }
    
    /// Check if a use has been marked as consumed
    pub fn is_use_consumed(&self, ssa_value: &DuplicatedSSAValue, use_site: &RegisterUse) -> bool {
        self.consumed_uses
            .get(ssa_value)
            .map(|uses| uses.contains(use_site))
            .unwrap_or(false)
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
            ControlFlowKind::Conditional { condition_block, .. } => {
                blocks.push(*condition_block);
            }
            ControlFlowKind::Loop { header_block, .. } => {
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
        
        // Display declaration strategies
        if !self.declaration_strategies.is_empty() {
            writeln!(f, "\n  Declaration Strategies:")?;
            for (ssa_value, strategy) in &self.declaration_strategies {
                writeln!(f, "    {} -> {:?}", ssa_value, strategy)?;
            }
        }
        
        // Display block declarations
        if !self.block_declarations.is_empty() {
            writeln!(f, "\n  Block Declarations:")?;
            for (block, values) in &self.block_declarations {
                let values_str: Vec<String> = values.iter().map(|v| v.to_string()).collect();
                writeln!(f, "    Block {}: [{}]", block.index(), values_str.join(", "))?;
            }
        }
        
        writeln!(f, "}}")
    }
}

impl ControlFlowPlan {
    /// Format a structure recursively for display
    fn fmt_structure(&self, f: &mut fmt::Formatter<'_>, structure: &ControlFlowStructure, indent: usize) -> fmt::Result {
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
            ControlFlowKind::Switch { dispatch_block, case_groups, default_case, .. } => {
                writeln!(f, "Switch (dispatch: {})", dispatch_block.index())?;
                for (i, group) in case_groups.iter().enumerate() {
                    let keys: Vec<_> = group.cases.iter()
                        .flat_map(|c| c.keys.iter())
                        .collect();
                    writeln!(f, "{}  CaseGroup {}: keys={:?}", indent_str, i, keys)?;
                    
                    // Show setup instructions if any (deduplicated by SSA value)
                    let mut unique_setup = std::collections::HashMap::new();
                    for case in &group.cases {
                        for instr in &case.setup_instructions {
                            unique_setup.entry(instr.ssa_value.clone())
                                .or_insert(instr.clone());
                        }
                    }
                    let setup_instructions: Vec<_> = unique_setup.values().cloned().collect();
                    if !setup_instructions.is_empty() {
                        writeln!(f, "{}    Setup: {} instructions", indent_str, setup_instructions.len())?;
                        for setup in setup_instructions.iter().take(3) {
                            write!(f, "{}      - {} ← ", indent_str, setup.ssa_value)?;
                            if let Some(ref value) = setup.value {
                                writeln!(f, "{:?}", value)?;
                            } else {
                                writeln!(f, "<instruction>")?;
                            }
                        }
                        if setup_instructions.len() > 3 {
                            writeln!(f, "{}      ... and {} more", indent_str, setup_instructions.len() - 3)?;
                        }
                    }
                    
                    if let Some(ref fallthrough) = group.fallthrough {
                        if !fallthrough.blocks_to_duplicate.is_empty() {
                            writeln!(f, "{}    (falls through to group {} with duplication)", 
                                indent_str, fallthrough.to_case_index)?;
                        } else {
                            writeln!(f, "{}    (falls through to group {})", indent_str, fallthrough.to_case_index)?;
                        }
                    }
                    if let Some(body) = self.get_structure(group.body) {
                        self.fmt_structure(f, body, indent + 2)?;
                    }
                    // Show blocks to duplicate after the body
                    if let Some(ref fallthrough) = group.fallthrough {
                        if !fallthrough.blocks_to_duplicate.is_empty() {
                            let block_indices: Vec<_> = fallthrough.blocks_to_duplicate.iter()
                                .map(|b| format!("Block {}", b.index()))
                                .collect();
                            writeln!(f, "{}    Blocks to duplicate from group {}: [{}]", 
                                indent_str, fallthrough.to_case_index, block_indices.join(", "))?;
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
            ControlFlowKind::Conditional { condition_block, true_branch, false_branch, .. } => {
                writeln!(f, "Conditional (condition: {})", condition_block.index())?;
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
            ControlFlowKind::Loop { loop_type, header_block, body, .. } => {
                writeln!(f, "Loop ({:?}, header: {})", loop_type, header_block.index())?;
                if let Some(body_structure) = self.get_structure(*body) {
                    self.fmt_structure(f, body_structure, indent + 1)?;
                }
            }
            ControlFlowKind::TryCatch { try_body, catch_clause, finally_body } => {
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
                if let Some(finally) = finally_body {
                    writeln!(f, "{}  Finally:", indent_str)?;
                    if let Some(finally_structure) = self.get_structure(*finally) {
                        self.fmt_structure(f, finally_structure, indent + 2)?;
                    }
                }
            }
            ControlFlowKind::BasicBlock { block, instruction_count, is_synthetic } => {
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