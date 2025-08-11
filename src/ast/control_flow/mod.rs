//! Control flow handling and conversion
//!
//! This module provides utilities for handling control flow structures during AST generation,
//! including converters for blocks, conditionals, switches, and loops.

// Infrastructure modules
pub mod block_sequencer;
pub mod control_flow_builder;
pub mod region_analyzer;

// Control flow converters
pub mod block_converter;
pub mod conditional_converter;
pub mod switch_converter;

// Re-export infrastructure types
pub use block_sequencer::BlockSequencer;
pub use control_flow_builder::ControlFlowBuilder;
pub use region_analyzer::RegionAnalyzer;

// Re-export converter types
pub use block_converter::{BlockConversionError, BlockConversionStats, BlockToStatementConverter};
pub use conditional_converter::ConditionalConverter;
pub use switch_converter::{SwitchConversionError, SwitchConverter};

/// Information about a control flow region in the CFG
#[derive(Debug, Clone)]
pub struct ControlFlowRegion {
    /// Entry block of the region
    pub entry: petgraph::graph::NodeIndex,
    /// Exit block (where control flow converges after the region)
    pub exit: Option<petgraph::graph::NodeIndex>,
    /// All blocks that are part of this region
    pub blocks: Vec<petgraph::graph::NodeIndex>,
    /// Blocks that are targets of break statements
    pub break_targets: std::collections::HashSet<petgraph::graph::NodeIndex>,
    /// Blocks that are targets of continue statements  
    pub continue_targets: std::collections::HashSet<petgraph::graph::NodeIndex>,
}
