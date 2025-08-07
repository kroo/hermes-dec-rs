//! Shared infrastructure for control flow AST generation
//!
//! This module provides common functionality for converting CFG control flow
//! structures (loops, switches, try-catch) into JavaScript AST nodes.

pub mod block_sequencer;
pub mod control_flow_builder;
pub mod region_analyzer;

pub use block_sequencer::BlockSequencer;
pub use control_flow_builder::ControlFlowBuilder;
pub use region_analyzer::RegionAnalyzer;

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
