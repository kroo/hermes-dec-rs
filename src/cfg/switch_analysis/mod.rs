//! Switch pattern analysis module
//!
//! This module provides analysis for different types of switch patterns in the CFG:
//! - Dense switches: Use SwitchImm instruction with jump tables (handled directly in AST conversion)
//! - Sparse switches: Use chains of JStrictEqual comparisons
//!
//! The sparse switch analysis happens in two phases:
//! 1. Region detection (sparse_switch.rs) - identifies switch regions during CFG analysis
//! 2. Detailed analysis (sparse_switch_analyzer.rs) - extracts switch info for AST conversion

pub mod dense_switch_analyzer; // Currently unused - dense switches are handled directly
pub mod sparse_switch; // Region detection for CFG analysis
pub mod sparse_switch_analyzer; // Detailed analysis for AST conversion
pub mod switch_info; // Common types for switch information

// Re-export commonly used types
pub use sparse_switch::{find_sparse_switch_patterns, sparse_candidate_to_switch_region};
pub use sparse_switch_analyzer::{SetupSafetyChecker, SparseSwitchAnalyzer};
pub use switch_info::*;

// Note: DenseSwitchAnalyzer is not currently used - dense switches are handled
// directly in convert_dense_switch_region without needing a separate analyzer
