//! Switch pattern analysis module
//!
//! This module provides analysis for different types of switch patterns in the CFG:
//! - Dense switches: Use SwitchImm instruction with jump tables (handled directly in AST conversion)
//! - Sparse switches: Use chains of JStrictEqual comparisons
//!
//! The sparse switch analysis happens in two phases:
//! 1. Detection phase (sparse_switch_detector.rs) - identifies switch regions during CFG analysis
//! 2. Analysis phase (sparse_switch_analyzer.rs) - extracts detailed switch info for AST conversion
//!
//! Note: Dense switches are currently handled directly in switch_converter::convert_dense_switch_region
//! without a separate analyzer, as they can be processed in a single pass.

pub mod sparse_switch_detector; // Early pattern detection during CFG analysis
pub mod sparse_switch_analyzer; // Detailed analysis for AST conversion
pub mod switch_info; // Common types for switch information

// Re-export commonly used types
pub use sparse_switch_detector::{find_sparse_switch_patterns, sparse_candidate_to_switch_region};
pub use sparse_switch_analyzer::{SetupSafetyChecker, SparseSwitchAnalyzer};
pub use switch_info::*;
