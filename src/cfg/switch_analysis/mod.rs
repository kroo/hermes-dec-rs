//! Switch pattern analysis module
//!
//! This module provides analysis for different types of switch patterns in the CFG

pub mod dense_switch;
pub mod sparse_switch;
pub mod switch_imm_analyzer;
pub mod switch_info;

// Re-export commonly used types
pub use dense_switch::{DenseSwitchAnalyzer, SetupSafetyChecker};
pub use sparse_switch::{find_sparse_switch_patterns, sparse_candidate_to_switch_region};
pub use switch_imm_analyzer::SwitchImmAnalyzer;
pub use switch_info::*;