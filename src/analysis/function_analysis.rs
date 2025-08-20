//! Function-level analysis container
//!
//! This module provides a container for all analyses performed on a single function,
//! ensuring that all necessary analysis data is available before AST conversion begins.

use crate::{
    cfg::{ssa::SSAAnalysis, Cfg},
    hbc::{function_table::HbcFunction, HbcFile},
};

/// Contains all analysis results for a single function
pub struct FunctionAnalysis<'a> {
    /// The function being analyzed
    pub function: HbcFunction,

    /// The function's control flow graph
    pub cfg: Cfg<'a>,

    /// SSA analysis for the function
    pub ssa: SSAAnalysis,

    /// Reference to the containing HBC file
    pub hbc_file: &'a HbcFile<'a>,

    /// Function index in the HBC file
    pub function_index: u32,
}

impl<'a> FunctionAnalysis<'a> {
    /// Create a new function analysis container
    pub fn new(
        function: HbcFunction,
        cfg: Cfg<'a>,
        ssa: SSAAnalysis,
        hbc_file: &'a HbcFile<'a>,
        function_index: u32,
    ) -> Self {
        Self {
            function,
            cfg,
            ssa,
            hbc_file,
            function_index,
        }
    }

    /// Create a value tracker for this function
    pub fn value_tracker(&self) -> super::ValueTracker<'_> {
        super::ValueTracker::new(&self.cfg, &self.ssa, self.hbc_file)
    }
}
