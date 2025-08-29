//! HBC file-level analysis container
//!
//! This module provides a container for all analyses performed on an entire HBC file,
//! including cross-function analysis and global tracking.

use crate::{cfg::ssa::construct_ssa, hbc::HbcFile};
use std::collections::HashMap;
use std::sync::Arc;

use super::{FunctionAnalysis, GlobalAnalysisResult, GlobalSSAAnalyzer};

/// Contains all analysis results for an entire HBC file
pub struct HbcAnalysis<'a> {
    /// The HBC file being analyzed
    pub hbc_file: &'a HbcFile<'a>,

    /// Global analysis results (cross-function SSA, etc.)
    pub global_analysis: Arc<GlobalAnalysisResult>,

    /// Constructor detection results (placeholder)
    pub constructor_info: HashMap<u32, ()>,

    /// Function classification results (placeholder)
    pub function_classifier: HashMap<u32, ()>,

    /// Per-function analysis cache
    function_analyses: HashMap<u32, FunctionAnalysis<'a>>,
}

impl<'a> HbcAnalysis<'a> {
    /// Create a new HBC file analysis by running all analyses
    pub fn analyze(hbc_file: &'a HbcFile<'a>) -> Result<Self, String> {
        // Run global SSA analysis
        let global_analysis_result = GlobalSSAAnalyzer::analyze(hbc_file)
            .map_err(|e| format!("Global SSA analysis failed: {:?}", e))?;
        let global_analysis = Arc::new(global_analysis_result);

        // Run constructor detection (needs fixing - for now create empty HashMap)
        let constructor_info = HashMap::new();

        // Run function classification (needs fixing - for now create empty HashMap)
        let function_classifier = HashMap::new();

        Ok(Self {
            hbc_file,
            global_analysis,
            constructor_info,
            function_classifier,
            function_analyses: HashMap::new(),
        })
    }

    /// Get or create function analysis for a specific function
    pub fn get_function_analysis(
        &mut self,
        function_index: u32,
    ) -> Result<&FunctionAnalysis<'a>, String> {
        log::debug!(
            "HbcAnalysis::get_function_analysis for function {}",
            function_index
        );

        // Check cache first
        if self.function_analyses.contains_key(&function_index) {
            log::debug!("Function {} already in cache", function_index);
            return Ok(&self.function_analyses[&function_index]);
        }

        // Get the function
        log::debug!("Getting function {} from HBC file...", function_index);
        let function = self
            .hbc_file
            .functions
            .get(function_index, self.hbc_file)
            .map_err(|e| format!("Function {} not found: {:?}", function_index, e))?;
        log::debug!(
            "Got function {} with {} instructions",
            function_index,
            function.instructions.len()
        );

        // Build CFG
        log::debug!("Building CFG for function {}...", function_index);
        let mut cfg = crate::cfg::Cfg::new(self.hbc_file, function_index);
        cfg.build();
        log::debug!(
            "CFG built for function {} - {} nodes",
            function_index,
            cfg.graph().node_count()
        );

        // Build SSA
        log::debug!("Building SSA for function {}...", function_index);
        let ssa = construct_ssa(&cfg, function_index)
            .map_err(|e| format!("Failed to build SSA: {:?}", e))?;
        log::debug!("SSA built for function {}", function_index);

        // Create function analysis
        log::debug!(
            "Creating function analysis for function {}...",
            function_index
        );
        let analysis = FunctionAnalysis::new(function, cfg, ssa, self.hbc_file, function_index);

        // Cache it
        self.function_analyses.insert(function_index, analysis);
        log::debug!(
            "Function analysis created and cached for function {}",
            function_index
        );

        // Return reference
        Ok(&self.function_analyses[&function_index])
    }

    /// Get the global analysis results
    pub fn global_analysis(&self) -> &Arc<GlobalAnalysisResult> {
        &self.global_analysis
    }

    /// Get function analysis if it already exists (doesn't create new)
    pub fn get_function_analysis_ref(&self, function_index: u32) -> Option<&FunctionAnalysis<'a>> {
        self.function_analyses.get(&function_index)
    }
}
