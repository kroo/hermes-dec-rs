//! Nested switch detection and analysis
//!
//! This module identifies switches that are nested within other switches,
//! which require special handling during AST generation.

use petgraph::graph::NodeIndex;
use std::collections::HashSet;

use crate::cfg::{Cfg, analysis::SwitchRegion};

/// Information about nested switches within a switch statement
#[derive(Debug, Clone)]
pub struct NestedSwitchAnalysis {
    /// Indices of switch regions that are nested inside other switches
    pub nested_switch_indices: HashSet<usize>,
    /// Mapping from outer switch region index to its nested switch indices
    pub nesting_map: Vec<Vec<usize>>,
}

impl NestedSwitchAnalysis {
    /// Analyze which switches are nested inside other switches
    pub fn analyze(regions: &[SwitchRegion]) -> Self {
        let mut nested_switch_indices = HashSet::new();
        let mut nesting_map = vec![Vec::new(); regions.len()];
        
        // Check each pair of switches to see if one is nested in the other
        for (outer_idx, outer_region) in regions.iter().enumerate() {
            for (inner_idx, inner_region) in regions.iter().enumerate() {
                if outer_idx != inner_idx {
                    // Check if inner switch is nested within outer switch's case bodies
                    for (_, case_analysis) in &outer_region.case_analyses {
                        if case_analysis.blocks.contains(&inner_region.dispatch) {
                            nested_switch_indices.insert(inner_idx);
                            nesting_map[outer_idx].push(inner_idx);
                            break;
                        }
                    }
                }
            }
        }
        
        Self {
            nested_switch_indices,
            nesting_map,
        }
    }
    
    /// Check if a switch region is nested inside another switch
    pub fn is_nested(&self, region_index: usize) -> bool {
        self.nested_switch_indices.contains(&region_index)
    }
    
    /// Get the indices of switches nested within a given switch
    pub fn get_nested_switches(&self, outer_index: usize) -> &[usize] {
        self.nesting_map.get(outer_index).map(|v| v.as_slice()).unwrap_or(&[])
    }
    
    /// Check if a switch is in a case body
    /// 
    /// A switch is in the case body if:
    /// 1. The switch dispatch block IS the case target block (nested switch as first statement)
    /// 2. OR the switch dispatch block is reachable from the case target block
    pub fn is_switch_in_case_body(
        case_target: NodeIndex,
        switch_region: &SwitchRegion,
        cfg: &Cfg,
    ) -> bool {
        // Common pattern: the case target block IS the dispatch block of the nested switch
        if switch_region.dispatch == case_target {
            return true; // This case immediately starts with a nested switch
        }
        
        // Check if case target has a direct edge to the switch dispatch
        use petgraph::visit::EdgeRef;
        for edge in cfg.graph().edges(case_target) {
            if edge.target() == switch_region.dispatch {
                return true;
            }
        }
        
        // Also check if the switch dispatch is the case target + 1 (common pattern)
        // This handles the case where instructions are split across consecutive blocks
        if switch_region.dispatch.index() == case_target.index() + 1 {
            return true;
        }
        
        false
    }
    
    /// Check if a case group contains a nested switch
    /// 
    /// Returns the switch region if the case contains a nested switch, None otherwise
    pub fn find_nested_switch_in_case<'a>(
        case_target: NodeIndex,
        regions: &'a [SwitchRegion],
        cfg: &Cfg,
    ) -> Option<&'a SwitchRegion> {
        for region in regions {
            if Self::is_switch_in_case_body(case_target, region, cfg) {
                return Some(region);
            }
        }
        None
    }
}