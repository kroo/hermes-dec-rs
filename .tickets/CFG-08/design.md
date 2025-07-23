# CFG-08 Design: Switch dispatch grouping

## Overview
Implement switch region detection by grouping switch cases and finding their common post-dominator join point.

## Implementation Plan

### 1. Switch Region Structure
```rust
#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub case_index: usize,  // Switch case index
    pub case_head: NodeIndex, // First block of this case
}

#[derive(Debug, Clone)]
pub struct SwitchRegion {
    pub dispatch: NodeIndex,  // Block containing switch instruction
    pub cases: Vec<SwitchCase>,
    pub default_head: Option<NodeIndex>, // Default case head (if any)
    pub join_block: NodeIndex, // Common post-dominator of all cases
}

#[derive(Debug, Clone)]
pub struct SwitchAnalysis {
    pub regions: Vec<SwitchRegion>,
    pub node_to_regions: HashMap<NodeIndex, Vec<usize>>, // node -> region indices
}
```

### 2. Switch Region Detection Algorithm
```rust
impl Cfg {
    pub fn find_switch_regions(&self) -> SwitchAnalysis {
        let mut regions = Vec::new();
        let mut node_to_regions = HashMap::new();
        let post_doms = self.analyze_post_dominators();
        
        // Find all switch dispatch blocks
        let switch_dispatches = self.find_switch_dispatches();
        
        for &dispatch in &switch_dispatches {
            if let Some(region) = self.detect_switch_region(dispatch, &post_doms) {
                let region_idx = regions.len();
                regions.push(region.clone());
                
                // Update node_to_regions mapping
                node_to_regions.entry(region.dispatch).or_default().push(region_idx);
                node_to_regions.entry(region.join_block).or_default().push(region_idx);
                for case in &region.cases {
                    node_to_regions.entry(case.case_head).or_default().push(region_idx);
                }
                if let Some(default) = region.default_head {
                    node_to_regions.entry(default).or_default().push(region_idx);
                }
            }
        }
        
        SwitchAnalysis { regions, node_to_regions }
    }
}
```

### 3. Switch Dispatch Detection
```rust
impl Cfg {
    fn find_switch_dispatches(&self) -> Vec<NodeIndex> {
        let mut dispatches = Vec::new();
        
        for node in self.graph.node_indices() {
            let mut switch_edges = 0;
            let mut default_edges = 0;
            
            for edge in self.graph.edges_directed(node, petgraph::Direction::Outgoing) {
                match edge.weight() {
                    EdgeKind::Switch(_) => switch_edges += 1,
                    EdgeKind::Default => default_edges += 1,
                    _ => {}
                }
            }
            
            // Valid switch dispatch has multiple switch edges (and optionally default)
            if switch_edges >= 2 {
                dispatches.push(node);
            }
        }
        
        dispatches
    }
}
```

### 4. Switch Region Detection
```rust
impl Cfg {
    fn detect_switch_region(&self, dispatch: NodeIndex, post_doms: &PostDominatorAnalysis) -> Option<SwitchRegion> {
        // Collect all switch cases and default
        let (cases, default_head) = self.collect_switch_cases(dispatch)?;
        
        if cases.is_empty() {
            return None;
        }
        
        // Find common post-dominator of all case heads
        let case_heads: Vec<NodeIndex> = cases.iter().map(|c| c.case_head).collect();
        let join_block = self.find_common_post_dominator(&case_heads, post_doms)?;
        
        // Validate the region structure
        if self.is_valid_switch_region(dispatch, &cases, default_head, join_block) {
            Some(SwitchRegion {
                dispatch,
                cases,
                default_head,
                join_block,
            })
        } else {
            None
        }
    }
}
```

### 5. Switch Case Collection
```rust
impl Cfg {
    fn collect_switch_cases(&self, dispatch: NodeIndex) -> Option<(Vec<SwitchCase>, Option<NodeIndex>)> {
        let mut cases = Vec::new();
        let mut default_head = None;
        
        for edge in self.graph.edges_directed(dispatch, petgraph::Direction::Outgoing) {
            match edge.weight() {
                EdgeKind::Switch(idx) => {
                    cases.push(SwitchCase {
                        case_index: *idx,
                        case_head: edge.target(),
                    });
                }
                EdgeKind::Default => {
                    default_head = Some(edge.target());
                }
                _ => {}
            }
        }
        
        // Sort cases by index for consistent ordering
        cases.sort_by_key(|c| c.case_index);
        
        if cases.is_empty() {
            None
        } else {
            Some((cases, default_head))
        }
    }
}
```

### 6. Common Post-Dominator for Multiple Nodes
```rust
impl Cfg {
    fn find_common_post_dominator(&self, nodes: &[NodeIndex], post_doms: &PostDominatorAnalysis) -> Option<NodeIndex> {
        if nodes.is_empty() {
            return None;
        }
        
        // Start with post-dominators of first node
        let mut common_doms = post_doms.get_post_dominators(nodes[0]).clone();
        
        // Intersect with post-dominators of all other nodes
        for &node in &nodes[1..] {
            let node_doms = post_doms.get_post_dominators(node);
            common_doms = common_doms.intersection(node_doms).cloned().collect();
        }
        
        // Find lowest (most distant from root) common post-dominator
        common_doms.into_iter()
            .filter(|&dom| !nodes.contains(&dom)) // Exclude case heads themselves
            .min_by_key(|&dom| {
                self.compute_post_dominator_height(dom, post_doms)
            })
    }
}
```

### 7. Switch Region Validation
```rust
impl Cfg {
    fn is_valid_switch_region(&self, dispatch: NodeIndex, cases: &[SwitchCase], default_head: Option<NodeIndex>, join_block: NodeIndex) -> bool {
        // Ensure all case heads are different
        let mut heads: HashSet<NodeIndex> = cases.iter().map(|c| c.case_head).collect();
        if let Some(default) = default_head {
            if heads.contains(&default) {
                return false;
            }
            heads.insert(default);
        }
        
        if heads.len() != cases.len() + default_head.is_some() as usize {
            return false;
        }
        
        // Ensure join_block is reachable from all case heads
        for case in cases {
            if !self.is_reachable(case.case_head, join_block) {
                return false;
            }
        }
        
        if let Some(default) = default_head {
            if !self.is_reachable(default, join_block) {
                return false;
            }
        }
        
        true
    }
}
```

### 8. Helper Methods
```rust
impl SwitchAnalysis {
    pub fn get_regions_containing_node(&self, node: NodeIndex) -> &[usize] {
        self.node_to_regions.get(&node).map(|v| v.as_slice()).unwrap_or(&[])
    }
    
    pub fn is_node_in_switch(&self, node: NodeIndex) -> bool {
        self.node_to_regions.contains_key(&node)
    }
    
    pub fn get_switch_for_dispatch(&self, dispatch: NodeIndex) -> Option<&SwitchRegion> {
        self.node_to_regions.get(&dispatch)
            .and_then(|indices| indices.first())
            .map(|&idx| &self.regions[idx])
    }
}
```

## Test Cases
- Switch with 3 cases + default
- Sparse switch (gaps in case values)
- Switch with only default case
- Nested switch structures
- Switch with early returns (no join block)
- Invalid structures (duplicate case heads)

## Considerations
- Switch cases must be grouped by their dispatch block
- All case heads must have a common post-dominator
- Default case is optional but must be handled
- Case indices should be preserved for code generation
- Performance: O(N²) in worst case for common post-dominator computation
- May need to handle irreducible switch structures 