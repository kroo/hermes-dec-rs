# CFG-07 Design: Robust if/else region detection

## Overview
Implement robust if/else region detection using post-dominator analysis to accurately identify conditional sources, then/else heads, and join points.

## Implementation Plan

### 1. If/Else Region Structure
```rust
#[derive(Debug, Clone)]
pub struct IfElseRegion {
    pub conditional_source: NodeIndex,  // S: block with conditional jump
    pub then_head: NodeIndex,          // Then branch head
    pub else_head: NodeIndex,          // Else branch head  
    pub join_block: NodeIndex,         // J: lowest common post-dominator
}

#[derive(Debug, Clone)]
pub struct IfElseAnalysis {
    pub regions: Vec<IfElseRegion>,
    pub node_to_regions: HashMap<NodeIndex, Vec<usize>>, // node -> region indices
}
```

### 2. Robust If/Else Detection Algorithm
```rust
impl Cfg {
    pub fn find_if_else_regions(&self) -> IfElseAnalysis {
        let mut regions = Vec::new();
        let mut node_to_regions = HashMap::new();
        let post_doms = self.analyze_post_dominators();
        
        // Find all conditional sources (blocks with True/False edges)
        let conditional_sources = self.find_conditional_sources();
        
        for &source in &conditional_sources {
            if let Some(region) = self.detect_if_else_region(source, &post_doms) {
                let region_idx = regions.len();
                regions.push(region.clone());
                
                // Update node_to_regions mapping
                for &node in &[region.conditional_source, region.then_head, region.else_head, region.join_block] {
                    node_to_regions.entry(node).or_default().push(region_idx);
                }
            }
        }
        
        IfElseAnalysis { regions, node_to_regions }
    }
}
```

### 3. Conditional Source Detection
```rust
impl Cfg {
    fn find_conditional_sources(&self) -> Vec<NodeIndex> {
        let mut sources = Vec::new();
        
        for node in self.graph.node_indices() {
            let mut true_edges = 0;
            let mut false_edges = 0;
            
            for edge in self.graph.edges_directed(node, petgraph::Direction::Outgoing) {
                match edge.weight() {
                    EdgeKind::True => true_edges += 1,
                    EdgeKind::False => false_edges += 1,
                    _ => {}
                }
            }
            
            // Valid if/else source has exactly one True and one False edge
            if true_edges == 1 && false_edges == 1 {
                sources.push(node);
            }
        }
        
        sources
    }
}
```

### 4. If/Else Region Detection
```rust
impl Cfg {
    fn detect_if_else_region(&self, source: NodeIndex, post_doms: &PostDominatorAnalysis) -> Option<IfElseRegion> {
        // Find then and else heads
        let (then_head, else_head) = self.find_then_else_heads(source)?;
        
        // Find lowest common post-dominator of both heads
        let join_block = self.find_lowest_common_post_dominator(then_head, else_head, post_doms)?;
        
        // Validate the region structure
        if self.is_valid_if_else_region(source, then_head, else_head, join_block) {
            Some(IfElseRegion {
                conditional_source: source,
                then_head,
                else_head,
                join_block,
            })
        } else {
            None
        }
    }
}
```

### 5. Then/Else Head Detection
```rust
impl Cfg {
    fn find_then_else_heads(&self, source: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        let mut then_head = None;
        let mut else_head = None;
        
        for edge in self.graph.edges_directed(source, petgraph::Direction::Outgoing) {
            match edge.weight() {
                EdgeKind::True => then_head = Some(edge.target()),
                EdgeKind::False => else_head = Some(edge.target()),
                _ => {}
            }
        }
        
        then_head.zip(else_head)
    }
}
```

### 6. Lowest Common Post-Dominator
```rust
impl Cfg {
    fn find_lowest_common_post_dominator(&self, node1: NodeIndex, node2: NodeIndex, post_doms: &PostDominatorAnalysis) -> Option<NodeIndex> {
        let doms1 = post_doms.get_post_dominators(node1);
        let doms2 = post_doms.get_post_dominators(node2);
        
        // Find intersection
        let common_doms: HashSet<_> = doms1.intersection(doms2).collect();
        
        // Find lowest (most distant from root) common post-dominator
        common_doms.into_iter()
            .filter(|&&dom| dom != node1 && dom != node2) // Exclude self
            .min_by_key(|&&dom| {
                // Use distance from root as "height" metric
                self.compute_post_dominator_height(dom, post_doms)
            })
            .copied()
    }
    
    fn compute_post_dominator_height(&self, node: NodeIndex, post_doms: &PostDominatorAnalysis) -> usize {
        let mut height = 0;
        let mut current = node;
        
        while let Some(parent) = post_doms.immediate_post_dominator(current) {
            height += 1;
            current = parent;
        }
        
        height
    }
}
```

### 7. Region Validation
```rust
impl Cfg {
    fn is_valid_if_else_region(&self, source: NodeIndex, then_head: NodeIndex, else_head: NodeIndex, join_block: NodeIndex) -> bool {
        // Ensure then_head and else_head are different
        if then_head == else_head {
            return false;
        }
        
        // Ensure join_block is reachable from both heads
        if !self.is_reachable(then_head, join_block) || !self.is_reachable(else_head, join_block) {
            return false;
        }
        
        // Ensure no path from source to join_block bypasses both heads
        // (This is a simplified check - more sophisticated validation may be needed)
        true
    }
    
    fn is_reachable(&self, from: NodeIndex, to: NodeIndex) -> bool {
        petgraph::algo::has_path_connecting(&self.graph, from, to, None)
    }
}
```

## Test Cases
- Canonical diamond: A→{B,C}→D
- Nested if/else structures
- If/else with early returns (no join block)
- Complex conditional chains
- Invalid structures (same then/else head)
- Multiple if/else regions in same function

## Considerations
- Post-dominator analysis is essential for accurate join block detection
- Conditional sources must have exactly one True and one False edge
- Join block must be reachable from both then and else heads
- Nested if/else regions may overlap
- Performance: O(N²) in worst case for LCPD computation
- May need more sophisticated validation for complex control flow 