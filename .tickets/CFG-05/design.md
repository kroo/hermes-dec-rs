# CFG-05 Design: Natural-loop body computation

## Overview
Implement natural loop detection by computing the complete loop body for each back-edge, enabling proper loop region analysis.

## Implementation Plan

### 1. Loop Structure
```rust
#[derive(Debug, Clone)]
pub struct Loop {
    pub header: NodeIndex,
    pub body_nodes: HashSet<NodeIndex>,
    pub back_edges: Vec<(NodeIndex, NodeIndex)>, // (tail, header) pairs
}

#[derive(Debug, Clone)]
pub struct LoopAnalysis {
    pub loops: Vec<Loop>,
    pub node_to_loops: HashMap<NodeIndex, Vec<usize>>, // node -> loop indices
}
```

### 2. Natural Loop Detection Algorithm
```rust
impl Cfg {
    pub fn find_natural_loops(&self) -> LoopAnalysis {
        let mut loops = Vec::new();
        let mut node_to_loops = HashMap::new();
        
        // Find all back edges
        let back_edges = self.find_back_edges();
        
        // Group back edges by header
        let mut header_to_back_edges: HashMap<NodeIndex, Vec<NodeIndex>> = HashMap::new();
        for (tail, header) in back_edges {
            header_to_back_edges.entry(header).or_default().push(tail);
        }
        
        // Compute natural loop for each header
        for (header, tails) in header_to_back_edges {
            let loop_body = self.compute_natural_loop_body(header, &tails);
            let loop_idx = loops.len();
            
            let loop_struct = Loop {
                header,
                body_nodes: loop_body,
                back_edges: tails.into_iter().map(|tail| (tail, header)).collect(),
            };
            
            loops.push(loop_struct);
            
            // Update node_to_loops mapping
            for &node in &loop_body {
                node_to_loops.entry(node).or_default().push(loop_idx);
            }
        }
        
        LoopAnalysis { loops, node_to_loops }
    }
}
```

### 3. Natural Loop Body Computation
```rust
impl Cfg {
    fn compute_natural_loop_body(&self, header: NodeIndex, tails: &[NodeIndex]) -> HashSet<NodeIndex> {
        let mut loop_body = HashSet::new();
        loop_body.insert(header);
        
        // Add all tail nodes
        for &tail in tails {
            loop_body.insert(tail);
        }
        
        // Worklist algorithm to find all nodes in loop
        let mut worklist: Vec<NodeIndex> = tails.to_vec();
        
        while let Some(node) = worklist.pop() {
            // Add all predecessors of this node
            for pred in self.graph.neighbors_directed(node, petgraph::Direction::Incoming) {
                if !loop_body.contains(&pred) {
                    loop_body.insert(pred);
                    worklist.push(pred);
                }
            }
        }
        
        loop_body
    }
}
```

### 4. Back Edge Detection
```rust
impl Cfg {
    fn find_back_edges(&self) -> Vec<(NodeIndex, NodeIndex)> {
        let mut back_edges = Vec::new();
        let dominators = self.analyze_dominators();
        
        // Check each edge
        for edge in self.graph.edge_indices() {
            let (tail, head) = self.graph.edge_endpoints(edge).unwrap();
            
            // Edge is a back edge if head dominates tail
            if dominators.dominates(head, tail) {
                back_edges.push((tail, head));
            }
        }
        
        back_edges
    }
}
```

### 5. Helper Methods
```rust
impl LoopAnalysis {
    pub fn get_loops_containing_node(&self, node: NodeIndex) -> &[usize] {
        self.node_to_loops.get(&node).map(|v| v.as_slice()).unwrap_or(&[])
    }
    
    pub fn is_node_in_loop(&self, node: NodeIndex) -> bool {
        self.node_to_loops.contains_key(&node)
    }
    
    pub fn get_innermost_loop(&self, node: NodeIndex) -> Option<&Loop> {
        self.node_to_loops.get(&node)
            .and_then(|indices| indices.last())
            .map(|&idx| &self.loops[idx])
    }
}
```

## Test Cases
- Simple while loop (one back edge)
- Nested loops (inner and outer loop bodies)
- Multiple back edges to same header
- Irreducible loops (multiple headers)
- Loops with complex control flow inside
- Edge cases: self-loops, empty loop bodies

## Considerations
- Natural loops are maximal - they include all nodes that can reach the back edge
- Multiple back edges to same header are merged into single loop
- Nested loops are handled correctly by the node_to_loops mapping
- Performance: worklist algorithm is efficient for most CFGs
- Irreducible loops may require special handling in later phases 