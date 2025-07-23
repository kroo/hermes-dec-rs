# CFG-06 Design: Post-dominator analysis

## Overview
Implement post-dominator analysis by reversing the CFG and using the same simple_fast algorithm as forward dominator analysis.

## Implementation Plan

### 1. Post-Dominator Structure
```rust
#[derive(Debug, Clone)]
pub struct PostDominatorAnalysis {
    pub post_dominators: HashMap<NodeIndex, HashSet<NodeIndex>>,
    pub immediate_post_dominators: HashMap<NodeIndex, Option<NodeIndex>>,
    pub post_dominator_tree: Graph<NodeIndex, (), Directed>,
}

impl PostDominatorAnalysis {
    pub fn dominates(&self, post_dominator: NodeIndex, node: NodeIndex) -> bool {
        self.post_dominators
            .get(&node)
            .map(|doms| doms.contains(&post_dominator))
            .unwrap_or(false)
    }
    
    pub fn immediate_post_dominator(&self, node: NodeIndex) -> Option<NodeIndex> {
        self.immediate_post_dominators.get(&node).copied().flatten()
    }
}
```

### 2. Post-Dominator Analysis Implementation
```rust
impl Cfg {
    pub fn analyze_post_dominators(&self) -> PostDominatorAnalysis {
        // Create reverse graph for post-dominator analysis
        let reverse_graph = self.create_reverse_graph();
        
        // Use EXIT node as root (or find sink nodes if no EXIT)
        let root = self.find_post_dominator_root();
        
        // Run simple_fast on reverse graph
        let post_doms = petgraph::algo::simple_fast(&reverse_graph, root);
        
        // Convert to our format
        let mut post_dominators = HashMap::new();
        let mut immediate_post_dominators = HashMap::new();
        
        for node in self.graph.node_indices() {
            let node_doms = post_doms.dominators(node).collect::<HashSet<_>>();
            post_dominators.insert(node, node_doms);
            
            let immediate = post_doms.immediate_dominator(node);
            immediate_post_dominators.insert(node, immediate);
        }
        
        // Build post-dominator tree
        let post_dominator_tree = self.build_post_dominator_tree(&post_doms);
        
        PostDominatorAnalysis {
            post_dominators,
            immediate_post_dominators,
            post_dominator_tree,
        }
    }
}
```

### 3. Reverse Graph Creation
```rust
impl Cfg {
    fn create_reverse_graph(&self) -> Graph<NodeIndex, (), Directed> {
        let mut reverse_graph = Graph::new();
        
        // Add all nodes
        for node in self.graph.node_indices() {
            reverse_graph.add_node(node);
        }
        
        // Add reverse edges
        for edge in self.graph.edge_indices() {
            let (tail, head) = self.graph.edge_endpoints(edge).unwrap();
            reverse_graph.add_edge(head, tail, ());
        }
        
        reverse_graph
    }
}
```

### 4. Root Finding
```rust
impl Cfg {
    fn find_post_dominator_root(&self) -> NodeIndex {
        // If we have an EXIT node, use it
        if let Some(exit_node) = self.exit_node {
            return exit_node;
        }
        
        // Otherwise, find sink nodes (nodes with no outgoing edges)
        let mut sink_nodes = Vec::new();
        for node in self.graph.node_indices() {
            if self.graph.neighbors_directed(node, petgraph::Direction::Outgoing).next().is_none() {
                sink_nodes.push(node);
            }
        }
        
        // If multiple sinks, create a virtual root
        if sink_nodes.len() > 1 {
            let virtual_root = self.graph.add_node(NodeIndex::new(usize::MAX)); // Virtual node
            for sink in sink_nodes {
                self.graph.add_edge(sink, virtual_root, EdgeKind::Uncond);
            }
            virtual_root
        } else {
            sink_nodes[0]
        }
    }
}
```

### 5. Post-Dominator Tree Construction
```rust
impl Cfg {
    fn build_post_dominator_tree(&self, post_doms: &petgraph::algo::Dominators<NodeIndex>) -> Graph<NodeIndex, (), Directed> {
        let mut tree = Graph::new();
        
        // Add all nodes
        for node in self.graph.node_indices() {
            tree.add_node(node);
        }
        
        // Add tree edges
        for node in self.graph.node_indices() {
            if let Some(immediate) = post_doms.immediate_dominator(node) {
                tree.add_edge(immediate, node, ());
            }
        }
        
        tree
    }
}
```

### 6. Helper Methods
```rust
impl PostDominatorAnalysis {
    pub fn get_post_dominators(&self, node: NodeIndex) -> &HashSet<NodeIndex> {
        self.post_dominators.get(&node).unwrap_or(&HashSet::new())
    }
    
    pub fn is_post_dominated_by(&self, node: NodeIndex, post_dominator: NodeIndex) -> bool {
        self.dominates(post_dominator, node)
    }
    
    pub fn get_post_dominator_frontier(&self, node: NodeIndex) -> HashSet<NodeIndex> {
        // Nodes that are not post-dominated by node but have a predecessor that is
        let mut frontier = HashSet::new();
        
        for pred in self.graph.neighbors_directed(node, petgraph::Direction::Incoming) {
            if !self.dominates(node, pred) {
                frontier.insert(pred);
            }
        }
        
        frontier
    }
}
```

## Test Cases
- Diamond shape: A→{B,C}→D (D post-dominates B and C)
- Linear chain: A→B→C→D (each node post-dominates all previous)
- CFG with EXIT node
- CFG with multiple exit points
- Unreachable code blocks
- Complex control flow with loops

## Considerations
- Post-dominator analysis requires a single root (EXIT node or virtual root)
- Reverse graph preserves all edge information
- Same simple_fast algorithm as forward dominators
- Handles unreachable code by not including it in post-dominator sets
- Performance: O(E log N) where E is edges and N is nodes 