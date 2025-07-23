# CFG-10 Design: Graphviz edge labels

## Overview
Enhance DOT generation to include clear, readable labels for different edge kinds to improve CFG visualization and debugging.

## Implementation Plan

### 1. Edge Label Mapping
```rust
impl Cfg {
    fn get_edge_label(&self, edge_kind: &EdgeKind) -> Option<String> {
        match edge_kind {
            EdgeKind::True => Some("T".to_string()),
            EdgeKind::False => Some("F".to_string()),
            EdgeKind::Switch(idx) => Some(format!("Sw{}", idx)),
            EdgeKind::Default => Some("Def".to_string()),
            EdgeKind::Fall => Some("Fall".to_string()),
            EdgeKind::Uncond => None, // No label for unconditional edges
        }
    }
}
```

### 2. Update DOT Generation
```rust
impl Cfg {
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph CFG {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box];\n\n");
        
        // Add nodes
        for node in self.graph.node_indices() {
            if let Some(block) = self.graph.node_weight(node) {
                let label = self.format_block_label(block);
                dot.push_str(&format!("  {} [label=\"{}\"];\n", node.index(), label));
            }
        }
        
        dot.push_str("\n");
        
        // Add edges with labels
        for edge in self.graph.edge_indices() {
            let (tail, head) = self.graph.edge_endpoints(edge).unwrap();
            let edge_kind = self.graph.edge_weight(edge).unwrap();
            
            let mut edge_str = format!("  {} -> {}", tail.index(), head.index());
            
            // Add label if edge kind has one
            if let Some(label) = self.get_edge_label(edge_kind) {
                edge_str.push_str(&format!(" [label=\"{}\"]", label));
            }
            
            edge_str.push_str(";\n");
            dot.push_str(&edge_str);
        }
        
        dot.push_str("}\n");
        dot
    }
}
```

### 3. Enhanced Edge Label Formatting
```rust
impl Cfg {
    fn get_edge_label(&self, edge_kind: &EdgeKind) -> Option<String> {
        match edge_kind {
            EdgeKind::True => Some("T".to_string()),
            EdgeKind::False => Some("F".to_string()),
            EdgeKind::Switch(idx) => Some(format!("Sw{}", idx)),
            EdgeKind::Default => Some("Def".to_string()),
            EdgeKind::Fall => Some("Fall".to_string()),
            EdgeKind::Uncond => None,
        }
    }
    
    fn get_edge_color(&self, edge_kind: &EdgeKind) -> Option<&'static str> {
        match edge_kind {
            EdgeKind::True => Some("green"),
            EdgeKind::False => Some("red"),
            EdgeKind::Switch(_) => Some("blue"),
            EdgeKind::Default => Some("orange"),
            EdgeKind::Fall => Some("gray"),
            EdgeKind::Uncond => Some("black"),
        }
    }
}
```

### 4. Enhanced DOT with Colors
```rust
impl Cfg {
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph CFG {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box];\n\n");
        
        // Add nodes
        for node in self.graph.node_indices() {
            if let Some(block) = self.graph.node_weight(node) {
                let label = self.format_block_label(block);
                dot.push_str(&format!("  {} [label=\"{}\"];\n", node.index(), label));
            }
        }
        
        dot.push_str("\n");
        
        // Add edges with labels and colors
        for edge in self.graph.edge_indices() {
            let (tail, head) = self.graph.edge_endpoints(edge).unwrap();
            let edge_kind = self.graph.edge_weight(edge).unwrap();
            
            let mut edge_str = format!("  {} -> {}", tail.index(), head.index());
            let mut attributes = Vec::new();
            
            // Add label if edge kind has one
            if let Some(label) = self.get_edge_label(edge_kind) {
                attributes.push(format!("label=\"{}\"", label));
            }
            
            // Add color
            if let Some(color) = self.get_edge_color(edge_kind) {
                attributes.push(format!("color=\"{}\"", color));
            }
            
            // Add attributes if any
            if !attributes.is_empty() {
                edge_str.push_str(&format!(" [{}]", attributes.join(", ")));
            }
            
            edge_str.push_str(";\n");
            dot.push_str(&edge_str);
        }
        
        dot.push_str("}\n");
        dot
    }
}
```

### 5. Test Cases
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_edge_label_generation() {
        let cfg = create_test_cfg();
        let dot = cfg.to_dot();
        
        // Check that edge labels are present
        assert!(dot.contains("label=\"T\""));
        assert!(dot.contains("label=\"F\""));
        assert!(dot.contains("label=\"Sw0\""));
        assert!(dot.contains("label=\"Def\""));
        assert!(dot.contains("label=\"Fall\""));
        
        // Check that unconditional edges don't have labels
        assert!(!dot.contains("label=\"Uncond\""));
    }
    
    #[test]
    fn test_edge_colors() {
        let cfg = create_test_cfg();
        let dot = cfg.to_dot();
        
        // Check that colors are present
        assert!(dot.contains("color=\"green\"")); // True edges
        assert!(dot.contains("color=\"red\""));   // False edges
        assert!(dot.contains("color=\"blue\""));  // Switch edges
    }
    
    fn create_test_cfg() -> Cfg {
        // Create a simple test CFG with various edge types
        let mut cfg = Cfg::new();
        
        // Add nodes
        let node1 = cfg.graph.add_node(BasicBlock::Code { /* ... */ });
        let node2 = cfg.graph.add_node(BasicBlock::Code { /* ... */ });
        let node3 = cfg.graph.add_node(BasicBlock::Code { /* ... */ });
        
        // Add edges with different kinds
        cfg.graph.add_edge(node1, node2, EdgeKind::True);
        cfg.graph.add_edge(node1, node3, EdgeKind::False);
        
        cfg
    }
}
```

### 6. CLI Integration
```rust
impl Cfg {
    pub fn to_dot_with_options(&self, include_labels: bool, include_colors: bool) -> String {
        let mut dot = String::new();
        dot.push_str("digraph CFG {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box];\n\n");
        
        // Add nodes...
        
        // Add edges with optional labels and colors
        for edge in self.graph.edge_indices() {
            let (tail, head) = self.graph.edge_endpoints(edge).unwrap();
            let edge_kind = self.graph.edge_weight(edge).unwrap();
            
            let mut edge_str = format!("  {} -> {}", tail.index(), head.index());
            let mut attributes = Vec::new();
            
            if include_labels {
                if let Some(label) = self.get_edge_label(edge_kind) {
                    attributes.push(format!("label=\"{}\"", label));
                }
            }
            
            if include_colors {
                if let Some(color) = self.get_edge_color(edge_kind) {
                    attributes.push(format!("color=\"{}\"", color));
                }
            }
            
            if !attributes.is_empty() {
                edge_str.push_str(&format!(" [{}]", attributes.join(", ")));
            }
            
            edge_str.push_str(";\n");
            dot.push_str(&edge_str);
        }
        
        dot.push_str("}\n");
        dot
    }
}
```

## Test Cases
- CFG with all edge types (True, False, Switch, Default, Fall, Uncond)
- CFG with no conditional edges
- CFG with multiple switch cases
- Large CFG to test performance
- Edge cases: empty CFG, single node CFG

## Considerations
- Edge labels should be concise but clear
- Colors help distinguish edge types visually
- Performance impact should be minimal
- Maintain backward compatibility with existing DOT output
- Consider accessibility (color-blind friendly)
- May want to make labels/colors configurable via CLI options 