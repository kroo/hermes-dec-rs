//! CFG visualization module
//!
//! This module contains visualization utilities for CFGs.

use crate::cfg::{Block, EdgeKind};
use petgraph::graph::DiGraph;

/// DOT generation options
#[derive(Debug, Clone)]
pub struct DotOptions {
    /// Include edge labels
    pub include_labels: bool,
    /// Include edge colors
    pub include_colors: bool,
    /// Include node details
    pub include_node_details: bool,
}

impl Default for DotOptions {
    fn default() -> Self {
        Self {
            include_labels: true,
            include_colors: true,
            include_node_details: false,
        }
    }
}

/// Generate DOT representation of a CFG
pub fn generate_dot(graph: &DiGraph<Block, EdgeKind>, options: &DotOptions) -> String {
    let mut dot = String::new();
    dot.push_str("digraph CFG {\n");
    dot.push_str("  rankdir=TB;\n");
    dot.push_str("  node [shape=box];\n\n");

    // Add nodes
    for node in graph.node_indices() {
        if let Some(block) = graph.node_weight(node) {
            let label = format_block_label(block, options);
            dot.push_str(&format!("  {} [label=\"{}\"];\n", node.index(), label));
        }
    }

    dot.push_str("\n");

    // Add edges with labels and colors
    for edge in graph.edge_indices() {
        let (tail, head) = graph.edge_endpoints(edge).unwrap();
        let edge_kind = graph.edge_weight(edge).unwrap();

        let mut edge_str = format!("  {} -> {}", tail.index(), head.index());
        let mut attributes = Vec::new();

        // Add label if requested
        if options.include_labels {
            if let Some(label) = get_edge_label(edge_kind) {
                attributes.push(format!("label=\"{}\"", label));
            }
        }

        // Add color if requested
        if options.include_colors {
            if let Some(color) = get_edge_color(edge_kind) {
                attributes.push(format!("color=\"{}\"", color));
            }
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

/// Format a block label for DOT
fn format_block_label(block: &Block, options: &DotOptions) -> String {
    if options.include_node_details {
        format!(
            "Block {}: {} instructions",
            block.start_pc(),
            block.instruction_count()
        )
    } else {
        format!("Block {}", block.start_pc())
    }
}

/// Get edge label for DOT
fn get_edge_label(edge_kind: &EdgeKind) -> Option<String> {
    match edge_kind {
        EdgeKind::True => Some("T".to_string()),
        EdgeKind::False => Some("F".to_string()),
        EdgeKind::Switch(idx) => Some(format!("Sw{}", idx)),
        EdgeKind::Default => Some("Def".to_string()),
        EdgeKind::Fall => Some("Fall".to_string()),
        EdgeKind::GeneratorFallthrough => Some("GenF".to_string()),
        EdgeKind::GeneratorResume => Some("GenR".to_string()),
        EdgeKind::Exception => Some("Exc".to_string()),
        EdgeKind::Uncond => None, // No label for unconditional edges
    }
}

/// Get edge color for DOT
fn get_edge_color(edge_kind: &EdgeKind) -> Option<&'static str> {
    match edge_kind {
        EdgeKind::True => Some("green"),
        EdgeKind::False => Some("red"),
        EdgeKind::Switch(_) => Some("blue"),
        EdgeKind::Default => Some("orange"),
        EdgeKind::Fall => Some("gray"),
        EdgeKind::GeneratorFallthrough => Some("lightblue"),
        EdgeKind::GeneratorResume => Some("cyan"),
        EdgeKind::Exception => Some("purple"),
        EdgeKind::Uncond => Some("black"),
    }
}

/// Generate a simple DOT representation (backward compatibility)
pub fn generate_simple_dot(graph: &DiGraph<Block, EdgeKind>) -> String {
    use petgraph::dot::{Config, Dot};
    format!("{:?}", Dot::with_config(graph, &[Config::EdgeNoLabel]))
}
