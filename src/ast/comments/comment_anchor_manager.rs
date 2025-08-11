//! Comment anchor management system for AST building
//!
//! This module implements the Comment Anchors pattern from the oxc manual AST comments guide.
//! It separates comment management from position tracking, providing a clean API for attaching
//! comments to AST nodes during manual construction.

use oxc_ast::{ast::CommentNewlines, Comment};
use oxc_span::Span;
use std::collections::HashMap;

/// Unique identifier for AST nodes during building
pub type NodeId = usize;

/// Position of comment relative to node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentPosition {
    /// Comments before the node
    Leading,
    /// Comments after the node on the same line
    Trailing,
    /// Comments inside block statements
    Inner,
}

/// Comment kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    /// Single line comment //
    Line,
    /// Block comment /* */
    Block,
}

impl From<CommentKind> for oxc_ast::CommentKind {
    fn from(kind: CommentKind) -> Self {
        match kind {
            CommentKind::Line => oxc_ast::CommentKind::Line,
            CommentKind::Block => oxc_ast::CommentKind::Block,
        }
    }
}

impl From<CommentPosition> for oxc_ast::CommentPosition {
    fn from(pos: CommentPosition) -> Self {
        match pos {
            CommentPosition::Leading => oxc_ast::CommentPosition::Leading,
            CommentPosition::Trailing => oxc_ast::CommentPosition::Trailing,
            // Note: oxc doesn't have Inner, so we use Leading for inner comments
            CommentPosition::Inner => oxc_ast::CommentPosition::Leading,
        }
    }
}

/// A pending comment that hasn't been assigned a final position yet
#[derive(Debug, Clone)]
pub struct PendingComment {
    pub text: String,
    pub kind: CommentKind,
    pub position: CommentPosition,
}

/// Manages comments during AST building with stable node IDs
pub struct CommentAnchorManager {
    /// Next available node ID
    next_node_id: NodeId,
    /// Comments mapped to node IDs
    comments: HashMap<NodeId, Vec<PendingComment>>,
    /// Starting ID to avoid collisions with real positions
    id_offset: usize,
}

impl CommentAnchorManager {
    /// Create a new comment anchor manager
    pub fn new() -> Self {
        Self {
            next_node_id: 1_000_000, // Start with high ID to avoid collisions
            comments: HashMap::new(),
            id_offset: 1_000_000,
        }
    }

    /// Get the next node ID and increment the counter
    pub fn next_node_id(&mut self) -> NodeId {
        let id = self.next_node_id;
        self.next_node_id += 1;
        id
    }

    /// Add a comment to a node
    pub fn add_comment(
        &mut self,
        node_id: NodeId,
        text: impl Into<String>,
        kind: CommentKind,
        position: CommentPosition,
    ) {
        self.comments
            .entry(node_id)
            .or_default()
            .push(PendingComment {
                text: text.into(),
                kind,
                position,
            });
    }

    /// Add an instruction comment to a node
    pub fn add_instruction_comment(&mut self, node_id: NodeId, pc: u32, instruction: String) {
        self.add_comment(
            node_id,
            format!(" PC {}: {}", pc, instruction),
            CommentKind::Line,
            CommentPosition::Leading,
        );
    }

    /// Add an SSA comment to a node
    pub fn add_ssa_comment(&mut self, node_id: NodeId, ssa_info: String) {
        self.add_comment(
            node_id,
            format!(" SSA: {}", ssa_info),
            CommentKind::Line,
            CommentPosition::Leading,
        );
    }

    /// Check if a node has comments
    pub fn has_comments(&self, node_id: NodeId) -> bool {
        self.comments.contains_key(&node_id)
    }

    /// Get comments for a node (for testing/debugging)
    pub fn get_comments(&self, node_id: NodeId) -> Option<&Vec<PendingComment>> {
        self.comments.get(&node_id)
    }

    /// Convert pending comments to final comments with proper positions
    /// This should be called after the AST is built and positions are assigned
    pub fn finalize_comments(self, id_to_position: &HashMap<NodeId, u32>) -> Vec<Comment> {
        let mut final_comments = Vec::new();

        for (node_id, pending_comments) in self.comments {
            if let Some(&pos) = id_to_position.get(&node_id) {
                for (i, pc) in pending_comments.iter().enumerate() {
                    let span = calculate_comment_span(pos, i, pc.position);

                    final_comments.push(Comment {
                        span,
                        kind: pc.kind.into(),
                        attached_to: pos,
                        position: pc.position.into(),
                        newlines: CommentNewlines::None,
                        content: oxc_ast::CommentContent::None,
                    });
                }
            }
        }

        // Sort comments by position
        final_comments.sort_by_key(|c| c.span.start);
        final_comments
    }

    /// Create a span that can be used as a temporary node ID marker
    pub fn create_id_span(&self, node_id: NodeId) -> Span {
        Span::new(node_id as u32, node_id as u32)
    }

    /// Check if a span contains a node ID (vs a real position)
    pub fn is_node_id_span(&self, span: Span) -> bool {
        span.start >= self.id_offset as u32 && span.start == span.end
    }

    /// Extract node ID from a span
    pub fn extract_node_id(&self, span: Span) -> Option<NodeId> {
        if self.is_node_id_span(span) {
            Some(span.start as NodeId)
        } else {
            None
        }
    }
}

impl Default for CommentAnchorManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate the span for a comment based on node position and comment index
fn calculate_comment_span(node_pos: u32, comment_index: usize, position: CommentPosition) -> Span {
    match position {
        CommentPosition::Leading => {
            // Place comments before the node with spacing
            let offset = (comment_index as u32 + 1) * 10;
            let start = node_pos.saturating_sub(offset);
            Span::new(start, start + 5)
        }
        CommentPosition::Trailing => {
            // Place comments after the node
            let start = node_pos + 50 + (comment_index as u32 * 10);
            Span::new(start, start + 5)
        }
        CommentPosition::Inner => {
            // Place inner comments slightly after the node start
            let start = node_pos + 5 + (comment_index as u32 * 10);
            Span::new(start, start + 5)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment_anchor_manager() {
        let mut manager = CommentAnchorManager::new();

        let node1 = manager.next_node_id();
        let node2 = manager.next_node_id();

        manager.add_comment(
            node1,
            "Test comment 1",
            CommentKind::Line,
            CommentPosition::Leading,
        );
        manager.add_comment(
            node2,
            "Test comment 2",
            CommentKind::Block,
            CommentPosition::Trailing,
        );

        assert!(manager.has_comments(node1));
        assert!(manager.has_comments(node2));
        assert!(!manager.has_comments(999));

        let mut id_to_position = HashMap::new();
        id_to_position.insert(node1, 100);
        id_to_position.insert(node2, 200);

        let comments = manager.finalize_comments(&id_to_position);
        assert_eq!(comments.len(), 2);

        // Comments should be sorted by position
        assert!(comments[0].span.start < comments[1].span.start);
    }
}
