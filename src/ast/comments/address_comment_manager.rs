//! Address-based comment management for AST building
//!
//! This module implements the Address-Based Comment Anchors pattern from the oxc manual guide.
//! It uses allocator addresses as natural unique identifiers for attaching comments to AST nodes.

use oxc_allocator::{Address, GetAddress};
use oxc_ast::Comment;
use oxc_span::Span;
use std::collections::HashMap;

use crate::ast::InstructionIndex;

/// Position of comment relative to node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentPosition {
    /// Comments before the node
    Leading,
    /// Comments after the node on the same line
    Trailing,
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

/// Manages comments during AST building using allocator addresses
pub struct AddressCommentManager {
    /// Comments mapped to allocator addresses
    comments: HashMap<Address, Vec<PendingComment>>,
}

impl AddressCommentManager {
    /// Create a new address-based comment manager
    pub fn new() -> Self {
        Self {
            comments: HashMap::new(),
        }
    }

    /// Add a comment to any node using its allocator address
    pub fn add_comment<T: GetAddress>(
        &mut self,
        node: &T,
        text: impl Into<String>,
        kind: CommentKind,
        position: CommentPosition,
    ) {
        let addr = node.address();
        self.comments.entry(addr).or_default().push(PendingComment {
            text: text.into(),
            kind,
            position,
        });
    }

    /// Add an instruction comment to a node
    pub fn add_instruction_comment<T: GetAddress>(
        &mut self,
        node: &T,
        pc: InstructionIndex,
        instruction: String,
    ) {
        self.add_comment(
            node,
            format!(" PC {}: {}", pc, instruction),
            CommentKind::Line,
            CommentPosition::Leading,
        );
    }

    /// Add an SSA comment to a node
    pub fn add_ssa_comment<T: GetAddress>(&mut self, node: &T, ssa_info: String) {
        self.add_comment(
            node,
            format!(" SSA: {}", ssa_info),
            CommentKind::Line,
            CommentPosition::Leading,
        );
    }

    /// Convenience method for JSDoc comments
    pub fn add_jsdoc<T: GetAddress>(&mut self, node: &T, description: &str) {
        let jsdoc = format!("*\n * {}\n ", description);
        self.add_comment(node, jsdoc, CommentKind::Block, CommentPosition::Leading);
    }

    /// Check if a node has comments
    pub fn has_comments(&self, addr: Address) -> bool {
        self.comments.contains_key(&addr)
    }

    /// Convert pending comments to real comments with synthetic source
    pub fn finalize_comments(
        self,
        address_positions: &HashMap<Address, u32>,
    ) -> (Vec<Comment>, String) {
        let mut source_parts = Vec::new();
        let mut final_comments = Vec::new();

        // First pass: calculate formatted text lengths for all comments
        let mut formatted_comments = Vec::new();
        for (addr, pending_comments) in self.comments.iter() {
            if let Some(&node_pos) = address_positions.get(addr) {
                let mut node_comments = Vec::new();
                for pending in pending_comments.iter() {
                    let formatted_text = match pending.kind {
                        CommentKind::Line => format!("//{}", pending.text),
                        CommentKind::Block => format!("/*{}*/", pending.text),
                    };
                    node_comments.push((pending, formatted_text));
                }
                formatted_comments.push((node_pos, node_comments));
            }
        }

        // Sort by node position to ensure proper ordering
        formatted_comments.sort_by_key(|(pos, _)| *pos);

        // Second pass: assign positions ensuring no overlaps
        let mut current_pos = 0u32;
        // Store (original_node_pos, adjusted_pos_for_layout, comments)
        let mut final_positions: Vec<(u32, u32, Vec<(PendingComment, String)>)> = Vec::new();

        // Process each node with its comments
        for (_i, (node_pos, node_comments)) in formatted_comments.iter().enumerate() {
            // Separate leading and trailing comments (cloning the PendingComment)
            let leading: Vec<(PendingComment, String)> = node_comments
                .iter()
                .filter(|(pending, _)| pending.position == CommentPosition::Leading)
                .map(|(p, t)| ((*p).clone(), t.clone()))
                .collect();
            let trailing: Vec<(PendingComment, String)> = node_comments
                .iter()
                .filter(|(pending, _)| pending.position == CommentPosition::Trailing)
                .map(|(p, t)| ((*p).clone(), t.clone()))
                .collect();

            // Calculate space needed
            let leading_space: u32 = leading
                .iter()
                .map(|(_, text)| text.len() as u32 + 1) // +1 for newline
                .sum();
            let trailing_space: u32 = trailing
                .iter()
                .map(|(_, text)| text.len() as u32 + 1) // +1 for space
                .sum();

            // Ensure we have enough space before this node for its leading comments
            let min_start = current_pos + leading_space;
            let actual_node_pos = if min_start > *node_pos {
                // We need more space, shift this node forward
                min_start + 10 // Add small buffer
            } else {
                *node_pos
            };

            // Store the adjusted position
            let mut adjusted_comments = Vec::new();
            for (pending, text) in leading {
                adjusted_comments.push((pending, text));
            }
            for (pending, text) in trailing {
                adjusted_comments.push((pending, text));
            }
            final_positions.push((*node_pos, actual_node_pos, adjusted_comments));

            // Update current position to after this node and its trailing comments
            current_pos = actual_node_pos + 10 + trailing_space; // Node gets 10 units of space
        }

        // Now actually place the comments using the adjusted positions
        current_pos = 0;
        for (original_node_pos, adjusted_node_pos, node_comments) in final_positions {
            let leading_comments: Vec<_> = node_comments
                .iter()
                .filter(|(pending, _)| pending.position == CommentPosition::Leading)
                .collect();
            let trailing_comments: Vec<_> = node_comments
                .iter()
                .filter(|(pending, _)| pending.position == CommentPosition::Trailing)
                .collect();

            // Place leading comments before the node
            for (i, (pending, formatted_text)) in leading_comments.iter().enumerate() {
                let comment_pos = current_pos;
                source_parts.push((comment_pos, formatted_text.clone()));

                let comment = Comment {
                    span: Span::new(comment_pos, comment_pos + formatted_text.len() as u32),
                    attached_to: original_node_pos,
                    kind: pending.kind.into(),
                    position: pending.position.into(),
                    newlines: oxc_ast::ast::CommentNewlines::Leading,
                    content: oxc_ast::CommentContent::None,
                };

                final_comments.push(comment);
                current_pos += formatted_text.len() as u32;

                // Add newline after each leading comment
                if i < leading_comments.len() - 1 {
                    source_parts.push((current_pos, "\n".to_string()));
                    current_pos += 1;
                }
            }

            // Add final newline before node if we had leading comments
            if !leading_comments.is_empty() {
                source_parts.push((current_pos, "\n".to_string()));
                // current_pos += 1; // Not needed since we reassign below
            }

            // Skip past the adjusted node position
            current_pos = adjusted_node_pos + 10; // Node takes 10 units

            // Place trailing comments
            if !trailing_comments.is_empty() {
                source_parts.push((current_pos, " ".to_string()));
                current_pos += 1;

                for (i, (pending, formatted_text)) in trailing_comments.iter().enumerate() {
                    let comment_pos = current_pos;
                    source_parts.push((comment_pos, formatted_text.clone()));

                    let comment = Comment {
                        span: Span::new(comment_pos, comment_pos + formatted_text.len() as u32),
                        attached_to: original_node_pos,
                        kind: pending.kind.into(),
                        position: pending.position.into(),
                        newlines: oxc_ast::ast::CommentNewlines::Trailing,
                        content: oxc_ast::CommentContent::None,
                    };

                    final_comments.push(comment);
                    current_pos += formatted_text.len() as u32;

                    if i < trailing_comments.len() - 1 {
                        source_parts.push((current_pos, " ".to_string()));
                        current_pos += 1;
                    }
                }
            }

            // Add some space before next node
            current_pos += 2;
        }

        // Build synthetic source text based on actual content
        source_parts.sort_by_key(|(pos, _)| *pos);
        let max_pos = source_parts
            .iter()
            .map(|(pos, text)| pos + text.len() as u32)
            .max()
            .unwrap_or(0);

        let total_source_size = (max_pos + 1) as usize; // Just enough space
        let mut synthetic_source = " ".repeat(total_source_size);

        for (pos, text) in &source_parts {
            let start = *pos as usize;
            let end = start + text.len();
            if start < synthetic_source.len() && end <= synthetic_source.len() {
                synthetic_source.replace_range(start..end, text);
            }
        }

        // Sort comments by position and return
        final_comments.sort_by_key(|c| c.span.start);
        (final_comments, synthetic_source)
    }

    /// Get comments for an address (for testing/debugging)
    pub fn get_comments(&self, addr: Address) -> Option<&Vec<PendingComment>> {
        self.comments.get(&addr)
    }

    /// Get all pending comments (for debugging)
    pub fn iter_comments(&self) -> impl Iterator<Item = (&Address, &Vec<PendingComment>)> {
        self.comments.iter()
    }
}

impl Default for AddressCommentManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxc_allocator::Allocator;
    use oxc_ast::AstBuilder;
    use oxc_span::SPAN;

    #[test]
    fn test_address_comment_manager() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);
        let mut manager = AddressCommentManager::new();

        // Create a node and get its address
        let node = ast_builder.alloc(ast_builder.statement_return(SPAN, None));
        let addr = node.address();

        // Add comments
        manager.add_comment(
            &node,
            "Test comment 1",
            CommentKind::Line,
            CommentPosition::Leading,
        );
        manager.add_jsdoc(&node, "JSDoc comment");

        assert!(manager.has_comments(addr));
        assert_eq!(manager.get_comments(addr).unwrap().len(), 2);

        // Test finalization
        let mut address_positions = HashMap::new();
        address_positions.insert(addr, 100);

        let (comments, synthetic_source) = manager.finalize_comments(&address_positions);
        assert_eq!(comments.len(), 2);
        assert!(!synthetic_source.trim().is_empty());
    }

    #[test]
    fn test_long_comments_no_overlap() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);
        let mut manager = AddressCommentManager::new();

        // Create nodes
        let node1 = ast_builder.alloc(ast_builder.statement_return(SPAN, None));
        let node2 = ast_builder.alloc(ast_builder.statement_return(SPAN, None));

        // Add very long comments
        let long_comment = "This is a very long comment that should not overlap with other comments even when multiple long comments are placed near each other in the synthetic source";
        manager.add_comment(
            &node1,
            long_comment,
            CommentKind::Line,
            CommentPosition::Leading,
        );
        manager.add_comment(
            &node1,
            "Another long comment that follows the first one",
            CommentKind::Line,
            CommentPosition::Leading,
        );
        manager.add_comment(
            &node2,
            long_comment,
            CommentKind::Block,
            CommentPosition::Trailing,
        );

        // Position nodes close together to test spacing
        let mut address_positions = HashMap::new();
        address_positions.insert(node1.address(), 200);
        address_positions.insert(node2.address(), 250); // Only 50 positions apart

        let (comments, synthetic_source) = manager.finalize_comments(&address_positions);

        // Should have 3 comments without overlapping spans
        assert_eq!(comments.len(), 3);

        // Verify no overlapping spans by checking each comment's span doesn't intersect with others
        for (i, comment1) in comments.iter().enumerate() {
            for (j, comment2) in comments.iter().enumerate() {
                if i != j {
                    let overlap = comment1.span.start < comment2.span.end
                        && comment2.span.start < comment1.span.end;
                    assert!(
                        !overlap,
                        "Comments {} and {} have overlapping spans: {:?} and {:?}",
                        i, j, comment1.span, comment2.span
                    );
                }
            }
        }

        // Verify synthetic source contains all comment text
        assert!(synthetic_source.contains(&format!("//{}", long_comment)));
        assert!(synthetic_source.contains("//Another long comment"));
        assert!(synthetic_source.contains(&format!("/*{}*/", long_comment)));
    }
}
