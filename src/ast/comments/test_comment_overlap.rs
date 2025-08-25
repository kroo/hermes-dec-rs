//! Test for reproducing overlapping comment issue

#[cfg(test)]
mod tests {
    use super::super::*;
    use oxc_allocator::{Allocator, GetAddress};
    use oxc_ast::AstBuilder;
    use std::collections::HashMap;

    #[test]
    fn test_multiple_comments_on_same_node() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        // Create a simple statement
        let test_str = ast_builder.allocator.alloc_str("test");
        let lit = ast_builder.expression_string_literal(oxc_span::SPAN, test_str, None);
        let stmt = ast_builder.statement_expression(oxc_span::SPAN, lit);

        // Create comment manager and add multiple comments to the same statement
        let mut comment_manager = AddressCommentManager::new();

        // Add two comments like we do in the actual code - use long text
        comment_manager.add_comment(
            &stmt,
            "PC 27: LoadConstString { operand_0: 1, operand_1: 62 } - This is a very long comment that might cause issues with wrapping or truncation",
            CommentKind::Line,
            CommentPosition::Leading,
        );

        comment_manager.add_comment(
            &stmt,
            "DEF: r1 → r1_5 [var1_d] - Another long comment to test if multiple long comments cause overlapping issues when rendered",
            CommentKind::Line,
            CommentPosition::Leading,
        );

        // Create position map
        let mut address_positions = HashMap::new();
        let stmt_addr = stmt.address();
        address_positions.insert(stmt_addr, 100); // Arbitrary position

        // Finalize comments
        let (comments, synthetic_source) = comment_manager.finalize_comments(&address_positions);

        // Print for debugging
        println!("Synthetic source: {}", synthetic_source);
        println!("Number of comments: {}", comments.len());

        for comment in &comments {
            println!(
                "Comment span: {:?}, attached to: {}",
                comment.span, comment.attached_to
            );
        }

        // Check that comments don't overlap
        assert_eq!(comments.len(), 2, "Should have 2 comments");

        if comments.len() >= 2 {
            let first_comment = &comments[0];
            let second_comment = &comments[1];

            // Comments should not overlap
            assert!(
                first_comment.span.end <= second_comment.span.start,
                "Comments overlap! First: {:?}, Second: {:?}",
                first_comment.span,
                second_comment.span
            );

            // Check the synthetic source contains both comments properly
            assert!(
                synthetic_source.contains("PC 27"),
                "Should contain first comment"
            );
            assert!(
                synthetic_source.contains("DEF: r1"),
                "Should contain second comment"
            );
        }
    }

    #[test]
    fn test_adjacent_nodes_with_comments() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        // Create three adjacent statements
        let stmt1 = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("first"),
                None,
            ),
        );

        let stmt2 = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("second"),
                None,
            ),
        );

        let stmt3 = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("third"),
                None,
            ),
        );

        // Create comment manager and add comments to adjacent nodes
        let mut comment_manager = AddressCommentManager::new();

        comment_manager.add_comment(
            &stmt1,
            "First statement comment",
            CommentKind::Line,
            CommentPosition::Leading,
        );

        comment_manager.add_comment(
            &stmt2,
            "Second statement comment - this one is longer to test spacing",
            CommentKind::Line,
            CommentPosition::Leading,
        );

        comment_manager.add_comment(&stmt3, "Third", CommentKind::Line, CommentPosition::Leading);

        // Position nodes close together
        let mut address_positions = HashMap::new();
        address_positions.insert(stmt1.address(), 100);
        address_positions.insert(stmt2.address(), 150);
        address_positions.insert(stmt3.address(), 200);

        // Finalize comments
        let (comments, _synthetic_source) = comment_manager.finalize_comments(&address_positions);

        // Verify no overlapping
        assert_eq!(comments.len(), 3, "Should have 3 comments");

        for (i, comment1) in comments.iter().enumerate() {
            for (j, comment2) in comments.iter().enumerate() {
                if i != j {
                    let overlap = comment1.span.start < comment2.span.end
                        && comment2.span.start < comment1.span.end;
                    assert!(
                        !overlap,
                        "Comments {} and {} overlap: {:?} and {:?}",
                        i, j, comment1.span, comment2.span
                    );
                }
            }
        }
    }

    #[test]
    fn test_mixed_comment_types() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        // Create a statement
        let stmt = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("test"),
                None,
            ),
        );

        // Add different types of comments
        let mut comment_manager = AddressCommentManager::new();

        // Line comment (leading)
        comment_manager.add_comment(
            &stmt,
            " This is a line comment",
            CommentKind::Line,
            CommentPosition::Leading,
        );

        // Block comment (leading)
        comment_manager.add_comment(
            &stmt,
            " This is a block comment ",
            CommentKind::Block,
            CommentPosition::Leading,
        );

        // Line comment (trailing)
        comment_manager.add_comment(
            &stmt,
            " Trailing line",
            CommentKind::Line,
            CommentPosition::Trailing,
        );

        // Block comment (trailing)
        comment_manager.add_comment(
            &stmt,
            " Trailing block ",
            CommentKind::Block,
            CommentPosition::Trailing,
        );

        // Position the node
        let mut address_positions = HashMap::new();
        address_positions.insert(stmt.address(), 500);

        // Finalize
        let (comments, synthetic_source) = comment_manager.finalize_comments(&address_positions);

        // Should have 4 comments without overlap
        assert_eq!(comments.len(), 4, "Should have 4 comments");

        // Check that leading comments come before trailing
        let leading_comments: Vec<_> = comments
            .iter()
            .filter(|c| c.position == oxc_ast::CommentPosition::Leading)
            .collect();
        let trailing_comments: Vec<_> = comments
            .iter()
            .filter(|c| c.position == oxc_ast::CommentPosition::Trailing)
            .collect();

        assert_eq!(leading_comments.len(), 2, "Should have 2 leading comments");
        assert_eq!(
            trailing_comments.len(),
            2,
            "Should have 2 trailing comments"
        );

        // Leading should come before node position
        for comment in &leading_comments {
            assert!(
                comment.span.end <= 500,
                "Leading comment should end before node position"
            );
        }

        // Trailing should come after node position
        for comment in &trailing_comments {
            assert!(
                comment.span.start >= 500,
                "Trailing comment should start after node position"
            );
        }

        // Debug: print the positions to verify trailing vs leading
        println!("Node at position: 500");
        for comment in &comments {
            let position_type = if comment.position == oxc_ast::CommentPosition::Leading {
                "Leading"
            } else {
                "Trailing"
            };
            println!("  {} comment at span: {:?}", position_type, comment.span);
        }

        // Check synthetic source contains all comment formats
        assert!(synthetic_source.contains("// This is a line comment"));
        assert!(synthetic_source.contains("/* This is a block comment */"));
        assert!(synthetic_source.contains("// Trailing line"));
        assert!(synthetic_source.contains("/* Trailing block */"));
    }

    #[test]
    fn test_very_long_and_short_comments() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        // Create two statements
        let stmt1 = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("stmt1"),
                None,
            ),
        );

        let stmt2 = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("stmt2"),
                None,
            ),
        );

        let mut comment_manager = AddressCommentManager::new();

        // Very long comment
        let long_comment = "This is an extremely long comment that contains a lot of information \
                           about the implementation details, including various SSA values, \
                           instruction indices, register definitions, and other metadata that \
                           might be included in a real decompilation scenario where we need to \
                           track many different aspects of the code generation process";

        // Very short comment
        let short_comment = "x";

        comment_manager.add_comment(
            &stmt1,
            long_comment,
            CommentKind::Line,
            CommentPosition::Leading,
        );

        comment_manager.add_comment(
            &stmt1,
            short_comment,
            CommentKind::Line,
            CommentPosition::Trailing,
        );

        comment_manager.add_comment(
            &stmt2,
            short_comment,
            CommentKind::Line,
            CommentPosition::Leading,
        );

        comment_manager.add_comment(
            &stmt2,
            long_comment,
            CommentKind::Block,
            CommentPosition::Trailing,
        );

        // Position nodes
        let mut address_positions = HashMap::new();
        address_positions.insert(stmt1.address(), 1000);
        address_positions.insert(stmt2.address(), 2000);

        // Finalize
        let (comments, synthetic_source) = comment_manager.finalize_comments(&address_positions);

        assert_eq!(comments.len(), 4, "Should have 4 comments");

        // Verify no overlap
        for (i, comment1) in comments.iter().enumerate() {
            for (j, comment2) in comments.iter().enumerate() {
                if i != j {
                    let overlap = comment1.span.start < comment2.span.end
                        && comment2.span.start < comment1.span.end;
                    assert!(
                        !overlap,
                        "Comments {} and {} overlap: {:?} and {:?}",
                        i, j, comment1.span, comment2.span
                    );
                }
            }
        }

        // Check content is preserved
        assert!(synthetic_source.contains(&format!("//{}\n", long_comment)));
        assert!(synthetic_source.contains(&format!("//{}\n", short_comment)));
        assert!(synthetic_source.contains(&format!("/*{}*/", long_comment)));
    }

    #[test]
    fn test_many_comments_single_node() {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        let stmt = ast_builder.statement_expression(
            oxc_span::SPAN,
            ast_builder.expression_string_literal(
                oxc_span::SPAN,
                ast_builder.allocator.alloc_str("test"),
                None,
            ),
        );

        let mut comment_manager = AddressCommentManager::new();

        // Add many comments to the same node
        for i in 0..10 {
            comment_manager.add_comment(
                &stmt,
                format!("Leading comment #{}", i),
                CommentKind::Line,
                CommentPosition::Leading,
            );
        }

        for i in 0..5 {
            comment_manager.add_comment(
                &stmt,
                format!("Trailing comment #{}", i),
                CommentKind::Line,
                CommentPosition::Trailing,
            );
        }

        // Position node
        let mut address_positions = HashMap::new();
        address_positions.insert(stmt.address(), 5000);

        // Finalize
        let (comments, synthetic_source) = comment_manager.finalize_comments(&address_positions);

        assert_eq!(comments.len(), 15, "Should have 15 comments total");

        // All should be attached to the same node
        for comment in &comments {
            assert_eq!(
                comment.attached_to, 5000,
                "All comments should be attached to position 5000"
            );
        }

        // Verify no overlap
        for (i, comment1) in comments.iter().enumerate() {
            for (j, comment2) in comments.iter().enumerate() {
                if i != j {
                    let overlap = comment1.span.start < comment2.span.end
                        && comment2.span.start < comment1.span.end;
                    assert!(
                        !overlap,
                        "Comments {} and {} overlap: {:?} and {:?}",
                        i, j, comment1.span, comment2.span
                    );
                }
            }
        }

        // Check all comments are in the source
        for i in 0..10 {
            assert!(
                synthetic_source.contains(&format!("//Leading comment #{}", i)),
                "Should contain leading comment #{}",
                i
            );
        }

        for i in 0..5 {
            assert!(
                synthetic_source.contains(&format!("//Trailing comment #{}", i)),
                "Should contain trailing comment #{}",
                i
            );
        }
    }
}
