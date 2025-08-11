//! Comment management system
//!
//! This module provides a unified system for managing comments in the AST,
//! including address-based comment anchors and position assignment.

pub mod address_comment_manager;
pub mod comment_anchor_manager;
pub mod position_assigner;

pub use address_comment_manager::{AddressCommentManager, CommentKind, CommentPosition, PendingComment};
pub use comment_anchor_manager::{CommentAnchorManager, NodeId};
pub use position_assigner::PositionAssigner;