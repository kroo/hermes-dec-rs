//! Conversion context and state management
//!
//! This module provides context information needed during AST generation,
//! including access to HBC file tables and conversion state.

pub mod expression_context;

pub use expression_context::{ExpressionContext, ExpressionContextError};