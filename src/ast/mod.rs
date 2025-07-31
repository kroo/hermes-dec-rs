//! Abstract Syntax Tree (AST) module
//!
//! This module provides instruction-to-expression conversion for Hermes bytecode decompilation.
//! The core functionality is built around the InstructionToExpressionConverter which transforms
//! Hermes instructions into OXC AST expressions.

pub mod converter;
pub mod expression_context;
pub mod register_manager;

// Re-export the main types for public API
pub use converter::{ConversionError, ConversionStats, InstructionToExpressionConverter};
pub use expression_context::{ExpressionContext, ExpressionContextError};
pub use register_manager::{RegisterLifetime, RegisterManager, RegisterStats};
