//! Abstract Syntax Tree (AST) module
//!
//! This module provides instruction-to-expression conversion for Hermes bytecode decompilation.
//! The core functionality is built around the InstructionToExpressionConverter which transforms
//! Hermes instructions into OXC AST expressions.

pub mod register_manager;
pub mod expression_context;
pub mod converter;

// Re-export the main types for public API
pub use register_manager::{RegisterManager, RegisterLifetime, RegisterStats};
pub use expression_context::{ExpressionContext, ExpressionContextError};
pub use converter::{InstructionToExpressionConverter, ConversionError, ConversionStats};
