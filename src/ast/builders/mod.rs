//! AST construction utilities
//!
//! This module provides high-level builders for constructing AST nodes,
//! including function builders and expression builders.

pub mod function_builder;

pub use function_builder::{build_function_program, generate_code_with_comments, FunctionBuilder};
