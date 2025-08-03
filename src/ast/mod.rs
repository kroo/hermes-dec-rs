//! Abstract Syntax Tree (AST) module
//!
//! This module provides instruction-to-expression and block-to-statement conversion
//! for Hermes bytecode decompilation. The core functionality includes:
//! - InstructionToExpressionConverter: Transforms individual instructions into OXC AST expressions
//! - BlockToStatementConverter: Converts CFG basic blocks into JavaScript statement sequences

pub mod block_converter;
pub mod expression_context;
// pub mod instruction_converter;
pub mod instruction_to_statement_converter;
pub mod register_manager;
pub mod variable_mapper;

// Re-export the main types for public API
pub use block_converter::{BlockConversionError, BlockConversionStats, BlockToStatementConverter};
pub use expression_context::{ExpressionContext, ExpressionContextError};
// pub use instruction_converter::{
//     ConversionError, ConversionStats, InstructionToExpressionConverter,
// };
pub use instruction_to_statement_converter::{
    InstructionResult, InstructionToStatementConverter, JumpCondition, JumpType,
    StatementConversionError,
};
pub use register_manager::{RegisterLifetime, RegisterManager, RegisterStats};
pub use variable_mapper::{VariableMapError, VariableMapper, VariableMapping, VariableScope};
