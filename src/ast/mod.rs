//! Abstract Syntax Tree (AST) module
//!
//! This module provides instruction-to-expression and block-to-statement conversion
//! for Hermes bytecode decompilation. The module is organized into sub-modules by functionality:
//!
//! - `builders/`: High-level AST construction utilities
//! - `comments/`: Comment management and attachment system
//! - `context/`: Conversion context and state management
//! - `control_flow/`: Control flow pattern detection and conversion
//! - `instructions/`: Low-level instruction-to-AST conversion
//! - `variables/`: Variable and register management

pub mod builders;
pub mod comments;
pub mod context;
pub mod control_flow;
pub mod instructions;
pub mod variables;

// Re-export the main types for public API
pub use crate::hbc::{InstructionIndex, InstructionOffset};
pub use builders::{build_function_program, generate_code_with_comments, FunctionBuilder};
pub use comments::{
    AddressCommentManager, CommentAnchorManager, CommentKind, CommentPosition, NodeId,
    PendingComment, PositionAssigner,
};
pub use context::{ExpressionContext, ExpressionContextError};
pub use control_flow::{
    BlockConversionError, BlockConversionStats, BlockToStatementConverter, ConditionalConverter,
    SwitchConversionError, SwitchConverter,
};
pub use instructions::{
    InstructionResult, InstructionToStatementConverter, JumpCondition, JumpType,
    StatementConversionError,
};
pub use variables::{
    RegisterLifetime, RegisterManager, RegisterStats, VariableMapError, VariableMapper,
    VariableMapping,
};
