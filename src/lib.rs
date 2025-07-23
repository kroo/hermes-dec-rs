//! Hermes-dec-rs: Rust-based high-level decompiler for Hermes bytecode
//!
//! This library provides functionality to parse, analyze, and decompile Hermes HBC
//! (Hermes Bytecode) files into readable JavaScript/TypeScript source code.

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod decompiler;
pub mod error;
pub mod generated;
pub mod hbc;

pub use decompiler::Decompiler;
pub use error::{Error as DecompilerError, Result as DecompilerResult};

// Re-export commonly used types
pub use ast::AstBuilder;
pub use cfg::{Block, Cfg};
pub use hbc::{HbcFile, HbcHeader};
