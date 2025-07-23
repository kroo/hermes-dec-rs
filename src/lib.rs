//! Hermes-dec-rs: Rust-based high-level decompiler for Hermes bytecode
//! 
//! This library provides functionality to parse, analyze, and decompile Hermes HBC
//! (Hermes Bytecode) files into readable JavaScript/TypeScript source code.

pub mod ast;
pub mod cfg;
pub mod cli;
pub mod decompiler;
pub mod error;
pub mod hbc;
pub mod generated;

pub use error::{Error as DecompilerError, Result as DecompilerResult};
pub use decompiler::Decompiler;

// Re-export commonly used types
pub use hbc::{HbcFile, HbcHeader};
pub use cfg::{Cfg, Block};
pub use ast::AstBuilder;
