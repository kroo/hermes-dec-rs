use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Result type for decompiler operations
pub type Result<T> = std::result::Result<T, Error>;

/// Custom error types for the Hermes decompiler
#[derive(Error, Debug, Diagnostic, Clone)]
pub enum Error {
    #[error("I/O error: {0}")]
    #[diagnostic(code(hermes_dec::io_error))]
    Io(String),

    #[error("Invalid HBC magic number: expected 0x{expected:08X}, got 0x{got:08X}")]
    #[diagnostic(code(hermes_dec::invalid_magic))]
    InvalidMagic { expected: u32, got: u32 },

    #[error("Unsupported HBC version: {version}")]
    #[diagnostic(code(hermes_dec::unsupported_version))]
    UnsupportedVersion { version: u32 },

    #[error("Parse error at offset {offset}: {message}")]
    #[diagnostic(code(hermes_dec::parse_error))]
    Parse { offset: usize, message: String },

    #[error("Control flow structuring failed: {message}")]
    #[diagnostic(code(hermes_dec::structuring_error))]
    Structuring { message: String },

    #[error("AST generation failed: {message}")]
    #[diagnostic(code(hermes_dec::ast_error))]
    Ast { message: String },

    #[error("Code generation failed: {message}")]
    #[diagnostic(code(hermes_dec::codegen_error))]
    Codegen { message: String },

    #[error("Unknown opcode: {opcode} at PC {pc}")]
    #[diagnostic(code(hermes_dec::unknown_opcode))]
    UnknownOpcode { opcode: u8, pc: u32 },

    #[error("Invalid instruction arguments: {message}")]
    #[diagnostic(code(hermes_dec::invalid_args))]
    InvalidArgs { message: String },

    #[error("Internal error: {message}")]
    #[diagnostic(code(hermes_dec::internal_error))]
    Internal { message: String },
}

impl Error {
    /// Create a parse error with span information
    pub fn parse_with_span(offset: usize, message: impl Into<String>, _span: SourceSpan) -> Self {
        Error::Parse {
            offset,
            message: message.into(),
        }
    }

    /// Create an internal error
    pub fn internal(message: impl Into<String>) -> Self {
        Error::Internal {
            message: message.into(),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::Io(err.to_string())
    }
}
