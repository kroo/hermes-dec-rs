//! Expression generation context and state management
//!
//! This module provides context information needed for generating expressions,
//! including access to HBC file tables and current processing state.

use crate::hbc::HbcFile;

/// Context for expression generation
///
/// Provides access to HBC file tables and tracks the current state of
/// expression generation, including which basic block is being processed.
#[derive(Debug)]
pub struct ExpressionContext<'a> {
    /// Current basic block being processed
    pub current_block: Option<u32>,
    /// Current program counter
    pub current_pc: u32,
    /// Reference to HBC file for table lookups
    pub hbc_file: Option<&'a HbcFile<'a>>,
    /// Function index being processed (for context)
    pub function_index: Option<u32>,
}

impl<'a> ExpressionContext<'a> {
    /// Create a new expression context without HBC file access
    pub fn new() -> Self {
        Self {
            current_block: None,
            current_pc: 0,
            hbc_file: None,
            function_index: None,
        }
    }

    /// Create a new expression context with HBC file access
    pub fn with_hbc_file(hbc_file: &'a HbcFile<'a>) -> Self {
        Self {
            current_block: None,
            current_pc: 0,
            hbc_file: Some(hbc_file),
            function_index: None,
        }
    }

    /// Create a new expression context with full initialization
    pub fn with_context(hbc_file: &'a HbcFile<'a>, function_index: u32, initial_pc: u32) -> Self {
        Self {
            current_block: None,
            current_pc: initial_pc,
            hbc_file: Some(hbc_file),
            function_index: Some(function_index),
        }
    }

    /// Set the current basic block being processed
    pub fn set_block(&mut self, block_id: u32) {
        self.current_block = Some(block_id);
    }

    /// Set the current program counter
    pub fn set_pc(&mut self, pc: u32) {
        self.current_pc = pc;
    }

    /// Set the function index being processed
    pub fn set_function_index(&mut self, function_index: u32) {
        self.function_index = Some(function_index);
    }

    /// Update HBC file reference (useful when switching files)
    pub fn set_hbc_file(&mut self, hbc_file: &'a HbcFile<'a>) {
        self.hbc_file = Some(hbc_file);
    }

    /// Get the current basic block ID
    pub fn current_block(&self) -> Option<u32> {
        self.current_block
    }

    /// Get the current program counter
    pub fn current_pc(&self) -> u32 {
        self.current_pc
    }

    /// Get the current function index
    pub fn function_index(&self) -> Option<u32> {
        self.function_index
    }

    /// Check if HBC file access is available
    pub fn has_hbc_file(&self) -> bool {
        self.hbc_file.is_some()
    }

    /// Get reference to HBC file if available
    pub fn hbc_file(&self) -> Option<&HbcFile<'a>> {
        self.hbc_file
    }

    /// Look up a string from the string table
    pub fn lookup_string(&self, string_id: u32) -> Result<String, ExpressionContextError> {
        match self.hbc_file {
            Some(hbc_file) => hbc_file
                .strings
                .get(string_id)
                .map_err(|e| ExpressionContextError::StringTableError(e.to_string())),
            None => Err(ExpressionContextError::NoHbcFile),
        }
    }

    /// Look up a BigInt from the BigInt table
    pub fn lookup_bigint(&self, bigint_id: u32) -> Result<String, ExpressionContextError> {
        match self.hbc_file {
            Some(hbc_file) => {
                // Get BigInt value from table and append 'n' suffix for JavaScript BigInt literal
                match hbc_file.bigints.get(bigint_id) {
                    Ok(bigint_str) => Ok(format!("{}n", bigint_str)),
                    Err(e) => Err(ExpressionContextError::BigIntTableError(e)),
                }
            }
            None => Err(ExpressionContextError::NoHbcFile),
        }
    }

    /// Look up a function name from the function table
    pub fn lookup_function_name(&self, function_id: u32) -> Result<String, ExpressionContextError> {
        match self.hbc_file {
            Some(hbc_file) => hbc_file
                .functions
                .get_function_name(function_id, &hbc_file.strings)
                .ok_or_else(|| ExpressionContextError::FunctionNotFound(function_id)),
            None => Err(ExpressionContextError::NoHbcFile),
        }
    }

    /// Get the HBC version being processed
    pub fn hbc_version(&self) -> Option<u32> {
        self.hbc_file.map(|hbc| hbc.header.version)
    }

    /// Check if we're processing a specific HBC version
    pub fn is_hbc_version(&self, version: u32) -> bool {
        self.hbc_version().map_or(false, |v| v == version)
    }

    /// Create a child context for nested processing (preserves HBC file reference)
    pub fn create_child_context(&self) -> Self {
        Self {
            current_block: None,
            current_pc: 0,
            hbc_file: self.hbc_file,
            function_index: self.function_index,
        }
    }

    /// Reset context state while preserving HBC file reference
    pub fn reset(&mut self) {
        self.current_block = None;
        self.current_pc = 0;
        // Keep hbc_file and function_index
    }
}

impl<'a> Default for ExpressionContext<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur during expression context operations
#[derive(Debug, thiserror::Error)]
pub enum ExpressionContextError {
    #[error("No HBC file available for table lookups")]
    NoHbcFile,
    #[error("String table error: {0}")]
    StringTableError(String),
    #[error("BigInt table error: {0}")]
    BigIntTableError(String),
    #[error("Function {0} not found in function table")]
    FunctionNotFound(u32),
    #[error("Invalid table index {index} for table {table_name}")]
    InvalidTableIndex { table_name: String, index: u32 },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression_context_creation() {
        let ctx = ExpressionContext::new();
        assert_eq!(ctx.current_block(), None);
        assert_eq!(ctx.current_pc(), 0);
        assert_eq!(ctx.function_index(), None);
        assert!(!ctx.has_hbc_file());
    }

    #[test]
    fn test_expression_context_state_management() {
        let mut ctx = ExpressionContext::new();

        // Test setting block
        ctx.set_block(42);
        assert_eq!(ctx.current_block(), Some(42));

        // Test setting PC
        ctx.set_pc(100);
        assert_eq!(ctx.current_pc(), 100);

        // Test setting function index
        ctx.set_function_index(5);
        assert_eq!(ctx.function_index(), Some(5));
    }

    #[test]
    fn test_expression_context_reset() {
        let mut ctx = ExpressionContext::new();

        ctx.set_block(42);
        ctx.set_pc(100);
        ctx.set_function_index(5);

        ctx.reset();

        assert_eq!(ctx.current_block(), None);
        assert_eq!(ctx.current_pc(), 0);
        // function_index should be preserved
        assert_eq!(ctx.function_index(), Some(5));
    }

    #[test]
    fn test_child_context_creation() {
        let mut parent_ctx = ExpressionContext::new();
        parent_ctx.set_function_index(10);

        let child_ctx = parent_ctx.create_child_context();

        // Child should start fresh but preserve function context
        assert_eq!(child_ctx.current_block(), None);
        assert_eq!(child_ctx.current_pc(), 0);
        assert_eq!(child_ctx.function_index(), Some(10));
        assert_eq!(child_ctx.has_hbc_file(), parent_ctx.has_hbc_file());
    }

    #[test]
    fn test_error_cases_without_hbc_file() {
        let ctx = ExpressionContext::new();

        // Should return errors when trying to access tables without HBC file
        assert!(matches!(
            ctx.lookup_string(0),
            Err(ExpressionContextError::NoHbcFile)
        ));

        assert!(matches!(
            ctx.lookup_bigint(0),
            Err(ExpressionContextError::NoHbcFile)
        ));

        assert!(matches!(
            ctx.lookup_function_name(0),
            Err(ExpressionContextError::NoHbcFile)
        ));
    }
}
