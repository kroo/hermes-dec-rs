//! Variable management system
//!
//! This module provides services for managing variables during AST generation,
//! including register-to-variable mapping and SSA-based variable naming.

pub mod register_manager;
pub mod variable_mapper;

pub use register_manager::{RegisterLifetime, RegisterManager, RegisterStats};
pub use variable_mapper::{VariableMapError, VariableMapper, VariableMapping};
