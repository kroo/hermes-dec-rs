#![allow(dead_code)]

//! Hermes Bytecode Instructions Auto-Generation
//!
//! This module handles automatic generation of Rust instruction types
//! for all Hermes bytecode versions from v51 to the latest.

pub mod codegen;
pub mod hermes_repo_fetcher;
pub mod hermes_repo_parser;
pub mod versions;
