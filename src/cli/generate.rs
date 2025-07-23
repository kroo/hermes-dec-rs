use anyhow::Result;
use crate::hbc::instructions::codegen;

pub fn generate_instructions() -> Result<()> {
    codegen::generate_unified_instructions()
} 