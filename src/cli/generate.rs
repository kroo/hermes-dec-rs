use crate::hbc::instructions::codegen;
use anyhow::Result;

pub fn generate_instructions() -> Result<()> {
    codegen::generate_unified_instructions()
}
