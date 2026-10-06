//! Bounded source-level object provenance, not runtime heap identity.
use super::sites;
use crate::DecompilerResult;
use std::io::Write;
use std::path::Path;

pub use super::sites::ObjectOptions as Options;

/// Report property sites, optionally anchored to an exact local definition PC.
pub fn report(
    input: &Path,
    function: u32,
    origin_pc: Option<u32>,
    options: &Options,
) -> DecompilerResult<Vec<u8>> {
    sites::report_objects(input, function, origin_pc, options)
}

/// Complete analysis and byte-budget validation before writing to stdout.
pub fn run(
    input: &Path,
    function: u32,
    origin_pc: Option<u32>,
    options: &Options,
) -> DecompilerResult<()> {
    let bytes = report(input, function, origin_pc, options)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}
