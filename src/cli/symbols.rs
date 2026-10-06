//! Source-only slot-write candidates and raw literal syntax, never runtime values.
use super::origins;
use crate::bundle::export_function_fragments;
use crate::{DecompilerError, DecompilerResult, HbcFile};
use std::io::Write;
use std::path::Path;

const OUTPUT_CAP: usize = 16 * 1024 * 1024;
const SLOT_CAP: usize = 4096;
const MATCH_CAP: usize = 64;
const MATCH_BYTES: usize = 1024;
const TOTAL_MATCH_BYTES: usize = 64 * 1024;

#[derive(Clone, Debug)]
pub struct Options {
    pub depth: usize,
    pub definition_limit: usize,
    pub literal_limit: usize,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
    pub slots: Vec<u32>,
    pub matches: Vec<String>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            depth: 8,
            definition_limit: 64,
            literal_limit: 32,
            limit: 32,
            offset: 0,
            max_bytes: 100_000,
            scan_work: origins::DEFAULT_SYMBOL_SCAN_WORK,
            slots: Vec::new(),
            matches: Vec::new(),
        }
    }
}

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

fn validate(options: &Options) -> DecompilerResult<()> {
    if options.depth > 64
        || !(1..=4096).contains(&options.definition_limit)
        || !(1..=128).contains(&options.literal_limit)
        || !(1..=1000).contains(&options.limit)
        || !(1..=OUTPUT_CAP).contains(&options.max_bytes)
        || !(1..=origins::MAX_SYMBOL_SCAN_WORK).contains(&options.scan_work)
    {
        return Err(error("symbols bounds: depth 0..64, definition_limit 1..4096, literal_limit 1..128, limit 1..1000, max_bytes 1..16777216, scan_work 1..16777216"));
    }
    if options.slots.len() > SLOT_CAP
        || options.matches.len() > MATCH_CAP
        || options
            .matches
            .iter()
            .any(|s| s.is_empty() || s.len() > MATCH_BYTES)
    {
        return Err(error("symbols filter bounds: at most 4096 slots and 64 nonempty matches of at most 1024 bytes each"));
    }
    // Individual lengths and counts are bounded before summing.
    if options.matches.iter().map(String::len).sum::<usize>() > TOTAL_MATCH_BYTES {
        return Err(error("symbols matches exceed 65536 total bytes"));
    }
    Ok(())
}

/// Read one function's complete exporter fragment and delegate static analysis.
/// Offset is a raw store ordinal, not an index into filtered results.
pub fn report(input: &Path, function: u32, options: &Options) -> DecompilerResult<Vec<u8>> {
    validate(options)?;
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let header = hbc
        .functions
        .get_parsed_header(function)
        .ok_or_else(|| error(format!("Unknown function {function}")))?;
    let exceptions: Vec<_> = header
        .exc_handlers
        .iter()
        .map(|handler| (handler.start, handler.end, handler.target))
        .collect();
    let mut fragments = export_function_fragments(&hbc, &[function])?;
    let (id, source) = fragments
        .pop()
        .ok_or_else(|| error("Missing symbols exporter fragment"))?;
    if id != function || !fragments.is_empty() {
        return Err(error("Unexpected symbols exporter fragments"));
    }
    let query = origins::SymbolOptions {
        function,
        depth: options.depth,
        definition_limit: options.definition_limit,
        literal_limit: options.literal_limit,
        limit: options.limit,
        offset: options.offset,
        max_bytes: options.max_bytes,
        scan_work: options.scan_work,
        slots: options.slots.clone(),
        matches: options.matches.clone(),
    };
    let bytes = origins::analyze_symbols_source(&source, &query, &exceptions)?;
    if bytes.len() > options.max_bytes {
        return Err(error("symbols output byte budget exceeded"));
    }
    Ok(bytes)
}

/// Build and serialize the entire report before writing anything to stdout.
pub fn run(input: &Path, function: u32, options: &Options) -> DecompilerResult<()> {
    let bytes = report(input, function, options)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inclusive_bounds_and_unbounded_raw_cursor() {
        for depth in [0, 64] {
            for (definition_limit, literal_limit, limit, max_bytes) in
                [(1, 1, 1, 1), (4096, 128, 1000, OUTPUT_CAP)]
            {
                validate(&Options {
                    depth,
                    definition_limit,
                    literal_limit,
                    limit,
                    offset: usize::MAX,
                    max_bytes,
                    ..Options::default()
                })
                .unwrap();
            }
        }
    }
}
