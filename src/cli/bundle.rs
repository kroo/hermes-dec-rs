use crate::bundle::{export_bundle, BundleOptions};
use crate::{DecompilerError, DecompilerResult, HbcFile};
use std::path::Path;

pub fn export(
    input: &Path,
    output: Option<&Path>,
    minify: bool,
    entry_module: Option<String>,
) -> DecompilerResult<()> {
    let start = std::time::Instant::now();
    let data = std::fs::read(input)?;
    let read_time = start.elapsed();
    let hbc = HbcFile::parse_for_bundle(&data)
        .map_err(|message| DecompilerError::Internal { message })?;
    let parse_time = start.elapsed() - read_time;
    let code = export_bundle(
        &hbc,
        &BundleOptions {
            minify,
            entry_module,
        },
    )?;
    let export_time = start.elapsed() - read_time - parse_time;
    let bytes = code.len();
    let write_start = std::time::Instant::now();
    match output {
        Some(path) => std::fs::write(path, code)?,
        None => print!("{code}"),
    }
    log::info!("bundle timing: read={read_time:?}, parse={parse_time:?}, export={export_time:?}, write={:?}, total={:?}, functions={}, bytes={bytes}", write_start.elapsed(), start.elapsed(), hbc.functions.count());
    Ok(())
}
