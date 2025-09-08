use crate::analysis::package_analysis::analyze_package;
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::path::Path;

pub fn package_analyze(input: &Path, json: bool, summary: bool) -> DecompilerResult<()> {
    let data = std::fs::read(input).map_err(DecompilerError::from)?;
    let hbc = HbcFile::parse(&data).map_err(|e| DecompilerError::Internal { message: e })?;

    let report = analyze_package(&hbc)?;

    if json {
        let s = serde_json::to_string_pretty(&report)
            .map_err(|e| DecompilerError::Internal { message: e.to_string() })?;
        println!("{}", s);
        return Ok(());
    }

    // Text summary
    println!("Package Analysis Summary\n-------------------------");
    println!("Functions: {}", report.stats.function_count);
    println!("Strings:   {}", report.stats.string_count);
    println!("Modules:   {}", report.modules.len());
    println!("Entrypoints: {}", report.entrypoints.len());

    if !summary {
        for m in &report.modules {
            let name = m
                .filename
                .as_deref()
                .or(Some("<static>"))
                .unwrap_or("");
            let entry_mark = if m.is_entry { "*" } else { " " };
            let fn_str = m
                .function_id
                .map(|v| v.to_string())
                .unwrap_or_else(|| "-".into());
            println!("{} fn={} sym={} {} (deps: {})", entry_mark, fn_str, m.symbol_id.map(|v| v.to_string()).unwrap_or_else(|| "-".into()), name, m.dependencies.len());
        }
    }

    Ok(())
}
