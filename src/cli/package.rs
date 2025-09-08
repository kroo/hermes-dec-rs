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
            let entry_mark = if m.is_entry { "*" } else { " " };
            let fn_str = m.function_id.map(|v| v.to_string()).unwrap_or_else(|| "-".into());
            let mid = m
                .module_id_u32
                .map(|n| n.to_string())
                .or_else(|| m.module_id_str.clone())
                .unwrap_or_else(|| "-".into());
            let name = m.filename.as_deref().unwrap_or("");
            let depth = m.depth.map(|d| d.to_string()).unwrap_or_else(|| "-".into());
            let indeg = m.indegree.map(|d| d.to_string()).unwrap_or_else(|| "0".into());
            let cluster = m.cluster.as_deref().unwrap_or("-");
            // Sample first up to 4 deps for readability
            let sample = if m.dependencies.is_empty() {
                String::new()
            } else {
                let take = m.dependencies.iter().take(4).cloned().collect::<Vec<_>>();
                format!(" [{}]", take.join(", "))
            };
            println!(
                "{} mid={} fn={} deps={} depth={} indeg={} cluster={} {}{}",
                entry_mark,
                mid,
                fn_str,
                m.dependencies.len(),
                depth,
                indeg,
                cluster,
                name,
                sample
            );
        }
    }

    Ok(())
}
