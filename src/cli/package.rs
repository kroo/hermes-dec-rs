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
    // Cluster coverage statistics
    let ce = report.cluster_stats.clustered_edges;
    let te = report.cluster_stats.total_edges;
    let pct = if te > 0 { (ce as f64) * 100.0 / (te as f64) } else { 0.0 };
    println!("Clustered deps: {}/{} ({:.1}%)", ce, te, pct);

    // Strength metrics (brief)
    let sm = &report.strength_metrics;
    let pct_pairs = if sm.total_pairs > 0 { (sm.edges_ge_threshold as f64) * 100.0 / (sm.total_pairs as f64) } else { 0.0 };
    println!(
        "Strength: thresh={:.2}, edges>=thresh: {}/{} ({:.1}%), avg_s={:.3}, avg_J(dep)={:.3}, avg_J(cons)={:.3}",
        sm.threshold, sm.edges_ge_threshold, sm.total_pairs, pct_pairs, sm.avg_strength_over_edges, sm.avg_jaccard_deps_over_pairs, sm.avg_jaccard_consumers_over_pairs
    );
    println!(
        "Counts: bidir={}, unidir={}, transitive={}",
        sm.bidirectional_count, sm.unidirectional_count, sm.transitive_count
    );

    // Main entry and cluster summary
    println!("Main: {}", report.main.as_deref().unwrap_or("-"));
    let cs = &report.cluster_summary;
    println!(
        "Clusters: total={} | sizes: 2-4={}, 5-9={}, 10-19={}, 20+={} | patterns: tight={}, star={}, chain={}, cluster={}",
        cs.total,
        cs.buckets.s02_04,
        cs.buckets.s05_09,
        cs.buckets.s10_19,
        cs.buckets.s20_plus,
        cs.tight,
        cs.star,
        cs.chain,
        cs.cluster
    );

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
