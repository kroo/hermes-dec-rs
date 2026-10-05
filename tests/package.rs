use assert_cmd::Command;
use hermes_dec_rs::analysis::package_analysis::analyze_package;
use hermes_dec_rs::hbc::HbcFile;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;

fn load_report(
    path: &str,
) -> hermes_dec_rs::DecompilerResult<hermes_dec_rs::analysis::package_analysis::PackageReport> {
    let data = fs::read(path)?;
    let hbc = HbcFile::parse(&data)
        .map_err(|message| hermes_dec_rs::DecompilerError::Internal { message })?;
    analyze_package(&hbc)
}

#[test]
fn test_analyze_package_extracts_commonjs_metadata_and_paths() {
    let report = load_report("data/cjs_v96.hbc").expect("package analysis should succeed");

    assert_eq!(report.modules.len(), 2);
    assert_eq!(report.main.as_deref(), Some("index.js"));
    assert_eq!(report.entrypoints, vec!["index.js"]);
    assert!(report.graph_summary.extracted);
    assert_eq!(report.graph_summary.resolved_edges, 1);
    assert_eq!(report.graph_summary.unresolved_edges, 1);
    assert!(report.graph_summary.meaningful_paths);
    assert!(!report.directory_summary.dirs.is_empty());
    assert_eq!(report.directory_summary.dirs[0].dir, "app");
    assert_eq!(report.directory_summary.dirs[1].dir, "modules");

    let index = report
        .modules
        .iter()
        .find(|module| module.filename.as_deref() == Some("index.js"))
        .expect("index.js module should be present");
    assert!(index.is_entry);
    assert!(index
        .dependencies
        .iter()
        .any(|dependency| dependency == "math.js"));
    assert!(index
        .dependencies
        .iter()
        .any(|dependency| dependency == "external:path"));
    assert_eq!(index.suggested_path.as_deref(), Some("app/index.js"));

    let math = report
        .modules
        .iter()
        .find(|module| module.filename.as_deref() == Some("math.js"))
        .expect("math.js module should be present");
    assert!(math.dependencies.is_empty());
    assert_eq!(math.suggested_path.as_deref(), Some("modules/math.js"));
}

#[test]
fn test_analyze_package_show_source_fixture_still_infers_layout() {
    let report = load_report("data/cjs-show-source.hbc").expect("package analysis should succeed");

    assert_eq!(report.main.as_deref(), Some("index.js"));
    assert!(report.graph_summary.extracted);
    assert!(report.modules.iter().all(|module| module
        .suggested_path
        .as_deref()
        .is_some_and(|path| !path.contains("unknown"))));
    assert!(report
        .modules
        .iter()
        .any(|module| module.suggested_path.as_deref() == Some("app/index.js")));
}

#[test]
fn test_package_analyze_summary_shows_graph_and_layout_details() {
    let mut cmd = Command::cargo_bin("hermes-dec-rs").expect("binary should build");
    cmd.args(["package-analyze", "data/cjs_v96.hbc", "--summary"]);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Main: index.js"))
        .stdout(predicate::str::contains("Graph: extracted"))
        .stdout(predicate::str::contains(
            "Layout: filename-aware suggested paths available",
        ))
        .stdout(predicate::str::contains(
            "* index.js -> app/index.js deps=[math.js, external:path]",
        ))
        .stdout(predicate::str::contains("math.js -> modules/math.js"));
}

#[test]
fn test_package_analyze_json_includes_validated_fields() {
    let mut cmd = Command::cargo_bin("hermes-dec-rs").expect("binary should build");
    let output = cmd
        .args(["package-analyze", "data/cjs_v96.hbc", "--json"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    let json: Value = serde_json::from_slice(&output).expect("output should be valid JSON");
    assert_eq!(json["main"], Value::String("index.js".to_string()));
    assert_eq!(json["graph_summary"]["extracted"], Value::Bool(true));
    assert_eq!(json["graph_summary"]["resolved_edges"], Value::from(1));
    assert!(json["cluster_summary"].is_object());
    assert!(json["directory_summary"].is_object());
    assert_eq!(json["directory_summary"]["dirs"][0]["dir"], "app");
    assert_eq!(json["directory_summary"]["dirs"][1]["dir"], "modules");

    let modules = json["modules"]
        .as_array()
        .expect("modules should be an array");
    assert!(modules
        .iter()
        .any(|module| module["suggested_path"] == Value::String("app/index.js".to_string())));
    assert!(modules
        .iter()
        .any(|module| module["suggested_path"] == Value::String("modules/math.js".to_string())));
}
