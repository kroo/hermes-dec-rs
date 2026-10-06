use hermes_dec_rs::cli::{json_literals, sites};
use serde_json::Value;
use std::collections::BTreeSet;

fn literal_report(source: &str) -> hermes_dec_rs::DecompilerResult<Vec<u8>> {
    sites::analyze_literal_view_source(
        source,
        7,
        &BTreeSet::new(),
        &sites::LiteralViewOptions {
            json: true,
            ..Default::default()
        },
    )
}

#[test]
fn literal_view_rejects_wrong_exporter_root() {
    let source = "F[8] = function() { var r=[];\n// HBC function 7, PC 0\nr[0]=1;\n// HBC function 7, PC 1\nreturn r[0];\n};";
    let result = literal_report(source);
    assert!(
        result.is_err(),
        "wrong root accepted: {}",
        String::from_utf8_lossy(result.as_ref().unwrap())
    );
}

#[test]
fn literal_view_rejects_marker_outside_root() {
    let source = "// HBC function 7, PC 0\nF[7] = function() { var r=[]; r[0]=1;\n// HBC function 7, PC 1\nreturn r[0];\n};";
    let result = literal_report(source);
    assert!(
        result.is_err(),
        "outside marker accepted: {}",
        String::from_utf8_lossy(result.as_ref().unwrap())
    );
}

#[test]
fn embedded_json_does_not_silently_round_integer_beyond_u64() {
    let source = "F[7] = function() { var r=[];\n// HBC function 7, PC 0\nr[0]='{\"n\":18446744073709551617}';\n};";
    let bytes = json_literals::analyze_source(
        source,
        7,
        &json_literals::Options {
            pointer: Some("/n".into()),
            ..Default::default()
        },
    )
    .unwrap();
    let report: Value = serde_json::from_slice(&bytes).unwrap();
    let items = report["literals"].as_array().unwrap();
    assert!(
        items.is_empty(),
        "altered literal returned as evidence: {}",
        String::from_utf8_lossy(&bytes)
    );
    assert_eq!(report["counts"]["lossy_number_documents"], 1);
}

#[test]
fn logical_assignment_rhs_writes_remain_conditional() {
    for operator in ["||=", "&&=", "??="] {
        let source = format!("F[7] = function() {{ var r=[];\n// HBC function 7, PC 0\nr[0]=1;\n// HBC function 7, PC 1\nr[1] {operator} (r[0]=2);\n// HBC function 7, PC 2\nreturn r[0];\n}};");
        let bytes = literal_report(&source).unwrap();
        let report: Value = serde_json::from_slice(&bytes).unwrap();
        let row = &report["rows"][2];
        assert_eq!(
            row["view"], row["source"],
            "conditional RHS promoted for {operator}: {row}"
        );
    }
}

#[test]
fn cli_budget_failures_leave_stdout_empty() {
    let fixture =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("data/bundle_semantics.hbc");
    for command in ["read", "json-literals"] {
        let output = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .args([command, fixture.to_str().unwrap(), "0", "--max-bytes", "1"])
            .output()
            .unwrap();
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
    }
}
