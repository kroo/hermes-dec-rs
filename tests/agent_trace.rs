// Include the production analyzer to test its private source-only entry point.
pub use hermes_dec_rs::{bundle, DecompilerError, DecompilerResult};
// The production fast parser is crate-private. The public parser decodes the
// same tables, with additional CFG indexes, for this standalone test harness.
struct HbcFile;
impl HbcFile {
    fn parse_for_bundle(data: &[u8]) -> Result<hermes_dec_rs::HbcFile<'_>, String> {
        hermes_dec_rs::HbcFile::parse(data)
    }
}
#[path = "../src/cli/trace.rs"]
mod trace;

use serde_json::Value;
use std::collections::BTreeSet;
use std::path::Path;

fn source(body: &str) -> String {
    format!("function f() {{ const r = []; let pc = 0; switch (pc) {{ case 0: {{\n{body}\n}} default: throw 0; }} }}")
}

fn analyze(
    body: &str,
    pc: u32,
    depth: usize,
    limit: usize,
    bytes: usize,
) -> DecompilerResult<Value> {
    let result = trace::trace_source(&source(body), 0, pc, depth, limit, bytes, &BTreeSet::new())?;
    Ok(serde_json::from_slice(&result).unwrap())
}

const CHAIN: &str = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nr[0] = r[0].x;\n// HBC function 0, PC 5\nr[1] = r[0];\n// HBC function 0, PC 8\nreturn r[1];";

#[test]
fn clobber_uses_previous_definition_not_itself() {
    let value = analyze(CHAIN, 8, 4, 10, 10000).unwrap();
    assert_eq!(value["schema_version"], 1);
    let nodes = value["nodes"].as_array().unwrap();
    assert_eq!(
        nodes
            .iter()
            .map(|n| n["pc"].as_u64().unwrap())
            .collect::<Vec<_>>(),
        [0, 2, 5, 8]
    );
    assert_eq!(nodes[1]["uses"][0]["previous_definition_pc"], 0);
    assert_eq!(nodes[2]["uses"][0]["previous_definition_pc"], 2);
    assert!(nodes[1]["javascript"]
        .as_str()
        .unwrap()
        .contains("r[0] = r[0].x;"));
}

#[test]
fn block_entry_resets_but_branch_can_read_before_reset() {
    let body = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\npc = r[0] ? 5 : 8;\n} case 5: {\n// HBC function 0, PC 5\nr[1] = r[0];";
    assert_eq!(
        analyze(body, 2, 3, 10, 10000).unwrap()["nodes"][1]["uses"][0]["previous_definition_pc"],
        0
    );
    let value = analyze(body, 5, 3, 10, 10000).unwrap();
    assert_eq!(value["nodes"].as_array().unwrap().len(), 1);
    assert!(value["nodes"][0]["uses"][0]["previous_definition_pc"].is_null());
}

#[test]
fn exception_range_entry_and_exit_are_resets() {
    for boundary in [2, 5] {
        let result = trace::trace_source(
            &source(CHAIN),
            0,
            5,
            5,
            10,
            10000,
            &BTreeSet::from([boundary]),
        )
        .unwrap();
        let value: Value = serde_json::from_slice(&result).unwrap();
        let nodes = value["nodes"].as_array().unwrap();
        assert!(nodes.iter().all(|n| n["pc"] != 0));
        assert!(nodes[0]["uses"][0]["previous_definition_pc"].is_null());
    }
}

#[test]
fn property_effects_and_string_contents_are_not_register_definitions() {
    let body = "// HBC function 0, PC 0\nr[0] = {};\n// HBC function 0, PC 2\nr[1] = 'r[99] = r[98]';\n// HBC function 0, PC 5\nr[0].x = r[1];\n// HBC function 0, PC 8\nr[2] = r[0].x;";
    let effect = analyze(body, 5, 3, 10, 10000).unwrap();
    assert_eq!(effect["nodes"][2]["defines"], serde_json::json!([]));
    assert_eq!(effect["nodes"][1]["uses"], serde_json::json!([]));
    let read = analyze(body, 8, 3, 10, 10000).unwrap();
    assert_eq!(read["nodes"][1]["uses"][0]["previous_definition_pc"], 0);
    assert_eq!(read["nodes"].as_array().unwrap().len(), 2);
}

#[test]
fn multi_statement_write_does_not_create_self_edge() {
    let value = analyze(
        "// HBC function 0, PC 0\nr[0] = 1; r[1] = r[0];",
        0,
        3,
        10,
        10000,
    )
    .unwrap();
    assert!(value["nodes"][0]["uses"][0]["previous_definition_pc"].is_null());
    assert_eq!(
        value["nodes"][0]["uses"][0]["status"],
        "unresolved_intra_pc_write"
    );
}

#[test]
fn helper_effects_comments_equality_and_updates_are_syntactic() {
    let body = "// HBC function 0, PC 0\nr[0] = {}; r[1] = 1;\n// HBC function 0, PC 2\nown(r[0], 'r[7]', r[1], true); // r[7] = r[9]\n// HBC function 0, PC 5\nr[0] === r[1];\n// HBC function 0, PC 8\nr[1]++;";
    let effect = analyze(body, 2, 3, 10, 10000).unwrap();
    assert_eq!(effect["nodes"][1]["defines"], serde_json::json!([]));
    assert_eq!(effect["nodes"][1]["uses"].as_array().unwrap().len(), 2);
    assert!(effect["nodes"][1]["javascript"]
        .as_str()
        .unwrap()
        .contains("own(r[0]"));
    assert_eq!(
        analyze(body, 5, 3, 10, 10000).unwrap()["nodes"][1]["defines"],
        serde_json::json!([])
    );
    let update = analyze(body, 8, 3, 10, 10000).unwrap();
    assert_eq!(update["nodes"][1]["defines"], serde_json::json!([1]));
    assert_eq!(update["nodes"][1]["uses"][0]["previous_definition_pc"], 0);
}

#[test]
fn budgets_and_invalid_pc_are_explicit() {
    let zero = analyze(CHAIN, 8, 0, 1, 10000).unwrap();
    assert_eq!(zero["depth_truncated"], true);
    assert_eq!(zero["nodes"].as_array().unwrap().len(), 1);
    let shallow = analyze(CHAIN, 8, 1, 2, 10000).unwrap();
    assert_eq!(shallow["nodes"].as_array().unwrap().len(), 2);
    assert_eq!(shallow["depth_truncated"], true);
    assert!(analyze(CHAIN, 8, 4, 2, 10000)
        .unwrap_err()
        .to_string()
        .contains("node budget"));
    assert!(analyze(CHAIN, 8, 4, 10, 10)
        .unwrap_err()
        .to_string()
        .contains("byte budget"));
    for pc in [1, 3, 1000] {
        assert!(analyze(CHAIN, pc, 3, 10, 10000).is_err());
    }
    assert!(analyze(CHAIN, 8, 65, 10, 10000).is_err());
    assert!(analyze(CHAIN, 8, 3, 0, 10000).is_err());
    assert!(analyze(CHAIN, 8, 3, 4097, 10000).is_err());
    assert!(analyze(CHAIN, 8, 3, 10, 0).is_err());
    assert!(analyze(CHAIN, 8, 3, 10, 16_777_217).is_err());
}

#[test]
fn snippets_are_bounded_utf8_without_truncating_analysis() {
    let body = format!(
        "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nr[1] = [\"{}\", r[0]];",
        "界".repeat(1000)
    );
    let value = analyze(&body, 2, 3, 10, 10000).unwrap();
    let node = &value["nodes"][1];
    assert_eq!(node["snippet_truncated"], true);
    assert!(node["javascript"].as_str().unwrap().len() <= 1024);
    assert!(node["original_bytes"].as_u64().unwrap() > 3000);
    assert_eq!(node["uses"][0]["previous_definition_pc"], 0);
}

#[test]
fn authored_hbc_uses_complete_exporter_and_rejects_unknown_function_pc() {
    let input = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/simple_arithmetic.hbc"
    ));
    let bytes = trace::report(input, 0, 0, 2, 20, 20000).unwrap();
    let value: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(value["source"], "export_function_fragments");
    assert!(value["nodes"][0]["javascript"]
        .as_str()
        .unwrap()
        .contains("// HBC function 0, PC 0"));
    assert_eq!(bytes, trace::report(input, 0, 0, 2, 20, 20000).unwrap());
    assert!(trace::report(input, u32::MAX, 0, 2, 20, 20000).is_err());
    assert!(trace::report(input, 0, u32::MAX, 2, 20, 20000).is_err());
    assert!(trace::run(input, 0, u32::MAX, 2, 20, 20000).is_err());
}

#[test]
fn late_pc_in_large_authored_initializer() {
    use std::fmt::Write;
    let mut body = String::new();
    for pc in 0..30_000 {
        writeln!(&mut body, "// HBC function 0, PC {pc}\nr[0] = r[0] + 1;").unwrap();
    }
    let value = analyze(&body, 29_999, 8, 64, 100000).unwrap();
    let nodes = value["nodes"].as_array().unwrap();
    assert_eq!(nodes.len(), 9);
    assert_eq!(nodes[0]["pc"], 29_991);
    assert_eq!(nodes[8]["uses"][0]["previous_definition_pc"], 29_998);
    assert_eq!(value["depth_truncated"], true);
}

#[test]
fn wired_cli_has_atomic_budget_failures_and_stderr_errors() {
    use assert_cmd::Command;
    let input = concat!(env!("CARGO_MANIFEST_DIR"), "/data/simple_arithmetic.hbc");
    let output = Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args(["trace", input, "0", "0"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let value: Value = serde_json::from_slice(&output).unwrap();
    assert_eq!(value["schema_version"], 1);
    assert!(output.len() <= 100000);
    for tail in [
        vec!["0", "4294967295"],
        vec!["4294967295", "0"],
        vec!["0", "0", "--max-bytes", "1"],
        vec!["0", "0", "--limit", "0"],
        vec!["0", "0", "--depth", "65"],
    ] {
        let result = Command::cargo_bin("hermes-dec-rs")
            .unwrap()
            .args(["trace", input])
            .args(tail)
            .assert()
            .failure();
        assert!(result.get_output().stdout.is_empty());
        assert!(!result.get_output().stderr.is_empty());
    }
}
