// Include the owned production module without requiring parent CLI wiring.
pub use hermes_dec_rs::{bundle, DecompilerError, DecompilerResult};
struct HbcFile;
impl HbcFile {
    fn parse_for_bundle(data: &[u8]) -> Result<hermes_dec_rs::HbcFile<'_>, String> {
        hermes_dec_rs::HbcFile::parse(data)
    }
}
#[path = "../src/cli/sites.rs"]
mod sites;

use serde_json::{json, Value};
use std::collections::BTreeSet;
use std::path::Path;

fn source(body: &str) -> String {
    format!("function f() {{ const r = []; let pc = 0; switch (pc) {{ case 0: {{\n{body}\n}} default: throw 0; }} }}")
}

fn analyze(body: &str, kind: &str, slots: &[u32], depth: usize) -> Value {
    let bytes = sites::catalog_sources(
        &[(0, source(body), BTreeSet::new())],
        kind,
        slots,
        depth,
        1000,
        0,
        16_777_216,
    )
    .unwrap();
    serde_json::from_slice(&bytes).unwrap()
}

const CONSTRUCTOR: &str = "// HBC function 0, PC 0\nr[1] = getCtor();\n// HBC function 0, PC 2\nr[2] = {};\n// HBC function 0, PC 4\nr[3] = 17;\n// HBC function 0, PC 6\nr[4] = 'opaque';\n// HBC function 0, PC 8\nr[5] = construct(r[1], r[2], [r[4],r[3],false]);\n// HBC function 0, PC 10\nr[6].slots[9] = r[5];";

#[test]
fn ordered_constructor_args_receiver_and_literal_definitions() {
    let result = analyze(CONSTRUCTOR, "constructor", &[], 3);
    let site = &result["sites"][0];
    assert_eq!(result["total"], 1);
    assert_eq!(site["pc"], 8);
    assert_eq!(site["operands"][0]["role"], "callee");
    assert_eq!(site["operands"][1]["role"], "preallocated_receiver");
    assert_eq!(site["operands"][2]["argument_index"], 0);
    assert_eq!(site["operands"][2]["expression"]["javascript"], "r[4]");
    assert_eq!(site["operands"][3]["expression"]["javascript"], "r[3]");
    assert_eq!(site["operands"][4]["expression"]["javascript"], "false");
    assert_eq!(
        site["operands"][0]["edges"][0]["source"]["javascript"],
        "r[1] = getCtor()"
    );
    assert_eq!(site["operands"][2]["edges"][0]["source_pc"], 6);
    assert_eq!(
        site["operands"][2]["nodes"][0]["source"]["javascript"],
        "r[4] = 'opaque'"
    );
    assert_eq!(
        site["operands"][3]["edges"][0]["source"]["javascript"],
        "r[3] = 17"
    );
    assert_eq!(site["operands"][4]["edges"], json!([]));
}

#[test]
fn store_of_construct_result_has_local_transitive_provenance() {
    let result = analyze(CONSTRUCTOR, "slot-write", &[9], 4);
    let operand = &result["sites"][0]["operands"][2];
    assert_eq!(operand["edges"][0]["source_pc"], 8);
    assert!(operand["edges"][0]["source"]["javascript"]
        .as_str()
        .unwrap()
        .contains("construct("));
    let pcs: Vec<_> = operand["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .map(|n| n["source_pc"].as_u64().unwrap())
        .collect();
    assert_eq!(pcs, [0, 2, 4, 6, 8]);
    assert_eq!(operand["depth_truncated"], false);
}

#[test]
fn slot_filter_excludes_non_slots_and_plain_destination_is_not_read() {
    let body = format!("{CONSTRUCTOR}\n// HBC function 0, PC 12\nr[7] = r[6].slots[9];\n// HBC function 0, PC 14\nr[6].slots[10] = r[7];\n// HBC function 0, PC 16\nput(r[6], 'x', r[7], false, strict);");
    let result = analyze(&body, "all", &[9], 2);
    assert_eq!(result["total"], 2);
    assert_eq!(result["sites"][0]["kind"], "slot-write");
    assert_eq!(result["sites"][1]["kind"], "slot-read");
    for site in result["sites"].as_array().unwrap() {
        assert_eq!(site["operands"][0]["role"], "environment");
        assert_eq!(site["operands"][0]["expression"]["javascript"], "r[6]");
        assert_eq!(site["operands"][1]["role"], "slot_index");
        assert_eq!(site["operands"][1]["expression"]["javascript"], "9");
    }
    assert_eq!(result["sites"][1]["operands"][0]["edges"][0]["register"], 6);
    assert_eq!(analyze(&body, "slot-read", &[], 2)["total"], 1);
    assert_eq!(analyze(&body, "property-write", &[], 2)["total"], 1);
}

#[test]
fn properties_helpers_and_assignment_effects_are_not_register_writes() {
    let body = "// HBC function 0, PC 0\nr[0] = {};\n// HBC function 0, PC 2\nr[1] = 3;\n// HBC function 0, PC 4\nown(r[0], 'x', r[1], true);\n// HBC function 0, PC 6\nr[0].x = r[1];\n// HBC function 0, PC 8\nr[0][r[1]] = r[1];\n// HBC function 0, PC 10\nput(r[0], 'y', r[1], false, strict);";
    let result = analyze(body, "property-write", &[], 2);
    assert_eq!(result["total"], 4);
    for site in result["sites"].as_array().unwrap() {
        assert_eq!(site["operands"][0]["edges"][0]["source_pc"], 0);
        assert_eq!(site["operands"][2]["edges"][0]["source_pc"], 2);
    }
}

#[test]
fn overwrites_use_latest_prior_assignment_not_runtime_values() {
    let body = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nr[0] = r[0].getter;\n// HBC function 0, PC 4\nr[2].slots[1] = r[0];";
    let result = analyze(body, "slot-write", &[], 3);
    let value = &result["sites"][0]["operands"][2];
    assert_eq!(value["edges"][0]["source_pc"], 2);
    assert_eq!(value["nodes"][1]["edges"][0]["source_pc"], 0);
    assert_eq!(
        value["nodes"][1]["source"]["javascript"],
        "r[0] = r[0].getter"
    );
}

#[test]
fn every_case_and_exception_boundary_resets_definitions() {
    let body = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nr[2].slots[1] = r[0];\n} case 4: {\n// HBC function 0, PC 4\nr[2].slots[1] = r[0];";
    let result = analyze(body, "slot-write", &[], 3);
    assert_eq!(
        result["sites"][0]["operands"][2]["edges"][0]["source_pc"],
        0
    );
    assert!(result["sites"][1]["operands"][2]["edges"][0]["source_pc"].is_null());
    let straight =
        "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 4\nr[2].slots[1] = r[0];";
    for boundary in [1, 2, 4] {
        let bytes = sites::catalog_sources(
            &[(0, source(straight), BTreeSet::from([boundary]))],
            "all",
            &[],
            3,
            10,
            0,
            100000,
        )
        .unwrap();
        let result: Value = serde_json::from_slice(&bytes).unwrap();
        assert!(result["sites"][0]["operands"][2]["edges"][0]["source_pc"].is_null());
    }
    let inline = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nswitch (r[1]) {case 1: r[2].slots[1] = r[0]; break; default: break;}";
    assert!(
        analyze(inline, "all", &[], 2)["sites"][0]["operands"][2]["edges"][0]["source_pc"]
            .is_null()
    );
}

#[test]
fn intra_pc_definitions_are_explicitly_unresolved() {
    let body = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 2\nr[0] = 2; r[2].slots[1] = r[0];";
    let edge = &analyze(body, "all", &[], 3)["sites"][0]["operands"][2]["edges"][0];
    assert!(edge["source_pc"].is_null());
    assert_eq!(edge["status"], "unresolved_intra_pc_write");
}

#[test]
fn literal_fake_registers_and_line_anchored_pc_are_not_syntax() {
    let body = "// HBC function 0, PC 0\nr[0] = `fake\n// HBC function 0, PC 999\nr[7] = r[8]`;\n// HBC function 0, PC 2\nown(r[2], 'r[7]', 'r[8]', true); // r[4] = r[9]\n// HBC function 0, PC 4\nr[2].slots[1] = r[0];";
    let result = analyze(body, "all", &[], 3);
    assert_eq!(result["total"], 2);
    assert_eq!(result["sites"][0]["pc"], 2);
    assert_eq!(result["sites"][0]["operands"][1]["edges"], json!([]));
    assert_eq!(result["sites"][0]["operands"][2]["edges"], json!([]));
    assert_eq!(
        result["sites"][1]["operands"][2]["nodes"][0]["edges"],
        json!([])
    );
}

#[test]
fn utf8_previews_are_bounded_and_analysis_remains_complete() {
    let body = format!("// HBC function 0, PC 0\nr[0] = '{}';\n// HBC function 0, PC 2\nr[2].slots[1] = [r[0], '{}'];", "界".repeat(700), "界".repeat(700));
    let result = analyze(&body, "all", &[], 2);
    let value = &result["sites"][0]["operands"][2];
    for snippet in [
        &result["sites"][0]["exact_expression"],
        &value["expression"],
        &value["edges"][0]["source"],
        &value["nodes"][0]["source"],
    ] {
        assert!(snippet["javascript"].as_str().unwrap().len() <= 1024);
        assert_eq!(snippet["truncated"], true);
        assert!(snippet["original_bytes"].as_u64().unwrap() > 2000);
    }
    assert_eq!(value["edges"][0]["source_pc"], 0);
}

#[test]
fn stable_batch_paging_and_span_identity() {
    let sources = [
        (
            2,
            source(CONSTRUCTOR).replace("function 0,", "function 2,"),
            BTreeSet::new(),
        ),
        (0, source(CONSTRUCTOR), BTreeSet::new()),
    ];
    let page = |offset| {
        let bytes = sites::catalog_sources(&sources, "all", &[], 1, 1, offset, 100000).unwrap();
        serde_json::from_slice::<Value>(&bytes).unwrap()
    };
    for offset in 0..4 {
        let result = page(offset);
        assert_eq!(result, page(offset));
        assert_eq!(result["total"], 4);
        assert_eq!(
            result["sites"][0]["function_id"],
            if offset < 2 { 0 } else { 2 }
        );
        assert_eq!(
            result["sites"][0]["pc"],
            if offset % 2 == 0 { 8 } else { 10 }
        );
        assert_eq!(
            result["next_offset"],
            if offset == 3 {
                Value::Null
            } else {
                json!(offset + 1)
            }
        );
        let site = &result["sites"][0];
        let src = &sources[if offset < 2 { 1 } else { 0 }].1;
        let start = site["source_span"][0].as_u64().unwrap() as usize;
        let end = site["source_span"][1].as_u64().unwrap() as usize;
        assert_eq!(
            &src[start..end],
            site["exact_expression"]["javascript"].as_str().unwrap()
        );
    }
    assert_eq!(page(usize::MAX)["sites"], json!([]));
    let same_pc = analyze(
        "// HBC function 0, PC 0\nown(r[0], 'a', 1, true); own(r[0], 'b', 2, true);",
        "all",
        &[],
        0,
    );
    assert_eq!(same_pc["sites"][0]["ordinal"], 0);
    assert_eq!(same_pc["sites"][1]["ordinal"], 1);
}

#[test]
fn depth_node_operand_and_edge_truncation_are_explicit() {
    use std::fmt::Write;
    let mut body = String::new();
    for pc in 0..40 {
        writeln!(&mut body, "// HBC function 0, PC {pc}\nr[{pc}] = {pc};").unwrap();
    }
    let args = (0..40)
        .map(|i| format!("r[{i}]"))
        .collect::<Vec<_>>()
        .join(",");
    writeln!(
        &mut body,
        "// HBC function 0, PC 40\nr[99].slots[1] = [{args}];"
    )
    .unwrap();
    let deep = analyze(&body, "all", &[], 8);
    let operand = &deep["sites"][0]["operands"][2];
    assert_eq!(operand["nodes"].as_array().unwrap().len(), 32);
    assert_eq!(operand["node_truncated"], true);
    assert_eq!(operand["edges_truncated"], true);
    assert_eq!(operand["edge_count"], 40);
    let shallow = analyze(&body, "all", &[], 0);
    assert_eq!(shallow["sites"][0]["operands"][2]["depth_truncated"], true);
    assert_eq!(shallow["sites"][0]["operands"][2]["nodes"], json!([]));
    let args = (0..150)
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let large = analyze(
        &format!("// HBC function 0, PC 0\nr[0] = construct(r[1], r[2], [{args}]);"),
        "all",
        &[],
        0,
    );
    assert_eq!(large["sites"][0]["operand_count"], 152);
    assert_eq!(large["sites"][0]["operands_truncated"], true);
    assert_eq!(large["sites"][0]["operands"].as_array().unwrap().len(), 128);
}

#[test]
fn invalid_bounds_markers_parse_and_budgets_fail() {
    let inputs = [(0, source(CONSTRUCTOR), BTreeSet::new())];
    for (kind, slots, depth, limit, bytes) in [
        ("bad", vec![], 0, 1, 10000),
        ("constructor", vec![1], 0, 1, 10000),
        ("all", vec![], 9, 1, 10000),
        ("all", vec![], 0, 0, 10000),
        ("all", vec![], 0, 1001, 10000),
        ("all", vec![], 0, 1, 0),
        ("all", vec![], 0, 1, 16_777_217),
        ("all", vec![], 0, 1, 1),
    ] {
        assert!(sites::catalog_sources(&inputs, kind, &slots, depth, limit, 0, bytes).is_err());
    }
    for body in [
        "// HBC function 0, PC bad\nown(r[0], 1, 2);",
        "// HBC function 1, PC 0\nown(r[0], 1, 2);",
        "// HBC function 0, PC 0\nown(r[0], 1, 2);\n// HBC function 0, PC 0\nown(r[0], 1, 2);",
        "// HBC function 0, PC 0\nconstruct(r[0]);",
        "own(r[0], 1, 2);",
        "// HBC function 0, PC 0\nr[0] = ;",
    ] {
        assert!(sites::catalog_sources(
            &[(0, source(body), BTreeSet::new())],
            "all",
            &[],
            0,
            10,
            0,
            100000
        )
        .is_err());
    }
}

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/simple_arithmetic.hbc"
    ))
}

#[test]
fn authored_hbc_complete_exporter_and_unknown_function() {
    let bytes = sites::report(fixture(), &[0], "all", &[], 2, 10, 0, 100000).unwrap();
    let result: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(result["source"], "export_function_fragments");
    assert_eq!(result["parsed_source_complete"], true);
    assert!(result.get("source_complete").is_none());
    assert_eq!(
        bytes,
        sites::report(fixture(), &[0, 0], "all", &[], 2, 10, 0, 100000).unwrap()
    );
    assert!(sites::report(fixture(), &[u32::MAX], "all", &[], 2, 10, 0, 100000).is_err());
    assert!(sites::report(fixture(), &[], "all", &[], 2, 10, 0, 100000).is_err());
    assert!(sites::catalog_sources(&[], "all", &[], 2, 10, 0, 100000).is_err());
}

#[test]
fn typed_top_level_exporter_wrappers_are_not_sites() {
    let fragment = "M[0] = ['fake r[7]', 2, 0];\nF[0] = function function_0(env, self, args) {\nconst r=[]; switch(0) {case 0: {\n// HBC function 0, PC 0\nr[0] = 3;\n// HBC function 0, PC 2\nr[1].slots[1] = r[0];\n} default: throw 0;}\n};";
    let bytes = sites::catalog_sources(
        &[(0, fragment.to_owned(), BTreeSet::new())],
        "all",
        &[],
        1,
        20,
        0,
        100000,
    )
    .unwrap();
    let value: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(value["total"], 1);
    assert_eq!(value["sites"][0]["pc"], 2);
    assert_eq!(value["sites"][0]["operands"][2]["edges"][0]["source_pc"], 0);
}

#[test]
fn wired_cli_json_filter_paging_and_atomic_errors() {
    use assert_cmd::Command;
    let input = concat!(env!("CARGO_MANIFEST_DIR"), "/data/constructor_test.hbc");
    let stdout = Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args([
            "sites",
            input,
            "0,1,2,3",
            "--kind",
            "constructor",
            "--limit",
            "1",
            "--max-bytes",
            "1000000",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let result: Value = serde_json::from_slice(&stdout).unwrap();
    assert_eq!(result["source"], "export_function_fragments");
    assert!(result["total"].as_u64().unwrap() > 0);
    assert_eq!(result["sites"][0]["kind"], "constructor");
    assert_eq!(result["sites"][0]["function_id"], 1);
    assert!(result["constructor_scope"]
        .as_str()
        .unwrap()
        .contains("intrinsic new allocations are not counted"));
    let filtered = Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args(["sites", input, "0", "--kind", "all", "--slot", "4294967295"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let filtered: Value = serde_json::from_slice(&filtered).unwrap();
    assert_eq!(filtered["total"], 0);
    assert!(filtered["slot_filter_policy"]
        .as_str()
        .unwrap()
        .contains("excludes all non-slot"));
    for args in [
        vec!["4294967295"],
        vec!["0", "--max-bytes", "1"],
        vec!["0", "--depth", "9"],
        vec!["0", "--limit", "0"],
        vec!["0", "--kind", "constructor", "--slot", "1"],
        vec![],
    ] {
        let output = Command::cargo_bin("hermes-dec-rs")
            .unwrap()
            .args(["sites", input])
            .args(args)
            .assert()
            .failure();
        assert!(output.get_output().stdout.is_empty());
        assert!(!output.get_output().stderr.is_empty());
    }
}

#[test]
fn sites_stdout_child() {
    let Ok(mode) = std::env::var("AGENT_SITES_STDOUT_CHILD") else {
        return;
    };
    if mode == "budget" {
        assert!(sites::run(fixture(), &[0], "all", &[], 0, 1, 0, 1).is_err());
    }
    if mode == "unknown" {
        assert!(sites::run(fixture(), &[u32::MAX], "all", &[], 0, 1, 0, 100000).is_err());
    }
    if mode == "bounds" {
        assert!(sites::run(fixture(), &[0], "all", &[], 9, 1, 0, 100000).is_err());
    }
    if mode == "compact-budget" {
        assert!(sites::run_compact(fixture(), &[0], "all", &[], 0, 1, 0, 1).is_err());
    }
    if mode == "compact-bounds" {
        assert!(sites::run_compact(fixture(), &[0], "all", &[], 9, 1, 0, 100000).is_err());
    }
    std::process::exit(0);
}

#[test]
fn errors_write_no_stdout_before_parent_cli_wiring() {
    let run = |mode| {
        std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "sites_stdout_child", "--nocapture", "--quiet"])
            .env("AGENT_SITES_STDOUT_CHILD", mode)
            .output()
            .unwrap()
    };
    let baseline = run("baseline");
    assert!(baseline.status.success());
    for mode in [
        "budget",
        "unknown",
        "bounds",
        "compact-budget",
        "compact-bounds",
    ] {
        let output = run(mode);
        assert!(output.status.success());
        assert_eq!(output.stdout, baseline.stdout);
    }
}

#[test]
fn authored_thirty_thousand_pc_initializer_is_indexed_once() {
    use std::fmt::Write;
    let mut body = String::new();
    for pc in 0..30_000 {
        writeln!(
            &mut body,
            "// HBC function 0, PC {pc}\nr[0] = r[0] + 1; r[2].slots[1] = r[0];"
        )
        .unwrap();
    }
    let bytes = sites::catalog_sources(
        &[(0, source(&body), BTreeSet::new())],
        "all",
        &[],
        8,
        1,
        29_999,
        100000,
    )
    .unwrap();
    let result: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(result["total"], 30_000);
    assert_eq!(result["sites"][0]["pc"], 29_999);
    assert_eq!(result["next_offset"], Value::Null);
    assert_eq!(
        result["sites"][0]["operands"][2]["edges"][0]["status"],
        "unresolved_intra_pc_write"
    );
}

fn expand_compact(mut report: Value) -> Value {
    let definitions = report
        .as_object_mut()
        .unwrap()
        .remove("definitions")
        .unwrap();
    assert_eq!(
        report.as_object_mut().unwrap().remove("format").unwrap(),
        "compact"
    );
    let expand_edges = |edges: &mut Value, function: &Value| {
        for edge in edges.as_array_mut().unwrap() {
            let id = edge.as_object_mut().unwrap().remove("definition").unwrap();
            if id.is_null() {
                for field in ["source_pc", "source_span", "source"] {
                    edge[field] = Value::Null;
                }
            } else {
                let definition = &definitions[id.as_str().unwrap()];
                assert_eq!(&definition["function_id"], function);
                assert_eq!(definition["defines_register"], edge["register"]);
                for field in ["source_pc", "source_span", "source"] {
                    edge[field] = definition[field].clone();
                }
            }
        }
    };
    for site in report["sites"].as_array_mut().unwrap() {
        let function = site["function_id"].clone();
        for operand in site["operands"].as_array_mut().unwrap() {
            expand_edges(&mut operand["edges"], &function);
            for node in operand["nodes"].as_array_mut().unwrap() {
                let id = node.as_object_mut().unwrap().remove("definition").unwrap();
                let definition = &definitions[id.as_str().unwrap()];
                assert_eq!(definition["function_id"], function);
                for field in ["source_pc", "source_span", "defines_register", "source"] {
                    node[field] = definition[field].clone();
                }
                expand_edges(&mut node["edges"], &function);
            }
        }
    }
    report
}

fn compare_formats(
    sources: &[(u32, String, BTreeSet<u32>)],
    depth: usize,
    limit: usize,
    offset: usize,
) -> (Vec<u8>, Vec<u8>, Value) {
    let default =
        sites::catalog_sources(sources, "all", &[], depth, limit, offset, 16_777_216).unwrap();
    let compact =
        sites::catalog_sources_compact(sources, "all", &[], depth, limit, offset, 16_777_216)
            .unwrap();
    assert_eq!(
        compact,
        sites::catalog_sources_compact(sources, "all", &[], depth, limit, offset, 16_777_216)
            .unwrap()
    );
    let value: Value = serde_json::from_slice(&compact).unwrap();
    assert_eq!(
        expand_compact(value.clone()),
        serde_json::from_slice::<Value>(&default).unwrap()
    );
    (default, compact, value)
}

#[test]
fn compact_overlapping_dag_roundtrips_and_reduces_bytes() {
    let body = "// HBC function 0, PC 0\nr[0] = opaque();\n// HBC function 0, PC 1\nr[1] = r[0] + r[0];\n// HBC function 0, PC 2\nr[2] = r[1] + r[0];\n// HBC function 0, PC 3\nconstruct(r[2], r[1], [r[2], r[1], r[0], r[2], r[1]]);\n// HBC function 0, PC 4\nr[2].slots[1] = r[2] + r[1];";
    let sources = [(0, source(body), BTreeSet::new())];
    let (default, compact, value) = compare_formats(&sources, 8, 10, 0);
    assert_eq!(value["definitions"].as_object().unwrap().len(), 3);
    assert!(
        compact.len() * 4 < default.len() * 3,
        "default={}, compact={}",
        default.len(),
        compact.len()
    );
    // Depth zero still retains edge target sources, but no selected traversal nodes.
    let (_, _, shallow) = compare_formats(&sources, 0, 10, 0);
    assert_eq!(shallow["sites"][0]["operands"][0]["nodes"], json!([]));
    assert_eq!(shallow["sites"][0]["operands"][0]["depth_truncated"], true);
    assert!(!shallow["definitions"].as_object().unwrap().is_empty());
}

#[test]
fn compact_function_span_overwrite_and_paging_identities_do_not_collide() {
    let body = "// HBC function 0, PC 0\nr[0] = 1;\n// HBC function 0, PC 1\nr[1].slots[1] = r[0];\n// HBC function 0, PC 2\nr[0] = 1;\n// HBC function 0, PC 3\nr[1].slots[1] = r[0];\n} case 4: {\n// HBC function 0, PC 4\nr[1].slots[1] = r[0];";
    let sources = [
        (
            1,
            source(body).replace("function 0,", "function 1,"),
            BTreeSet::new(),
        ),
        (0, source(body), BTreeSet::new()),
    ];
    let (_, _, full) = compare_formats(&sources, 3, 10, 0);
    let defs = full["definitions"].as_object().unwrap();
    assert_eq!(defs.len(), 4);
    let spans: BTreeSet<_> = defs
        .values()
        .map(|d| d["source_span"].to_string())
        .collect();
    assert_eq!(spans.len(), 2);
    for offset in 0..=6 {
        let (_, _, page) = compare_formats(&sources, 3, 1, offset);
        assert_eq!(page["total"], 6);
        assert!(page["definitions"].as_object().unwrap().len() <= 1);
    }
    compare_formats(&sources, 3, 1, usize::MAX);
}

#[test]
fn compact_preserves_all_truncation_metadata_and_budget_failure() {
    use std::fmt::Write;
    let mut body = format!("// HBC function 0, PC 0\nr[0] = '{}';\n", "x".repeat(1500));
    for pc in 1..40 {
        writeln!(body, "// HBC function 0, PC {pc}\nr[{pc}] = r[0];").unwrap();
    }
    let reads = (0..40)
        .map(|r| format!("r[{r}]"))
        .collect::<Vec<_>>()
        .join(",");
    writeln!(body, "// HBC function 0, PC 40\nr[40] = [{reads}];").unwrap();
    let args = std::iter::repeat_n("r[40]", 150)
        .collect::<Vec<_>>()
        .join(",");
    writeln!(body, "// HBC function 0, PC 41\nconstruct(r[40], r[0], [{args}]);\n// HBC function 0, PC 42\nr[9].slots[1] = [{reads}];").unwrap();
    let sources = [(0, source(&body), BTreeSet::new())];
    for depth in [0, 1, 8] {
        let (_, _, value) = compare_formats(&sources, depth, 10, 0);
        assert_eq!(value["sites"][0]["operands_truncated"], true);
        assert_eq!(value["sites"][1]["operands"][2]["edges_truncated"], true);
    }
    assert!(sites::catalog_sources_compact(&sources, "all", &[], 8, 10, 0, 1).is_err());
    assert!(sites::catalog_sources_compact(&sources, "all", &[], 9, 10, 0, 100000).is_err());
    assert!(sites::catalog_sources_compact(&[], "all", &[], 0, 10, 0, 100000).is_err());
}

#[test]
fn apply_sites_preserve_numeric_value_offset_order_and_prior_overwrites() {
    let body = "// HBC function 0, PC 0\nr[0] = oldObject;\n// HBC function 0, PC 1\nr[1] = oldMethod;\n// HBC function 0, PC 2\nr[0] = buffer;\n// HBC function 0, PC 3\nr[1] = r[0].writeUInt16LE;\n// HBC function 0, PC 4\nr[2] = 513;\n// HBC function 0, PC 5\nr[3] = 7;\n// HBC function 0, PC 6\nr[4] = apply(r[1], r[0], [r[2], r[3]]);\n// HBC function 0, PC 7\napply(r[1], r[0], [1025, 9]);";
    let result = analyze(body, "call", &[], 3);
    assert_eq!(result["total"], 2);
    assert!(result["call_scope"]
        .as_str()
        .unwrap()
        .contains("apply-helper"));
    let operands = &result["sites"][0]["operands"];
    assert_eq!(operands[0]["role"], "callee");
    assert_eq!(operands[1]["role"], "receiver");
    assert!(operands[0]["argument_index"].is_null());
    assert!(operands[1]["argument_index"].is_null());
    assert_eq!(operands[0]["edges"][0]["source_pc"], 3);
    assert_eq!(operands[1]["edges"][0]["source_pc"], 2);
    for (i, register, pc) in [(0, 2, 4), (1, 3, 5)] {
        let operand = &operands[i + 2];
        assert_eq!(operand["role"], "user_argument");
        assert_eq!(operand["argument_index"], i);
        assert_eq!(operand["edges"][0]["register"], register);
        assert_eq!(operand["edges"][0]["source_pc"], pc);
    }
    assert_eq!(
        operands[2]["nodes"][0]["source"]["javascript"],
        "r[2] = 513"
    );
    assert_eq!(operands[3]["nodes"][0]["source"]["javascript"], "r[3] = 7");
    let literals = &result["sites"][1]["operands"];
    assert_eq!(literals[2]["expression"]["javascript"], "1025");
    assert_eq!(literals[3]["expression"]["javascript"], "9");
    assert_eq!(literals[2]["edges"], json!([]));
    let sources = [(0, source(body), BTreeSet::new())];
    compare_formats(&sources, 3, 10, 0);
    let compact =
        sites::catalog_sources_compact(&sources, "call", &[], 3, 1000, 0, 16_777_216).unwrap();
    assert_eq!(
        expand_compact(serde_json::from_slice(&compact).unwrap()),
        result
    );
}

#[test]
fn apply_scope_excludes_arbitrary_calls_and_preserves_constructor_roles() {
    let body = "// HBC function 0, PC 0\nnew Uint8Array(2); buffer.writeUInt16LE(513, 7); fn.call(buffer, 513, 7); fn.apply(buffer, [513, 7]); other(r[0], r[1], [2]); construct(r[0], r[1], [2]);\n// HBC function 0, PC 1\napply(r[0], r[1], []);";
    let calls = analyze(body, "call", &[], 1);
    assert_eq!(calls["total"], 1);
    assert_eq!(calls["sites"][0]["operands"].as_array().unwrap().len(), 2);
    let constructors = analyze(body, "constructor", &[], 1);
    assert_eq!(constructors["total"], 1);
    assert_eq!(
        constructors["sites"][0]["operands"][1]["role"],
        "preallocated_receiver"
    );
    assert!(constructors.get("call_scope").is_none());
    compare_formats(&[(0, source(body), BTreeSet::new())], 1, 10, 0);
    let empty = analyze("// HBC function 0, PC 0\nother();", "call", &[], 0);
    assert_eq!(empty["total"], 0);
    assert!(empty["call_scope"].is_string());
}

#[test]
fn malformed_apply_shapes_spreads_and_holes_fail_both_formats() {
    for call in [
        "apply(r[0]);",
        "apply(r[0], r[1], r[2]);",
        "apply(r[0], r[1], [] , 4);",
        "apply(...r[0], r[1], []);",
        "apply(r[0], ...r[1], []);",
        "apply(r[0], r[1], [...r[2]]);",
        "apply(r[0], r[1], [,]);",
        "apply(r[0], r[1], [1,,2]);",
        "apply?.(r[0], r[1], []);",
    ] {
        let sources = [(
            0,
            source(&format!("// HBC function 0, PC 0\n{call}")),
            BTreeSet::new(),
        )];
        for catalog in [sites::catalog_sources, sites::catalog_sources_compact] {
            let error = catalog(&sources, "call", &[], 1, 10, 0, 100000).unwrap_err();
            assert!(
                error.to_string().contains("apply helper"),
                "{call}: {error}"
            );
        }
    }
}
