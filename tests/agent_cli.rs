use serde_json::Value;
use std::path::PathBuf;
use std::process::{Command, Output};

fn cli(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
        .args(args)
        .output()
        .unwrap()
}

fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/bundle_semantics.hbc")
}

fn json(args: &[&str]) -> Value {
    let output = cli(args);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap()
}

#[test]
fn workspace_summary_points_to_generated_navigation_without_running_js() {
    let temp = tempfile::tempdir().unwrap();
    let input = temp.path().join("input 'quoted' $name.hbc");
    std::fs::copy(fixture(), &input).unwrap();
    let output = temp.path().join("output 'quoted' $name");
    let summary = json(&[
        "workspace",
        input.to_str().unwrap(),
        "-o",
        output.to_str().unwrap(),
    ]);
    assert_eq!(summary["workspace"], output.to_str().unwrap());
    assert_eq!(summary["guide"], "GUIDE.md");
    assert!(output.join(summary["guide"].as_str().unwrap()).is_file());
    assert!(output.join(summary["manifest"].as_str().unwrap()).is_file());
    let guide = std::fs::read_to_string(output.join("GUIDE.md")).unwrap();
    assert!(!guide.contains("input 'quoted' $name"));
    assert!(!guide.contains("output 'quoted' $name"));
    assert!(guide.contains("--expressions"));
    let manifest: Value =
        serde_json::from_slice(&std::fs::read(output.join("manifest.json")).unwrap()).unwrap();
    let fragment = std::fs::read(output.join("f0.js")).unwrap();
    let prefix = manifest["functions"][0]["fragment_prefix_bytes"]
        .as_u64()
        .unwrap() as usize;
    let origins = json(&[
        "origins",
        input.to_str().unwrap(),
        "0",
        "0",
        "--expressions",
    ]);
    let raw_prefix = origins["expression_source"]["raw_fragment_prefix"]
        .as_str()
        .unwrap();
    assert!(fragment[prefix..].starts_with(raw_prefix.as_bytes()));
    for expression in origins["instruction_expressions"].as_array().unwrap() {
        for node in expression["nodes"].as_array().unwrap() {
            let start = node["source"]["start"].as_u64().unwrap() as usize;
            let end = node["source"]["end"].as_u64().unwrap() as usize;
            let raw = &fragment[prefix + start..prefix + end];
            assert!(std::str::from_utf8(raw).is_ok());
            assert!(raw.starts_with(node["source"]["preview"].as_str().unwrap().as_bytes()));
            assert_eq!(node["source"]["original_bytes"], raw.len());
        }
    }
}

#[test]
fn source_properties_are_wired_with_continuation_and_atomic_failures() {
    let input = fixture();
    let path = input.to_str().unwrap();
    let before = std::fs::read(&input).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&before).unwrap();
    let function = (0..hbc.functions.count())
        .find(|&id| {
            let bytes =
                hermes_dec_rs::cli::properties::report(&input, id, &Default::default()).unwrap();
            let report: Value = serde_json::from_slice(&bytes).unwrap();
            report["stores_total"].as_u64().unwrap() > 1
        })
        .expect("fixture must exercise multiple property stores")
        .to_string();
    let first = json(&[
        "properties",
        path,
        &function,
        "--limit",
        "1",
        "--scan-work",
        "128",
    ]);
    assert_eq!(first["schema"], "properties-v1");
    assert_eq!(
        first,
        json(&[
            "properties",
            path,
            &function,
            "--limit",
            "1",
            "--scan-work",
            "128"
        ])
    );
    assert!(first["next_offset"].is_number());
    let mut continued = vec!["properties", path, &function];
    continued.extend(
        first["continuation_query"]["flags"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap()),
    );
    let next = json(&continued);
    assert_eq!(next["offset"], first["next_offset"]);
    assert_eq!(next["query_work_cap"], 128);
    assert_eq!(next["limit"], 1);
    let absent = json(&[
        "properties",
        path,
        &function,
        "--match",
        "[literal];$(not-executed)",
    ]);
    assert!(absent["rows"].as_array().unwrap().is_empty());
    assert_eq!(
        absent["matches"],
        serde_json::json!(["[literal];$(not-executed)"])
    );
    for tail in [
        vec!["--max-bytes", "1"],
        vec!["--scan-work", "0"],
        vec!["--definition-limit", "0"],
    ] {
        let mut args = vec!["properties", path, &function];
        args.extend(tail);
        let output = cli(&args);
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
    }
    let help = cli(&["properties", "--help"]);
    assert!(help.status.success());
    assert!(String::from_utf8(help.stdout)
        .unwrap()
        .contains("--definition-limit"));
    assert_eq!(std::fs::read(&input).unwrap(), before);
}

#[test]
fn source_symbols_and_text_provenance_are_wired_with_atomic_budgets() {
    let input = fixture();
    let path = input.to_str().unwrap();
    let symbols = json(&["symbols", path, "0", "--limit", "2"]);
    assert_eq!(symbols["schema"], "symbols-v1");
    assert_eq!(symbols, json(&["symbols", path, "0", "--limit", "2"]));
    assert_eq!(symbols["expression_source"]["offset_unit"], "utf8_bytes");
    assert_eq!(symbols["query_work_cap"], 1_048_576);
    let data = std::fs::read(&input).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&data).unwrap();
    let function = (0..hbc.functions.count())
        .find(|&id| {
            let bytes =
                hermes_dec_rs::cli::symbols::report(&input, id, &Default::default()).unwrap();
            let value: Value = serde_json::from_slice(&bytes).unwrap();
            value["stores_total"].as_u64().unwrap() > 1
        })
        .expect("fixture must have a function with multiple slot stores")
        .to_string();
    let empty = json(&[
        "symbols",
        path,
        &function,
        "--match",
        "not-a-present-literal;$(no-eval)",
        "--scan-work",
        "1",
    ]);
    assert!(empty["rows"].as_array().unwrap().is_empty());
    assert_eq!(empty["query_work_cap"], 1);
    assert_eq!(empty["scan_complete"], true);
    assert_eq!(empty["query_work_used"], 0);
    assert_eq!(empty["filter_literal_diagnostics"]["complete"], true);
    let page = json(&[
        "symbols",
        path,
        &function,
        "--scan-work",
        "1",
        "--limit",
        "1",
    ]);
    assert!(page["next_offset"].is_number());
    let mut continued = vec!["symbols", path, &function];
    continued.extend(
        page["continuation_query"]["flags"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap()),
    );
    let next = json(&continued);
    assert_eq!(next["offset"], page["next_offset"]);
    assert_eq!(next["matches"], page["matches"]);
    assert_eq!(next["query_work_cap"], 1);
    for work in ["0", "16777217"] {
        let invalid = cli(&[
            "symbols",
            "/private/tmp/missing-scan-input.hbc",
            "0",
            "--scan-work",
            work,
        ]);
        assert!(!invalid.status.success());
        assert!(invalid.stdout.is_empty());
        assert!(String::from_utf8_lossy(&invalid.stderr).contains("scan_work"));
    }
    for command in ["symbols", "origins"] {
        let args = if command == "symbols" {
            vec![command, path, "0", "--max-bytes", "1"]
        } else {
            vec![command, path, "0", "0", "--text", "--max-bytes", "1"]
        };
        let output = cli(&args);
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
    }
    for expressions in [false, true] {
        let mut args = vec!["origins", path, "0", "0", "--text"];
        if expressions {
            args.push("--expressions");
        }
        let output = cli(&args);
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let text = String::from_utf8(output.stdout).unwrap();
        let mut first = None;
        for line in text.lines() {
            let (kind, payload) = line.split_once(' ').unwrap();
            let value: Value = serde_json::from_str(payload).unwrap();
            if kind == "origins" {
                first = Some(value);
            }
        }
        let first = first.unwrap();
        assert_eq!(first["source_schema"], "origins-v1");
        assert!(first["expression_policy"]
            .as_str()
            .unwrap()
            .contains("omitted"));
    }
}

#[test]
fn search_literal_buffers_names_and_property_references() {
    let input = fixture();
    let input = input.to_str().unwrap();
    for query in [
        "exhaustion",
        "generator cleanup",
        "bundleResults",
        "iteratorScenario",
    ] {
        let result = json(&["search", input, query, "--json"]);
        assert_eq!(result["schema_version"], 1);
        assert!(result["total"].as_u64().unwrap() > 0, "{query}");
        assert!(result["functions"][0]["function_id"].is_number());
        if query != "iteratorScenario" {
            assert!(result["functions"]
                .as_array()
                .unwrap()
                .iter()
                .any(|f| !f["evidence"].as_array().unwrap().is_empty()));
        }
    }
}

#[test]
fn bounded_search_is_deterministic_and_paginates() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let first = json(&["search", input, "return", "--limit", "1", "--json"]);
    assert_eq!(
        first,
        json(&["search", input, "return", "--limit", "1", "--json"])
    );
    assert_eq!(first["functions"].as_array().unwrap().len(), 1);
    assert_eq!(first["next_offset"], 1);
    let second = json(&[
        "search", input, "return", "--limit", "1", "--offset", "1", "--json",
    ]);
    assert_ne!(
        first["functions"][0]["function_id"],
        second["functions"][0]["function_id"]
    );
    assert_eq!(
        json(&["search", input, "not-a-real-symbol-987", "--json"])["total"],
        0
    );
}

#[test]
fn conjunctive_search_matches_intersection_of_full_navigation_results() {
    use std::collections::BTreeSet;
    let input = fixture();
    let input = input.to_str().unwrap();
    let ids = |value: &Value| -> BTreeSet<u64> {
        value["functions"]
            .as_array()
            .unwrap()
            .iter()
            .map(|function| function["function_id"].as_u64().unwrap())
            .collect()
    };
    for (left, right) in [
        ("generator", "cleanup"),
        ("iteratorScenario", "next"),
        ("exhaustion", "exhaustion"),
        ("return", "missing-symbol-987"),
    ] {
        let a = json(&["search", input, left, "--limit", "1000", "--json"]);
        let b = json(&["search", input, right, "--limit", "1000", "--json"]);
        let both = json(&[
            "search", input, left, right, "--all", "--limit", "1000", "--json",
        ]);
        let intersection: BTreeSet<_> = ids(&a).intersection(&ids(&b)).copied().collect();
        assert_eq!(ids(&both), intersection, "{left} / {right}");
        assert_eq!(both["query_mode"], "all_in_function");
        assert_eq!(a["query_mode"], "any_in_function");
        if left == "exhaustion" {
            assert!(!intersection.is_empty());
        }
    }
}

#[test]
fn conjunctive_search_keeps_paging_regex_and_word_contracts() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let result = json(&[
        "search", input, "return", "return", "--all", "--limit", "1", "--json",
    ]);
    assert!(result["total"].as_u64().unwrap() > 1);
    assert_eq!(result["next_offset"], 1);
    assert_eq!(
        result,
        json(&["search", input, "return", "return", "--all", "--limit", "1", "--json",])
    );
    let regex = json(&[
        "search",
        input,
        "generator",
        "cleanup",
        "--all",
        "--regex",
        "--word",
        "--json",
    ]);
    assert!(regex["total"].as_u64().unwrap() > 0);
    let invalid = cli(&["search", input, "generator", "[", "--all", "--regex"]);
    assert!(!invalid.status.success());
    assert!(invalid.stdout.is_empty());
}

#[test]
fn compact_call_catalog_is_wired_without_changing_argument_order() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let result = json(&[
        "sites",
        input,
        "0",
        "--kind",
        "call",
        "--compact",
        "--depth",
        "1",
    ]);
    assert_eq!(result["format"], "compact");
    assert!(result["definitions"].is_object());
    assert!(result["total"].as_u64().unwrap() > 0);
    let operands = result["sites"][0]["operands"].as_array().unwrap();
    assert_eq!(operands[0]["role"], "callee");
    assert_eq!(operands[1]["role"], "receiver");
    for (index, argument) in operands.iter().skip(2).enumerate() {
        assert_eq!(argument["role"], "user_argument");
        assert_eq!(argument["argument_index"], index);
    }
    let rejected = cli(&[
        "sites",
        input,
        "0",
        "--kind",
        "call",
        "--compact",
        "--max-bytes",
        "1",
    ]);
    assert!(!rejected.status.success());
    assert!(rejected.stdout.is_empty());
}

#[test]
fn site_source_and_pc_filters_are_wired_and_atomic() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let result = json(&[
        "sites",
        input,
        "0",
        "--kind",
        "call",
        "--compact",
        "--match",
        "r[",
        "--from-pc",
        "0",
        "--to-pc",
        "4294967295",
        "--limit",
        "1",
    ]);
    assert_eq!(result["sites"].as_array().unwrap().len(), 1);
    assert_eq!(result["filter"]["matches"], serde_json::json!(["r["]));
    assert_eq!(result["filter"]["runtime_match_complete"], false);
    assert!(!result["sites"][0]["source_matches"]["evidence"]
        .as_array()
        .unwrap()
        .is_empty());
    for args in [
        vec!["sites", input, "0", "--match", ""],
        vec!["sites", input, "0", "--from-pc", "2", "--to-pc", "1"],
        vec!["sites", input, "0", "--match", "r[", "--max-bytes", "1"],
    ] {
        let rejected = cli(&args);
        assert!(!rejected.status.success());
        assert!(rejected.stdout.is_empty());
    }
}

#[test]
fn batch_capture_navigation_is_wired_with_explicit_uncertainty() {
    let input = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/closure_capture_test.hbc");
    let input = input.to_str().unwrap();
    let search = json(&["search", input, "deeplyNested", "--json"]);
    let id = search["functions"][0]["function_id"]
        .as_u64()
        .unwrap()
        .to_string();
    let result = json(&["captures", input, &id, "--depth", "3", "--limit", "1"]);
    assert_eq!(result["authoritative_lexical_resolution"], false);
    assert_eq!(result["returned"], 1);
    assert_eq!(result["reads"][0]["unresolved"], true);
    assert!(result["reads"][0]["candidate_total"].as_u64().unwrap() > 0);
    assert!(result["reads"][0]["excerpt"]["javascript"].is_string());
    let rejected = cli(&["captures", input, &id, "--max-bytes", "1"]);
    assert!(!rejected.status.success());
    assert!(rejected.stdout.is_empty());
}

#[test]
fn errors_do_not_contaminate_stdout_or_overwrite_output() {
    let input = fixture();
    let input = input.to_str().unwrap();
    for args in [
        vec!["search", input, "[", "--regex"],
        vec!["show", input, "999999"],
        vec!["refs", input, "0", "--depth", "0"],
    ] {
        let out = cli(&args);
        assert!(!out.status.success());
        assert!(out.stdout.is_empty());
        assert!(!out.stderr.is_empty());
    }
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("answer.js");
    std::fs::write(&file, "keep").unwrap();
    let out = cli(&[
        "show",
        input,
        "0",
        "--max-bytes",
        "1",
        "-o",
        file.to_str().unwrap(),
    ]);
    assert!(!out.status.success());
    assert!(out.stdout.is_empty());
    assert_eq!(std::fs::read_to_string(file).unwrap(), "keep");
}

#[test]
fn batch_js_has_original_function_ids_and_pcs_and_is_syntactically_valid() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let output = cli(&["show", input, "0,1", "--max-bytes", "1000000"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let js = String::from_utf8(output.stdout).unwrap();
    assert!(js.contains("F[0] = function function_0"));
    assert!(js.contains("F[1] = function function_1"));
    assert!(js.contains("// HBC function 0, PC 0"));
    assert!(js.contains("not standalone"));
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("bodies.js");
    std::fs::write(&path, js).unwrap();
    assert!(Command::new("node")
        .arg("--check")
        .arg(path)
        .status()
        .unwrap()
        .success());
    let result = json(&["show", input, "0,1", "--json", "--max-bytes", "1000000"]);
    assert_eq!(result["standalone"], false);
    assert_eq!(result["functions"].as_array().unwrap().len(), 2);
}

#[test]
fn closure_edges_can_be_followed_back_to_the_creator() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let outgoing = json(&["refs", input, "0", "--direction", "out"]);
    let edge = &outgoing["edges"][0];
    assert_eq!(edge["from"], 0);
    assert_eq!(edge["kind"], "closure");
    assert!(edge["pc"].is_number());
    let child = edge["to"].as_u64().unwrap().to_string();
    let incoming = json(&["refs", input, &child, "--direction", "in"]);
    assert!(incoming["edges"].as_array().unwrap().contains(edge));
}

#[test]
fn js_excerpt_is_bounded_and_explicitly_incomplete() {
    let input = fixture();
    let input = input.to_str().unwrap();
    let result = json(&["show", input, "0", "--around-pc", "0", "--context", "0"]);
    assert_eq!(result["first_pc"], 0);
    assert_eq!(result["last_pc"], 0);
    assert_eq!(result["complete_function"], false);
    assert!(result["next_pc"].is_number());
    assert!(result["javascript_excerpt"]
        .as_str()
        .unwrap()
        .starts_with("// HBC function 0, PC 0"));
    let out = cli(&["show", input, "0", "--around-pc", "1"]);
    assert!(!out.status.success());
    assert!(out.stdout.is_empty());
    let out = cli(&["show", input, "0,1", "--around-pc", "0"]);
    assert!(!out.status.success());
    assert!(out.stdout.is_empty());
    let larger = json(&[
        "show",
        input,
        "1",
        "--around-pc",
        "0",
        "--context",
        "250",
        "--max-bytes",
        "1000000",
    ]);
    assert_eq!(larger["requested_pc"], 0);
    let out = cli(&["show", input, "0", "--around-pc", "0", "--context", "1001"]);
    assert!(!out.status.success());
    assert!(out.stdout.is_empty());
}

#[test]
fn authored_hbc_literal_marker_does_not_shift_pc_excerpts() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/pc_marker_collision.hbc");
    let data = std::fs::read(&path).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&data).unwrap();
    let function = (0..hbc.functions.count())
        .find(|&id| hbc.functions.get_function_name(id, &hbc.strings).unwrap() == "markerCollision")
        .unwrap();
    assert_eq!(function, 1);
    let instructions = hbc.functions.get_instructions_ref(function).unwrap();
    let ret = instructions
        .iter()
        .find(|ins| ins.instruction.name() == "Ret")
        .unwrap()
        .offset
        .value();
    let result = json(&[
        "show",
        path.to_str().unwrap(),
        &function.to_string(),
        "--around-pc",
        &ret.to_string(),
        "--context",
        "0",
    ]);
    assert_eq!(result["first_pc"], ret);
    assert_eq!(result["last_pc"], ret);
    let code = result["javascript_excerpt"].as_str().unwrap();
    assert!(code.contains("return r[1]"));
    assert!(!code.contains("PC 999"));
    let literal = instructions
        .iter()
        .find(|ins| ins.instruction.name() == "LoadConstString")
        .unwrap()
        .offset
        .value();
    let result = json(&[
        "show",
        path.to_str().unwrap(),
        "1",
        "--around-pc",
        &literal.to_string(),
        "--context",
        "0",
    ]);
    assert!(result["javascript_excerpt"]
        .as_str()
        .unwrap()
        .contains("PC 999"));
    assert_eq!(result["next_pc"], ret);
}

#[test]
fn assignment_names_navigate_to_opaque_closure_bodies() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/closure_capture_test.hbc");
    let result = json(&["search", path.to_str().unwrap(), "makeCounter", "--json"]);
    let functions = result["functions"].as_array().unwrap();
    assert!(functions.iter().any(|f| f["assignments"]
        .as_array()
        .unwrap()
        .iter()
        .any(|a| a["name"] == "makeCounter")));
    assert!(functions[0]["matching_assignments"].as_u64().unwrap() > 0);
    let id = functions[0]["function_id"].as_u64().unwrap().to_string();
    let shown = json(&["show", path.to_str().unwrap(), &id, "--json"]);
    assert!(!shown["functions"][0]["static_assignments"]
        .as_array()
        .unwrap()
        .is_empty());
    let text = cli(&["show", path.to_str().unwrap(), &id]);
    assert!(text.status.success());
    assert!(String::from_utf8(text.stdout)
        .unwrap()
        .contains("Static assignment candidates"));
    let by_word = json(&[
        "search",
        path.to_str().unwrap(),
        "Counter",
        "--word",
        "--json",
    ]);
    assert_eq!(by_word["total"], 0);
    assert!(
        json(&["search", path.to_str().unwrap(), "Counter", "--json"])["total"]
            .as_u64()
            .unwrap()
            > 0
    );
}

#[test]
fn inspection_defaults_to_bounded_machine_summary() {
    let path = fixture();
    let input = path.to_str().unwrap();
    let output = cli(&["inspect", input]);
    assert!(output.status.success());
    assert!(output.stdout.len() < 1024);
    let summary: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(summary["schema_version"], 1);
    assert_eq!(summary["hbc_version"], 96);
    assert!(summary["functions"].as_u64().unwrap() > 0);
    let bad = cli(&["inspect", "/private/tmp/no-such-agent-input.hbc"]);
    assert!(!bad.status.success());
    assert!(bad.stdout.is_empty());
}

#[test]
fn origins_cli_is_deterministic_bounded_and_read_only() {
    let path = fixture();
    let input = path.to_str().unwrap();
    let before = std::fs::read(&path).unwrap();
    let report = json(&["origins", input, "0", "0"]);
    assert_eq!(report, json(&["origins", input, "0", "0"]));
    assert_eq!(report["schema_version"], 1);
    assert_eq!(report["schema"], "origins-v1");
    assert!(report["semantics"].as_str().unwrap().contains("candidate"));
    assert_eq!(report["function"], 0);
    assert_eq!(report["pc"], 0);
    let expression_report = json(&["origins", input, "0", "0", "--expressions"]);
    assert_eq!(
        expression_report,
        json(&["origins", input, "0", "0", "--expressions"])
    );
    assert!(expression_report["instruction_expressions"].is_array());
    assert_eq!(expression_report["demands"], report["demands"]);
    let expression_overflow = cli(&[
        "origins",
        input,
        "0",
        "0",
        "--expressions",
        "--max-bytes",
        "1",
    ]);
    assert!(!expression_overflow.status.success());
    assert!(expression_overflow.stdout.is_empty());
    for extra in [
        vec!["--depth", "65"],
        vec!["--limit", "0"],
        vec!["--max-bytes", "1"],
    ] {
        let mut args = vec!["origins", input, "0", "0"];
        args.extend(extra);
        let failed = cli(&args);
        assert!(!failed.status.success());
        assert!(failed.stdout.is_empty());
    }
    let unknown = cli(&["origins", input, "0", "4294967295"]);
    assert!(!unknown.status.success());
    assert!(unknown.stdout.is_empty());
    assert_eq!(before, std::fs::read(path).unwrap());
}

#[test]
fn site_follow_up_queries_are_wired_and_budget_errors_emit_no_stdout() {
    let path = fixture();
    let input = path.to_str().unwrap();
    let before = std::fs::read(&path).unwrap();
    let report = json(&["sites", input, "0", "--compact", "--depth", "8"]);
    assert_eq!(
        report,
        json(&["sites", input, "0", "--compact", "--depth", "8"])
    );
    let queries = report["follow_up_queries"].as_array().unwrap();
    for q in queries {
        assert_eq!(q["command"], "origins");
        assert_eq!(q["input_scope"], "same_input_hbc");
        assert!(report["sites"]
            .as_array()
            .unwrap()
            .iter()
            .any(|s| s["function_id"] == q["function"] && s["pc"] == q["pc"]));
    }
    let failed = cli(&["sites", input, "0", "--compact", "--max-bytes", "1"]);
    assert!(!failed.status.success());
    assert!(failed.stdout.is_empty());
    assert_eq!(before, std::fs::read(path).unwrap());
}
