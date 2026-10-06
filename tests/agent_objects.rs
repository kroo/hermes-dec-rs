use hermes_dec_rs::cli::{objects, sites};

use hermes_dec_rs::{bundle, HbcFile};
use objects::{report, Options};
use serde_json::Value;
use std::collections::BTreeSet;
use std::path::Path;
use std::process::Command;

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/simple_object_test.hbc"
    ))
}

fn malformed_fixture() -> &'static Path {
    // Authored JavaScript is deliberately not a valid HBC input.
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/simple_object_test.js"
    ))
}

fn assert_source_join(source: &str, span: &Value, snippet: &Value) {
    let span = span.as_array().expect("two UTF-8 byte offsets");
    assert_eq!(span.len(), 2);
    let start = span[0].as_u64().unwrap() as usize;
    let end = span[1].as_u64().unwrap() as usize;
    let original = source.get(start..end).expect("valid raw fragment span");
    let text = snippet["javascript"].as_str().unwrap();
    if snippet["truncated"].as_bool() == Some(true) {
        assert!(original.starts_with(text));
    } else {
        assert_eq!(original, text);
    }
}

#[test]
fn defaults_match_the_public_contract() {
    let options = Options::default();
    assert_eq!(options.alias_depth, 16);
    assert_eq!(options.depth, 3);
    assert_eq!(options.limit, 5);
    assert_eq!(options.offset, 0);
    assert_eq!(options.max_bytes, 100_000);
    assert_eq!(options.scan_work, 2_097_152);
    assert!(options.matches.is_empty());
    // This assignment also verifies that Options is the core type, not a copy.
    let _: sites::ObjectOptions = options;
}

#[test]
fn fixture_reports_are_deterministic_read_only_and_source_spans_join() {
    let before = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&before).unwrap();
    let mut sites_seen = 0;
    let mut origins_seen = 0;
    for function in 0..hbc.functions.count() {
        let options = Options::default();
        let bytes = report(fixture(), function, None, &options).unwrap();
        assert_eq!(bytes, report(fixture(), function, None, &options).unwrap());
        assert_eq!(
            bytes,
            sites::report_objects(fixture(), function, None, &options).unwrap()
        );
        let value: Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(value["schema"], "objects-v1");
        assert_eq!(value["function"], function);
        assert!(value["origin_pc"].is_null());
        assert_eq!(value["work_cap"], options.scan_work);
        assert!(value["work_used"].as_u64().unwrap() <= options.scan_work as u64);
        let source = bundle::export_function_fragments(&hbc, &[function])
            .unwrap()
            .remove(0)
            .1;
        let definitions = value["definitions"].as_object().unwrap();
        for (id, definition) in definitions {
            assert_source_join(&source, &definition["source_span"], &definition["source"]);
            assert_eq!(
                id,
                &format!(
                    "{function}:{}:{}",
                    definition["source_span"][0], definition["source_span"][1]
                )
            );
        }
        for site in value["sites"].as_array().unwrap() {
            sites_seen += 1;
            assert_source_join(&source, &site["source_span"], &site["exact_expression"]);
            let origin = &site["object_origin"];
            assert!(origin["status"].is_string());
            assert!(origin["alias_depth_truncated"].is_boolean());
            for id in origin["alias_definition_ids"].as_array().unwrap() {
                assert!(definitions.contains_key(id.as_str().unwrap()));
            }
            if let Some(id) = origin["terminal_definition"].as_str() {
                origins_seen += 1;
                let definition = definitions.get(id).expect("shared terminal definition");
                let pc = definition["source_pc"].as_u64().unwrap() as u32;
                let anchored = report(fixture(), function, Some(pc), &options).unwrap();
                assert_eq!(
                    anchored,
                    sites::report_objects(fixture(), function, Some(pc), &options).unwrap()
                );
                let anchored: Value = serde_json::from_slice(&anchored).unwrap();
                assert_eq!(anchored["origin_pc"], pc);
                assert!(!anchored["sites"].as_array().unwrap().is_empty());
                for sibling in anchored["sites"].as_array().unwrap() {
                    let terminal = sibling["object_origin"]["terminal_definition"]
                        .as_str()
                        .unwrap();
                    assert_eq!(anchored["definitions"][terminal]["source_pc"], pc);
                }
            }
        }
    }
    assert!(sites_seen > 0, "fixture must exercise property sites");
    assert!(
        origins_seen > 0,
        "fixture must exercise local object origins"
    );
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn wrapper_preserves_options_and_exception_boundaries() {
    for input in [
        fixture(),
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/data/try_catch_test.hbc"
        )),
    ] {
        let before = std::fs::read(input).unwrap();
        let hbc = HbcFile::parse(&before).unwrap();
        for function in 0..hbc.functions.count() {
            let source = bundle::export_function_fragments(&hbc, &[function])
                .unwrap()
                .remove(0)
                .1;
            let exceptions: BTreeSet<_> = hbc
                .functions
                .get_parsed_header(function)
                .unwrap()
                .exc_handlers
                .iter()
                .flat_map(|handler| [handler.start, handler.end, handler.target])
                .collect();
            for offset in [0, 1, usize::MAX] {
                let options = Options {
                    alias_depth: 0,
                    depth: 0,
                    limit: 1,
                    offset,
                    scan_work: 16_777_216,
                    matches: vec!["raw syntax; $(not-executed)".into(), "[.*]".into()],
                    ..Options::default()
                };
                assert_eq!(
                    report(input, function, None, &options).unwrap(),
                    sites::analyze_objects_source(&source, function, None, &exceptions, &options)
                        .unwrap()
                );
            }
        }
        assert_eq!(std::fs::read(input).unwrap(), before);
    }
}

#[test]
fn unknown_function_and_noninstruction_pc_are_rejected_read_only() {
    let before = std::fs::read(fixture()).unwrap();
    let count = HbcFile::parse(&before).unwrap().functions.count();
    for function in [count, u32::MAX] {
        assert!(report(fixture(), function, None, &Options::default())
            .unwrap_err()
            .to_string()
            .contains("Unknown function"));
    }
    assert!(report(fixture(), 0, Some(u32::MAX), &Options::default()).is_err());
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn invalid_bounds_are_rejected_before_input_read() {
    let missing = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/agent_objects_nonexistent_input.hbc"
    ));
    assert!(!missing.exists());
    let mut invalid = Vec::new();
    for alias_depth in [65, usize::MAX] {
        invalid.push(Options {
            alias_depth,
            ..Options::default()
        });
    }
    for depth in [9, usize::MAX] {
        invalid.push(Options {
            depth,
            ..Options::default()
        });
    }
    for limit in [0, 1001, usize::MAX] {
        invalid.push(Options {
            limit,
            ..Options::default()
        });
    }
    for max_bytes in [0, 16_777_217, usize::MAX] {
        invalid.push(Options {
            max_bytes,
            ..Options::default()
        });
    }
    for scan_work in [0, 16_777_217, usize::MAX] {
        invalid.push(Options {
            scan_work,
            ..Options::default()
        });
    }
    for matches in [
        vec!["x".into(); 65],
        vec![String::new()],
        vec!["x".repeat(1025)],
        vec!["\u{00e9}".repeat(513)],
    ] {
        invalid.push(Options {
            matches,
            ..Options::default()
        });
    }
    for options in invalid {
        let expected = sites::analyze_objects_source("", 0, None, &BTreeSet::new(), &options)
            .unwrap_err()
            .to_string();
        for message in [
            report(missing, 0, None, &options).unwrap_err().to_string(),
            objects::run(missing, 0, None, &options)
                .unwrap_err()
                .to_string(),
        ] {
            assert_eq!(
                message, expected,
                "input read must follow options validation"
            );
        }
    }
    assert!(!missing.exists());
}

#[test]
fn inclusive_option_bounds_and_match_count_are_accepted() {
    for (alias_depth, depth) in [(0, 0), (64, 8)] {
        let options = Options {
            alias_depth,
            depth,
            limit: 1000,
            offset: usize::MAX,
            max_bytes: 16_777_216,
            scan_work: 16_777_216,
            matches: vec!["x".repeat(1024); 64],
        };
        let bytes = report(fixture(), 0, None, &options).unwrap();
        let value: Value = serde_json::from_slice(&bytes).unwrap();
        assert!(value["sites"].as_array().unwrap().is_empty());
        assert!(value["next_offset"].is_null());
    }
}

#[test]
fn serialization_budget_includes_the_entire_report() {
    let mut exact = Options {
        max_bytes: 16_777_216,
        ..Options::default()
    };
    // Query flags embed max_bytes, so find the length for the exact cap itself.
    let mut converged = None;
    for _ in 0..16 {
        let bytes = report(fixture(), 0, None, &exact).unwrap();
        if bytes.len() == exact.max_bytes {
            converged = Some(bytes);
            break;
        }
        assert!(bytes.len() < exact.max_bytes);
        exact.max_bytes = bytes.len();
    }
    let bytes = converged.expect("serialized length must converge with its own byte cap");
    assert_eq!(bytes.len(), exact.max_bytes);
    serde_json::from_slice::<Value>(&bytes).unwrap();
    for max_bytes in [1, exact.max_bytes - 1] {
        let options = Options {
            max_bytes,
            ..Options::default()
        };
        let message = report(fixture(), 0, None, &options)
            .unwrap_err()
            .to_string();
        assert!(message.contains("byte budget"), "{message}");
    }
}

#[test]
fn malformed_authored_input_is_rejected_without_writes() {
    let before = std::fs::read(malformed_fixture()).unwrap();
    assert!(report(malformed_fixture(), 0, None, &Options::default()).is_err());
    assert!(objects::run(malformed_fixture(), 0, None, &Options::default()).is_err());
    assert_eq!(std::fs::read(malformed_fixture()).unwrap(), before);
}

// A separate test process avoids stdout-capture races with the test harness.
#[test]
fn objects_emit_helper() {
    let Ok(mode) = std::env::var("AGENT_OBJECTS_EMIT_MODE") else {
        return;
    };
    let options = Options {
        max_bytes: if mode == "budget" { 1 } else { 100_000 },
        alias_depth: if mode == "invalid" { 65 } else { 16 },
        ..Options::default()
    };
    let function = if mode == "unknown" { u32::MAX } else { 0 };
    let input = if mode == "malformed" {
        malformed_fixture()
    } else {
        fixture()
    };
    let result = objects::run(input, function, None, &options);
    assert_eq!(result.is_ok(), mode == "success");
}

#[test]
fn stdout_contains_complete_report_or_nothing_on_analysis_failure() {
    for mode in ["success", "budget", "invalid", "unknown", "malformed"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "objects_emit_helper", "--nocapture"])
            .env("AGENT_OBJECTS_EMIT_MODE", mode)
            .output()
            .unwrap();
        assert!(output.status.success(), "{output:?}");
        let start = output.stdout.iter().position(|&byte| byte == b'{');
        if mode == "success" {
            let expected = report(fixture(), 0, None, &Options::default()).unwrap();
            let start = start.expect("serialized report");
            assert_eq!(
                output.stdout.get(start..start + expected.len()),
                Some(expected.as_slice())
            );
            serde_json::from_slice::<Value>(&expected).unwrap();
            assert!(!output.stdout[start + expected.len()..].contains(&b'{'));
        } else {
            assert!(start.is_none(), "unexpected partial report: {output:?}");
        }
    }
}
