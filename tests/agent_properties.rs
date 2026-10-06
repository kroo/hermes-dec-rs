use hermes_dec_rs::bundle;
use hermes_dec_rs::cli::{origins, properties};
use hermes_dec_rs::HbcFile;
use properties::{report, Options};
use serde_json::Value;
use std::path::Path;
use std::process::Command;

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

fn assert_source_join(source: &str, excerpt: &Value) {
    let start = excerpt["start"].as_u64().unwrap() as usize;
    let end = excerpt["end"].as_u64().unwrap() as usize;
    let original = source.get(start..end).expect("valid raw fragment span");
    let javascript = excerpt["javascript"].as_str().unwrap();
    if excerpt["snippet_truncated"].as_bool() != Some(true) {
        assert_eq!(original, javascript);
    }
}

fn query(function: u32, options: &Options) -> origins::PropertyOptions {
    origins::PropertyOptions {
        function,
        depth: options.depth,
        definition_limit: options.definition_limit,
        limit: options.limit,
        offset: options.offset,
        max_bytes: options.max_bytes,
        scan_work: options.scan_work,
        matches: options.matches.clone(),
    }
}

#[test]
fn defaults_match_the_public_contract() {
    let options = Options::default();
    assert_eq!(options.depth, 8);
    assert_eq!(options.definition_limit, 64);
    assert_eq!(options.limit, 5);
    assert_eq!(options.offset, 0);
    assert_eq!(options.max_bytes, 100_000);
    assert_eq!(options.scan_work, 1_048_576);
    assert_eq!(origins::MAX_PROPERTY_SCAN_WORK, 16_777_216);
    assert!(options.matches.is_empty());
}

#[test]
fn fixture_is_deterministic_read_only_and_sources_join() {
    let before = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&before).unwrap();
    let mut rows_seen = 0;
    for function in 0..hbc.functions.count() {
        let options = Options::default();
        let bytes = report(fixture(), function, &options).unwrap();
        assert_eq!(bytes, report(fixture(), function, &options).unwrap());
        let value: Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(value["schema"], "properties-v1");
        assert_eq!(value["schema_version"], 1);
        assert_eq!(value["function"], function);
        assert_eq!(value["offset"], 0);
        assert_eq!(value["source"], "export_function_fragments");
        assert_eq!(value["query_work_cap"], options.scan_work);
        assert!(value["query_work_used"].as_u64().unwrap() <= options.scan_work as u64);
        let source = bundle::export_function_fragments(&hbc, &[function])
            .unwrap()
            .remove(0)
            .1;
        let definitions = value["definitions"].as_object().unwrap();
        for (id, definition) in definitions {
            assert_source_join(&source, &definition["source"]);
            assert_eq!(
                id,
                &format!(
                    "{}:{}:{}:{}",
                    function,
                    definition["register"],
                    definition["source"]["start"],
                    definition["source"]["end"]
                )
            );
        }
        for row in value["rows"].as_array().unwrap() {
            rows_seen += 1;
            assert_eq!(row["function"], function);
            assert_source_join(&source, &row["object"]);
            for role in ["key", "value"] {
                assert_source_join(&source, &row[role]["source"]);
                for id in row[role]["definition_ids"].as_array().unwrap() {
                    assert!(definitions.contains_key(id.as_str().unwrap()));
                }
                for dependency in row[role]["dependencies"].as_array().unwrap() {
                    if let Some(id) = dependency["owner_definition"].as_str() {
                        assert!(definitions.contains_key(id));
                    }
                    let read = dependency["read_span"].as_array().unwrap();
                    let start = read[0].as_u64().unwrap() as usize;
                    let end = read[1].as_u64().unwrap() as usize;
                    assert!(source.get(start..end).is_some());
                    for id in dependency["candidates"].as_array().unwrap() {
                        assert!(definitions.contains_key(id.as_str().unwrap()));
                    }
                }
            }
            let evidence = row["match_evidence"].as_array().unwrap();
            assert!(evidence.len() <= 8);
            for entry in evidence {
                assert_source_join(&source, &entry["source"]);
                if let Some(id) = entry["definition_id"].as_str() {
                    assert!(definitions.contains_key(id));
                }
            }
        }
        assert_eq!(
            bytes,
            report(
                fixture(),
                function,
                &Options {
                    max_bytes: bytes.len(),
                    ..Options::default()
                }
            )
            .unwrap()
        );
        assert!(report(
            fixture(),
            function,
            &Options {
                max_bytes: 1,
                ..Options::default()
            }
        )
        .is_err());
    }
    assert!(rows_seen > 0, "fixture must exercise property stores");
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn wrapper_preserves_typed_options_and_exception_metadata() {
    let data = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    for function in 0..hbc.functions.count() {
        let source = bundle::export_function_fragments(&hbc, &[function])
            .unwrap()
            .remove(0)
            .1;
        let exceptions: Vec<_> = hbc
            .functions
            .get_parsed_header(function)
            .unwrap()
            .exc_handlers
            .iter()
            .map(|h| (h.start, h.end, h.target))
            .collect();
        for offset in [0, 1, usize::MAX] {
            let options = Options {
                depth: 0,
                definition_limit: 1,
                limit: 1,
                offset,
                scan_work: 128,
                matches: vec!["raw syntax; $(not-executed)".into(), "[.*]".into()],
                ..Options::default()
            };
            assert_eq!(
                report(fixture(), function, &options).unwrap(),
                origins::analyze_properties_source(
                    &source,
                    &query(function, &options),
                    &exceptions
                )
                .unwrap()
            );
        }
    }
    assert_eq!(std::fs::read(fixture()).unwrap(), data);
}

#[test]
fn unknown_function_is_rejected_without_modifying_input() {
    let before = std::fs::read(fixture()).unwrap();
    let count = HbcFile::parse(&before).unwrap().functions.count();
    for function in [count, u32::MAX] {
        assert!(report(fixture(), function, &Options::default())
            .unwrap_err()
            .to_string()
            .contains("Unknown function"));
    }
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn invalid_options_are_rejected_before_reading_input() {
    let mut invalid = Vec::new();
    for depth in [65, usize::MAX] {
        invalid.push(Options {
            depth,
            ..Options::default()
        });
    }
    for definition_limit in [0, 4097, usize::MAX] {
        invalid.push(Options {
            definition_limit,
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
    for scan_work in [0, origins::MAX_PROPERTY_SCAN_WORK + 1, usize::MAX] {
        invalid.push(Options {
            scan_work,
            ..Options::default()
        });
    }
    for matches in [
        vec!["x".into(); 65],
        vec![String::new()],
        vec!["x".repeat(1025)],
        vec!["x".repeat(1024); 65],
        vec!["\u{00e9}".repeat(513)],
    ] {
        invalid.push(Options {
            matches,
            ..Options::default()
        });
    }
    let temp = tempfile::tempdir().unwrap();
    let missing = temp.path().join("nonexistent.hbc");
    for options in invalid {
        for message in [
            report(&missing, 0, &options).unwrap_err().to_string(),
            properties::run(&missing, 0, &options)
                .unwrap_err()
                .to_string(),
        ] {
            assert!(message.contains("properties"), "{message}");
        }
    }
    assert_eq!(std::fs::read_dir(temp.path()).unwrap().count(), 0);
}

#[test]
fn malformed_input_is_rejected_without_writes() {
    let temp = tempfile::tempdir().unwrap();
    let input = temp.path().join("bad.hbc");
    std::fs::write(&input, b"not bytecode").unwrap();
    assert!(report(&input, 0, &Options::default()).is_err());
    assert_eq!(std::fs::read(&input).unwrap(), b"not bytecode");
    assert_eq!(std::fs::read_dir(temp.path()).unwrap().count(), 1);
}

// Use a separate test process to capture stdout without harness redirection races.
#[test]
fn properties_emit_helper() {
    let Ok(mode) = std::env::var("AGENT_PROPERTIES_EMIT_MODE") else {
        return;
    };
    let options = Options {
        max_bytes: if mode == "budget" { 1 } else { 100_000 },
        depth: if mode == "invalid" { 65 } else { 8 },
        ..Options::default()
    };
    let function = if mode == "unknown" { u32::MAX } else { 0 };
    let result = properties::run(fixture(), function, &options);
    assert_eq!(result.is_ok(), mode == "success");
}

#[test]
fn stdout_contains_only_a_complete_report_or_no_report_on_failure() {
    for mode in ["success", "budget", "invalid", "unknown"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "properties_emit_helper", "--nocapture"])
            .env("AGENT_PROPERTIES_EMIT_MODE", mode)
            .output()
            .unwrap();
        assert!(output.status.success(), "{output:?}");
        let start = output.stdout.iter().position(|&b| b == b'{');
        if mode == "success" {
            let expected = report(fixture(), 0, &Options::default()).unwrap();
            let start = start.expect("serialized report");
            assert_eq!(&output.stdout[start..start + expected.len()], &expected);
            serde_json::from_slice::<Value>(&expected).unwrap();
        } else {
            assert!(start.is_none(), "unexpected report: {output:?}");
        }
    }
}
