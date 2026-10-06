use hermes_dec_rs::bundle;
use hermes_dec_rs::cli::{origins, symbols};

use serde_json::Value;
use std::path::Path;
use symbols::{report, Options};

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

fn assert_source_join(source: &str, span: &Value) {
    let start = span["start"].as_u64().unwrap() as usize;
    let end = span["end"].as_u64().unwrap() as usize;
    let javascript = span["javascript"].as_str().unwrap();
    let original = source.get(start..end).expect("valid raw fragment span");
    if span["snippet_truncated"].as_bool() != Some(true) {
        assert_eq!(original, javascript);
    }
}

#[test]
fn fixture_is_deterministic_read_only_and_sources_join() {
    let before = std::fs::read(fixture()).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&before).unwrap();
    let mut rows_seen = 0;
    for function in 0..hbc.functions.count() {
        let options = Options::default();
        let bytes = report(fixture(), function, &options).unwrap();
        assert_eq!(bytes, report(fixture(), function, &options).unwrap());
        let value: Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(value["function"], function);
        assert_eq!(value["offset"], 0);
        assert!(value["semantics"].is_string());
        let source = bundle::export_function_fragments(&hbc, &[function])
            .unwrap()
            .remove(0)
            .1;
        for row in value["rows"].as_array().unwrap() {
            rows_seen += 1;
            assert_eq!(row["function"], function);
            assert_source_join(&source, &row["environment"]);
            assert_source_join(&source, &row["value"]);
            for mention in row["literal_mentions"].as_array().unwrap() {
                assert_source_join(&source, &mention["source"]);
            }
        }
        let exact = Options {
            max_bytes: bytes.len(),
            ..Options::default()
        };
        assert_eq!(bytes, report(fixture(), function, &exact).unwrap());
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
    assert!(rows_seen > 0, "fixture must exercise slot stores");
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn unknown_function_is_rejected_without_modifying_input() {
    let before = std::fs::read(fixture()).unwrap();
    let count = hermes_dec_rs::HbcFile::parse(&before)
        .unwrap()
        .functions
        .count();
    for function in [count, u32::MAX] {
        assert!(report(fixture(), function, &Options::default())
            .unwrap_err()
            .to_string()
            .contains("Unknown function"));
    }
    assert_eq!(std::fs::read(fixture()).unwrap(), before);
}

#[test]
fn wrapper_preserves_typed_options_and_exception_metadata() {
    let data = std::fs::read(fixture()).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&data).unwrap();
    let function = 0;
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
            literal_limit: 1,
            limit: 1,
            offset,
            slots: vec![0, u32::MAX],
            matches: vec!["raw syntax; $(not-executed)".into()],
            ..Options::default()
        };
        let query = origins::SymbolOptions {
            function,
            depth: options.depth,
            definition_limit: options.definition_limit,
            literal_limit: options.literal_limit,
            limit: options.limit,
            offset: options.offset,
            max_bytes: options.max_bytes,
            scan_work: options.scan_work,
            slots: options.slots.clone(),
            matches: options.matches.clone(),
        };
        assert_eq!(
            report(fixture(), function, &options).unwrap(),
            origins::analyze_symbols_source(&source, &query, &exceptions).unwrap()
        );
    }
}

#[test]
fn literal_mentions_join_raw_syntax_in_a_complete_typed_fragment() {
    let source = "M[0] = ['f',0,0]; F[0] = function function_0(env,self,args,newTarget,callee,state) { const r = objectCreate(null); let pc = 0, caught; for (;;) { try { switch (pc) { case 0: {\n// HBC function 0, PC 0\nr[1] = 'raw\\nsyntax';\n// HBC function 0, PC 2\nr[7].slots[3] = r[1];\n// HBC function 0, PC 4\nreturn;\n} default: throw new ErrorCtor('bad'); } } catch (error) { throw error; } } };";
    let query = origins::SymbolOptions {
        function: 0,
        depth: 8,
        definition_limit: 64,
        literal_limit: 32,
        limit: 32,
        offset: 0,
        max_bytes: 100_000,
        scan_work: origins::DEFAULT_SYMBOL_SCAN_WORK,
        slots: vec![],
        matches: vec![],
    };
    let bytes = origins::analyze_symbols_source(source, &query, &[]).unwrap();
    let value: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(value["stores_total"], 1);
    let rows = value["rows"].as_array().unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["pc"], 2);
    assert_eq!(rows[0]["slot"], 3);
    assert_source_join(source, &rows[0]["environment"]);
    assert_source_join(source, &rows[0]["value"]);
    let mentions = rows[0]["literal_mentions"].as_array().unwrap();
    assert_eq!(mentions.len(), 1);
    assert_eq!(mentions[0]["pc"], 0);
    assert_source_join(source, &mentions[0]["source"]);
    assert_eq!(mentions[0]["source"]["javascript"], "'raw\\nsyntax'");
}

#[test]
fn invalid_options_are_rejected_before_reading_input() {
    let invalid = [
        Options {
            depth: 65,
            ..Options::default()
        },
        Options {
            definition_limit: 0,
            ..Options::default()
        },
        Options {
            definition_limit: 4097,
            ..Options::default()
        },
        Options {
            literal_limit: 0,
            ..Options::default()
        },
        Options {
            literal_limit: 129,
            ..Options::default()
        },
        Options {
            limit: 0,
            ..Options::default()
        },
        Options {
            limit: 1001,
            ..Options::default()
        },
        Options {
            max_bytes: 0,
            ..Options::default()
        },
        Options {
            max_bytes: 16_777_217,
            ..Options::default()
        },
        Options {
            scan_work: 0,
            ..Options::default()
        },
        Options {
            scan_work: origins::MAX_SYMBOL_SCAN_WORK + 1,
            ..Options::default()
        },
        Options {
            slots: vec![0; 4097],
            ..Options::default()
        },
        Options {
            matches: vec!["x".into(); 65],
            ..Options::default()
        },
        Options {
            matches: vec![String::new()],
            ..Options::default()
        },
        Options {
            matches: vec!["x".repeat(1025)],
            ..Options::default()
        },
        Options {
            matches: vec!["x".repeat(1024); 65],
            ..Options::default()
        },
    ];
    let missing = fixture().join("nonexistent");
    for options in invalid {
        let message = report(&missing, 0, &options).unwrap_err().to_string();
        assert!(message.contains("symbols"), "{message}");
    }
}
