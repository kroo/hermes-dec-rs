use hermes_dec_rs::cli::json_literals::{self, analyze_source, Options};
use serde_json::{json, Value};
use std::path::Path;

#[test]
fn wired_cli_reports_literal_scope_and_fails_atomically() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("data/bundle_semantics.hbc");
    assert!(fixture.is_file());
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
        .args(["json-literals", fixture.to_str().unwrap(), "0"])
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let v: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v["schema"], "json-literals-v1");
    assert!(v["root_literal_scan_complete"].as_bool().unwrap());
    assert!(v["counts"]["raw_literals"].as_u64().unwrap() > 0);
    for flags in [
        vec!["--pointer", "invalid"],
        vec!["--pc", "4294967295"],
        vec!["--scan-work", "1"],
        vec!["--max-bytes", "1"],
    ] {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .args(["json-literals", fixture.to_str().unwrap(), "0"])
            .args(flags)
            .output()
            .unwrap();
        assert!(!out.status.success());
        assert!(out.stdout.is_empty());
    }
}

#[test]
fn compiled_authored_literal_cli_continuation_roundtrips_source_spans() {
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("data/agent_json_literals.hbc");
    let invoke = |args: &[String]| {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .args(args)
            .output()
            .unwrap();
        assert!(
            out.status.success(),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        serde_json::from_slice::<Value>(&out.stdout).unwrap()
    };
    let first = invoke(&[
        "json-literals".into(),
        fixture.to_str().unwrap().into(),
        "0".into(),
        "--match=keep".into(),
        "--pointer=/escaped~1key/items/0/n".into(),
        "--limit=1".into(),
    ]);
    assert_eq!(first["literals"][0]["selected_value"], 1);
    assert_eq!(first["counts"]["duplicate_key_documents"], 1);
    assert_eq!(first["counts"]["lossy_number_documents"], 1);
    let continuation = &first["continuation"];
    let mut args = vec![
        continuation["command"].as_str().unwrap().into(),
        continuation["input"].as_str().unwrap().into(),
        continuation["function"].to_string(),
    ];
    args.extend(
        continuation["flags"]
            .as_array()
            .unwrap()
            .iter()
            .map(|s| s.as_str().unwrap().into()),
    );
    let second = invoke(&args);
    assert_eq!(second["literals"][0]["selected_value"], 2);
    assert!(second["next_offset"].is_null());
    let bytes = std::fs::read(&fixture).unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&bytes).unwrap();
    let source =
        hermes_dec_rs::bundle::export_function_fragment_bounded(&hbc, 0, 1_000_000, 1_000_000)
            .unwrap();
    for report in [first, second] {
        let literal = &report["literals"][0];
        let start = literal["source_span"]["start"].as_u64().unwrap() as usize;
        let end = literal["source_span"]["end"].as_u64().unwrap() as usize;
        assert_eq!(
            &source[start..end],
            literal["raw_source_preview"].as_str().unwrap()
        );
    }
}

fn source(body: &str) -> String {
    format!("M[7] = ['metadata', 0, 0];\nF[7] = function function_7(env, self, args) {{\nconst r = [];\n{body}\n}};")
}

fn analyze(body: &str, options: &Options) -> Value {
    serde_json::from_slice(&analyze_source(&source(body), 7, options).unwrap()).unwrap()
}

fn document(value: &Value) -> String {
    // JSON quoting is also valid authored JS string syntax.
    serde_json::to_string(&value.to_string()).unwrap()
}

fn body(values: &[Value]) -> String {
    values
        .iter()
        .enumerate()
        .map(|(i, v)| {
            format!(
                "// HBC function 7, PC {}\nr[{}] = {};\n",
                i * 2,
                i,
                document(v)
            )
        })
        .collect()
}

#[test]
fn authored_nested_documents_are_full_literal_evidence() {
    let values = [
        json!({"nested":{"list":[true,null,{"deep":"value"}]}}),
        json!([1, 2, 3]),
    ];
    let r = analyze(&body(&values), &Options::default());
    assert_eq!(r["literals"][0]["selected_value"], values[0]);
    assert_eq!(r["literals"][1]["selected_value"], values[1]);
    assert_eq!(r["counts"]["raw_literals"], 2);
    assert_eq!(r["counts"]["json_documents"], 2);
    assert!(r["semantics"]
        .as_str()
        .unwrap()
        .contains("NOT runtime values"));
    assert_eq!(r["evidence"], "embedded_json_literal");
    assert_eq!(r["schema"], "json-literals-v1");
    assert_eq!(r["schema_version"], 1);
}

#[test]
fn rfc6901_keys_and_empty_pointer() {
    let b = body(&[json!({"a/b":{"~key":{"": ["first", "second"]}}, "01":"object key"})]);
    let mut o = Options {
        pointer: Some("/a~1b/~0key//1".into()),
        ..Options::default()
    };
    assert_eq!(analyze(&b, &o)["literals"][0]["selected_value"], "second");
    o.pointer = Some("/01".into());
    assert_eq!(
        analyze(&b, &o)["literals"][0]["selected_value"],
        "object key"
    );
    o.pointer = Some(String::new());
    assert!(analyze(&b, &o)["literals"][0]["selected_value"].is_object());
}

#[test]
fn array_indices_are_canonical_and_not_js_paths() {
    let b = body(&[json!(["first", "second"])]);
    for pointer in [
        "/01",
        "/-",
        "/+1",
        "/ 1",
        "/1.0",
        "/99999999999999999999999999999",
    ] {
        let o = Options {
            pointer: Some(pointer.into()),
            ..Options::default()
        };
        let r = analyze(&b, &o);
        assert_eq!(r["counts"]["pointer_missing"], 1, "{pointer}");
        assert_eq!(r["literals"], json!([]));
    }
    for pointer in ["x.y", "#/0", "/~2", "/~"] {
        let o = Options {
            pointer: Some(pointer.into()),
            ..Options::default()
        };
        assert!(analyze_source(&source(&b), 7, &o).is_err());
    }
}

#[test]
fn filters_are_or_case_insensitive_and_regex_escaped() {
    let b = body(&[
        json!({"text":"UPPER [a.*]"}),
        json!({"text":"other"}),
        json!({"text":"aZZ"}),
    ]);
    let o = Options {
        matches: vec!["[A.*]".into(), "OTHER".into()],
        ..Options::default()
    };
    let r = analyze(&b, &o);
    assert_eq!(r["literals"].as_array().unwrap().len(), 2);
    assert_eq!(r["literals"][0]["literal_id"], 0);
    assert_eq!(r["literals"][1]["literal_id"], 1);
    assert_eq!(r["counts"]["filtered"], 1);
}

#[test]
fn pc_selects_union_of_distinct_literals() {
    let b = "// HBC function 7, PC 1\nr[0] = '{\"a\":1}'; r[1] = '[2]';\n// HBC function 7, PC 9\nr[2] = '[3]';";
    let o = Options {
        pc: Some(1),
        ..Options::default()
    };
    let r = analyze(b, &o);
    assert_eq!(r["literals"].as_array().unwrap().len(), 2);
    assert_eq!(r["literals"][0]["pc"], 1);
    assert_eq!(r["literals"][1]["pc"], 1);
    assert_ne!(
        r["literals"][0]["literal_id"],
        r["literals"][1]["literal_id"]
    );
}

#[test]
fn duplicate_documents_have_distinct_stable_raw_ordinals() {
    let b = "// HBC function 7, PC 0\nr[0]='not JSON';r[1]='{}';r[2]='{}';";
    let o = Options {
        limit: 1,
        ..Options::default()
    };
    let r = analyze(b, &o);
    assert_eq!(r["literals"][0]["literal_id"], 1);
    assert_eq!(r["next_offset"], 2);
    let resumed = analyze(b, &Options { offset: 2, ..o });
    assert_eq!(resumed["literals"][0]["literal_id"], 2);
    assert_eq!(resumed["next_offset"], Value::Null);
}

#[test]
fn cursor_applies_before_all_filters_and_preserves_options() {
    let b = body(&[
        json!({"x":"keep"}),
        json!({"x":"skip"}),
        json!({"x":"keep"}),
        json!({"x":"keep"}),
    ]);
    let o = Options {
        matches: vec!["keep".into()],
        pointer: Some("/x".into()),
        limit: 1,
        offset: 1,
        ..Options::default()
    };
    let r = analyze(&b, &o);
    assert_eq!(r["literals"][0]["literal_id"], 2);
    assert_eq!(r["next_offset"], 3);
    let args = &r["continuation"]["options"];
    assert_eq!(args["matches"], json!(["keep"]));
    assert_eq!(args["pointer"], "/x");
    assert_eq!(args["offset"], 3);
    assert_eq!(args["max_bytes"], o.max_bytes);
    assert_eq!(args["scan_work"], o.scan_work);
    assert_eq!(r["continuation"]["function"], 7);
    assert_eq!(r["continuation"]["api"], "analyze_source");
    assert_eq!(r["continuation"]["command"], "json-literals");
    assert_eq!(r["continuation"]["input_scope"], "same_input_hbc");
    assert_eq!(
        r["continuation"]["flags"],
        json!([
            "--limit",
            "1",
            "--offset",
            "3",
            "--max-bytes",
            o.max_bytes.to_string(),
            "--scan-work",
            o.scan_work.to_string(),
            "--pointer=/x",
            "--match=keep"
        ])
    );
    assert_eq!(
        analyze(&b, &Options { offset: 100, ..o })["literals"],
        json!([])
    );
}

#[test]
fn utf8_spans_and_raw_previews_use_original_source() {
    let value = json!({"text":"é🙂".repeat(100)});
    let s = source(&body(&[value.clone()]));
    let r: Value =
        serde_json::from_slice(&analyze_source(&s, 7, &Options::default()).unwrap()).unwrap();
    let item = &r["literals"][0];
    let start = item["source_span"]["start"].as_u64().unwrap() as usize;
    let end = item["source_span"]["end"].as_u64().unwrap() as usize;
    let raw = &s[start..end];
    let preview = item["raw_source_preview"].as_str().unwrap();
    assert!(preview.len() <= 256);
    assert!(raw.starts_with(preview));
    assert_eq!(
        raw.len(),
        item["raw_source_bytes"].as_u64().unwrap() as usize
    );
    assert_eq!(item["selected_value"], value);
    assert_eq!(r["source_basis"]["source_bytes"], s.len());
}

#[test]
fn marker_looking_strings_and_block_comments_are_not_pc_anchors() {
    let b = "// HBC function 7, PC 2\nr[0]='// HBC function 999, PC 0';\nr[1]='{\"text\":\"// HBC function 999, PC 0\"}';\n/* // HBC function 999, PC 0 */\nr[2]='[]';";
    let r = analyze(b, &Options::default());
    assert_eq!(r["literals"][0]["pc"], 2);
    assert_eq!(r["literals"][1]["pc"], 2);
}

#[test]
fn parser_literal_decoding_handles_escapes_and_surrogate_pairs() {
    let b = r#"// HBC function 7, PC 0
r[0] = '\x7b"a":"\uD83D\uDE42","b":"line\\nend","c":"\\\\"}';
r[1] = '{"v":"\u{1F642}"}';
r[2] = '{"v":"é"}';"#;
    let r = analyze(b, &Options::default());
    assert_eq!(
        r["literals"][0]["selected_value"],
        json!({"a":"🙂","b":"line\nend","c":"\\"})
    );
    assert_eq!(r["literals"][1]["selected_value"], json!({"v":"🙂"}));
    assert_eq!(r["literals"][2]["selected_value"], json!({"v":"é"}));
}

#[test]
fn lone_js_surrogates_and_malformed_json_are_explicit_omissions() {
    let b = r#"// HBC function 7, PC 0
r[0] = '{"v":"\uD800"}';
r[1] = '{"v":"\\uD800"}';
r[2] = '{broken}';
r[3] = '42';
r[4] = '{}';"#;
    let r = analyze(b, &Options::default());
    assert_eq!(r["counts"]["string_code_unit_omissions"], 1);
    assert_eq!(r["counts"]["malformed_json"], 2);
    assert_eq!(r["counts"]["non_documents"], 1);
    assert_eq!(r["literals"][0]["literal_id"], 4);
}

#[test]
fn nested_function_arrow_and_class_literals_are_excluded() {
    let b = "// HBC function 7, PC 0\nr[0]='{}';\nfunction inner(){\n// HBC function 7, PC 99\nreturn '[999]';}\nconst arrow=()=> '[888]';\nclass C { m(){return '[777]';} }\nr[1]='[1]';";
    let r = analyze(b, &Options::default());
    assert_eq!(r["counts"]["nested_scopes_excluded"], 3);
    assert_eq!(r["counts"]["raw_literals"], 2);
    assert_eq!(r["literals"][1]["pc"], 0);
    assert_eq!(r["literals"][1]["literal_id"], 1);
}

#[test]
fn wrong_function_unordered_and_missing_markers_fail() {
    for b in [
        "// HBC function 8, PC 0\nr[0]='{}';",
        "// HBC function 7, PC 2\nr[0]='{}';\n// HBC function 7, PC 1\nr[1]='{}';",
        "// HBC function 7, PC 2\nr[0]='{}';\n// HBC function 7, PC 2\nr[1]='{}';",
        "r[0]='{}';",
        "// HBC function 7, PC nope\nr[0]='{}';",
        "// HBC function 7, PC +1\nr[0]='{}';",
        "// HBC function +7, PC 1\nr[0]='{}';",
        "r[0]='{}';\n// HBC function 7, PC 0\nr[1]='{}';",
    ] {
        assert!(
            analyze_source(&source(b), 7, &Options::default()).is_err(),
            "{b}"
        );
    }
}

#[test]
fn source_scope_is_one_matching_exporter_root() {
    let good = source("// HBC function 7, PC 0\nr[0]='{}';");
    for s in [
        good.replace("F[7]", "F[8]"),
        format!("{good}\n{good}"),
        "function f(){\n// HBC function 7, PC 0\nreturn '{}';}".into(),
        format!("// HBC function 7, PC 0\n{good}"),
        "F[7] = makeFunction();".into(),
    ] {
        assert!(analyze_source(&s, 7, &Options::default()).is_err());
    }
    let s = format!("const outside = '[999]';\n{good}");
    let r: Value =
        serde_json::from_slice(&analyze_source(&s, 7, &Options::default()).unwrap()).unwrap();
    assert_eq!(r["counts"]["raw_literals"], 1);
}

#[test]
fn calls_and_constructors_are_not_executed() {
    let b = "// HBC function 7, PC 0\nr[0]=JSON.parse('{\"x\":1}');r[1]=new Unknown('[2]');r[2]=framework.get();";
    let r = analyze(b, &Options::default());
    assert_eq!(r["literals"][0]["selected_value"], json!({"x":1}));
    assert_eq!(r["literals"][1]["selected_value"], json!([2]));
}

#[test]
fn options_validate_before_input_read() {
    let missing = Path::new("/this-authored-test-path-does-not-exist/input.hbc");
    let bad = [
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
            max_bytes: 16 * 1024 * 1024 + 1,
            ..Options::default()
        },
        Options {
            scan_work: 0,
            ..Options::default()
        },
        Options {
            scan_work: 128 * 1024 * 1024 + 1,
            ..Options::default()
        },
        Options {
            matches: vec![String::new()],
            ..Options::default()
        },
        Options {
            matches: vec!["a".into(); 65],
            ..Options::default()
        },
        Options {
            matches: vec!["a".repeat(1025)],
            ..Options::default()
        },
        Options {
            pointer: Some("/".repeat(1025)),
            ..Options::default()
        },
        Options {
            pointer: Some("not a pointer".into()),
            ..Options::default()
        },
    ];
    for options in bad {
        let message = json_literals::report(missing, 7, &options)
            .unwrap_err()
            .to_string();
        assert!(!message.contains("No such file"), "{message}");
        assert!(json_literals::run(missing, 7, &options).is_err());
    }
}

#[test]
fn parser_source_and_work_budgets_fail_closed() {
    let good = source("// HBC function 7, PC 0\nr[0]='{}';");
    assert!(analyze_source(
        &good,
        7,
        &Options {
            scan_work: good.len(),
            ..Options::default()
        }
    )
    .is_err());
    assert!(analyze_source(
        &source("// HBC function 7, PC 0\nr[0]='unterminated;"),
        7,
        &Options::default()
    )
    .is_err());
    let deep = format!(
        "// HBC function 7, PC 0\nr[0]={}'[]'{};",
        "(".repeat(129),
        ")".repeat(129)
    );
    assert!(analyze_source(&source(&deep), 7, &Options::default()).is_err());
    let deep_json = format!("{}0{}", "[".repeat(129), "]".repeat(129));
    let b = format!(
        "// HBC function 7, PC 0\nr[0]={};",
        serde_json::to_string(&deep_json).unwrap()
    );
    assert!(analyze_source(&source(&b), 7, &Options::default()).is_err());
    let giant = source(&format!(
        "// HBC function 7, PC 0\nr[0]='{}';",
        "x".repeat(1024 * 1024)
    ));
    assert!(analyze_source(&giant, 7, &Options::default()).is_err());
}

#[test]
fn filters_have_charged_work_and_aggregate_byte_limits() {
    let b = body(&[
        json!({"v":"x".repeat(600_000)}),
        json!({"v":"x".repeat(600_000)}),
    ]);
    let o = Options {
        matches: vec!["absent".into(); 64],
        scan_work: 128 * 1024 * 1024,
        ..Options::default()
    };
    let e = analyze_source(&source(&b), 7, &o).unwrap_err().to_string();
    assert!(e.contains("aggregate decoded filter"), "{e}");
    let o = Options {
        scan_work: 2_000_000,
        ..o
    };
    assert!(analyze_source(&source(&b), 7, &o).is_err());
}

#[test]
fn oversized_full_value_is_an_error_not_a_truncated_result() {
    let b = body(&[json!({"full":"x".repeat(20_000)})]);
    let o = Options {
        max_bytes: 4000,
        ..Options::default()
    };
    let e = analyze_source(&source(&b), 7, &o).unwrap_err().to_string();
    assert!(e.contains("output byte budget"), "{e}");
    let r = analyze(&b, &Options::default());
    assert_eq!(
        r["literals"][0]["selected_value"]["full"]
            .as_str()
            .unwrap()
            .len(),
        20_000
    );
}

#[test]
fn aggregate_output_and_metadata_are_also_bounded() {
    let b = body(&[json!(["x".repeat(2000)]), json!(["x".repeat(2000)])]);
    assert!(analyze_source(
        &source(&b),
        7,
        &Options {
            max_bytes: 3000,
            ..Options::default()
        }
    )
    .is_err());
    assert!(analyze_source(
        &source("// HBC function 7, PC 0\nr[0]='{}';"),
        7,
        &Options {
            max_bytes: 1,
            ..Options::default()
        }
    )
    .is_err());
}

#[test]
fn hbc_input_size_is_checked_before_read_and_parse() {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let path = std::env::temp_dir().join(format!(
        "agent-json-literals-{}-{nonce}.hbc",
        std::process::id()
    ));
    let file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap();
    file.set_len(128 * 1024 * 1024 + 1).unwrap();
    drop(file);
    let result = json_literals::report(&path, 7, &Options::default());
    std::fs::remove_file(&path).unwrap();
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("HBC input exceeds 128 MiB"));
}

#[test]
fn exporter_member_division_does_not_hide_literal_evidence() {
    let b = "// HBC function 7, PC 0\nr[0]=r[1] / r[2];r[3]='{}';";
    assert_eq!(
        analyze(b, &Options::default())["counts"]["json_documents"],
        1
    );
    for expression in ["`{}'`", "/'{}'/", "a / b"] {
        let b = format!("// HBC function 7, PC 0\nr[0]={expression};");
        assert!(analyze_source(&source(&b), 7, &Options::default()).is_err());
    }
}

#[test]
fn literal_line_continuation_and_unicode_replacement_character() {
    let b = "// HBC function 7, PC 0\nr[0]='{\\\n\"v\":\"�\"}';";
    let r = analyze(b, &Options::default());
    assert_eq!(r["literals"][0]["selected_value"], json!({"v":"�"}));
    assert_eq!(r["counts"]["string_code_unit_omissions"], 0);
}

#[test]
fn default_budget_handles_authored_26_mib_root_source() {
    let o = Options::default();
    assert_eq!(o.scan_work, 64 * 1024 * 1024);
    assert_eq!(o.limit, 32);
    assert_eq!(o.offset, 0);
    assert_eq!(o.max_bytes, 1024 * 1024);
    let b = format!(
        "/*{}*/\n// HBC function 7, PC 0\nr[0]='{{}}';",
        "x".repeat(26 * 1024 * 1024)
    );
    let r = analyze(&b, &o);
    assert_eq!(r["literals"][0]["selected_value"], json!({}));
    assert!(r["work_used"].as_u64().unwrap() > 26 * 1024 * 1024);
    assert_eq!(r["limits"]["scan_work"], 64 * 1024 * 1024);
}

#[test]
fn flat_2000_element_array_does_not_hide_unrelated_json() {
    let elements = (0..2000)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let b = format!("// HBC function 7, PC 0\nr[0]=[{elements}];\n// HBC function 7, PC 2\nr[1]='{{\"ok\":true}}';");
    let r = analyze(&b, &Options::default());
    assert_eq!(r["counts"]["raw_literals"], 1);
    assert_eq!(r["literals"][0]["selected_value"], json!({"ok":true}));
    assert_eq!(r["literals"][0]["pc"], 2);
    let unary = format!(
        "// HBC function 7, PC 0\nr[0]=[0,{}1];r[1]='{{}}';",
        "!".repeat(257)
    );
    assert!(analyze_source(&source(&unary), 7, &Options::default()).is_err());
}

#[test]
fn requested_pc_must_exist_in_root_even_for_empty_or_past_end_pages() {
    let b = "// HBC function 7, PC 0\nr[0]='{}';\nfunction inner(){\n// HBC function 7, PC 99\nreturn '{}';}\n// HBC function 7, PC 2\nr[1]=42;";
    for pc in [1, 99, u32::MAX] {
        let o = Options {
            pc: Some(pc),
            offset: usize::MAX,
            ..Options::default()
        };
        let e = analyze_source(&source(b), 7, &o).unwrap_err().to_string();
        assert!(e.contains("unknown root PC"), "{e}");
    }
    let r = analyze(
        b,
        &Options {
            pc: Some(2),
            ..Options::default()
        },
    );
    assert_eq!(r["literals"], json!([]));
    let deep_json = format!("{}0{}", "[".repeat(129), "]".repeat(129));
    let b = format!(
        "// HBC function 7, PC 0\nr[0]={};",
        serde_json::to_string(&deep_json).unwrap()
    );
    let e = analyze_source(
        &source(&b),
        7,
        &Options {
            pc: Some(1),
            ..Options::default()
        },
    )
    .unwrap_err()
    .to_string();
    assert!(e.contains("unknown root PC"), "{e}");
}

#[test]
fn cli_continuation_preserves_pc_matches_pointer_and_bounds_as_tokens() {
    let b = "// HBC function 7, PC 4\nr[0]='{\"x/y\":\"-literal with spaces\"}';r[1]='{\"x/y\":\"-literal with spaces\"}';";
    let o = Options {
        pc: Some(4),
        matches: vec![
            "-literal with spaces".into(),
            "a=b;$(not shell code)".into(),
        ],
        pointer: Some("/x~1y".into()),
        limit: 1,
        max_bytes: 90_000,
        scan_work: 70_000_000,
        ..Options::default()
    };
    let r = analyze(b, &o);
    assert_eq!(
        r["continuation"]["flags"],
        json!([
            "--limit",
            "1",
            "--offset",
            "1",
            "--max-bytes",
            "90000",
            "--scan-work",
            "70000000",
            "--pc",
            "4",
            "--pointer=/x~1y",
            "--match=-literal with spaces",
            "--match=a=b;$(not shell code)"
        ])
    );
    assert_eq!(r["continuation"]["options"]["pc"], 4);
    assert_eq!(r["continuation"]["options"]["matches"], json!(o.matches));
    let r = analyze(
        b,
        &Options {
            pointer: Some(String::new()),
            ..o
        },
    );
    assert!(r["continuation"]["flags"]
        .as_array()
        .unwrap()
        .contains(&json!("--pointer=")));
}

#[test]
fn nested_scope_comments_are_excluded_before_id_and_anchor_validation() {
    let b = "// HBC function 7, PC 0\nr[0]='{}';\nfunction inner(){\n// HBC function 8, PC 99\nreturn '[999]';}\nconst arrow=()=>{\n// HBC function invalid, PC nope\nreturn '[888]';};\nclass C { m(){ return '[777]'; // HBC function 9, PC 1\n} }\nr[1]='[1]';";
    let r = analyze(b, &Options::default());
    assert_eq!(r["schema"], "json-literals-v1");
    assert_eq!(r["counts"]["nested_scopes_excluded"], 3);
    assert_eq!(r["counts"]["raw_literals"], 2);
    assert_eq!(r["literals"][1]["literal_id"], 1);
    assert_eq!(r["literals"][1]["function"], 7);
    assert_eq!(r["literals"][1]["pc"], 0);
    let e = analyze_source(
        &source(b),
        7,
        &Options {
            pc: Some(99),
            ..Options::default()
        },
    )
    .unwrap_err()
    .to_string();
    assert!(e.contains("unknown root PC"), "{e}");
}

#[test]
fn root_pc_comments_are_line_anchored_not_trailing_syntax() {
    for b in [
        "r[0]=0; // HBC function 7, PC 0\nr[1]='{}';",
        "// HBC function 7, PC 0\nr[0]='{}'; // HBC function 7, PC 2\nr[1]='[]';",
        "/* preceding comment */ // HBC function 7, PC 0\nr[0]='{}';",
    ] {
        let e = analyze_source(&source(b), 7, &Options::default())
            .unwrap_err()
            .to_string();
        assert!(e.contains("line-anchored"), "{e}");
    }
    for newline in ["\n", "\r\n", "\r", "\u{2028}", "\u{2029}"] {
        let b = format!("r[0]=0;{newline} \t// HBC function 7, PC 2{newline}r[1]='{{}}';");
        let r = analyze(&b, &Options::default());
        assert_eq!(r["literals"][0]["pc"], 2);
        let deep = format!(
            "// HBC function 7, PC 2{newline}r[0]={}'[]'{};",
            "(".repeat(129),
            ")".repeat(129)
        );
        let e = analyze_source(&source(&deep), 7, &Options::default())
            .unwrap_err()
            .to_string();
        assert!(e.contains("source nesting depth"), "{e}");
    }
}

fn json_text_body(texts: &[&str]) -> String {
    texts
        .iter()
        .enumerate()
        .map(|(i, text)| {
            format!(
                "// HBC function 7, PC {}\nr[{}]={};\n",
                i * 2,
                i,
                serde_json::to_string(text).unwrap()
            )
        })
        .collect()
}

#[test]
fn duplicate_keys_omit_entire_documents_recursively_not_last_wins_values() {
    let b = json_text_body(&[
        r#"{"x":1,"x":2}"#,
        r#"{"nested":{"x":1,"x":2}}"#,
        r#"[{"x":1,"x":2}]"#,
        r#"[{"array":[{"x":1,"x":2}]}]"#,
        r#"{"x":1,"x":2,"y":1,"y":2}"#,
        r#"{"good":true}"#,
    ]);
    let r = analyze(&b, &Options::default());
    assert_eq!(r["counts"]["raw_literals"], 6);
    assert_eq!(r["counts"]["duplicate_key_documents"], 5);
    assert_eq!(r["counts"]["malformed_json"], 0);
    assert_eq!(r["counts"]["json_documents"], 1);
    assert_eq!(r["literals"].as_array().unwrap().len(), 1);
    assert_eq!(r["literals"][0]["literal_id"], 5);
    assert_eq!(r["literals"][0]["selected_value"], json!({"good":true}));
    assert!(r["omission_policy"]
        .as_str()
        .unwrap()
        .contains("duplicate-key JSON documents"));
    assert!(r["duplicate_key_policy"]
        .as_str()
        .unwrap()
        .contains("never return a last-wins value"));
    // A valid pointer into another branch cannot hide duplicate-bearing content.
    let b = json_text_body(&[r#"{"good":true,"other":{"x":1,"x":2}}"#]);
    let r = analyze(
        &b,
        &Options {
            pointer: Some("/good".into()),
            ..Options::default()
        },
    );
    assert_eq!(r["literals"], json!([]));
    assert_eq!(r["counts"]["duplicate_key_documents"], 1);
    assert_eq!(r["counts"]["pointer_missing"], 0);
}

#[test]
fn escaped_equivalent_keys_collide_after_json_decoding() {
    let b = json_text_body(&[
        r#"{"a":1,"\u0061":2}"#,
        r#"{"a/b":1,"a\/b":2}"#,
        r#"{"🙂":1,"\ud83d\ude42":2}"#,
        r#"{"":1,"":2}"#,
        r#"{"A":1,"a":2}"#,
    ]);
    let r = analyze(&b, &Options::default());
    assert_eq!(r["counts"]["duplicate_key_documents"], 4);
    assert_eq!(r["counts"]["malformed_json"], 0);
    assert_eq!(r["literals"][0]["literal_id"], 4);
    assert_eq!(r["literals"][0]["selected_value"], json!({"A":1,"a":2}));
}

#[test]
fn sibling_objects_can_share_names_and_unique_values_keep_standard_numbers() {
    let texts = [
        r#"[{"x":1},{"x":2}]"#,
        r#"{"left":{"x":1},"right":{"x":2}}"#,
        r#"{"x":{"x":[null,true,false,"value",{"x":3}]}}"#,
        r#"[-9223372036854775808,18446744073709551615,1.25,1e30,-0.0]"#,
    ];
    let r = analyze(&json_text_body(&texts), &Options::default());
    assert_eq!(r["counts"]["duplicate_key_documents"], 0);
    assert_eq!(r["counts"]["malformed_json"], 0);
    for (i, text) in texts.iter().enumerate() {
        let original: Value = serde_json::from_str(text).unwrap();
        assert_eq!(r["literals"][i]["selected_value"], original);
    }
    let b = json_text_body(&["[1e400]", r#"{"x":1} trailing"#]);
    let r = analyze(&b, &Options::default());
    assert_eq!(r["literals"], json!([]));
    assert_eq!(r["counts"]["malformed_json"], 2);
    assert_eq!(r["counts"]["duplicate_key_documents"], 0);
}

#[test]
fn lossy_numeric_documents_are_omitted_without_float_equality() {
    let docs = [
        r#"{"n":18446744073709551617}"#,
        r#"{"n":-9223372036854775809}"#,
        r#"{"n":1.0000000000000001}"#,
        r#"{"n":1e-400}"#,
        r#"{"nested":[0,1.234567890123456789]}"#,
    ];
    let b: String = docs
        .iter()
        .enumerate()
        .map(|(i, doc)| {
            format!(
                "// HBC function 7, PC {i}\nr[{i}]={};\n",
                serde_json::to_string(doc).unwrap()
            )
        })
        .collect();
    let r = analyze(&b, &Options::default());
    assert_eq!(r["counts"]["lossy_number_documents"], docs.len());
    assert!(r["literals"].as_array().unwrap().is_empty());
    assert_eq!(r["counts"]["malformed_json"], 0);
}

#[test]
fn decimal_equivalence_preserves_values_without_claiming_original_spelling() {
    let b = "// HBC function 7, PC 0\nr[0]='[0.10,1.2500,1e30,10e-1,9007199254740993,-0.0,-0]';";
    let r = analyze(b, &Options::default());
    assert_eq!(r["counts"]["lossy_number_documents"], 0);
    assert_eq!(r["literals"][0]["selected_value"][4], 9007199254740993_u64);
    assert!(r["literals"][0]["selected_value"][6]
        .as_f64()
        .unwrap()
        .is_sign_negative());
    assert!(r["number_policy"]
        .as_str()
        .unwrap()
        .contains("no JavaScript number conversion"));
}

#[test]
fn number_like_strings_are_not_numeric_evidence() {
    let r = analyze(
        &body(&[json!({"text":"18446744073709551617 -0 1e-400 \"escaped\""})]),
        &Options::default(),
    );
    assert_eq!(r["counts"]["lossy_number_documents"], 0);
    assert_eq!(r["counts"]["returned"], 1);
}

#[test]
fn duplicate_detection_stops_at_first_duplicate_with_explicit_policy() {
    let b = json_text_body(&[r#"{"x":1,"x":BROKEN TAIL}"#]);
    let r = analyze(&b, &Options::default());
    assert_eq!(r["literals"], json!([]));
    assert_eq!(r["counts"]["duplicate_key_documents"], 1);
    assert_eq!(r["counts"]["malformed_json"], 0);
    assert!(r["duplicate_key_policy"]
        .as_str()
        .unwrap()
        .contains("remaining document syntax is not validated"));
}
