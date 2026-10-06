//! Authored source only. Candidate views are not executable-value assertions.
use hermes_dec_rs::cli::sites::{analyze_literal_view_source, LiteralViewOptions};
use serde_json::Value;
use std::collections::BTreeSet;

fn source(instructions: &[(u32, &str)]) -> String {
    let mut s = String::from("F[7] = function(e, a) { var r = [];\n");
    for (pc, body) in instructions {
        s.push_str(&format!("// HBC function 7, PC {pc}\n{body}\n"));
    }
    s.push_str("};\n");
    s
}

fn options() -> LiteralViewOptions {
    LiteralViewOptions {
        json: true,
        ..Default::default()
    }
}

fn report(s: &str, o: &LiteralViewOptions) -> Value {
    let b = analyze_literal_view_source(s, 7, &BTreeSet::new(), o).unwrap();
    assert!(b.len() <= o.max_bytes);
    serde_json::from_slice(&b).unwrap()
}

fn row(v: &Value, pc: u32) -> &Value {
    v["rows"]
        .as_array()
        .unwrap()
        .iter()
        .find(|r| r["pc"] == pc)
        .unwrap()
}

fn joins(s: &str, v: &Value) {
    for r in v["rows"].as_array().unwrap() {
        let a = r["source_span"][0].as_u64().unwrap() as usize;
        let b = r["source_span"][1].as_u64().unwrap() as usize;
        assert_eq!(&s[a..b], r["source"].as_str().unwrap());
        for d in r["reads"].as_array().unwrap() {
            let a = d["source_span"][0].as_u64().unwrap() as usize;
            let b = d["source_span"][1].as_u64().unwrap() as usize;
            assert_eq!(&s[a..b], format!("r[{}]", d["register"]));
            if let Some(text) = d["literal"].as_str() {
                let a = d["literal_span"][0].as_u64().unwrap() as usize;
                let b = d["literal_span"][1].as_u64().unwrap() as usize;
                assert_eq!(&s[a..b], text);
                let id = d["definition_id"].as_str().unwrap();
                let fields = id.split(':').collect::<Vec<_>>();
                assert_eq!(fields[0], "7");
                let start = fields[1].parse::<usize>().unwrap();
                let end = fields[2].parse::<usize>().unwrap();
                assert!(start <= a && b <= end);
            }
        }
    }
}

#[test]
fn primitive_candidates_have_exact_raw_joins_and_ordered_views() {
    let s = source(&[
        (0, "r[0] = 'field';"),
        (2, "r[1] = -0;"),
        (4, "r[2] = true;"),
        (6, "r[3] = null;"),
        (8, "apply(r[9], r[8], [r[0], r[1], r[2], r[3]]);"),
    ]);
    let v = report(&s, &options());
    assert_eq!(v["schema"], "literal-view-v1");
    assert!(v["warning"].as_str().unwrap().contains("NOT executable"));
    assert_eq!(
        row(&v, 8)["view"],
        "apply(r[9], r[8], [('field'), (-0), (true), (null)]);\n};\n"
    );
    joins(&s, &v);
    let view = String::from("F[7] = function(e,a) { var r=[];\n")
        + &v["rows"]
            .as_array()
            .unwrap()
            .iter()
            .map(|r| r["view"].as_str().unwrap())
            .collect::<String>();
    let allocator = oxc_allocator::Allocator::default();
    assert!(
        oxc_parser::Parser::new(&allocator, &view, oxc_span::SourceType::default())
            .parse()
            .errors
            .is_empty()
    );
}

#[test]
fn plain_copies_retain_old_literal_after_original_register_overwrite() {
    let s = source(&[
        (0, "r[0] = 'old';"),
        (1, "r[1] = r[0];"),
        (2, "r[2] = r[1];"),
        (3, "r[0] = 'new';"),
        (4, "apply(r[9], null, [r[2], r[0]]);"),
    ]);
    let v = report(&s, &options());
    let r = row(&v, 4);
    assert!(r["view"].as_str().unwrap().contains("[('old'), ('new')]"));
    let old = r["reads"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["register"] == 2)
        .unwrap();
    assert_eq!(old["definition_pc"], 0);
    assert_eq!(old["alias_definition_ids"].as_array().unwrap().len(), 2);
    joins(&s, &v);
    let limited = report(
        &s,
        &LiteralViewOptions {
            alias_depth: 1,
            ..options()
        },
    );
    assert!(row(&limited, 4)["view"].as_str().unwrap().contains("r[2]"));
    assert_eq!(row(&limited, 4)["reads"][1]["status"], "alias_depth_limit");
}

#[test]
fn update_and_compound_assignment_targets_are_never_substituted() {
    for mutation in ["r[0]++;", "--r[0];", "r[0] += r[1];"] {
        let s = source(&[
            (0, "r[0] = 1;"),
            (1, "r[1] = 2;"),
            (2, mutation),
            (3, "return r[0];"),
        ]);
        let v = report(&s, &options());
        let r = row(&v, 2);
        assert!(r["view"].as_str().unwrap().contains("r[0]"));
        assert_eq!(r["reads"][0]["status"], "assignment_target_not_substituted");
        assert!(row(&v, 3)["view"].as_str().unwrap().contains("r[0]"));
        joins(&s, &v);
    }
}

#[test]
fn same_pc_and_conditional_writes_remain_unresolved() {
    for mutation in [
        "r[0] = 2; apply(r[9], null, [r[0]]);",
        "r[0] = 2; r[0] = 3;",
        "a && (r[0] = 2);",
        "r[0] ||= 2;",
    ] {
        let s = source(&[(0, "r[0] = 1;"), (1, mutation), (2, "return r[0];")]);
        let v = report(&s, &options());
        if mutation.contains("apply") {
            assert!(row(&v, 1)["view"].as_str().unwrap().contains("[r[0]]"));
        } else {
            assert!(row(&v, 2)["view"].as_str().unwrap().contains("r[0]"));
        }
    }
}

#[test]
fn calls_fields_objects_arrays_bigints_and_templates_are_not_evaluated() {
    for rhs in [
        "getValue()",
        "r[2].field",
        "{}",
        "[1,2]",
        "1n",
        "`name`",
        "void 0",
        "construct(r[2], null, [1,2])",
    ] {
        let s = source(&[(0, &format!("r[0] = {rhs};")), (1, "return r[0];")]);
        let v = report(&s, &options());
        assert_eq!(row(&v, 1)["source"], row(&v, 1)["view"], "{rhs}");
        assert_eq!(row(&v, 1)["reads"][0]["status"], "non_primitive_definition");
    }
}

#[test]
fn block_and_exception_entries_do_not_inherit_literals() {
    let s = source(&[(0, "r[0] = 1;"), (1, "return r[0];")]);
    let b = analyze_literal_view_source(&s, 7, &BTreeSet::from([1]), &options()).unwrap();
    let v: Value = serde_json::from_slice(&b).unwrap();
    assert_eq!(row(&v, 1)["source"], row(&v, 1)["view"]);
    let s="F[7] = function() { var r=[]; switch(a) { case 0:\n// HBC function 7, PC 0\nr[0]=1;\nbreak; case 1:\n// HBC function 7, PC 1\nreturn r[0];\n}};";
    let v = report(s, &options());
    assert_eq!(row(&v, 1)["source"], row(&v, 1)["view"]);
}

#[test]
fn raw_string_code_units_utf8_and_fake_markers_are_preserved() {
    let text = format!("r[0] = '{}'; r[1] = '\\ud800';", '\u{e9}');
    let s = source(&[
        (0, &text),
        (1, "r[2] = `\n// HBC function 999, PC 222\n`;"),
        (2, "apply(r[9], null, [r[0], r[1]]);"),
    ]);
    let v = report(&s, &options());
    assert_eq!(v["unfiltered_total"], 3);
    assert!(row(&v, 2)["view"].as_str().unwrap().contains("('\\ud800')"));
    assert!(row(&v, 2)["view"].as_str().unwrap().contains('\u{e9}'));
    joins(&s, &v);
}

#[test]
fn filters_match_literal_dependencies_and_page_by_raw_pc_ordinal() {
    let s = source(&[
        (0, "r[0] = 'needle';"),
        (1, "r[1] = 2;"),
        (2, "return r[0];"),
        (3, "r[2] = 3;"),
        (4, "apply(r[9], null, [r[0]]);"),
    ]);
    let o = LiteralViewOptions {
        matches: vec!["needle".into()],
        limit: 1,
        offset: 1,
        ..options()
    };
    let v = report(&s, &o);
    assert_eq!(v["total"], 3);
    assert_eq!(v["rows"][0]["pc"], 2);
    assert_eq!(v["next_offset"], 4);
    assert!(!v["scan_complete"].as_bool().unwrap());
    let flags = v["continuation_query"]["flags"].as_array().unwrap();
    assert!(flags.windows(2).any(|p| p[0] == "--offset" && p[1] == "4"));
    assert!(flags.contains(&serde_json::json!("--match=needle")));
    let v = report(&s, &LiteralViewOptions { offset: 4, ..o });
    assert_eq!(v["rows"][0]["pc"], 4);
    assert!(v["scan_complete"].as_bool().unwrap());
    let v = report(
        &s,
        &LiteralViewOptions {
            from_pc: Some(2),
            to_pc: Some(2),
            ..options()
        },
    );
    assert_eq!(v["rows"].as_array().unwrap().len(), 1);
    assert_eq!(v["rows"][0]["pc"], 2);
}

#[test]
fn literal_and_per_pc_read_limits_are_explicit() {
    let huge = format!("r[0] = '{}';", "x".repeat(4097));
    let s = source(&[(0, &huge), (1, "return r[0];")]);
    let v = report(&s, &options());
    assert_eq!(row(&v, 1)["reads"][0]["status"], "literal_byte_limit");
    assert_eq!(row(&v, 1)["source"], row(&v, 1)["view"]);
    let many = format!("apply(r[9], null, [{}]);", vec!["r[0]"; 140].join(","));
    let s = source(&[(0, "r[0] = 1;"), (1, &many)]);
    let v = report(&s, &options());
    assert_eq!(row(&v, 1)["read_count"], 141);
    assert_eq!(row(&v, 1)["reads_omitted"], 13);
    assert_eq!(row(&v, 1)["reads"].as_array().unwrap().len(), 128);
    assert!(row(&v, 1)["view"].as_str().unwrap().contains("r[0]"));
}

#[test]
fn text_and_json_keep_raw_evidence_and_fail_atomically_at_bounds() {
    let s = source(&[(0, "r[0] = 'name';"), (1, "return r[0];")]);
    let text = analyze_literal_view_source(&s, 7, &BTreeSet::new(), &Default::default()).unwrap();
    let text = String::from_utf8(text).unwrap();
    assert!(text.contains("return r[0]"));
    assert!(text.contains("return ('name')"));
    assert!(text.contains("NOT executable"));
    for o in [
        LiteralViewOptions {
            scan_work: 1,
            ..options()
        },
        LiteralViewOptions {
            max_bytes: 1,
            ..options()
        },
        LiteralViewOptions {
            limit: 0,
            ..options()
        },
        LiteralViewOptions {
            alias_depth: 65,
            ..options()
        },
        LiteralViewOptions {
            matches: vec![String::new()],
            ..options()
        },
        LiteralViewOptions {
            from_pc: Some(2),
            to_pc: Some(1),
            ..options()
        },
    ] {
        assert!(analyze_literal_view_source(&s, 7, &BTreeSet::new(), &o).is_err());
    }
}

#[test]
fn unsupported_mutations_and_bad_markers_fail_closed() {
    for mutation in [
        "a?.[r[0]=2];",
        "[r[0]]=[2];",
        "r.length=0;",
        "r[key]=2;",
        "(r)[0]=2;",
        "r.length++;",
        "delete (r)[0];",
    ] {
        let s = source(&[(0, "r[0] = 1;"), (1, mutation), (2, "return r[0];")]);
        assert!(
            analyze_literal_view_source(&s, 7, &BTreeSet::new(), &options()).is_err(),
            "{mutation}"
        );
    }
    for s in [
        source(&[(0, "r[0]=1;"), (0, "return r[0];")]),
        source(&[(0, "r[0]=1;")]).replace("function 7, PC", "function 8, PC"),
        "F[7] = function() { var r=[]; return 1; };".into(),
        "not valid javascript {".into(),
    ] {
        assert!(analyze_literal_view_source(&s, 7, &BTreeSet::new(), &options()).is_err());
    }
}

#[test]
fn wired_cli_errors_do_not_emit_partial_stdout() {
    let fixture =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("data/bundle_semantics.hbc");
    assert!(fixture.is_file());
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
        .args([
            "read",
            fixture.to_str().unwrap(),
            "0",
            "--json",
            "--limit",
            "2",
        ])
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let v: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v["schema"], "literal-view-v1");
    assert!(!v["rows"].as_array().unwrap().is_empty());
    for flags in [
        vec!["--max-bytes", "1"],
        vec!["--scan-work", "1"],
        vec!["--alias-depth", "65"],
    ] {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .args(["read", fixture.to_str().unwrap(), "0"])
            .args(flags)
            .output()
            .unwrap();
        assert!(!out.status.success());
        assert!(out.stdout.is_empty());
    }
}

#[test]
fn construction_cap_is_checked_before_copying_large_source_rows() {
    let huge = format!("r[0] = '{}';", "x".repeat(2 * 1024 * 1024));
    let s = source(&[(0, &huge), (1, "return r[0];")]);
    let error = analyze_literal_view_source(
        &s,
        7,
        &BTreeSet::new(),
        &LiteralViewOptions {
            max_bytes: 16_777_216,
            ..options()
        },
    )
    .unwrap_err()
    .to_string();
    assert!(error.contains("construction byte cap"), "{error}");
}

#[test]
fn continuation_matches_remain_literal_argument_tokens() {
    let s = source(&[(0, "r[0]='-text with spaces';"), (2, "return r[0];")]);
    let v = report(
        &s,
        &LiteralViewOptions {
            matches: vec!["-text with spaces".into()],
            limit: 1,
            ..options()
        },
    );
    assert!(v["continuation_query"]["flags"]
        .as_array()
        .unwrap()
        .contains(&serde_json::json!("--match=-text with spaces")));
}
