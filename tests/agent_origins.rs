// Standalone production module, deliberately not wired into the CLI here.
pub use hermes_dec_rs::{bundle, DecompilerError, DecompilerResult};
struct HbcFile;
impl HbcFile {
    fn parse_for_bundle(data: &[u8]) -> Result<hermes_dec_rs::HbcFile<'_>, String> {
        hermes_dec_rs::HbcFile::parse(data)
    }
}
#[path = "../src/cli/expression_view.rs"]
mod expression_view;
#[path = "../src/cli/origins.rs"]
mod origins;
use serde_json::Value;

fn source(cases: &str) -> String {
    format!("M[0] = ['f',0,0]; F[0] = function function_0(env,self,args,newTarget,callee,state) {{ const r = objectCreate(null); let pc = 0, caught; for (;;) {{ try {{ switch (pc) {{ {cases} default: throw new ErrorCtor('bad'); }} }} catch (error) {{ throw error; }} }} }};")
}
fn analyze(cases: &str, pc: u32) -> Value {
    serde_json::from_slice(
        &origins::analyze_source(&source(cases), 0, pc, 8, 256, 1_000_000, &[]).unwrap(),
    )
    .unwrap()
}
fn pcs(v: &Value) -> Vec<u64> {
    let mut pcs: Vec<_> = v["definitions"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["pc"].as_u64().unwrap())
        .collect();
    pcs.sort_unstable();
    pcs.dedup();
    pcs
}

fn symbol_options() -> origins::SymbolOptions {
    origins::SymbolOptions {
        function: 0,
        depth: 8,
        definition_limit: 64,
        literal_limit: 32,
        limit: 32,
        offset: 0,
        max_bytes: 1_000_000,
        scan_work: origins::DEFAULT_SYMBOL_SCAN_WORK,
        slots: vec![],
        matches: vec![],
    }
}

#[test]
fn symbols_follow_cross_block_alternatives_without_claiming_slot_values() {
    let s = source("case 0: {\n// HBC function 0, PC 0\npc = r[0] ? 2 : 4; continue; } case 2: {\n// HBC function 0, PC 2\nr[1] = 'first';\n// HBC function 0, PC 3\npc = 6; continue; } case 4: {\n// HBC function 0, PC 4\nr[1] = 'other';\n// HBC function 0, PC 5\npc = 6; continue; } case 6: {\n// HBC function 0, PC 6\nr[7].slots[9] = r[1]; return; }");
    let bytes = origins::analyze_symbols_source(&s, &symbol_options(), &[]).unwrap();
    let v: Value = serde_json::from_slice(&bytes).unwrap();
    let row = &v["rows"][0];
    assert_eq!(row["slot"], 9);
    assert_eq!(row["literal_mentions_total"], 2);
    assert_eq!(row["literal_search_complete"], true);
    assert_eq!(row["definition_count"], 2);
    assert_eq!(row["unresolved"], false);
    assert_eq!(
        row["dependencies"][0]["candidates"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    assert!(row.get("value_name").is_none());
    assert!(v["semantics"].as_str().unwrap().contains("not slot values"));
    for mention in row["literal_mentions"].as_array().unwrap() {
        let text = mention["source"]["javascript"].as_str().unwrap();
        assert!(["'first'", "'other'"].contains(&text));
        assert!(v["definitions"]
            .get(mention["definition_id"].as_str().unwrap())
            .is_some());
    }
}

#[test]
fn symbols_cursors_are_raw_store_ordinals_and_filtering_does_not_change_identity() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[7].slots[1] = 'a';\n// HBC function 0, PC 2\nr[7].slots[2] = 'b';\n// HBC function 0, PC 4\nr[7].slots[3] = 'a'; return; }");
    let o = origins::SymbolOptions {
        limit: 1,
        matches: vec!["a".into()],
        ..symbol_options()
    };
    let first: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert_eq!(first["next_offset"], 1);
    assert_eq!(first["scan_complete"], false);
    let o = origins::SymbolOptions { offset: 1, ..o };
    let second: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert_eq!(second["rows"][0]["store_ordinal"], 2);
    assert_eq!(second["rows"][0]["slot"], 3);
    assert_eq!(second["scanned"], 2);
    assert_eq!(second["scan_complete"], true);
    assert!(second["next_offset"].is_null());
}

#[test]
fn symbols_do_not_search_environment_labels_or_decode_raw_js_strings() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[7] = 'environment-label';\n// HBC function 0, PC 2\nr[1] = '\\u0061bc';\n// HBC function 0, PC 4\nr[7].slots[1] = r[1]; return; }");
    for term in ["environment-label", "abc"] {
        let o = origins::SymbolOptions {
            matches: vec![term.into()],
            ..symbol_options()
        };
        let v: Value =
            serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
        assert!(v["rows"].as_array().unwrap().is_empty(), "{term}");
    }
    let o = origins::SymbolOptions {
        matches: vec!["\\u0061".into()],
        ..symbol_options()
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert_eq!(
        v["rows"][0]["literal_mentions"][0]["source"]["javascript"],
        "'\\u0061bc'"
    );
}

#[test]
fn symbols_work_budget_continuation_survives_empty_pages_and_hostile_tokens() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[7].slots[1] = 'skip';\n// HBC function 0, PC 2\nr[7].slots[2] = 'needle;$(not-executed)'; return; }");
    let o = origins::SymbolOptions {
        scan_work: 1,
        matches: vec!["needle;$(not-executed)".into()],
        ..symbol_options()
    };
    let first: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert!(first["rows"].as_array().unwrap().is_empty());
    assert_eq!(first["query_work_used"], 1);
    assert_eq!(first["query_work_cap"], 1);
    assert_eq!(first["query_work_truncated"], true);
    assert_eq!(first["next_offset"], 1);
    let next = &first["continuation_query"];
    assert_eq!(next["input"], "INPUT");
    assert_eq!(next["function"], 0);
    assert!(next.get("shell").is_none());
    let flags = next["flags"].as_array().unwrap();
    assert!(flags
        .windows(2)
        .any(|v| v == [serde_json::json!("--offset"), serde_json::json!("1")]));
    assert!(flags
        .windows(2)
        .any(|v| v == [serde_json::json!("--scan-work"), serde_json::json!("1")]));
    assert!(flags.windows(2).any(|v| v
        == [
            serde_json::json!("--match"),
            serde_json::json!("needle;$(not-executed)")
        ]));
    let continued = origins::SymbolOptions {
        offset: 1,
        ..o.clone()
    };
    let second: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &continued, &[]).unwrap())
            .unwrap();
    assert_eq!(second["rows"][0]["store_ordinal"], 1);
    assert_eq!(second["scan_complete"], true);
    assert!(second["continuation_query"].is_null());
    let larger = origins::SymbolOptions { scan_work: 2, ..o };
    let complete: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &larger, &[]).unwrap())
            .unwrap();
    assert_eq!(complete["rows"], second["rows"]);
    assert_eq!(complete["query_work_used"], 2);
    assert_eq!(complete["scan_complete"], true);
}

#[test]
fn symbols_filtered_out_unresolved_rows_still_report_dependency_omissions() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[7] = 'absent';\n// HBC function 0, PC 2\nr[7].slots[1] = r[1]; return; }");
    let o = origins::SymbolOptions {
        matches: vec!["absent".into()],
        ..symbol_options()
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert!(v["rows"].as_array().unwrap().is_empty());
    assert_eq!(v["scan_complete"], true);
    assert_eq!(v["dependency_search_incomplete_rows"], 1);
    for scan_work in [0, origins::MAX_SYMBOL_SCAN_WORK + 1] {
        let invalid = origins::SymbolOptions {
            scan_work,
            ..o.clone()
        };
        assert!(origins::analyze_symbols_source(&s, &invalid, &[]).is_err());
    }
}

#[test]
fn symbol_filter_diagnostics_separate_identifier_spelling_from_raw_literals() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[1] = 'raw-name';\n// HBC function 0, PC 2\nr[7] = 'environment-name';\n// HBC function 0, PC 4\nr[7].slots[1] = r[1]; return; }");
    for (term, count, rows) in [
        ("raw_name", 0, 0),
        ("RAW-NAME", 1, 1),
        ("environment-name", 1, 0),
    ] {
        let o = origins::SymbolOptions {
            matches: vec![term.into()],
            ..symbol_options()
        };
        let v: Value =
            serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
        let diagnostic = &v["filter_literal_diagnostics"];
        assert_eq!(diagnostic["complete"], true);
        assert_eq!(diagnostic["terms"][0]["term"], term);
        assert_eq!(diagnostic["terms"][0]["indexed_literal_mentions"], count);
        let examples = diagnostic["terms"][0]["examples"].as_array().unwrap();
        assert_eq!(examples.len(), count as usize);
        for example in examples {
            let span = &example["source"];
            assert_eq!(
                &s[span["start"].as_u64().unwrap() as usize
                    ..span["end"].as_u64().unwrap() as usize],
                span["javascript"].as_str().unwrap()
            );
            assert_eq!(
                example["pc"],
                if term == "environment-name" { 2 } else { 0 }
            );
        }
        assert_eq!(v["rows"].as_array().unwrap().len(), rows);
        if count == 0 {
            assert_eq!(v["dependency_queries_used"], 0);
            assert_eq!(v["query_work_used"], 0);
            assert_eq!(v["dependency_queries_skipped_by_literal_filter"], 1);
            assert_eq!(v["literal_filter_excludes_all_indexed_mentions"], true);
        } else {
            assert!(v["dependency_queries_used"].as_u64().unwrap() > 0);
            assert_eq!(v["literal_filter_excludes_all_indexed_mentions"], false);
        }
        assert!(diagnostic["scope"]
            .as_str()
            .unwrap()
            .contains("not RHS dependencies"));
    }
    let v: Value = serde_json::from_slice(
        &origins::analyze_symbols_source(&s, &symbol_options(), &[]).unwrap(),
    )
    .unwrap();
    assert!(v["filter_literal_diagnostics"].is_null());
}

#[test]
fn symbol_filter_literal_examples_are_bounded_separately_from_search_counts() {
    let assignments = (0..20)
        .map(|i| format!("// HBC function 0, PC {}\nr[1] = 'repeat';\n", i * 2))
        .collect::<String>();
    let s = source(&format!(
        "case 0: {{\n{assignments}// HBC function 0, PC 42\nr[7].slots[1] = r[1]; return; }}"
    ));
    let o = origins::SymbolOptions {
        matches: vec!["repeat".into()],
        ..symbol_options()
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    let d = &v["filter_literal_diagnostics"];
    assert_eq!(d["complete"], true);
    assert_eq!(d["example_limit_per_term"], 3);
    assert_eq!(d["terms"][0]["indexed_literal_mentions"], 20);
    assert_eq!(d["terms"][0]["examples_truncated"], true);
    let examples = d["terms"][0]["examples"].as_array().unwrap();
    assert_eq!(examples.len(), 3);
    assert_eq!(
        examples
            .iter()
            .map(|e| e["pc"].as_u64().unwrap())
            .collect::<Vec<_>>(),
        vec![0, 2, 4]
    );
}

#[test]
fn symbol_filter_diagnostics_bound_work_and_keep_zero_counts_incomplete() {
    let literal = "X".repeat(1_000_000);
    let s = source(&format!(
        "case 0: {{\n// HBC function 0, PC 0\nr[7].slots[1] = '{literal}'; return; }}"
    ));
    let o = origins::SymbolOptions {
        matches: vec!["missing".into(); 64],
        ..symbol_options()
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    let d = &v["filter_literal_diagnostics"];
    assert_eq!(d["complete"], false);
    assert_eq!(d["tokens_inspected"], 0);
    assert_eq!(d["tokens_total"], 1);
    assert_eq!(v["literal_filter_excludes_all_indexed_mentions"], false);
    assert_eq!(v["dependency_queries_used"], 1);
    assert!(d["charged_work"].as_u64().unwrap() <= d["work_cap"].as_u64().unwrap());
    assert!(d["terms"]
        .as_array()
        .unwrap()
        .iter()
        .all(|r| r["indexed_literal_mentions"] == 0));
}

#[test]
fn symbols_filter_retains_matching_mentions_beyond_the_display_cap_and_marks_search_caps() {
    let literals = (0..600)
        .map(|i| format!("'label{i}'"))
        .collect::<Vec<_>>()
        .join(",");
    let s = source(&format!("case 0: {{\n// HBC function 0, PC 0\nr[1] = [{literals}];\n// HBC function 0, PC 2\nr[7].slots[1] = r[1]; return; }}"));
    let o = origins::SymbolOptions {
        literal_limit: 1,
        matches: vec!["label300".into()],
        ..symbol_options()
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    let row = &v["rows"][0];
    assert_eq!(row["literal_mentions_total"], 600);
    assert_eq!(
        row["literal_mentions"][0]["source"]["javascript"],
        "'label300'"
    );
    assert_eq!(row["literal_mentions_truncated"], true);
    assert_eq!(row["literal_search_complete"], false);
    assert_eq!(v["literal_search_truncated_rows"], 1);
    let o = origins::SymbolOptions {
        matches: vec!["label599".into()],
        ..o
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_symbols_source(&s, &o, &[]).unwrap()).unwrap();
    assert!(v["rows"].as_array().unwrap().is_empty());
    assert_eq!(v["literal_search_truncated_rows"], 1);
}

#[test]
fn expression_graphs_are_opt_in_and_join_exact_definition_spans() {
    let s = source("case 0: {\n// HBC function 0, PC 0\nr[1] = 'raw';\n// HBC function 0, PC 2\nr[1] = r[1].property;\n// HBC function 0, PC 4\nr[7].slots[0] = r[1]; return;\n}");
    let query = origins::Query {
        function: 0,
        pc: 4,
        depth: 8,
        limit: 64,
        max_bytes: 100_000,
        expressions: true,
    };
    let bytes = origins::analyze_query(&s, query, &[]).unwrap();
    assert_eq!(bytes, origins::analyze_query(&s, query, &[]).unwrap());
    let v: Value = serde_json::from_slice(&bytes).unwrap();
    let plain: Value =
        serde_json::from_slice(&origins::analyze_source(&s, 0, 4, 8, 64, 100_000, &[]).unwrap())
            .unwrap();
    assert!(plain.get("instruction_expressions").is_none());
    assert_eq!(v["demands"], plain["demands"]);
    let root = &v["instruction_expressions"][0];
    assert_eq!(root["semantics"], "syntax_only");
    assert_eq!(v["expression_source"]["offset_unit"], "utf8_bytes");
    assert_eq!(v["expression_source"]["source_bytes"], s.len());
    assert!(s.starts_with(
        v["expression_source"]["raw_fragment_prefix"]
            .as_str()
            .unwrap()
    ));
    assert!(plain.get("expression_source").is_none());
    for d in v["definitions"].as_array().unwrap() {
        let view = &d["expression"];
        assert_eq!(view["source"]["start"], d["source"]["start"]);
        assert_eq!(view["source"]["end"], d["source"]["end"]);
        assert_eq!(view["semantics"], "syntax_only");
        assert!(!view["nodes"].as_array().unwrap().is_empty());
    }
    let small = origins::Query {
        max_bytes: 128,
        ..query
    };
    assert!(origins::analyze_query(&s, small, &[]).is_err());
}

#[test]
fn selected_instruction_expression_count_is_bounded_and_explicit() {
    let statements = std::iter::repeat_n("r[7].slots[0] = 'raw';", 40).collect::<String>();
    let s = source(&format!(
        "case 0: {{\n// HBC function 0, PC 0\n{statements} return;\n}}"
    ));
    let query = origins::Query {
        function: 0,
        pc: 0,
        depth: 0,
        limit: 64,
        max_bytes: 1_000_000,
        expressions: true,
    };
    let v: Value =
        serde_json::from_slice(&origins::analyze_query(&s, query, &[]).unwrap()).unwrap();
    assert_eq!(v["instruction_expressions"].as_array().unwrap().len(), 32);
    assert_eq!(v["instruction_expressions_total"], 40);
    assert_eq!(v["instruction_expressions_omitted"], 8);
    assert_eq!(v["expressions_truncated"], true);
    assert_eq!(v["truncated"], true);
}
const DIAMOND: &str = "case 0: {\n// HBC function 0, PC 0\nr[1] = 'early';\n// HBC function 0, PC 2\npc = (r[9]) ? 10 : 20; continue;\n} case 10: {\n// HBC function 0, PC 10\nr[2] = r[1];\n// HBC function 0, PC 12\npc = 30; continue;\n} case 20: {\n// HBC function 0, PC 20\nr[2] = 'other'; pc = 30; continue;\n} case 30: {\n// HBC function 0, PC 30\nreturn r[2];\n}";

#[test]
fn diamond_has_both_candidates_and_early_alias() {
    let v = analyze(DIAMOND, 30);
    assert_eq!(pcs(&v), vec![0, 10, 20]);
    assert_eq!(v["demands"][0]["candidates"].as_array().unwrap().len(), 2);
    assert_eq!(v["unresolved"], false);
    assert_eq!(v["demands"][0]["cycle"], false);
}

#[test]
fn default_query_budget_does_not_starve_later_root_reads() {
    let mut cases = String::from("case 0: {\n// HBC function 0, PC 0\nr[7] = environment(env, 1); r[1] = 'early'; pc = 1; continue;\n}");
    for pc in 1..600 {
        cases.push_str(&format!(
            "case {pc}: {{\n// HBC function 0, PC {pc}\npc = {}; continue;\n}}",
            pc + 1
        ));
    }
    cases.push_str("case 600: {\n// HBC function 0, PC 600\nr[7].slots[0] = r[1]; return;\n}");
    let v: Value = serde_json::from_slice(
        &origins::analyze_source(&source(&cases), 0, 600, 8, 64, 100000, &[]).unwrap(),
    )
    .unwrap();
    let roots: Vec<_> = v["demands"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["owner_definition"].is_null())
        .collect();
    assert_eq!(roots.len(), 2);
    for demand in roots {
        assert_eq!(demand["candidates"].as_array().unwrap().len(), 1);
        assert_eq!(demand["truncated"], false);
        assert_eq!(demand["unresolved"], false);
    }
    assert_eq!(v["limits"]["query_work"], 8192);
}
#[test]
fn fallthrough_uses_source_case_order_and_overwrite_kills() {
    let v = analyze("case 0: {\n// HBC function 0, PC 0\nr[1] = 1;\n} case 20: {\n// HBC function 0, PC 20\nr[1] = 2;\n// HBC function 0, PC 22\nreturn r[1];\n} case 10: {\n// HBC function 0, PC 10\nr[1] = 3; return r[1];\n}",22);
    assert_eq!(pcs(&v), vec![20]);
    assert_eq!(v["normal_edges"], serde_json::json!([[0, 20]]));
}
#[test]
fn cycle_is_safe_and_can_find_loop_end_write() {
    let v = analyze("case 0: {\n// HBC function 0, PC 0\npc = 10; continue;\n} case 10: {\n// HBC function 0, PC 10\nr[2] = r[1];\n// HBC function 0, PC 12\nr[1] = 7; pc = 10; continue;\n}",10);
    assert!(pcs(&v).contains(&12));
    assert_eq!(v["demands"][0]["cycle"], true);
    assert_eq!(v["unresolved"], true);
}
#[test]
fn same_pc_writes_are_candidates_not_ordered_values() {
    let v = analyze("case 0: {\n// HBC function 0, PC 0\nr[1] = 4;\n// HBC function 0, PC 2\nr[1] = 1; r[1] = 2; return r[1];\n}",2);
    assert_eq!(v["demands"][0]["same_pc_ambiguity"], true);
    assert_eq!(v["demands"][0]["candidates"].as_array().unwrap().len(), 3);
}

#[test]
fn assignment_rhs_cannot_reach_its_enclosing_destination_write() {
    let v = analyze("case 0: {\n// HBC function 0, PC 0\nr[1] = 'earlier';\n// HBC function 0, PC 2\nr[1] = r[1].property;\n// HBC function 0, PC 4\nreturn r[1];\n}", 4);
    assert_eq!(pcs(&v), vec![0, 2]);
    let rhs = v["demands"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["pc"] == 2)
        .unwrap();
    assert_eq!(rhs["same_pc_ambiguity"], false);
    let candidates = rhs["candidates"].as_array().unwrap();
    assert_eq!(candidates.len(), 1);
    assert_eq!(
        v["definitions"][candidates[0].as_u64().unwrap() as usize]["pc"],
        0
    );
}
#[test]
fn indirect_pc_and_nested_switch_are_unknown() {
    for code in [
        "pc = r[1]; continue;",
        "switch(r[1]) { case 2: r[2] = 9; } return r[2];",
        "foo(pc = 20); return r[1];",
    ] {
        let v = analyze(&format!("case 0: {{\n// HBC function 0, PC 0\nr[1] = 7;\n// HBC function 0, PC 2\n{code}\n}}"),2);
        assert_eq!(v["unresolved"], true);
        assert!(!v["unknown"].as_array().unwrap().is_empty());
    }
}
#[test]
fn exception_metadata_and_generator_stop_candidates() {
    let s = source(DIAMOND);
    for (s, exceptions) in [
        (s.clone(), vec![(0, 30, 30)]),
        (
            s.replace(
                "const r = objectCreate(null); let pc = 0, caught;",
                "const r = state.r; let pc = state.pc, caught = state.caught;",
            ),
            vec![],
        ),
    ] {
        let v: Value = serde_json::from_slice(
            &origins::analyze_source(&s, 0, 30, 8, 100, 100000, &exceptions).unwrap(),
        )
        .unwrap();
        assert_eq!(v["unresolved"], true);
        assert!(pcs(&v).is_empty());
    }
    let s = s.replace(
        "throw error;",
        "if (pc < 30) { pc = 30; continue; } throw error;",
    );
    let v: Value =
        serde_json::from_slice(&origins::analyze_source(&s, 0, 30, 8, 100, 100000, &[]).unwrap())
            .unwrap();
    assert_eq!(v["unknown"][0], "exception_flow");
}
#[test]
fn fake_markers_and_utf8_spans() {
    let cases = "case 0: {\n// HBC function 0, PC 0\nr[1] = `\n// HBC function 0, PC 999\n\u{00e9}\u{1f642}`;\n// HBC function 0, PC 2\nreturn r[1];\n}";
    let s = source(cases);
    let v = analyze(cases, 2);
    let d = &v["definitions"][0]["source"];
    assert_eq!(
        &s[d["start"].as_u64().unwrap() as usize..d["end"].as_u64().unwrap() as usize],
        d["javascript"].as_str().unwrap()
    );
    assert!(origins::analyze_source(&s, 0, 999, 1, 20, 10000, &[]).is_err());
}
#[test]
fn budgets_depth_and_errors() {
    let s = source(DIAMOND);
    for (depth, limit, bytes) in [
        (65, 100, 10000),
        (1, 0, 10000),
        (1, 4097, 10000),
        (1, 10, 0),
        (1, 10, 16_777_217),
        (1, 10, 1),
    ] {
        assert!(origins::analyze_source(&s, 0, 30, depth, limit, bytes, &[]).is_err());
    }
    let v: Value =
        serde_json::from_slice(&origins::analyze_source(&s, 0, 30, 0, 1, 10000, &[]).unwrap())
            .unwrap();
    assert_eq!(v["truncated"], true);
    assert_eq!(v["definitions"].as_array().unwrap().len(), 1);
    assert!(origins::analyze_source("not JS !", 0, 0, 1, 10, 10000, &[]).is_err());
    assert!(
        origins::analyze_source("switch(pc) {case 0: break;}", 0, 0, 1, 10, 10000, &[]).is_err()
    );
    assert!(origins::report(
        std::path::Path::new("/nonexistent-origins-input"),
        origins::Query {
            function: 0,
            pc: 0,
            depth: 1,
            limit: 10,
            max_bytes: 10000,
            expressions: false
        }
    )
    .is_err());
    assert!(origins::run(
        std::path::Path::new("/nonexistent-origins-input"),
        origins::Query {
            function: 0,
            pc: 0,
            depth: 65,
            limit: 10,
            max_bytes: 10000,
            expressions: false
        }
    )
    .is_err());
}

#[test]
fn displayed_edges_are_bounded_and_counted() {
    let mut cases = String::new();
    for pc in 0..300 {
        cases.push_str(&format!(
            "case {pc}: {{\n// HBC function 0, PC {pc}\npc = {}; continue;\n}}",
            pc + 1
        ));
    }
    cases.push_str("case 300: {\n// HBC function 0, PC 300\nreturn 0;\n}");
    let s = source(&cases);
    let v: Value =
        serde_json::from_slice(&origins::analyze_source(&s, 0, 300, 1, 100, 32768, &[]).unwrap())
            .unwrap();
    assert_eq!(v["schema_version"], 1);
    assert_eq!(v["normal_edges_total"], 300);
    assert_eq!(v["normal_edges_returned"], 256);
    assert_eq!(v["normal_edges_truncated"], true);
    assert_eq!(v["normal_edges"].as_array().unwrap().len(), 256);
    assert_eq!(v["truncated"], true);
    assert!(origins::analyze_source(&s, 0, 300, 1, 100, 128, &[]).is_err());
}

#[test]
fn unsupported_prelude_and_shadowing_fail_closed() {
    let s = source(DIAMOND);
    for s in [
        s.replace("let pc = 0, caught;", "let pc = args[0], caught;"),
        s.replace("for (;;) {", "return r[2]; for (;;) {"),
        s.replace("const r = objectCreate(null)", "const r = args[0]"),
    ] {
        assert!(origins::analyze_source(&s, 0, 30, 2, 10, 10000, &[]).is_err());
    }
    let v = analyze("case 0: {\n// HBC function 0, PC 0\nlet pc = 2; r[1] = 7;\n// HBC function 0, PC 2\nreturn r[1];\n}",2);
    assert_eq!(v["unresolved"], true);
    assert!(pcs(&v).is_empty());
    let s = source(DIAMOND).replace(
        "default: throw new ErrorCtor('bad');",
        "default: pc = 30; continue;",
    );
    assert!(origins::analyze_source(&s, 0, 30, 2, 10, 10000, &[]).is_err());
}

#[test]
fn public_fixture_export_and_report_are_compatible() {
    let input = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("data/simple_arithmetic.hbc");
    let v: Value = serde_json::from_slice(
        &origins::report(
            &input,
            origins::Query {
                function: 0,
                pc: 0,
                depth: 2,
                limit: 128,
                max_bytes: 32768,
                expressions: false,
            },
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(v["schema_version"], 1);
    assert_eq!(v["function"], 0);
    assert_eq!(v["pc"], 0);
}

#[test]
fn source_block_and_control_work_caps_fail_closed() {
    let too_big = " ".repeat(64 * 1024 * 1024 + 1);
    let error = origins::analyze_source(&too_big, 0, 0, 1, 10, 10000, &[]).unwrap_err();
    assert!(error.to_string().contains("source byte cap"));
    let mut cases = String::new();
    for pc in 0..4097 {
        cases.push_str(&format!(
            "case {pc}: {{\n// HBC function 0, PC {pc}\nreturn 0;\n}}"
        ));
    }
    let error = origins::analyze_source(&source(&cases), 0, 0, 1, 10, 10000, &[]).unwrap_err();
    assert!(error.to_string().contains("block cap"));
    let cases = format!(
        "case 0: {{\n// HBC function 0, PC 0\n{}return 0;\n}}",
        "0;".repeat(1048577)
    );
    let error = origins::analyze_source(&source(&cases), 0, 0, 1, 10, 10000, &[]).unwrap_err();
    assert!(error.to_string().contains("control indexing work cap"));
}
