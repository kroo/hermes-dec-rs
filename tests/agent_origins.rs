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
