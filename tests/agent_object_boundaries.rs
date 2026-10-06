//! Authored-source boundaries; no application bytecode or JavaScript execution.
use hermes_dec_rs::cli::sites::{analyze_objects_source, ObjectOptions};
use serde_json::Value;
use std::collections::BTreeSet;
use std::io::Write;
use std::process::Command;

const FUNCTION: u32 = 7;
const CHILD_MODE: &str = "AGENT_OBJECT_BOUNDARIES_SCAN_CHILD";

fn source(instructions: &[(u32, &str)]) -> String {
    let mut source = String::from("F[7] = function() { var r = [];\n");
    for (pc, statement) in instructions {
        source.push_str(&format!("// HBC function 7, PC {pc}\n{statement}\n"));
    }
    source.push_str("};\n");
    source
}

fn report(source: &str, origin_pc: Option<u32>, options: &ObjectOptions) -> Value {
    let bytes = analyze_objects_source(source, FUNCTION, origin_pc, &BTreeSet::new(), options)
        .expect("supported authored exporter source");
    serde_json::from_slice(&bytes).expect("complete objects JSON")
}

fn rows(report: &Value) -> &[Value] {
    report["sites"].as_array().expect("site array")
}

fn assert_rejected(source: &str) {
    assert!(
        analyze_objects_source(
            source,
            FUNCTION,
            None,
            &BTreeSet::new(),
            &ObjectOptions::default(),
        )
        .is_err(),
        "accepted unsupported boundary: {source}"
    );
}

#[test]
fn conditional_updates_never_supply_terminal_definitions() {
    for mutation in [
        "a && r[0]++;",
        "a || --r[0];",
        "a ? r[0]++ : 0;",
        "a ?? ++r[0];",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, mutation),
            (2, "put(r[0], 'key', 0, false, false);"),
        ]);
        let discovery = report(&js, None, &ObjectOptions::default());
        assert_eq!(rows(&discovery).len(), 1);
        let origin = &rows(&discovery)[0]["object_origin"];
        assert_eq!(origin["status"], "unresolved_ambiguous_source_write");
        assert!(origin["terminal_definition"].is_null(), "{mutation}");
        assert!(discovery["object_queries"].as_array().unwrap().is_empty());
        for pc in [0, 1] {
            assert!(rows(&report(&js, Some(pc), &ObjectOptions::default())).is_empty());
        }
    }
}

#[test]
fn logical_register_assignments_never_supply_terminal_definitions() {
    for mutation in ["r[0] &&= r[1];", "r[0] ||= r[1];", "r[0] ??= r[1];"] {
        let js = source(&[
            (0, "r[0] = {}; r[1] = {};"),
            (1, mutation),
            (2, "own(r[0], 'key', 0, true);"),
        ]);
        let discovery = report(&js, None, &ObjectOptions::default());
        assert_eq!(rows(&discovery).len(), 1);
        let origin = &rows(&discovery)[0]["object_origin"];
        assert_eq!(origin["status"], "unresolved_ambiguous_source_write");
        assert!(origin["terminal_definition"].is_null(), "{mutation}");
        assert!(origin["alias_definition_ids"]
            .as_array()
            .unwrap()
            .is_empty());
        for pc in [0, 1] {
            assert!(rows(&report(&js, Some(pc), &ObjectOptions::default())).is_empty());
        }
    }
}

#[test]
fn optional_chains_cannot_publish_conditional_register_origins() {
    for mutation in [
        "a?.[r[0] = {}];",
        "a?.[r[0]++];",
        "a?.b[r[0] = {}];",
        "a?.b.c[r[0] = {}];",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, mutation),
            (2, "own(r[0], 'key', 0, true);"),
        ]);
        assert_rejected(&js);
    }
}

#[test]
fn destructuring_cannot_hide_register_or_binding_mutations() {
    for mutation in [
        "[r[0]] = [{}];",
        "[r] = [[]];",
        "({key: r[0]} = {key: {}});",
        "({key: r} = {key: []});",
        "[a, ...r[0]] = [];",
        "({key: [r[0]]} = a);",
        "[a = (r[0] = {})] = [];",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, mutation),
            (2, "own(r[0], 'key', 0, true);"),
        ]);
        assert_rejected(&js);
    }
}

#[test]
fn register_binding_updates_and_slot_deletions_fail_closed() {
    for mutation in [
        "r++;",
        "--r;",
        "delete r[0];",
        "delete (r[0]);",
        "delete (r)[0];",
        "a && delete r[0];",
        "r.length = 0;",
        "r[key] = 0;",
        "(r)[0] = 0;",
        "r.length++;",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, mutation),
            (2, "own(r[0], 'key', 0, true);"),
        ]);
        assert_rejected(&js);
    }
}

#[test]
fn application_property_deletion_does_not_delete_register_definitions() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "delete r[0].key;"),
        (2, "own(r[0], 'key', 0, true);"),
    ]);
    let result = report(&js, Some(0), &ObjectOptions::default());
    assert_eq!(rows(&result).len(), 1);
    assert_eq!(rows(&result)[0]["object_origin"]["status"], "resolved");
}

#[test]
fn unconditional_updates_remain_source_definitions_not_aliases() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "r[0]++;"),
        (2, "own(r[0], 'key', 0, true);"),
    ]);
    let result = report(&js, Some(1), &ObjectOptions::default());
    assert_eq!(rows(&result).len(), 1);
    let origin = &rows(&result)[0]["object_origin"];
    assert_eq!(origin["status"], "resolved");
    let id = origin["terminal_definition"].as_str().unwrap();
    assert_eq!(result["definitions"][id]["source_pc"], 1);
    assert_eq!(result["definitions"][id]["source"]["javascript"], "r[0]++");
    assert!(origin["alias_definition_ids"]
        .as_array()
        .unwrap()
        .is_empty());
}

#[test]
fn definitions_crossing_pc_markers_fail_even_if_filtered_out() {
    for rhs in ["{}", "r[1]"] {
        let js = format!(
            "F[7] = function() {{ var r = [];\n\
             // HBC function 7, PC 0\nr[1] = {{}};\n\
             // HBC function 7, PC 1\nr[0] = (\n\
             // HBC function 7, PC 2\n{rhs});\n\
             // HBC function 7, PC 3\nput(r[0], 'key', 0, false, false);\n}};\n"
        );
        for matches in [vec![], vec!["absent".to_owned()]] {
            let options = ObjectOptions {
                matches,
                ..ObjectOptions::default()
            };
            let error = analyze_objects_source(&js, FUNCTION, None, &BTreeSet::new(), &options)
                .unwrap_err()
                .to_string();
            assert!(error.contains("definition crosses PC boundary"), "{error}");
        }
    }
}

fn interleaved_source() -> String {
    source(&[
        (0, "r[0] = {};"),
        (1, "r[1] = {};"),
        (2, "put(r[0], 'skip', 0, false, false);"),
        (3, "own(r[1], 'needle', 0, true);"),
        (4, "put(r[0], 'needle', 1, false, false);"),
        (5, "own(r[0], 'skip', 0, true);"),
        (6, "own(r[0], 'needle', 2, true);"),
        (7, "put(r[1], 'needle', 0, false, false);"),
        (8, "put(r[0], 'skip', 0, false, false);"),
        (9, "own(r[0], 'needle', 3, true);"),
    ])
}

#[test]
fn raw_cursor_counts_source_and_origin_filtered_stores() {
    let js = interleaved_source();
    let mut options = ObjectOptions {
        matches: vec!["needle".into()],
        offset: 1,
        limit: 1,
        alias_depth: 4,
        depth: 2,
        ..ObjectOptions::default()
    };
    for (pc, next) in [(4, Some(4usize)), (6, Some(7)), (9, None)] {
        let result = report(&js, Some(0), &options);
        assert_eq!(rows(&result).len(), 1);
        assert_eq!(rows(&result)[0]["pc"], pc);
        assert_eq!(result["unfiltered_total"], 8);
        assert_eq!(result["total"], 3);
        assert_eq!(result["offset"], options.offset);
        assert_eq!(result["next_offset"], serde_json::json!(next));
        if let Some(next) = next {
            let query = &result["continuation_query"];
            assert_eq!(query["command"], "objects");
            assert_eq!(query["function"], FUNCTION);
            assert_eq!(query["origin_pc"], 0);
            let flags = query["flags"].as_array().unwrap();
            for (flag, value) in [
                ("--offset", next.to_string()),
                ("--match", "needle".to_owned()),
                ("--alias-depth", options.alias_depth.to_string()),
                ("--depth", options.depth.to_string()),
                ("--limit", options.limit.to_string()),
                ("--max-bytes", options.max_bytes.to_string()),
                ("--scan-work", options.scan_work.to_string()),
            ] {
                assert!(flags
                    .windows(2)
                    .any(|pair| pair[0] == flag && pair[1] == value));
            }
            options.offset = next;
        } else {
            assert!(result["continuation_query"].is_null());
        }
    }
    let discovery = report(
        &js,
        None,
        &ObjectOptions {
            offset: 2,
            limit: 1,
            matches: vec!["needle".into()],
            ..ObjectOptions::default()
        },
    );
    assert_eq!(rows(&discovery)[0]["pc"], 4);
}

#[test]
fn same_pc_distinct_definitions_have_explicit_union_navigation() {
    let js = source(&[
        (0, "r[0] = {}; r[1] = {};"),
        (1, "put(r[0], 'first', 0, false, false);"),
        (2, "own(r[1], 'second', 0, true);"),
    ]);
    let result = report(&js, Some(0), &ObjectOptions::default());
    assert_eq!(result["origin_scope"], "pc_wide_definition_union");
    assert_eq!(rows(&result).len(), 2);
    let ids: BTreeSet<_> = rows(&result)
        .iter()
        .map(|row| {
            assert_eq!(row["object_origin"]["status"], "resolved");
            row["object_origin"]["terminal_definition"]
                .as_str()
                .unwrap()
        })
        .collect();
    assert_eq!(
        ids.len(),
        2,
        "distinct source definitions retain distinct IDs"
    );
    let registers: BTreeSet<_> = ids
        .iter()
        .map(|id| {
            let definition = &result["definitions"][*id];
            assert_eq!(definition["source_pc"], 0);
            definition["defines_register"].as_u64().unwrap()
        })
        .collect();
    assert_eq!(registers, BTreeSet::from([0, 1]));
    let queries = result["object_queries"].as_array().unwrap();
    assert_eq!(queries.len(), 1);
    assert_eq!(queries[0]["origin_pc"], 0);
    assert_eq!(queries[0]["origin_scope"], "pc_wide_definition_union");
    let query_ids: BTreeSet<_> = queries[0]["terminal_definition_ids"]
        .as_array()
        .unwrap()
        .iter()
        .map(|id| id.as_str().unwrap())
        .collect();
    assert_eq!(query_ids, ids);
}

#[test]
fn root_block_and_catch_register_shadowing_is_rejected() {
    let root_parameter = source(&[(0, "r[0] = {};"), (1, "own(r[0], 'key', 0, true);")]).replacen(
        "function()",
        "function(r)",
        1,
    );
    assert_rejected(&root_parameter);
    for shadow in [
        "{ let r = []; r[0] = {}; }",
        "{ const r = []; r[0] = {}; }",
        "{ var r = []; r[0] = {}; }",
        "try { throw 0; } catch (r) { r[0] = {}; }",
        "try { throw 0; } catch ({ r }) { r[0] = {}; }",
    ] {
        assert_rejected(&source(&[
            (0, "r[0] = {};"),
            (1, shadow),
            (2, "put(r[0], 'key', 0, false, false);"),
        ]));
    }
}

#[test]
fn only_one_non_nested_root_function_is_supported() {
    let valid = source(&[(0, "r[0] = {};"), (1, "own(r[0], 'key', 0, true);")]);
    assert_eq!(
        rows(&report(&valid, None, &ObjectOptions::default())).len(),
        1
    );
    assert_rejected(&valid.replacen("function()", "() =>", 1));
    assert_rejected(&format!("{valid}\nfunction extra() {{}}"));
    assert_rejected("var r = [];\n// HBC function 7, PC 0\nr[0] = {};\n");
    assert_rejected(&source(&[
        (0, "function nested() { r[0] = {}; }"),
        (1, "own(r[0], 'key', 0, true);"),
    ]));
    assert_rejected(&format!("{valid}\nr[0] = {{}};"));
}

#[test]
fn finite_structured_loops_do_not_supply_local_origins() {
    for statement in [
        "while (a) { r[0] = {}; }",
        "do { r[0] = {}; } while (a);",
        "for (let i = 0; i < 2; i++) { r[0] = {}; }",
        "for (r[0] of values) {}",
        "for (r[0] in values) {}",
    ] {
        assert_rejected(&source(&[
            (0, "r[0] = {};"),
            (1, statement),
            (2, "put(r[0], 'key', 0, false, false);"),
        ]));
    }
}

#[test]
fn structured_catch_and_switch_joins_do_not_supply_local_origins() {
    for statement in [
        "try { mayThrow(); } catch (error) { r[0] = {}; }",
        "try { r[0] = {}; } finally { cleanup(); }",
        "switch (tag) { case 0: r[0] = {}; break; default: r[1] = {}; }",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, statement),
            (2, "own(r[0], 'key', 0, true);"),
        ]);
        assert_join_unresolved_or_rejected(&js, 2);
    }
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "try {"),
        (2, "r[0] = {};"),
        (3, "} catch (error) {"),
        (4, "r[0] = {};"),
        (5, "}"),
        (6, "own(r[0], 'key', 0, true);"),
    ]);
    assert_join_unresolved_or_rejected(&js, 6);
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "switch (tag) { case 0:"),
        (2, "r[0] = {}; break;"),
        (3, "default:"),
        (4, "r[0] = {};"),
        (5, "}"),
        (6, "own(r[0], 'key', 0, true);"),
    ]);
    assert_join_unresolved_or_rejected(&js, 6);
}

fn assert_join_unresolved_or_rejected(js: &str, site_pc: u32) {
    // Either fail closed or retain the store with no cross-join definition guess.
    let Ok(bytes) = analyze_objects_source(
        js,
        FUNCTION,
        None,
        &BTreeSet::new(),
        &ObjectOptions::default(),
    ) else {
        return;
    };
    let result: Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(rows(&result).len(), 1);
    assert_eq!(rows(&result)[0]["pc"], site_pc);
    let origin = &rows(&result)[0]["object_origin"];
    assert!(
        origin["status"]
            .as_str()
            .unwrap()
            .starts_with("unresolved_"),
        "cross-join origin: {origin}"
    );
    assert!(origin["terminal_definition"].is_null());
    assert!(result["object_queries"].as_array().unwrap().is_empty());
}

#[test]
fn exporter_dispatcher_scaffold_preserves_same_case_origins_only() {
    let js = "F[7] = function() { var r = []; let pc = 0;\n\
              for (;;) { try { switch (pc) { case 0: {\n\
              // HBC function 7, PC 0\nr[0] = {};\n\
              // HBC function 7, PC 1\nput(r[0], 'same', 0, false, false);\n\
              pc = 2; continue;\n} case 2: {\n\
              // HBC function 7, PC 2\nown(r[0], 'external', 0, true);\n\
              return;\n} default: throw 0; } } catch (error) { throw error; } } };\n";
    let result = report(js, None, &ObjectOptions::default());
    assert_eq!(rows(&result).len(), 2);
    assert_eq!(rows(&result)[0]["object_origin"]["status"], "resolved");
    assert_eq!(
        rows(&result)[1]["object_origin"]["status"],
        "unresolved_block_entry_or_external"
    );
    assert!(rows(&result)[1]["object_origin"]["terminal_definition"].is_null());
}

fn dag_source() -> String {
    let mut instructions = vec![(0, "r[0] = {};".to_owned()), (1, "r[1] = 1;".to_owned())];
    for register in 2..=32 {
        let dependencies = (1..register)
            .map(|previous| format!("r[{previous}]"))
            .collect::<Vec<_>>()
            .join(", ");
        instructions.push((register, format!("r[{register}] = make({dependencies});")));
    }
    instructions.push((33, "put(r[0], 'dag', r[32], false, false);".to_owned()));
    source(
        &instructions
            .iter()
            .map(|(pc, statement)| (*pc, statement.as_str()))
            .collect::<Vec<_>>(),
    )
}

#[test]
fn no_match_operand_dag_consumes_scan_budget_before_output() {
    let js = dag_source();
    let options = ObjectOptions {
        depth: 8,
        limit: 1,
        max_bytes: 16_777_216,
        ..ObjectOptions::default()
    };
    let complete = report(&js, None, &options);
    assert_eq!(rows(&complete).len(), 1);
    assert_eq!(complete["filter_work_used"], 0);
    assert!(complete["matches"].as_array().unwrap().is_empty());
    assert!(complete["work_used"].as_u64().unwrap() > 64);
    let value_operand = rows(&complete)[0]["operands"]
        .as_array()
        .unwrap()
        .iter()
        .find(|operand| operand["role"] == "value")
        .unwrap();
    assert!(value_operand["nodes"].as_array().unwrap().len() >= 8);
    let tight = ObjectOptions {
        scan_work: 64,
        ..options
    };
    let error = analyze_objects_source(&js, FUNCTION, None, &BTreeSet::new(), &tight)
        .unwrap_err()
        .to_string();
    assert!(
        error.contains("scan_work cap exceeded before stdout"),
        "{error}"
    );
}

// Buffer the authored-source API result before emission, as the CLI wrapper does.
#[test]
fn authored_scan_budget_stdout_child() {
    let Ok(mode) = std::env::var(CHILD_MODE) else {
        return;
    };
    let options = ObjectOptions {
        depth: 8,
        limit: 1,
        max_bytes: 16_777_216,
        scan_work: if mode == "failure" { 64 } else { 2_097_152 },
        ..ObjectOptions::default()
    };
    match analyze_objects_source(&dag_source(), FUNCTION, None, &BTreeSet::new(), &options) {
        Ok(bytes) => std::io::stdout().lock().write_all(&bytes).unwrap(),
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(17);
        }
    }
}

#[test]
fn scan_budget_subprocess_failure_emits_no_partial_report() {
    for mode in ["success", "failure"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "authored_scan_budget_stdout_child",
                "--nocapture",
            ])
            .env(CHILD_MODE, mode)
            .output()
            .unwrap();
        if mode == "failure" {
            assert_eq!(output.status.code(), Some(17));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("scan_work cap exceeded before stdout"),
                "{stderr}"
            );
            assert!(!output.stdout.contains(&b'{'), "partial report: {output:?}");
        } else {
            assert!(output.status.success(), "{output:?}");
            let expected = analyze_objects_source(
                &dag_source(),
                FUNCTION,
                None,
                &BTreeSet::new(),
                &ObjectOptions {
                    depth: 8,
                    limit: 1,
                    max_bytes: 16_777_216,
                    ..ObjectOptions::default()
                },
            )
            .unwrap();
            let start = output.stdout.iter().position(|&byte| byte == b'{').unwrap();
            assert_eq!(
                output.stdout.get(start..start + expected.len()),
                Some(expected.as_slice())
            );
            assert!(!output.stdout[start + expected.len()..].contains(&b'{'));
        }
    }
}
