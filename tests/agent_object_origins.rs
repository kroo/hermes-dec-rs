//! Source-local candidate regressions, not assertions about evaluated object identity.
//! All source is authored here; no bytecode, application fixtures, or execution is used.
use hermes_dec_rs::cli::sites::{analyze_objects_source, ObjectOptions};
use serde_json::Value;
use std::collections::BTreeSet;

const FUNCTION: u32 = 7;

fn source(instructions: &[(u32, &str)]) -> String {
    let mut result = format!("F[{FUNCTION}] = function(e, a) {{\nvar r = [];\n");
    for (pc, javascript) in instructions {
        result.push_str(&format!(
            "// HBC function {FUNCTION}, PC {pc}\n{javascript}\n"
        ));
    }
    result.push_str("};\n");
    result
}

fn report(source: &str, pc: Option<u32>, options: &ObjectOptions) -> Value {
    let bytes = analyze_objects_source(source, FUNCTION, pc, &BTreeSet::new(), options)
        .expect("authored source must be analyzed by the production API");
    assert!(bytes.len() <= options.max_bytes);
    serde_json::from_slice(&bytes).expect("complete JSON report")
}

fn sites(report: &Value) -> &[Value] {
    report["sites"].as_array().expect("compact sites")
}

fn origin(site: &Value) -> &Value {
    &site["object_origin"]
}

fn terminal<'a>(report: &'a Value, site: &Value) -> &'a Value {
    assert_eq!(origin(site)["status"], "resolved");
    let id = origin(site)["terminal_definition"]
        .as_str()
        .expect("terminal is a shared definition ID, not an inline heap value");
    report["definitions"]
        .get(id)
        .expect("shared definition exists")
}

fn site_at(report: &Value, pc: u32) -> &Value {
    sites(report)
        .iter()
        .find(|site| site["pc"] == pc)
        .expect("property site at authored PC")
}

fn tokens(query: &Value) -> Vec<&str> {
    query
        .as_array()
        .expect("structured command tokens, never shell text")
        .iter()
        .map(|token| token.as_str().expect("each command token is a string"))
        .collect()
}

#[test]
fn production_defaults_match_the_bounded_contract() {
    let options = ObjectOptions::default();
    assert_eq!(options.alias_depth, 16);
    assert_eq!(options.depth, 3);
    assert_eq!(options.limit, 5);
    assert_eq!(options.offset, 0);
    assert_eq!(options.max_bytes, 100_000);
    assert_eq!(options.scan_work, 2_097_152);
    assert!(options.matches.is_empty());
}

#[test]
fn discovery_follow_up_clears_search_and_offset_to_find_siblings() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "put(r[0], 'k0', 'needle', false, e);"),
        (2, "own(r[0], 'k1', 'needle', true);"),
        (3, "r[0].k2 = 2;"),
    ]);
    let options = ObjectOptions {
        matches: vec!["needle".into()],
        offset: 1,
        ..ObjectOptions::default()
    };
    let discovery = report(&js, None, &options);
    assert_eq!(discovery["schema"], "objects-v1");
    assert_eq!(discovery["function"], FUNCTION);
    assert!(discovery["origin_pc"].is_null());
    assert_eq!(sites(&discovery).len(), 1);
    assert_eq!(terminal(&discovery, &sites(&discovery)[0])["source_pc"], 0);
    let queries = discovery["object_queries"].as_array().expect("follow-ups");
    assert_eq!(queries.len(), 1, "one command per distinct returned origin");
    assert_eq!(queries[0]["command"], "objects");
    assert_eq!(queries[0]["function"], FUNCTION);
    assert_eq!(queries[0]["origin_pc"], 0);
    let command = tokens(&queries[0]["flags"]);
    assert!(!command.contains(&"--match"));
    assert!(command.windows(2).any(|pair| pair == ["--offset", "0"]));
    let siblings = report(
        &js,
        Some(0),
        &ObjectOptions {
            offset: 0,
            matches: vec![],
            ..options
        },
    );
    assert_eq!(sites(&siblings).len(), 3);
    assert_eq!(siblings["object_queries"].as_array().unwrap().len(), 1);
    for site in sites(&siblings) {
        assert_eq!(terminal(&siblings, site)["source_pc"], 0);
    }
}

#[test]
fn only_transparent_register_copies_extend_object_aliases() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "r[1] = r[0];"),
        (2, "r[2] = r[1];"),
        (3, "put(r[2], 'k0', 0, false, e);"),
        (4, "r[3] = r[0].k1;"),
        (5, "put(r[3], 'k2', 0, false, e);"),
        (6, "r[4] = construct(r[0], r[1], [r[2]]);"),
        (7, "put(r[4], 'k3', 0, false, e);"),
    ]);
    let result = report(&js, None, &ObjectOptions::default());
    let copy = site_at(&result, 3);
    assert_eq!(origin(copy)["register"], 2);
    assert_eq!(terminal(&result, copy)["source_pc"], 0);
    assert_eq!(
        origin(copy)["alias_definition_ids"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    for id in origin(copy)["alias_definition_ids"].as_array().unwrap() {
        assert_ne!(id, &origin(copy)["terminal_definition"]);
        assert!(result["definitions"].get(id.as_str().unwrap()).is_some());
    }
    assert_eq!(terminal(&result, site_at(&result, 5))["source_pc"], 4);
    assert_eq!(terminal(&result, site_at(&result, 7))["source_pc"], 6);
    assert!(origin(site_at(&result, 5))["alias_definition_ids"]
        .as_array()
        .unwrap()
        .is_empty());
    assert!(origin(site_at(&result, 7))["alias_definition_ids"]
        .as_array()
        .unwrap()
        .is_empty());
    let siblings = report(&js, Some(0), &ObjectOptions::default());
    assert_eq!(
        sites(&siblings).len(),
        1,
        "member reads and constructor operands are dependencies, not aliases"
    );
}

#[test]
fn register_reassignment_changes_terminal_pc_without_heap_identity_claims() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "r[1] = r[0];"),
        (2, "put(r[1], 'k0', 0, false, e);"),
        (3, "r[0] = {};"),
        (4, "put(r[0], 'k1', 0, false, e);"),
        (5, "put(r[1], 'k2', 0, false, e);"),
    ]);
    let result = report(&js, None, &ObjectOptions::default());
    assert_eq!(terminal(&result, site_at(&result, 2))["source_pc"], 0);
    assert_eq!(terminal(&result, site_at(&result, 4))["source_pc"], 3);
    assert_eq!(terminal(&result, site_at(&result, 5))["source_pc"], 0);
    assert_ne!(
        origin(site_at(&result, 2))["terminal_definition"],
        origin(site_at(&result, 4))["terminal_definition"]
    );
    assert_eq!(
        sites(&report(&js, Some(0), &ObjectOptions::default())).len(),
        2
    );
    assert_eq!(
        sites(&report(&js, Some(3), &ObjectOptions::default())).len(),
        1
    );
    assert!(
        sites(&report(&js, Some(1), &ObjectOptions::default())).is_empty(),
        "anchor filters terminal PC, not alias PC"
    );
}

#[test]
fn intra_pc_non_register_missing_and_block_entry_remain_explicit() {
    let js = source(&[
        (0, "r[0] = {}; put(r[0], 'k0', 0, false, e);"),
        (1, "put(r[9], 'k1', 0, false, e);"),
        (2, "put(e, 'k2', 0, false, e);"),
    ]);
    let result = report(&js, None, &ObjectOptions::default());
    for site in sites(&result) {
        assert_ne!(origin(site)["status"], "resolved");
        assert!(origin(site)["status"].as_str().is_some());
        assert!(origin(site)["terminal_definition"].is_null());
    }
    assert_ne!(
        origin(site_at(&result, 0))["status"],
        origin(site_at(&result, 2))["status"]
    );
    assert_eq!(
        origin(site_at(&result, 0))["status"],
        "unresolved_intra_pc_write"
    );
    assert_eq!(
        origin(site_at(&result, 1))["status"],
        "unresolved_block_entry_or_external"
    );
    assert_eq!(
        origin(site_at(&result, 2))["status"],
        "non_register_expression"
    );
    let blocks = source(&[
        (0, "switch (a) { case 0: r[0] = {}; break;"),
        (1, "case 1: put(r[0], 'k0', 0, false, e); break; }"),
    ]);
    let result = report(&blocks, None, &ObjectOptions::default());
    assert_ne!(origin(site_at(&result, 1))["status"], "resolved");
    assert!(origin(site_at(&result, 1))["terminal_definition"].is_null());
}

#[test]
fn exception_boundary_prevents_prior_definition_guess() {
    let js = source(&[(0, "r[0] = {};"), (10, "put(r[0], 'k0', 0, false, e);")]);
    let options = ObjectOptions::default();
    for boundary in [5, 10] {
        let bytes =
            analyze_objects_source(&js, FUNCTION, None, &BTreeSet::from([boundary]), &options)
                .unwrap();
        let result: Value = serde_json::from_slice(&bytes).unwrap();
        assert_ne!(origin(site_at(&result, 10))["status"], "resolved");
        assert!(origin(site_at(&result, 10))["terminal_definition"].is_null());
    }
}

#[test]
fn conditional_and_multiple_same_pc_writes_do_not_become_terminal_candidates() {
    for statement in [
        "r[0] = {}; r[0] = {};",
        "a && (r[0] = {});",
        "a ? (r[0] = {}) : (r[0] = {});",
        "if (a) { r[0] = {}; }",
    ] {
        let js = source(&[
            (0, "r[0] = {};"),
            (1, statement),
            (2, "put(r[0], 'k0', 0, false, e);"),
        ]);
        let result = report(&js, None, &ObjectOptions::default());
        assert_ne!(
            origin(site_at(&result, 2))["status"],
            "resolved",
            "guessed origin after {statement}"
        );
        assert!(origin(site_at(&result, 2))["terminal_definition"].is_null());
        assert!(sites(&report(&js, Some(0), &ObjectOptions::default())).is_empty());
        assert!(sites(&report(&js, Some(1), &ObjectOptions::default())).is_empty());
    }
}

#[test]
fn unresolved_copy_dependencies_remain_unresolved_instead_of_becoming_terminals() {
    for (definition, expected_status) in [
        ("r[1] = r[9];", "unresolved_block_entry_or_external"),
        ("r[0] = {}; r[1] = r[0];", "unresolved_intra_pc_write"),
    ] {
        let js = source(&[(0, definition), (1, "put(r[1], 'k0', 0, false, e);")]);
        let result = report(&js, None, &ObjectOptions::default());
        let candidate = origin(site_at(&result, 1));
        assert_eq!(candidate["status"], expected_status);
        assert!(candidate["terminal_definition"].is_null());
        assert_eq!(
            candidate["alias_definition_ids"].as_array().unwrap().len(),
            1
        );
        assert!(sites(&report(&js, Some(0), &ObjectOptions::default())).is_empty());
    }
}

#[test]
fn alias_depth_is_independent_from_provenance_depth_and_fails_closed() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "r[1] = r[0];"),
        (2, "r[2] = r[1];"),
        (3, "put(r[2], 'k0', 0, false, e);"),
    ]);
    for alias_depth in [0, 1] {
        let options = ObjectOptions {
            alias_depth,
            depth: 8,
            ..ObjectOptions::default()
        };
        let result = report(&js, None, &options);
        let candidate = origin(site_at(&result, 3));
        assert_eq!(candidate["alias_depth_truncated"], true);
        assert_eq!(candidate["status"], "alias_depth_truncated");
        assert!(candidate["terminal_definition"].is_null());
        assert!(sites(&report(&js, Some(0), &options)).is_empty());
    }
    let result = report(
        &js,
        None,
        &ObjectOptions {
            alias_depth: 2,
            depth: 0,
            ..ObjectOptions::default()
        },
    );
    assert_eq!(terminal(&result, site_at(&result, 3))["source_pc"], 0);
    assert_eq!(origin(site_at(&result, 3))["alias_depth_truncated"], false);
}

#[test]
fn malformed_helpers_fail_closed_even_if_search_would_exclude_them() {
    for helper in [
        "put?.(r[0], 'k0', 0, false, e);",
        "own?.(r[0], 'k0', 0, true);",
        "put(...a, 'k0', 0, false, e);",
        "own(r[0], ...a, 0, true);",
        "put(r[0], 'k0');",
        "put(r[0], 'k0', 0, false);",
        "put(r[0], 'k0', 0, false, e, 1);",
        "own(r[0], 'k0', 0);",
        "own(r[0], 'k0', 0, true, 1);",
        "construct?.(r[0], r[1], []);",
        "construct(r[0], r[1], [...a]);",
        "construct(r[0], r[1], [,]);",
        "construct(...a, r[1], []);",
        "construct(r[0], r[1]);",
        "construct(r[0], r[1], [], 0);",
        "apply?.(r[0], r[1], []);",
        "apply(r[0], r[1], [...a]);",
        "apply(r[0], r[1], [,]);",
        "apply(r[0], r[1]);",
    ] {
        let js = source(&[(0, "r[0] = {};"), (1, helper)]);
        let options = ObjectOptions {
            matches: vec!["absent".into()],
            ..ObjectOptions::default()
        };
        assert!(
            analyze_objects_source(&js, FUNCTION, None, &BTreeSet::new(), &options).is_err(),
            "accepted unsupported helper: {helper}"
        );
    }
}

#[test]
fn classes_and_nested_scopes_cannot_supply_local_origins() {
    for statement in [
        "function g() { r[0] = {}; }",
        "r[1] = function() { r[0] = {}; };",
        "r[1] = () => { r[0] = {}; };",
        "class C { m() { r[0] = {}; } }",
        "r[1] = class { m() { r[0] = {}; } };",
    ] {
        let js = source(&[(0, statement), (1, "put(r[0], 'k0', 0, false, e);")]);
        assert!(
            analyze_objects_source(
                &js,
                FUNCTION,
                None,
                &BTreeSet::new(),
                &ObjectOptions::default()
            )
            .is_err(),
            "accepted unsupported scope: {statement}"
        );
    }
}

#[test]
fn raw_matches_are_or_dependencies_not_decoded_property_names() {
    let js = source(&[
        (0, r#"r[0] = { k0: '\u0061lpha' };"#),
        (1, "r[1] = 'opaque';"),
        (2, "put(r[0], '\\u0062eta', r[1], false, e);"),
    ]);
    for matches in [
        vec!["\\u0061lpha".into()],
        vec!["absent".into(), "opaque".into()],
    ] {
        let result = report(
            &js,
            None,
            &ObjectOptions {
                matches,
                ..ObjectOptions::default()
            },
        );
        assert_eq!(sites(&result).len(), 1);
    }
    for decoded in ["alpha", "beta"] {
        let result = report(
            &js,
            None,
            &ObjectOptions {
                matches: vec![decoded.into()],
                ..ObjectOptions::default()
            },
        );
        assert!(
            sites(&result).is_empty(),
            "raw source is not decoded: {decoded}"
        );
    }
}

#[test]
fn utf8_spans_and_shared_ids_point_to_exact_source_bytes() {
    let js = source(&[
        (0, "r[0] = { k0: '雪é' };"),
        (1, "put(r[0], 'λ', 'é', false, e);"),
        (2, "own(r[0], 'k1', 0, true);"),
    ]);
    let result = report(&js, None, &ObjectOptions::default());
    let id = origin(site_at(&result, 1))["terminal_definition"]
        .as_str()
        .unwrap();
    assert_eq!(origin(site_at(&result, 2))["terminal_definition"], id);
    let definition = terminal(&result, site_at(&result, 1));
    let start = definition["source_span"][0].as_u64().unwrap() as usize;
    let end = definition["source_span"][1].as_u64().unwrap() as usize;
    assert_eq!(id, format!("{FUNCTION}:{start}:{end}"));
    assert_eq!(definition["function_id"], FUNCTION);
    assert_eq!(definition["defines_register"], 0);
    assert_eq!(definition["source"]["javascript"], &js[start..end]);
    assert_eq!(definition["source"]["original_bytes"], end - start);
    for site in sites(&result) {
        let start = site["source_span"][0].as_u64().unwrap() as usize;
        let end = site["source_span"][1].as_u64().unwrap() as usize;
        assert!(js.is_char_boundary(start) && js.is_char_boundary(end));
    }
}

#[test]
fn continuation_preserves_options_and_match_as_individual_tokens() {
    let needle = "opaque 'quoted' ; $(x) \\ 雪";
    let literal = serde_json::to_string(needle).unwrap();
    let raw_match = &literal[1..literal.len() - 1];
    let js = source(&[
        (0, "r[0] = {};"),
        (1, &format!("put(r[0], 'k0', {literal}, false, e);")),
        (2, &format!("own(r[0], 'k1', {literal}, true);")),
        (3, &format!("put(r[0], 'k2', {literal}, false, e);")),
    ]);
    let options = ObjectOptions {
        alias_depth: 4,
        depth: 2,
        limit: 1,
        offset: 1,
        max_bytes: 70_000,
        scan_work: 90_000,
        matches: vec![raw_match.into()],
    };
    let result = report(&js, Some(0), &options);
    assert_eq!(result["next_offset"], 2);
    assert_eq!(result["total"], 3);
    assert_eq!(result["continuation_query"]["command"], "objects");
    assert_eq!(result["continuation_query"]["function"], FUNCTION);
    assert_eq!(result["continuation_query"]["origin_pc"], 0);
    let command = tokens(&result["continuation_query"]["flags"]);
    for (flag, value) in [
        ("--alias-depth", "4"),
        ("--depth", "2"),
        ("--limit", "1"),
        ("--offset", "2"),
        ("--max-bytes", "70000"),
        ("--scan-work", "90000"),
        ("--match", raw_match),
    ] {
        assert!(
            command.windows(2).any(|pair| pair == [flag, value]),
            "missing token pair {flag}"
        );
    }
    assert_eq!(
        command.iter().filter(|&&token| token == raw_match).count(),
        1
    );
    let last = report(
        &js,
        Some(0),
        &ObjectOptions {
            offset: 2,
            ..options
        },
    );
    assert_eq!(sites(&last).len(), 1);
    assert!(last["next_offset"].is_null());
    assert!(last["continuation_query"].is_null());
}

#[test]
fn filtered_reports_keep_uncertainty_summary_for_all_eligible_sites() {
    let js = source(&[
        (0, "r[0] = {};"),
        (1, "put(r[0], 'needle', 0, false, e);"),
        (2, "put(r[9], 'k0', 0, false, e);"),
    ]);
    let full = report(&js, None, &ObjectOptions::default());
    let filtered = report(
        &js,
        None,
        &ObjectOptions {
            matches: vec!["needle".into()],
            ..ObjectOptions::default()
        },
    );
    assert_eq!(sites(&filtered).len(), 1);
    assert!(full["origin_summary"].is_object());
    assert_eq!(filtered["origin_summary"], full["origin_summary"]);
    assert_eq!(filtered["total"], 1);
    assert_eq!(filtered["unfiltered_total"], 2);
    assert_eq!(filtered["origin_summary"]["unresolved_origins"], 1);
    assert_eq!(filtered["origin_summary"]["non_register_expressions"], 0);
}

#[test]
fn anchor_must_be_exact_indexed_pc_and_function_must_match() {
    let js = source(&[(0, "r[0] = {};"), (10, "put(r[0], 'k0', 0, false, e);")]);
    for pc in [1, 9, 11, u32::MAX] {
        assert!(analyze_objects_source(
            &js,
            FUNCTION,
            Some(pc),
            &BTreeSet::new(),
            &ObjectOptions::default()
        )
        .is_err());
    }
    assert!(analyze_objects_source(
        &js,
        FUNCTION + 1,
        None,
        &BTreeSet::new(),
        &ObjectOptions::default()
    )
    .is_err());
}

#[test]
fn invalid_options_and_output_work_source_bounds_fail_closed() {
    let js = source(&[(0, "r[0] = {};"), (1, "put(r[0], 'k0', 0, false, e);")]);
    let invalid = [
        ObjectOptions {
            alias_depth: 65,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            depth: 9,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            limit: 0,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            limit: 1001,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            max_bytes: 0,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            max_bytes: 16_777_217,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            scan_work: 0,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            scan_work: 16_777_217,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            matches: vec![String::new()],
            ..ObjectOptions::default()
        },
        ObjectOptions {
            matches: vec!["a".repeat(1025)],
            ..ObjectOptions::default()
        },
        ObjectOptions {
            matches: vec!["a".into(); 65],
            ..ObjectOptions::default()
        },
        ObjectOptions {
            max_bytes: 1,
            ..ObjectOptions::default()
        },
        ObjectOptions {
            scan_work: 1,
            ..ObjectOptions::default()
        },
    ];
    for options in invalid {
        assert!(analyze_objects_source(&js, FUNCTION, None, &BTreeSet::new(), &options).is_err());
    }
    let mut oversized = String::from("/*");
    oversized.push_str(&"x".repeat(64 * 1024 * 1024));
    oversized.push_str("*/\n");
    oversized.push_str(&js);
    assert!(analyze_objects_source(
        &oversized,
        FUNCTION,
        None,
        &BTreeSet::new(),
        &ObjectOptions::default()
    )
    .is_err());
}

#[test]
fn repeated_sibling_writes_share_definitions_with_linear_scan_work() {
    fn repeated(count: u32) -> String {
        let mut js = source(&[(0, "r[0] = {};"), (1, "r[1] = r[0];")]);
        js.truncate(js.len() - 3);
        for pc in 2..count + 2 {
            js.push_str(&format!(
                "// HBC function {FUNCTION}, PC {pc}\nput(r[1], 'k0', 0, false, e);\n"
            ));
        }
        js.push_str("};\n");
        js
    }
    let options = ObjectOptions {
        limit: 1,
        scan_work: 2_097_152,
        ..ObjectOptions::default()
    };
    let small = report(&repeated(2_000), Some(0), &options);
    let large = report(&repeated(8_000), Some(0), &options);
    assert_eq!(sites(&large).len(), 1);
    assert_eq!(large["next_offset"], 1);
    assert_eq!(large["total"], 8_000);
    assert_eq!(large["object_queries"].as_array().unwrap().len(), 1);
    assert!(large["definitions"].as_object().unwrap().len() <= 2);
    let small_work = small["work_used"].as_u64().unwrap();
    let large_work = large["work_used"].as_u64().unwrap();
    assert!(small_work > 0);
    assert!(
        large_work <= small_work * 5 + 1024,
        "four times as many siblings must not cause quadratic traversal"
    );
    assert!(large_work <= large["work_cap"].as_u64().unwrap());
    let exhausted = ObjectOptions {
        scan_work: small_work as usize,
        ..options
    };
    assert!(analyze_objects_source(
        &repeated(8_000),
        FUNCTION,
        Some(0),
        &BTreeSet::new(),
        &exhausted
    )
    .is_err());
}
