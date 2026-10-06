use hermes_dec_rs::cli::origins::{
    analyze_properties_source, PropertyOptions, DEFAULT_PROPERTY_SCAN_WORK, MAX_PROPERTY_SCAN_WORK,
};
use serde_json::Value;

fn options() -> PropertyOptions {
    PropertyOptions {
        function: 0,
        depth: 8,
        definition_limit: 64,
        limit: 5,
        offset: 0,
        max_bytes: 100_000,
        scan_work: DEFAULT_PROPERTY_SCAN_WORK,
        matches: vec![],
    }
}

fn source(cases: &str) -> String {
    format!("M[0] = ['f',0,0]; F[0] = function function_0(env,self,args,newTarget,callee,state) {{ const r = objectCreate(null); let pc = 0, caught; for (;;) {{ try {{ switch (pc) {{ {cases} default: throw new ErrorCtor('bad'); }} }} catch (error) {{ throw error; }} }} }};")
}

fn linear(statements: &[&str]) -> String {
    let body = statements
        .iter()
        .enumerate()
        .map(|(i, statement)| format!("// HBC function 0, PC {}\n{statement}\n", i * 2))
        .collect::<String>();
    source(&format!("case 0: {{\n{body}return; }}"))
}

fn analyze(source: &str, options: &PropertyOptions) -> Value {
    let bytes = analyze_properties_source(source, options, &[]).unwrap();
    assert!(bytes.len() <= options.max_bytes);
    serde_json::from_slice(&bytes).unwrap()
}

fn rows(report: &Value) -> &[Value] {
    report["rows"].as_array().unwrap()
}

fn javascript(excerpt: &Value) -> &str {
    excerpt["javascript"].as_str().unwrap()
}

fn assert_excerpt(source: &str, excerpt: &Value) {
    let start = excerpt["start"].as_u64().unwrap() as usize;
    let end = excerpt["end"].as_u64().unwrap() as usize;
    assert_eq!(source.get(start..end).unwrap(), javascript(excerpt));
    assert_eq!(excerpt["snippet_truncated"], false);
}

fn definition_pcs(report: &Value, role: &Value) -> Vec<u64> {
    let mut pcs = role["definition_ids"]
        .as_array()
        .unwrap()
        .iter()
        .map(|id| {
            report["definitions"][id.as_str().unwrap()]["pc"]
                .as_u64()
                .unwrap()
        })
        .collect::<Vec<_>>();
    pcs.sort_unstable();
    pcs.dedup();
    pcs
}

#[test]
fn properties_follow_cross_block_source_alternatives() {
    let s = source("case 0: {\n// HBC function 0, PC 0\npc = r[0] ? 2 : 4; continue; } case 2: {\n// HBC function 0, PC 2\nr[1] = 'first';\n// HBC function 0, PC 3\npc = 6; continue; } case 4: {\n// HBC function 0, PC 4\nr[1] = 'other';\n// HBC function 0, PC 5\npc = 6; continue; } case 6: {\n// HBC function 0, PC 6\nr[7][r[1]] = 123; return; }");
    let v = analyze(&s, &options());
    assert_eq!(v["schema"], "properties-v1");
    assert_eq!(v["schema_version"], 1);
    assert_eq!(v["function"], 0);
    assert_eq!(v["stores_total"], 1);
    assert_eq!(rows(&v).len(), 1);
    let row = &rows(&v)[0];
    assert_eq!(row["function"], 0);
    assert_eq!(row["pc"], 6);
    assert_eq!(definition_pcs(&v, &row["key"]), vec![2, 4]);
    assert_eq!(row["key"]["unresolved"], false);
    assert_eq!(row["key"]["truncated"], false);
    assert!(row["key"]["unknown"].as_array().unwrap().is_empty());
    assert_eq!(row["key"]["queried"], true);
    assert_eq!(row["value"]["queried"], true);
    let dependency = &row["key"]["dependencies"][0];
    assert_eq!(dependency["register"], 1);
    assert_eq!(dependency["pc"], 6);
    assert!(dependency["owner_definition"].is_null());
    assert_eq!(dependency["candidates"].as_array().unwrap().len(), 2);
    for flag in ["unresolved", "cycle", "same_pc_ambiguity", "truncated"] {
        assert_eq!(dependency[flag], false, "{flag}");
    }
    assert!(row["value"]["definition_ids"]
        .as_array()
        .unwrap()
        .is_empty());
}

#[test]
fn property_filters_search_keys_not_values_or_object_labels() {
    let s = linear(&[
        "r[7] = 'object-label';",
        "r[1] = 'key-label';",
        "r[2] = 'value-label';",
        "r[7][r[1]] = r[2];",
    ]);
    let plain = analyze(&s, &options());
    assert_eq!(definition_pcs(&plain, &rows(&plain)[0]["key"]), vec![2]);
    assert_eq!(definition_pcs(&plain, &rows(&plain)[0]["value"]), vec![4]);
    for term in ["object-label", "value-label", "r[2]"] {
        let mut o = options();
        o.matches = vec![term.into()];
        let v = analyze(&s, &o);
        assert!(rows(&v).is_empty(), "unexpected key match for {term}");
        assert_eq!(v["stores_total"], 1);
    }
    for term in ["KEY-LABEL", "r[1]"] {
        let mut o = options();
        o.matches = vec!["absent".into(), term.into()];
        let v = analyze(&s, &o);
        assert_eq!(rows(&v).len(), 1, "{term}");
        assert_eq!(rows(&v)[0]["key"], rows(&plain)[0]["key"]);
        assert_eq!(rows(&v)[0]["value"], rows(&plain)[0]["value"]);
        assert!(rows(&v)[0]["match_evidence_count"].as_u64().unwrap() > 0);
        let evidence = rows(&v)[0]["match_evidence"].as_array().unwrap();
        assert!(!evidence.is_empty() && evidence.len() <= 8);
        for item in evidence {
            assert_excerpt(&s, &item["source"]);
            assert!(!javascript(&item["source"]).contains("value-label"));
            if let Some(id) = item["definition_id"].as_str() {
                assert!(v["definitions"].get(id).is_some());
            }
        }
    }
}

#[test]
fn property_key_search_is_escaped_case_insensitive_raw_javascript() {
    let s = linear(&[
        "r[1] = '\\u0061bc';",
        "r[7][r[1]] = 1;",
        "r[7]['a.b[0]'] = 2;",
        "r[7]['axb0'] = 3;",
    ]);
    for (term, ordinals) in [("abc", vec![]), ("\\U0061", vec![0]), ("A.B[0]", vec![1])] {
        let mut o = options();
        o.matches = vec![term.into()];
        let v = analyze(&s, &o);
        let actual = rows(&v)
            .iter()
            .map(|r| r["store_ordinal"].as_u64().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(actual, ordinals, "{term}");
    }
}

#[test]
fn property_assignments_exclude_registers_numeric_slots_and_non_simple_writes() {
    let s = linear(&[
        "r[1] = 'register-definition';",
        "r.privateName = 9;",
        "r[7].slots[9] = r[1];",
        "r[7].slots[0xA] = 1;",
        "r[7].staticName = r[1];",
        "r[7]['computedName'] = 2;",
        "r[7].slots['named'] = 3;",
    ]);
    let v = analyze(&s, &options());
    assert_eq!(v["stores_total"], 3);
    assert_eq!(rows(&v).len(), 3);
    for (i, (object, key, value, pc)) in [
        ("r[7]", "staticName", "r[1]", 8),
        ("r[7]", "'computedName'", "2", 10),
        ("r[7].slots", "'named'", "3", 12),
    ]
    .into_iter()
    .enumerate()
    {
        let row = &rows(&v)[i];
        assert_eq!(row["store_ordinal"], i);
        assert_eq!(row["pc"], pc);
        assert_eq!(javascript(&row["object"]), object);
        assert_eq!(javascript(&row["key"]["source"]), key);
        assert_eq!(javascript(&row["value"]["source"]), value);
        assert!(!row["form"].as_str().unwrap().is_empty());
    }
    assert_eq!(rows(&v)[0]["form"], "static_assignment");
    assert_eq!(rows(&v)[1]["form"], "computed_assignment");
    for statement in ["r[7].staticName += 1;", "r[7]['computedName']++;"] {
        let s = linear(&[statement]);
        if let Ok(bytes) = analyze_properties_source(&s, &options(), &[]) {
            let v: Value = serde_json::from_slice(&bytes).unwrap();
            assert!(rows(&v).is_empty(), "{statement}");
        }
    }
}

#[test]
fn property_helpers_keep_object_key_value_argument_order() {
    let s = linear(&[
        "r[1] = 'key-origin';",
        "r[2] = 'value-origin';",
        "put(r[7], r[1], r[2], false, true);",
        "own(r[8], 'own-key', r[2], false);",
    ]);
    let v = analyze(&s, &options());
    assert_eq!(v["stores_total"], 2);
    let put = &rows(&v)[0];
    assert_eq!(put["form"], "put");
    assert_eq!(javascript(&put["object"]), "r[7]");
    assert_eq!(javascript(&put["key"]["source"]), "r[1]");
    assert_eq!(javascript(&put["value"]["source"]), "r[2]");
    assert_eq!(definition_pcs(&v, &put["key"]), vec![0]);
    assert_eq!(definition_pcs(&v, &put["value"]), vec![2]);
    let own = &rows(&v)[1];
    assert_eq!(own["form"], "own");
    assert_eq!(javascript(&own["object"]), "r[8]");
    assert_eq!(javascript(&own["key"]["source"]), "'own-key'");
    assert_eq!(javascript(&own["value"]["source"]), "r[2]");
    assert!(own["key"]["definition_ids"].as_array().unwrap().is_empty());
    assert_eq!(definition_pcs(&v, &own["value"]), vec![2]);
    for term in ["value-origin", "false", "true"] {
        let mut o = options();
        o.matches = vec![term.into()];
        assert!(rows(&analyze(&s, &o)).is_empty(), "{term}");
    }
}

#[test]
fn malformed_property_helpers_are_not_accepted_as_property_rows() {
    for call in [
        "put(r[7], 'key', 1, false);",
        "put(r[7], 'key', 1, false, true, 0);",
        "own(r[7], 'key', 1);",
        "own(r[7], 'key', 1, false, 0);",
        "put?.(r[7], 'key', 1, false, true);",
        "own?.(r[7], 'key', 1, false);",
        "put(...r[7], 'key', 1, false, true);",
        "put(r[7], ...r[1], 1, false, true);",
        "put(r[7], 'key', ...r[2], false, true);",
        "put(r[7], 'key', 1, false, ...r[2]);",
        "own(r[7], 'key', 1, ...r[2]);",
    ] {
        let s = linear(&[call]);
        // Rejection may be a whole-query diagnostic or an omitted malformed site.
        if let Ok(bytes) = analyze_properties_source(&s, &options(), &[]) {
            let v: Value = serde_json::from_slice(&bytes).unwrap();
            assert!(rows(&v).is_empty(), "accepted malformed helper: {call}");
            assert_eq!(v["stores_total"], 0, "{call}");
        }
    }
}

#[test]
fn property_paging_uses_raw_ordinals_and_preserves_filtered_row_identity() {
    let s = linear(&[
        "r[7]['needle'] = 1;",
        "r[7]['skip'] = 2;",
        "r[7]['NEEDLE'] = 3;",
    ]);
    let plain = analyze(&s, &options());
    let mut o = options();
    o.limit = 1;
    o.matches = vec!["needle".into()];
    let first = analyze(&s, &o);
    assert_eq!(first["offset"], 0);
    assert_eq!(first["stores_total"], 3);
    assert_eq!(first["scanned"], 1);
    assert_eq!(first["next_offset"], 1);
    assert_eq!(first["scan_complete"], false);
    assert_eq!(rows(&first)[0]["store_ordinal"], 0);
    o.offset = first["next_offset"].as_u64().unwrap() as usize;
    let second = analyze(&s, &o);
    assert_eq!(second["scanned"], 2);
    assert_eq!(second["scan_complete"], true);
    assert!(second["next_offset"].is_null());
    let row = &rows(&second)[0];
    assert_eq!(row["store_ordinal"], 2);
    for field in [
        "function",
        "pc",
        "store_ordinal",
        "form",
        "object",
        "key",
        "value",
    ] {
        assert_eq!(row[field], rows(&plain)[2][field], "{field}");
    }
    o.offset = 3;
    let end = analyze(&s, &o);
    assert!(rows(&end).is_empty());
    assert_eq!(end["scanned"], 0);
    assert_eq!(end["scan_complete"], true);
    assert!(end["next_offset"].is_null());
}

#[test]
fn property_utf8_excerpts_and_definition_ids_join_exact_source_bytes() {
    let s = linear(&[
        "r[1] = '\u{00e9}\u{1f680}';",
        "r[2] = r[1];",
        "r[7][r[2]] = r[1];",
    ]);
    let mut o = options();
    o.matches = vec!["\u{00e9}".into()];
    let v = analyze(&s, &o);
    assert_eq!(v["expression_source"]["offset_unit"], "utf8_bytes");
    assert_eq!(v["expression_source"]["source_bytes"], s.len());
    assert_eq!(rows(&v).len(), 1);
    let definitions = v["definitions"].as_object().unwrap();
    assert!(!definitions.is_empty());
    for (id, definition) in definitions {
        let span = &definition["source"];
        assert_excerpt(&s, span);
        assert_eq!(
            id,
            &format!(
                "0:{}:{}:{}",
                definition["register"], span["start"], span["end"]
            )
        );
    }
    for row in rows(&v) {
        assert_excerpt(&s, &row["object"]);
        for role in ["key", "value"] {
            assert_excerpt(&s, &row[role]["source"]);
            for id in row[role]["definition_ids"].as_array().unwrap() {
                assert!(definitions.contains_key(id.as_str().unwrap()));
            }
            for dependency in row[role]["dependencies"].as_array().unwrap() {
                let span = &dependency["read_span"];
                assert_eq!(span.as_array().unwrap().len(), 2);
                let start = span[0].as_u64().unwrap() as usize;
                let end = span[1].as_u64().unwrap() as usize;
                assert_eq!(
                    s.get(start..end).unwrap(),
                    format!("r[{}]", dependency["register"])
                );
                for id in dependency["candidates"].as_array().unwrap() {
                    assert!(definitions.contains_key(id.as_str().unwrap()));
                }
                if let Some(id) = dependency["owner_definition"].as_str() {
                    assert!(definitions.contains_key(id));
                }
            }
        }
        for evidence in row["match_evidence"].as_array().unwrap() {
            assert_excerpt(&s, &evidence["source"]);
        }
    }
}

#[test]
fn property_match_evidence_display_cap_does_not_drop_source_candidates() {
    let mut cases = String::new();
    for i in 0..9 {
        let branch_pc = i * 4;
        let definition_pc = branch_pc + 2;
        let next_pc = branch_pc + 4;
        cases.push_str(&format!(
            "case {branch_pc}: {{\n// HBC function 0, PC {branch_pc}\npc = r[9] ? {definition_pc} : {next_pc}; continue; }} case {definition_pc}: {{\n// HBC function 0, PC {definition_pc}\nr[1] = 'needle{i}'; pc = 100; continue; }} "
        ));
    }
    cases.push_str("case 36: {\n// HBC function 0, PC 36\nr[1] = 'needle9'; pc = 100; continue; } case 100: {\n// HBC function 0, PC 100\nr[7][r[1]] = 0; return; }");
    let s = source(&cases);
    let mut o = options();
    o.matches = vec!["needle".into()];
    let v = analyze(&s, &o);
    assert_eq!(rows(&v).len(), 1);
    let row = &rows(&v)[0];
    assert_eq!(row["key"]["definition_ids"].as_array().unwrap().len(), 10);
    assert_eq!(row["match_evidence_count"], 10);
    assert_eq!(row["match_evidence_truncated"], true);
    assert_eq!(row["match_evidence"].as_array().unwrap().len(), 8);
    assert_eq!(row["key"]["truncated"], false);
    o.matches = vec!["needle9".into()];
    let late = analyze(&s, &o);
    assert_eq!(rows(&late).len(), 1);
    assert_eq!(rows(&late)[0]["match_evidence_count"], 1);
    assert_eq!(
        javascript(&rows(&late)[0]["match_evidence"][0]["source"]),
        "needle9"
    );
}

#[test]
fn property_output_budget_is_atomic_and_includes_metadata_and_shared_definitions() {
    let s = linear(&["r[1] = 'key';", "r[7][r[1]] = r[1];"]);
    let o = options();
    let complete = analyze_properties_source(&s, &o, &[]).unwrap();
    let mut exact = options();
    exact.max_bytes = complete.len();
    assert_eq!(
        analyze_properties_source(&s, &exact, &[]).unwrap(),
        complete
    );
    for budget in [1, 128, complete.len() - 1] {
        let mut small = options();
        small.max_bytes = budget;
        assert!(
            analyze_properties_source(&s, &small, &[]).is_err(),
            "budget {budget}"
        );
    }
}

#[test]
fn property_dependency_work_cap_is_shared_across_key_value_and_rows() {
    let s = linear(&[
        "r[1] = 'needle';",
        "r[2] = r[1];",
        "r[3] = r[2];",
        "r[4] = r[3];",
        "r[7][r[4]] = r[4];",
        "r[8][r[4]] = r[4];",
    ]);
    let complete = analyze(&s, &options());
    assert_eq!(rows(&complete).len(), 2);
    assert!(complete["query_work_used"].as_u64().unwrap() > 1);
    let mut o = options();
    o.scan_work = 1;
    let capped = analyze(&s, &o);
    assert_eq!(capped["query_work_cap"], 1);
    assert!(capped["query_work_used"].as_u64().unwrap() <= 1);
    assert!(
        capped["scan_complete"] == false
            || rows(&capped).iter().any(|row| {
                ["key", "value"]
                    .iter()
                    .any(|role| row[*role]["truncated"] == true || row[*role]["unresolved"] == true)
            })
    );
}

#[test]
fn property_scan_work_is_aggregate_bounded_and_continuable() {
    assert_eq!(DEFAULT_PROPERTY_SCAN_WORK, 1_048_576);
    assert_eq!(MAX_PROPERTY_SCAN_WORK, 16_777_216);
    let s = linear(&[
        "r[2] = 'value-origin';",
        "r[7]['skip'] = 1;",
        "r[7]['needle'] = r[2];",
        "r[7]['needle'] = 3;",
    ]);
    let mut o = options();
    o.scan_work = 1;
    o.matches = vec!["needle".into()];
    let first = analyze(&s, &o);
    assert_eq!(first["query_work_cap"], 1);
    assert!(first["query_work_used"].as_u64().unwrap() <= 1);
    assert_eq!(first["scan_complete"], false);
    assert_eq!(rows(&first).len(), 1);
    let matched = &rows(&first)[0];
    assert_eq!(matched["store_ordinal"], 1);
    assert_eq!(matched["key"]["queried"], true);
    assert_eq!(matched["key"]["unresolved"], false);
    assert_eq!(matched["key"]["truncated"], false);
    assert_eq!(matched["value"]["queried"], false);
    assert_eq!(matched["value"]["unresolved"], true);
    assert_eq!(matched["value"]["truncated"], true);
    assert_eq!(first["values_not_queried"], 1);
    assert_eq!(first["query_work_truncated"], true);
    assert_eq!(javascript(&matched["value"]["source"]), "r[2]");
    assert!(matched["value"]["definition_ids"]
        .as_array()
        .unwrap()
        .is_empty());
    assert!(matched["value"]["dependencies"]
        .as_array()
        .unwrap()
        .is_empty());
    assert!(first["definitions"].as_object().unwrap().is_empty());
    assert_eq!(first["scanned"], 2);
    let next = first["next_offset"].as_u64().unwrap() as usize;
    assert!(next > 0 && next < 3);
    o.offset = next;
    o.scan_work = DEFAULT_PROPERTY_SCAN_WORK;
    let rest = analyze(&s, &o);
    assert_eq!(rest["scan_complete"], true);
    assert!(rest["next_offset"].is_null());
    assert_eq!(next, 2);
    assert_eq!(rows(&rest).len(), 1);
    assert_eq!(rows(&rest)[0]["store_ordinal"], 2);
    assert_eq!(rows(&rest)[0]["value"]["queried"], true);
    for cap in [0, MAX_PROPERTY_SCAN_WORK + 1] {
        o.scan_work = cap;
        assert!(analyze_properties_source(&s, &o, &[]).is_err());
    }
}

#[test]
fn property_class_initializers_are_not_enclosing_pc_evidence() {
    for statement in [
        "r[0] = class { field = (r[7]['needle'] = r[1]); };",
        "r[0] = class { static field = (r[7]['needle'] = r[1]); };",
        "r[0] = class { static { put(r[7], 'needle', r[1], false, true); } };",
    ] {
        let s = linear(&["r[1] = 'outer';", statement]);
        let error = analyze_properties_source(&s, &options(), &[])
            .unwrap_err()
            .to_string();
        assert!(error.contains("class scopes"), "{error}");
    }
}

#[test]
fn property_many_stores_at_one_pc_do_not_need_quadratic_read_scans() {
    let statements = "put(r[7], 'skip', r[2], false, true);".repeat(20_000);
    let s = linear(&[&statements]);
    let mut o = options();
    o.scan_work = 1;
    o.matches = vec!["absent".into()];
    let report = analyze(&s, &o);
    assert_eq!(report["stores_total"], 20_000);
    assert_eq!(report["scanned"], 20_000);
    assert_eq!(report["scan_complete"], true);
    assert_eq!(report["query_work_used"], 0);
    assert_eq!(report["filter_work_used"], 120_000);
    assert!(rows(&report).is_empty());
}

#[test]
fn property_dependency_flags_do_not_include_unused_edge_display_caps() {
    let mut cases = String::new();
    for pc in 0..300 {
        cases.push_str(&format!(
            "case {pc}: {{\n// HBC function 0, PC {pc}\npc = {}; continue; }}",
            pc + 1
        ));
    }
    cases.push_str(
        "case 300: {\n// HBC function 0, PC 300\nput(r[7], 'key', 3, false, true); return; }",
    );
    let report = analyze(&source(&cases), &options());
    assert_eq!(rows(&report).len(), 1);
    assert_eq!(rows(&report)[0]["key"]["truncated"], false);
    assert_eq!(rows(&report)[0]["value"]["truncated"], false);
    assert_eq!(report["key_search_incomplete_rows"], 0);
}

#[test]
fn property_depth_boundary_uses_scoped_read_index_for_constant_candidates() {
    let skipped = "put(r[7], 'skip', r[2], false, true);".repeat(20_000);
    let retained = "put(r[7], r[1], 3, false, true);".repeat(1000);
    let s = linear(&[&format!("r[1] = 'needle';{skipped}"), &retained]);
    let mut o = options();
    o.depth = 0;
    o.limit = 1000;
    o.max_bytes = 4_000_000;
    o.matches = vec!["needle".into()];
    let report = analyze(&s, &o);
    assert_eq!(rows(&report).len(), 1000);
    assert_eq!(report["scan_complete"], true);
    for row in rows(&report) {
        assert_eq!(row["key"]["truncated"], false);
        assert_eq!(definition_pcs(&report, &row["key"]), vec![0]);
    }
}

#[test]
fn property_filter_byte_work_exhaustion_fails_closed() {
    let s = linear(&[&format!("r[7]['{}'] = 1;", "x".repeat(33_554_433))]);
    let mut o = options();
    o.matches = vec!["needle".into()];
    let error = analyze_properties_source(&s, &o, &[])
        .unwrap_err()
        .to_string();
    assert!(error.contains("filter work cap"), "{error}");
}

#[test]
fn property_exhausted_key_search_can_continue_after_an_empty_filtered_page() {
    let s = linear(&["r[7][r[1]] = 1;", "r[7]['needle'] = 2;"]);
    let mut o = options();
    o.scan_work = 1;
    o.matches = vec!["needle".into()];
    let first = analyze(&s, &o);
    assert!(rows(&first).is_empty());
    assert_eq!(first["scan_complete"], false);
    assert_eq!(first["scanned"], 1);
    assert_eq!(first["query_work_used"], 1);
    assert_eq!(first["next_offset"], 1);
    o.offset = 1;
    o.scan_work = DEFAULT_PROPERTY_SCAN_WORK;
    let next = analyze(&s, &o);
    assert_eq!(rows(&next).len(), 1);
    assert_eq!(rows(&next)[0]["store_ordinal"], 1);
    assert_eq!(next["scan_complete"], true);
    assert!(next["next_offset"].is_null());
}
