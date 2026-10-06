//! Independent authored-source adversarial checks. No application execution.
use hermes_dec_rs::cli::{initializers, links};
use serde_json::{json, Value};
use std::collections::BTreeSet;

fn fragment(body: &str) -> String {
    format!("F[0] = function(env) {{\nvar r = [];\n{body}\n}};")
}

fn initializer(source: &str) -> Value {
    initializers::report_source(source, 0, &BTreeSet::new()).unwrap()
}

fn array<'a>(value: &'a Value, key: &str) -> &'a [Value] {
    value[key].as_array().unwrap().as_slice()
}

fn occurrence(source: &str, needle: &str, nth: usize) -> (usize, usize) {
    let start = source.match_indices(needle).nth(nth).unwrap().0;
    (start, start + needle.len())
}

fn span_text<'a>(source: &'a str, span: &Value) -> &'a str {
    let start = span[0].as_u64().unwrap() as usize;
    let end = span[1].as_u64().unwrap() as usize;
    &source[start..end]
}

fn exact_snippet(source: &str, snippet: &Value, expected: &str) {
    assert_eq!(span_text(source, &snippet["source_span"]), expected);
    assert_eq!(snippet["javascript"], expected);
    assert_eq!(snippet["original_bytes"], expected.len());
    assert_eq!(snippet["bytes_omitted"], 0);
    assert_eq!(snippet["truncated"], false);
}

fn exact_link<'a>(report: &'a Value, source: &str, needle: &str, nth: usize) -> &'a Value {
    let (start, end) = occurrence(source, needle, nth);
    let row = array(report, "records")
        .iter()
        .find(|r| r["source"]["start"] == start && r["source"]["end"] == end)
        .unwrap_or_else(|| panic!("missing exact link {needle} at {start}:{end}"));
    assert_eq!(
        row["source"]["source_id"],
        format!("f0:source:{start}:{end}")
    );
    assert_eq!(&source[start..end], needle);
    assert_eq!(row["binding_verified"], false);
    assert_eq!(row["runtime_verified"], false);
    row
}

fn definition<'a>(report: &'a Value, source: &str, expected: &str) -> &'a Value {
    let (start, end) = occurrence(source, expected, 0);
    let key = format!("0:{start}:{end}");
    let def = report["definitions"].get(&key).unwrap();
    assert_eq!(def["source_span"], json!([start, end]));
    exact_snippet(source, &def["source"], expected);
    def
}

fn no_candidate(row: &Value) {
    assert!(array(row, "definition_ids_source_order").is_empty());
    assert!(!array(row, "rhs_edges").is_empty());
    for edge in array(row, "rhs_edges") {
        assert!(edge["definition_id"].is_null(), "unsafe edge: {edge}");
        assert_ne!(edge["status"], "local_definition_candidate");
    }
}

#[test]
fn review_utf8_cross_report_join_keeps_operand_and_store_coordinates() {
    let source = fragment(
        "// HBC function 0, PC 10\nr[1] = F[3];\n\
         // HBC function 0, PC 20\n/* multibyte: \u{00e9}\u{1f980} */ env[5] = r[1];",
    );
    let before = source.clone();
    let link = links::report_source(&source, 0, 8).unwrap();
    let init = initializer(&source);
    assert_eq!(source, before);
    assert_eq!(link["source"]["end"], source.len());
    assert_eq!(
        link["source"]["source_id"],
        format!("f0:source:0:{}", source.len())
    );
    let mention = exact_link(&link, &source, "F[3]", 0);
    assert_eq!(mention["pc"], 10);
    assert_eq!(mention["status"], "syntactic_only");
    let store = exact_link(&link, &source, "env[5]", 0);
    assert_eq!(store["role"], "write");
    let row = &array(&init, "rows")[0];
    exact_snippet(&source, &row["source"], "env[5] = r[1]");
    exact_snippet(&source, &row["rhs"], "r[1]");
    assert_eq!(store["rhs"]["start"], row["rhs"]["source_span"][0]);
    assert_eq!(store["rhs"]["end"], row["rhs"]["source_span"][1]);
    let def = definition(&init, &source, "r[1] = F[3]");
    assert_eq!(def["rhs"]["source_span"][0], mention["source"]["start"]);
    assert_eq!(def["rhs"]["source_span"][1], mention["source"]["end"]);
    assert_eq!(
        row["rhs_edges"][0]["definition_id"],
        row["definition_ids_source_order"][0]
    );
}

#[test]
fn review_negative_escaped_and_unsafe_ids_are_not_folded_or_wrapped() {
    let source = fragment(
        r"// HBC function 0, PC 1
F[-1]; F['2']; F[1 + 2]; F[9007199254740992]; F[4294967296];
\u0046[7]; \u0065nv[8]; \u0063losure(6); closure(-3); closure('4');
const closure = fake; closure(0xC, F[0xD]);
env[-1] = 1; env['2'] = 2; env[4294967296] = 3; env[0xA] = F[0xB];",
    );
    let report = links::report_source(&source, 0, 16).unwrap();
    assert_eq!(report["total"], 6);
    assert_eq!(
        exact_link(&report, &source, "F[4294967296]", 0)["status"],
        "unresolved_out_of_range"
    );
    assert_eq!(exact_link(&report, &source, "env[0xA]", 0)["id"], 10);
    assert_eq!(exact_link(&report, &source, "F[0xB]", 0)["id"], 11);
    assert_eq!(
        exact_link(&report, &source, "env[4294967296]", 0)["id"],
        4294967296u64
    );
    let helper = exact_link(&report, &source, "0xC", 0);
    assert_eq!(helper["id"], 12);
    assert_eq!(helper["role"], "closure_argument");
    assert_eq!(helper["status"], "syntactic_only");
    assert_eq!(
        exact_link(&report, &source, "F[0xD]", 0)["role"],
        "helper_call_argument"
    );
    // Initializer slots have a narrower u32 domain; invalid slots are not slot 0.
    let slots = fragment(
        "// HBC function 0, PC 1\nenv[-1] = 1; env['2'] = 2; env[4294967296] = 3; env[0xA] = 4;",
    );
    let init = initializer(&slots);
    assert_eq!(init["total_stores"], 1);
    assert_eq!(init["rows"][0]["slot"], 10);
    exact_snippet(&slots, &init["rows"][0]["source"], "env[0xA] = 4");
}

#[test]
fn review_root_selection_rejects_conflicting_scaffolding() {
    let body = "function(env) { var r = [];\n// HBC function 0, PC 1\nenv[1] = F[2];\n}";
    for source in [
        format!("F[1] = {body};"),
        format!("F[0] += {body};"),
        format!("F[0] = {body}; F[0] = {body};"),
        format!("F[-0] = {body};"),
    ] {
        assert!(
            links::report_source(&source, 0, 8).is_err(),
            "links accepted {source}"
        );
        assert!(
            initializers::report_source(&source, 0, &BTreeSet::new()).is_err(),
            "initializers accepted {source}"
        );
    }
}

#[test]
fn review_links_do_not_leak_inner_pc_or_deferred_scope_mentions() {
    let source = fragment(
        "if (flag) {\n// HBC function 0, PC 4\nF[1];\n}\nF[2];\n\
         const thunk = () => {\n// HBC function 9, PC 0\nF[3];\n};\n\
         class C { [F[4]] = F[5]; method() { env[7]; } }\n\
         const fake = '// HBC function 0, PC 99';\nF[6];",
    );
    let report = links::report_source(&source, 0, 8).unwrap();
    assert_eq!(report["total"], 3);
    assert_eq!(exact_link(&report, &source, "F[1]", 0)["pc"], 4);
    assert!(exact_link(&report, &source, "F[2]", 0)["pc"].is_null());
    assert!(exact_link(&report, &source, "F[6]", 0)["pc"].is_null());
    assert_eq!(report["skipped_scope_count"], 2);
}

#[test]
fn review_same_pc_order_and_multiple_writes_never_choose_a_definition() {
    for body in [
        "// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\nenv[0] = r[1]; r[1] = F[3];",
        "// HBC function 0, PC 1\nr[1] = F[2]; r[1] = F[3];\n// HBC function 0, PC 2\nenv[0] = r[1];",
        "// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\nr[1] += F[3];\n// HBC function 0, PC 3\nenv[0] = r[1];",
    ] {
        let source = fragment(body);
        let report = initializer(&source);
        let row = &array(&report, "rows")[0];
        no_candidate(row);
        exact_snippet(&source, &row["rhs_edges"][0]["expression"], "r[1]");
        assert!(report["definitions"].as_object().unwrap().is_empty());
    }
    let source = fragment("// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\nr[2] = r[1]; r[1] = F[3];\n// HBC function 0, PC 3\nenv[1] = r[2];");
    let report = initializer(&source);
    let snapshot = definition(&report, &source, "r[2] = r[1]");
    assert!(snapshot["edges"][0]["definition_id"].is_null());
    assert_eq!(snapshot["edges"][0]["status"], "unresolved_same_pc_write");
    assert_eq!(report["definitions"].as_object().unwrap().len(), 1);
}

#[test]
fn review_conditional_nested_mutations_kill_both_branch_and_following_edges() {
    for mutation in [
        "if (flag) { r[1] = F[3]; env[0] = r[1]; } else { r[1] = F[4]; }",
        "flag && (r[1] = F[3]);",
        "flag ? (r[1] = F[3]) : (r[1] = F[4]);",
        "sink(flag || (r[1] = F[3]));",
    ] {
        let source = fragment(&format!("// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\n{mutation}\n// HBC function 0, PC 3\nenv[1] = r[1];"));
        let report = initializer(&source);
        for row in array(&report, "rows") {
            no_candidate(row);
            exact_snippet(&source, &row["rhs"], "r[1]");
        }
        if mutation.starts_with("if") {
            assert_eq!(report["rows"][0]["conditional_syntax"], true);
        }
    }
    // A later alias is eligible itself, but cannot resurrect the pre-branch value.
    let source = fragment("// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\nif (flag) r[1] = F[3];\n// HBC function 0, PC 3\nr[2] = r[1];\n// HBC function 0, PC 4\nenv[1] = r[2];");
    let report = initializer(&source);
    let snapshot = definition(&report, &source, "r[2] = r[1]");
    exact_snippet(&source, &snapshot["edges"][0]["expression"], "r[1]");
    assert!(snapshot["edges"][0]["definition_id"].is_null());
    assert_eq!(report["definitions"].as_object().unwrap().len(), 1);
    assert_eq!(report["rows"][0]["unresolved_reads"], 1);
}

#[test]
fn review_block_dispatcher_and_exception_entries_break_prior_aliases() {
    for body in [
        "// HBC function 0, PC 1\nr[1] = F[2];\n{\n// HBC function 0, PC 2\nenv[1] = r[1];\n}",
        "switch (pc) { case 1:\n// HBC function 0, PC 1\nr[1] = F[2]; break;\ncase 2:\n// HBC function 0, PC 2\nenv[1] = r[1]; break; }",
        "// HBC function 0, PC 1\nr[1] = F[2];\ntry {\n// HBC function 0, PC 2\nenv[1] = r[1];\n} catch (err) {\n// HBC function 0, PC 3\nenv[2] = r[1];\n}",
    ] {
        let source = fragment(body);
        let report = initializer(&source);
        for row in array(&report, "rows") { no_candidate(row); }
    }
    let source = fragment(
        "// HBC function 0, PC 10\nr[1] = F[2];\n// HBC function 0, PC 30\nenv[1] = r[1];",
    );
    for boundary in [20, 30] {
        let report = initializers::report_source(&source, 0, &BTreeSet::from([boundary])).unwrap();
        no_candidate(&report["rows"][0]);
    }
    let source = fragment("// HBC function 0, PC 10\nr[1] = F[2];\n// HBC function 0, PC 30\nr[2] = r[1];\n// HBC function 0, PC 40\nenv[1] = r[2];");
    let report = initializers::report_source(&source, 0, &BTreeSet::from([20])).unwrap();
    let snapshot = definition(&report, &source, "r[2] = r[1]");
    assert!(snapshot["edges"][0]["definition_id"].is_null());
    assert_eq!(report["definitions"].as_object().unwrap().len(), 1);
}

#[test]
fn review_updates_delete_shadowing_and_container_aliases_fail_closed() {
    for mutation in [
        "r[1]++;",
        "--r[1];",
        "delete r[1];",
        "delete env[0];",
        "{ let r = []; r[1] = F[3]; }",
        "{ let env = {}; env[0] = F[3]; }",
        "const alias = r; alias[1] = F[3];",
        "r = [];",
        "env = {};",
        "[r[1]] = [F[3]];",
        "eval('r[1] = F[3]');",
        "try { throw 1; } catch (r) { r[1] = F[3]; }",
        "var env = {};",
        "Object.assign(r, {1: F[3]});",
    ] {
        let source = fragment(&format!("// HBC function 0, PC 1\nr[1] = F[2];\n// HBC function 0, PC 2\n{mutation}\n// HBC function 0, PC 3\nenv[1] = r[1];"));
        assert!(
            initializers::report_source(&source, 0, &BTreeSet::new()).is_err(),
            "accepted barrier: {mutation}"
        );
    }
}

#[test]
fn review_opaque_constructor_and_helper_operands_preserve_ambiguous_argument_order() {
    let source = fragment("// HBC function 0, PC 1\nr[1] = F[4];\n// HBC function 0, PC 2\nr[2] = new r[1](r[1], F[5], ...tail);\n// HBC function 0, PC 3\nr[3] = construct(r[1], r[2], [r[2], , F[6], ...tail]);\n// HBC function 0, PC 4\nenv[2] = r[3];");
    let report = initializer(&source);
    let ctor = definition(&report, &source, "r[2] = new r[1](r[1], F[5], ...tail)");
    let node = array(&ctor["syntax"], "nodes")
        .iter()
        .find(|n| n["kind"] == "constructor")
        .unwrap();
    for (operand, expected) in array(node, "operands")
        .iter()
        .zip(["r[1]", "r[1]", "F[5]", "...tail"])
    {
        exact_snippet(&source, &operand["expression"], expected);
    }
    assert_eq!(array(node, "operands").len(), 4);
    let start = occurrence(&source, "new r[1](r[1], F[5], ...tail)", 0).0;
    assert_eq!(
        node["operands"][0]["expression"]["source_span"],
        json!([start + 4, start + 8])
    );
    assert_eq!(
        node["operands"][1]["expression"]["source_span"],
        json!([start + 9, start + 13])
    );
    let helper = definition(
        &report,
        &source,
        "r[3] = construct(r[1], r[2], [r[2], , F[6], ...tail])",
    );
    let shape = array(&helper["syntax"], "nodes")
        .iter()
        .find(|n| n["kind"] == "construct_call_shape")
        .unwrap();
    assert_eq!(shape["operand_count"], 6);
    assert_eq!(shape["operands"][0]["role"], "target_callee");
    assert_eq!(shape["operands"][1]["role"], "receiver");
    for (index, expected) in [
        (0, "r[1]"),
        (1, "r[2]"),
        (2, "r[2]"),
        (4, "F[6]"),
        (5, "...tail"),
    ] {
        exact_snippet(&source, &shape["operands"][index]["expression"], expected);
    }
    assert_eq!(shape["operands"][4]["index"], 2);
    assert_eq!(shape["operands"][5]["index"], 3);
    assert_eq!(array(helper, "edges").len(), 3);
    assert_eq!(
        helper["edges"][1]["definition_id"],
        helper["edges"][2]["definition_id"]
    );
    for def in report["definitions"].as_object().unwrap().values() {
        for forbidden in ["value", "result", "runtime_value", "constructed_value"] {
            assert!(def.get(forbidden).is_none());
        }
    }
    assert!(report["semantics"]
        .as_str()
        .unwrap()
        .contains("opaque syntax"));
}

#[test]
fn review_environment_expressions_are_distinct_syntax_not_frame_identity() {
    let source = fragment("// HBC function 0, PC 1\nr[1] = F[3];\n// HBC function 0, PC 2\nleft.slots[7] = r[1]; right.slots[7] = r[1]; env[7] = r[1];");
    let report = initializer(&source);
    assert_eq!(report["total_stores"], 3);
    for (row, environment) in array(&report, "rows").iter().zip(["left", "right", "env"]) {
        exact_snippet(&source, &row["environment"], environment);
        assert_eq!(row["slot"], 7);
        for forbidden in [
            "frame_id",
            "environment_id",
            "binding_id",
            "runtime_verified",
        ] {
            assert!(row.get(forbidden).is_none());
        }
    }
    assert_ne!(
        report["rows"][0]["source_span"],
        report["rows"][1]["source_span"]
    );
    let link = links::report_source(&source, 0, 8).unwrap();
    assert_eq!(
        array(&link, "records")
            .iter()
            .filter(|r| r["kind"] == "env_slot")
            .count(),
        1
    );
    exact_link(&link, &source, "env[7]", 0);
    for (member, environment) in [("left.slots[7]", "left"), ("right.slots[7]", "right")] {
        let row = exact_link(&link, &source, member, 0);
        let (start, end) = occurrence(&source, environment, 0);
        assert_eq!(row["kind"], "slots_member");
        assert_eq!(row["environment"]["start"], start);
        assert_eq!(row["environment"]["end"], end);
        assert_eq!(
            row["environment"]["source_id"],
            format!("f0:source:{start}:{end}")
        );
    }
}

#[test]
fn review_marker_lookalikes_cannot_silently_relabel_source() {
    for marker in [
        "  // HBC function 9, PC 2",
        "  // HBC function 0, PC -2",
        "// HBC function 0, PC 1 trailing",
        "/* HBC function 0, PC 2 */",
    ] {
        let source = fragment(&format!(
            "// HBC function 0, PC 1\nr[1] = F[2];\n{marker}\nenv[1] = r[1];"
        ));
        assert!(
            links::report_source(&source, 0, 8).is_err(),
            "links ignored marker {marker}"
        );
        assert!(
            initializers::report_source(&source, 0, &BTreeSet::new()).is_err(),
            "initializers ignored marker {marker}"
        );
    }
    for outside in ["// HBC function 0, PC 0", "  // HBC function 0, PC 0"] {
        let source = format!(
            "{outside}\n{}",
            fragment("// HBC function 0, PC 1\nenv[1] = F[2];")
        );
        assert!(links::report_source(&source, 0, 8).is_err());
        assert!(
            initializers::report_source(&source, 0, &BTreeSet::new()).is_err(),
            "ignored out-of-root marker"
        );
    }
    for boundary in [
        "  // HBC function 0, PC 2\n",
        "/* spacer */\r// HBC function 0, PC 2\r",
        "/* spacer */\u{2028}// HBC function 0, PC 2\u{2028}",
    ] {
        let source = fragment(&format!(
            "// HBC function 0, PC 1\nr[1] = F[2];\n{boundary}env[1] = r[1];"
        ));
        if let Ok(report) = links::report_source(&source, 0, 8) {
            assert_eq!(
                exact_link(&report, &source, "env[1]", 0)["pc"],
                2,
                "links reused previous PC for {boundary:?}"
            );
        }
        if let Ok(report) = initializers::report_source(&source, 0, &BTreeSet::new()) {
            assert_eq!(
                report["rows"][0]["pc"], 2,
                "initializers reused previous PC for {boundary:?}"
            );
            exact_snippet(&source, &report["rows"][0]["source"], "env[1] = r[1]");
        }
    }
}

#[test]
fn review_caps_disclose_omissions_without_rewriting_complete_source() {
    let limits = initializer(&fragment("// HBC function 0, PC 0\nreturn 0;"));
    let cap = limits["limits"]["rows"].as_u64().unwrap() as usize;
    let mut body = String::new();
    for i in 0..=cap {
        body.push_str(&format!("// HBC function 0, PC {i}\nenv[{i}] = {i};\n"));
    }
    let source = fragment(&body);
    let original = source.clone();
    // Direct env[N] stores have no generic sites continuation. A capped table
    // must be unavailable, rather than returning an unreplayable cursor.
    assert!(initializers::report_source(&source, 0, &BTreeSet::new()).is_err());
    assert_eq!(source, original);
    assert!(source.contains(&format!("env[{cap}] = {cap};")));

    let exporter = source.replace("env[", "env.slots[");
    let resumed = initializer(&exporter);
    let included = array(&resumed, "rows").len();
    assert!(included > 1000 && included <= cap);
    assert_eq!(resumed["total_stores"], cap + 1);
    assert_eq!(resumed["rows_omitted"], cap + 1 - included);
    assert_eq!(resumed["next_offset"], included);
    assert_eq!(resumed["source_scan_complete"], true);
    assert_eq!(resumed["table_complete"], false);
    exact_snippet(
        &exporter,
        &resumed["rows"][included - 1]["source"],
        &format!("env.slots[{}] = {}", included - 1, included - 1),
    );
    let query = &resumed["continuation_query"];
    assert_eq!(query["command"], "sites");
    assert_eq!(query["function"], 0);
    assert_eq!(query["offset"], included);
    assert_eq!(
        query["flags"],
        json!([
            "0",
            "--kind",
            "slot-write",
            "--offset",
            included.to_string(),
            "--limit",
            "1000",
            "--depth",
            "8",
            "--compact",
            "--max-bytes",
            "16777216"
        ])
    );

    let long = format!("'{}'", "\u{00e9}".repeat(180));
    let arguments = (0..40)
        .map(|i| format!("F[{i}]"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = fragment(&format!(
        "// HBC function 0, PC 1\nenv[1] = {long}; env[2] = sink({arguments});"
    ));
    let report = initializer(&source);
    let preview = &report["rows"][0]["rhs"];
    assert_eq!(span_text(&source, &preview["source_span"]), long);
    assert_eq!(preview["truncated"], true);
    let rendered = preview["javascript"].as_str().unwrap();
    assert!(long.starts_with(rendered));
    assert!(rendered.len() <= 256);
    assert_eq!(preview["bytes_omitted"], long.len() - rendered.len());
    let call = array(&report["rows"][1]["syntax"], "nodes")
        .iter()
        .find(|n| n["kind"] == "call")
        .unwrap();
    assert_eq!(call["operand_count"], 41);
    assert_eq!(array(call, "operands").len(), 32);
    assert_eq!(call["operands_omitted"], 9);
    exact_snippet(&source, &call["operands"][31]["expression"], "F[30]");

    let deep = fragment(&format!(
        "// HBC function 0, PC 1\nenv[1] = {}F[2]{};",
        "(".repeat(300),
        ")".repeat(300)
    ));
    assert!(initializers::report_source(&deep, 0, &BTreeSet::new()).is_err());
    assert!(links::report_source(&deep, 0, 8).is_err());

    let mut chain = String::from("// HBC function 0, PC 1\nr[0] = F[2];\n");
    for i in 1..20 {
        chain.push_str(&format!(
            "// HBC function 0, PC {}\nr[{i}] = r[{}];\n",
            i + 1,
            i - 1
        ));
    }
    chain.push_str("// HBC function 0, PC 21\nenv[1] = r[19];");
    let source = fragment(&chain);
    let report = initializer(&source);
    assert_eq!(report["definitions"].as_object().unwrap().len(), 16);
    assert!(report["dependency_edges_omitted"].as_u64().unwrap() > 0);
    assert_eq!(report["table_complete"], true);
    let tail = definition(&report, &source, "r[19] = r[18]");
    exact_snippet(&source, &tail["edges"][0]["expression"], "r[18]");

    // Core-source acceptance is separate from snippet/output caps. This remains
    // only a size probe, not certification of a real 26 MiB initializer graph.
    let source = fragment(&format!(
        "/*{}*/\n// HBC function 0, PC 1\nenv[1] = F[2];",
        "x".repeat(26 * 1024 * 1024)
    ));
    let report = initializer(&source);
    assert_eq!(report["total_stores"], 1);
    exact_snippet(&source, &report["rows"][0]["source"], "env[1] = F[2]");
    let report = links::report_source(&source, 0, 8).unwrap();
    assert_eq!(report["total"], 2);
    exact_link(&report, &source, "env[1]", 0);
}
