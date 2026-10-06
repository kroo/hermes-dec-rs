pub use hermes_dec_rs::{DecompilerError, DecompilerResult};
#[path = "../src/cli/links.rs"]
mod links;

use serde_json::Value;

fn report(source: &str) -> Value {
    links::report_source(source, 0, 8).unwrap()
}

fn records(report: &Value) -> &[Value] {
    report["records"].as_array().unwrap()
}

fn text<'s>(source: &'s str, location: &Value) -> &'s str {
    &source
        [location["start"].as_u64().unwrap() as usize..location["end"].as_u64().unwrap() as usize]
}

#[test]
fn ignores_fake_markers_and_mentions_in_strings() {
    let source = "const x = '// HBC function 7, PC bad F[2] env[3]'; F[1];";
    let r = report(source);
    assert_eq!(records(&r).len(), 1);
    assert!(records(&r)[0]["pc"].is_null());
}

#[test]
fn template_text_is_ignored_but_interpolations_are_executable_syntax() {
    let r = report("`fake\n// HBC function 7, PC bad\nF[3] env[4] ${F[2]} ${env[1]}`;");
    assert_eq!(records(&r).len(), 2);
    assert_eq!(records(&r)[0]["id"], 2);
    assert_eq!(records(&r)[1]["kind"], "env_slot");
}

#[test]
fn ordinary_comments_and_regex_literals_do_not_invent_links() {
    let r = report(
        "/* fake F[2] env[3] // HBC function 9, PC bad */\n// note: F[4] env[6]\n/F\\[7\\]/; F[1];",
    );
    assert_eq!(records(&r).len(), 1);
}

#[test]
fn exact_utf8_spans_and_rhs_use_entire_raw_fragment() {
    let source = "M[0] = ['界'];\nF[0] = function f(env) {\n// HBC function 0, PC 12\nenv[2] = 'é界' + F[3];\n};";
    let r = report(source);
    let rs = records(&r);
    assert_eq!(text(source, &r["source"]), source);
    assert_eq!(rs.len(), 2);
    assert_eq!(text(source, &rs[0]["source"]), "env[2]");
    assert_eq!(text(source, &rs[0]["rhs"]), "'é界' + F[3]");
    assert_eq!(rs[0]["pc"], 12);
    assert_eq!(text(source, &rs[1]["source"]), "F[3]");
    assert_eq!(rs[1]["pc"], 12);
    let start = source.find("env[2]").unwrap();
    assert_eq!(
        rs[0]["source"]["source_id"],
        format!("f0:source:{start}:{}", start + 6)
    );
}

#[test]
fn numeric_literals_only_no_computation_coercion_or_unary_ids() {
    let r = report("F[n]; F[1+1]; F[-1]; F[+2]; F[1.5]; F['2']; F[2n]; F[(2)]; F[0x2]; F[2e0]; F[9007199254740992]; env[-1]; env[1.5];");
    assert_eq!(records(&r).len(), 2);
    assert!(records(&r).iter().all(|r| r["id"] == 2));
}

#[test]
fn out_of_range_mentions_are_explicitly_unresolved_without_paths() {
    let r = report("F[8]; F[4294967296]; closure(9007199254740991, env);");
    assert_eq!(records(&r).len(), 3);
    for record in records(&r) {
        assert_eq!(record["status"], "unresolved_out_of_range");
        assert!(record.get("path").is_none());
        assert_eq!(record["runtime_verified"], false);
    }
}

#[test]
fn skips_nested_functions_arrows_and_classes_including_their_pc_comments() {
    let source = "F[0] = function f(env) { F[1]; function nested() {\n// HBC function 9, PC bad\nF[2]; env[2] = 2; } const a = () => F[3]; class C { [F[4]]() { env[4]; } static { F[5]; } } env[1]; };";
    let r = report(source);
    assert_eq!(records(&r).len(), 2);
    assert_eq!(r["skipped_scope_count"], 3);
    assert_eq!(records(&r)[0]["id"], 1);
    assert_eq!(records(&r)[1]["kind"], "env_slot");
}

#[test]
fn authored_top_level_functions_are_not_the_exporter_root() {
    let r = report("function f() { F[1]; } (() => env[1])(); F[2];");
    assert_eq!(records(&r).len(), 1);
    assert_eq!(records(&r)[0]["id"], 2);
}

#[test]
fn same_pc_can_contain_multiple_statements_without_binding_inference() {
    let r = report("// HBC function 0, PC 3\nenv[1] = F[2]; F[2](null); env[1];");
    assert_eq!(records(&r).len(), 4);
    assert!(records(&r)
        .iter()
        .all(|r| r["pc"] == 3 && r["binding_verified"] == false));
    assert_eq!(records(&r)[2]["role"], "call_callee");
}

#[test]
fn missing_pc_is_null_not_zero() {
    let r = report("F[0]; env[0];");
    assert!(records(&r).iter().all(|r| r["pc"].is_null()));
    assert_eq!(r["status"], "complete");
    assert!(r["next_offset"].is_null());
}

#[test]
fn malformed_real_pc_comments_fail_closed() {
    for source in [
        "// HBC function 0, PC bad\nF[1];",
        "// HBC function 0, PC -1\nF[1];",
        "// HBC function 0, PC 1 suffix\nF[1];",
        "// HBC function 0 PC 1\nF[1];",
        "//HBC function 0, PC 1\nF[1];",
        "/* HBC function 0, PC 1 */ F[1];",
        "F[1]; // HBC function 0, PC 1\nF[2];",
        "// HBC function 0, PC 4294967296\nF[1];",
    ] {
        assert!(links::report_source(source, 0, 8).is_err(), "{source}");
    }
}

#[test]
fn mismatched_unordered_and_duplicate_pc_comments_fail_closed() {
    for source in [
        "// HBC function 1, PC 0\nF[1];",
        "// HBC function 0, PC 2\nF[1];\n// HBC function 0, PC 1\nF[2];",
        "// HBC function 0, PC 1\nF[1];\n// HBC function 0, PC 1\nF[2];",
    ] {
        assert!(links::report_source(source, 0, 8).is_err());
    }
}

#[test]
fn pc_does_not_leak_out_of_its_containing_block_or_across_markers() {
    let r = report("{\n// HBC function 0, PC 1\nF[1]; } F[2];\n// HBC function 0, PC 2\nF[3];");
    assert_eq!(records(&r)[0]["pc"], 1);
    assert!(records(&r)[1]["pc"].is_null());
    assert_eq!(records(&r)[2]["pc"], 2);
}

#[test]
fn source_expression_crossing_pc_boundary_has_no_pc() {
    let r = report("// HBC function 0, PC 1\nF[\n// HBC function 0, PC 2\n3];");
    assert!(records(&r)[0]["pc"].is_null());
}

#[test]
fn env_reads_writes_delete_compound_and_updates_are_distinguished() {
    let source = "env[0]; env[1] = F[2]; env[2] += env[3]; ++env[4]; delete env[5]; env[6].x = 1;";
    let r = report(source);
    let slots: Vec<_> = records(&r)
        .iter()
        .filter(|r| r["kind"] == "env_slot")
        .collect();
    let access: Vec<_> = slots
        .iter()
        .map(|r| r["access"].as_str().unwrap())
        .collect();
    assert_eq!(
        access,
        [
            "read",
            "write",
            "read_write",
            "read",
            "read_write",
            "delete",
            "read"
        ]
    );
    assert_eq!(text(source, &slots[1]["rhs"]), "F[2]");
    assert_eq!(text(source, &slots[2]["rhs"]), "env[3]");
    assert!(slots[4]["rhs"].is_null());
    assert!(slots[5]["rhs"].is_null());
    assert!(slots[6]["rhs"].is_null());
}

#[test]
fn patterns_and_iteration_targets_do_not_claim_plain_reads_or_rhs_values() {
    let r = report("[env[1]] = values; ({x: env[2]} = value); for (env[3] of values) {} for (env[4] in value) {}");
    assert_eq!(records(&r).len(), 4);
    for slot in records(&r) {
        assert_eq!(slot["access"], "write");
        assert_eq!(slot["role"], "pattern_or_iteration_write");
        assert!(slot["rhs"].is_null());
    }
}

#[test]
fn helper_and_member_roles_are_only_immediate_syntactic_contexts() {
    let r = report("closure(2, env); apply(F[3], null, []); arbitrary(F[4]); F[5].x; closure(n, env); other.closure(7, env);");
    let roles: Vec<_> = records(&r)
        .iter()
        .map(|r| r["role"].as_str().unwrap())
        .collect();
    assert_eq!(
        roles,
        [
            "closure_argument",
            "helper_call_argument",
            "call_argument",
            "member_read"
        ]
    );
}

#[test]
fn exact_env_and_f_names_do_not_match_other_objects_or_escaped_names() {
    let r = report(
        "other.env[1]; environment[3]; other.F[4]; \\u0046[5]; \\u0065nv[6]; env?.[7]; F[1];",
    );
    assert_eq!(records(&r).len(), 1);
    assert_eq!(records(&r)[0]["id"], 1);
}

#[test]
fn deterministic_order_and_source_ids_are_exact_input_local_byte_joins() {
    let source = "env[3] = [F[2], env[1], closure(4, env)];";
    let first = report(source);
    assert_eq!(first, report(source));
    for (ordinal, record) in records(&first).iter().enumerate() {
        assert_eq!(record["ordinal"], ordinal);
        let span = &record["source"];
        assert_eq!(
            span["source_id"],
            format!("f0:source:{}:{}", span["start"], span["end"])
        );
        assert!(!text(source, span).is_empty());
    }
}

#[test]
fn production_limits_support_large_initializers() {
    let r = report("F[1];");
    assert_eq!(r["limits"]["source_bytes"], 64 * 1024 * 1024);
    assert_eq!(r["limits"]["syntax_work"], 32_000_000);
    assert_eq!(r["limits"]["records"], 131_072);
    assert_eq!(r["limits"]["output_bytes"], 64 * 1024 * 1024);
    assert_eq!(r["limits"]["budget_policy"], "reject_before_output");
}

#[test]
fn depth_cap_fails_before_returning_an_inventory() {
    let deep = format!("{}F[1]{};", "(".repeat(160), ")".repeat(160));
    assert!(links::report_source(&deep, 0, 8).is_err());
}

#[test]
fn comment_containment_work_is_bounded_even_for_many_sibling_scopes() {
    let source = "{ /* ordinary */ F[1]; }".repeat(4500);
    let result = links::report_source(&source, 0, 8).unwrap();
    assert_eq!(records(&result).len(), 4500);
    assert!(result["work"].as_u64().unwrap() < 1_000_000);
    assert!(records(&result).iter().all(|record| record["pc"].is_null()));
}

#[test]
fn valid_exporter_over_one_mib_keeps_byte_spans_and_excludes_scaffolding() {
    let source = format!(
        "M[0] = ['{}'];\nF[0] = function f(env) {{\n// HBC function 0, PC 0\nenv[1] = F[2];\n}};",
        "x".repeat(1_048_576)
    );
    assert!(source.len() > 1_048_576);
    let r = report(&source);
    assert_eq!(records(&r).len(), 2);
    assert_eq!(text(&source, &records(&r)[0]["source"]), "env[1]");
    assert_eq!(text(&source, &records(&r)[1]["source"]), "F[2]");
    assert!(records(&r).iter().all(|r| r["pc"] == 0));
    assert_eq!(r["source"]["end"], source.len());
}

#[test]
fn many_sibling_containers_do_not_multiply_comment_query_work() {
    let mut source = String::from("F[0] = function(env) {\n");
    for pc in 0..5000 {
        source.push_str(&format!("{{\n// HBC function 0, PC {pc}\nF[1];\n}}\n"));
    }
    source.push_str("};\n");
    let result = report(&source);
    assert_eq!(records(&result).len(), 5000);
    for (pc, record) in records(&result).iter().enumerate() {
        assert_eq!(record["pc"], pc);
    }
    assert!(result["work"].as_u64().unwrap() < 1_000_000);
}

#[test]
fn invalid_js_wrong_root_and_pc_outside_root_are_errors() {
    for source in [
        "F[1] = ;",
        "F[1] = function f() {};",
        "F[0] = function f() {}; F[0] = function g() {};",
        "// HBC function 0, PC 1\nF[0] = function f() {};",
    ] {
        assert!(links::report_source(source, 0, 8).is_err());
    }
    assert!(links::report_source("F[1];", 8, 8).is_err());
}

#[test]
fn hostile_source_is_never_returned_as_a_shell_command_or_terminal_snippet() {
    let source = "const s = '\\x1b[31m; $(touch /tmp/never)'; env[1] = s; F[2];";
    let r = report(source);
    let serialized = serde_json::to_string(&r).unwrap();
    assert!(!serialized.contains("touch"));
    assert!(!serialized.contains('\u{1b}'));
    assert!(records(&r).iter().all(|r| r.get("command").is_none()));
}

#[test]
fn static_slots_reads_writes_compound_updates_and_delete_keep_syntax_roles() {
    let source = "// HBC function 0, PC 7\nr[1].slots[2]; r[3].slots[4] = F[2]; r[5].slots[6] += env[1]; r[7].slots[8]++; delete r[9].slots[10];";
    let r = report(source);
    let slots: Vec<_> = records(&r)
        .iter()
        .filter(|r| r["kind"] == "slots_member")
        .collect();
    assert_eq!(slots.len(), 5);
    let access: Vec<_> = slots
        .iter()
        .map(|r| r["access"].as_str().unwrap())
        .collect();
    assert_eq!(
        access,
        ["read", "write", "read_write", "read_write", "delete"]
    );
    assert_eq!(text(source, &slots[1]["rhs"]), "F[2]");
    assert_eq!(text(source, &slots[2]["rhs"]), "env[1]");
    assert!(slots[3]["rhs"].is_null());
    assert!(slots[4]["rhs"].is_null());
    assert!(slots.iter().all(|r| r["pc"] == 7));
    assert_eq!(text(source, &slots[0]["environment"]), "r[1]");
    assert_eq!(text(source, &slots[4]["environment"]), "r[9]");
}

#[test]
fn slot_used_as_property_write_address_is_a_read_not_a_slot_write() {
    let source = "r[0].slots[1].field = r[2].slots[3]; r[4].slots[5][env[6]] += 1; delete r[7].slots[8].field;";
    let r = report(source);
    let slots: Vec<_> = records(&r)
        .iter()
        .filter(|r| r["kind"] == "slots_member")
        .collect();
    assert_eq!(slots.len(), 4);
    assert!(slots
        .iter()
        .all(|r| r["access"] == "read" && r["rhs"].is_null()));
    assert_eq!(text(source, &slots[0]["environment"]), "r[0]");
    let env = records(&r)
        .iter()
        .find(|r| r["kind"] == "env_slot")
        .unwrap();
    assert_eq!(env["access"], "read");
}

#[test]
fn nested_environment_spans_are_exact_utf8_source_not_frame_identities() {
    let source =
        "const label = '界'; env[1].frames[r[2]].slots[3] = r[4].slots[5].slots[6]; env[7];";
    let r = report(source);
    let slots: Vec<_> = records(&r)
        .iter()
        .filter(|r| r["kind"] == "slots_member")
        .collect();
    assert_eq!(slots.len(), 3);
    assert_eq!(
        text(source, &slots[0]["environment"]),
        "env[1].frames[r[2]]"
    );
    assert_eq!(text(source, &slots[0]["rhs"]), "r[4].slots[5].slots[6]");
    assert_eq!(text(source, &slots[1]["environment"]), "r[4]");
    assert_eq!(text(source, &slots[2]["environment"]), "r[4].slots[5]");
    for slot in slots {
        assert_eq!(slot["binding_verified"], false);
        assert_eq!(slot["runtime_verified"], false);
        assert!(slot.get("frame_id").is_none());
        assert!(slot.get("lexical_identity").is_none());
        assert!(slot["environment"].get("resolved_register").is_none());
    }
    let envs: Vec<_> = records(&r)
        .iter()
        .filter(|r| r["kind"] == "env_slot")
        .collect();
    assert_eq!(envs.len(), 2);
    assert!(envs.iter().all(|r| r["access"] == "read"));
}

#[test]
fn slots_properties_require_static_nonoptional_shape_and_numeric_literal_ids() {
    let r = report("r[0]['slots'][1]; r[0]?.slots[2]; r[0].slots?.[3]; r[0].slots[n]; r[0].slots[-1]; r[0].slots[1.5]; r[0].slots['2']; r[0].slots[1+1]; r[0].s\\u006cots[4]; r[0].slots[0x5]; env[6]; env.slots[7];");
    assert_eq!(records(&r).len(), 3);
    assert_eq!(records(&r)[0]["kind"], "slots_member");
    assert_eq!(records(&r)[0]["id"], 5);
    assert_eq!(records(&r)[1]["kind"], "env_slot");
    assert_eq!(records(&r)[2]["kind"], "slots_member");
    assert_eq!(records(&r)[2]["id"], 7);
}

#[test]
fn slots_mentions_in_nested_scopes_or_literal_text_are_not_indexed() {
    let r = report("'r[0].slots[1]'; `r[2].slots[3]`; /* r[4].slots[5] */ function f() { r[6].slots[7]; } (() => r[8].slots[9]); r[10].slots[11];");
    assert_eq!(records(&r).len(), 1);
    assert_eq!(records(&r)[0]["kind"], "slots_member");
    assert_eq!(records(&r)[0]["id"], 11);
}
