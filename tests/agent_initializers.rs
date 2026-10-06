// Compile the owned source directly as well as through the parent's CLI wiring.
pub use hermes_dec_rs::{DecompilerError, DecompilerResult};
#[path = "../src/cli/initializers.rs"]
mod initializers;

use serde_json::Value;
use std::collections::BTreeSet;

fn source(body: &str) -> String {
    format!("M[0] = ['authored',0,0];\nF[0] = function f(env) {{ const r=[]; let pc=0; switch(pc) {{ case 0: {{\n{body}\n}} default: throw 0; }} }};")
}

fn report(body: &str) -> Value {
    initializers::report_source(&source(body), 0, &BTreeSet::new()).unwrap()
}

fn row(report: &Value) -> &Value {
    &report["rows"][0]
}

fn join(source: &str, snippet: &Value) {
    let a = snippet["source_span"][0].as_u64().unwrap() as usize;
    let b = snippet["source_span"][1].as_u64().unwrap() as usize;
    let raw = source.get(a..b).expect("exact UTF-8 byte span");
    let preview = snippet["javascript"].as_str().unwrap();
    assert!(raw.starts_with(preview));
    assert_eq!(
        raw.len(),
        snippet["original_bytes"].as_u64().unwrap() as usize
    );
    assert_eq!(
        raw.len() - preview.len(),
        snippet["bytes_omitted"].as_u64().unwrap() as usize
    );
    if snippet["truncated"] == false {
        assert_eq!(raw, preview);
    }
}

#[test]
fn plain_register_alias_chains_are_local_source_candidates() {
    let v=report("// HBC function 0, PC 0\nr[1]='seed';\n// HBC function 0, PC 2\nr[2]=r[1];\n// HBC function 0, PC 4\nr[3]=r[2];\n// HBC function 0, PC 6\nr[7].slots[9]=r[3];");
    assert_eq!(
        row(&v)["definition_ids_source_order"]
            .as_array()
            .unwrap()
            .len(),
        3
    );
    assert_eq!(row(&v)["unresolved_reads"], 0);
    assert_eq!(v["definitions"].as_object().unwrap().len(), 3);
    assert_eq!(
        row(&v)["rhs_edges"][0]["status"],
        "local_definition_candidate"
    );
}

#[test]
fn same_pc_store_read_does_not_resolve_a_definition() {
    let v = report("// HBC function 0, PC 0\nr[1]=1; r[7].slots[0]=r[1];");
    assert_eq!(
        row(&v)["rhs_edges"][0]["status"],
        "unresolved_same_pc_write"
    );
    assert!(v["definitions"].as_object().unwrap().is_empty());
}

#[test]
fn duplicate_same_pc_writes_poison_the_next_source() {
    let v = report(
        "// HBC function 0, PC 0\nr[1]=1; r[1]=2;\n// HBC function 0, PC 2\nr[7].slots[0]=r[1];",
    );
    assert!(row(&v)["rhs_edges"][0]["definition_id"].is_null());
    assert_eq!(row(&v)["unresolved_reads"], 1);
}

#[test]
fn conditional_writes_are_not_unique_prior_definitions() {
    for assignment in [
        "if(flag) r[1]=2;",
        "flag && (r[1]=2);",
        "flag ? (r[1]=2) : (r[1]=3);",
        "r[1] ||= 2;",
    ] {
        let v=report(&format!("// HBC function 0, PC 0\nr[1]=1;\n// HBC function 0, PC 2\n{assignment}\n// HBC function 0, PC 4\nr[7].slots[0]=r[1];"));
        assert!(
            row(&v)["rhs_edges"][0]["definition_id"].is_null(),
            "{assignment}"
        );
    }
}

#[test]
fn conditional_store_is_retained_but_dependencies_fail_closed() {
    let v = report(
        "// HBC function 0, PC 0\nr[1]=1;\n// HBC function 0, PC 2\nif(flag) r[7].slots[0]=r[1];",
    );
    assert_eq!(row(&v)["conditional_syntax"], true);
    assert_eq!(
        row(&v)["rhs_edges"][0]["status"],
        "unresolved_control_barrier"
    );
}

#[test]
fn block_case_and_early_exit_boundaries_are_barriers() {
    for barrier in ["{}", "return;", "break;", "pc=4; continue;"] {
        let v=report(&format!("// HBC function 0, PC 0\nr[1]=1; {barrier}\n// HBC function 0, PC 2\nr[7].slots[0]=r[1];"));
        assert!(
            row(&v)["rhs_edges"][0]["definition_id"].is_null(),
            "{barrier}"
        );
    }
    let v=report("// HBC function 0, PC 0\nr[1]=1;\n} case 2: {\n// HBC function 0, PC 2\nr[7].slots[0]=r[1];");
    assert!(row(&v)["rhs_edges"][0]["definition_id"].is_null());
}

#[test]
fn exception_boundaries_between_markers_clear_candidates() {
    let js =
        source("// HBC function 0, PC 0\nr[1]=1;\n// HBC function 0, PC 4\nr[7].slots[0]=r[1];");
    for pc in [1, 2, 4] {
        let v = initializers::report_source(&js, 0, &BTreeSet::from([pc])).unwrap();
        assert!(row(&v)["rhs_edges"][0]["definition_id"].is_null());
    }
    let v = initializers::report_source(&js, 0, &BTreeSet::new()).unwrap();
    assert!(row(&v)["rhs_edges"][0]["definition_id"].is_string());
}

#[test]
fn reassignment_uses_the_latest_unique_source_write() {
    let v=report("// HBC function 0, PC 0\nr[1]='old';\n// HBC function 0, PC 2\nr[1]='new';\n// HBC function 0, PC 4\nr[7].slots[0]=r[1];");
    let key = row(&v)["rhs_edges"][0]["definition_id"].as_str().unwrap();
    assert_eq!(v["definitions"][key]["source_pc"], 2);
    assert_eq!(v["definitions"][key]["rhs"]["javascript"], "'new'");
}

#[test]
fn environment_expression_is_separate_and_never_an_rhs_origin() {
    let v=report("// HBC function 0, PC 0\nr[7]='not-an-environment-value';\n// HBC function 0, PC 2\nr[1]='rhs';\n// HBC function 0, PC 4\nr[7].slots[0]=r[1];");
    assert_eq!(row(&v)["environment"]["javascript"], "r[7]");
    assert_eq!(v["definitions"].as_object().unwrap().len(), 1);
    assert!(v["definitions"]
        .as_object()
        .unwrap()
        .values()
        .all(|d| d["defines_register"] == 1));
}

#[test]
fn constructors_and_helper_arguments_preserve_syntax_order() {
    let v=report("// HBC function 0, PC 0\nr[1]=construct(Ctor, receiver, ['last', 'first', false]);\n// HBC function 0, PC 2\nr[7].slots[0]=[r[1], new Thing('b','a')];");
    let key = row(&v)["rhs_edges"][0]["definition_id"].as_str().unwrap();
    let shape = v["definitions"][key]["syntax"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["kind"] == "construct_call_shape")
        .unwrap();
    let args: Vec<_> = shape["operands"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|o| o["role"] == "argument_syntax")
        .map(|o| o["expression"]["javascript"].as_str().unwrap())
        .collect();
    assert_eq!(args, ["'last'", "'first'", "false"]);
    let constructor = row(&v)["syntax"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["kind"] == "constructor")
        .unwrap();
    assert_eq!(
        constructor["operands"][1]["expression"]["javascript"],
        "'b'"
    );
    assert_eq!(
        constructor["operands"][2]["expression"]["javascript"],
        "'a'"
    );
    assert!(v["semantics"].as_str().unwrap().contains("opaque syntax"));
}

#[test]
fn object_array_member_and_call_summaries_are_ordered_not_evaluated() {
    let v=report("// HBC function 0, PC 0\nr[7].slots[1]={z:call(obj.field,2,1),a:[3,,...items],['q']:obj[key]};");
    let nodes = row(&v)["syntax"]["nodes"].as_array().unwrap();
    let kinds: Vec<_> = nodes.iter().map(|n| n["kind"].as_str().unwrap()).collect();
    assert!(
        kinds.contains(&"object")
            && kinds.contains(&"array")
            && kinds.contains(&"call")
            && kinds.contains(&"member")
            && kinds.contains(&"computed_member")
    );
    assert_eq!(nodes[0]["operands"][0]["expression"]["javascript"], "z");
    assert_eq!(nodes[0]["operands"][2]["expression"]["javascript"], "a");
    let array = nodes.iter().find(|n| n["kind"] == "array").unwrap();
    assert_eq!(array["operands"][1]["role"], "hole");
    assert_eq!(array["operands"][2]["role"], "spread");
}

#[test]
fn escaped_strings_and_multibyte_spans_join_raw_fragments_exactly() {
    let js=source("// HBC function 0, PC 0\nr[1]='界\\n\\u0061\\\"';\n// HBC function 0, PC 2\nr[7].slots[0]=[r[1],'é'];");
    let v = initializers::report_source(&js, 0, &BTreeSet::new()).unwrap();
    for d in v["definitions"].as_object().unwrap().values() {
        join(&js, &d["source"]);
        join(&js, &d["rhs"]);
        assert_eq!(d["rhs"]["javascript"], "'界\\n\\u0061\\\"'");
    }
    for r in v["rows"].as_array().unwrap() {
        join(&js, &r["source"]);
        join(&js, &r["environment"]);
        join(&js, &r["rhs"]);
        for n in r["syntax"]["nodes"].as_array().unwrap() {
            join(&js, &n["expression"]);
            for o in n["operands"].as_array().unwrap() {
                join(&js, &o["expression"]);
            }
        }
    }
    for (key, d) in v["definitions"].as_object().unwrap() {
        assert_eq!(
            *key,
            format!("0:{}:{}", d["source_span"][0], d["source_span"][1])
        );
    }
    assert_eq!(
        v,
        initializers::report_source(&js, 0, &BTreeSet::new()).unwrap()
    );
}

#[test]
fn numeric_environment_keys_are_syntax_not_decoded_named_properties() {
    let v=report("// HBC function 0, PC 0\nenv[0xA]=1; r[7].slots[2]=2; env['3']=3; r[7].slots[key]=4; obj[5]=5;");
    assert_eq!(v["total_stores"], 2);
    assert_eq!(v["rows"][0]["slot"], 10);
    assert_eq!(v["rows"][1]["slot"], 2);
    assert_eq!(v["rows"][0]["environment"]["javascript"], "env");
}

#[test]
fn authored_shadowing_and_unsupported_syntax_fail_closed() {
    for body in [
        "let r=[];",
        "let env=[];",
        "r=[];",
        "env={};",
        "use(r);",
        "r [1]=2;",
        "r[1]++;",
        "delete r[1];",
        "while(flag) {}",
        "for(let i=0;i<2;i++) {}",
        "label: {}",
        "function nested() {}",
        "r[1]=()=>1;",
        "r[1]=class {};",
        "r[1]=x?.y;",
        "eval('r[1]=2');",
        "({x:r[1]}=x);",
        "r[7].slots[0]+=1;",
        "switch(flag) {case 1: break;}",
    ] {
        let js = source(&format!(
            "// HBC function 0, PC 0\n{body}\nr[7].slots[0]=r[1];"
        ));
        assert!(
            initializers::report_source(&js, 0, &BTreeSet::new()).is_err(),
            "{body}"
        );
    }
}

#[test]
fn missing_wrong_duplicate_and_crossing_pc_markers_are_rejected() {
    for body in [
        "r[7].slots[0]=1;",
        "// HBC function 1, PC 0\nr[7].slots[0]=1;",
        "// HBC function 0, PC 0\nr[1]=1;\n// HBC function 0, PC 0\nr[7].slots[0]=1;",
        "// HBC function 0, PC 0\nr[7].slots[0]=[\n// HBC function 0, PC 2\n1];",
    ] {
        assert!(initializers::report_source(&source(body), 0, &BTreeSet::new()).is_err());
    }
    assert!(initializers::report_source(
        &source("// HBC function 0, PC 0\nreturn;"),
        1,
        &BTreeSet::new()
    )
    .is_err());
}

#[test]
fn chain_depth_and_operand_omissions_are_explicit() {
    let mut body = String::from("// HBC function 0, PC 0\nr[0]=opaque();\n");
    for n in 1..24 {
        body.push_str(&format!(
            "// HBC function 0, PC {n}\nr[{n}]=r[{}];\n",
            n - 1
        ));
    }
    body.push_str("// HBC function 0, PC 24\nr[99].slots[0]=r[23];");
    let v = report(&body);
    assert!(row(&v)["dependency_edges_omitted"].as_u64().unwrap() > 0);
    let v = report(&format!(
        "// HBC function 0, PC 0\nr[7].slots[0]=[{}];",
        (0..80)
            .map(|i| format!("r[{i}]"))
            .collect::<Vec<_>>()
            .join(",")
    ));
    assert_eq!(row(&v)["dependency_edges_omitted"], 48);
    assert_eq!(row(&v)["syntax"]["operands_omitted"], 48);
}

#[test]
fn row_cap_has_exact_raw_store_cursor_and_generic_continuation() {
    let body = (0..16389)
        .map(|i| format!("// HBC function 0, PC {i}\nr[7].slots[{i}]=0;\n"))
        .collect::<String>();
    let v = report(&body);
    let count = v["rows"].as_array().unwrap().len();
    assert!(count > 1000 && count <= 16384);
    assert_eq!(v["rows_omitted"], 16389 - count);
    assert_eq!(v["next_offset"], count);
    assert_eq!(v["continuation_query"]["command"], "sites");
    assert_eq!(v["continuation_query"]["offset"], count);
    assert_eq!(v["table_complete"], false);
    assert_eq!(v["source_scan_complete"], true);
}

#[test]
fn modest_tables_are_not_cut_off_at_one_thousand_stores() {
    let body = (0..1200)
        .map(|i| format!("// HBC function 0, PC {i}\nr[7].slots[{i}]=0;\n"))
        .collect::<String>();
    let v = report(&body);
    assert_eq!(v["rows"].as_array().unwrap().len(), 1200);
    assert_eq!(v["table_complete"], true);
    assert!(v["next_offset"].is_null());
}

#[test]
fn marker_indentation_and_all_js_line_terminators_preserve_exact_pcs() {
    for newline in ["\n", "\r", "\r\n", "\u{2028}", "\u{2029}"] {
        let body=format!("   // HBC function 0, PC 0{newline}r[1]=1;{newline}\t// HBC function 0, PC 2{newline}r[7].slots[0]=r[1];");
        let v = report(&body);
        assert_eq!(row(&v)["pc"], 2, "{newline:?}");
        let key = row(&v)["rhs_edges"][0]["definition_id"].as_str().unwrap();
        assert_eq!(v["definitions"][key]["source_pc"], 0);
    }
    for malformed in [
        "//HBC function 0, PC 2",
        "// HBC  function 0, PC 2",
        "/* HBC function 0, PC 2 */",
        "x=1; // HBC function 0, PC 2",
    ] {
        let js = source(&format!(
            "// HBC function 0, PC 0\nr[1]=1;\n{malformed}\nr[7].slots[0]=r[1];"
        ));
        assert!(
            initializers::report_source(&js, 0, &BTreeSet::new()).is_err(),
            "{malformed}"
        );
    }
}

#[test]
fn output_cap_returns_a_partial_bounded_report_not_a_false_complete_table() {
    let item = format!(
        "{{['{}']:['{}','{}']}}",
        "x".repeat(300),
        "y".repeat(300),
        "z".repeat(300)
    );
    let array = std::iter::repeat_n(item, 32).collect::<Vec<_>>().join(",");
    let body = (0..1000)
        .map(|i| format!("// HBC function 0, PC {i}\nr[7].slots[{i}]=[{array}];\n"))
        .collect::<String>();
    let v = report(&body);
    let count = v["rows"].as_array().unwrap().len();
    assert!(count > 0 && count < 1000);
    assert_eq!(v["next_offset"], count);
    assert_eq!(v["rows_omitted"], 1000 - count);
    assert_eq!(v["table_complete"], false);
    assert!(serde_json::to_vec(&v).unwrap().len() <= 16_777_216);
}

#[test]
fn large_26_mib_raw_fragments_are_supported_and_source_cap_is_64_mib() {
    let body = format!(
        "/*{}*/\n// HBC function 0, PC 0\nr[7].slots[0]=1;",
        "x".repeat(26 * 1024 * 1024)
    );
    let v = report(&body);
    assert_eq!(v["total_stores"], 1);
    assert_eq!(v["limits"]["source_bytes"], 64 * 1024 * 1024);
    let too_large = " ".repeat(64 * 1024 * 1024 + 1);
    assert!(initializers::report_source(&too_large, 0, &BTreeSet::new())
        .unwrap_err()
        .to_string()
        .contains("source byte cap"));
}

#[test]
fn deep_syntax_and_long_utf8_snippets_are_bounded() {
    let body = format!(
        "// HBC function 0, PC 0\nr[7].slots[0]={}1{};",
        "(".repeat(300),
        ")".repeat(300)
    );
    assert!(initializers::report_source(&source(&body), 0, &BTreeSet::new()).is_err());
    let unary = source(&format!(
        "// HBC function 0, PC 0\nr[7].slots[0]={}1;",
        "typeof ".repeat(300)
    ));
    assert!(initializers::report_source(&unary, 0, &BTreeSet::new()).is_err());
    let js = source(&format!(
        "// HBC function 0, PC 0\nr[7].slots[0]='{}';",
        "界".repeat(500)
    ));
    let v = initializers::report_source(&js, 0, &BTreeSet::new()).unwrap();
    join(&js, &row(&v)["rhs"]);
    assert_eq!(row(&v)["rhs"]["truncated"], true);
}

#[test]
fn field_reads_and_branch_results_remain_opaque_syntax() {
    let v = report("// HBC function 0, PC 0\nr[7].slots[0]=flag ? obj.field : opaque();");
    let nodes = row(&v)["syntax"]["nodes"].as_array().unwrap();
    assert_eq!(nodes[0]["kind"], "conditional");
    assert!(nodes.iter().any(|n| n["kind"] == "member"));
    assert!(nodes.iter().any(|n| n["kind"] == "call"));
    assert!(row(&v).get("value").is_none());
    let v=report("// HBC function 0, PC 0\nr[1]=flag ? obj.field : opaque();\n// HBC function 0, PC 2\nr[7].slots[0]=r[1];");
    assert!(row(&v)["rhs_edges"][0]["definition_id"].is_null());
}

#[test]
fn empty_syntax_table_explicitly_cannot_prove_absence() {
    let v = report("// HBC function 0, PC 0\nreturn;");
    assert_eq!(v["total_stores"], 0);
    assert!(v["rows"].as_array().unwrap().is_empty());
    assert!(v["negative_result_policy"]
        .as_str()
        .unwrap()
        .contains("cannot prove absence"));
}

#[test]
fn repeated_chain_queries_have_an_independent_aggregate_work_budget() {
    let mut body = String::from(
        "// HBC function 0, PC 0\nr[0]=opaque();\n// HBC function 0, PC 1\nr[1]=r[0];\n",
    );
    for pc in 2..32 {
        body.push_str(&format!(
            "// HBC function 0, PC {pc}\nr[{pc}]=r[{}]+r[{}];\n",
            pc - 1,
            pc - 2
        ));
    }
    for pc in 32..4032 {
        body.push_str(&format!(
            "// HBC function 0, PC {pc}\nr[99].slots[0]=r[31];\n"
        ));
    }
    let v = report(&body);
    assert_eq!(v["stop_reason"], "query_work");
    let included = v["rows"].as_array().unwrap().len();
    assert!(included > 0 && included < 4000);
    assert_eq!(v["next_offset"], included);
    assert_eq!(v["rows_omitted"], 4000 - included);
    assert!(v["query_work_used"].as_u64().unwrap() <= v["limits"]["query_work"].as_u64().unwrap());
    assert!(serde_json::to_vec(&v).unwrap().len() <= 16_777_216);
}
