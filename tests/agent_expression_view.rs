pub use hermes_dec_rs::{DecompilerError, DecompilerResult};
#[path = "../src/cli/expression_view.rs"]
mod expression_view;

use oxc_allocator::Allocator;
use oxc_ast::ast::{Expression, Statement};
use oxc_parser::Parser;
use oxc_span::{GetSpan, SourceType, Span};
use serde_json::Value;

fn project(source: &str) -> Vec<Value> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let wanted: Vec<_> = parsed
        .program
        .body
        .iter()
        .filter_map(|s| match s {
            Statement::ExpressionStatement(s) => Some(s.expression.span()),
            _ => None,
        })
        .collect();
    expression_view::views(source, &parsed.program, &wanted).unwrap()
}

fn nodes(view: &Value) -> &[Value] {
    view["nodes"].as_array().unwrap()
}

fn kind<'a>(view: &'a Value, name: &str) -> &'a Value {
    nodes(view).iter().find(|n| n["kind"] == name).unwrap()
}

fn edge<'a>(view: &'a Value, edge: &Value) -> &'a Value {
    &nodes(view)[edge["node"].as_u64().unwrap() as usize]
}

fn omitted(view: &Value, reason: &str) -> bool {
    view["omissions"]
        .as_array()
        .unwrap()
        .iter()
        .any(|r| r == reason)
}

fn rhs(view: &Value) -> &Value {
    edge(view, &kind(view, "assignment")["rhs"])
}

#[test]
fn self_read_stays_symbolic_and_compound_assignment_is_syntax() {
    let v = project("r[1] += r[1].property;").remove(0);
    let a = kind(&v, "assignment");
    assert_eq!(a["operator"], "+=");
    let destination = edge(&v, &a["destination"]);
    assert_eq!(destination["register"], 1);
    assert_eq!(destination["access"], "destination");
    let rhs = edge(&v, &a["rhs"]);
    let read = edge(&v, &rhs["receiver"]);
    assert_eq!(read["register"], 1);
    assert_eq!(read["access"], "read");
    assert_eq!(edge(&v, &rhs["key"])["source"]["preview"], "property");
    assert!(nodes(&v).iter().all(|n| n.get("value").is_none()));
}

#[test]
fn property_call_and_helper_roles_are_separate() {
    let vs = project("r[0] = r[2].method(r[3], ...r[4]); r[1] = apply(r[5], r[6], [r[7], r[8]]);");
    let c = kind(&vs[0], "call");
    assert_eq!(edge(&vs[0], &c["callee"])["kind"], "member");
    assert_eq!(c["arguments"][0]["position"], 0);
    assert_eq!(edge(&vs[0], &c["arguments"][1]["node"])["kind"], "spread");
    let c = kind(&vs[1], "call");
    assert_eq!(c["syntactic_helper_shape"], "apply");
    assert_eq!(c["helper_identity_verified"], false);
    assert_eq!(c["arguments"][0]["role"], "syntactic_callee");
    assert_eq!(c["arguments"][1]["role"], "receiver");
    assert_eq!(c["arguments"][2]["role"], "user_arguments_array");
    assert_eq!(
        edge(&vs[1], &c["arguments"][2]["node"])["items"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    let v = project("r[0] = obj.apply(r[1], r[2], []);").remove(0);
    assert!(kind(&v, "call").get("syntactic_helper_shape").is_none());
}

#[test]
fn constructor_preallocation_and_new_remain_distinct() {
    let vs = project("r[1] = {}; r[2] = construct(r[3], r[1], [r[4]]); r[5] = new r[6](r[7]);");
    let c = kind(&vs[1], "call");
    assert_eq!(c["syntactic_helper_shape"], "construct");
    assert_eq!(c["arguments"][1]["role"], "preallocated_receiver");
    assert_eq!(edge(&vs[1], &c["arguments"][1]["node"])["register"], 1);
    assert_eq!(kind(&vs[2], "new")["arguments"][0]["role"], "argument");
}

#[test]
fn nested_aliases_and_captured_names_are_not_resolved() {
    let source = "r[1] = r[2]; function f() { r[3] = alias(r[1], captured); }";
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    assert!(parsed.errors.is_empty());
    let start = source.find("r[3]").unwrap();
    let end = source[start..].find(';').unwrap() + start;
    let v = expression_view::views(
        source,
        &parsed.program,
        &[Span::new(start as u32, end as u32)],
    )
    .unwrap()
    .remove(0);
    assert!(nodes(&v)
        .iter()
        .any(|n| n["source"]["preview"] == "captured" && n["symbolic"] == true));
    assert!(nodes(&v).iter().any(|n| n["register"] == 1));
    assert!(!nodes(&v).iter().any(|n| n["register"] == 2));
}

#[test]
fn arrays_preserve_holes_spreads_and_positions() {
    let v = project("r[0] = [1,, ...r[2], undefined];").remove(0);
    let a = kind(&v, "array");
    assert_eq!(a["items_total"], 4);
    assert_eq!(edge(&v, &a["items"][1]["node"])["kind"], "hole");
    assert_eq!(edge(&v, &a["items"][2]["node"])["kind"], "spread");
    assert_eq!(a["items"][3]["position"], 3);
    assert_eq!(edge(&v, &a["items"][3]["node"])["kind"], "identifier");
}

#[test]
fn raw_literals_and_fake_markers_are_never_decoded_or_pc_mapped() {
    let vs = project("r[1] = '\\uD800'; r[2] = '// HBC function 0, PC 999'; r[3] = 0xff; r[4] = 123n; r[5] = /a/g;");
    assert_eq!(rhs(&vs[0])["source"]["preview"], "'\\uD800'");
    assert_eq!(rhs(&vs[0])["literal_kind"], "string");
    assert_eq!(rhs(&vs[2])["source"]["preview"], "0xff");
    assert_eq!(rhs(&vs[3])["literal_kind"], "bigint");
    assert_eq!(rhs(&vs[4])["literal_kind"], "regexp");
    for v in vs {
        assert!(!v.to_string().contains("\"pc\""));
        assert!(nodes(&v).iter().all(|n| n.get("value").is_none()));
    }
}

#[test]
fn multibyte_source_and_preview_caps_are_byte_exact() {
    let source = format!("r[1] = '{}';", "\u{1f642}".repeat(100));
    let v = project(&source).remove(0);
    let literal = rhs(&v);
    let s = &literal["source"];
    assert_eq!(s["original_bytes"], 402);
    assert_eq!(s["truncated"], true);
    assert!(s["preview"].as_str().unwrap().len() <= 256);
    assert!(omitted(&v, "source"));
    for n in nodes(&v) {
        let s = &n["source"];
        let start = s["start"].as_u64().unwrap() as usize;
        let end = s["end"].as_u64().unwrap() as usize;
        assert_eq!(
            source[start..end].len() as u64,
            s["original_bytes"].as_u64().unwrap()
        );
        assert!(source[start..end].starts_with(s["preview"].as_str().unwrap()));
    }
}

#[test]
fn unary_binary_logical_and_both_conditional_branches_survive() {
    let v = project("r[1] = !r[2] && (r[3] ? r[4] + r[5] : r[6] || r[7]);").remove(0);
    let logical = kind(&v, "logical");
    assert!(logical.get("left").is_some() && logical.get("right").is_some());
    let c = kind(&v, "conditional");
    assert_eq!(edge(&v, &c["consequent"])["kind"], "binary");
    assert_eq!(edge(&v, &c["alternate"])["kind"], "logical");
    kind(&v, "unary");
    for r in 2..=7 {
        assert!(nodes(&v).iter().any(|n| n["register"] == r));
    }
}

#[test]
fn budgets_are_fixed_and_omissions_are_explicit() {
    let deep = format!("r[0] = {}r[1]{};", "(".repeat(30), ")".repeat(30));
    let v = project(&deep).remove(0);
    assert!(omitted(&v, "depth"));
    assert!(nodes(&v).iter().all(|n| n["depth"].as_u64().unwrap() <= 16));
    let items = format!("r[0] = [{}];", vec!["0"; 40].join(","));
    let v = project(&items).remove(0);
    assert!(omitted(&v, "items"));
    assert_eq!(kind(&v, "array")["items_omitted"], 8);
    assert_eq!(kind(&v, "array")["items"].as_array().unwrap().len(), 32);
    let source = format!("r[0] = [{}];", vec!["r[1] + r[2]"; 32].join(","));
    let v = project(&source).remove(0);
    assert_eq!(nodes(&v).len(), 128);
    assert!(omitted(&v, "nodes"));
    assert!(v.to_string().contains("\"omitted\":\"nodes\""));
    assert_eq!(v["limits"]["visitor_work"], 32_000_000);
}

#[test]
fn global_budget_includes_duplicate_requested_views() {
    let source = format!("r[0] = [{}];", vec!["r[1] + r[2]"; 32].join(","));
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, &source, SourceType::default()).parse();
    let span = match &parsed.program.body[0] {
        Statement::ExpressionStatement(s) => s.expression.span(),
        _ => unreachable!(),
    };
    let vs = expression_view::views(&source, &parsed.program, &vec![span; 40]).unwrap();
    assert_eq!(vs.iter().map(|v| nodes(v).len()).sum::<usize>(), 4096);
    assert!(vs[32..]
        .iter()
        .all(|v| omitted(v, "global_nodes") && nodes(v).is_empty()));
}

#[test]
fn wanted_order_updates_and_missing_exact_spans() {
    let source = "r[1] = 1; r[2]++;";
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    let vs = expression_view::views(
        source,
        &parsed.program,
        &[Span::new(10, 16), Span::new(0, 8), Span::new(0, 9)],
    )
    .unwrap();
    assert_eq!(kind(&vs[0], "update")["destination_is_also_read"], true);
    kind(&vs[1], "assignment");
    assert_eq!(vs[2]["root"]["unresolved"], "missing_exact_expression_span");
    assert!(nodes(&vs[2]).is_empty());
}

#[test]
fn arbitrary_expressions_including_inherited_arguments_have_exact_spans() {
    let source = "foo(r[2]); r[1].slot; r[0] = [r[3]];";
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    assert!(parsed.errors.is_empty());
    let wanted: Vec<_> = ["r[3]", "foo(r[2])", "r[1].slot", "r[2]"]
        .iter()
        .map(|text| {
            let start = source.find(text).unwrap();
            Span::new(start as u32, (start + text.len()) as u32)
        })
        .collect();
    let vs = expression_view::views(source, &parsed.program, &wanted).unwrap();
    for (v, text) in vs.iter().zip(["r[3]", "foo(r[2])", "r[1].slot", "r[2]"]) {
        assert_eq!(edge(v, &v["root"])["source"]["preview"], text);
    }
}

#[test]
fn traversal_depth_cap_is_an_error_not_a_panic() {
    let source = format!("r[0] = {}r[1]{};", "(".repeat(150), ")".repeat(150));
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, &source, SourceType::default()).parse();
    assert!(parsed.errors.is_empty());
    let error = expression_view::views(&source, &parsed.program, &[]).unwrap_err();
    assert!(error.to_string().contains("visitor depth cap"));
}

#[test]
fn unsupported_syntax_is_explicit_not_fabricated() {
    let vs = project("r[0] = {x: 1}; r[1] = () => captured; r[2] = `marker ${r[3]}`;");
    for v in vs {
        let opaque = kind(&v, "opaque");
        assert_eq!(opaque["unresolved"], true);
        assert!(!opaque["source"]["preview"].as_str().unwrap().is_empty());
        assert!(opaque.get("ast_variant").is_some());
    }
}

#[test]
fn malformed_spans_and_source_mismatch_fail_closed() {
    let allocator = Allocator::default();
    let source = "r[1] = '\u{00e9}';";
    let mut parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    assert!(expression_view::views("r[2] = '\u{00e9}';", &parsed.program, &[]).is_err());
    for span in [Span::new(10, 9), Span::new(0, 100), Span::new(9, 10)] {
        assert!(expression_view::views(source, &parsed.program, &[span]).is_err());
    }
    assert!(
        expression_view::views(&" ".repeat(64 * 1024 * 1024 + 1), &parsed.program, &[])
            .unwrap_err()
            .to_string()
            .contains("source byte cap")
    );
    if let Statement::ExpressionStatement(s) = &mut parsed.program.body[0] {
        if let Expression::AssignmentExpression(e) = &mut s.expression {
            e.span = Span::new(0, 100);
        }
    }
    assert!(expression_view::views(source, &parsed.program, &[]).is_err());
}
