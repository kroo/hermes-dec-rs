pub use hermes_dec_rs::{DecompilerError, DecompilerResult};
#[path = "../src/cli/origins_text.rs"]
mod origins_text;
use serde_json::{json, Value};

fn fixture() -> Value {
    let source = json!({"start":0,"end":6,"javascript":"é\n\u{1b}\"x","snippet_truncated":false});
    json!({"schema":"origins-v1","schema_version":1,"semantics":"candidate definitions only; no values, heap, captured slots, or constructor semantics",
        "function":3,"pc":7,"unknown":["unsupported control"],"truncated":true,"unresolved":true,
        "blocks":1,"normal_edges":[],"normal_edges_total":0,"normal_edges_returned":0,"normal_edges_truncated":false,"control_index_work":1,
        "limits":{"dependency_depth":2,"definition_nodes":5,"output_bytes":99999},
        "definitions":[{"id":0,"pc":7,"block_pc":7,"register":2,"source":source},{"id":1,"pc":7,"block_pc":7,"register":2,"source":source}],
        "demands":[{"owner_definition":0,"pc":7,"register":2,"read":source,"candidates":[0,1],"unresolved":true,"same_pc_ambiguity":true,"cycle":true,"truncated":true}]})
}

#[test]
fn deterministic_links_and_escaped_utf8() {
    let r = fixture();
    let bytes = origins_text::render(&r, 10000).unwrap();
    assert_eq!(bytes, origins_text::render(&r, 10000).unwrap());
    let text = String::from_utf8(bytes).unwrap();
    assert!(text.contains("é\\n\\u001b\\\"x"));
    assert!(!text.contains('\u{1b}'));
    assert!(text.contains("\"candidates\":[0,1]"));
    assert!(text.contains("\"owner_definition\":0"));
    for flag in ["cycle", "same_pc_ambiguity", "unresolved", "truncated"] {
        assert!(text.contains(&format!("\"{flag}\":true")));
    }
    for line in text.lines() {
        serde_json::from_str::<Value>(line.split_once(' ').unwrap().1).unwrap();
    }
}

#[test]
fn exact_budget_is_atomic() {
    let r = fixture();
    let n = origins_text::render(&r, 10000).unwrap().len();
    assert_eq!(origins_text::render(&r, n).unwrap().len(), n);
    assert!(origins_text::render(&r, n - 1).is_err());
    assert!(origins_text::render(&r, 0).is_err());
}

#[test]
fn typed_omissions_are_explicit() {
    let mut r = fixture();
    r["definitions"][0]["expression"] = json!({"schema_version":1,"semantics":"syntax_only","source":{"start":0,"end":6},"root":{"node":0},"projected_nodes":1,"nodes":[{"id":0,"kind":"conditional","consequent":{"omitted":"depth"},"alternate":{"node":0}}],"omissions":["depth"],"truncated":true,"limits":{"depth":2}});
    let text = String::from_utf8(origins_text::render(&r, 10000).unwrap()).unwrap();
    assert!(text.contains("\"renderer_omitted_nodes\":1"));
    assert!(text.contains("\"alternate\":1"));
    assert!(text.contains("\"omissions\":[\"depth\"]"));
}

#[test]
fn invalid_inputs_return_errors() {
    for value in [Value::Null, json!([]), json!({})] {
        assert!(origins_text::render(&value, 10000).is_err());
    }
    for field in ["schema", "limits", "demands", "pc"] {
        let mut r = fixture();
        r.as_object_mut().unwrap().remove(field);
        assert!(origins_text::render(&r, 10000).is_err());
    }
    let mut r = fixture();
    r["demands"][0]["candidates"] = json!([9]);
    assert!(origins_text::render(&r, 10000).is_err());
    let mut r = fixture();
    r["definitions"][0]["source"]["end"] = json!(0);
    assert!(origins_text::render(&r, 10000).is_err());
    let mut r = fixture();
    r["normal_edges_returned"] = json!(1);
    assert!(origins_text::render(&r, 10000).is_err());
    let mut r = fixture();
    r["demands"][0]
        .as_object_mut()
        .unwrap()
        .remove("owner_definition");
    assert!(origins_text::render(&r, 10000).is_err());
}
