use hermes_dec_rs::cli::sites::workspace_literal_notes;
use serde_json::Value;
use std::collections::BTreeSet;

fn source(rows: &[(u32, &str)]) -> String {
    let mut s = String::from("F[7] = function () { var r = [];\n");
    for (pc, row) in rows {
        s.push_str(&format!("// HBC function 7, PC {pc}\n{row}\n"));
    }
    s.push_str("};\n");
    s
}

#[test]
fn workspace_notes_join_raw_literals_and_preserve_alias_provenance() {
    let s = source(&[
        (0, "r[0] = 'caf\u{00e9}';"),
        (1, "r[1] = r[0];"),
        (2, "r[0] = 'new';"),
        (3, "apply(r[9], r[8], [r[1]]);"),
    ]);
    let notes = workspace_literal_notes(&s, 7, &BTreeSet::new()).unwrap();
    let uses: Vec<Value> = serde_json::from_str(&notes.notes[&3]).unwrap();
    assert_eq!(uses.len(), 1);
    let value = &uses[0];
    assert_eq!(value["literal"], "'caf\u{00e9}'");
    assert_eq!(value["from"][0], 0);
    assert_eq!(value["via"].as_array().unwrap().len(), 1);
    for (key, offset, text) in [("from", 1, "'caf\u{00e9}'"), ("use_span", 0, "r[1]")] {
        let a = value[key][offset].as_u64().unwrap() as usize;
        let b = value[key][offset + 1].as_u64().unwrap() as usize;
        assert_eq!(&s[a..b], text);
    }
    assert!(notes.next_offset.is_none());
    assert_eq!(notes.copy_reads_skipped, 1);
    assert!(!notes.notes.contains_key(&1));
    assert!(notes.unresolved_reads >= 2);
}

#[test]
fn uncertain_definitions_and_mutating_lvalues_are_not_notes() {
    for s in [
        source(&[(0, "r[0] = 12;"), (1, "r[0] += 1;")]),
        source(&[
            (0, "r[0] = 12;"),
            (1, "if (flag) r[0] = 13;"),
            (2, "apply(r[9], r[8], [r[0]]);"),
        ]),
        source(&[(0, "r[0] = 12; r[0] = 13;"), (1, "return r[0];")]),
    ] {
        let notes = workspace_literal_notes(&s, 7, &BTreeSet::new()).unwrap();
        assert!(notes.notes.is_empty());
        assert!(notes.unresolved_reads > 0);
    }
}

#[test]
fn workspace_notes_respect_exception_boundaries() {
    let s = source(&[(0, "r[0] = 12;"), (1, "return r[0];")]);
    assert!(!workspace_literal_notes(&s, 7, &BTreeSet::new())
        .unwrap()
        .notes
        .is_empty());
    assert!(workspace_literal_notes(&s, 7, &BTreeSet::from([1]))
        .unwrap()
        .notes
        .is_empty());
}

#[test]
fn rows_omit_over_budget_candidates_without_changing_raw_source() {
    let literal = format!("'{}'", "x".repeat(4000));
    let call = format!("apply(r[9], r[8], [{}]);", vec!["r[0]"; 150].join(","));
    let s = source(&[(0, &format!("r[0] = {literal};")), (1, &call)]);
    let notes = workspace_literal_notes(&s, 7, &BTreeSet::new()).unwrap();
    let note = &notes.notes[&1];
    assert!(note.len() <= 65536);
    let values: Vec<Value> = serde_json::from_str(note).unwrap();
    assert!(values.len() < 128);
    assert!(notes.reads_omitted > 22);
    assert!(s.contains(&call));
}

#[test]
fn unsupported_register_mutation_rejects_notes_not_raw_workspace() {
    let s = source(&[(0, "r.length = 1;")]);
    assert!(workspace_literal_notes(&s, 7, &BTreeSet::new()).is_err());
}

#[test]
fn unicode_line_separator_escaping_is_charged_to_row_budget() {
    let literal = format!("'{}'", "\u{2028}\u{2029}".repeat(600));
    let call = format!("apply(r[9], r[8], [{}]);", vec!["r[0]"; 100].join(","));
    let s = source(&[(0, &format!("r[0] = {literal};")), (1, &call)]);
    let notes = workspace_literal_notes(&s, 7, &BTreeSet::new()).unwrap();
    let note = &notes.notes[&1];
    assert!(note.len() <= 65536);
    assert!(!note.contains(['\u{2028}', '\u{2029}', '\n', '\r']));
    let values: Vec<Value> = serde_json::from_str(note).unwrap();
    assert_eq!(values[0]["literal"], literal);
    assert!(notes.reads_omitted > 0);
    hermes_dec_rs::cli::compact::render_with_notes(&s, 7, &notes.notes).unwrap();
}
