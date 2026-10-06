use hermes_dec_rs::cli::compact::{render, render_with_notes};
use hermes_dec_rs::cli::sites::workspace_literal_notes;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;
use std::process::Command;

fn source(rows: &[(u32, &str)]) -> String {
    let mut source = String::from("F[7] = function() { var r = [];\n");
    for (pc, statement) in rows {
        source.push_str(&format!("// HBC function 7, PC {pc}\n{statement}\n"));
    }
    source.push_str("};\n");
    source
}

fn values(notes: &BTreeMap<u32, String>, pc: u32) -> Vec<Value> {
    notes
        .get(&pc)
        .map(|note| serde_json::from_str(note).unwrap())
        .unwrap_or_default()
}

#[test]
fn alias_snapshot_and_every_provenance_span_join_exact_raw_bytes() {
    let raw = source(&[
        (0, "r[0] = '\\n caf\u{00e9}';"),
        (4, "r[1] = r[0];"),
        (8, "r[0] = 'replacement';"),
        (12, "r[2] = r[1];"),
        (16, "return r[2];"),
    ]);
    let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
    let uses = values(&notes.notes, 16);
    assert_eq!(uses.len(), 1);
    let candidate = &uses[0];
    assert_eq!(candidate["literal"], "'\\n caf\u{00e9}'");
    assert_eq!(candidate["from"][0], 0);
    let span = |value: &Value, offset: usize| {
        let start = value[offset].as_u64().unwrap() as usize;
        let end = value[offset + 1].as_u64().unwrap() as usize;
        &raw[start..end]
    };
    assert_eq!(span(&candidate["from"], 1), candidate["literal"]);
    assert_eq!(span(&candidate["use_span"], 0), "r[2]");
    let definition_source = |id: &str| {
        let parts: Vec<_> = id.split(':').collect();
        assert_eq!(parts[0], "7");
        &raw[parts[1].parse::<usize>().unwrap()..parts[2].parse::<usize>().unwrap()]
    };
    assert_eq!(
        definition_source(candidate["definition"].as_str().unwrap()),
        "r[0] = '\\n caf\u{00e9}'"
    );
    let aliases = candidate["via"].as_array().unwrap();
    assert_eq!(aliases.len(), 2);
    assert_eq!(
        definition_source(aliases[0].as_str().unwrap()),
        "r[2] = r[1]"
    );
    assert_eq!(
        definition_source(aliases[1].as_str().unwrap()),
        "r[1] = r[0]"
    );
}

#[test]
fn conditional_same_pc_and_case_barriers_never_supply_final_read_candidates() {
    for raw in [
        source(&[
            (0, "r[0] = 12;"),
            (4, "flag && (r[0] = 13);"),
            (8, "return r[0];"),
        ]),
        source(&[
            (0, "r[0] = 12;"),
            (4, "flag ? (r[0] = 13) : 0;"),
            (8, "return r[0];"),
        ]),
        source(&[
            (0, "r[0] = 12;"),
            (4, "r[9] ||= (r[0] = 13);"),
            (8, "return r[0];"),
        ]),
        source(&[(0, "r[0] = 12; r[0] = 13;"), (8, "return r[0];")]),
        source(&[(0, "r[0] = 12;"), (8, "r[0] = 13; return r[0];")]),
        source(&[
            (0, "switch (pc) { case 0: r[0] = 12; break; case 1:"),
            (8, "return r[0]; }"),
        ]),
        source(&[
            (0, "r[0] = 12;"),
            (4, "try { r[0] = 13; } catch (error) {}"),
            (8, "return r[0];"),
        ]),
    ] {
        let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
        assert!(
            values(&notes.notes, 8).is_empty(),
            "{raw}\n{:?}",
            notes.notes
        );
    }
    let raw = source(&[(0, "r[0] = 12;"), (8, "return r[0];")]);
    for exceptions in [BTreeSet::from([4]), BTreeSet::from([8])] {
        let notes = workspace_literal_notes(&raw, 7, &exceptions).unwrap();
        assert!(values(&notes.notes, 8).is_empty());
    }
}

#[test]
fn aliases_do_not_repair_unresolved_reads_and_mutation_targets_are_excluded() {
    let raw = source(&[
        (0, "r[0] = 12;"),
        (4, "flag && (r[0] = 13);"),
        (8, "r[1] = r[0];"),
        (12, "return r[1];"),
    ]);
    let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
    assert!(values(&notes.notes, 12).is_empty());
    for mutation in ["r[0] += 1;", "r[0]++;", "++r[0];"] {
        let raw = source(&[(0, "r[0] = 12;"), (4, mutation), (8, "return r[0];")]);
        let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
        assert!(notes.notes.is_empty(), "{mutation}: {:?}", notes.notes);
    }
}

#[test]
fn labeled_block_exit_does_not_promote_a_bypassed_definition() {
    let raw = source(&[
        (0, "r[0] = 12; done: { break done;"),
        (4, "r[0] = 13; }"),
        (8, "return r[0];"),
    ]);
    assert!(
        workspace_literal_notes(&raw, 7, &BTreeSet::new()).is_err(),
        "unsupported labeled control flow must fail closed, not promote a bypassed definition"
    );
    assert!(hermes_dec_rs::cli::sites::analyze_literal_view_source(
        &raw,
        7,
        &BTreeSet::new(),
        &Default::default()
    )
    .is_err());
}

#[test]
fn unicode_line_separator_escaping_is_charged_to_final_note_row_limit() {
    for separator in ['\u{2028}', '\u{2029}'] {
        // A 3002-byte lexeme is below the literal cap, but serialization followed
        // by single-line escaping nearly doubles its note size.
        let literal = format!("'{}'", separator.to_string().repeat(1000));
        let call = format!("consume({});", vec!["r[0]"; 32].join(","));
        let raw = source(&[(0, &format!("r[0] = {literal};")), (4, &call)]);
        let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
        let note = &notes.notes[&4];
        assert!(
            note.len() <= 65536,
            "final escaped note has {} bytes",
            note.len()
        );
        assert!(!note.contains(['\n', '\r', '\u{2028}', '\u{2029}']));
        assert!(notes.reads_omitted > 0);
        for candidate in values(&notes.notes, 4) {
            assert_eq!(candidate["literal"], literal);
        }
        render_with_notes(&raw, 7, &notes.notes).unwrap();
    }
}

#[test]
fn serialized_multiline_lexemes_round_trip_without_note_line_injection() {
    let raw = source(&[
        (0, "r[0] = 'first\\\nsecond\u{2028}\u{2029}';"),
        (4, "return r[0];"),
    ]);
    let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
    let note = &notes.notes[&4];
    assert!(!note.contains(['\n', '\r', '\u{2028}', '\u{2029}']));
    assert_eq!(
        values(&notes.notes, 4)[0]["literal"],
        "'first\\\nsecond\u{2028}\u{2029}'"
    );
    let rendered = render_with_notes(&raw, 7, &notes.notes).unwrap();
    assert_eq!(rendered.matches("# literal candidates @4:").count(), 1);
    assert!(rendered.contains("'first\\\nsecond\u{2028}\u{2029}'"));
}

#[test]
fn notes_wait_out_multiline_payloads_and_comments_without_losing_source_bytes() {
    for payload in [
        "return 'first\\\nsecond'; /* trailing\ncomment */\n",
        "return `first\n${r[0]}\nlast`; // trailing\n",
        "return /# literal candidates @0:/; /* trailing\ncomment */\n",
    ] {
        let raw = source(&[(0, payload)]);
        let note = "opaque note";
        let rendered = render_with_notes(&raw, 7, &BTreeMap::from([(0, note.into())])).unwrap();
        let body = rendered.split_once('\n').unwrap().1;
        assert!(body.contains(&format!("{payload}# literal candidates @0: {note}\n")));
        assert_eq!(
            body.replace("# literal candidates @0: opaque note\n", ""),
            raw.replace("// HBC function 7, PC 0", "@0")
        );
    }
}

#[test]
fn primitive_lexeme_cap_and_alias_depth_are_explicit_omissions_not_false_values() {
    let large = format!("'{}'", "x".repeat(4095));
    let raw = source(&[(0, &format!("r[0] = {large};")), (4, "return r[0];")]);
    let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
    assert!(notes.notes.is_empty());
    assert_eq!(notes.unresolved_reads, 1);
    assert!(notes.next_offset.is_none());
    let mut raw = source(&[(0, "r[0] = 12;")]);
    raw.truncate(raw.len() - 3);
    for index in 1..=17 {
        raw.push_str(&format!(
            "// HBC function 7, PC {index}\nr[{index}] = r[{}];\n",
            index - 1
        ));
    }
    raw.push_str("// HBC function 7, PC 18\nreturn r[17];\n};\n");
    let notes = workspace_literal_notes(&raw, 7, &BTreeSet::new()).unwrap();
    assert!(values(&notes.notes, 18).is_empty());
    assert!(notes.unresolved_reads > 0);
}

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/simple_arithmetic.hbc"
    ))
}

#[test]
fn cli_raw_only_omits_views_and_default_raw_files_are_byte_identical() {
    let temp = tempfile::tempdir().unwrap();
    let mut directories = Vec::new();
    for raw_only in [true, false] {
        let output = temp.path().join(if raw_only { "raw" } else { "views" });
        let mut command = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"));
        command
            .arg("workspace")
            .arg(fixture())
            .arg("-o")
            .arg(&output);
        if raw_only {
            command.arg("--raw-only");
        }
        let result = command.output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let reply: Value = serde_json::from_slice(&result.stdout).unwrap();
        let manifest: Value =
            serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
        assert_eq!(
            reply["views"],
            if raw_only {
                Value::Null
            } else {
                Value::from("view")
            }
        );
        assert_eq!(output.join("view").exists(), !raw_only);
        assert_eq!(manifest.get("view_note_contract").is_some(), !raw_only);
        for entry in manifest["functions"].as_array().unwrap() {
            assert_eq!(entry.get("literal_notes").is_some(), !raw_only);
            assert_eq!(entry.get("view_path").is_some(), !raw_only);
        }
        directories.push((output, manifest));
    }
    for entry in directories[0].1["functions"].as_array().unwrap() {
        let path = entry["path"].as_str().unwrap();
        assert_eq!(
            fs::read(directories[0].0.join(path)).unwrap(),
            fs::read(directories[1].0.join(path)).unwrap()
        );
    }
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 2);
}

#[test]
fn cli_failure_is_atomic_and_never_reports_a_success_manifest() {
    let temp = tempfile::tempdir().unwrap();
    let input = temp.path().join("bad.hbc");
    fs::write(&input, b"authored malformed input").unwrap();
    for raw_only in [true, false] {
        let output = temp.path().join("output");
        let mut command = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"));
        command.arg("workspace").arg(&input).arg("-o").arg(&output);
        if raw_only {
            command.arg("--raw-only");
        }
        let result = command.output().unwrap();
        assert!(!result.status.success());
        assert!(result.stdout.is_empty());
        assert!(!output.exists());
        assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
    }
}

#[test]
fn notes_reject_unknown_pcs_and_never_touch_marker_spoofs_inside_payloads() {
    let raw = source(&[(
        0,
        "return `\n// HBC function 7, PC 99\n`; /*\n// HBC function 7, PC 100\n*/",
    )]);
    assert!(render_with_notes(&raw, 7, &BTreeMap::from([(99, "{}".into())])).is_err());
    let rendered = render(&raw, 7).unwrap();
    assert_eq!(
        rendered.split_once('\n').unwrap().1,
        raw.replacen("// HBC function 7, PC 0", "@0", 1)
    );
}
