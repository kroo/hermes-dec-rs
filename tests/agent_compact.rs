use hermes_dec_rs::cli::compact::{render, render_with_notes};
use std::collections::BTreeMap;

fn body(output: &str) -> &str {
    let (header, body) = output.split_once('\n').unwrap();
    assert_eq!(header, "Inspection-only; raw f7.js authoritative; @PC labels are function-local bytes, not text offsets; candidates are source-local, not substitutions or runtime values.");
    body
}

#[test]
fn preserves_control_flow_metadata_literals_and_calls() {
    let source = concat!(
        "// metadata: authored fragment\n",
        "F[7] = function(a) {\n",
        "  var r = [], pc = 0;\n",
        "  while (true) {\n",
        "    switch (pc) {\n",
        "      case 0:\n",
        "// HBC function 7, PC 0\n",
        "        r[0] = 'literal  value'; // retain spaces\n",
        "// HBC function 7, PC 17\n",
        "        if (a) { pc = 23; continue; }\n",
        "        pc = 42; continue;\n",
        "      case 23:\n",
        "// HBC function 7, PC 23\n",
        "        r[1] = target.call(r[0], 1);\n",
        "        pc = 42; continue;\n",
        "      case 42:\n",
        "// HBC function 7, PC 42\n",
        "        return r[1];\n",
        "      default: throw new Error('bad pc');\n",
        "    }\n",
        "  }\n",
        "};\n",
    );
    let expected = source
        .replace("// HBC function 7, PC 0", "@0")
        .replace("// HBC function 7, PC 17", "@17")
        .replace("// HBC function 7, PC 23", "@23")
        .replace("// HBC function 7, PC 42", "@42");
    assert_eq!(body(&render(source, 7).unwrap()), expected);
}

#[test]
fn parser_comments_not_literal_text_are_authoritative() {
    let source = concat!(
        "F[7] = function() {\n",
        "// HBC function 7, PC 0\n",
        "  var r = [];\n",
        "  r[0] = `first\n",
        "// HBC function 999, PC malformed\n",
        "// HBC function 7, PC 0\n",
        "last ${'expression'}`;\n",
        "  r[1] = 'escaped\\\n",
        "// HBC function 999, PC 0';\n",
        "  r[2] = \"// HBC function 8, PC 0\";\n",
        "  /* block\n",
        "// HBC function 999, PC 0\n",
        "  */\n",
        "  // HBC function 999, PC broken\n",
        "  r[3] = 1; // HBC function 999, PC broken\n",
        "// unrelated // HBC function 999, PC broken\n",
        "// HBC function 7, PC 4\n",
        "  return r;\n",
        "};",
    );
    let expected = source
        .replacen("// HBC function 7, PC 0", "@0", 1)
        .replace("// HBC function 7, PC 4", "@4");
    assert_eq!(body(&render(source, 7).unwrap()), expected);
}

#[test]
fn preserves_all_line_terminators_unicode_and_final_newline() {
    for ending in ["\n", "\r\n", "\r", "\u{2028}", "\u{2029}"] {
        for final_ending in ["", ending] {
            let source = format!("F[7] = function() {{{ending}// HBC function 7, PC 0{ending}  return 'caf\u{e9} \u{1f980}';{ending}}};{final_ending}");
            let expected = source.replace("// HBC function 7, PC 0", "@0");
            assert_eq!(body(&render(&source, 7).unwrap()), expected);
        }
    }
}

#[test]
fn no_markers_preserves_source_including_whitespace() {
    for source in ["", " \t\r\n", "F[7] = function() { return 1; };", "F[7] = function() {\n  // ordinary comment\n  return `\n// HBC function 8, PC nope\n`;\n};\n"] {
        assert_eq!(body(&render(source, 7).unwrap()), source);
    }
}

#[test]
fn rejects_malformed_wrong_id_overflow_and_nonordered_markers() {
    for comment in [
        "// HBC function 8, PC 0",
        "// HBC function 7, PC -1",
        "// HBC function 7, PC +1",
        "// HBC function 7, PC 1.0",
        "// HBC function 7, PC 0x10",
        "// HBC function 7, PC 4294967296",
        "// HBC function 4294967296, PC 0",
        "// HBC function 7, PC ",
        "// HBC function 7, PC 0 trailing",
        "// HBC function 7, PC 0 ",
        "// HBC function 7 PC 0",
        "// HBC function",
        "// HBC function 7, PC 2\n// HBC function 7, PC 2",
        "// HBC function 7, PC 9\n// HBC function 7, PC 2",
    ] {
        let source = format!("F[7] = function() {{\n{comment}\nreturn 0;\n}};");
        assert!(render(&source, 7).is_err(), "accepted {comment:?}");
    }
}

#[test]
fn rejects_invalid_complete_javascript() {
    for source in [
        "F[7] = function() {\n// HBC function 7, PC 0\n",
        "F[7] = function() { return 'unterminated; };",
        "F[7] = function() { var = ; };",
    ] {
        assert!(render(source, 7).is_err());
    }
}

#[test]
fn accepts_numeric_pc_extremes_without_line_truncation() {
    let literal = "x".repeat(256 * 1024);
    let source = format!("F[7] = function() {{\n// HBC function 7, PC 0000\nvar r = '{literal}';\n// HBC function 7, PC 4294967295\nreturn r;\n}};");
    let expected = source
        .replace("// HBC function 7, PC 0000", "@0")
        .replace("// HBC function 7, PC 4294967295", "@4294967295");
    assert_eq!(body(&render(&source, 7).unwrap()), expected);
}

#[test]
fn oversized_source_is_rejected_before_parsing() {
    let source = " ".repeat(64 * 1024 * 1024 + 1);
    let error = render(&source, 7).unwrap_err().to_string();
    assert!(error.contains("64 MiB"));
}

#[test]
fn notes_follow_instruction_blocks_without_changing_source() {
    let source = "F[7] = function() {\n// HBC function 7, PC 0\nif (a) {\n  call('unchanged');\n  pc = 12;\n}\n// HBC function 7, PC 12\nreturn `first\n// HBC function 999, PC fake\nlast`;\n};";
    let notes = BTreeMap::from([(0, "{\"candidate\":\"a\"}".into()), (12, "[\"b\"]".into())]);
    let expected = source
        .replace("// HBC function 7, PC 0", "@0")
        .replace("// HBC function 7, PC 12", "@12")
        .replace(
            "}\n@12",
            "}\n# literal candidates @0: {\"candidate\":\"a\"}\n@12",
        )
        .replace("last`;\n", "last`;\n# literal candidates @12: [\"b\"]\n");
    assert_eq!(
        body(&render_with_notes(source, 7, &notes).unwrap()),
        expected
    );
}

#[test]
fn notes_never_split_same_line_multiline_literals_or_comments() {
    for payload in ["`first\nsecond`", "'first\\\nsecond'"] {
        let source = format!("F[7] = function() {{\n// HBC function 7, PC 0\ncall(); var value = {payload}; /* first\nsecond */\nreturn value;\n}};");
        let notes = BTreeMap::from([(0, "{\"verbatim\":  true}".into())]);
        let rendered = render_with_notes(&source, 7, &notes).unwrap();
        assert!(rendered.contains(payload));
        assert!(rendered.contains("/* first\nsecond */"));
        assert!(
            rendered.contains("return value;\n# literal candidates @0: {\"verbatim\":  true}\n};")
        );
    }
}

#[test]
fn notes_are_opaque_bounded_single_lines_for_known_pcs() {
    let source = "F[7] = function() {\n// HBC function 7, PC 0\nreturn 0;\n};";
    for note in [
        "a\nb".into(),
        "a\rb".into(),
        "a\u{2028}b".into(),
        "x".repeat(65537),
    ] {
        assert!(render_with_notes(source, 7, &BTreeMap::from([(0, note)])).is_err());
    }
    assert!(render_with_notes(source, 7, &BTreeMap::from([(1, "{}".into())])).is_err());
    assert!(render_with_notes(
        "F[7] = function() {};",
        7,
        &BTreeMap::from([(0, "{}".into())])
    )
    .is_err());
    let notes = BTreeMap::from([(0, "x".repeat(65536))]);
    assert!(render_with_notes(source, 7, &notes)
        .unwrap()
        .contains(&notes[&0]));
    assert_eq!(
        render(source, 7).unwrap(),
        render_with_notes(source, 7, &BTreeMap::new()).unwrap()
    );
}

#[test]
fn aggregate_note_limit_is_checked_before_return() {
    let notes = (0..2048).map(|pc| (pc, "x".repeat(65536))).collect();
    let error = render_with_notes("F[7] = function() {};", 7, &notes)
        .unwrap_err()
        .to_string();
    assert!(error.contains("128 MiB"));
}

#[test]
fn final_instruction_note_precedes_exporter_scaffolding() {
    let source = "F[7] = function() {\nfor (;;) { try { switch (pc) {\ncase 0: {\n// HBC function 7, PC 0\nreturn r[0];\n}\ndefault: throw new Error('bad pc');\n} } catch (error) {\nthrow error;\n} }\n};";
    let notes = BTreeMap::from([(0, "{\"source_local\":true}".into())]);
    let expected = source.replace("// HBC function 7, PC 0", "@0").replace(
        "return r[0];\n",
        "return r[0];\n# literal candidates @0: {\"source_local\":true}\n",
    );
    assert_eq!(
        body(&render_with_notes(source, 7, &notes).unwrap()),
        expected
    );
}

#[test]
fn note_waits_for_trailing_multiline_block_comment() {
    let source = "F[7] = function() {\n// HBC function 7, PC 0\nreturn 1; /* keep\n// HBC function 999, PC spoof\nall comment bytes */\n};";
    let expected = source.replace("// HBC function 7, PC 0", "@0").replace(
        "all comment bytes */\n",
        "all comment bytes */\n# literal candidates @0: {}\n",
    );
    assert_eq!(
        body(&render_with_notes(source, 7, &BTreeMap::from([(0, "{}".into())])).unwrap()),
        expected
    );
}

#[test]
fn notes_preserve_crlf_source_and_need_no_json_parsing() {
    let source = "F[7] = function() {\r\n// HBC function 7, PC 0\r\nreturn 'unchanged';\r\n};";
    let expected = source.replace("// HBC function 7, PC 0", "@0").replace(
        "return 'unchanged';\r\n",
        "return 'unchanged';\r\n# literal candidates @0: opaque  text\n",
    );
    assert_eq!(
        body(&render_with_notes(source, 7, &BTreeMap::from([(0, "opaque  text".into())])).unwrap()),
        expected
    );
}

#[test]
fn real_interpolation_comments_are_markers_but_template_text_is_not() {
    let source = concat!(
        "F[7] = function() {\n",
        "// HBC function 7, PC 0\n",
        "var r = `first\n",
        "// HBC function 999, PC spoof\n",
        "${(() => {\n",
        "// HBC function 7, PC 4\n",
        "return 1;\n",
        "})()}\n",
        "last`;\n",
        "return r;\n",
        "};",
    );
    let expected = source
        .replace("// HBC function 7, PC 0", "@0")
        .replace("// HBC function 7, PC 4", "@4");
    assert_eq!(body(&render(source, 7).unwrap()), expected);
    let notes = BTreeMap::from([(0, "{}".into()), (4, "[]".into())]);
    let rendered = render_with_notes(source, 7, &notes).unwrap();
    assert!(rendered
        .contains("last`;\n# literal candidates @0: {}\n# literal candidates @4: []\nreturn r;"));
    assert_eq!(
        body(&rendered)
            .replace("# literal candidates @0: {}\n", "")
            .replace("# literal candidates @4: []\n", ""),
        expected,
    );
}
