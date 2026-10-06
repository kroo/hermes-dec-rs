use hermes_dec_rs::bundle::{export_function_fragment_bounded, export_function_fragments};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::tables::string_table::{
    OverflowStringTableEntry, SmallStringTableEntry, StringKind,
};
use hermes_dec_rs::HbcFile;
use std::path::Path;

const MAX: usize = 64 * 1024 * 1024;

fn fixture(name: &str) -> Vec<u8> {
    std::fs::read(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("data")
            .join(name),
    )
    .unwrap()
}

fn string_budget(hbc: &HbcFile<'_>) -> usize {
    // Independent sizing via the existing serializer; tests may allocate freely.
    (0..hbc.strings.string_count)
        .map(|id| {
            let entry = hbc.strings.get_entry(id).unwrap();
            if entry.is_utf16 {
                2 + entry
                    .bytes
                    .chunks_exact(2)
                    .map(|pair| {
                        let unit = u16::from_le_bytes([pair[0], pair[1]]);
                        if (32..=126).contains(&unit) && unit != 34 && unit != 92 {
                            1
                        } else {
                            6
                        }
                    })
                    .sum::<usize>()
            } else {
                serde_json::to_string(&String::from_utf8_lossy(entry.bytes))
                    .unwrap()
                    .replace('\u{2028}', "\\u2028")
                    .replace('\u{2029}', "\\u2029")
                    .len()
            }
        })
        .sum()
}

#[test]
fn bounded_output_matches_existing_small_fixtures() {
    for name in [
        "test_empty.hbc",
        "simple_arithmetic.hbc",
        "escape_test.hbc",
        "array_constants.hbc",
        "bigints.hbc",
        "regex_test.hbc",
        "dense_switch_test.hbc",
        "closure_test.hbc",
        "finally_test.hbc",
        "bundle_private_state.hbc",
    ] {
        let bytes = fixture(name);
        let hbc = HbcFile::parse(&bytes).unwrap();
        let indices: Vec<_> = (0..hbc.functions.count()).collect();
        let budget = string_budget(&hbc);
        for (id, expected) in export_function_fragments(&hbc, &indices).unwrap() {
            let actual = export_function_fragment_bounded(&hbc, id, MAX, budget).unwrap();
            assert_eq!(actual, expected, "{name}, function {id}");
        }
    }
}

#[test]
fn exact_source_and_string_boundaries_are_accepted() {
    let bytes = fixture("simple_arithmetic.hbc");
    let hbc = HbcFile::parse(&bytes).unwrap();
    let expected = export_function_fragments(&hbc, &[0]).unwrap().remove(0).1;
    let strings = string_budget(&hbc);
    assert_eq!(
        export_function_fragment_bounded(&hbc, 0, expected.len(), strings).unwrap(),
        expected
    );
    let source_error = export_function_fragment_bounded(&hbc, 0, expected.len() - 1, strings)
        .unwrap_err()
        .to_string();
    assert!(source_error.contains("max_source_bytes"), "{source_error}");
    let string_error = export_function_fragment_bounded(&hbc, 0, MAX, strings - 1)
        .unwrap_err()
        .to_string();
    assert!(string_error.contains("max_string_bytes"), "{string_error}");
    assert!(export_function_fragment_bounded(&hbc, 0, 1, MAX)
        .unwrap_err()
        .to_string()
        .contains("max_source_bytes"));
    assert!(export_function_fragment_bounded(&hbc, 0, MAX, 1)
        .unwrap_err()
        .to_string()
        .contains("max_string_bytes"));
}

#[test]
fn invalid_limits_and_function_indices_fail_clearly() {
    let bytes = fixture("test_empty.hbc");
    let hbc = HbcFile::parse(&bytes).unwrap();
    for (source, strings, field) in [
        (0, MAX, "max_source_bytes"),
        (MAX + 1, MAX, "max_source_bytes"),
        (usize::MAX, MAX, "max_source_bytes"),
        (MAX, 0, "max_string_bytes"),
        (MAX, MAX + 1, "max_string_bytes"),
        (MAX, usize::MAX, "max_string_bytes"),
    ] {
        let message = export_function_fragment_bounded(&hbc, 0, source, strings)
            .unwrap_err()
            .to_string();
        assert!(
            message.contains(field) && message.contains("64 MiB"),
            "{message}"
        );
    }
    for id in [hbc.functions.count(), u32::MAX] {
        let message = export_function_fragment_bounded(&hbc, id, MAX, MAX)
            .unwrap_err()
            .to_string();
        assert!(message.contains("Unknown function"), "{message}");
    }
    assert!(export_function_fragment_bounded(&hbc, hbc.functions.count() - 1, MAX, MAX).is_ok());
}

#[test]
fn unused_duplicate_entries_count_towards_string_budget() {
    let bytes = fixture("test_empty.hbc");
    let mut hbc = HbcFile::parse(&bytes).unwrap();
    let expected = export_function_fragments(&hbc, &[0]).unwrap().remove(0).1;
    let original_budget = string_budget(&hbc);
    let entry = &hbc.strings.small_entries[0];
    let (is_utf16, is_identifier, offset, length) = (
        entry.is_utf16,
        entry.is_identifier,
        entry.offset,
        entry.length,
    );
    for _ in 0..8 {
        hbc.strings.small_entries.push(SmallStringTableEntry {
            is_utf16,
            is_identifier,
            offset,
            length,
        });
        hbc.strings.string_kinds.push(StringKind::String);
        hbc.strings.string_cache.push(None);
        hbc.strings.string_count += 1;
    }
    let expanded_budget = string_budget(&hbc);
    assert!(expanded_budget > original_budget);
    assert!(
        export_function_fragment_bounded(&hbc, 0, MAX, original_budget)
            .unwrap_err()
            .to_string()
            .contains("max_string_bytes")
    );
    assert_eq!(
        export_function_fragment_bounded(&hbc, 0, MAX, expanded_budget).unwrap(),
        expected
    );
}

#[test]
fn escaped_and_lossy_unused_entries_have_exact_budgets() {
    let bytes = fixture("test_empty.hbc");
    let hbc = HbcFile::parse(&bytes).unwrap();
    let mut storage = hbc.strings.storage.to_vec();
    let offset = storage.len() as u32;
    let utf8 = b"plain\"\\\n\0\x08\x0c\xe2\x80\xa8\xe2\x80\xa9\xff\xf0\x90";
    storage.extend_from_slice(utf8);
    let utf16_offset = storage.len() as u32;
    storage.extend_from_slice(&[b'A', 0, 0, 0xd8, 0x28, 0x20, b'"', 0]);
    let mut hbc = HbcFile::parse(&bytes).unwrap();
    hbc.strings.storage = &storage;
    for (is_utf16, offset, length) in [(false, offset, utf8.len() as u8), (true, utf16_offset, 4)] {
        hbc.strings.small_entries.push(SmallStringTableEntry {
            is_utf16,
            is_identifier: None,
            offset,
            length,
        });
        hbc.strings.string_kinds.push(StringKind::String);
        hbc.strings.string_cache.push(None);
        hbc.strings.string_count += 1;
    }
    let budget = string_budget(&hbc);
    let expected = export_function_fragments(&hbc, &[0]).unwrap().remove(0).1;
    assert_eq!(
        export_function_fragment_bounded(&hbc, 0, MAX, budget).unwrap(),
        expected
    );
    assert!(export_function_fragment_bounded(&hbc, 0, MAX, budget - 1)
        .unwrap_err()
        .to_string()
        .contains("max_string_bytes"));
}

#[test]
fn ascii_table_is_not_rejected_by_a_blanket_escape_multiplier() {
    let bytes = fixture("test_empty.hbc");
    let original = HbcFile::parse(&bytes).unwrap();
    let mut storage = original.strings.storage.to_vec();
    let offset = storage.len() as u32;
    storage.resize(storage.len() + 1024 * 1024, b'a');
    let mut hbc = HbcFile::parse(&bytes).unwrap();
    hbc.strings.storage = &storage;
    let overflow = hbc.strings.overflow_entries.len() as u32;
    hbc.strings.overflow_entries.push(OverflowStringTableEntry {
        offset,
        length: 1024 * 1024,
    });
    for _ in 0..12 {
        hbc.strings.small_entries.push(SmallStringTableEntry {
            is_utf16: false,
            is_identifier: None,
            offset: overflow,
            length: 255,
        });
        hbc.strings.string_kinds.push(StringKind::String);
        hbc.strings.string_cache.push(None);
        hbc.strings.string_count += 1;
    }
    assert!(export_function_fragment_bounded(&hbc, 0, MAX, MAX).is_ok());
}

#[test]
fn literal_count_is_refused_before_unpacking_or_formatting() {
    let bytes = fixture("test_empty.hbc");
    let mut hbc = HbcFile::parse(&bytes).unwrap();
    hbc.functions.parsed_headers[0]
        .cached_instructions
        .get_mut()
        .unwrap()
        .as_mut()
        .unwrap()[0]
        .instruction = UnifiedInstruction::NewArrayWithBuffer {
        operand_0: 0,
        operand_1: u16::MAX,
        operand_2: u16::MAX,
        // Invalid offset must not be consulted before count preflight.
        operand_3: u16::MAX,
    };
    let message = export_function_fragment_bounded(&hbc, 0, 4096, MAX)
        .unwrap_err()
        .to_string();
    assert!(message.contains("max_source_bytes"), "{message}");
}

#[test]
fn repeated_literal_references_are_preflighted_before_cloning() {
    let bytes = fixture("test_empty.hbc");
    let mut hbc = HbcFile::parse(&bytes).unwrap();
    let mut storage = hbc.strings.storage.to_vec();
    let offset = storage.len() as u32;
    storage.extend_from_slice(&[b'x'; 32]);
    hbc.strings.storage = &storage;
    let id = u8::try_from(hbc.strings.string_count).unwrap();
    hbc.strings.small_entries.push(SmallStringTableEntry {
        is_utf16: false,
        is_identifier: None,
        offset,
        length: 32,
    });
    hbc.strings.string_kinds.push(StringKind::String);
    hbc.strings.string_cache.push(None);
    hbc.strings.string_count += 1;
    // Extended ByteString sequence: 100 copies of one 32-byte string.
    let mut literals = vec![0xe0, 100];
    literals.resize(102, id);
    hbc.serialized_literals.arrays_data = &literals;
    hbc.functions.parsed_headers[0]
        .cached_instructions
        .get_mut()
        .unwrap()
        .as_mut()
        .unwrap()[0]
        .instruction = UnifiedInstruction::NewArrayWithBuffer {
        operand_0: 0,
        operand_1: 100,
        operand_2: 100,
        operand_3: 0,
    };
    let message = export_function_fragment_bounded(&hbc, 0, 1024, MAX)
        .unwrap_err()
        .to_string();
    assert!(message.contains("max_source_bytes"), "{message}");
}
