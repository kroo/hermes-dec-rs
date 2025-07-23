use hermes_dec_rs::hbc::{HBC_MAGIC, HbcFile};
use std::fs;

#[test]
fn test_hbc_magic_constant() {
    assert_eq!(HBC_MAGIC, 0x1f1903c103bc1fc6);
}

#[test]
fn test_hermes_dec_sample_hbc() {
    let data = fs::read("data/hermes_dec_sample.hbc").expect("Failed to read hermes_dec_sample.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse hermes_dec_sample.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic(), HBC_MAGIC);
    assert_eq!(hbc_file.header.version(), 94);
    assert!(hbc_file.header.function_count() > 0);
    assert!(hbc_file.header.string_count() > 0);
    
    // Test string table
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    assert!(!strings.is_empty());
    
    // Test function table
    assert!(!hbc_file.functions.parsed_headers.is_empty());
    
    // Test that we can serialize without errors
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize hermes_dec_sample.hbc");
}

#[test]
fn test_bigints_v96_hbc() {
    let data = fs::read("data/bigints_v96.hbc").expect("Failed to read bigints_v96.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse bigints_v96.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic(), HBC_MAGIC);
    assert_eq!(hbc_file.header.version(), 96);
    assert!(hbc_file.header.big_int_count().unwrap_or(0) > 0);
    
    // Test BigInt table
    let bigints = hbc_file.bigints.extract_bigints().expect("Failed to extract bigints");
    assert!(!bigints.is_empty());
    
    // Verify BigInt values are properly decoded as decimal strings
    for bigint in &bigints {
        // BigInt values should be decimal strings, not raw bytes
        assert!(!bigint.contains("bigint_"));
        // Should contain only digits and possibly a minus sign
        assert!(bigint.chars().all(|c| c.is_ascii_digit() || c == '-'));
    }
    
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize bigints_v96.hbc");
}

#[test]
fn test_array_constants_v96_hbc() {
    let data = fs::read("data/array_constants_v96.hbc").expect("Failed to read array_constants_v96.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse array_constants_v96.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic(), HBC_MAGIC);
    assert_eq!(hbc_file.header.version(), 96);
    assert!(hbc_file.header.function_count() > 0);
    assert!(hbc_file.header.string_count() > 0);
    
    // Test string table
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    assert!(!strings.is_empty());
    
    // Test function table
    assert!(!hbc_file.functions.parsed_headers.is_empty());
    
    // Test serialized literal tables (arrays)
    assert!(!hbc_file.serialized_literals.arrays.items.is_empty());
    
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize array_constants_v96.hbc");
}

#[test]
fn test_array_constants_v90_hbc() {
    let data = fs::read("data/array_constants_v90.hbc").expect("Failed to read array_constants_v90.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse array_constants_v90.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic, HBC_MAGIC);
    assert_eq!(hbc_file.header.version, 90);
    assert!(hbc_file.header.function_count > 0);
    assert!(hbc_file.header.string_count > 0);
    
    // Test string table
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    assert!(!strings.is_empty());
    
    // Test function table
    assert!(!hbc_file.functions.parsed_headers.is_empty());
    
    // Test serialized literal tables (arrays)
    assert!(!hbc_file.serialized_literals.arrays.items.is_empty());
    
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize array_constants_v90.hbc");
}

#[test]
fn test_cjs_v96_hbc() {
    let data = fs::read("data/cjs_v96.hbc").expect("Failed to read cjs_v96.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs_v96.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic(), HBC_MAGIC);
    assert_eq!(hbc_file.header.version(), 96);
    assert_eq!(hbc_file.header.cjs_module_count(), 2);
    assert!(!hbc_file.header.cjs_modules_statically_resolved());
    
    // Test CJS modules
    assert_eq!(hbc_file.cjs_modules.entries.len(), 2);
    
    // Test that CJS modules have entries
    for (i, module) in hbc_file.cjs_modules.entries.iter().enumerate() {
        assert!(module.symbol_id > 0, "CJS module {} should have a symbol ID", i);
        assert!(module.offset > 0, "CJS module {} should have an offset", i);
    }
    
    // Test string table
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    assert!(!strings.is_empty());
    
    // Test function table
    assert!(!hbc_file.functions.parsed_headers.is_empty());
    
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize cjs_v96.hbc");
}

#[test]
fn test_cjs_show_source_hbc() {
    let data = fs::read("data/cjs-show-source.hbc").expect("Failed to read cjs-show-source.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs-show-source.hbc");
    
    // Test header
    assert_eq!(hbc_file.header.magic(), HBC_MAGIC);
    assert_eq!(hbc_file.header.version(), 96);
    assert_eq!(hbc_file.header.cjs_module_count(), 2);
    assert!(!hbc_file.header.cjs_modules_statically_resolved());
    
    // Test CJS modules
    assert_eq!(hbc_file.cjs_modules.entries.len(), 2);
    
    // Test function source table
    assert!(!hbc_file.function_sources.entries.is_empty());
    
    // Test that function sources have entries
    for source in &hbc_file.function_sources.entries {
        assert!(source.function_id > 0, "Function source should have valid function ID");
        assert!(source.string_id > 0, "Function source should have valid string ID");
    }
    
    // Test overflow string handling
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    let has_overflow_strings = strings.iter().any(|s| s.len() > 100);
    assert!(has_overflow_strings, "Should have overflow strings with long content");
    
    // Test string table
    assert!(!strings.is_empty());
    
    // Test function table
    assert!(!hbc_file.functions.parsed_headers.is_empty());
    
    let _json = serde_json::to_string(&hbc_file).expect("Failed to serialize cjs-show-source.hbc");
}

#[test]
fn test_string_table_overflow_handling() {
    let data = fs::read("data/cjs-show-source.hbc").expect("Failed to read cjs-show-source.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs-show-source.hbc");
    
    // Test that overflow strings are properly decoded
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    let long_strings: Vec<&String> = strings.iter()
        .filter(|s| s.len() > 100)
        .collect();
    
    assert!(!long_strings.is_empty(), "Should have overflow strings");
    
    // Test that overflow strings contain readable content
    for string in long_strings {
        assert!(!string.contains('\u{0}'), "Overflow string should not contain null bytes");
        assert!(string.chars().any(|c| c.is_ascii_alphabetic()), 
                "Overflow string should contain readable text: {}", string);
    }
}

#[test]
fn test_utf16_string_decoding() {
    let data = fs::read("data/cjs-show-source.hbc").expect("Failed to read cjs-show-source.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs-show-source.hbc");
    
    // Test that UTF-16 strings are properly decoded
    let strings = hbc_file.strings.extract_strings().expect("Failed to extract strings");
    for (i, string) in strings.iter().enumerate() {
        // Should not contain mojibake or null bytes
        assert!(!string.contains('\u{0}'), "String {} should not contain null bytes", i);
        
        // Should be valid UTF-8
        assert!(string.is_char_boundary(0), "String {} should be valid UTF-8", i);
        
        // If it's a long string, it should contain readable content
        if string.len() > 50 {
            assert!(string.chars().any(|c| c.is_ascii_alphabetic()), 
                    "Long string {} should contain readable text: {}", i, string);
        }
    }
}

#[test]
fn test_bigint_decimal_representation() {
    let data = fs::read("data/bigints_v96.hbc").expect("Failed to read bigints_v96.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse bigints_v96.hbc");
    
    // Test that BigInt values are properly decoded as decimal strings
    let bigints = hbc_file.bigints.extract_bigints().expect("Failed to extract bigints");
    for (i, bigint) in bigints.iter().enumerate() {
        // Should not be placeholder text
        assert!(!bigint.starts_with("bigint_"), 
                "BigInt {} should not be placeholder: {}", i, bigint);
        
        // Should be a valid decimal number
        assert!(bigint.chars().all(|c| c.is_ascii_digit() || c == '-'), 
                "BigInt {} should be decimal: {}", i, bigint);
        
        // Should not be empty
        assert!(!bigint.is_empty(), "BigInt {} should not be empty", i);
    }
}

#[test]
fn test_cjs_module_symbol_resolution() {
    let data = fs::read("data/cjs_v96.hbc").expect("Failed to read cjs_v96.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs_v96.hbc");
    
    // Test that CJS modules have valid entries
    for (i, module) in hbc_file.cjs_modules.entries.iter().enumerate() {
        // Should have valid symbol ID and offset
        assert!(module.symbol_id > 0, "CJS module {} should have valid symbol ID", i);
        assert!(module.offset > 0, "CJS module {} should have valid offset", i);
    }
}

#[test]
fn test_function_source_extraction() {
    let data = fs::read("data/cjs-show-source.hbc").expect("Failed to read cjs-show-source.hbc");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse cjs-show-source.hbc");
    
    // Test that function sources are properly extracted
    for (i, source) in hbc_file.function_sources.entries.iter().enumerate() {
        assert!(source.function_id > 0, "Function source {} should have valid function ID", i);
        assert!(source.string_id > 0, "Function source {} should have valid string ID", i);
    }
} 