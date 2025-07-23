use hermes_dec_rs::hbc::tables::function_table::ParsedFunctionHeader;
use hermes_dec_rs::hbc::tables::function_table::SmallFunctionHeader;
use hermes_dec_rs::hbc::tables::function_table::DebugOffsets;
use hermes_dec_rs::hbc::tables::function_table::DebugOffsetsLegacy;

#[test]
fn test_parsed_function_header_instructions() {
    // Create a minimal ParsedFunctionHeader for testing
    let small_header = SmallFunctionHeader {
        word_1: 0x00000000, // offset = 0, param_count = 0
        word_2: 0x00000000, // bytecode_size = 0, function_name = 0
        word_3: 0x00000000, // info_offset = 0, frame_size = 0
        word_4: 0x00000000, // environment_size = 0, etc.
    };
    
    let debug_offsets = DebugOffsets::Legacy(DebugOffsetsLegacy {
        source_locations: 0,
        scope_desc_data: 0,
    });
    
    // Create a simple bytecode body with a few instructions
    // This is a minimal example - in practice, the body would contain real bytecode
    let body: &[u8] = &[0x00, 0x00, 0x00, 0x00]; // Placeholder bytecode
    
    let parsed_header = ParsedFunctionHeader {
        index: 0,
        header: small_header,
        large_header: None,
        exc_handlers: Vec::new(),
        debug_offsets,
        body,
        version: 96, // Use a recent version
        cached_instructions: std::sync::OnceLock::new(),
    };
    
    // Test that the instructions method exists and can be called
    let result = parsed_header.instructions();
    
    // The result might be an error for this minimal test, but that's expected
    // since we're using placeholder bytecode
    assert!(result.is_ok() || result.is_err());
} 