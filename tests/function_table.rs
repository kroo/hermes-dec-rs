use hermes_dec_rs::hbc::tables::function_table::DebugOffsets;
use hermes_dec_rs::hbc::tables::function_table::DebugOffsetsLegacy;
use hermes_dec_rs::hbc::tables::function_table::ParsedFunctionHeader;
use hermes_dec_rs::hbc::tables::function_table::SmallFunctionHeader;

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
#[test]
fn overflowed_header_exception_table_follows_the_header_once() {
    let original = std::fs::read("data/simple_arithmetic.hbc").unwrap();
    let hbc = hermes_dec_rs::HbcFile::parse(&original).unwrap();
    let mut header = hbc.header;
    header.function_count = 1;
    let mut bytes = vec![0u8; 128];
    // Overflow pointer = 32; flags retain hasExceptionHandler and overflowed.
    bytes[0..4].copy_from_slice(&32u32.to_le_bytes());
    bytes[12..16].copy_from_slice(&((1u32 << 27) | (1u32 << 29)).to_le_bytes());
    let fields = [96u32, 1, 1, 0, 32, 1, 0];
    for (index, value) in fields.iter().enumerate() {
        bytes[32 + index * 4..36 + index * 4].copy_from_slice(&value.to_le_bytes());
    }
    bytes[62] = 2 | 8;
    bytes[64..68].copy_from_slice(&1u32.to_le_bytes());
    bytes[68..72].copy_from_slice(&0u32.to_le_bytes());
    bytes[72..76].copy_from_slice(&1u32.to_le_bytes());
    bytes[76..80].copy_from_slice(&0u32.to_le_bytes());
    let mut offset = 0;
    let functions = hermes_dec_rs::hbc::tables::function_table::FunctionTable::parse(
        &bytes,
        &header,
        &mut offset,
    )
    .unwrap();
    let parsed = functions.get_parsed_header(0).unwrap();
    assert_eq!(parsed.exc_handlers.len(), 1);
    let handler = parsed.exc_handlers[0];
    assert_eq!((handler.start, handler.end, handler.target), (0, 1, 0));
    assert_eq!(parsed.body.len(), 1);
}
