use hermes_dec_rs::cli::disasm::disasm;
use std::fs;
use std::path::Path;

/// Test that dense switch instructions are parsed and disassembled correctly
#[test]
fn test_dense_switch_disassembly() {
    let hbc_file = Path::new("data/dense_switch_test.hbc");
    let expected_file = Path::new("data/dense_switch_test.hasm.expected");
    
    // Verify test files exist
    assert!(hbc_file.exists(), "Test HBC file not found: {}", hbc_file.display());
    assert!(expected_file.exists(), "Expected file not found: {}", expected_file.display());
    
    // Run disassembler
    let result = disasm(hbc_file);
    assert!(result.is_ok(), "Disassembly failed: {:?}", result.err());
    
    // Wait for output file and read it
    let output_file = hbc_file.with_extension("hasm");
    let mut attempts = 0;
    while !output_file.exists() && attempts < 10 {
        std::thread::sleep(std::time::Duration::from_millis(100));
        attempts += 1;
    }
    
    assert!(output_file.exists(), "Output file was not created: {}", output_file.display());
    
    let actual_content = fs::read_to_string(&output_file)
        .expect("Failed to read output file");
    let expected_content = fs::read_to_string(expected_file)
        .expect("Failed to read expected file");
    
    // Compare the contents
    assert_eq!(
        actual_content, expected_content,
        "Disassembly output does not match expected output"
    );
    
    // Verify specific dense switch instruction is present
    assert!(
        actual_content.contains("SwitchImm"),
        "Expected SwitchImm instruction not found in disassembly"
    );
    
    // Verify the switch has the correct parameters
    // SwitchImm r0, 150, [144], 0, 20
    assert!(
        actual_content.contains("SwitchImm         r0, 150, [144], 0, 20"),
        "Expected SwitchImm instruction with correct parameters not found"
    );
    
    // Verify individual case labels are present
    let expected_cases = [
        "twenty", "nineteen", "eighteen", "seventeen", "sixteen",
        "fifteen", "fourteen", "thirteen", "twelve", "eleven",
        "ten", "nine", "eight", "seven", "six",
        "five", "four", "three", "two", "one", "zero", "other"
    ];
    
    for case in &expected_cases {
        assert!(
            actual_content.contains(&format!("LoadConstString   r0, \"{}\"", case)),
            "Expected case '{}' not found in disassembly",
            case
        );
    }
}

/// Test that sparse switch instructions (using individual comparisons) are also handled correctly
#[test]
fn test_sparse_switch_disassembly() {
    let hbc_file = Path::new("data/dense_switch_test.hbc");
    
    // Run disassembler
    let result = disasm(hbc_file);
    assert!(result.is_ok(), "Disassembly failed: {:?}", result.err());
    
    // Read the output
    let output_file = hbc_file.with_extension("hasm");
    let actual_content = fs::read_to_string(&output_file)
        .expect("Failed to read output file");
    
    // Verify sparse switch uses individual comparisons
    assert!(
        actual_content.contains("JStrictEqualLong"),
        "Expected JStrictEqualLong instructions for sparse switch not found"
    );
    
    assert!(
        actual_content.contains("JStrictEqual"),
        "Expected JStrictEqual instructions for sparse switch not found"
    );
    
    // Verify negative number handling in sparse switch
    assert!(
        actual_content.contains("LoadConstInt      r0, 4294967291"), // -5
        "Expected negative number constant not found"
    );
    
    // Verify label structure for sparse switch
    let expected_labels = ["L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11", "L12", "L13", "L14", "L15", "L16"];
    for label in &expected_labels {
        assert!(
            actual_content.contains(&format!("{}:", label)),
            "Expected label '{}' not found in disassembly",
            label
        );
    }
}

/// Test that both switch types produce correct control flow
#[test]
fn test_switch_control_flow() {
    let hbc_file = Path::new("data/dense_switch_test.hbc");
    
    // Run disassembler
    let result = disasm(hbc_file);
    assert!(result.is_ok(), "Disassembly failed: {:?}", result.err());
    
    // Read the output
    let output_file = hbc_file.with_extension("hasm");
    let actual_content = fs::read_to_string(&output_file)
        .expect("Failed to read output file");
    
    // Verify function structure
    assert!(
        actual_content.contains("Function<denseSwitchTest>1"),
        "denseSwitchTest function not found"
    );
    
    assert!(
        actual_content.contains("Function<largeSwitchTest>2"),
        "largeSwitchTest function not found"
    );
    
    // Verify both functions have proper return statements
    let return_count = actual_content.matches("Ret               r0").count();
    assert!(
        return_count >= 20, // At least 20 return statements (cases + default)
        "Expected at least 20 return statements, found {}",
        return_count
    );
}

/// Test that the HBC file can be parsed correctly
#[test]
fn test_dense_switch_hbc_parsing() {
    let hbc_file = Path::new("data/dense_switch_test.hbc");
    let data = fs::read(hbc_file).expect("Failed to read HBC file");
    
    let parsed_file = hermes_dec_rs::hbc::HbcFile::parse(&data)
        .expect("Failed to parse HBC file");
    
    // Verify basic structure
    assert_eq!(parsed_file.header.function_count(), 3);
    assert!(parsed_file.header.string_count() > 0);
    
    // Verify functions can be accessed
    let functions = &parsed_file.functions;
    assert_eq!(functions.count(), 3);
    
    // Verify we can get instructions for each function
    for i in 0..functions.count() {
        let instructions = functions.get_instructions(i)
            .expect(&format!("Failed to get instructions for function {}", i));
        assert!(!instructions.is_empty(), "Function {} has no instructions", i);
    }
} 