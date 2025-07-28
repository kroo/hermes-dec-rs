use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::hbc::tables::switch_table::{SwitchCase, SwitchTable, SwitchTableCollection};
use hermes_dec_rs::hbc::HbcFile;
use std::fs;
use std::path::Path;

/// Test that switch tables are parsed correctly from the dense_switch_test.hbc file
#[test]
fn test_switch_table_parsing() {
    let hbc_file_path = Path::new("data/dense_switch_test.hbc");

    // Verify test file exists
    assert!(
        hbc_file_path.exists(),
        "Test HBC file not found: {}",
        hbc_file_path.display()
    );

    // Parse the HBC file
    let data = fs::read(hbc_file_path).expect("Failed to read HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse HBC file");

    // Verify basic structure
    assert_eq!(hbc_file.header.function_count(), 3);

    // Check that function 1 has switch tables
    assert!(hbc_file.switch_tables.has_switch_tables(1));
    assert_eq!(hbc_file.switch_tables.get_switch_table_count(1), 1);

    // Get the switch table for function 1
    let switch_tables = hbc_file
        .switch_tables
        .get_switch_tables_for_function(1)
        .unwrap();
    assert_eq!(switch_tables.len(), 1);

    let switch_table = &switch_tables[0];

    // Verify switch table properties based on the SwitchImm instruction
    // SwitchImm r0, 150, [144], 0, 20
    assert_eq!(switch_table.min_value, 0);
    assert_eq!(switch_table.max_value, 20);
    assert_eq!(switch_table.default_offset, 144);
    assert_eq!(switch_table.jump_table_offset, 150);
    assert_eq!(switch_table.switch_instruction_index, 1);
    assert_eq!(switch_table.function_index, 1);

    // Verify all cases are present (0-20, inclusive)
    assert_eq!(switch_table.case_count(), 21);
    assert!(switch_table.is_complete());

    // Verify case values are in the expected range
    let case_values = switch_table.get_case_values();
    assert_eq!(case_values.len(), 21);
    assert_eq!(case_values[0], 0);
    assert_eq!(case_values[20], 20);

    // Verify specific cases exist
    assert!(switch_table.get_case_by_value(0).is_some());
    assert!(switch_table.get_case_by_value(10).is_some());
    assert!(switch_table.get_case_by_value(20).is_some());
    assert!(switch_table.get_case_by_value(21).is_none()); // Out of range

    // Verify value range checking
    assert!(switch_table.is_value_in_range(0));
    assert!(switch_table.is_value_in_range(10));
    assert!(switch_table.is_value_in_range(20));
    assert!(!switch_table.is_value_in_range(21));
    assert!(!switch_table.is_value_in_range(100));

    // Verify get_case_for_value works correctly
    assert!(switch_table.get_case_for_value(0).is_some());
    assert!(switch_table.get_case_for_value(10).is_some());
    assert!(switch_table.get_case_for_value(20).is_some());
    assert!(switch_table.get_case_for_value(21).is_none()); // Out of range
}

/// Test that switch tables can be retrieved by instruction index
#[test]
fn test_switch_table_by_instruction() {
    let hbc_file_path = Path::new("data/dense_switch_test.hbc");
    let data = fs::read(hbc_file_path).expect("Failed to read HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse HBC file");

    // Get switch table by instruction index (SwitchImm is at instruction 1 in function 1)
    let switch_table = hbc_file
        .switch_tables
        .get_switch_table_by_instruction(1, 1)
        .unwrap();

    assert_eq!(switch_table.min_value, 0);
    assert_eq!(switch_table.max_value, 20);
    assert_eq!(switch_table.case_count(), 21);

    // Test non-existent switch table
    assert!(hbc_file
        .switch_tables
        .get_switch_table_by_instruction(1, 99)
        .is_none());
}

/// Test that functions without switch instructions have no switch tables
#[test]
fn test_no_switch_tables() {
    let hbc_file_path = Path::new("data/dense_switch_test.hbc");
    let data = fs::read(hbc_file_path).expect("Failed to read HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse HBC file");

    // Function 0 should not have switch tables (it's the main function)
    assert!(!hbc_file.switch_tables.has_switch_tables(0));
    assert_eq!(hbc_file.switch_tables.get_switch_table_count(0), 0);
    assert!(hbc_file
        .switch_tables
        .get_switch_tables_for_function(0)
        .is_none());
}

/// Test switch table collection functionality
#[test]
fn test_switch_table_collection() {
    let mut collection = SwitchTableCollection::new();

    // Create a test switch table
    let mut switch_table = SwitchTable::new(0, 5, 100, 150, 1000, 1, 0);
    switch_table.add_case(0, 10);
    switch_table.add_case(1, 20);
    switch_table.add_case(2, 30);
    switch_table.add_case(3, 40);
    switch_table.add_case(4, 50);
    switch_table.add_case(5, 60);

    collection.add_switch_table(switch_table);

    // Verify collection properties
    assert_eq!(collection.get_switch_table_count(0), 1);
    assert!(collection.has_switch_tables(0));
    assert!(!collection.has_switch_tables(1));

    // Verify we can retrieve the switch table
    let tables = collection.get_switch_tables_for_function(0).unwrap();
    assert_eq!(tables.len(), 1);
    assert_eq!(tables[0].case_count(), 6);
    assert!(tables[0].is_complete());

    // Verify instruction mapping
    let table = collection.get_switch_table_by_instruction(0, 1).unwrap();
    assert_eq!(table.min_value, 0);
    assert_eq!(table.max_value, 5);
}

/// Test switch case creation and properties
#[test]
fn test_switch_case() {
    let case = SwitchCase {
        value: 42,
        target_offset: 100,
        target_instruction_index: Some(10),
    };

    assert_eq!(case.value, 42);
    assert_eq!(case.target_offset, 100);
    assert_eq!(case.target_instruction_index, Some(10));
}

/// Test switch table edge cases
#[test]
fn test_switch_table_edge_cases() {
    // Test empty range
    let mut switch_table = SwitchTable::new(5, 5, 100, 150, 1000, 1, 0);
    switch_table.add_case(5, 10);

    assert_eq!(switch_table.case_count(), 1);
    assert!(switch_table.is_complete());
    assert!(switch_table.is_value_in_range(5));
    assert!(!switch_table.is_value_in_range(4));
    assert!(!switch_table.is_value_in_range(6));

    // Test large range
    let mut switch_table = SwitchTable::new(0, 100, 100, 150, 1000, 1, 0);
    for i in 0..=100 {
        switch_table.add_case(i, (i * 10) as i32);
    }

    assert_eq!(switch_table.case_count(), 101);
    assert!(switch_table.is_complete());
    assert!(switch_table.is_value_in_range(0));
    assert!(switch_table.is_value_in_range(50));
    assert!(switch_table.is_value_in_range(100));
    assert!(!switch_table.is_value_in_range(101));
}

/// Test that the dense switch test file produces the expected switch table structure
#[test]
fn test_dense_switch_expected_structure() {
    let hbc_file_path = Path::new("data/dense_switch_test.hbc");
    let data = fs::read(hbc_file_path).expect("Failed to read HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse HBC file");

    // Function 1 should have exactly one switch table
    let switch_tables = hbc_file
        .switch_tables
        .get_switch_tables_for_function(1)
        .unwrap();
    assert_eq!(switch_tables.len(), 1);

    let switch_table = &switch_tables[0];

    // Based on the JavaScript source, we expect cases 0-20 with "other" as default
    // The switch table should have all 21 cases (0-20 inclusive)
    assert_eq!(switch_table.expected_case_count(), 21);
    assert_eq!(switch_table.case_count(), 21);
    assert!(switch_table.is_complete());

    // Verify all expected case values are present
    for i in 0..=20 {
        assert!(
            switch_table.get_case_by_value(i).is_some(),
            "Case {} not found",
            i
        );
        assert!(
            switch_table.is_value_in_range(i),
            "Value {} not in range",
            i
        );
        assert!(
            switch_table.get_case_for_value(i).is_some(),
            "Case {} not found via get_case_for_value",
            i
        );
    }

    // Verify out-of-range values are handled correctly
    assert!(!switch_table.is_value_in_range(21));
    assert!(switch_table.get_case_for_value(21).is_none());
    assert!(switch_table.get_case_by_value(21).is_none());

    // Get instructions for function 1 to verify switch case targets
    let instructions = hbc_file
        .functions
        .get_instructions(1)
        .expect("Failed to get instructions for function 1");

    // Define the expected case-to-string mapping based on the JavaScript source
    let expected_case_strings = [
        (0, "zero"),
        (1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine"),
        (10, "ten"),
        (11, "eleven"),
        (12, "twelve"),
        (13, "thirteen"),
        (14, "fourteen"),
        (15, "fifteen"),
        (16, "sixteen"),
        (17, "seventeen"),
        (18, "eighteen"),
        (19, "nineteen"),
        (20, "twenty"),
    ];

    // Verify that each switch case points to a LoadConstString instruction with the correct string
    for (case_value, expected_string) in expected_case_strings.iter() {
        let case = switch_table
            .get_case_by_value(*case_value)
            .expect(&format!("Case {} not found", case_value));

        // Get the target instruction index
        let target_instruction_index = case.target_instruction_index.expect(&format!(
            "Case {} has no target instruction index",
            case_value
        ));

        // Verify the target instruction exists
        assert!(
            target_instruction_index < instructions.len() as u32,
            "Case {} target instruction index {} out of bounds ({} instructions)",
            case_value,
            target_instruction_index,
            instructions.len()
        );

        let target_instruction = &instructions[target_instruction_index as usize];

        // Verify the target instruction is a LoadConstString
        match &target_instruction.instruction {
            hermes_dec_rs::generated::unified_instructions::UnifiedInstruction::LoadConstString { operand_0: _register, operand_1: string_id } => {
                // Get the string from the string table
                let actual_string = hbc_file.strings.get(*string_id as u32).expect(&format!(
                    "Failed to get string with ID {} for case {}", string_id, case_value
                ));

                assert_eq!(
                    actual_string, *expected_string,
                    "Case {} points to string '{}', expected '{}'",
                    case_value, actual_string, expected_string
                );
            },
            other_instruction => {
                panic!(
                    "Case {} target instruction is {:?}, expected LoadConstString",
                    case_value, other_instruction
                );
            }
        }
    }

    // Verify the default case points to "other"
    let default_instruction_index = switch_table
        .default_instruction_index
        .expect("Default case has no instruction index");
    assert!(
        default_instruction_index < instructions.len() as u32,
        "Default case target instruction index {} out of bounds ({} instructions)",
        default_instruction_index,
        instructions.len()
    );

    let default_instruction = &instructions[default_instruction_index as usize];
    match &default_instruction.instruction {
        hermes_dec_rs::generated::unified_instructions::UnifiedInstruction::LoadConstString {
            operand_0: _register,
            operand_1: string_id,
        } => {
            let actual_string = hbc_file
                .strings
                .get(*string_id as u32)
                .expect("Failed to get string for default case");
            assert_eq!(
                actual_string, "other",
                "Default case points to string '{}', expected 'other'",
                actual_string
            );
        }
        other_instruction => {
            panic!(
                "Default case target instruction is {:?}, expected LoadConstString",
                other_instruction
            );
        }
    }

    // Verify a few specific cases with detailed logging
    let test_cases = [
        (0, "zero"),
        (5, "five"),
        (10, "ten"),
        (15, "fifteen"),
        (20, "twenty"),
    ];
    for (case_value, expected_string) in test_cases.iter() {
        let case = switch_table.get_case_by_value(*case_value).unwrap();

        if let Some(target_instruction_index) = case.target_instruction_index {
            let target_instruction = &instructions[target_instruction_index as usize];

            match &target_instruction.instruction {
                hermes_dec_rs::generated::unified_instructions::UnifiedInstruction::LoadConstString { operand_0: _register, operand_1: string_id } => {
                    let actual_string = hbc_file.strings.get(*string_id as u32).unwrap();
                    println!("✓ Case {}: target_offset={}, instruction_index={}, string='{}'", 
                        case_value, case.target_offset, case.target_instruction_index.unwrap(), actual_string);
                    assert_eq!(actual_string, *expected_string);
                },
                _ => {
                    println!("✗ Case {} target is not LoadConstString: {:?}", case_value, target_instruction.instruction);
                    panic!("Case {} target is not LoadConstString", case_value);
                }
            }
        } else {
            println!("✗ Case {} has no target instruction index", case_value);
            panic!("Case {} has no target instruction index", case_value);
        }
    }
}

/// Demonstrate switch table parsing results
#[test]
fn demonstrate_switch_table_parsing() {
    let hbc_file_path = Path::new("data/dense_switch_test.hbc");
    let data = fs::read(hbc_file_path).expect("Failed to read HBC file");
    let hbc_file = HbcFile::parse(&data).expect("Failed to parse HBC file");

    println!("\n=== Switch Table Parsing Demonstration ===");
    println!("HBC File: {} functions", hbc_file.header.function_count());

    for function_index in 0..hbc_file.header.function_count() {
        println!("\nFunction {}:", function_index);

        if hbc_file.switch_tables.has_switch_tables(function_index) {
            let switch_table_count = hbc_file
                .switch_tables
                .get_switch_table_count(function_index);
            println!("  ✓ Switch tables: {} found", switch_table_count);

            if let Some(switch_tables) = hbc_file
                .switch_tables
                .get_switch_tables_for_function(function_index)
            {
                for (i, switch_table) in switch_tables.iter().enumerate() {
                    println!("  Switch table {}:", i);
                    println!(
                        "    Range: {} to {} (inclusive)",
                        switch_table.min_value, switch_table.max_value
                    );
                    println!(
                        "    Cases: {} (expected: {})",
                        switch_table.case_count(),
                        switch_table.expected_case_count()
                    );
                    println!("    Complete: {}", switch_table.is_complete());
                    println!("    Default offset: {}", switch_table.default_offset);
                    println!("    Jump table offset: {}", switch_table.jump_table_offset);
                    println!(
                        "    Switch instruction index: {}",
                        switch_table.switch_instruction_index
                    );

                    // Show some case examples
                    if !switch_table.cases.is_empty() {
                        println!("    Sample cases:");
                        for case in switch_table.cases.iter().take(5) {
                            println!(
                                "      Case {}: target_offset={}",
                                case.value, case.target_offset
                            );
                        }
                        if switch_table.cases.len() > 5 {
                            println!("      ... and {} more cases", switch_table.cases.len() - 5);
                        }
                    }
                }
            }
        } else {
            println!("  ✗ Switch tables: none");
        }
    }

    // Test specific switch table retrieval
    println!("\n=== Switch Table Retrieval Tests ===");

    // Function 1 should have a switch table at instruction 1
    if let Some(switch_table) = hbc_file.switch_tables.get_switch_table_by_instruction(1, 1) {
        println!("✓ Found switch table for function 1, instruction 1");
        println!(
            "  Range: {} to {}",
            switch_table.min_value, switch_table.max_value
        );
        println!("  Cases: {}", switch_table.case_count());
    } else {
        println!("✗ No switch table found for function 1, instruction 1");
    }

    // Test non-existent switch table
    if hbc_file
        .switch_tables
        .get_switch_table_by_instruction(1, 99)
        .is_none()
    {
        println!("✓ Correctly returned None for non-existent switch table");
    } else {
        println!("✗ Unexpectedly found switch table for non-existent instruction");
    }

    println!("\n=== Demonstration Complete ===");
}
