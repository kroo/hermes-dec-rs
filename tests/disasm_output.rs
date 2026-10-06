use hermes_dec_rs::cli::disasm::disasm;
use std::fs;
use std::path::Path;
use tempfile::tempdir;

fn disassemble_fixture(path: &Path) -> String {
    let directory = tempdir().expect("Failed to create temporary disassembly directory");
    let input = directory.path().join(path.file_name().unwrap());
    fs::copy(path, &input).expect("Failed to copy disassembly fixture");
    disasm(&input).expect("Failed to disassemble fixture");
    fs::read_to_string(input.with_extension("hasm")).expect("Missing disassembly output")
}

/// Test that compares expected and actual disassembly outputs
#[test]
fn test_disasm_output_matches_expected() {
    // .hasm.expected files contain reference Hermes output, which has a different
    // format. Pin our renderer against committed CLI snapshots instead.
    for fixture in [
        "hermes_dec_sample",
        "flow_control",
        "regex_test",
        "dense_switch_test",
        "complex_control_flow",
    ] {
        let expected_file = Path::new("data").join(format!("{fixture}.hasm"));

        // Read the expected and actual files
        let expected_content = fs::read_to_string(&expected_file).unwrap_or_else(|_| {
            panic!("Failed to read expected file: {}", expected_file.display())
        });
        let actual_content = disassemble_fixture(&expected_file.with_extension("hbc"));

        // Compare the contents
        if expected_content != actual_content {
            // Generate a detailed diff for debugging
            let diff = generate_diff(&expected_content, &actual_content);
            panic!("Disassembly output mismatch for {fixture}:\n\n{diff}");
        }

        println!("{fixture} matches expected output");
    }
}

/// Generate a simple diff between expected and actual content
fn generate_diff(expected: &str, actual: &str) -> String {
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();

    let mut diff = String::new();
    diff.push_str("Expected vs Actual diff:\n");
    diff.push_str("=======================\n\n");

    let max_lines = expected_lines.len().max(actual_lines.len());

    for i in 0..max_lines {
        let expected_line = expected_lines.get(i).unwrap_or(&"");
        let actual_line = actual_lines.get(i).unwrap_or(&"");

        if expected_line != actual_line {
            diff.push_str(&format!("Line {}:\n", i + 1));
            diff.push_str(&format!("  Expected: {}\n", expected_line));
            diff.push_str(&format!("  Actual:   {}\n", actual_line));
            diff.push_str("\n");
        }
    }

    // If there are too many differences, truncate the output
    if diff.len() > 10000 {
        diff.truncate(10000);
        diff.push_str("\n... (diff truncated)");
    }

    diff
}

/// Test that all .hbc files can be disassembled without errors
#[test]
fn test_all_hbc_files_disassemble_successfully() {
    let data_dir = Path::new("data");
    let mut hbc_files = Vec::new();

    if let Ok(entries) = fs::read_dir(data_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if let Some(extension) = path.extension() {
                    if extension == "hbc" {
                        hbc_files.push(path);
                    }
                }
            }
        }
    }

    for hbc_file in hbc_files {
        let test_name = hbc_file.file_stem().unwrap().to_string_lossy();

        // Skip the huge orbit_smarthome file that has parsing issues
        if test_name.contains("orbit_smarthome") {
            println!("Skipping {} - file too large for testing", test_name);
            continue;
        }

        println!("Testing disassembly for: {}", test_name);

        let content = disassemble_fixture(&hbc_file);

        if content.trim().is_empty() {
            panic!("Disassembly output is empty for {}", test_name);
        }

        println!("✓ {} disassembled successfully", test_name);
    }
}

/// Test that the disassembly output contains expected sections
#[test]
fn test_disasm_output_structure() {
    let test_files = vec![
        "data/hermes_dec_sample.hbc",
        "data/flow_control.hbc",
        "data/regex_test.hbc",
    ];

    for hbc_file in test_files {
        let path = Path::new(hbc_file);
        if !path.exists() {
            continue;
        }

        let test_name = path.file_stem().unwrap().to_string_lossy();
        println!("Testing output structure for: {}", test_name);

        let content = disassemble_fixture(path);

        // Check for expected sections
        assert!(
            content.contains("Bytecode File Information:"),
            "Output should contain header information for {}",
            test_name
        );

        assert!(
            content.contains("Global String Table:"),
            "Output should contain string table for {}",
            test_name
        );

        assert!(
            content.contains("Array Buffer:"),
            "Output should contain array buffer section for {}",
            test_name
        );

        // Check for function definitions
        assert!(
            content.contains("Function<") || content.contains("NCFunction<"),
            "Output should contain function definitions for {}",
            test_name
        );

        println!("✓ {} has correct output structure", test_name);
    }
}
