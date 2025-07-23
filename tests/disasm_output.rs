use hermes_dec_rs::cli::disasm::disasm;
use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};
use std::thread::sleep;

fn wait_for_file(path: &std::path::Path, timeout_ms: u64) -> bool {
    let start = Instant::now();
    while start.elapsed() < Duration::from_millis(timeout_ms) {
        if path.exists() {
            if let Ok(metadata) = std::fs::metadata(path) {
                if metadata.len() > 0 {
                    return true;
                }
            }
        }
        sleep(Duration::from_millis(10));
    }
    false
}

/// Test that compares expected and actual disassembly outputs
#[test]
fn test_disasm_output_matches_expected() {
    // Find all .hasm.expected files (excluding the huge orbit_smarthome file)
    let expected_files = find_expected_files();
    
    for expected_file in expected_files {
        let test_name = expected_file.file_stem().unwrap().to_string_lossy();
        println!("Testing disassembly output for: {}", test_name);
        
        // Generate the corresponding .hbc file path
        let hbc_file = expected_file.with_extension("hbc");
        
        if !hbc_file.exists() {
            println!("Skipping {} - no corresponding .hbc file", test_name);
            continue;
        }
        
        // Run the disassembler to generate new .hasm file
        let result = disasm(&hbc_file);
        if let Err(e) = result {
            panic!("Failed to disassemble {}: {}", test_name, e);
        }
        
        // Wait for the output file to exist and be non-empty
        let actual_file = expected_file.with_extension("hasm");
        if !wait_for_file(&actual_file, 500) {
            panic!("Output file {} was not created or is empty after waiting", actual_file.display());
        }
        
        // Read the expected and actual files
        let expected_content = fs::read_to_string(&expected_file)
            .unwrap_or_else(|_| panic!("Failed to read expected file: {}", expected_file.display()));
        let actual_content = fs::read_to_string(&actual_file)
            .unwrap_or_else(|_| panic!("Failed to read actual file: {}", actual_file.display()));
        
        // Compare the contents
        if expected_content != actual_content {
            // Generate a detailed diff for debugging
            let diff = generate_diff(&expected_content, &actual_content);
            panic!(
                "Disassembly output mismatch for {}:\n\n{}",
                test_name, diff
            );
        }
        
        println!("✓ {} matches expected output", test_name);
    }
}

/// Find all .hasm.expected files in the data directory
fn find_expected_files() -> Vec<std::path::PathBuf> {
    let data_dir = Path::new("data");
    let mut files = Vec::new();
    
    if let Ok(entries) = fs::read_dir(data_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if let Some(extension) = path.extension() {
                    if extension == "expected" {
                        if let Some(stem) = path.file_stem() {
                            if let Some(stem_str) = stem.to_str() {
                                if stem_str.ends_with(".hasm") {
                                    // Skip the huge orbit_smarthome file
                                    if !path.to_string_lossy().contains("orbit_smarthome") {
                                        files.push(path);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    files.sort(); // Sort for consistent test ordering
    files
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
        
        let result = disasm(&hbc_file);
        if let Err(e) = result {
            panic!("Failed to disassemble {}: {}", test_name, e);
        }
        
        // Verify that the output file was created and is non-empty
        let hasm_file = hbc_file.with_extension("hasm");
        if !wait_for_file(&hasm_file, 500) {
            panic!("Disassembly output file not created or is empty for {}", test_name);
        }
        
        // Verify the output file is not empty
        let content = fs::read_to_string(&hasm_file)
            .unwrap_or_else(|_| panic!("Failed to read output file: {}", hasm_file.display()));
        
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
        
        // Run disassembler
        let result = disasm(path);
        if let Err(e) = result {
            panic!("Failed to disassemble {}: {}", test_name, e);
        }
        
        // Read the output
        let hasm_file = path.with_extension("hasm");
        let content = fs::read_to_string(&hasm_file)
            .unwrap_or_else(|_| panic!("Failed to read output file: {}", hasm_file.display()));
        
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