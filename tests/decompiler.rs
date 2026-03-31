use hermes_dec_rs::{decompiler::Decompiler, hbc::HbcFile};
use std::fs;
use std::path::Path;

#[test]
fn test_decompiler_creation() {
    let _decompiler = Decompiler::new();
    // Test only that creation does not panic
}

#[test]
fn test_decompiler_with_empty_functions() {
    // This test will need to be updated once we have proper HBC file creation
    // For now, it just verifies the module compiles
    assert!(true);
}

#[test]
fn test_switch_with_fallthrough_decompiles() {
    let hbc_path = Path::new("data/complex_control_flow.hbc");
    let data = fs::read(hbc_path).expect("Failed to read HBC fixture");
    let hbc = HbcFile::parse(&data).expect("Failed to parse HBC fixture");

    let mut decompiler = Decompiler::new().expect("Failed to create decompiler");
    let output = decompiler
        .decompile_function(&hbc, 4)
        .expect("Failed to decompile switchWithFallthrough");

    assert!(
        output.contains("switch (param6)"),
        "expected switchWithFallthrough to decompile to a structured outer switch:\n{}",
        output
    );
    assert!(
        output.contains("case 1:"),
        "expected switchWithFallthrough to retain its first case:\n{}",
        output
    );
    assert!(
        output.contains("switch (var1_g)"),
        "expected switchWithFallthrough to retain its nested switch:\n{}",
        output
    );
    assert!(
        output.contains("return var0_b;"),
        "expected switchWithFallthrough to retain its shared-tail return:\n{}",
        output
    );
}
