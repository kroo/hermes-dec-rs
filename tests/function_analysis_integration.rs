//! Integration tests for function analysis features

use hermes_dec_rs::decompiler::Decompiler;
use hermes_dec_rs::hbc::HbcFile;

#[test]
fn test_default_parameter_detection() {
    // Load a test HBC file that contains functions with default parameters
    let test_data = include_bytes!("../data/ast-04-tests/test_default_params.hbc");
    let hbc_file = HbcFile::parse(test_data).expect("Failed to parse HBC file");

    let mut decompiler = Decompiler::new().expect("Failed to create decompiler");
    let result = decompiler
        .decompile_function(&hbc_file, 1)
        .expect("Failed to decompile function");

    // Check that the decompiled output contains default parameter syntax
    assert!(
        result.contains("arg0 = "),
        "Expected default parameter in function signature"
    );
}

#[test]
fn test_constructor_detection() {
    // Load a test HBC file that contains constructor functions
    let test_data = include_bytes!("../data/ast-04-tests/test_constructor_simple.hbc");
    let hbc_file = HbcFile::parse(test_data).expect("Failed to parse HBC file");

    let mut decompiler = Decompiler::new().expect("Failed to create decompiler");
    let result = decompiler
        .decompile_function(&hbc_file, 1)
        .expect("Failed to decompile function");

    // Check that the constructor is properly identified
    assert!(
        result.contains("/*Constructor:"),
        "Expected constructor comment in: {}",
        result
    );
}

#[test]
fn test_method_classification() {
    // Load a test HBC file that contains methods
    let test_data = include_bytes!("../data/ast-04-tests/test_method_detection.hbc");
    let hbc_file = HbcFile::parse(test_data).expect("Failed to parse HBC file");

    let mut decompiler = Decompiler::new().expect("Failed to create decompiler");

    // Test method detection (function 3 is the object method)
    let result = decompiler
        .decompile_function(&hbc_file, 3)
        .expect("Failed to decompile method");

    assert!(
        result.contains("/*Method*/"),
        "Expected method classification comment in:\n{}",
        result
    );

    // Test prototype method detection (function 4)
    let prototype_result = decompiler
        .decompile_function(&hbc_file, 4)
        .expect("Failed to decompile prototype method");

    assert!(
        prototype_result.contains("/*Prototype method*/"),
        "Expected prototype method classification comment in:\n{}",
        prototype_result
    );
}
