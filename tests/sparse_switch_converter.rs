use assert_cmd::Command;
use hermes_dec_rs::{decompiler::Decompiler, hbc::HbcFile};
use std::fs;
use std::path::Path;

fn decompile_fixture(hbc_path: &Path, func_index: u32) -> String {
    let data = fs::read(hbc_path).expect("Failed to read HBC fixture");
    let hbc = HbcFile::parse(&data).expect("Failed to parse HBC fixture");
    let mut decompiler = Decompiler::new().expect("Failed to create decompiler");
    decompiler
        .decompile_function(&hbc, func_index)
        .expect("Failed to decompile sparse switch fixture")
}

fn decompile_fixture_via_cli(hbc_path: &Path, func_index: u32) -> String {
    let output = Command::cargo_bin("hermes-dec-rs")
        .expect("Failed to resolve hermes-dec-rs binary")
        .args([
            "decompile",
            hbc_path
                .to_str()
                .expect("fixture path should be valid UTF-8"),
            "--function",
            &func_index.to_string(),
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    String::from_utf8(output).expect("CLI output should be valid UTF-8")
}

fn count_occurrences(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

#[test]
fn test_sparse_switch_fixture_decompiles_to_structured_switch() {
    let hbc_path = Path::new("data/dense_switch_test.hbc");
    let output = decompile_fixture(hbc_path, 2);

    assert!(
        output.contains("switch (param1)"),
        "expected sparse switch fixture to decompile as a structured switch:\n{}",
        output
    );

    for case in [
        "case 100:",
        "case 200:",
        "case 201:",
        "case 400:",
        "case 401:",
        "case 403:",
        "case 404:",
        "case 500:",
    ] {
        assert!(
            output.contains(case),
            "expected sparse switch case `{}` in decompiled output:\n{}",
            case,
            output
        );
    }

    assert_eq!(
        count_occurrences(&output, "case "),
        8,
        "expected exactly eight sparse switch cases:\n{}",
        output
    );
    assert_eq!(
        count_occurrences(&output, "default:"),
        1,
        "expected exactly one sparse switch default case:\n{}",
        output
    );
    assert_eq!(
        count_occurrences(&output, "switch ("),
        1,
        "expected sparse switch fixture to emit exactly one switch:\n{}",
        output
    );
}

#[test]
fn test_sparse_switch_cli_matches_library_output() {
    let hbc_path = Path::new("data/dense_switch_test.hbc");
    let library_output = decompile_fixture(hbc_path, 2);
    let cli_output = decompile_fixture_via_cli(hbc_path, 2);

    assert_eq!(
        cli_output.trim(),
        library_output.trim(),
        "CLI decompilation output diverged from the library path"
    );
}

#[test]
fn test_sparse_switch_cli_regression_command_is_stable() {
    let hbc_path = Path::new("data/dense_switch_test.hbc");
    let output = decompile_fixture_via_cli(hbc_path, 2);

    assert!(
        output.contains("function largeSwitchTest(arg0)"),
        "expected CLI decompile regression output for the sparse switch fixture:\n{}",
        output
    );
    assert!(
        output.contains("const var0_h = \"unknown\";"),
        "expected sparse switch default branch in CLI output:\n{}",
        output
    );
    assert!(
        !output.contains("if (param1"),
        "expected sparse switch fixture to avoid falling back to an if-chain:\n{}",
        output
    );
}
