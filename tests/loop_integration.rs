use assert_cmd::Command;
use hermes_dec_rs::{
    cfg::{analysis::LoopType, ssa::construct_ssa, Cfg},
    decompiler::Decompiler,
    hbc::HbcFile,
};
use regex::Regex;
use std::fs;
use std::io::Write;
use std::path::Path;
use tempfile::NamedTempFile;

fn analyze_loops(hbc_path: &Path, func_index: u32) -> Option<Vec<LoopType>> {
    let data = fs::read(hbc_path).ok()?;
    let hbc = HbcFile::parse(&data).ok()?;
    let mut cfg = Cfg::new(&hbc, func_index);
    cfg.build();
    let loop_analysis = cfg.analyze_loops();
    if loop_analysis.loops.is_empty() {
        return None;
    }
    Some(
        loop_analysis
            .loops
            .iter()
            .map(|l| l.loop_type.clone())
            .collect(),
    )
}

fn decompile(hbc_path: &Path, func_index: u32) -> Option<String> {
    let data = fs::read(hbc_path).ok()?;
    let hbc = HbcFile::parse(&data).ok()?;
    let mut decompiler = Decompiler::new().ok()?;
    decompiler.decompile_function(&hbc, func_index).ok()
}

fn analyze_ssa(
    hbc_path: &Path,
    func_index: u32,
) -> Option<(Cfg<'static>, hermes_dec_rs::cfg::ssa::SSAAnalysis)> {
    let data = fs::read(hbc_path).ok()?;
    let leaked_data: &'static [u8] = Box::leak(data.into_boxed_slice());
    let hbc = HbcFile::parse(leaked_data).ok()?;
    let leaked_hbc = Box::leak(Box::new(hbc));
    let mut cfg = Cfg::new(leaked_hbc, func_index);
    cfg.build();
    let ssa = construct_ssa(&cfg, func_index).ok()?;
    Some((cfg, ssa))
}

fn count_occurrences(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

#[test]
fn test_while_loop_round_trip() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/while_loop.hbc");
    let loops = analyze_loops(hbc_path, 1).unwrap();
    assert_eq!(loops, vec![LoopType::While]);
    let output = decompile(hbc_path, 1).unwrap();
    assert!(output.contains("let var0;"));
    assert!(output.contains("let var3;"));
    assert!(output.contains("while ("));
    assert!(output.contains("<"));
    assert!(!output.contains("do {"));
    assert!(!output.contains("const var0 ="));
    assert!(!output.contains("let var0 ="));
    assert_eq!(count_occurrences(&output, "while ("), 1);
    Ok(())
}

#[test]
fn test_while_loop_self_edge_header_gets_phi() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/while_loop.hbc");
    let (cfg, ssa) = analyze_ssa(hbc_path, 1).unwrap();
    let header = cfg.builder().get_block_at_pc(3).unwrap();

    let frontier = ssa
        .dominance_frontiers
        .get(&header)
        .expect("loop header should have a dominance frontier");
    assert!(
        frontier.contains(&header),
        "self-edge loop header should appear in its own frontier"
    );

    let header_phis = ssa
        .phi_functions
        .get(&header)
        .expect("loop header should receive a phi");
    assert!(
        header_phis.iter().any(|phi| phi.result.register == 3),
        "loop-carried register r3 should get a header phi"
    );

    Ok(())
}

#[test]
fn test_loop_type_detection() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/loop_types.hbc");
    let loops = analyze_loops(hbc_path, 1).unwrap();
    assert_eq!(
        loops,
        vec![
            LoopType::While,
            LoopType::While,
            LoopType::While,
            LoopType::ForIn,
            LoopType::ForOf,
        ]
    );

    Ok(())
}

#[test]
fn test_forin_decompiles_to_native_forin_loop() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/loop_types.hbc");
    let output = decompile(hbc_path, 1).unwrap();
    let forin_section = output
        .split("const param3 = arg1;")
        .next()
        .unwrap_or(&output);
    assert!(output.contains("const param5 = arg0;"));
    assert!(Regex::new(r"for \(const [A-Za-z0-9_]+ in (param5|arg0)\)")?.is_match(&output));
    assert!(output.contains(".call("));
    assert!(!output.contains("throw new Error(\"Unsupported for-in loop fallback\")"));
    assert!(!forin_section.contains("Object.keys("));
    assert!(!forin_section.contains("[Symbol.iterator]()"));

    Ok(())
}

#[test]
fn test_loop_types_use_initialized_loop_conditions() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/loop_types.hbc");
    let output = decompile(hbc_path, 1).unwrap();

    assert!(output.contains("let var0_a;"));
    assert!(output.contains("let var1_b;"));
    assert!(output.contains("let var2_d;"));
    assert_eq!(count_occurrences(&output, "let var0_a;"), 1);
    assert_eq!(count_occurrences(&output, "let var1_b;"), 1);
    assert_eq!(count_occurrences(&output, "let var2_d;"), 1);

    assert!(output.contains("while (var1 < var3)"));
    assert!(output.contains("while (var0_a < var5)"));
    assert!(output.contains("while (var2_d < var3)"));
    assert!(!output.contains("while (var0_a < var3)"));
    assert!(!output.contains("while (var1_b < var5)"));

    Ok(())
}

#[test]
fn test_try_in_loop_preserves_loop_shape() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/try_catch_test.hbc");
    let output = decompile(hbc_path, 4).unwrap();
    assert!(output.contains("do {"));
    assert!(output.contains("catch ("));
    assert!(output.contains("var6 = 0;"));
    assert!(!output.contains("switch (0)"));
    assert!(!output.contains("try {}"));
    assert!(!output.contains("while (var0 === var1)"));
    assert_eq!(count_occurrences(&output, "let var6;"), 1);
    assert_eq!(
        count_occurrences(&output, "const var10 = \"Loop error\";"),
        1
    );

    Ok(())
}

#[test]
fn test_exception_handling_control_flow_decompiles_without_overflow(
) -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/complex_control_flow.hbc");
    let output = decompile(hbc_path, 7).unwrap();
    assert!(output.contains("function exceptionHandlingControlFlow("));
    assert!(output.contains("switch (__hbc_pc)"));
    assert!(output.contains("catch (__hbc_caught)"));
    assert!(!output.contains("Unsupported exception-region loop reconstruction"));

    let optimized = Command::cargo_bin("hermes-dec-rs")?
        .args([
            "decompile",
            "data/complex_control_flow.hbc",
            "--function",
            "7",
            "--optimize-safe",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert_eq!(output.trim(), String::from_utf8(optimized)?.trim());

    let mut script = NamedTempFile::new()?;
    let original = fs::read_to_string("data/complex_control_flow.js")?;
    writeln!(script, "const reference = (() => {{\n{original}\nreturn exceptionHandlingControlFlow;\n}})();\n{output}")?;
    writeln!(
        script,
        r#"
const assert = require('node:assert/strict');
const vm = require('node:vm');
const cases = [
  [],
  [{{type: 'divide', a: 4, b: 2}}],
  [{{type: 'divide', a: 1, b: 0}}, {{type: 'divide', a: 9, b: 3}}],
  [{{type: 'divide', a: 1, b: 0, critical: true, cleanup: true}}, {{type: 'divide', a: 4, b: 2}}],
  [{{type: 'process', steps: 8}}],
  [{{type: 'process', steps: 4, fail_at_step_2: true, continue_on_error: true}}],
  [{{type: 'process', steps: 4, fail_at_step_2: true, continue_on_error: false}}],
  [{{type: 'unknown', cleanup: true}}, {{type: 'divide', a: 10, b: 2, cleanup: true}}],
  new Proxy([], {{get(target, key) {{return key === 'length' ? NaN : target[key];}}}})
];
for (const random of [0, 0.95]) {{
  Math.random = () => random;
  for (const operations of cases) {{
    const expected = reference(operations);
    const actual = vm.runInNewContext('exceptionHandlingControlFlow(operations)',
      {{exceptionHandlingControlFlow, operations}}, {{timeout: 1000}});
    assert.deepEqual(actual, expected);
  }}
}}
// An exception thrown inside a handler must propagate to its enclosing handler.
for (const fn of [reference, exceptionHandlingControlFlow]) {{
  assert.throws(() => fn(new Proxy([], {{get() {{throw new Error('outside protected range');}}}})),
    /outside protected range/);
  assert.throws(() => fn([null]), /critical/);
}}
"#
    )?;

    Command::new("node").arg(script.path()).assert().success();

    Ok(())
}

#[test]
fn test_forof_fallback_uses_iterator_next_exit() -> Result<(), Box<dyn std::error::Error>> {
    let hbc_path = Path::new("data/ast-04-tests/test_rest_params.hbc");
    let output = decompile(hbc_path, 1).unwrap();
    assert!(output.contains("const var2 = var3[Symbol.iterator]();"));
    assert!(output.contains("var2.next();"));
    assert!(Regex::new(r"if \([^)]*=== undefined\)")?.is_match(&output));
    assert!(output.contains("finally {"));
    assert!(output.contains("var2.return && var2.return();"));
    assert!(Regex::new(r"var0\s*=\s*[^;]+\s*\+\s*[^;]+;")?.is_match(&output));

    Ok(())
}
