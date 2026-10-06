use hermes_dec_rs::bundle::{export_bundle, BundleOptions};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::HbcFile;
use std::path::Path;
use std::process::Command;

fn bundle(fixture: &str, minify: bool) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("data")
        .join(fixture);
    let bytes = std::fs::read(path).unwrap();
    let hbc = HbcFile::parse(&bytes).unwrap();
    export_bundle(
        &hbc,
        &BundleOptions {
            minify,
            ..Default::default()
        },
    )
    .unwrap()
}

fn compare(fixture: &str, probe: &str) {
    compare_compiled(fixture, &format!("{fixture}.hbc"), probe);
}

fn compare_compiled(fixture: &str, bytecode: &str, probe: &str) {
    let output = bundle(bytecode, false);
    let compact = bundle(bytecode, true);
    let source = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("data")
            .join(format!("{fixture}.js")),
    )
    .unwrap();
    let script = format!(
        r#"
const vm = require('node:vm');
const assert = require('node:assert/strict');
async function run(code) {{
  const logs = [];
  const context = vm.createContext({{setTimeout, clearTimeout, console: {{log: (...xs) => logs.push(xs)}}}});
  vm.runInContext(code, context, {{timeout: 10000}});
  const value = await vm.runInContext({probe}, context, {{timeout: 10000}});
  return JSON.stringify({{value, logs}});
}}
(async () => {{
  const expected = await run({source});
  assert.equal(await run({output}), expected);
  assert.equal(await run({compact}), expected);
}})().catch(error => {{ console.error(error); process.exitCode = 1; }});
"#,
        probe = serde_json::to_string(probe).unwrap(),
        output = serde_json::to_string(&output).unwrap(),
        compact = serde_json::to_string(&compact).unwrap(),
        source = serde_json::to_string(&source).unwrap()
    );
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("compare.cjs");
    std::fs::write(&path, script).unwrap();
    let result = Command::new("node")
        .arg(&path)
        .output()
        .expect("Node.js is required for bundle behavior tests");
    assert!(
        result.status.success(),
        "{fixture}: {}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn closures_share_captured_slots_but_not_independent_invocations() {
    compare(
        "closure_capture_test",
        r#"(() => {
      const a = makeCounter(), b = makeCounter();
      const counts = [a.increment(), a.increment(), b.get(), a.decrement()];
      const outer = outerFunction(2);
      const x = outer.inner(3);
      const nested = outer.another(4);
      return [counts, x, nested('!'), outer.getOuter(), outer.getNum()];
    })()"#,
    );
}

#[test]
fn exceptions_and_finally_preserve_results_and_errors() {
    compare(
        "finally_test",
        r#"(() => {
      let error;
      try { finallyWithException(); } catch(e) { error = e.message; }
      return [justFinally(), tryCatchFinally(), error];
    })()"#,
    );
}

#[test]
fn generators_and_dense_switches_execute_as_complete_bundles() {
    compare(
        "flow_control",
        "[generatorTest(0), generatorTest(8), whileTest(5), switchTest('a'), forLoopTest([1,2,3])]",
    );
    compare("closure_test", "[outerFunction().inner(), outerFunction().another(), generatorOuter().next().value().next()]");
}

#[test]
fn export_all_small_corpus_functions_without_placeholders() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("data");
    for entry in std::fs::read_dir(root).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|extension| extension == "hbc")
            && path.file_name().unwrap() != "massive_literals.hbc"
        {
            let bytes = std::fs::read(&path).unwrap();
            let hbc = HbcFile::parse(&bytes).unwrap();
            let result = export_bundle(&hbc, &BundleOptions::default());
            assert!(result.is_ok(), "{}: {:?}", path.display(), result.err());
            let code = result.unwrap();
            assert_eq!(
                code.matches(" = function function_").count(),
                hbc.functions.count() as usize
            );
        }
    }
}

#[test]
fn unsupported_semantics_and_invalid_targets_fail_closed() {
    let bytes = include_bytes!("../data/closure_capture_test.hbc");
    for (instruction, expected, version) in [
        (
            UnifiedInstruction::DebuggerCheckBreak {},
            "Unsupported opcode DebuggerCheckBreak",
            96,
        ),
        (
            UnifiedInstruction::JmpLong {
                operand_0: i32::MAX,
            },
            "Invalid instruction target",
            96,
        ),
        (
            UnifiedInstruction::GetBuiltinClosure {
                operand_0: 0,
                operand_1: 53,
            },
            "Unsupported builtin 53",
            96,
        ),
        (
            UnifiedInstruction::GetBuiltinClosure {
                operand_0: 0,
                operand_1: 0,
            },
            "Unsupported builtin 0 for HBC 91",
            91,
        ),
    ] {
        let mut hbc = HbcFile::parse(bytes).unwrap();
        hbc.header.version = version;
        let instructions = hbc.functions.parsed_headers[0]
            .cached_instructions
            .get_mut()
            .unwrap()
            .as_mut()
            .unwrap();
        instructions[0].instruction = instruction;
        let failure = export_bundle(&hbc, &BundleOptions::default())
            .unwrap_err()
            .to_string();
        assert!(failure.contains(expected), "{failure}");
    }
}

#[test]
fn iterator_generator_constructor_and_literal_semantics() {
    compare("bundle_semantics", "bundleResults");
    compare_compiled(
        "bundle_semantics",
        "bundle_semantics_v90.hbc",
        "bundleResults",
    );
}

#[test]
fn hermes_specific_arguments_and_array_iterator_fast_path() {
    for fixture in [
        "bundle_hermes_semantics.hbc",
        "bundle_hermes_semantics_v90.hbc",
    ] {
        let code = bundle(fixture, false);
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("hermes.cjs");
        let expected = if fixture.contains("v90") {
            include_str!("../data/bundle_hermes_semantics_v90.expected.json")
        } else {
            include_str!("../data/bundle_hermes_semantics.expected.json")
        }
        .trim();
        std::fs::write(&path, format!("const vm=require('node:vm'); const assert=require('node:assert/strict'); let output; vm.runInNewContext({}, {{print:value=>output=value}}); assert.equal(output, {});", serde_json::to_string(&code).unwrap(), serde_json::to_string(expected).unwrap())).unwrap();
        let result = Command::new("node").arg(path).output().unwrap();
        assert!(
            result.status.success(),
            "{fixture}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
}

#[test]
fn async_closures_resume_in_their_captured_environment() {
    compare(
        "closure_test",
        "(async () => { const inner = await asyncOuter(); return await inner(); })()",
    );
}

#[test]
fn private_runtime_does_not_consult_replaced_collection_intrinsics() {
    compare("bundle_intrinsics", "intrinsicResults");
}

#[test]
fn direct_eval_matches_hermes_isolated_scope_not_the_exporter_scope() {
    let code = bundle("bundle_eval.hbc", false);
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("eval.cjs");
    // Expected values captured from HBC 96 Hermes, not Node source execution.
    let script = format!("const vm = require('node:vm'); const assert = require('node:assert/strict'); const c = vm.createContext({{}}); vm.runInContext({}, c); assert.equal(vm.runInContext('JSON.stringify(evalResults)', c), '[\"undefined\",\"object\",0,null,true,4,\"undefined\",7,\"undefined\"]');", serde_json::to_string(&code).unwrap());
    std::fs::write(&path, script).unwrap();
    let result = Command::new("node").arg(path).output().unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn full_bundle_export_is_deterministic_and_minification_is_valid() {
    let plain = bundle("closure_capture_test.hbc", false);
    assert_eq!(plain, bundle("closure_capture_test.hbc", false));
    assert!(plain.contains("function function_10("));
    let compact = bundle("closure_capture_test.hbc", true);
    assert!(compact.len() < plain.len());
    let path = tempfile::NamedTempFile::with_suffix(".js").unwrap();
    std::fs::write(path.path(), compact).unwrap();
    assert!(Command::new("node")
        .arg("--check")
        .arg(path.path())
        .status()
        .unwrap()
        .success());
}

#[test]
fn cli_exports_the_entire_bundle() {
    let temp = tempfile::tempdir().unwrap();
    let path = temp.path().join("bundle.js");
    assert_cmd::Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args(["export-bundle", "data/closure_capture_test.hbc", "-o"])
        .arg(&path)
        .assert()
        .success();
    assert_eq!(
        std::fs::read_to_string(path).unwrap(),
        bundle("closure_capture_test.hbc", false)
    );
}

#[test]
fn commonjs_dependencies_cycles_and_cache_keep_module_identity() {
    let code = bundle("bundle_modules.hbc", false);
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("cjs.cjs");
    std::fs::write(&path, format!("const vm=require('node:vm'); const assert=require('node:assert/strict'); const result=vm.runInNewContext({}); assert.equal(JSON.stringify(result.result), '[42,6,true,true]');", serde_json::to_string(&code).unwrap())).unwrap();
    let result = Command::new("node").arg(path).output().unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn cli_contract_rejects_inapplicable_flags_and_checks_requested_version() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("bundle.js");
    std::fs::write(&path, "previous output").unwrap();
    for extra in ["--optimize-safe", "--skip-validation"] {
        assert_cmd::Command::cargo_bin("hermes-dec-rs")
            .unwrap()
            .args(["decompile", "data/closure_capture_test.hbc", extra, "-o"])
            .arg(&path)
            .assert()
            .failure();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "previous output");
    }
    assert_cmd::Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args([
            "export-bundle",
            "data/bundle_modules.hbc",
            "--entry-module",
            "missing.js",
            "-o",
        ])
        .arg(&path)
        .assert()
        .failure();
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "previous output");
    assert_cmd::Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args([
            "decompile",
            "data/closure_capture_test.hbc",
            "--hbc-version",
            "90",
        ])
        .assert()
        .failure();
    assert_cmd::Command::cargo_bin("hermes-dec-rs")
        .unwrap()
        .args([
            "decompile",
            "data/closure_capture_test.hbc",
            "--minify",
            "-o",
        ])
        .arg(&path)
        .assert()
        .success();
    assert!(
        std::fs::read_to_string(&path).unwrap().len()
            < bundle("closure_capture_test.hbc", false).len()
    );
}
