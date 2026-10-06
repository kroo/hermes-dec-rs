use hermes_dec_rs::bundle::{export_bundle, BundleOptions};
use hermes_dec_rs::HbcFile;
use std::process::Command;

// Verified by native Hermes against the committed, authored HBC 96 fixture.
const EXPECTED: &str = r#"{"results":[23,[4,5,"number",3,"number"],[4,5,"undefined",2,"undefined"],["undefined",0,"undefined"],8,false,17,false,21,true,"alpha;beta;",0,0,2,1,1,false],"failure":""}"#;

#[test]
fn numeric_prototypes_cannot_observe_private_vm_state() {
    let bytes = include_bytes!("../data/bundle_private_state.hbc");
    let hbc = HbcFile::parse(bytes).unwrap();
    assert_eq!(hbc.header.version(), 96);
    let source = include_str!("../data/bundle_private_state.js");
    for minify in [false, true] {
        let output = export_bundle(
            &hbc,
            &BundleOptions {
                minify,
                ..Default::default()
            },
        )
        .unwrap();
        let script = format!(
            r#"
const vm = require('node:vm');
const assert = require('node:assert/strict');
function run(code) {{
  let output;
  const context = vm.createContext({{print: value => output = value}});
  vm.runInContext(code, context, {{timeout: 5000}});
  assert.equal(vm.runInContext("Object.hasOwn(Array.prototype, '0')", context), false);
  assert.equal(vm.runInContext('typeof Object.create', context), 'function');
  return output;
}}
assert.equal(run({source}), {expected});
assert.equal(run({output}), {expected});
"#,
            source = serde_json::to_string(source).unwrap(),
            output = serde_json::to_string(&output).unwrap(),
            expected = serde_json::to_string(EXPECTED).unwrap(),
        );
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("private-state.cjs");
        std::fs::write(&path, script).unwrap();
        let result = Command::new("node")
            .arg(path)
            .output()
            .expect("Node.js is required for bundle behavior tests");
        assert!(
            result.status.success(),
            "minify={minify}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
}
