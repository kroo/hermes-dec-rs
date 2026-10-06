use hermes_dec_rs::bundle::{export_bundle, BundleOptions};
use hermes_dec_rs::cfg::{ssa::construct_ssa, Cfg};
use hermes_dec_rs::{Decompiler, HbcFile};
use std::collections::{BTreeMap, BTreeSet};
use std::process::Command;

#[test]
fn test_complex_nested_conditionals() {
    let data = std::fs::read("data/complex_control_flow.hbc").unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let structured = Decompiler::new()
        .unwrap()
        .decompile_function(&hbc, 2)
        .unwrap();
    let bundle = export_bundle(&hbc, &BundleOptions::default()).unwrap();
    let source = std::fs::read_to_string("data/complex_control_flow.js").unwrap();
    let script = format!(
        r#"
const vm=require('node:vm'), assert=require('node:assert/strict');
const expected=vm.runInNewContext({source}+'\ncomplexNestedConditionals;');
const structured=vm.runInNewContext('('+{structured}+')');
const context=vm.createContext({{}}); vm.runInContext({bundle},context);
const complete=context.complexNestedConditionals;
const values=[-12,-6,-1,0,1,2,5];
for(const a of values) for(const b of values) for(const c of values) for(const d of values) {{
  const args=[a,b,c,d], result=expected(...args);
  assert.equal(structured(...args),result,JSON.stringify(args));
  assert.equal(complete(...args),result,JSON.stringify(args));
}}
"#,
        source = serde_json::to_string(&source).unwrap(),
        structured = serde_json::to_string(&structured).unwrap(),
        bundle = serde_json::to_string(&bundle).unwrap()
    );
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("complex.cjs");
    std::fs::write(&path, script).unwrap();
    let result = Command::new("node").arg(path).output().unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn test_ssa_variable_progression() {
    let data = std::fs::read("data/complex_control_flow.hbc").unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let mut cfg = Cfg::new(&hbc, 2);
    cfg.build();
    let analysis = construct_ssa(&cfg, 2).unwrap();
    let mut versions = BTreeMap::<u8, BTreeSet<u32>>::new();
    for (definition, value) in &analysis.ssa_values {
        assert_eq!(definition, &value.def_site);
        assert_eq!(definition.register, value.register);
        assert!(
            versions
                .entry(value.register)
                .or_default()
                .insert(value.version),
            "duplicate SSA version"
        );
    }
    assert!(versions.len() >= 4);
    assert!(versions.values().any(|versions| versions.len() > 1));
    for versions in versions.values() {
        assert!(*versions.first().unwrap() <= 1);
        let ordered: Vec<_> = versions.iter().copied().collect();
        assert!(ordered.windows(2).all(|pair| pair[1] - pair[0] <= 2));
    }
}
