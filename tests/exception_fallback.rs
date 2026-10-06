use hermes_dec_rs::decompiler::Decompiler;
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::HbcFile;
use std::process::Command;

fn compare_inequality_variant(long: bool, strict: bool) {
    let bytes = include_bytes!("../data/complex_control_flow.hbc");
    let mut hbc = HbcFile::parse(bytes).unwrap();
    let instructions = hbc.functions.parsed_headers[7]
        .cached_instructions
        .get_mut()
        .unwrap()
        .as_mut()
        .unwrap();
    let instruction = instructions
        .iter_mut()
        .find(|i| matches!(i.instruction, UnifiedInstruction::JStrictNotEqual { .. }))
        .unwrap();
    let UnifiedInstruction::JStrictNotEqual {
        operand_0,
        operand_1,
        operand_2,
    } = instruction.instruction
    else {
        unreachable!()
    };
    // Keep fixture offsets/targets intact while exercising all decoded variants.
    // These operands compare numeric loop indices, where != and !== agree.
    instruction.instruction = match (long, strict) {
        (false, false) => UnifiedInstruction::JNotEqual {
            operand_0,
            operand_1,
            operand_2,
        },
        (false, true) => UnifiedInstruction::JStrictNotEqual {
            operand_0,
            operand_1,
            operand_2,
        },
        (true, false) => UnifiedInstruction::JNotEqualLong {
            operand_0: i32::from(operand_0),
            operand_1,
            operand_2,
        },
        (true, true) => UnifiedInstruction::JStrictNotEqualLong {
            operand_0: i32::from(operand_0),
            operand_1,
            operand_2,
        },
    };
    let output = Decompiler::new()
        .unwrap()
        .decompile_function(&hbc, 7)
        .unwrap();
    assert!(output.contains("switch (__hbc_pc)"));
    let original = include_str!("../data/complex_control_flow.js");
    let script = format!(
        r#"
const assert = require('node:assert/strict');
const vm = require('node:vm');
const reference = (() => {{ {original}; return exceptionHandlingControlFlow; }})();
{output}
const cases = [
  [],
  [{{type:'process', steps:8}}],
  [{{type:'process', steps:4, fail_at_step_2:true, continue_on_error:true}}],
  [{{type:'process', steps:3, fail_at_step_2:true, continue_on_error:false}}],
  [{{type:'process', steps:3, fail_at_step_2:true, continue_on_error:false, critical:true, cleanup:true}}, {{type:'process', steps:1}}],
  [{{type:'divide', a:1, b:0, cleanup:true}}, {{type:'divide', a:9, b:3}}],
  new Proxy([], {{get(target,key) {{return key === 'length' ? NaN : target[key];}}}})
];
for (const random of [0, 0.95]) {{
  Math.random = () => random;
  for (const operations of cases) {{
    const expected = reference(operations);
    const actual = vm.runInNewContext('exceptionHandlingControlFlow(operations)',
      {{exceptionHandlingControlFlow, operations}}, {{timeout:1000}});
    assert.deepEqual(actual, expected);
  }}
}}
"#
    );
    let result = Command::new("node")
        .args(["-e", &script])
        .output()
        .expect("Node.js is required for exception fallback regressions");
    assert!(
        result.status.success(),
        "long={long}, strict={strict}: {}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn loose_inequality_short_preserves_step_error_branch() {
    compare_inequality_variant(false, false);
}

#[test]
fn loose_inequality_long_preserves_step_error_branch() {
    compare_inequality_variant(true, false);
}

#[test]
fn strict_inequality_short_preserves_step_error_branch() {
    compare_inequality_variant(false, true);
}

#[test]
fn strict_inequality_long_preserves_step_error_branch() {
    compare_inequality_variant(true, true);
}
