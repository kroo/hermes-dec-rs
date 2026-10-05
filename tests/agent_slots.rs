use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::{cli::slots::run, HbcFile};
use serde_json::Value;
use std::path::Path;
use std::process::Command;

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

fn function_id(name: &str) -> u32 {
    let data = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    (0..hbc.functions.count())
        .find(|&id| hbc.functions.get_function_name(id, &hbc.strings).unwrap() == name)
        .unwrap()
}

// A subprocess captures stdout without global redirection or harness races.
#[test]
fn slots_emit_helper() {
    let Ok(args) = std::env::var("AGENT_SLOTS_ARGS") else {
        return;
    };
    let args: Vec<usize> = args.split(',').map(|s| s.parse().unwrap()).collect();
    let input = std::env::var_os("AGENT_SLOTS_INPUT").unwrap();
    run(
        Path::new(&input),
        args[0] as u32,
        args[1] as u32,
        args[2],
        args[3],
    )
    .unwrap();
}

fn report(input: &Path, function: u32, slot: u32, depth: usize, limit: usize) -> Value {
    let output = Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "slots_emit_helper", "--nocapture"])
        .env("AGENT_SLOTS_INPUT", input)
        .env(
            "AGENT_SLOTS_ARGS",
            format!("{function},{slot},{depth},{limit}"),
        )
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    let line = stdout.lines().find(|line| line.starts_with('{')).unwrap();
    assert!(line.len() < 2_000_000);
    serde_json::from_str(line).unwrap()
}

#[test]
fn reports_static_ancestors_with_pc_addressed_source_context() {
    let id = function_id("deeplyNested");
    let parent = function_id("anotherInner");
    let grandparent = function_id("outerFunction");
    let value = report(fixture(), id, 0, 2, 100);
    assert_eq!(value, report(fixture(), id, 0, 2, 100));
    assert_eq!(value["schema_version"], 1);
    assert_eq!(value["authoritative_lexical_resolution"], false);
    assert_eq!(value["truncated"], false);
    let candidates = value["candidates"].as_array().unwrap();
    assert!(!candidates.is_empty());
    assert!(candidates
        .iter()
        .any(|c| c["ancestor_function_id"] == parent));
    assert!(candidates
        .iter()
        .any(|c| c["ancestor_function_id"] == grandparent));
    for c in candidates {
        assert_eq!(c["kind"], "static_candidate");
        assert_eq!(c["slot"], 0);
        assert_ne!(c["ancestor_function_id"], id);
        let path = c["closure_path"].as_array().unwrap();
        assert_eq!(path[0]["child_function_id"], id);
        assert_eq!(
            path.last().unwrap()["parent_function_id"],
            c["ancestor_function_id"]
        );
        assert!(path.len() <= 2);
        assert!(c["excerpt"]["javascript"]
            .as_str()
            .unwrap()
            .contains(&format!(
                "// HBC function {}, PC {}\n",
                c["ancestor_function_id"], c["pc"]
            )));
        assert!(c["excerpt"]["javascript"]
            .as_str()
            .unwrap()
            .contains(".slots[0] ="));
        assert!(c["excerpt"]["instruction_count"].as_u64().unwrap() <= 17);
    }
    assert!(candidates.iter().any(|c| {
        let js = c["excerpt"]["javascript"].as_str().unwrap();
        js.contains("hello") || js.contains("toUpperCase")
    }));
}

#[test]
fn respects_depth_limit_and_empty_results() {
    let id = function_id("deeplyNested");
    let one = report(fixture(), id, 0, 1, 100);
    assert_eq!(one["ancestor_total"], 1);
    assert_eq!(one["depth_truncated"], true);
    let all = report(fixture(), id, 0, 8, 100);
    let limited = report(fixture(), id, 0, 8, 1);
    assert!(all["total"].as_u64().unwrap() > 1);
    assert_eq!(limited["total"], all["total"]);
    assert_eq!(limited["returned"], 1);
    assert_eq!(limited["truncated"], true);
    assert_eq!(limited["candidates"][0], all["candidates"][0]);
    let absent = report(fixture(), id, u32::MAX, 8, 100);
    assert_eq!(absent["total"], 0);
    assert_eq!(absent["candidates"], serde_json::json!([]));
    let root = report(fixture(), 0, 0, 8, 100);
    assert_eq!(root["ancestor_total"], 0);
    assert_eq!(root["total"], 0);
}

#[test]
fn rejects_invalid_ids_bounds_and_inputs_without_writes() {
    for (depth, limit) in [(0, 1), (9, 1), (1, 0), (1, 101), (usize::MAX, usize::MAX)] {
        assert!(run(fixture(), 0, 0, depth, limit).is_err());
    }
    assert!(run(fixture(), u32::MAX, 0, 1, 1)
        .unwrap_err()
        .to_string()
        .contains("Unknown function"));
    let temp = tempfile::tempdir().unwrap();
    let missing = temp.path().join("missing.hbc");
    assert!(run(&missing, 0, 0, 1, 1).is_err());
    let bad = temp.path().join("bad.hbc");
    std::fs::write(&bad, b"not bytecode").unwrap();
    assert!(run(&bad, 0, 0, 1, 1).is_err());
    assert_eq!(std::fs::read(&bad).unwrap(), b"not bytecode");
    assert_eq!(std::fs::read_dir(temp.path()).unwrap().count(), 1);
}

#[test]
fn generator_and_async_creators_are_parents_but_direct_calls_are_not() {
    let id = function_id("deeplyNested");
    let parent = function_id("anotherInner");
    let original = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&original).unwrap();
    let instruction = hbc.functions.get_instructions_ref(parent).unwrap().iter()
        .find(|i| matches!(i.instruction, UnifiedInstruction::CreateClosure { operand_2, .. } if u32::from(operand_2) == id))
        .unwrap();
    let header = &hbc.functions.parsed_headers[parent as usize];
    let body_offset = header
        .large_header
        .as_ref()
        .map_or(header.header.offset(), |h| h.offset);
    let offset = body_offset as usize + instruction.offset.value() as usize;
    let temp = tempfile::tempdir().unwrap();
    let input = temp.path().join("variant.hbc");
    for name in [
        "CreateGeneratorClosure",
        "CreateAsyncClosure",
        "CreateGenerator",
        "CallDirect",
    ] {
        let opcode = (0..=u8::MAX)
            .find(|&opcode| {
                let mut cursor = 0;
                let payload = &original[offset + 1..];
                UnifiedInstruction::parse(hbc.header.version(), opcode, payload, &mut cursor)
                    .is_ok_and(|(i, _)| {
                        i.name() == name && i.size() == instruction.instruction.size()
                    })
            })
            .expect("fixture version must support same-width closure and direct-call variants");
        let mut bytes = original.clone();
        bytes[offset] = opcode;
        std::fs::write(&input, &bytes).unwrap();
        let value = report(&input, id, 0, 1, 100);
        if name == "CallDirect" {
            assert_eq!(value["ancestor_total"], 0);
            assert_eq!(value["total"], 0);
        } else {
            assert_eq!(value["ancestor_total"], 1);
            assert!(value["total"].as_u64().unwrap() > 0);
            assert_eq!(value["closure_edges"][0]["kind"], name);
            assert_eq!(value["closure_edges"][0]["parent_function_id"], parent);
        }
        assert_eq!(std::fs::read(&input).unwrap(), bytes);
        assert_eq!(std::fs::read_dir(temp.path()).unwrap().count(), 1);
    }
}

#[test]
fn generator_body_reaches_stores_through_its_wrapper() {
    let input = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/bundle_semantics.hbc"
    ));
    let data = std::fs::read(input).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let (wrapper, body) = (0..hbc.functions.count())
        .find_map(|id| {
            hbc.functions
                .get_instructions_ref(id)
                .unwrap()
                .iter()
                .find_map(|ins| match ins.instruction {
                    UnifiedInstruction::CreateGenerator { operand_2, .. } => {
                        Some((id, u32::from(operand_2)))
                    }
                    _ => None,
                })
        })
        .unwrap();
    let value = report(input, body, 0, 8, 100);
    assert!(value["ancestor_total"].as_u64().unwrap() > 0);
    assert!(value["candidates"]
        .as_array()
        .unwrap()
        .iter()
        .any(|candidate| {
            candidate["closure_path"]
                .as_array()
                .unwrap()
                .iter()
                .any(|edge| {
                    edge["parent_function_id"] == wrapper && edge["kind"] == "CreateGenerator"
                })
        }));
}
