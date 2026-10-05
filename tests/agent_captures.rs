use hermes_dec_rs::cli::captures::{report, report_hbc, run};
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction as I;
use hermes_dec_rs::hbc::function_table::{
    DebugOffsets, DebugOffsetsLegacy, HbcFunctionInstruction, ParsedFunctionHeader,
    SmallFunctionHeader,
};
use hermes_dec_rs::hbc::{InstructionIndex, InstructionOffset, HBC_MAGIC};
use hermes_dec_rs::HbcFile;
use serde_json::Value;
use std::{path::Path, process::Command, sync::OnceLock};

const EMPTY: [u8; 256] = {
    let mut bytes = [0; 256];
    let magic = HBC_MAGIC.to_le_bytes();
    let mut i = 0;
    while i < 8 {
        bytes[i] = magic[i];
        i += 1;
    }
    bytes[8] = 96;
    bytes[33] = 1;
    bytes
};

fn synthetic(functions: Vec<Vec<I>>) -> HbcFile<'static> {
    let mut hbc = HbcFile::parse(&EMPTY).unwrap();
    hbc.functions.count = functions.len() as u32;
    hbc.strings.string_count = 1;
    hbc.strings.string_cache = vec![Some("authored".into())];
    hbc.strings.storage = b"authored";
    hbc.strings.small_entries = vec![
        hermes_dec_rs::hbc::tables::string_table::SmallStringTableEntry {
            is_utf16: false,
            is_identifier: Some(false),
            offset: 0,
            length: 8,
        },
    ];
    hbc.strings.string_kinds = vec![hermes_dec_rs::hbc::tables::string_table::StringKind::String];
    for (id, code) in functions.into_iter().enumerate() {
        let mut pc = 0;
        let instructions = code
            .into_iter()
            .enumerate()
            .map(|(index, instruction)| {
                let offset = pc;
                pc += instruction.size() as u32;
                HbcFunctionInstruction {
                    offset: InstructionOffset(offset),
                    function_index: id as u32,
                    instruction_index: InstructionIndex(index),
                    instruction,
                }
            })
            .collect::<Vec<_>>();
        let cached_instructions = OnceLock::new();
        cached_instructions.set(Ok(instructions)).unwrap();
        hbc.functions.parsed_headers.push(ParsedFunctionHeader {
            index: id as u32,
            header: SmallFunctionHeader {
                word_1: 0,
                word_2: pc,
                word_3: 16 << 25,
                word_4: 0,
            },
            large_header: None,
            exc_handlers: vec![],
            debug_offsets: DebugOffsets::Legacy(DebugOffsetsLegacy {
                source_locations: 0,
                scope_desc_data: 0,
            }),
            body: &[],
            version: 96,
            cached_instructions,
        });
    }
    hbc
}

fn read(slot: u8) -> I {
    I::LoadFromEnvironment {
        operand_0: 0,
        operand_1: 1,
        operand_2: slot,
    }
}
fn store(slot: u8, env: u8) -> I {
    I::StoreToEnvironment {
        operand_0: env,
        operand_1: slot,
        operand_2: 0,
    }
}
fn closure(child: u16) -> I {
    I::CreateClosure {
        operand_0: 0,
        operand_1: 1,
        operand_2: child,
    }
}
fn ret() -> I {
    I::Ret { operand_0: 0 }
}
fn value(hbc: &HbcFile<'_>, functions: &[u32], depth: usize, limit: usize, offset: usize) -> Value {
    serde_json::from_slice(&report_hbc(hbc, functions, depth, limit, offset, 16_777_216).unwrap())
        .unwrap()
}

#[test]
fn repeated_reads_mixed_registers_and_long_forms_are_candidates_not_bindings() {
    let hbc = synthetic(vec![vec![
        store(7, 1),
        read(7),
        store(7, 2),
        read(7),
        I::LoadFromEnvironmentL {
            operand_0: 0,
            operand_1: 3,
            operand_2: 300,
        },
        I::StoreToEnvironmentL {
            operand_0: 3,
            operand_1: 300,
            operand_2: 0,
        },
        I::StoreNPToEnvironmentL {
            operand_0: 4,
            operand_1: 300,
            operand_2: 0,
        },
        I::StoreNPToEnvironment {
            operand_0: 2,
            operand_1: 7,
            operand_2: 0,
        },
        ret(),
    ]]);
    let result = value(&hbc, &[0, 0], 1, 100, 0);
    assert_eq!(result["total"], 3);
    let rows = result["reads"].as_array().unwrap();
    assert_eq!(rows[0]["candidate_total"], 3);
    assert_eq!(rows[2]["candidate_total"], 2);
    assert_eq!(rows[0]["candidates"][0]["relative_pc"], "before_read_pc");
    assert_eq!(rows[0]["candidates"][1]["relative_pc"], "after_read_pc");
    assert_eq!(rows[0]["candidates"][1]["env_register_matches_read"], false);
    assert_eq!(rows[2]["read"]["env_register"], 3);
    assert!(rows
        .iter()
        .all(|r| r["unresolved"] == true && r["missing"] == false));
    for row in rows {
        assert!(row["excerpt"]["javascript"]
            .as_str()
            .unwrap()
            .contains("r["));
        for instruction in row["excerpt"]["instructions"].as_array().unwrap() {
            assert!(instruction.get("javascript").is_none());
            let start = instruction["excerpt_span"]["start"].as_u64().unwrap() as usize;
            let end = instruction["excerpt_span"]["end"].as_u64().unwrap() as usize;
            let javascript = row["excerpt"]["javascript"].as_str().unwrap();
            assert!(end - start <= 256);
            assert!(javascript.is_char_boundary(start) && javascript.is_char_boundary(end));
            assert!(javascript[start..end]
                .starts_with(&format!("// HBC function 0, PC {}\n", instruction["pc"])));
        }
        assert!(row["excerpt"]["instruction_count"].as_u64().unwrap() <= 7);
    }
}

#[test]
fn shortest_witness_alternatives_cycles_generators_and_direct_calls() {
    let hbc = synthetic(vec![
        vec![store(1, 1), closure(1), closure(2), ret()],
        vec![
            store(1, 1),
            I::CreateGeneratorLongIndex {
                operand_0: 0,
                operand_1: 1,
                operand_2: 3,
            },
            ret(),
        ],
        vec![
            store(1, 2),
            I::CreateGeneratorClosure {
                operand_0: 0,
                operand_1: 1,
                operand_2: 3,
            },
            ret(),
        ],
        vec![read(1), closure(0), ret()],
        vec![
            store(1, 1),
            I::CallDirect {
                operand_0: 0,
                operand_1: 1,
                operand_2: 3,
            },
            ret(),
        ],
    ]);
    let one = value(&hbc, &[3], 1, 10, 0);
    assert_eq!(one["reads"][0]["depth_truncated"], true);
    let all = value(&hbc, &[3], 8, 10, 0);
    let row = &all["reads"][0];
    assert_eq!(row["ancestor_total"], 3);
    assert_eq!(row["candidate_total"], 3);
    assert_eq!(row["depth_truncated"], false);
    assert_eq!(row["candidates"][0]["write"]["function_id"], 1);
    assert_eq!(row["candidates"][1]["write"]["function_id"], 2);
    assert_eq!(row["candidates"][2]["write"]["function_id"], 0);
    assert_eq!(
        row["candidates"][2]["closure_path"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    assert_eq!(row["closure_edges_total"], 5);
    assert!(row["candidates"]
        .as_array()
        .unwrap()
        .iter()
        .all(|c| c["write"]["function_id"] != 4));
}

#[test]
fn cap_prefers_same_function_then_nearest_ancestor_over_distant_low_ids() {
    let mut distant = vec![store(1, 1); 25];
    distant.extend([closure(1), ret()]);
    let mut nearest = vec![store(1, 2); 25];
    nearest.extend([closure(2), ret()]);
    let hbc = synthetic(vec![
        distant,
        nearest,
        vec![store(1, 1), read(1), store(1, 3), ret()],
    ]);
    let result = value(&hbc, &[2], 8, 10, 0);
    let row = &result["reads"][0];
    assert_eq!(row["candidate_total"], 52);
    assert_eq!(row["candidate_returned"], 20);
    assert_eq!(row["candidates_truncated"], true);
    assert_eq!(row["missing"], false);
    assert_eq!(row["unresolved"], true);
    let candidates = row["candidates"].as_array().unwrap();
    assert_eq!(candidates[0]["write"]["function_id"], 2);
    assert_eq!(candidates[1]["write"]["function_id"], 2);
    assert!(candidates[2..]
        .iter()
        .all(|c| c["write"]["function_id"] == 1));
    assert!(candidates[2..]
        .windows(2)
        .all(|pair| pair[0]["write"]["pc"].as_u64() < pair[1]["write"]["pc"].as_u64()));
    assert!(result["candidate_ranking"]
        .as_str()
        .unwrap()
        .contains("navigation only, not runtime likelihood"));
}

#[test]
fn branch_writes_are_not_order_resolved_and_caps_preserve_totals() {
    let mut code = vec![
        read(1),
        I::JmpTrue {
            operand_0: 7,
            operand_1: 0,
        },
    ];
    code.extend((0..30).map(|_| store(1, 2)));
    code.extend((0..110).map(|_| closure(1)));
    code.push(ret());
    let hbc = synthetic(vec![code, vec![read(1), read(2), ret()]]);
    let result = value(&hbc, &[1, 0], 1, 100, 0);
    assert_eq!(result["total"], 3);
    let row = &result["reads"][1];
    assert_eq!(row["candidate_total"], 30);
    assert_eq!(row["candidate_returned"], 20);
    assert_eq!(row["candidates_truncated"], true);
    assert_eq!(row["closure_edges_total"], 110);
    assert_eq!(row["closure_edges"].as_array().unwrap().len(), 100);
    assert_eq!(row["closure_edges_truncated"], true);
    assert_eq!(result["reads"][2]["missing"], true);
    for offset in 0..3 {
        let page = value(&hbc, &[0, 1], 1, 1, offset);
        assert_eq!(page["reads"][0], result["reads"][offset]);
        assert_eq!(
            page["next_offset"],
            if offset < 2 {
                Value::from(offset + 1)
            } else {
                Value::Null
            }
        );
    }
    assert_eq!(
        value(&hbc, &[0], 1, 1, usize::MAX)["reads"],
        serde_json::json!([])
    );
}

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

#[test]
fn authored_fixture_report_is_stable_and_validates_before_output() {
    let bytes = std::fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&bytes).unwrap();
    let ids: Vec<_> = (0..hbc.functions.count()).collect();
    let first = report(fixture(), &ids, 8, 100, 0, 16_777_216).unwrap();
    assert_eq!(
        first,
        report(fixture(), &ids, 8, 100, 0, first.len()).unwrap()
    );
    assert!(report(fixture(), &ids, 8, 100, 0, first.len() - 1).is_err());
    assert_eq!(std::fs::read(fixture()).unwrap(), bytes);
    for (depth, limit, budget) in [
        (0, 1, 1000),
        (9, 1, 1000),
        (1, 0, 1000),
        (1, 1001, 1000),
        (1, 1, 0),
        (1, 1, 16_777_217),
    ] {
        assert!(report(fixture(), &[0], depth, limit, 0, budget).is_err());
    }
    assert!(report(fixture(), &[], 1, 1, 0, 1000).is_err());
    assert!(report(fixture(), &[u32::MAX], 1, 1, 0, 1000)
        .unwrap_err()
        .to_string()
        .contains("Unknown function"));
}

#[test]
fn authored_generator_body_navigates_through_wrapper() {
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
                .find_map(|ins| {
                    if let I::CreateGenerator { operand_2, .. } = ins.instruction {
                        Some((id, u32::from(operand_2)))
                    } else {
                        None
                    }
                })
        })
        .unwrap();
    let result = value(&hbc, &[body], 8, 100, 0);
    assert!(result["total"].as_u64().unwrap() > 0);
    assert!(result["reads"].as_array().unwrap().iter().all(|row| {
        row["closure_edges"].as_array().unwrap().iter().any(|edge| {
            edge["parent_function_id"] == wrapper
                && edge["child_function_id"] == body
                && edge["kind"] == "CreateGenerator"
        })
    }));
}

#[test]
fn malformed_input_is_not_modified() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("invalid.hbc");
    std::fs::write(&input, b"authored invalid header").unwrap();
    assert!(report(&input, &[0], 1, 1, 0, 1000).is_err());
    assert_eq!(std::fs::read(&input).unwrap(), b"authored invalid header");
}

#[test]
fn quiet_stdout_helper() {
    let Ok(mode) = std::env::var("AGENT_CAPTURES_ERROR") else {
        return;
    };
    println!("BEGIN_CAPTURE");
    let result = match mode.as_str() {
        "budget" => run(fixture(), &[0], 1, 1, 0, 1),
        "unknown" => run(fixture(), &[u32::MAX], 1, 1, 0, 1000),
        "missing" => run(
            Path::new("/definitely-not-an-authored-capture.hbc"),
            &[0],
            1,
            1,
            0,
            1000,
        ),
        _ => run(fixture(), &[0], 0, 1, 0, 1000),
    };
    assert!(result.is_err());
    println!("END_CAPTURE");
}

#[test]
fn failures_leave_stdout_quiet() {
    for mode in ["budget", "unknown", "missing", "bounds"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "quiet_stdout_helper", "--nocapture"])
            .env("AGENT_CAPTURES_ERROR", mode)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let text = String::from_utf8(output.stdout).unwrap();
        assert!(text.contains("BEGIN_CAPTURE\nEND_CAPTURE"), "{text}");
    }
}
