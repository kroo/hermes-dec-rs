use hermes_dec_rs::cli::bindings::collect;
use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction as I;
use hermes_dec_rs::hbc::function_table::{
    DebugOffsets, DebugOffsetsLegacy, ExceptionHandlerInfo, HbcFunctionInstruction,
    ParsedFunctionHeader, SmallFunctionHeader,
};
use hermes_dec_rs::hbc::*;
use std::sync::OnceLock;

// Authored empty HBC header; instruction streams below are synthetic, not bundle data.
const EMPTY_HBC: [u8; 256] = {
    let mut bytes = [0; 256];
    let magic = HBC_MAGIC.to_le_bytes();
    let mut i = 0;
    while i < 8 {
        bytes[i] = magic[i];
        i += 1;
    }
    bytes[8] = 96;
    bytes[32] = 0;
    bytes[33] = 1;
    bytes
};

fn fixture(code: Vec<I>) -> HbcFile<'static> {
    let mut hbc = HbcFile::parse(&EMPTY_HBC).unwrap();
    hbc.strings.string_count = 3;
    hbc.strings.string_cache = vec![
        Some("namespace".into()),
        Some("assigned".into()),
        Some("\u{1f980} with \"quotes\"".into()),
    ];
    let mut pc = 0;
    let instructions = code
        .into_iter()
        .enumerate()
        .map(|(index, instruction)| {
            let offset = pc;
            pc += instruction.size() as u32;
            HbcFunctionInstruction {
                offset: InstructionOffset(offset),
                function_index: 0,
                instruction_index: InstructionIndex(index),
                instruction,
            }
        })
        .collect::<Vec<_>>();
    hbc.functions.count = 2;
    for index in 0..2 {
        let cached_instructions = OnceLock::new();
        cached_instructions
            .set(Ok(if index == 0 {
                instructions.clone()
            } else {
                vec![]
            }))
            .unwrap();
        hbc.functions.parsed_headers.push(ParsedFunctionHeader {
            index,
            header: SmallFunctionHeader {
                word_1: 0,
                word_2: 0,
                word_3: 0,
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

fn closure() -> I {
    I::CreateClosure {
        operand_0: 1,
        operand_1: 9,
        operand_2: 1,
    }
}

fn put() -> I {
    I::PutById {
        operand_0: 0,
        operand_1: 1,
        operand_2: 0,
        operand_3: 1,
    }
}

#[test]
fn direct_assignment_tracks_global_paths_and_full_width_aliases() {
    let hbc = fixture(vec![
        I::GetGlobalObject { operand_0: 0 },
        I::GetByIdShort {
            operand_0: 0,
            operand_1: 0,
            operand_2: 0,
            operand_3: 0,
        },
        I::MovLong {
            operand_0: 70000,
            operand_1: 0,
        },
        closure(),
        I::MovLong {
            operand_0: 65537,
            operand_1: 1,
        },
        I::LoadConstUndefined { operand_0: 1 },
        I::MovLong {
            operand_0: 2,
            operand_1: 65537,
        },
        I::MovLong {
            operand_0: 0,
            operand_1: 70000,
        },
        I::PutByIdLong {
            operand_0: 0,
            operand_1: 2,
            operand_2: 0,
            operand_3: 1,
        },
    ]);
    let found = collect(&hbc).unwrap();
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].function_id, 1);
    assert_eq!(found[0].source_function_id, 0);
    assert_eq!(found[0].name, "assigned");
    assert_eq!(found[0].target, "globalThis.namespace.assigned");
    assert_eq!(
        found[0].pc,
        hbc.functions.get_instructions_ref(0).unwrap()[8].offset.0
    );
}

#[test]
fn long_move_clobbers_high_register_without_truncating() {
    let hbc = fixture(vec![
        closure(),
        I::MovLong {
            operand_0: 65537,
            operand_1: 1,
        },
        I::MovLong {
            operand_0: 65537,
            operand_1: 99999,
        },
        I::MovLong {
            operand_0: 1,
            operand_1: 65537,
        },
        put(),
    ]);
    assert!(collect(&hbc).unwrap().is_empty());
}

#[test]
fn all_closure_and_property_write_families() {
    let closures = vec![
        closure(),
        I::CreateClosureLongIndex {
            operand_0: 1,
            operand_1: 9,
            operand_2: 1,
        },
        I::CreateGeneratorClosure {
            operand_0: 1,
            operand_1: 9,
            operand_2: 1,
        },
        I::CreateGeneratorClosureLongIndex {
            operand_0: 1,
            operand_1: 9,
            operand_2: 1,
        },
        I::CreateAsyncClosure {
            operand_0: 1,
            operand_1: 9,
            operand_2: 1,
        },
        I::CreateAsyncClosureLongIndex {
            operand_0: 1,
            operand_1: 9,
            operand_2: 1,
        },
    ];
    let writes = vec![
        put(),
        I::PutByIdLong {
            operand_0: 0,
            operand_1: 1,
            operand_2: 0,
            operand_3: 1,
        },
        I::TryPutById {
            operand_0: 0,
            operand_1: 1,
            operand_2: 0,
            operand_3: 1,
        },
        I::TryPutByIdLong {
            operand_0: 0,
            operand_1: 1,
            operand_2: 0,
            operand_3: 1,
        },
        I::PutNewOwnById {
            operand_0: 0,
            operand_1: 1,
            operand_2: 1,
        },
        I::PutNewOwnByIdShort {
            operand_0: 0,
            operand_1: 1,
            operand_2: 1,
        },
        I::PutNewOwnByIdLong {
            operand_0: 0,
            operand_1: 1,
            operand_2: 1,
        },
        I::PutNewOwnNEById {
            operand_0: 0,
            operand_1: 1,
            operand_2: 1,
        },
        I::PutNewOwnNEByIdLong {
            operand_0: 0,
            operand_1: 1,
            operand_2: 1,
        },
    ];
    for creation in closures {
        for write in &writes {
            let found = collect(&fixture(vec![creation.clone(), write.clone()])).unwrap();
            assert_eq!(found.len(), 1, "{} -> {}", creation.name(), write.name());
            assert_eq!(found[0].name, "assigned");
            assert!(found[0].target.is_empty());
        }
    }
}

#[test]
fn unknown_calls_and_other_writes_do_not_name_returned_functions() {
    let clobbers = vec![
        I::Call1 {
            operand_0: 1,
            operand_1: 7,
            operand_2: 8,
        },
        I::Construct {
            operand_0: 1,
            operand_1: 7,
            operand_2: 0,
        },
        I::GetById {
            operand_0: 1,
            operand_1: 7,
            operand_2: 0,
            operand_3: 0,
        },
        I::LoadConstUndefined { operand_0: 1 },
        I::Add {
            operand_0: 1,
            operand_1: 7,
            operand_2: 8,
        },
        I::DelById {
            operand_0: 1,
            operand_1: 7,
            operand_2: 0,
        },
        I::GetPNameList {
            operand_0: 7,
            operand_1: 8,
            operand_2: 1,
            operand_3: 9,
        },
    ];
    for clobber in clobbers {
        assert!(
            collect(&fixture(vec![closure(), clobber.clone(), put()]))
                .unwrap()
                .is_empty(),
            "{}",
            clobber.name()
        );
    }
    assert!(collect(&fixture(vec![
        I::Call1 {
            operand_0: 1,
            operand_1: 7,
            operand_2: 8
        },
        put()
    ]))
    .unwrap()
    .is_empty());
}

#[test]
fn jumps_returns_throws_and_generator_boundaries_clear_state() {
    let boundaries = vec![
        I::Jmp { operand_0: 2 },
        I::JmpTrue {
            operand_0: 3,
            operand_1: 8,
        },
        I::Ret { operand_0: 8 },
        I::Throw { operand_0: 8 },
        I::SaveGeneratorLong { operand_0: 5 },
        I::SaveGenerator { operand_0: 2 },
        I::ResumeGenerator {
            operand_0: 7,
            operand_1: 8,
        },
        I::CompleteGenerator {},
    ];
    for boundary in boundaries {
        assert!(
            collect(&fixture(vec![closure(), boundary.clone(), put()]))
                .unwrap()
                .is_empty(),
            "{}",
            boundary.name()
        );
    }
}

#[test]
fn branch_targets_reset_even_before_backwards_branch_is_seen() {
    let write_pc = closure().size() as i32;
    let branch_pc = write_pc + put().size() as i32;
    let hbc = fixture(vec![
        closure(),
        put(),
        I::JmpLong {
            operand_0: write_pc - branch_pc,
        },
    ]);
    assert!(collect(&hbc).unwrap().is_empty());
}

#[test]
fn forward_join_and_exception_targets_reset_fallthrough_state() {
    let branch_size = I::JmpLong { operand_0: 0 }.size() as i32;
    let hbc = fixture(vec![
        I::JmpLong {
            operand_0: branch_size + closure().size() as i32,
        },
        closure(),
        put(),
    ]);
    assert!(collect(&hbc).unwrap().is_empty());
    let mut hbc = fixture(vec![closure(), put()]);
    hbc.functions.parsed_headers[0]
        .exc_handlers
        .push(ExceptionHandlerInfo {
            start: 0,
            end: closure().size() as u32,
            target: closure().size() as u32,
        });
    assert!(collect(&hbc).unwrap().is_empty());
}

#[test]
fn unicode_property_names_are_preserved_and_paths_are_escaped() {
    let hbc = fixture(vec![
        I::GetGlobalObject { operand_0: 0 },
        closure(),
        I::PutNewOwnById {
            operand_0: 0,
            operand_1: 1,
            operand_2: 2,
        },
    ]);
    let found = collect(&hbc).unwrap();
    assert_eq!(found[0].name, "\u{1f980} with \"quotes\"");
    assert_eq!(
        found[0].target,
        format!(
            "globalThis[{}]",
            serde_json::to_string(&found[0].name).unwrap()
        )
    );
}

#[test]
fn long_unicode_chains_stop_tracking_paths_but_keep_assignment_names() {
    let mut code = vec![I::GetGlobalObject { operand_0: 0 }];
    for _ in 0..200 {
        code.push(I::GetByIdLong {
            operand_0: 0,
            operand_1: 0,
            operand_2: 0,
            operand_3: 2,
        });
    }
    code.extend([closure(), put()]);
    let found = collect(&fixture(code)).unwrap();
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].name, "assigned");
    assert!(found[0].target.is_empty());
}

#[test]
fn switch_case_target_resets_a_region_before_the_switch() {
    let switch = I::SwitchImm {
        operand_0: 8,
        operand_1: 0,
        operand_2: 0,
        operand_3: 0,
        operand_4: 0,
    };
    let case_pc = closure().size() as i32;
    let switch_pc = case_pc + put().size() as i32;
    let mut hbc = fixture(vec![closure(), put(), switch]);
    let mut table = switch_table::SwitchTable::new(0, 0, 0, 0, 0, 2, 0);
    table.add_case(0, case_pc - switch_pc);
    hbc.switch_tables.add_switch_table(table);
    assert!(collect(&hbc).unwrap().is_empty());
}

#[test]
fn undecoded_switch_targets_disable_propagation_conservatively() {
    let hbc = fixture(vec![
        closure(),
        put(),
        I::SwitchImm {
            operand_0: 8,
            operand_1: 0,
            operand_2: 0,
            operand_3: 0,
            operand_4: 0,
        },
    ]);
    assert!(collect(&hbc).unwrap().is_empty());
}

#[test]
fn property_receiver_clobber_drops_only_its_syntactic_target() {
    let hbc = fixture(vec![
        I::GetGlobalObject { operand_0: 0 },
        I::LoadConstUndefined { operand_0: 0 },
        closure(),
        put(),
    ]);
    let found = collect(&hbc).unwrap();
    assert_eq!(found.len(), 1);
    assert!(found[0].target.is_empty());
}

#[test]
fn floating_point_loads_clobber_only_their_destination_without_integer_conversion() {
    for number in [1.5, f64::INFINITY, f64::NAN] {
        let hbc = fixture(vec![
            closure(),
            I::LoadConstDouble {
                operand_0: 1,
                operand_1: number,
            },
            put(),
        ]);
        assert!(collect(&hbc).unwrap().is_empty());
        let hbc = fixture(vec![
            closure(),
            I::LoadConstDouble {
                operand_0: 7,
                operand_1: number,
            },
            put(),
        ]);
        assert_eq!(collect(&hbc).unwrap().len(), 1);
    }
}

#[test]
fn instruction_decode_errors_are_not_suppressed() {
    let mut hbc = fixture(vec![]);
    let cache = OnceLock::new();
    cache
        .set(Err(hermes_dec_rs::DecompilerError::Parse {
            offset: 0,
            message: "synthetic truncated instruction".into(),
        }))
        .unwrap();
    hbc.functions.parsed_headers[0].cached_instructions = cache;
    assert!(collect(&hbc).is_err());
}

#[test]
fn path_size_bounds_keep_full_names_without_splitting_unicode_or_escapes() {
    for property in [
        "a".repeat(1013),
        "a".repeat(1014),
        "\u{1f980}".repeat(254),
        "\"".repeat(510),
    ] {
        let mut hbc = fixture(vec![
            I::GetGlobalObject { operand_0: 0 },
            closure(),
            I::PutNewOwnById {
                operand_0: 0,
                operand_1: 1,
                operand_2: 2,
            },
        ]);
        hbc.strings.string_cache[2] = Some(property.clone());
        let found = collect(&hbc).unwrap();
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].name, property);
        if property == "a".repeat(1013) {
            assert_eq!(found[0].target.len(), 1024);
        } else {
            assert!(found[0].target.is_empty());
        }
    }
}

#[test]
fn switch_default_target_resets_fallthrough_region() {
    let switch = I::SwitchImm {
        operand_0: 8,
        operand_1: 0,
        operand_2: 0,
        operand_3: 0,
        operand_4: 0,
    };
    let switch_size = switch.size() as u32;
    let delta = switch_size + closure().size() as u32;
    let mut hbc = fixture(vec![
        I::SwitchImm {
            operand_0: 8,
            operand_1: 0,
            operand_2: delta as i32,
            operand_3: 0,
            operand_4: 0,
        },
        closure(),
        put(),
    ]);
    hbc.switch_tables
        .add_switch_table(switch_table::SwitchTable::new(
            0,
            0,
            delta as i32,
            0,
            0,
            0,
            0,
        ));
    assert!(collect(&hbc).unwrap().is_empty());
}
