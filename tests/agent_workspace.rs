use hermes_dec_rs::generated::unified_instructions::UnifiedInstruction;
use hermes_dec_rs::{bundle::export_function_fragments, cli::workspace::workspace, HbcFile};
use serde_json::Value;
use std::fs;
use std::path::Path;

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

#[test]
fn exports_every_complete_function_with_a_searchable_index() {
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("workspace");
    workspace(fixture(), &output).unwrap();
    let data = fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let ids: Vec<u32> = (0..hbc.functions.count()).collect();
    let fragments = export_function_fragments(&hbc, &ids).unwrap();
    let manifest: Value =
        serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
    assert_eq!(manifest["schema_version"], 1);
    assert_eq!(manifest["hbc_version"], hbc.header.version());
    assert_eq!(manifest["function_count"], hbc.functions.count());
    assert_eq!(manifest["file_count"], u64::from(hbc.functions.count()) + 3);
    assert_eq!(manifest["string_count"], hbc.strings.string_count);
    assert_eq!(manifest["standalone"], false);
    assert_eq!(manifest["power_loss_durable"], false);
    assert_eq!(manifest["runtime_path"], "runtime.js");
    assert!(manifest["runtime_helpers"]
        .as_str()
        .unwrap()
        .contains("not standalone"));
    let rows: Vec<Value> = fs::read_to_string(output.join("index.jsonl"))
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(rows.len(), hbc.functions.count() as usize);
    let entries = manifest["functions"].as_array().unwrap();
    assert_eq!(entries.len(), rows.len());
    let mut bytes = 0;
    for id in 0..hbc.functions.count() {
        let entry = &entries[id as usize];
        let row = &rows[id as usize];
        let path = format!("f{id}.js");
        let code = fs::read_to_string(output.join(&path)).unwrap();
        let (fragment_id, fragment) = &fragments[id as usize];
        assert_eq!(*fragment_id, id);
        let header = "// Complete JS inspection fragment, not a standalone program.\n// F = function bodies; M = metadata; r = registers; env = captured environment.\n// self = this; args = arguments. Runtime helpers and referenced F entries\n// are defined by export-bundle, not by this individual file.\n";
        assert_eq!(code, format!("{header}{fragment}"));
        assert_eq!(entry["id"], id);
        assert_eq!(entry["path"], path);
        assert_eq!(
            entry["name"],
            hbc.functions.get_function_name(id, &hbc.strings).unwrap()
        );
        assert_eq!(entry["js_bytes"], code.len());
        assert_eq!(row["id"], id);
        assert_eq!(row["name"], entry["name"]);
        assert_eq!(row["path"], entry["path"]);
        assert!(row["snippets"].as_array().unwrap().len() <= 32);
        assert!(row["static_assignments"].as_array().unwrap().len() <= 12);
        assert!(row["snippets"].as_array().unwrap().iter().all(|s| s
            .as_str()
            .unwrap()
            .chars()
            .count()
            <= 160));
        bytes += code.len();
    }
    assert!(rows
        .iter()
        .any(|row| !row["snippets"].as_array().unwrap().is_empty()));
    assert!(rows.iter().any(|row| row["static_assignments"]
        .as_array()
        .unwrap()
        .iter()
        .any(|site| site["name"] == "makeCounter")));
    let runtime = fs::read_to_string(output.join("runtime.js")).unwrap();
    assert!(runtime.starts_with("// Inspection-only runtime helper source, not a runnable app."));
    assert!(runtime.ends_with(include_str!("../src/bundle/runtime.js")));
    assert_eq!(manifest["runtime_bytes"], runtime.len());
    assert_eq!(manifest["js_bytes"], bytes + runtime.len());
    assert_eq!(fs::read_dir(&output).unwrap().count(), rows.len() + 3);
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
}

#[test]
fn refuses_existing_directories_and_files_without_changing_them() {
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("workspace");
    fs::create_dir(&output).unwrap();
    assert!(workspace(fixture(), &output).is_err());
    assert_eq!(fs::read_dir(&output).unwrap().count(), 0);
    fs::write(output.join("keep"), "untouched").unwrap();
    assert!(workspace(fixture(), &output).is_err());
    assert_eq!(
        fs::read_to_string(output.join("keep")).unwrap(),
        "untouched"
    );
    let file = temp.path().join("file");
    fs::write(&file, "untouched").unwrap();
    assert!(workspace(fixture(), &file).is_err());
    assert_eq!(fs::read_to_string(file).unwrap(), "untouched");
}

#[test]
fn malformed_or_missing_input_leaves_no_output_or_staging() {
    let temp = tempfile::tempdir().unwrap();
    let input = temp.path().join("bad.hbc");
    let output = temp.path().join("workspace");
    assert!(workspace(&input, &output).is_err());
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 0);
    fs::write(&input, b"not bytecode").unwrap();
    assert!(workspace(&input, &output).is_err());
    assert!(!output.exists());
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
}

#[test]
fn unsupported_later_function_refuses_the_whole_workspace() {
    let temp = tempfile::tempdir().unwrap();
    let mut data = fs::read(fixture()).unwrap();
    let (offset, opcode) = {
        let hbc = HbcFile::parse(&data).unwrap();
        let opcode = (0..=u8::MAX)
            .find(|&opcode| {
                let mut offset = 0;
                let mut operands = [0; 64];
                operands[1] = 53;
                matches!(
                    UnifiedInstruction::parse(hbc.header.version(), opcode, &operands, &mut offset),
                    Ok((
                        UnifiedInstruction::GetBuiltinClosure {
                            operand_0: 0,
                            operand_1: 53
                        },
                        _
                    ))
                )
            })
            .unwrap();
        let (id, instruction) = (1..hbc.functions.count())
            .rev()
            .find_map(|id| {
                hbc.functions
                    .get_instructions_ref(id)
                    .unwrap()
                    .iter()
                    .find(|ins| ins.instruction.size() == 3)
                    .map(|ins| (id, ins))
            })
            .expect("small fixture must contain a later three-byte instruction");
        let header = &hbc.functions.parsed_headers[id as usize];
        let body_offset = header
            .large_header
            .as_ref()
            .map_or(header.header.offset(), |h| h.offset);
        (body_offset as usize + instruction.offset.0 as usize, opcode)
    };
    // Preserve instruction width and boundaries, but introduce an unsupported builtin.
    data[offset..offset + 3].copy_from_slice(&[opcode, 0, 53]);
    let hbc = HbcFile::parse(&data).unwrap();
    assert!(export_function_fragments(&hbc, &[0]).is_ok());
    let input = temp.path().join("unsupported.hbc");
    fs::write(&input, &data).unwrap();
    let output = temp.path().join("workspace");
    let error = workspace(&input, &output).unwrap_err().to_string();
    assert!(error.contains("Unsupported builtin 53"), "{error}");
    assert!(!output.exists());
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
}

#[cfg(unix)]
#[test]
fn refuses_dangling_output_symlinks() {
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("workspace");
    std::os::unix::fs::symlink(temp.path().join("missing"), &output).unwrap();
    assert!(workspace(fixture(), &output).is_err());
    assert!(fs::symlink_metadata(&output)
        .unwrap()
        .file_type()
        .is_symlink());
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
}
