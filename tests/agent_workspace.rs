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
    assert_eq!(
        manifest["file_count"],
        u64::from(hbc.functions.count()) * 2 + 4
    );
    assert_eq!(manifest["view_directory"], "view");
    assert_eq!(manifest["guide_path"], "GUIDE.md");
    assert!(manifest["js_bytes_description"]
        .as_str()
        .unwrap()
        .contains("excludes GUIDE.md"));
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
    let mut view_bytes = 0;
    let navigation = manifest["navigation"].as_array().unwrap();
    assert_eq!(navigation.len(), 10);
    for query in navigation {
        assert_eq!(query["command"], "hermes-dec-rs");
        assert_eq!(query["function"], "FUNCTION");
        assert_eq!(query["input"], "INPUT");
        assert!(query.get("shell").is_none());
    }
    assert_eq!(navigation[0]["subcommand"], "origins");
    assert_eq!(navigation[0]["pc"], "PC");
    assert_eq!(navigation[0]["flags"], serde_json::json!(["--expressions"]));
    assert_eq!(navigation[4]["subcommand"], "symbols");
    assert!(navigation[4].get("pc").is_none());
    assert_eq!(
        navigation[4]["flags"],
        serde_json::json!(["--slot", "SLOT"])
    );
    assert_eq!(navigation[5]["subcommand"], "origins");
    assert_eq!(navigation[5]["pc"], "PC");
    assert_eq!(navigation[5]["flags"], serde_json::json!(["--text"]));
    assert_eq!(navigation[6]["subcommand"], "properties");
    assert_eq!(navigation[7]["subcommand"], "objects");
    assert_eq!(navigation[8]["subcommand"], "read");
    assert_eq!(navigation[9]["subcommand"], "json-literals");
    assert!(navigation[7].get("pc").is_none());
    assert_eq!(
        navigation[7]["flags"],
        serde_json::json!(["--match", "TEXT"])
    );
    assert_eq!(
        navigation[6]["flags"],
        serde_json::json!(["--match", "KEY"])
    );
    for id in 0..hbc.functions.count() {
        let entry = &entries[id as usize];
        let row = &rows[id as usize];
        let path = format!("f{id}.js");
        let code = fs::read_to_string(output.join(&path)).unwrap();
        let (fragment_id, fragment) = &fragments[id as usize];
        assert_eq!(*fragment_id, id);
        let prefix = entry["fragment_prefix_bytes"].as_u64().unwrap() as usize;
        assert_eq!(&code.as_bytes()[prefix..], fragment.as_bytes());
        let header = &code[..prefix];
        assert!(header.starts_with("// Complete JS inspection fragment"));
        assert!(header.lines().all(|line| line.starts_with("// ")));
        assert!(header.contains(&format!("origins INPUT {id} PC --expressions")));
        assert!(header.contains(&format!("sites INPUT {id} --compact")));
        assert!(header.contains(&format!("captures INPUT {id}")));
        assert!(header.contains(&format!("sites INPUT {id} --kind slot-write --slot SLOT")));
        assert!(header.contains("GUIDE.md"));
        assert!(entry.get("navigation").is_none());
        // Join every byte range, including UTF-8, without assuming a fixed header length.
        for (start, ch) in fragment.char_indices() {
            let end = start + ch.len_utf8();
            assert_eq!(
                &code.as_bytes()[prefix + start..prefix + end],
                &fragment.as_bytes()[start..end]
            );
        }
        assert_eq!(entry["id"], id);
        assert_eq!(entry["path"], path);
        assert_eq!(
            entry["name"],
            hbc.functions.get_function_name(id, &hbc.strings).unwrap()
        );
        assert_eq!(entry["js_bytes"], code.len());
        let view_path = format!("view/f{id}.txt");
        assert_eq!(entry["view_path"], view_path);
        assert_eq!(row["view_path"], view_path);
        let view = fs::read_to_string(output.join(view_path)).unwrap();
        assert_eq!(entry["view_bytes"], view.len());
        assert!(view.contains("# literal_note_status:"));
        assert!(entry["literal_notes"].is_object());
        view_bytes += view.len();
        assert_eq!(row["id"], id);
        assert_eq!(row["name"], entry["name"]);
        assert_eq!(row["path"], entry["path"]);
        assert_eq!(row["fragment_prefix_bytes"], entry["fragment_prefix_bytes"]);
        assert!(row.get("navigation").is_none());
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
    assert_eq!(manifest["views_bytes"], view_bytes);
    let guide = fs::read_to_string(output.join("GUIDE.md")).unwrap();
    for topic in [
        "complete f<ID>.js",
        "UTF-8 byte",
        "fragment_prefix_bytes",
        "--expressions",
        "ordered call roles",
        "--match TEXT",
        "--from-pc PC",
        "captures INPUT ID",
        "candidates",
        "symbols INPUT ID --slot SLOT",
        "raw store-ordinal cursor",
        "scan_complete",
        "origins INPUT ID PC --text",
        "omits typed graph nodes",
        "objects INPUT ID --match TEXT",
        "object_queries",
        "Only plain register copies",
        "Do not execute",
    ] {
        assert!(guide.contains(topic), "missing guide topic: {topic}");
    }
    assert_eq!(fs::read_dir(&output).unwrap().count(), rows.len() + 5);
    assert_eq!(
        fs::read_dir(output.join("view")).unwrap().count(),
        rows.len()
    );
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
}

#[test]
fn raw_only_workspace_preserves_raw_fragments_without_views() {
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("raw");
    hermes_dec_rs::cli::workspace::workspace_with_views(fixture(), &output, false).unwrap();
    let manifest: Value =
        serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
    let count = manifest["function_count"].as_u64().unwrap();
    assert_eq!(manifest["file_count"], count + 4);
    assert_eq!(manifest["views_bytes"], 0);
    assert!(manifest.get("view_directory").is_none());
    assert!(!output.join("view").exists());
    assert_eq!(fs::read_dir(&output).unwrap().count() as u64, count + 4);
    for function in manifest["functions"].as_array().unwrap() {
        assert!(function.get("view_path").is_none());
        assert!(function.get("literal_notes").is_none());
    }
}

#[test]
fn wired_cli_reports_and_writes_the_selected_workspace_layout() {
    let temp = tempfile::tempdir().unwrap();
    for raw_only in [false, true] {
        let output = temp.path().join(if raw_only { "raw" } else { "views" });
        let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"));
        command
            .arg("workspace")
            .arg(fixture())
            .arg("-o")
            .arg(&output);
        if raw_only {
            command.arg("--raw-only");
        }
        let result = command.output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let response: Value = serde_json::from_slice(&result.stdout).unwrap();
        assert_eq!(
            response["views"],
            if raw_only {
                Value::Null
            } else {
                Value::from("view")
            }
        );
        assert_eq!(output.join("view").exists(), !raw_only);
        let manifest: Value =
            serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
        for function in manifest["functions"].as_array().unwrap() {
            if !raw_only {
                let text = fs::read_to_string(output.join(function["view_path"].as_str().unwrap()))
                    .unwrap();
                assert!(text.contains("@0"));
                assert!(text.contains("# literal_note_status:"));
                assert_eq!(text.len() as u64, function["view_bytes"].as_u64().unwrap());
            }
        }
    }
}

#[test]
fn navigation_never_interpolates_input_paths_or_function_names() {
    let temp = tempfile::tempdir().unwrap();
    let mut data = fs::read(fixture()).unwrap();
    let original = b"makeCounter";
    let hostile = b"$(touch x);";
    assert_eq!(original.len(), hostile.len());
    let offset = data
        .windows(original.len())
        .position(|s| s == original)
        .unwrap();
    data[offset..offset + original.len()].copy_from_slice(hostile);
    let input = temp.path().join("input ' $(touch injected);.hbc");
    fs::write(&input, &data).unwrap();
    let output = temp.path().join("output `touch injected`;");
    workspace(&input, &output).unwrap();
    let manifest: Value =
        serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
    let entries = manifest["functions"].as_array().unwrap();
    assert!(entries.iter().any(|entry| entry["name"] == "$(touch x);"));
    let guide = fs::read_to_string(output.join("GUIDE.md")).unwrap();
    for entry in entries {
        let code = fs::read(output.join(entry["path"].as_str().unwrap())).unwrap();
        let prefix = entry["fragment_prefix_bytes"].as_u64().unwrap() as usize;
        let header = std::str::from_utf8(&code[..prefix]).unwrap();
        let navigation = serde_json::to_string(&manifest["navigation"]).unwrap();
        for text in [header, &navigation, &guide] {
            assert!(!text.contains("$(touch x);"));
            assert!(!text.contains("injected"));
            assert!(!text.contains(temp.path().to_str().unwrap()));
        }
    }
    assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 2);
}

#[test]
fn origins_expression_spans_join_at_the_manifest_prefix() {
    use hermes_dec_rs::cli::origins;
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("workspace");
    workspace(fixture(), &output).unwrap();
    let data = fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let manifest: Value =
        serde_json::from_slice(&fs::read(output.join("manifest.json")).unwrap()).unwrap();
    let mut joined = 0;
    for (id, raw) in export_function_fragments(&hbc, &[0]).unwrap() {
        let pc = hbc
            .functions
            .get_instructions_ref(id)
            .unwrap()
            .last()
            .unwrap()
            .offset
            .0;
        let report: Value = serde_json::from_slice(
            &origins::report(
                fixture(),
                origins::Query {
                    function: id,
                    pc,
                    depth: 3,
                    limit: 64,
                    max_bytes: 100_000,
                    expressions: true,
                },
            )
            .unwrap(),
        )
        .unwrap();
        assert_eq!(report["expression_source"]["source_bytes"], raw.len());
        let entry = &manifest["functions"][id as usize];
        let prefix = entry["fragment_prefix_bytes"].as_u64().unwrap() as usize;
        let code = fs::read(output.join(entry["path"].as_str().unwrap())).unwrap();
        for source in report["instruction_expressions"]
            .as_array()
            .unwrap()
            .iter()
            .map(|view| &view["source"])
            .chain(
                report["definitions"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|definition| &definition["source"]),
            )
        {
            let start = source["start"].as_u64().unwrap() as usize;
            let end = source["end"].as_u64().unwrap() as usize;
            assert_eq!(
                &code[prefix + start..prefix + end],
                &raw.as_bytes()[start..end]
            );
            joined += 1;
        }
    }
    assert!(joined > 0);
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
