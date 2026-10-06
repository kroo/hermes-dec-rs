use hermes_dec_rs::cli::workspace::{
    workspace_with_evidence, workspace_with_views, EvidenceDesign,
};
use serde_json::Value;
use std::{fs, path::Path, process::Command};

fn fixture() -> &'static Path {
    Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/data/closure_capture_test.hbc"
    ))
}

fn manifest(path: &Path) -> Value {
    serde_json::from_slice(&fs::read(path.join("manifest.json")).unwrap()).unwrap()
}

#[test]
fn each_design_preserves_all_raw_and_compact_bytes_and_reports_exact_file_counts() {
    let temp = tempfile::tempdir().unwrap();
    let baseline = temp.path().join("v19");
    workspace_with_views(fixture(), &baseline, true).unwrap();
    let old = manifest(&baseline);
    for (name, design, expected) in [
        ("none", EvidenceDesign::None, vec![]),
        ("links", EvidenceDesign::Links, vec!["links.jsonl"]),
        (
            "initializers",
            EvidenceDesign::Initializers,
            vec!["initializers.jsonl"],
        ),
        (
            "all",
            EvidenceDesign::All,
            vec!["links.jsonl", "initializers.jsonl"],
        ),
    ] {
        let output = temp.path().join(name);
        workspace_with_evidence(fixture(), &output, true, design).unwrap();
        let current = manifest(&output);
        let count = current["function_count"].as_u64().unwrap();
        assert_eq!(current["js_bytes"], old["js_bytes"]);
        assert_eq!(current["views_bytes"], old["views_bytes"]);
        assert_eq!(current["file_count"], count * 2 + 4 + expected.len() as u64);
        for entry in current["functions"].as_array().unwrap() {
            for key in ["path", "view_path"] {
                let file = entry[key].as_str().unwrap();
                assert_eq!(
                    fs::read(output.join(file)).unwrap(),
                    fs::read(baseline.join(file)).unwrap()
                );
            }
        }
        for file in ["runtime.js", "index.jsonl"] {
            assert_eq!(
                fs::read(output.join(file)).unwrap(),
                fs::read(baseline.join(file)).unwrap()
            );
        }
        let summaries = current["source_evidence"]
            .as_array()
            .cloned()
            .unwrap_or_default();
        assert_eq!(summaries.len(), expected.len());
        for (summary, path) in summaries.iter().zip(expected.iter()) {
            assert_eq!(summary["path"], *path);
            let bytes = fs::read(output.join(path)).unwrap();
            assert_eq!(summary["bytes"], bytes.len());
            let rows: Vec<Value> = std::str::from_utf8(&bytes)
                .unwrap()
                .lines()
                .map(|line| serde_json::from_str(line).unwrap())
                .collect();
            assert_eq!(rows.len() as u64, count);
            assert_eq!(summary["functions"], count);
            for (id, row) in rows.iter().enumerate() {
                assert_eq!(row["function"], id);
                assert_eq!(row["raw_path"], current["functions"][id]["path"]);
                assert_eq!(
                    row["fragment_prefix_bytes"],
                    current["functions"][id]["fragment_prefix_bytes"]
                );
                assert!(row["report"].is_object());
            }
        }
        for file in ["links.jsonl", "initializers.jsonl"] {
            assert_eq!(output.join(file).exists(), expected.contains(&file));
        }
        assert_eq!(
            fs::read_dir(&output).unwrap().count() as u64,
            count + 5 + expected.len() as u64
        );
        assert_eq!(
            current.get("source_evidence_contract").is_some(),
            !expected.is_empty()
        );
    }
}

#[test]
fn cli_defaults_to_both_designs_but_can_select_each_independently() {
    let temp = tempfile::tempdir().unwrap();
    for (name, flags, links, initializers) in [
        ("default", vec![], true, true),
        ("links", vec!["--evidence", "links"], true, false),
        (
            "initializers",
            vec!["--evidence", "initializers"],
            false,
            true,
        ),
        ("none", vec!["--evidence", "none"], false, false),
        ("raw", vec!["--raw-only"], false, false),
    ] {
        let output = temp.path().join(name);
        let result = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .arg("workspace")
            .arg(fixture())
            .arg("-o")
            .arg(&output)
            .args(flags)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let response: Value = serde_json::from_slice(&result.stdout).unwrap();
        assert_eq!(output.join("links.jsonl").exists(), links);
        assert_eq!(output.join("initializers.jsonl").exists(), initializers);
        assert_eq!(output.join("view").exists(), name != "raw");
        assert_eq!(
            response["evidence"],
            match name {
                "default" => "all",
                "raw" => "none",
                _ => name,
            }
        );
    }
}

#[test]
fn conflicting_or_invalid_design_flags_have_no_partial_publication() {
    let temp = tempfile::tempdir().unwrap();
    for flags in [
        vec!["--raw-only", "--evidence", "links"],
        vec!["--evidence", "unknown"],
    ] {
        let output = temp.path().join("output");
        let result = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .arg("workspace")
            .arg(fixture())
            .arg("-o")
            .arg(&output)
            .args(flags)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(result.stdout.is_empty());
        assert!(!output.exists());
        assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 0);
    }
}

#[test]
fn existing_output_is_preserved_for_every_design() {
    let temp = tempfile::tempdir().unwrap();
    let output = temp.path().join("existing");
    fs::create_dir(&output).unwrap();
    fs::write(output.join("user-file"), b"untouched").unwrap();
    for design in [
        EvidenceDesign::None,
        EvidenceDesign::Links,
        EvidenceDesign::Initializers,
        EvidenceDesign::All,
    ] {
        assert!(workspace_with_evidence(fixture(), &output, true, design).is_err());
        assert_eq!(fs::read(output.join("user-file")).unwrap(), b"untouched");
        assert_eq!(fs::read_dir(&output).unwrap().count(), 1);
        assert_eq!(fs::read_dir(temp.path()).unwrap().count(), 1);
    }
}

#[test]
fn initializer_raw_store_ordinals_replay_through_the_wired_sites_cli() {
    use hermes_dec_rs::{bundle::export_function_fragments, HbcFile};
    let data = fs::read(fixture()).unwrap();
    let hbc = HbcFile::parse(&data).unwrap();
    let ids: Vec<_> = (0..hbc.functions.count()).collect();
    let mut checked = 0;
    for (id, source) in export_function_fragments(&hbc, &ids).unwrap() {
        let exceptions = hbc
            .functions
            .get_parsed_header(id)
            .unwrap()
            .exc_handlers
            .iter()
            .flat_map(|handler| [handler.start, handler.end, handler.target])
            .collect();
        let report =
            hermes_dec_rs::cli::initializers::report_source(&source, id, &exceptions).unwrap();
        for row in report["rows"].as_array().unwrap() {
            let result = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
                .arg("sites")
                .arg(fixture())
                .arg(id.to_string())
                .args(["--kind", "slot-write", "--offset"])
                .arg(row["store_ordinal"].as_u64().unwrap().to_string())
                .args([
                    "--limit",
                    "1",
                    "--depth",
                    "0",
                    "--compact",
                    "--max-bytes",
                    "1000000",
                ])
                .output()
                .unwrap();
            assert!(
                result.status.success(),
                "{}",
                String::from_utf8_lossy(&result.stderr)
            );
            let page: Value = serde_json::from_slice(&result.stdout).unwrap();
            assert_eq!(page["sites"].as_array().unwrap().len(), 1);
            let site = &page["sites"][0];
            assert_eq!(site["pc"], row["pc"]);
            assert_eq!(site["slot"], row["slot"]);
            assert_eq!(site["source_span"], row["source_span"]);
            checked += 1;
        }
    }
    assert!(
        checked > 0,
        "must verify actual captured-slot stores, not empty pages"
    );
}
