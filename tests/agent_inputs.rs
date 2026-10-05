pub use hermes_dec_rs::{hbc, DecompilerError, DecompilerResult};
#[path = "../src/cli/inputs.rs"]
#[allow(dead_code)]
mod inputs;

use serde_json::Value;
use std::{fs, path::Path, process::Command};

fn authored_prefix() -> Vec<u8> {
    let mut bytes = hbc::HBC_MAGIC.to_le_bytes().to_vec();
    bytes.extend_from_slice(&96u32.to_le_bytes());
    bytes
}

fn discover(path: &Path, limit: usize, offset: usize) -> Value {
    serde_json::from_slice(&inputs::discover(path, limit, offset, 100_000).unwrap()).unwrap()
}

#[test]
fn ignored_hidden_nested_authored_fixture_is_discovered() {
    let temp = tempfile::tempdir().unwrap();
    fs::write(temp.path().join(".gitignore"), "*.hbc\n.hidden/\n").unwrap();
    let nested = temp.path().join(".hidden/nested");
    fs::create_dir_all(&nested).unwrap();
    let fixture = temp.path().join("authored.HBC");
    fs::write(&fixture, authored_prefix()).unwrap();
    fs::copy(&fixture, nested.join("copied.hbc")).unwrap();
    let result = discover(temp.path(), 20, 0);
    assert_eq!(result["total_matching_magic_candidates"], 2);
    assert_eq!(
        result["coverage"]["excluded_directories"],
        serde_json::json!([])
    );
    assert!(result["candidates"][0]["path"]
        .as_str()
        .unwrap()
        .contains(".hidden/nested/copied.hbc"));
    assert_eq!(result["full_validation"], false);
}

#[test]
fn file_roots_rejections_and_prefix_only_reads() {
    let temp = tempfile::tempdir().unwrap();
    let file = temp.path().join("large.bundle");
    fs::write(&file, authored_prefix()).unwrap();
    fs::OpenOptions::new()
        .write(true)
        .open(&file)
        .unwrap()
        .set_len(20_000_000)
        .unwrap();
    let result = discover(&file, 20, 0);
    assert_eq!(result["candidates"][0]["size_bytes"], 20_000_000);
    assert_eq!(result["candidates"][0]["version"], 96);
    fs::write(temp.path().join("plain.bundle"), b"plain javascript bundle").unwrap();
    fs::write(temp.path().join("short.hbc"), &authored_prefix()[..9]).unwrap();
    let result = discover(temp.path(), 20, 0);
    assert_eq!(result["scanned_extension_candidates"], 3);
    assert_eq!(result["rejected_prefix_count"], 2);
    assert_eq!(result["read_errors"]["total"], 0);
}

#[test]
fn sorted_pages_and_bounds() {
    let temp = tempfile::tempdir().unwrap();
    for name in ["z.hbc", "a.hbc", "m.HBC"] {
        fs::write(temp.path().join(name), authored_prefix()).unwrap();
    }
    let first = discover(temp.path(), 1, 0);
    assert!(first["candidates"][0]["path"]
        .as_str()
        .unwrap()
        .ends_with("a.hbc"));
    assert_eq!(first["next_offset"], 1);
    assert_eq!(discover(temp.path(), 1, 2)["next_offset"], Value::Null);
    assert_eq!(
        discover(temp.path(), 1, usize::MAX)["candidates"],
        serde_json::json!([])
    );
    assert!(inputs::discover(temp.path(), 0, 0, 100_000).is_err());
    assert!(inputs::discover(temp.path(), 1001, 0, 100_000).is_err());
    assert!(inputs::discover(temp.path(), 20, 0, 16_777_217).is_err());
    assert!(inputs::discover(temp.path(), 20, 0, 1).is_err());
    assert!(inputs::discover(&temp.path().join("absent"), 20, 0, 100_000).is_err());
}

#[test]
fn larger_candidates_rank_before_small_fixtures() {
    let temp = tempfile::tempdir().unwrap();
    let small = temp.path().join("a.hbc");
    let large = temp.path().join("z.bundle");
    fs::write(&small, authored_prefix()).unwrap();
    fs::write(&large, authored_prefix()).unwrap();
    fs::OpenOptions::new()
        .write(true)
        .open(&large)
        .unwrap()
        .set_len(1000)
        .unwrap();
    assert_eq!(
        discover(temp.path(), 1, 0)["candidates"][0]["size_bytes"],
        1000
    );
}

#[cfg(unix)]
#[test]
fn symlink_loop_and_file_links_are_skipped() {
    use std::os::unix::fs::symlink;
    let temp = tempfile::tempdir().unwrap();
    let file = temp.path().join("original.hbc");
    fs::write(&file, authored_prefix()).unwrap();
    symlink(temp.path(), temp.path().join("loop")).unwrap();
    let link = temp.path().join("link.hbc");
    symlink(&file, &link).unwrap();
    let result = discover(temp.path(), 20, 0);
    assert_eq!(result["skipped_symlinks"], 2);
    assert_eq!(result["total_matching_magic_candidates"], 1);
    assert_eq!(discover(&link, 20, 0)["skipped_symlinks"], 1);
    assert_eq!(fs::read(file).unwrap(), authored_prefix());
}

// APFS rejects invalid UTF-8 names; Linux filesystems permit this fixture.
#[cfg(target_os = "linux")]
#[test]
fn non_utf8_paths_are_marked_lossy() {
    use std::os::unix::ffi::OsStringExt;
    let temp = tempfile::tempdir().unwrap();
    fs::write(
        temp.path()
            .join(std::ffi::OsString::from_vec(b"x\xff.hbc".to_vec())),
        authored_prefix(),
    )
    .unwrap();
    assert_eq!(
        discover(temp.path(), 20, 0)["candidates"][0]["path_lossy"],
        true
    );
}

#[test]
fn wired_cli_budget_and_missing_root_leave_stdout_quiet() {
    let temp = tempfile::tempdir().unwrap();
    fs::write(temp.path().join("authored.hbc"), authored_prefix()).unwrap();
    let success = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
        .arg("inputs")
        .arg(temp.path())
        .output()
        .unwrap();
    assert!(
        success.status.success(),
        "{}",
        String::from_utf8_lossy(&success.stderr)
    );
    let parsed: Value = serde_json::from_slice(&success.stdout).unwrap();
    assert_eq!(parsed["total_matching_magic_candidates"], 1);
    for (root, budget) in [
        (temp.path().to_path_buf(), "1"),
        (temp.path().join("absent"), "100000"),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_hermes-dec-rs"))
            .arg("inputs")
            .arg(root)
            .args(["--max-bytes", budget])
            .output()
            .unwrap();
        assert!(!output.status.success());
        assert!(output.stdout.is_empty());
    }
}
