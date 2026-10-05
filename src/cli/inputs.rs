use crate::{hbc::HBC_MAGIC, DecompilerError, DecompilerResult};
use serde_json::{json, Value};
use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

const PREVIEW_LIMIT: usize = 20;

fn path_fields(path: &Path) -> Value {
    json!({"path": path.to_string_lossy(), "path_lossy": path.to_str().is_none()})
}

#[derive(Default)]
struct Issues {
    total: usize,
    preview: Vec<Value>,
}

impl Issues {
    fn record(&mut self, path: &Path, reason: impl ToString) {
        self.total += 1;
        if self.preview.len() < PREVIEW_LIMIT {
            let mut entry = path_fields(path);
            entry["reason"] = json!(reason.to_string());
            self.preview.push(entry);
        }
    }

    fn report(&self) -> Value {
        json!({"total": self.total, "preview": self.preview})
    }
}

/// Discover prefix-matching files without decoding programs or reading file bodies.
pub fn run(root: &Path, limit: usize, offset: usize, max_bytes: usize) -> DecompilerResult<()> {
    let bytes = discover(root, limit, offset, max_bytes)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

pub fn discover(
    root: &Path,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    if !(1..=1000).contains(&limit) || !(1..=16_777_216).contains(&max_bytes) {
        return Err(DecompilerError::Io(
            "limit must be 1..1000 and max-bytes 1..16777216".into(),
        ));
    }
    let root = if root.is_absolute() {
        root.to_path_buf()
    } else {
        std::env::current_dir()?.join(root)
    };
    // Do not canonicalize: canonicalization would follow a symlink root.
    let root_metadata = fs::symlink_metadata(&root)?;
    let mut pending = vec![root.clone()];
    let mut candidates: Vec<(PathBuf, u64)> = Vec::new();
    let mut read_errors = Issues::default();
    let mut dir_errors = Issues::default();
    let mut rejected = Issues::default();
    let mut skipped_symlinks = 0usize;
    while let Some(path) = pending.pop() {
        let metadata = if path == root {
            Ok(root_metadata.clone())
        } else {
            fs::symlink_metadata(&path)
        };
        let metadata = match metadata {
            Ok(metadata) => metadata,
            Err(error) => {
                read_errors.record(&path, error);
                continue;
            }
        };
        if metadata.file_type().is_symlink() {
            skipped_symlinks += 1;
        } else if metadata.is_dir() {
            match fs::read_dir(&path) {
                Ok(entries) => {
                    let mut children = Vec::new();
                    for entry in entries {
                        match entry {
                            Ok(entry) => children.push(entry.path()),
                            Err(error) => dir_errors.record(&path, error),
                        }
                    }
                    children.sort();
                    pending.extend(children.into_iter().rev());
                }
                Err(error) => dir_errors.record(&path, error),
            }
        } else if metadata.is_file()
            && path
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| e.eq_ignore_ascii_case("hbc") || e.eq_ignore_ascii_case("bundle"))
        {
            candidates.push((path, metadata.len()));
        }
    }
    candidates.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    let scanned = candidates.len();
    let mut matches = Vec::new();
    for (path, size) in candidates {
        // Recheck the type immediately before opening; never open known special files.
        let prefix = (|| -> std::io::Result<[u8; 12]> {
            let metadata = fs::symlink_metadata(&path)?;
            if !metadata.is_file() || metadata.file_type().is_symlink() {
                return Err(std::io::Error::other(
                    "file type changed before prefix read",
                ));
            }
            let mut file = fs::File::open(&path)?;
            let mut prefix = [0; 12];
            file.read_exact(&mut prefix)?;
            Ok(prefix)
        })();
        match prefix {
            Ok(prefix) if prefix[..8] == HBC_MAGIC.to_le_bytes() => {
                let mut entry = path_fields(&path);
                entry["size_bytes"] = json!(size);
                entry["version"] = json!(u32::from_le_bytes(prefix[8..12].try_into().unwrap()));
                entry["full_validation"] = json!(false);
                matches.push(entry);
            }
            Ok(_) => rejected.record(&path, "wrong magic"),
            Err(error) if error.kind() == std::io::ErrorKind::UnexpectedEof => {
                rejected.record(&path, "truncated header: fewer than 12 bytes")
            }
            Err(error) => read_errors.record(&path, error),
        }
    }
    let total = matches.len();
    let end = offset.saturating_add(limit).min(total);
    let page = &matches[offset.min(total)..end];
    let report = json!({
        "schema_version": 1, "root": path_fields(&root), "candidates": page,
        "total_matching_magic_candidates": total,
        "offset": offset, "limit": limit,
        "next_offset": if end < total { Some(end) } else { None },
        "scanned_extension_candidates": scanned, "rejected_prefix_count": rejected.total,
        "rejected_prefixes": rejected.report(), "read_errors": read_errors.report(),
        "dir_errors": dir_errors.report(), "skipped_symlinks": skipped_symlinks,
        "full_validation": false,
        "sort": "size_bytes descending, then native path order",
        "coverage": {"extension_selection": ["hbc", "bundle"], "case_insensitive": true,
            "includes_hidden": true, "includes_git_ignored": true, "excluded_directories": [],
            "symlink_policy": "skip observed symlinks, including root; concurrent filesystem replacement races are not excluded", "prefix_bytes": 12,
            "validation": "magic and little-endian version only; no format validation or program decoding",
            "error_policy": "read and directory errors are incomplete coverage, not negative proof",
            "path_policy": "absolute display paths; path_lossy marks non-UTF8 paths",
            "preview_limit": PREVIEW_LIMIT}
    });
    let mut bytes =
        serde_json::to_vec(&report).map_err(|error| DecompilerError::Io(error.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > max_bytes {
        return Err(DecompilerError::Io(format!(
            "input discovery output exceeds max-bytes ({max_bytes})"
        )));
    }
    Ok(bytes)
}

#[cfg(all(test, unix))]
mod tests {
    #[test]
    fn invalid_utf8_display_paths_are_explicitly_lossy() {
        use std::os::unix::ffi::OsStringExt;
        let path = std::path::PathBuf::from(std::ffi::OsString::from_vec(b"x\xff.hbc".to_vec()));
        let fields = super::path_fields(&path);
        assert_eq!(fields["path_lossy"], true);
        assert!(fields["path"].as_str().unwrap().ends_with(".hbc"));
    }
}
