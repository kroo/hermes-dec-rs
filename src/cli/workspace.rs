//! Offline, protocol-independent function workspaces for shell inspection.

use crate::{bundle::export_function_fragments, DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::{StaticMemberExpression, StringLiteral};
use oxc_ast_visit::{walk::walk_static_member_expression, Visit};
use serde::Serialize;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

const MAX_SNIPPETS: usize = 32;
const MAX_SNIPPET_CHARS: usize = 160;
const FRAGMENT_HEADER: &str = "// Complete JS inspection fragment, not a standalone program.\n// F = function bodies; M = metadata; r = registers; env = captured environment.\n// self = this; args = arguments. Runtime helpers and referenced F entries\n// are defined by export-bundle, not by this individual file.\n";
const GUIDE: &str = include_str!("workspace_guide.md");
const RUNTIME_HEADER: &str = "// Inspection-only runtime helper source, not a runnable app.\n// This file documents the helpers referenced by f<ID>.js fragments.\n// Function and metadata tables and bundle assembly are not included.\n";
const RUNTIME_SOURCE: &str = include_str!("../bundle/runtime.js");
static STAGING_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Serialize)]
struct FunctionEntry {
    id: u32,
    name: String,
    path: String,
    js_bytes: usize,
    fragment_prefix_bytes: usize,
}

#[derive(Serialize)]
struct Navigation {
    command: &'static str,
    subcommand: &'static str,
    input: &'static str,
    function: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pc: Option<&'static str>,
    flags: Vec<&'static str>,
}

fn navigation() -> Vec<Navigation> {
    [
        ("origins", Some("PC"), vec!["--expressions"]),
        ("sites", None, vec!["--compact"]),
        (
            "sites",
            None,
            vec!["--compact", "--kind", "slot-write", "--slot", "SLOT"],
        ),
        ("captures", None, vec![]),
        ("symbols", None, vec!["--slot", "SLOT"]),
        ("origins", Some("PC"), vec!["--text"]),
    ]
    .into_iter()
    .map(|(subcommand, pc, flags)| Navigation {
        command: "hermes-dec-rs",
        subcommand,
        input: "INPUT",
        function: "FUNCTION",
        pc,
        flags,
    })
    .collect()
}

fn fragment_header(id: u32) -> String {
    format!("{FRAGMENT_HEADER}// Navigation (placeholders; see GUIDE.md):\n// Definitions/call roles: hermes-dec-rs origins INPUT {id} PC --expressions\n// Filter/page sites: hermes-dec-rs sites INPUT {id} --compact\n// Numeric env slots: hermes-dec-rs captures INPUT {id}\n// Slot stores: hermes-dec-rs sites INPUT {id} --kind slot-write --slot SLOT --compact\n")
}

#[derive(Serialize)]
struct IndexEntry<'a> {
    schema_version: u32,
    id: u32,
    name: &'a str,
    path: &'a str,
    fragment_prefix_bytes: usize,
    snippets: Vec<String>,
    snippets_truncated: bool,
    static_assignments: Vec<serde_json::Value>,
    static_assignments_truncated: bool,
}

#[derive(Serialize)]
struct Manifest {
    schema_version: u32,
    hbc_version: u32,
    function_count: u32,
    file_count: u64,
    string_count: u32,
    input_bytes: usize,
    js_bytes: usize,
    js_bytes_description: &'static str,
    runtime_bytes: usize,
    standalone: bool,
    power_loss_durable: bool,
    runtime_path: &'static str,
    runtime_helpers: &'static str,
    index_path: &'static str,
    guide_path: &'static str,
    navigation: Vec<Navigation>,
    index_description: &'static str,
    max_snippets_per_function: usize,
    max_snippet_chars: usize,
    functions: Vec<FunctionEntry>,
}

#[derive(Default)]
struct Snippets {
    values: Vec<String>,
    truncated: bool,
}

impl Snippets {
    fn add(&mut self, value: &str) {
        if value.is_empty() {
            return;
        }
        let snippet: String = value.chars().take(MAX_SNIPPET_CHARS).collect();
        self.truncated |= snippet.len() < value.len();
        if self.values.contains(&snippet) {
            return;
        }
        if self.values.len() == MAX_SNIPPETS {
            self.truncated = true;
        } else {
            self.values.push(snippet);
        }
    }
}

impl<'a> Visit<'a> for Snippets {
    fn visit_string_literal(&mut self, literal: &StringLiteral<'a>) {
        self.add(literal.value.as_str());
    }

    fn visit_static_member_expression(&mut self, member: &StaticMemberExpression<'a>) {
        self.add(member.property.name.as_str());
        walk_static_member_expression(self, member);
    }
}

fn snippets(code: &str) -> DecompilerResult<Snippets> {
    let allocator = oxc_allocator::Allocator::default();
    let parsed = oxc_parser::Parser::new(&allocator, code, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(DecompilerError::internal(format!(
            "Workspace JS syntax validation failed: {:?}",
            parsed.errors
        )));
    }
    let mut snippets = Snippets::default();
    snippets.visit_program(&parsed.program);
    Ok(snippets)
}

struct Staging {
    path: PathBuf,
    published: bool,
}

impl Staging {
    fn create(parent: &Path) -> DecompilerResult<Self> {
        for _ in 0..100 {
            let id = STAGING_ID.fetch_add(1, Ordering::Relaxed);
            let path = parent.join(format!(".agent-workspace-{}-{id}.tmp", std::process::id()));
            match fs::create_dir(&path) {
                Ok(()) => {
                    return Ok(Self {
                        path,
                        published: false,
                    })
                }
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(error.into()),
            }
        }
        Err(DecompilerError::internal(
            "Cannot allocate workspace staging directory",
        ))
    }
}

impl Drop for Staging {
    fn drop(&mut self) {
        if self.published {
            return;
        }
        if let Err(error) = fs::remove_dir_all(&self.path) {
            if error.kind() != std::io::ErrorKind::NotFound {
                log::warn!(
                    "Failed to clean workspace staging {}: {error}",
                    self.path.display()
                );
            }
        }
    }
}

fn json<T: Serialize>(writer: &mut impl Write, value: &T) -> DecompilerResult<()> {
    serde_json::to_writer(writer, value)
        .map_err(|error| DecompilerError::internal(format!("Workspace JSON: {error}")))
}

// std::fs::rename can replace an existing empty directory. Use an exclusive
// rename so even an output created concurrently is never overwritten.
#[cfg(any(target_os = "macos", target_os = "linux"))]
fn publish(source: &Path, destination: &Path) -> std::io::Result<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let source = CString::new(source.as_os_str().as_bytes())?;
    let destination = CString::new(destination.as_os_str().as_bytes())?;
    #[cfg(target_os = "macos")]
    let result = {
        extern "C" {
            fn renamex_np(
                from: *const std::ffi::c_char,
                to: *const std::ffi::c_char,
                flags: u32,
            ) -> i32;
        }
        // SAFETY: Both NUL-terminated paths remain alive for the call.
        unsafe { renamex_np(source.as_ptr(), destination.as_ptr(), 4) }
    };
    #[cfg(target_os = "linux")]
    let result = {
        extern "C" {
            fn renameat2(
                from_fd: i32,
                from: *const std::ffi::c_char,
                to_fd: i32,
                to: *const std::ffi::c_char,
                flags: u32,
            ) -> i32;
        }
        // SAFETY: Both NUL-terminated paths remain alive; -100 is AT_FDCWD.
        unsafe { renameat2(-100, source.as_ptr(), -100, destination.as_ptr(), 1) }
    };
    if result == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
fn publish(_source: &Path, _destination: &Path) -> std::io::Result<()> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "Atomic no-replace workspace publication requires macOS or Linux",
    ))
}

/// Export every function once into a new directory, without executing JS.
/// Files contain complete readable fragments, not standalone programs: runtime
/// helpers and references to other functions remain explicit. Only index
/// snippets are bounded. An existing output (including a symlink) is refused.
/// The output's parent must already exist; failures clean up staging files.
/// Atomic no-replace publication currently requires macOS or Linux.
/// Writes are closed before publication, but no power-loss durability is promised.
pub fn workspace(input: &Path, output: &Path) -> DecompilerResult<()> {
    let start = std::time::Instant::now();
    match fs::symlink_metadata(output) {
        Ok(_) => {
            return Err(DecompilerError::internal(format!(
                "Workspace output already exists: {}",
                output.display()
            )))
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }
    if output.file_name().is_none() {
        return Err(DecompilerError::internal(
            "Workspace output must name a new directory",
        ));
    }
    let parent = output
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    let mut staging = Staging::create(parent)?;
    let data = fs::read(input)?;
    let read_time = start.elapsed();
    let hbc = HbcFile::parse_for_bundle(&data).map_err(DecompilerError::internal)?;
    let parse_time = start.elapsed() - read_time;
    let ids: Vec<u32> = (0..hbc.functions.count()).collect();
    let mut assignments = std::collections::BTreeMap::<u32, Vec<serde_json::Value>>::new();
    for site in super::bindings::collect(&hbc)? {
        assignments.entry(site.function_id).or_default().push(serde_json::json!({
            "name":site.name.chars().take(MAX_SNIPPET_CHARS).collect::<String>(),
            "target":site.target.chars().take(MAX_SNIPPET_CHARS).collect::<String>(),
            "text_truncated":site.name.chars().count()>MAX_SNIPPET_CHARS || site.target.chars().count()>MAX_SNIPPET_CHARS,
            "source_function_id":site.source_function_id,"pc":site.pc,
            "kind":"static_property_assignment"
        }));
    }
    let fragments = export_function_fragments(&hbc, &ids)?;
    let export_time = start.elapsed() - read_time - parse_time;
    if fragments.len() != ids.len() {
        return Err(DecompilerError::internal(
            "Workspace exporter returned an incomplete function set",
        ));
    }
    let mut manifest = Manifest {
        schema_version: 1,
        hbc_version: hbc.header.version(),
        function_count: hbc.functions.count(),
        file_count: u64::from(hbc.functions.count()) + 4,
        string_count: hbc.strings.string_count,
        input_bytes: data.len(),
        js_bytes: 0,
        js_bytes_description: "Function JS including inspection headers plus runtime.js; excludes GUIDE.md, manifest.json and index.jsonl.",
        runtime_bytes: RUNTIME_HEADER.len() + RUNTIME_SOURCE.len(),
        standalone: false,
        power_loss_durable: false,
        runtime_path: "runtime.js",
        runtime_helpers: "JS files are complete inspection fragments, not standalone programs. F entries reference other functions; M, env, r, self, args and runtime helpers are supplied by export-bundle. See each file's header.",
        index_path: "index.jsonl",
        guide_path: "GUIDE.md",
        navigation: navigation(),
        index_description: "One row per function in ID order. Snippets are decoded JS string literals and static property names, including generated helper strings. Static assignments are bounded local closure-to-property navigation evidence, not runtime exports. Search f<ID>.js for complete contents; index omission is not evidence of absence.",
        max_snippets_per_function: MAX_SNIPPETS,
        max_snippet_chars: MAX_SNIPPET_CHARS,
        functions: Vec::new(),
    };
    let write_start = std::time::Instant::now();
    let mut index = BufWriter::new(File::create(staging.path.join("index.jsonl"))?);
    for (expected_id, (id, code)) in ids.into_iter().zip(fragments) {
        if id != expected_id {
            return Err(DecompilerError::internal(
                "Workspace exporter returned out-of-order function IDs",
            ));
        }
        let header = fragment_header(id);
        let code = format!("{header}{code}");
        let snippet = snippets(&code)?;
        let entry = FunctionEntry {
            id,
            name: hbc
                .functions
                .get_function_name(id, &hbc.strings)
                .ok_or_else(|| {
                    DecompilerError::internal(format!("Missing function name for {id}"))
                })?,
            path: format!("f{id}.js"),
            js_bytes: code.len(),
            fragment_prefix_bytes: header.len(),
        };
        let mut file = File::create(staging.path.join(&entry.path))?;
        file.write_all(code.as_bytes())?;
        let sites = assignments.remove(&id).unwrap_or_default();
        let sites_truncated = sites.len() > 12;
        json(
            &mut index,
            &IndexEntry {
                schema_version: 1,
                id,
                name: &entry.name,
                path: &entry.path,
                fragment_prefix_bytes: entry.fragment_prefix_bytes,
                snippets: snippet.values,
                snippets_truncated: snippet.truncated,
                static_assignments: sites.into_iter().take(12).collect(),
                static_assignments_truncated: sites_truncated,
            },
        )?;
        index.write_all(b"\n")?;
        manifest.js_bytes += code.len();
        manifest.functions.push(entry);
    }
    index.flush()?;
    drop(index);
    let mut runtime = File::create(staging.path.join("runtime.js"))?;
    runtime.write_all(RUNTIME_HEADER.as_bytes())?;
    runtime.write_all(RUNTIME_SOURCE.as_bytes())?;
    drop(runtime);
    manifest.js_bytes += manifest.runtime_bytes;
    fs::write(staging.path.join("GUIDE.md"), GUIDE)?;
    let mut file = BufWriter::new(File::create(staging.path.join("manifest.json"))?);
    json(&mut file, &manifest)?;
    file.write_all(b"\n")?;
    file.flush()?;
    drop(file);
    publish(&staging.path, output)?;
    staging.published = true;
    log::info!("workspace timing: read={read_time:?}, parse={parse_time:?}, export={export_time:?}, index_and_write={:?}, total={:?}, functions={}, js_bytes={}", write_start.elapsed(), start.elapsed(), manifest.function_count, manifest.js_bytes);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix_offsets_account_for_variable_function_id_width() {
        let raw = "F[10] = function () { return '\u{00e9}'; };\n";
        for id in [9, 10, u32::MAX] {
            let header = fragment_header(id);
            let code = format!("{header}{raw}");
            assert_eq!(&code.as_bytes()[header.len()..], raw.as_bytes());
        }
        assert_ne!(fragment_header(9).len(), fragment_header(10).len());
    }

    #[test]
    fn snippets_decode_strings_and_bound_only_the_index() {
        let long = "\u{00e9}".repeat(MAX_SNIPPET_CHARS + 1);
        let mut code = format!("const x = {{}}; x.property; x['escaped\\nvalue']; '{long}';");
        for i in 0..MAX_SNIPPETS + 2 {
            code.push_str(&format!("'literal{i}';"));
        }
        let result = snippets(&code).unwrap();
        assert_eq!(result.values.len(), MAX_SNIPPETS);
        assert!(result.truncated);
        assert!(result.values.contains(&"property".into()));
        assert!(result.values.contains(&"escaped\nvalue".into()));
        assert!(result
            .values
            .iter()
            .all(|s| s.chars().count() <= MAX_SNIPPET_CHARS));
    }

    #[test]
    fn exclusive_publish_preserves_an_empty_competing_output() {
        let temp = tempfile::tempdir().unwrap();
        let stage = Staging::create(temp.path()).unwrap();
        fs::write(stage.path.join("payload"), "complete").unwrap();
        let output = temp.path().join("output");
        fs::create_dir(&output).unwrap();
        assert!(publish(&stage.path, &output).is_err());
        assert_eq!(fs::read_dir(&output).unwrap().count(), 0);
        assert!(stage.path.join("payload").exists());
        let path = stage.path.clone();
        drop(stage);
        assert!(!path.exists());
    }
}
