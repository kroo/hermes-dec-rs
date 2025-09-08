//! Metro package/module analysis
//!
//! This module analyzes Metro (React Native) standard bundles embedded in Hermes
//! bytecode. It focuses on parsing the pre-code/modules/post-code structure in
//! function 0, extracting module IDs, dependency maps, and (when present) verbose
//! names/paths. See docs/standard-bundle-format.md and
//! docs/path-reconstruction-heuristics.md for details.

use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::hbc::HbcFile;
use serde::Serialize;

/// Summary statistics for a bundle
#[derive(Debug, Clone, Serialize, Default)]
pub struct PackageStats {
    pub function_count: u32,
    pub string_count: u32,
}

/// A single Metro module and its metadata
#[derive(Debug, Clone, Serialize, Default)]
pub struct ModuleInfo {
    /// Function ID that implements this module (entry.offset)
    pub function_id: u32,
    /// Symbol ID (filename string_id for non-static bundles; module ID for static)
    pub symbol_id: Option<u32>,
    /// Resolved filename/verboseName when available (non-static bundles)
    pub filename: Option<String>,
    /// Dependencies (to be populated by deeper analysis)
    pub dependencies: Vec<String>,
    /// True if this module appears to be executed from post-code (entrypoint)
    pub is_entry: bool,
}

/// High-level package report
#[derive(Debug, Clone, Serialize, Default)]
pub struct PackageReport {
    pub stats: PackageStats,
    pub modules: Vec<ModuleInfo>,
    /// Function IDs of modules executed as entry points
    pub entrypoints: Vec<u32>,
}

/// Analyze a Hermes HBC file for Metro bundle structure and module graph.
///
/// Note: This is an initial scaffold that returns file-level stats and an empty
/// module list when no bundle pattern is detected. Module extraction will be
/// implemented incrementally using the documented patterns.
pub fn analyze_package(hbc: &HbcFile) -> DecompilerResult<PackageReport> {
    // Resolve string table once
    let strings = hbc
        .strings
        .extract_strings()
        .unwrap_or_else(|_| Vec::new());

    // Build module list from CommonJS table
    let mut modules: Vec<ModuleInfo> = Vec::new();
    for entry in &hbc.cjs_modules.entries {
        let filename = if !hbc.cjs_modules.is_static {
            strings.get(entry.symbol_id as usize).cloned()
        } else {
            None
        };
        modules.push(ModuleInfo {
            function_id: entry.offset,
            symbol_id: Some(entry.symbol_id),
            filename,
            ..Default::default()
        });
    }

    // Detect entrypoints by scanning function 0 for direct calls to module functions
    let mut entry_fn_ids: Vec<u32> = Vec::new();
    if hbc.functions.count() > 0 {
        if let Ok(instructions) = hbc.functions.get_instructions(0) {
            for inst in &instructions {
                match &inst.instruction {
                    UnifiedInstruction::CallDirect { operand_2, .. } => {
                        let fid = *operand_2 as u32;
                        entry_fn_ids.push(fid);
                    }
                    _ => {}
                }
            }
        }
    }
    entry_fn_ids.sort_unstable();
    entry_fn_ids.dedup();

    // Mark modules that appear in entry function calls
    for m in &mut modules {
        if entry_fn_ids.binary_search(&m.function_id).is_ok() {
            m.is_entry = true;
        }
    }

    let stats = PackageStats {
        function_count: hbc.functions.count(),
        string_count: hbc.header.string_count(),
    };

    Ok(PackageReport {
        stats,
        modules,
        entrypoints: entry_fn_ids,
    })
}

// This will parse through function 0, looking for a structure matching the following:
//
// [PRE-CODE] + [MODULES] + [POST-CODE]
//
// ### Pre-Code Section
//
// - Runtime polyfills and initialization code
// - Metro's require implementation
// - Script-type modules (type: `js/script`)
// - Usually includes React Native polyfills in RN apps
//
// ### Modules Section
//
// - Application modules wrapped in `__d()` calls
// - Each module has type: `js/module`
// - Sorted by module ID (numeric or string)
//
// ### Post-Code Section
//
// - Entry point execution statements (`require()` calls)
// - Source map URL comment
// - Source URL comment (optional)
//
//
//
// ### Pattern Variations by Parameters
//
// **Development Build (3 parameters):**
//
// ```javascript
// __d(function() { /* code */ }, "moduleId", ["dep1", "dep2"], "path/to/module")
// ```
//
// **Production Build (2 parameters):**
//
// ```javascript
// __d(function() { /* code */ }, 123, [456, 789])
// ```
//
// ## Dependency Map Formats

// ### Array Format (Standard)

// ```javascript
// [moduleId1, moduleId2, moduleId3]
// ```

// - Index corresponds to `_dependencyMap[index]` in module code
// - Null entries represent unresolved dependencies

// ### Object Format (With Async Paths)

// ```javascript
// {
//   0: moduleId1,
//   1: moduleId2,
//   paths: {
//     moduleId2: "/path/to/chunk.bundle?modulesOnly=true&runModule=false"
//   }
// }
// ```
