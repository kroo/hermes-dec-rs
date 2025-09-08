//! Metro package/module analysis
//!
//! This module analyzes Metro (React Native) standard bundles embedded in Hermes
//! bytecode. It focuses on parsing the pre-code/modules/post-code structure in
//! function 0, extracting module IDs, dependency maps, and (when present) verbose
//! names/paths. See docs/standard-bundle-format.md and
//! docs/path-reconstruction-heuristics.md for details.

use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::generated::unified_instructions::UnifiedInstruction;
use crate::{
    analysis::{call_site_analysis::CallSiteAnalysis, value_tracker::ValueTracker},
    cfg::{ssa, Cfg},
    hbc::InstructionIndex,
};
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
    /// Function ID that implements this module (entry.offset or CreateClosure target)
    pub function_id: Option<u32>,
    /// Metro module ID (numeric)
    pub module_id_u32: Option<u32>,
    /// Metro module ID (string)
    pub module_id_str: Option<String>,
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
    /// Detected entry modules by module ID (string or numeric rendered as string)
    pub entrypoints: Vec<String>,
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

    // Build initial module list from CommonJS table (if present)
    let mut modules: Vec<ModuleInfo> = Vec::new();
    for entry in &hbc.cjs_modules.entries {
        let filename = if !hbc.cjs_modules.is_static {
            strings.get(entry.symbol_id as usize).cloned()
        } else {
            None
        };
        modules.push(ModuleInfo {
            function_id: Some(entry.offset),
            symbol_id: Some(entry.symbol_id),
            filename,
            ..Default::default()
        });
    }

    // Detect entrypoints by scanning function 0 for direct calls to module functions
    // Prepare CFG/SSA for function 0 to analyze calls (__d registrations and __r/require entrypoints)
    let mut detected_entries: Vec<String> = Vec::new();
    let mut detected_entries_ordered: Vec<String> = Vec::new();
    if hbc.functions.count() > 0 {
        let mut cfg = Cfg::new(hbc, 0);
        cfg.build();
        let ssa = ssa::construct_ssa(&cfg, 0).map_err(|e| DecompilerError::Internal {
            message: format!("Failed to build SSA: {}", e),
        })?;
        let tracker = ValueTracker::new(&cfg, &ssa, hbc);
        let call_analysis = CallSiteAnalysis::analyze(&cfg);

        // Helper to resolve a register value before an instruction to a tracked value
        let resolve_reg = |reg: u8, pc: InstructionIndex| -> crate::analysis::value_tracking::types::TrackedValue {
            if let Some(val) = ssa.get_value_before_instruction(reg, pc) {
                tracker.get_value(val)
            } else {
                crate::analysis::value_tracking::types::TrackedValue::Unknown
            }
        };

        // Helper to flatten property access into a dotted path and detect if rooted at global
        fn flatten_property_path(
            v: &crate::analysis::value_tracking::types::TrackedValue,
        ) -> (bool, Vec<String>) {
            use crate::analysis::value_tracking::types::TrackedValue as TV;
            let mut path = Vec::new();
            let mut cur = v;
            let mut root_global = false;
            loop {
                match cur {
                    TV::PropertyAccess { object, property } => {
                        path.push(property.clone());
                        cur = object.as_ref();
                    }
                    TV::GlobalObject => {
                        root_global = true;
                        break;
                    }
                    _ => break,
                }
            }
            path.reverse();
            (root_global, path)
        }

        // Helper to extract constant string/number
        fn const_string_or_number(
            v: &crate::analysis::value_tracking::types::TrackedValue,
        ) -> Option<String> {
            use crate::analysis::value_tracking::types::{ConstantValue as CV, TrackedValue as TV};
            match v {
                TV::Constant(CV::String(s)) => Some(s.clone()),
                TV::Constant(CV::Number(n)) => {
                    Some(if n.fract() == 0.0 { (*n as i64).to_string() } else { n.to_string() })
                }
                _ => None,
            }
        }

        // Iterate calls in source order by PC
        let mut calls: Vec<(InstructionIndex, &crate::analysis::call_site_analysis::CallSiteInfo)> =
            call_analysis
                .call_sites
                .iter()
                .map(|((_bid, pc), info)| (*pc, info))
                .collect();
        calls.sort_by_key(|(pc, _)| pc.0);

        // Iterate calls and detect __d registrations and require entrypoints
        for (pc, info) in calls {
            // Determine callee
            let callee_tv = resolve_reg(info.callee_register, pc);
            let (is_global, path) = flatten_property_path(&callee_tv);

            // Map to practical name
            let callee_name = if is_global && !path.is_empty() {
                Some(path.last().cloned().unwrap())
            } else {
                None
            };

            // Gather argument values (skip implicit 'this' at index 0)
            let mut arg_vals = Vec::new();
            for (i, reg) in info.argument_registers.iter().enumerate() {
                // 0: this, >=1: actual args
                if i == 0 {
                    continue;
                }
                arg_vals.push(resolve_reg(*reg, pc));
            }

            if let Some(name) = &callee_name {
                match name.as_str() {
                    "__d" => {
                        // Expected args: fn, moduleId, dependencyMap, [verboseName]
                        let mut module = ModuleInfo::default();

                        // module function id if closure
                        if let Some(fn_tv) = arg_vals.get(0) {
                            // Try to inspect def instruction for CreateClosure
                            if let crate::analysis::value_tracking::types::TrackedValue::Parameter { ssa_value: _, .. }
                            | crate::analysis::value_tracking::types::TrackedValue::Phi { ssa_value: _ }
                            | crate::analysis::value_tracking::types::TrackedValue::MutableObject { .. }
                            | crate::analysis::value_tracking::types::TrackedValue::PropertyAccess { .. }
                            | crate::analysis::value_tracking::types::TrackedValue::Constant(_)
                            | crate::analysis::value_tracking::types::TrackedValue::GlobalObject
                            | crate::analysis::value_tracking::types::TrackedValue::Unknown = fn_tv
                            {
                                // Best-effort: walk to SSA def site for the callee register itself
                                if let Some(fn_ssa) = ssa.get_value_before_instruction(info.argument_registers[1], pc) {
                                    let block = &cfg.graph()[fn_ssa.def_site.block_id];
                                    let inst_offset = fn_ssa.def_site.instruction_idx.value()
                                        - block.start_pc().value();
                                    if let Some(instr) = block.instructions().get(inst_offset) {
                                        match &instr.instruction {
                                            UnifiedInstruction::CreateClosure { operand_2, .. }
                                            | UnifiedInstruction::CreateAsyncClosure { operand_2, .. }
                                            | UnifiedInstruction::CreateGeneratorClosure { operand_2, .. } => {
                                                module.function_id = Some(*operand_2 as u32);
                                            }
                                            UnifiedInstruction::CreateClosureLongIndex { operand_2, .. }
                                            | UnifiedInstruction::CreateAsyncClosureLongIndex { operand_2, .. }
                                            | UnifiedInstruction::CreateGeneratorClosureLongIndex { operand_2, .. } => {
                                                module.function_id = Some(*operand_2 as u32);
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }

                        // moduleId (2nd arg)
                        if let Some(id_tv) = arg_vals.get(1) {
                            if let Some(s) = const_string_or_number(id_tv) {
                                if let Ok(n) = s.parse::<u32>() {
                                    module.module_id_u32 = Some(n);
                                } else {
                                    module.module_id_str = Some(s.clone());
                                }
                            }
                        }

                        // dependencyMap (3rd arg)
                        if let Some(dep_tv) = arg_vals.get(2) {
                            // Attempt to extract dependency IDs from array/object literals
                            let deps = extract_dependencies(dep_tv);
                            if !deps.is_empty() {
                                module.dependencies = deps;
                            }
                        }

                        // verboseName (dev bundles)
                        if let Some(vn_tv) = arg_vals.get(3) {
                            if let Some(s) = const_string_or_number(vn_tv) {
                                module.filename = Some(s);
                            }
                        }

                        // Merge into existing list if we can match by function_id or module_id
                        if let Some(fid) = module.function_id {
                            if let Some(existing) = modules.iter_mut().find(|m| m.function_id == Some(fid)) {
                                // Update fields
                                if module.module_id_u32.is_some() { existing.module_id_u32 = module.module_id_u32; }
                                if module.module_id_str.is_some() { existing.module_id_str = module.module_id_str.clone(); }
                                if module.filename.is_some() { existing.filename = module.filename.clone(); }
                            } else {
                                modules.push(module);
                            }
                        } else if module.module_id_u32.is_some() || module.module_id_str.is_some() {
                            // No function id; match by module id if present
                            if let Some(existing) = modules.iter_mut().find(|m| m.module_id_u32 == module.module_id_u32 && m.module_id_str == module.module_id_str) {
                                if module.filename.is_some() { existing.filename = module.filename.clone(); }
                            } else {
                                modules.push(module);
                            }
                        }
                    }

                    "require" | "__r" => {
                        // require(moduleId) entry execution
                        if let Some(first_arg) = arg_vals.get(0) {
                            if let Some(s) = const_string_or_number(first_arg) {
                                detected_entries_ordered.push(s.clone());
                                detected_entries.push(s);
                            }
                        }
                    }

                    _ => {}
                }
            }
        }

        // Additionally detect direct calls to known function IDs in function 0
        if let Ok(instrs0) = hbc.functions.get_instructions(0) {
            let mut entry_fids: Vec<u32> = Vec::new();
            for inst in &instrs0 {
                match &inst.instruction {
                    UnifiedInstruction::CallDirect { operand_2, .. } => {
                        entry_fids.push(*operand_2 as u32)
                    }
                    UnifiedInstruction::CallDirectLongIndex { operand_2, .. } => {
                        entry_fids.push(*operand_2 as u32)
                    }
                    _ => {}
                }
            }
            entry_fids.sort_unstable();
            entry_fids.dedup();
            for m in &mut modules {
                if let Some(fid) = m.function_id {
                    if entry_fids.binary_search(&fid).is_ok() {
                        m.is_entry = true;
                    }
                }
            }
        }
    }

    detected_entries.sort();
    detected_entries.dedup();

    // Last __r is typically the main entry; keep ordered list in report via entrypoints order
    // For now we keep only unique set in entrypoints field, but could expose main separately later

    // Mark modules with matching module IDs as entries
    for m in &mut modules {
        let id_str = m
            .module_id_u32
            .map(|n| n.to_string())
            .or_else(|| m.module_id_str.clone());
        if let Some(id) = id_str {
            if detected_entries.binary_search(&id).is_ok() {
                m.is_entry = true;
            }
        }
    }

    let stats = PackageStats {
        function_count: hbc.functions.count(),
        string_count: hbc.header.string_count(),
    };

    Ok(PackageReport {
        stats,
        modules,
        entrypoints: detected_entries,
    })
}

/// Extract dependency IDs from a tracked value representing Metro dependency map
fn extract_dependencies(
    v: &crate::analysis::value_tracking::types::TrackedValue,
) -> Vec<String> {
    use crate::analysis::value_tracking::types::{ConstantValue as CV, MutationKind, TrackedValue as TV};

    // Constant array literal
    if let TV::Constant(CV::ArrayLiteral(elems)) = v {
        return elems
            .iter()
            .filter_map(|c| match c {
                CV::String(s) => Some(s.clone()),
                CV::Number(n) => Some(if n.fract() == 0.0 {
                    (*n as i64).to_string()
                } else {
                    n.to_string()
                }),
                _ => None,
            })
            .collect();
    }

    // Mutable array constructed then filled
    if let TV::MutableObject {
        base_type: crate::analysis::value_tracking::types::ObjectBaseType::Array { .. },
        mutations,
        ..
    } = v
    {
        let mut max_index = 0usize;
        let mut items: std::collections::BTreeMap<usize, String> = std::collections::BTreeMap::new();
        for m in mutations {
            if let MutationKind::ArraySet { index, value } = &m.kind {
                // index is Constant(Number)
                if let TV::Constant(CV::Number(n)) = index.as_ref() {
                    let idx = *n as usize;
                    max_index = max_index.max(idx);
                    // value constant
                    if let Some(s) = match value.as_ref() {
                        TV::Constant(CV::String(s)) => Some(s.clone()),
                        TV::Constant(CV::Number(n)) => Some(if n.fract() == 0.0 {
                            (*n as i64).to_string()
                        } else {
                            n.to_string()
                        }),
                        _ => None,
                    } {
                        items.insert(idx, s);
                    }
                }
            }
        }
        if !items.is_empty() {
            let mut out = Vec::with_capacity(max_index + 1);
            for i in 0..=max_index {
                if let Some(s) = items.get(&i) {
                    out.push(s.clone());
                } else {
                    out.push("".to_string()); // placeholder for holes
                }
            }
            // Trim trailing empty slots
            while let Some(last) = out.last() {
                if last.is_empty() {
                    out.pop();
                } else {
                    break;
                }
            }
            return out;
        }
    }

    // Object format: numeric keys and optional paths field
    if let TV::MutableObject {
        base_type: crate::analysis::value_tracking::types::ObjectBaseType::Object,
        mutations,
        ..
    } = v
    {
        let mut pairs: Vec<(usize, String)> = Vec::new();
        for m in mutations {
            if let MutationKind::PropertySet { key, value } = &m.kind {
                // key may be string constant numeric (e.g., "0")
                if let TV::Constant(CV::String(k)) = key.as_ref() {
                    if let Ok(idx) = k.parse::<usize>() {
                        if let Some(s) = match value.as_ref() {
                            TV::Constant(CV::String(s)) => Some(s.clone()),
                            TV::Constant(CV::Number(n)) => Some(if n.fract() == 0.0 {
                                (*n as i64).to_string()
                            } else {
                                n.to_string()
                            }),
                            _ => None,
                        } {
                            pairs.push((idx, s));
                        }
                    }
                }
            }
        }
        if !pairs.is_empty() {
            pairs.sort_by_key(|(i, _)| *i);
            return pairs.into_iter().map(|(_, s)| s).collect();
        }
    }

    Vec::new()
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
