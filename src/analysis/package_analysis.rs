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
use crate::{
    analysis::{call_site_analysis::CallSiteAnalysis, value_tracker::ValueTracker},
    cfg::{ssa, Cfg},
    hbc::InstructionIndex,
};
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
    /// Dependency graph metrics
    pub depth: Option<usize>,
    pub indegree: Option<usize>,
    /// Simple cluster label based on heuristics (entry/core/feature/deep/shared)
    pub cluster: Option<String>,
    /// Assigned dependency cluster id
    pub cluster_id: Option<usize>,
    /// Suggested output path (proposed directory/file mapping for decompilation)
    pub suggested_path: Option<String>,
}

/// High-level package report
#[derive(Debug, Clone, Serialize, Default)]
pub struct PackageReport {
    pub stats: PackageStats,
    pub modules: Vec<ModuleInfo>,
    /// Detected entry modules by module ID (string or numeric rendered as string)
    pub entrypoints: Vec<String>,
    /// Discovered dependency clusters
    pub clusters: Vec<DependencyCluster>,
    /// Statistics about clustering coverage
    pub cluster_stats: ClusterStats,
    /// Strength/edge metrics to help tune clustering
    pub strength_metrics: StrengthMetrics,
    /// The likely main entry module (last __r in suffix), if found
    pub main: Option<String>,
    /// Cluster summary for quick inspection
    pub cluster_summary: ClusterSummary,
    /// Summary of proposed directory structure
    pub directory_summary: DirectorySummary,
    /// High-level status of extracted dependency and layout signals
    pub graph_summary: GraphSummary,
}

/// Analyze a Hermes HBC file for Metro bundle structure and module graph.
///
/// Note: This is an initial scaffold that returns file-level stats and an empty
/// module list when no bundle pattern is detected. Module extraction will be
/// implemented incrementally using the documented patterns.
pub fn analyze_package(hbc: &HbcFile) -> DecompilerResult<PackageReport> {
    // Resolve string table once
    let strings = hbc.strings.extract_strings().unwrap_or_else(|_| Vec::new());

    // Build initial module list from CommonJS table (if present)
    let mut modules: Vec<ModuleInfo> = Vec::new();
    for entry in &hbc.cjs_modules.entries {
        let filename = if !hbc.cjs_modules.is_static {
            strings.get(entry.symbol_id as usize).cloned()
        } else {
            None
        };
        let stable_id = filename
            .as_deref()
            .map(normalize_module_name)
            .unwrap_or_else(|| format!("fn:{}", entry.offset));
        modules.push(ModuleInfo {
            function_id: Some(entry.offset),
            module_id_str: Some(stable_id),
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
        let resolve_reg = |reg: u8,
                           pc: InstructionIndex|
         -> crate::analysis::value_tracking::types::TrackedValue {
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
                TV::Constant(CV::Number(n)) => Some(if n.fract() == 0.0 {
                    (*n as i64).to_string()
                } else {
                    n.to_string()
                }),
                _ => None,
            }
        }

        // Iterate calls in source order by PC
        let mut calls: Vec<(
            InstructionIndex,
            &crate::analysis::call_site_analysis::CallSiteInfo,
        )> = call_analysis
            .call_sites
            .iter()
            .map(|((_bid, pc), info)| (*pc, info))
            .collect();
        calls.sort_by_key(|(pc, _)| pc.0);

        // Helper: summarize a tracked value for logging
        fn summarize_tracked_value(
            v: &crate::analysis::value_tracking::types::TrackedValue,
        ) -> String {
            use crate::analysis::value_tracking::types::{
                ConstantValue as CV, ObjectBaseType, TrackedValue as TV,
            };
            match v {
                TV::Constant(CV::String(s)) => format!(
                    "Constant(String:{})",
                    s.chars().take(40).collect::<String>()
                ),
                TV::Constant(CV::Number(n)) => format!("Constant(Number:{})", n),
                TV::Constant(CV::Boolean(b)) => format!("Constant(Boolean:{})", b),
                TV::Constant(CV::Null) => "Constant(Null)".to_string(),
                TV::Constant(CV::Undefined) => "Constant(Undefined)".to_string(),
                TV::Constant(CV::ArrayLiteral(elems)) => {
                    format!("Constant(Array:{} elements)", elems.len())
                }
                TV::Constant(CV::ObjectLiteral(props)) => {
                    format!("Constant(Object:{} props)", props.len())
                }
                TV::Parameter { index, .. } => format!("Parameter#index{}", index),
                TV::GlobalObject => "GlobalObject".to_string(),
                TV::PropertyAccess { property, .. } => format!("PropertyAccess(.{})", property),
                TV::MutableObject {
                    base_type,
                    mutations,
                    ..
                } => {
                    let kind = match base_type {
                        ObjectBaseType::Object => "Object",
                        ObjectBaseType::Array { .. } => "Array",
                        ObjectBaseType::ObjectWithBuffer => "ObjectWithBuffer",
                        ObjectBaseType::Function => "Function",
                    };
                    format!("Mutable({} with {} mutations)", kind, mutations.len())
                }
                _ => "Unknown".to_string(),
            }
        }

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
                        // Strict pattern: require >= 3 args and CreateClosure first arg; moduleId must be constant string/number
                        if arg_vals.len() < 3 {
                            log::warn!(
                                "__d call at pc {} has {} args (expected >=3) — skipping",
                                pc.0,
                                arg_vals.len()
                            );
                            continue;
                        }

                        // module function id must be CreateClosure target
                        let mut function_id: Option<u32> = None;
                        if let Some(fn_ssa) =
                            ssa.get_value_before_instruction(info.argument_registers[1], pc)
                        {
                            let block = &cfg.graph()[fn_ssa.def_site.block_id];
                            let inst_offset =
                                fn_ssa.def_site.instruction_idx.value() - block.start_pc().value();
                            if let Some(instr) = block.instructions().get(inst_offset) {
                                match &instr.instruction {
                                    UnifiedInstruction::CreateClosure { operand_2, .. }
                                    | UnifiedInstruction::CreateAsyncClosure {
                                        operand_2, ..
                                    }
                                    | UnifiedInstruction::CreateGeneratorClosure {
                                        operand_2,
                                        ..
                                    } => {
                                        function_id = Some(*operand_2 as u32);
                                    }
                                    UnifiedInstruction::CreateClosureLongIndex {
                                        operand_2,
                                        ..
                                    }
                                    | UnifiedInstruction::CreateAsyncClosureLongIndex {
                                        operand_2,
                                        ..
                                    }
                                    | UnifiedInstruction::CreateGeneratorClosureLongIndex {
                                        operand_2,
                                        ..
                                    } => {
                                        function_id = Some(*operand_2 as u32);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        if function_id.is_none() {
                            log::warn!(
                                "__d call at pc {} — first arg is not a CreateClosure; skipping",
                                pc.0
                            );
                            continue;
                        }

                        // moduleId (2nd arg) must be constant string/number
                        let module_id_str = match const_string_or_number(&arg_vals[1]) {
                            Some(s) => s,
                            None => {
                                log::warn!("__d call at pc {} — second arg (moduleId) not constant; skipping", pc.0);
                                continue;
                            }
                        };

                        // dependencyMap (3rd arg) — extract and log details
                        let dep_tv = &arg_vals[2];
                        let deps = extract_dependencies(dep_tv);
                        log::info!(
                            "__d at pc {}: fn_id={}, module_id={}, dep_arg={}, deps_extracted={}{}",
                            pc.0,
                            function_id.unwrap(),
                            module_id_str,
                            summarize_tracked_value(dep_tv),
                            deps.len(),
                            if deps.is_empty() {
                                "".to_string()
                            } else {
                                format!(
                                    ", sample=[{}]",
                                    deps.iter().take(5).cloned().collect::<Vec<_>>().join(", ")
                                )
                            }
                        );

                        let mut module = ModuleInfo::default();
                        module.function_id = function_id;
                        if let Ok(n) = module_id_str.parse::<u32>() {
                            module.module_id_u32 = Some(n);
                        } else {
                            module.module_id_str = Some(module_id_str.clone());
                        }
                        if !deps.is_empty() {
                            module.dependencies = deps;
                        }

                        // verboseName (dev bundles)
                        if let Some(vn_tv) = arg_vals.get(3) {
                            if let Some(s) = const_string_or_number(vn_tv) {
                                module.filename = Some(s);
                            }
                        }

                        // Merge by function_id/module_id
                        if let Some(fid) = module.function_id {
                            if let Some(existing) =
                                modules.iter_mut().find(|m| m.function_id == Some(fid))
                            {
                                if module.module_id_u32.is_some() {
                                    existing.module_id_u32 = module.module_id_u32;
                                }
                                if module.module_id_str.is_some() {
                                    existing.module_id_str = module.module_id_str.clone();
                                }
                                if module.filename.is_some() {
                                    existing.filename = module.filename.clone();
                                }
                                if !module.dependencies.is_empty() {
                                    existing.dependencies = module.dependencies.clone();
                                }
                            } else {
                                modules.push(module);
                            }
                        } else if module.module_id_u32.is_some() || module.module_id_str.is_some() {
                            if let Some(existing) = modules.iter_mut().find(|m| {
                                m.module_id_u32 == module.module_id_u32
                                    && m.module_id_str == module.module_id_str
                            }) {
                                if module.filename.is_some() {
                                    existing.filename = module.filename.clone();
                                }
                                if !module.dependencies.is_empty() {
                                    existing.dependencies = module.dependencies.clone();
                                }
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

            // No fallback handling — only process explicit __d calls
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

    analyze_commonjs_dependencies(hbc, &mut modules);

    detected_entries.sort();
    detected_entries.dedup();

    // Mark modules with matching module IDs as entries
    for m in &mut modules {
        if let Some(id) = module_key(m) {
            if detected_entries.binary_search(&id).is_ok() {
                m.is_entry = true;
            }
        }
    }

    let mut main_entry = detected_entries_ordered.last().cloned();
    if detected_entries.is_empty() {
        let (inferred_entries, inferred_main) = infer_commonjs_entrypoints(&mut modules);
        if !inferred_entries.is_empty() {
            detected_entries = inferred_entries;
        }
        if main_entry.is_none() {
            main_entry = inferred_main;
        }
    }

    let stats = PackageStats {
        function_count: hbc.functions.count(),
        string_count: hbc.header.string_count(),
    };

    let mut report = PackageReport {
        stats,
        modules,
        entrypoints: detected_entries,
        clusters: Vec::new(),
        cluster_stats: ClusterStats::default(),
        strength_metrics: StrengthMetrics::default(),
        main: main_entry,
        cluster_summary: ClusterSummary::default(),
        directory_summary: DirectorySummary::default(),
        graph_summary: GraphSummary::default(),
    };
    annotate_dependency_clusters(&mut report);
    compute_dependency_clusters(&mut report);
    compute_cluster_stats(&mut report);
    compute_cluster_summary(&mut report);
    compute_directory_plan(&mut report);
    compute_graph_summary(&mut report);
    Ok(report)
}

fn normalize_module_name(name: &str) -> String {
    let trimmed = name.trim().trim_matches('"').replace('\\', "/");
    let normalized = trimmed.strip_prefix("./").unwrap_or(trimmed.as_str());
    normalized.to_string()
}

fn module_key(m: &ModuleInfo) -> Option<String> {
    m.module_id_u32
        .map(|n| n.to_string())
        .or_else(|| m.module_id_str.clone())
        .or_else(|| m.filename.as_deref().map(normalize_module_name))
        .or_else(|| m.function_id.map(|id| format!("fn:{id}")))
}

fn tracked_value_as_string_or_number(
    value: &crate::analysis::value_tracking::types::TrackedValue,
) -> Option<String> {
    use crate::analysis::value_tracking::types::{ConstantValue as CV, TrackedValue as TV};

    match value {
        TV::Constant(CV::String(s)) => Some(s.clone()),
        TV::Constant(CV::Number(n)) => Some(if n.fract() == 0.0 {
            (*n as i64).to_string()
        } else {
            n.to_string()
        }),
        _ => None,
    }
}

fn analyze_commonjs_dependencies(hbc: &HbcFile, modules: &mut [ModuleInfo]) {
    use std::collections::HashSet;

    if modules.is_empty() {
        return;
    }

    let alias_map = build_commonjs_alias_map(modules);

    for module in modules.iter_mut() {
        let Some(function_id) = module.function_id else {
            continue;
        };

        let mut cfg = Cfg::new(hbc, function_id);
        cfg.build();
        let Ok(ssa) = ssa::construct_ssa(&cfg, function_id) else {
            log::warn!(
                "Skipping CommonJS dependency analysis for function {} due to SSA failure",
                function_id
            );
            continue;
        };
        let tracker = ValueTracker::new(&cfg, &ssa, hbc);
        let call_analysis = CallSiteAnalysis::analyze(&cfg);

        let resolve_reg = |reg: u8,
                           pc: InstructionIndex|
         -> crate::analysis::value_tracking::types::TrackedValue {
            if let Some(val) = ssa.get_value_before_instruction(reg, pc) {
                tracker.get_value(val)
            } else {
                crate::analysis::value_tracking::types::TrackedValue::Unknown
            }
        };

        let mut seen = HashSet::new();
        let mut dependencies = module.dependencies.clone();
        for dep in &dependencies {
            seen.insert(dep.clone());
        }

        let mut calls: Vec<(
            InstructionIndex,
            &crate::analysis::call_site_analysis::CallSiteInfo,
        )> = call_analysis
            .call_sites
            .iter()
            .map(|((_bid, pc), info)| (*pc, info))
            .collect();
        calls.sort_by_key(|(pc, _)| pc.0);

        for (pc, info) in calls {
            let callee = resolve_reg(info.callee_register, pc);
            let is_require = matches!(
                callee,
                crate::analysis::value_tracking::types::TrackedValue::Parameter { index: 2, .. }
            );
            if !is_require {
                continue;
            }

            let mut actual_args = info.argument_registers.iter().skip(1);
            let Some(first_arg_reg) = actual_args.next() else {
                continue;
            };
            let Some(specifier) =
                tracked_value_as_string_or_number(&resolve_reg(*first_arg_reg, pc))
            else {
                continue;
            };

            let resolved = resolve_commonjs_dependency(&specifier, &alias_map);
            if seen.insert(resolved.clone()) {
                dependencies.push(resolved);
            }
        }

        module.dependencies = dependencies;
    }
}

fn build_commonjs_alias_map(modules: &[ModuleInfo]) -> std::collections::HashMap<String, String> {
    use std::collections::HashMap;
    use std::path::Path;

    let mut alias_candidates: HashMap<String, Vec<String>> = HashMap::new();
    for module in modules {
        let Some(key) = module_key(module) else {
            continue;
        };
        let Some(filename) = module.filename.as_deref() else {
            alias_candidates.entry(key.clone()).or_default().push(key);
            continue;
        };

        let normalized = normalize_module_name(filename);
        let path = Path::new(&normalized);
        let mut aliases = vec![normalized.clone(), format!("./{normalized}")];

        if let Some(file_name) = path.file_name().and_then(|value| value.to_str()) {
            aliases.push(file_name.to_string());
            aliases.push(format!("./{file_name}"));
        }
        if let Some(stem) = path.file_stem().and_then(|value| value.to_str()) {
            aliases.push(stem.to_string());
            aliases.push(format!("./{stem}"));
        }
        if let Some(parent) = path.parent().and_then(|value| value.to_str()) {
            if !parent.is_empty() && parent != "." {
                if let Some(stem) = path.file_stem().and_then(|value| value.to_str()) {
                    let joined = format!("{parent}/{stem}");
                    aliases.push(joined.clone());
                    aliases.push(format!("./{joined}"));
                }
                if path.file_name().and_then(|value| value.to_str()) == Some("index.js") {
                    aliases.push(parent.to_string());
                    aliases.push(format!("./{parent}"));
                }
            }
        }

        aliases.sort();
        aliases.dedup();
        for alias in aliases {
            alias_candidates.entry(alias).or_default().push(key.clone());
        }
    }

    alias_candidates
        .into_iter()
        .filter_map(|(alias, targets)| {
            if targets.len() == 1 {
                Some((alias, targets.into_iter().next().unwrap()))
            } else {
                None
            }
        })
        .collect()
}

fn resolve_commonjs_dependency(
    specifier: &str,
    alias_map: &std::collections::HashMap<String, String>,
) -> String {
    let normalized = normalize_module_name(specifier);
    if let Some(resolved) = alias_map.get(&normalized) {
        return resolved.clone();
    }
    if let Some(resolved) = alias_map.get(&format!("./{normalized}")) {
        return resolved.clone();
    }
    format!("external:{normalized}")
}

fn infer_commonjs_entrypoints(modules: &mut [ModuleInfo]) -> (Vec<String>, Option<String>) {
    use std::collections::{HashMap, HashSet};

    let module_keys: HashSet<String> = modules.iter().filter_map(module_key).collect();
    if module_keys.is_empty() {
        return (Vec::new(), None);
    }

    let mut indegree: HashMap<String, usize> = HashMap::new();
    for module in modules.iter() {
        for dependency in &module.dependencies {
            if module_keys.contains(dependency) {
                *indegree.entry(dependency.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut candidates: Vec<(usize, String)> = modules
        .iter()
        .filter_map(|module| {
            let key = module_key(module)?;
            let incoming = indegree.get(&key).copied().unwrap_or(0);
            if incoming == 0 {
                Some((entrypoint_score(module), key))
            } else {
                None
            }
        })
        .collect();

    if candidates.is_empty() {
        if let Some(best) = modules
            .iter()
            .filter_map(|module| module_key(module).map(|key| (entrypoint_score(module), key)))
            .max_by_key(|(score, _)| *score)
        {
            for module in modules.iter_mut() {
                module.is_entry = module_key(module).as_deref() == Some(best.1.as_str());
            }
            return (vec![best.1.clone()], Some(best.1));
        }
        return (Vec::new(), None);
    }

    candidates.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
    let ordered_entries: Vec<String> = candidates.iter().map(|(_, key)| key.clone()).collect();
    for module in modules.iter_mut() {
        if let Some(key) = module_key(module) {
            module.is_entry = ordered_entries.iter().any(|entry| entry == &key);
        }
    }
    (ordered_entries.clone(), ordered_entries.first().cloned())
}

fn entrypoint_score(module: &ModuleInfo) -> usize {
    let mut score = module.dependencies.len();
    if let Some(filename) = module.filename.as_deref() {
        let normalized = normalize_module_name(filename);
        if normalized.ends_with("index.js") {
            score += 100;
        }
        if normalized.ends_with("main.js") || normalized.ends_with("app.js") {
            score += 80;
        }
    }
    if module.function_id.is_some() {
        score += 1;
    }
    score
}

/// Extract dependency IDs from a tracked value representing Metro dependency map
fn extract_dependencies(v: &crate::analysis::value_tracking::types::TrackedValue) -> Vec<String> {
    use crate::analysis::value_tracking::types::{
        ConstantValue as CV, MutationKind, TrackedValue as TV,
    };

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
        let mut items: std::collections::BTreeMap<usize, String> =
            std::collections::BTreeMap::new();
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

/// Compute simple dependency clustering and annotate modules with graph metrics
fn annotate_dependency_clusters(report: &mut PackageReport) {
    // Build id key for modules and adjacency
    use std::collections::{HashMap, HashSet, VecDeque};
    let mut key_to_index: HashMap<String, usize> = HashMap::new();
    for (i, m) in report.modules.iter().enumerate() {
        if let Some(k) = module_key(m) {
            key_to_index.insert(k, i);
        }
    }

    let mut indegree: HashMap<String, usize> = HashMap::new();
    let mut adj: HashMap<String, Vec<String>> = HashMap::new();
    for m in &report.modules {
        if let Some(k) = module_key(m) {
            let mut edges = Vec::new();
            for d in &m.dependencies {
                edges.push(d.clone());
                *indegree.entry(d.clone()).or_insert(0) += 1;
            }
            adj.insert(k, edges);
        }
    }

    // BFS from entrypoints to compute depth
    let mut depth_map: HashMap<String, usize> = HashMap::new();
    let mut q: VecDeque<(String, usize)> = VecDeque::new();
    let mut seen: HashSet<String> = HashSet::new();
    for e in &report.entrypoints {
        q.push_back((e.clone(), 0));
        seen.insert(e.clone());
        depth_map.insert(e.clone(), 0);
    }
    while let Some((u, d)) = q.pop_front() {
        if let Some(children) = adj.get(&u) {
            for v in children {
                if !seen.contains(v) {
                    seen.insert(v.clone());
                    depth_map.insert(v.clone(), d + 1);
                    q.push_back((v.clone(), d + 1));
                }
            }
        }
    }

    // Apply metrics + clustering
    for m in &mut report.modules {
        if let Some(k) = module_key(m) {
            m.depth = depth_map.get(&k).copied();
            m.indegree = indegree.get(&k).copied();
            // Cluster label based on depth and indegree
            let cluster = if m.is_entry {
                Some("entry".to_string())
            } else if let Some(dep) = m.depth {
                if dep == 1 {
                    Some("core".to_string())
                } else if dep <= 3 {
                    Some("feature".to_string())
                } else {
                    Some("deep".to_string())
                }
            } else {
                None
            };

            // Shared override for high indegree
            let high_in = m.indegree.unwrap_or(0) >= 25;
            m.cluster = if high_in {
                Some("shared".to_string())
            } else {
                cluster
            };
        }
    }
}

/// Compute cluster groupings based on pairwise connection strengths.
fn compute_dependency_clusters(report: &mut PackageReport) {
    use std::collections::{HashMap, HashSet};

    // Build adjacency
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for m in &report.modules {
        if let Some(k) = module_key(m) {
            let set: HashSet<String> = m.dependencies.iter().cloned().collect();
            deps.insert(k, set);
        }
    }

    // Compute indegree for hub filtering and consumers (reverse adjacency)
    let mut indeg: HashMap<String, usize> = HashMap::new();
    for (k, dset) in &deps {
        for d in dset {
            *indeg.entry(d.clone()).or_insert(0) += 1;
        }
        indeg.entry(k.clone()).or_insert(0); // ensure present
    }
    let mut consumers: HashMap<String, HashSet<String>> = HashMap::new();
    for (k, dset) in &deps {
        for d in dset {
            consumers.entry(d.clone()).or_default().insert(k.clone());
        }
    }

    // Helpers
    fn jaccard_filtered(
        a: &HashSet<String>,
        b: &HashSet<String>,
        indeg: &HashMap<String, usize>,
        hub_cutoff: usize,
    ) -> f32 {
        let filt = |s: &HashSet<String>| -> HashSet<String> {
            s.iter()
                .filter(|x| indeg.get(*x).copied().unwrap_or(0) < hub_cutoff)
                .cloned()
                .collect()
        };
        let af = filt(a);
        let bf = filt(b);
        if af.is_empty() && bf.is_empty() {
            return 0.0;
        }
        let inter = af.intersection(&bf).count() as f32;
        let uni = (af.len() + bf.len()).saturating_sub(inter as usize) as f32;
        if uni <= 0.0 {
            0.0
        } else {
            inter / uni
        }
    }

    // Pairwise strengths + metrics
    let mut strength: HashMap<(String, String), f32> = HashMap::new();
    let mut total_pairs: usize = 0;
    let mut pairs_with_edge: usize = 0;
    let mut edges_ge_threshold: usize = 0;
    let mut sum_strength_edges: f64 = 0.0;
    let mut sum_jaccard_deps_over_pairs: f64 = 0.0;
    let mut sum_jaccard_cons_over_pairs: f64 = 0.0;
    let mut bidir_count: usize = 0;
    let mut unidir_count: usize = 0;
    let mut trans_count: usize = 0;
    let mut top_edges: Vec<EdgeDebug> = Vec::new();
    let keys: Vec<String> = deps.keys().cloned().collect();
    for i in 0..keys.len() {
        for j in (i + 1)..keys.len() {
            let a = &keys[i];
            let b = &keys[j];
            let da = deps.get(a).unwrap();
            let db = deps.get(b).unwrap();
            total_pairs += 1;

            let mut s = 0.0f32;
            // Bidirectional / unidirectional
            let a_to_b = da.contains(b);
            let b_to_a = db.contains(a);
            if a_to_b && b_to_a {
                s += 0.6;
                bidir_count += 1;
            } else if a_to_b || b_to_a {
                s += 0.35;
                unidir_count += 1;
            }

            // Shared dependencies (Jaccard, hub-filtered)
            let j_dep = jaccard_filtered(da, db, &indeg, 100);
            s += 0.5 * j_dep as f32;
            sum_jaccard_deps_over_pairs += j_dep as f64;

            // Shared consumers (reverse adjacency)
            let empty_hs: HashSet<String> = HashSet::new();
            let ca_ref = consumers.get(a).unwrap_or(&empty_hs);
            let cb_ref = consumers.get(b).unwrap_or(&empty_hs);
            let j_con = jaccard_filtered(ca_ref, cb_ref, &indeg, 100);
            s += 0.3 * j_con as f32;
            sum_jaccard_cons_over_pairs += j_con as f64;

            // Transitive hint
            let mut trans = false;
            for x in da {
                if let Some(dx) = deps.get(x) {
                    if dx.contains(b) {
                        trans = true;
                        break;
                    }
                }
            }
            if !trans {
                for x in db {
                    if let Some(dx) = deps.get(x) {
                        if dx.contains(a) {
                            trans = true;
                            break;
                        }
                    }
                }
            }
            if trans {
                s += 0.1;
                trans_count += 1;
            }

            if s > 0.0 {
                let s_cap = s.min(1.0);
                strength.insert((a.clone(), b.clone()), s_cap);
                strength.insert((b.clone(), a.clone()), s_cap);
                pairs_with_edge += 1;
                sum_strength_edges += s_cap as f64;
                if s_cap >= 0.55 {
                    edges_ge_threshold += 1;
                }
                // Maintain top edges list (capped)
                if top_edges.len() < 100 {
                    top_edges.push(EdgeDebug {
                        a: a.clone(),
                        b: b.clone(),
                        strength: s_cap,
                        bidirectional: a_to_b && b_to_a,
                        unidirectional: a_to_b ^ b_to_a,
                        jaccard_deps: j_dep,
                        jaccard_consumers: j_con,
                        transitive: trans,
                    });
                } else {
                    // Replace worst if this is better
                    if let Some((idx, _)) = top_edges
                        .iter()
                        .enumerate()
                        .min_by(|a, b| a.1.strength.partial_cmp(&b.1.strength).unwrap())
                    {
                        if s_cap > top_edges[idx].strength {
                            top_edges[idx] = EdgeDebug {
                                a: a.clone(),
                                b: b.clone(),
                                strength: s_cap,
                                bidirectional: a_to_b && b_to_a,
                                unidirectional: a_to_b ^ b_to_a,
                                jaccard_deps: j_dep,
                                jaccard_consumers: j_con,
                                transitive: trans,
                            };
                        }
                    }
                }
            }
        }
    }

    // Union-Find
    struct UF {
        parent: HashMap<String, String>,
        size: HashMap<String, usize>,
    }
    impl UF {
        fn new(nodes: &[String]) -> Self {
            let mut parent = HashMap::new();
            let mut size = HashMap::new();
            for n in nodes {
                parent.insert(n.clone(), n.clone());
                size.insert(n.clone(), 1);
            }
            Self { parent, size }
        }
        fn find(&mut self, x: &String) -> String {
            let p = self.parent.get(x).cloned().unwrap();
            if &p != x {
                let root = self.find(&p);
                self.parent.insert(x.clone(), root.clone());
                root
            } else {
                p
            }
        }
        fn union(&mut self, a: &String, b: &String) {
            let ra = self.find(a);
            let rb = self.find(b);
            if ra == rb {
                return;
            }
            let sa = *self.size.get(&ra).unwrap();
            let sb = *self.size.get(&rb).unwrap();
            if sa >= sb {
                self.parent.insert(rb.clone(), ra.clone());
                *self.size.get_mut(&ra).unwrap() += sb;
            } else {
                self.parent.insert(ra.clone(), rb.clone());
                *self.size.get_mut(&rb).unwrap() += sa;
            }
        }
    }

    let threshold = 0.55f32;
    let mut uf = UF::new(&keys);
    for ((a, b), w) in &strength {
        if *w >= threshold {
            uf.union(a, b);
        }
    }

    // Build clusters using the union-find with unions applied
    let mut root_to_members: HashMap<String, Vec<String>> = HashMap::new();
    for k in &keys {
        let r = uf.find(k);
        root_to_members.entry(r).or_default().push(k.clone());
    }

    // Compute cluster metrics and patterns
    let mut clusters: Vec<DependencyCluster> = Vec::new();
    let mut key_to_cluster: HashMap<String, usize> = HashMap::new();
    let mut cid = 0usize;
    for (_root, members) in root_to_members.into_iter() {
        let size = members.len();
        // Skip singletons from clustering to avoid noise
        if size <= 1 {
            continue;
        }
        // Average internal strength
        let mut total = 0.0f32;
        let mut cnt = 0usize;
        for i in 0..members.len() {
            for j in (i + 1)..members.len() {
                if let Some(w) = strength.get(&(members[i].clone(), members[j].clone())) {
                    total += *w;
                    cnt += 1;
                }
            }
        }
        let avg = if cnt > 0 { total / (cnt as f32) } else { 0.0 };

        // External edges count
        let set: HashSet<String> = members.iter().cloned().collect();
        let mut ext_edges = 0usize;
        for m in &members {
            if let Some(dset) = deps.get(m) {
                for d in dset {
                    if !set.contains(d) {
                        ext_edges += 1;
                    }
                }
            }
        }

        // For size-2 clusters, require strong pair strength; otherwise skip
        if size == 2 {
            let w = strength
                .get(&(members[0].clone(), members[1].clone()))
                .copied()
                .unwrap_or(0.0);
            if w < 0.75 {
                continue;
            }
        }

        // Pattern detection
        let pattern = detect_cluster_pattern(&members, &deps);

        // Initial cluster
        clusters.push(DependencyCluster {
            id: cid,
            modules: members.clone(),
            size,
            avg_strength: avg,
            pattern,
            external_edges: ext_edges,
        });
        for m in &clusters.last().unwrap().modules {
            key_to_cluster.insert(m.clone(), cid);
        }
        cid += 1;
    }

    // Expansion: attach neighbors with aggregate strength >= 1.0 to clusters of size>=3
    for cl in &mut clusters {
        if cl.size < 3 {
            continue;
        }
        let current_members: HashSet<String> = cl.modules.iter().cloned().collect();
        let mut to_add: Vec<String> = Vec::new();
        for n in &keys {
            if current_members.contains(n) {
                continue;
            }
            if key_to_cluster.contains_key(n) {
                continue;
            }
            let s_sum: f32 = cl
                .modules
                .iter()
                .map(|m| *strength.get(&(n.clone(), m.clone())).unwrap_or(&0.0))
                .sum();
            if s_sum >= 1.0 {
                to_add.push(n.clone());
            }
        }
        for n in to_add {
            cl.modules.push(n.clone());
            cl.size += 1;
            key_to_cluster.insert(n, cl.id);
        }
    }

    // Annotate modules with cluster id and optionally override cluster label
    for m in &mut report.modules {
        let key = module_key(m);
        if let Some(k) = key {
            if let Some(id) = key_to_cluster.get(&k) {
                m.cluster_id = Some(*id);
            }
        }
    }

    report.clusters = clusters;
    // Strength metrics
    let avg_strength = if pairs_with_edge > 0 {
        (sum_strength_edges / pairs_with_edge as f64) as f32
    } else {
        0.0
    };
    let avg_j_dep = if total_pairs > 0 {
        (sum_jaccard_deps_over_pairs / total_pairs as f64) as f32
    } else {
        0.0
    };
    let avg_j_con = if total_pairs > 0 {
        (sum_jaccard_cons_over_pairs / total_pairs as f64) as f32
    } else {
        0.0
    };
    // Sort descending top edges
    top_edges.sort_by(|a, b| b.strength.partial_cmp(&a.strength).unwrap());
    report.strength_metrics = StrengthMetrics {
        threshold,
        total_pairs,
        pairs_with_edge,
        edges_ge_threshold,
        avg_strength_over_edges: avg_strength,
        avg_jaccard_deps_over_pairs: avg_j_dep,
        avg_jaccard_consumers_over_pairs: avg_j_con,
        bidirectional_count: bidir_count,
        unidirectional_count: unidir_count,
        transitive_count: trans_count,
        top_edges,
    };
}

fn detect_cluster_pattern(
    members: &[String],
    deps: &std::collections::HashMap<String, std::collections::HashSet<String>>,
) -> String {
    use std::collections::HashSet;
    let set: HashSet<String> = members.iter().cloned().collect();
    if members.len() == 1 {
        return "island".into();
    }
    if members.len() == 2 {
        return "pair".into();
    }
    // Build internal degrees
    let mut deg_in = vec![0usize; members.len()];
    let mut deg_out = vec![0usize; members.len()];
    for (i, m) in members.iter().enumerate() {
        if let Some(ds) = deps.get(m) {
            for d in ds {
                if set.contains(d) {
                    deg_out[i] += 1;
                    if let Some(j) = members.iter().position(|x| x == d) {
                        deg_in[j] += 1;
                    }
                }
            }
        }
    }
    let n = members.len();
    let sum_in: usize = deg_in.iter().sum();
    let sum_out: usize = deg_out.iter().sum();
    let max_in = *deg_in.iter().max().unwrap_or(&0);
    // Tight cluster: sizeable and well-connected
    if n > 5 && (sum_in + sum_out) as f32 / (n as f32) >= 4.0 {
        return "tight".into();
    }
    // Star: one node with high indegree relative to others
    if max_in >= (n.saturating_sub(1)).max(1) / 2 {
        return "star".into();
    }
    // Chain: internal degrees mostly <=2
    let deg_ok = deg_in
        .iter()
        .zip(deg_out.iter())
        .filter(|(a, b)| (**a + **b) <= 2)
        .count();
    if deg_ok == n {
        return "chain".into();
    }
    // Island: small external connectivity implied by caller
    "cluster".into()
}

fn compute_cluster_stats(report: &mut PackageReport) {
    use std::collections::HashMap;

    let mut key_to_cluster: HashMap<String, usize> = HashMap::new();
    for m in &report.modules {
        if let (Some(k), Some(cid)) = (module_key(m), m.cluster_id) {
            key_to_cluster.insert(k, cid);
        }
    }

    let mut total_edges = 0usize;
    let mut clustered_edges = 0usize;
    for m in &report.modules {
        let src_cid = m.cluster_id;
        // let _src_key = module_key(m);
        for d in &m.dependencies {
            total_edges += 1;
            if let (Some(scid), Some(&dcid)) = (src_cid, key_to_cluster.get(d)) {
                if scid == dcid {
                    clustered_edges += 1;
                }
            }
        }
    }
    let ratio = if total_edges > 0 {
        (clustered_edges as f32) / (total_edges as f32)
    } else {
        0.0
    };
    report.cluster_stats = ClusterStats {
        total_edges,
        clustered_edges,
        clustered_ratio: ratio,
    };
}

fn compute_cluster_summary(report: &mut PackageReport) {
    let mut summary = ClusterSummary::default();
    summary.total = report.clusters.len();
    let mut buckets = ClusterSizeBuckets::default();
    for c in &report.clusters {
        match c.size {
            0..=1 => {}
            2..=4 => buckets.s02_04 += 1,
            5..=9 => buckets.s05_09 += 1,
            10..=19 => buckets.s10_19 += 1,
            _ => buckets.s20_plus += 1,
        }
        match c.pattern.as_str() {
            "tight" => summary.tight += 1,
            "star" => summary.star += 1,
            "chain" => summary.chain += 1,
            _ => summary.cluster += 1,
        }
    }
    summary.buckets = buckets;
    report.cluster_summary = summary;
}

fn compute_directory_plan(report: &mut PackageReport) {
    use std::collections::HashMap;

    let candidate_paths: Vec<String> = report
        .modules
        .iter()
        .map(|module| suggest_module_path(module, report.main.as_deref()))
        .collect();
    let mut path_counts: HashMap<String, usize> = HashMap::new();
    for path in &candidate_paths {
        *path_counts.entry(path.clone()).or_insert(0) += 1;
    }

    let mut dir_counts: HashMap<String, usize> = HashMap::new();
    for (module, candidate_path) in report.modules.iter_mut().zip(candidate_paths) {
        let path = if path_counts.get(&candidate_path).copied().unwrap_or(0) > 1 {
            disambiguate_suggested_path(&candidate_path, module)
        } else {
            candidate_path
        };
        module.suggested_path = Some(path.clone());
        let dir = path
            .rsplit_once('/')
            .map(|(parent, _)| parent.to_string())
            .unwrap_or_else(|| ".".to_string());
        *dir_counts.entry(dir).or_insert(0) += 1;
    }

    let mut dirs: Vec<DirBucket> = dir_counts
        .into_iter()
        .map(|(dir, count)| DirBucket { dir, count })
        .collect();
    dirs.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.dir.cmp(&b.dir)));
    report.directory_summary = DirectorySummary { dirs };
}

fn suggest_module_path(module: &ModuleInfo, main: Option<&str>) -> String {
    if let Some(filename) = module.filename.as_deref() {
        let normalized = normalize_module_name(filename);

        if module.is_entry || main == module_key(module).as_deref() {
            return format!("app/{normalized}");
        }
        if module.indegree.unwrap_or(0) >= 3 {
            return format!("shared/{normalized}");
        }
        if let Some(cluster_id) = module.cluster_id {
            return format!("features/cluster_{cluster_id}/{normalized}");
        }
        return format!("modules/{normalized}");
    }

    let key = module_key(module).unwrap_or_else(|| "unknown".to_string());
    if module.is_entry || main == Some(key.as_str()) {
        return format!("app/{key}.js");
    }
    if let Some(cluster_id) = module.cluster_id {
        return format!("features/cluster_{cluster_id}/{key}.js");
    }
    if module.indegree.unwrap_or(0) >= 3 {
        return format!("shared/{key}.js");
    }
    format!("modules/{key}.js")
}

fn disambiguate_suggested_path(candidate_path: &str, module: &ModuleInfo) -> String {
    let suffix = module
        .function_id
        .map(|function_id| format!("fn{function_id}"))
        .or_else(|| {
            module_key(module).map(|key| {
                key.chars()
                    .map(|ch| match ch {
                        'a'..='z' | 'A'..='Z' | '0'..='9' => ch,
                        _ => '_',
                    })
                    .collect::<String>()
            })
        })
        .unwrap_or_else(|| "module".to_string());

    if let Some((base, ext)) = candidate_path.rsplit_once('.') {
        return format!("{base}__{suffix}.{ext}");
    }
    format!("{candidate_path}__{suffix}")
}

fn compute_graph_summary(report: &mut PackageReport) {
    use std::collections::HashSet;

    let module_keys: HashSet<String> = report.modules.iter().filter_map(module_key).collect();
    let mut total_edges = 0usize;
    let mut resolved_edges = 0usize;
    for module in &report.modules {
        for dependency in &module.dependencies {
            total_edges += 1;
            if module_keys.contains(dependency) {
                resolved_edges += 1;
            }
        }
    }

    let meaningful_paths = report.modules.iter().any(has_filename_derived_path_signal);

    report.graph_summary = GraphSummary {
        extracted: !report.modules.is_empty(),
        resolved_edges,
        unresolved_edges: total_edges.saturating_sub(resolved_edges),
        meaningful_clusters: !report.clusters.is_empty(),
        meaningful_paths,
    };
}

fn has_filename_derived_path_signal(module: &ModuleInfo) -> bool {
    let Some(filename) = module.filename.as_deref() else {
        return false;
    };
    let Some(suggested_path) = module.suggested_path.as_deref() else {
        return false;
    };

    let normalized = normalize_module_name(filename);
    suggested_path == format!("app/{normalized}")
        || suggested_path == format!("modules/{normalized}")
        || suggested_path == format!("shared/{normalized}")
        || suggested_path.starts_with("features/")
            && suggested_path.ends_with(&format!("/{normalized}"))
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
#[derive(Debug, Clone, Serialize, Default)]
pub struct DependencyCluster {
    pub id: usize,
    pub modules: Vec<String>,
    pub size: usize,
    pub avg_strength: f32,
    pub pattern: String,
    pub external_edges: usize,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct ClusterStats {
    pub total_edges: usize,
    pub clustered_edges: usize,
    pub clustered_ratio: f32,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct StrengthMetrics {
    pub threshold: f32,
    pub total_pairs: usize,
    pub pairs_with_edge: usize,
    pub edges_ge_threshold: usize,
    pub avg_strength_over_edges: f32,
    pub avg_jaccard_deps_over_pairs: f32,
    pub avg_jaccard_consumers_over_pairs: f32,
    pub bidirectional_count: usize,
    pub unidirectional_count: usize,
    pub transitive_count: usize,
    pub top_edges: Vec<EdgeDebug>,
}

#[derive(Debug, Clone, Serialize)]
pub struct EdgeDebug {
    pub a: String,
    pub b: String,
    pub strength: f32,
    pub bidirectional: bool,
    pub unidirectional: bool,
    pub jaccard_deps: f32,
    pub jaccard_consumers: f32,
    pub transitive: bool,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct ClusterSummary {
    pub total: usize,
    pub buckets: ClusterSizeBuckets,
    pub tight: usize,
    pub star: usize,
    pub chain: usize,
    pub cluster: usize,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct ClusterSizeBuckets {
    pub s02_04: usize,
    pub s05_09: usize,
    pub s10_19: usize,
    pub s20_plus: usize,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct DirectorySummary {
    pub dirs: Vec<DirBucket>,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct DirBucket {
    pub dir: String,
    pub count: usize,
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct GraphSummary {
    pub extracted: bool,
    pub resolved_edges: usize,
    pub unresolved_edges: usize,
    pub meaningful_clusters: bool,
    pub meaningful_paths: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn module_with_filename(filename: &str, function_id: u32) -> ModuleInfo {
        ModuleInfo {
            function_id: Some(function_id),
            module_id_str: Some(normalize_module_name(filename)),
            filename: Some(filename.to_string()),
            ..Default::default()
        }
    }

    #[test]
    fn commonjs_alias_map_resolves_index_directory_specifiers() {
        let modules = vec![module_with_filename("dir/index.js", 1)];
        let alias_map = build_commonjs_alias_map(&modules);

        assert_eq!(
            alias_map.get("dir").map(String::as_str),
            Some("dir/index.js")
        );
        assert_eq!(
            alias_map.get("./dir").map(String::as_str),
            Some("dir/index.js")
        );
        assert_eq!(
            resolve_commonjs_dependency("./dir", &alias_map),
            "dir/index.js"
        );
    }

    #[test]
    fn directory_plan_is_stable_for_equal_bucket_counts() {
        let mut report = PackageReport {
            modules: vec![
                ModuleInfo {
                    is_entry: true,
                    ..module_with_filename("index.js", 1)
                },
                module_with_filename("math.js", 2),
            ],
            main: Some("index.js".to_string()),
            ..Default::default()
        };

        compute_directory_plan(&mut report);

        let dirs: Vec<&str> = report
            .directory_summary
            .dirs
            .iter()
            .map(|bucket| bucket.dir.as_str())
            .collect();
        assert_eq!(dirs, vec!["app", "modules"]);
    }

    #[test]
    fn directory_plan_preserves_nested_relative_paths() {
        let mut report = PackageReport {
            modules: vec![
                module_with_filename("foo/index.js", 1),
                module_with_filename("bar/index.js", 2),
            ],
            ..Default::default()
        };

        compute_directory_plan(&mut report);

        let paths: Vec<&str> = report
            .modules
            .iter()
            .filter_map(|module| module.suggested_path.as_deref())
            .collect();
        assert!(paths.contains(&"modules/foo/index.js"));
        assert!(paths.contains(&"modules/bar/index.js"));
    }

    #[test]
    fn graph_summary_treats_zero_edge_module_reports_as_extracted_but_fallback_only() {
        let mut report = PackageReport {
            modules: vec![ModuleInfo {
                function_id: Some(123),
                module_id_str: Some("fn:123".to_string()),
                ..Default::default()
            }],
            ..Default::default()
        };

        compute_directory_plan(&mut report);
        compute_graph_summary(&mut report);

        assert!(report.graph_summary.extracted);
        assert_eq!(report.graph_summary.resolved_edges, 0);
        assert_eq!(report.graph_summary.unresolved_edges, 0);
        assert!(!report.graph_summary.meaningful_paths);
        assert_eq!(
            report.modules[0].suggested_path.as_deref(),
            Some("modules/fn:123.js")
        );
    }
}
