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
    /// Dependency graph metrics
    pub depth: Option<usize>,
    pub indegree: Option<usize>,
    /// Simple cluster label based on heuristics (entry/core/feature/deep/shared)
    pub cluster: Option<String>,
    /// Assigned dependency cluster id
    pub cluster_id: Option<usize>,
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

        // Helper: summarize a tracked value for logging
        fn summarize_tracked_value(
            v: &crate::analysis::value_tracking::types::TrackedValue,
        ) -> String {
            use crate::analysis::value_tracking::types::{ConstantValue as CV, ObjectBaseType, TrackedValue as TV};
            match v {
                TV::Constant(CV::String(s)) => format!("Constant(String:{})", s.chars().take(40).collect::<String>()),
                TV::Constant(CV::Number(n)) => format!("Constant(Number:{})", n),
                TV::Constant(CV::Boolean(b)) => format!("Constant(Boolean:{})", b),
                TV::Constant(CV::Null) => "Constant(Null)".to_string(),
                TV::Constant(CV::Undefined) => "Constant(Undefined)".to_string(),
                TV::Constant(CV::ArrayLiteral(elems)) => format!("Constant(Array:{} elements)", elems.len()),
                TV::Constant(CV::ObjectLiteral(props)) => format!("Constant(Object:{} props)", props.len()),
                TV::Parameter { index, .. } => format!("Parameter#index{}", index),
                TV::GlobalObject => "GlobalObject".to_string(),
                TV::PropertyAccess { property, .. } => format!("PropertyAccess(.{})", property),
                TV::MutableObject { base_type, mutations, .. } => {
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
                            log::warn!("__d call at pc {} has {} args (expected >=3) — skipping", pc.0, arg_vals.len());
                            continue;
                        }

                        // module function id must be CreateClosure target
                        let mut function_id: Option<u32> = None;
                        if let Some(fn_ssa) = ssa.get_value_before_instruction(info.argument_registers[1], pc) {
                            let block = &cfg.graph()[fn_ssa.def_site.block_id];
                            let inst_offset = fn_ssa.def_site.instruction_idx.value() - block.start_pc().value();
                            if let Some(instr) = block.instructions().get(inst_offset) {
                                match &instr.instruction {
                                    UnifiedInstruction::CreateClosure { operand_2, .. }
                                    | UnifiedInstruction::CreateAsyncClosure { operand_2, .. }
                                    | UnifiedInstruction::CreateGeneratorClosure { operand_2, .. } => {
                                        function_id = Some(*operand_2 as u32);
                                    }
                                    UnifiedInstruction::CreateClosureLongIndex { operand_2, .. }
                                    | UnifiedInstruction::CreateAsyncClosureLongIndex { operand_2, .. }
                                    | UnifiedInstruction::CreateGeneratorClosureLongIndex { operand_2, .. } => {
                                        function_id = Some(*operand_2 as u32);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        if function_id.is_none() {
                            log::warn!("__d call at pc {} — first arg is not a CreateClosure; skipping", pc.0);
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
                            if deps.is_empty() { "".to_string() } else { format!(", sample=[{}]", deps.iter().take(5).cloned().collect::<Vec<_>>().join(", ")) }
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
                            if let Some(existing) = modules.iter_mut().find(|m| m.function_id == Some(fid)) {
                                if module.module_id_u32.is_some() { existing.module_id_u32 = module.module_id_u32; }
                                if module.module_id_str.is_some() { existing.module_id_str = module.module_id_str.clone(); }
                                if module.filename.is_some() { existing.filename = module.filename.clone(); }
                                if !module.dependencies.is_empty() { existing.dependencies = module.dependencies.clone(); }
                            } else {
                                modules.push(module);
                            }
                        } else if module.module_id_u32.is_some() || module.module_id_str.is_some() {
                            if let Some(existing) = modules.iter_mut().find(|m| m.module_id_u32 == module.module_id_u32 && m.module_id_str == module.module_id_str) {
                                if module.filename.is_some() { existing.filename = module.filename.clone(); }
                                if !module.dependencies.is_empty() { existing.dependencies = module.dependencies.clone(); }
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

    let mut report = PackageReport { stats, modules, entrypoints: detected_entries, clusters: Vec::new() };
    annotate_dependency_clusters(&mut report);
    compute_dependency_clusters(&mut report);
    Ok(report)
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

/// Compute simple dependency clustering and annotate modules with graph metrics
fn annotate_dependency_clusters(report: &mut PackageReport) {
    // Build id key for modules and adjacency
    fn module_key(m: &ModuleInfo) -> Option<String> {
        if let Some(n) = m.module_id_u32 { Some(n.to_string()) }
        else { m.module_id_str.clone() }
    }

    use std::collections::{HashMap, VecDeque, HashSet};
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
                if dep == 1 { Some("core".to_string()) }
                else if dep <= 3 { Some("feature".to_string()) }
                else { Some("deep".to_string()) }
            } else { None };

            // Shared override for high indegree
            let high_in = m.indegree.unwrap_or(0) >= 25;
            m.cluster = if high_in { Some("shared".to_string()) } else { cluster };
        }
    }
}

/// Compute cluster groupings based on pairwise connection strengths.
fn compute_dependency_clusters(report: &mut PackageReport) {
    use std::collections::{HashMap, HashSet};

    fn module_key(m: &ModuleInfo) -> Option<String> {
        if let Some(n) = m.module_id_u32 { Some(n.to_string()) } else { m.module_id_str.clone() }
    }

    // Build adjacency
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for m in &report.modules {
        if let Some(k) = module_key(m) {
            let set: HashSet<String> = m.dependencies.iter().cloned().collect();
            deps.insert(k, set);
        }
    }

    // Pairwise strengths
    let mut strength: HashMap<(String, String), f32> = HashMap::new();
    let keys: Vec<String> = deps.keys().cloned().collect();
    for i in 0..keys.len() {
        for j in (i + 1)..keys.len() {
            let a = &keys[i];
            let b = &keys[j];
            let da = deps.get(a).unwrap();
            let db = deps.get(b).unwrap();

            let mut s = 0.0f32;
            let a_to_b = da.contains(b);
            let b_to_a = db.contains(a);
            if a_to_b && b_to_a {
                s += 0.6;
            } else if a_to_b || b_to_a {
                s += 0.3;
            }

            // Shared dependencies
            let shared = da.intersection(db).count() as f32;
            if shared > 0.0 {
                s += (0.1 * shared).min(0.4);
            }

            // Transitive dependencies A->X->B or B->X->A
            let mut trans = false;
            for x in da {
                if let Some(dx) = deps.get(x) {
                    if dx.contains(b) {
                        trans = true;
                        break;
                    }
                }
            }
            for x in db {
                if let Some(dx) = deps.get(x) {
                    if dx.contains(a) {
                        trans = true;
                        break;
                    }
                }
            }
            if trans { s += 0.1; }

            if s > 0.0 {
                strength.insert((a.clone(), b.clone()), s);
                strength.insert((b.clone(), a.clone()), s);
            }
        }
    }

    // Union-Find
    struct UF { parent: HashMap<String, String>, size: HashMap<String, usize> }
    impl UF {
        fn new(nodes: &[String]) -> Self {
            let mut parent = HashMap::new();
            let mut size = HashMap::new();
            for n in nodes { parent.insert(n.clone(), n.clone()); size.insert(n.clone(), 1); }
            Self { parent, size }
        }
        fn find(&mut self, x: &String) -> String {
            let p = self.parent.get(x).cloned().unwrap();
            if &p != x { let root = self.find(&p); self.parent.insert(x.clone(), root.clone()); root } else { p }
        }
        fn union(&mut self, a: &String, b: &String) {
            let ra = self.find(a); let rb = self.find(b);
            if ra == rb { return; }
            let sa = *self.size.get(&ra).unwrap(); let sb = *self.size.get(&rb).unwrap();
            if sa >= sb { self.parent.insert(rb.clone(), ra.clone()); *self.size.get_mut(&ra).unwrap() += sb; }
            else { self.parent.insert(ra.clone(), rb.clone()); *self.size.get_mut(&rb).unwrap() += sa; }
        }
    }

    let threshold = 0.7f32;
    let mut uf = UF::new(&keys);
    for ((a, b), w) in &strength { if *w >= threshold { uf.union(a, b); } }

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
        if size <= 1 { continue; }
        // Average internal strength
        let mut total = 0.0f32; let mut cnt = 0usize;
        for i in 0..members.len() { for j in (i+1)..members.len() {
            if let Some(w) = strength.get(&(members[i].clone(), members[j].clone())) { total += *w; cnt += 1; }
        }}
        let avg = if cnt > 0 { total / (cnt as f32) } else { 0.0 };

        // External edges count
        let set: HashSet<String> = members.iter().cloned().collect();
        let mut ext_edges = 0usize;
        for m in &members {
            if let Some(dset) = deps.get(m) {
                for d in dset {
                    if !set.contains(d) { ext_edges += 1; }
                }
            }
        }

        // For size-2 clusters, require very high pair strength; otherwise skip
        if size == 2 {
            let w = strength.get(&(members[0].clone(), members[1].clone())).copied().unwrap_or(0.0);
            if w < 0.9 { continue; }
        }

        // Pattern detection
        let pattern = detect_cluster_pattern(&members, &deps);

        clusters.push(DependencyCluster { id: cid, modules: members.clone(), size, avg_strength: avg, pattern, external_edges: ext_edges });
        for m in members { key_to_cluster.insert(m, cid); }
        cid += 1;
    }

    // Annotate modules with cluster id and optionally override cluster label
    for m in &mut report.modules {
        let key = module_key(m);
        if let Some(k) = key { if let Some(id) = key_to_cluster.get(&k) { m.cluster_id = Some(*id); } }
    }

    report.clusters = clusters;
}

fn detect_cluster_pattern(members: &[String], deps: &std::collections::HashMap<String, std::collections::HashSet<String>>) -> String {
    use std::collections::HashSet;
    let set: HashSet<String> = members.iter().cloned().collect();
    if members.len() == 1 { return "island".into(); }
    if members.len() == 2 { return "pair".into(); }
    // Build internal degrees
    let mut deg_in = vec![0usize; members.len()];
    let mut deg_out = vec![0usize; members.len()];
    for (i, m) in members.iter().enumerate() {
        if let Some(ds) = deps.get(m) {
            for d in ds {
                if set.contains(d) {
                    deg_out[i] += 1;
                    if let Some(j) = members.iter().position(|x| x == d) { deg_in[j] += 1; }
                }
            }
        }
    }
    let n = members.len();
    let sum_in: usize = deg_in.iter().sum();
    let sum_out: usize = deg_out.iter().sum();
    let max_in = *deg_in.iter().max().unwrap_or(&0);
    // Tight cluster: sizeable and well-connected
    if n > 5 && (sum_in + sum_out) as f32 / (n as f32) >= 4.0 { return "tight".into(); }
    // Star: one node with high indegree relative to others
    if max_in >= (n.saturating_sub(1)).max(1) / 2 { return "star".into(); }
    // Chain: internal degrees mostly <=2
    let deg_ok = deg_in.iter().zip(deg_out.iter()).filter(|(a,b)| (**a + **b) <= 2).count();
    if deg_ok == n { return "chain".into(); }
    // Island: small external connectivity implied by caller
    "cluster".into()
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
