//! Static captured-slot candidates, not runtime lexical resolution.
use crate::bundle::{export_function_fragments, operands};
use crate::{DecompilerError, DecompilerResult, HbcFile};
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::Write;
use std::path::Path;

const CONTEXT: usize = 8;
const BLOCK_BYTES: usize = 512;
const EDGE_LIMIT: usize = 100;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

#[derive(Clone, Serialize)]
struct Edge {
    parent_function_id: u32,
    child_function_id: u32,
    pc: u32,
    env_register: i64,
    kind: &'static str,
}

struct WriteCandidate {
    function: u32,
    pc: u32,
    env: i64,
    opcode: &'static str,
}

fn excerpt(code: &str, id: u32, pc: u32) -> DecompilerResult<serde_json::Value> {
    let prefix = format!("// HBC function {id}, PC ");
    let mut markers = Vec::new();
    let mut offset = 0;
    for line in code.split_inclusive('\n') {
        if let Some(value) = line.strip_prefix(&prefix) {
            if let Ok(pc) = value.trim().parse::<u32>() {
                markers.push((pc, offset));
            }
        }
        offset += line.len();
    }
    let index = markers
        .iter()
        .position(|(value, _)| *value == pc)
        .ok_or_else(|| error(format!("Missing JS PC comment for function {id}, PC {pc}")))?;
    let start = index.saturating_sub(CONTEXT);
    let end = (index + CONTEXT + 1).min(markers.len());
    let mut text = String::new();
    let mut bytes_truncated = false;
    for i in start..end {
        let block = &code[markers[i].1..markers.get(i + 1).map_or(code.len(), |m| m.1)];
        let mut length = block.len().min(BLOCK_BYTES);
        while !block.is_char_boundary(length) {
            length -= 1;
        }
        text.push_str(&block[..length]);
        if length < block.len() {
            text.push_str("\n// [instruction JS truncated]\n");
            bytes_truncated = true;
        }
    }
    Ok(json!({
        "javascript": text, "first_pc": markers[start].0,
        "last_pc": markers[end - 1].0, "instruction_count": end - start,
        "total_instructions": markers.len(), "context_each_side": CONTEXT,
        "truncated": bytes_truncated || start > 0 || end < markers.len(),
        "bytes_truncated": bytes_truncated, "standalone": false
    }))
}

/// Emit bounded JSON describing possible writes in static closure ancestors.
/// This function reads the input and writes only to stdout, never to a file.
pub fn run(
    input: &Path,
    function: u32,
    slot: u32,
    depth: usize,
    limit: usize,
) -> DecompilerResult<()> {
    if !(1..=8).contains(&depth) || !(1..=100).contains(&limit) {
        return Err(error("--depth must be 1..8 and --limit must be 1..100"));
    }
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    if function >= hbc.functions.count() {
        return Err(error(format!("Unknown function {function}")));
    }
    let mut parents: BTreeMap<u32, Vec<Edge>> = BTreeMap::new();
    for id in 0..hbc.functions.count() {
        for instruction in hbc.functions.get_instructions_ref(id)? {
            let kind = instruction.instruction.name();
            if !matches!(
                kind,
                "CreateClosure"
                    | "CreateClosureLongIndex"
                    | "CreateGeneratorClosure"
                    | "CreateGeneratorClosureLongIndex"
                    | "CreateAsyncClosure"
                    | "CreateAsyncClosureLongIndex"
                    | "CreateGenerator"
                    | "CreateGeneratorLongIndex"
            ) {
                continue;
            }
            let o = operands(&instruction.instruction)?;
            let child = u32::try_from(o[2]).map_err(|_| error("Invalid closure function ID"))?;
            if child >= hbc.functions.count() {
                return Err(error(format!("Unknown closure function {child}")));
            }
            parents.entry(child).or_default().push(Edge {
                parent_function_id: id,
                child_function_id: child,
                pc: instruction.offset.value(),
                env_register: o[1],
                kind,
            });
        }
    }
    // Keep one shortest witness per ancestor, rather than enumerating exponentially
    // many capture paths. The edge list separately preserves alternative sites.
    let mut paths: BTreeMap<u32, Vec<Edge>> = BTreeMap::new();
    paths.insert(function, Vec::new());
    let mut queue = VecDeque::from([function]);
    let mut edges = Vec::new();
    let mut depth_truncated = false;
    while let Some(child) = queue.pop_front() {
        let path = paths[&child].clone();
        for edge in parents.get(&child).into_iter().flatten() {
            if path.len() == depth {
                depth_truncated |= !paths.contains_key(&edge.parent_function_id);
                continue;
            }
            edges.push(edge.clone());
            if let std::collections::btree_map::Entry::Vacant(entry) =
                paths.entry(edge.parent_function_id)
            {
                let mut next = path.clone();
                next.push(edge.clone());
                entry.insert(next);
                queue.push_back(edge.parent_function_id);
            }
        }
    }
    paths.remove(&function);
    let mut selected = Vec::new();
    let mut total = 0usize;
    for &id in paths.keys() {
        for instruction in hbc.functions.get_instructions_ref(id)? {
            let opcode = instruction.instruction.name();
            if !matches!(
                opcode,
                "StoreToEnvironment"
                    | "StoreToEnvironmentL"
                    | "StoreNPToEnvironment"
                    | "StoreNPToEnvironmentL"
            ) {
                continue;
            }
            let o = operands(&instruction.instruction)?;
            if o[1] != i64::from(slot) {
                continue;
            }
            total += 1;
            if selected.len() < limit {
                selected.push(WriteCandidate {
                    function: id,
                    pc: instruction.offset.value(),
                    env: o[0],
                    opcode,
                });
            }
        }
    }
    let ids: Vec<_> = selected
        .iter()
        .map(|c| c.function)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let fragments: BTreeMap<_, _> = export_function_fragments(&hbc, &ids)?.into_iter().collect();
    let mut candidates = Vec::new();
    for c in selected {
        candidates.push(json!({
            "kind": "static_candidate", "ancestor_function_id": c.function,
            "pc": c.pc, "env_register": c.env, "slot": slot, "opcode": c.opcode,
            "closure_path": paths[&c.function],
            "excerpt": excerpt(&fragments[&c.function], c.function, c.pc)?
        }));
    }
    let result = json!({
        "schema_version": 1, "function_id": function, "slot": slot,
        "depth": depth, "limit": limit, "total": total,
        "returned": candidates.len(), "truncated": total > limit,
        "ancestor_total": paths.len(), "depth_truncated": depth_truncated,
        "closure_edges_total": edges.len(), "closure_edges_truncated": edges.len() > EDGE_LIMIT,
        "closure_edges": edges.into_iter().take(EDGE_LIMIT).collect::<Vec<_>>(),
        "closure_path_order": "requested function outward to ancestor",
        "closure_path_policy": "one shortest static witness; alternative capture sites in closure_edges",
        "authoritative_lexical_resolution": false, "candidates": candidates,
        "notes": "Static candidates, not values or automatic naming. The same slot number can refer to different runtime environments. Branches, environment-register reassignment, multiple capture sites and nested GetEnvironment levels matter. CallDirect is not lexical ancestry. Excerpts are bounded decompiled JS, not standalone programs; they do not establish execution order or runtime environment identity. Total counts unique writes only within the requested ancestor depth."
    });
    let mut stdout = std::io::stdout().lock();
    serde_json::to_writer(&mut stdout, &result).map_err(|e| error(e.to_string()))?;
    writeln!(stdout)?;
    Ok(())
}
