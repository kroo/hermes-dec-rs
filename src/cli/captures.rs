//! Batch static capture navigation. Slot equality is not runtime scope identity.
use crate::bundle::{export_function_fragments, operands};
use crate::{DecompilerError, DecompilerResult, HbcFile};
use serde::Serialize;
use serde_json::{json, Value};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::Write;
use std::path::Path;

const CANDIDATE_CAP: usize = 20;
const EDGE_CAP: usize = 100;
const CONTEXT: usize = 3;
const INSTRUCTION_BYTES: usize = 256;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

fn bounds(functions: &[u32], depth: usize, limit: usize, max_bytes: usize) -> DecompilerResult<()> {
    if functions.is_empty() {
        return Err(error("captures requires explicitly selected functions"));
    }
    if !(1..=8).contains(&depth)
        || !(1..=1000).contains(&limit)
        || !(1..=16_777_216).contains(&max_bytes)
    {
        return Err(error(
            "captures bounds: depth 1..8, limit 1..1000, max_bytes 1..16777216",
        ));
    }
    Ok(())
}

#[derive(Clone, Serialize)]
struct Edge {
    parent_function_id: u32,
    child_function_id: u32,
    pc: u32,
    env_register: i64,
    kind: &'static str,
}

#[derive(Clone, Serialize)]
struct Site {
    function_id: u32,
    pc: u32,
    env_register: i64,
    slot: u32,
    opcode: &'static str,
}

#[derive(Default)]
struct Index {
    reads: Vec<Site>,
    stores: BTreeMap<(u32, u32), Vec<Site>>,
    parents: BTreeMap<u32, Vec<Edge>>,
}

impl Index {
    fn build(hbc: &HbcFile<'_>, functions: &BTreeSet<u32>) -> DecompilerResult<Self> {
        let mut index = Self::default();
        for id in 0..hbc.functions.count() {
            for instruction in hbc.functions.get_instructions_ref(id)? {
                let opcode = instruction.instruction.name();
                let read = matches!(opcode, "LoadFromEnvironment" | "LoadFromEnvironmentL");
                let store = matches!(
                    opcode,
                    "StoreToEnvironment"
                        | "StoreToEnvironmentL"
                        | "StoreNPToEnvironment"
                        | "StoreNPToEnvironmentL"
                );
                let closure = matches!(
                    opcode,
                    "CreateClosure"
                        | "CreateClosureLongIndex"
                        | "CreateGeneratorClosure"
                        | "CreateGeneratorClosureLongIndex"
                        | "CreateAsyncClosure"
                        | "CreateAsyncClosureLongIndex"
                        | "CreateGenerator"
                        | "CreateGeneratorLongIndex"
                );
                if !(store || closure || read && functions.contains(&id)) {
                    continue;
                }
                let o = operands(&instruction.instruction)?;
                let pc = instruction.offset.value();
                if closure {
                    let child =
                        u32::try_from(o[2]).map_err(|_| error("Invalid closure function ID"))?;
                    if child >= hbc.functions.count() {
                        return Err(error(format!("Unknown closure function {child}")));
                    }
                    index.parents.entry(child).or_default().push(Edge {
                        parent_function_id: id,
                        child_function_id: child,
                        pc,
                        env_register: o[1],
                        kind: opcode,
                    });
                } else {
                    let site = Site {
                        function_id: id,
                        pc,
                        env_register: o[usize::from(read)],
                        slot: u32::try_from(o[if read { 2 } else { 1 }])
                            .map_err(|_| error("Invalid environment slot"))?,
                        opcode,
                    };
                    if read {
                        index.reads.push(site);
                    } else {
                        index.stores.entry((id, site.slot)).or_default().push(site);
                    }
                }
            }
        }
        Ok(index)
    }

    fn ancestry(&self, function: u32, depth: usize) -> Ancestry {
        let mut result = Ancestry::default();
        result.paths.insert(function, Vec::new());
        let mut queue = VecDeque::from([function]);
        while let Some(child) = queue.pop_front() {
            let path = result.paths[&child].clone();
            for edge in self.parents.get(&child).into_iter().flatten() {
                if path.len() == depth {
                    result.depth_truncated |= !result.paths.contains_key(&edge.parent_function_id);
                    continue;
                }
                result.edge_total += 1;
                if result.edges.len() < EDGE_CAP {
                    result.edges.push(edge.clone());
                }
                if let std::collections::btree_map::Entry::Vacant(entry) =
                    result.paths.entry(edge.parent_function_id)
                {
                    let mut next = path.clone();
                    next.push(edge.clone());
                    entry.insert(next);
                    queue.push_back(edge.parent_function_id);
                }
            }
        }
        result.ranked_functions = result.paths.keys().copied().collect();
        result
            .ranked_functions
            .sort_by_key(|id| (result.paths[id].len(), *id));
        result
    }
}

#[derive(Default)]
struct Ancestry {
    paths: BTreeMap<u32, Vec<Edge>>,
    ranked_functions: Vec<u32>,
    edges: Vec<Edge>,
    edge_total: usize,
    depth_truncated: bool,
}

struct SourceIndex {
    code: String,
    markers: Vec<(u32, usize)>,
    positions: BTreeMap<u32, usize>,
}

impl SourceIndex {
    fn new(id: u32, code: String) -> DecompilerResult<Self> {
        let allocator = oxc_allocator::Allocator::default();
        let parsed =
            oxc_parser::Parser::new(&allocator, &code, oxc_span::SourceType::default()).parse();
        if !parsed.errors.is_empty() {
            return Err(error("Invalid inspection JavaScript"));
        }
        let comments: BTreeSet<_> = parsed
            .program
            .comments
            .iter()
            .map(|c| c.span.start as usize)
            .collect();
        let prefix = format!("// HBC function {id}, PC ");
        let mut markers = Vec::new();
        let mut positions = BTreeMap::new();
        let mut offset = 0;
        for line in code.split_inclusive('\n') {
            if comments.contains(&offset) {
                if let Some(value) = line.strip_prefix(&prefix) {
                    let pc = value
                        .trim()
                        .parse::<u32>()
                        .map_err(|_| error("Invalid numeric PC marker"))?;
                    if markers.last().is_some_and(|&(last, _)| last >= pc) {
                        return Err(error("Unordered or duplicate PC markers"));
                    }
                    positions.insert(pc, markers.len());
                    markers.push((pc, offset));
                }
            }
            offset += line.len();
        }
        Ok(Self {
            code,
            markers,
            positions,
        })
    }

    fn excerpt(&self, pc: u32) -> Value {
        let Some(&position) = self.positions.get(&pc) else {
            return json!({"missing": true, "javascript": null, "standalone": false});
        };
        let start = position.saturating_sub(CONTEXT);
        let end = (position + CONTEXT + 1).min(self.markers.len());
        let mut blocks = Vec::new();
        let mut text = String::new();
        let mut bytes_truncated = false;
        for i in start..end {
            let block = &self.code
                [self.markers[i].1..self.markers.get(i + 1).map_or(self.code.len(), |m| m.1)];
            let mut length = block.len().min(INSTRUCTION_BYTES);
            while !block.is_char_boundary(length) {
                length -= 1;
            }
            let truncated = length < block.len();
            bytes_truncated |= truncated;
            let excerpt_start = text.len();
            text.push_str(&block[..length]);
            blocks.push(json!({"pc": self.markers[i].0, "original_bytes": block.len(),
                "truncated": truncated, "excerpt_span": {"start": excerpt_start, "end": text.len()}}));
        }
        json!({"missing": false, "javascript": text, "instructions": blocks,
            "instruction_count": end - start, "context_each_side": CONTEXT,
            "excerpt_span_units": "UTF-8 bytes; start inclusive, end exclusive in javascript",
            "bytes_per_instruction": INSTRUCTION_BYTES, "bytes_truncated": bytes_truncated,
            "truncated": bytes_truncated || start > 0 || end < self.markers.len(), "standalone": false})
    }
}

/// In-memory equivalent for synthetic HBC tests; performs the same batch lowering.
pub fn report_hbc(
    hbc: &HbcFile<'_>,
    functions: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    bounds(functions, depth, limit, max_bytes)?;
    let functions: BTreeSet<_> = functions.iter().copied().collect();
    for &id in &functions {
        if id >= hbc.functions.count() {
            return Err(error(format!("Unknown function {id}")));
        }
    }
    let index = Index::build(hbc, &functions)?;
    let reads: Vec<_> = index.reads.iter().skip(offset).take(limit).collect();
    let mut ancestors = BTreeMap::new();
    let mut needed = BTreeSet::new();
    let mut plans = Vec::new();
    for read in &reads {
        let ancestry = ancestors
            .entry(read.function_id)
            .or_insert_with(|| index.ancestry(read.function_id, depth));
        needed.insert(read.function_id);
        let mut selected = Vec::new();
        let mut total = 0;
        for &id in &ancestry.ranked_functions {
            let stores = index
                .stores
                .get(&(id, read.slot))
                .map_or(&[][..], Vec::as_slice);
            total += stores.len();
            for store in stores.iter().take(CANDIDATE_CAP - selected.len()) {
                needed.insert(store.function_id);
                selected.push(store);
            }
        }
        plans.push((selected, total));
    }
    let ids: Vec<_> = needed.into_iter().collect();
    let sources: BTreeMap<_, _> = export_function_fragments(hbc, &ids)?
        .into_iter()
        .map(|(id, code)| SourceIndex::new(id, code).map(|source| (id, source)))
        .collect::<DecompilerResult<_>>()?;
    let mut rows = Vec::new();
    let mut missing_total = 0;
    let mut candidate_total = 0;
    let mut candidate_returned = 0;
    let mut missing_excerpt_total = 0;
    for (read, (stores, total)) in reads.iter().zip(plans) {
        let ancestry = &ancestors[&read.function_id];
        candidate_total += total;
        candidate_returned += stores.len();
        missing_total += usize::from(total == 0);
        let excerpt = sources[&read.function_id].excerpt(read.pc);
        missing_excerpt_total += usize::from(excerpt["missing"] == true);
        let mut candidates = Vec::new();
        for store in stores {
            let same = store.function_id == read.function_id;
            let relative = if !same {
                "different_function"
            } else if store.pc < read.pc {
                "before_read_pc"
            } else {
                "after_read_pc"
            };
            let snippet = sources[&store.function_id].excerpt(store.pc);
            missing_excerpt_total += usize::from(snippet["missing"] == true);
            candidates.push(json!({"kind": "static_candidate", "write": store,
                "relation": if same {"same_function"} else {"static_ancestor"},
                "relative_pc": relative, "pc_delta": if same {Some(i64::from(store.pc) - i64::from(read.pc))} else {None},
                "env_register_matches_read": if same {Some(store.env_register == read.env_register)} else {None},
                "closure_path": ancestry.paths[&store.function_id], "excerpt": snippet}));
        }
        rows.push(json!({"read": read, "excerpt": excerpt, "candidates": candidates,
            "candidate_total": total, "candidate_returned": candidates.len(),
            "candidates_truncated": total > CANDIDATE_CAP, "missing": total == 0, "unresolved": true,
            "ancestor_total": ancestry.paths.len() - 1, "depth_truncated": ancestry.depth_truncated,
            "closure_edges": ancestry.edges, "closure_edges_total": ancestry.edge_total,
            "closure_edges_truncated": ancestry.edge_total > EDGE_CAP}));
    }
    let consumed = offset.min(index.reads.len()) + rows.len();
    let result = json!({"schema_version": 1, "source": "export_function_fragments",
        "functions": functions, "depth": depth, "limit": limit, "offset": offset,
        "total": index.reads.len(), "returned": rows.len(),
        "next_offset": if consumed < index.reads.len() {Some(consumed)} else {None},
        "truncated": offset > 0 || consumed < index.reads.len(),
        "candidate_cap": CANDIDATE_CAP, "edge_cap": EDGE_CAP,
        "candidate_ranking": "shortest witness length, function ID, PC; same-function first; navigation only, not runtime likelihood",
        "page_totals": {"candidate_total": candidate_total, "candidate_returned": candidate_returned,
            "missing_total": missing_total, "unresolved_total": rows.len(), "missing_excerpt_total": missing_excerpt_total},
        "reads": rows, "standalone": false, "authoritative_lexical_resolution": false,
        "closure_path_policy": "one shortest witness per ancestor; alternatives in closure_edges",
        "closure_path_order": "read function outward to ancestor",
        "notes": "Inspection JS and static navigation candidates only, not evaluated values or bindings. Equal slots in different environment registers or runtime scopes remain ambiguous. Relative PC is not execution order: same-function writes before and after a read are candidates. Generator wrapper/body edges are included; CallDirect is excluded. No lexical levels or names are inferred. Page totals count candidate occurrences per read, within depth; alternative edges are capped independently of witnesses."});
    let mut bytes = serde_json::to_vec(&result).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > max_bytes {
        return Err(error(format!(
            "captures report exceeds max_bytes ({max_bytes}); reduce limit or page further"
        )));
    }
    Ok(bytes)
}

/// Serialize completely before stdout, including byte-budget and input validation.
pub fn report(
    input: &Path,
    functions: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    bounds(functions, depth, limit, max_bytes)?;
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    report_hbc(&hbc, functions, depth, limit, offset, max_bytes)
}

pub fn run(
    input: &Path,
    functions: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<()> {
    let bytes = report(input, functions, depth, limit, offset, max_bytes)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeric_marker_index_ignores_literal_comments_and_preserves_utf8() {
        let code = format!(
            "// HBC function 0, PC 1\nr[0] = `fake\n// HBC function 0, PC 9\n`;\n// HBC function 0, PC 10\nr[2] = '{}';\n// HBC function 0, PC 11\nr[3] = r[2];\n",
            "界".repeat(200)
        );
        let source = SourceIndex::new(0, code).unwrap();
        assert_eq!(
            source.markers.iter().map(|m| m.0).collect::<Vec<_>>(),
            [1, 10, 11]
        );
        assert_eq!(source.excerpt(9)["missing"], true);
        assert_eq!(source.excerpt(0)["missing"], true);
        let excerpt = source.excerpt(10);
        assert_eq!(excerpt["bytes_truncated"], true);
        let javascript = excerpt["javascript"].as_str().unwrap();
        let mut previous_end = 0;
        for instruction in excerpt["instructions"].as_array().unwrap() {
            assert!(instruction.get("javascript").is_none());
            let start = instruction["excerpt_span"]["start"].as_u64().unwrap() as usize;
            let end = instruction["excerpt_span"]["end"].as_u64().unwrap() as usize;
            assert_eq!(start, previous_end);
            assert!(end - start <= INSTRUCTION_BYTES);
            assert!(javascript.is_char_boundary(start) && javascript.is_char_boundary(end));
            let pc = instruction["pc"].as_u64().unwrap() as u32;
            let original_start = source.markers[source.positions[&pc]].1;
            assert_eq!(
                &javascript[start..end],
                &source.code[original_start..original_start + end - start]
            );
            previous_end = end;
        }
        assert_eq!(previous_end, javascript.len());
        assert!(excerpt["javascript"]
            .as_str()
            .unwrap()
            .contains("r[3] = r[2]"));
    }
}
