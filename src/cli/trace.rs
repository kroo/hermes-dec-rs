//! Bounded syntactic provenance of physical registers, never runtime values.
use crate::bundle::export_function_fragments;
use crate::{DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::{AssignmentExpression, ComputedMemberExpression, SwitchCase, UpdateExpression};
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::Write;
use std::path::Path;

const SNIPPET_BYTES: usize = 1024;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

fn bounds(depth: usize, limit: usize, max_bytes: usize) -> DecompilerResult<()> {
    if depth > 64 || !(1..=4096).contains(&limit) || !(1..=16_777_216).contains(&max_bytes) {
        return Err(error(
            "trace bounds: depth 0..64, limit 1..4096, max_bytes 1..16777216",
        ));
    }
    Ok(())
}

#[derive(Default)]
struct Syntax<'s> {
    source: &'s str,
    reads: Vec<(u32, Span)>,
    writes: Vec<(u32, Span, Span)>,
    boundaries: BTreeSet<usize>,
}

impl Syntax<'_> {
    fn register(&self, span: Span) -> Option<u32> {
        let text = &self.source[span.start as usize..span.end as usize];
        let digits = text.strip_prefix("r[")?.strip_suffix(']')?;
        if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
            return None;
        }
        digits.parse().ok()
    }
}

impl<'a> Visit<'a> for Syntax<'_> {
    fn visit_computed_member_expression(&mut self, it: &ComputedMemberExpression<'a>) {
        if let Some(register) = self.register(it.span) {
            self.reads.push((register, it.span));
        }
        walk::walk_computed_member_expression(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        let lhs = it.left.span();
        if let Some(register) = self.register(lhs) {
            self.writes.push((register, lhs, it.span));
            // A plain destination is not a use. Compound assignments read it.
            if it.operator.is_assign() {
                self.visit_expression(&it.right);
                return;
            }
        }
        walk::walk_assignment_expression(self, it);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        let lhs = it.argument.span();
        if let Some(register) = self.register(lhs) {
            self.writes.push((register, lhs, it.span));
        }
        walk::walk_update_expression(self, it);
    }

    fn visit_switch_case(&mut self, it: &SwitchCase<'a>) {
        self.boundaries.insert(it.span.start as usize);
        walk::walk_switch_case(self, it);
    }
}

#[derive(Clone, Serialize)]
struct Use {
    register: u32,
    previous_definition_pc: Option<u32>,
    status: &'static str,
}

#[derive(Serialize)]
struct Node {
    pc: u32,
    javascript: String,
    original_bytes: usize,
    snippet_truncated: bool,
    defines: Vec<u32>,
    uses: Vec<Use>,
}

/// Analyze the complete exporter source; only displayed excerpts are truncated.
/// Crate-visible; integration tests exercise the same source analysis directly.
pub(crate) fn trace_source(
    source: &str,
    function: u32,
    pc: u32,
    depth: usize,
    limit: usize,
    max_bytes: usize,
    exception_boundaries: &BTreeSet<u32>,
) -> DecompilerResult<Vec<u8>> {
    bounds(depth, limit, max_bytes)?;
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("trace requires valid complete exporter JavaScript"));
    }
    let mut syntax = Syntax {
        source,
        ..Syntax::default()
    };
    syntax.visit_program(&parsed.program);
    // AST traversal order is not a source-order contract. Index spans once,
    // then inspect only each PC's slices (not the entire AST per instruction).
    syntax.reads.sort_unstable_by_key(|(_, span)| span.start);
    syntax
        .writes
        .sort_unstable_by_key(|(_, _, span)| span.start);
    let prefix = format!("// HBC function {function}, PC ");
    let mut markers = Vec::new();
    let mut offset = 0;
    for line in source.split_inclusive('\n') {
        if let Some(value) = line.strip_prefix(&prefix) {
            let value = value
                .trim()
                .parse::<u32>()
                .map_err(|_| error("Invalid JS PC marker"))?;
            if markers.last().is_some_and(|&(last, _, _)| last >= value) {
                return Err(error("Unordered or duplicate JS PC markers"));
            }
            markers.push((value, offset, offset + line.len()));
        }
        offset += line.len();
    }
    let root = markers
        .iter()
        .position(|m| m.0 == pc)
        .ok_or_else(|| error(format!("Unknown exact PC {pc} in function {function}")))?;
    let mut definitions: BTreeMap<u32, u32> = BTreeMap::new();
    let mut nodes = BTreeMap::new();
    let mut previous_start = 0;
    for (index, &(instruction_pc, start, body_start)) in markers.iter().enumerate().take(root + 1) {
        if syntax
            .boundaries
            .range(previous_start..body_start)
            .next()
            .is_some()
            || exception_boundaries.contains(&instruction_pc)
        {
            definitions.clear();
        }
        let end = markers.get(index + 1).map_or(source.len(), |m| m.1);
        // Dispatch labels and the exception footer belong to the wrapper, not
        // the last instruction. Internal inline switch cases remain intact.
        let mut excerpt_end = end;
        let mut line_offset = body_start;
        for line in source[body_start..end].split_inclusive('\n') {
            if line.starts_with("case ")
                || line.starts_with("default:")
                || line.starts_with("} } catch (")
            {
                excerpt_end = line_offset;
                break;
            }
            line_offset += line.len();
        }
        let write_start = syntax
            .writes
            .partition_point(|(_, _, s)| (s.start as usize) < body_start);
        let write_end = syntax
            .writes
            .partition_point(|(_, _, s)| (s.start as usize) < excerpt_end);
        let writes = &syntax.writes[write_start..write_end];
        let read_start = syntax
            .reads
            .partition_point(|(_, s)| (s.start as usize) < body_start);
        let read_end = syntax
            .reads
            .partition_point(|(_, s)| (s.start as usize) < excerpt_end);
        let mut first_write_end: BTreeMap<u32, u32> = BTreeMap::new();
        for &(register, _, span) in writes {
            first_write_end
                .entry(register)
                .and_modify(|end| *end = (*end).min(span.end))
                .or_insert(span.end);
        }
        let mut uses = BTreeMap::new();
        for &(register, span) in &syntax.reads[read_start..read_end] {
            // A later read in a multi-statement opcode cannot be attributed to
            // the old physical register after an intra-PC write.
            let intra_pc = first_write_end
                .get(&register)
                .is_some_and(|&end| end <= span.start);
            let definition = if intra_pc {
                None
            } else {
                definitions.get(&register).copied()
            };
            let status = if intra_pc {
                "unresolved_intra_pc_write"
            } else if definition.is_some() {
                "prior_definition"
            } else {
                "unresolved_block_entry_or_external"
            };
            uses.insert(
                (register, definition, status),
                Use {
                    register,
                    previous_definition_pc: definition,
                    status,
                },
            );
        }
        let defines: BTreeSet<_> = writes.iter().map(|&(r, _, _)| r).collect();
        let excerpt = &source[start..excerpt_end];
        let mut length = excerpt.len().min(SNIPPET_BYTES);
        while !excerpt.is_char_boundary(length) {
            length -= 1;
        }
        nodes.insert(
            instruction_pc,
            Node {
                pc: instruction_pc,
                javascript: excerpt[..length].to_owned(),
                original_bytes: excerpt.len(),
                snippet_truncated: length < excerpt.len(),
                defines: defines.iter().copied().collect(),
                uses: uses.into_values().collect(),
            },
        );
        for register in defines {
            definitions.insert(register, instruction_pc);
        }
        previous_start = body_start;
    }
    let mut selected = BTreeMap::from([(pc, 0usize)]);
    let mut queue = VecDeque::from([pc]);
    let mut depth_truncated = false;
    while let Some(current) = queue.pop_front() {
        let level = selected[&current];
        for dependency in nodes[&current]
            .uses
            .iter()
            .filter_map(|u| u.previous_definition_pc)
        {
            if selected.contains_key(&dependency) {
                continue;
            }
            if level == depth {
                depth_truncated = true;
                continue;
            }
            if selected.len() == limit {
                return Err(error(
                    "trace node budget exceeded; increase limit or reduce depth",
                ));
            }
            selected.insert(dependency, level + 1);
            queue.push_back(dependency);
        }
    }
    let output_nodes: Vec<_> = selected.keys().map(|key| &nodes[key]).collect();
    let report = serde_json::json!({
        "schema_version": 1, "function_id": function, "pc": pc,
        "source": "export_function_fragments", "provenance": "syntactic_physical_register_definitions",
        "warning": "Syntactic definitions are not guaranteed runtime writes, runtime values or executable substitutions. Reads after intra-PC writes are unresolved. Calls, getters and mutations are not evaluated. No deep array/property tracking, interprocedural or lexical runtime resolution; no cross-block reaching-definition guesses.",
        "depth": depth, "limit": limit, "max_bytes": max_bytes,
        "snippet_byte_limit": SNIPPET_BYTES, "depth_truncated": depth_truncated,
        "nodes": output_nodes
    });
    let mut bytes = serde_json::to_vec(&report).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > max_bytes {
        return Err(error("trace output byte budget exceeded"));
    }
    Ok(bytes)
}

/// Emit one bounded JSON document, validating all budgets before stdout.
/// Errors are returned to the CLI owner for nonzero exit and stderr reporting.
pub fn run(
    input: &Path,
    function: u32,
    pc: u32,
    depth: usize,
    limit: usize,
    max_bytes: usize,
) -> DecompilerResult<()> {
    let bytes = report(input, function, pc, depth, limit, max_bytes)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

pub(crate) fn report(
    input: &Path,
    function: u32,
    pc: u32,
    depth: usize,
    limit: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    bounds(depth, limit, max_bytes)?;
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let header = hbc
        .functions
        .get_parsed_header(function)
        .ok_or_else(|| error(format!("Unknown function {function}")))?;
    let boundaries = header
        .exc_handlers
        .iter()
        .flat_map(|h| [h.start, h.end, h.target])
        .collect();
    let source = export_function_fragments(&hbc, &[function])?.remove(0).1;
    trace_source(&source, function, pc, depth, limit, max_bytes, &boundaries)
}
