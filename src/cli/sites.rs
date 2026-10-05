//! Generic, bounded site catalog over complete exporter JavaScript, not runtime values.
use crate::bundle::export_function_fragments;
use crate::{DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::{
    AssignmentExpression, AssignmentTarget, CallExpression, ComputedMemberExpression, Expression,
    Program, Statement, SwitchCase, UpdateExpression,
};
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
use regex::{Regex, RegexBuilder};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::Write;
use std::path::Path;

const PREVIEW: usize = 1024;
const NODE_CAP: usize = 32;
const OPERAND_CAP: usize = 128;

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

fn bounds(
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    max_bytes: usize,
) -> DecompilerResult<()> {
    if ![
        "all",
        "constructor",
        "call",
        "slot-write",
        "slot-read",
        "property-write",
    ]
    .contains(&kind)
    {
        return Err(error("Unknown site kind"));
    }
    if !slots.is_empty() && !["all", "slot-write", "slot-read"].contains(&kind) {
        return Err(error(
            "--slot requires all, slot-write or slot-read; non-slot sites are excluded",
        ));
    }
    if depth > 8 || !(1..=1000).contains(&limit) || !(1..=16_777_216).contains(&max_bytes) {
        return Err(error(
            "sites bounds: depth 0..8, limit 1..1000, max_bytes 1..16777216",
        ));
    }
    Ok(())
}

#[derive(Serialize)]
struct Snippet {
    javascript: String,
    original_bytes: usize,
    truncated: bool,
}

fn snippet(source: &str, span: Span) -> Snippet {
    let text = &source[span.start as usize..span.end as usize];
    let mut end = text.len().min(PREVIEW);
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    Snippet {
        javascript: text[..end].to_owned(),
        original_bytes: text.len(),
        truncated: end < text.len(),
    }
}

#[derive(Clone)]
struct OperandSpan {
    role: &'static str,
    index: Option<usize>,
    span: Span,
}

struct SiteSpan {
    kind: &'static str,
    span: Span,
    slot: Option<u32>,
    operands: Vec<OperandSpan>,
}

#[derive(Default)]
struct Syntax<'s> {
    source: &'s str,
    reads: Vec<(u32, Span)>,
    writes: Vec<(u32, Span)>,
    boundaries: BTreeSet<u32>,
    sites: Vec<SiteSpan>,
    malformed_constructor: bool,
    malformed_call: bool,
    wrapper_assignments: BTreeSet<u32>,
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

    fn slot(it: &ComputedMemberExpression<'_>) -> Option<u32> {
        let Expression::StaticMemberExpression(object) = &it.object else {
            return None;
        };
        if object.property.name != "slots" {
            return None;
        }
        let Expression::NumericLiteral(index) = &it.expression else {
            return None;
        };
        let value = index.value;
        (value >= 0.0 && value <= f64::from(u32::MAX) && value.fract() == 0.0)
            .then_some(value as u32)
    }

    fn operand(role: &'static str, span: Span) -> OperandSpan {
        OperandSpan {
            role,
            index: None,
            span,
        }
    }

    fn environment(it: &ComputedMemberExpression<'_>) -> Span {
        match &it.object {
            Expression::StaticMemberExpression(slots) => slots.object.span(),
            _ => it.object.span(),
        }
    }
}

impl<'a> Visit<'a> for Syntax<'_> {
    fn visit_program(&mut self, it: &Program<'a>) {
        // Only the typed, top-level tables emitted by the exporter are wrappers.
        // Continue walking their RHS so F[id]'s function body is still cataloged.
        for statement in &it.body {
            let Statement::ExpressionStatement(statement) = statement else {
                continue;
            };
            let Expression::AssignmentExpression(assignment) = &statement.expression else {
                continue;
            };
            let AssignmentTarget::ComputedMemberExpression(member) = &assignment.left else {
                continue;
            };
            let Expression::Identifier(table) = &member.object else {
                continue;
            };
            if assignment.operator.is_assign()
                && matches!(member.expression, Expression::NumericLiteral(_))
                && ((table.name == "F"
                    && matches!(assignment.right, Expression::FunctionExpression(_)))
                    || (table.name == "M"
                        && matches!(assignment.right, Expression::ArrayExpression(_))))
            {
                self.wrapper_assignments.insert(assignment.span.start);
            }
        }
        walk::walk_program(self, it);
    }

    fn visit_computed_member_expression(&mut self, it: &ComputedMemberExpression<'a>) {
        if let Some(register) = self.register(it.span) {
            self.reads.push((register, it.span));
        }
        if let Some(slot) = Self::slot(it) {
            self.sites.push(SiteSpan {
                kind: "slot-read",
                span: it.span,
                slot: Some(slot),
                operands: vec![
                    Self::operand("environment", Self::environment(it)),
                    Self::operand("slot_index", it.expression.span()),
                ],
            });
        }
        walk::walk_computed_member_expression(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if self.wrapper_assignments.contains(&it.span.start) {
            self.visit_expression(&it.right);
            return;
        }
        let lhs = it.left.span();
        if let Some(register) = self.register(lhs) {
            self.writes.push((register, it.span));
            if it.operator.is_assign() {
                self.visit_expression(&it.right);
                return;
            }
        } else {
            let member = match &it.left {
                AssignmentTarget::ComputedMemberExpression(member) => Some((
                    Self::slot(member),
                    if Self::slot(member).is_some() {
                        Self::environment(member)
                    } else {
                        member.object.span()
                    },
                    member.expression.span(),
                )),
                AssignmentTarget::StaticMemberExpression(member) => {
                    Some((None, member.object.span(), member.property.span))
                }
                _ => None,
            };
            if let Some((slot, object, key)) = member {
                self.sites.push(SiteSpan {
                    kind: if slot.is_some() {
                        "slot-write"
                    } else {
                        "property-write"
                    },
                    span: it.span,
                    slot,
                    operands: vec![
                        Self::operand(
                            if slot.is_some() {
                                "environment"
                            } else {
                                "object"
                            },
                            object,
                        ),
                        Self::operand(if slot.is_some() { "slot_index" } else { "key" }, key),
                        Self::operand("value", it.right.span()),
                    ],
                });
                if it.operator.is_assign() {
                    // Visit address computations, but do not label a plain destination as a load.
                    if let AssignmentTarget::ComputedMemberExpression(member) = &it.left {
                        self.visit_expression(&member.object);
                        self.visit_expression(&member.expression);
                    } else if let AssignmentTarget::StaticMemberExpression(member) = &it.left {
                        self.visit_expression(&member.object);
                    }
                    self.visit_expression(&it.right);
                    return;
                }
            }
        }
        walk::walk_assignment_expression(self, it);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if let Some(register) = self.register(it.argument.span()) {
            self.writes.push((register, it.span));
        }
        walk::walk_update_expression(self, it);
    }

    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if let Expression::Identifier(callee) = &it.callee {
            let name = callee.name.as_str();
            if name == "construct" {
                if let [callee, receiver, oxc_ast::ast::Argument::ArrayExpression(args)] =
                    it.arguments.as_slice()
                {
                    let mut operands = vec![
                        Self::operand("callee", callee.span()),
                        Self::operand("preallocated_receiver", receiver.span()),
                    ];
                    operands.extend(args.elements.iter().enumerate().map(|(index, arg)| {
                        OperandSpan {
                            role: "user_argument",
                            index: Some(index),
                            span: arg.span(),
                        }
                    }));
                    self.sites.push(SiteSpan {
                        kind: "constructor",
                        span: it.span,
                        slot: None,
                        operands,
                    });
                } else {
                    self.malformed_constructor = true;
                }
            } else if name == "apply" {
                if let [callee, receiver, oxc_ast::ast::Argument::ArrayExpression(args)] =
                    it.arguments.as_slice()
                {
                    // Spreads and holes do not have known ordered argument positions.
                    if it.optional
                        || matches!(callee, oxc_ast::ast::Argument::SpreadElement(_))
                        || matches!(receiver, oxc_ast::ast::Argument::SpreadElement(_))
                        || args.elements.iter().any(|arg| {
                            matches!(
                                arg,
                                oxc_ast::ast::ArrayExpressionElement::SpreadElement(_)
                                    | oxc_ast::ast::ArrayExpressionElement::Elision(_)
                            )
                        })
                    {
                        self.malformed_call = true;
                    } else {
                        let mut operands = vec![
                            Self::operand("callee", callee.span()),
                            Self::operand("receiver", receiver.span()),
                        ];
                        operands.extend(args.elements.iter().enumerate().map(|(index, arg)| {
                            OperandSpan {
                                role: "user_argument",
                                index: Some(index),
                                span: arg.span(),
                            }
                        }));
                        self.sites.push(SiteSpan {
                            kind: "call",
                            span: it.span,
                            slot: None,
                            operands,
                        });
                    }
                } else {
                    self.malformed_call = true;
                }
            } else if name == "put" || name == "own" {
                let operands = it
                    .arguments
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| OperandSpan {
                        role: match index {
                            0 => "object",
                            1 => "key",
                            2 => "value",
                            _ => "helper_argument",
                        },
                        index: Some(index),
                        span: arg.span(),
                    })
                    .collect();
                self.sites.push(SiteSpan {
                    kind: "property-write",
                    span: it.span,
                    slot: None,
                    operands,
                });
            }
        }
        walk::walk_call_expression(self, it);
    }

    fn visit_switch_case(&mut self, it: &SwitchCase<'a>) {
        self.boundaries.insert(it.span.start);
        walk::walk_switch_case(self, it);
    }
}

#[derive(Clone)]
struct Read {
    register: u32,
    span: Span,
    definition: Option<usize>,
    status: &'static str,
}

struct Definition {
    pc: u32,
    register: u32,
    span: Span,
}

struct Index {
    syntax: Vec<SiteSpan>,
    markers: Vec<(u32, u32, u32)>,
    reads: Vec<Read>,
    definitions: Vec<Definition>,
}

fn reads_in(reads: &[Read], span: Span) -> &[Read] {
    let start = reads.partition_point(|r| r.span.start < span.start);
    let end = reads.partition_point(|r| r.span.start < span.end);
    &reads[start..end]
}

fn index_source(
    source: &str,
    function: u32,
    exceptions: &BTreeSet<u32>,
) -> DecompilerResult<Index> {
    if source.len() > u32::MAX as usize {
        return Err(error("Exporter source exceeds AST span bounds"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("sites requires valid complete exporter JavaScript"));
    }
    let mut syntax = Syntax {
        source,
        ..Syntax::default()
    };
    syntax.visit_program(&parsed.program);
    if syntax.malformed_constructor {
        return Err(error("Malformed exporter construct helper"));
    }
    if syntax.malformed_call {
        return Err(error(
            "Malformed or unsupported exporter apply helper: requires callee, receiver and an array without spreads or holes",
        ));
    }
    // A line-anchored marker must also be an actual parser comment, never literal text.
    let comments: BTreeSet<_> = parsed
        .program
        .comments
        .iter()
        .map(|c| c.span.start)
        .collect();
    let prefix = format!("// HBC function {function}, PC ");
    let mut markers = Vec::new();
    let mut offset = 0u32;
    for line in source.split_inclusive('\n') {
        if comments.contains(&offset) {
            if let Some(value) = line.strip_prefix(&prefix) {
                let pc = value
                    .trim()
                    .parse::<u32>()
                    .map_err(|_| error("Invalid numeric JS PC marker"))?;
                if markers.last().is_some_and(|&(last, _, _)| last >= pc) {
                    return Err(error("Unordered or duplicate JS PC markers"));
                }
                markers.push((pc, offset, offset + line.len() as u32));
            } else if line.starts_with("// HBC function ") {
                return Err(error("Unknown function in JS PC marker"));
            }
        }
        offset += line.len() as u32;
    }
    if markers.is_empty() {
        return Err(error("Missing numeric JS PC markers"));
    }
    syntax.reads.sort_by_key(|(_, span)| span.start);
    syntax.writes.sort_by_key(|(_, span)| span.start);
    syntax
        .sites
        .sort_by_key(|site| (site.span.start, site.span.end, site.kind));
    let mut reads = Vec::with_capacity(syntax.reads.len());
    let mut definitions = Vec::with_capacity(syntax.writes.len());
    let mut prior: BTreeMap<u32, usize> = BTreeMap::new();
    let mut previous = 0;
    for (i, &(pc, _, start)) in markers.iter().enumerate() {
        let end = markers.get(i + 1).map_or(source.len() as u32, |m| m.1);
        if syntax.boundaries.range(previous..start).next().is_some()
            || exceptions
                .range(previous_pc(&markers, i)..=pc)
                .next()
                .is_some()
        {
            prior.clear();
        }
        let ws = syntax.writes.partition_point(|(_, s)| s.start < start);
        let we = syntax.writes.partition_point(|(_, s)| s.start < end);
        let mut first_end = BTreeMap::new();
        for &(reg, span) in &syntax.writes[ws..we] {
            first_end
                .entry(reg)
                .and_modify(|e: &mut u32| *e = (*e).min(span.end))
                .or_insert(span.end);
        }
        let rs = syntax.reads.partition_point(|(_, s)| s.start < start);
        let re = syntax.reads.partition_point(|(_, s)| s.start < end);
        for &(register, span) in &syntax.reads[rs..re] {
            let intra = first_end.get(&register).is_some_and(|&e| e <= span.start);
            let boundary = syntax.boundaries.range(start..=span.start).next().is_some();
            let definition = if intra || boundary {
                None
            } else {
                prior.get(&register).copied()
            };
            let status = if intra {
                "unresolved_intra_pc_write"
            } else if definition.is_some() {
                "prior_definition"
            } else {
                "unresolved_block_entry_or_external"
            };
            reads.push(Read {
                register,
                span,
                definition,
                status,
            });
        }
        let last_boundary = syntax.boundaries.range(start..end).next_back().copied();
        if last_boundary.is_some() {
            prior.clear();
        }
        for &(register, span) in &syntax.writes[ws..we] {
            let id = definitions.len();
            definitions.push(Definition { pc, register, span });
            if last_boundary.is_none_or(|b| span.start > b) {
                prior.insert(register, id);
            }
        }
        previous = start;
    }
    Ok(Index {
        syntax: syntax.sites,
        markers,
        reads,
        definitions,
    })
}

fn previous_pc(markers: &[(u32, u32, u32)], i: usize) -> u32 {
    if i == 0 {
        0
    } else {
        markers[i - 1].0.saturating_add(1)
    }
}

#[derive(Serialize)]
struct Edge {
    register: u32,
    status: &'static str,
    source_pc: Option<u32>,
    source_span: Option<[u32; 2]>,
    source: Option<Snippet>,
}

fn edge(source: &str, index: &Index, read: &Read) -> Edge {
    let definition = read.definition.map(|i| &index.definitions[i]);
    Edge {
        register: read.register,
        status: read.status,
        source_pc: definition.map(|d| d.pc),
        source_span: definition.map(|d| [d.span.start, d.span.end]),
        source: definition.map(|d| snippet(source, d.span)),
    }
}

#[derive(Serialize)]
struct Node {
    source_pc: u32,
    source_span: [u32; 2],
    defines_register: u32,
    source: Snippet,
    edges: Vec<Edge>,
    edge_count: usize,
    edges_truncated: bool,
}

#[derive(Serialize)]
struct Operand {
    role: &'static str,
    argument_index: Option<usize>,
    expression: Snippet,
    edges: Vec<Edge>,
    edge_count: usize,
    edges_truncated: bool,
    nodes: Vec<Node>,
    depth_truncated: bool,
    node_truncated: bool,
}

fn operand(source: &str, index: &Index, span: &OperandSpan, depth: usize) -> Operand {
    let reads = reads_in(&index.reads, span.span);
    let mut selected = BTreeMap::new();
    let mut queue = VecDeque::new();
    let mut depth_truncated = false;
    let mut node_truncated = reads.len() > NODE_CAP;
    for read in reads.iter().take(NODE_CAP) {
        if let Some(id) = read.definition {
            queue.push_back((id, 1));
        }
    }
    while let Some((id, level)) = queue.pop_front() {
        if selected.contains_key(&id) {
            continue;
        }
        if level > depth {
            depth_truncated = true;
            continue;
        }
        if selected.len() == NODE_CAP {
            node_truncated = true;
            continue;
        }
        selected.insert(id, level);
        let dependencies = reads_in(&index.reads, index.definitions[id].span);
        node_truncated |= dependencies.len() > NODE_CAP;
        for read in dependencies.iter().take(NODE_CAP) {
            if let Some(next) = read.definition {
                queue.push_back((next, level + 1));
            }
        }
    }
    let nodes = selected
        .keys()
        .map(|&id| {
            let d = &index.definitions[id];
            let uses = reads_in(&index.reads, d.span);
            Node {
                source_pc: d.pc,
                source_span: [d.span.start, d.span.end],
                defines_register: d.register,
                source: snippet(source, d.span),
                edges: uses
                    .iter()
                    .take(NODE_CAP)
                    .map(|r| edge(source, index, r))
                    .collect(),
                edge_count: uses.len(),
                edges_truncated: uses.len() > NODE_CAP,
            }
        })
        .collect();
    Operand {
        role: span.role,
        argument_index: span.index,
        expression: snippet(source, span.span),
        edges: reads
            .iter()
            .take(NODE_CAP)
            .map(|r| edge(source, index, r))
            .collect(),
        edge_count: reads.len(),
        edges_truncated: reads.len() > NODE_CAP,
        nodes,
        depth_truncated,
        node_truncated,
    }
}

#[derive(Serialize)]
struct Site {
    function_id: u32,
    pc: u32,
    kind: &'static str,
    source_span: [u32; 2],
    ordinal: usize,
    exact_expression: Snippet,
    source: Snippet,
    operands: Vec<Operand>,
    operand_count: usize,
    operands_truncated: bool,
    slot: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    source_matches: Option<SourceMatches>,
}

#[derive(Default)]
pub struct SiteFilter {
    pub matches: Vec<String>,
    pub from_pc: Option<u32>,
    pub to_pc: Option<u32>,
}

impl SiteFilter {
    fn matcher(&self) -> DecompilerResult<Option<Regex>> {
        if self.from_pc.zip(self.to_pc).is_some_and(|(a, b)| a > b) {
            return Err(error("--from-pc must not exceed --to-pc"));
        }
        if self.matches.len() > 16
            || self
                .matches
                .iter()
                .any(|query| query.is_empty() || query.len() > 1024)
        {
            return Err(error(
                "--match accepts at most 16 nonempty queries of at most 1024 bytes",
            ));
        }
        if self.matches.is_empty() {
            return Ok(None);
        }
        RegexBuilder::new(
            &self
                .matches
                .iter()
                .map(|q| regex::escape(q))
                .collect::<Vec<_>>()
                .join("|"),
        )
        .case_insensitive(true)
        .build()
        .map(Some)
        .map_err(|e| error(e.to_string()))
    }

    fn active(&self) -> bool {
        !self.matches.is_empty() || self.from_pc.is_some() || self.to_pc.is_some()
    }
}

#[derive(Serialize)]
struct SourceMatch {
    pc: u32,
    source_span: [u32; 2],
    matched_span: [u32; 2],
    matched_source: Snippet,
    kind: &'static str,
}

#[derive(Serialize)]
struct SourceMatches {
    evidence: Vec<SourceMatch>,
    evidence_count: usize,
    evidence_truncated: bool,
    dependency_search_truncated: bool,
    unresolved_dependency_reads: bool,
}

struct MatchCandidates {
    direct: bool,
    definitions: BTreeSet<usize>,
    truncated: bool,
    unresolved: bool,
}

fn matching_dependencies(
    source: &str,
    index: &Index,
    site: &SiteSpan,
    depth: usize,
    matcher: &Regex,
    definition_matches: &[bool],
) -> MatchCandidates {
    let mut result = MatchCandidates {
        direct: matcher.is_match(&source[site.span.start as usize..site.span.end as usize]),
        definitions: BTreeSet::new(),
        truncated: site.operands.len() > OPERAND_CAP,
        unresolved: false,
    };
    for operand in site.operands.iter().take(OPERAND_CAP) {
        let reads = reads_in(&index.reads, operand.span);
        result.truncated |= reads.len() > NODE_CAP;
        let mut queue = VecDeque::new();
        let mut visited = BTreeSet::new();
        for read in reads.iter().take(NODE_CAP) {
            if let Some(id) = read.definition {
                queue.push_back((id, 1));
            } else {
                result.unresolved = true;
            }
        }
        while let Some((id, level)) = queue.pop_front() {
            if visited.contains(&id) {
                continue;
            }
            if level > depth || visited.len() == NODE_CAP {
                result.truncated = true;
                continue;
            }
            visited.insert(id);
            if definition_matches[id] {
                result.definitions.insert(id);
            }
            let reads = reads_in(&index.reads, index.definitions[id].span);
            result.truncated |= reads.len() > NODE_CAP;
            for read in reads.iter().take(NODE_CAP) {
                if let Some(next) = read.definition {
                    queue.push_back((next, level + 1));
                } else {
                    result.unresolved = true;
                }
            }
        }
    }
    result
}

fn match_evidence(
    source: &str,
    matcher: &Regex,
    pc: u32,
    span: Span,
    kind: &'static str,
) -> SourceMatch {
    let found = matcher
        .find(&source[span.start as usize..span.end as usize])
        .unwrap();
    let matched = Span::new(
        span.start + found.start() as u32,
        span.start + found.end() as u32,
    );
    SourceMatch {
        pc,
        source_span: [span.start, span.end],
        matched_span: [matched.start, matched.end],
        matched_source: snippet(source, matched),
        kind,
    }
}

fn source_matches(
    source: &str,
    index: &Index,
    site: &SiteSpan,
    pc: u32,
    matcher: &Regex,
    matches: &MatchCandidates,
) -> SourceMatches {
    const EVIDENCE_LIMIT: usize = 8;
    let mut evidence = Vec::new();
    if matches.direct {
        evidence.push(match_evidence(
            source,
            matcher,
            pc,
            site.span,
            "site_expression",
        ));
    }
    for &id in matches
        .definitions
        .iter()
        .take(EVIDENCE_LIMIT - evidence.len())
    {
        let definition = &index.definitions[id];
        evidence.push(match_evidence(
            source,
            matcher,
            definition.pc,
            definition.span,
            "local_definition",
        ));
    }
    let evidence_count = usize::from(matches.direct) + matches.definitions.len();
    SourceMatches {
        evidence_truncated: evidence_count > evidence.len(),
        evidence_count,
        evidence,
        dependency_search_truncated: matches.truncated,
        unresolved_dependency_reads: matches.unresolved,
    }
}

// Relationships and traversal limits remain operand-local. Only immutable definition
// identity/source metadata is shared, including targets outside the traversal depth.
#[derive(Serialize)]
struct CompactDefinition {
    function_id: u32,
    source_pc: u32,
    source_span: [u32; 2],
    defines_register: u32,
    source: Snippet,
}

type DefinitionTable = BTreeMap<String, CompactDefinition>;

fn intern_definition(
    table: &mut DefinitionTable,
    function_id: u32,
    source_pc: u32,
    source_span: [u32; 2],
    defines_register: u32,
    source: Snippet,
) -> String {
    let id = format!("{function_id}:{}:{}", source_span[0], source_span[1]);
    table.entry(id.clone()).or_insert(CompactDefinition {
        function_id,
        source_pc,
        source_span,
        defines_register,
        source,
    });
    id
}

#[derive(Serialize)]
struct CompactEdge {
    register: u32,
    status: &'static str,
    definition: Option<String>,
}

fn compact_edges(
    edges: Vec<Edge>,
    function_id: u32,
    table: &mut DefinitionTable,
) -> Vec<CompactEdge> {
    edges
        .into_iter()
        .map(|edge| CompactEdge {
            register: edge.register,
            status: edge.status,
            definition: match (edge.source_pc, edge.source_span, edge.source) {
                (Some(pc), Some(span), Some(source)) => Some(intern_definition(
                    table,
                    function_id,
                    pc,
                    span,
                    edge.register,
                    source,
                )),
                _ => None,
            },
        })
        .collect()
}

#[derive(Serialize)]
struct CompactNode {
    definition: String,
    edges: Vec<CompactEdge>,
    edge_count: usize,
    edges_truncated: bool,
}

fn compact_sites(sites: Vec<Site>) -> DecompilerResult<(serde_json::Value, DefinitionTable)> {
    let mut table = DefinitionTable::new();
    let mut output = Vec::with_capacity(sites.len());
    for mut site in sites {
        let function_id = site.function_id;
        let mut operands = Vec::with_capacity(site.operands.len());
        for mut operand in std::mem::take(&mut site.operands) {
            let edges = compact_edges(std::mem::take(&mut operand.edges), function_id, &mut table);
            let nodes: Vec<_> = std::mem::take(&mut operand.nodes)
                .into_iter()
                .map(|node| CompactNode {
                    definition: intern_definition(
                        &mut table,
                        function_id,
                        node.source_pc,
                        node.source_span,
                        node.defines_register,
                        node.source,
                    ),
                    edges: compact_edges(node.edges, function_id, &mut table),
                    edge_count: node.edge_count,
                    edges_truncated: node.edges_truncated,
                })
                .collect();
            let mut value = serde_json::to_value(operand).map_err(|e| error(e.to_string()))?;
            value["edges"] = serde_json::to_value(edges).map_err(|e| error(e.to_string()))?;
            value["nodes"] = serde_json::to_value(nodes).map_err(|e| error(e.to_string()))?;
            operands.push(value);
        }
        let mut value = serde_json::to_value(site).map_err(|e| error(e.to_string()))?;
        value["operands"] = serde_json::Value::Array(operands);
        output.push(value);
    }
    Ok((serde_json::Value::Array(output), table))
}

/// Sources must be complete exporter fragments. Parsing and span indexing happen once per function.
#[allow(clippy::too_many_arguments)]
pub(crate) fn catalog_sources(
    sources: &[(u32, String, BTreeSet<u32>)],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    catalog_sources_format(
        sources,
        kind,
        slots,
        depth,
        limit,
        offset,
        max_bytes,
        false,
        &SiteFilter::default(),
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn catalog_sources_compact(
    sources: &[(u32, String, BTreeSet<u32>)],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    catalog_sources_format(
        sources,
        kind,
        slots,
        depth,
        limit,
        offset,
        max_bytes,
        true,
        &SiteFilter::default(),
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn catalog_sources_filtered(
    sources: &[(u32, String, BTreeSet<u32>)],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
    compact: bool,
    filter: &SiteFilter,
) -> DecompilerResult<Vec<u8>> {
    catalog_sources_format(
        sources, kind, slots, depth, limit, offset, max_bytes, compact, filter,
    )
}

#[allow(clippy::too_many_arguments)]
fn catalog_sources_format(
    sources: &[(u32, String, BTreeSet<u32>)],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
    compact: bool,
    filter: &SiteFilter,
) -> DecompilerResult<Vec<u8>> {
    bounds(kind, slots, depth, limit, max_bytes)?;
    let matcher = filter.matcher()?;
    if sources.is_empty() {
        return Err(error("sites requires explicit nonempty function selection"));
    }
    let mut ordered: Vec<_> = sources.iter().collect();
    ordered.sort_by_key(|s| s.0);
    if ordered.windows(2).any(|w| w[0].0 == w[1].0) {
        return Err(error("Duplicate function source"));
    }
    let mut total = 0usize;
    let mut unfiltered_total = 0usize;
    let mut dependency_search_truncated_sites = 0usize;
    let mut unresolved_dependency_sites = 0usize;
    let mut sites = Vec::new();
    for &(function, ref source, ref exceptions) in ordered {
        let index = index_source(source, function, exceptions)?;
        let definition_matches: Vec<_> = index
            .definitions
            .iter()
            .map(|d| {
                matcher.as_ref().is_some_and(|m| {
                    m.is_match(&source[d.span.start as usize..d.span.end as usize])
                })
            })
            .collect();
        let mut ordinals = BTreeMap::new();
        for site in &index.syntax {
            let marker = index.markers.partition_point(|m| m.2 <= site.span.start);
            if marker == 0 {
                return Err(error("Site has unknown PC"));
            }
            let (pc, _, body_start) = index.markers[marker - 1];
            let end = index
                .markers
                .get(marker)
                .map_or(source.len() as u32, |m| m.1);
            if site.span.end > end {
                return Err(error("Site crosses PC boundary"));
            }
            let ordinal = ordinals.entry((pc, site.kind)).or_insert(0usize);
            let this_ordinal = *ordinal;
            *ordinal += 1;
            if kind != "all" && kind != site.kind {
                continue;
            }
            if !slots.is_empty() && !site.slot.is_some_and(|slot| slots.contains(&slot)) {
                continue;
            }
            unfiltered_total += 1;
            if filter.from_pc.is_some_and(|start| pc < start)
                || filter.to_pc.is_some_and(|end| pc > end)
            {
                continue;
            }
            let matches = matcher.as_ref().map(|m| {
                matching_dependencies(source, &index, site, depth, m, &definition_matches)
            });
            if let Some(matches) = &matches {
                dependency_search_truncated_sites += usize::from(matches.truncated);
                unresolved_dependency_sites += usize::from(matches.unresolved);
                if !matches.direct && matches.definitions.is_empty() {
                    continue;
                }
            }
            if total >= offset && sites.len() < limit {
                sites.push(Site {
                    function_id: function,
                    pc,
                    kind: site.kind,
                    source_span: [site.span.start, site.span.end],
                    ordinal: this_ordinal,
                    exact_expression: snippet(source, site.span),
                    source: snippet(source, Span::new(body_start, end)),
                    operands: site
                        .operands
                        .iter()
                        .take(OPERAND_CAP)
                        .map(|s| operand(source, &index, s, depth))
                        .collect(),
                    operand_count: site.operands.len(),
                    operands_truncated: site.operands.len() > OPERAND_CAP,
                    slot: site.slot,
                    source_matches: matches.as_ref().map(|matches| {
                        source_matches(source, &index, site, pc, matcher.as_ref().unwrap(), matches)
                    }),
                });
            }
            total += 1;
        }
    }
    let next_offset = offset.checked_add(sites.len()).filter(|&next| next < total);
    let call_scope = kind == "call" || sites.iter().any(|site| site.kind == "call");
    let (sites, definitions) = if compact {
        compact_sites(sites)?
    } else {
        (
            serde_json::to_value(sites).map_err(|e| error(e.to_string()))?,
            DefinitionTable::new(),
        )
    };
    let mut report = serde_json::json!({
        "schema_version": 1, "source": "export_function_fragments", "parsed_source_complete": true,
        "constructor_scope": "Exporter construct-helper invocations only; intrinsic new allocations are not counted.",
        "provenance": "syntactic_local_prior_register_definitions",
        "cross_block_navigation": "For unresolved block-entry register reads, origins INPUT FUNCTION_ID SITE_PC reports bounded alternative definitions over normal dispatcher paths; it does not evaluate values or resolve captures.",
        "warning": "Definitions are syntactic and not guaranteed runtime writes, runtime values or executable substitutions. Calls/getters are labels only; no lexical resolution, constructor semantics, object mutation evaluation, full heap values or cross-block guesses. Intra-PC definitions are not resolved.",
        "slot_filter_policy": "A nonempty slot filter excludes all non-slot records, including with kind=all.",
        "kind": kind, "slots": slots, "depth": depth, "limit": limit, "offset": offset, "max_bytes": max_bytes,
        "snippet_byte_limit": PREVIEW, "node_cap_per_operand": NODE_CAP, "edge_cap_per_field": NODE_CAP, "operand_cap_per_record": OPERAND_CAP,
        "total": total, "next_offset": next_offset, "sites": sites
    });
    if call_scope {
        report["call_scope"] = serde_json::json!(
            "Exporter apply-helper invocations only; intrinsic and unrelated helper calls are not counted. User argument indexes exclude callee and receiver; spreads and holes are unsupported."
        );
    }
    if filter.active() {
        report["filter"] = serde_json::json!({
            "matches": filter.matches, "from_pc": filter.from_pc, "to_pc": filter.to_pc,
            "range_policy": "inclusive function-local PCs, applied independently to each selected function",
            "unfiltered_total": unfiltered_total,
            "dependency_search_truncated_sites": dependency_search_truncated_sites,
            "unresolved_dependency_sites": unresolved_dependency_sites,
            "match_policy": "OR literal substrings, case-insensitive, over complete JS site expressions and bounded same-block prior definition spans. Not decoded string values, runtime identities, heap mutations, lexical bindings or cross-function values.",
            "match_depth": depth, "node_cap_per_operand": NODE_CAP, "operand_cap_per_record": OPERAND_CAP,
            "evidence_limit_per_site": 8, "runtime_match_complete": false,
            "negative_result_policy": "No match in this bounded source scope is not proof of absence in runtime behavior or unresolved dependencies."
        });
    }
    if compact {
        report["format"] = serde_json::json!("compact");
        report["definitions"] =
            serde_json::to_value(definitions).map_err(|e| error(e.to_string()))?;
    }
    let mut bytes = serde_json::to_vec(&report).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > max_bytes {
        return Err(error("sites output byte budget exceeded before stdout"));
    }
    Ok(bytes)
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn report(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    report_format(
        input,
        functions,
        kind,
        slots,
        depth,
        limit,
        offset,
        max_bytes,
        false,
        &SiteFilter::default(),
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn report_compact(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<Vec<u8>> {
    report_format(
        input,
        functions,
        kind,
        slots,
        depth,
        limit,
        offset,
        max_bytes,
        true,
        &SiteFilter::default(),
    )
}

#[allow(clippy::too_many_arguments)]
fn report_format(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
    compact: bool,
    filter: &SiteFilter,
) -> DecompilerResult<Vec<u8>> {
    bounds(kind, slots, depth, limit, max_bytes)?;
    filter.matcher()?;
    if functions.is_empty() {
        return Err(error("sites requires explicit nonempty function selection"));
    }
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let ids: BTreeSet<u32> = functions.iter().copied().collect();
    let mut boundaries = BTreeMap::new();
    for &id in &ids {
        let header = hbc
            .functions
            .get_parsed_header(id)
            .ok_or_else(|| error(format!("Unknown function {id}")))?;
        boundaries.insert(
            id,
            header
                .exc_handlers
                .iter()
                .flat_map(|h| [h.start, h.end, h.target])
                .collect(),
        );
    }
    let fragments = export_function_fragments(&hbc, &ids.into_iter().collect::<Vec<_>>())?;
    let sources = fragments
        .into_iter()
        .map(|(id, source)| (id, source, boundaries.remove(&id).unwrap_or_default()))
        .collect::<Vec<_>>();
    if filter.active() {
        catalog_sources_filtered(
            &sources, kind, slots, depth, limit, offset, max_bytes, compact, filter,
        )
    } else if compact {
        catalog_sources_compact(&sources, kind, slots, depth, limit, offset, max_bytes)
    } else {
        catalog_sources(&sources, kind, slots, depth, limit, offset, max_bytes)
    }
}

#[allow(clippy::too_many_arguments)]
pub fn run_filtered(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
    compact: bool,
    filter: &SiteFilter,
) -> DecompilerResult<()> {
    let bytes = report_format(
        input, functions, kind, slots, depth, limit, offset, max_bytes, compact, filter,
    )?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

/// Validate and serialize the entire page before writing any stdout bytes.
#[allow(clippy::too_many_arguments)]
pub fn run(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<()> {
    let bytes = report(
        input, functions, kind, slots, depth, limit, offset, max_bytes,
    )?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}

/// Compact output has the same atomic byte-budget check as the default catalog.
#[allow(clippy::too_many_arguments)]
pub fn run_compact(
    input: &Path,
    functions: &[u32],
    kind: &str,
    slots: &[u32],
    depth: usize,
    limit: usize,
    offset: usize,
    max_bytes: usize,
) -> DecompilerResult<()> {
    let bytes = report_compact(
        input, functions, kind, slots, depth, limit, offset, max_bytes,
    )?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}
