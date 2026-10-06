//! Demand-driven candidate reaching definitions over exporter JS. Never evaluates JS.
use crate::bundle::export_function_fragments;
use crate::{DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::*;
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
use regex::RegexBuilder;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::Write;
use std::path::Path;

const SOURCE_CAP: usize = 64 * 1024 * 1024;
const OUTPUT_CAP: usize = 16 * 1024 * 1024;
const BLOCK_CAP: usize = 4096;
const EDGE_CAP: usize = 8192;
const DEFINITION_CAP: usize = 1048576;
const READ_CAP: usize = 2097152;
const PREVIEW_CAP: usize = 1024;
const DISPLAY_EDGE_CAP: usize = 256;
const CONTROL_WORK_CAP: usize = 2097152;
const SYNTAX_WORK_CAP: usize = 8388608;
const INSTRUCTION_EXPRESSION_CAP: usize = 32;

fn error(s: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(s.into())
}
fn bounds(depth: usize, limit: usize, bytes: usize) -> DecompilerResult<()> {
    if depth > 64 || !(1..=4096).contains(&limit) || !(1..=OUTPUT_CAP).contains(&bytes) {
        return Err(error(
            "origins bounds: depth 0..64, limit 1..4096, max_bytes 1..16777216",
        ));
    }
    Ok(())
}
fn numeric(e: &Expression<'_>) -> Option<u32> {
    if let Expression::NumericLiteral(n) = e {
        if n.value >= 0.0 && n.value <= u32::MAX as f64 && n.value.fract() == 0.0 {
            return Some(n.value as u32);
        }
    }
    None
}
fn identifier(e: &Expression<'_>, name: &str) -> bool {
    matches!(e, Expression::Identifier(i) if i.name == name)
}
fn register(object: &Expression<'_>, index: &Expression<'_>) -> Option<u32> {
    identifier(object, "r").then(|| numeric(index)).flatten()
}
fn state_member(e: &Expression<'_>, name: &str) -> bool {
    matches!(e, Expression::StaticMemberExpression(m) if identifier(&m.object, "state") && m.property.name == name)
}

/// Recognize only the exporter's register/dispatcher prelude. Extra executable
/// statements or shadowed dispatcher bindings invalidate normal-flow inference.
fn prelude_generator(body: &FunctionBody<'_>) -> DecompilerResult<bool> {
    let mut bindings = BTreeMap::new();
    for (position, s) in body.statements.iter().enumerate() {
        match s {
            Statement::ForStatement(_) if position + 1 == body.statements.len() => (),
            Statement::VariableDeclaration(v) => {
                for d in &v.declarations {
                    let BindingPatternKind::BindingIdentifier(id) = &d.id.kind else {
                        return Err(error("Unsupported dispatcher prelude binding"));
                    };
                    if bindings
                        .insert(id.name.as_str(), (v.kind, d.init.as_ref()))
                        .is_some()
                    {
                        return Err(error("Duplicate dispatcher binding"));
                    }
                }
            }
            _ => return Err(error("Unsupported executable dispatcher prelude")),
        }
    }
    if bindings
        .keys()
        .any(|k| !matches!(*k, "strict" | "r" | "pc" | "caught"))
    {
        return Err(error("Unknown dispatcher prelude binding"));
    }
    if let Some((kind, init)) = bindings.get("strict") {
        if *kind != VariableDeclarationKind::Const
            || !matches!(init, Some(Expression::BooleanLiteral(_)))
        {
            return Err(error("Unsupported strict binding"));
        }
    }
    let Some((VariableDeclarationKind::Const, Some(r))) = bindings.get("r") else {
        return Err(error("Missing exporter register binding"));
    };
    let generator = state_member(r, "r");
    if !generator
        && !matches!(r, Expression::CallExpression(c) if identifier(&c.callee, "objectCreate") && c.arguments.len() == 1 && matches!(&c.arguments[0], Argument::NullLiteral(_)))
    {
        return Err(error("Unsupported register initialization"));
    }
    let Some((VariableDeclarationKind::Let, Some(pc))) = bindings.get("pc") else {
        return Err(error("Missing exporter PC binding"));
    };
    let Some((VariableDeclarationKind::Let, caught)) = bindings.get("caught") else {
        return Err(error("Missing exporter caught binding"));
    };
    if (generator
        && (!state_member(pc, "pc") || !caught.is_some_and(|e| state_member(e, "caught"))))
        || (!generator && (numeric(pc) != Some(0) || caught.is_some()))
    {
        return Err(error("Unsupported dispatcher entry state"));
    }
    Ok(generator)
}

#[derive(Default)]
struct Syntax {
    reads: Vec<(u32, Span)>,
    writes: Vec<(u32, Span)>,
    unsupported: bool,
    pc_writes: Vec<Span>,
    work: usize,
    literals: Vec<Span>,
    slot_writes: Vec<SlotWrite>,
    capture_symbols: bool,
    capture_properties: bool,
    property_writes: Vec<PropertyWrite>,
    malformed_properties: bool,
}
impl<'a> Visit<'a> for Syntax {
    fn visit_string_literal(&mut self, it: &StringLiteral<'a>) {
        if self.capture_symbols {
            self.literals.push(it.span);
        }
    }
    fn visit_expression(&mut self, it: &Expression<'a>) {
        self.work += 1;
        if self.work <= SYNTAX_WORK_CAP {
            walk::walk_expression(self, it);
        }
    }
    fn visit_statement(&mut self, it: &Statement<'a>) {
        self.work += 1;
        if self.work <= SYNTAX_WORK_CAP {
            walk::walk_statement(self, it);
        }
    }
    fn visit_computed_member_expression(&mut self, it: &ComputedMemberExpression<'a>) {
        if let Some(r) = register(&it.object, &it.expression) {
            self.reads.push((r, it.span));
        } else if identifier(&it.object, "r") {
            self.unsupported = true;
        }
        walk::walk_computed_member_expression(self, it);
    }
    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if self.capture_properties {
            let target = match &it.left {
                AssignmentTarget::ComputedMemberExpression(m) if !identifier(&m.object, "r") => {
                    let numeric_slot = matches!(&m.object, Expression::StaticMemberExpression(s) if s.property.name == "slots")
                        && numeric(&m.expression).is_some();
                    (!numeric_slot).then_some((
                        m.object.span(),
                        m.expression.span(),
                        "computed_assignment",
                    ))
                }
                AssignmentTarget::StaticMemberExpression(m) if !identifier(&m.object, "r") => {
                    Some((m.object.span(), m.property.span, "static_assignment"))
                }
                _ => None,
            };
            if let Some((object, key, form)) = target {
                if it.operator.is_assign() {
                    self.property_writes.push(PropertyWrite {
                        span: it.span,
                        object,
                        key,
                        value: it.right.span(),
                        form,
                        helper_arguments: vec![],
                    });
                } else {
                    self.malformed_properties = true;
                }
            }
        }
        if self.capture_symbols {
            if let AssignmentTarget::ComputedMemberExpression(m) = &it.left {
                if let Expression::StaticMemberExpression(slots) = &m.object {
                    if slots.property.name == "slots" && it.operator.is_assign() {
                        if let Some(slot) = numeric(&m.expression) {
                            self.slot_writes.push(SlotWrite {
                                slot,
                                environment: slots.object.span(),
                                value: it.right.span(),
                            });
                        }
                    }
                }
            }
        }
        if matches!(&it.left, AssignmentTarget::AssignmentTargetIdentifier(i) if i.name == "pc") {
            self.pc_writes.push(it.span);
        }
        if matches!(&it.left, AssignmentTarget::AssignmentTargetIdentifier(i) if i.name == "r" || i.name == "state")
        {
            self.unsupported = true;
        }
        if let AssignmentTarget::ComputedMemberExpression(m) = &it.left {
            if let Some(r) = register(&m.object, &m.expression) {
                self.writes.push((r, it.span));
                if it.operator.is_assign() {
                    self.visit_expression(&it.right);
                    return;
                }
            } else if identifier(&m.object, "r") {
                self.unsupported = true;
            }
        }
        walk::walk_assignment_expression(self, it);
    }
    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if self.capture_properties {
            if let Expression::Identifier(callee) = &it.callee {
                let arity = match callee.name.as_str() {
                    "put" => Some(5),
                    "own" => Some(4),
                    _ => None,
                };
                if let Some(arity) = arity {
                    if it.optional
                        || it.arguments.len() != arity
                        || it
                            .arguments
                            .iter()
                            .any(|a| matches!(a, Argument::SpreadElement(_)))
                    {
                        self.malformed_properties = true;
                    } else {
                        self.property_writes.push(PropertyWrite {
                            span: it.span,
                            object: it.arguments[0].span(),
                            key: it.arguments[1].span(),
                            value: it.arguments[2].span(),
                            form: if arity == 5 { "put" } else { "own" },
                            helper_arguments: it.arguments[3..].iter().map(GetSpan::span).collect(),
                        });
                    }
                }
            }
        }
        walk::walk_call_expression(self, it);
    }
    fn visit_class(&mut self, it: &Class<'a>) {
        if self.capture_properties {
            // Instance fields are deferred scopes, not the enclosing PC's flow.
            self.malformed_properties = true;
        } else {
            walk::walk_class(self, it);
        }
    }
    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if matches!(&it.argument, SimpleAssignmentTarget::AssignmentTargetIdentifier(i) if i.name == "pc")
        {
            self.unsupported = true;
        }
        if let SimpleAssignmentTarget::ComputedMemberExpression(m) = &it.argument {
            if let Some(r) = register(&m.object, &m.expression) {
                self.writes.push((r, it.span));
            }
        }
        walk::walk_update_expression(self, it);
    }
    fn visit_function(&mut self, _it: &Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {
        self.unsupported = true;
    }
    fn visit_arrow_function_expression(&mut self, _it: &ArrowFunctionExpression<'a>) {
        self.unsupported = true;
    }
    fn visit_conditional_expression(&mut self, it: &ConditionalExpression<'a>) {
        let before = self.writes.len();
        walk::walk_conditional_expression(self, it);
        self.unsupported |= self.writes.len() != before;
    }
    fn visit_logical_expression(&mut self, it: &LogicalExpression<'a>) {
        let before = self.writes.len();
        walk::walk_logical_expression(self, it);
        self.unsupported |= self.writes.len() != before;
    }
    fn visit_variable_declarator(&mut self, it: &VariableDeclarator<'a>) {
        if !matches!(&it.id.kind, BindingPatternKind::BindingIdentifier(i) if i.name != "pc" && i.name != "r" && i.name != "state")
        {
            self.unsupported = true;
        }
        walk::walk_variable_declarator(self, it);
    }
    fn visit_yield_expression(&mut self, _it: &YieldExpression<'a>) {
        self.unsupported = true;
    }
    fn visit_await_expression(&mut self, _it: &AwaitExpression<'a>) {
        self.unsupported = true;
    }
}

struct Instruction {
    pc: u32,
    block: usize,
    span: Span,
    reads: Vec<(u32, Span)>,
    writes: Vec<(u32, Span)>,
    expressions: Vec<Span>,
    expressions_total: usize,
    literals: Vec<Span>,
    slot_writes: Vec<SlotWrite>,
    property_writes: Vec<PropertyWrite>,
}
struct PropertyWrite {
    span: Span,
    object: Span,
    key: Span,
    value: Span,
    form: &'static str,
    helper_arguments: Vec<Span>,
}
struct SlotWrite {
    slot: u32,
    environment: Span,
    value: Span,
}
struct Block {
    pc: u32,
    instructions: Vec<usize>,
    predecessors: Vec<usize>,
    unknown: bool,
    writes: BTreeMap<u32, Vec<usize>>,
}

struct SourceIndex {
    instructions: Vec<Instruction>,
    blocks: Vec<Block>,
    pc_index: BTreeMap<u32, usize>,
    edges: BTreeSet<(u32, u32)>,
    unknown: Vec<&'static str>,
    stopped: bool,
    control_work: usize,
    reads_sorted: bool,
}

#[derive(Default)]
struct IndexFeatures {
    expressions: bool,
    symbols: bool,
    properties: bool,
}

#[derive(Serialize)]
struct Excerpt {
    start: u32,
    end: u32,
    javascript: String,
    snippet_truncated: bool,
}
fn excerpt(source: &str, span: Span) -> Excerpt {
    let s = &source[span.start as usize..span.end as usize];
    let mut end = s.len().min(PREVIEW_CAP);
    while !s.is_char_boundary(end) {
        end -= 1;
    }
    Excerpt {
        start: span.start,
        end: span.end,
        javascript: s[..end].into(),
        snippet_truncated: end < s.len(),
    }
}
#[derive(Serialize)]
struct Definition {
    id: usize,
    pc: u32,
    block_pc: u32,
    register: u32,
    source: Excerpt,
    #[serde(skip_serializing_if = "Option::is_none")]
    expression: Option<serde_json::Value>,
}
#[derive(Serialize)]
struct Demand {
    owner_definition: Option<usize>,
    pc: u32,
    register: u32,
    read: Excerpt,
    candidates: Vec<usize>,
    unresolved: bool,
    same_pc_ambiguity: bool,
    cycle: bool,
    truncated: bool,
}
#[derive(Serialize)]
struct Report {
    schema_version: u32,
    schema: &'static str,
    semantics: &'static str,
    function: u32,
    pc: u32,
    unknown: Vec<&'static str>,
    truncated: bool,
    #[serde(skip)]
    dependency_truncated: bool,
    unresolved: bool,
    blocks: usize,
    normal_edges: Vec<(u32, u32)>,
    normal_edges_total: usize,
    normal_edges_returned: usize,
    normal_edges_truncated: bool,
    control_index_work: usize,
    limits: serde_json::Value,
    definitions: Vec<Definition>,
    demands: Vec<Demand>,
    #[serde(skip_serializing_if = "Option::is_none")]
    instruction_expressions: Option<Vec<serde_json::Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    instruction_expressions_total: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    instruction_expressions_omitted: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    expressions_truncated: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    expression_source: Option<serde_json::Value>,
}

#[derive(Clone, Copy)]
pub struct Query {
    pub function: u32,
    pub pc: u32,
    pub depth: usize,
    pub limit: usize,
    pub max_bytes: usize,
    pub expressions: bool,
}

/// Analyze one complete annotated F[function] fragment. Exception tuples are
/// (start, exclusive_end, target), obtained from HBC metadata, not inferred JS.
/// Unsupported control is explicitly unknown; candidates are never substitutions.
pub fn analyze_source(
    source: &str,
    function: u32,
    pc: u32,
    depth: usize,
    limit: usize,
    max_bytes: usize,
    exceptions: &[(u32, u32, u32)],
) -> DecompilerResult<Vec<u8>> {
    analyze_query(
        source,
        Query {
            function,
            pc,
            depth,
            limit,
            max_bytes,
            expressions: false,
        },
        exceptions,
    )
}

pub fn analyze_query(
    source: &str,
    query: Query,
    exceptions: &[(u32, u32, u32)],
) -> DecompilerResult<Vec<u8>> {
    bounds(query.depth, query.limit, query.max_bytes)?;
    if source.len() > SOURCE_CAP {
        return Err(error("origins source byte cap exceeded"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("origins requires valid complete exporter JS"));
    }
    let index = index_program(
        source,
        query.function,
        &parsed.program,
        exceptions,
        IndexFeatures {
            expressions: query.expressions,
            symbols: false,
            properties: false,
        },
    )?;
    let (mut result, _) = query_index(source, &index, query, None, None)?;
    if query.expressions {
        let root = index.pc_index[&query.pc];
        let root_spans = &index.instructions[root].expressions;
        let mut spans = root_spans.clone();
        spans.extend(
            result
                .definitions
                .iter()
                .map(|d| Span::new(d.source.start, d.source.end)),
        );
        let views = super::expression_view::views(source, &parsed.program, &spans)?;
        let truncated = views.iter().any(|v| v["truncated"] == true)
            || index.instructions[root].expressions_total > INSTRUCTION_EXPRESSION_CAP;
        let mut projected = views.into_iter();
        result.instruction_expressions = Some(projected.by_ref().take(root_spans.len()).collect());
        for definition in &mut result.definitions {
            definition.expression = projected.next();
        }
        result.expressions_truncated = Some(truncated);
        result.truncated |= truncated;
        result.expression_source = Some(expression_source(source));
    }
    let mut bytes = serde_json::to_vec(&result).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > query.max_bytes {
        return Err(error("origins output byte budget exceeded"));
    }
    Ok(bytes)
}

fn expression_source(source: &str) -> serde_json::Value {
    let mut end = source.len().min(128);
    while !source.is_char_boundary(end) {
        end -= 1;
    }
    serde_json::json!({
        "offset_unit": "utf8_bytes",
        "offset_origin": "complete_raw_exporter_fragment_without_inspection_header",
        "source_bytes": source.len(), "raw_fragment_prefix": &source[..end],
        "prefix_truncated": end < source.len(),
        "workspace_join": "find raw_fragment_prefix bytes after the inspection header; add that offset to source spans"
    })
}

fn index_program(
    source: &str,
    function: u32,
    program: &Program<'_>,
    exceptions: &[(u32, u32, u32)],
    features: IndexFeatures,
) -> DecompilerResult<SourceIndex> {
    let mut functions = Vec::new();
    for s in &program.body {
        if let Statement::ExpressionStatement(s) = s {
            if let Expression::AssignmentExpression(a) = &s.expression {
                if let AssignmentTarget::ComputedMemberExpression(m) = &a.left {
                    if identifier(&m.object, "F")
                        && numeric(&m.expression) == Some(function)
                        && a.operator.is_assign()
                    {
                        if let Expression::FunctionExpression(f) = &a.right {
                            functions.push(f);
                        }
                    }
                }
            }
        }
    }
    if functions.len() != 1 {
        return Err(error(
            "origins requires one exact typed F[function] wrapper",
        ));
    }
    let body = functions[0]
        .body
        .as_ref()
        .ok_or_else(|| error("Missing function body"))?;
    if functions[0].generator || functions[0].r#async {
        return Err(error("Native generator/async wrappers are not supported"));
    }
    let generator = prelude_generator(body)?;
    let loops: Vec<_> = body
        .statements
        .iter()
        .filter_map(|s| {
            if let Statement::ForStatement(f) = s {
                Some(f)
            } else {
                None
            }
        })
        .collect();
    if loops.len() != 1
        || loops[0].init.is_some()
        || loops[0].test.is_some()
        || loops[0].update.is_some()
    {
        return Err(error("Unsupported exporter dispatcher loop"));
    }
    let Statement::BlockStatement(loop_body) = &loops[0].body else {
        return Err(error("Missing dispatcher block"));
    };
    let [Statement::TryStatement(t)] = loop_body.body.as_slice() else {
        return Err(error("Missing dispatcher try"));
    };
    let [Statement::SwitchStatement(dispatch)] = t.block.body.as_slice() else {
        return Err(error("Ambiguous dispatcher switch"));
    };
    if !identifier(&dispatch.discriminant, "pc") || t.finalizer.is_some() {
        return Err(error("Unsupported dispatcher"));
    }
    let defaults: Vec<_> = dispatch.cases.iter().filter(|c| c.test.is_none()).collect();
    if defaults.len() != 1
        || dispatch.cases.last().is_none_or(|c| c.test.is_some())
        || !matches!(defaults[0].consequent.as_slice(), [Statement::ThrowStatement(s)] if matches!(&s.argument, Expression::NewExpression(n) if identifier(&n.callee, "ErrorCtor")))
    {
        return Err(error("Unsupported dispatcher default control"));
    }
    let mut unknown = Vec::new();
    // A resumed generator's registers/entry PC are external state. A handler
    // may observe any earlier throwing operation, including a partial PC write.
    if generator {
        unknown.push("generator_state");
    }
    let exceptional = !exceptions.is_empty() || t.handler.as_ref().is_none_or(|h| {
        !matches!(h.body.body.as_slice(), [Statement::ThrowStatement(s)] if identifier(&s.argument, "error"))
    });
    if exceptional {
        unknown.push("exception_flow");
    }
    let stopped = generator || exceptional;
    let prefix = format!(" HBC function {function}, PC ");
    let mut markers = Vec::new();
    for c in &program.comments {
        if !c.is_line() {
            continue;
        }
        let text = &source[c.content_span().start as usize..c.content_span().end as usize];
        if let Some(n) = text.strip_prefix(&prefix) {
            let n = n.parse::<u32>().map_err(|_| error("Invalid PC marker"))?;
            markers.push((c.span, n));
        }
    }
    let mut blocks = Vec::new();
    let mut instructions: Vec<Instruction> = Vec::new();
    let mut targets: Vec<Vec<u32>> = Vec::new();
    let mut pc_index = BTreeMap::new();
    let mut total_reads = 0;
    let mut total_writes = 0;
    let mut total_literals = 0;
    let mut total_slot_writes = 0;
    let mut total_property_writes = 0;
    let mut control_work = 0;
    let mut syntax_work = 0;
    for (case_position, case) in dispatch.cases.iter().enumerate() {
        let Some(test) = &case.test else {
            continue;
        };
        let block_pc = numeric(test).ok_or_else(|| error("Non-numeric dispatcher case"))?;
        if blocks.len() >= BLOCK_CAP {
            return Err(error("origins block cap exceeded"));
        }
        let [Statement::BlockStatement(b)] = case.consequent.as_slice() else {
            return Err(error("Unsupported dispatcher case shape"));
        };
        let bid = blocks.len();
        let mut block = Block {
            pc: block_pc,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            unknown: false,
            writes: BTreeMap::new(),
        };
        let mut current = None;
        let mut previous_end = b.span.start;
        for s in &b.body {
            control_work += 1;
            if control_work > CONTROL_WORK_CAP {
                return Err(error("origins control indexing work cap exceeded"));
            }
            let span = s.span();
            // Only lexer comments between direct statements are PC annotations.
            // Comments in strings, templates, or nested AST statements cannot label PCs.
            let lo = markers.partition_point(|(c, _)| c.start < previous_end);
            let hi = markers.partition_point(|(c, _)| c.end <= span.start);
            previous_end = span.end;
            if hi > lo {
                if hi - lo != 1 {
                    return Err(error("Ambiguous PC annotation"));
                }
                let m = markers[lo].1;
                if instructions.len() >= DEFINITION_CAP
                    || pc_index.insert(m, instructions.len()).is_some()
                {
                    return Err(error("Duplicate PC or instruction cap exceeded"));
                }
                current = Some(instructions.len());
                block.instructions.push(instructions.len());
                instructions.push(Instruction {
                    pc: m,
                    block: bid,
                    span,
                    reads: Vec::new(),
                    writes: Vec::new(),
                    expressions: Vec::new(),
                    expressions_total: 0,
                    literals: Vec::new(),
                    slot_writes: Vec::new(),
                    property_writes: Vec::new(),
                });
            }
            let id = current.ok_or_else(|| error("Missing exact PC annotation"))?;
            let ins = &mut instructions[id];
            ins.span.end = span.end;
            if features.expressions {
                if let Statement::ExpressionStatement(e) = s {
                    ins.expressions_total += 1;
                    if ins.expressions.len() < INSTRUCTION_EXPRESSION_CAP {
                        ins.expressions.push(e.expression.span());
                    }
                }
            }
            let mut syntax = Syntax {
                capture_symbols: features.symbols,
                capture_properties: features.properties,
                ..Syntax::default()
            };
            syntax.visit_statement(s);
            if syntax.malformed_properties {
                return Err(error("properties requires simple property assignments and non-optional put/own calls with exact arity and no spreads; class scopes are unsupported"));
            }
            syntax_work += syntax.work;
            if syntax_work > SYNTAX_WORK_CAP {
                return Err(error("origins syntax indexing work cap exceeded"));
            }
            if !syntax.pc_writes.is_empty() {
                let direct = match s {
                    Statement::ExpressionStatement(e) => match &e.expression {
                        Expression::AssignmentExpression(a) => Some(a.span),
                        _ => None,
                    },
                    _ => None,
                };
                block.unknown |=
                    syntax.pc_writes.len() != 1 || direct != syntax.pc_writes.first().copied();
            }
            total_reads += syntax.reads.len();
            total_writes += syntax.writes.len();
            total_literals += syntax.literals.len();
            total_slot_writes += syntax.slot_writes.len();
            total_property_writes += syntax.property_writes.len();
            if total_reads > READ_CAP
                || total_writes > DEFINITION_CAP
                || total_literals > DEFINITION_CAP
                || total_slot_writes > DEFINITION_CAP
                || total_property_writes > DEFINITION_CAP
            {
                return Err(error("origins syntax cap exceeded"));
            }
            ins.reads.extend(syntax.reads);
            ins.writes.extend(syntax.writes);
            ins.literals.extend(syntax.literals);
            ins.slot_writes.extend(syntax.slot_writes);
            ins.property_writes.extend(syntax.property_writes);
            block.unknown |= syntax.unsupported;
        }
        if block.instructions.first().map(|&i| instructions[i].pc) != Some(block_pc) {
            return Err(error("Case PC does not match first instruction"));
        }
        for &iid in &block.instructions {
            for &(reg, _) in &instructions[iid].writes {
                control_work += 1;
                if control_work > CONTROL_WORK_CAP {
                    return Err(error("origins control indexing work cap exceeded"));
                }
                let ids = block.writes.entry(reg).or_default();
                if ids.last() != Some(&iid) {
                    ids.push(iid);
                }
            }
        }
        let mut outgoing = Vec::new();
        let mut falls = true;
        for (i, s) in b.body.iter().enumerate() {
            control_work += 1;
            if control_work > CONTROL_WORK_CAP {
                return Err(error("origins control indexing work cap exceeded"));
            }
            match s {
                Statement::ExpressionStatement(e) => {
                    if let Expression::AssignmentExpression(a) = &e.expression {
                        if matches!(&a.left, AssignmentTarget::AssignmentTargetIdentifier(id) if id.name == "pc")
                        {
                            falls = false;
                            let terminal = i + 2 == b.body.len()
                                && matches!(&b.body[i+1], Statement::ContinueStatement(c) if c.label.is_none());
                            if !terminal || !a.operator.is_assign() {
                                block.unknown = true;
                                continue;
                            }
                            if let Some(n) = numeric(&a.right) {
                                outgoing.push(n);
                            } else if let Expression::ConditionalExpression(c) = &a.right {
                                if let (Some(a), Some(b)) =
                                    (numeric(&c.consequent), numeric(&c.alternate))
                                {
                                    outgoing.extend([a, b]);
                                } else {
                                    block.unknown = true;
                                }
                            } else {
                                block.unknown = true;
                            }
                        }
                    }
                }
                Statement::ReturnStatement(_) | Statement::ThrowStatement(_) => {
                    falls = false;
                    if i + 1 != b.body.len() {
                        block.unknown = true;
                    }
                }
                Statement::ContinueStatement(c) => {
                    falls = false;
                    if c.label.is_some() || i == 0 || outgoing.is_empty() {
                        block.unknown = true;
                    }
                }
                Statement::VariableDeclaration(_) | Statement::EmptyStatement(_) => (),
                _ => {
                    block.unknown = true;
                    falls = false;
                }
            }
        }
        outgoing.sort_unstable();
        outgoing.dedup();
        if falls {
            if let Some(next) = dispatch
                .cases
                .get(case_position + 1)
                .and_then(|c| c.test.as_ref())
                .and_then(numeric)
            {
                outgoing.push(next);
            }
        }
        targets.push(outgoing);
        blocks.push(block);
    }
    let mut block_index = BTreeMap::new();
    for (i, b) in blocks.iter().enumerate() {
        if block_index.insert(b.pc, i).is_some() {
            return Err(error("Duplicate dispatch case"));
        }
    }
    if !block_index.contains_key(&0) {
        return Err(error("Missing dispatcher entry case 0"));
    }
    let mut edges = BTreeSet::new();
    for i in 0..blocks.len() {
        blocks[i].unknown |= targets[i].iter().any(|p| !block_index.contains_key(p));
        for target in &targets[i] {
            control_work += 1;
            if control_work > CONTROL_WORK_CAP {
                return Err(error("origins control indexing work cap exceeded"));
            }
            if let Some(&j) = block_index.get(target) {
                if !blocks[i].unknown {
                    edges.insert((blocks[i].pc, *target));
                    blocks[j].predecessors.push(i);
                }
            } else {
                blocks[i].unknown = true;
            }
        }
        if edges.len() > EDGE_CAP {
            return Err(error("origins edge cap exceeded"));
        }
    }
    if blocks.iter().any(|b| b.unknown) {
        unknown.push("unsupported_control_or_write");
    }
    for instruction in &mut instructions {
        if features.properties {
            instruction.reads.sort_by_key(|(_, s)| (s.start, s.end));
        }
        instruction.literals.sort_by_key(|s| (s.start, s.end));
        instruction
            .property_writes
            .sort_by_key(|p| (p.span.start, p.span.end));
    }
    Ok(SourceIndex {
        instructions,
        blocks,
        pc_index,
        edges,
        unknown,
        stopped,
        control_work,
        reads_sorted: features.properties,
    })
}

fn scoped_reads(reads: &[(u32, Span)], scope: Span, sorted: bool) -> &[(u32, Span)] {
    if sorted {
        let lo = reads.partition_point(|(_, s)| s.start < scope.start);
        let hi = reads.partition_point(|(_, s)| s.start < scope.end);
        &reads[lo..hi]
    } else {
        reads
    }
}

fn query_index(
    source: &str,
    index: &SourceIndex,
    query: Query,
    scope: Option<Span>,
    remaining_work: Option<usize>,
) -> DecompilerResult<(Report, usize)> {
    let Query {
        function,
        pc,
        depth,
        limit,
        max_bytes,
        expressions,
    } = query;
    let SourceIndex {
        instructions,
        blocks,
        pc_index,
        edges,
        unknown,
        stopped,
        control_work,
        reads_sorted,
    } = index;
    let root = *pc_index
        .get(&pc)
        .ok_or_else(|| error(format!("Unknown exact PC {pc}")))?;
    // Unknown destinations may reach any block: retain known candidates but
    // explicitly mark every demand incomplete, never certify a sole definition.
    let globally_unknown = !unknown.is_empty();
    let mut definitions = BTreeMap::new();
    let mut demands = Vec::new();
    let mut queue =
        VecDeque::from([(root, scope.unwrap_or(instructions[root].span), None, 0usize)]);
    let mut expanded = BTreeSet::new();
    let mut truncated = false;
    let work_cap = remaining_work.map_or(limit.saturating_mul(128), |n| {
        n.min(limit.saturating_mul(128))
    });
    let mut work = 0;
    while let Some((instruction, scope, owner, level)) = queue.pop_front() {
        if !expanded.insert((instruction, scope.start, scope.end)) {
            continue;
        }
        for &(reg, read_span) in
            scoped_reads(&instructions[instruction].reads, scope, *reads_sorted)
        {
            if read_span.start < scope.start || read_span.end > scope.end {
                continue;
            }
            if demands.len() >= work_cap {
                truncated = true;
                break;
            }
            let ins = &instructions[instruction];
            let mut demand = Demand {
                owner_definition: owner,
                pc: ins.pc,
                register: reg,
                read: excerpt(source, read_span),
                candidates: Vec::new(),
                unresolved: globally_unknown,
                same_pc_ambiguity: false,
                cycle: false,
                truncated: false,
            };
            if *stopped {
                demands.push(demand);
                continue;
            }
            // Iterative DFS gray/black states distinguish cycles from diamond
            // reconvergence without copying a path for each incoming edge.
            let mut pending = vec![(ins.block, Some(instruction), false)];
            let mut visited = BTreeSet::new();
            let mut active = BTreeSet::new();
            while let Some((bid, cutoff, leaving)) = pending.pop() {
                work += 1;
                if work > work_cap {
                    demand.truncated = true;
                    demand.unresolved = true;
                    truncated = true;
                    break;
                }
                if leaving {
                    active.remove(&(bid, cutoff));
                    visited.insert((bid, cutoff));
                    continue;
                }
                if visited.contains(&(bid, cutoff)) {
                    continue;
                }
                if !active.insert((bid, cutoff)) {
                    demand.cycle = true;
                    demand.unresolved = true;
                    continue;
                }
                pending.push((bid, cutoff, true));
                let b = &blocks[bid];
                if b.unknown {
                    demand.unresolved = true;
                    continue;
                }
                let mut found = false;
                let ids = b.writes.get(&reg).map(Vec::as_slice).unwrap_or(&[]);
                let end = cutoff.map_or(ids.len(), |c| ids.partition_point(|&i| i <= c));
                for &iid in ids[..end].iter().rev() {
                    let writes: Vec<_> = instructions[iid]
                        .writes
                        .iter()
                        .filter(|(r, span)| {
                            // An enclosing assignment finishes after its RHS read.
                            // Other same-PC writes still retain explicit ambiguity.
                            *r == reg
                                && !(cutoff == Some(iid)
                                    && span.start <= read_span.start
                                    && read_span.end <= span.end)
                        })
                        .collect();
                    if writes.is_empty() {
                        continue;
                    }
                    if writes.len() > 1 {
                        demand.same_pc_ambiguity = true;
                        demand.unresolved = true;
                    }
                    if cutoff == Some(iid) {
                        demand.same_pc_ambiguity = true;
                        demand.unresolved = true;
                    }
                    for &&(_, span) in &writes {
                        work += 1;
                        if work > work_cap {
                            demand.truncated = true;
                            demand.unresolved = true;
                            truncated = true;
                            break;
                        }
                        let key = (iid, reg, span.start);
                        let id = if let Some(d) = definitions.get(&key) {
                            let d: &Definition = d;
                            d.id
                        } else {
                            if definitions.len() >= limit {
                                demand.truncated = true;
                                demand.unresolved = true;
                                truncated = true;
                                continue;
                            }
                            let id = definitions.len();
                            definitions.insert(
                                key,
                                Definition {
                                    id,
                                    pc: instructions[iid].pc,
                                    block_pc: b.pc,
                                    register: reg,
                                    source: excerpt(source, span),
                                    expression: None,
                                },
                            );
                            id
                        };
                        demand.candidates.push(id);
                        if level < depth {
                            queue.push_back((iid, span, Some(id), level + 1));
                        } else if scoped_reads(&instructions[iid].reads, span, *reads_sorted)
                            .iter()
                            .any(|(_, s)| s.start >= span.start && s.end <= span.end)
                        {
                            demand.truncated = true;
                            truncated = true;
                        }
                    }
                    if cutoff != Some(iid) {
                        found = true;
                        break;
                    }
                }
                if !found {
                    if b.predecessors.is_empty() || b.pc == 0 {
                        demand.unresolved = true;
                    }
                    for &p in b.predecessors.iter().rev() {
                        if active.contains(&(p, Some(instruction))) {
                            // Re-enter the initial block at its end: a loop may
                            // write after the selected instruction's read.
                            demand.cycle = true;
                        }
                        pending.push((p, None, false));
                    }
                }
            }
            demand.candidates.sort_unstable();
            demand.candidates.dedup();
            if demand.candidates.is_empty() {
                demand.unresolved = true;
            }
            demands.push(demand);
        }
        if work > work_cap || demands.len() >= work_cap {
            truncated = true;
            break;
        }
    }
    let unresolved = demands.iter().any(|d| d.unresolved) || globally_unknown;
    let mut definitions: Vec<_> = definitions.into_values().collect();
    definitions.sort_by_key(|d| d.id);
    let normal_edges_total = edges.len();
    let normal_edges_truncated = normal_edges_total > DISPLAY_EDGE_CAP;
    let normal_edges: Vec<_> = edges.iter().copied().take(DISPLAY_EDGE_CAP).collect();
    let charged_work = work.saturating_add(demands.len()).max(1).min(work_cap);
    let result = Report {
        schema_version: 1,
        schema: "origins-v1",
        semantics:
            "candidate definitions only; no values, heap, captured slots, or constructor semantics",
        function,
        pc,
        unknown: unknown.clone(),
        truncated: truncated || normal_edges_truncated,
        dependency_truncated: truncated,
        unresolved,
        blocks: blocks.len(),
        normal_edges_total,
        normal_edges_returned: normal_edges.len(),
        normal_edges_truncated,
        normal_edges,
        control_index_work: *control_work,
        limits: serde_json::json!({
            "source_bytes": SOURCE_CAP,
            "output_bytes": max_bytes,
            "blocks": BLOCK_CAP,
            "normal_edges": EDGE_CAP,
            "displayed_edges": DISPLAY_EDGE_CAP,
            "instructions_or_writes": DEFINITION_CAP,
            "reads": READ_CAP,
            "control_index_work": CONTROL_WORK_CAP,
            "syntax_index_work": SYNTAX_WORK_CAP,
            "definition_nodes": limit,
            "dependency_depth": depth,
            "query_work": work_cap,
            "snippet_bytes": PREVIEW_CAP
        }),
        definitions,
        demands,
        instruction_expressions: None,
        instruction_expressions_total: expressions.then_some(instructions[root].expressions_total),
        instruction_expressions_omitted: expressions.then_some(
            instructions[root]
                .expressions_total
                .saturating_sub(INSTRUCTION_EXPRESSION_CAP),
        ),
        expressions_truncated: expressions.then_some(false),
        expression_source: None,
    };
    Ok((result, charged_work))
}

#[derive(Clone)]
pub struct SymbolOptions {
    pub function: u32,
    pub depth: usize,
    pub definition_limit: usize,
    pub literal_limit: usize,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
    pub slots: Vec<u32>,
    pub matches: Vec<String>,
}

#[derive(Clone)]
pub struct PropertyOptions {
    pub function: u32,
    pub depth: usize,
    pub definition_limit: usize,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
    pub matches: Vec<String>,
}

pub const DEFAULT_PROPERTY_SCAN_WORK: usize = 1_048_576;
pub const MAX_PROPERTY_SCAN_WORK: usize = 16_777_216;
const PROPERTY_FILTER_WORK_CAP: usize = 33_554_432;

fn property_match(
    matcher: &regex::Regex,
    source: &str,
    span: Span,
    work: &mut usize,
) -> DecompilerResult<Option<Span>> {
    let bytes = (span.end - span.start) as usize;
    if bytes > PROPERTY_FILTER_WORK_CAP - *work {
        return Err(error("properties filter work cap exceeded before stdout"));
    }
    *work += bytes;
    Ok(matcher
        .find(&source[span.start as usize..span.end as usize])
        .map(|m| Span::new(span.start + m.start() as u32, span.start + m.end() as u32)))
}

fn property_role(
    source: &str,
    span: Span,
    report: &Report,
) -> DecompilerResult<(serde_json::Value, BTreeMap<String, serde_json::Value>)> {
    let mut ids = BTreeMap::new();
    let mut definitions = BTreeMap::new();
    for d in &report.definitions {
        let id = format!(
            "{}:{}:{}:{}",
            report.function, d.register, d.source.start, d.source.end
        );
        ids.insert(d.id, id.clone());
        definitions.insert(
            id,
            serde_json::json!({"pc":d.pc,"register":d.register,"source":d.source}),
        );
    }
    let lookup = |id| {
        ids.get(&id)
            .cloned()
            .ok_or_else(|| error("Invalid property candidate ID"))
    };
    let dependencies = report
        .demands
        .iter()
        .map(|d| {
            let candidates = d
                .candidates
                .iter()
                .map(|&id| lookup(id))
                .collect::<DecompilerResult<Vec<_>>>()?;
            let owner = d.owner_definition.map(lookup).transpose()?;
            Ok(
                serde_json::json!({"owner_definition":owner,"pc":d.pc,"register":d.register,
            "read_span":[d.read.start,d.read.end],"candidates":candidates,"unresolved":d.unresolved,
            "same_pc_ambiguity":d.same_pc_ambiguity,"cycle":d.cycle,"truncated":d.truncated}),
            )
        })
        .collect::<DecompilerResult<Vec<_>>>()?;
    Ok((
        serde_json::json!({"source":excerpt(source,span),"queried":true,
        "definition_ids":ids.into_values().collect::<Vec<_>>(),"dependencies":dependencies,
        "unresolved":report.unresolved,"truncated":report.dependency_truncated,"unknown":report.unknown}),
        definitions,
    ))
}

/// Keep raw object/key/value roles distinct; source candidates are not field values.
pub fn analyze_properties_source(
    source: &str,
    options: &PropertyOptions,
    exceptions: &[(u32, u32, u32)],
) -> DecompilerResult<Vec<u8>> {
    let o = options;
    bounds(o.depth, o.definition_limit, o.max_bytes)?;
    if !(1..=1000).contains(&o.limit)
        || !(1..=MAX_PROPERTY_SCAN_WORK).contains(&o.scan_work)
        || o.matches.len() > 64
        || o.matches.iter().any(|m| m.is_empty() || m.len() > 1024)
    {
        return Err(error("properties bounds: limit 1..1000, scan_work 1..16777216, at most 64 nonempty matches of 1024 bytes"));
    }
    if source.len() > SOURCE_CAP {
        return Err(error("properties source byte cap exceeded"));
    }
    let matcher = if o.matches.is_empty() {
        None
    } else {
        let pattern = o
            .matches
            .iter()
            .map(|m| regex::escape(m))
            .collect::<Vec<_>>()
            .join("|");
        Some(
            RegexBuilder::new(&pattern)
                .case_insensitive(true)
                .size_limit(8 * 1024 * 1024)
                .build()
                .map_err(|e| error(e.to_string()))?,
        )
    };
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("properties requires valid complete exporter JS"));
    }
    let index = index_program(
        source,
        o.function,
        &parsed.program,
        exceptions,
        IndexFeatures {
            properties: true,
            ..IndexFeatures::default()
        },
    )?;
    let stores_total: usize = index
        .instructions
        .iter()
        .map(|i| i.property_writes.len())
        .sum();
    let mut ordinal = 0;
    let mut scanned = 0;
    let mut work = 0;
    let mut filter_work = 0;
    let mut unqueried_values = 0;
    let mut skipped_keys = 0;
    let mut incomplete_keys = 0;
    let mut next_offset = None;
    let mut rows = Vec::new();
    let mut definitions = BTreeMap::<String, serde_json::Value>::new();
    let mut serialized_records_bytes = 0usize;
    'instructions: for instruction in &index.instructions {
        for store in &instruction.property_writes {
            let cursor = ordinal;
            ordinal += 1;
            if cursor < o.offset {
                continue;
            }
            if rows.len() == o.limit || work == o.scan_work {
                next_offset = Some(cursor);
                break 'instructions;
            }
            scanned += 1;
            let direct_match = match &matcher {
                Some(m) => property_match(m, source, store.key, &mut filter_work)?,
                None => None,
            };
            let key_read_start = instruction
                .reads
                .partition_point(|(_, s)| s.start < store.key.start);
            let key_reads = instruction
                .reads
                .get(key_read_start)
                .is_some_and(|(_, s)| s.start < store.key.end && s.end <= store.key.end);
            if matcher.is_some() && direct_match.is_none() && !key_reads {
                skipped_keys += 1;
                continue;
            }
            let query = Query {
                function: o.function,
                pc: instruction.pc,
                depth: o.depth,
                limit: o.definition_limit,
                max_bytes: o.max_bytes,
                expressions: false,
            };
            let (key_result, used) = query_index(
                source,
                &index,
                query,
                Some(store.key),
                Some(o.scan_work - work),
            )?;
            work += used;
            incomplete_keys += usize::from(
                key_result.unresolved
                    || key_result.dependency_truncated
                    || !key_result.unknown.is_empty(),
            );
            let mut evidence = Vec::new();
            let mut evidence_count = 0usize;
            if let Some(matcher) = &matcher {
                let mut add_match = |pc, matched: Option<Span>, id: Option<String>| {
                    if let Some(span) = matched {
                        evidence_count += 1;
                        if evidence.len() < 8 {
                            evidence.push(serde_json::json!({"pc":pc,"definition_id":id,"source":excerpt(source,span)}));
                        }
                    }
                };
                add_match(instruction.pc, direct_match, None);
                for d in &key_result.definitions {
                    add_match(
                        d.pc,
                        property_match(
                            matcher,
                            source,
                            Span::new(d.source.start, d.source.end),
                            &mut filter_work,
                        )?,
                        Some(format!(
                            "{}:{}:{}:{}",
                            o.function, d.register, d.source.start, d.source.end
                        )),
                    );
                }
                if evidence_count == 0 {
                    continue;
                }
            }
            let (key, mut local_definitions) = property_role(source, store.key, &key_result)?;
            let value = if work < o.scan_work {
                let (result, used) = query_index(
                    source,
                    &index,
                    query,
                    Some(store.value),
                    Some(o.scan_work - work),
                )?;
                work += used;
                let (role, defs) = property_role(source, store.value, &result)?;
                local_definitions.extend(defs);
                role
            } else {
                unqueried_values += 1;
                serde_json::json!({"source":excerpt(source,store.value),"queried":false,"definition_ids":[],"dependencies":[],"unresolved":true,"truncated":true,"unknown":["aggregate_query_work_exhausted_before_value"]})
            };
            let row = serde_json::json!({"function":o.function,"pc":instruction.pc,"store_ordinal":cursor,"form":store.form,
                "object":excerpt(source,store.object),"key":key,"value":value,
                "helper_arguments":store.helper_arguments.iter().map(|&s|excerpt(source,s)).collect::<Vec<_>>(),
                "match_evidence_count":evidence_count,"match_evidence_truncated":evidence_count>evidence.len(),"match_evidence":evidence});
            serialized_records_bytes = serialized_records_bytes.saturating_add(
                serde_json::to_vec(&row)
                    .map_err(|e| error(e.to_string()))?
                    .len()
                    + 1,
            );
            for (id, definition) in &local_definitions {
                if !definitions.contains_key(id) {
                    serialized_records_bytes = serialized_records_bytes.saturating_add(
                        serde_json::to_vec(id)
                            .map_err(|e| error(e.to_string()))?
                            .len()
                            + 2
                            + serde_json::to_vec(definition)
                                .map_err(|e| error(e.to_string()))?
                                .len(),
                    );
                }
            }
            if serialized_records_bytes > o.max_bytes {
                return Err(error(
                    "properties output byte budget exceeded before stdout",
                ));
            }
            rows.push(row);
            definitions.extend(local_definitions);
        }
    }
    let continuation_query = next_offset.map(|offset| {
        let mut flags = vec!["--offset".to_owned(),offset.to_string(),"--depth".to_owned(),o.depth.to_string(),"--definition-limit".to_owned(),o.definition_limit.to_string(),"--limit".to_owned(),o.limit.to_string(),"--max-bytes".to_owned(),o.max_bytes.to_string(),"--scan-work".to_owned(),o.scan_work.to_string()];
        for term in &o.matches { flags.extend(["--match".to_owned(),term.clone()]); }
        serde_json::json!({"command":"hermes-dec-rs","subcommand":"properties","input":"INPUT","function":o.function,"flags":flags,
        "options":{"offset":offset,"depth":o.depth,"definition_limit":o.definition_limit,"limit":o.limit,"max_bytes":o.max_bytes,"scan_work":o.scan_work,"matches":o.matches},
        "semantics":"Continue the raw property-store scan with original input and chosen binary. Paging does not repair earlier omitted or unqueried dependencies. Options and flags are data, not shell code."})});
    let report = serde_json::json!({"schema_version":1,"schema":"properties-v1","function":o.function,
        "semantics":"Property-write source shapes and normal-flow candidate definitions only. Object identity, property values, helper results, getters/setters and captured frames are not resolved. No JS is evaluated.",
        "source":"export_function_fragments","parsed_source_complete":true,"expression_source":expression_source(source),
        "stores_total":stores_total,"offset":o.offset,"scanned":scanned,"next_offset":next_offset,"scan_complete":next_offset.is_none(),
        "query_work_used":work,"query_work_cap":o.scan_work,"query_work_truncated":work==o.scan_work&&(next_offset.is_some()||unqueried_values>0||incomplete_keys>0),
        "filter_work_used":filter_work,"filter_work_cap":PROPERTY_FILTER_WORK_CAP,"values_not_queried":unqueried_values,
        "keys_skipped_without_register_reads":skipped_keys,"key_search_incomplete_rows":incomplete_keys,
        "depth":o.depth,"definition_limit":o.definition_limit,"limit":o.limit,"matches":o.matches,
        "match_policy":"OR escaped case-insensitive substrings in raw key expressions and candidate key definition syntax, not object/value expressions, decoded strings, runtime key names or values.",
        "negative_result_policy":"Unscanned stores and omitted/unresolved key or value dependencies are not evidence of runtime absence. scan_complete covers stores only; queried=false means that role was not analyzed.",
        "ordinal_policy":"Property-write source order, not runtime execution order or matched-row position.",
        "continuation_query":continuation_query,"definitions":definitions,"rows":rows});
    let mut bytes = serde_json::to_vec(&report).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > o.max_bytes {
        return Err(error(
            "properties output byte budget exceeded before stdout",
        ));
    }
    Ok(bytes)
}

pub const DEFAULT_SYMBOL_SCAN_WORK: usize = 1_048_576;
pub const MAX_SYMBOL_SCAN_WORK: usize = 16_777_216;
const SYMBOL_FILTER_WORK_CAP: usize = 33_554_432;
const SYMBOL_FILTER_EXAMPLE_CAP: usize = 3;
const SYMBOL_LITERAL_RECORD_CAP: usize = 262_144;
const SYMBOL_LITERAL_BYTE_CAP: usize = 128 * 1024 * 1024;
const SYMBOL_LITERAL_ROW_CAP: usize = 512;

#[derive(Default)]
struct LiteralWork {
    records: usize,
    bytes: usize,
}

#[derive(Serialize)]
struct LiteralMention {
    pc: u32,
    definition_id: Option<String>,
    register: Option<u32>,
    source: Excerpt,
    matches_filter: bool,
}

fn filter_literal_diagnostics(
    source: &str,
    index: &SourceIndex,
    terms: &[String],
    folded: &[String],
) -> serde_json::Value {
    if terms.is_empty() {
        return serde_json::Value::Null;
    }
    let total: usize = index.instructions.iter().map(|i| i.literals.len()).sum();
    let mut counts = vec![0usize; terms.len()];
    let mut examples = vec![Vec::<serde_json::Value>::new(); terms.len()];
    let mut inspected = 0usize;
    let mut work = 0usize;
    'instructions: for instruction in &index.instructions {
        for span in &instruction.literals {
            let raw = &source[span.start as usize..span.end as usize];
            // Charge input bytes before case conversion and expanded bytes
            // before comparisons; do not assume a Unicode expansion factor.
            if raw.len() > SYMBOL_FILTER_WORK_CAP - work {
                break 'instructions;
            }
            work += raw.len();
            let raw = raw.to_lowercase();
            let comparisons = raw.len().saturating_mul(terms.len());
            if comparisons > SYMBOL_FILTER_WORK_CAP - work {
                break 'instructions;
            }
            work += comparisons;
            inspected += 1;
            for ((count, examples), term) in counts.iter_mut().zip(&mut examples).zip(folded) {
                if raw.contains(term) {
                    *count += 1;
                    if examples.len() < SYMBOL_FILTER_EXAMPLE_CAP {
                        examples.push(
                            serde_json::json!({"pc":instruction.pc,"source":excerpt(source,*span)}),
                        );
                    }
                }
            }
        }
    }
    serde_json::json!({
        "scope":"Indexed instruction string-literal tokens, not RHS dependencies or runtime values. Nested function bodies and non-string identifier/property labels are excluded.",
        "terms": terms.iter().zip(counts).zip(examples).map(|((term,count),examples)|serde_json::json!({"term":term,"indexed_literal_mentions":count,"examples_truncated":count>examples.len(),"examples":examples})).collect::<Vec<_>>(),
        "tokens_total":total,"tokens_inspected":inspected,"complete":inspected==total,
        "charged_work":work,"work_cap":SYMBOL_FILTER_WORK_CAP,
        "example_limit_per_term":SYMBOL_FILTER_EXAMPLE_CAP,
        "interpretation":"A spelling may match literal syntax elsewhere without reaching a store RHS. No inspected matches is not runtime absence. Identifier spellings are not aliases for raw strings; check original JS spelling and scope."
    })
}

fn literal_mentions(
    source: &str,
    instruction: &Instruction,
    span: Span,
    definition: Option<(String, u32)>,
    matches: &[String],
    row_remaining: usize,
    work: &mut LiteralWork,
) -> (Vec<LiteralMention>, usize, bool) {
    let start = instruction
        .literals
        .partition_point(|s| s.start < span.start);
    let end = instruction
        .literals
        .partition_point(|s| s.end <= span.end)
        .max(start);
    let selected = &instruction.literals[start..end];
    let total = selected.len();
    let mut out = Vec::new();
    for &literal in selected {
        if out.len() == row_remaining || work.records == SYMBOL_LITERAL_RECORD_CAP {
            break;
        }
        let raw = &source[literal.start as usize..literal.end as usize];
        if raw.len() > SYMBOL_LITERAL_BYTE_CAP - work.bytes {
            break;
        }
        work.records += 1;
        work.bytes += raw.len();
        let folded = (!matches.is_empty()).then(|| raw.to_lowercase());
        out.push(LiteralMention {
            pc: instruction.pc,
            definition_id: definition.as_ref().map(|(id, _)| id.clone()),
            register: definition.as_ref().map(|(_, r)| *r),
            source: excerpt(source, literal),
            matches_filter: folded
                .as_ref()
                .is_some_and(|raw| matches.iter().any(|m| raw.contains(m))),
        });
    }
    let truncated = out.len() < total;
    (out, total, truncated)
}

/// Project raw string mentions from candidate slot-write RHS dependencies.
/// Slot/environment shape is navigation evidence, never lexical-frame identity.
pub fn analyze_symbols_source(
    source: &str,
    options: &SymbolOptions,
    exceptions: &[(u32, u32, u32)],
) -> DecompilerResult<Vec<u8>> {
    let o = options;
    bounds(o.depth, o.definition_limit, o.max_bytes)?;
    if !(1..=128).contains(&o.literal_limit)
        || !(1..=1000).contains(&o.limit)
        || !(1..=MAX_SYMBOL_SCAN_WORK).contains(&o.scan_work)
        || o.matches.len() > 64
        || o.matches.iter().any(|s| s.is_empty() || s.len() > 1024)
        || o.slots.len() > 4096
    {
        return Err(error("symbols bounds: literals 1..128, limit 1..1000, scan_work 1..16777216, at most 64 nonempty match terms of 1024 bytes and 4096 slots"));
    }
    if source.len() > SOURCE_CAP {
        return Err(error("symbols source byte cap exceeded"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("symbols requires valid complete exporter JS"));
    }
    let index = index_program(
        source,
        o.function,
        &parsed.program,
        exceptions,
        IndexFeatures {
            expressions: false,
            symbols: true,
            properties: false,
        },
    )?;
    let stores_total: usize = index.instructions.iter().map(|i| i.slot_writes.len()).sum();
    let matches: Vec<_> = o.matches.iter().map(|s| s.to_lowercase()).collect();
    let filter_diagnostics = filter_literal_diagnostics(source, &index, &o.matches, &matches);
    let literal_filter_excludes = filter_diagnostics["complete"] == true
        && filter_diagnostics["terms"].as_array().is_some_and(|terms| {
            terms
                .iter()
                .all(|term| term["indexed_literal_mentions"] == 0)
        });
    let wanted_slots: BTreeSet<_> = o.slots.iter().copied().collect();
    let mut definitions = BTreeMap::<String, serde_json::Value>::new();
    let mut rows = Vec::new();
    let mut scanned = 0;
    let mut work_used = 0;
    let mut next_offset = None;
    let mut query_work_truncated = false;
    let mut literal_work = LiteralWork::default();
    let mut literal_search_truncated_rows = 0usize;
    let mut dependency_search_incomplete_rows = 0usize;
    let mut dependency_queries_used = 0usize;
    let mut dependency_queries_skipped = 0usize;
    let mut serialized_records_bytes = 0usize;
    let mut ordinal = 0;
    for instruction in &index.instructions {
        for store in &instruction.slot_writes {
            let cursor = ordinal;
            ordinal += 1;
            if cursor < o.offset {
                continue;
            }
            if rows.len() >= o.limit
                || work_used == o.scan_work
                || literal_work.records == SYMBOL_LITERAL_RECORD_CAP
                || literal_work.bytes == SYMBOL_LITERAL_BYTE_CAP
            {
                next_offset = Some(cursor);
                query_work_truncated = work_used == o.scan_work;
                break;
            }
            scanned += 1;
            if !wanted_slots.is_empty() && !wanted_slots.contains(&store.slot) {
                continue;
            }
            // Every projected mention comes from this same indexed literal set.
            // A complete zero-match diagnostic excludes syntax rows, not values.
            if literal_filter_excludes {
                dependency_queries_skipped += 1;
                continue;
            }
            dependency_queries_used += 1;
            let (result, used) = query_index(
                source,
                &index,
                Query {
                    function: o.function,
                    pc: instruction.pc,
                    depth: o.depth,
                    limit: o.definition_limit,
                    max_bytes: o.max_bytes,
                    expressions: false,
                },
                Some(store.value),
                Some(o.scan_work - work_used),
            )?;
            work_used += used;
            dependency_search_incomplete_rows +=
                usize::from(result.truncated || result.unresolved || !result.unknown.is_empty());
            let (mut literals, mut literal_mentions_total, mut literal_search_truncated) =
                literal_mentions(
                    source,
                    instruction,
                    store.value,
                    None,
                    &matches,
                    SYMBOL_LITERAL_ROW_CAP,
                    &mut literal_work,
                );
            let mut ids = Vec::new();
            let mut local_definitions = BTreeMap::new();
            for definition in &result.definitions {
                let id = format!(
                    "{}:{}:{}:{}",
                    o.function, definition.register, definition.source.start, definition.source.end
                );
                let defined_at = index
                    .pc_index
                    .get(&definition.pc)
                    .ok_or_else(|| error("Missing candidate definition PC"))?;
                let (mentions, total, incomplete) = literal_mentions(
                    source,
                    &index.instructions[*defined_at],
                    Span::new(definition.source.start, definition.source.end),
                    Some((id.clone(), definition.register)),
                    &matches,
                    SYMBOL_LITERAL_ROW_CAP - literals.len(),
                    &mut literal_work,
                );
                literal_mentions_total = literal_mentions_total
                    .checked_add(total)
                    .ok_or_else(|| error("symbols literal mention count overflow"))?;
                literal_search_truncated |= incomplete;
                literals.extend(mentions);
                ids.push(id.clone());
                local_definitions.insert(id, serde_json::json!({
                    "pc": definition.pc, "register": definition.register, "source": definition.source
                }));
            }
            literal_search_truncated_rows += usize::from(literal_search_truncated);
            if !matches.is_empty() && !literals.iter().any(|s| s.matches_filter) {
                continue;
            }
            // Match retention is independent of the display cap; retain matching
            // tokens first rather than making a positive row hide all its evidence.
            literals.sort_by_key(|m| {
                (
                    !m.matches_filter,
                    m.pc,
                    m.source.start,
                    m.source.end,
                    m.definition_id.clone(),
                )
            });
            let dependencies: Vec<_> = result
                .demands
                .iter()
                .map(|d| {
                    let candidate_ids: Vec<_> = d
                        .candidates
                        .iter()
                        .map(|&id| {
                            ids.get(id)
                                .cloned()
                                .ok_or_else(|| error("Invalid candidate definition ID"))
                        })
                        .collect::<DecompilerResult<_>>()?;
                    let owner = d
                        .owner_definition
                        .map(|id| {
                            ids.get(id)
                                .cloned()
                                .ok_or_else(|| error("Invalid owner definition ID"))
                        })
                        .transpose()?;
                    Ok(
                        serde_json::json!({"owner_definition":owner,"pc":d.pc,"register":d.register,
                    "read_span":[d.read.start,d.read.end],"candidates":candidate_ids,
                    "unresolved":d.unresolved,"same_pc_ambiguity":d.same_pc_ambiguity,
                    "cycle":d.cycle,"truncated":d.truncated}),
                    )
                })
                .collect::<DecompilerResult<_>>()?;
            let row = serde_json::json!({
                "function":o.function,"pc":instruction.pc,"slot":store.slot,
                "store_ordinal":cursor,"environment":excerpt(source,store.environment),
                "value":excerpt(source,store.value),
                "literal_mentions":literals.into_iter().take(o.literal_limit).collect::<Vec<_>>(),
                "literal_mentions_total":literal_mentions_total,
                "literal_mentions_truncated":literal_mentions_total > o.literal_limit,
                "literal_search_complete":!literal_search_truncated,
                "definition_count":ids.len(),"definition_ids":ids,"dependencies":dependencies,
                "unresolved":result.unresolved,"truncated":result.truncated,
                "unknown":result.unknown,
            });
            serialized_records_bytes = serialized_records_bytes.saturating_add(
                serde_json::to_vec(&row)
                    .map_err(|e| error(e.to_string()))?
                    .len()
                    + 1,
            );
            for (id, definition) in &local_definitions {
                if !definitions.contains_key(id) {
                    serialized_records_bytes = serialized_records_bytes.saturating_add(
                        serde_json::to_vec(id)
                            .map_err(|e| error(e.to_string()))?
                            .len()
                            + 2
                            + serde_json::to_vec(definition)
                                .map_err(|e| error(e.to_string()))?
                                .len(),
                    );
                }
            }
            if serialized_records_bytes > o.max_bytes {
                return Err(error("symbols output byte budget exceeded before stdout"));
            }
            rows.push(row);
            definitions.extend(local_definitions);
        }
        if next_offset.is_some() {
            break;
        }
    }
    let continuation_query = next_offset.map(|offset| {
        let mut flags = vec![
            "--offset".to_owned(), offset.to_string(),
            "--depth".to_owned(), o.depth.to_string(),
            "--definition-limit".to_owned(), o.definition_limit.to_string(),
            "--literal-limit".to_owned(), o.literal_limit.to_string(),
            "--limit".to_owned(), o.limit.to_string(),
            "--max-bytes".to_owned(), o.max_bytes.to_string(),
            "--scan-work".to_owned(), o.scan_work.to_string(),
        ];
        for slot in &o.slots {
            flags.extend(["--slot".to_owned(), slot.to_string()]);
        }
        for term in &o.matches {
            flags.extend(["--match".to_owned(), term.clone()]);
        }
        serde_json::json!({"command":"hermes-dec-rs","subcommand":"symbols",
            "input":"INPUT","function":o.function,"flags":flags,
            "reason":"store_scan_incomplete",
            "semantics":"Continue at the next raw store ordinal, even if rows is empty. Earlier omitted dependencies are not repaired by paging. Argument tokens are not shell code; use the original input and chosen CLI."})
    });
    let report = serde_json::json!({
        "schema_version":1,"schema":"symbols-v1","function":o.function,
        "semantics":"Raw string mentions in candidate slot-write RHS dependencies, not slot values, symbol names, constructor results, runtime bindings or frame identities. No JS is evaluated.",
        "source":"export_function_fragments","parsed_source_complete":true,
        "expression_source":expression_source(source),"stores_total":stores_total,
        "offset":o.offset,"scanned":scanned,"next_offset":next_offset,
        "continuation_query":continuation_query,
        "scan_complete":next_offset.is_none(),"query_work_truncated":query_work_truncated,
        "query_work_used":work_used,"query_work_cap":o.scan_work,
        "dependency_search_incomplete_rows":dependency_search_incomplete_rows,
        "dependency_queries_used":dependency_queries_used,
        "dependency_queries_skipped_by_literal_filter":dependency_queries_skipped,
        "literal_filter_excludes_all_indexed_mentions":literal_filter_excludes,
        "literal_search_truncated_rows":literal_search_truncated_rows,
        "literal_work_truncated": next_offset.is_some() && (literal_work.records == SYMBOL_LITERAL_RECORD_CAP || literal_work.bytes == SYMBOL_LITERAL_BYTE_CAP),
        "literal_records_used":literal_work.records,"literal_records_cap":SYMBOL_LITERAL_RECORD_CAP,
        "literal_bytes_used":literal_work.bytes,"literal_bytes_cap":SYMBOL_LITERAL_BYTE_CAP,
        "literal_search_cap_per_row":SYMBOL_LITERAL_ROW_CAP,
        "depth":o.depth,"definition_limit":o.definition_limit,"literal_limit":o.literal_limit,
        "limit":o.limit,"matches":o.matches,"slots":o.slots,
        "filter_literal_diagnostics":filter_diagnostics,
        "match_policy":"OR case-insensitive substrings in complete raw JS string-literal tokens discovered through bounded normal-flow candidate definitions, not decoded strings or runtime values. Other labels/properties and unresolved captures are not searched.",
        "negative_result_policy":"Omitted or unresolved dependencies and unscanned stores are not evidence of runtime absence.",
        "mention_order":"Matching tokens first, then PC/source span; this is source order, not evaluation or argument order.",
        "definitions":definitions,"rows":rows,
    });
    let mut bytes = serde_json::to_vec(&report).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > o.max_bytes {
        return Err(error("symbols output byte budget exceeded before stdout"));
    }
    Ok(bytes)
}

/// Build and validate the entire report before emitting anything to stdout.
pub fn report(input: &Path, query: Query) -> DecompilerResult<Vec<u8>> {
    let Query {
        function,
        depth,
        limit,
        max_bytes,
        ..
    } = query;
    bounds(depth, limit, max_bytes)?;
    let data = std::fs::read(input)?;
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let header = hbc
        .functions
        .get_parsed_header(function)
        .ok_or_else(|| error("Unknown function"))?;
    let exceptions: Vec<_> = header
        .exc_handlers
        .iter()
        .map(|h| (h.start, h.end, h.target))
        .collect();
    let source = export_function_fragments(&hbc, &[function])?.remove(0).1;
    if query.expressions {
        analyze_query(&source, query, &exceptions)
    } else {
        analyze_source(
            &source,
            function,
            query.pc,
            depth,
            limit,
            max_bytes,
            &exceptions,
        )
    }
}
pub fn run(input: &Path, query: Query) -> DecompilerResult<()> {
    let bytes = report(input, query)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}
