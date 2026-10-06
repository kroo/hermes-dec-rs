//! Source-only inventory. Raw exporter bytes, not a workspace header or a view,
//! are the coordinate system. No binding resolution or runtime interpretation.
use crate::{DecompilerError, DecompilerResult};
use oxc_ast::{ast::*, AstKind};
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
use serde_json::{json, Value};
use std::collections::BTreeSet;

const SOURCE_BYTES: usize = 64 * 1024 * 1024;
const WORK: usize = 32_000_000;
const DEPTH: usize = 128;
const RECORDS: usize = 131_072;
const OUTPUT_BYTES: usize = 64 * 1024 * 1024;
const MAX_INTEGER: f64 = 9_007_199_254_740_991.0;

#[derive(Clone, Copy)]
struct Limits {
    source: usize,
    work: usize,
    depth: usize,
    records: usize,
    output: usize,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            source: SOURCE_BYTES,
            work: WORK,
            depth: DEPTH,
            records: RECORDS,
            output: OUTPUT_BYTES,
        }
    }
}

fn error(message: &str) -> DecompilerError {
    DecompilerError::internal(format!("source references: {message}"))
}

fn integer(expression: &Expression<'_>) -> Option<u64> {
    let Expression::NumericLiteral(literal) = expression else {
        return None;
    };
    let value = literal.value;
    (value.is_finite() && (0.0..=MAX_INTEGER).contains(&value) && value.fract() == 0.0)
        .then_some(value as u64)
}

struct Record {
    kind: &'static str,
    id: u64,
    span: Span,
    role: &'static str,
    rhs: Option<Span>,
    environment: Option<Span>,
}

#[derive(Default)]
struct Syntax<'s> {
    source: &'s str,
    root: Option<Span>,
    root_body: Option<Span>,
    scaffolding: Option<Span>,
    limits: Limits,
    records: Vec<Record>,
    skipped: Vec<Span>,
    containers: Vec<Span>,
    // Exact spans only: a target's base expression is not itself a write.
    contexts: Vec<(Span, &'static str, Option<Span>)>,
    work: usize,
    depth: usize,
    failure: Option<&'static str>,
}

impl Syntax<'_> {
    fn begin(&mut self) -> bool {
        if self.failure.is_some() {
            return false;
        }
        if self.work >= self.limits.work || self.depth >= self.limits.depth {
            self.failure = Some("syntax work/depth budget exceeded");
            return false;
        }
        self.work += 1;
        self.depth += 1;
        true
    }

    fn named(&self, expression: &Expression<'_>, name: &str) -> bool {
        matches!(expression, Expression::Identifier(id) if id.name == name
            && self.source.get(id.span.start as usize..id.span.end as usize) == Some(name))
    }

    fn push(&mut self, record: Record) {
        if self.records.len() == self.limits.records {
            self.failure = Some("record budget exceeded");
        } else if self.failure.is_none() {
            self.records.push(record);
        }
    }
}

macro_rules! gated {
    ($visit:ident, $ty:ty, $walk:ident) => {
        fn $visit(&mut self, it: &$ty) {
            if self.begin() {
                walk::$walk(self, it);
                self.depth -= 1;
            }
        }
    };
}

impl<'a> Visit<'a> for Syntax<'_> {
    fn enter_node(&mut self, kind: AstKind<'a>) {
        let span = kind.span();
        if self
            .source
            .get(span.start as usize..span.end as usize)
            .is_none()
        {
            self.failure = Some("invalid UTF-8 AST span");
        }
        match kind {
            AstKind::BlockStatement(_) | AstKind::SwitchCase(_) | AstKind::FunctionBody(_) => {
                self.containers.push(span);
            }
            _ => {}
        }
    }

    fn visit_span(&mut self, _: &Span) {
        if self.begin() {
            self.depth -= 1;
        }
    }

    gated!(visit_expression, Expression<'a>, walk_expression);
    gated!(visit_statement, Statement<'a>, walk_statement);
    gated!(
        visit_binding_pattern,
        BindingPattern<'a>,
        walk_binding_pattern
    );
    gated!(
        visit_assignment_target,
        AssignmentTarget<'a>,
        walk_assignment_target
    );
    gated!(
        visit_assignment_target_pattern,
        AssignmentTargetPattern<'a>,
        walk_assignment_target_pattern
    );
    gated!(
        visit_assignment_target_with_default,
        AssignmentTargetWithDefault<'a>,
        walk_assignment_target_with_default
    );

    fn visit_function(&mut self, it: &Function<'a>, _: oxc_syntax::scope::ScopeFlags) {
        if Some(it.span) != self.root {
            self.skipped.push(it.span);
            return;
        }
        // Exporter parameters/defaults are not part of the selected body.
        if let Some(body) = &it.body {
            if self.begin() {
                self.visit_function_body(body);
                self.depth -= 1;
            }
        }
    }

    fn visit_arrow_function_expression(&mut self, it: &ArrowFunctionExpression<'a>) {
        self.skipped.push(it.span);
    }

    fn visit_class(&mut self, it: &Class<'a>) {
        // Conservative: class initializers, computed keys and methods are excluded.
        self.skipped.push(it.span);
    }

    fn visit_simple_assignment_target(&mut self, it: &SimpleAssignmentTarget<'a>) {
        if !self.begin() {
            return;
        }
        let span = it.span();
        let explicit = self.contexts.iter().rev().any(|c| c.0 == span);
        if !explicit {
            self.contexts
                .push((span, "pattern_or_iteration_write", None));
        }
        walk::walk_simple_assignment_target(self, it);
        if !explicit {
            self.contexts.pop();
        }
        self.depth -= 1;
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if !self.begin() {
            return;
        }
        let role = if it.operator.is_assign() {
            "write"
        } else {
            "read_write"
        };
        self.contexts
            .push((it.left.span(), role, Some(it.right.span())));
        walk::walk_assignment_expression(self, it);
        self.contexts.pop();
        self.depth -= 1;
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if self.begin() {
            self.contexts.push((it.argument.span(), "read_write", None));
            walk::walk_update_expression(self, it);
            self.contexts.pop();
            self.depth -= 1;
        }
    }

    fn visit_unary_expression(&mut self, it: &UnaryExpression<'a>) {
        if self.begin() {
            let role = if it.operator == UnaryOperator::Delete {
                "delete"
            } else {
                "read"
            };
            self.contexts.push((it.argument.span(), role, None));
            walk::walk_unary_expression(self, it);
            self.contexts.pop();
            self.depth -= 1;
        }
    }

    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if !self.begin() {
            return;
        }
        self.enter_node(AstKind::CallExpression(self.alloc(it)));
        self.visit_span(&it.span);
        self.contexts.push((it.callee.span(), "call_callee", None));
        self.visit_expression(&it.callee);
        self.contexts.pop();
        let helper = ["closure", "apply", "construct"]
            .into_iter()
            .find(|name| self.named(&it.callee, name));
        for (position, argument) in it.arguments.iter().enumerate() {
            if !self.begin() {
                break;
            }
            // Only the current argument is contextualized. Keeping every
            // argument on the context stack would make large calls quadratic.
            self.contexts.push((
                argument.span(),
                if helper.is_some() {
                    "helper_call_argument"
                } else {
                    "call_argument"
                },
                None,
            ));
            if helper == Some("closure") && position == 0 && !it.optional {
                if let Some(id) = argument.as_expression().and_then(integer) {
                    self.push(Record {
                        kind: "function",
                        id,
                        span: argument.span(),
                        role: "closure_argument",
                        rhs: None,
                        environment: None,
                    });
                }
            }
            self.visit_argument(argument);
            self.contexts.pop();
            self.depth -= 1;
        }
        self.depth -= 1;
    }

    fn visit_computed_member_expression(&mut self, it: &ComputedMemberExpression<'a>) {
        if !self.begin() {
            return;
        }
        let (kind, environment) = if self.named(&it.object, "F") {
            (Some("function"), None)
        } else if self.named(&it.object, "env") && !it.optional {
            (Some("env_slot"), Some(it.object.span()))
        } else if let Expression::StaticMemberExpression(slots) = &it.object {
            if !it.optional
                && !slots.optional
                && slots.property.name == "slots"
                && self
                    .source
                    .get(slots.property.span.start as usize..slots.property.span.end as usize)
                    == Some("slots")
            {
                (Some("slots_member"), Some(slots.object.span()))
            } else {
                (None, None)
            }
        } else {
            (None, None)
        };
        if let (Some(kind), Some(id)) = (kind, integer(&it.expression)) {
            if Some(it.span) == self.scaffolding {
                walk::walk_computed_member_expression(self, it);
                self.depth -= 1;
                return;
            }
            let context = self.contexts.iter().rev().find(|c| c.0 == it.span);
            let (role, rhs) = context.map_or(("member_read", None), |c| (c.1, c.2));
            self.push(Record {
                kind,
                id,
                span: it.span,
                role,
                rhs,
                environment,
            });
        }
        walk::walk_computed_member_expression(self, it);
        self.depth -= 1;
    }
}

fn decimal(text: &str) -> Option<u32> {
    (!text.is_empty() && text.bytes().all(|b| b.is_ascii_digit()))
        .then(|| text.parse().ok())
        .flatten()
}

struct Marker {
    pc: u32,
    span: Span,
    end: u32,
}

struct IntervalSweep {
    spans: Vec<Span>,
    cursor: usize,
    ends: BTreeSet<u32>,
}

impl IntervalSweep {
    fn new(spans: &[Span]) -> Self {
        let mut spans = spans.to_vec();
        spans.sort_unstable_by_key(|span| (span.start, span.end));
        Self {
            spans,
            cursor: 0,
            ends: BTreeSet::new(),
        }
    }

    fn covering_end(&mut self, location: Span) -> Option<u32> {
        while let Some(span) = self
            .spans
            .get(self.cursor)
            .filter(|span| span.start <= location.start)
        {
            self.ends.insert(span.end);
            self.cursor += 1;
        }
        // Queries follow parser comment order. Expired intervals can remain in
        // the set: this range excludes them without rescanning sibling scopes.
        self.ends.range(location.end..).next().copied()
    }
}

fn markers(
    source: &str,
    program: &Program<'_>,
    syntax: &Syntax<'_>,
    function: u32,
) -> DecompilerResult<Vec<Marker>> {
    let mut result: Vec<Marker> = Vec::new();
    let mut skipped = IntervalSweep::new(&syntax.skipped);
    let mut containers = IntervalSweep::new(&syntax.containers);
    for comment in &program.comments {
        if skipped.covering_end(comment.span).is_some() {
            continue;
        }
        let text = &source[comment.span.start as usize..comment.span.end as usize];
        let content = text.get(2..).unwrap_or("").trim_start();
        if !content.starts_with("HBC") {
            continue;
        }
        if syntax
            .root_body
            .is_some_and(|body| comment.span.start < body.start || comment.span.end > body.end)
        {
            return Err(error("PC comment outside exporter body"));
        }
        let line_start = source[..comment.span.start as usize]
            .rfind('\n')
            .map_or(0, |i| i + 1);
        if !text.starts_with("// HBC function ")
            || !source[line_start..comment.span.start as usize]
                .bytes()
                .all(|b| matches!(b, b' ' | b'\t'))
        {
            return Err(error("malformed or non-leading PC comment"));
        }
        let payload = text.strip_prefix("// HBC function ").unwrap().trim_end();
        let (owner, pc) = payload
            .split_once(", PC ")
            .ok_or_else(|| error("malformed PC comment"))?;
        let owner = decimal(owner).ok_or_else(|| error("malformed PC function"))?;
        let pc = decimal(pc).ok_or_else(|| error("malformed PC number"))?;
        if owner != function || result.last().is_some_and(|m| m.pc >= pc) {
            return Err(error(
                "function-mismatched, duplicate or out-of-order PC comment",
            ));
        }
        let end = containers
            .covering_end(comment.span)
            .unwrap_or(source.len() as u32);
        result.push(Marker {
            pc,
            span: comment.span,
            end,
        });
    }
    for i in 0..result.len().saturating_sub(1) {
        result[i].end = result[i].end.min(result[i + 1].span.start);
    }
    Ok(result)
}

fn location(function: u32, span: Span) -> Value {
    json!({"start": span.start, "end": span.end,
        "source_id": format!("f{function}:source:{}:{}", span.start, span.end)})
}

/// Inventory a complete, unmodified raw fragment. All budgets reject atomically.
/// IDs are exact byte-span join keys within this input, not cross-input hashes.
pub fn report_source(source: &str, function: u32, function_count: u32) -> DecompilerResult<Value> {
    report_with_limits(source, function, function_count, Limits::default())
}

fn report_with_limits(
    source: &str,
    function: u32,
    function_count: u32,
    limits: Limits,
) -> DecompilerResult<Value> {
    if source.len() > limits.source || source.len() > u32::MAX as usize {
        return Err(error("source byte budget exceeded"));
    }
    if function >= function_count {
        return Err(error("selected function is out of range"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if parsed.panicked || !parsed.errors.is_empty() {
        return Err(error("valid complete JavaScript required"));
    }
    let mut syntax = Syntax {
        source,
        limits,
        ..Syntax::default()
    };
    // Only the literal exporter wrapper is an admitted executable function scope.
    for statement in &parsed.program.body {
        let Statement::ExpressionStatement(statement) = statement else {
            continue;
        };
        let Expression::AssignmentExpression(assignment) = &statement.expression else {
            continue;
        };
        let AssignmentTarget::ComputedMemberExpression(member) = &assignment.left else {
            continue;
        };
        let Expression::FunctionExpression(body) = &assignment.right else {
            continue;
        };
        if syntax.named(&member.object, "F") {
            if !assignment.operator.is_assign()
                || integer(&member.expression) != Some(u64::from(function))
                || syntax.root.replace(body.span).is_some()
            {
                return Err(error("wrong or multiple exporter function wrappers"));
            }
            syntax.root_body = body.body.as_ref().map(|body| body.span);
            syntax.scaffolding = Some(member.span);
        }
    }
    syntax.visit_program(&parsed.program);
    if let Some(failure) = syntax.failure {
        return Err(error(failure));
    }
    // Sorting and ordered interval queries replace comments * containers work.
    let intervals = syntax.skipped.len() + syntax.containers.len();
    let levels = (usize::BITS - intervals.max(1).leading_zeros()) as usize + 1;
    let comment_work = intervals
        .checked_add(parsed.program.comments.len().saturating_mul(2))
        .and_then(|items| items.checked_mul(levels))
        .and_then(|cost| cost.checked_add(syntax.work))
        .filter(|&cost| cost <= limits.work)
        .ok_or_else(|| error("comment containment work budget exceeded"))?;
    syntax.work = comment_work;
    let markers = markers(source, &parsed.program, &syntax, function)?;
    syntax
        .records
        .sort_by_key(|r| (r.span.start, r.span.end, r.kind, r.role));
    let mut records = Vec::with_capacity(syntax.records.len());
    let mut output_work = 0usize;
    for (ordinal, record) in syntax.records.iter().enumerate() {
        let index = markers.partition_point(|m| m.span.end <= record.span.start);
        let pc = index.checked_sub(1).and_then(|i| {
            let marker = &markers[i];
            (record.span.end <= marker.end).then_some(marker.pc)
        });
        let resolution = if record.kind == "function" && record.id >= u64::from(function_count) {
            "unresolved_out_of_range"
        } else {
            "syntactic_only"
        };
        let access = match record.role {
            "write" | "pattern_or_iteration_write" => "write",
            "read_write" => "read_write",
            "delete" => "delete",
            _ => "read",
        };
        let row = json!({"ordinal": ordinal, "kind": record.kind, "id": record.id,
            "source": location(function, record.span), "pc": pc, "role": record.role, "access": access,
            "environment": record.environment.map(|span| location(function, span)),
            "rhs": record.rhs.map(|span| location(function, span)), "status": resolution,
            "binding_verified": false, "runtime_verified": false});
        output_work += serde_json::to_vec(&row)
            .map_err(|_| error("JSON serialization failed"))?
            .len()
            + 1;
        if output_work > limits.output {
            return Err(error("output byte budget exceeded"));
        }
        records.push(row);
    }
    let value = json!({"schema_version": 1, "function": function, "function_count": function_count,
        "semantics": "source_syntax_only", "source": location(function, Span::new(0, source.len() as u32)),
        "span_basis": "entire_raw_exporter_fragment_utf8_bytes", "status": "complete",
        "scan_complete": true, "next_offset": null, "offset_scope": "raw_source_reference_ordinal",
        "total": records.len(), "records": records, "skipped_scope_count": syntax.skipped.len(),
        "limits": {"source_bytes": limits.source, "syntax_work": limits.work, "visitor_depth": limits.depth,
            "records": limits.records, "output_bytes": limits.output, "budget_policy": "reject_before_output"},
        "work": syntax.work,
        "limitations": ["No bytecode validation of PCs; PCs are source-comment containment only",
            "F references are function-table mentions, never proven runtime calls",
            "env slots are syntactic frame expressions, never lexical or object identities",
            "Static .slots members identify source syntax only; their environment expression is never resolved",
            "Optional or computed slots properties are excluded",
            "Helper names and all bindings are unverified",
            "Nested functions, arrows and entire classes are excluded",
            "The exporter root F assignment is scaffolding and is excluded",
            "Only nonnegative safe-integer numeric literals are indexed; no constant folding",
            "Pattern and iteration writes have no authoritative RHS join",
            "Compound RHS spans are source operands, not final stored values",
            "Source IDs are input-local byte-span keys, not content hashes"]});
    // Count serialized bytes without allocating a second full output buffer.
    let mut counter = OutputCounter {
        remaining: limits.output,
    };
    serde_json::to_writer(&mut counter, &value)
        .map_err(|_| error("output byte budget exceeded"))?;
    Ok(value)
}

struct OutputCounter {
    remaining: usize,
}

impl std::io::Write for OutputCounter {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.remaining = self
            .remaining
            .checked_sub(bytes.len())
            .ok_or_else(|| std::io::Error::other("output byte budget exceeded"))?;
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod budget_tests {
    use super::*;

    #[test]
    fn source_budget_rejects_before_parse() {
        let limits = Limits {
            source: 4,
            ..Limits::default()
        };
        assert!(report_with_limits("F[1];", 0, 8, limits).is_err());
    }

    #[test]
    fn record_budget_rejects_atomically() {
        let limits = Limits {
            records: 2,
            ..Limits::default()
        };
        assert!(report_with_limits("F[1]; F[2]; F[3];", 0, 8, limits).is_err());
    }

    #[test]
    fn work_budget_and_comment_containment_budget_reject() {
        let limits = Limits {
            work: 2,
            ..Limits::default()
        };
        assert!(report_with_limits("F[1];", 0, 8, limits).is_err());
        let source = "{ /* ordinary */ F[1]; }".repeat(40);
        let limits = Limits {
            work: 500,
            ..Limits::default()
        };
        assert!(report_with_limits(&source, 0, 8, limits).is_err());
    }

    #[test]
    fn output_budget_checks_rows_and_report_envelope() {
        let limits = Limits {
            output: 200,
            ..Limits::default()
        };
        assert!(report_with_limits("F[1];", 0, 8, limits).is_err());
        assert!(report_with_limits("", 0, 8, limits).is_err());
    }
}
