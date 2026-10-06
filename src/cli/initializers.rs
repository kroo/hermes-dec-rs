//! Bounded initializer-store syntax. No execution, binding or heap-identity inference.
use crate::{DecompilerError, DecompilerResult};
use oxc_ast::ast::*;
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
use serde_json::{json, Value};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

const SOURCE_CAP: usize = 64 * 1024 * 1024;
const WORK_CAP: usize = 8_388_608;
const QUERY_WORK_CAP: usize = 8_388_608;
const AST_DEPTH: usize = 256;
const ROW_CAP: usize = 16_384;
const OUTPUT_CAP: usize = 16_777_216;
const CHAIN_DEPTH: usize = 16;
const ITEM_CAP: usize = 32;
const PREVIEW: usize = 256;

fn error(message: &str) -> DecompilerError {
    DecompilerError::internal(format!("initializers: {message}"))
}

fn text(source: &str, span: Span) -> &str {
    &source[span.start as usize..span.end as usize]
}

fn snippet(source: &str, span: Span) -> Value {
    let raw = text(source, span);
    let mut end = raw.len().min(PREVIEW);
    while !raw.is_char_boundary(end) {
        end -= 1;
    }
    json!({"source_span":[span.start,span.end],"javascript":&raw[..end],
        "original_bytes":raw.len(),"bytes_omitted":raw.len()-end,"truncated":end<raw.len()})
}

fn id(function: u32, span: Span) -> String {
    format!("{function}:{}:{}", span.start, span.end)
}

fn numeric(expr: &Expression<'_>) -> Option<u32> {
    let Expression::NumericLiteral(n) = expr else {
        return None;
    };
    (n.value >= 0.0 && n.value <= f64::from(u32::MAX) && n.value.fract() == 0.0)
        .then_some(n.value as u32)
}

fn register(source: &str, span: Span) -> Option<u32> {
    let digits = text(source, span).strip_prefix("r[")?.strip_suffix(']')?;
    (!digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()))
        .then(|| digits.parse().ok())
        .flatten()
}

// Conservative pre-parser budgets also bound delimiter/operator recursion. Strings
// and comments are skipped; templates are intentionally outside this source subset.
fn preflight(source: &str) -> DecompilerResult<()> {
    if source.len() > SOURCE_CAP {
        return Err(error("source byte cap exceeded"));
    }
    let b = source.as_bytes();
    let (mut i, mut depth, mut run) = (0, 0usize, 0usize);
    while i < b.len() {
        match b[i] {
            b'\'' | b'"' => {
                let quote = b[i];
                i += 1;
                while i < b.len() && b[i] != quote {
                    i += if b[i] == b'\\' { 2 } else { 1 };
                }
                run = 0;
            }
            b'/' if b.get(i + 1) == Some(&b'/') => {
                while i < b.len()
                    && !matches!(b[i], b'\n' | b'\r')
                    && !b[i..].starts_with(&[0xe2, 0x80, 0xa8])
                    && !b[i..].starts_with(&[0xe2, 0x80, 0xa9])
                {
                    i += 1;
                }
            }
            b'/' if b.get(i + 1) == Some(&b'*') => {
                i += 2;
                while i + 1 < b.len() && &b[i..i + 2] != b"*/" {
                    i += 1;
                }
                i += 1;
            }
            b'`' => return Err(error("template syntax is unsupported")),
            b'(' | b'[' | b'{' => {
                depth += 1;
                run = 0;
            }
            b')' | b']' | b'}' => {
                depth = depth.saturating_sub(1);
                run = 0;
            }
            b'!' | b'~' | b'+' | b'-' => run += 1,
            c if c.is_ascii_whitespace() => {}
            c if c.is_ascii_alphabetic() || matches!(c, b'_' | b'$') => {
                let start = i;
                while i + 1 < b.len()
                    && (b[i + 1].is_ascii_alphanumeric() || matches!(b[i + 1], b'_' | b'$'))
                {
                    i += 1;
                }
                if matches!(
                    &source[start..i + 1],
                    "new" | "void" | "typeof" | "delete" | "await" | "yield"
                ) {
                    run += 1;
                } else {
                    run = 0;
                }
            }
            _ => run = 0,
        }
        if depth > AST_DEPTH || run > AST_DEPTH {
            return Err(error("source nesting cap exceeded"));
        }
        i += 1;
    }
    Ok(())
}

struct Definition {
    register: u32,
    span: Span,
    rhs: Span,
    eligible: bool,
    pc: u32,
}

struct Read {
    register: u32,
    span: Span,
    definition: Option<usize>,
    status: &'static str,
}

struct Store {
    span: Span,
    environment: Span,
    rhs: Span,
    slot: u32,
    conditional: bool,
    pc: u32,
    exporter_slot: bool,
}

struct Operand {
    role: &'static str,
    index: usize,
    span: Span,
}

struct Summary {
    span: Span,
    kind: &'static str,
    operands: Vec<Operand>,
    operand_count: usize,
}

struct Syntax<'s> {
    source: &'s str,
    root: Span,
    allowed_bindings: BTreeSet<u32>,
    definitions: Vec<Definition>,
    reads: Vec<Read>,
    stores: Vec<Store>,
    summaries: Vec<Summary>,
    boundaries: BTreeSet<u32>,
    statement_assignments: BTreeSet<u32>,
    conditional: usize,
    depth: usize,
    work: usize,
    failure: Option<&'static str>,
}

impl Syntax<'_> {
    fn enter(&mut self) -> bool {
        self.work += 1;
        if self.failure.is_some() {
            return false;
        }
        if self.work > WORK_CAP || self.depth >= AST_DEPTH {
            self.failure = Some("AST work/depth cap exceeded");
            return false;
        }
        self.depth += 1;
        true
    }

    fn barrier(&mut self, span: Span) {
        self.boundaries.extend([span.start, span.end]);
    }

    fn summary(&mut self, span: Span, kind: &'static str, operands: Vec<Operand>, count: usize) {
        self.summaries.push(Summary {
            span,
            kind,
            operands,
            operand_count: count,
        });
    }
}

fn operand(role: &'static str, index: usize, span: Span) -> Operand {
    Operand { role, index, span }
}

impl<'a> Visit<'a> for Syntax<'_> {
    fn visit_statement(&mut self, it: &Statement<'a>) {
        if !self.enter() {
            return;
        }
        let conditional = matches!(it, Statement::IfStatement(_));
        match it {
            Statement::LabeledStatement(_)
            | Statement::WhileStatement(_)
            | Statement::DoWhileStatement(_)
            | Statement::ForInStatement(_)
            | Statement::ForOfStatement(_)
            | Statement::WithStatement(_)
            | Statement::FunctionDeclaration(_)
            | Statement::ClassDeclaration(_) => {
                self.failure = Some("structured/labeled control flow or nested scopes unsupported");
            }
            Statement::ForStatement(f)
                if f.init.is_some() || f.test.is_some() || f.update.is_some() =>
            {
                self.failure = Some("structured loops unsupported");
            }
            Statement::SwitchStatement(s) if !s.discriminant.is_specific_id("pc") => {
                self.failure = Some("only exporter pc dispatcher switches supported");
            }
            Statement::BlockStatement(_)
            | Statement::IfStatement(_)
            | Statement::ForStatement(_)
            | Statement::SwitchStatement(_)
            | Statement::TryStatement(_) => self.barrier(it.span()),
            Statement::BreakStatement(b) if b.label.is_some() => {
                self.failure = Some("labeled control flow unsupported");
            }
            Statement::ContinueStatement(c) if c.label.is_some() => {
                self.failure = Some("labeled control flow unsupported");
            }
            Statement::BreakStatement(_)
            | Statement::ContinueStatement(_)
            | Statement::ReturnStatement(_)
            | Statement::ThrowStatement(_) => {
                self.boundaries.insert(it.span().end);
            }
            Statement::ExpressionStatement(s) => {
                if let Expression::AssignmentExpression(a) = &s.expression {
                    self.statement_assignments.insert(a.span.start);
                }
            }
            _ => {}
        }
        self.conditional += usize::from(conditional);
        if self.failure.is_none() {
            walk::walk_statement(self, it);
        }
        self.conditional -= usize::from(conditional);
        self.depth -= 1;
    }

    fn visit_function(&mut self, it: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        if it.span != self.root {
            self.failure = Some("nested function scopes unsupported");
            return;
        }
        walk::walk_function(self, it, flags);
    }

    fn visit_binding_identifier(&mut self, it: &BindingIdentifier<'a>) {
        if (it.name == "r" || it.name == "env") && !self.allowed_bindings.contains(&it.span.start) {
            self.failure = Some("register/environment shadowing unsupported");
        }
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        if it.name == "r" {
            self.failure = Some("register container alias/use unsupported");
        }
    }

    fn visit_switch_case(&mut self, it: &SwitchCase<'a>) {
        self.barrier(it.span);
        walk::walk_switch_case(self, it);
    }

    fn visit_expression(&mut self, it: &Expression<'a>) {
        if !self.enter() {
            return;
        }
        let conditional = matches!(
            it,
            Expression::ConditionalExpression(_) | Expression::LogicalExpression(_)
        );
        if conditional {
            self.barrier(it.span());
            self.conditional += 1;
        }
        match it {
            Expression::ConditionalExpression(c) => {
                self.summary(
                    c.span,
                    "conditional",
                    vec![
                        operand("test", 0, c.test.span()),
                        operand("consequent", 0, c.consequent.span()),
                        operand("alternate", 0, c.alternate.span()),
                    ],
                    3,
                );
            }
            Expression::LogicalExpression(l) => {
                self.summary(
                    l.span,
                    "logical",
                    vec![
                        operand("left", 0, l.left.span()),
                        operand("right", 0, l.right.span()),
                    ],
                    2,
                );
            }
            Expression::ArrowFunctionExpression(_)
            | Expression::ClassExpression(_)
            | Expression::ChainExpression(_)
            | Expression::RegExpLiteral(_)
            | Expression::TaggedTemplateExpression(_) => {
                self.failure = Some("unsupported expression scope/optional/regex/template syntax");
            }
            Expression::CallExpression(c) => {
                if c.optional || c.callee.is_specific_id("eval") {
                    self.failure = Some("optional calls/direct eval unsupported");
                }
                let mut ops = vec![operand("callee", 0, c.callee.span())];
                ops.extend(
                    c.arguments
                        .iter()
                        .take(ITEM_CAP - 1)
                        .enumerate()
                        .map(|(i, a)| {
                            operand(
                                if matches!(a, Argument::SpreadElement(_)) {
                                    "spread_argument"
                                } else {
                                    "argument"
                                },
                                i,
                                a.span(),
                            )
                        }),
                );
                self.summary(c.span, "call", ops, c.arguments.len() + 1);
                // This is a spelling/shape label only, not a resolved helper binding.
                if c.callee.is_specific_id("construct") || c.callee.is_specific_id("apply") {
                    if let [target, receiver, Argument::ArrayExpression(args)] =
                        c.arguments.as_slice()
                    {
                        let mut ops = vec![
                            operand("target_callee", 0, target.span()),
                            operand("receiver", 0, receiver.span()),
                        ];
                        ops.extend(
                            args.elements
                                .iter()
                                .take(ITEM_CAP - 2)
                                .enumerate()
                                .map(|(i, a)| operand("argument_syntax", i, a.span())),
                        );
                        self.summary(
                            c.span,
                            if c.callee.is_specific_id("construct") {
                                "construct_call_shape"
                            } else {
                                "apply_call_shape"
                            },
                            ops,
                            args.elements.len() + 2,
                        );
                    }
                }
            }
            Expression::NewExpression(n) => {
                let mut ops = vec![operand("callee", 0, n.callee.span())];
                ops.extend(
                    n.arguments
                        .iter()
                        .take(ITEM_CAP - 1)
                        .enumerate()
                        .map(|(i, a)| operand("argument_syntax", i, a.span())),
                );
                self.summary(n.span, "constructor", ops, n.arguments.len() + 1);
            }
            Expression::StaticMemberExpression(m) => {
                self.summary(
                    m.span,
                    "member",
                    vec![
                        operand("object", 0, m.object.span()),
                        operand("property", 0, m.property.span),
                    ],
                    2,
                );
            }
            Expression::ComputedMemberExpression(m) => {
                self.summary(
                    m.span,
                    "computed_member",
                    vec![
                        operand("object", 0, m.object.span()),
                        operand("key", 0, m.expression.span()),
                    ],
                    2,
                );
            }
            Expression::ArrayExpression(a) => {
                let ops = a
                    .elements
                    .iter()
                    .take(ITEM_CAP)
                    .enumerate()
                    .map(|(i, e)| {
                        operand(
                            match e {
                                ArrayExpressionElement::Elision(_) => "hole",
                                ArrayExpressionElement::SpreadElement(_) => "spread",
                                _ => "element",
                            },
                            i,
                            e.span(),
                        )
                    })
                    .collect();
                self.summary(a.span, "array", ops, a.elements.len());
            }
            Expression::ObjectExpression(o) => {
                let mut ops = Vec::new();
                let mut count = 0;
                for (i, p) in o.properties.iter().enumerate() {
                    match p {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            count += 2;
                            if ops.len() + 2 <= ITEM_CAP {
                                ops.push(operand(
                                    if p.computed { "computed_key" } else { "key" },
                                    i,
                                    p.key.span(),
                                ));
                                ops.push(operand("value", i, p.value.span()));
                            }
                        }
                        ObjectPropertyKind::SpreadProperty(p) => {
                            count += 1;
                            if ops.len() < ITEM_CAP {
                                ops.push(operand("spread", i, p.span));
                            }
                        }
                    }
                }
                self.summary(o.span, "object", ops, count);
            }
            _ => {}
        }
        if self.failure.is_none() {
            walk::walk_expression(self, it);
        }
        if conditional {
            self.conditional -= 1;
        }
        self.depth -= 1;
    }

    fn visit_computed_member_expression(&mut self, it: &ComputedMemberExpression<'a>) {
        if it.object.without_parentheses().is_specific_id("r") {
            let Some(reg) = register(self.source, it.span) else {
                self.failure = Some("noncanonical register access unsupported");
                return;
            };
            self.reads.push(Read {
                register: reg,
                span: it.span,
                definition: None,
                status: "unresolved_local_source",
            });
            return;
        }
        walk::walk_computed_member_expression(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        let lhs = it.left.span();
        if let Some(reg) = register(self.source, lhs) {
            self.definitions.push(Definition {
                register: reg,
                span: it.span,
                rhs: it.right.span(),
                pc: 0,
                eligible: it.operator.is_assign()
                    && self.conditional == 0
                    && self.statement_assignments.contains(&it.span.start),
            });
            self.visit_expression(&it.right);
            return;
        }
        match &it.left {
            AssignmentTarget::AssignmentTargetIdentifier(i) if i.name == "r" || i.name == "env" => {
                self.failure = Some("register/environment reassignment unsupported");
                return;
            }
            AssignmentTarget::ComputedMemberExpression(m) => {
                if m.object.without_parentheses().is_specific_id("r") {
                    self.failure = Some("noncanonical register mutation unsupported");
                    return;
                }
                let environment = match &m.object {
                    Expression::StaticMemberExpression(s) if s.property.name == "slots" => {
                        Some(s.object.span())
                    }
                    Expression::Identifier(i) if i.name == "env" => Some(i.span),
                    _ => None,
                };
                if let Some(environment) = environment {
                    if !it.operator.is_assign() {
                        self.failure = Some("non-simple environment store unsupported");
                        return;
                    }
                    if let Some(slot) = numeric(&m.expression) {
                        self.stores.push(Store {
                            span: it.span,
                            environment,
                            rhs: it.right.span(),
                            slot,
                            pc: 0,
                            conditional: self.conditional > 0,
                            exporter_slot: matches!(
                                &m.object,
                                Expression::StaticMemberExpression(_)
                            ),
                        });
                    }
                }
                self.visit_expression(&m.object);
                self.visit_expression(&m.expression);
            }
            AssignmentTarget::StaticMemberExpression(m) => self.visit_expression(&m.object),
            AssignmentTarget::AssignmentTargetIdentifier(_) => {}
            _ => {
                self.failure = Some("destructuring assignment unsupported");
                return;
            }
        }
        self.visit_expression(&it.right);
    }

    fn visit_update_expression(&mut self, _: &UpdateExpression<'a>) {
        self.failure = Some("update expressions unsupported");
    }

    fn visit_unary_expression(&mut self, it: &UnaryExpression<'a>) {
        if it.operator.is_delete() {
            self.failure = Some("delete expressions unsupported");
        } else {
            walk::walk_unary_expression(self, it);
        }
    }
}

fn within(outer: Span, inner: Span) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
}

fn reads_in(reads: &[Read], span: Span) -> &[Read] {
    let start = reads.partition_point(|r| r.span.start < span.start);
    let end = reads.partition_point(|r| r.span.start < span.end);
    &reads[start..end]
}

fn edge(function: u32, source: &str, syntax: &Syntax<'_>, read: &Read) -> Value {
    json!({"register":read.register,"expression":snippet(source,read.span),"status":read.status,
        "definition_id":read.definition.map(|d|id(function,syntax.definitions[d].span))})
}

#[derive(Default)]
struct QueryBudget {
    used: usize,
    exhausted: bool,
}

impl QueryBudget {
    fn charge(&mut self, amount: usize) -> DecompilerResult<()> {
        if self.used.saturating_add(amount) > QUERY_WORK_CAP {
            self.exhausted = true;
            return Err(error("aggregate query work cap exceeded"));
        }
        self.used += amount;
        Ok(())
    }
}

fn syntax_summary(
    source: &str,
    syntax: &Syntax<'_>,
    span: Span,
    budget: &mut QueryBudget,
) -> DecompilerResult<Value> {
    // Two binary searches over at most WORK_CAP entries, then bounded output.
    budget.charge(64)?;
    let start = syntax
        .summaries
        .partition_point(|n| n.span.start < span.start);
    let end = syntax
        .summaries
        .partition_point(|n| n.span.start < span.end);
    let nodes = &syntax.summaries[start..end];
    budget.charge(
        nodes.len().min(ITEM_CAP)
            + nodes
                .iter()
                .take(ITEM_CAP)
                .map(|n| n.operands.len())
                .sum::<usize>(),
    )?;
    let omitted_operands: usize = nodes
        .iter()
        .take(ITEM_CAP)
        .map(|n| n.operand_count - n.operands.len())
        .sum();
    let summaries: Vec<_> = nodes.iter().take(ITEM_CAP).map(|n| json!({"kind":n.kind,
        "expression":snippet(source,n.span),"operand_count":n.operand_count,
        "operands_omitted":n.operand_count-n.operands.len(),
        "operands":n.operands.iter().map(|o|json!({"role":o.role,"index":o.index,"expression":snippet(source,o.span)})).collect::<Vec<_>>() })).collect();
    Ok(
        json!({"nodes":summaries,"nodes_omitted":nodes.len().saturating_sub(ITEM_CAP),"operands_omitted":omitted_operands,
        "order":"source preorder; operand indices are syntactic, not execution order"}),
    )
}

/// Inspect a complete raw exporter fragment, excluding workspace headers.
/// Candidate chains are local source definitions, never runtime bindings or values.
pub fn report_source(
    source: &str,
    function: u32,
    exception_boundaries: &BTreeSet<u32>,
) -> DecompilerResult<Value> {
    preflight(source)?;
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if parsed.panicked || !parsed.errors.is_empty() {
        return Err(error("valid complete exporter JavaScript required"));
    }
    let mut root = None;
    for statement in &parsed.program.body {
        let Statement::ExpressionStatement(s) = statement else {
            return Err(error("unsupported exporter wrapper"));
        };
        let Expression::AssignmentExpression(a) = &s.expression else {
            return Err(error("unsupported exporter wrapper"));
        };
        let AssignmentTarget::ComputedMemberExpression(m) = &a.left else {
            return Err(error("unsupported exporter wrapper"));
        };
        if !a.operator.is_assign() || numeric(&m.expression) != Some(function) {
            return Err(error("wrong-function exporter wrapper"));
        }
        if m.object.is_specific_id("M") && matches!(a.right, Expression::ArrayExpression(_)) {
            continue;
        }
        let Expression::FunctionExpression(f) = &a.right else {
            return Err(error("literal exporter function required"));
        };
        if !m.object.is_specific_id("F") || root.replace(f.as_ref()).is_some() {
            return Err(error("one matching exporter root required"));
        }
    }
    let root = root.ok_or_else(|| error("missing exporter root"))?;
    let body = root
        .body
        .as_ref()
        .ok_or_else(|| error("missing exporter body"))?;
    let mut allowed_bindings = BTreeSet::new();
    let mut env_binding = false;
    for param in &root.params.items {
        let BindingPatternKind::BindingIdentifier(b) = &param.pattern.kind else {
            return Err(error("simple exporter parameters required"));
        };
        if b.name == "env" {
            if env_binding {
                return Err(error("duplicate environment parameter"));
            }
            env_binding = true;
            allowed_bindings.insert(b.span.start);
        }
    }
    let mut register_binding = false;
    for statement in &body.statements {
        if let Statement::VariableDeclaration(d) = statement {
            for d in &d.declarations {
                if let BindingPatternKind::BindingIdentifier(b) = &d.id.kind {
                    if b.name == "r" {
                        if register_binding {
                            return Err(error("duplicate root register binding"));
                        }
                        register_binding = true;
                        allowed_bindings.insert(b.span.start);
                    }
                }
            }
        }
    }
    if !register_binding {
        return Err(error("root register binding required"));
    }
    let mut syntax = Syntax {
        source,
        root: root.span,
        allowed_bindings,
        definitions: vec![],
        reads: vec![],
        stores: vec![],
        summaries: vec![],
        boundaries: BTreeSet::new(),
        statement_assignments: BTreeSet::new(),
        conditional: 0,
        depth: 0,
        work: 0,
        failure: None,
    };
    syntax.visit_function(root, oxc_syntax::scope::ScopeFlags::empty());
    if let Some(message) = syntax.failure {
        return Err(error(message));
    }
    let prefix = format!("// HBC function {function}, PC ");
    let mut markers = Vec::new();
    for comment in &parsed.program.comments {
        let raw = text(source, comment.span);
        let content = raw
            .strip_prefix("//")
            .or_else(|| raw.strip_prefix("/*"))
            .unwrap_or(raw)
            .trim_start();
        if content.starts_with("HBC") {
            if !within(body.span, comment.span) {
                return Err(error("PC marker outside root"));
            }
            let before = &source[..comment.span.start as usize];
            let line_start = before
                .rfind(['\n', '\r', '\u{2028}', '\u{2029}'])
                .map_or(0, |i| {
                    i + before[i..].chars().next().map_or(0, char::len_utf8)
                });
            if !before[line_start..].chars().all(char::is_whitespace) {
                return Err(error("PC markers must occupy a source line"));
            }
            let pc = raw
                .strip_prefix(&prefix)
                .and_then(|s| s.trim().parse::<u32>().ok())
                .ok_or_else(|| error("invalid function/PC marker"))?;
            if markers.last().is_some_and(|&(last, _, _)| last >= pc) {
                return Err(error("unordered/duplicate PC markers"));
            }
            markers.push((pc, comment.span.start, comment.span.end));
        }
    }
    if markers.is_empty() {
        return Err(error("numeric PC markers required"));
    }
    syntax.definitions.sort_by_key(|d| d.span.start);
    syntax.reads.sort_by_key(|r| r.span.start);
    syntax.stores.sort_by_key(|s| s.span.start);
    syntax
        .summaries
        .sort_by_key(|s| (s.span.start, std::cmp::Reverse(s.span.end)));
    let locate = |span: Span| -> DecompilerResult<u32> {
        let i = markers.partition_point(|m| m.2 <= span.start);
        if i == 0 || span.end > markers.get(i).map_or(body.span.end, |m| m.1) {
            return Err(error("record outside/crossing PC region"));
        }
        Ok(markers[i - 1].0)
    };
    for d in &mut syntax.definitions {
        d.pc = locate(d.span)?;
    }
    for s in &mut syntax.stores {
        s.pc = locate(s.span)?;
    }
    for r in &syntax.reads {
        locate(r.span)?;
    }
    let mut prior = BTreeMap::<u32, usize>::new();
    let mut previous_end = body.span.start;
    let mut previous_pc = 0;
    for (i, &(pc, _, start)) in markers.iter().enumerate() {
        let end = markers.get(i + 1).map_or(body.span.end, |m| m.1);
        if syntax
            .boundaries
            .range(previous_end..=start)
            .next()
            .is_some()
            || exception_boundaries
                .range(previous_pc..=pc)
                .next()
                .is_some()
        {
            prior.clear();
        }
        let ds = syntax.definitions.partition_point(|d| d.span.start < start);
        let de = syntax.definitions.partition_point(|d| d.span.start < end);
        let mut counts = BTreeMap::<u32, usize>::new();
        for d in &syntax.definitions[ds..de] {
            *counts.entry(d.register).or_default() += 1;
        }
        let rs = syntax.reads.partition_point(|r| r.span.start < start);
        let re = syntax.reads.partition_point(|r| r.span.start < end);
        for read in &mut syntax.reads[rs..re] {
            if counts.contains_key(&read.register) {
                read.status = "unresolved_same_pc_write";
            } else if syntax
                .boundaries
                .range(start..=read.span.start)
                .next()
                .is_some()
            {
                read.status = "unresolved_control_barrier";
            } else if let Some(&d) = prior.get(&read.register) {
                read.definition = Some(d);
                read.status = "local_definition_candidate";
            }
        }
        let last = syntax.boundaries.range(start..end).next_back().copied();
        if last.is_some() {
            prior.clear();
        }
        for (n, d) in syntax.definitions[ds..de].iter().enumerate() {
            if last.is_some_and(|b| d.span.start <= b) || !d.eligible || counts[&d.register] > 1 {
                prior.remove(&d.register);
            } else {
                prior.insert(d.register, ds + n);
            }
        }
        previous_end = end;
        previous_pc = pc.saturating_add(1);
    }
    let total = syntax.stores.len();
    let mut report = json!({"schema":"initializers-v1","function":function,"source":"export_function_fragments",
        "source_span_basis":"complete raw function fragment, excluding workspace headers; UTF-8 byte offsets",
        "semantics":"Syntactic numeric env[N] or expression.slots[N] writes and source-local prior register-definition candidates. No runtime bindings/values, lexical resolution, heap identity, captures, JS execution, framework or protocol evaluation. Calls, constructors, branches and field reads remain opaque syntax.",
        "negative_result_policy":"Empty, unresolved or omitted tables cannot prove absence, even when the source scan is complete.",
        "limits":{"source_bytes":SOURCE_CAP,"ast_work":WORK_CAP,"query_work":QUERY_WORK_CAP,"ast_depth":AST_DEPTH,"rows":ROW_CAP,"output_bytes":OUTPUT_CAP,"chain_depth":CHAIN_DEPTH,"items_per_list":ITEM_CAP,"snippet_bytes":PREVIEW},
        "work_used":syntax.work,"total_stores":total,"rows":[],"definitions":{},"rows_omitted":total,
        "next_offset":if total>0 {Some(0)} else {None},"offset_scope":"raw numeric environment-store ordinal in this function",
        "source_scan_complete":true,"table_complete":false,"unresolved_reads":0,"dependency_edges_omitted":0,"query_work_used":0,"stop_reason":null,
        "continuation_query":null,"references_outside_definition_table_possible":true,
        "count_scope":"unresolved/omitted dependencies count included RHS chains only; environment is separate and not resolved"});
    // Charge serialized additions once, rather than cloning/serializing a growing
    // multi-megabyte report for every store. Reserve metadata and JSON separators.
    let mut output_bytes = serde_json::to_vec(&report)
        .map_err(|_| error("JSON serialization failed"))?
        .len()
        + 4096;
    let mut budget = QueryBudget::default();
    for (ordinal, store) in syntax.stores.iter().take(ROW_CAP).enumerate() {
        let built = (|| -> DecompilerResult<_> {
            budget.charge(64)?;
            let direct = reads_in(&syntax.reads, store.rhs);
            budget.charge(direct.len().min(ITEM_CAP) * 3)?;
            let mut selected = BTreeSet::new();
            let mut queue = VecDeque::new();
            let mut omitted = direct.len().saturating_sub(ITEM_CAP);
            let mut unresolved = direct
                .iter()
                .take(ITEM_CAP)
                .filter(|r| r.definition.is_none())
                .count();
            for r in direct.iter().take(ITEM_CAP) {
                if let Some(d) = r.definition {
                    queue.push_back((d, 1));
                }
            }
            let mut definitions = BTreeMap::new();
            while let Some((d, level)) = queue.pop_front() {
                budget.charge(1)?;
                if selected.contains(&d) {
                    continue;
                }
                if level > CHAIN_DEPTH || selected.len() == ITEM_CAP {
                    omitted += 1;
                    continue;
                }
                selected.insert(d);
                let def = &syntax.definitions[d];
                budget.charge(64)?;
                let reads = reads_in(&syntax.reads, def.rhs);
                budget.charge(reads.len().min(ITEM_CAP) * 3)?;
                omitted += reads.len().saturating_sub(ITEM_CAP);
                unresolved += reads
                    .iter()
                    .take(ITEM_CAP)
                    .filter(|r| r.definition.is_none())
                    .count();
                for r in reads.iter().take(ITEM_CAP) {
                    if let Some(next) = r.definition {
                        if next < d {
                            queue.push_back((next, level + 1));
                        } else {
                            return Err(error("non-prior definition dependency"));
                        }
                    }
                }
                definitions.insert(id(function,def.span),json!({"source_pc":def.pc,"defines_register":def.register,"source_span":[def.span.start,def.span.end],
                "source":snippet(source,def.span),"rhs":snippet(source,def.rhs),"syntax":syntax_summary(source,&syntax,def.rhs,&mut budget)?,
                "edges":reads.iter().take(ITEM_CAP).map(|r|edge(function,source,&syntax,r)).collect::<Vec<_>>(),"edges_omitted":reads.len().saturating_sub(ITEM_CAP)}));
            }
            let row = json!({"store_ordinal":ordinal,"pc":store.pc,"slot":store.slot,"source_span":[store.span.start,store.span.end],"source":snippet(source,store.span),
            "environment":snippet(source,store.environment),"rhs":snippet(source,store.rhs),"conditional_syntax":store.conditional,
            "syntax":syntax_summary(source,&syntax,store.rhs,&mut budget)?,"rhs_edges":direct.iter().take(ITEM_CAP).map(|r|edge(function,source,&syntax,r)).collect::<Vec<_>>(),
            "definition_ids_source_order":selected.iter().map(|&d|id(function,syntax.definitions[d].span)).collect::<Vec<_>>(),
            "unresolved_reads":unresolved,"dependency_edges_omitted":omitted});
            Ok((row, definitions, unresolved, omitted))
        })();
        let (row, definitions, unresolved, omitted) = match built {
            Ok(built) => built,
            Err(_) if budget.exhausted => {
                report["stop_reason"] = json!("query_work");
                break;
            }
            Err(err) => return Err(err),
        };
        let mut additional = serde_json::to_vec(&row)
            .map_err(|_| error("JSON serialization failed"))?
            .len()
            + 1;
        for (key, value) in &definitions {
            if report["definitions"].get(key).is_none() {
                additional += serde_json::to_vec(key)
                    .map_err(|_| error("JSON serialization failed"))?
                    .len()
                    + serde_json::to_vec(value)
                        .map_err(|_| error("JSON serialization failed"))?
                        .len()
                    + 2;
            }
        }
        if output_bytes.saturating_add(additional) > OUTPUT_CAP {
            report["stop_reason"] = json!("output_bytes");
            break;
        }
        output_bytes += additional;
        report["rows"]
            .as_array_mut()
            .expect("report rows")
            .push(row);
        for (key, value) in definitions {
            report["definitions"]
                .as_object_mut()
                .expect("report definitions")
                .insert(key, value);
        }
        report["unresolved_reads"] =
            json!(report["unresolved_reads"].as_u64().unwrap_or(0) + unresolved as u64);
        report["dependency_edges_omitted"] =
            json!(report["dependency_edges_omitted"].as_u64().unwrap_or(0) + omitted as u64);
        report["rows_omitted"] = json!(total - ordinal - 1);
        report["next_offset"] = json!(if ordinal + 1 < total {
            Some(ordinal + 1)
        } else {
            None
        });
        report["table_complete"] = json!(ordinal + 1 == total);
    }
    report["query_work_used"] = json!(budget.used);
    if report["next_offset"].is_number() && report["stop_reason"].is_null() {
        report["stop_reason"] = json!("rows");
    }
    if total == 0 {
        report["table_complete"] = json!(true);
    }
    if let Some(offset) = report["next_offset"].as_u64() {
        if syntax.stores.iter().all(|s| s.exporter_slot) {
            report["continuation_query"] = json!({"command":"sites","function":function,"kind":"slot-write","offset":offset,
                "flags":[function.to_string(),"--kind","slot-write","--offset",offset.to_string(),"--limit","1000","--depth","8","--compact","--max-bytes","16777216"],
                "input_scope":"same input HBC; raw slot-write ordinal, no filters","semantics":"Continue generic source-site inspection, not an evaluated initializer table"});
        } else {
            return Err(error("bounded direct env[N] table has no compatible sites continuation; inspect complete raw source"));
        }
    }
    if serde_json::to_vec(&report)
        .map_err(|_| error("JSON serialization failed"))?
        .len()
        > OUTPUT_CAP
    {
        return Err(error("serialized report byte cap exceeded"));
    }
    Ok(report)
}
