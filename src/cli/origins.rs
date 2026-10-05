//! Demand-driven candidate reaching definitions over exporter JS. Never evaluates JS.
use crate::bundle::export_function_fragments;
use crate::{DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::*;
use oxc_ast_visit::{walk, Visit};
use oxc_span::{GetSpan, Span};
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
}
impl<'a> Visit<'a> for Syntax {
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
}
struct Block {
    pc: u32,
    instructions: Vec<usize>,
    predecessors: Vec<usize>,
    unknown: bool,
    writes: BTreeMap<u32, Vec<usize>>,
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
    bounds(depth, limit, max_bytes)?;
    if source.len() > SOURCE_CAP {
        return Err(error("origins source byte cap exceeded"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() {
        return Err(error("origins requires valid complete exporter JS"));
    }
    let mut functions = Vec::new();
    for s in &parsed.program.body {
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
    for c in &parsed.program.comments {
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
                });
            }
            let id = current.ok_or_else(|| error("Missing exact PC annotation"))?;
            let ins = &mut instructions[id];
            ins.span.end = span.end;
            let mut syntax = Syntax::default();
            syntax.visit_statement(s);
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
            if total_reads > READ_CAP || total_writes > DEFINITION_CAP {
                return Err(error("origins syntax cap exceeded"));
            }
            ins.reads.extend(syntax.reads);
            ins.writes.extend(syntax.writes);
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
    let root = *pc_index
        .get(&pc)
        .ok_or_else(|| error(format!("Unknown exact PC {pc}")))?;
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
    // Unknown destinations may reach any block: retain known candidates but
    // explicitly mark every demand incomplete, never certify a sole definition.
    let globally_unknown = !unknown.is_empty();
    let mut definitions = BTreeMap::new();
    let mut demands = Vec::new();
    let mut queue = VecDeque::from([(root, instructions[root].span, None, 0usize)]);
    let mut expanded = BTreeSet::new();
    let mut truncated = false;
    let work_cap = limit.saturating_mul(128);
    let mut work = 0;
    while let Some((instruction, scope, owner, level)) = queue.pop_front() {
        if !expanded.insert((instruction, scope.start, scope.end)) {
            continue;
        }
        for &(reg, read_span) in &instructions[instruction].reads {
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
            if stopped {
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
                                },
                            );
                            id
                        };
                        demand.candidates.push(id);
                        if level < depth {
                            queue.push_back((iid, span, Some(id), level + 1));
                        } else if instructions[iid]
                            .reads
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
    let normal_edges: Vec<_> = edges.into_iter().take(DISPLAY_EDGE_CAP).collect();
    let result = Report {
        schema_version: 1,
        schema: "origins-v1",
        semantics:
            "candidate definitions only; no values, heap, captured slots, or constructor semantics",
        function,
        pc,
        unknown,
        truncated: truncated || normal_edges_truncated,
        unresolved,
        blocks: blocks.len(),
        normal_edges_total,
        normal_edges_returned: normal_edges.len(),
        normal_edges_truncated,
        normal_edges,
        control_index_work: control_work,
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
    };
    let mut bytes = serde_json::to_vec(&result).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > max_bytes {
        return Err(error("origins output byte budget exceeded"));
    }
    Ok(bytes)
}

/// Build and validate the entire report before emitting anything to stdout.
pub fn report(
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
        .ok_or_else(|| error("Unknown function"))?;
    let exceptions: Vec<_> = header
        .exc_handlers
        .iter()
        .map(|h| (h.start, h.end, h.target))
        .collect();
    let source = export_function_fragments(&hbc, &[function])?.remove(0).1;
    analyze_source(&source, function, pc, depth, limit, max_bytes, &exceptions)
}
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
