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
const OBJECT_SOURCE_CAP: usize = 64 * 1024 * 1024;
const OBJECT_SYNTAX_CAP: usize = 8_388_608;
const OBJECT_FILTER_CAP: usize = 134_217_728;

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
    object_mode: bool,
    aliases: BTreeMap<u32, Span>,
    primitive_rhs: BTreeMap<u32, Span>,
    literal_read_exclusions: BTreeSet<(u32, u32)>,
    ambiguous_writes: BTreeSet<u32>,
    conditional_depth: usize,
    function_depth: usize,
    root_function: Option<Span>,
    root_register: Option<Span>,
    work: usize,
    depth: usize,
    object_error: Option<&'static str>,
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
    fn visit_expression(&mut self, it: &Expression<'a>) {
        if self.object_mode {
            self.work += 1;
            if self.work > OBJECT_SYNTAX_CAP || self.depth == 256 {
                self.object_error = Some("objects syntax work/depth cap exceeded");
                return;
            }
            self.depth += 1;
        }
        walk::walk_expression(self, it);
        if self.object_mode {
            self.depth -= 1;
        }
    }

    fn visit_statement(&mut self, it: &Statement<'a>) {
        if self.object_mode {
            self.work += 1;
            if self.work > OBJECT_SYNTAX_CAP || self.depth == 256 {
                self.object_error = Some("objects syntax work/depth cap exceeded");
                return;
            }
            self.depth += 1;
        }
        walk::walk_statement(self, it);
        if self.object_mode {
            self.depth -= 1;
        }
    }

    fn visit_function(
        &mut self,
        it: &oxc_ast::ast::Function<'a>,
        flags: oxc_syntax::scope::ScopeFlags,
    ) {
        if self.object_mode && self.function_depth > 0 {
            self.object_error = Some("objects nested function scopes are unsupported");
            return;
        }
        if self.object_mode {
            if self.root_function.replace(it.span).is_some() {
                self.object_error = Some("objects requires one root function scope");
                return;
            }
            self.boundaries.extend([it.span.start, it.span.end]);
            if let Some(body) = &it.body {
                for statement in &body.statements {
                    let Statement::VariableDeclaration(declaration) = statement else {
                        continue;
                    };
                    for declaration in &declaration.declarations {
                        if let oxc_ast::ast::BindingPatternKind::BindingIdentifier(binding) =
                            &declaration.id.kind
                        {
                            if binding.name == "r"
                                && self.root_register.replace(binding.span).is_some()
                            {
                                self.object_error =
                                    Some("objects requires one root register binding");
                            }
                        }
                    }
                }
            }
        }
        self.function_depth += 1;
        walk::walk_function(self, it, flags);
        self.function_depth -= 1;
    }

    fn visit_arrow_function_expression(&mut self, it: &oxc_ast::ast::ArrowFunctionExpression<'a>) {
        if self.object_mode {
            self.object_error = Some("objects nested function scopes are unsupported");
        } else {
            walk::walk_arrow_function_expression(self, it);
        }
    }

    fn visit_class(&mut self, it: &oxc_ast::ast::Class<'a>) {
        if self.object_mode {
            self.object_error = Some("objects class scopes are unsupported");
        } else {
            walk::walk_class(self, it);
        }
    }

    fn visit_binding_identifier(&mut self, it: &oxc_ast::ast::BindingIdentifier<'a>) {
        if self.object_mode && it.name == "r" && self.root_register != Some(it.span) {
            self.object_error = Some("objects register shadowing is unsupported");
        }
    }

    fn visit_labeled_statement(&mut self, it: &oxc_ast::ast::LabeledStatement<'a>) {
        if self.object_mode {
            self.object_error = Some("objects labeled control flow is unsupported");
            return;
        }
        walk::walk_labeled_statement(self, it);
    }

    fn visit_for_statement(&mut self, it: &oxc_ast::ast::ForStatement<'a>) {
        if self.object_mode {
            if it.init.is_some() || it.test.is_some() || it.update.is_some() {
                self.object_error = Some("objects structured loops are unsupported");
                return;
            }
            self.boundaries.extend([it.span.start, it.span.end]);
        }
        walk::walk_for_statement(self, it);
    }

    fn visit_while_statement(&mut self, it: &oxc_ast::ast::WhileStatement<'a>) {
        if self.object_mode {
            self.object_error = Some("objects structured loops are unsupported");
        } else {
            walk::walk_while_statement(self, it);
        }
    }

    fn visit_do_while_statement(&mut self, it: &oxc_ast::ast::DoWhileStatement<'a>) {
        if self.object_mode {
            self.object_error = Some("objects structured loops are unsupported");
        } else {
            walk::walk_do_while_statement(self, it);
        }
    }

    fn visit_for_in_statement(&mut self, it: &oxc_ast::ast::ForInStatement<'a>) {
        if self.object_mode {
            self.object_error = Some("objects structured loops are unsupported");
        } else {
            walk::walk_for_in_statement(self, it);
        }
    }

    fn visit_for_of_statement(&mut self, it: &oxc_ast::ast::ForOfStatement<'a>) {
        if self.object_mode {
            self.object_error = Some("objects structured loops are unsupported");
        } else {
            walk::walk_for_of_statement(self, it);
        }
    }

    fn visit_switch_statement(&mut self, it: &oxc_ast::ast::SwitchStatement<'a>) {
        if self.object_mode {
            self.boundaries.extend([it.span.start, it.span.end]);
        }
        walk::walk_switch_statement(self, it);
    }

    fn visit_try_statement(&mut self, it: &oxc_ast::ast::TryStatement<'a>) {
        if self.object_mode {
            self.boundaries.extend([it.span.start, it.span.end]);
            if let Some(handler) = &it.handler {
                self.boundaries
                    .extend([handler.span.start, handler.span.end]);
            }
            if let Some(finalizer) = &it.finalizer {
                self.boundaries
                    .extend([finalizer.span.start, finalizer.span.end]);
            }
        }
        walk::walk_try_statement(self, it);
    }

    fn visit_conditional_expression(&mut self, it: &oxc_ast::ast::ConditionalExpression<'a>) {
        self.conditional_depth += 1;
        walk::walk_conditional_expression(self, it);
        self.conditional_depth -= 1;
    }

    fn visit_logical_expression(&mut self, it: &oxc_ast::ast::LogicalExpression<'a>) {
        self.conditional_depth += 1;
        walk::walk_logical_expression(self, it);
        self.conditional_depth -= 1;
    }

    fn visit_chain_expression(&mut self, it: &oxc_ast::ast::ChainExpression<'a>) {
        if self.object_mode {
            self.object_error = Some("objects optional chains are unsupported");
        } else {
            walk::walk_chain_expression(self, it);
        }
    }

    fn visit_if_statement(&mut self, it: &oxc_ast::ast::IfStatement<'a>) {
        if self.object_mode {
            self.boundaries.extend([it.span.start, it.span.end]);
            self.conditional_depth += 1;
        }
        walk::walk_if_statement(self, it);
        if self.object_mode {
            self.conditional_depth -= 1;
        }
    }

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
        if self.object_mode
            && matches!(
                it.left,
                AssignmentTarget::ArrayAssignmentTarget(_)
                    | AssignmentTarget::ObjectAssignmentTarget(_)
            )
        {
            self.object_error = Some("objects destructuring assignments are unsupported");
            return;
        }
        if self.object_mode
            && matches!(&it.left, AssignmentTarget::AssignmentTargetIdentifier(id) if id.name == "r")
        {
            self.object_error = Some("objects register binding reassignment is unsupported");
            return;
        }
        if self.wrapper_assignments.contains(&it.span.start) {
            self.visit_expression(&it.right);
            return;
        }
        let lhs = it.left.span();
        let root_member = match &it.left {
            AssignmentTarget::ComputedMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            AssignmentTarget::StaticMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            _ => false,
        };
        if self.object_mode && root_member && self.register(lhs).is_none() {
            self.object_error = Some("objects noncanonical register mutations are unsupported");
            return;
        }
        if let Some(register) = self.register(lhs) {
            self.writes.push((register, it.span));
            if self.object_mode {
                if self.conditional_depth > 0 || it.operator.is_logical() {
                    self.ambiguous_writes.insert(it.span.start);
                }
                if it.operator.is_assign() && self.register(it.right.span()).is_some() {
                    self.aliases.insert(it.span.start, it.right.span());
                }
                if it.operator.is_assign() {
                    let rhs = it.right.without_parentheses();
                    let primitive = match rhs {
                        Expression::StringLiteral(_)
                        | Expression::NumericLiteral(_)
                        | Expression::BooleanLiteral(_)
                        | Expression::NullLiteral(_) => true,
                        Expression::UnaryExpression(u) => {
                            matches!(
                                u.operator,
                                oxc_ast::ast::UnaryOperator::UnaryNegation
                                    | oxc_ast::ast::UnaryOperator::UnaryPlus
                            ) && matches!(
                                u.argument.without_parentheses(),
                                Expression::NumericLiteral(_)
                            )
                        }
                        _ => false,
                    };
                    if primitive {
                        self.primitive_rhs.insert(it.span.start, rhs.span());
                    }
                } else {
                    self.literal_read_exclusions.insert((lhs.start, lhs.end));
                }
            }
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
                if self.object_mode && !it.operator.is_assign() {
                    self.object_error = Some("objects requires simple property assignments");
                    return;
                }
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
        // Logical-assignment RHS is conditional too, including nested writes.
        let conditional = self.object_mode && it.operator.is_logical();
        self.conditional_depth += usize::from(conditional);
        walk::walk_assignment_expression(self, it);
        self.conditional_depth -= usize::from(conditional);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        let root_member = match &it.argument {
            oxc_ast::ast::SimpleAssignmentTarget::ComputedMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            oxc_ast::ast::SimpleAssignmentTarget::StaticMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            _ => false,
        };
        if self.object_mode && root_member && self.register(it.argument.span()).is_none() {
            self.object_error = Some("objects noncanonical register mutations are unsupported");
            return;
        }
        if self.object_mode
            && matches!(&it.argument, oxc_ast::ast::SimpleAssignmentTarget::AssignmentTargetIdentifier(id) if id.name == "r")
        {
            self.object_error = Some("objects register binding reassignment is unsupported");
            return;
        }
        if let Some(register) = self.register(it.argument.span()) {
            if self.object_mode {
                self.literal_read_exclusions
                    .insert((it.argument.span().start, it.argument.span().end));
            }
            self.writes.push((register, it.span));
            if self.object_mode && self.conditional_depth > 0 {
                self.ambiguous_writes.insert(it.span.start);
            }
        }
        walk::walk_update_expression(self, it);
    }

    fn visit_unary_expression(&mut self, it: &oxc_ast::ast::UnaryExpression<'a>) {
        let root_member = match it.argument.without_parentheses() {
            Expression::ComputedMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            Expression::StaticMemberExpression(m) => {
                m.object.without_parentheses().is_specific_id("r")
            }
            _ => false,
        };
        if self.object_mode && it.operator.is_delete() && root_member {
            self.object_error = Some("objects register slot deletion is unsupported");
            return;
        }
        walk::walk_unary_expression(self, it);
    }

    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if self.object_mode && it.optional {
            self.object_error = Some("objects optional calls are unsupported");
            return;
        }
        if let Expression::Identifier(callee) = &it.callee {
            let name = callee.name.as_str();
            if self.object_mode && (name == "construct" || name == "apply") {
                let valid = match it.arguments.as_slice() {
                    [callee, receiver, oxc_ast::ast::Argument::ArrayExpression(args)] => {
                        !it.optional
                            && !matches!(callee, oxc_ast::ast::Argument::SpreadElement(_))
                            && !matches!(receiver, oxc_ast::ast::Argument::SpreadElement(_))
                            && !args.elements.iter().any(|arg| {
                                matches!(
                                    arg,
                                    oxc_ast::ast::ArrayExpressionElement::SpreadElement(_)
                                        | oxc_ast::ast::ArrayExpressionElement::Elision(_)
                                )
                            })
                    }
                    _ => false,
                };
                if !valid {
                    self.object_error = Some("objects malformed construct/apply helper");
                    return;
                }
            }
            if name == "construct" && !self.object_mode {
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
            } else if name == "apply" && !self.object_mode {
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
                if self.object_mode
                    && (it.optional
                        || it.arguments.len() != if name == "put" { 5 } else { 4 }
                        || it
                            .arguments
                            .iter()
                            .any(|a| matches!(a, oxc_ast::ast::Argument::SpreadElement(_))))
                {
                    self.object_error =
                        Some("objects put/own requires exact arity, no optional call or spreads");
                    return;
                }
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
    alias: Option<Span>,
    primitive: Option<Span>,
}

struct Index {
    syntax: Vec<SiteSpan>,
    markers: Vec<(u32, u32, u32)>,
    reads: Vec<Read>,
    definitions: Vec<Definition>,
    literal_read_exclusions: BTreeSet<(u32, u32)>,
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
    index_source_mode(source, function, exceptions, false)
}

fn index_source_mode(
    source: &str,
    function: u32,
    exceptions: &BTreeSet<u32>,
    object_mode: bool,
) -> DecompilerResult<Index> {
    if object_mode && source.len() > OBJECT_SOURCE_CAP {
        return Err(error("objects source byte cap exceeded"));
    }
    if source.len() > u32::MAX as usize {
        return Err(error("Exporter source exceeds AST span bounds"));
    }
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() || parsed.panicked {
        return Err(error("sites requires valid complete exporter JavaScript"));
    }
    let mut syntax = Syntax {
        source,
        object_mode,
        ..Syntax::default()
    };
    syntax.visit_program(&parsed.program);
    if let Some(message) = syntax.object_error {
        return Err(error(message));
    }
    if object_mode {
        if syntax.root_register.is_none() {
            return Err(error("objects requires one root register binding"));
        }
        let root = syntax
            .root_function
            .ok_or_else(|| error("objects requires one root function scope"))?;
        let mut exporter_body = None;
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
            if !member.object.is_specific_id("F") {
                continue;
            }
            if !assignment.operator.is_assign()
                || !matches!(&member.expression, Expression::NumericLiteral(id) if id.value == f64::from(function))
            {
                return Err(error("objects wrong-function exporter root"));
            }
            let Expression::FunctionExpression(fun) = &assignment.right else {
                return Err(error("objects root requires a literal function"));
            };
            if fun.span != root || exporter_body.is_some() {
                return Err(error("objects requires one matching exporter root"));
            }
            exporter_body = fun.body.as_ref().map(|body| body.span);
        }
        let body = exporter_body.ok_or_else(|| error("objects missing matching exporter root"))?;
        if parsed.program.comments.iter().any(|comment| {
            source[comment.span.start as usize..comment.span.end as usize]
                .starts_with("// HBC function ")
                && (comment.span.start < body.start || comment.span.end > body.end)
        }) {
            return Err(error("objects PC marker outside exporter root"));
        }
        if syntax
            .reads
            .iter()
            .chain(&syntax.writes)
            .any(|(_, span)| span.start < root.start || span.end > root.end)
            || syntax
                .sites
                .iter()
                .any(|site| site.span.start < root.start || site.span.end > root.end)
        {
            return Err(error(
                "objects register/property syntax outside root function",
            ));
        }
    }
    if object_mode
        && (syntax.reads.len() > 2_097_152
            || syntax.writes.len() > 1_048_576
            || syntax.sites.len() > 1_048_576)
    {
        return Err(error("objects syntax record cap exceeded"));
    }
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
    let mut ambiguous = BTreeSet::new();
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
            ambiguous.clear();
        }
        let ws = syntax.writes.partition_point(|(_, s)| s.start < start);
        let we = syntax.writes.partition_point(|(_, s)| s.start < end);
        let mut first_end = BTreeMap::new();
        let mut write_counts = BTreeMap::<u32, usize>::new();
        for &(reg, span) in &syntax.writes[ws..we] {
            if object_mode && span.end > end {
                return Err(error("objects definition crosses PC boundary"));
            }
            *write_counts.entry(reg).or_default() += 1;
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
            } else if object_mode && ambiguous.contains(&register) {
                "unresolved_ambiguous_source_write"
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
            ambiguous.clear();
        }
        for &(register, span) in &syntax.writes[ws..we] {
            let id = definitions.len();
            definitions.push(Definition {
                pc,
                register,
                span,
                alias: syntax.aliases.get(&span.start).copied(),
                primitive: syntax.primitive_rhs.get(&span.start).copied(),
            });
            if last_boundary.is_none_or(|b| span.start > b) {
                if object_mode
                    && (write_counts[&register] > 1
                        || syntax.ambiguous_writes.contains(&span.start))
                {
                    prior.remove(&register);
                    ambiguous.insert(register);
                } else {
                    prior.insert(register, id);
                    ambiguous.remove(&register);
                }
            }
        }
        previous = start;
    }
    Ok(Index {
        syntax: syntax.sites,
        markers,
        reads,
        definitions,
        literal_read_exclusions: syntax.literal_read_exclusions,
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

fn object_operand(
    source: &str,
    index: &Index,
    span: &OperandSpan,
    depth: usize,
    budget: &mut ObjectBudget,
) -> DecompilerResult<Operand> {
    // Preflight the bounded traversal before allocating verbose edge/snippet copies.
    budget.charge()?;
    let reads = reads_in(&index.reads, span.span);
    let mut seen = BTreeSet::new();
    let mut queue = VecDeque::new();
    for read in reads.iter().take(NODE_CAP) {
        budget.charge()?;
        if let Some(id) = read.definition {
            queue.push_back((id, 1));
        }
    }
    while let Some((id, level)) = queue.pop_front() {
        budget.charge()?;
        if seen.contains(&id) || level > depth || seen.len() == NODE_CAP {
            continue;
        }
        seen.insert(id);
        for read in reads_in(&index.reads, index.definitions[id].span)
            .iter()
            .take(NODE_CAP)
        {
            budget.charge()?;
            if let Some(id) = read.definition {
                queue.push_back((id, level + 1));
            }
        }
    }
    Ok(operand(source, index, span, depth))
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

#[derive(Serialize)]
struct FollowUpQuery {
    command: &'static str,
    function: u32,
    pc: u32,
    flags: [&'static str; 1],
    input_scope: &'static str,
    reason: &'static str,
    limits: &'static str,
}

fn follow_up_queries(sites: &[Site]) -> Vec<FollowUpQuery> {
    // Inspect only included provenance, before compacting away the typed edges.
    let targets: BTreeSet<_> = sites
        .iter()
        .filter(|site| {
            site.operands.iter().any(|operand| {
                operand
                    .edges
                    .iter()
                    .chain(operand.nodes.iter().flat_map(|node| &node.edges))
                    .any(|edge| edge.status == "unresolved_block_entry_or_external")
            })
        })
        .map(|site| (site.function_id, site.pc))
        .collect();
    targets
        .into_iter()
        .map(|(function, pc)| FollowUpQuery {
            command: "origins",
            function,
            pc,
            flags: ["--expressions"],
            input_scope: "same_input_hbc",
            reason: "unresolved_block_entry_or_external",
            limits: "Evidence is an included direct operand or bounded prior-definition dependency read. Candidate navigation over bounded normal dispatcher paths only, not runtime values or capture resolution; unknown registers are not evidence of captures. Omitted dependencies are not proof of absence.",
        })
        .collect()
}

#[derive(Default)]
pub struct SiteFilter {
    pub matches: Vec<String>,
    pub from_pc: Option<u32>,
    pub to_pc: Option<u32>,
}

impl SiteFilter {
    fn matcher(&self) -> DecompilerResult<Option<Regex>> {
        self.matcher_with_limit(16)
    }

    fn matcher_with_limit(&self, limit: usize) -> DecompilerResult<Option<Regex>> {
        if self.from_pc.zip(self.to_pc).is_some_and(|(a, b)| a > b) {
            return Err(error("--from-pc must not exceed --to-pc"));
        }
        if self.matches.len() > limit
            || self
                .matches
                .iter()
                .any(|query| query.is_empty() || query.len() > 1024)
        {
            return Err(error(format!(
                "--match accepts at most {limit} nonempty queries of at most 1024 bytes"
            )));
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
    mut budget: Option<&mut ObjectBudget>,
) -> DecompilerResult<MatchCandidates> {
    let direct = if let Some(b) = budget.as_deref_mut() {
        b.matches(source, site.span, matcher)?
    } else {
        matcher.is_match(&source[site.span.start as usize..site.span.end as usize])
    };
    let mut result = MatchCandidates {
        direct,
        definitions: BTreeSet::new(),
        truncated: site.operands.len() > OPERAND_CAP,
        unresolved: false,
    };
    for operand in site.operands.iter().take(OPERAND_CAP) {
        if let Some(b) = budget.as_deref_mut() {
            b.charge()?;
        }
        let reads = reads_in(&index.reads, operand.span);
        result.truncated |= reads.len() > NODE_CAP;
        let mut queue = VecDeque::new();
        let mut visited = BTreeSet::new();
        for read in reads.iter().take(NODE_CAP) {
            if let Some(b) = budget.as_deref_mut() {
                b.charge()?;
            }
            if let Some(id) = read.definition {
                queue.push_back((id, 1));
            } else {
                result.unresolved = true;
            }
        }
        while let Some((id, level)) = queue.pop_front() {
            if let Some(b) = budget.as_deref_mut() {
                b.charge()?;
            }
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
                if let Some(b) = budget.as_deref_mut() {
                    b.charge()?;
                }
                if let Some(next) = read.definition {
                    queue.push_back((next, level + 1));
                } else {
                    result.unresolved = true;
                }
            }
        }
    }
    Ok(result)
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

#[derive(Clone, Debug)]
pub struct ObjectOptions {
    pub alias_depth: usize,
    pub depth: usize,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
    pub matches: Vec<String>,
}

impl Default for ObjectOptions {
    fn default() -> Self {
        Self {
            alias_depth: 16,
            depth: 3,
            limit: 5,
            offset: 0,
            max_bytes: 100_000,
            scan_work: 2_097_152,
            matches: vec![],
        }
    }
}

fn object_options(options: &ObjectOptions) -> DecompilerResult<Option<Regex>> {
    if options.alias_depth > 64
        || options.depth > 8
        || !(1..=1000).contains(&options.limit)
        || !(1..=16_777_216).contains(&options.max_bytes)
        || !(1..=16_777_216).contains(&options.scan_work)
    {
        return Err(error("objects bounds: alias_depth 0..64, depth 0..8, limit 1..1000, max_bytes/scan_work 1..16777216"));
    }
    SiteFilter {
        matches: options.matches.clone(),
        ..SiteFilter::default()
    }
    .matcher_with_limit(64)
}

/// Navigation views retain raw JS; substitutions are source candidates, not execution.
pub struct LiteralViewOptions {
    pub matches: Vec<String>,
    pub from_pc: Option<u32>,
    pub to_pc: Option<u32>,
    pub alias_depth: usize,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
    pub json: bool,
}

impl Default for LiteralViewOptions {
    fn default() -> Self {
        Self {
            matches: vec![],
            from_pc: None,
            to_pc: None,
            alias_depth: 16,
            limit: 50,
            offset: 0,
            max_bytes: 100_000,
            scan_work: 2_097_152,
            json: false,
        }
    }
}

fn literal_view_options(o: &LiteralViewOptions) -> DecompilerResult<Option<Regex>> {
    if o.alias_depth > 64
        || !(1..=1000).contains(&o.limit)
        || !(1..=16_777_216).contains(&o.max_bytes)
        || !(1..=16_777_216).contains(&o.scan_work)
    {
        return Err(error(
            "read bounds: alias_depth 0..64, limit 1..1000, max_bytes/scan_work 1..16777216",
        ));
    }
    SiteFilter {
        matches: o.matches.clone(),
        from_pc: o.from_pc,
        to_pc: o.to_pc,
    }
    .matcher_with_limit(64)
}

#[derive(Serialize)]
struct LiteralRead {
    register: u32,
    source_span: [u32; 2],
    status: &'static str,
    definition_id: Option<String>,
    definition_pc: Option<u32>,
    literal_span: Option<[u32; 2]>,
    alias_definition_ids: Vec<String>,
    literal: Option<String>,
}

#[derive(Serialize)]
struct LiteralRow {
    pc: u32,
    source_span: [u32; 2],
    source: String,
    view: String,
    reads: Vec<LiteralRead>,
    read_count: usize,
    reads_omitted: usize,
}

fn primitive_read(
    source: &str,
    function: u32,
    index: &Index,
    read: &Read,
    depth: usize,
    budget: &mut ObjectBudget,
) -> DecompilerResult<LiteralRead> {
    budget.charge()?;
    let mut result = LiteralRead {
        register: read.register,
        source_span: [read.span.start, read.span.end],
        status: read.status,
        definition_id: None,
        definition_pc: None,
        literal_span: None,
        alias_definition_ids: vec![],
        literal: None,
    };
    if index
        .literal_read_exclusions
        .contains(&(read.span.start, read.span.end))
    {
        result.status = "assignment_target_not_substituted";
        return Ok(result);
    }
    let mut current = read;
    loop {
        budget.charge()?;
        let Some(id) = current.definition else {
            result.status = current.status;
            return Ok(result);
        };
        let d = &index.definitions[id];
        let definition_id = format!("{function}:{}:{}", d.span.start, d.span.end);
        if let Some(span) = d.primitive {
            result.definition_id = Some(definition_id);
            result.definition_pc = Some(d.pc);
            result.literal_span = Some([span.start, span.end]);
            if span.end - span.start > 4096 {
                result.status = "literal_byte_limit";
            } else {
                budget.filter_charge(span)?;
                result.literal = Some(source[span.start as usize..span.end as usize].to_owned());
                result.status = "source_literal_candidate";
            }
            return Ok(result);
        }
        let Some(alias) = d.alias else {
            result.status = "non_primitive_definition";
            result.definition_id = Some(definition_id);
            result.definition_pc = Some(d.pc);
            return Ok(result);
        };
        if result.alias_definition_ids.len() == depth {
            result.status = "alias_depth_limit";
            return Ok(result);
        }
        result.alias_definition_ids.push(definition_id);
        let [next] = reads_in(&index.reads, alias) else {
            result.status = "unresolved_alias_read";
            return Ok(result);
        };
        if next.span != alias || next.definition.is_some_and(|next| next >= id) {
            result.status = "unresolved_alias_read";
            return Ok(result);
        }
        current = next;
    }
}

fn literal_view_flags(o: &LiteralViewOptions, offset: usize) -> Vec<String> {
    let mut flags = vec![
        "--alias-depth".into(),
        o.alias_depth.to_string(),
        "--limit".into(),
        o.limit.to_string(),
        "--offset".into(),
        offset.to_string(),
        "--max-bytes".into(),
        o.max_bytes.to_string(),
        "--scan-work".into(),
        o.scan_work.to_string(),
    ];
    for m in &o.matches {
        flags.push(format!("--match={m}"));
    }
    for (flag, value) in [("--from-pc", o.from_pc), ("--to-pc", o.to_pc)] {
        if let Some(pc) = value {
            flags.extend([flag.into(), pc.to_string()]);
        }
    }
    if o.json {
        flags.push("--json".into());
    }
    flags
}

/// A bounded, source-linked candidate view. This never replaces runnable export.
pub fn analyze_literal_view_source(
    source: &str,
    function: u32,
    exceptions: &BTreeSet<u32>,
    o: &LiteralViewOptions,
) -> DecompilerResult<Vec<u8>> {
    let matcher = literal_view_options(o)?;
    let index = index_source_mode(source, function, exceptions, true)?;
    let mut budget = ObjectBudget {
        used: 0,
        cap: o.scan_work,
        filter_used: 0,
    };
    let mut rows = Vec::new();
    let mut next_offset = None;
    let mut total = 0usize;
    let mut construction = 0usize;
    let mut unresolved = 0usize;
    let mut omitted = 0usize;
    for (ordinal, &(pc, _, start)) in index.markers.iter().enumerate() {
        budget.charge()?;
        if o.from_pc.is_some_and(|p| pc < p) || o.to_pc.is_some_and(|p| pc > p) {
            continue;
        }
        let end = index
            .markers
            .get(ordinal + 1)
            .map_or(source.len() as u32, |m| m.1);
        let span = Span::new(start, end);
        let raw = &source[start as usize..end as usize];
        let mut matched = matcher
            .as_ref()
            .map(|m| budget.matches(source, span, m))
            .transpose()?
            .unwrap_or(true);
        let reads = reads_in(&index.reads, span);
        let mut candidates = Vec::new();
        for read in reads {
            let candidate =
                primitive_read(source, function, &index, read, o.alias_depth, &mut budget)?;
            unresolved += usize::from(candidate.status != "source_literal_candidate");
            if let (Some(m), Some(text)) = (&matcher, &candidate.literal) {
                matched |= m.is_match(text);
            }
            if candidates.len() < 128 {
                candidates.push(candidate);
            }
        }
        omitted += reads.len().saturating_sub(128);
        if !matched {
            continue;
        }
        total += 1;
        if ordinal < o.offset {
            continue;
        }
        if rows.len() == o.limit {
            next_offset.get_or_insert(ordinal);
            continue;
        }
        // Charge worst-case JSON escaping before duplicating raw source or literals.
        let estimate = raw.len().saturating_mul(12).saturating_add(
            candidates
                .iter()
                .map(|r| {
                    r.literal
                        .as_ref()
                        .map_or(0, String::len)
                        .saturating_mul(12)
                        .saturating_add(r.alias_definition_ids.len().saturating_mul(128))
                        .saturating_add(1024)
                })
                .sum::<usize>(),
        );
        construction = construction.saturating_add(estimate);
        if construction > 16_777_216 {
            return Err(error(
                "read construction byte cap exceeded before stdout; narrow the PC range or page",
            ));
        }
        let mut view = String::with_capacity(raw.len());
        let mut cursor = start as usize;
        for candidate in &candidates {
            if let Some(literal) = &candidate.literal {
                let [a, b] = candidate.source_span;
                if (a as usize) < cursor || b > end {
                    return Err(error("read overlapping or cross-PC replacement"));
                }
                view.push_str(&source[cursor..a as usize]);
                view.push('(');
                view.push_str(literal);
                view.push(')');
                cursor = b as usize;
            }
        }
        view.push_str(&source[cursor..end as usize]);
        rows.push(LiteralRow {
            pc,
            source_span: [start, end],
            source: raw.to_owned(),
            view,
            reads: candidates,
            read_count: reads.len(),
            reads_omitted: reads.len().saturating_sub(128),
        });
    }
    let continuation = next_offset.map(|offset| serde_json::json!({"command":"read","function":function,
        "flags":literal_view_flags(o,offset),"input_scope":"same_input_hbc","offset_scope":"Raw PC-marker ordinal before filters",
        "semantics":"Argument tokens are data, not shell code; paging does not repair unresolved or omitted reads."}));
    let warning = "Navigation-only source-local literal candidates, NOT executable output or proof of runtime values. Raw source is authoritative. Only string/number/boolean/null literal syntax and plain register copies within normal local blocks are substituted. Calls, heap fields, captures and framework objects are never evaluated. Same-PC/conditional/block/exception uncertainty remains unresolved.";
    let mut bytes = if o.json {
        serde_json::to_vec(&serde_json::json!({"schema":"literal-view-v1","schema_version":1,"function":function,
            "source":"export_function_fragments","source_span_basis":"Complete raw function fragment, excluding workspace prefix",
            "warning":warning,"format":"json","limit":o.limit,"offset":o.offset,"offset_scope":"Raw PC-marker ordinal before filters",
            "from_pc":o.from_pc,"to_pc":o.to_pc,"matches":o.matches,"alias_depth":o.alias_depth,
            "total":total,"unfiltered_total":index.markers.len(),"next_offset":next_offset,"scan_complete":next_offset.is_none(),
            "summary_scope":"All reads in the selected PC range, including source-filtered-out uncertainty",
            "unresolved_reads":unresolved,"reads_omitted":omitted,"work_used":budget.used,"work_cap":budget.cap,
            "filter_work_used":budget.filter_used,"filter_work_cap":OBJECT_FILTER_CAP,"construction_upper_bound":construction,
            "limits":{"source_bytes":OBJECT_SOURCE_CAP,"candidate_literal_bytes":4096,"reads_per_row":128,"output_bytes":o.max_bytes,
                "construction_bytes":16_777_216,"syntax_work":OBJECT_SYNTAX_CAP,"syntax_depth":256},
            "rows":rows,"continuation_query":continuation})).map_err(|e|error(e.to_string()))?
    } else {
        let mut text = format!("# F{function} source-local literal view\n# {warning}\n# Spans join complete raw fragments, excluding workspace prefix.\n");
        for row in &rows {
            use std::fmt::Write as _;
            writeln!(
                text,
                "PC {} raw [{}:{}]:\n{}",
                row.pc,
                row.source_span[0],
                row.source_span[1],
                row.source.trim_end()
            )
            .unwrap();
            if row.source != row.view {
                writeln!(text, "candidate view:\n{}", row.view.trim_end()).unwrap();
            }
            for read in &row.reads {
                writeln!(
                    text,
                    "  r[{}] [{}:{}] {} definition={} PC={} literal_span={:?} aliases={}",
                    read.register,
                    read.source_span[0],
                    read.source_span[1],
                    read.status,
                    read.definition_id.as_deref().unwrap_or("?"),
                    read.definition_pc
                        .map_or_else(|| "?".into(), |p| p.to_string()),
                    read.literal_span,
                    read.alias_definition_ids.join(",")
                )
                .unwrap();
            }
            if row.reads_omitted > 0 {
                writeln!(text, "  omitted_reads={}", row.reads_omitted).unwrap();
            }
        }
        use std::fmt::Write as _;
        writeln!(text,"# matched_PCs={total} unresolved_reads={unresolved} omitted_reads={omitted} scan_complete={} work={}/{}",next_offset.is_none(),budget.used,budget.cap).unwrap();
        if let Some(query) = continuation {
            writeln!(text, "# continuation_query={query}").unwrap();
        }
        text.into_bytes()
    };
    if !bytes.ends_with(b"\n") {
        bytes.push(b'\n');
    }
    if bytes.len() > o.max_bytes {
        return Err(error("read output byte budget exceeded before stdout; narrow the PC range, reduce --limit, or increase --max-bytes"));
    }
    Ok(bytes)
}

pub fn report_literal_view(
    input: &Path,
    function: u32,
    options: &LiteralViewOptions,
) -> DecompilerResult<Vec<u8>> {
    literal_view_options(options)?;
    use std::io::Read;
    let mut data = Vec::new();
    std::fs::File::open(input)?
        .take(134_217_729)
        .read_to_end(&mut data)?;
    if data.len() > 134_217_728 {
        return Err(error("read HBC input cap exceeded before parsing"));
    }
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let header = hbc
        .functions
        .get_parsed_header(function)
        .ok_or_else(|| error("Unknown read function"))?;
    let exceptions = header
        .exc_handlers
        .iter()
        .flat_map(|h| [h.start, h.end, h.target])
        .collect();
    let source = crate::bundle::export_function_fragment_bounded(
        &hbc,
        function,
        OBJECT_SOURCE_CAP,
        OBJECT_SOURCE_CAP,
    )?;
    analyze_literal_view_source(&source, function, &exceptions, options)
}

/// Compact workspace notes use the same conservative candidates as `read`.
/// Raw source is always retained; these notes never substitute executable JS.
#[derive(Default)]
pub struct WorkspaceLiteralNotes {
    pub notes: BTreeMap<u32, String>,
    pub unresolved_reads: usize,
    pub reads_omitted: usize,
    pub copy_reads_skipped: usize,
    pub next_offset: Option<usize>,
    pub work_used: usize,
}

#[derive(Serialize)]
struct WorkspaceLiteralUse {
    r: u32,
    use_span: [u32; 2],
    literal: String,
    from: [u32; 3],
    definition: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    via: Vec<String>,
}

pub fn workspace_literal_notes(
    source: &str,
    function: u32,
    exceptions: &BTreeSet<u32>,
) -> DecompilerResult<WorkspaceLiteralNotes> {
    const NOTE_BYTES: usize = 8 * 1024 * 1024;
    const ROW_BYTES: usize = 64 * 1024;
    let index = index_source_mode(source, function, exceptions, true)?;
    let mut result = WorkspaceLiteralNotes::default();
    let copies: BTreeSet<_> = index
        .definitions
        .iter()
        .filter_map(|d| d.alias)
        .map(|span| (span.start, span.end))
        .collect();
    let mut budget = ObjectBudget {
        used: 0,
        cap: 16_777_216,
        filter_used: 0,
    };
    let mut bytes = 0usize;
    for (ordinal, &(pc, _, start)) in index.markers.iter().enumerate() {
        if budget.used == budget.cap {
            result.next_offset = Some(ordinal);
            break;
        }
        budget.charge()?;
        let end = index
            .markers
            .get(ordinal + 1)
            .map_or(source.len() as u32, |m| m.1);
        let reads = reads_in(&index.reads, Span::new(start, end));
        let mut candidates = Vec::new();
        let mut row_bytes = 0usize;
        for read in reads.iter().take(128) {
            // Copies remain in the raw view and provenance chains; annotate actual uses.
            if copies.contains(&(read.span.start, read.span.end)) {
                result.copy_reads_skipped += 1;
                continue;
            }
            let candidate = match primitive_read(source, function, &index, read, 16, &mut budget) {
                Ok(candidate) => candidate,
                Err(_) if budget.used == budget.cap || budget.filter_used > OBJECT_FILTER_CAP => {
                    result.next_offset = Some(ordinal);
                    break;
                }
                Err(error) => return Err(error),
            };
            if candidate.literal.is_none() {
                result.unresolved_reads += 1;
                continue;
            }
            let (Some(literal), Some(definition), Some(pc), Some([a, b])) = (
                candidate.literal,
                candidate.definition_id,
                candidate.definition_pc,
                candidate.literal_span,
            ) else {
                return Err(error("workspace literal candidate lacks source provenance"));
            };
            let candidate = WorkspaceLiteralUse {
                r: candidate.register,
                use_span: candidate.source_span,
                literal,
                from: [pc, a, b],
                definition,
                via: candidate.alias_definition_ids,
            };
            // Exact per-candidate serialization keeps the row bound independent of escaping.
            let size = serde_json::to_string(&candidate)
                .map_err(|e| error(e.to_string()))?
                .replace('\u{2028}', "\\u2028")
                .replace('\u{2029}', "\\u2029")
                .len();
            if row_bytes.saturating_add(size + 1) > ROW_BYTES - 2 {
                result.reads_omitted += 1;
                continue;
            }
            row_bytes += size + 1;
            candidates.push(candidate);
        }
        result.reads_omitted += reads.len().saturating_sub(128);
        if result.next_offset.is_some() {
            break;
        }
        if !candidates.is_empty() {
            let note = serde_json::to_string(&candidates)
                .map_err(|e| error(e.to_string()))?
                .replace('\u{2028}', "\\u2028")
                .replace('\u{2029}', "\\u2029");
            if bytes.saturating_add(note.len()) > NOTE_BYTES {
                result.next_offset = Some(ordinal);
                break;
            }
            bytes += note.len();
            result.notes.insert(pc, note);
        }
    }
    result.work_used = budget.used;
    Ok(result)
}

struct ObjectBudget {
    used: usize,
    cap: usize,
    filter_used: usize,
}
impl ObjectBudget {
    fn charge(&mut self) -> DecompilerResult<()> {
        if self.used == self.cap {
            return Err(error("objects scan_work cap exceeded before stdout"));
        }
        self.used += 1;
        Ok(())
    }
    fn filter_charge(&mut self, span: Span) -> DecompilerResult<()> {
        let bytes = (span.end - span.start) as usize;
        if bytes > OBJECT_FILTER_CAP - self.filter_used {
            return Err(error("objects filter byte-work cap exceeded before stdout"));
        }
        self.filter_used += bytes;
        Ok(())
    }
    fn matches(&mut self, source: &str, span: Span, matcher: &Regex) -> DecompilerResult<bool> {
        self.filter_charge(span)?;
        Ok(matcher.is_match(&source[span.start as usize..span.end as usize]))
    }
}

struct ObjectTrace {
    status: &'static str,
    register: Option<u32>,
    terminal: Option<usize>,
    aliases: Vec<usize>,
    alias_depth_truncated: bool,
}

fn object_trace(
    index: &Index,
    site: &SiteSpan,
    depth: usize,
    budget: &mut ObjectBudget,
) -> DecompilerResult<ObjectTrace> {
    budget.charge()?;
    let mut trace = ObjectTrace {
        status: "non_register_expression",
        register: None,
        terminal: None,
        aliases: vec![],
        alias_depth_truncated: false,
    };
    let Some(object) = site.operands.iter().find(|o| o.role == "object") else {
        return Ok(trace);
    };
    let reads = reads_in(&index.reads, object.span);
    let [read] = reads else {
        return Ok(trace);
    };
    if read.span != object.span {
        return Ok(trace);
    }
    trace.register = Some(read.register);
    let mut current = read;
    let mut visited = BTreeSet::new();
    loop {
        budget.charge()?;
        let Some(id) = current.definition else {
            trace.status = current.status;
            break;
        };
        if !visited.insert(id) {
            trace.status = "unresolved_alias_cycle";
            break;
        }
        let d = &index.definitions[id];
        let Some(alias) = d.alias else {
            trace.status = "resolved";
            trace.terminal = Some(id);
            break;
        };
        if trace.aliases.len() == depth {
            trace.status = "alias_depth_truncated";
            trace.alias_depth_truncated = true;
            break;
        }
        trace.aliases.push(id);
        let [next] = reads_in(&index.reads, alias) else {
            trace.status = "unresolved_alias_read";
            break;
        };
        if next.span != alias {
            trace.status = "unresolved_alias_read";
            break;
        }
        current = next;
    }
    Ok(trace)
}

fn object_flags(o: &ObjectOptions, offset: usize, include_matches: bool) -> Vec<String> {
    let mut flags = vec![
        "--alias-depth".into(),
        o.alias_depth.to_string(),
        "--depth".into(),
        o.depth.to_string(),
        "--limit".into(),
        o.limit.to_string(),
        "--offset".into(),
        offset.to_string(),
        "--max-bytes".into(),
        o.max_bytes.to_string(),
        "--scan-work".into(),
        o.scan_work.to_string(),
    ];
    if include_matches {
        for m in &o.matches {
            flags.extend(["--match".into(), m.clone()]);
        }
    }
    flags
}

/// Source-local object-definition navigation; never resolves heap identity or values.
pub fn analyze_objects_source(
    source: &str,
    function: u32,
    origin_pc: Option<u32>,
    exceptions: &BTreeSet<u32>,
    options: &ObjectOptions,
) -> DecompilerResult<Vec<u8>> {
    let o = options;
    let matcher = object_options(o)?;
    let index = index_source_mode(source, function, exceptions, true)?;
    if origin_pc.is_some_and(|pc| index.markers.binary_search_by_key(&pc, |m| m.0).is_err()) {
        return Err(error("Unknown exact objects origin PC"));
    }
    let mut budget = ObjectBudget {
        used: 0,
        cap: o.scan_work,
        filter_used: 0,
    };
    let mut definition_matches = Vec::with_capacity(index.definitions.len());
    for d in &index.definitions {
        definition_matches.push(match &matcher {
            Some(m) => {
                budget.charge()?;
                budget.matches(source, d.span, m)?
            }
            None => false,
        });
    }
    let mut selected = Vec::new();
    let mut traces = Vec::new();
    let mut next_offset = None;
    let mut construction_bytes = 0usize;
    let mut total = 0usize;
    let mut unfiltered_total = 0usize;
    let mut unresolved_origins = 0usize;
    let mut alias_truncated = 0usize;
    let mut non_register = 0usize;
    let mut dependency_search_truncated_sites = 0usize;
    let mut unresolved_dependency_sites = 0usize;
    let mut ordinals = BTreeMap::new();
    for site in index.syntax.iter().filter(|s| s.kind == "property-write") {
        unfiltered_total += 1;
        let marker = index.markers.partition_point(|m| m.2 <= site.span.start);
        if marker == 0 {
            return Err(error("objects site has unknown PC"));
        }
        let (pc, _, body_start) = index.markers[marker - 1];
        let end = index
            .markers
            .get(marker)
            .map_or(source.len() as u32, |m| m.1);
        if site.span.end > end {
            return Err(error("objects site crosses PC boundary"));
        }
        let ordinal = ordinals.entry(pc).or_insert(0usize);
        let this_ordinal = *ordinal;
        *ordinal += 1;
        let trace = object_trace(&index, site, o.alias_depth, &mut budget)?;
        unresolved_origins +=
            usize::from(trace.status != "resolved" && trace.status != "non_register_expression");
        alias_truncated += usize::from(trace.alias_depth_truncated);
        non_register += usize::from(trace.status == "non_register_expression");
        if origin_pc.is_some_and(|wanted| {
            trace
                .terminal
                .is_none_or(|id| index.definitions[id].pc != wanted)
        }) {
            continue;
        }
        let matches = matcher
            .as_ref()
            .map(|m| {
                matching_dependencies(
                    source,
                    &index,
                    site,
                    o.depth,
                    m,
                    &definition_matches,
                    Some(&mut budget),
                )
            })
            .transpose()?;
        if let Some(m) = &matches {
            dependency_search_truncated_sites += usize::from(m.truncated);
            unresolved_dependency_sites += usize::from(m.unresolved);
            if !m.direct && m.definitions.is_empty() {
                continue;
            }
        }
        let store_ordinal = unfiltered_total - 1;
        if store_ordinal >= o.offset && selected.len() == o.limit && next_offset.is_none() {
            next_offset = Some(store_ordinal);
        }
        if store_ordinal >= o.offset && selected.len() < o.limit {
            if let Some(m) = &matches {
                if m.direct {
                    budget.filter_charge(site.span)?;
                }
                for &id in m.definitions.iter().take(8 - usize::from(m.direct)) {
                    budget.filter_charge(index.definitions[id].span)?;
                }
            }
            let selected_site = Site {
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
                    .map(|s| object_operand(source, &index, s, o.depth, &mut budget))
                    .collect::<DecompilerResult<_>>()?,
                operand_count: site.operands.len(),
                operands_truncated: site.operands.len() > OPERAND_CAP,
                slot: site.slot,
                source_matches: matches.as_ref().map(|m| {
                    source_matches(source, &index, site, pc, matcher.as_ref().unwrap(), m)
                }),
            };
            construction_bytes += serde_json::to_vec(&selected_site)
                .map_err(|e| error(e.to_string()))?
                .len();
            if construction_bytes > 16_777_216 {
                return Err(error(
                    "objects pre-compaction construction byte cap exceeded before stdout",
                ));
            }
            selected.push(selected_site);
            traces.push(trace);
        }
        total += 1;
    }
    let follow_up_queries = follow_up_queries(&selected);
    let (mut sites, mut definitions) = compact_sites(selected)?;
    let mut query_pcs = BTreeSet::new();
    for (site, trace) in sites.as_array_mut().unwrap().iter_mut().zip(traces) {
        let mut intern = |id: usize| {
            let d = &index.definitions[id];
            intern_definition(
                &mut definitions,
                function,
                d.pc,
                [d.span.start, d.span.end],
                d.register,
                snippet(source, d.span),
            )
        };
        let terminal = trace.terminal.map(|id| {
            query_pcs.insert(index.definitions[id].pc);
            intern(id)
        });
        let aliases = trace.aliases.into_iter().map(intern).collect::<Vec<_>>();
        site["object_origin"] = serde_json::json!({"status":trace.status,"register":trace.register,"terminal_definition":terminal,"alias_definition_ids":aliases,"alias_depth_truncated":trace.alias_depth_truncated});
    }
    let object_queries = query_pcs.into_iter().map(|pc| {
        budget.charge()?;
        let start = index.definitions.partition_point(|d| d.pc < pc);
        let end = index.definitions.partition_point(|d| d.pc <= pc);
        let mut ids = Vec::new();
        for d in index.definitions[start..end].iter().take(64) {
            budget.charge()?;
            ids.push(format!("{function}:{}:{}",d.span.start,d.span.end));
        }
        Ok(serde_json::json!({"command":"objects","function":function,"origin_pc":pc,
            "origin_scope":"pc_wide_definition_union","terminal_definition_ids":ids,
            "source_definition_count":end-start,"definition_ids_truncated":end-start>64,
            "flags":object_flags(o,0,false),"input_scope":"same_input_hbc",
            "semantics":"Inspect source writes rooted at any definition at this PC. Match terms and offset deliberately cleared. PC-wide source-definition IDs remain distinct and may include definitions not selected as origins; this union is not one object or runtime heap identity."}))
    }).collect::<DecompilerResult<Vec<_>>>()?;
    let continuation_query=next_offset.map(|offset|serde_json::json!({"command":"objects","function":function,"origin_pc":origin_pc,"flags":object_flags(o,offset,true),"input_scope":"same_input_hbc","semantics":"Continue matched source-record paging with the original input and chosen binary. Argument tokens are data, never shell code."}));
    let report = serde_json::json!({"schema":"objects-v1","schema_version":1,"source":"export_function_fragments","parsed_source_complete":true,"function":function,"origin_pc":origin_pc,"format":"compact",
        "origin_scope":"pc_wide_definition_union","semantics":"Source-local property writes grouped by terminal prior register-definition PC through plain register copies. Anchors union distinct definitions at that PC, not one object. Not runtime objects, allocation identity, evaluated fields, constructor semantics, execution order or frame identity. No JS executes.",
        "alias_policy":"Only simple plain r[N] = r[M] copies. Member accesses, calls, constructor arguments and arbitrary expression dependencies are not aliases. Intra-PC/conditional writes and cross-block/exception entry remain unresolved.",
        "negative_result_policy":"Unresolved or omitted origins/dependencies cannot prove runtime absence. Discovery covers bounded source candidates; sibling mode is not a complete heap or field-value reconstruction.",
        "depth":o.depth,"alias_depth":o.alias_depth,"limit":o.limit,"offset":o.offset,"offset_scope":"Raw property-store ordinal in this function, before origin or source filters", "matches":o.matches,"total":total,"unfiltered_total":unfiltered_total,"next_offset":next_offset,"scan_complete":next_offset.is_none(),
        "work_used":budget.used,"work_cap":budget.cap,"filter_work_used":budget.filter_used,"filter_work_cap":OBJECT_FILTER_CAP,
        "origin_summary":{"scope":"All eligible property-write sites, including source-filtered-out uncertainty","unresolved_origins":unresolved_origins,"alias_depth_truncated":alias_truncated,"non_register_expressions":non_register},
        "filter_summary":{"dependency_search_truncated_sites":dependency_search_truncated_sites,"unresolved_dependency_sites":unresolved_dependency_sites,"match_policy":"OR escaped case-insensitive raw site/local candidate syntax, not decoded names or values; source search depth is bounded."},
        "construction_bytes":construction_bytes,"limits":{"source_bytes":OBJECT_SOURCE_CAP,"output_bytes":16_777_216,"pre_compaction_bytes":16_777_216,"syntax_work":OBJECT_SYNTAX_CAP,"syntax_depth":256,"instructions_or_writes":1_048_576,"reads":2_097_152,"nodes_per_operand":NODE_CAP,"operands_per_record":OPERAND_CAP,"snippet_bytes":PREVIEW},
        "definitions":definitions,"sites":sites,"object_queries":object_queries,"follow_up_queries":follow_up_queries,"continuation_query":continuation_query});
    let mut bytes = serde_json::to_vec(&report).map_err(|e| error(e.to_string()))?;
    bytes.push(b'\n');
    if bytes.len() > o.max_bytes {
        return Err(error("objects output byte budget exceeded before stdout"));
    }
    Ok(bytes)
}

pub fn report_objects(
    input: &Path,
    function: u32,
    origin_pc: Option<u32>,
    options: &ObjectOptions,
) -> DecompilerResult<Vec<u8>> {
    object_options(options)?;
    use std::io::Read;
    let mut data = Vec::new();
    std::fs::File::open(input)?
        .take(134_217_729)
        .read_to_end(&mut data)?;
    if data.len() > 134_217_728 {
        return Err(error("objects HBC input byte cap exceeded before parsing"));
    }
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let header = hbc
        .functions
        .get_parsed_header(function)
        .ok_or_else(|| error(format!("Unknown function {function}")))?;
    let exceptions = header
        .exc_handlers
        .iter()
        .flat_map(|h| [h.start, h.end, h.target])
        .collect();
    let source = crate::bundle::export_function_fragment_bounded(
        &hbc,
        function,
        OBJECT_SOURCE_CAP,
        OBJECT_SOURCE_CAP,
    )?;
    analyze_objects_source(&source, function, origin_pc, &exceptions, options)
}

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
            let matches = matcher
                .as_ref()
                .map(|m| {
                    matching_dependencies(source, &index, site, depth, m, &definition_matches, None)
                })
                .transpose()?;
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
    let follow_up_queries = follow_up_queries(&sites);
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
        "total": total, "next_offset": next_offset, "sites": sites,
        "follow_up_queries": follow_up_queries
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
