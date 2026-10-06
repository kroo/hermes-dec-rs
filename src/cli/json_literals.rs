//! Bounded embedded-JSON literal evidence. This never evaluates JavaScript.
use crate::{bundle::export_function_fragment_bounded, DecompilerError, DecompilerResult, HbcFile};
use oxc_ast::ast::*;
use oxc_ast_visit::{walk, Visit};
use oxc_span::Span;
use regex::{Regex, RegexBuilder};
use serde::de::{Error as _, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use serde_json::{json, Value};
use std::fmt;
use std::io::{Read, Write};
use std::path::Path;

const SOURCE_CAP: usize = 64 * 1024 * 1024;
const INPUT_CAP: usize = 128 * 1024 * 1024;
const OUTPUT_CAP: usize = 16 * 1024 * 1024;
const LITERAL_CAP: usize = 1024 * 1024;
const FILTER_CAP: usize = 64 * 1024 * 1024;
const WORK_CAP: usize = 128 * 1024 * 1024;
const DEPTH_CAP: usize = 128;
const PREVIEW_CAP: usize = 256;
const DUPLICATE_KEY_ERROR: &str = "json-literals duplicate decoded object key";

// Deserialize through serde_json's standard number/recursion guards, but never
// overwrite an existing object member. The caller bounds the decoded document
// before deserialization; avoid reserving from arbitrary container size hints.
struct UniqueValue(Value);

impl<'de> Deserialize<'de> for UniqueValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct UniqueVisitor;

        impl<'de> Visitor<'de> for UniqueVisitor {
            type Value = UniqueValue;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a JSON value with unique decoded keys in each object")
            }

            fn visit_bool<E: serde::de::Error>(self, value: bool) -> Result<Self::Value, E> {
                Ok(UniqueValue(Value::Bool(value)))
            }

            fn visit_i64<E: serde::de::Error>(self, value: i64) -> Result<Self::Value, E> {
                Ok(UniqueValue(Value::Number(value.into())))
            }

            fn visit_u64<E: serde::de::Error>(self, value: u64) -> Result<Self::Value, E> {
                Ok(UniqueValue(Value::Number(value.into())))
            }

            fn visit_f64<E: serde::de::Error>(self, value: f64) -> Result<Self::Value, E> {
                serde_json::Number::from_f64(value)
                    .map(|number| UniqueValue(Value::Number(number)))
                    .ok_or_else(|| E::custom("non-finite JSON number"))
            }

            fn visit_str<E: serde::de::Error>(self, value: &str) -> Result<Self::Value, E> {
                self.visit_string(value.to_owned())
            }

            fn visit_string<E: serde::de::Error>(self, value: String) -> Result<Self::Value, E> {
                Ok(UniqueValue(Value::String(value)))
            }

            fn visit_unit<E: serde::de::Error>(self) -> Result<Self::Value, E> {
                Ok(UniqueValue(Value::Null))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut array: A) -> Result<Self::Value, A::Error> {
                let mut values = Vec::new();
                while let Some(UniqueValue(value)) = array.next_element::<UniqueValue>()? {
                    values.push(value);
                }
                Ok(UniqueValue(Value::Array(values)))
            }

            fn visit_map<A: MapAccess<'de>>(self, mut object: A) -> Result<Self::Value, A::Error> {
                let mut values = serde_json::Map::new();
                while let Some(key) = object.next_key::<String>()? {
                    if values.contains_key(&key) {
                        return Err(A::Error::custom(DUPLICATE_KEY_ERROR));
                    }
                    let UniqueValue(value) = object.next_value::<UniqueValue>()?;
                    values.insert(key, value);
                }
                Ok(UniqueValue(Value::Object(values)))
            }
        }

        deserializer.deserialize_any(UniqueVisitor)
    }
}

#[derive(Clone, Debug)]
/// Defaults: 32 results, raw offset 0, 1 MiB output, 64 MiB scan work,
/// no filters, and the entire JSON document. Hard bounds are reported in output.
pub struct Options {
    pub matches: Vec<String>,
    pub pc: Option<u32>,
    pub pointer: Option<String>,
    pub limit: usize,
    pub offset: usize,
    pub max_bytes: usize,
    pub scan_work: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            matches: Vec::new(),
            pc: None,
            pointer: None,
            limit: 32,
            offset: 0,
            max_bytes: 1024 * 1024,
            scan_work: 64 * 1024 * 1024,
        }
    }
}

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::internal(message.into())
}

fn pointer_tokens(pointer: &str) -> DecompilerResult<Vec<String>> {
    if pointer.len() > 1024 || (!pointer.is_empty() && !pointer.starts_with('/')) {
        return Err(error(
            "pointer must be an RFC6901 pointer of at most 1024 bytes",
        ));
    }
    if pointer.is_empty() {
        return Ok(Vec::new());
    }
    pointer[1..]
        .split('/')
        .map(|token| {
            let mut result = String::new();
            let mut chars = token.chars();
            while let Some(c) = chars.next() {
                result.push(if c == '~' {
                    match chars.next() {
                        Some('0') => '~',
                        Some('1') => '/',
                        _ => return Err(error("invalid RFC6901 escape")),
                    }
                } else {
                    c
                });
            }
            Ok(result)
        })
        .collect()
}

fn validate(options: &Options) -> DecompilerResult<(Vec<String>, Option<Regex>)> {
    if !(1..=1000).contains(&options.limit)
        || !(1..=OUTPUT_CAP).contains(&options.max_bytes)
        || !(1..=WORK_CAP).contains(&options.scan_work)
    {
        return Err(error(
            "json-literals bounds: limit 1..1000, max_bytes 1..16777216, scan_work 1..134217728",
        ));
    }
    if options.matches.len() > 64
        || options
            .matches
            .iter()
            .any(|s| s.is_empty() || s.len() > 1024)
    {
        return Err(error(
            "at most 64 nonempty matches of at most 1024 bytes each",
        ));
    }
    let tokens = pointer_tokens(options.pointer.as_deref().unwrap_or(""))?;
    let matcher = if options.matches.is_empty() {
        None
    } else {
        Some(
            RegexBuilder::new(
                &options
                    .matches
                    .iter()
                    .map(|s| regex::escape(s))
                    .collect::<Vec<_>>()
                    .join("|"),
            )
            .case_insensitive(true)
            .size_limit(8 * 1024 * 1024)
            .build()
            .map_err(|e| error(format!("literal match compilation: {e}")))?,
        )
    };
    Ok((tokens, matcher))
}

struct Budget {
    used: usize,
    cap: usize,
}
impl Budget {
    fn charge(&mut self, amount: usize) -> DecompilerResult<()> {
        if amount > self.cap.saturating_sub(self.used) {
            return Err(error(
                "json-literals scan_work budget exceeded; no partial inspection",
            ));
        }
        self.used += amount;
        Ok(())
    }
}

/// Conservative pre-parser guard for exporter syntax, not a second JS parser.
/// Reject templates/regex syntax conservatively rather than allowing them to
/// hide recursive syntax. Division after a computed register/member operand
/// is unambiguous and covers exporter division. Other division forms are
/// conservatively refused. The real parser is authoritative for literals.
fn preflight(source: &str, budget: &mut Budget) -> DecompilerResult<()> {
    if source.len() > SOURCE_CAP {
        return Err(error("json-literals source exceeds 64 MiB"));
    }
    budget.charge(source.len())?;
    let b = source.as_bytes();
    let mut i = 0;
    let mut depth = 0usize;
    let mut run = 0usize;
    let mut expression_bytes = 0usize;
    let mut bracket_operand = false;
    while i < b.len() {
        // A conservative expression-window cap also catches unary/binary
        // chains spread over many shallow parenthesized subexpressions before
        // the parser can allocate or recurse. Literal/comment contents do not
        // contribute to this window.
        expression_bytes += 1;
        if expression_bytes > 512 {
            return Err(error("source expression window budget exceeded"));
        }
        match b[i] {
            b'\'' | b'"' => {
                let start = i;
                let quote = b[i];
                i += 1;
                bracket_operand = false;
                while i < b.len() && b[i] != quote {
                    if b[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                    if i - start > LITERAL_CAP {
                        return Err(error("raw JS string exceeds 1 MiB pre-parser cap"));
                    }
                }
                if i - start + 1 > LITERAL_CAP {
                    return Err(error("raw JS string exceeds 1 MiB pre-parser cap"));
                }
                i += 1;
            }
            b'/' if b.get(i + 1) == Some(&b'/') => {
                i += 2;
                while i < b.len()
                    && !matches!(b[i], b'\n' | b'\r')
                    && !b[i..].starts_with(b"\xe2\x80\xa8")
                    && !b[i..].starts_with(b"\xe2\x80\xa9")
                {
                    i += 1;
                }
            }
            b'/' if b.get(i + 1) == Some(&b'*') => {
                i += 2;
                while i + 1 < b.len() && &b[i..i + 2] != b"*/" {
                    i += 1;
                }
                i += 2;
            }
            b'/' if bracket_operand => {
                i += 1;
                bracket_operand = false;
            }
            b'`' | b'/' => {
                return Err(error(
                    "unsupported exporter template/regex/non-member-division syntax",
                ))
            }
            b'(' | b'[' | b'{' => {
                depth += 1;
                if depth > DEPTH_CAP {
                    return Err(error("source nesting depth budget exceeded"));
                }
                run = 0;
                bracket_operand = false;
                i += 1;
            }
            b')' | b']' | b'}' => {
                depth = depth.saturating_sub(1);
                run = 0;
                bracket_operand = b[i] == b']';
                i += 1;
            }
            b';' | b',' => {
                expression_bytes = 0;
                run = 0;
                bracket_operand = false;
                i += 1;
            }
            b'\xe2'
                if b[i..].starts_with(b"\xe2\x80\xa8") || b[i..].starts_with(b"\xe2\x80\xa9") =>
            {
                i += 3;
            }
            c if c.is_ascii_whitespace() => i += 1,
            _ => {
                bracket_operand = false;
                run += 1;
                if run > 256 {
                    return Err(error("source expression run budget exceeded"));
                }
                i += 1;
            }
        }
    }
    Ok(())
}

#[derive(Default)]
struct Syntax<'a> {
    literals: Vec<&'a StringLiteral<'a>>,
    nested: Vec<Span>,
    depth: usize,
    work: usize,
    cap: usize,
    failed: bool,
}
impl Syntax<'_> {
    fn enter(&mut self) -> bool {
        self.work += 1;
        if self.work > self.cap || self.depth >= DEPTH_CAP {
            self.failed = true;
            return false;
        }
        self.depth += 1;
        true
    }
}
impl<'a> Visit<'a> for Syntax<'a> {
    fn visit_expression(&mut self, it: &Expression<'a>) {
        if !self.enter() {
            return;
        }
        if let Expression::StringLiteral(literal) = it {
            // Allocator-backed AST nodes outlive this traversal.
            self.literals.push(self.alloc(literal));
        } else {
            walk::walk_expression(self, it);
        }
        self.depth -= 1;
    }
    fn visit_statement(&mut self, it: &Statement<'a>) {
        if !self.enter() {
            return;
        }
        walk::walk_statement(self, it);
        self.depth -= 1;
    }
    fn visit_function(&mut self, it: &Function<'a>, _: oxc_syntax::scope::ScopeFlags) {
        self.nested.push(it.span);
    }
    fn visit_arrow_function_expression(&mut self, it: &ArrowFunctionExpression<'a>) {
        self.nested.push(it.span);
    }
    fn visit_class(&mut self, it: &Class<'a>) {
        self.nested.push(it.span);
    }
}

fn root<'a>(program: &'a Program<'a>, function: u32) -> DecompilerResult<&'a FunctionBody<'a>> {
    let mut body = None;
    for statement in &program.body {
        let Statement::ExpressionStatement(statement) = statement else {
            continue;
        };
        let Expression::AssignmentExpression(assignment) = &statement.expression else {
            continue;
        };
        let AssignmentTarget::ComputedMemberExpression(member) = &assignment.left else {
            continue;
        };
        if !matches!(&member.object, Expression::Identifier(id) if id.name == "F") {
            continue;
        }
        if !assignment.operator.is_assign()
            || !matches!(&member.expression, Expression::NumericLiteral(id) if id.value == f64::from(function))
        {
            return Err(error("wrong-function exporter root"));
        }
        let Expression::FunctionExpression(fun) = &assignment.right else {
            return Err(error("exporter root must be a literal function expression"));
        };
        if body.is_some() {
            return Err(error("multiple exporter roots"));
        }
        body = fun.body.as_ref();
    }
    body.map(|b| &**b)
        .ok_or_else(|| error("requires one F[function] exporter root"))
}

fn select<'a>(mut value: &'a Value, tokens: &[String]) -> Option<&'a Value> {
    for token in tokens {
        value = match value {
            Value::Object(object) => object.get(token)?,
            Value::Array(array) => {
                if token.is_empty()
                    || (token.len() > 1 && token.starts_with('0'))
                    || !token.bytes().all(|c| c.is_ascii_digit())
                {
                    return None;
                }
                array.get(token.parse::<usize>().ok()?)?
            }
            _ => return None,
        };
    }
    Some(value)
}

// Compare decimal values without expanding exponents or using floating-point
// arithmetic. Preserve negative zero as distinct evidence. The JSON parser has
// already validated grammar and bounded the document before this check.
fn decimal_form(token: &str) -> Option<(bool, String, i64)> {
    let negative = token.starts_with('-');
    let token = token.strip_prefix('-').unwrap_or(token);
    let (mantissa, exponent) = token
        .split_once(['e', 'E'])
        .map_or(Some((token, 0)), |(m, e)| Some((m, e.parse::<i64>().ok()?)))?;
    let fraction = mantissa.split_once('.').map_or(0, |(_, f)| f.len());
    let digits: String = mantissa.chars().filter(|&c| c != '.').collect();
    let digits = digits.trim_start_matches('0');
    if digits.is_empty() {
        return Some((negative, "0".into(), 0));
    }
    let significant = digits.trim_end_matches('0');
    let scale = exponent
        .checked_sub(i64::try_from(fraction).ok()?)?
        .checked_add(i64::try_from(digits.len() - significant.len()).ok()?)?;
    Some((negative, significant.into(), scale))
}

fn numbers_preserved(document: &str, budget: &mut Budget) -> DecompilerResult<bool> {
    budget.charge(document.len())?;
    let bytes = document.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'"' {
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                i += if bytes[i] == b'\\' { 2 } else { 1 };
            }
            i += 1;
        } else if bytes[i] == b'-' || bytes[i].is_ascii_digit() {
            let start = i;
            while i < bytes.len()
                && matches!(bytes[i], b'0'..=b'9' | b'-' | b'+' | b'.' | b'e' | b'E')
            {
                i += 1;
            }
            let token = &document[start..i];
            let Ok(number) = token.parse::<serde_json::Number>() else {
                return Ok(false);
            };
            let original = decimal_form(token);
            if original.is_none() || original != decimal_form(&number.to_string()) {
                return Ok(false);
            }
        } else {
            i += 1;
        }
    }
    Ok(true)
}

fn preview(source: &str) -> &str {
    let mut end = source.len().min(PREVIEW_CAP);
    while !source.is_char_boundary(end) {
        end -= 1;
    }
    &source[..end]
}

struct Output {
    bytes: Vec<u8>,
    cap: usize,
}
impl Write for Output {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        if bytes.len() > self.cap.saturating_sub(self.bytes.len()) {
            return Err(std::io::Error::other(
                "json-literals output byte budget exceeded",
            ));
        }
        self.bytes.extend_from_slice(bytes);
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn serialize(value: &Value, cap: usize) -> DecompilerResult<Vec<u8>> {
    let mut output = Output {
        bytes: Vec::new(),
        cap,
    };
    serde_json::to_writer(&mut output, value).map_err(|e| error(e.to_string()))?;
    Ok(output.bytes)
}

fn continuation_flags(options: &Options, offset: usize) -> Vec<String> {
    let mut flags = vec![
        "--limit".into(),
        options.limit.to_string(),
        "--offset".into(),
        offset.to_string(),
        "--max-bytes".into(),
        options.max_bytes.to_string(),
        "--scan-work".into(),
        options.scan_work.to_string(),
    ];
    if let Some(pc) = options.pc {
        flags.extend(["--pc".into(), pc.to_string()]);
    }
    if let Some(pointer) = &options.pointer {
        flags.push(format!("--pointer={pointer}"));
    }
    // Equals-form keeps leading hyphens, empty pointer values and whitespace
    // lossless without shell quoting or depending on allow_hyphen_values.
    flags.extend(options.matches.iter().map(|s| format!("--match={s}")));
    flags
}

/// Inspect literal syntax in one raw exporter fragment. Pagination uses ALL
/// root-body string-expression ordinals, before JSON/PC/match/pointer filters.
/// Limits bound expansion/work, not total memory or elapsed execution time.
pub fn analyze_source(source: &str, function: u32, options: &Options) -> DecompilerResult<Vec<u8>> {
    analyze(source, function, options, None)
}

fn analyze(
    source: &str,
    function: u32,
    options: &Options,
    input: Option<&str>,
) -> DecompilerResult<Vec<u8>> {
    let (tokens, matcher) = validate(options)?;
    let mut budget = Budget {
        used: 0,
        cap: options.scan_work,
    };
    preflight(source, &mut budget)?;
    let allocator = oxc_allocator::Allocator::default();
    let parsed =
        oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
    if !parsed.errors.is_empty() || parsed.panicked {
        return Err(error("requires valid complete exporter JavaScript"));
    }
    let body = root(&parsed.program, function)?;
    let mut syntax = Syntax {
        cap: budget.cap.saturating_sub(budget.used),
        ..Syntax::default()
    };
    syntax.visit_function_body(body);
    if syntax.failed {
        return Err(error(
            "AST work/depth budget exceeded; no partial inspection",
        ));
    }
    budget.charge(syntax.work)?;
    let mut anchors = Vec::new();
    for comment in &parsed.program.comments {
        budget.charge(1)?;
        let raw = &source[comment.span.start as usize..comment.span.end as usize];
        let Some(marker) = raw.strip_prefix("// HBC function ") else {
            continue;
        };
        // Excluded function/class scopes own their comments, including IDs
        // and malformed markers that are not root provenance.
        budget.charge(syntax.nested.len())?;
        if syntax
            .nested
            .iter()
            .any(|s| s.start <= comment.span.start && comment.span.end <= s.end)
        {
            continue;
        }
        let line_prefix = source[..comment.span.start as usize]
            .rsplit(['\n', '\r', '\u{2028}', '\u{2029}'])
            .next()
            .unwrap_or("");
        budget.charge(line_prefix.len())?;
        if !line_prefix.bytes().all(|c| matches!(c, b' ' | b'\t')) {
            return Err(error(
                "root PC marker must be line-anchored, not a trailing comment",
            ));
        }
        let Some((id, pc)) = marker.trim().split_once(", PC ") else {
            return Err(error("malformed PC marker"));
        };
        if id.is_empty()
            || !id.bytes().all(|c| c.is_ascii_digit())
            || id.parse::<u32>().ok() != Some(function)
        {
            return Err(error("wrong-function PC marker"));
        }
        if pc.is_empty() || !pc.bytes().all(|c| c.is_ascii_digit()) {
            return Err(error("malformed PC marker"));
        }
        let pc = pc
            .parse::<u32>()
            .map_err(|_| error("malformed PC marker"))?;
        if comment.span.start < body.span.start || comment.span.end > body.span.end {
            return Err(error("PC marker outside exporter root"));
        }
        if anchors.last().is_some_and(|&(last, _, _)| last >= pc) {
            return Err(error("unordered or duplicate PC markers"));
        }
        anchors.push((pc, comment.span.start, comment.span.end));
    }
    if anchors.is_empty() {
        return Err(error("missing PC markers"));
    }
    if let Some(pc) = options.pc {
        budget.charge(anchors.len())?;
        if !anchors.iter().any(|anchor| anchor.0 == pc) {
            return Err(error(format!(
                "unknown root PC {pc} for function {function}"
            )));
        }
    }
    let total = syntax.literals.len();
    let sort_levels = usize::BITS - total.leading_zeros();
    budget.charge(total.saturating_mul(sort_levels as usize))?;
    syntax.literals.sort_by_key(|l| l.span.start);
    let mut items = Vec::new();
    let mut malformed = 0;
    let mut duplicate_keys = 0;
    let mut lossy_numbers = 0;
    let mut code_units = 0;
    let mut non_documents = 0;
    let mut filtered = 0;
    let mut pointer_missing = 0;
    let mut documents = 0;
    let mut eligible = 0usize;
    let mut filter_bytes = 0usize;
    let mut next = None;
    let mut anchor_index = 0;
    let mut retained_value_bytes = 0usize;
    for (ordinal, literal) in syntax.literals.iter().enumerate() {
        budget.charge(1)?;
        while anchor_index + 1 < anchors.len() && anchors[anchor_index + 1].1 < literal.span.start {
            anchor_index += 1;
        }
        let (pc, _, end) = anchors[anchor_index];
        if literal.span.start < end
            || anchors
                .get(anchor_index + 1)
                .is_some_and(|a| literal.span.end > a.1)
        {
            return Err(error("unanchored or cross-PC literal"));
        }
        if literal.lone_surrogates {
            code_units += 1;
            continue;
        }
        let decoded = literal.value.as_str();
        if decoded.len() > LITERAL_CAP {
            return Err(error("decoded literal exceeds 1 MiB"));
        }
        budget.charge(decoded.len())?;
        let trimmed = decoded.trim_start();
        if !trimmed.starts_with(['{', '[']) {
            non_documents += 1;
            continue;
        }
        // serde_json has a recursion guard; a depth-limit error is a budget
        // failure, not a silently omitted malformed document.
        let value = match serde_json::from_str::<UniqueValue>(decoded) {
            Ok(UniqueValue(value)) => value,
            Err(e) if e.to_string().contains("recursion limit exceeded") => {
                return Err(error("JSON depth budget exceeded"));
            }
            Err(e) if e.to_string().starts_with(DUPLICATE_KEY_ERROR) => {
                duplicate_keys += 1;
                continue;
            }
            Err(_) => {
                malformed += 1;
                continue;
            }
        };
        if !numbers_preserved(decoded, &mut budget)? {
            lossy_numbers += 1;
            continue;
        }
        documents += 1;
        if ordinal < options.offset {
            continue;
        }
        if options.pc.is_some_and(|wanted| wanted != pc) {
            filtered += 1;
            continue;
        }
        if let Some(matcher) = &matcher {
            let cost = decoded.len().saturating_mul(options.matches.len());
            if cost > FILTER_CAP.saturating_sub(filter_bytes) {
                return Err(error("aggregate decoded filter byte budget exceeded"));
            }
            filter_bytes += cost;
            budget.charge(cost)?;
            if !matcher.is_match(decoded) {
                filtered += 1;
                continue;
            }
        }
        let Some(selected) = select(&value, &tokens) else {
            pointer_missing += 1;
            continue;
        };
        eligible += 1;
        if items.len() == options.limit {
            next.get_or_insert(ordinal);
            continue;
        }
        let raw = &source[literal.span.start as usize..literal.span.end as usize];
        // Preflight the complete selected value before constructing its report
        // copy. The final bounded writer also includes all metadata overhead.
        let selected_bytes = serialize(
            selected,
            options.max_bytes.saturating_sub(retained_value_bytes),
        )?;
        retained_value_bytes += selected_bytes.len();
        items.push(json!({
            "literal_id": ordinal, "function": function, "pc": pc,
            "source_span": {"start":literal.span.start,"end":literal.span.end},
            "raw_source_preview": preview(raw), "raw_source_bytes":raw.len(),
            "raw_source_preview_omitted_bytes":raw.len()-preview(raw).len(),
            "decoded_json_bytes":decoded.len(), "pointer":options.pointer,
            "selected_value":selected, "evidence":"embedded_json_literal"
        }));
    }
    let continuation = next.map(|offset| json!({
        "command":"json-literals","flags":continuation_flags(options,offset),
        "input_scope":"same_input_hbc",
        "cli_policy":"Pass command, input, function, then flags as separate argument tokens, never shell code. Reuse the original input HBC and binary. For analyze_source, the caller must supply the corresponding original input.",
        "api":if input.is_some() {"report"} else {"analyze_source"},
        "input":input,"function":function,
        "source_scope":"same complete raw exporter fragment",
        "options":{"matches":options.matches,"pc":options.pc,"pointer":options.pointer,
            "limit":options.limit,"offset":offset,"max_bytes":options.max_bytes,"scan_work":options.scan_work}
    }));
    serialize(
        &json!({
            "schema":"json-literals-v1","schema_version":1,"source":"export_function_fragment_bounded",
            "function":function,"input":input,"evidence":"embedded_json_literal",
            "semantics":"Literal evidence only, NOT runtime values or schema reachability. No JavaScript, calls, constructors or framework code is executed. Negative results do not establish absence of decoded runtime values.",
            "source_basis":{"offset_unit":"utf8_bytes","offset_origin":"complete_raw_exporter_fragment_without_inspection_header",
                "source_bytes":source.len(),"raw_fragment_prefix":preview(source),"root_body_span":{"start":body.span.start,"end":body.span.end}},
            "parsed_source_complete":true,"root_literal_scan_complete":true,
            "pagination":"raw root string-expression ordinal before filters",
            "match_policy":"OR escaped case-insensitive substring match on complete decoded JSON literal text",
            "pointer_policy":"strict RFC6901; array indices canonical decimal only; not a JS path",
            "omission_policy":"Non-object/array strings, malformed JSON, duplicate-key JSON documents, lossy-number JSON documents, and JS lone-surrogate code units are omitted. Nested functions/classes are excluded, not assigned root PCs. JSON lone surrogates rejected by serde_json are malformed JSON. No selected value is truncated.",
            "number_policy":"Omit the entire document if any numeric token changes its exact decimal value or negative-zero sign when re-serialized by serde_json. Compare normalized decimal coefficients and bounded i64 exponents, never floating-point equality. Original numeric spelling is only in the source literal; no JavaScript number conversion is implied.",
            "duplicate_key_policy":"Omit the entire document at the first duplicate decoded object key, recursively including objects inside arrays; never return a last-wins value. Escaped-equivalent keys collide. Separate sibling objects may share names. Count once per omitted document, separately from malformed_json. Parsing stops at the duplicate; remaining document syntax is not validated.",
            "counts":{"raw_literals":total,"json_documents":documents,"returned":items.len(),"eligible_at_or_after_offset":eligible,
                "non_documents":non_documents,"malformed_json":malformed,"duplicate_key_documents":duplicate_keys,"lossy_number_documents":lossy_numbers,"string_code_unit_omissions":code_units,
                "nested_scopes_excluded":syntax.nested.len(),"filtered":filtered,"pointer_missing":pointer_missing,
                "before_offset":options.offset.min(total),"pagination_omitted":eligible.saturating_sub(items.len())},
            "limits":{"hbc_bytes":INPUT_CAP,"source_bytes":SOURCE_CAP,"string_table_bytes":SOURCE_CAP,
                "raw_literal_bytes":LITERAL_CAP,"decoded_literal_bytes":LITERAL_CAP,"source_depth":DEPTH_CAP,
                "source_expression_window_bytes":512,"source_expression_run_bytes":256,
                "json_depth":"serde_json recursion guard (128)","aggregate_filter_bytes":FILTER_CAP,
                "output_bytes":options.max_bytes,"scan_work":options.scan_work,
                "policy":"Expansion/work budgets, not a total-memory or elapsed-time sandbox; conservative syntax guards may refuse valid non-exporter JS."},
            "work_used":budget.used,"filter_bytes_used":filter_bytes,"offset":options.offset,
            "next_offset":next,"continuation":continuation,"literals":items
        }),
        options.max_bytes,
    )
}

/// Validate options before opening input, bound the read even if the file grows,
/// and bound exporter source plus conversion of the entire HBC string table.
pub fn report(input: &Path, function: u32, options: &Options) -> DecompilerResult<Vec<u8>> {
    validate(options)?;
    let input_name = input
        .to_str()
        .ok_or_else(|| error("input path must be UTF-8 for lossless continuation"))?;
    let file = std::fs::File::open(input)?;
    if file.metadata()?.len() > INPUT_CAP as u64 {
        return Err(error("HBC input exceeds 128 MiB"));
    }
    let mut data = Vec::new();
    file.take(INPUT_CAP as u64 + 1).read_to_end(&mut data)?;
    if data.len() > INPUT_CAP {
        return Err(error("HBC input exceeds 128 MiB"));
    }
    let hbc = HbcFile::parse_for_bundle(&data).map_err(error)?;
    let source = export_function_fragment_bounded(&hbc, function, SOURCE_CAP, SOURCE_CAP)?;
    analyze(&source, function, options, Some(input_name))
}

/// Report failures are atomic before stdout. A subsequent stdout I/O failure
/// itself cannot be made transactional for an arbitrary pipe.
pub fn run(input: &Path, function: u32, options: &Options) -> DecompilerResult<()> {
    let bytes = report(input, function, options)?;
    std::io::stdout().lock().write_all(&bytes)?;
    Ok(())
}
