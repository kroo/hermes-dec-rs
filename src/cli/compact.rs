//! Byte-preserving, non-executable inspection text for raw function fragments.

use crate::{DecompilerError, DecompilerResult};
use oxc_allocator::Allocator;
use oxc_ast::ast::{Statement, StringLiteral, TemplateLiteral};
use oxc_ast_visit::{walk, Visit};
use oxc_parser::Parser;
use oxc_span::{GetSpan, SourceType, Span};
use std::collections::BTreeMap;
use std::fmt::Write;

const MAX_SOURCE_BYTES: usize = 64 * 1024 * 1024;
const MAX_OUTPUT_BYTES: usize = 128 * 1024 * 1024;
const MARKER_PREFIX: &str = "// HBC function";
const MAX_NOTE_BYTES: usize = 65536;

struct Marker<'a> {
    start: usize,
    end: usize,
    pc: u32,
    note: Option<&'a str>,
    instruction_end: usize,
    note_at: usize,
    statement_depth: Option<usize>,
    instruction_closed: bool,
}

struct InstructionSpans<'a, 'n> {
    markers: &'a mut [Marker<'n>],
    current: usize,
    depth: usize,
    literals: Vec<Span>,
    inside_template: bool,
}

impl<'ast> Visit<'ast> for InstructionSpans<'_, '_> {
    fn visit_statement(&mut self, statement: &Statement<'ast>) {
        let span = statement.span();
        while self.current + 1 < self.markers.len()
            && self.markers[self.current + 1].start <= span.start as usize
        {
            self.current += 1;
        }
        if let Some(marker) = self.markers.get_mut(self.current) {
            if span.start as usize >= marker.end && !marker.instruction_closed {
                let depth = *marker.statement_depth.get_or_insert(self.depth);
                if self.depth < depth {
                    // Leaving the instruction's sibling list is structural
                    // exporter scaffolding, not part of the last instruction.
                    marker.instruction_closed = true;
                } else if self.depth == depth {
                    marker.instruction_end = marker.instruction_end.max(span.end as usize);
                }
            }
        }
        self.depth += 1;
        walk::walk_statement(self, statement);
        self.depth -= 1;
    }

    fn visit_string_literal(&mut self, literal: &StringLiteral<'ast>) {
        if !self.inside_template {
            self.literals.push(literal.span);
        }
    }

    fn visit_template_literal(&mut self, literal: &TemplateLiteral<'ast>) {
        // Protect the entire template, including interpolations, while still
        // finding instruction statements inside those expressions. Nested
        // literal spans are redundant and would break the ordered span sweep.
        let inside_template = self.inside_template;
        if !inside_template {
            self.literals.push(literal.span);
        }
        self.inside_template = true;
        walk::walk_template_literal(self, literal);
        self.inside_template = inside_template;
    }
}

fn after_line(source: &str, start: usize) -> usize {
    let bytes = source.as_bytes();
    let mut at = start;
    while at < bytes.len() {
        match bytes[at] {
            b'\r' => return at + 1 + usize::from(bytes.get(at + 1) == Some(&b'\n')),
            b'\n' => return at + 1,
            0xe2 if bytes.get(at + 1) == Some(&0x80)
                && matches!(bytes.get(at + 2), Some(0xa8 | 0xa9)) =>
            {
                return at + 3;
            }
            _ => at += 1,
        }
    }
    at
}

fn error(message: impl Into<String>) -> DecompilerError {
    DecompilerError::InvalidArgs {
        message: message.into(),
    }
}

fn decimal(value: &str) -> Option<u32> {
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    value.parse().ok()
}

fn marker(text: &str, function: u32) -> DecompilerResult<u32> {
    let (id, pc) = text
        .strip_prefix("// HBC function ")
        .and_then(|value| value.split_once(", PC "))
        .and_then(|(id, pc)| Some((decimal(id)?, decimal(pc)?)))
        .ok_or_else(|| error("Malformed HBC function/PC marker in compact source"))?;
    if id != function {
        return Err(error(format!(
            "HBC marker function {id} does not match requested function {function}"
        )));
    }
    Ok(pc)
}

fn line_start(source: &str, start: usize) -> bool {
    // Oxc recognizes all four JavaScript line terminators. Do not scan backward
    // through entire lines: long lines and many comments must stay linear.
    start == 0
        || matches!(source.as_bytes()[start - 1], b'\n' | b'\r')
        || source[..start].ends_with('\u{2028}')
        || source[..start].ends_with('\u{2029}')
}

/// Render complete raw exporter JavaScript without normalizing its source.
///
/// Only actual, column-zero parser line comments in the reserved HBC marker
/// namespace are replaced. Labels occupy the original marker's own line; no
/// continuation line is prefixed, including multiline literal payloads.
/// Missing markers are allowed. Marker IDs must match and PCs must increase.
pub fn render(source: &str, function: u32) -> DecompilerResult<String> {
    render_with_notes(source, function, &BTreeMap::new())
}

/// Attach opaque, parent-built source-local candidate notes, never runtime values.
pub fn render_with_notes(
    source: &str,
    function: u32,
    notes: &BTreeMap<u32, String>,
) -> DecompilerResult<String> {
    if source.len() > MAX_SOURCE_BYTES {
        return Err(error("Compact source exceeds the 64 MiB limit"));
    }
    let mut note_bytes = 0usize;
    for (pc, note) in notes {
        if note.len() > MAX_NOTE_BYTES || note.contains(['\r', '\n', '\u{2028}', '\u{2029}']) {
            return Err(error(
                "Compact notes must be single-line and at most 65536 bytes",
            ));
        }
        note_bytes = note_bytes
            .checked_add("# literal candidates @".len() + pc.to_string().len() + 2 + note.len() + 1)
            .filter(|size| *size <= MAX_OUTPUT_BYTES)
            .ok_or_else(|| error("Compact notes exceed the 128 MiB output limit"))?;
    }
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::default()).parse();
    if parsed.panicked || !parsed.errors.is_empty() {
        return Err(error(
            "Compact rendering requires valid complete JavaScript",
        ));
    }

    let header = format!(
        "Inspection-only; raw f{function}.js authoritative; @PC labels are function-local bytes, not text offsets; candidates are source-local, not substitutions or runtime values.\n"
    );
    let mut output_bytes = source.len() + header.len();
    let mut previous_pc = None;
    let mut previous_end = 0;
    let mut markers = Vec::new();
    let mut remaining_notes = notes.iter().peekable();
    // Comments are emitted in lexical source order by Oxc. Validate that
    // invariant rather than sorting or using a text-based marker search.
    for comment in &parsed.program.comments {
        let start = comment.span.start as usize;
        let end = comment.span.end as usize;
        if start < previous_end {
            return Err(error("Overlapping or unordered parser comment spans"));
        }
        previous_end = end;
        if !comment.is_line() || !line_start(source, start) {
            continue;
        }
        let text = &source[start..end];
        if !text.starts_with(MARKER_PREFIX) {
            continue;
        }
        let pc = marker(text, function)?;
        if previous_pc.is_some_and(|previous| previous >= pc) {
            return Err(error("Unordered or duplicate HBC PCs in compact source"));
        }
        previous_pc = Some(pc);
        if remaining_notes.peek().is_some_and(|(key, _)| **key < pc) {
            return Err(error("Compact note references an unknown marker PC"));
        }
        let note = if remaining_notes.peek().is_some_and(|(key, _)| **key == pc) {
            remaining_notes.next().map(|(_, note)| note.as_str())
        } else {
            None
        };
        markers.push(Marker {
            start,
            end,
            pc,
            note,
            instruction_end: end,
            note_at: end,
            statement_depth: None,
            instruction_closed: false,
        });
        // A u32 label is at most eleven bytes and always shorter than a marker.
        output_bytes = output_bytes - text.len() + 1 + pc.to_string().len();
    }
    if remaining_notes.next().is_some() {
        return Err(error("Compact note references an unknown marker PC"));
    }

    if !notes.is_empty() {
        let mut spans = InstructionSpans {
            markers: &mut markers,
            current: 0,
            depth: 0,
            literals: Vec::new(),
            inside_template: false,
        };
        spans.visit_program(&parsed.program);
        let literals = spans.literals;
        let mut literal_index = 0;
        let mut comment_index = 0;
        let mut boundary = 0;
        for marker in &mut markers {
            let Some(note) = marker.note else { continue };
            // Every scan advances: even many notes on one long physical line
            // do not rescan that line. Defer across a protected payload intact.
            if boundary <= marker.instruction_end {
                boundary = after_line(source, marker.instruction_end);
            }
            loop {
                while literals
                    .get(literal_index)
                    .is_some_and(|s| s.end as usize <= boundary)
                {
                    literal_index += 1;
                }
                while parsed
                    .program
                    .comments
                    .get(comment_index)
                    .is_some_and(|c| c.span.end as usize <= boundary)
                {
                    comment_index += 1;
                }
                let literal_end = literals
                    .get(literal_index)
                    .filter(|s| (s.start as usize) < boundary && boundary < s.end as usize)
                    .map_or(boundary, |s| s.end as usize);
                let comment_end = parsed
                    .program
                    .comments
                    .get(comment_index)
                    .filter(|c| {
                        (c.span.start as usize) < boundary && boundary < c.span.end as usize
                    })
                    .map_or(boundary, |c| c.span.end as usize);
                let protected_end = literal_end.max(comment_end);
                if protected_end == boundary {
                    break;
                }
                boundary = after_line(source, protected_end);
            }
            marker.note_at = boundary;
            let separator = usize::from(!line_start(source, boundary));
            output_bytes = output_bytes
                .checked_add(
                    separator
                        + "# literal candidates @".len()
                        + marker.pc.to_string().len()
                        + ": ".len()
                        + note.len()
                        + 1,
                )
                .ok_or_else(|| error("Compact output size overflow"))?;
        }
    }
    if output_bytes > MAX_OUTPUT_BYTES {
        return Err(error("Compact output exceeds the 128 MiB limit"));
    }

    let mut output = String::new();
    output
        .try_reserve_exact(output_bytes)
        .map_err(|_| error("Unable to allocate bounded compact output"))?;
    output.push_str(&header);
    let mut copied = 0;
    let mut pending = markers
        .iter()
        .filter(|marker| marker.note.is_some())
        .peekable();
    for marker in &markers {
        while pending
            .peek()
            .is_some_and(|note| note.note_at <= marker.start)
        {
            write_note(&mut output, source, &mut copied, pending.next().unwrap())?;
        }
        output.push_str(&source[copied..marker.start]);
        write!(output, "@{}", marker.pc).map_err(|_| error("Unable to write compact label"))?;
        copied = marker.end;
    }
    for marker in pending {
        write_note(&mut output, source, &mut copied, marker)?;
    }
    output.push_str(&source[copied..]);
    debug_assert_eq!(output.len(), output_bytes);
    Ok(output)
}

fn write_note(
    output: &mut String,
    source: &str,
    copied: &mut usize,
    marker: &Marker<'_>,
) -> DecompilerResult<()> {
    output.push_str(&source[*copied..marker.note_at]);
    if !line_start(source, marker.note_at) {
        output.push('\n');
    }
    writeln!(
        output,
        "# literal candidates @{}: {}",
        marker.pc,
        marker.note.unwrap()
    )
    .map_err(|_| error("Unable to write compact note"))?;
    *copied = marker.note_at;
    Ok(())
}
