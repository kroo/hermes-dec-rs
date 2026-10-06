# Workspace Navigation

Start with view/f<ID>.txt for compact PC-labelled inspection text and separate
local primitive literal candidates. It retains all raw non-marker JS text and
control flow; no dispatcher, branch, return, call or field is evaluated or removed.
Use complete f<ID>.js bodies as authoritative raw evidence; index.jsonl is only a bounded search aid.
Follow F references to other function files and use runtime.js to inspect helpers.
Do not execute these fragments or the app.

When present, links.jsonl and initializers.jsonl remove repeated source-inventory
work. Each has one row per function in ID order: select the `function` you need,
then inspect `report`. Consult manifest source_evidence for enabled designs,
byte counts and unavailable-function counts. Source report spans join raw_path
after fragment_prefix_bytes, not the compact view's character positions.
Function-table mentions are not proven runtime calls. Numeric env slots do not
establish lexical frame identity. Initializer tables preserve source dependencies
and ordered syntax, not evaluated constructor results, framework records or values.
Follow intermediate call/constructor layers in complete JS before drawing an
end-to-end conclusion; one local copy is not proof of an absent later conversion.
Check every report's limits/status/omissions. Unsupported analysis remains explicit
and does not remove any raw source. Missing rows/candidates never prove runtime
absence. `workspace --evidence links|initializers|all|none` selects these sidecars;
`--raw-only` omits both sidecars and compact views. The library's existing
workspace/workspace_with_views entry points retain their previous layouts.

links reports expose `records` (source function/slot mentions); initializers expose
`rows` (stores) and a shared `definitions` table. Store `definition_ids_source_order`
selects its included dependency definitions; edges can name omitted definitions,
so null/missing joins are uncertainty, not a value. Definition `syntax.nodes`
preserves call/constructor/member/array/object operand roles and source order;
operand indices are syntactic, not execution order. Sources and previews are raw
spellings, not decoded keywords. IDs are input-local spans, not content hashes:
never join reports from another workspace or edited raw file.

For targeted JSON inspection with jq, replace FUNCTION and SLOT with numeric IDs.
These examples inspect JSON only, never execute recovered JS. Always read the
first status/limit row before the evidence rows; do not suppress unavailable or
bounded-prefix information just because a selected slot has no included store.

    jq -c --argjson fn FUNCTION --argjson slot SLOT '
      select(.function == $fn) | .report |
      {status,reason,limits,scan_complete,total},
      (.records[]? | select((.kind == "env_slot" or .kind == "slots_member")
        and .id == $slot))' links.jsonl

    jq -c --argjson fn FUNCTION --argjson slot SLOT '
      select(.function == $fn) | .report as $r |
      ($r | {status,reason,limits,source_scan_complete,table_complete,total_stores,
        rows_omitted,next_offset,stop_reason,continuation_query}),
      ($r.rows[]? | select(.slot == $slot) |
        {store:., definitions:[.definition_ids_source_order[]? | $r.definitions[.]]})
      ' initializers.jsonl

Initializers continuation is generic sites inspection, not an evaluated table.
Its raw slot-write cursor is replayable only for supported exporter .slots writes;
unreplayable bounded direct env[N] tables fail closed. Links budgets fail atomically
instead of returning a false complete prefix. Sources are capped at64 MiB; AST/
query/output limits are in each report. These are work/expansion limits, not a total
memory, parser-safety or elapsed-time sandbox. Missing scope/binding evidence
never establishes the result of a call, constructor or notification.

View files are deliberately .txt, not executable JS. @PC labels are function-local
byte PCs, not text positions. Candidate notes are single-line JSON arrays whose
use_span/from/definition IDs join raw fragments, excluding the workspace
header. Notes only follow local primitive literals and plain register copies;
plain-copy RHS notes are skipped to avoid repetition; copies still appear in
raw text and the eventual use's alias provenance, with a separate skipped count.
`from` is [definition byte PC, literal start byte, literal end byte]; use_span is
[read start byte, read end byte]. `definition` and optional `via` are exact-source
IDs of the terminal definition and aliases. See manifest view_note_contract.
calls, captured slots, properties and framework objects remain opaque. Inspect the
per-function literal_note_status footer and manifest entry for uncertainty, bounded
prefixes or unavailable analysis. Raw inspection text is complete even when notes
are omitted; no candidate is not evidence of runtime absence. Use read INPUT ID
--offset OFFSET --scan-work 16777216 --json for a prefix continuation, consulting
its own limits. --raw-only omits view files; use raw bodies in that layout.

Header examples use hermes-dec-rs with INPUT, PC and SLOT placeholders, not
input paths or function names. Supply the original HBC input as a safely quoted
argument (or a separate process argument); never evaluate names or JSON as shell.
Replace hermes-dec-rs with your chosen/frozen CLI executable; do not assume PATH
or the current checkout's binary.
manifest.json navigation entries are structured argument templates, not shell code.

PC comments mark function-local bytecode offsets, not JS character positions.
Origins expression/source spans use UTF-8 byte offsets into the complete raw
exporter fragment without its inspection header, start inclusive/end exclusive.
For raw span [start, end), slice f<ID>.js bytes at
[fragment_prefix_bytes + start, fragment_prefix_bytes + end), using that function's
manifest entry. Other reports may label excerpt-relative spans; heed their units.
js_bytes counts function JS including headers plus runtime.js, excluding this guide
and the manifest/index. guide_path names this file; file_count includes it.

At a PC with unclear register definitions, use origins INPUT ID PC --expressions.
It adds typed syntax for definitions and ordered call roles (callee, receiver and
user arguments where recognized). These are bounded source candidates on normal
JS paths, not evaluated values or path-insensitive value claims; unknown or omitted
evidence is not absence. Inspect alternatives in the complete body.

For a less opaque local view, use read INPUT ID --match TEXT. It pairs raw JS
with string/number/boolean/null literal candidates through plain register copies
within one straight-line block. It does not evaluate calls, fields, captures or
framework helpers. Conditional, same-PC, block-entry and exception ambiguities
remain raw; views are not executable output. --json includes exact source spans
and definition IDs. --offset counts raw PC markers before filters.

For embedded JSON strings, use json-literals INPUT ID --pointer /PATH. The JSON
report slices object/array documents with strict RFC6901 pointers; --match TEXT
filters their complete decoded literal text, and --pc restricts an instruction.
It never executes JSON.parse or proves schema reachability. Duplicate-key,
lossy-number and malformed documents are omitted with explicit counts, not repaired. Selected
values are complete or the query fails its byte budget. --offset counts raw root
string expressions before filters; continuation has separate input/function
and flag tokens. No runtime values or protocol behavior follow from these alone.

Use sites INPUT ID --compact for a bounded page and shared definition table.
Narrow with --kind call, --match TEXT (source substrings, not decoded values),
--from-pc PC / --to-pc PC (inclusive), or --kind slot-write --slot SLOT.
Use --offset to page; consult truncation fields and byte budgets before concluding.

For numeric env slots, use captures INPUT ID to navigate reads and candidate
same-function/ancestor stores, then inspect candidate function files and slot-write
sites there. Registers unresolved by origins are not automatically captures.
Capture candidates do not establish runtime bindings, values or execution order.

For raw strings behind slot-write dependencies, use symbols INPUT ID --slot SLOT
or --match TEXT. It parses the complete function once per page and follows bounded
normal-flow candidate register definitions. The resulting raw string mentions
are not decoded strings, symbol names, slot values or lexical-frame identities.
The environment expression is separate from the searched RHS. Unknowns and
literal-search omissions matter even when a page has no rows. Follow next_offset
with --offset; it is a raw store-ordinal cursor, not a filtered result count.
scan_complete means the store scan ended, not that all dependencies are known.
continuation_query preserves options as separate argument tokens; supply your
chosen binary and original input. --scan-work adjusts aggregate dependency work
(default 1048576, maximum 16777216), not depth or literal/definition limits.
dependency_search_incomplete_rows also counts filtered-out uncertain rows;
continuing the store scan does not repair those earlier omitted dependencies.
filter_literal_diagnostics checks bounded raw instruction-string syntax outside
the RHS query too. Its matches do not establish store dependencies; its complete
flag covers only that diagnostic scope. Identifier spellings are not aliases for
raw strings. Check original JS spelling and diagnostic limits, not just the cursor.
Each term has at most three PC-labelled literal examples; inspect their original
JS context. Example omissions are separate from search completeness, and these
locations do not establish slot values, dependencies or argument roles.
Only a complete zero-match diagnostic skips dependency queries; explicit skipped
counts do not prove runtime absence. Partial diagnostics never skip these queries.

For less verbose provenance, use origins INPUT ID PC --text. Add --expressions
for syntax summaries, but text explicitly omits typed graph nodes and cannot
resolve their local IDs. Use ordinary JSON for complete returned syntax graphs.
Text is escaped line-oriented evidence, not evaluated JS or shell commands.

For property keys and their value expressions, use properties INPUT ID --match KEY.
Unlike symbols (slot-write RHS strings), this searches raw property-key expressions
and their bounded normal-flow candidate definitions. It includes simple member
assignments and exporter put/own helper shapes; object, key, value and helper
arguments remain separate. Object identity and evaluated values are not inferred.
Rows share source-span definition IDs, with alternatives and unknowns preserved.
Follow next_offset/continuation_query even after an empty filtered page. The cursor
counts raw property stores; paging does not repair earlier omitted dependencies.
--scan-work bounds aggregate key/value query work. queried=false explicitly marks
a role not analyzed after exhaustion; scan_complete describes stores only.
Filtering raw JS syntax does not decode string escapes or establish runtime keys.
The filter-work cap fails before stdout, as do output overflow and unsupported
put/own shapes or class scopes. Deferred class initializers are not enclosing-PC
evidence. Use complete bodies to verify each candidate and argument role.

Templates are stored once in manifest.json. Replace FUNCTION with the numeric ID
from a function entry or index row; INPUT remains the original HBC input argument.

For sibling writes in array/record initializers, use objects INPUT ID --match TEXT.
It searches raw site syntax and bounded local prior-definition dependencies, not
decoded keys or values. object_queries supplies source-derived origin-PC queries;
run objects INPUT ID ORIGIN_PC with their argument flags to page sibling writes.
Discovery filters are cleared in these queries so unmatched siblings are visible.
An origin-PC anchor is explicitly a PC-wide definition union: multiple definition
IDs at one PC remain distinct, not one inferred object. Query source-definition
IDs are capped at 64 with counts and omission flags, and may include definitions
not selected as local origins.
Only plain register copies extend a local source origin. Member reads, constructor
arguments and helper results are not aliases or evaluated object identities.
Shared definition IDs join exact source spans; object_origin names the terminal
definition separately from copy definitions. Block entries, exception boundaries,
conditional/same-PC writes and alias-depth omissions remain unresolved.
Consult origin_summary even for filtered-out rows; an empty anchored page is not
proof of runtime absence. Continuation preserves filters and uses raw property
store ordinals. --scan-work bounds aggregate indexing queries (default 2097152,
maximum 16777216); --alias-depth (0..64) is separate from provenance --depth (0..8).
Output/work/filter caps fail before stdout rather than returning partial JSON.
Optional chains, destructuring assignments, register shadowing, binding updates,
slot deletions, labeled control flow and structured non-dispatcher loops are unsupported.
No source is executed or normalized into framework-specific records or values.
