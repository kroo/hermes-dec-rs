# Workspace Navigation

Start with complete f<ID>.js bodies; index.jsonl is only a bounded search aid.
Follow F references to other function files and use runtime.js to inspect helpers.
Do not execute these fragments or the app.

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
