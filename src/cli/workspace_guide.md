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

For less verbose provenance, use origins INPUT ID PC --text. Add --expressions
for syntax summaries, but text explicitly omits typed graph nodes and cannot
resolve their local IDs. Use ordinary JSON for complete returned syntax graphs.
Text is escaped line-oriented evidence, not evaluated JS or shell commands.

Templates are stored once in manifest.json. Replace FUNCTION with the numeric ID
from a function entry or index row; INPUT remains the original HBC input argument.
