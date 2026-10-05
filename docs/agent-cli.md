# Agent exploration CLI

Prefer bounded navigation and complete selected JavaScript over dumping an
entire project's tables or disassembly into model context. These commands parse
without whole-project CFG/SSA analysis. They do not execute the input application.
CLI `inspect` defaults to a bounded JSON summary; use explicit `--format json`
for the legacy full-table dump or `--format text` for one-line counts. The library
`inspect()` default is unchanged. Invalid-input diagnostics go only to stderr.

```sh
hermes-dec-rs inputs .
hermes-dec-rs inspect INPUT.hbc
hermes-dec-rs search INPUT.hbc iterator --limit 10 --json
hermes-dec-rs show INPUT.hbc 0,1 -o bodies.js --max-bytes 1000000
hermes-dec-rs refs INPUT.hbc 1 --direction in --depth 2
hermes-dec-rs show INPUT.hbc 0 --around-pc 0 --context 8
hermes-dec-rs slots INPUT.hbc 1 0 --depth 3 --limit 10
hermes-dec-rs trace INPUT.hbc 0 0 --depth 32 --limit 128
hermes-dec-rs origins INPUT.hbc 0 0 --depth 8 --limit 64
hermes-dec-rs origins INPUT.hbc 0 0 --expressions --max-bytes 1000000
hermes-dec-rs origins INPUT.hbc 0 0 --text
hermes-dec-rs symbols INPUT.hbc 0 --match RAW_TOKEN --limit 8
hermes-dec-rs symbols INPUT.hbc 0 --slot 1,2 --literal-limit 16
hermes-dec-rs sites INPUT.hbc 0 --kind constructor --depth 3
hermes-dec-rs sites INPUT.hbc 0 --kind slot-write --slot 0 --depth 8
hermes-dec-rs sites INPUT.hbc 0 --match FIELD_NAME --depth 8 --compact
hermes-dec-rs sites INPUT.hbc 0 --from-pc 0 --to-pc 100 --kind call --compact
hermes-dec-rs captures INPUT.hbc 1,2 --depth 3 --limit 5
hermes-dec-rs workspace INPUT.hbc -o NEW_DIRECTORY
```

## Evidence and output contracts

- `inputs [ROOT]` discovers `.hbc`/`.bundle` header candidates, including hidden
  and Git-ignored files, sorted largest first so sample fixtures do not hide app
  bundles on the first page. It reads only 12-byte magic/version prefixes, not
  whole programs. Header matches are not proof of validity or protocol presence;
  follow with `inspect`/`search`. Symlinks observed during traversal are skipped;
  concurrent filesystem replacement races are not excluded. Read errors,
  directory errors, filename lossiness and extension-selection scope are explicit.
- `search` uses OR substring matching, case-insensitive by default; `--regex`
  and `--case-sensitive` are explicit. It maps string/name matches to original
  function IDs and function-local byte PCs, including serialized literal buffers.
  Function results are stably ranked and paginated with `--offset`/`--limit`.
  `--all` requires every query to match somewhere in a function's name,
  referenced strings/properties or assigned aliases, not at the same instruction.
  This is an intersection of navigation matches, not proof of behavioral linkage.
  Evidence previews are bounded; missing preview text is not evidence of absence.
  `--word` matches whole alphanumeric components (punctuation and underscores
  are boundaries), useful for avoiding short-query substring noise. Direct local
  closure-to-property assignments also supply navigation names for opaque bodies
  and rank ahead of literal-only matches. These are syntactic assignment sites,
  not authoritative runtime exports or resolved environment-slot values.
- `show` lowers complete selected function bodies through the same correctness-
  first backend as `export-bundle`, with original PCs as comments. Physical
  registers, dispatch, captured environments and helper calls remain explicit.
  The output is JavaScript, not the recovered original source. A JS fragment
  needs the bundle runtime and its referenced functions; it is not standalone.
  An exceeded `--max-bytes` budget fails before emitting or overwriting output.
  Bounded static assignment candidates accompany selected bodies as JSON
  metadata or escaped JS comments; these are not authoritative runtime names.
- `show --around-pc` requires one function and an exact instruction boundary.
  It emits JSON containing a bounded JS excerpt, coverage and next-PC metadata.
  Excerpts may omit surrounding braces and are not complete JavaScript programs.
  `--context` accepts 0..1000; large excerpts still need a sufficient byte budget.
- `refs` reports only direct calls and static closure creation. It is not a
  dynamic call graph. `slots` gives possible writes in closure ancestors, with
  source JS excerpts and capture-path evidence, not authoritative lexical values
  or automatic symbol resolution. Slot numbers alone do not identify environments.
- `trace INPUT FUNCTION PC` follows prior physical register assignments within
  the emitted JS block, with exact PC/source evidence and explicit unresolved
  dependencies. It does not evaluate getters, calls or constructors, track deep
  mutations, resolve runtime environments, or guess definitions across blocks.
  Source snippets are bounded with explicit truncation. Depth omissions are
  flagged; node/byte budget overflow fails before emitting stdout. Use `show`
  to inspect the full operation and source, not executable substitutions of a
  trace expression. For a captured slot, inspect `slots` candidates, then trace
  a candidate store's arguments; lexical identity still requires verification.
- `origins INPUT FUNCTION PC` is experimental cross-block register navigation
  over the exporter's normal JavaScript dispatcher paths. It retains alternative
  reaching definitions at joins, with exact source spans and PCs. Candidates are
  not evaluated values, executable substitutions or proof that a path runs.
  Indirect/unsupported control, same-PC ordering, cycles and bounded omissions
  remain explicitly unresolved; exceptional/generator behavior is not silently
  treated as ordinary flow. It does not infer heap mutations, captured-slot
  identity or framework constructor semantics. Use complete `show` source to
  verify the alternatives. Errors and byte overflow emit no partial JSON.
  The `limits` object declares indexing and query caps; exceeding indexing caps
  fails closed. Source is limited to 64 MiB, output to 16 MiB, blocks to 4,096,
  and definitions/instructions to 1,048,576. The default query retains at most
  64 definition nodes with depth eight and 8,192 work units. Displayed normal
  edges are capped at 256 with total/returned/truncated counts; the internal
  normal-flow index is separate. Unsupported exceptional/generator functions
  return explicit unknowns without candidate traversal, not guessed normal flow.
  `unresolved: false` describes syntactic definition links only, never runtime
  values, captured identities, predicate feasibility or constructor behavior.
  Add `--expressions` for bounded typed syntax graphs on selected definitions
  and direct expression statements at the requested PC. View-local node IDs
  describe register reads, literal kinds/raw JS, member keys, ordered arguments,
  arrays, operators and both conditional branches; exact UTF-8 source spans
  join them to complete JS. `expression_source` states the coordinate origin:
  the raw exporter fragment, without the inspection header added by workspace
  files. Locate its bounded `raw_fragment_prefix` in the workspace file and add
  that byte offset when joining spans. They do not normalize framework data or resolve
  aliases into values. Exporter-shaped call roles are syntactic labels, not
  proof of helper identity or API parameter names. Unsupported nodes and
  omitted depth/nodes/items remain explicit. Raise `--max-bytes` for larger
  graphs; byte overflow still emits no partial JSON. The original compact
  candidate report is unchanged without this flag. Direct expression statements
  at one PC are capped at 32, with total/omitted counts. Per-view graphs retain
  at most 128 nodes, depth 16 and 32 collection items, with a shared 4,096-node
  projection budget. These syntax caps do not change candidate reachability;
  `expressions_truncated` reports projection omissions separately. Expression
  indexing is bounded at 32 million visitor frames (including enum/collection
  wrappers) and depth 256; exceeding either fails before emitting JSON.
- `sites` catalogs constructor invocations with ordered explicit arguments,
  property writes and environment-slot accesses in explicitly selected functions.
  It parses complete generated JS once per function and includes bounded local
  register-definition provenance so large initializer scans need not start with
  custom scripts. `--kind`, `--slot`, `--offset` and `--limit` filter/page stable
  function/PC/site records; a slot filter excludes non-slot records. Constructor
  arguments distinguish the preallocated receiver from user arguments.
  Intrinsic `new` allocations are not included in the constructor-helper category.
  The default page contains at most five sites; raise the byte budget for larger
  pages or deeper provenance as needed. No
  constructor/framework semantics, heap evaluation or lexical identity are
  inferred. Depth/node/operand/source omissions are explicit, and byte-budget
  overflow errors before stdout. An index of declared sites is not proof of
  execution order or an implementable protocol by itself.
  `--compact` shares definition sources in a deterministic `definitions` table
  keyed by function ID and JS span; operand-local edges and traversal/truncation
  metadata remain separate. It removes repetition, not uncertainty. Default JSON
  remains expanded for compatibility. `--kind call` catalogs exporter `apply`
  invocations with a separate callee, receiver and ordered user arguments; it does
  not catalog every helper, builtin or direct call, or infer API parameter names.
  `follow_up_queries` gives unique, ordered `origins` function/PC targets with
  `--expressions` when included operand or prior-definition edges reach an
  unresolved block-entry/external register read. Use the same input HBC and
  the stated flags. Suggestions cover only the returned page, not omitted
  dependencies, same-PC ambiguity alone or inferred captures. They are candidate
  navigation, not resolved values; an empty list is not proof of absence.
  Repeatable `--match TEXT` filters OR case-insensitive literal substrings over
  complete site-expression source and bounded same-block prior definition spans.
  It searches beyond display previews, not decoded string values or runtime
  identities. Exact source/PC match witnesses accompany results. Dependency depth,
  nodes, operands and witness counts remain bounded and their omissions explicit;
  a missing match is not evidence of absence in unresolved/cross-block/captured
  values. `--from-pc`/`--to-pc` apply inclusive function-local PC ranges independently
  to each selected function. Page offsets count sites after filtering.
- `captures INPUT FUNCTION_IDS` batches captured-slot reads and matching-slot store
  candidates from the same function and static closure ancestors, with bounded
  JS evidence. This avoids manually extracting each slot number before lookup.
  It includes generator wrapper/body edges, not direct-call ancestry. Equal slot
  indices or environment-register names do not prove runtime scope identity.
  Same-function stores before and after the read are candidates; relative PC is
  not execution order. Reads are stably paginated; depth/candidate/edge/snippet
  limits and missing/unresolved states are explicit. Reports are fully serialized
  and budget-checked before stdout. Follow candidates with `sites`/`show` to inspect
  their initializer source; this command does not normalize framework data or
  infer callable/field names from runtime environments.
- `symbols INPUT FUNCTION` extracts raw string-literal mentions in bounded
  candidate slot-write RHS dependencies. It reuses one complete parsed normal-
  dispatcher index across the page and follows cross-block register candidates.
  This is a navigation index, not evaluated slot values, constructor results,
  decoded strings or lexical-frame identities. The environment expression is
  recorded separately, not searched as a value dependency. Shared source-span
  definition IDs and dependency links retain alternatives, cycles, same-PC
  ambiguity and unknowns. `--match` is OR case-insensitive raw-token substring
  filtering; escaped syntax is not decoded. Matching tokens are retained ahead
  of other mentions within the display cap. `--slot` narrows numeric syntax only.
  `--offset`/`next_offset` are raw store-ordinal cursors, not matched-row indexes.
  `scan_complete` covers store scanning only: dependency/literal truncation and
  unknowns are separately explicit, including for excluded rows. Query work,
  literal projection work/bytes, source size and fully serialized stdout have
  bounds. No match in this bounded scope is not runtime absence.
- `origins --text` emits escaped, stable line-oriented candidate records with
  complete returned definition/read links and status flags. It does not evaluate
  or substitute source. With `--expressions`, typed graph nodes are explicitly
  omitted by the renderer; summaries retain counts, spans and omissions, and
  local graph IDs cannot be resolved from text. Use JSON for full syntax graphs.
  Text is bounded by requested `--max-bytes`; the intermediate JSON has its own
  16 MiB ceiling. Either error occurs before any stdout, not midway through lines.
- `workspace` exports every complete function into `f<ID>.js`, plus a manifest,
  bounded `index.jsonl`, a short `GUIDE.md` and inspection-only `runtime.js`.
  The guide and function headers point to source-derived capture, site and
  typed-expression navigation, without supplying app-specific hints.
  The guide and shared templates also offer raw RHS symbol mentions and escaped
  text provenance, with cursor, omission and graph-node limitations explicit.
  Function manifest entries record the inspection-prefix byte length for exact joins
  from raw-fragment UTF-8 expression spans to workspace files. Navigation
  records are shared manifest templates, not executable shell strings or
  resolved values; function IDs are supplied by the selected function entry.
  The new directory is
  published atomically without replacing an existing file/directory/symlink.
  A failure does not publish a partial workspace. This is not a power-loss
  durability guarantee. The index is incomplete by design: search function files
  for full contents. Many-file export has additional filesystem/indexing costs.
  The index also includes bounded static assignment-name candidates.
- JSON interfaces include `schema_version: 1`. Errors use stderr and nonzero
  status; successful JSON stdout is not mixed with progress logging.

`export-bundle` remains the complete executable-JS export, including runtime and
bootstrap. Its performance target measures process startup, reading, parsing,
all-function lowering, validation and writing, not only disassembly. Workspace
generation and individual exploration commands are separate latency measurements.
Legacy `decompile --function` uses the structured CFG/SSA backend; it can be much
slower on large projects. Use `show` for fast, correctness-first function bodies.

## Cold-context evaluation

Start agents with `fork_context: false`, the task question, an executable path,
an output directory, a fixed wall-time cap and identical evidence restrictions.
Do not provide known function IDs, UUIDs, schema names, previous answers or other
protocol hints. Freeze each tested binary; concurrent CLI designs may differ.
Record model and reasoning effort and do not attribute cross-model, cross-budget
differences solely to the CLI. Generated JS is the primary protocol evidence;
metadata and references are navigation aids. Do not run the app or access devices.

Record host sleep/suspension separately from CLI latency. A suspended local host
can also delay deadline enforcement; do not relabel an overrun or subtract guessed
sleep time to manufacture a completed bounded trial. On macOS, a temporary
`caffeinate -i -s -t 2400` assertion can cover a 30-minute trial; `-s` requires AC
power. Verify the actual assertion with `pmset -g assertions` and the power source
with `pmset -g batt`, then stop only the assertion process created for the trial.
This does not change permanent power settings or guarantee protection from
explicit forced sleep. Preserve actual wall times, completion status and logs.

Keep trial answers outside the repository and grade against an independently
derived, frozen reference. Report incompleteness and factual errors separately
from elapsed time; a short incomplete answer is not a successful speedup. Record
both first-answer and final-artifact times, deadline overruns, parent interruptions,
CLI commands and major bottlenecks. Baselines allowing disassembly must be labeled
mixed-evidence, not treated as equivalent to JS-primary trials.

```sh
node scripts/measure_cli_agents.mjs --json -o NEW_REPORT.json SESSION_A.jsonl SESSION_B.jsonl
node --test scripts/tests/measure_cli_agents.test.mjs
```

The telemetry helper reads only explicit JSONL paths. It reports actual recorded
thread token counters, cached input and output separately, with schema/line
provenance. Cumulative snapshots are not summed. Missing metrics are null, never
estimated. Tool-output bytes are not model tokens. Reports contain numerical
metrics, not transcript text. Compare completeness and accuracy before trying
smaller models or claiming a reduction in time/tokens.

See [trial results](agent-cli-results.md) for checkpoint scores, measured token
counters, timing caveats and full-JS export performance, including failed targets.
