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
hermes-dec-rs sites INPUT.hbc 0 --kind constructor --depth 3
hermes-dec-rs sites INPUT.hbc 0 --kind slot-write --slot 0 --depth 8
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
- `workspace` exports every complete function into `f<ID>.js`, plus a manifest,
  bounded `index.jsonl` and inspection-only `runtime.js`. The new directory is
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
