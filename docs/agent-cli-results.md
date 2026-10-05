# Cold-context CLI trials

Snapshot: 2026-10-05. These are exploratory trials, not a controlled experiment
or a claim that the goal has been achieved. Answers and proprietary protocol
evidence remain outside the repository so later agents cannot find an answer key.

## Protocol reconstruction

Agents began with no inherited conversation, discovered their input, and used a
frozen executable. An independent evaluator froze its reference and rubric before
reading their answers. Its gate requires at least 85/100, at least half credit in
every category, implementable core controls for both families, and no material
crypto/framing/control error. Static client recovery is distinguished from native
or firmware behavior that cannot be established without device testing.

| Design | Model / effort | Cap (s) | Reported answer checkpoint (s) | CLI calls | Score | Gate |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| Original A | 6.1 Sol / high | 720 | 674 | 7 | 51.5 | fail |
| Original B | 6.1 Sol / high | 720 | 663 | 8 | 54 | fail |
| Original C | 6.1 Sol / high | 720 | 686 | 13 | 54.5 | fail |
| Query v1 | 6.1 Sol / medium | 360 | 406 | 22 | 47.5 | fail |
| Workspace v1 A | 6.1 Sol / medium | 360 | 487 | 20 | 53 | fail |
| Workspace v1 B | 6.1 Sol / low | 360 | 361 | 17 | 48 | fail |
| Query v2 | 6.1 Sol / medium | 360 | 433 | 19 | 56.5 | fail |
| Query v3 A | 6.1 Sol / medium | 720 | 767 | 10 | 81 | fail |
| Query v3 B | 6.1 Sol / low | 720 | 708 | 19 | 73.5 | fail |
| Query v4 | 6.1 Sol / medium | 720 | 742 | 6 | 82 | fail |
| Query v3 control | 6.1 Sol / medium | 720 | 937 | 7 | 73.5 | fail |
| Query v5 | 6.1 Sol / medium | 720 | 714 | 9 | 87.5 | fail |
| Query v5 discovery failure | 6 Luna / low | 720 | 51 | unavailable | 1 | fail |
| Query v6 | 6.1 Sol / low | 720 | censored at 720 | unavailable | 49 | fail |
| Query v6 | 6 Luna / low | 720 | 187 (answer artifact) | 16 (reported) | 17.5 | fail |

Original trials primarily used disassembly and were interrupted after about
439 seconds of research, with artifact drafting afterward. All later trials
required JS-primary evidence. Reported checkpoints are not necessarily ultimate
metadata/session completion. Overruns are visible, not excluded. Workspace v1
agents did not adopt workspace generation; v3 medium did. Query v2 did not adopt
the new slot command. Feature availability is not feature effectiveness.

V3 added assignment-name search, whole-component matching and generic workflow
help. It also had a longer cap, incremental artifact instructions, and stricter
help/artifact-only discovery. Increased coverage cannot be attributed solely to
CLI changes. Remaining recoverable gaps include operation-to-serializer tracing,
units/indexing, codec error paths and checksum details. One low-effort answer had
a material packed-header size error. Honest device-free limitations are not the
same as recoverable client-audit omissions.

## Actual Token Counters

These are final recorded thread counters, including follow-up artifact/report
turns, extracted from explicit local JSONL sessions by `measure_cli_agents.mjs`.
Cached prompt input is included in total input; repeated cached context is not
unique source size. Uncached input is input minus cached input, not a monetary
cost estimate. Reasoning output is a subset of output, not added a second time.

| Trial | Input | Cached input | Uncached input | Output | Total |
| --- | ---: | ---: | ---: | ---: | ---: |
| Original A | 4,402,907 | 4,163,200 | 239,707 | 16,140 | 4,419,047 |
| Original B | 3,366,022 | 3,183,232 | 182,790 | 14,556 | 3,380,578 |
| Original C | 3,332,379 | 3,173,120 | 159,259 | 14,225 | 3,346,604 |
| Query v1 | 3,172,900 | 3,007,616 | 165,284 | 12,330 | 3,185,230 |
| Workspace v1 A | 3,090,201 | 2,902,528 | 187,673 | 10,374 | 3,100,575 |
| Workspace v1 B | 2,620,938 | 2,453,888 | 167,050 | 9,298 | 2,630,236 |
| Query v2 | 4,420,970 | 4,046,592 | 374,378 | 12,310 | 4,433,280 |
| Query v3 A | 4,010,914 | 3,656,832 | 354,082 | 19,163 | 4,030,077 |
| Query v3 B | 5,389,670 | 4,902,400 | 487,270 | 16,562 | 5,406,232 |
| Query v4 | 4,449,313 | 4,167,424 | 281,889 | 17,681 | 4,466,994 |
| Query v3 control | 5,026,570 | 4,762,496 | 264,074 | 20,472 | 5,047,042 |
| Query v5 | 3,883,536 | 3,628,160 | 255,376 | 17,709 | 3,901,245 |
| Query v5 discovery failure | 596,408 | 577,536 | 18,872 | 3,234 | 599,642 |
| Query v6 Sol | 4,513,629 | 4,278,144 | 235,485 | 14,578 | 4,528,207 |
| Query v6 Luna | 2,369,408 | 2,253,312 | 116,096 | 8,069 | 2,377,477 |

Neither a reliable full answer nor a token reduction is established yet. Small
samples, different caps/efforts/evidence policies and interrupted baselines rule
out a causal speedup claim. Do not promote a smaller model merely because an
incomplete answer is shorter.

## Full-JS Export Performance

Apple M2 Max, 12 logical CPUs, default Rayon threads, warmed filesystem samples;
no OS cache flush. The checkpoint release binary SHA-256 was
`e331035f2ff6c97a8002e7cc7f2a46097092bd49ec6c7d2abc969d71c8fe2f30`.
These timings precede the later trace navigation change. Full-export output hashes
were unchanged from the committed exporter. No Cargo jobs overlapped these runs.

| Project | Functions | JS bytes | Warm runs | Median (ms) | Worst / p95 (ms) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Orbit | 40,251 | 78,606,965 | 10 | 396.0 | 401.5 |
| Modern Animal | 49,647 | 64,041,722 | 10 | 248.4 | 256.6 |

Process wall time includes startup, reading, parsing, all-function lowering,
internal validation and writing. External `node --check` is performed afterward,
outside the timed command. This is full JavaScript generation, not disassembly.
First observed process invocations were 415.8 ms and 586.1 ms respectively;
they are not measured cold-disk starts. Checkpoint under-one-second targets pass
on this host, not on every host or under arbitrary contention.

Earlier contended Orbit runs missed the target: median 1,629 ms / worst 2,728 ms
with Cargo work overlapping, and median 986 ms / worst 1,053 ms with Clippy
overlapping. These misses remain part of the performance evidence. Multi-file
workspace export/indexing is a separate workload, not covered by the bundle target.

The trace-enabled v4 frozen binary
`0e2ef3afc35be651890ad5a5a33b9f3c0c341a7feb29e9b4cd060ea5a010b744`
was also checked for ten warmed runs per project with independent research
agents active, but no Cargo jobs overlapping. Orbit median/worst were
416.0/586.3 ms; Modern Animal 243.2/359.4 ms. First invocations were
815.7/235.1 ms, and full-export hashes remained unchanged. These runs pass the
one-second target; missing sampled peak RSS is unavailable, not zero.

A subsequent source review found generator-body ancestry missing from `slots`
and marker-like string contents interfering with `show --around-pc`. Both were
fixed in source with regressions. Frozen v4 and previous-design control trials
remain on their original binaries; results must not be described as testing
those later fixes. The pair uses identical medium effort, a 720-second cap,
JS-only evidence and generic operational/numeric completeness instructions.
Both chose workspace generation and custom static-analysis scripts; v4 did not
adopt `trace`. V4's final summary corrects its stale measurement checkpoint to
742 seconds. Control research was interrupted after the deadline; final drafting
ended at 937 seconds with no additional research reported after intervention.
V4 recovered more operational detail but mislabeled a control operation, failing
the gate. Its total recorded tokens were lower, while uncached input was higher
than control. With n=1, deadline overruns and unequal finalization, this does not
establish a reliable speedup, lower cost, or qualified smaller-model promotion.

V5 added bounded constructor/store-site catalogs. The medium trial inspected
`sites --help` but still used workspace export and custom extraction; actual
catalog-query adoption is not established. Its raw score was 89.5, minus two
points for conflating a correlation field with an ACK event tag. Thus 87.5 meets
the numerical threshold but fails the no-material-error and full-control gates.
The answer was written at approximately 715 seconds from its first-action clock;
the required measurement artifact followed at approximately 724 seconds. Its
observed session span was 737.412 seconds. No parent hard-stop timer was enforced.

The Luna trial falsely concluded that the relevant app input was unavailable
after filtered file discovery missed ignored large-project files. Its reported
timings conflict (18 versus 51 seconds); the observed session span is 89.037
seconds, not a successful answer latency. Reported CLI counts are inconsistent,
so that table entry is unavailable rather than an inferred count. The telemetry
records 33 total tool calls for medium and 15 for Luna, not CLI-only counts.
Input discovery now includes hidden and Git-ignored header candidates, largest
first, without supplying protocol-specific hints. An earlier Luna probe overran
its soft cap and was stopped late; its partial answer is not a qualifying trial.

A later native-versus-export differential regression exposed numeric prototype
accessors corrupting private VM arrays. Source now isolates registers, lexical
slots and scratch lists, and guards missing formal arguments; genuine application
arrays retain their prototype behavior. This changes exported JS hashes. All
performance numbers above precede that fix and must not be presented as its
verification. Frozen trial binaries remain unchanged.

After that correction, frozen v6 binary
`b9fdb848f11d7c775a9b75dfb554b379f4756191ae98c754fb3188d451c043ca`
passed ten warmed full-export runs per project without Cargo jobs overlapping:

| Project | Functions | JS bytes | Median (ms) | Worst / p95 (ms) | First invocation (ms) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Orbit | 40,251 | 80,544,307 | 390.5 | 406.9 | 780.7 |
| Modern Animal | 49,647 | 66,405,608 | 238.6 | 255.7 | 244.6 |

Both pass the one-second target on this host. The output hashes are now
`7cd0612ea4b632e00affb1b3c1fecd7cb16c901b244389ccf9a7443aead5216e`
and `7b5cf0ff74e6c34fa4b44f709a014071be4721de2f45885111920f093eef3ad6`
respectively. External syntax checks remain outside the timed command.
All 403 Rust tests and 20 telemetry tests pass (one optional telemetry test
skipped); formatting passes. Clippy has no diagnostics in the new CLI modules
or regression files, but existing repository warnings remain.

V6's first attempted smaller-model pair was aborted because the harness used
the frozen executable path as its artifact directory. That is a harness error,
excluded from model scoring; fresh agents restarted with separate directories.
The second pair used actual dispatch-inclusive 720-second deadlines. Sol was
closed while running 28 milliseconds after its deadline, with only a saved
work-in-progress answer and no measurement artifact: 49/100, not a completed
answer. Its observed session span was 719.859 seconds. Luna's answer artifact
was saved 186.733 seconds after dispatch (session span 214.710 seconds), but
coverage was only 17.5/100 and it reversed a buffer-write value and offset.
Its measurement file says 170 seconds; final text says 190 seconds. These clocks
are not silently substituted for dispatch-inclusive completion. It reported
16 CLI invocations; telemetry records 29 total tool calls. Sol recorded 35 tools.
The independent evaluator found neither full gate passed and no established
actual site/trace-query use from their reported evidence. Relevant input discovery
now succeeds; reliable protocol reconstruction and smaller-model promotion do not.

The following unscored interface prototypes are frozen separately:
v7 adds conjunctive search (`--all`), binary
`d4232745dcabbfb59422d680ade0c51539c29413c13f6205dc6f2786f8d35392`;
v8 additionally adds explicit ordinary-call argument roles and compact shared
provenance, binary
`4b337b02cc5faa0dfda76afd6805a0052974df022f14bbce2da8729fff929f05`.
A generic two-term whole-word search on Modern Animal narrowed 465 candidates
to 36. This measures navigation filtering, not a protocol-answer speedup.
One real 20-constructor page measured 146,701 bytes expanded versus 128,248
compact (about 12.6% smaller); both exceed the default 100,000-byte budget.
Synthetic highly overlapping DAG tests save more, but that is not a universal
compression claim. Timing for that page overlapped later Cargo work, so it is
not an isolated latency benchmark. Sources/relationships round-trip exactly in
the authored compact-format tests; values remain unevaluated.

Latest verification: all 412 Rust tests pass, with no new-module/test Clippy
diagnostics after fixing the prototype's repeat-iterator warning. Five pinned
library projects and six behavioral fixtures match native Hermes on both HBC
90 and 96 after the private-storage correction. Existing unrelated Clippy
warnings remain; the export PR's build/test/fmt/Clippy checks pass, while its
review-bot job fails because an OAuth access token expired.
