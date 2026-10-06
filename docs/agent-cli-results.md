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
| Query v7 matched | 6.1 Sol / medium | 1200 | 1171 (self final) | 11 | 89.5 | fail |
| Query v8 matched | 6.1 Sol / medium | 1200 | censored at 1200 | unavailable | 24 | fail |

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
| Query v7 matched | 6,381,203 | 5,973,632 | 407,571 | 29,491 | 6,410,694 |
| Query v8 matched | 5,622,755 | 5,268,480 | 354,275 | 27,101 | 5,649,856 |

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

The v7/v8 pair then ran concurrently with identical medium-effort Sol settings,
cold prompts and enforced dispatch-inclusive 1,200-second deadlines. V7 finalized
before its deadline: 1,171 seconds self-reported, observed session span 1,183.004
seconds, and parent final observation at 1,183.173 seconds. Its raw score was
91.5 minus two points for a materially wrong advertisement-identity byte extent.
Final 89.5 still fails the full-spec gate: recoverable version/sync/correlation/
schedule gaps remain. V8 was closed while running 36 milliseconds after its
deadline, with an incomplete saved scaffold and missing measurement artifact:
24/100, no identified material error, but no implementable control coverage.
Its observed session span was 1,199.855 seconds. V7 recorded 48 total tools;
v8 recorded 50. Lower counters for a censored partial are not a successful saving.

The evaluator did not infer feature adoption from help or undeclared files.
Separately, a parent audit of explicit CLI command literals plus generated report
metadata establishes one actual v8 compact slot-write catalog query, not ordinary
call-catalog usage. V7's measurements identify site/slot help only and extensive
custom initializer normalization scripts. Neither establishes conjunctive-query
adoption. These are interface-availability trials, not proof that the added
features caused either score. With n=1, censoring, incomplete answers and a longer
cap than earlier trials, no causal speedup or smaller-model promotion is established.

The v8 binary also passed ten warmed full-JS exports per large project with no
Cargo overlap. Orbit median/worst: 386.9/390.7 ms; Modern Animal: 240.3/390.7 ms.
Output hashes match corrected v6 outputs. All-feature local Rust tests still pass
(412 on macOS), as do the telemetry tests. The stacked draft CLI PR now runs
build/test/fmt/Clippy checks and telemetry tests; all four checks passed after its
branch-filter fix. The separate review-bot authentication failure remains.

Next interface work should target the demonstrated custom initializer/captured-
symbol navigation burden, while preserving source/PC evidence and avoiding
path-insensitive guesses presented as resolved values. More feature availability
alone is not sufficient. The reliable full-protocol reconstruction goal remains
unachieved; all protocol agents in these rounds have completed or been closed.

V9 adds batched capture navigation and bounded site-source/range filtering,
frozen as binary
`f3b598019805b7393c2473de55bfd1dfb44304b8449bc4ea999e910b226c6e83`.
Capture candidates are ranked by shortest static witness, not runtime likelihood;
same-function stores both before and after a read remain candidates. Excerpts
are serialized once, with per-instruction UTF-8 spans. Source filters search
complete expressions and bounded prior definitions before pagination; negative
results and dependency truncation are explicitly non-authoritative.

All 428 local Rust tests pass, formatting passes, and Clippy reports no diagnostics
in the new modules or regression files. Existing unrelated warnings remain.
Ten warmed full-export runs per large project, with no Cargo overlap, measured
Orbit median/worst 374.6/403.2 ms and Modern Animal 244.8/260.4 ms. Corrected
output hashes remain unchanged. These times include startup through writing;
external Node syntax checks run outside the timer, as before.

A single isolated large-initializer constructor-source query returned five sites
in 616 ms / 39,272 bytes; a capture batch returned four reads in 108 ms / 17,242
bytes. These are single-sample navigation measurements, not general latency
guarantees or cold-agent protocol improvements. Framework data normalization
and complete protocol reconstruction remain unverified by this interface work.

The fresh v9 medium/low Sol pair used identical frozen binaries and cold prompts
with dispatch-inclusive 1,200-second deadlines. Medium finalized before its cap:
1,169.017 seconds self-reported through artifact writing, 1,183.320 seconds
observed session span, 1,183.496 seconds at parent final observation. Low saved a
partial answer and measurements but was still running when closed 36 ms after
its deadline; its session span is 1,199.853 seconds. Its self-reported 1,170-second
artifact duration is not silently substituted for completed-task latency.

| V9 trial | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Sol medium | 6,303,207 | 6,029,440 | 273,767 | 28,911 | 6,332,118 | 51 |
| Sol low, censored | 5,704,284 | 5,395,712 | 308,572 | 29,311 | 5,733,595 | 48 |

Cached input is included in input/total, not unique source volume. Reasoning is
included in output (2,628 medium; 1,517 low). Snapshot counters are not summed.
Medium reports 11 CLI invocations and low eight; tool counts include other work.
Both used workspace JS and an actual compact slot-write catalog query. Neither
reports using captures, conjunctive search or source/range filters. A separate
literal-command audit establishes no new-feature query, disassembly or Rust/docs
reads among those command literals, but is not a comprehensive dynamic-command
audit. Help inspection alone is not feature adoption.

Independent grading against the unchanged reference/rubric gives medium 84.5
(86.5 coverage minus two material-error points) and low 78 (88 coverage minus
ten material-error points). Medium misinterprets an advertisement flag; low
misstates the provisioning characteristic and a protobuf control-field name.
Both fail the full gate, retaining recoverable synchronization, correlation,
version-boundary and scheduling gaps. More complete raw coverage in a censored
partial is not successful completion. No reliable improvement, causal feature
benefit or smaller-model promotion is established. The goal remains active.

V10 is an unscored cross-block navigation prototype, frozen separately as
`b44da8720ee08d07b60de7e76e9c83362d84da6289e45d056773b66496cff112`.
The experimental `origins` command indexes typed normal dispatcher paths once,
then returns bounded alternative register definitions without evaluating calls,
getters, captures, constructors or heap mutations. Exceptional/generator flows
stop candidate traversal. Unsupported control, cycles, same-PC ambiguity and
omissions remain explicit. Enclosing assignment destinations cannot reach their
own RHS reads. The existing local tools retain their conservative contracts;
sites now points to origins for unresolved block-entry reads.

The first prototype rejected the real 26,241,704-byte raw initializer at its 16 MiB
source cap. After raising bounded indexing caps, its original 1,024-work default
query still exhausted the budget on the first register before inspecting the
second. The final default permits 8,192 work units, with a synthetic two-register
long-predecessor regression. Three isolated warmed queries over that initializer
then took 601.1, 538.8 and 539.2 ms, returning 13,184 bytes / 18 definitions.
Both selected root reads have untruncated candidate links; 861 normal edges are
indexed but only 256 displayed, explicitly flagged as display truncation. All
displayed definition spans were checked against the complete JS. This proves
navigation evidence, not resolved values or cold-agent protocol success.

The final frozen binary passed ten warmed full-JS exports per large project with
no Cargo overlap: Orbit median/worst 392.0/411.4 ms; Modern Animal 237.9/261.5 ms.
Corrected output hashes remain unchanged, and external syntax checks pass.
The prototype has not yet been tested in a qualifying cold protocol trial;
no improvement or model promotion is inferred from these microbenchmarks.
All 443 local Rust tests pass, including 14 origins tests and the wired CLI
regression. Formatting passes; Clippy has no diagnostics in the new source or
tests, while existing unrelated repository warnings remain. Telemetry tests
still pass (20, with one optional real-session test skipped).

The first paired v9/v10 medium-Sol trial is invalid as a clean 1,800-second
comparison: both agents were still running when closed roughly 261 seconds late.
macOS power logs establish repeated maintenance sleep/dark-wake intervals,
matching simultaneous long session gaps. Both left only short scaffolds; frozen
grading gives 4 and 5.5, with both gates failing. Their recorded totals were
677,954 and 904,823 tokens respectively. These low counts are not efficiency
gains, and neither guessed active-time subtraction nor a 30-minute completion
claim is appropriate. The attempt and telemetry remain preserved privately.

A fresh cold medium-Sol retry used the same separately frozen binaries and
identical prompts, with a temporary verified AC sleep assertion. The assertion
was removed after completion; permanent power settings and unrelated assertions
were unchanged. No sleep/wake events were recorded during the retry interval.
Ordinary wall time, not sleep-adjusted time, is reported below. Both agents
completed before their 1,800-second dispatch deadlines.

| Awake trial | Parent final observation (s) | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A: v9 | 1,694.675 | 12,783,950 | 12,235,904 | 548,046 | 55,292 | 12,839,242 | 94 |
| B: v10 | 1,617.551 | 11,768,619 | 11,180,672 | 587,947 | 56,191 | 11,824,810 | 87 |

Session spans are 1,694.513 and 1,617.395 seconds. A's artifact checkpoint is
1,689.442 seconds; B saved its report at 1,582.302 and finished validation at
1,612.302 seconds. Checkpoints are not substituted for parent-observed completion.
Cached input is included in input and total, not unique source volume or billing;
reasoning is included in output (7,623 and 5,459). Actual tool-output UTF-8 volume
was 1,197,349 and 1,128,587 bytes. CLI invocations were 11 and 12 respectively,
including help. Snapshot counters are not summed.

Both used complete workspace JS and compact slot-write sites. B also issued two
actual origins queries: one remained unresolved/truncated, while the other
provided source-backed hoisted numeric-bound candidates. Help inspection alone
does not establish captures adoption; neither reports an actual captures query
or distinct site-source filter query. Both still wrote custom initializer and
register-role parsers. Literal-command auditing is not a comprehensive dynamic
command audit, and self-assessed utility is not causal effectiveness evidence.

Independent grading against the unchanged reference/rubric gives both **93**
and both **fail the full gate**. A has raw coverage 93 with no independently
confirmed material error; B has raw 95 minus two points for treating a randomized
default as fixed. Both substantially cover controls/status for both codecs, but
recoverable read-body encoding, synchronization/correlation, version-boundary,
normalization and lifecycle coverage remains incomplete. Novel details beyond
the frozen reference are unadjudicated, not certified; firmware/native unknowns
are distinct from recoverable omissions.

B is about 77 seconds (4.5%) faster in this single pair, but uses about 7.3% more
uncached input and 1.6% more output tokens. Lower recorded total is mainly cached
input. Equal scores, n=1, a longer cap than earlier rounds, and different awake
conditions do not establish a reliable speedup or justify smaller-model
promotion. The full goal remains active. The next prototype targets typed
source-expression structure rather than inferred values or domain-specific
protocol hints.

V11 adds opt-in `origins --expressions`, frozen separately as
`5527cf148eb3b1c219c432a9ad1c98acd515df0e68834e740cd8894b6cf8de28`.
The complete already-parsed exporter AST supplies bounded typed expression
graphs, not evaluated values or framework normalization. Exact source spans
join register-definition candidates and selected direct expression statements.
Literal raw syntax, symbolic register reads, call roles/order, array holes/spread,
operators and both conditional branches remain explicit. Unsupported syntax is
opaque. Helper-shaped roles are not helper-identity or API-name claims. Original
compact origins output remains unchanged without the flag.

The initial expression visitor's eight-million-frame cap rejected the large
initializer query after 1,125.4 ms with empty stdout. A 32-million-frame bounded
cap accommodates its measured 15,170,544 frames, including enum/collection
wrappers. The final query took 1,191.5, 678.4 and 702.9 ms in three isolated runs,
returning 51,601 bytes / 17 definitions / one root view / 144 projected nodes.
All node previews, original byte extents and UTF-8 spans match complete source;
no expression omissions occurred. These are microqueries, not a general query
latency guarantee or protocol-success evidence.

The first external span audit mistakenly joined raw-fragment coordinates to a
workspace file including its 280-byte inspection header. The corrected audit
uses the new explicit `expression_source` coordinate origin and raw-prefix
locator. The raw initializer is 26,241,704 bytes; the workspace file is
26,241,984 bytes. This is a coordinate-layout distinction, not a JS content
change. Future consumers need the same documented offset when joining spans.

All 463 local Rust tests pass after final integration, including syntax graphs,
caps, source-coordinate joins, ordinary-report compatibility and CLI atomic
budget errors. Formatting passes; Clippy has no diagnostics in changed source
or tests, with existing unrelated warnings retained. Telemetry tests pass
(20, one optional real-session test skipped). Ten warmed full-JS exports per
large project, without Cargo overlap, give Orbit median/worst 401.4/445.8 ms and
Modern Animal 245.4/255.6 ms. Corrected output hashes remain unchanged, and
external Node syntax checks pass outside the export timer. Process RSS sampling
was unavailable in the sandbox and is not reported as zero. No cold-agent
benefit or smaller-model promotion is inferred before a fresh qualifying trial.

A fresh awake medium-Sol pair compared frozen v10 against v11 with the same
cold prompt and 1,800-second dispatch caps. Both finalized before their caps;
the temporary AC assertion was verified and removed, and power logs show no
sleep/wake events in the interval. No sleep-adjusted time is credited.

| Trial | Parent final observation (s) | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A: v10 | 1,627.114 | 11,667,455 | 11,068,928 | 598,527 | 55,453 | 11,722,908 | 86 |
| B: v11 | 1,674.907 | 11,252,740 | 10,726,656 | 526,084 | 59,512 | 11,312,252 | 84 |

Observed session spans are 1,626.945 and 1,674.689 seconds. Self-reported artifact
checkpoints are 1,621.657 and 1,668.526 seconds, not substituted for completed
task latency. Tool-output UTF-8 volume is 1,366,904 and 1,304,509 bytes. Reasoning
is included in output (6,718 and 8,426); cached input is included in input/total,
not unique source volume or billing. Snapshot counters are not summed.

A reports 12 CLI invocations (seven help/five research), including actual
captures and property-site source filtering. B reports nine (five help/four
research), including a compact slot-write query. Both primarily used complete
workspace JS and custom static extraction. Neither issued an origins query;
**B did not use `--expressions`**. Declared commands and a literal-command session
audit agree, but the latter is not a comprehensive dynamic-command audit.
No disassembly is established in those command literals or self-reports.

B takes about 48 seconds longer, with fewer uncached input but more output
tokens. This is an interface-availability trial, not a test of expression-graph
effectiveness or a causal efficiency result. Independent frozen-rubric grading
gives A **92.5** and B **94.5**, with no independently confirmed material-error
deductions. Both **fail the full gate**, chiefly incomplete product/version/type
distinctions. Both substantially support bounded basic controls/status in both
families, but complete caller, normalization, sync/correlation and recovery
contracts remain incomplete. Novel exact request bytes and normalizer/lifecycle
claims outside the frozen audit are unadjudicated, not certified. Native/firmware
unknowns do not excuse recoverable client-evidence gaps. No model promotion is
claimed. The grader's conduct summary incorrectly says neither trial used site
source filtering: A's explicit `--match` invocation is established by its declared
commands and the parent's literal-command audit. This conduct correction does
not change protocol scores, gates or the frozen reference/rubric.
The adoption failure motivates concrete, source-derived next-query navigation
rather than another generic feature-description string or protocol-specific
hints. The full goal remains active.

For commit `45953d2`, hosted build and formatting checks pass. Test and Clippy
jobs, and the separate review bot, were cancelled without running steps;
GitHub annotations state that a hosted runner was not acquired after repeated
attempts. This is not an observed Rust/compiler failure. The failed Rust jobs
were retried, without changing workflow timeouts or code to mask provisioning
failure. Local verification above remains separate from pending hosted results.

V12 freezes concrete site follow-up navigation separately as
`9826d7a100e7f9594f9aad479c85441a7dda3e0fa2b09a4c1087814e33cb3fb0`.
`follow_up_queries` derives unique function/PC origins targets from included
typed provenance with block-entry/external register reads. It covers returned
sites only and survives compacting without mining serialized text. Fully local
reads, same-PC ambiguity alone and omitted dependencies do not manufacture
suggestions. Queries use the same input and `--expressions`; they are not shell
strings, values, capture identities or proof that normal paths execute.

One isolated real-initializer site query returned one site and one follow-up in
1,047.5 ms / 3,173 bytes. Executing its suggested query unchanged succeeded with
default budgets in 679.9 ms / 60,233 bytes, returning 18 candidate definitions
and one expression view without expression truncation. This is an unscored
navigation microcheck, not proof of protocol completeness, feature adoption or
a general sub-second query guarantee.

All 469 local Rust tests pass, including new page/dedup/compact/parity/omission
and wired CLI budget tests. Formatting passes; Clippy has no changed-source/test
diagnostics, with existing unrelated warnings retained. Telemetry tests pass
(20, one optional real-session test skipped). Ten warmed full-JS export runs per
project, with no Cargo overlap, give Orbit median/worst 418.4/437.3 ms and Modern
Animal 262.5/269.2 ms. Corrected JS hashes remain unchanged; external Node syntax
checks pass outside the timer. Latency remains below the one-second target on
these runs; small cross-round timing changes are not attributed causally to the
new navigation field. A fresh qualifying cold trial is still required before
claiming adoption, efficiency or smaller-model readiness. The full goal remains
active.

### V12 Awake Three-Agent Trial

Frozen v11 and v12 were compared with the same cold prompt and 1,800-second
dispatch caps: medium Sol on both binaries, plus an exploratory low-Sol v12 run.
All three finalized before their caps and were closed. The temporary AC-only
sleep assertion was verified, removed after completion, and power logs contain
no sleep/wake events in the trial interval. No sleep correction is credited.

| Trial | Parent final observation (s) | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A: v11 medium | 1,584.947 | 13,139,009 | 12,622,080 | 516,929 | 55,732 | 13,194,741 | 99 |
| B: v12 medium | 1,563.444 | 12,240,708 | 11,487,744 | 752,964 | 56,043 | 12,296,751 | 91 |
| C: v12 low | 1,646.250 | 14,236,739 | 13,623,552 | 613,187 | 57,877 | 14,294,616 | 108 |

Observed session spans are 1,584.749, 1,563.074 and 1,645.843 seconds. Local
artifact checkpoints (1,549.450, 1,558.278 and 1,642.016 seconds) are not
substituted for finalized task latency. Actual tool-output UTF-8 volume is
1,075,541 / 1,603,288 / 1,187,036 bytes. Reasoning tokens are included in output
(5,138 / 4,597 / 6,724); cached input is included in input/total. These cumulative
session counters are not unique source volume or billing, and snapshots are not
summed. Three simultaneous agents and periodic read-only artifact monitoring
are not isolated CLI performance conditions.

A declares nine CLI calls (five help/four research), including property-site
`--match`. B declares seven (four help/three research), but no sites query.
C declares eight (five help/three research), including a compact slot-write
query. C's actual returned JSON contains one concrete origins follow-up, which
was not followed. None used origins or typed expressions; captures/trace use
is also not established. Declared commands and periodic literal-command counts
are partial conduct evidence, not a comprehensive dynamic-command audit.
All three primarily used complete workspace JS and custom static extraction;
no disassembly use is established by those reports or command literals.

Independent grading against the unchanged frozen reference/rubric gives
**95 / 90 / 92.5**, and **all fail the full gate**. A has no independently
confirmed material-error deduction. B has two source-confirmed wire-layout/tag
errors in prose despite correct declared appendices; C applies a normalization
not supported on the cited path. Complete product/hardware/schema distinctions
and closed-loop client coverage remain incomplete. Novel precision beyond the
frozen audit is unadjudicated, not certified; native/firmware unknowns remain
distinct from recoverable client omissions.

B is about 21.5 seconds (1.4%) faster than A, but uses about 45.7% more uncached
input tokens and did not exercise the new navigation field. C is slower than
both medium trials. No causal interface win, reliable speedup, correctness
completion or Luna promotion is established. The next interface prototype
places generic navigation beside workspace JS, where agents actually work,
rather than supplying protocol-specific hints. The full goal remains active.

The hosted Rust retry for commit `64b42ff` now passes build, formatting, Clippy
and tests (including telemetry tests), run `37371706042`. Earlier no-step
cancellations were hosted-runner acquisition failures, not observed code
failures. This green retry does not retroactively validate unrun jobs or any
future commit.

### V13 Workspace-Adjacent Navigation

V13 is frozen separately as
`ad1932f146de7918c122cfb4a74eb433e7bba44b85c1974304ce7d8cec801b8b`.
Generated `GUIDE.md`, function-specific header examples and the workspace JSON
summary make provenance navigation discoverable beside complete JavaScript.
Shared structured manifest templates avoid repeating query objects in every
function entry. Each manifest/index row records `fragment_prefix_bytes` for
exact raw-fragment expression-span joins. Templates use placeholders and the
chosen executable, not interpolated input paths/function names or shell code.
Candidate uncertainty, omissions, call roles and PC-versus-UTF-8 units remain
explicit. No framework evaluation, protocol hints or source-body rewriting is
introduced.

All 473 local Rust tests pass, including hostile path/name isolation, variable
ID-width prefix accounting, exact span joins and the wired CLI summary. Formatting
passes; Clippy has no changed-code/test diagnostics, with unrelated existing
warnings retained. Telemetry tests pass (20, one optional real-session test
skipped). Ten warmed full-JS exports per project, without Cargo overlap, give
Orbit median/worst 403.2/440.5 ms and Modern Animal 253.4/325.8 ms. Corrected
output hashes are unchanged; external Node syntax checks remain outside the
timer. RSS sampling remains unavailable, not zero. An earlier Modern Animal
series overlapped a small-fixture workspace smoke check during setup and is not
used for the isolated summary above.

One sequential Orbit workspace sample per binary took v12 4,396.3 ms and v13
4,434.2 ms. Every one of 40,251 raw function bodies compares byte-identically
after its recorded inspection prefix. Header guidance adds 13,278,641 JS bytes;
manifest/index sizes rise from 2,504,333/14,774,949 to 3,637,873/15,901,977 bytes.
Workspace costs are separate from the sub-second executable bundle target.
Warm cache, ordering and filesystem effects prevent a causal latency conclusion
from this single pair. Feature adoption and research correctness still require
new cold trials; no efficiency win or smaller-model qualification is claimed.

### V13 Censored Adoption Probe

Two fresh low-reasoning Sol agents used the same cold full-protocol prompt on
frozen v12 and v13, with 1,800-second hard caps. Before dispatch, the parent
recorded a policy allowing an early stop after at least eight minutes when
interface-discovery evidence sufficed. Research was interrupted at 514.599 and
514.594 seconds; finalization was requested without further source research.
These are censored adoption probes, not completed protocol-latency comparisons.
The temporary AC assertion was verified and removed; power logs contain no
sleep/wake events in the interval. No sleep correction is credited.

| Trial | Session span through interruption/finalization (s) | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A: v12 low | 631.208 | 5,607,026 | 5,384,320 | 222,706 | 14,149 | 5,621,175 | 43 |
| B: v13 low | 615.788 | 4,894,883 | 4,595,072 | 299,811 | 22,097 | 4,916,980 | 46 |

A did not finalize within the requested window and was closed while running at
631.430 seconds. Its saved answer and in-progress measurements remain unchanged;
the parent closure record establishes censorship, not agent completion. B saved
explicitly incomplete/censored artifacts at its reported 604.045-second checkpoint
and finalized; the parent first observed completion at 616.042 seconds and closed
it at 631.287 seconds. The checkpoint is not substituted for completed task time.
Both sessions contain an explicit research interruption; A's finalization turn
was also interrupted, while B's completed. Actual tool-output UTF-8 volume is
527,755 / 651,399 bytes. Reasoning (2,126 / 1,482) is included in output and cached
input is included in input/total; these are actual cumulative session counters,
not billing, unique source volume or estimates. Counter snapshots are not summed.

A's twelve explicit literal CLI invocations include six help/six research calls,
including an actual compact slot-write site query. Because A's measurements
never finalized, this inventory comes from tool-call command literals, not a
completed self-report; dynamic wrappers are not comprehensively audited. B
declares seven calls (five help/two research): input discovery and workspace
export were used; search/captures/sites were help-only. Neither an origins nor
typed-expression query is established. Complete workspace JS remains the main
enabler; custom static text scripts handle initializer symbols, slots and schema
navigation. No app/device execution or disassembly is established by the recorded
conduct. The new guidance has not established adoption or reduced that work.

No full-protocol grading, efficiency win or model-promotion claim is made from
these early-stopped reports. They are not substituted for the earlier complete
but failed correctness trials. A more useful next prototype should address
bounded generic symbol/dependency extraction itself, not add more navigation
strings or supply private protocol hints. The full goal remains active.

At code commit `9402d42`, hosted build, formatting, Clippy and tests pass in run
`37378535016`. The separate review bot fails with an explicit expired OAuth/401
error (`37378535017`); it needs reauthentication, not a Rust code workaround.

### V14 Source-Symbol And Text Prototypes

V14 is frozen separately as
`d60d1e6ccdb2888302a2dbd77976867931cfe378dc100cbee68aa6527c4e29ce`.
`symbols` projects raw string mentions from bounded candidate slot-write RHS
dependencies across normal dispatcher blocks. A single parsed/indexed source
is reused across the page. This addresses extraction rather than another query
hint: results include shared source-span definition IDs, read/candidate links,
raw mentions and uncertainty. There is no decoded-value, keyword-constructor,
framework, lexical-frame or heap resolution. Environment expressions are
recorded separately from the searched RHS. Slot syntax is not binding identity.
Store-ordinal cursors, candidate/mention limits, unknowns and negative-result
scope are explicit. Aggregate query work is capped at 1,048,576 charged steps,
literal projection at 262,144 records / 128 MiB raw bytes, and literal inspection
at 512 records per row; omitted searches are not absence. Matching mentions are
retained ahead of other tokens inside the display cap.

A parallel prototype, `origins --text`, emits escaped line-oriented candidate
records. All returned definition/read identity links and status flags remain;
typed graph nodes are explicitly omitted by the renderer, not flattened into
values. Source/count/omission summaries remain, with an explicit warning that
local syntax graph IDs cannot be resolved in text. Use JSON for full graphs.
The requested text budget and intermediate 16 MiB JSON ceiling are distinct;
errors occur before any stdout. Ordinary origins JSON remains the original
interface. Symbol indexing is opt-in, not an added AST walk for ordinary queries.

All 488 local Rust tests pass, including cross-block alternatives, raw source
joins, raw-store cursors, environment exclusion, non-decoding of escaped string
tokens, matching-token retention beyond the display cap, explicit literal-search
omissions, wrapper preflight and both wired atomic CLI modes. Formatting passes;
Clippy has no changed-code/test diagnostics, with unrelated warnings retained.
Telemetry tests pass (20, one optional real-session test skipped).

Sequential large-initializer microchecks compare ordinary and expression-enabled
origins JSON byte-identically with frozen v13 (13,184 and 60,233 bytes). The first
v14 plain query took 1,126.0 ms versus v13 579.1 ms; three alternating repeats give
v13 585.9/574.1/577.8 ms and v14 595.1/591.7/588.0 ms. This initial outlier is
preserved, not discarded from a generic latency claim. There is no general
sub-second exploration guarantee, and the small repeated timing difference is
not causally attributed from this sample. Expression JSON queries took
683.3/709.5 ms for v13/v14. Text with expression summaries took 675.6 ms and
28,851 bytes versus 60,233 JSON bytes; the smaller output intentionally omits
syntax graph nodes and is not a lossless graph compression claim.

Two symbolic slot queries took 635.5/609.7 ms, returning 11,340/12,366 bytes,
one row each, four/six raw mentions and 18/20 candidate definitions. All 52
inspected spans join their original raw-fragment extents and previews exactly.
A raw token selected from that source evidence, without supplying its slot,
finds the target in two bounded pages (728.6 + 652.8 ms). The first page returns
no rows, scans 2,485 stores and explicitly exhausts its work budget with a next
cursor; the second scans 563 stores and returns two candidates. That first empty
page is not absence. These are source-navigation microchecks, not protocol
grading, agent adoption or efficiency evidence.

Ten warmed full-JS exports per project, without Cargo/query overlap, give Orbit
median/worst 429.6/440.9 ms and Modern Animal 273.0/290.4 ms. Corrected hashes
are unchanged and external Node syntax checks pass outside the timer. RSS
sampling remains unavailable, not zero. Small cross-round timing shifts are not
attributed to this CLI-only prototype. Fresh qualifying cold trials are still
required; the full goal remains active and Luna promotion is not established.

### Cold V13/V14 Medium-Sol Comparison

Two independent medium `gpt-6.1-sol` agents received the same cold JS-primary
task and dispatch-inclusive 30-minute cap, using the separately frozen v13 and
v14 binaries. Both finalized before the cap and were closed after terminal
completion. Neither trial was censored. A common non-interrupting deadline
reminder supplied no protocol hints. The independent frozen grader scored A
95.5 and B 95, but both full gates fail: required version distinctions and
complete client control/query coverage remain incomplete. One exact read-byte
prescription is unverified against the frozen audit, not a newly proven device
rejection. Precision outside that audit is not retroactively certified.

| Trial | Parent terminal observation (s) | Session span (s) | Input tokens | Cached input | Uncached input | Output tokens | Recorded total | Tools |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| A: v13 medium | 1,636.274 | 1,636.228 | 13,628,137 | 13,046,016 | 582,121 | 61,086 | 13,689,223 | 101 |
| B: v14 medium | 1,565.544 | 1,565.455 | 14,730,644 | 14,077,184 | 653,460 | 55,858 | 14,786,502 | 104 |

These are actual final cumulative session counters, not summed snapshots,
unique evidence volume or billing estimates. Reasoning tokens (7,965 / 6,181)
are included in output; cached input is included in input and total. Actual
tool-output UTF-8 volume is 1,285,057 / 1,470,217 bytes. Each session has an
explicit task completion and no interruption/timeout marker. Agent artifact
checkpoints (1,631.030 / 1,558.959 seconds dispatch-inclusive) exclude a few
seconds of final write/reply overhead and do not replace terminal task time.

A reports 13 CLI invocations including actual compact PC-bounded call sites,
batched JS and captures. B reports 14 including actual symbols, captures and
origins text queries; its show call is help-only. Neither uses typed expressions.
B reports text provenance helped; its filtered symbols query returns no rows,
exhausts the work budget, scans 2,474 of 7,398 stores and supplies a next cursor.
It does not continue that scan and instead uses custom static text extraction.
That empty page is not negative protocol evidence. Complete JS plus bespoke
initializer/slot/schema extraction still dominates both workflows.

The temporary AC sleep assertion was verified and removed after trial closure;
the power-log interval contains no sleep/wake events. Parent guidance edits and
short Cargo verification overlapped early research but changed neither frozen
binary. These parallel workflow measurements are not isolated query benchmarks.
The modest wall-time difference, higher recorded input for B and failed gates
do not establish a causal efficiency improvement or qualify Luna promotion.
Private dispatch/closure/telemetry/grade artifacts and original sessions remain
preserved; no protocol reports or proprietary bundle are committed.

At prototype commit `4603da9`, hosted build/fmt/Clippy/test checks pass in run
`37384048672`. At guidance commit `74163e8`, they pass in `37385971508`;
25 focused workspace/CLI tests and all 488 Rust tests also pass locally.
The separate prototype review-bot log explicitly reports expired OAuth/401
(`37384048786`); code checks are not blocked by that credential failure.

### V15 Scan Controls And Literal Locations

V15 is separately frozen as
`7a1df9dd68f0370b0ee3f9778a28c2ccf043726cff683e8ff4b5184a04464a0e`.
`symbols --scan-work` controls aggregate dependency work up to 16,777,216;
the original 1,048,576 default is retained. Unfinished pages return structured
continuation arguments preserving cursor, filters and limits. Uncertain
filtered-out rows are counted, and paging does not repair earlier omissions.

Bounded `filter_literal_diagnostics` separately checks indexed instruction-string
syntax (not identifiers, nested function bodies or runtime values). Counts,
scope, completeness and a 33,554,432 charged-work ceiling are explicit; input
bytes are charged before case conversion and expanded bytes before comparisons.
Each term has at most three PC-labelled raw-span examples, independently capped.
Only a complete zero-match diagnostic skips dependency queries, with skipped
counts explicit; a partial diagnostic never enables that shortcut. Literal
locations are not store associations, argument identities or evaluated values.

All 493 Rust tests pass, including empty-page continuation, hostile argument
tokens, preflight scan bounds, filtered-out unknowns, diagnostic scope versus
RHS scope, complete/partial zero-match guards and independent example limits.
Formatting passes and Clippy has no changed-code/test diagnostics; unrelated
warnings remain. A wired-test assumption that function zero had multiple stores
failed initially; the repaired test selects a fixture function that actually
exercises continuation. No dependency or executable bundle/runtime code changes.

Private sequential microchecks preserve the unsuccessful larger-budget attempt:
the same completed-agent query at 1,048,576 / 4,194,304 / 16,777,216 work scans
2,474 / 4,737 / all 7,398 stores but returns no RHS rows. Three warmed timings
are 619.0/608.0/618.8, 794.0/791.6/799.2 and 1,001.5/1,005.0/1,011.6 ms.
The larger default was not retained. An initial smaller diagnostic cap was
incomplete after 62,051 of 118,181 tokens and did not skip any queries; its
1,029.4 ms first query and 630.1/630.3 ms repeats remain preserved.

The final diagnostic scans all 118,181 indexed tokens, finding one occurrence
per requested term outside the returned bounded RHS evidence. Original JS
context shows property-helper keys, not slot-write RHS labels: this is a scope
gap, not absence or proof of a slot value. The returned PC examples offer direct
JS navigation, including existing property-write sites; no protocol hints are
baked into the binary. The final same-query timings are 1,041.5/668.3/661.9 ms
and 4,566 output bytes. No generic sub-second query guarantee or speed win is
claimed. Positive slot queries take 568.9/573.9 ms, with candidate rows and
definitions identical to v14; all 70 inspected candidate/example spans join
the raw JS exactly. These are microchecks, not fresh cold-agent qualification.

Ten isolated warmed full-JS exports per large input give Orbit median/worst
402.2/435.0 ms and Modern Animal 245.8/259.4 ms. All outputs retain the corrected
hashes and external Node syntax checks pass outside timing. RSS is unavailable,
not zero. Full-project startup/read/parse/lower/validate/write remains below one
second; small cross-round changes are not causally attributed to CLI analysis.
The next trial must test actual use of literal locations/property-role evidence
and complete version/control coverage, not count an empty page as a failed
literal search. The goal remains active; no Luna promotion is established.

### V16 Property Key/Value Candidates

V16 is frozen separately as
`4b61cab9aea4995a2e318afa305cef59770ac6dc541302a5e30f14f2cec6e581`.
The generic `properties` view searches raw property-key syntax and bounded
normal-flow key-definition candidates, with object, key, value and helper
arguments kept separate. It reuses one complete parsed function index per page.
Shared raw-span IDs retain alternatives and unknowns; no property values,
object identities, helper semantics or captured frames are evaluated.
Raw store-ordinal continuation preserves options as separate argument tokens.
Key/value aggregate work and raw filter-byte work are separately bounded;
unqueried values are explicit and earlier omissions are not repaired by paging.
Dependency omissions do not inherit the unused normal-edge display cap.

All 522 Rust tests pass, including helper shape/arity rejection, class-scope
rejection, cross-block alternatives, key versus value/object filter scope,
UTF-8 joins, paging after empty pages, explicit exhausted value roles, exact
output budgets and filter-work exhaustion. Review found uncharged read-list
scans and deferred class initializers leaking into enclosing-PC evidence.
Properties-only sorted scope lookups and fail-closed class handling fix both;
regressions exercise 20,000 same-PC skipped stores and 1,000 depth-zero dynamic
key queries. Ordinary origins/symbols traversal and JSON remain unchanged.
Formatting passes; Clippy has no changed-code/test diagnostics, with unrelated
warnings retained. Telemetry tests pass 20 with one optional real-session skip.

A private sequential replay of the earlier real agent query now scans all
36,290 property stores and returns six key/value records, versus no slot-RHS
records in symbols. Timings are 1,063.6/640.9/646.0 ms, with 50,591 output
bytes and 729 successful source joins. All literal keys are untruncated; five
value candidate queries remain depth-truncated and one is untruncated. These
are source candidates, not certified protocol values or a cold-agent result.
Positive symbols checks preserve v14 rows/definitions exactly with 70 source
joins, taking 636.2/627.4 ms. First-run outliers are retained; no generic
sub-second query guarantee or causal speed improvement is claimed.

Ten isolated warmed full-JS exports per large input give Orbit median/worst
426.0/462.3 ms and Modern Animal 296.9/320.3 ms. Startup/read/parse/all-function
lowering/internal validation/write are timed; external Node syntax checks pass
outside the timer. Both corrected output hashes remain unchanged, and RSS is
unavailable, not zero. No Cargo/query/trial work overlaps these latency runs.

A new three-arm cold experiment is dispatched with identical research prompts:
v15 medium, v16 medium, and exploratory v16 low, all 6.1 Sol. Each has an
independent frozen binary/output directory and a 30-minute dispatch-inclusive
cap. No source IDs, protocol constants or grader findings are provided. Actual
token telemetry and unchanged independent grading are required before any
efficiency or full-protocol claim. Completed results follow; the goal remains
active and Luna remains unqualified.

### Completed V15/V16 Medium/Low Cold Comparison

All three agents completed before the 1,800-second cap, with explicit session
completion markers and no interruption/timeout. The private frozen binaries
and reference/rubric hashes remain unchanged. Owned temporary awake assertion
was active on AC during research and released afterward; no sleep/wake events
occurred in the trial interval. The user's separate assertion was untouched.

| Trial | Frozen CLI | 6.1 Sol Effort | Parent Wall Seconds | Session Wall Seconds | Score | Full Gate |
|---|---|---|---:|---:|---:|---|
| A | v15 | medium | 1598.981 | 1598.944 | 92.5 | fail |
| B | v16 | medium | 1349.722 | 1349.665 | 93.5 | fail |
| C | v16 | low | 1747.301 | 1747.198 | 92.5 | fail |

Parent timings include dispatch through observed terminal status, including
notification handling. Agent artifact clock samples are separate and earlier
than terminal observation. Only still-running A/C received the same generic
time reminder; B had already completed. No protocol hints or changed criteria
were supplied. Trials were concurrent on one host, not isolated causal tests.

| Trial | Tool Calls | Tool Output Bytes | Actual Input Tokens | Cached Input | Uncached Input | Output Tokens |
|---|---:|---:|---:|---:|---:|---:|
| A | 105 | 1369638 | 15936766 | 15055104 | 881662 | 65852 |
| B | 89 | 1248972 | 12163538 | 11604096 | 559442 | 55068 |
| C | 114 | 1549535 | 15782134 | 14823680 | 958454 | 72727 |

These are final recorded cumulative counters, not estimated or summed snapshots.
Cached input is included in input, and reasoning is included in output. Recorded
reasoning counts are 9,108/9,458/8,854; total tokens are
16,002,618/12,218,606/15,854,861. Tool calls include non-CLI research tools, while
self-reported CLI calls are 18/17/11. Neither metric is unique evidence size or
a billing estimate; no token counter decreases or missing completion markers.

Independent frozen grading credits only answer.md and explicitly declared
appendices, not hidden logs, workspace reconstruction or undeclared scratch.
All three support bounded core control/status mappings for both families, but
all omit required model/version interpretation. B also omits some client-visible
update-format distinctions. A incurs a two-point advertisement byte-extent
error; C incurs a two-point unsupported numeric-coercion error. Neither is
classified as an invented fundamental wire/control protocol. Native/firmware
unknowns remain separate from these recoverable client omissions, and novel
precision outside the frozen completed audit is unadjudicated, not certified.

A actually uses symbols and origins text; sites/captures/show are help-only.
B uses one scoped properties data query, compact filtered sites and origins,
but workspace JS and bespoke initializer extraction remain primary. C uses
workspace/search/captures/origins; its narrative mentions scoped properties,
but no actual properties command is reported, so adoption is not established.
Typed-expression adoption is not established in any arm. No disassembly is
reported as research evidence. More report rows/types do not close semantic
mappings or automatically satisfy the gate.

B has lower observed time and tokens in this round, but a failed full gate and
single concurrent comparison do not establish a successful causal efficiency
win. Low effort is slower and has more recorded output than B here; it is not
a demonstrated cost/reliability improvement. No Luna promotion is justified.
The common bottleneck remains bespoke static initializer/argument/field
reconstruction plus operation-level direction/unit/version cross-checking.
The next generic CLI experiment should reduce that extraction work without
turning source candidates into evaluated framework values or adding app hints.
The goal remains active, and all research/evaluator agents are closed.

At implementation commit `462f787`, hosted build/fmt/Clippy/test checks pass
in `37393756939`. The separate current review workflow `37393756701` explicitly
fails with expired OAuth/401 authentication_error and needs reauthentication,
not a code bypass. All 522 local Rust tests and corrected telemetry test
invocation pass; an initial mistyped telemetry test path ran no tests and is
not counted as successful verification. No executable bundle/runtime or
dependency changes in this iteration; full-JS performance evidence above
predates research and retains the corrected output hashes.
