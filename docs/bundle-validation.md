# Bundle Validation

## What Is Tested

`cargo test --test bundle` exports every function in the small committed HBC
corpus. Executable comparisons check shared and independent captured environments,
exceptions/finally, dense switches, loops, custom iterator getters and cleanup,
generators (next/throw/return/delegation, shared methods, receiver checks and
prototype changes), async closures and tagging, construction/new.target,
nonconstructable functions, strict this, arguments/rest, symbols, templates and
BigInt. Source comparisons run both plain and minified exports. CommonJS tests
check dependency loading, cycles and cache identity. CLI failures must preserve
previous output. HBC 90 and 96 fixtures check version-dependent semantics.

Hermes DirectEval has an isolated scope, unlike Node's normal direct eval. Its
test uses an actual Hermes baseline. Likewise, strict arguments have a caller
poison accessor, and the built-in array-iterator fast path has no iterator object
to close. The `bundle_hermes_semantics*.expected.json` snapshots come from actual
Hermes 90 and 96 execution, not inferred Node source behavior. Their arguments
and array-iterator results agree. They also check arrow-function prototype
properties: HBC 90 exposes one
despite prohibiting construction; HBC 96 does not. Hermes permits construction
of the tested object-literal method in both versions, unlike Node source.

The overflowed-function-header regression checks the exception table starts just
after the large header. HBC 95+ DirectEval's extra strictness byte is consumed
explicitly because unified instruction metadata still describes the older shape.

## Public Projects

The offline harness uses these pinned npm archives, retaining their licenses
(MIT except TypeScript, which is Apache-2.0):

| Project | Version | Compiled functions | SHA-256 of archive |
| --- | --- | ---: | --- |
| Lodash | 4.17.21 | 696 | `6a087ac9e5702a0c9d60fbcd48696012646ec8df1491dea472b150e79fcaf804` |
| Immutable | 4.3.7 | 704 | `def89fdd1c1cfdf037ef4ae87f30bb332ef7df8cd668195d91fecdecd836aa61` |
| Moment | 2.30.1 | 360 | `52219a9fee5e1faade4c72536c173c54cedd5e2619272dd0c251a30aeafcde8c` |
| TypeScript | 5.9.3 | 21,604 | `10e108c9cf7d5f2879053dff18515fb405abf2ccef63eaaf017d9c571687a1d3` |
| Babel standalone | 7.28.4 | 7,574 | `abe1d3dfe38b902afc7a4da8d6b9a0ea23e5a795e39d94bf09a23b0197c42a17` |

For each project the harness executes a deterministic workload in Node source,
compiles the complete library plus workload with Hermes `-O`, runs that HBC in
Hermes, exports all functions, and compares the exported JavaScript execution
with both baselines. It records compiler version, source/HBC/result checksums,
function count, output size and timings in `report.json`. TypeScript and Babel
exercise compiler/transformation workloads, including typed syntax, JSX, classes,
async and generators. These are selected operations, not exhaustive upstream
test suites. Lodash dynamic templates are
excluded because the original Hermes execution fails to compile their generated
Function source; this is not treated as an exporter failure or success.

The full TypeScript and Babel sources are downleveled to ES5 by the pinned
TypeScript compiler because these Hermes compilers do not accept class syntax.
Node execution checks preprocessing against the original source before HBC
compilation; the transformed-source checksum is recorded. Babel runs with a
minimal shared console host in all three baselines, comparing captured logs as
well as transformation results. This host is not a substitute React Native host.

Fetch archives separately; normal Rust tests never need network access:

```bash
npm pack lodash@4.17.21 immutable@4.3.7 moment@2.30.1 \
  typescript@5.9.3 @babel/standalone@7.28.4 \
  --ignore-scripts --pack-destination /tmp/hermes-large-projects
node scripts/validate_bundle_projects.mjs /tmp/hermes-large-projects \
  /path/to/hermesc /path/to/hermes target/release/hermes-dec-rs \
  /tmp/hermes-bundle-validation
```

Append `--reuse-inputs` on later runs with the same output directory to reuse
compiled HBC and downleveled sources. Archive, source, transformed-source and HBC
checksums and compiler version must match the previous passing report. Original
Node source, transformed Node source, native Hermes and new exports all execute
again; no behavioral validation is skipped.

Repeat with Hermes HBC 90 and 96 compiler/runtime pairs. Large sources and output
remain outside the repository. Small fixtures can be regenerated using
`hermesc -O -emit-binary -out OUTPUT.hbc INPUT.js`; CommonJS fixtures use
`-commonjs` from `data/bundle_modules`, listing entry.js, math.js and cycle.js.

The additional fixture harness recompiles semantic fixtures and the 6.5 MB
massive-literal source, comparing each complete export with actual Hermes
execution. Node source is also checked where the source semantics agree with
Hermes; DirectEval and strict-arguments cases use Hermes as the authority:

```bash
node scripts/validate_bundle_fixtures.mjs /path/to/hermesc /path/to/hermes \
  target/release/hermes-dec-rs /tmp/hermes-fixture-validation
```

## Performance

The local Orbit HBC 90 fixture contains 40,251 functions; Modern Animal HBC 96
contains 49,647. Export includes the
entire table, runtime and original entrypoint; it is not a first-function sample.
The initial serial exporter took 7.47 seconds and emitted about 151 MB. Borrowed
instruction caches, direct operand extraction, shared escaped-string caches,
balanced large-first jobs with reusable per-job syntax-parser arenas, a
bundle-only parser that omits unused CFG indexes, and straight-line case
compaction reduce work without dropping
functions or bypassing syntax validation. Exception-bearing functions still
record the exact instruction PC before every operation.

```bash
cargo build --release
node scripts/benchmark_bundle.mjs target/release/hermes-dec-rs \
  data/large_test_files/com.orbit.orbitsmarthome_3.0.41-1263_4arch_7dpi_24lang_eee1ad5ef236d8570b12a434217e0e36.hbc \
  /tmp/hermes-orbit-benchmark 10 1000
```

This measures child-process wall time including startup, read, parse, export,
syntax validation and file output. All timed runs use identical full checks.
The first run is recorded separately; it is not an OS-cache-flushed cold run.
Warm-run median, p95, worst time and all-runs-under-budget status are reported.
An extra run samples RSS via ps and is excluded from latency statistics. Node
checks syntax, and hashes check deterministic output. CPU/thread settings and
input/binary checksums make comparisons attributable. Timing thresholds are
hardware-dependent and deliberately opt-in rather than flaky default tests.
Minification has an additional whole-output parse/codegen cost and is not the
default throughput target.

Criterion now has explicitly named `full_bundle_parse_lower_validate` benchmarks,
in addition to the old `decompiler_creation` microbenchmark. Those API benchmarks
read input before timing and use the general parser, including analysis indexes;
they do not measure file I/O or replace the end-to-end CLI benchmark. Set
`HERMES_BENCH_LARGE=1` to include local large fixtures.

## Verified Checkpoint

On 2026-10-05, all 326 Rust tests passed with zero ignored. Formatting, all-target
build and Clippy completed; existing repository Clippy warnings remain. The five
public projects and five semantic/stress fixtures pass complete-export execution
comparisons under both Hermes 90 and 96. Small behavioral tests also check plain
and minified output. The nested-conditional tests formerly ignored now execute
2,401 input combinations; conditional-chain identity and else-if lowering were
repaired, and the empty-block PC-range test was restored.

Final release CLI measurements on an Apple M2 Max (12 logical CPUs, default Rayon
threads), with syntax validation retained and output determinism checked:

| Input | Functions | JS bytes | First run | Warm median | Warm p95 | Warm worst | Sampled RSS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Orbit HBC 90 | 40,251 | 78,606,965 | 829 ms | 764 ms | 971 ms | 972 ms | 531 MiB |
| Modern Animal HBC 96 | 49,647 | 64,041,722 | 262 ms | 448 ms | 555 ms | 578 ms | 317 MiB |

Each input had 20 measured warm exports, all below 1,000 ms, plus the separately
recorded first and memory-sampling runs. Earlier configurations did exceed the
budget; these are measured results, not a universal latency guarantee. The
remaining Orbit margin is small, and memory is still significant. Avoid
comparing runs during concurrent compilers; continue tracking load, latency and
RSS on future changes. Machine-readable checksums, toolchain identities and
individual timing samples are retained in [bundle-validation-results.json](bundle-validation-results.json).

## Limits

Orbit and Modern Animal export completely and pass syntax checks, but their app
behavior needs the original React Native host; neither has been validated by
booting that host.
Opaque function/module names are supported, not recovered original source layout.
Known-version builtin mappings are explicit; unverified builtin versions fail
closed. HBC 90 and 96 provide behavioral evidence, not universal certification.
Exotic proxy/constructor reflection, cross-native generator branding, external host
intrinsics and arbitrary malformed HBC remain areas for deeper validation.

The runtime targets modern JavaScript with BigInt, generators, Symbol, Reflect,
WeakMap and Promise. It must run in the same realm as its required host globals.
DirectEval and lazily reified non-strict arguments need dynamic Function support;
strict CSP environments need an alternate runtime for those operations. Execution
must occur in an appropriately isolated host for untrusted bytecode. Syntax
validation does not make bundle execution safe or establish semantic correctness.
