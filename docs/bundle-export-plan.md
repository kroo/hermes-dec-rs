# Reliable Full-Bundle Export

## Goal and Acceptance

Export every function in an HBC file as executable JavaScript, with opaque stable
function names acceptable. Preserve closures, environments, arguments, calls,
construction, exception table priority, iterator cleanup, and module bootstrap.
Do not substitute placeholders for unsupported instructions. Export must fail
before replacing output when semantics are not implemented.

Correctness is established by differential execution against Hermes bytecode,
not by syntax checks alone. Cover existing fixtures, dedicated semantic edge
cases, and several large reproducible workloads. Large proprietary app startup
needs its native host and is not evidence of full correctness when only parsed.

## Design

The existing structured single-function decompiler remains available. A separate
correctness-first backend lowers instructions to JavaScript statements over
physical registers and byte-offset dispatch. All functions share a private
closure factory and lexical environment runtime; only the HBC entrypoint runs.
Original handler ranges retain compiler-generated finally paths. Unknown opcodes
produce contextual errors, never silently skipped instructions. High-level
exception reconstruction is an incremental readability improvement after this
backend provides an executable reference.

## Steps

1. Add full-bundle API and CLI with explicit supported options, closure/environment
   wiring, stable IDs, and output validation. Avoid global SSA analysis for export.
2. Implement opcode semantics and runtime helpers from the matching Hermes
   interpreter, including exceptions, iterator cleanup, generators, and builtins.
3. Add deterministic behavioral regressions and a reproducible Hermes/Node
   differential harness. Keep network access outside tests and fixtures licensed.
4. Export and exercise multiple large projects; report function/opcode coverage,
   execution results, and any native-host limitations separately.
5. Run existing tests, formatting, build and lint checks; update user-facing docs
   with verified compatibility and remaining limitations.

## Verification

Use local HBC 90 and 96 Hermes tools when available. Small committed compiled
fixtures keep normal CI deterministic without requiring a compiler or network.
Optional large-project runs must identify exact source versions, checksums,
compiler settings and observable behavior. Node syntax validity and complete
function coverage are necessary but not sufficient for semantic correctness.

## Performance Acceptance

Target full-project export in less than one second on both local large_test_files
bundles using a release build. Measure parsing, lowering, syntax validation, and
output I/O separately, plus end-to-end wall time and peak memory. Report the first
run separately from warm runs; do not label it cache-flushed cold. This is an
acceptance target, not an established result.
Do not meet it by dropping functions, skipping unsupported instructions, or
removing correctness checks. Public library workloads are additional correctness
checks, not substitutes for the large app benchmark.

## Completed Checkpoint

The executable-export acceptance suite passes: all 326 existing/new tests, five
public projects on HBC 90 and 96, and native differential semantic/stress probes.
Both local large app fixtures export every function and pass syntax checks; their
native-host startup remains unverified. The final release build exports each
large fixture below one second in all 20 measured warm runs, with stage timing,
memory samples and checksums retained. See [validation](bundle-validation.md) for
measured conditions, supported semantics and remaining limits. Readable structured
exception reconstruction and original package/file naming remain future work.
