# Current Status and Roadmap

The implementation uses HBC parsing, CFG/SSA analysis, a control-flow plan, and
OXC AST generation. Structured output covers tested conditionals, dense/sparse
switches, while/do-while loops, for-in enumeration, and selected try/catch patterns.
CommonJS/Metro analysis reports modules, dependencies, entrypoints, and heuristic
layout suggestions. CLI decompilation defaults to an executable full bundle;
`--function N` selects the existing structured single-function backend.

Whole-bundle lowering preserves physical registers and bytecode control flow,
including original exception-table priority, iterator cleanup, lexical capture,
generators, async resume and CommonJS loading. Independent functions are lowered
and syntax-checked in parallel, with deterministic output order. Unknown semantics
fail export rather than emitting placeholders. See
[validation evidence and commands](bundle-validation.md).

The executable-export checkpoint passes 326 tests with none ignored, differential
workloads from five public projects on Hermes 90/96, and both large fixture
throughput checks (20 warm exports each under one second on the measured M2 Max).
Native app boot, universal reflection equivalence, original file layout, and
low-memory export are not claimed by this checkpoint.

## Stabilization

- Keep corpus CFG invariant checks separate from instruction rendering snapshots.
  Existing DOT files pin graph nodes, edges, and edge kinds. A small representative
  fixture pins exact DOT rendering. Tests must not silently create missing goldens.
- For exception loops that the structurer cannot safely raise, use instruction
  dispatch with mutable bytecode registers and original handler ranges. This
  preserves compiled finally paths and nested handlers at the cost of readability.
- Compare generated JavaScript with source fixtures at runtime, including caught
  errors, rethrows, critical exits, and cleanup on success and failure.
- Disassembly rendering tests compare committed CLI `.hasm` snapshots in temporary
  directories. `.hasm.expected` files remain reference Hermes output with a
  different format and are not overwritten by tests.

The exception dispatcher uses the first matching protected range in table order,
as implemented by Hermes in
[BytecodeDataProvider.cpp](https://github.com/facebook/hermes/blob/main/lib/BCGen/HBC/BytecodeDataProvider.cpp).

## Next Milestones

1. Raise complex exception dispatch to readable loops and try/catch/finally once
   equivalent behavior is established. Bundle regressions now cover exhaustion,
   early break, thrown next(), throwing return(), and cleanup exception priority.
2. Extend bundle validation to additional real apps with native-host harnesses,
   more bytecode versions, and reflective/exotic object behavior. CLI contracts
   and the library full-bundle return path are implemented; package/file naming
   and reconstruction remain separate from executable export.
3. Expand semantic tests for safe optimizations, closures, captured environments,
   and serialized literals. Replace remaining tests that only check output strings
   or skip absent fixtures with behavioral assertions where practical.
4. Harden parser error paths and document tested opcode/version coverage. The
   registry ends at HBC 96; newer versions require generated definitions and tests.
5. Maintain the sub-second full-bundle release target, track latency and memory
   with `scripts/benchmark_bundle.mjs`, and reduce existing Clippy warnings.

## Verification

```bash
cargo fmt --all --check
cargo build --all-targets --all-features
cargo test --all-features
cargo clippy --all-targets --all-features -- -W clippy::all
```

Runtime tests require Node.js. Tests should use temporary directories for generated
disassembly. Run `cargo audit` when changing dependencies.
