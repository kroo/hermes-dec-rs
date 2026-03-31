# Auto

## Configuration
- **Artifacts Path**: {@artifacts_path} → `.zenflow/tasks/{task_id}`

---

## Agent Instructions

Ask the user questions when anything is unclear or needs their input. This includes:
- Ambiguous or incomplete requirements
- Technical decisions that affect architecture or user experience
- Trade-offs that require business context

Do not make assumptions on important decisions — get clarification first.

---

## Workflow Steps

### [x] Step: Implementation
<!-- chat-id: 7a47b713-67ca-4d7b-abc4-32fc24c052a5 -->

The milestone issue already exists as task `#64` with the requested title, branch context, scope, acceptance criteria, and verification commands. This implementation step therefore records completion of the milestone-creation task itself, with no repository code changes required.

**Debug requests, questions, and investigations:** answer or investigate first. Do not create a plan upfront — the user needs an answer, not a plan. A plan may become relevant later once the investigation reveals what needs to change.

**For all other tasks**, before writing any code, assess the scope of the actual change (not the prompt length — a one-sentence prompt can describe a large feature). Scale your approach:

- **Trivial** (typo, config tweak, single obvious change): implement directly, no plan needed.
- **Small** (a few files, clear what to do): write 2–3 sentences in `plan.md` describing what and why, then implement. No substeps.
- **Medium** (multiple components, design decisions, edge cases): write a plan in `plan.md` with requirements, affected files, key decisions, verification. Break into 3–5 steps.
- **Large** (new feature, cross-cutting, unclear scope): gather requirements and write a technical spec first (`requirements.md`, `spec.md` in `{@artifacts_path}/`). Then write `plan.md` with concrete steps referencing the spec.

**Skip planning and implement directly when** the task is trivial, or the user explicitly asks to "just do it" / gives a clear direct instruction.

To reflect the actual purpose of the first step, you can rename it to something more relevant (e.g., Planning, Investigation). Do NOT remove meta information like comments for any step.

Rule of thumb for step size: each step = a coherent unit of work (component, endpoint, test suite). Not too granular (single function), not too broad (entire feature). Unit tests are part of each step, not separate.

Update `{@artifacts_path}/plan.md`.

### [x] Step: Baseline the current control-flow regressions
<!-- chat-id: cd6039fa-3041-467d-a5b8-7852cae872fc -->
Capture the current behavior for the milestone fixtures before changing logic. Focus on `data/dense_switch_test.hbc` function 8, `data/while_loop.hbc` function 1, the existing sparse-switch cases, and the exception-heavy control-flow fixtures so we know which failures are reconstruction bugs versus missing assertions.

Baseline captured on the current `pr59-rebased` worktree without changing reconstruction logic:

- `cargo test dense_switch -- --nocapture` passes, but the coverage is still limited to disassembly/parsing shape checks and does not assert structured decompilation for nested switches or join cleanup.
- `cargo test --test loop_integration -- --nocapture` fails in `test_while_loop_round_trip`: loop analysis returns `[DoWhile]` for `data/while_loop.hbc` function `1` where the test expects `[While]`.
- `cargo run -- decompile data/while_loop.hbc --function 1` currently emits a `do { ... } while (var3_a < var1);` loop, confirming the misclassification in decompiled output.
- `cargo run -- decompile data/dense_switch_test.hbc --function 8` reconstructs the nested inner switches, but `case 1` duplicates unreachable tail statements after the inner `switch` (`const var3_b = ... return ...; const var3_c = ... return ...;`).
- `cargo run -- decompile data/dense_switch_test.hbc --function 2` shows the existing large sparse switch decompiling cleanly as a `switch`, so the current sparse-switch baseline is not a reproduced crash on that fixture.
- `cargo test --test sparse_switch_converter -- --nocapture` passes only as a scaffold: the test suite reports missing `data/sparse_switch_*.hbc` fixtures and skips the integration path, so current sparse-switch coverage is mostly placeholder and cannot yet prove traversal safety.
- `cargo run -- decompile data/complex_control_flow.hbc --function 7` (`exceptionHandlingControlFlow`) and `cargo run -- decompile data/try_catch_test.hbc --function 4` (`tryInLoop`) both show exception-region reconstruction flattening. The emitted output contains empty or truncated `try` regions, missing loop structure, and incomplete control-flow after catches.
- `cargo run -- decompile data/dense_switch_test.hbc --function 9` (`switchWithTryCatch`) further shows the same exception-region degradation: it reduces to `try {} catch (...) { ... }` and loses the switch body entirely.

### [x] Step: Stabilize nested switch reconstruction and join handling
<!-- chat-id: 31f13587-881a-4174-980b-466d66cc5fdd -->
Finish the work in the control-flow planner/converter so nested switches inside conditional branches or case bodies reconstruct as structured regions and do not duplicate dead tails after inner joins. The main code paths here are the planner and AST conversion layers already modified on this branch.

Follow-up landed here: refresh per-case processed-block tracking after nested structures so inner switch join/tail blocks do not leak back into the enclosing case body, fix sparse-switch discriminator recovery so multi-parameter dispatch blocks stop selecting the wrong `LoadParam`, and recover switch metadata from valid sparse `SwitchRegion`s instead of panicking when the full analyzer declines them. Verified with `cargo run -- decompile data/dense_switch_test.hbc --function 8`, `cargo run -- decompile data/complex_control_flow.hbc --function 4`, `cargo test test_dense_switch_nested_join_regression -- --nocapture`, and `cargo test test_switch_with_fallthrough_decompiles -- --nocapture`.

### [x] Step: Fix loop classification and exception-region loop reconstruction
<!-- chat-id: bc95ffb7-f84a-4230-9daf-92eef7122978 -->
Tighten loop-shape detection so real `while` loops stop degrading into `do-while`, then route loop-containing exception regions through the same loop-aware reconstruction path instead of flattening them. This step also decides and documents the expected fallback for `for-in` and `for-of` when full AST lowering is still not possible.

Current follow-up landed on this step: preserve `GetPNameList` / `IteratorBegin` setup blocks in the control-flow plan and route iterator loops through an explicit `while (true) { ... if (exit) break; ... }` fallback instead of a broken plain `while (cond)`. `ForOf` fallback now derives its break test from the `IteratorNext` result rather than the generic loop-header comparison, keeps the synthetic iterator-close region as a lowered `try { while (true) { ... } } finally { iterator.return && iterator.return(); }` cleanup wrapper, and is covered by a concrete regression on `data/ast-04-tests/test_rest_params.hbc`. `ForIn` no longer emits the broken non-advancing pseudo-loop; it now bails out explicitly with `throw new Error("Unsupported for-in loop fallback")`, and sequential conversion treats that bailout as terminal so later loop structures are not emitted as dead code. The loop-aware exception fast-path is gated on actual loop intersection instead of every single-region handler, and the exception-root reconstruction now skips regions already consumed by nested loop-aware expansion so `data/try_catch_test.hbc` function `4` no longer appends a duplicated trailing top-level `try/catch`. Loop classification also now distinguishes header-local conditionals from real loop exits: when the header branches only within the loop and the back-edge block owns the exit test, reconstruction emits a bottom-tested loop, so `tryInLoop` now decompiles as `do { ... } while (var6 < var5);` instead of the bogus `while (var0 === var1)`. Constant initialization in that path now respects prior declarations, reducing the old `let var6; let var6 = 0;` corruption to `let var6;` followed by `var6 = 0;`. Single-block loop headers now stay classified as `While` instead of degrading into `DoWhile`, and `tests/loop_integration.rs` now contains non-optional fixture assertions for the concrete `loop_types.hbc` loop mix, the documented `for-in` bailout, the `for-of` cleanup/exit shape, and the absence of duplicated `tryInLoop` declarations and trailing handler output. Verified with `cargo fmt --all --check`, `cargo test --test loop_integration -- --nocapture`, `cargo run --quiet -- decompile data/while_loop.hbc --function 1`, `cargo run --quiet -- decompile data/try_catch_test.hbc --function 4`, `cargo run --quiet -- decompile data/simple_loops.hbc --function 1`, `cargo run --quiet -- decompile data/loop_types.hbc --function 1`, and `cargo run --quiet -- decompile data/complex_control_flow.hbc --function 7`.

### [x] Step: Replace sparse-switch depth guards with structural traversal
<!-- chat-id: 0c97ad42-fdff-49bf-a5ea-92934bb3b519 -->
Remove the ad hoc recursion-depth bailout behavior in sparse-switch analysis and replace it with traversal that is structurally bounded by the CFG and join relationships. Preserve explicit, tested bailouts only for genuinely unsupported patterns such as exception interference or irreducible flow.

Current follow-up landed on this step: move the structural bound into sparse-switch detection itself so comparison-chain discovery now tracks visited comparison blocks, bails out explicitly on overlap/cycles instead of truncating the chain, and is no longer dependent on planner-side depth caps. Coverage now includes a real decompilation regression for `data/dense_switch_test.hbc` function `2` plus detector-level regressions for a 12-comparison sparse-switch chain and a cyclic unsupported shape. Verified with `cargo fmt --all`, `cargo test test_large_sparse_switch_fixture_decompiles_as_switch -- --nocapture`, and `cargo test sparse_switch_detector -- --nocapture`.

### [ ] Step: Convert fixtures into regression tests and run targeted verification
Promote the current loop and nested-control-flow fixtures from optional or placeholder coverage into meaningful assertions in `tests/loop_integration.rs`, dense-switch coverage, and any sparse-switch regression tests needed for the new traversal. Finish with the targeted commands from the milestone issue so the branch has a clear pass/fail definition.
