# CFG-07: Robust if/else region detection

## Goal
Rewrite find_if_else_joins to identify conditional source S (two labelled edges), compute lowest common post-dominator J of both targets, and return tuple (S, then_head, else_head, J).

## Acceptance Criteria
- [ ] Identify conditional source S (two labelled edges)
- [ ] Compute lowest common post-dominator J of both targets
- [ ] Return tuple (S, then_head, else_head, J)
- [ ] Passes on canonical diamond sample
- [ ] Handle nested if/else structures correctly
- [ ] Handle if/else with early returns
- [ ] Handle complex conditional chains
- [ ] Performance: efficient computation for large CFGs

## Current State
Heuristic "≥2 predecessors" approach produces many false-positives and doesn't properly identify the conditional source or join points.

## Impact
Enables accurate if/else region detection and provides foundation for structured control flow reconstruction. 