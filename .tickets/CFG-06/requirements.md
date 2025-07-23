# CFG-06: Post-dominator analysis

## Goal
Implement post-doms using reverse graph + simple_fast.

## Acceptance Criteria
- [ ] Implement post-doms using reverse graph + simple_fast
- [ ] Expose analyze_post_dominators() parallel to existing dominators helper
- [ ] Unit test: diamond shape A→{B,C}→D ⇒ D post-dominates B & C
- [ ] Handle CFG with EXIT node as root for post-dominator analysis
- [ ] Support same API as dominator analysis (dominates, immediate_dominator, etc.)
- [ ] Performance: efficient computation for large CFGs
- [ ] Handle unreachable code correctly

## Current State
Only forward dominator analysis exists. Post-dominator analysis is missing, which is needed for join block detection and early exit analysis.

## Impact
Enables proper join block detection, early exit analysis, and provides foundation for structured control flow reconstruction. 