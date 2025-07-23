# CFG-02: Add explicit Exit node & edges

## Goal
Add synthetic EXIT node once per function and create proper edges from Return/Throw terminators.

## Acceptance Criteria
- [ ] Add synthetic EXIT node once per function
- [ ] For Return / Throw terminators, create from → EXIT edge (EdgeKind::Uncond)
- [ ] Dominator and post-dominator tests must include EXIT as single sink
- [ ] EXIT node has no outgoing edges (true sink)
- [ ] All terminating blocks have path to EXIT
- [ ] CFG remains acyclic with EXIT as final destination

## Current State
Return and Throw instructions terminate blocks but don't create explicit exit edges. The CFG has no formal exit point.

## Impact
Enables proper dominator/post-dominator analysis and provides a clear termination point for control flow analysis. 