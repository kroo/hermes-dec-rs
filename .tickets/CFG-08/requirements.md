# CFG-08: Switch dispatch grouping

## Goal
Using outgoing Switch(idx) edges, group successors until common post-dominator J found; return SwitchRegion { dispatch, cases: Vec<(idx, head)>, join: J }.

## Acceptance Criteria
- [ ] Group successors using outgoing Switch(idx) edges
- [ ] Find common post-dominator J for all case heads
- [ ] Return SwitchRegion { dispatch, cases: Vec<(idx, head)>, join: J }
- [ ] Test bundle with switch(3) verifies case grouping and join detection
- [ ] Handle sparse switch tables (gaps in case values)
- [ ] Handle switches with default case
- [ ] Handle nested switch structures
- [ ] Performance: efficient computation for large switch tables

## Current State
Heuristic ">2 successors" approach doesn't properly group switch cases or identify join points.

## Impact
Enables accurate switch region detection and provides foundation for structured switch statement reconstruction. 