# CFG-05: Natural-loop body computation

## Goal
For each back-edge (tail → header) compute set of nodes dominated by header and reachable from header to tail.

## Acceptance Criteria
- [ ] For each back-edge (tail → header) compute set of nodes dominated by header
- [ ] Compute nodes reachable from header to tail
- [ ] Return Loop { header, body_nodes }
- [ ] Test: simple while bytecode yields one loop with correct body list
- [ ] Handle nested loops correctly
- [ ] Handle multiple back-edges to same header
- [ ] Handle irreducible loops (multiple headers)
- [ ] Performance: efficient computation for large CFGs

## Current State
Simple back-edge detection exists but doesn't compute the complete loop body or handle complex loop structures.

## Impact
Enables proper loop region detection and provides foundation for loop-based optimizations and structured control flow reconstruction. 