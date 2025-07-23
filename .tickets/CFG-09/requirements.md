# CFG-09: Leader after Return/Throw

## Goal
Update find_leaders to insert pc+1 when last instr is Return/Throw.

## Acceptance Criteria
- [ ] Update find_leaders to insert pc+1 when last instr is Return/Throw
- [ ] Regression test: unreachable block after return becomes separate leader
- [ ] Handle both Return and Throw instructions
- [ ] Handle Return/Throw as last instruction in function
- [ ] Maintain existing leader detection logic
- [ ] Performance: minimal impact on leader detection speed

## Current State
No leader is inserted after Return/Throw terminators, causing unreachable code to be included in the same basic block.

## Impact
Enables proper handling of unreachable code and improves basic block separation for terminating instructions. 