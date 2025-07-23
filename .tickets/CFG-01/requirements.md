# CFG-01: Populate pc_to_block map

## Goal
Fill pc_to_block during block insertion (start_pc..end_pc → NodeIndex).

## Acceptance Criteria
- [ ] Fill pc_to_block during block insertion (start_pc..end_pc → NodeIndex)
- [ ] Unit test: lookup by arbitrary PC returns expected node for three-block sample
- [ ] All PC values within block ranges map to correct NodeIndex
- [ ] Edge cases handled: single-instruction blocks, overlapping ranges
- [ ] Performance: O(1) lookup time for any PC value

## Current State
The `pc_to_block` map exists but is never populated during CFG construction. Only `block_starts` map is currently filled.

## Impact
This enables diagnostic utilities and lookup functions to efficiently find which basic block contains a given program counter value. 