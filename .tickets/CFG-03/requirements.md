# CFG-03: Multi-target Switch support

## Goal
Parse SwitchImm/SwitchLong tables and add one edge per case with EdgeKind::Switch(idx), plus default.

## Acceptance Criteria
- [ ] Parse SwitchImm/SwitchLong tables
- [ ] Add one edge per case with EdgeKind::Switch(idx)
- [ ] Add default edge for switch statements
- [ ] Remove hard-coded Switch(0)
- [ ] Unit test on fake switch with 3 cases + default: graph has four outgoing edges
- [ ] Handle both SwitchImm and SwitchLong instruction variants
- [ ] Support sparse switch tables (gaps in case values)

## Current State
SwitchImm/SwitchLong instructions only create a single hard-coded Switch(0) edge instead of proper case-specific edges.

## Impact
Enables accurate control flow analysis for switch statements and proper region detection for switch dispatch patterns. 