# CFG-04: Precise conditional-edge kinds

## Goal
Extend get_edge_kind mapping to return True/False for all J* variants currently defaulting to Uncond.

## Acceptance Criteria
- [ ] Extend get_edge_kind mapping to return True/False for all J* variants
- [ ] Every conditional jump opcode in is_conditional_jump has non-Uncond kind
- [ ] Support all conditional jump variants: JEqual, JLess, JLessEqual, JGreater, JGreaterEqual, JNotEqual
- [ ] Handle both signed and unsigned comparison variants
- [ ] Maintain backward compatibility with existing True/False edge kinds
- [ ] Unit tests verify correct edge kind assignment for each conditional jump type

## Current State
Several conditional variants (JEqual, JLess*, etc.) are classified as Uncond instead of True/False, reducing analysis precision.

## Impact
Improves control flow analysis accuracy and enables better region detection for complex conditional structures. 