# CFG-10: Graphviz edge labels

## Goal
Modify to_dot() to emit label text for EdgeKind variants (T, F, Sw2, Fall).

## Acceptance Criteria
- [ ] Modify to_dot() to emit label text for EdgeKind variants
- [ ] Support all edge kinds: True, False, Switch(idx), Default, Fall, Uncond
- [ ] Visual sanity check: sample graph DOT contains labelled edges
- [ ] Edge labels are clear and readable
- [ ] Handle edge cases (no label for Uncond edges)
- [ ] Performance: minimal impact on DOT generation speed
- [ ] Maintain existing DOT structure and formatting

## Current State
DOT generation doesn't include edge labels, making it difficult to understand control flow in visualizations.

## Impact
Improves debugging and visualization capabilities by making control flow edges clearly labeled in Graphviz output. 