# CFG Enhancement Tickets

This directory contains tickets for enhancing the Control Flow Graph (CFG) module to bring it up to production-ready capabilities for the Hermes decompiler.

## Ticket Overview

### Core Graph Completeness
- **[CFG-01](CFG-01/)** - Populate pc_to_block map
- **[CFG-02](CFG-02/)** - Add explicit Exit node & edges

### Edge Fidelity
- **[CFG-03](CFG-03/)** - Multi-target Switch support
- **[CFG-04](CFG-04/)** - Precise conditional-edge kinds

### Analyses
- **[CFG-05](CFG-05/)** - Natural-loop body computation
- **[CFG-06](CFG-06/)** - Post-dominator analysis
- **[CFG-07](CFG-07/)** - Robust if/else region detection
- **[CFG-08](CFG-08/)** - Switch dispatch grouping

### Quality / Developer Experience
- **[CFG-09](CFG-09/)** - Leader after Return/Throw
- **[CFG-10](CFG-10/)** - Graphviz edge labels

## Implementation Priority

### Phase 1: Foundation (CFG-01, CFG-02, CFG-09)
These tickets establish the basic CFG infrastructure:
- Complete PC-to-block mapping for diagnostics
- Proper exit node handling for analysis
- Correct leader detection for unreachable code

### Phase 2: Edge Precision (CFG-03, CFG-04)
These tickets improve control flow accuracy:
- Proper switch case handling
- Precise conditional edge classification

### Phase 3: Advanced Analysis (CFG-05, CFG-06)
These tickets enable sophisticated control flow analysis:
- Natural loop detection
- Post-dominator analysis for join detection

### Phase 4: Region Detection (CFG-07, CFG-08)
These tickets enable structured control flow reconstruction:
- Robust if/else region detection
- Switch region grouping

### Phase 5: Developer Experience (CFG-10)
This ticket improves debugging and visualization:
- Clear edge labels in Graphviz output

## Current State

The CFG module currently provides:
- ✅ Basic leader detection
- ✅ Basic block creation
- ✅ Simple edge creation
- ✅ Dominator analysis
- ✅ Simple back-edge detection
- ✅ Heuristic region detection

## Target State

After implementing all tickets, the CFG module will provide:
- ✅ Complete PC-to-block mapping
- ✅ Proper exit node handling
- ✅ Precise edge classification
- ✅ Multi-target switch support
- ✅ Natural loop detection
- ✅ Post-dominator analysis
- ✅ Robust region detection
- ✅ Enhanced visualization

## Dependencies

- CFG-06 (Post-dominator analysis) is required for CFG-07 and CFG-08
- CFG-03 (Switch support) is required for CFG-08
- CFG-04 (Conditional edges) is required for CFG-07
- CFG-02 (Exit node) is required for CFG-06

## Testing Strategy

Each ticket includes comprehensive test cases covering:
- Happy path scenarios
- Edge cases and error conditions
- Performance considerations
- Integration with existing functionality

## Success Criteria

The CFG module will be considered complete when:
1. All tickets are implemented and tested
2. Performance remains acceptable for large functions
3. Integration with AST generation works correctly
4. Visualization tools provide clear control flow representation
5. Region detection enables structured decompilation 