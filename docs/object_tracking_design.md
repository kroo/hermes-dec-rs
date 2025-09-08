# Integrated Object Tracking Design for ValueTracker

## Overview

This document describes the design for integrating comprehensive object tracking into the existing ValueTracker system in hermes-dec-rs. The goal is to enable safe object literal reconstruction while maintaining semantic correctness.

## Problem Statement

Currently, `NewObject` instructions are tracked as constant empty object literals:
```rust
UnifiedInstruction::NewObject { .. } => {
    TrackedValue::Constant(ConstantValue::ObjectLiteral(Vec::new()))
}
```

This leads to incorrect inlining where empty objects `{}` are inlined while property assignments remain, referencing undefined variables.

## Status Update (2024-01-07)

### Completed Work ✅

#### Object Versioning and Mutation Tracking
- Implemented `MutableObject` variant with comprehensive mutation tracking
- Objects are tracked with creation PC, version number, base type, and mutation list
- Mutations are collected via SSA def-use chains
- Support for all mutation types: property sets, array element sets, dynamic property access
- Fixed `PutByVal` instruction argument order bug

#### Escape Analysis
- Basic escape analysis infrastructure implemented in `src/analysis/value_tracking/escape_analysis.rs`
- Identifies escape reasons: returns, function arguments, storage in containers, closures
- Integrated with ValueTracker via `check_object_escape` method

#### Successful Modularization
- Refactored ValueTracker from 1034 lines to 884 lines
- Created well-organized module structure:
  - `value_tracking/types.rs` - Core types with helper methods
  - `value_tracking/escape_analysis.rs` - Escape analysis
  - `value_tracking/constant_folding.rs` - Constant folding operations
  - `value_tracking/literal_tracker.rs` - Literal tracking from buffers
- Moved utility methods to types (e.g., `is_constant()` is now instance method)
- Removed dead code and unused methods

## Solution: Integrated ValueTracker with Object Versioning

### Phase 1: Extended TrackedValue Enum

```rust
pub enum TrackedValue {
    // Existing variants
    Register(RegisterRef),
    Constant(ConstantValue),
    Parameter { index: usize, name: String },
    GlobalThis,
    PropertyAccess { base: Box<TrackedValue>, property: Box<TrackedValue> },
    PhiValue { sources: Vec<(NodeIndex, TrackedValue)> },
    
    // New variants for object tracking
    MutableObject {
        creation_pc: usize,
        version: ObjectVersion,
        mutations: Vec<ObjectMutation>,
    },
    
    MergedObject {
        // Like PhiValue but for objects
        sources: Vec<(NodeIndex, ObjectVersion)>,
        mutations_after_merge: Vec<ObjectMutation>,
    },
}

#[derive(Clone, Debug)]
pub struct ObjectVersion {
    pub pc: usize,
    pub version_number: usize,
    pub base_type: ObjectBaseType,
}

#[derive(Clone, Debug)]
pub enum ObjectBaseType {
    Object,
    Array { initial_length: Option<usize> },
    Function { constructor_reg: RegisterRef },
}

#[derive(Clone, Debug)]
pub struct ObjectMutation {
    pub pc: usize,
    pub kind: MutationKind,
}

#[derive(Clone, Debug)]
pub enum MutationKind {
    PropertySet { key: TrackedValue, value: TrackedValue },
    PropertyDefine { key: TrackedValue, value: TrackedValue },
    ArrayPush { value: TrackedValue },
    ArraySet { index: TrackedValue, value: TrackedValue },
    ProtoSet { proto: TrackedValue },
}
```

### Phase 2: Enhanced ValueTracker Methods

```rust
impl ValueTracker {
    // Existing method, modified
    pub fn track_instruction(&mut self, pc: usize, instr: &UnifiedInstruction, ssa: &SSAAnalysis) {
        match instr {
            // Object creation instructions
            UnifiedInstruction::NewObject { dst } => {
                let version = ObjectVersion {
                    pc,
                    version_number: 0,
                    base_type: ObjectBaseType::Object,
                };
                self.set_register(
                    *dst,
                    TrackedValue::MutableObject {
                        creation_pc: pc,
                        version,
                        mutations: Vec::new(),
                    }
                );
            }
            
            UnifiedInstruction::NewArray { dst, size } => {
                let version = ObjectVersion {
                    pc,
                    version_number: 0,
                    base_type: ObjectBaseType::Array {
                        initial_length: Some(*size as usize),
                    },
                };
                self.set_register(
                    *dst,
                    TrackedValue::MutableObject {
                        creation_pc: pc,
                        version,
                        mutations: Vec::new(),
                    }
                );
            }
            
            // Property mutations
            UnifiedInstruction::PutOwnByIndex { object, index, value } => {
                self.track_mutation(
                    *object,
                    pc,
                    MutationKind::PropertySet {
                        key: TrackedValue::Constant(ConstantValue::Number(*index as f64)),
                        value: self.get_register(*value).cloned().unwrap_or_else(|| 
                            TrackedValue::Register(RegisterRef::new(*value))
                        ),
                    }
                );
            }
            
            UnifiedInstruction::PutOwnById { object, prop_name_idx, value } => {
                let key = self.resolve_string_id(*prop_name_idx);
                self.track_mutation(
                    *object,
                    pc,
                    MutationKind::PropertySet {
                        key: TrackedValue::Constant(ConstantValue::String(key)),
                        value: self.get_register(*value).cloned().unwrap_or_else(|| 
                            TrackedValue::Register(RegisterRef::new(*value))
                        ),
                    }
                );
            }
            
            // ... handle other mutation instructions
        }
    }
    
    // New method for tracking mutations
    fn track_mutation(&mut self, object_reg: RegisterRef, pc: usize, mutation: MutationKind) {
        if let Some(TrackedValue::MutableObject { mutations, version, .. }) = 
            self.register_values.get_mut(&object_reg) 
        {
            mutations.push(ObjectMutation { pc, kind: mutation });
            version.version_number += 1;
        }
    }
    
    // New method for escape analysis
    pub fn analyze_escapes(&self, object_reg: RegisterRef, ssa: &SSAAnalysis) -> bool {
        // Check if object escapes the function
        if let Some(uses) = ssa.get_uses(object_reg) {
            for use_site in uses {
                match &use_site.instruction {
                    // Object doesn't escape for property sets
                    UnifiedInstruction::PutOwnById { object, .. } |
                    UnifiedInstruction::PutOwnByIndex { object, .. } 
                        if *object == object_reg => continue,
                    
                    // Object escapes if: returned, passed to function, stored to outer scope
                    UnifiedInstruction::Return { .. } |
                    UnifiedInstruction::Call { .. } |
                    UnifiedInstruction::Construct { .. } |
                    UnifiedInstruction::StoreToEnvironment { .. } => return true,
                    
                    _ => {}
                }
            }
        }
        false
    }
    
    // New method for reconstructing object literals
    pub fn try_reconstruct_object(&self, value: &TrackedValue, ssa: &SSAAnalysis) -> Option<Expression> {
        match value {
            TrackedValue::MutableObject { creation_pc, mutations, .. } => {
                // Only reconstruct if object doesn't escape
                let object_reg = self.find_register_for_pc(*creation_pc)?;
                if self.analyze_escapes(object_reg, ssa) {
                    return None;
                }
                
                // Build object literal from mutations
                let mut properties = Vec::new();
                for mutation in mutations {
                    match &mutation.kind {
                        MutationKind::PropertySet { key, value } => {
                            let key_expr = self.value_to_expression(key)?;
                            let value_expr = self.value_to_expression(value)?;
                            properties.push(ObjectProperty {
                                key: key_expr,
                                value: value_expr,
                                computed: false,
                                shorthand: false,
                            });
                        }
                        _ => {} // Handle other mutation types
                    }
                }
                
                Some(Expression::ObjectLiteral(properties))
            }
            _ => None
        }
    }
}
```

### Phase 3: PHI-Aware Object Merging

```rust
impl ValueTracker {
    pub fn handle_phi_objects(&mut self, block: NodeIndex, predecessors: &[NodeIndex]) {
        // For each register that has object values from different predecessors
        for reg in self.get_all_registers() {
            let mut object_sources = Vec::new();
            
            for &pred in predecessors {
                if let Some(TrackedValue::MutableObject { version, .. }) = 
                    self.get_value_at_block_end(pred, reg) 
                {
                    object_sources.push((pred, version.clone()));
                }
            }
            
            if object_sources.len() > 1 {
                // Create merged object
                self.set_register(
                    reg,
                    TrackedValue::MergedObject {
                        sources: object_sources,
                        mutations_after_merge: Vec::new(),
                    }
                );
            }
        }
    }
}
```

### Phase 4: Integration with Expression Generation

```rust
impl ValueTracker {
    pub fn value_to_expression(&self, value: &TrackedValue) -> Option<Expression> {
        match value {
            // Existing cases...
            
            TrackedValue::MutableObject { .. } => {
                // Try to reconstruct if safe
                self.try_reconstruct_object(value, &self.ssa)
            }
            
            TrackedValue::MergedObject { .. } => {
                // Cannot safely reconstruct merged objects
                None
            }
            
            _ => // existing logic
        }
    }
}
```

## Implementation Phases

### Immediate Fix (Option A)
Change `NewObject` tracking to return `None` or a special `TrackedValue::MutableReference`:
```rust
UnifiedInstruction::NewObject { dst } => {
    // Don't track as constant
    TrackedValue::Register(RegisterRef::new(*dst))
}
```

### Phase 1: Object Versioning Infrastructure
- Extend `TrackedValue` enum with `MutableObject` and `MergedObject`
- Implement basic mutation tracking
- Update `track_instruction` for object creation opcodes

### Phase 2: Escape Analysis
- Implement `analyze_escapes` method
- Track which objects escape the function
- Mark escaping objects as non-inlinable

### Phase 3: Pattern Detection
- Detect common patterns (object literal, array literal, etc.)
- Track mutation sequences
- Identify safe inlining points

### Phase 4: Object Literal Reconstruction
- Implement `try_reconstruct_object`
- Generate object literal expressions from mutation sequences
- Integrate with existing expression generation

## Benefits

1. **Correctness**: Objects are only inlined when safe to do so
2. **Integration**: Builds on existing ValueTracker infrastructure
3. **Incremental**: Can be implemented in phases
4. **Comprehensive**: Handles all object/array creation patterns
5. **Control-flow aware**: Uses PHI analysis for merged objects

## Module Structure (Implemented)

The ValueTracker has been successfully modularized into:

### Current Structure
```
src/analysis/
├── value_tracker.rs (884 lines - core logic)
└── value_tracking/
    ├── mod.rs              - Module exports
    ├── types.rs            - Core types (TrackedValue, ConstantValue, etc.)
    ├── escape_analysis.rs  - Escape analysis for objects
    ├── constant_folding.rs - Constant folding operations
    └── literal_tracker.rs  - Tracking literals from serialized buffers
```

### What Remains in value_tracker.rs
- Core `ValueTracker` struct
- Mutation tracking methods (need full context)
- PHI analysis (tightly integrated)
- The large `analyze_instruction` method (core mapping logic)
- Binary operation folding (needs register lookup)

### API Considerations
Some methods couldn't be extracted due to circular dependencies:
- Mutation tracking needs access to value resolution
- Binary folding needs register value lookup
- These remain in the main ValueTracker for now

## Testing Strategy

1. Test with `constructor_test.hbc` functions 1 and 4 ✅
2. Verify empty objects are not incorrectly inlined ✅
3. Ensure property assignments are preserved or inlined together ✅
4. Test mutation tracking with `simple_object_test.hbc` ✅
5. Test array mutations with `PutByVal` ✅
6. Test escape analysis with various patterns (basic infrastructure ✅, full testing pending)
7. Verify PHI merging for objects across control flow (pending)

## Future Enhancements

1. **Prototype chain tracking**: Track prototype modifications
2. **Method extraction**: Detect and inline method definitions
3. **Array comprehension**: Detect array initialization patterns
4. **Spread operator detection**: Identify object spread patterns
5. **Destructuring hints**: Provide hints for destructuring patterns