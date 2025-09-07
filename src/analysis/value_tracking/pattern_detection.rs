//! Pattern detection for object construction and modification
//!
//! This module identifies common patterns in object/array construction
//! that can be safely reconstructed as literal expressions.

use crate::analysis::value_tracking::types::{ObjectMutation, MutationKind, ObjectBaseType, TrackedValue};
use std::collections::{HashMap, HashSet};

/// Common patterns for object/array construction
#[derive(Debug, Clone, PartialEq)]
pub enum ConstructionPattern {
    /// Simple object literal: const obj = { a: 1, b: 2 }
    SimpleObjectLiteral {
        properties: Vec<(String, PropertyValue)>,
    },
    
    /// Simple array literal: const arr = [1, 2, 3]
    SimpleArrayLiteral {
        elements: Vec<ArrayElement>,
    },
    
    /// Object with computed properties: const obj = { [key]: value }
    ObjectWithComputedProps {
        static_properties: Vec<(String, PropertyValue)>,
        computed_properties: Vec<ComputedProperty>,
    },
    
    /// Array with sparse elements: const arr = [1, , 3]
    SparseArray {
        elements: HashMap<usize, ArrayElement>,
        length: usize,
    },
    
    /// Complex pattern that can't be simplified
    Complex,
}

/// Value assigned to an object property
#[derive(Debug, Clone, PartialEq)]
pub enum PropertyValue {
    Constant(String),
    Register(u8),
    Unknown,
}

/// Array element value
#[derive(Debug, Clone, PartialEq)]
pub enum ArrayElement {
    Constant(String),
    Register(u8),
    Hole, // Sparse array hole
    Unknown,
}

/// Computed property in an object
#[derive(Debug, Clone, PartialEq)]
pub struct ComputedProperty {
    pub key_register: u8,
    pub value: PropertyValue,
}

/// Pattern detector for analyzing object construction patterns
pub struct PatternDetector;

impl PatternDetector {
    /// Analyze mutations to detect construction patterns
    pub fn detect_pattern(
        base_type: &ObjectBaseType,
        mutations: &[ObjectMutation],
        creation_pc: u32,
    ) -> ConstructionPattern {
        // Check if all mutations happen immediately after creation
        if !Self::are_mutations_immediate(mutations, creation_pc) {
            return ConstructionPattern::Complex;
        }
        
        match base_type {
            ObjectBaseType::Object => Self::detect_object_pattern(mutations),
            ObjectBaseType::Array { .. } => Self::detect_array_pattern(mutations),
            _ => ConstructionPattern::Complex,
        }
    }
    
    /// Check if all mutations happen immediately after object creation
    fn are_mutations_immediate(mutations: &[ObjectMutation], creation_pc: u32) -> bool {
        if mutations.is_empty() {
            return true;
        }
        
        let creation_pc_usize = creation_pc as usize;
        
        // Get the range of PCs for mutations
        let min_pc = mutations.iter().map(|m| m.pc).min().unwrap_or(creation_pc_usize);
        let max_pc = mutations.iter().map(|m| m.pc).max().unwrap_or(creation_pc_usize);
        
        // Check if mutations are consecutive (allowing small gaps for control flow)
        // This is a heuristic - we allow up to 10 instructions between creation and last mutation
        (max_pc - creation_pc_usize) <= 10 && (min_pc > creation_pc_usize)
    }
    
    /// Detect patterns in object mutations
    fn detect_object_pattern(mutations: &[ObjectMutation]) -> ConstructionPattern {
        let mut static_props = Vec::new();
        let mut computed_props = Vec::new();
        let mut seen_keys = HashSet::new();
        
        for mutation in mutations {
            match &mutation.kind {
                MutationKind::PropertySet { key, value } |
                MutationKind::PropertyDefine { key, value } => {
                    // Check if key is a constant string
                    if let TrackedValue::Constant(crate::analysis::value_tracking::types::ConstantValue::String(key_str)) = &**key {
                        // Check for duplicate keys
                        if !seen_keys.insert(key_str.clone()) {
                            // Multiple assignments to same key - complex pattern
                            return ConstructionPattern::Complex;
                        }
                        
                        // Convert value to PropertyValue
                        let prop_value = match &**value {
                            TrackedValue::Constant(c) => PropertyValue::Constant(format!("{:?}", c)),
                            TrackedValue::Parameter { index, .. } => PropertyValue::Register(*index as u8),
                            _ => PropertyValue::Unknown,
                        };
                        
                        static_props.push((key_str.clone(), prop_value));
                    } else {
                        // Computed property key
                        if let TrackedValue::Parameter { index: key_idx, .. } = &**key {
                            let prop_value = match &**value {
                                TrackedValue::Parameter { index, .. } => PropertyValue::Register(*index as u8),
                                _ => PropertyValue::Unknown,
                            };
                            computed_props.push(ComputedProperty {
                                key_register: *key_idx as u8,
                                value: prop_value,
                            });
                        } else {
                            return ConstructionPattern::Complex;
                        }
                    }
                }
                
                _ => return ConstructionPattern::Complex,
            }
        }
        
        if computed_props.is_empty() {
            ConstructionPattern::SimpleObjectLiteral {
                properties: static_props,
            }
        } else {
            ConstructionPattern::ObjectWithComputedProps {
                static_properties: static_props,
                computed_properties: computed_props,
            }
        }
    }
    
    /// Detect patterns in array mutations
    fn detect_array_pattern(mutations: &[ObjectMutation]) -> ConstructionPattern {
        let mut elements = Vec::new();
        let mut sparse_elements = HashMap::new();
        let mut max_index = 0usize;
        let mut is_sparse = false;
        
        for mutation in mutations {
            match &mutation.kind {
                MutationKind::ArraySet { index, value } => {
                    // Try to get the index as a constant
                    if let TrackedValue::Constant(crate::analysis::value_tracking::types::ConstantValue::Number(n)) = &**index {
                        let idx = *n as usize;
                        max_index = max_index.max(idx);
                        
                        // Check if this creates a sparse array
                        if idx > elements.len() && !is_sparse {
                            is_sparse = true;
                            // Convert existing elements to sparse representation
                            for (i, elem) in elements.drain(..).enumerate() {
                                if !matches!(elem, ArrayElement::Hole) {
                                    sparse_elements.insert(i, elem);
                                }
                            }
                        }
                        
                        // Convert value to ArrayElement
                        let elem = match &**value {
                            TrackedValue::Constant(c) => ArrayElement::Constant(format!("{:?}", c)),
                            TrackedValue::Parameter { index, .. } => ArrayElement::Register(*index as u8),
                            _ => ArrayElement::Unknown,
                        };
                        
                        if is_sparse {
                            sparse_elements.insert(idx, elem);
                        } else {
                            // Ensure we have enough elements
                            while elements.len() <= idx {
                                elements.push(ArrayElement::Hole);
                            }
                            elements[idx] = elem;
                        }
                    } else {
                        // Dynamic index - complex pattern
                        return ConstructionPattern::Complex;
                    }
                }
                
                MutationKind::ArrayPush { value } => {
                    let elem = match &**value {
                        TrackedValue::Constant(c) => ArrayElement::Constant(format!("{:?}", c)),
                        TrackedValue::Parameter { index, .. } => ArrayElement::Register(*index as u8),
                        _ => ArrayElement::Unknown,
                    };
                    elements.push(elem);
                    max_index = elements.len() - 1;
                }
                
                MutationKind::PropertySet { .. } |
                MutationKind::PropertyDefine { .. } => {
                    // Arrays with non-numeric properties are complex
                    return ConstructionPattern::Complex;
                }
                
                _ => return ConstructionPattern::Complex,
            }
        }
        
        if is_sparse {
            ConstructionPattern::SparseArray {
                elements: sparse_elements,
                length: max_index + 1,
            }
        } else {
            // Check if the array is truly simple (no holes except trailing)
            let mut last_non_hole = 0;
            for (i, elem) in elements.iter().enumerate() {
                if !matches!(elem, ArrayElement::Hole) {
                    last_non_hole = i;
                }
            }
            
            // Trim trailing holes
            elements.truncate(last_non_hole + 1);
            
            // Check for internal holes
            let has_internal_holes = elements.iter()
                .take(last_non_hole)
                .any(|e| matches!(e, ArrayElement::Hole));
            
            if has_internal_holes {
                // Convert to sparse representation
                let mut sparse = HashMap::new();
                for (i, elem) in elements.into_iter().enumerate() {
                    if !matches!(elem, ArrayElement::Hole) {
                        sparse.insert(i, elem);
                    }
                }
                ConstructionPattern::SparseArray {
                    elements: sparse,
                    length: last_non_hole + 1,
                }
            } else {
                ConstructionPattern::SimpleArrayLiteral { elements }
            }
        }
    }
    
    /// Check if a pattern is safe to inline as a literal
    pub fn is_safe_to_inline(pattern: &ConstructionPattern) -> bool {
        match pattern {
            ConstructionPattern::SimpleObjectLiteral { .. } => true,
            ConstructionPattern::SimpleArrayLiteral { .. } => true,
            ConstructionPattern::ObjectWithComputedProps { .. } => false, // Need runtime evaluation
            ConstructionPattern::SparseArray { .. } => true,
            ConstructionPattern::Complex => false,
        }
    }
    
    /// Estimate the complexity score of a pattern (lower is simpler)
    pub fn complexity_score(pattern: &ConstructionPattern) -> usize {
        match pattern {
            ConstructionPattern::SimpleObjectLiteral { properties } => properties.len(),
            ConstructionPattern::SimpleArrayLiteral { elements } => elements.len(),
            ConstructionPattern::ObjectWithComputedProps { static_properties, computed_properties } => {
                static_properties.len() + computed_properties.len() * 2
            }
            ConstructionPattern::SparseArray { elements, .. } => elements.len() * 2,
            ConstructionPattern::Complex => usize::MAX,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::value_tracking::types::ConstantValue;
    
    #[test]
    fn test_simple_object_pattern() {
        let mutations = vec![
            ObjectMutation {
                pc: 2,
                kind: MutationKind::PropertySet {
                    key: Box::new(TrackedValue::Constant(ConstantValue::String("x".to_string()))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(1.0))),
                },
            },
            ObjectMutation {
                pc: 3,
                kind: MutationKind::PropertySet {
                    key: Box::new(TrackedValue::Constant(ConstantValue::String("y".to_string()))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(2.0))),
                },
            },
        ];
        
        let pattern = PatternDetector::detect_pattern(
            &ObjectBaseType::Object,
            &mutations,
            1,
        );
        
        match pattern {
            ConstructionPattern::SimpleObjectLiteral { properties } => {
                assert_eq!(properties.len(), 2);
                assert_eq!(properties[0].0, "x");
                assert_eq!(properties[1].0, "y");
            }
            _ => panic!("Expected SimpleObjectLiteral pattern"),
        }
    }
    
    #[test]
    fn test_simple_array_pattern() {
        let mutations = vec![
            ObjectMutation {
                pc: 2,
                kind: MutationKind::ArraySet {
                    index: Box::new(TrackedValue::Constant(ConstantValue::Number(0.0))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(1.0))),
                },
            },
            ObjectMutation {
                pc: 3,
                kind: MutationKind::ArraySet {
                    index: Box::new(TrackedValue::Constant(ConstantValue::Number(1.0))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(2.0))),
                },
            },
        ];
        
        let pattern = PatternDetector::detect_pattern(
            &ObjectBaseType::Array { initial_length: None },
            &mutations,
            1,
        );
        
        match pattern {
            ConstructionPattern::SimpleArrayLiteral { elements } => {
                assert_eq!(elements.len(), 2);
            }
            _ => panic!("Expected SimpleArrayLiteral pattern"),
        }
    }
    
    #[test]
    fn test_sparse_array_detection() {
        let mutations = vec![
            ObjectMutation {
                pc: 2,
                kind: MutationKind::ArraySet {
                    index: Box::new(TrackedValue::Constant(ConstantValue::Number(0.0))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(1.0))),
                },
            },
            ObjectMutation {
                pc: 3,
                kind: MutationKind::ArraySet {
                    index: Box::new(TrackedValue::Constant(ConstantValue::Number(5.0))),
                    value: Box::new(TrackedValue::Constant(ConstantValue::Number(2.0))),
                },
            },
        ];
        
        let pattern = PatternDetector::detect_pattern(
            &ObjectBaseType::Array { initial_length: None },
            &mutations,
            1,
        );
        
        match pattern {
            ConstructionPattern::SparseArray { elements, length } => {
                assert_eq!(elements.len(), 2);
                assert_eq!(length, 6);
            }
            _ => panic!("Expected SparseArray pattern"),
        }
    }
}