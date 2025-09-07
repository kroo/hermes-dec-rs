//! Types for value tracking and analysis
//!
//! This module defines the core types used throughout the value tracking system.

use crate::cfg::ssa::types::SSAValue;

/// Constant value types that can be tracked
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    /// Array literal with its elements
    ArrayLiteral(Vec<ConstantValue>),
    /// Object literal with its key-value pairs
    ObjectLiteral(Vec<(String, ConstantValue)>),
}

impl ConstantValue {
    /// Try to convert the constant value to an i32
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            ConstantValue::Number(n) => {
                let int_val = *n as i32;
                // Check if the conversion is lossless
                if int_val as f64 == *n {
                    Some(int_val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Check if this value is a primitive (not an array or object)
    pub fn is_primitive(&self) -> bool {
        !matches!(
            self,
            ConstantValue::ArrayLiteral(_) | ConstantValue::ObjectLiteral(_)
        )
    }
}

/// Types of values that can be tracked
#[derive(Debug, Clone, PartialEq)]
pub enum TrackedValue {
    /// A compile-time constant
    Constant(ConstantValue),
    /// A function parameter
    Parameter {
        /// Parameter index (1-based, 0 is 'this')
        index: u32,
        /// The SSA value containing the parameter
        ssa_value: SSAValue,
    },
    /// A phi node result
    Phi {
        /// The SSA value produced by the phi
        ssa_value: SSAValue,
    },
    /// The global object (globalThis)
    GlobalObject,
    /// A property access chain
    PropertyAccess {
        /// The object being accessed
        object: Box<TrackedValue>,
        /// The property name
        property: String,
    },
    /// Unknown or dynamic value
    Unknown,
    /// A mutable object being tracked
    MutableObject {
        /// The PC where the object was created
        creation_pc: usize,
        /// Current version of the object
        version: usize,
        /// Base type of the object
        base_type: ObjectBaseType,
        /// Mutations applied to this object
        mutations: Vec<ObjectMutation>,
    },
    /// A merged object from PHI nodes
    MergedObject {
        /// Source objects from different control flow paths
        sources: Vec<(petgraph::graph::NodeIndex, TrackedValue)>,
        /// Mutations applied after the merge
        mutations_after_merge: Vec<ObjectMutation>,
    },
}

/// Base type of a mutable object
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectBaseType {
    /// Plain object created with NewObject
    Object,
    /// Array created with NewArray
    Array { initial_length: Option<usize> },
    /// Object created with NewObjectWithBuffer
    ObjectWithBuffer,
    /// Function object
    Function,
}

/// A mutation applied to an object
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectMutation {
    /// PC where the mutation occurred
    pub pc: usize,
    /// Type of mutation
    pub kind: MutationKind,
}

/// Type of mutation applied to an object
#[derive(Debug, Clone, PartialEq)]
pub enum MutationKind {
    /// Property set (e.g., obj.x = value or obj["x"] = value)
    PropertySet { 
        key: Box<TrackedValue>, 
        value: Box<TrackedValue> 
    },
    /// Property definition (e.g., Object.defineProperty)
    PropertyDefine { 
        key: Box<TrackedValue>, 
        value: Box<TrackedValue> 
    },
    /// Array element set
    ArraySet { 
        index: Box<TrackedValue>, 
        value: Box<TrackedValue> 
    },
    /// Array push operation
    ArrayPush { 
        value: Box<TrackedValue> 
    },
    /// Prototype set
    ProtoSet { 
        proto: Box<TrackedValue> 
    },
}

impl TrackedValue {
    /// Check if this is a constant value
    pub fn is_constant(&self) -> bool {
        matches!(self, TrackedValue::Constant(_))
    }

    /// Check if this is a parameter
    pub fn is_parameter(&self) -> bool {
        matches!(self, TrackedValue::Parameter { .. })
    }

    /// Extract the constant value if this is a constant
    pub fn as_constant(&self) -> Option<&ConstantValue> {
        match self {
            TrackedValue::Constant(c) => Some(c),
            _ => None,
        }
    }

    /// Check if this is the global object
    pub fn is_global_object(&self) -> bool {
        matches!(self, TrackedValue::GlobalObject)
    }

    /// Check if this is unknown
    pub fn is_unknown(&self) -> bool {
        matches!(self, TrackedValue::Unknown)
    }

    /// Check if this is a mutable object
    pub fn is_mutable_object(&self) -> bool {
        matches!(self, TrackedValue::MutableObject { .. })
    }
}