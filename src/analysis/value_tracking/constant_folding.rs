//! Constant folding operations
//!
//! This module handles compile-time evaluation of operations on constant values.

use crate::analysis::value_tracking::{ConstantValue, TrackedValue};

/// Performs constant folding operations
pub struct ConstantFolder;

impl ConstantFolder {
    /// Fold a binary operation with constant operands
    pub fn fold_binary_operation<F>(
        left: &TrackedValue,
        right: &TrackedValue,
        op: F,
    ) -> TrackedValue
    where
        F: Fn(f64, f64) -> f64,
    {
        match (left, right) {
            (
                TrackedValue::Constant(ConstantValue::Number(l)),
                TrackedValue::Constant(ConstantValue::Number(r)),
            ) => TrackedValue::Constant(ConstantValue::Number(op(*l, *r))),
            _ => TrackedValue::Unknown,
        }
    }

    /// Fold a string concatenation operation
    pub fn fold_string_concat(left: &TrackedValue, right: &TrackedValue) -> TrackedValue {
        match (left, right) {
            (
                TrackedValue::Constant(ConstantValue::String(l)),
                TrackedValue::Constant(ConstantValue::String(r)),
            ) => {
                let mut result = l.clone();
                result.push_str(r);
                TrackedValue::Constant(ConstantValue::String(result))
            }
            _ => TrackedValue::Unknown,
        }
    }

    /// Fold a comparison operation
    pub fn fold_comparison<F>(left: &TrackedValue, right: &TrackedValue, op: F) -> TrackedValue
    where
        F: Fn(&ConstantValue, &ConstantValue) -> bool,
    {
        match (left, right) {
            (TrackedValue::Constant(l), TrackedValue::Constant(r)) => {
                TrackedValue::Constant(ConstantValue::Boolean(op(l, r)))
            }
            _ => TrackedValue::Unknown,
        }
    }

    /// Fold a unary operation
    pub fn fold_unary_operation<F>(value: &TrackedValue, op: F) -> TrackedValue
    where
        F: Fn(&ConstantValue) -> ConstantValue,
    {
        match value {
            TrackedValue::Constant(c) => TrackedValue::Constant(op(c)),
            _ => TrackedValue::Unknown,
        }
    }

    /// Check equality between constant values
    pub fn constant_eq(left: &ConstantValue, right: &ConstantValue) -> bool {
        match (left, right) {
            (ConstantValue::Number(l), ConstantValue::Number(r)) => l == r,
            (ConstantValue::String(l), ConstantValue::String(r)) => l == r,
            (ConstantValue::Boolean(l), ConstantValue::Boolean(r)) => l == r,
            (ConstantValue::Null, ConstantValue::Null) => true,
            (ConstantValue::Undefined, ConstantValue::Undefined) => true,
            _ => false,
        }
    }

    /// Check strict equality between constant values
    pub fn constant_strict_eq(left: &ConstantValue, right: &ConstantValue) -> bool {
        Self::constant_eq(left, right)
    }

    /// Check less than between constant values
    pub fn constant_lt(left: &ConstantValue, right: &ConstantValue) -> bool {
        match (left, right) {
            (ConstantValue::Number(l), ConstantValue::Number(r)) => l < r,
            (ConstantValue::String(l), ConstantValue::String(r)) => l < r,
            _ => false,
        }
    }

    /// Check less than or equal between constant values
    pub fn constant_le(left: &ConstantValue, right: &ConstantValue) -> bool {
        match (left, right) {
            (ConstantValue::Number(l), ConstantValue::Number(r)) => l <= r,
            (ConstantValue::String(l), ConstantValue::String(r)) => l <= r,
            _ => false,
        }
    }

    /// Check greater than between constant values
    pub fn constant_gt(left: &ConstantValue, right: &ConstantValue) -> bool {
        match (left, right) {
            (ConstantValue::Number(l), ConstantValue::Number(r)) => l > r,
            (ConstantValue::String(l), ConstantValue::String(r)) => l > r,
            _ => false,
        }
    }

    /// Check greater than or equal between constant values
    pub fn constant_ge(left: &ConstantValue, right: &ConstantValue) -> bool {
        match (left, right) {
            (ConstantValue::Number(l), ConstantValue::Number(r)) => l >= r,
            (ConstantValue::String(l), ConstantValue::String(r)) => l >= r,
            _ => false,
        }
    }

    /// Negate a constant value
    pub fn constant_negate(value: &ConstantValue) -> ConstantValue {
        match value {
            ConstantValue::Number(n) => ConstantValue::Number(-n),
            ConstantValue::Boolean(b) => ConstantValue::Boolean(!b),
            _ => ConstantValue::Undefined,
        }
    }

    /// Convert a constant to boolean
    pub fn constant_to_bool(value: &ConstantValue) -> ConstantValue {
        match value {
            ConstantValue::Boolean(b) => ConstantValue::Boolean(*b),
            ConstantValue::Number(n) => ConstantValue::Boolean(*n != 0.0 && !n.is_nan()),
            ConstantValue::String(s) => ConstantValue::Boolean(!s.is_empty()),
            ConstantValue::Null | ConstantValue::Undefined => ConstantValue::Boolean(false),
            _ => ConstantValue::Boolean(true),
        }
    }

    /// Perform typeof operation on a constant
    pub fn constant_typeof(value: &ConstantValue) -> ConstantValue {
        let type_str = match value {
            ConstantValue::Number(_) => "number",
            ConstantValue::String(_) => "string",
            ConstantValue::Boolean(_) => "boolean",
            ConstantValue::Null => "object",
            ConstantValue::Undefined => "undefined",
            ConstantValue::ArrayLiteral(_) => "object",
            ConstantValue::ObjectLiteral(_) => "object",
        };
        ConstantValue::String(type_str.to_string())
    }
}
