//! Object and array literal reconstruction from tracked values and patterns
//!
//! This module converts tracked mutable objects with their mutations into
//! JavaScript literal expressions when safe to do so.

use crate::analysis::value_tracking::escape_analysis::EscapeAnalyzer;
use crate::analysis::value_tracking::pattern_detection::{
    ArrayElement, ConstructionPattern, PatternDetector, PropertyValue,
};
use crate::analysis::value_tracking::types::{
    ConstantValue, ObjectBaseType, ObjectMutation, TrackedValue,
};
use crate::cfg::ssa::SSAAnalysis;
use crate::cfg::ssa::types::SSAValue;
use crate::cfg::Cfg;
use std::collections::HashMap;

/// Result of attempting to reconstruct an object/array literal
#[derive(Debug, Clone)]
pub enum ReconstructionResult {
    /// Successfully reconstructed as a literal expression
    Literal(String),
    /// Cannot be reconstructed (with reason)
    CannotReconstruct(String),
    /// Partially reconstructible with some dynamic parts
    Partial {
        template: String,
        dynamic_parts: Vec<DynamicPart>,
    },
}

/// A dynamic part in a partially reconstructible literal
#[derive(Debug, Clone)]
pub struct DynamicPart {
    pub placeholder: String,
    pub register: u8,
    pub description: String,
}

/// Reconstructs object and array literals from tracked values
pub struct LiteralReconstructor<'a> {
    ssa: &'a SSAAnalysis,
    cfg: &'a Cfg<'a>,
}

impl<'a> LiteralReconstructor<'a> {
    /// Create a new literal reconstructor
    pub fn new(ssa: &'a SSAAnalysis, cfg: &'a Cfg<'a>) -> Self {
        Self { ssa, cfg }
    }

    /// Attempt to reconstruct a tracked value as a literal
    pub fn reconstruct(&self, value: &TrackedValue, ssa_value: &SSAValue) -> ReconstructionResult {
        match value {
            TrackedValue::Constant(c) => {
                ReconstructionResult::Literal(self.constant_to_js(c))
            }
            
            TrackedValue::MutableObject {
                creation_pc,
                base_type,
                mutations,
                ..
            } => {
                self.reconstruct_mutable_object(*creation_pc, base_type, mutations, ssa_value)
            }
            
            TrackedValue::Unknown => {
                ReconstructionResult::CannotReconstruct("Value is unknown".to_string())
            }
            
            TrackedValue::Parameter { index, .. } => {
                ReconstructionResult::CannotReconstruct(
                    format!("Value is parameter {}", index)
                )
            }
            
            TrackedValue::GlobalObject => {
                ReconstructionResult::Literal("globalThis".to_string())
            }
            
            TrackedValue::PropertyAccess { object, property } => {
                match self.reconstruct(object, ssa_value) {
                    ReconstructionResult::Literal(obj_js) => {
                        ReconstructionResult::Literal(format!("{}.{}", obj_js, property))
                    }
                    _ => ReconstructionResult::CannotReconstruct(
                        "Cannot reconstruct property access base".to_string()
                    ),
                }
            }
            
            _ => ReconstructionResult::CannotReconstruct(
                format!("Cannot reconstruct {:?}", value)
            ),
        }
    }

    /// Reconstruct a mutable object/array
    fn reconstruct_mutable_object(
        &self,
        creation_pc: usize,
        base_type: &ObjectBaseType,
        mutations: &[ObjectMutation],
        ssa_value: &SSAValue,
    ) -> ReconstructionResult {
        // Check if object escapes
        let escape_analyzer = EscapeAnalyzer::new(self.ssa, self.cfg);
        let def = crate::cfg::ssa::types::RegisterDef {
            register: ssa_value.register,
            block_id: ssa_value.def_site.block_id,
            instruction_idx: ssa_value.def_site.instruction_idx,
        };
        let escape_result = escape_analyzer.analyze_object_escape(&def);
        
        if !escape_result.safe_to_inline {
            let reasons = escape_result.reasons.iter()
                .map(|r| format!("{:?}", r))
                .collect::<Vec<_>>()
                .join(", ");
            return ReconstructionResult::CannotReconstruct(
                format!("Object not safe to inline: {} (escapes: {})", reasons, escape_result.escapes)
            );
        }
        
        // Detect pattern
        let pattern = PatternDetector::detect_pattern(base_type, mutations, creation_pc as u32);
        
        // Check if pattern is safe to inline
        if !PatternDetector::is_safe_to_inline(&pattern) {
            return ReconstructionResult::CannotReconstruct(
                "Pattern is not safe to inline".to_string()
            );
        }
        
        // Reconstruct based on pattern
        match pattern {
            ConstructionPattern::SimpleObjectLiteral { properties } => {
                self.reconstruct_simple_object(&properties)
            }
            
            ConstructionPattern::SimpleArrayLiteral { elements } => {
                self.reconstruct_simple_array(&elements)
            }
            
            ConstructionPattern::SparseArray { elements, length } => {
                self.reconstruct_sparse_array(&elements, length)
            }
            
            _ => ReconstructionResult::CannotReconstruct(
                "Complex pattern cannot be reconstructed".to_string()
            ),
        }
    }

    /// Reconstruct a simple object literal
    fn reconstruct_simple_object(&self, properties: &[(String, PropertyValue)]) -> ReconstructionResult {
        if properties.is_empty() {
            return ReconstructionResult::Literal("{}".to_string());
        }
        
        let mut parts = Vec::new();
        let mut dynamic_parts = Vec::new();
        
        for (key, value) in properties {
            let key_js = if Self::is_valid_identifier(key) {
                key.clone()
            } else {
                format!("\"{}\"", Self::escape_string(key))
            };
            
            match value {
                PropertyValue::Constant(val) => {
                    // Parse the debug format back to clean JS
                    let clean_val = Self::clean_debug_value(val);
                    parts.push(format!("{}: {}", key_js, clean_val));
                }
                PropertyValue::Register(reg) => {
                    let placeholder = format!("__REG_{}__", reg);
                    parts.push(format!("{}: {}", key_js, placeholder));
                    dynamic_parts.push(DynamicPart {
                        placeholder: placeholder.clone(),
                        register: *reg,
                        description: format!("Value for property '{}'", key),
                    });
                }
                PropertyValue::Unknown => {
                    return ReconstructionResult::CannotReconstruct(
                        format!("Unknown value for property '{}'", key)
                    );
                }
            }
        }
        
        let js = format!("{{ {} }}", parts.join(", "));
        
        if dynamic_parts.is_empty() {
            ReconstructionResult::Literal(js)
        } else {
            ReconstructionResult::Partial {
                template: js,
                dynamic_parts,
            }
        }
    }

    /// Reconstruct a simple array literal
    fn reconstruct_simple_array(&self, elements: &[ArrayElement]) -> ReconstructionResult {
        if elements.is_empty() {
            return ReconstructionResult::Literal("[]".to_string());
        }
        
        let mut parts = Vec::new();
        let mut dynamic_parts = Vec::new();
        
        for (i, elem) in elements.iter().enumerate() {
            match elem {
                ArrayElement::Constant(val) => {
                    let clean_val = Self::clean_debug_value(val);
                    parts.push(clean_val);
                }
                ArrayElement::Register(reg) => {
                    let placeholder = format!("__REG_{}__", reg);
                    parts.push(placeholder.clone());
                    dynamic_parts.push(DynamicPart {
                        placeholder: placeholder.clone(),
                        register: *reg,
                        description: format!("Element at index {}", i),
                    });
                }
                ArrayElement::Hole => {
                    parts.push("".to_string()); // Empty slot for sparse array
                }
                ArrayElement::Unknown => {
                    return ReconstructionResult::CannotReconstruct(
                        format!("Unknown element at index {}", i)
                    );
                }
            }
        }
        
        let js = format!("[{}]", parts.join(", "));
        
        if dynamic_parts.is_empty() {
            ReconstructionResult::Literal(js)
        } else {
            ReconstructionResult::Partial {
                template: js,
                dynamic_parts,
            }
        }
    }

    /// Reconstruct a sparse array
    fn reconstruct_sparse_array(
        &self,
        elements: &HashMap<usize, ArrayElement>,
        length: usize,
    ) -> ReconstructionResult {
        // For sparse arrays, we'll use array literal with holes
        let mut parts = vec!["".to_string(); length];
        let mut dynamic_parts = Vec::new();
        
        for (idx, elem) in elements {
            match elem {
                ArrayElement::Constant(val) => {
                    parts[*idx] = Self::clean_debug_value(val);
                }
                ArrayElement::Register(reg) => {
                    let placeholder = format!("__REG_{}__", reg);
                    parts[*idx] = placeholder.clone();
                    dynamic_parts.push(DynamicPart {
                        placeholder: placeholder.clone(),
                        register: *reg,
                        description: format!("Element at index {}", idx),
                    });
                }
                ArrayElement::Unknown => {
                    return ReconstructionResult::CannotReconstruct(
                        format!("Unknown element at index {}", idx)
                    );
                }
                _ => {}
            }
        }
        
        let js = format!("[{}]", parts.join(", "));
        
        if dynamic_parts.is_empty() {
            ReconstructionResult::Literal(js)
        } else {
            ReconstructionResult::Partial {
                template: js,
                dynamic_parts,
            }
        }
    }

    /// Convert a constant value to JavaScript
    fn constant_to_js(&self, value: &ConstantValue) -> String {
        Self::constant_to_js_static(value)
    }
    
    /// Static version for testing
    fn constant_to_js_static(value: &ConstantValue) -> String {
        match value {
            ConstantValue::Number(n) => {
                if n.is_nan() {
                    "NaN".to_string()
                } else if n.is_infinite() {
                    if n.is_sign_positive() {
                        "Infinity".to_string()
                    } else {
                        "-Infinity".to_string()
                    }
                } else {
                    n.to_string()
                }
            }
            ConstantValue::String(s) => format!("\"{}\"", Self::escape_string(s)),
            ConstantValue::Boolean(b) => b.to_string(),
            ConstantValue::Null => "null".to_string(),
            ConstantValue::Undefined => "undefined".to_string(),
            ConstantValue::ArrayLiteral(elements) => {
                let parts: Vec<String> = elements.iter()
                    .map(|e| Self::constant_to_js_static(e))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            ConstantValue::ObjectLiteral(props) => {
                let parts: Vec<String> = props.iter()
                    .map(|(k, v)| {
                        let key = if Self::is_valid_identifier(k) {
                            k.clone()
                        } else {
                            format!("\"{}\"", Self::escape_string(k))
                        };
                        format!("{}: {}", key, Self::constant_to_js_static(v))
                    })
                    .collect();
                format!("{{ {} }}", parts.join(", "))
            }
        }
    }

    /// Check if a string is a valid JavaScript identifier
    fn is_valid_identifier(s: &str) -> bool {
        if s.is_empty() {
            return false;
        }
        
        let first = s.chars().next().unwrap();
        if !first.is_alphabetic() && first != '_' && first != '$' {
            return false;
        }
        
        s.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '$')
    }

    /// Escape a string for JavaScript
    fn escape_string(s: &str) -> String {
        s.chars()
            .map(|c| match c {
                '"' => "\\\"".to_string(),
                '\\' => "\\\\".to_string(),
                '\n' => "\\n".to_string(),
                '\r' => "\\r".to_string(),
                '\t' => "\\t".to_string(),
                c if c.is_control() => format!("\\u{:04x}", c as u32),
                c => c.to_string(),
            })
            .collect()
    }
    
    /// Clean a debug format value string to proper JS
    fn clean_debug_value(val: &str) -> String {
        // Handle common debug formats
        if val.starts_with("Number(") && val.ends_with(")") {
            let num_str = &val[7..val.len()-1];
            // Remove unnecessary decimal point for integers
            if let Ok(n) = num_str.parse::<f64>() {
                if n.fract() == 0.0 && n.abs() < 1e10 {
                    return (n as i64).to_string();
                }
            }
            return num_str.to_string();
        } else if val.starts_with("String(\"") && val.ends_with("\")") {
            return val[7..val.len()-1].to_string();
        } else if val.starts_with("Boolean(") && val.ends_with(")") {
            return val[8..val.len()-1].to_string();
        } else if val == "Null" {
            return "null".to_string();
        } else if val == "Undefined" {
            return "undefined".to_string();
        }
        val.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_to_js() {
        assert_eq!(
            LiteralReconstructor::constant_to_js_static(&ConstantValue::Number(42.0)),
            "42"
        );
        assert_eq!(
            LiteralReconstructor::constant_to_js_static(&ConstantValue::String("hello".to_string())),
            "\"hello\""
        );
        assert_eq!(
            LiteralReconstructor::constant_to_js_static(&ConstantValue::Boolean(true)),
            "true"
        );
        assert_eq!(
            LiteralReconstructor::constant_to_js_static(&ConstantValue::Null),
            "null"
        );
        assert_eq!(
            LiteralReconstructor::constant_to_js_static(&ConstantValue::Undefined),
            "undefined"
        );
    }

    #[test]
    fn test_escape_string() {
        assert_eq!(
            LiteralReconstructor::escape_string("hello \"world\""),
            "hello \\\"world\\\""
        );
        assert_eq!(
            LiteralReconstructor::escape_string("line1\nline2"),
            "line1\\nline2"
        );
    }

    #[test]
    fn test_is_valid_identifier() {
        assert!(LiteralReconstructor::is_valid_identifier("foo"));
        assert!(LiteralReconstructor::is_valid_identifier("_bar"));
        assert!(LiteralReconstructor::is_valid_identifier("$baz"));
        assert!(LiteralReconstructor::is_valid_identifier("foo123"));
        
        assert!(!LiteralReconstructor::is_valid_identifier("123foo"));
        assert!(!LiteralReconstructor::is_valid_identifier("foo-bar"));
        assert!(!LiteralReconstructor::is_valid_identifier("foo bar"));
        assert!(!LiteralReconstructor::is_valid_identifier(""));
    }
}