//! High-level analysis of HBC files
//!
//! This module provides cross-function analysis capabilities including:
//! - Function relationship analysis
//! - Global variable tracking
//! - Module boundary detection
//! - Bundle analysis

pub mod constructor_detector;
pub mod default_params;
pub mod function_classifier;
pub mod global_ssa;
pub mod value_tracker;

pub use constructor_detector::{ConstructorDetector, ConstructorInfo};
pub use default_params::{DefaultParameterAnalyzer, DefaultParameterInfo};
pub use function_classifier::{FunctionClassifier, FunctionType};
pub use global_ssa::{GlobalAnalysisResult, GlobalSSAAnalyzer};
pub use value_tracker::{ConstantValue, TrackedValue, ValueTracker};
