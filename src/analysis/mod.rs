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
pub mod function_analysis;
pub mod global_ssa;
pub mod hbc_analysis;
pub mod value_tracker;

pub use constructor_detector::{ConstructorDetector, ConstructorInfo};
pub use default_params::{DefaultParameterAnalyzer, DefaultParameterInfo};
pub use function_classifier::{FunctionClassifier, FunctionType};
pub use function_analysis::FunctionAnalysis;
pub use global_ssa::{GlobalAnalysisResult, GlobalSSAAnalyzer};
pub use hbc_analysis::HbcAnalysis;
pub use value_tracker::{ConstantValue, TrackedValue, ValueTracker};
