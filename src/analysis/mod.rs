//! High-level analysis of HBC files
//!
//! This module provides cross-function analysis capabilities including:
//! - Function relationship analysis
//! - Global variable tracking
//! - Module boundary detection
//! - Bundle analysis
//! - Call site analysis for tracking argument registers

pub mod call_site_analysis;
pub mod constructor_detector;
pub mod control_flow_plan;
pub mod control_flow_plan_analyzer;
pub mod control_flow_plan_builder;
pub mod default_params;
pub mod function_analysis;
pub mod function_classifier;
pub mod global_ssa;
pub mod hbc_analysis;
pub mod package_analysis;
pub mod optimization_passes;
pub mod ssa_usage_tracker;
pub mod value_tracker;
pub mod value_tracking;

pub use call_site_analysis::{CallSiteAnalysis, CallSiteInfo};
pub use constructor_detector::{ConstructorDetector, ConstructorInfo};
pub use control_flow_plan::{ControlFlowPlan, ControlFlowStructure};
pub use default_params::{DefaultParameterAnalyzer, DefaultParameterInfo};
pub use function_analysis::FunctionAnalysis;
pub use function_classifier::{FunctionClassifier, FunctionType};
pub use global_ssa::{GlobalAnalysisResult, GlobalSSAAnalyzer};
pub use hbc_analysis::HbcAnalysis;
pub use package_analysis::{ModuleInfo, PackageReport, PackageStats};
pub use value_tracker::{ConstantValue, TrackedValue, ValueTracker};
pub use value_tracking::{EscapeAnalysisResult, EscapeAnalyzer, EscapeReason};
