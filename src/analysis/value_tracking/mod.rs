//! Value tracking subsystem modules

pub mod constant_folding;
pub mod escape_analysis;
pub mod literal_tracker;
pub mod pattern_detection;
pub mod reconstruction;
pub mod types;

// Re-export commonly used types
pub use constant_folding::ConstantFolder;
pub use escape_analysis::{EscapeAnalysisResult, EscapeAnalyzer, EscapeReason};
pub use literal_tracker::LiteralTracker;
pub use pattern_detection::{ConstructionPattern, PatternDetector};
pub use reconstruction::{LiteralReconstructor, ReconstructionResult};
pub use types::{ConstantValue, MutationKind, ObjectBaseType, ObjectMutation, TrackedValue};