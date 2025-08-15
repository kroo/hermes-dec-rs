//! Type-safe wrappers for instruction addressing
//!
//! This module provides type-safe wrappers to distinguish between:
//! - Instruction offsets (absolute PC values in the bytecode)
//! - Instruction indices (positions within a function's instruction list)

use serde::{Deserialize, Serialize};
use std::fmt;

/// Absolute instruction offset in the bytecode (PC value)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct InstructionOffset(pub u32);

impl InstructionOffset {
    pub fn new(offset: u32) -> Self {
        Self(offset)
    }

    pub fn value(&self) -> u32 {
        self.0
    }
}

impl From<u32> for InstructionOffset {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<InstructionOffset> for u32 {
    fn from(offset: InstructionOffset) -> u32 {
        offset.0
    }
}

impl fmt::Display for InstructionOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Add<i32> for InstructionOffset {
    type Output = InstructionOffset;

    fn add(self, rhs: i32) -> Self::Output {
        InstructionOffset::from((self.0 as i32 + rhs) as u32)
    }
}

impl std::ops::Add<InstructionOffset> for u32 {
    type Output = u32;

    fn add(self, rhs: InstructionOffset) -> Self::Output {
        self + rhs.0
    }
}

/// Instruction index within a function (0-based position in function's instruction list)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct InstructionIndex(pub usize);

impl InstructionIndex {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn zero() -> Self {
        Self(0)
    }

    pub fn value(&self) -> usize {
        self.0
    }

    pub const MAX: InstructionIndex = InstructionIndex(usize::MAX);

    pub fn saturating_sub(self, rhs: usize) -> Self {
        Self(self.0.saturating_sub(rhs))
    }
}

impl std::ops::Add<i32> for InstructionIndex {
    type Output = InstructionIndex;

    fn add(self, rhs: i32) -> Self::Output {
        InstructionIndex::from((self.0 as i32 + rhs) as usize)
    }
}

impl std::ops::Add<InstructionIndex> for InstructionIndex {
    type Output = InstructionIndex;

    fn add(self, rhs: InstructionIndex) -> Self::Output {
        InstructionIndex::from(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for InstructionIndex {
    type Output = InstructionIndex;

    fn add(self, rhs: usize) -> Self::Output {
        InstructionIndex::from(self.0 + rhs)
    }
}

impl std::ops::Add<u32> for InstructionIndex {
    type Output = InstructionIndex;

    fn add(self, rhs: u32) -> Self::Output {
        InstructionIndex::from(self.0 + rhs as usize)
    }
}

impl From<usize> for InstructionIndex {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<InstructionIndex> for usize {
    fn from(index: InstructionIndex) -> usize {
        index.0
    }
}

impl From<u32> for InstructionIndex {
    fn from(value: u32) -> Self {
        Self(value as usize)
    }
}

impl From<InstructionIndex> for u32 {
    fn from(index: InstructionIndex) -> u32 {
        index.0 as u32
    }
}

impl fmt::Display for InstructionIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
