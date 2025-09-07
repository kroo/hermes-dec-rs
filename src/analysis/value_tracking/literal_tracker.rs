//! Literal tracking from serialized literal buffers
//!
//! This module handles tracking of array and object literals that are created
//! from Hermes's serialized literal buffers.

use crate::analysis::value_tracking::{ConstantValue, TrackedValue};
use crate::hbc::serialized_literal_parser::{unpack_slp_array, SLPValue};
use crate::hbc::HbcFile;

/// Tracks literal values from serialized buffers
pub struct LiteralTracker<'a> {
    hbc_file: &'a HbcFile<'a>,
}

impl<'a> LiteralTracker<'a> {
    /// Create a new literal tracker
    pub fn new(hbc_file: &'a HbcFile<'a>) -> Self {
        Self { hbc_file }
    }

    /// Track an array literal from NewArrayWithBuffer instruction
    pub fn track_array_literal(
        &self,
        _size_hint: u16,
        num_literals: u16,
        buffer_start_index: u32,
    ) -> TrackedValue {
        // Try to look up the array data from the serialized literals
        let literal_tables = &self.hbc_file.serialized_literals;

        // Validate bounds
        if buffer_start_index as usize >= literal_tables.arrays_data.len() {
            return TrackedValue::Unknown;
        }

        // Parse the array from serialized data
        let slice_from_offset = &literal_tables.arrays_data[buffer_start_index as usize..];
        let parsed_array = match unpack_slp_array(slice_from_offset, Some(num_literals as usize)) {
            Ok(array) => array,
            Err(_) => return TrackedValue::Unknown,
        };

        // Convert SLPValues to ConstantValues
        let mut elements = Vec::new();
        for slp_value in parsed_array.items {
            match self.slp_value_to_constant(&slp_value) {
                Some(const_val) => elements.push(const_val),
                None => return TrackedValue::Unknown, // If any element can't be tracked as constant, don't track the array
            }
        }

        TrackedValue::Constant(ConstantValue::ArrayLiteral(elements))
    }

    /// Track an object literal from NewObjectWithBuffer instruction
    pub fn track_object_literal(
        &self,
        _size_hint: u16,
        num_literals: u16,
        key_buffer_start_index: u32,
        value_buffer_start_index: u32,
    ) -> TrackedValue {
        let literal_tables = &self.hbc_file.serialized_literals;

        // Validate bounds for keys
        let key_end_index = key_buffer_start_index + num_literals as u32;
        let value_end_index = value_buffer_start_index + num_literals as u32;

        if key_end_index as usize > literal_tables.object_keys.items.len()
            || value_end_index as usize > literal_tables.object_values.items.len()
        {
            return TrackedValue::Unknown;
        }

        // Get keys and values
        let keys = &literal_tables.object_keys.items
            [key_buffer_start_index as usize..key_end_index as usize];
        let values = &literal_tables.object_values.items
            [value_buffer_start_index as usize..value_end_index as usize];

        // Convert to constant key-value pairs
        let mut properties = Vec::new();
        for (key, value) in keys.iter().zip(values.iter()) {
            // Keys must be strings
            let key_str = match self.slp_value_to_string(key) {
                Some(s) => s,
                None => return TrackedValue::Unknown,
            };

            // Convert value to constant
            let const_val = match self.slp_value_to_constant(value) {
                Some(v) => v,
                None => return TrackedValue::Unknown,
            };

            properties.push((key_str, const_val));
        }

        TrackedValue::Constant(ConstantValue::ObjectLiteral(properties))
    }

    /// Convert an SLPValue to a ConstantValue
    pub fn slp_value_to_constant(&self, slp_value: &SLPValue) -> Option<ConstantValue> {
        match slp_value {
            SLPValue::Null => Some(ConstantValue::Null),
            SLPValue::True => Some(ConstantValue::Boolean(true)),
            SLPValue::False => Some(ConstantValue::Boolean(false)),
            SLPValue::Number(n) => Some(ConstantValue::Number(*n)),
            SLPValue::Integer(i) => Some(ConstantValue::Number(*i as f64)),
            SLPValue::LongString(id) => self
                .hbc_file
                .strings
                .get((*id).into())
                .ok()
                .map(|s| ConstantValue::String(s)),
            SLPValue::ShortString(id) => self
                .hbc_file
                .strings
                .get((*id as u32).into())
                .ok()
                .map(|s| ConstantValue::String(s)),
            SLPValue::ByteString(id) => self
                .hbc_file
                .strings
                .get((*id as u32).into())
                .ok()
                .map(|s| ConstantValue::String(s)),
        }
    }

    /// Convert an SLPValue to a string (for object keys)
    pub fn slp_value_to_string(&self, slp_value: &SLPValue) -> Option<String> {
        match slp_value {
            SLPValue::LongString(id) => self.hbc_file.strings.get((*id).into()).ok(),
            SLPValue::ShortString(id) => self.hbc_file.strings.get((*id as u32).into()).ok(),
            SLPValue::ByteString(id) => self.hbc_file.strings.get((*id as u32).into()).ok(),
            _ => None, // Non-string keys not supported for now
        }
    }
}