use super::super::header::HbcHeader;
use super::super::serialized_literal_parser::{unpack_slp_array, SLPArray};
use super::function_table::FunctionTable;
use crate::generated::unified_instructions::UnifiedInstruction;
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct SerializedLiteralTables<'a> {
    pub arrays: SLPArray,
    pub object_keys: SLPArray,
    pub object_values: SLPArray,
    pub arrays_data: &'a [u8],
    pub object_keys_data: &'a [u8],
    pub object_values_data: &'a [u8],
}

impl<'a> SerializedLiteralTables<'a> {
    pub fn parse(data: &'a [u8], header: &HbcHeader, offset: &mut usize) -> Result<Self, String> {
        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse arrays
        let arrays_start = *offset;
        let arrays_size = header.array_buffer_size() as usize;
        if arrays_start + arrays_size > data.len() {
            return Err(format!(
                "Arrays would exceed data bounds: start=0x{:x}, size={}, data_len={}",
                arrays_start,
                arrays_size,
                data.len()
            ));
        }
        let arrays_data = &data[arrays_start..arrays_start + arrays_size];
        *offset += arrays_size;

        // eprintln!("array size: {}", arrays_size);
        let arrays = unpack_slp_array(arrays_data, None);

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse object keys
        let object_keys_start = *offset;
        let object_keys_size = header.obj_key_buffer_size() as usize;
        if object_keys_start + object_keys_size > data.len() {
            return Err(format!(
                "Object keys would exceed data bounds: start=0x{:x}, size={}, data_len={}",
                object_keys_start,
                object_keys_size,
                data.len()
            ));
        }
        let object_keys_data = &data[object_keys_start..object_keys_start + object_keys_size];
        *offset += object_keys_size;

        let object_keys = unpack_slp_array(object_keys_data, None);

        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);

        // Parse object values
        let object_values_start = *offset;
        let object_values_size = header.obj_value_buffer_size() as usize;
        if object_values_start + object_values_size > data.len() {
            return Err(format!(
                "Object values would exceed data bounds: start=0x{:x}, size={}, data_len={}",
                object_values_start,
                object_values_size,
                data.len()
            ));
        }
        let object_values_data =
            &data[object_values_start..object_values_start + object_values_size];
        *offset += object_values_size;

        let object_values = unpack_slp_array(object_values_data, None);

        // Packed buffers can overlap and are not necessarily linear tag streams.
        // Referenced sequences are validated using opcode counts after decoding.
        Ok(SerializedLiteralTables {
            arrays: arrays.unwrap_or_else(|_| SLPArray::new()),
            object_keys: object_keys.unwrap_or_else(|_| SLPArray::new()),
            object_values: object_values.unwrap_or_else(|_| SLPArray::new()),
            arrays_data,
            object_keys_data,
            object_values_data,
        })
    }

    pub fn validate_references(&mut self, functions: &FunctionTable<'_>) -> Result<(), String> {
        let mut references: [BTreeMap<u32, u32>; 3] = Default::default();
        let mut add = |buffer: usize, offset: u32, count: u32| {
            references[buffer]
                .entry(offset)
                .and_modify(|n| *n = (*n).max(count))
                .or_insert(count);
        };
        for index in 0..functions.count() {
            for instruction in functions
                .get_instructions_ref(index)
                .map_err(|e| e.to_string())?
            {
                macro_rules! reference {
                    ($($name:ident),*) => {
                        match &instruction.instruction {
                            $(UnifiedInstruction::$name { operand_2, operand_3, .. } => {
                                add(0, *operand_3 as u32, *operand_2 as u32);
                            },)*
                            _ => {}
                        }
                    };
                }
                macro_rules! object_reference {
                    ($($name:ident),*) => {
                        match &instruction.instruction {
                            $(UnifiedInstruction::$name { operand_2, operand_3, operand_4, .. } => {
                                add(1, *operand_3 as u32, *operand_2 as u32);
                                add(2, *operand_4 as u32, *operand_2 as u32);
                            },)*
                            _ => {}
                        }
                    };
                }
                reference!(NewArrayWithBuffer, NewArrayWithBufferLong);
                object_reference!(NewObjectWithBuffer, NewObjectWithBufferLong);
            }
        }
        for (buffer, (data, view)) in [
            (self.arrays_data, &mut self.arrays),
            (self.object_keys_data, &mut self.object_keys),
            (self.object_values_data, &mut self.object_values),
        ]
        .into_iter()
        .enumerate()
        {
            let linear_view = !view.items.is_empty() || data.is_empty();
            validate_buffer_references(data, view, linear_view, &references[buffer], buffer)?;
        }
        Ok(())
    }

    fn align_to_padding(offset: &mut usize, padding: usize) {
        let remainder = *offset % padding;
        if remainder != 0 {
            *offset += padding - remainder;
        }
    }

    pub fn arrays_count(&self) -> usize {
        self.arrays.items.len()
    }

    pub fn object_keys_count(&self) -> usize {
        self.object_keys.items.len()
    }

    pub fn object_values_count(&self) -> usize {
        self.object_values.items.len()
    }

    pub fn get_arrays_strings(&self, string_table: &[String]) -> Vec<String> {
        self.arrays.to_strings(string_table)
    }

    pub fn get_object_keys_strings(&self, string_table: &[String]) -> Vec<String> {
        self.object_keys.to_strings(string_table)
    }

    pub fn get_object_values_strings(&self, string_table: &[String]) -> Vec<String> {
        self.object_values.to_strings(string_table)
    }
}

fn validate_buffer_references(
    data: &[u8],
    view: &mut SLPArray,
    linear_view: bool,
    references: &BTreeMap<u32, u32>,
    buffer: usize,
) -> Result<(), String> {
    if !linear_view {
        view.items.clear();
    }
    for (&offset, &count) in references {
        let bytes = data
            .get(offset as usize..)
            .ok_or_else(|| format!("Literal buffer {buffer} offset {offset} out of bounds"))?;
        let sequence = unpack_slp_array(bytes, Some(count as usize))
            .map_err(|e| format!("Literal buffer {buffer}, offset {offset}, count {count}: {e}"))?;
        if !linear_view {
            view.items.extend(sequence.items);
        }
    }
    Ok(())
}

impl<'a> Serialize for SerializedLiteralTables<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("SerializedLiteralTables", 3)?;

        state.serialize_field("arrays", &self.arrays)?;
        state.serialize_field("object_keys", &self.object_keys)?;
        state.serialize_field("object_values", &self.object_values)?;
        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hbc::serialized_literal_parser::SLPValue;

    #[test]
    fn packed_views_use_referenced_offsets_not_a_linear_scan() {
        // Two valid overlapping sequences: a short-string ID at 0 and two
        // byte-string IDs at 1. A linear scan mistakes a payload for a tag.
        let data = [0x51, 0x62, 0x01, 0xc0];
        assert!(unpack_slp_array(&data, None).is_err());
        let mut view = SLPArray::new();
        validate_buffer_references(
            &data,
            &mut view,
            false,
            &BTreeMap::from([(0, 1), (1, 2)]),
            0,
        )
        .unwrap();
        assert_eq!(view.items.len(), 3);
        assert!(matches!(view.items[0], SLPValue::ShortString(354)));
        assert!(matches!(view.items[1], SLPValue::ByteString(1)));
        assert!(matches!(view.items[2], SLPValue::ByteString(192)));
        // Referencing a genuinely truncated sequence must still fail closed.
        assert!(
            validate_buffer_references(&data, &mut view, false, &BTreeMap::from([(3, 1)]), 0)
                .is_err()
        );
    }
}
