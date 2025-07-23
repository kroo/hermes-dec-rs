use serde::Serialize;
use super::super::header::HbcHeader;
use super::super::serialized_literal_parser::{SLPArray, unpack_slp_array};

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
            return Err(format!("Arrays would exceed data bounds: start=0x{:x}, size={}, data_len={}", 
                             arrays_start, arrays_size, data.len()));
        }
        let arrays_data = &data[arrays_start..arrays_start + arrays_size];
        *offset += arrays_size;
        
        // eprintln!("array size: {}", arrays_size);
        let arrays = unpack_slp_array(arrays_data, None)
            .map_err(|e| format!("Failed to parse arrays: {}", e))?;
        
        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);
        
        // Parse object keys
        let object_keys_start = *offset;
        let object_keys_size = header.obj_key_buffer_size() as usize;
        if object_keys_start + object_keys_size > data.len() {
            return Err(format!("Object keys would exceed data bounds: start=0x{:x}, size={}, data_len={}", 
                             object_keys_start, object_keys_size, data.len()));
        }
        let object_keys_data = &data[object_keys_start..object_keys_start + object_keys_size];
        *offset += object_keys_size;
        
        let object_keys = unpack_slp_array(object_keys_data, None)
            .map_err(|e| format!("Failed to parse object keys: {}", e))?;
        
        // Align to 4-byte boundary
        Self::align_to_padding(offset, 4);
        
        // Parse object values
        let object_values_start = *offset;
        let object_values_size = header.obj_value_buffer_size() as usize;
        if object_values_start + object_values_size > data.len() {
            return Err(format!("Object values would exceed data bounds: start=0x{:x}, size={}, data_len={}", 
                             object_values_start, object_values_size, data.len()));
        }
        let object_values_data = &data[object_values_start..object_values_start + object_values_size];
        *offset += object_values_size;
        
        let object_values = unpack_slp_array(object_values_data, None)
            .map_err(|e| format!("Failed to parse object values: {}", e))?;
        
        Ok(SerializedLiteralTables {
            arrays,
            object_keys,
            object_values,
            arrays_data,
            object_keys_data,
            object_values_data,
        })
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

impl<'a> Serialize for SerializedLiteralTables<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("SerializedLiteralTables", 3)?;
        
        // For now, use empty string table since we don't have access to it in serialization
        // This will be fixed by custom serialization in HbcFile
        state.serialize_field("arrays", &self.arrays)?;
        state.serialize_field("object_keys", &self.object_keys)?;
        state.serialize_field("object_values", &self.object_values)?;
        state.end()
    }
} 