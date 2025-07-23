use scroll::Pread;
use scroll::ctx::SizeWith;
use serde::{Deserialize, Serialize};

/// Context for parsing HBC headers with version-specific format information
#[derive(Debug, Clone, Copy)]
pub struct HbcContext {
    /// HBC version that determines field presence
    pub version: u32,
    /// Endianness for parsing
    pub endian: scroll::Endian,
}

impl HbcContext {
    /// Create a new context for parsing
    pub fn new(version: u32, endian: scroll::Endian) -> Self {
        Self { version, endian }
    }
    
    /// Check if BigInt fields are present (version >= 87)
    pub fn has_big_int_support(&self) -> bool {
        self.version >= 87
    }
    
    /// Check if segment_id is used (version >= 78)
    pub fn has_segment_id_support(&self) -> bool {
        self.version >= 78
    }
    
    /// Check if function_source_count is present (version >= 84)
    pub fn has_function_source_count_support(&self) -> bool {
        self.version >= 84
    }
}

/// Complete HBC header structure that handles all versions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HbcHeader {
    /// Magic number (0x1f1903c103bc1fc6) - identifies this as a valid HBC file
    pub magic: u64,
    
    /// HBC version - determines which fields are present and instruction set
    pub version: u32,
    
    /// SHA1 hash of the source file - used for integrity checking and caching
    pub source_hash: [u8; 20],
    
    /// Total file length in bytes (until the end of BytecodeFileFooter)
    pub file_length: u32,
    
    /// Index of the global code function (entry point)
    pub global_code_index: u32,
    
    /// Total number of functions in the bytecode file
    pub function_count: u32,
    
    /// Number of string kind entries in the string table
    pub string_kind_count: u32,
    
    /// Number of strings that are identifiers (used for property access optimization)
    pub identifier_count: u32,
    
    /// Number of strings in the main string table
    pub string_count: u32,
    
    /// Number of strings in the overflow string table
    pub overflow_string_count: u32,
    
    /// Size in bytes of the string storage blob
    pub string_storage_size: u32,
    
    /// Number of BigInt literals in the BigInt table (version >= 87)
    pub big_int_count: Option<u32>,
    
    /// Size in bytes of the BigInt storage blob (version >= 87)
    pub big_int_storage_size: Option<u32>,
    
    /// Number of RegExp literals in the RegExp table
    pub reg_exp_count: u32,
    
    /// Size in bytes of the RegExp storage blob
    pub reg_exp_storage_size: u32,
    
    /// Size in bytes of the array buffer
    pub array_buffer_size: u32,
    
    /// Size in bytes of the object key buffer
    pub obj_key_buffer_size: u32,
    
    /// Size in bytes of the object value buffer
    pub obj_value_buffer_size: u32,
    
    /// CJS module offset in the file (version < 78)
    pub cjs_module_offset: Option<u32>,
    
    /// Segment ID for bundle splitting (version >= 78)
    pub segment_id: Option<u32>,
    
    /// Number of CommonJS modules in the file
    pub cjs_module_count: u32,
    
    /// Number of function sources preserved for debugging (version >= 84)
    pub function_source_count: Option<u32>,
    
    /// Byte offset to debug information in the file
    pub debug_info_offset: u32,
    
    /// Flags byte containing various boolean flags
    pub flags: u8,
}

impl HbcHeader {
    pub fn magic(&self) -> u64 {
        self.magic
    }
    
    pub fn version(&self) -> u32 {
        self.version
    }
    
    pub fn source_hash(&self) -> &[u8; 20] {
        &self.source_hash
    }
    
    pub fn file_length(&self) -> u32 {
        self.file_length
    }
    
    pub fn global_code_index(&self) -> u32 {
        self.global_code_index
    }
    
    pub fn function_count(&self) -> u32 {
        self.function_count
    }
    
    pub fn string_kind_count(&self) -> u32 {
        self.string_kind_count
    }
    
    pub fn identifier_count(&self) -> u32 {
        self.identifier_count
    }
    
    pub fn string_count(&self) -> u32 {
        self.string_count
    }
    
    pub fn overflow_string_count(&self) -> u32 {
        self.overflow_string_count
    }
    
    pub fn string_storage_size(&self) -> u32 {
        self.string_storage_size
    }
    
    pub fn big_int_count(&self) -> Option<u32> {
        self.big_int_count
    }
    
    pub fn big_int_storage_size(&self) -> Option<u32> {
        self.big_int_storage_size
    }
    
    pub fn reg_exp_count(&self) -> u32 {
        self.reg_exp_count
    }
    
    pub fn reg_exp_storage_size(&self) -> u32 {
        self.reg_exp_storage_size
    }
    
    pub fn array_buffer_size(&self) -> u32 {
        self.array_buffer_size
    }
    
    pub fn obj_key_buffer_size(&self) -> u32 {
        self.obj_key_buffer_size
    }
    
    pub fn obj_value_buffer_size(&self) -> u32 {
        self.obj_value_buffer_size
    }
    
    pub fn cjs_module_offset(&self) -> Option<u32> {
        self.cjs_module_offset
    }
    
    pub fn segment_id(&self) -> Option<u32> {
        self.segment_id
    }
    
    pub fn cjs_module_count(&self) -> u32 {
        self.cjs_module_count
    }
    
    pub fn function_source_count(&self) -> Option<u32> {
        self.function_source_count
    }
    
    pub fn debug_info_offset(&self) -> u32 {
        self.debug_info_offset
    }
    
    pub fn static_builtins(&self) -> bool {
        (self.flags & 0x01) != 0
    }
    
    pub fn cjs_modules_statically_resolved(&self) -> bool {
        (self.flags & 0x02) != 0
    }
    
    pub fn has_async(&self) -> bool {
        (self.flags & 0x04) != 0
    }
}

impl SizeWith for HbcHeader {
    fn size_with(_ctx: &()) -> usize {
        128 // Approximate size, varies by version
    }
}

impl scroll::ctx::TryFromCtx<'_, HbcContext> for HbcHeader {
    type Error = scroll::Error;
    fn try_from_ctx(src: &[u8], ctx: HbcContext) -> Result<(Self, usize), Self::Error> {
        let mut offset = 0;
        
        // Parse base header fields (common to all versions)
        let magic = src.gread_with(&mut offset, ctx.endian)?;
        let version = src.gread_with(&mut offset, ctx.endian)?;
        let source_hash: [u8; 20] = src.gread_with(&mut offset, ctx.endian)?;
        let file_length = src.gread_with(&mut offset, ctx.endian)?;
        let global_code_index = src.gread_with(&mut offset, ctx.endian)?;
        let function_count = src.gread_with(&mut offset, ctx.endian)?;
        let string_kind_count = src.gread_with(&mut offset, ctx.endian)?;
        let identifier_count = src.gread_with(&mut offset, ctx.endian)?;
        let string_count = src.gread_with(&mut offset, ctx.endian)?;
        let overflow_string_count = src.gread_with(&mut offset, ctx.endian)?;
        let string_storage_size = src.gread_with(&mut offset, ctx.endian)?;
        
        // Parse version-specific fields
        let (big_int_count, big_int_storage_size) = if ctx.has_big_int_support() {
            let count = src.gread_with(&mut offset, ctx.endian)?;
            let size = src.gread_with(&mut offset, ctx.endian)?;
            (Some(count), Some(size))
        } else {
            (None, None)
        };
        
        let reg_exp_count = src.gread_with(&mut offset, ctx.endian)?;
        let reg_exp_storage_size = src.gread_with(&mut offset, ctx.endian)?;
        let array_buffer_size = src.gread_with(&mut offset, ctx.endian)?;
        let obj_key_buffer_size = src.gread_with(&mut offset, ctx.endian)?;
        let obj_value_buffer_size = src.gread_with(&mut offset, ctx.endian)?;
        
        // Parse segment/module fields based on version
        let (cjs_module_offset, segment_id) = if ctx.has_segment_id_support() {
            let seg_id = src.gread_with(&mut offset, ctx.endian)?;
            (None, Some(seg_id))
        } else {
            let module_offset = src.gread_with(&mut offset, ctx.endian)?;
            (Some(module_offset), None)
        };
        
        let cjs_module_count = src.gread_with(&mut offset, ctx.endian)?;
        
        // Parse function source count if supported
        let function_source_count = if ctx.has_function_source_count_support() {
            let count = src.gread_with(&mut offset, ctx.endian)?;
            Some(count)
        } else {
            None
        };
        
        let debug_info_offset = src.gread_with(&mut offset, ctx.endian)?;
        let flags = src.gread_with(&mut offset, ctx.endian)?;
        
        // Skip padding to align to 32 bytes
        let padding_size = (32 - (offset % 32)) % 32;
        offset += padding_size;
        
        Ok((HbcHeader {
            magic,
            version,
            source_hash,
            file_length,
            global_code_index,
            function_count,
            string_kind_count,
            identifier_count,
            string_count,
            overflow_string_count,
            string_storage_size,
            big_int_count,
            big_int_storage_size,
            reg_exp_count,
            reg_exp_storage_size,
            array_buffer_size,
            obj_key_buffer_size,
            obj_value_buffer_size,
            cjs_module_offset,
            segment_id,
            cjs_module_count,
            function_source_count,
            debug_info_offset,
            flags,
        }, offset))
    }
} 