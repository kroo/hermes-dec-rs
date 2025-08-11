use super::super::{
    header::HbcHeader,
    instruction_types::{InstructionIndex, InstructionOffset},
    HbcFile,
};
use super::string_table::StringTable;
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::generated::unified_instructions::UnifiedInstruction;
use regex::Regex;
use scroll::Pread;
use serde::{
    ser::{SerializeStruct, Serializer},
    Deserialize, Serialize,
};

#[derive(Debug, Clone, Copy, scroll::Pread)]
#[repr(C, packed)]
pub struct SmallFunctionHeader {
    // first word
    pub word_1: u32,
    // second word
    pub word_2: u32,
    // third word
    pub word_3: u32,
    // fourth word
    pub word_4: u32,
}

impl SmallFunctionHeader {
    pub fn offset(&self) -> u32 {
        self.word_1 & 0x1FFFFFF // 25 bits
    }

    pub fn param_count(&self) -> u32 {
        (self.word_1 >> 25) & 0x7F // 7 bits
    }

    pub fn bytecode_size_in_bytes(&self) -> u32 {
        self.word_2 & 0x7FFF // 15 bits
    }

    pub fn function_name(&self) -> u32 {
        (self.word_2 >> 15) & 0x1FFFF // 17 bits
    }

    pub fn info_offset(&self) -> u32 {
        self.word_3 & 0x1FFFFFF // 25 bits
    }

    pub fn frame_size(&self) -> u32 {
        (self.word_3 >> 25) & 0x7F // 7 bits
    }

    pub fn environment_size(&self) -> u8 {
        (self.word_4 & 0xFF) as u8 // 8 bits
    }

    pub fn highest_read_cache_index(&self) -> u8 {
        ((self.word_4 >> 8) & 0xFF) as u8 // 8 bits
    }

    pub fn highest_write_cache_index(&self) -> u8 {
        ((self.word_4 >> 16) & 0xFF) as u8 // 8 bits
    }

    pub fn prohibit_invoke(&self) -> u8 {
        ((self.word_4 >> 24) & 0x3) as u8 // 2 bits
    }

    pub fn strict_mode(&self) -> bool {
        ((self.word_4 >> 26) & 0x1) == 1 // 1 bit
    }

    pub fn has_exception_handler(&self) -> bool {
        ((self.word_4 >> 27) & 0x1) == 1 // 1 bit
    }

    pub fn has_debug_info(&self) -> bool {
        ((self.word_4 >> 28) & 0x1) == 1 // 1 bit
    }

    pub fn overflowed(&self) -> bool {
        ((self.word_4 >> 29) & 0x1) == 1 // 1 bit
    }

    pub fn flags(&self) -> u32 {
        self.word_4 & 0xFFFFFFF
    }
}

impl std::fmt::Display for SmallFunctionHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SmallFunctionHeader(offset=0x{:x}, param_count={}, bytecode_size=0x{:x}, function_name=0x{:x}, info_offset=0x{:x}, frame_size=0x{:x}, environment_size=0x{:x}{}{}{}{}{}{}{})",
            self.offset(),
            self.param_count(),
            self.bytecode_size_in_bytes(),
            self.function_name(),
            self.info_offset(),
            self.frame_size(),
            self.environment_size(),
            if self.highest_read_cache_index() > 0 { format!(", highest_read_cache={}", self.highest_read_cache_index()) } else { String::new() },
            if self.highest_write_cache_index() > 0 { format!(", highest_write_cache={}", self.highest_write_cache_index()) } else { String::new() },
            if self.prohibit_invoke() > 0 { format!(", prohibit_invoke={}", self.prohibit_invoke()) } else { String::new() },
            if self.strict_mode() { ", strict_mode=true" } else { "" },
            if self.has_exception_handler() { ", has_exception_handler=true" } else { "" },
            if self.has_debug_info() { ", has_debug_info=true" } else { "" },
            if self.overflowed() { ", overflowed=true" } else { "" })
    }
}

#[derive(Debug, Clone, Copy, scroll::Pread)]
#[repr(C, packed)]
pub struct LargeFunctionHeader {
    pub offset: u32,
    pub param_count: u32,
    pub bytecode_size_in_bytes: u32,
    pub function_name: u32,
    pub info_offset: u32,
    pub frame_size: u32,
    pub environment_size: u32,
    pub highest_read_cache_index: u8,
    pub highest_write_cache_index: u8,
    pub flags: u8, // prohibit_invoke: 2, strict_mode: 1, has_exception_handler: 1, has_debug_info: 1, overflowed: 1, unused: 2
}

#[derive(Debug, Clone, Copy, scroll::Pread)]
#[repr(C, packed)]
pub struct ExceptionHandlerInfo {
    pub start: u32,
    pub end: u32,
    pub target: u32,
}

#[derive(Debug, Clone, Copy, scroll::Pread)]
#[repr(C, packed)]
pub struct DebugOffsetsModern {
    pub source_locations: u32,
    pub scope_desc_data: u32,
    pub textified_callees: u32, // Only present in version >= 91 (except 92)
}

#[derive(Debug, Clone, Copy, scroll::Pread)]
#[repr(C, packed)]
pub struct DebugOffsetsLegacy {
    pub source_locations: u32,
    pub scope_desc_data: u32,
}

#[derive(Debug)]
pub enum DebugOffsets {
    Modern(DebugOffsetsModern),
    Legacy(DebugOffsetsLegacy),
}

fn parse_debug_offsets(data: &[u8], version: u32, offset: usize) -> Result<DebugOffsets, String> {
    let source_locations = u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap());
    let scope_desc_data = u32::from_le_bytes(data[offset + 4..offset + 8].try_into().unwrap());

    if version >= 91 && version != 92 {
        Ok(DebugOffsets::Modern(DebugOffsetsModern {
            source_locations,
            scope_desc_data,
            textified_callees: u32::from_le_bytes(
                data[offset + 8..offset + 12].try_into().unwrap(),
            ),
        }))
    } else {
        Ok(DebugOffsets::Legacy(DebugOffsetsLegacy {
            source_locations,
            scope_desc_data,
        }))
    }
}

#[derive(Debug)]
pub struct FunctionTable<'a> {
    pub headers: &'a [u8],
    pub count: u32,
    pub parsed_headers: Vec<ParsedFunctionHeader<'a>>,
}

#[derive(Debug)]
pub struct ParsedFunctionHeader<'a> {
    pub index: u32,
    pub header: SmallFunctionHeader,
    pub large_header: Option<LargeFunctionHeader>,
    pub exc_handlers: Vec<ExceptionHandlerInfo>,
    pub debug_offsets: DebugOffsets,
    pub body: &'a [u8],
    pub version: u32,
    /// Cached parsed instructions to avoid re-parsing
    pub cached_instructions: std::sync::OnceLock<DecompilerResult<Vec<HbcFunctionInstruction>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HbcFunctionInstruction {
    /// The absolute byte offset of the instruction in the bytecode
    pub offset: InstructionOffset,
    /// The function index
    pub function_index: u32,
    /// The instruction index within the function (0-based position in function's instruction list)
    pub instruction_index: InstructionIndex,
    /// The instruction contents itself
    pub instruction: UnifiedInstruction,
}

/// HBC function information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HbcFunction {
    /// Function name (from string table)
    pub name: String,
    /// Function flags
    pub flags: u32,
    /// Parameter count
    pub param_count: u32,
    /// Environment size
    pub env_size: u32,
    /// Instruction count
    pub instruction_count: u32,
    /// Instructions
    pub instructions: Vec<HbcFunctionInstruction>,
}

impl HbcFunctionInstruction {
    pub fn format_instruction(&self, hbc_file: &HbcFile) -> String {
        static JUMP_LABEL_REGEX: once_cell::sync::Lazy<Regex> =
            once_cell::sync::Lazy::new(|| Regex::new(r"\[\-?\d+\]").unwrap());

        let formatted_instruction = self.instruction.format_instruction(hbc_file);
        let jump_operand = hbc_file
            .jump_table
            .get_label_by_jump_op_index(self.function_index, self.instruction_index.into());
        if let Some(label) = jump_operand {
            // replace [\d+] with label
            JUMP_LABEL_REGEX
                .replace_all(&formatted_instruction, label)
                .to_string()
        } else {
            formatted_instruction
        }
    }

    pub fn name(&self) -> &str {
        self.instruction.name()
    }

    pub fn category(&self) -> &str {
        self.instruction.category()
    }
}

impl<'a> FunctionTable<'a> {
    pub fn parse(
        data: &'a [u8],
        header: &HbcHeader,
        offset: &mut usize,
    ) -> Result<Self, scroll::Error> {
        let count = header.function_count();
        let mut parsed_headers = Vec::with_capacity(count as usize);

        // Align offset to 4 bytes
        if *offset % 4 != 0 {
            *offset += 4 - (*offset % 4);
        }

        for i in 0..count {
            // Read small header first
            let _header_start = *offset;

            let small_header: SmallFunctionHeader = data.gread_with(offset, scroll::LE)?;
            let header_pos = *offset;

            // Handle overflow case and parse large header if needed
            let mut large_header = None;
            if small_header.overflowed() {
                // Check overflowed bit
                let mut new_offset =
                    ((small_header.info_offset() << 16) | small_header.offset()) as usize;
                let large_header_data: LargeFunctionHeader =
                    data.gread_with(&mut new_offset, scroll::LE)?;
                large_header = Some(large_header_data);
                *offset = new_offset + std::mem::size_of::<LargeFunctionHeader>();
            } else {
                *offset = small_header.info_offset() as usize;
            }

            // Read exception handlers if present
            let mut exc_handlers = Vec::new();
            if small_header.has_exception_handler() {
                // Check has_exception_handler bit
                // Align to 4 bytes
                if *offset % 4 != 0 {
                    *offset += 4 - (*offset % 4);
                }

                let exc_count = u32::from_le_bytes(data[*offset..*offset + 4].try_into().unwrap());
                *offset += 4;

                // Read exception handler info
                for _ in 0..exc_count {
                    let exc_handler: ExceptionHandlerInfo = data.gread_with(offset, scroll::LE)?;
                    exc_handlers.push(exc_handler);
                }
            }

            // Read debug info if present
            let debug_offsets =
                if small_header.has_debug_info() {
                    // Check has_debug_info bit
                    // Align to 4 bytes
                    if *offset % 4 != 0 {
                        *offset += 4 - (*offset % 4);
                    }

                    // Read debug info
                    let debug_offsets = parse_debug_offsets(data, header.version(), *offset)
                        .map_err(|_| scroll::Error::BadInput {
                            size: 0,
                            msg: "Failed to parse debug offsets",
                        })?;
                    *offset += if header.version() >= 91 && header.version() != 92 {
                        12
                    } else {
                        8
                    };
                    debug_offsets
                } else {
                    DebugOffsets::Legacy(DebugOffsetsLegacy {
                        source_locations: 0,
                        scope_desc_data: 0,
                    })
                };

            let body_offset = if small_header.overflowed() {
                large_header.unwrap().offset as usize
            } else {
                small_header.offset() as usize
            };
            let body_size = if small_header.overflowed() {
                large_header.unwrap().bytecode_size_in_bytes as usize
            } else {
                small_header.bytecode_size_in_bytes() as usize
            };

            // Create the complete parsed function header
            let parsed_header = ParsedFunctionHeader {
                index: i,
                header: small_header,
                large_header,
                exc_handlers,
                debug_offsets,
                body: &data[body_offset..body_offset + body_size],
                version: header.version(),
                cached_instructions: std::sync::OnceLock::new(),
            };

            parsed_headers.push(parsed_header);
            *offset = header_pos;
        }

        Ok(FunctionTable {
            headers: data,
            count,
            parsed_headers,
        })
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    pub fn get_function_name(&self, index: u32, string_table: &StringTable) -> Option<String> {
        if index >= self.parsed_headers.len() as u32 {
            return None;
        }

        let parsed_header = &self.parsed_headers[index as usize];
        let name_index = parsed_header.header.function_name() as u32;

        // Use O(1) string lookup instead of extracting all strings
        match string_table.get(name_index) {
            Ok(name) => Some(name),
            Err(_) => Some(format!("function_{}", name_index)),
        }
    }

    pub fn get_placeholder(&self, index: u32) -> Option<String> {
        if index < self.count {
            Some(format!("function_{}", index))
        } else {
            None
        }
    }

    pub fn get_parsed_header(&self, index: u32) -> Option<&ParsedFunctionHeader<'a>> {
        if index < self.parsed_headers.len() as u32 {
            Some(&self.parsed_headers[index as usize])
        } else {
            None
        }
    }

    pub fn get(&self, index: u32, hbc_file: &HbcFile) -> DecompilerResult<HbcFunction> {
        if index < self.count {
            let parsed_header = &self.parsed_headers[index as usize];
            let instructions = parsed_header.instructions()?;
            Ok(HbcFunction {
                name: self
                    .get_function_name(index, &hbc_file.strings)
                    .unwrap_or_else(|| format!("function_{}", index)),
                flags: parsed_header.header.flags(),
                param_count: parsed_header.header.param_count(),
                env_size: parsed_header.header.environment_size() as u32,
                instruction_count: parsed_header.header.bytecode_size_in_bytes(),
                instructions,
            })
        } else {
            Err(DecompilerError::Internal {
                message: format!("Function index {} out of bounds", index),
            })
        }
    }

    /// Get just the instructions for a function (more efficient than get() for jump table construction)
    pub fn get_instructions(&self, index: u32) -> DecompilerResult<Vec<HbcFunctionInstruction>> {
        if index < self.count {
            let parsed_header = &self.parsed_headers[index as usize];
            parsed_header.instructions()
        } else {
            Err(DecompilerError::Internal {
                message: format!("Function index {} out of bounds", index),
            })
        }
    }

    /// Get a single instruction by index
    pub fn get_instruction(
        &self,
        function_index: u32,
        instruction_index: InstructionIndex,
    ) -> DecompilerResult<HbcFunctionInstruction> {
        if function_index < self.count {
            let parsed_header = &self.parsed_headers[function_index as usize];
            parsed_header.instruction(instruction_index)
        } else {
            Err(DecompilerError::Internal {
                message: format!("Function index {} out of bounds", function_index),
            })
        }
    }
}

impl<'a> ParsedFunctionHeader<'a> {
    /// Parse the function body into instructions.
    /// This method caches the parsed instructions to avoid re-parsing.
    pub fn instructions(&self) -> DecompilerResult<Vec<HbcFunctionInstruction>> {
        if let Some(result) = self.cached_instructions.get() {
            return result.clone();
        }

        let result = self.parse_instructions();
        let _ = self.cached_instructions.set(result.clone());
        result
    }

    pub fn instruction(
        &self,
        instruction_index: InstructionIndex,
    ) -> DecompilerResult<HbcFunctionInstruction> {
        let instructions = self.instructions()?;
        let index = instruction_index.value();

        if index < instructions.len() {
            Ok(instructions[index].clone())
        } else {
            Err(DecompilerError::Internal {
                message: format!("Instruction index {} out of bounds", instruction_index),
            })
        }
    }

    /// Internal method to parse instructions from the function body.
    fn parse_instructions(&self) -> DecompilerResult<Vec<HbcFunctionInstruction>> {
        let mut instructions = Vec::new();
        let mut offset = 0;

        while offset < self.body.len() {
            if offset >= self.body.len() {
                break;
            }
            let start_offset = offset;
            let opcode = self.body[offset];
            offset += 1;

            match UnifiedInstruction::parse(self.version, opcode, self.body, &mut offset) {
                Ok((instruction, _bytes_read)) => {
                    instructions.push(HbcFunctionInstruction {
                        offset: InstructionOffset::from(start_offset as u32),
                        function_index: self.index as u32,
                        instruction_index: InstructionIndex::from(instructions.len()),
                        instruction,
                    });
                }
                Err(e) => {
                    return Err(DecompilerError::Internal {
                        message: format!(
                            "Failed to parse instruction at offset {}: {}",
                            start_offset, e
                        ),
                    });
                }
            }
        }

        Ok(instructions)
    }
}

impl<'a> Serialize for FunctionTable<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("FunctionTable", 2)?;
        state.serialize_field("count", &self.count())?;
        state.serialize_field("functions", &{
            let mut seq = Vec::with_capacity(self.count() as usize);
            for i in 0..self.count() {
                // For now, still use placeholder since we don't have access to string table here
                seq.push(self.get_placeholder(i as u32));
            }
            seq
        })?;
        state.end()
    }
}
