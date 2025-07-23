#![allow(dead_code)]
//! Hermes Bytecode (HBC) parsing module
//! 
//! This module handles parsing of HBC file headers, tables, and instruction streams.

use scroll::Pread;
use serde::Serialize;
use serde::ser::{Serializer, SerializeStruct};
use scroll::ctx::TryFromCtx;
use rayon::prelude::*;

pub mod header;
pub mod instructions;
pub mod tables;
pub mod regexp_bytecode;
pub mod serialized_literal_parser;

pub use header::*;
pub use tables::*;

/// Magic number for HBC files
pub const HBC_MAGIC: u64 = 0x1f1903c103bc1fc6;


/// Complete HBC file structure
#[derive(Debug)]
pub struct HbcFile<'a> {
    /// File header
    pub header: HbcHeader,
    /// String table
    pub strings: StringTable<'a>,
    /// Regular expression table
    pub regexps: RegExpTable<'a>,
    /// BigInt table
    pub bigints: BigIntTable<'a>,
    /// Function table
    pub functions: FunctionTable<'a>,
    /// Serialized literal tables (arrays, object keys, object values)
    pub serialized_literals: SerializedLiteralTables<'a>,
    /// CommonJS module table
    pub cjs_modules: CommonJsTable,
    /// Function source table
    pub function_sources: FunctionSourceTable,
    /// Jump table
    pub jump_table: JumpTable,
}

fn min_supported_version() -> u32 {
    51
}

fn max_supported_version() -> u32 {
    96
}

impl<'a> HbcFile<'a> {
    /// Get a string by ID with caching for better performance (placeholder)
    pub fn get_cached_string(&self, string_id: u32) -> Result<String, String> {
        self.strings.get(string_id)
    }

    /// Parse an HBC file from a byte slice
    pub fn parse(data: &'a [u8]) -> Result<Self, String> {
        let mut offset = 0;
        
        // Step 1: Parse base header to get version for context
        let magic = data.gread::<u64>(&mut offset)
            .map_err(|e| format!("Failed to parse magic: {}", e))?;

        if magic != HBC_MAGIC {
            return Err(format!("Invalid magic number: expected 0x{:016X}, got 0x{:016X}", 
                              HBC_MAGIC, magic));
        }

        let version = data.gread::<u32>(&mut offset)
            .map_err(|e| format!("Failed to parse version: {}", e))?;
                
        // Step 2: Create context with actual version
        let ctx = HbcContext::new(version, scroll::LE);
        
        // Step 3: Parse full header using TryFromCtx
        let (header, header_size) = HbcHeader::try_from_ctx(&data[0..], ctx)
            .map_err(|e| format!("Failed to parse header: {}", e))?;
        
        // ensure the offset is set to the end of the header
        offset = header_size;

        println!("Header parsed successfully:");
        println!("  Magic: 0x{:016X}", header.magic());
        println!("  Version: {}", header.version());
        println!("  Function count: {}", header.function_count());
        println!("  String count: {}", header.string_count());
        println!("  String storage size: {}", header.string_storage_size());
        println!("  RegExp count: {}", header.reg_exp_count());
        println!("  RegExp storage size: {}", header.reg_exp_storage_size());
        println!("  CJS module count: {}", header.cjs_module_count());
        println!("  CJS modules statically resolved: {}", header.cjs_modules_statically_resolved());
        println!("  Debug info offset: {}", header.debug_info_offset());
        if let Some(count) = header.big_int_count() {
            println!("  BigInt count: {}", count);
        }

        // Validate magic number
        if header.magic() != HBC_MAGIC {
            return Err(format!("Invalid magic number: expected 0x{:016X}, got 0x{:016X}", 
                              HBC_MAGIC, header.magic()));
        }
        
        // Validate version
        if header.version() < min_supported_version() || header.version() > max_supported_version() {
            return Err(format!("Unsupported HBC version: {}", header.version()));
        }

        // Parse tables
        let functions = FunctionTable::parse(data, &header, &mut offset)
            .map_err(|e| format!("Failed to parse FunctionTable: {}", e))?;

        // Parse string table
        let strings = StringTable::parse(data, &header, &mut offset)
            .map_err(|e| format!("Failed to parse StringTable: {}", e))?;
        
        // Populate string cache for fast lookups
        let mut strings = strings;
        strings.populate_cache()
            .map_err(|e| format!("Failed to populate string cache: {}", e))?;

        let serialized_literals = SerializedLiteralTables::parse(data, &header, &mut offset)
            .map_err(|e| format!("Failed to parse SerializedLiteralTables: {}", e))?;

        let bigints = if header.version() >= 87 {
            BigIntTable::parse(data, &header, &mut offset)
                .map_err(|e| format!("Failed to parse BigIntTable: {}", e))?
        } else {
            BigIntTable {
                table: &[],
                storage: &[],
                count: 0,
            }
        };

        let regexps = RegExpTable::parse(data, &header, &mut offset)
            .map_err(|e| format!("Failed to parse RegExpTable: {}", e))?;
        
        // Parse CommonJS modules table
        let cjs_modules = if header.cjs_module_count() > 0 {
            CommonJsTable::parse(data, &header, &mut offset)
                .map_err(|e| format!("Failed to parse CommonJsTable: {}", e))?
        } else {
            CommonJsTable {
                entries: Vec::new(),
                is_static: false,
            }
        };

        // Parse function source table
        let function_sources = FunctionSourceTable::parse(data, &header, &mut offset)
            .map_err(|e| format!("Failed to parse FunctionSourceTable: {}", e))?;

        let jump_table = JumpTable::new();

        let mut hbc_file = HbcFile {
            header,
            strings,
            regexps,
            bigints,
            functions,
            serialized_literals,
            cjs_modules,
            function_sources,
            jump_table,
        };

        // Pre-parse all instructions to avoid repeated parsing during jump table construction
        let parse_start = std::time::Instant::now();
        let parse_results: Result<Vec<_>, _> = (0..hbc_file.functions.count()).into_par_iter().map(|function_index| {
            hbc_file.functions.get_instructions(function_index)
                .map_err(|e| format!("Failed to pre-parse instructions for function {}: {}", function_index, e))
        }).collect();
        parse_results.map_err(|e| format!("Failed to pre-parse instructions: {}", e))?;
        let parse_elapsed = parse_start.elapsed();
        eprintln!("Pre-parsing completed in {:.2?} ({:.1} functions/second)", 
                 parse_elapsed, hbc_file.functions.count() as f64 / parse_elapsed.as_secs_f64());

        // Build jump table
        let function_count = hbc_file.functions.count();
        let start_time = std::time::Instant::now();
        
        // Build jump table data in parallel
        let jump_table_results: Result<Vec<_>, _> = (0..function_count).into_par_iter().map(|function_index| {
            // Get just the instructions (more efficient than full function data)
            let instructions = hbc_file.functions.get_instructions(function_index)
                .map_err(|e| format!("Failed to get instructions for function {}: {}", function_index, e))?;
            
            // Build jump table data for this function without modifying the main jump table
            JumpTable::build_for_function_parallel(function_index, &instructions)
                .map_err(|e| format!("Failed to build jump table for function {}: {}", function_index, e))
        }).collect();
        
        // Merge all results into the main jump table
        for result in jump_table_results? {
            let (function_index, labels, jumps, label_map, jump_map) = result;
            hbc_file.jump_table.merge_function_data(function_index, labels, jumps, label_map, jump_map);
        }
        
        let elapsed = start_time.elapsed();
        eprintln!("Jump table built successfully in {:.2?} ({:.1} functions/second)", 
                 elapsed, function_count as f64 / elapsed.as_secs_f64());
        println!("HbcFile parsed successfully");
        Ok(hbc_file)
    }
}

impl<'a> Serialize for HbcFile<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("HbcFile", 8)?;
        state.serialize_field("header", &self.header)?;
        state.serialize_field("strings", &self.strings)?;
        state.serialize_field("regexps", &self.regexps)?;
        state.serialize_field("bigints", &self.bigints)?;
        // Custom serialize functions with string table access
        state.serialize_field("functions", &{
            let mut seq = Vec::with_capacity(self.functions.count() as usize);
            for i in 0..self.functions.count() {
                if let Some(name) = self.functions.get_function_name(i, &self.strings) {
                    seq.push(name);
                } else {
                    seq.push(self.functions.get_placeholder(i as u32).unwrap_or_else(|| format!("function_{}", i)));
                }
            }
            seq
        })?;
        // Custom serialize serialized literals with resolved string table
        let string_table_strings = match self.strings.extract_strings() {
            Ok(strings) => strings,
            Err(_) => Vec::new(),
        };
        #[derive(serde::Serialize)]
        struct SerializedLiteralsWithStrings {
            arrays: Vec<String>,
            object_keys: Vec<String>,
            object_values: Vec<String>,
        }
        let serialized_literals_with_strings = SerializedLiteralsWithStrings {
            arrays: self.serialized_literals.arrays.to_strings(&string_table_strings),
            object_keys: self.serialized_literals.object_keys.to_strings(&string_table_strings),
            object_values: self.serialized_literals.object_values.to_strings(&string_table_strings),
        };
        state.serialize_field("serialized_literals", &serialized_literals_with_strings)?;

        // --- Custom CJS modules serialization ---
        #[derive(serde::Serialize)]
        struct CjsModuleResolvedEntry {
            symbol_id: u32,
            filename: Option<String>,
            offset: u32,
            function_name: Option<String>,
        }
        #[derive(serde::Serialize)]
        struct CjsModulesResolved {
            is_static: bool,
            entries: Vec<CjsModuleResolvedEntry>,
        }
        let cjs_modules_resolved = {
            let mut entries = Vec::with_capacity(self.cjs_modules.entries.len());
            for entry in &self.cjs_modules.entries {
                let filename = if !self.cjs_modules.is_static {
                    string_table_strings.get(entry.symbol_id as usize).cloned()
                } else {
                    None
                };
                let function_name = if entry.offset < self.functions.count() as u32 {
                    self.functions.get_function_name(entry.offset, &self.strings)
                } else {
                    None
                };
                entries.push(CjsModuleResolvedEntry {
                    symbol_id: entry.symbol_id,
                    filename,
                    offset: entry.offset,
                    function_name,
                });
            }
            CjsModulesResolved {
                is_static: self.cjs_modules.is_static,
                entries,
            }
        };
        state.serialize_field("cjs_modules", &cjs_modules_resolved)?;
        // --- End custom CJS modules serialization ---

        // --- Custom function sources serialization ---
        #[derive(serde::Serialize)]
        struct FunctionSourceResolvedEntry {
            function_id: u32,
            function_name: Option<String>,
            string_id: u32,
            source: Option<String>,
        }
        #[derive(serde::Serialize)]
        struct FunctionSourcesResolved {
            entries: Vec<FunctionSourceResolvedEntry>,
        }
        let function_sources_resolved = {
            let mut entries = Vec::with_capacity(self.function_sources.entries.len());
            for entry in &self.function_sources.entries {
                let function_name = if entry.function_id < self.functions.count() as u32 {
                    self.functions.get_function_name(entry.function_id, &self.strings)
                } else {
                    None
                };
                let source = string_table_strings.get(entry.string_id as usize).cloned();
                entries.push(FunctionSourceResolvedEntry {
                    function_id: entry.function_id,
                    function_name,
                    string_id: entry.string_id,
                    source,
                });
            }
            FunctionSourcesResolved {
                entries,
            }
        };
        state.serialize_field("function_sources", &function_sources_resolved)?;
        // --- End custom function sources serialization ---

        state.end()
    }
} 