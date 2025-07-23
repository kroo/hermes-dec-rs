use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use rayon::prelude::*;
use std::fs;
use std::io::{BufWriter, Write};

/// Run the disasm subcommand
pub fn disasm(input_path: &std::path::Path) -> DecompilerResult<()> {
    // Read the input file
    let data = match fs::read(input_path) {
        Ok(data) => data,
        Err(_) => {
            return Err(DecompilerError::Internal {
                message: format!("Failed to read file: {}", input_path.display()),
            });
        }
    };

    // Parse the HBC file
    let hbc_file = match HbcFile::parse(&data) {
        Ok(file) => file,
        Err(error) => {
            println!("Failed to parse HBC file: {}", error);
            return Err(DecompilerError::Internal {
                message: format!("Failed to parse HBC file: {}", error),
            });
        }
    };

    // Generate output filename
    let base_name = input_path
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let output_filename = input_path
        .parent()
        .unwrap()
        .join(format!("{}.hasm", base_name));

    // Generate the complete disassembly
    generate_hasm_output(&hbc_file, &output_filename)?;

    println!("Disassembled to {}", output_filename.display());

    Ok(())
}

/// Generate the complete .hasm output
fn generate_hasm_output(hbc_file: &HbcFile, output_path: &std::path::Path) -> DecompilerResult<()> {
    // Open file for writing with buffering
    let file = std::fs::File::create(output_path)
        .map_err(|e| DecompilerError::Io(format!("Failed to create output file: {}", e)))?;
    let mut file = BufWriter::with_capacity(1024 * 1024, file); // 1MB buffer

    // Generate header information
    let mut header_output = String::new();
    generate_header_info(hbc_file, &mut header_output)?;
    file.write_all(header_output.as_bytes())
        .map_err(|e| DecompilerError::Io(format!("Failed to write header: {}", e)))?;

    // Generate string table
    let mut string_output = String::new();
    generate_string_table(hbc_file, &mut string_output)?;
    file.write_all(string_output.as_bytes())
        .map_err(|e| DecompilerError::Io(format!("Failed to write string table: {}", e)))?;

    // Generate global bigint table
    let mut bigint_output = String::new();
    generate_bigint_table_section(hbc_file, &mut bigint_output)?;
    file.write_all(bigint_output.as_bytes())
        .map_err(|e| DecompilerError::Io(format!("Failed to write bigint table: {}", e)))?;

    // Generate array buffer section
    let mut array_output = String::new();
    generate_array_buffer_section(hbc_file, &mut array_output)?;
    file.write_all(array_output.as_bytes())
        .map_err(|e| DecompilerError::Io(format!("Failed to write array buffer: {}", e)))?;

    // Generate function source table
    let mut source_output = String::new();
    generate_function_source_table(hbc_file, &mut source_output)?;
    file.write_all(source_output.as_bytes()).map_err(|e| {
        DecompilerError::Io(format!("Failed to write function source table: {}", e))
    })?;

    // Generate all functions (streaming)
    generate_all_functions(hbc_file, &mut file)?;

    // Flush the buffer to ensure all data is written
    file.flush()
        .map_err(|e| DecompilerError::Io(format!("Failed to flush output: {}", e)))?;

    Ok(())
}

/// Generate the header information section
fn generate_header_info(hbc_file: &HbcFile, output: &mut String) -> DecompilerResult<()> {
    let header = &hbc_file.header;

    output.push_str("Bytecode File Information:\n");
    output.push_str(&format!(
        "  Bytecode version number: {}\n",
        header.version()
    ));

    // Format source hash as hex string
    let source_hash_hex = header
        .source_hash()
        .iter()
        .map(|b| format!("{:02x}", b))
        .collect::<String>();
    output.push_str(&format!("  Source hash: {}\n", source_hash_hex));

    output.push_str(&format!("  Function count: {}\n", header.function_count()));
    output.push_str(&format!("  String count: {}\n", header.string_count()));

    if let Some(big_int_count) = header.big_int_count() {
        output.push_str(&format!("  BigInt count: {}\n", big_int_count));
    } else {
        output.push_str("  BigInt count: 0\n");
    }

    output.push_str(&format!(
        "  String Kind Entry count: {}\n",
        header.string_kind_count()
    ));
    output.push_str(&format!("  RegExp count: {}\n", header.reg_exp_count()));

    if let Some(segment_id) = header.segment_id() {
        output.push_str(&format!("  Segment ID: {}\n", segment_id));
    } else {
        output.push_str("  Segment ID: 0\n");
    }

    output.push_str(&format!(
        "  CommonJS module count: {}\n",
        header.cjs_module_count()
    ));
    output.push_str(&format!(
        "  CommonJS module count (static): {}\n",
        if header.cjs_modules_statically_resolved() {
            header.cjs_module_count()
        } else {
            0
        }
    ));

    if let Some(function_source_count) = header.function_source_count() {
        output.push_str(&format!(
            "  Function source count: {}\n",
            function_source_count
        ));
    } else {
        output.push_str("  Function source count: 0\n");
    }

    output.push_str("  Bytecode options:\n");
    output.push_str(&format!(
        "    staticBuiltins: {}\n",
        if header.static_builtins() { 1 } else { 0 }
    ));
    output.push_str(&format!(
        "    cjsModulesStaticallyResolved: {}\n",
        if header.cjs_modules_statically_resolved() {
            1
        } else {
            0
        }
    ));
    output.push_str("\n");

    Ok(())
}

/// Generate the string table section
fn generate_string_table(hbc_file: &HbcFile, output: &mut String) -> DecompilerResult<()> {
    output.push_str("Global String Table:\n");

    for i in 0..hbc_file.strings.count() {
        let entry =
            hbc_file
                .strings
                .get_entry(i as u32)
                .map_err(|e| DecompilerError::Internal {
                    message: format!("Failed to get string entry: {}", e),
                })?;

        // Determine string type (ASCII/UTF16)
        let string_type = if entry.is_utf16 { "UTF-16" } else { "ASCII" };

        // Calculate the byte range using the actual offset and length
        let range = if entry.length > 0 {
            format!("{}..{}", entry.offset, entry.offset + entry.length - 1)
        } else {
            format!("{}..-1", entry.offset)
        };

        // Add identifier hash if this is an identifier
        let hash_prefix = if let Some(hash) = entry.identifier_hash {
            format!("#{:08X}: ", hash)
        } else {
            String::new()
        };

        // Determine the prefix based on whether this is an identifier
        let prefix = if entry.is_identifier.unwrap_or(false) {
            "i"
        } else {
            "s"
        };

        // Format the string content - UTF-16 strings should be displayed as hex bytes
        let string_content = if entry.is_utf16 {
            entry.hex_string()
        } else {
            entry.string_value()
        };

        output.push_str(&format!(
            "{}{}[{}, {}] {}{}\n",
            prefix, i, string_type, range, hash_prefix, string_content
        ));
    }
    output.push_str("\n");

    Ok(())
}

/// Generate the array buffer section
fn generate_array_buffer_section(hbc_file: &HbcFile, output: &mut String) -> DecompilerResult<()> {
    use crate::hbc::serialized_literal_parser::SLPValue;
    let arrays = &hbc_file.serialized_literals.arrays.items;
    let object_keys = &hbc_file.serialized_literals.object_keys.items;
    let object_values = &hbc_file.serialized_literals.object_values.items;

    // Always show Array Buffer section, even if empty
    output.push_str("Array Buffer:\n");
    for item in arrays.iter() {
        match item {
            SLPValue::LongString(idx) => output.push_str(&format!("[String {}]\n", idx)),
            SLPValue::ShortString(idx) => output.push_str(&format!("[String {}]\n", idx)),
            SLPValue::ByteString(idx) => output.push_str(&format!("[String {}]\n", idx)),
            _ => output.push_str(&format!("{:?}\n", item)),
        }
    }

    // Only show Object Key Buffer if not empty
    if !object_keys.is_empty() {
        output.push_str("Object Key Buffer:\n");
        for item in object_keys.iter() {
            match item {
                SLPValue::LongString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                SLPValue::ShortString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                SLPValue::ByteString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                _ => output.push_str(&format!("{:?}\n", item)),
            }
        }
    }

    // Only show Object Value Buffer if not empty
    if !object_values.is_empty() {
        output.push_str("Object Value Buffer:\n");
        for item in object_values.iter() {
            match item {
                SLPValue::LongString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                SLPValue::ShortString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                SLPValue::ByteString(idx) => output.push_str(&format!("[String {}]\n", idx)),
                SLPValue::Integer(val) => output.push_str(&format!("[int {}]\n", val)),
                _ => output.push_str(&format!("{:?}\n", item)),
            }
        }
    }
    output.push_str("\n");
    Ok(())
}

/// Generate the function source table section
fn generate_function_source_table(hbc_file: &HbcFile, output: &mut String) -> DecompilerResult<()> {
    if hbc_file.function_sources.count() > 0 {
        output.push_str("Function Source Table:\n");

        let strings =
            hbc_file
                .strings
                .extract_strings()
                .map_err(|e| DecompilerError::Internal {
                    message: format!("Failed to extract strings: {}", e),
                })?;

        for entry in &hbc_file.function_sources.entries {
            if let Some(_source_string) = strings.get(entry.string_id as usize) {
                output.push_str(&format!(
                    "  Function ID {} -> s{}\n",
                    entry.function_id, entry.string_id
                ));
            }
        }
        output.push_str("\n");
    }

    Ok(())
}

/// Generate function header string
fn generate_function_header(
    parsed_header: &crate::hbc::tables::function_table::ParsedFunctionHeader,
    function_name: &str,
) -> String {
    let header = &parsed_header.header;
    let prohibit_invoke = header.prohibit_invoke();
    let prohibit_invoke_str = if prohibit_invoke == 1 {
        "NCFunction"
    } else {
        "Function"
    };
    let function_header = format!(
        "{}<{}>{}(",
        prohibit_invoke_str, function_name, parsed_header.index
    );
    let function_header = format!("{}{} params, ", function_header, header.param_count());
    let function_header = format!("{}{} registers, ", function_header, header.frame_size());
    let function_header = format!(
        "{}{} symbols):\n",
        function_header,
        header.environment_size()
    );
    function_header
}

/// Format a single instruction
fn format_instruction(
    instruction: &crate::hbc::tables::function_table::HbcFunctionInstruction,
    hbc_file: &HbcFile,
) -> String {
    instruction.format_instruction(hbc_file)
}

/// Generate exception handlers string
fn generate_exception_handlers(
    parsed_header: &crate::hbc::tables::function_table::ParsedFunctionHeader,
    _jump_table: &crate::hbc::JumpTable,
) -> String {
    let mut exception_output = String::new();
    if !parsed_header.exc_handlers.is_empty() {
        exception_output.push_str("\nException Handlers:\n");
        for (i, handler) in parsed_header.exc_handlers.iter().enumerate() {
            // Copy values to avoid packed struct access issues
            let start = handler.start;
            let end = handler.end;
            let target = handler.target;
            exception_output.push_str(&format!(
                "{}: start = L{}, end = L{}, target = L{}\n",
                i, start, end, target
            ));
        }
    }
    exception_output
}

/// Generate a single function's output as a string
fn generate_single_function(
    hbc_file: &HbcFile,
    _idx: usize,
    parsed_header: &crate::hbc::tables::function_table::ParsedFunctionHeader,
) -> DecompilerResult<String> {
    let function_index = parsed_header.index;

    // Get instructions (should be cached)
    let instructions = hbc_file.functions.get_instructions(function_index)?;

    // Get function name
    let function_name = hbc_file
        .functions
        .get_function_name(function_index, &hbc_file.strings)
        .unwrap_or_else(|| format!("function_{}", function_index));

    // Generate header
    let header_output = generate_function_header(parsed_header, &function_name);

    // Format instructions (parallel for large functions)
    const PARALLEL_THRESHOLD: usize = 100; // Optimal threshold from analysis

    let formatted_instructions: Vec<String> = if instructions.len() >= PARALLEL_THRESHOLD {
        // Parallel instruction formatting for large functions
        instructions
            .par_iter()
            .map(|instruction| {
                let mut result = String::new();

                // Check if this instruction should have a label
                if let Some(label_name) = hbc_file.jump_table.get_label_by_inst_index(
                    instruction.function_index,
                    instruction.instruction_index,
                ) {
                    result.push_str(&format!("{}:\n", label_name));
                }

                // Format the instruction
                let formatted = format_instruction(instruction, hbc_file);
                result.push_str(&format!("  {}\n", formatted));

                result
            })
            .collect()
    } else {
        // Sequential instruction formatting for small functions
        instructions
            .iter()
            .map(|instruction| {
                let mut result = String::new();

                // Check if this instruction should have a label
                if let Some(label_name) = hbc_file.jump_table.get_label_by_inst_index(
                    instruction.function_index,
                    instruction.instruction_index,
                ) {
                    result.push_str(&format!("{}:\n", label_name));
                }

                // Format the instruction
                let formatted = format_instruction(instruction, hbc_file);
                result.push_str(&format!("  {}\n", formatted));

                result
            })
            .collect()
    };

    // Generate exception handlers
    let exception_output = generate_exception_handlers(parsed_header, &hbc_file.jump_table);

    // Build the complete function output
    let mut function_output = String::new();
    function_output.push_str(&header_output);
    for instruction_output in formatted_instructions {
        function_output.push_str(&instruction_output);
    }
    function_output.push_str(&exception_output);
    function_output.push('\n');

    Ok(function_output)
}

/// Generate all functions
fn generate_all_functions(
    hbc_file: &HbcFile,
    file: &mut BufWriter<std::fs::File>,
) -> DecompilerResult<()> {
    let count = hbc_file.functions.parsed_headers.len();
    eprintln!("Generating {} functions...", count);

    let total_start = std::time::Instant::now();
    let progress_bar = indicatif::ProgressBar::new(count as u64);
    progress_bar.set_style(
        indicatif::ProgressStyle::default_bar()
            .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos}/{len} functions")
            .unwrap()
            .progress_chars("##-"),
    );

    // Parallelize function generation
    let function_outputs: Vec<DecompilerResult<String>> = hbc_file
        .functions
        .parsed_headers
        .par_iter()
        .enumerate()
        .map(|(idx, parsed_header)| {
            let result = generate_single_function(hbc_file, idx, parsed_header);
            progress_bar.inc(1);
            result
        })
        .collect();

    // Write results sequentially to maintain order
    for function_output in function_outputs {
        let output = function_output?;
        file.write_all(output.as_bytes())
            .map_err(|e| DecompilerError::Io(format!("Failed to write function: {}", e)))?;
    }

    progress_bar.finish_with_message("Function generation completed");
    let total_elapsed = total_start.elapsed();
    eprintln!(
        "Function generation completed in {:.2}ms ({:.1} functions/second)",
        total_elapsed.as_millis(),
        count as f64 / total_elapsed.as_secs_f64()
    );

    Ok(())
}

fn generate_bigint_table_section(hbc_file: &HbcFile, output: &mut String) -> DecompilerResult<()> {
    let bigints = hbc_file
        .bigints
        .extract_bigints()
        .map_err(|e| DecompilerError::Internal {
            message: format!("Failed to extract bigints: {}", e),
        })?;
    if bigints.is_empty() {
        return Ok(());
    }
    output.push_str("Global BigInt Table:\n");
    for (i, value) in bigints.iter().enumerate() {
        // Find the byte range for this bigint
        if let Some(entry) = hbc_file.bigints.table.get(i * 8..i * 8 + 8) {
            let offset = u32::from_le_bytes([entry[0], entry[1], entry[2], entry[3]]);
            let length = u32::from_le_bytes([entry[4], entry[5], entry[6], entry[7]]);
            let end = if length > 0 {
                offset + length - 1
            } else {
                offset
            };
            // Truncate value if too long
            let (display, note) = if value.len() > 30 {
                (
                    &value[..16],
                    format!("... ({} decimal digits)", value.len()),
                )
            } else {
                (value.as_str(), String::new())
            };
            output.push_str(&format!(
                "  {}[{}..{}]: {}{}\n",
                i, offset, end, display, note
            ));
        }
    }
    output.push_str("\n");
    Ok(())
}
