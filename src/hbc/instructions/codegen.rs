use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::PathBuf;

use crate::hbc::instructions::{
    hermes_repo_fetcher::SourceFetcher,
    hermes_repo_parser::{BytecodeDefParser, InstructionDef, OperandType},
    versions::HbcVersion,
};

// =================================================================================
// Data Structures
// =================================================================================

/// Unified instruction metadata (name-based, not opcode-based).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedInstructionDef {
    pub name: String,
    pub operands: Vec<String>, // operand types as strings
    pub is_variadic: bool,
    pub doc: Option<String>,
    pub category: String,
}

/// A registry containing all generated instruction definitions and their version mappings.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratedInstructionRegistry {
    pub instructions: Vec<UnifiedInstructionDef>,
    pub version_mappings: HashMap<String, HashMap<u32, u8>>, // instruction_name -> version -> opcode
}

/// Represents the shape of an instruction's operands for macro generation.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct OperandShape {
    pub operand_types: Vec<String>,
}

impl OperandShape {
    fn new(operands: &[String]) -> Self {
        Self {
            operand_types: operands.to_vec(),
        }
    }

    fn to_macro_name(&self) -> String {
        format!(
            "OPERAND{}_{}",
            self.operand_types.len(),
            self.operand_types.join("_")
        )
    }
}

// =================================================================================
// Main Generation Logic
// =================================================================================

/// Generates unified instruction definitions by processing multiple versions of Hermes bytecode.
pub fn generate_unified_instructions() -> Result<()> {
    println!("Generating unified instructions from Hermes source...");
    let out_dir = PathBuf::from("src/generated");
    fs::create_dir_all(&out_dir)?;

    // Load version definitions from JSON.
    let versions = load_hbc_versions("src/hbc/instructions/versions.json")?;

    let mut all_instructions = HashMap::<String, UnifiedInstructionDef>::new();
    let mut version_mappings = HashMap::<String, HashMap<u32, u8>>::new();
    let mut operand_shapes = HashSet::<OperandShape>::new();

    let cache_dir = PathBuf::from("target/cache/hermes-shared");
    fs::create_dir_all(&cache_dir)?;
    let fetcher = SourceFetcher::new(cache_dir)?;

    for version in &versions {
        println!("Processing HBC version {}", version.version);
        let def_file_path = fetcher.fetch_version(version)?;
        let instructions =
            BytecodeDefParser::new().parse_content(&fs::read_to_string(def_file_path)?)?;

        process_instructions_for_version(
            instructions,
            version,
            &mut all_instructions,
            &mut version_mappings,
            &mut operand_shapes,
        );
    }

    let registry = GeneratedInstructionRegistry {
        instructions: all_instructions.into_values().collect(),
        version_mappings,
    };

    // Generate and write the Rust source files.
    generate_and_write_file(&out_dir.join("unified_instructions.rs"), |f| {
        generate_unified_instructions_module(f, &registry, &operand_shapes)
    })?;
    generate_and_write_file(&out_dir.join("generated_traits.rs"), |f| {
        generate_traits_module(f, &registry, &versions)
    })?;

    println!(
        "Generated unified instructions with {} unique instructions across {} versions.",
        registry.instructions.len(),
        versions.len()
    );
    println!(
        "Generated {} unique operand shape macros.",
        operand_shapes.len()
    );
    println!("Generated files:");
    println!("  - src/generated/unified_instructions.rs");
    println!("  - src/generated/generated_traits.rs");

    Ok(())
}

/// Processes instructions from a single HBC version, updating the unified collections.
fn process_instructions_for_version(
    instructions: Vec<InstructionDef>,
    version: &HbcVersion,
    all_instructions: &mut HashMap<String, UnifiedInstructionDef>,
    version_mappings: &mut HashMap<String, HashMap<u32, u8>>,
    operand_shapes: &mut HashSet<OperandShape>,
) {
    for inst in instructions {
        let name = inst.name.clone();
        let operands: Vec<String> = inst
            .operands
            .iter()
            .map(|op| operand_type_to_string(&op.operand_type))
            .collect();

        // The unified definition should use the operand types from the most "specific"
        // version (e.g., preferring `FunctionId16` over the more generic `UInt16`).
        let is_more_specific = all_instructions.get(&name).map_or(true, |existing| {
            has_more_specific_operand_types(&operands, &existing.operands)
        });

        if is_more_specific {
            if !operands.is_empty() {
                operand_shapes.insert(OperandShape::new(&operands));
            }
            let unified_def = UnifiedInstructionDef {
                name: name.clone(),
                operands,
                is_variadic: inst.is_variadic,
                doc: inst.doc,
                category: categorize_instruction(&name),
            };
            all_instructions.insert(name.clone(), unified_def);
        }

        version_mappings
            .entry(name)
            .or_default()
            .insert(version.version, inst.opcode);
    }
}

// =================================================================================
// Helper Functions
// =================================================================================

/// Deserializes HBC version information from a JSON file.
fn load_hbc_versions(path: &str) -> Result<Vec<HbcVersion>> {
    let content = fs::read_to_string(path).with_context(|| format!("Failed to read {}", path))?;
    #[derive(Deserialize)]
    struct VersionsFile {
        versions: Vec<HbcVersion>,
    }
    Ok(serde_json::from_str::<VersionsFile>(&content)?.versions)
}

/// Writes content generated by a provided function to a file.
fn generate_and_write_file<F>(path: &PathBuf, generator: F) -> Result<()>
where
    F: Fn(&mut String) -> Result<()>,
{
    let mut content = String::new();
    generator(&mut content)?;
    fs::write(path, content)?;
    Ok(())
}

/// Checks if new operand types are more specific (semantic) than existing ones.
fn has_more_specific_operand_types(new: &[String], existing: &[String]) -> bool {
    if new.len() != existing.len() {
        return false; // Incomparable.
    }
    let mut has_improvement = false;
    for (n, e) in new.iter().zip(existing.iter()) {
        let (spec_n, spec_e) = (
            get_operand_type_specificity(n),
            get_operand_type_specificity(e),
        );
        if spec_n < spec_e {
            return false; // A less specific type was found, so this is not an improvement.
        }
        if spec_n > spec_e {
            has_improvement = true;
        }
    }
    has_improvement
}

/// Returns a specificity score for an operand type. Higher is more specific.
fn get_operand_type_specificity(op_type: &str) -> u32 {
    match op_type {
        // Semantic types (most specific).
        _ if op_type.contains("Id") => 3,
        // Typed but generic.
        "Reg8" | "Reg32" | "Addr8" | "Addr32" | "Imm32" | "Double" => 2,
        // Generic integers (least specific).
        "UInt8" | "UInt16" | "UInt32" => 1,
        _ => 0,
    }
}

/// Assigns a category to an instruction based on its name.
fn categorize_instruction(name: &str) -> String {
    let lower = name.to_lowercase();
    let category = match lower {
        s if s.contains("jmp") || s.contains("switch") || s.starts_with("j") => "Jump",
        s if s.contains("call") || s.contains("construct") => "Call",
        s if s == "ret" => "Return",
        s if s.contains("throw") || s.contains("catch") => "Exception",
        s if s.starts_with("new") && (s.contains("object") || s.contains("array")) => {
            "ObjectCreation"
        }
        s if s.contains("get") || s.contains("put") || s.contains("del") => "PropertyAccess",
        s if s.contains("add")
            || s.contains("sub")
            || s.contains("mul")
            || s.contains("div")
            || s.contains("mod")
            || s.contains("negate") =>
        {
            "Arithmetic"
        }
        s if s.contains("bit") || s.contains("shift") => "Bitwise",
        s if s.contains("not") || s.contains("and") || s.contains("or") => "Logical",
        s if s.contains("load") && s.contains("const") => "ConstantLoad",
        s if s.contains("eq")
            || s.contains("less")
            || s.contains("greater")
            || s.contains("instance")
            || s.contains("in") =>
        {
            "Comparison"
        }
        s if s.contains("to") || s.contains("typeof") => "TypeConversion",
        s if s.contains("environment") || s.contains("global") => "Environment",
        s if s.contains("mov") || s.contains("load") || s.contains("store") => "Variable",
        s if s.contains("debug") || s.contains("profile") => "Debug",
        _ => "Other",
    };
    category.to_string()
}

/// Converts an instruction name to a valid Rust identifier.
fn sanitize_rust_identifier(name: &str) -> String {
    let mut result = name.replace(|c: char| !c.is_ascii_alphanumeric() && c != '_', "_");
    if result.chars().next().map_or(true, |c| c.is_ascii_digit()) {
        result.insert(0, '_');
    }
    result
}

// =================================================================================
// Rust Module Generation: unified_instructions.rs
// =================================================================================

/// Generates the entire content for the `unified_instructions.rs` module.
fn generate_unified_instructions_module(
    code: &mut String,
    registry: &GeneratedInstructionRegistry,
    operand_shapes: &HashSet<OperandShape>,
) -> Result<()> {
    // File header with allow attributes to suppress warnings in generated code.
    writeln!(code, "// Auto-generated unified instruction definitions.")?;
    writeln!(
        code,
        "// This file is generated by `build.rs` - do not edit manually.\n"
    )?;
    writeln!(code, "#![allow(unused_imports)]")?;
    writeln!(code, "#![allow(unused_macros)]")?;
    writeln!(code, "#![allow(dead_code)]\n")?;

    writeln!(code, "use anyhow::Result;")?;
    writeln!(code, "use scroll::{{Pread, LE}};")?;
    writeln!(code, "use serde::{{Serialize, Deserialize}};\n")?;

    // Generate parsing macros for each unique operand shape.
    for shape in operand_shapes {
        writeln!(code, "macro_rules! {} {{", shape.to_macro_name())?;
        writeln!(
            code,
            "    ($variant:ident, $bytes:expr, $offset:expr) => {{"
        )?;
        writeln!(code, "        {{")?;
        writeln!(code, "            let start = *$offset;")?;
        for (i, op_type) in shape.operand_types.iter().enumerate() {
            writeln!(
                code,
                "            let operand_{} = $bytes.gread_with::<{}>($offset, LE)?;",
                i,
                operand_type_to_rust_type(op_type)
            )?;
        }
        writeln!(
            code,
            "            let instr = UnifiedInstruction::$variant {{"
        )?;
        for i in 0..shape.operand_types.len() {
            writeln!(code, "                operand_{0}: operand_{0},", i)?;
        }
        writeln!(code, "            }};")?;
        writeln!(code, "            let bytes_read = *$offset - start;")?;
        writeln!(code, "            Ok((instr, bytes_read))")?;
        writeln!(code, "        }}")?;
        writeln!(code, "    }};\n}}")?;
    }

    // Generate the `define_instructions!` macro and its invocation.
    generate_define_instructions_macro(code, registry)?;
    generate_macro_invocation(code, registry)?;

    Ok(())
}

/// Generates the `define_instructions!` macro structure.
fn generate_define_instructions_macro(
    code: &mut String,
    registry: &GeneratedInstructionRegistry,
) -> Result<()> {
    writeln!(code, "macro_rules! define_instructions {{")?;
    writeln!(code, "    ($($name:ident {{ operands: [$($op:ident: $op_ty:ty),*], category: $cat:expr }}),*) => {{")?;

    // Enum Definition
    writeln!(
        code,
        "        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]"
    )?;
    writeln!(code, "        pub enum UnifiedInstruction {{")?;
    writeln!(code, "            $($name {{ $($op: $op_ty,)* }}),*")?;
    writeln!(code, "        }}\n")?;

    // `impl` block for `UnifiedInstruction`
    writeln!(code, "        impl UnifiedInstruction {{")?;
    // `name()` method
    writeln!(code, "            pub fn name(&self) -> &'static str {{")?;
    writeln!(code, "                match self {{")?;
    writeln!(
        code,
        "                    $(UnifiedInstruction::$name {{ .. }} => stringify!($name)),*"
    )?;
    writeln!(code, "                }}")?;
    writeln!(code, "            }}\n")?;
    // `category()` method
    writeln!(
        code,
        "            pub fn category(&self) -> &'static str {{"
    )?;
    writeln!(code, "                match self {{")?;
    writeln!(
        code,
        "                    $(UnifiedInstruction::$name {{ .. }} => $cat),*"
    )?;
    writeln!(code, "                }}")?;
    writeln!(code, "            }}\n")?;
    // `size()` method
    writeln!(code, "            pub fn size(&self) -> usize {{")?;
    writeln!(code, "                match self {{")?;
    for inst in &registry.instructions {
        let mut total_size = 1; // Base size: 1 byte for opcode
        for op_type in &inst.operands {
            total_size += match op_type.as_str() {
                "Reg8" | "UInt8" | "Addr8" | "StringId8" | "BigIntId8" | "FunctionId8"
                | "EnvId8" => 1,
                "UInt16" | "StringId16" | "BigIntId16" | "FunctionId16" => 2,
                "Reg32" | "UInt32" | "Addr32" | "Imm32" | "StringId32" | "BigIntId32"
                | "FunctionId32" | "RegExpId32" => 4,
                "Double" => 8,
                _ => 1, // Default fallback
            };
        }
        writeln!(
            code,
            "                    UnifiedInstruction::{} {{ .. }} => {}, // {}",
            sanitize_rust_identifier(&inst.name),
            total_size,
            inst.operands.join(", ")
        )?;
    }
    writeln!(code, "                }}")?;
    writeln!(code, "            }}\n")?;
    generate_format_instruction_method(code, registry)?;
    generate_parse_method(code, registry)?;
    writeln!(code, "        }}")?; // end impl
    writeln!(code, "    }};")?; // end macro_rules
    writeln!(code, "}}")?;
    Ok(())
}

/// Generates the `format_instruction` method within the `impl` block.
fn generate_format_instruction_method(
    code: &mut String,
    registry: &GeneratedInstructionRegistry,
) -> Result<()> {
    writeln!(
        code,
        "            pub fn format_instruction(&self, hbc_file: &crate::hbc::HbcFile) -> String {{"
    )?;
    writeln!(
        code,
        "                let mut operands: Vec<String> = Vec::new();"
    )?;
    writeln!(code, "                match self {{")?;

    for inst in &registry.instructions {
        write!(
            code,
            "                    UnifiedInstruction::{} {{",
            sanitize_rust_identifier(&inst.name)
        )?;
        let op_names: Vec<String> = (0..inst.operands.len())
            .map(|i| format!("operand_{}", i))
            .collect();
        write!(code, " {} }} => {{", op_names.join(", "))?;
        if inst.operands.is_empty() {
            write!(code, " /* No operands */ ")?;
        }
        writeln!(code)?;

        for (i, op_type) in inst.operands.iter().enumerate() {
            let field = format!("operand_{}", i);
            let formatter = match op_type.as_str() {
                "Reg8" | "Reg32" => format!("operands.push(format!(\"r{{}}\", {}));", field),
                "Addr32" | "Addr8" => format!("operands.push(format!(\"[{{}}]\", {}));", field),
                t if t.contains("StringId") => format!("operands.push(hbc_file.strings.get(*{} as u32).map(|s| format!(\"\\\"{{}}\\\"\", s)).unwrap_or_else(|_| format!(\"<string_error>\")));", field),
                t if t.contains("BigIntId") => format!("operands.push(hbc_file.bigints.get(*{} as u32).map(|b| b.clone()).map_or_else(|s| format!(\"{{}}\", s), |b| b.to_string()));", field),
                t if t.contains("FunctionId") => format!("operands.push(hbc_file.functions.get_function_name(*{} as u32, &hbc_file.strings).map(|name| name.clone()).map_or_else(|| format!(\"Function<unknown>{{}}\", {}), |name| format!(\"Function<{{}}>{{}}\", name, {})));", field, field, field),
                "RegExpId32" => format!("operands.push(format!(\"regexp#[{{}}]\", {}));", field),
                _ => format!("operands.push({}.to_string());", field),
            };
            writeln!(code, "                        {}", formatter)?;
        }
        writeln!(code, "                    }}")?;
    }
    writeln!(code, "                }}")?;
    writeln!(
        code,
        "                format!(\"{{:<17}} {{}}\", self.name(), operands.join(\", \"))"
    )?;
    writeln!(code, "            }}\n")?;
    Ok(())
}

/// Generates the `parse` method, which decodes an instruction from bytes.
fn generate_parse_method(code: &mut String, registry: &GeneratedInstructionRegistry) -> Result<()> {
    writeln!(code, "            pub fn parse(version: u32, opcode: u8, bytes: &[u8], offset: &mut usize) -> Result<(Self, usize)> {{")?;
    writeln!(code, "                match opcode {{")?;

    // Pivot the data to group versions by opcode and then by instruction name.
    // Final structure: opcode -> instruction_name -> [versions]
    let mut opcode_map = HashMap::<u8, HashMap<String, Vec<u32>>>::new();
    for (inst_name, version_map) in &registry.version_mappings {
        for (&version, &opcode) in version_map {
            opcode_map
                .entry(opcode)
                .or_default()
                .entry(inst_name.clone())
                .or_default()
                .push(version);
        }
    }

    // Sort opcodes for deterministic file output.
    let mut sorted_opcodes: Vec<_> = opcode_map.keys().copied().collect();
    sorted_opcodes.sort();

    for opcode in sorted_opcodes {
        writeln!(code, "                    {} => {{", opcode)?;
        let inst_map = opcode_map.get(&opcode).unwrap();

        // Sort the instructions associated with this opcode by their minimum version
        // to ensure the generated `if` chain is ordered correctly.
        let mut sorted_insts: Vec<_> = inst_map.iter().collect();
        sorted_insts.sort_by_key(|(_, versions)| versions.iter().min().unwrap_or(&0));

        for (inst_name, versions) in sorted_insts {
            let inst = registry
                .instructions
                .iter()
                .find(|i| &i.name == inst_name)
                .unwrap();
            let rust_name = sanitize_rust_identifier(inst_name);

            // Create a single version range check. This assumes versions for a given
            // instruction/opcode pair are contiguous, which is true for Hermes.
            let min_ver = versions.iter().min().unwrap();
            let max_ver = versions.iter().max().unwrap();
            let version_check = format!("({}..={}).contains(&version)", min_ver, max_ver);

            writeln!(code, "                        if {} {{", version_check)?;
            if inst.operands.is_empty() {
                writeln!(
                    code,
                    "                            return Ok((UnifiedInstruction::{} {{}}, 0));",
                    rust_name
                )?;
            } else {
                let shape = OperandShape::new(&inst.operands);
                writeln!(
                    code,
                    "                            return {}!({}, bytes, offset);",
                    shape.to_macro_name(),
                    rust_name
                )?;
            }
            writeln!(code, "                        }}")?;
        }

        writeln!(code, "                        Err(anyhow::anyhow!(\"Unknown instruction for opcode {{}} in version {{}}\", opcode, version))")?;
        writeln!(code, "                    }},")?;
    }

    writeln!(code, "                    _ => Err(anyhow::anyhow!(\"Unknown opcode {{}} for version {{}}\", opcode, version)),")?;
    writeln!(code, "                }}")?;
    writeln!(code, "            }}")?; // end parse
    Ok(())
}

/// Generates the final invocation of the `define_instructions!` macro.
fn generate_macro_invocation(
    code: &mut String,
    registry: &GeneratedInstructionRegistry,
) -> Result<()> {
    writeln!(code, "define_instructions! {{")?;
    for (i, inst) in registry.instructions.iter().enumerate() {
        writeln!(code, "    {} {{", sanitize_rust_identifier(&inst.name))?;
        write!(code, "        operands: [")?;
        let ops: Vec<String> = inst
            .operands
            .iter()
            .enumerate()
            .map(|(j, op_type)| format!("operand_{}: {}", j, operand_type_to_rust_type(op_type)))
            .collect();
        write!(code, "{}],", ops.join(", "))?;
        writeln!(code, "\n        category: \"{}\"", inst.category)?;
        // CORRECTED: The comma is the separator for the macro invocation.
        writeln!(
            code,
            "    }}{}",
            if i == registry.instructions.len() - 1 {
                ""
            } else {
                ","
            }
        )?;
    }
    writeln!(code, "}}")?;
    Ok(())
}

// =================================================================================
// Rust Module Generation: generated_traits.rs
// =================================================================================

/// Generates the entire content for the `generated_traits.rs` module.
fn generate_traits_module(
    code: &mut String,
    registry: &GeneratedInstructionRegistry,
    versions: &[HbcVersion],
) -> Result<()> {
    // File header
    writeln!(
        code,
        "// Auto-generated traits and version registry module."
    )?;
    writeln!(
        code,
        "// This file is generated by `build.rs` - do not edit manually.\n"
    )?;
    writeln!(
        code,
        "use crate::hbc::instructions::versions::{{HbcVersion, VersionRegistry}};"
    )?;
    writeln!(code, "use std::collections::HashMap;")?;
    writeln!(code, "use once_cell::sync::Lazy;\n")?;

    // InstructionCategory enum
    let categories: HashSet<_> = registry.instructions.iter().map(|i| &i.category).collect();
    writeln!(code, "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]")?;
    writeln!(code, "pub enum InstructionCategory {{")?;
    for cat in &categories {
        writeln!(code, "    {},", cat)?;
    }
    writeln!(code, "}}\n")?;

    // Helper functions (all are now public)
    writeln!(
        code,
        "pub fn get_category_from_name(instruction_name: &str) -> InstructionCategory {{"
    )?;
    writeln!(code, "    match instruction_name {{")?;
    for inst in &registry.instructions {
        writeln!(
            code,
            "        \"{}\" => InstructionCategory::{},",
            sanitize_rust_identifier(&inst.name),
            inst.category
        )?;
    }
    writeln!(code, "        _ => InstructionCategory::Other,")?;
    writeln!(code, "    }}")?;
    writeln!(code, "}}\n")?;

    writeln!(code, "pub fn is_jump_instruction(instruction_name: &str) -> bool {{ matches!(get_category_from_name(instruction_name), InstructionCategory::Jump) }}\n")?;
    writeln!(code, "pub fn is_call_instruction(instruction_name: &str) -> bool {{ matches!(get_category_from_name(instruction_name), InstructionCategory::Call) }}\n")?;
    writeln!(code, "pub fn is_return_instruction(instruction_name: &str) -> bool {{ matches!(get_category_from_name(instruction_name), InstructionCategory::Return) }}\n")?;

    writeln!(
        code,
        "pub fn can_throw_instruction(instruction_name: &str) -> bool {{"
    )?;
    let throwing_categories = [
        "Exception",
        "PropertyAccess",
        "Arithmetic",
        "Bitwise",
        "Logical",
        "Comparison",
        "TypeConversion",
    ];
    let match_arms = throwing_categories
        .iter()
        .map(|c| format!("InstructionCategory::{}", c))
        .collect::<Vec<_>>()
        .join(" | ");
    writeln!(
        code,
        "    matches!(get_category_from_name(instruction_name), {})",
        match_arms
    )?;
    writeln!(code, "}}\n")?;

    // Static Version Data with corrected struct construction
    writeln!(
        code,
        "static VERSION_DATA: Lazy<HashMap<u32, HbcVersion>> = Lazy::new(|| {{"
    )?;
    writeln!(code, "    let mut map = HashMap::new();")?;
    for v in versions {
        let tag = v.tag.as_ref().map_or("None".to_string(), |s| {
            format!("Some(\"{}\".to_string())", s)
        });
        let commit = v.commit.as_ref().map_or("None".to_string(), |s| {
            format!("Some(\"{}\".to_string())", s)
        });
        let date = v.release_date.as_ref().map_or("None".to_string(), |s| {
            format!("Some(\"{}\".to_string())", s)
        });
        writeln!(code, "    map.insert({}, HbcVersion {{ version: {}, tag: {}, commit: {}, release_date: {} }});", v.version, v.version, tag, commit, date)?;
    }
    writeln!(code, "    map")?;
    writeln!(code, "}});")?;

    // Static Version Registry with corrected initialization
    writeln!(
        code,
        "\npub static VERSION_REGISTRY: Lazy<VersionRegistry> = Lazy::new(|| {{"
    )?;
    writeln!(code, "    let mut registry = VersionRegistry::new();")?;
    writeln!(code, "    for version in VERSION_DATA.values() {{")?;
    writeln!(code, "        registry.add_version(version.clone());")?;
    writeln!(code, "    }}")?;
    writeln!(code, "    registry")?;
    writeln!(code, "}});")?;

    // Public helper functions for the registry
    writeln!(
        code,
        "\npub fn get_version_registry() -> &'static VersionRegistry {{ &VERSION_REGISTRY }}"
    )?;
    writeln!(code, "\npub fn is_version_supported(version: u32) -> bool {{ VERSION_REGISTRY.is_supported(version) }}")?;
    writeln!(code, "\npub fn supported_versions() -> Vec<u32> {{ VERSION_REGISTRY.versions().map(|v| v.version).collect() }}")?;
    writeln!(
        code,
        "\npub fn min_supported_version() -> u32 {{ {} }}",
        versions.iter().map(|v| v.version).min().unwrap_or(0)
    )?;
    writeln!(
        code,
        "\npub fn max_supported_version() -> u32 {{ {} }}",
        versions.iter().map(|v| v.version).max().unwrap_or(0)
    )?;

    Ok(())
}

// =================================================================================
// Type Conversion Utilities
// =================================================================================

/// Maps an `OperandType` enum to its string representation.
fn operand_type_to_string(operand_type: &OperandType) -> String {
    format!("{:?}", operand_type)
}

/// Maps a string operand type to its corresponding Rust type for code generation.
fn operand_type_to_rust_type(operand_type: &str) -> &'static str {
    match operand_type {
        "Reg8" | "UInt8" | "StringId8" | "EnvId8" => "u8",
        "Addr8" => "i8",
        "UInt16" | "StringId16" | "BigIntId16" | "FunctionId16" => "u16",
        "Reg32" | "UInt32" | "StringId32" | "BigIntId32" | "FunctionId32" | "RegExpId32" => "u32",
        "Imm32" | "I32" | "Addr32" => "i32",
        "Double" => "f64",
        _ => panic!("Unknown operand type for Rust mapping: {}", operand_type),
    }
}
