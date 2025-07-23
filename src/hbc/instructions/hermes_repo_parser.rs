use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Represents a single bytecode instruction definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InstructionDef {
    /// Instruction name (e.g., "LoadConstString")
    pub name: String,
    /// Instruction opcode value
    pub opcode: u8,
    /// Instruction operands
    pub operands: Vec<OperandDef>,
    /// Optional documentation comment
    pub doc: Option<String>,
    /// Whether this instruction has a variable number of operands
    pub is_variadic: bool,
}

/// Represents an operand definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperandDef {
    /// Operand name
    pub name: String,
    /// Operand type
    pub operand_type: OperandType,
    /// Whether this operand is optional
    pub optional: bool,
    /// Optional documentation
    pub doc: Option<String>,
}

/// Operand types for HBC instructions
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Copy)]
pub enum OperandType {
    /// 8-bit register
    Reg8,
    /// 32-bit register
    Reg32,
    /// 8-bit unsigned integer
    UInt8,
    /// 16-bit unsigned integer
    UInt16,
    /// 32-bit unsigned integer
    UInt32,
    /// 8-bit address
    Addr8,
    /// 32-bit address
    Addr32,
    /// 32-bit immediate value
    Imm32,
    /// 64-bit double precision float
    Double,
    /// String table indexes
    StringId32,
    StringId16,
    StringId8,
    /// BigInt table index
    BigIntId32,
    BigIntId16,
    /// Function table index
    FunctionId32,
    FunctionId16,
    /// RegExp table index
    RegExpId32,
    /// Environment index
    EnvId8,
}

impl OperandType {
    /// Get the size in bytes for this operand type
    pub fn size(&self) -> usize {
        match self {
            OperandType::Reg8
            | OperandType::UInt8
            | OperandType::Addr8
            | OperandType::StringId8 => 1,
            OperandType::UInt16
            | OperandType::StringId16
            | OperandType::BigIntId16
            | OperandType::FunctionId16 => 2,
            OperandType::Reg32
            | OperandType::UInt32
            | OperandType::Addr32
            | OperandType::Imm32
            | OperandType::StringId32
            | OperandType::BigIntId32
            | OperandType::FunctionId32
            | OperandType::RegExpId32
            | OperandType::EnvId8 => 4,
            OperandType::Double => 8,
        }
    }

    /// Parse operand type from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "Reg8" => Some(OperandType::Reg8),
            "Reg32" => Some(OperandType::Reg32),
            "UInt8" => Some(OperandType::UInt8),
            "UInt16" => Some(OperandType::UInt16),
            "UInt32" => Some(OperandType::UInt32),
            "Addr8" => Some(OperandType::Addr8),
            "Addr32" => Some(OperandType::Addr32),
            "Imm32" => Some(OperandType::Imm32),
            "Double" => Some(OperandType::Double),
            "StringId32" => Some(OperandType::StringId32),
            "StringId16" => Some(OperandType::StringId16),
            "StringId8" => Some(OperandType::StringId8),
            "BigIntId32" => Some(OperandType::BigIntId32),
            "BigIntId16" => Some(OperandType::BigIntId16),
            "FunctionId32" => Some(OperandType::FunctionId32),
            "FunctionId16" => Some(OperandType::FunctionId16),
            "RegExpId32" => Some(OperandType::RegExpId32),
            "EnvId8" => Some(OperandType::EnvId8),
            _ => None,
        }
    }

    pub fn from_str_and_prev_type(
        target_type: &str,
        prev_type: OperandType,
    ) -> Option<OperandType> {
        match target_type {
            "StringId" => match prev_type {
                OperandType::UInt32 => Some(OperandType::StringId32),
                OperandType::UInt16 => Some(OperandType::StringId16),
                OperandType::UInt8 => Some(OperandType::StringId8),
                // allow existing types to flow through
                OperandType::StringId32 => Some(OperandType::StringId32),
                OperandType::StringId16 => Some(OperandType::StringId16),
                OperandType::StringId8 => Some(OperandType::StringId8),
                // otherwise, this raises an error
                _ => None,
            },
            "BigIntId" => match prev_type {
                OperandType::UInt32 => Some(OperandType::BigIntId32),
                OperandType::UInt16 => Some(OperandType::BigIntId16),
                // allow existing types to flow through
                OperandType::BigIntId32 => Some(OperandType::BigIntId32),
                OperandType::BigIntId16 => Some(OperandType::BigIntId16),
                // otherwise, this raises an error
                _ => None,
            },
            "FunctionId" => match prev_type {
                OperandType::UInt32 => Some(OperandType::FunctionId32),
                OperandType::UInt16 => Some(OperandType::FunctionId16),
                // allow existing types to flow through
                OperandType::FunctionId32 => Some(OperandType::FunctionId32),
                OperandType::FunctionId16 => Some(OperandType::FunctionId16),
                // otherwise, this raises an error
                _ => None,
            },
            "RegExpId" => match prev_type {
                OperandType::UInt32 => Some(OperandType::RegExpId32),
                // allow existing types to flow through
                OperandType::RegExpId32 => Some(OperandType::RegExpId32),
                // otherwise, this raises an error
                _ => None,
            },
            "EnvId" => match prev_type {
                OperandType::UInt8 => Some(OperandType::EnvId8),
                // allow existing types to flow through
                OperandType::EnvId8 => Some(OperandType::EnvId8),
                // otherwise, this raises an error
                _ => None,
            },
            _ => None,
        }
    }
}

/// Represents a pending operand type directive that needs to be applied later
#[derive(Debug, Clone)]
struct PendingDirective {
    /// Name of the instruction this directive applies to
    instruction_name: String,
    /// 0-based operand index to modify
    operand_index: usize,
    /// Target operand type (e.g., "FunctionId", "StringId")
    target_type: String,
    /// Line number where this directive was found (for error reporting)
    line_number: usize,
}

/// Parser for BytecodeList.def files
pub struct BytecodeDefParser {
    /// Current line number for error reporting
    line_number: usize,
    /// Current instruction being parsed
    current_instruction: Option<InstructionDef>,
    /// All parsed instructions
    instructions: Vec<InstructionDef>,
    /// Pending operand type directives that couldn't be applied immediately
    pending_directives: Vec<PendingDirective>,
}

impl BytecodeDefParser {
    /// Create a new parser
    pub fn new() -> Self {
        Self {
            line_number: 0,
            current_instruction: None,
            instructions: Vec::new(),
            pending_directives: Vec::new(),
        }
    }

    /// Parse a BytecodeList.def file
    pub fn parse_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Vec<InstructionDef>> {
        let content = fs::read_to_string(path).context("Failed to read BytecodeList.def file")?;

        self.parse_content(&content)
    }

    /// Parse content from a string
    pub fn parse_content(&mut self, content: &str) -> Result<Vec<InstructionDef>> {
        self.line_number = 0;
        self.instructions.clear();
        self.pending_directives.clear();

        for line in content.lines() {
            self.line_number += 1;
            self.parse_line(line.trim())?;
        }

        // Finalize any pending instruction
        if let Some(mut instruction) = self.current_instruction.take() {
            // Apply pending directives before moving to instructions vector
            self.apply_pending_directives(&mut instruction)?;
            self.instructions.push(instruction);
        }

        // Apply any remaining pending directives to already processed instructions
        self.apply_remaining_pending_directives()?;

        // sort instructions by opcode
        self.instructions.sort_by_key(|i| i.opcode);

        Ok(self.instructions.clone())
    }

    /// Parse a single line
    fn parse_line(&mut self, line: &str) -> Result<()> {
        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
            return Ok(());
        }

        // Skip template lines that contain 'name' as a parameter (macro expansions)
        if line.starts_with("DEFINE_OPCODE") && line.contains("name") {
            return Ok(());
        }

        // Check for jump instruction definition macros
        if line.starts_with("DEFINE_JUMP_") {
            if let Err(e) = self.parse_jump_instruction_def(line) {
                println!(
                    "cargo:warning=Skipping malformed DEFINE_JUMP at line {}: {}",
                    self.line_number, e
                );
            }
        }
        // Check for instruction definition start - handle both formats
        else if line.starts_with("DEFINE_OPCODE") {
            if let Err(e) = self.parse_instruction_def(line) {
                println!(
                    "cargo:warning=Skipping malformed DEFINE_OPCODE at line {}: {}",
                    self.line_number, e
                );
            }
        } else if line.starts_with("DEFINE_OPERAND") {
            if self.current_instruction.is_some() {
                if let Err(e) = self.parse_operand_def(line) {
                    println!(
                        "cargo:warning=Skipping malformed DEFINE_OPERAND at line {}: {}",
                        self.line_number, e
                    );
                }
            } // else skip top-level DEFINE_OPERAND
        } else if line.starts_with("OPERAND_FUNCTION_ID") {
            if let Err(e) = self.parse_operand_id_directive(line, "FunctionId") {
                println!(
                    "cargo:warning=Skipping malformed OPERAND_FUNCTION_ID at line {}: {}",
                    self.line_number, e
                );
            }
        } else if line.starts_with("OPERAND_BIGINT_ID") {
            if let Err(e) = self.parse_operand_id_directive(line, "BigIntId") {
                println!(
                    "cargo:warning=Skipping malformed OPERAND_BIGINT_ID at line {}: {}",
                    self.line_number, e
                );
            }
        } else if line.starts_with("OPERAND_STRING_ID") {
            if let Err(e) = self.parse_operand_id_directive(line, "StringId") {
                println!(
                    "cargo:warning=Skipping malformed OPERAND_STRING_ID at line {}: {}",
                    self.line_number, e
                );
            }
        } else if line.starts_with("OPERAND_REGEXP_ID") {
            if let Err(e) = self.parse_operand_id_directive(line, "RegExpId") {
                println!(
                    "cargo:warning=Skipping malformed OPERAND_REGEXP_ID at line {}: {}",
                    self.line_number, e
                );
            }
        } else if line.starts_with("END_OPCODE") {
            if let Err(e) = self.finalize_instruction() {
                println!(
                    "cargo:warning=Malformed END_OPCODE at line {}: {}",
                    self.line_number, e
                );
            }
        } else {
            // Skip any other line
        }
        Ok(())
    }

    /// Parse an instruction definition line
    fn parse_instruction_def(&mut self, line: &str) -> Result<()> {
        // Finalize any previous instruction
        if let Some(mut instruction) = self.current_instruction.take() {
            // Apply pending directives before moving to instructions vector
            self.apply_pending_directives(&mut instruction)?;
            self.instructions.push(instruction);
        }

        // Handle both formats:
        // 1. DEFINE_OPCODE(name, opcode) - newer format
        // 2. DEFINE_OPCODE_N(name, operand_types...) - older format

        if line.starts_with("DEFINE_OPCODE_") {
            // Older format: DEFINE_OPCODE_N(name, operand_types...)
            let parts: Vec<&str> = line.split('(').collect();
            if parts.len() != 2 {
                anyhow::bail!(
                    "Invalid DEFINE_OPCODE_N format at line {}",
                    self.line_number
                );
            }

            let args = parts[1]
                .trim_end_matches(')')
                .split(',')
                .map(|s| s.trim())
                .collect::<Vec<_>>();
            if args.is_empty() {
                anyhow::bail!(
                    "DEFINE_OPCODE_N requires at least name at line {}",
                    self.line_number
                );
            }

            let name = args[0].to_string();
            // For older format, we'll assign opcodes sequentially starting from 0
            let opcode = self.instructions.len() as u8;

            self.current_instruction = Some(InstructionDef {
                name,
                opcode,
                operands: Vec::new(),
                doc: None,
                is_variadic: false,
            });

            // Parse operand types if present
            for operand_type_str in args.iter().skip(1) {
                if let Some(operand_type) = OperandType::from_str(operand_type_str) {
                    let operand = OperandDef {
                        name: format!(
                            "operand_{}",
                            self.current_instruction.as_ref().unwrap().operands.len()
                        ),
                        operand_type,
                        optional: false,
                        doc: None,
                    };
                    self.current_instruction
                        .as_mut()
                        .unwrap()
                        .operands
                        .push(operand);
                }
            }
        } else {
            // Newer format: DEFINE_OPCODE(name, opcode)
            let parts: Vec<&str> = line.split('(').collect();
            if parts.len() != 2 {
                anyhow::bail!("Invalid DEFINE_OPCODE format at line {}", self.line_number);
            }

            let args = parts[1]
                .trim_end_matches(')')
                .split(',')
                .map(|s| s.trim())
                .collect::<Vec<_>>();
            if args.len() < 2 {
                anyhow::bail!(
                    "DEFINE_OPCODE requires at least name and opcode at line {}",
                    self.line_number
                );
            }
            let name = args[0].trim().trim_matches('"').to_string();
            let opcode_str = args[1];
            let opcode = match opcode_str.strip_prefix("0x") {
                Some(hex) => u8::from_str_radix(hex, 16),
                None => opcode_str.parse(),
            };
            let opcode = match opcode {
                Ok(val) => val,
                Err(_) => {
                    println!(
                        "cargo:warning=Skipping DEFINE_OPCODE with invalid opcode '{}' at line {}",
                        opcode_str, self.line_number
                    );
                    return Ok(());
                }
            };
            self.current_instruction = Some(InstructionDef {
                name,
                opcode,
                operands: Vec::new(),
                doc: None,
                is_variadic: false,
            });
        }
        Ok(())
    }

    /// Parse an operand definition line
    fn parse_operand_def(&mut self, line: &str) -> Result<()> {
        let instruction = self
            .current_instruction
            .as_mut()
            .context("DEFINE_OPERAND found outside of instruction definition")?;

        // Parse DEFINE_OPERAND(type, name, optional)
        let parts: Vec<&str> = line.split('(').collect();
        if parts.len() != 2 {
            anyhow::bail!("Invalid DEFINE_OPERAND format at line {}", self.line_number);
        }

        let args_part = parts[1].trim_end_matches(')');
        let args: Vec<&str> = args_part.split(',').collect();

        if args.len() < 2 {
            anyhow::bail!(
                "DEFINE_OPERAND requires at least type and name at line {}",
                self.line_number
            );
        }

        let operand_type_str = args[0].trim().trim_matches('"');
        let operand_type = OperandType::from_str(operand_type_str).context(format!(
            "Unknown operand type '{}' at line {}",
            operand_type_str, self.line_number
        ))?;

        let name = args[1].trim().trim_matches('"').to_string();

        let optional = if args.len() > 2 {
            args[2].trim().to_lowercase() == "true"
        } else {
            false
        };

        let operand = OperandDef {
            name,
            operand_type,
            optional,
            doc: None,
        };

        instruction.operands.push(operand);

        Ok(())
    }

    /// Parse an operand ID directive (OPERAND_FUNCTION_ID, OPERAND_BIGINT_ID, etc.)
    fn parse_operand_id_directive(&mut self, line: &str, target_type: &str) -> Result<()> {
        // Parse OPERAND_FUNCTION_ID(instruction_name, operand_index)
        let parts: Vec<&str> = line.split('(').collect();
        if parts.len() != 2 {
            anyhow::bail!(
                "Invalid {} format at line {}",
                target_type,
                self.line_number
            );
        }

        let args_part = parts[1].trim_end_matches(')');
        let args: Vec<&str> = args_part.split(',').collect();

        if args.len() != 2 {
            anyhow::bail!(
                "{} requires exactly 2 arguments at line {}",
                target_type,
                self.line_number
            );
        }

        let instruction_name = args[0].trim().to_string();
        let operand_index_str = args[1].trim();

        let operand_index: usize = operand_index_str.parse().context(format!(
            "Invalid operand index '{}' at line {}",
            operand_index_str, self.line_number
        ))?;

        // Convert from 1-based indexing (Hermes source) to 0-based indexing (our parser)
        let operand_index = operand_index - 1;

        // Check if we have a current instruction and if the directive applies to it
        if let Some(instruction) = &mut self.current_instruction {
            println!("cargo:warning=DEBUG: Found {} directive for '{}' at line {}, current instruction is '{}'",
                    target_type, instruction_name, self.line_number, instruction.name);

            if instruction.name == instruction_name {
                // Directive applies to current instruction - apply immediately
                println!("cargo:warning=Processing {} directive for {} operand {} (0-based: {}) at line {}",
                        target_type, instruction_name, operand_index + 1, operand_index, self.line_number);

                // Update the operand type to the target type (FunctionId, BigIntId, etc.)
                if operand_index < instruction.operands.len() {
                    let old_type = instruction.operands[operand_index].operand_type;
                    let new_type = OperandType::from_str_and_prev_type(target_type, old_type)
                        .context(format!(
                            "Unknown operand type '{}' at line {}",
                            target_type, self.line_number
                        ))?;
                    instruction.operands[operand_index].operand_type = new_type;
                    println!(
                        "cargo:warning=Updated {} operand {} to type {}",
                        instruction_name, operand_index, target_type
                    );
                } else {
                    println!("cargo:warning=Operand index {} out of range for {} (has {} operands) at line {}",
                            operand_index, instruction_name, instruction.operands.len(), self.line_number);
                }
            } else {
                // Directive doesn't apply to current instruction - defer it
                println!("cargo:warning=Deferring {} directive for '{}' (current instruction is '{}') at line {}",
                        target_type, instruction_name, instruction.name, self.line_number);
                self.add_pending_directive(
                    instruction_name,
                    operand_index,
                    target_type.to_string(),
                );
            }
        } else {
            // No current instruction - defer the directive
            println!(
                "cargo:warning=Deferring {} directive for '{}' (no current instruction) at line {}",
                target_type, instruction_name, self.line_number
            );
            self.add_pending_directive(instruction_name, operand_index, target_type.to_string());
        }

        Ok(())
    }

    /// Finalize the current instruction
    fn finalize_instruction(&mut self) -> Result<()> {
        if self.current_instruction.is_none() {
            anyhow::bail!(
                "END_OPCODE found without matching DEFINE_OPCODE at line {}",
                self.line_number
            );
        }

        // Apply any pending directives for this instruction
        if let Some(mut instruction) = self.current_instruction.take() {
            // Apply pending directives before finalizing
            self.apply_pending_directives(&mut instruction)?;

            // Check if instruction is variadic (has variable number of operands)
            instruction.is_variadic = instruction.operands.iter().any(|op| op.optional);

            // Put the instruction back
            self.current_instruction = Some(instruction);
        }

        Ok(())
    }

    /// Get all parsed instructions
    pub fn instructions(&self) -> &[InstructionDef] {
        &self.instructions
    }

    /// Get instruction by name
    pub fn get_instruction(&self, name: &str) -> Option<&InstructionDef> {
        self.instructions.iter().find(|inst| inst.name == name)
    }

    /// Get instruction by opcode
    pub fn get_instruction_by_opcode(&self, opcode: u8) -> Option<&InstructionDef> {
        self.instructions.iter().find(|inst| inst.opcode == opcode)
    }

    /// Add a pending directive to be applied later
    fn add_pending_directive(
        &mut self,
        instruction_name: String,
        operand_index: usize,
        target_type: String,
    ) {
        let directive = PendingDirective {
            instruction_name,
            operand_index,
            target_type,
            line_number: self.line_number,
        };

        println!(
            "cargo:warning=Storing pending directive: {} operand {} -> {} (line {})",
            directive.instruction_name,
            directive.operand_index,
            directive.target_type,
            directive.line_number
        );

        self.pending_directives.push(directive);
    }

    /// Apply any pending directives for the given instruction
    fn apply_pending_directives(&mut self, instruction: &mut InstructionDef) -> Result<()> {
        let mut applied_count = 0;

        // Find and apply directives for this instruction
        for directive in &self.pending_directives {
            if directive.instruction_name == instruction.name {
                if directive.operand_index < instruction.operands.len() {
                    let old_type = instruction.operands[directive.operand_index].operand_type;
                    let new_type =
                        OperandType::from_str_and_prev_type(&directive.target_type, old_type)
                            .context(format!(
                                "Unknown operand type '{}' from pending directive at line {}",
                                directive.target_type, directive.line_number
                            ))?;

                    instruction.operands[directive.operand_index].operand_type = new_type;

                    println!("cargo:warning=Applied pending directive: {} operand {} -> {} (from line {})",
                            directive.instruction_name, directive.operand_index, directive.target_type, directive.line_number);

                    applied_count += 1;
                } else {
                    println!("cargo:warning=Pending directive operand index {} out of range for {} (has {} operands) from line {}",
                            directive.operand_index, directive.instruction_name, instruction.operands.len(), directive.line_number);
                }
            }
        }

        // Remove applied directives
        if applied_count > 0 {
            self.pending_directives
                .retain(|d| d.instruction_name != instruction.name);
        }

        Ok(())
    }

    /// Apply any remaining pending directives to already processed instructions
    fn apply_remaining_pending_directives(&mut self) -> Result<()> {
        let mut applied_count = 0;

        // Track which directives have been applied
        let mut applied_directives = Vec::new();

        // Apply pending directives to instructions in the instructions vector
        for instruction in &mut self.instructions {
            for (idx, directive) in self.pending_directives.iter().enumerate() {
                if directive.instruction_name == instruction.name {
                    if directive.operand_index < instruction.operands.len() {
                        let old_type = instruction.operands[directive.operand_index].operand_type;
                        let new_type =
                            OperandType::from_str_and_prev_type(&directive.target_type, old_type)
                                .context(format!(
                                "Unknown operand type '{}' from pending directive at line {}",
                                directive.target_type, directive.line_number
                            ))?;

                        instruction.operands[directive.operand_index].operand_type = new_type;

                        println!("cargo:warning=Applied remaining pending directive: {} operand {} -> {} (from line {})",
                                directive.instruction_name, directive.operand_index, directive.target_type, directive.line_number);

                        applied_count += 1;
                        applied_directives.push(idx);
                    } else {
                        println!("cargo:warning=Remaining pending directive operand index {} out of range for {} (has {} operands) from line {}",
                                directive.operand_index, directive.instruction_name, instruction.operands.len(), directive.line_number);
                    }
                }
            }
        }

        // Remove applied directives (in reverse order to maintain indices)
        applied_directives.sort_by(|a, b| b.cmp(a));
        for idx in applied_directives {
            self.pending_directives.remove(idx);
        }

        // Clear all pending directives after processing
        if applied_count > 0 {
            println!(
                "cargo:warning=Applied {} remaining pending directives",
                applied_count
            );
            self.pending_directives.clear();
        } else if !self.pending_directives.is_empty() {
            println!(
                "cargo:warning=Warning: {} pending directives could not be applied:",
                self.pending_directives.len()
            );
            for directive in &self.pending_directives {
                println!(
                    "cargo:warning=  - {} operand {} -> {} (line {})",
                    directive.instruction_name,
                    directive.operand_index,
                    directive.target_type,
                    directive.line_number
                );
            }
        }

        Ok(())
    }

    /// Parse a jump instruction definition line
    fn parse_jump_instruction_def(&mut self, line: &str) -> Result<()> {
        println!("DEBUG: Parsing jump line: '{}'", line);

        // Parse DEFINE_JUMP_N(name) where N is the number of operands
        let parts: Vec<&str> = line.split('(').collect();
        if parts.len() != 2 {
            anyhow::bail!("Invalid DEFINE_JUMP format at line {}", self.line_number);
        }

        let args = parts[1]
            .trim_end_matches(')')
            .split(',')
            .map(|s| s.trim())
            .collect::<Vec<_>>();
        if args.is_empty() {
            anyhow::bail!(
                "DEFINE_JUMP requires at least name at line {}",
                self.line_number
            );
        }

        let name = args[0].to_string();
        println!("DEBUG: Jump instruction name: '{}'", name);

        // Determine operand count from the macro name
        let operand_count = if line.contains("DEFINE_JUMP_1(") {
            1
        } else if line.contains("DEFINE_JUMP_2(") {
            2
        } else if line.contains("DEFINE_JUMP_3(") {
            3
        } else {
            println!(
                "DEBUG: Line doesn't match any DEFINE_JUMP pattern: '{}'",
                line
            );
            anyhow::bail!("Unknown DEFINE_JUMP variant at line {}", self.line_number);
        };

        println!("DEBUG: Operand count: {}", operand_count);

        // Create the short jump instruction (Addr8)
        let short_opcode = (self.instructions.len() + 1) as u8;

        println!("DEBUG: short_opcode = {}", short_opcode);
        let mut short_operands = Vec::new();
        short_operands.push(OperandDef {
            name: "target".to_string(),
            operand_type: OperandType::Addr8,
            optional: false,
            doc: None,
        });

        // Add additional operands based on count
        for i in 1..operand_count {
            short_operands.push(OperandDef {
                name: format!("operand_{}", i),
                operand_type: OperandType::Reg8,
                optional: false,
                doc: None,
            });
        }

        self.instructions.push(InstructionDef {
            name: name.clone(),
            opcode: short_opcode,
            operands: short_operands,
            doc: None,
            is_variadic: false,
        });

        // Create the long jump instruction (Addr32)
        let long_opcode = (self.instructions.len() + 1) as u8;
        let mut long_operands = Vec::new();
        long_operands.push(OperandDef {
            name: "target".to_string(),
            operand_type: OperandType::Addr32,
            optional: false,
            doc: None,
        });

        // Add additional operands based on count
        for i in 1..operand_count {
            long_operands.push(OperandDef {
                name: format!("operand_{}", i),
                operand_type: OperandType::Reg8,
                optional: false,
                doc: None,
            });
        }

        self.instructions.push(InstructionDef {
            name: format!("{}Long", name),
            opcode: long_opcode,
            operands: long_operands,
            doc: None,
            is_variadic: false,
        });

        println!("DEBUG: Successfully parsed jump instruction: {}", name);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operand_type_parsing() {
        assert_eq!(OperandType::from_str("Reg8"), Some(OperandType::Reg8));
        assert_eq!(OperandType::from_str("UInt32"), Some(OperandType::UInt32));
        assert_eq!(
            OperandType::from_str("StringId32"),
            Some(OperandType::StringId32)
        );
        assert_eq!(OperandType::from_str("Unknown"), None);
    }

    #[test]
    fn test_parse_simple_instruction() {
        let content = r#"
            DEFINE_OPCODE_2(LoadConstUInt8, Reg8, UInt8)
        "#;

        let mut parser = BytecodeDefParser::new();
        let instructions = parser.parse_content(content).unwrap();

        assert_eq!(instructions.len(), 1);
        let instruction = &instructions[0];
        assert_eq!(instruction.name, "LoadConstUInt8");
        assert_eq!(instruction.opcode, 0x00);
        assert_eq!(instruction.operands.len(), 2);
        assert_eq!(instruction.operands[0].name, "operand_0");
        assert_eq!(instruction.operands[0].operand_type, OperandType::Reg8);
        assert_eq!(instruction.operands[1].name, "operand_1");
        assert_eq!(instruction.operands[1].operand_type, OperandType::UInt8);
    }

    #[test]
    fn test_parse_multiple_instructions() {
        let content = r#"
            DEFINE_OPCODE_2(LoadConstString, Reg8, UInt16)
            OPERAND_STRING_ID(LoadConstString, 2)
            END_OPCODE

            DEFINE_OPCODE_2(LoadConstUInt8, Reg8, UInt8)
            END_OPCODE
        "#;

        let mut parser = BytecodeDefParser::new();
        let instructions = parser.parse_content(content).unwrap();

        assert_eq!(instructions.len(), 2);
        assert_eq!(instructions[0].name, "LoadConstString");
        assert_eq!(
            instructions[0].operands[1].operand_type,
            OperandType::StringId16
        );
        assert_eq!(instructions[1].name, "LoadConstUInt8");
    }

    #[test]
    fn test_parse_optional_operands() {
        let content = r#"
            DEFINE_OPCODE("Call", 0x10)
            DEFINE_OPERAND("UInt8", "argCount", false)
            DEFINE_OPERAND("UInt8", "extraArg", true)
            END_OPCODE
        "#;

        let mut parser = BytecodeDefParser::new();
        let instructions = parser.parse_content(content).unwrap();

        assert_eq!(instructions.len(), 1);
        let instruction = &instructions[0];
        assert!(instruction.is_variadic);
        assert_eq!(instruction.operands[0].optional, false);
        assert_eq!(instruction.operands[1].optional, true);
    }
}
