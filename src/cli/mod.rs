//! Command-line interface module
//! 
//! This module contains the implementations for the CLI subcommands.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

pub mod decompile;
pub mod disasm;
pub mod inspect;
pub mod generate;
pub mod cfg;

#[derive(Parser)]
#[command(name = "hermes-dec-rs")]
#[command(about = "Rust-based high-level decompiler for Hermes bytecode")]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Decompile Hermes bytecode to readable source code
    Decompile {
        /// Input HBC file
        input: std::path::PathBuf,
        /// Output source file (optional, defaults to stdout)
        output: Option<std::path::PathBuf>,
    },
    /// Disassemble Hermes bytecode to assembly-like format
    Disasm {
        /// Input HBC file
        input: std::path::PathBuf,
    },
    /// Inspect HBC file structure and metadata
    Inspect {
        /// Input HBC file
        input: std::path::PathBuf,
    },
    /// Generate unified instruction definitions from Hermes source
    Generate {
        /// Force regeneration even if files exist
        #[arg(short, long)]
        force: bool,
    },
    /// Build and analyze control flow graphs
    Cfg {
        /// Input HBC file
        input: std::path::PathBuf,
        /// Function index to analyze (optional, analyzes all if not specified)
        #[arg(short, long)]
        function: Option<usize>,
        /// Output DOT file for visualization (optional)
        #[arg(short, long)]
        dot: Option<std::path::PathBuf>,
    },
}

impl Cli {
    pub fn run() -> Result<(), Box<dyn std::error::Error>> {
        let cli = Cli::parse();

        match cli.command {
            Commands::Decompile { input, output } => {
                decompile::decompile(&input, output.as_deref())?;
            }
            Commands::Disasm { input } => {
                disasm::disasm(&input)?;
            }
            Commands::Inspect { input } => {
                inspect::inspect(&input)?;
            }
            Commands::Generate { force } => {
                if force {
                    // Remove existing generated files
                    let _ = std::fs::remove_dir_all("src/generated");
                }
                generate::generate_instructions()?;
            }
            Commands::Cfg { input, function, dot } => {
                cfg::cfg(&input, function, dot.as_deref())?;
            }
        }

        Ok(())
    }
}

/// Common CLI utilities
pub mod utils {
    use crate::error::{Error as DecompilerError, Result as DecompilerResult};
    use super::*;
    
    /// Read a file into a byte vector
    pub fn read_file(path: &std::path::Path) -> DecompilerResult<Vec<u8>> {
        std::fs::read(path)
            .map_err(DecompilerError::from)
    }
    
    /// Write output to file or stdout
    pub fn write_output(content: &str, output_path: Option<&PathBuf>) -> DecompilerResult<()> {
        match output_path {
            Some(path) => {
                std::fs::write(path, content)
                    .map_err(DecompilerError::from)
            }
            None => {
                println!("{}", content);
                Ok(())
            }
        }
    }
} 