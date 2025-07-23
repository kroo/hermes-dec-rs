use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use crate::decompiler::Decompiler;
use std::fs;

/// Run the decompile subcommand
pub fn decompile(input_path: &std::path::Path, output_path: Option<&std::path::Path>) -> DecompilerResult<()> {
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

    // Create decompiler
    let mut decompiler = Decompiler::new()?;

    // Decompile the file
    let output = match decompiler.decompile(&hbc_file) {
        Ok(output) => output,
        Err(_) => {
            return Err(DecompilerError::Internal {
                message: "Failed to decompile HBC file".to_string(),
            });
        }
    };

    // Write output
    match output_path {
        Some(path) => {
            match fs::write(path, &output) {
                Ok(_) => println!("Decompiled code written to: {}", path.display()),
                Err(_) => {
                    return Err(DecompilerError::Internal {
                        message: format!("Failed to write output to: {}", path.display()),
                    });
                }
            }
        }
        None => {
            println!("{}", output);
        }
    }

    Ok(())
} 