use crate::decompiler::Decompiler;
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::fs;

/// Run the decompile subcommand
pub fn decompile(
    input_path: &std::path::Path,
    function_index: usize,
    output_path: Option<&std::path::Path>,
    comments: &str,
) -> DecompilerResult<()> {
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
    let decompiler = Decompiler::new()?;

    // Decompile the specific function
    let output = match decompiler.decompile_function_with_options(&hbc_file, function_index as u32, comments) {
        Ok(output) => output,
        Err(e) => {
            return Err(DecompilerError::Internal {
                message: format!("Failed to decompile function {}: {}", function_index, e),
            });
        }
    };

    // Write output
    match output_path {
        Some(path) => match fs::write(path, &output) {
            Ok(_) => println!("Decompiled code written to: {}", path.display()),
            Err(_) => {
                return Err(DecompilerError::Internal {
                    message: format!("Failed to write output to: {}", path.display()),
                });
            }
        },
        None => {
            println!("{}", output);
        }
    }

    Ok(())
}
