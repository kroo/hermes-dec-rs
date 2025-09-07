use crate::decompiler::{DecompileOptions, Decompiler};
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::fs;

/// Arguments for the decompile command
#[derive(Debug, Clone)]
pub struct DecompileArgs {
    pub input_path: std::path::PathBuf,
    pub function_index: usize,
    pub output_path: Option<std::path::PathBuf>,
    pub comments: String,
    pub skip_validation: bool,
    pub decompile_nested: bool,
    pub inline_constants: Option<bool>,
    pub inline_all_constants: Option<bool>,
    pub inline_property_access: Option<bool>,
    pub inline_all_property_access: Option<bool>,
    pub inline_global_this: Option<bool>,
    pub simplify_calls: Option<bool>,
    pub unsafe_simplify_calls: Option<bool>,
    pub inline_parameters: Option<bool>,
    pub inline_constructor_calls: Option<bool>,
}

impl DecompileArgs {
    /// Convert to DecompileOptions
    pub fn to_options(&self) -> DecompileOptions {
        DecompileOptions::from_cli(
            &self.comments,
            self.skip_validation,
            self.decompile_nested,
            self.inline_constants.unwrap_or(false),
            self.inline_all_constants.unwrap_or(false),
            self.inline_property_access.unwrap_or(false),
            self.inline_all_property_access.unwrap_or(false),
            self.inline_global_this,
            self.simplify_calls,
            self.unsafe_simplify_calls,
            self.inline_parameters,
            self.inline_constructor_calls,
        )
    }
}

/// Run the decompile subcommand
pub fn decompile(args: &DecompileArgs) -> DecompilerResult<()> {
    // Read the input file
    let data = match fs::read(&args.input_path) {
        Ok(data) => data,
        Err(_) => {
            return Err(DecompilerError::Internal {
                message: format!("Failed to read file: {}", args.input_path.display()),
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

    // Create decompile options using the helper method
    let options = args.to_options();

    // Decompile the specific function
    let output = match decompiler.decompile_function_with_options(
        &hbc_file,
        args.function_index as u32,
        options,
    ) {
        Ok(output) => output,
        Err(e) => {
            return Err(DecompilerError::Internal {
                message: format!(
                    "Failed to decompile function {}: {}",
                    args.function_index, e
                ),
            });
        }
    };

    // Write output
    match &args.output_path {
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
