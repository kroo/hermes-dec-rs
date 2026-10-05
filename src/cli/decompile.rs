use crate::decompiler::{DecompileOptions, Decompiler};
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::fs;

/// Arguments for the decompile command
#[derive(Debug, Clone)]
pub struct DecompileArgs {
    pub minify: bool,
    pub hbc_version: Option<u32>,
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
    pub inline_object_literals: Option<bool>,
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
            self.inline_object_literals,
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

    if args
        .hbc_version
        .is_some_and(|expected| expected != hbc_file.header.version())
    {
        return Err(DecompilerError::Internal {
            message: format!(
                "Expected HBC {}, found {}",
                args.hbc_version.unwrap(),
                hbc_file.header.version()
            ),
        });
    }

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
    let output = if args.minify {
        let allocator = oxc_allocator::Allocator::default();
        let parsed =
            oxc_parser::Parser::new(&allocator, &output, oxc_span::SourceType::default()).parse();
        if !parsed.errors.is_empty() {
            return Err(DecompilerError::Internal {
                message: "Cannot minify invalid JavaScript".into(),
            });
        }
        oxc_codegen::Codegen::new()
            .with_options(oxc_codegen::CodegenOptions {
                minify: true,
                ..Default::default()
            })
            .build(&parsed.program)
            .code
    } else {
        output
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
