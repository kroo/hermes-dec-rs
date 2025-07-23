use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::fs;

/// Run the inspect subcommand
pub fn inspect(input_path: &std::path::Path) -> DecompilerResult<()> {
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
    let hbc_file: HbcFile = match HbcFile::parse(&data) {
        Ok(file) => file,
        Err(error) => {
            println!("Failed to parse HBC file: {}", error);
            return Err(DecompilerError::Internal {
                message: format!("Failed to parse HBC file: {}", error),
            });
        }
    };

    // Output as JSON
    match serde_json::to_string_pretty(&hbc_file) {
        Ok(json) => {
            println!("{}", json);
            Ok(())
        }
        Err(_) => Err(DecompilerError::Internal {
            message: "Failed to serialize HBC file to JSON".to_string(),
        }),
    }
}
