use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::fs;

/// Run the inspect subcommand
pub fn inspect(input_path: &std::path::Path) -> DecompilerResult<()> {
    inspect_with_format(input_path, "json")
}

pub fn inspect_with_format(input_path: &std::path::Path, format: &str) -> DecompilerResult<()> {
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
    let parsed = if format == "text" || format == "summary" {
        HbcFile::parse_for_bundle(&data)
    } else {
        HbcFile::parse(&data)
    };
    let hbc_file: HbcFile = match parsed {
        Ok(file) => file,
        Err(error) => {
            return Err(DecompilerError::Internal {
                message: format!("Failed to parse HBC file: {error}"),
            });
        }
    };

    if format == "summary" {
        println!(
            "{}",
            serde_json::json!({"schema_version":1,"hbc_version":hbc_file.header.version(),"input_bytes":data.len(),"functions":hbc_file.functions.count(),"strings":hbc_file.header.string_count(),"commonjs_modules":hbc_file.cjs_modules.entries.len(),"entrypoint":hbc_file.header.global_code_index(),"next":"search INPUT QUERY --json, then show INPUT FUNCTION_IDS; full tables require inspect --format json"})
        );
        return Ok(());
    }

    if format == "text" {
        println!(
            "HBC {}: {} functions, {} strings, {} CommonJS modules, entrypoint {}",
            hbc_file.header.version(),
            hbc_file.functions.count(),
            hbc_file.header.string_count(),
            hbc_file.cjs_modules.entries.len(),
            hbc_file.header.global_code_index()
        );
        return Ok(());
    }
    if format != "json" {
        return Err(DecompilerError::Internal {
            message: format!("Unsupported inspect format: {format}"),
        });
    }

    // Output as JSON
    match serde_json::to_string_pretty(&hbc_file) {
        Ok(json) => {
            println!("{json}");
            Ok(())
        }
        Err(_) => Err(DecompilerError::Internal {
            message: "Failed to serialize HBC file to JSON".to_string(),
        }),
    }
}
