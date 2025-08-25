//! Simple test to check basic decompilation works

use hermes_dec_rs::hbc::HbcFile;
use hermes_dec_rs::Decompiler;
use std::env;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <hbc_file>", args[0]);
        std::process::exit(1);
    }

    let hbc_path = Path::new(&args[1]);

    // Load and parse HBC file
    let data = fs::read(hbc_path)?;
    let hbc_file = HbcFile::parse(&data)?;

    println!("HBC file loaded successfully");
    println!("Version: {}", hbc_file.header.version());
    println!("Functions: {}", hbc_file.header.function_count());

    // Create decompiler
    let mut decompiler = Decompiler::new()?;

    // Try to decompile
    match decompiler.decompile(&hbc_file) {
        Ok(js_code) => {
            println!("\nDecompilation successful!");

            // Save output
            let output_path = hbc_path.with_extension("full_decompiled.js");
            fs::write(&output_path, &js_code)?;
            println!("Output saved to: {}", output_path.display());

            // Show first part of output
            println!("\nFirst 1000 characters:");
            println!("{}", &js_code.chars().take(1000).collect::<String>());
            if js_code.len() > 1000 {
                println!("\n... ({} more characters)", js_code.len() - 1000);
            }
        }
        Err(e) => {
            eprintln!("Decompilation failed: {}", e);

            // Try to decompile individual functions
            println!("\nTrying individual functions...");
            for i in 0..hbc_file.header.function_count().min(5) {
                println!("\n--- Function {} ---", i);

                // For now, just indicate we tried
                println!("Function {} would be processed here", i);
            }
        }
    }

    Ok(())
}
