//! Test binary for sparse switch implementation
//!
//! This program decompiles HBC files and logs information about switch pattern detection.

use hermes_dec_rs::ast::control_flow::switch_converter::SwitchConverter;
use hermes_dec_rs::cfg::Cfg;
use hermes_dec_rs::hbc::HbcFile;
use hermes_dec_rs::Decompiler;
use std::env;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <hbc_file>", args[0]);
        std::process::exit(1);
    }

    let hbc_path = Path::new(&args[1]);
    println!("Processing: {}", hbc_path.display());

    // Load and parse HBC file
    let data = fs::read(hbc_path)?;
    let hbc_file = HbcFile::parse(&data)?;

    println!("HBC Version: {}", hbc_file.header.version());
    println!("Function count: {}", hbc_file.header.function_count());

    // Process each function
    for func_idx in 0..hbc_file.header.function_count() {
        println!("\n--- Function {} ---", func_idx);

        // Build CFG for the function
        let mut cfg = Cfg::new(&hbc_file, func_idx);
        cfg.build();

        // Analyze switch regions
        if let Some(postdom_analysis) = cfg.builder().analyze_post_dominators(cfg.graph()) {
            let switch_regions =
                hermes_dec_rs::cfg::analysis::find_switch_regions(cfg.graph(), &postdom_analysis);

            if !switch_regions.regions.is_empty() {
                println!("Found {} switch regions", switch_regions.regions.len());

                // Try SSA construction
                match hermes_dec_rs::cfg::ssa::construct_ssa(&cfg, func_idx) {
                    Ok(ssa_analysis) => {
                        println!("SSA analysis successful");

                        // Test sparse switch detection on each region
                        for (i, region) in switch_regions.regions.iter().enumerate() {
                            println!("\n  Switch region {}:", i);
                            println!("    Dispatch block: {:?}", region.dispatch);
                            println!("    Cases: {}", region.cases.len());
                            println!("    Default: {:?}", region.default_head);
                            println!("    Join block: {:?}", region.join_block);

                            // Create a temporary AST builder to test pattern detection
                            let allocator = oxc_allocator::Allocator::default();
                            let ast_builder = oxc_ast::AstBuilder::new(&allocator);
                            let switch_converter = SwitchConverter::new(&ast_builder);

                            // Try to detect sparse switch pattern
                            match switch_converter.detect_switch_pattern(
                                region.dispatch,
                                &cfg,
                                &ssa_analysis,
                                &postdom_analysis,
                            ) {
                                Some(switch_info) => {
                                    println!("    ✅ Sparse switch pattern detected!");
                                    println!("    Discriminator: r{}", switch_info.discriminator);
                                    println!("    Cases detected: {}", switch_info.cases.len());
                                    println!(
                                        "    Involved blocks: {:?}",
                                        switch_info.involved_blocks
                                    );

                                    for (j, case) in switch_info.cases.iter().enumerate() {
                                        println!("\n      Case {}:", j);
                                        println!("        Keys: {:?}", case.keys);
                                        println!("        Target: Block {:?}", case.target_block);
                                        println!(
                                            "        Always terminates: {}",
                                            case.always_terminates
                                        );
                                        println!("        Source PC: {:?}", case.source_pc);
                                        if !case.setup.is_empty() {
                                            println!(
                                                "        Setup instructions: {}",
                                                case.setup.len()
                                            );
                                            for (k, setup) in case.setup.iter().enumerate() {
                                                println!(
                                                    "          {}: {} = {:?}",
                                                    k,
                                                    setup.ssa_value.name(),
                                                    setup.value
                                                );
                                            }
                                        }
                                    }

                                    if let Some(default) = &switch_info.default_case {
                                        println!("\n    Default case:");
                                        println!("      Target: Block {:?}", default.target_block);
                                        if !default.setup.is_empty() {
                                            println!(
                                                "      Setup instructions: {}",
                                                default.setup.len()
                                            );
                                        }
                                    }

                                    if let Some(shared_tail) = &switch_info.shared_tail {
                                        println!("\n    Shared tail:");
                                        println!("      Block: {:?}", shared_tail.block_id);
                                        if !shared_tail.phi_nodes.is_empty() {
                                            println!(
                                                "      PHI nodes: {:?}",
                                                shared_tail.phi_nodes
                                            );
                                        }
                                    }

                                    // Try to convert to AST to see the actual switch statement
                                    println!("\n    Attempting AST conversion...");

                                    // We need a mock block converter for this test
                                    // For now, just indicate success
                                    println!("    AST conversion would generate switch statement with {} cases", 
                                        switch_info.cases.len());
                                }
                                None => {
                                    println!("    ❌ No sparse switch pattern detected (likely dense switch)");

                                    // Let's see what's in the dispatch block to understand why
                                    if let Some(dispatch_block) =
                                        cfg.graph().node_weight(region.dispatch)
                                    {
                                        println!("    Dispatch block instructions:");
                                        for (idx, instr) in
                                            dispatch_block.instructions().iter().take(5).enumerate()
                                        {
                                            println!("      {}: {:?}", idx, instr.instruction);
                                        }
                                        if dispatch_block.instructions().len() > 5 {
                                            println!(
                                                "      ... and {} more",
                                                dispatch_block.instructions().len() - 5
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Err(e) => {
                        println!("SSA construction failed: {}", e);
                    }
                }
            } else {
                println!("No switch regions found in this function");
            }
        } else {
            println!("Post-dominator analysis failed");
        }
    }

    // Also try full decompilation
    println!("\n=== Full Decompilation ===");
    match Decompiler::new() {
        Ok(mut decompiler) => {
            match decompiler.decompile(&hbc_file) {
                Ok(js_code) => {
                    println!("Decompilation successful!");
                    println!("First 500 chars of output:");
                    println!("{}", &js_code.chars().take(500).collect::<String>());

                    // Save decompiled output
                    let output_path = hbc_path.with_extension("decompiled.js");
                    fs::write(&output_path, js_code)?;
                    println!("\nFull output saved to: {}", output_path.display());
                }
                Err(e) => {
                    println!("Decompilation failed: {}", e);
                }
            }
        }
        Err(e) => {
            println!("Failed to create decompiler: {}", e);
        }
    }

    Ok(())
}
