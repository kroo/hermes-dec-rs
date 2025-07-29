use crate::cfg::Cfg;
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::path::Path;

/// Build and analyze control flow graphs for functions in an HBC file
pub fn cfg(
    input_path: &Path,
    function_index: Option<usize>,
    output_dot: Option<&Path>,
    output_loops: Option<&Path>,
    output_analysis: Option<&Path>,
) -> DecompilerResult<()> {
    // Parse the HBC file
    let data = std::fs::read(input_path)
        .map_err(|e| DecompilerError::Io(format!("Failed to read input file: {}", e)))?;

    let hbc_file = HbcFile::parse(&data).map_err(|e| DecompilerError::Internal {
        message: format!("Failed to parse HBC file: {}", e),
    })?;

    println!("HBC file parsed successfully");
    println!("Function count: {}", hbc_file.functions.count());

    // If a specific function is requested, analyze only that one
    if let Some(func_idx) = function_index {
        if func_idx >= hbc_file.functions.count() as usize {
            return Err(DecompilerError::Internal {
                message: format!(
                    "Function index {} out of range (0-{})",
                    func_idx,
                    hbc_file.functions.count() - 1
                ),
            });
        }

        analyze_function_cfg(
            &hbc_file,
            func_idx as u32,
            output_dot,
            output_loops,
            output_analysis,
        )?;
    } else {
        // Analyze all functions
        println!("Analyzing CFG for all functions...");

        // If we have an analysis output, generate one comprehensive file with all functions
        if let Some(analysis_path) = output_analysis {
            println!("  Generating comprehensive analysis visualization for all functions...");
            let mut comprehensive_dot = String::new();
            comprehensive_dot.push_str("digraph {\n");
            comprehensive_dot.push_str("  rankdir=TB;\n");
            comprehensive_dot.push_str("  node [shape=box, fontname=\"monospace\"];\n");
            comprehensive_dot.push_str("  edge [fontname=\"Arial\"];\n\n");

            for i in 0..hbc_file.functions.count() {
                println!("  Processing Function {}: ", i);

                // Get function instructions
                let instructions = hbc_file.functions.get_instructions(i)?;
                if instructions.is_empty() {
                    println!("    Empty function, skipping");
                    continue;
                }

                // Build CFG for this function
                let mut cfg = Cfg::new(&hbc_file, i);
                cfg.build();

                println!("    Basic blocks: {}", cfg.graph().node_count());
                println!("    Edges: {}", cfg.graph().edge_count());

                // Add this function's analysis as a subgraph
                let function_dot = cfg.to_dot_subgraph_with_analysis(&hbc_file, i);
                comprehensive_dot.push_str(&function_dot);
                comprehensive_dot.push_str("\n");
            }

            comprehensive_dot.push_str("}\n");

            // Write the comprehensive analysis file
            std::fs::write(analysis_path, comprehensive_dot).map_err(|e| {
                DecompilerError::Io(format!("Failed to write analysis DOT file: {}", e))
            })?;
            println!(
                "  Comprehensive analysis visualization DOT exported to: {}",
                analysis_path.display()
            );
        }

        // Also generate individual function analysis if requested
        for i in 0..hbc_file.functions.count() {
            println!("Function {}: ", i);
            analyze_function_cfg(&hbc_file, i, output_dot, output_loops, None)?;
        }
    }

    Ok(())
}

/// Analyze CFG for a specific function
fn analyze_function_cfg(
    hbc_file: &HbcFile,
    function_index: u32,
    output_dot: Option<&Path>,
    output_loops: Option<&Path>,
    output_analysis: Option<&Path>,
) -> DecompilerResult<()> {
    // Get function instructions
    let instructions = hbc_file.functions.get_instructions(function_index)?;

    println!("  Instructions: {}", instructions.len());

    if instructions.is_empty() {
        println!("  Empty function, no CFG to build");
        return Ok(());
    }

    // Build CFG
    let mut cfg = Cfg::new(hbc_file, function_index);
    cfg.build();

    println!("  Basic blocks: {}", cfg.graph().node_count());
    println!("  Edges: {}", cfg.graph().edge_count());

    // Print basic block information
    for (i, node_index) in cfg.graph().node_indices().enumerate() {
        let block = &cfg.graph()[node_index];
        println!(
            "    Block {}: PC {}-{} ({} instructions)",
            i,
            block.start_pc(),
            block.end_pc(),
            block.instruction_count()
        );
    }

    // Analyze dominators and control flow structure
    if let Some(_dominators) = cfg.analyze_dominators() {
        println!("  Dominator analysis: Available");

        // Find natural loops
        let loops = cfg.find_natural_loops();
        if !loops.is_empty() {
            println!("  Natural loops: {} found", loops.len());
            for (header, tail) in loops {
                println!(
                    "    Loop: Block {} -> Block {}",
                    header.index(),
                    tail.index()
                );
            }
        }

        // Analyze loops with detailed information
        let loop_analysis = cfg.analyze_loops();
        if !loop_analysis.loops.is_empty() {
            println!("  Loop analysis: {} loops found", loop_analysis.loops.len());
            for (i, loop_info) in loop_analysis.loops.iter().enumerate() {
                let header_info = if loop_info.is_irreducible {
                    format!("irreducible with {} headers", loop_info.headers.len())
                } else {
                    format!("header: Block {}", loop_info.primary_header().index())
                };
                println!(
                    "    Loop {}: {:?} ({}, body: {} nodes)",
                    i,
                    loop_info.loop_type,
                    header_info,
                    loop_info.body_nodes.len()
                );
                println!("      Back edges: {} found", loop_info.back_edges.len());
                println!("      Exit nodes: {} found", loop_info.exit_nodes.len());
            }
        }

        // Find if/else join blocks
        let join_blocks = cfg.find_if_else_joins();
        if !join_blocks.is_empty() {
            println!("  If/else join blocks: {} found", join_blocks.len());
            for join in join_blocks {
                println!("    Join block: Block {}", join.index());
            }
        }

        // Find switch dispatches
        let switch_dispatches = cfg.find_switch_dispatches();
        if !switch_dispatches.is_empty() {
            println!("  Switch dispatches: {} found", switch_dispatches.len());
            for (i, dispatch) in switch_dispatches.iter().enumerate() {
                println!("    Switch {}: {} targets", i, dispatch.len());
            }
        }
    } else {
        println!("  Dominator analysis: Not available (empty graph)");
    }

    // Export DOT if requested
    if let Some(dot_path) = output_dot {
        let dot_content = cfg.to_dot();
        std::fs::write(dot_path, dot_content)
            .map_err(|e| DecompilerError::Io(format!("Failed to write DOT file: {}", e)))?;
        println!("  DOT exported to: {}", dot_path.display());
    }

    // Export loop visualization DOT if requested
    if let Some(loops_path) = output_loops {
        let loops_dot_content = cfg.to_dot_with_loops();
        std::fs::write(loops_path, loops_dot_content)
            .map_err(|e| DecompilerError::Io(format!("Failed to write loops DOT file: {}", e)))?;
        println!(
            "  Loop visualization DOT exported to: {}",
            loops_path.display()
        );
    }

    // Export comprehensive analysis visualization DOT if requested
    if let Some(analysis_path) = output_analysis {
        let analysis_dot_content = cfg.to_dot_with_analysis(hbc_file);
        std::fs::write(analysis_path, analysis_dot_content).map_err(|e| {
            DecompilerError::Io(format!("Failed to write analysis DOT file: {}", e))
        })?;
        println!(
            "  Comprehensive analysis visualization DOT exported to: {}",
            analysis_path.display()
        );
    }

    Ok(())
}
