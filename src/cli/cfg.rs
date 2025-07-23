use crate::cfg::Cfg;
use crate::error::{Error as DecompilerError, Result as DecompilerResult};
use crate::hbc::HbcFile;
use std::path::Path;

/// Build and analyze control flow graphs for functions in an HBC file
pub fn cfg(
    input_path: &Path,
    function_index: Option<usize>,
    output_dot: Option<&Path>,
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

        analyze_function_cfg(&hbc_file, func_idx as u32, output_dot)?;
    } else {
        // Analyze all functions
        println!("Analyzing CFG for all functions...");
        for i in 0..hbc_file.functions.count() {
            println!("Function {}: ", i);
            analyze_function_cfg(&hbc_file, i, None)?;
        }
    }

    Ok(())
}

/// Analyze CFG for a specific function
fn analyze_function_cfg(
    hbc_file: &HbcFile,
    function_index: u32,
    output_dot: Option<&Path>,
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

    Ok(())
}
