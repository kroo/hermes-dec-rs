//! Analyze control flow graph structures
//!
//! This module provides debugging and analysis tools for CFG structures,
//! including conditional chains, loops, and other control flow patterns.

use crate::cfg::Cfg;
use crate::hbc::HbcFile;
use anyhow::Result;
use std::path::Path;

/// Analyze the control flow graph for a specific function
pub fn analyze_cfg(input: &Path, function_index: usize) -> Result<()> {
    // Read the HBC file
    let file_data = std::fs::read(input)?;
    let hbc_file = HbcFile::parse(&file_data).map_err(|e| anyhow::anyhow!(e))?;

    // Check if function index is valid
    if function_index >= hbc_file.functions.parsed_headers.len() {
        anyhow::bail!(
            "Function index {} is out of range (max: {})",
            function_index,
            hbc_file.functions.parsed_headers.len() - 1
        );
    }

    // Build CFG for the function
    let mut cfg = Cfg::new(&hbc_file, function_index as u32);
    cfg.build();

    println!("=== CFG Analysis for Function {} ===", function_index);
    println!();

    // Print basic CFG info
    println!("Basic CFG Information:");
    println!("  Total blocks: {}", cfg.graph().node_count());
    println!("  Total edges: {}", cfg.graph().edge_count());
    println!();

    // Analyze conditional chains
    if let Some(analysis) = cfg.analyze_conditional_chains() {
        println!("Conditional Chain Analysis:");
        println!("  Total chains: {}", analysis.chains.len());
        println!();

        // Print detailed chain information
        for (i, chain) in analysis.chains.iter().enumerate() {
            print_chain_recursive(chain, 0, i);
        }
    } else {
        println!("No conditional chains found in this function.");
    }

    Ok(())
}

/// Recursively print conditional chain information
fn print_chain_recursive(
    chain: &crate::cfg::analysis::ConditionalChain,
    indent: usize,
    index: usize,
) {
    let indent_str = "  ".repeat(indent);

    println!(
        "{}Chain {}: {:?} (depth={}, join={})",
        indent_str,
        index,
        chain.chain_type,
        chain.nesting_depth,
        chain.join_block.index()
    );

    // Print branches
    for (i, branch) in chain.branches.iter().enumerate() {
        println!("{}  Branch {} ({:?}):", indent_str, i, branch.branch_type);
        println!(
            "{}    Condition block: {} (source: {})",
            indent_str,
            branch.condition_block.index(),
            branch.condition_source.index()
        );
        println!(
            "{}    Branch entry: {}",
            indent_str,
            branch.branch_entry.index()
        );
        if !branch.branch_blocks.is_empty() {
            println!(
                "{}    Branch blocks: {:?}",
                indent_str,
                branch
                    .branch_blocks
                    .iter()
                    .map(|n| n.index())
                    .collect::<Vec<_>>()
            );
        }
    }

    // Print nested chains
    if !chain.nested_chains.is_empty() {
        println!("{}  Nested chains:", indent_str);
        for (i, nested) in chain.nested_chains.iter().enumerate() {
            print_chain_recursive(nested, indent + 1, i);
        }
    }

    println!();
}
