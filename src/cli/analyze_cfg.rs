//! Analyze control flow graph structures
//!
//! This module provides debugging and analysis tools for CFG structures,
//! including conditional chains, loops, and other control flow patterns.

use crate::analysis::GlobalSSAAnalyzer;
use crate::ast::variables::VariableMapper;
use crate::cfg::Cfg;
use crate::hbc::HbcFile;
use anyhow::Result;
use std::path::Path;

/// Analyze the control flow graph for a specific function
pub fn analyze_cfg(input: &Path, function_index: usize, verbose: bool) -> Result<()> {
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

    // Build SSA for the function
    let ssa = GlobalSSAAnalyzer::analyze(&hbc_file)?;
    let fn_ssa = ssa.analyzer().get_function_analysis(function_index as u32);

    // Perform variable analysis
    let _var_analysis = if let Some(fn_ssa) = &fn_ssa {
        Some(crate::cfg::ssa::variable_analysis::analyze_variables(fn_ssa, &cfg)?)
    } else {
        None
    };

    // Create variable mapping
    let var_mapping = if let Some(fn_ssa) = &fn_ssa {
        let mut mapper = VariableMapper::new();
        Some(mapper.generate_mapping(fn_ssa, &cfg)?)
    } else {
        None
    };

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

    // Print verbose analysis if requested
    if verbose && fn_ssa.is_some() {
        let ssa = fn_ssa.unwrap();

        println!("Dominance Frontiers:");
        for block_idx in cfg.block_order() {
            if let Some(df) = ssa.dominance_frontiers.get(&block_idx) {
                if !df.is_empty() {
                    let df_list: Vec<String> =
                        df.iter().map(|idx| idx.index().to_string()).collect();
                    println!(
                        "  Block {}: DF = {{{}}}",
                        block_idx.index(),
                        df_list.join(", ")
                    );
                }
            }
        }
        println!();

        println!("Live-In Sets:");
        for block_idx in cfg.block_order() {
            if let Some(live_in) = ssa.live_in.get(&block_idx) {
                if !live_in.is_empty() {
                    let reg_list: Vec<String> =
                        live_in.iter().map(|reg| format!("r{}", reg)).collect();
                    println!("  Block {}: {{{}}}", block_idx.index(), reg_list.join(", "));
                }
            }
        }
        println!();

        println!("Live-Out Sets:");
        for block_idx in cfg.block_order() {
            if let Some(live_out) = ssa.live_out.get(&block_idx) {
                if !live_out.is_empty() {
                    let reg_list: Vec<String> =
                        live_out.iter().map(|reg| format!("r{}", reg)).collect();
                    println!("  Block {}: {{{}}}", block_idx.index(), reg_list.join(", "));
                }
            }
        }
        println!();

        // Print variable analysis information
        if let Some(var_mapping) = &var_mapping {
            println!("Variable Analysis:");
            println!("  Total variables: {}", var_mapping.ssa_to_var.len());
            println!("  Function-scope variables: {}", var_mapping.function_scope_vars.len());
            println!();
            
            println!("Variable Usage:");
            let mut var_usage: Vec<_> = var_mapping.variable_usage.iter().collect();
            var_usage.sort_by_key(|(name, _)| name.as_str());
            
            for (var_name, usage) in var_usage {
                let const_str = if usage.should_be_const { "const" } else { "let" };
                let param_str = if usage.is_parameter { " (param)" } else { "" };
                let def_pcs: Vec<_> = usage.definition_pcs.iter()
                    .map(|pc| pc.value().to_string())
                    .collect();
                println!("  {} {} - defined at PC: [{}], assignments: {}{}",
                    const_str,
                    var_name,
                    def_pcs.join(", "),
                    usage.assignment_count,
                    param_str
                );
            }
            println!();
            
            println!("First Definitions:");
            let mut first_defs: Vec<_> = var_mapping.first_definitions.iter().collect();
            first_defs.sort_by_key(|(name, _)| name.as_str());
            for (var_name, pc) in first_defs {
                println!("  {} - first defined at PC: {}", var_name, pc.value());
            }
            println!();
        }
    }

    for block_idx in cfg.block_order() {
        let block = cfg.graph().raw_nodes()[block_idx.index()].weight.clone();
        let start_pc = block.start_pc();
        let end_pc = block.end_pc();
        let label = if let Some(label) = hbc_file
            .jump_table
            .get_label_by_inst_index(function_index as u32, start_pc.value() as u32)
        {
            format!("({})", label)
        } else {
            String::new()
        };
        let phi_functions = fn_ssa.unwrap().get_phi_functions(block_idx);

        if block.is_exit() {
            println!("Block {} (EXIT)", block_idx.index());
        } else {
            println!(
                "Block {} idx={}..{} {}",
                block_idx.index(),
                start_pc.value(),
                end_pc.value(),
                label
            );
        }

        for phi_function in phi_functions {
            println!("  Phi function: {}", phi_function.format_phi_function());
        }

        for instruction in block.instructions() {
            println!("  {}", instruction.format_instruction(&hbc_file));

            if let Some(func_ssa) = fn_ssa {
                let definition = func_ssa
                    .definitions
                    .iter()
                    .find(|d| d.instruction_idx == instruction.instruction_index);
                let ssa_value = definition.map(|d| func_ssa.ssa_values.get(d)).flatten();
                let uses = func_ssa
                    .uses
                    .iter()
                    .filter(|u| u.instruction_idx == instruction.instruction_index)
                    .collect::<Vec<_>>();

                if let Some(def) = definition {
                    if let Some(ssa_value) = ssa_value {
                        // Get the coalesced representative if available
                        let coalesced_info = if let Some(ref var_analysis) = func_ssa.variable_analysis {
                            var_analysis.coalesced_values.get(ssa_value)
                                .map(|representative| format!(" -> {}", representative.name()))
                                .unwrap_or_else(|| " (not coalesced)".to_string())
                        } else {
                            String::new()
                        };
                        
                        if let Some(phi) = phi_functions.iter().find(|p| p.register == def.register)
                        {
                            println!(
                                "    Defines {}{}: {})",
                                ssa_value.name(),
                                coalesced_info,
                                phi.format_phi_function()
                            );
                        } else {
                            println!("    Defines {}{}", ssa_value.name(), coalesced_info);
                        }
                    } else {
                        println!(
                            "    ERROR: Invalid SSA value!  No SSA value found for register {}",
                            def.register
                        );
                    }
                } else {
                    // instruction doesn't have a definition
                }

                for reg_use in uses {
                    let reg_use_def = func_ssa.use_def_chains.get(reg_use);

                    if let Some(reg_use_def) = reg_use_def {
                        if let Some(reg_use_def_value) = func_ssa.ssa_values.get(reg_use_def) {
                            // Get the coalesced representative if available
                            let coalesced_info = if let Some(ref var_analysis) = func_ssa.variable_analysis {
                                var_analysis.coalesced_values.get(reg_use_def_value)
                                    .map(|representative| format!(" -> {}", representative.name()))
                                    .unwrap_or_else(|| " (not coalesced)".to_string())
                            } else {
                                String::new()
                            };
                            
                            if let Some(phi) = phi_functions
                                .iter()
                                .find(|phi| phi.register == reg_use_def.register)
                            {
                                println!(
                                    "    Uses {}{}: {}",
                                    reg_use_def_value.name(),
                                    coalesced_info,
                                    phi.format_phi_function()
                                );
                            } else {
                                println!("    Uses {}{}", reg_use_def_value.name(), coalesced_info);
                            }
                        } else {
                            println!(
                                "    ERROR: Invalid SSA value!  No SSA value found for register {}",
                                reg_use.register
                            );
                        }
                    } else {
                        println!(
                            "    ERROR: Invalid SSA value!  No reg use def found for register {}",
                            reg_use.register
                        );
                    }
                }
            }
        }
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
