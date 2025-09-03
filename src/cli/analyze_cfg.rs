//! Analyze control flow graph structures
//!
//! This module provides debugging and analysis tools for CFG structures,
//! including conditional chains, loops, and other control flow patterns.

use crate::analysis::ssa_usage_tracker::{DeclarationStrategy, SSAUsageTracker, UseStrategy};
use crate::analysis::{FunctionAnalysis, GlobalSSAAnalyzer};
use crate::ast::variables::VariableMapper;
use crate::cfg::ssa::DuplicatedSSAValue;
use crate::cfg::Cfg;
use crate::decompiler::InlineConfig;
use crate::hbc::HbcFile;
use anyhow::Result;
use std::path::Path;

/// Analyze the control flow graph for a specific function
pub fn analyze_cfg(
    input: &Path,
    function_index: usize,
    verbose: bool,
    inline_all_constants: bool,
    inline_all_property_access: bool,
    unsafe_simplify_calls: bool,
    inline_global_this: bool,
    inline_parameters: bool,
) -> Result<()> {
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

    // Build SSA for the function (lazy analysis)
    let ssa = GlobalSSAAnalyzer::analyze(&hbc_file)?;
    // Force analysis of the requested function
    ssa.analyzer()
        .ensure_function_analyzed_lazy(function_index as u32);
    let fn_ssa = ssa.analyzer().get_function_analysis(function_index as u32);

    // Perform variable analysis
    let _var_analysis = if let Some(fn_ssa) = &fn_ssa {
        Some(crate::cfg::ssa::variable_analysis::analyze_variables(
            fn_ssa, &cfg,
        )?)
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

    // Create FunctionAnalysis if we have SSA
    let func_analysis = if let Some(fn_ssa) = fn_ssa.as_ref() {
        // Get the function
        if let Ok(function) = hbc_file.functions.get(function_index as u32, &hbc_file) {
            // Build a new CFG for FunctionAnalysis (it needs ownership)
            let mut func_cfg = Cfg::new(&hbc_file, function_index as u32);
            func_cfg.build();

            // Clone the SSA analysis since FunctionAnalysis needs ownership
            Some(FunctionAnalysis::new(
                function,
                func_cfg,
                (*fn_ssa).clone(),
                &hbc_file,
                function_index as u32,
            ))
        } else {
            None
        }
    } else {
        None
    };

    // Create SSAUsageTracker if we have FunctionAnalysis
    let ssa_tracker = func_analysis.as_ref().map(|fa| SSAUsageTracker::new(fa));

    // Build ControlFlowPlan if we have FunctionAnalysis
    let control_flow_plan = func_analysis.as_ref().map(|fa| {
        let mut builder =
            crate::analysis::control_flow_plan_builder::ControlFlowPlanBuilder::new(&cfg, fa);

        // Apply optimization settings
        let mut inline_config = InlineConfig::default();
        inline_config.inline_all_constants = inline_all_constants;
        inline_config.inline_all_property_access = inline_all_property_access;
        inline_config.unsafe_simplify_calls = unsafe_simplify_calls;
        inline_config.inline_global_this = inline_global_this;
        inline_config.inline_parameters = inline_parameters;
        builder.set_inline_config(inline_config);

        // The builder.build() method already analyzes the plan internally
        builder.build()
    });

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

    // Analyze switch patterns
    let switch_analysis = if let Some(fn_ssa) = &fn_ssa {
        cfg.analyze_switch_regions(fn_ssa)
    } else {
        None
    };
    if let Some(ref analysis) = switch_analysis {
        println!("\nSwitch Analysis:");
        println!("  Total switch regions: {}", analysis.regions.len());
        println!();

        // Print detailed switch information
        for (i, region) in analysis.regions.iter().enumerate() {
            print_switch_region(&region, i, &cfg, fn_ssa.as_ref(), &hbc_file);
        }
    } else {
        println!("\nNo switch patterns found in this function.");
    }

    // Print verbose analysis if requested
    if verbose && fn_ssa.is_some() {
        let ssa = fn_ssa.clone().unwrap();

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
            println!(
                "  Function-scope variables: {}",
                var_mapping.function_scope_vars.len()
            );
            println!();

            println!("Variable Usage:");
            let mut var_usage: Vec<_> = var_mapping.variable_usage.iter().collect();
            var_usage.sort_by_key(|(name, _)| name.as_str());

            for (var_name, usage) in var_usage {
                let const_str = if usage.should_be_const {
                    "const"
                } else {
                    "let"
                };
                let param_str = if usage.is_parameter { " (param)" } else { "" };
                let def_pcs: Vec<_> = usage
                    .definition_pcs
                    .iter()
                    .map(|pc| pc.value().to_string())
                    .collect();
                println!(
                    "  {} {} - defined at PC: [{}], assignments: {}{}",
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

    // Pre-compute switch patterns for all regions to avoid repeated detection
    let mut all_switch_infos = Vec::new();
    if let Some(ref analysis) = switch_analysis {
        for region in &analysis.regions {
            // Check if this is a sparse switch by examining the dispatch block
            let dispatch_block = &cfg.graph()[region.dispatch];
            let mut is_sparse = false;
            for instr in dispatch_block.instructions() {
                match &instr.instruction {
                    crate::generated::unified_instructions::UnifiedInstruction::JStrictEqual { .. }
                    | crate::generated::unified_instructions::UnifiedInstruction::JStrictEqualLong { .. } => {
                        is_sparse = true;
                        break;
                    }
                    _ => {}
                }
            }

            if is_sparse && fn_ssa.is_some() {
                let analyzer =
                    crate::cfg::switch_analysis::SparseSwitchAnalyzer::with_hbc_file(&hbc_file);

                if let Some(postdom) = cfg.analyze_post_dominators() {
                    if let Some(switch_info) = analyzer.detect_switch_pattern(
                        region.dispatch,
                        &cfg,
                        &fn_ssa.clone().unwrap(),
                        &postdom,
                    ) {
                        all_switch_infos.push(Some(switch_info));
                    } else {
                        all_switch_infos.push(None);
                    }
                } else {
                    all_switch_infos.push(None);
                }
            } else {
                all_switch_infos.push(None);
            }
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
        let phi_functions = fn_ssa
            .as_ref()
            .map(|ssa| ssa.get_phi_functions(block_idx).to_vec())
            .unwrap_or_default();

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

        // Add block-level switch annotations
        if let Some(analysis) = &switch_analysis {
            let mut block_annotations = Vec::new();

            for (region_idx, region) in analysis.regions.iter().enumerate() {
                // Is this the dispatch block?
                if region.dispatch == block_idx {
                    block_annotations.push(format!("SWITCH_DISPATCH[{}]", region_idx));
                }

                // Is this a case head?
                for case in &region.cases {
                    if case.case_head == block_idx {
                        block_annotations
                            .push(format!("CASE_HEAD[{}.{}]", region_idx, case.case_index));
                    }
                }

                // Is this the default head?
                if let Some(default_head) = region.default_head {
                    if default_head == block_idx {
                        block_annotations.push(format!("DEFAULT_HEAD[{}]", region_idx));
                    }
                }

                // Is this the join block?
                if region.join_block == block_idx {
                    block_annotations.push(format!("SWITCH_JOIN[{}]", region_idx));
                }

                // Is this a shared tail block?
                if let Some(Some(switch_info)) = all_switch_infos.get(region_idx) {
                    if let Some(shared_tail) = &switch_info.shared_tail {
                        if shared_tail.block_id == block_idx {
                            block_annotations.push(format!("SHARED_TAIL[{}]", region_idx));
                        }
                    }
                }

                // Is this a comparison block?
                if let Some(Some(switch_info)) = all_switch_infos.get(region_idx) {
                    if switch_info
                        .cases
                        .iter()
                        .any(|c| c.comparison_block == block_idx)
                    {
                        block_annotations.push(format!("SWITCH_COMPARISON[{}]", region_idx));
                    }
                }
            }

            if !block_annotations.is_empty() {
                println!("  // {}", block_annotations.join(", "));
            }
        }

        for phi_function in &phi_functions {
            println!("  Phi function: {}", phi_function.format_phi_function());
        }

        for instruction in block.instructions() {
            let mut instruction_line = format!("  {}", instruction.format_instruction(&hbc_file));

            // Add switch-related annotations (only instruction-specific ones)
            if let Some(analysis) = &switch_analysis {
                let mut annotations = Vec::new();

                // Add instruction-specific annotations
                match &instruction.instruction {
                    crate::generated::unified_instructions::UnifiedInstruction::SwitchImm { .. } => {
                        annotations.push("DENSE_SWITCH".to_string());
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::Ret { .. } => {
                        if analysis.regions.iter().any(|r| r.join_block == block_idx) {
                            annotations.push("SHARED_RETURN".to_string());
                        }
                    }
                    crate::generated::unified_instructions::UnifiedInstruction::LoadConstString { .. }
                    | crate::generated::unified_instructions::UnifiedInstruction::LoadConstInt { .. }
                    | crate::generated::unified_instructions::UnifiedInstruction::LoadConstZero { .. } => {
                        // Check if this is a case value load
                        if analysis.regions.iter().any(|r| {
                            r.cases.iter().any(|c| c.case_head == block_idx) ||
                            r.default_head == Some(block_idx)
                        }) {
                            annotations.push("CASE_VALUE".to_string());
                        }
                    }
                    _ => {}
                }

                if !annotations.is_empty() {
                    instruction_line.push_str(&format!(" // {}", annotations.join(", ")));
                }
            }

            println!("{}", instruction_line);

            if let Some(ref func_ssa) = fn_ssa {
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
                        let coalesced_info =
                            if let Some(ref var_analysis) = func_ssa.variable_analysis {
                                var_analysis
                                    .coalesced_values
                                    .get(ssa_value)
                                    .map(|representative| format!(" -> {}", representative.name()))
                                    .unwrap_or_else(|| " (not coalesced)".to_string())
                            } else {
                                String::new()
                            };

                        // Get the actual variable name from the mapping
                        let var_name_info = if let Some(ref mapping) = var_mapping {
                            mapping
                                .ssa_to_var
                                .get(ssa_value)
                                .map(|name| format!(" [{}]", name))
                                .unwrap_or_else(|| " [unmapped]".to_string())
                        } else {
                            String::new()
                        };

                        if let Some(phi) = phi_functions
                            .iter()
                            .find(|p| p.register == def.register && p.result.def_site == *def)
                        {
                            println!(
                                "    Defines {}{}: {}{}",
                                ssa_value.name(),
                                coalesced_info,
                                phi.format_phi_function(),
                                var_name_info
                            );
                        } else {
                            println!(
                                "    Defines {}{}{}",
                                ssa_value.name(),
                                coalesced_info,
                                var_name_info
                            );
                        }

                        // Show declaration strategy if tracker is available
                        if let Some(ref tracker) = ssa_tracker {
                            let dup_ssa = DuplicatedSSAValue::original(ssa_value.clone());
                            let strategy = tracker.get_declaration_strategy(&dup_ssa);
                            let strategy_str = match strategy {
                                DeclarationStrategy::Skip => "Skip".to_string(),
                                DeclarationStrategy::SideEffectOnly => "SideEffectOnly".to_string(),
                                DeclarationStrategy::DeclareAtDominator { ref kind, .. } => {
                                    format!("DeclareAtDominator({:?})", kind)
                                }
                                DeclarationStrategy::DeclareAndInitialize { ref kind } => {
                                    format!("DeclareAndInitialize({:?})", kind)
                                }
                                DeclarationStrategy::AssignOnly => "AssignOnly".to_string(),
                            };
                            println!("      Declaration strategy: {}", strategy_str);
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
                            let coalesced_info = if let Some(ref var_analysis) =
                                func_ssa.variable_analysis
                            {
                                var_analysis
                                    .coalesced_values
                                    .get(reg_use_def_value)
                                    .map(|representative| format!(" -> {}", representative.name()))
                                    .unwrap_or_else(|| " (not coalesced)".to_string())
                            } else {
                                String::new()
                            };

                            // Get the actual variable name from the mapping
                            let var_name_info = if let Some(ref mapping) = var_mapping {
                                mapping
                                    .ssa_to_var
                                    .get(reg_use_def_value)
                                    .map(|name| format!(" [{}]", name))
                                    .unwrap_or_else(|| " [unmapped]".to_string())
                            } else {
                                String::new()
                            };

                            if let Some(phi) = phi_functions
                                .iter()
                                .find(|phi| phi.result.def_site == *reg_use_def)
                            {
                                println!(
                                    "    Uses {}{}: {}{}",
                                    reg_use_def_value.name(),
                                    coalesced_info,
                                    phi.format_phi_function(),
                                    var_name_info
                                );
                            } else {
                                println!(
                                    "    Uses {}{}{}",
                                    reg_use_def_value.name(),
                                    coalesced_info,
                                    var_name_info
                                );
                            }

                            // Show use strategy if tracker is available
                            if let Some(ref tracker) = ssa_tracker {
                                let dup_ssa =
                                    DuplicatedSSAValue::original(reg_use_def_value.clone());
                                let strategy = tracker.get_use_strategy(&dup_ssa, reg_use);
                                let strategy_str = match strategy {
                                    UseStrategy::UseVariable => "UseVariable".to_string(),
                                    UseStrategy::InlineValue(ref val) => {
                                        format!("InlineValue({:?})", val)
                                    }
                                    UseStrategy::InlinePropertyAccess(ref val) => {
                                        format!("InlinePropertyAccess({:?})", val)
                                    }
                                    UseStrategy::InlineParameter { param_index } => {
                                        format!("InlineParameter({})", param_index)
                                    }
                                    UseStrategy::InlineGlobalThis => "InlineGlobalThis".to_string(),
                                    UseStrategy::SimplifyCall { is_method_call } => {
                                        format!("SimplifyCall(method: {})", is_method_call)
                                    }
                                };
                                println!("      Use strategy: {}", strategy_str);
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

    // Print ControlFlowPlan if we have it
    if let Some(cfp) = control_flow_plan {
        println!("\n=== Control Flow Plan ===");
        println!("{}", cfp);

        // Print consumed uses and elimination details
        println!("\n=== SSA Consumption and Elimination Analysis ===");

        // Print consumed uses
        if !cfp.consumed_uses.is_empty() {
            println!("Consumed uses:");
            for (dup_ssa, uses) in &cfp.consumed_uses {
                // Also get the total uses from SSA analysis for comparison
                let total_uses = if let Some(fa) = func_analysis.as_ref() {
                    fa.ssa
                        .get_ssa_value_uses(dup_ssa.original_ssa_value())
                        .len()
                } else {
                    0
                };
                println!(
                    "  {} -> {} uses consumed (total: {})",
                    dup_ssa.original_ssa_value(),
                    uses.len(),
                    total_uses
                );
                for use_site in uses {
                    println!(
                        "    - Block {}, Instruction {}",
                        use_site.block_id.index(),
                        use_site.instruction_idx.value()
                    );
                }
            }
        } else {
            println!("No consumed uses tracked");
        }

        // Print declaration strategies
        println!("\nDeclaration strategies:");
        for (dup_ssa, strategy) in &cfp.declaration_strategies {
            let status = match strategy {
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::Skip => {
                    "ELIMINATED (Skip)"
                }
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::SideEffectOnly => {
                    "SIDE EFFECT ONLY"
                }
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::DeclareAndInitialize {
                    kind,
                } => match kind {
                    crate::analysis::ssa_usage_tracker::VariableKind::Let => {
                        "DeclareAndInitialize (let)"
                    }
                    crate::analysis::ssa_usage_tracker::VariableKind::Const => {
                        "DeclareAndInitialize (const)"
                    }
                },
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::AssignOnly => "AssignOnly",
                crate::analysis::ssa_usage_tracker::DeclarationStrategy::DeclareAtDominator {
                    kind,
                    ..
                } => match kind {
                    crate::analysis::ssa_usage_tracker::VariableKind::Let => {
                        "DeclareAtDominator (let)"
                    }
                    crate::analysis::ssa_usage_tracker::VariableKind::Const => {
                        "DeclareAtDominator (const)"
                    }
                },
            };
            println!("  {} -> {}", dup_ssa.original_ssa_value(), status);
        }
    }

    // Print constant value tracking if we have function analysis
    if let Some(fa) = func_analysis.as_ref() {
        println!("\n=== Constant Value Tracking ===");
        let value_tracker =
            crate::analysis::value_tracker::ValueTracker::new(&fa.cfg, &fa.ssa, &hbc_file);

        // Track all SSA values
        let mut constant_values = Vec::new();
        let mut phi_values = Vec::new();
        let mut param_values = Vec::new();

        for (_def_site, ssa_value) in &fa.ssa.ssa_values {
            let tracked = value_tracker.get_value(ssa_value);
            use crate::analysis::value_tracker::TrackedValue;
            match tracked {
                TrackedValue::Constant(c) => {
                    constant_values.push((ssa_value.clone(), c));
                }
                TrackedValue::Parameter { index, ssa_value } => {
                    param_values.push((ssa_value, index));
                }
                TrackedValue::Phi { ssa_value } => {
                    phi_values.push(ssa_value);
                }
                TrackedValue::GlobalObject => {
                    // Could track global object references if needed
                }
                TrackedValue::PropertyAccess { .. } => {
                    // Could track property accesses if needed
                }
                TrackedValue::Unknown => {
                    // Don't print unknown values to reduce noise
                }
            }
        }

        // Print parameters
        if !param_values.is_empty() {
            println!("Parameters:");
            for (ssa_value, index) in param_values {
                println!("  {} = Parameter(index={})", ssa_value, index);
            }
        }

        // Print PHI results
        if !phi_values.is_empty() {
            println!("PHI Results:");
            for ssa_value in phi_values {
                println!("  {} = PHI result", ssa_value);
            }
        }

        // Print constant values sorted by SSA value
        if !constant_values.is_empty() {
            println!("Constants:");
            constant_values.sort_by_key(|(ssa, _)| (ssa.register, ssa.version));
            for (ssa_value, constant) in constant_values {
                println!(
                    "  {} = {}",
                    ssa_value,
                    format_constant_value(&constant, &hbc_file)
                );
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
        "{}Chain {}: {:?} (depth={}, join={}, should_invert={})",
        indent_str,
        index,
        chain.chain_type,
        chain.nesting_depth,
        chain.join_block.index(),
        chain.should_invert
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

/// Print detailed switch region information
fn print_switch_region(
    region: &crate::cfg::analysis::SwitchRegion,
    index: usize,
    cfg: &Cfg,
    ssa: Option<&crate::cfg::ssa::SSAAnalysis>,
    hbc_file: &HbcFile,
) {
    println!("Switch Region {}:", index);
    println!(
        "  Dispatch block: {} (block index {})",
        region.dispatch.index(),
        region.dispatch.index()
    );
    println!(
        "  Join block: {} (block index {})",
        region.join_block.index(),
        region.join_block.index()
    );

    // Print case information
    println!("  Cases:");
    for case in &region.cases {
        println!(
            "    Case {}: head block {} (block index {})",
            case.case_index,
            case.case_head.index(),
            case.case_head.index()
        );
    }

    if let Some(default) = region.default_head {
        println!(
            "  Default: head block {} (block index {})",
            default.index(),
            default.index()
        );
    } else {
        println!("  Default: None");
    }

    // Analyze if this is a sparse switch pattern
    println!("\n  Pattern Analysis:");

    // Check if dispatch block contains comparisons (sparse switch) or SwitchImm (dense switch)
    let dispatch_block = &cfg.graph()[region.dispatch];
    let mut is_sparse = false;
    let mut _discriminator_reg = None;

    for instr in dispatch_block.instructions() {
        match &instr.instruction {
            crate::generated::unified_instructions::UnifiedInstruction::SwitchImm { .. } => {
                println!("    Type: Dense switch (SwitchImm instruction)");
                break;
            }
            crate::generated::unified_instructions::UnifiedInstruction::JStrictEqual {
                operand_1,
                operand_2,
                ..
            }
            | crate::generated::unified_instructions::UnifiedInstruction::JStrictEqualLong {
                operand_1,
                operand_2,
                ..
            } => {
                is_sparse = true;
                // Try to identify which operand is the discriminator
                _discriminator_reg = Some(*operand_1); // Assume first operand for now
                println!("    Type: Sparse switch (JStrictEqual comparisons)");
                println!(
                    "    Discriminator register: r{} (comparing with r{})",
                    operand_1, operand_2
                );
                break;
            }
            _ => {}
        }
    }

    // If sparse switch, try to analyze the pattern more deeply
    if is_sparse {
        println!("\n  Sparse Switch Analysis:");

        // Create a switch analyzer to detect the pattern
        let analyzer = crate::cfg::switch_analysis::SparseSwitchAnalyzer::with_hbc_file(hbc_file);

        // Try to detect the switch pattern
        if let Some(postdom) = cfg.analyze_post_dominators() {
            // Only proceed if we have SSA analysis
            if let Some(ssa_analysis) = ssa {
                if let Some(switch_info) =
                    analyzer.detect_switch_pattern(region.dispatch, cfg, ssa_analysis, &postdom)
                {
                    println!("    Detected switch pattern:");
                    println!("      Discriminator: r{}", switch_info.discriminator);
                    println!("      Cases: {}", switch_info.cases.len());

                    // Print case details
                    for (i, case) in switch_info.cases.iter().enumerate() {
                        println!("\n      Case {}:", i);
                        println!("        Keys: {:?}", case.keys);
                        println!("        Target block: {}", case.target_block.index());
                        println!(
                            "        Comparison block: {}",
                            case.comparison_block.index()
                        );
                        println!("        Execution order: {}", case.execution_order);
                        if !case.setup.is_empty() {
                            println!(
                                "        Setup instructions: {} instructions",
                                case.setup.len()
                            );
                            for (i, setup) in case.setup.iter().enumerate() {
                                println!(
                                    "          [{}] {:?} = {:?}",
                                    i, setup.ssa_value, setup.value
                                );
                            }
                        }
                    }

                    if let Some(default) = &switch_info.default_case {
                        println!("\n      Default case:");
                        println!("        Target block: {}", default.target_block.index());
                    }

                    // Analyze shared tail and PHI nodes
                    if let Some(shared_tail) = &switch_info.shared_tail {
                        println!("\n    Shared Tail Analysis:");
                        println!("      Shared tail block: {}", shared_tail.block_id.index());
                        println!("      PHI nodes: {}", shared_tail.phi_nodes.len());

                        for (_reg, phi_node) in &shared_tail.phi_nodes {
                            if let Some(ref ssa_phi) = phi_node.ssa_phi_value {
                                println!(
                                    "\n      PHI node: {} (register r{})",
                                    ssa_phi.name(),
                                    phi_node.register
                                );
                            } else {
                                println!("\n      PHI node for register r{}:", phi_node.register);
                            }
                            println!("        Values by predecessor:");
                            for (block_id, value) in &phi_node.values {
                                println!(
                                    "          Block {}: {}",
                                    block_id.index(),
                                    format_constant_value(value, hbc_file)
                                );
                            }
                        }

                        // Analyze what values each case contributes to PHI nodes
                        println!("\n      Case PHI Contributions:");

                        // TODO: Replace with ControlFlowPlanConverter once implemented
                        // Temporarily disabled during converter refactoring
                        /*
                        // Create a temporary switch converter for PHI analysis
                        let allocator = oxc_allocator::Allocator::default();
                        let ast_builder = oxc_ast::AstBuilder::new(&allocator);
                        let switch_converter =
                            crate::ast::control_flow::switch_converter::SwitchConverter::new(
                                &ast_builder,
                            );
                        */

                        for (i, case) in switch_info.cases.iter().enumerate() {
                            println!("        Case {} (keys {:?}):", i, case.keys);

                            // Create a temporary case group to analyze PHI contributions
                            let group = crate::cfg::switch_analysis::CaseGroup {
                                keys: case.keys.clone(),
                                target_block: case.target_block,
                                setup: case.setup.clone(),
                                always_terminates: case.always_terminates,
                                first_execution_order: case.execution_order,
                                comparison_blocks: vec![case.comparison_block],
                            };

                            for (_reg, phi_node) in &shared_tail.phi_nodes {
                                let phi_name = phi_node
                                    .ssa_phi_value
                                    .as_ref()
                                    .map(|ssa| ssa.name())
                                    .unwrap_or_else(|| format!("r{}", phi_node.register));

                                // For now, just show which value this case contributes
                                // The actual PHI contribution analysis is handled by the control flow plan
                                if group.target_block != shared_tail.block_id {
                                    println!("          {} = <computed in case block>", phi_name);
                                } else {
                                    println!("          {} = <direct jump to tail>", phi_name);
                                }
                            }
                        }

                        // Analyze default case PHI contributions if it exists
                        if let Some(default) = &switch_info.default_case {
                            println!("        Default case:");

                            for (_reg, phi_node) in &shared_tail.phi_nodes {
                                let phi_name = phi_node
                                    .ssa_phi_value
                                    .as_ref()
                                    .map(|ssa| ssa.name())
                                    .unwrap_or_else(|| format!("r{}", phi_node.register));

                                // Check if the PHI node has a value from the default target block
                                if let Some(value) = phi_node.values.get(&default.target_block) {
                                    println!(
                                        "          {} = {}",
                                        phi_name,
                                        format_constant_value(&value, hbc_file)
                                    );
                                } else {
                                    // Check if default flows through a block that computes a value
                                    if default.target_block != shared_tail.block_id {
                                        println!("          {} = <computed value>", phi_name);
                                    } else {
                                        println!(
                                            "          {} = <no contribution found>",
                                            phi_name
                                        );
                                    }
                                }
                            }
                        }
                    }
                } else {
                    println!("    Could not detect complete switch pattern");
                }
            } else {
                println!("    SSA analysis not available for switch pattern detection");
            }
        }
    }

    println!();
}

/// Format a ConstantValue for display
fn format_constant_value(value: &crate::analysis::ConstantValue, _hbc_file: &HbcFile) -> String {
    use crate::analysis::ConstantValue;

    match value {
        ConstantValue::Number(n) => format!("Number({})", n),
        ConstantValue::String(s) => format!("String(\"{}\")", s),
        ConstantValue::Boolean(b) => format!("Boolean({})", b),
        ConstantValue::Null => "Null".to_string(),
        ConstantValue::Undefined => "Undefined".to_string(),
        ConstantValue::ArrayLiteral(elements) => format!("Array[{}]", elements.len()),
        ConstantValue::ObjectLiteral(props) => format!("Object{{{} props}}", props.len()),
    }
}
